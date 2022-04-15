{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module InterpolatePlugin 
  ( makeToStringFunction
  , plugin'
  , showFunc
  , fromStringFunc
  , pluginSettingsShow
  , pluginSettingsShowThenFromString
  , defaultParserSettings
  , PluginParserSettings (..)
  , PluginSettings (..)
  , plugin
  ) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data qualified as Uniplate
import GHC
    ( SrcSpan(..),
      GhcPs,
      GenLocated(..),
      HsExpr(HsVar, HsApp, HsLit, OpApp),
      LHsExpr,
      RdrName (Qual),
      HsLit(HsString),
      DynFlags,
      noSrcSpan,
      srcSpanEndCol,
      srcSpanEndLine,
      srcSpanStartCol,
      srcSpanStartLine,
      LHsDecl,
      NoExtField(NoExtField),
      RealSrcSpan(srcSpanFile), mkModuleName )
import GhcPlugins
    ( Plugin(parsedResultAction),
      HsParsedModule(HsParsedModule),
      RdrName(Orig),
      Hsc,
      FastString,
      mkFastStringByteString,
      defaultPlugin,
      HasDynFlags(getDynFlags),
      unpackFS,
      throwErrors,
      throwOneError,
      mkVarOcc,
      neverQualify,
      text,
      mkRealSrcLoc,
      mkRealSrcSpan,
      SourceText(NoSourceText, SourceText), mkOccName )
import Data.String (fromString)
import PrelNames (gHC_BASE, gHC_SHOW, fromString_RDR)
import Data.Functor ((<&>), ($>), void)
-- import Data.ByteString (ByteString)
import Control.Applicative as Applicative ((<|>), Alternative (empty))
import Control.Monad (forM, when)
import Lexer (unP, mkPStatePure, mkParserFlags, ParseResult (POk, PFailed), getErrorMessages)
import Parser (parseExpression)
import StringBuffer (stringToStringBuffer)
import RdrHsSyn (runECP_P)
import qualified Text.Megaparsec as P
import qualified Control.Applicative.Combinators as PC
import qualified Text.Megaparsec.Char as P
import ErrUtils (mkErrMsg)
import Data.Char (isSpace)
import GHC.Exts (fromList, IsList (toList))
import qualified Data.List.NonEmpty as NEL
import OccName (varName)

deriving stock instance Data HsParsedModule

data PluginSettings = PluginSettings
  { toStringExpression :: HsExpr GhcPs
  , parserSettings :: PluginParserSettings
  }

data PluginParserSettings = PluginParserSettings
  { interpolateSymbol :: Char
  , allowOmitBraces :: Bool 
  }
  
makeToStringFunction :: String -> String -> HsExpr GhcPs
makeToStringFunction a b = mkVar $ Qual (mkModuleName a) (mkOccName varName b)

showFunc :: HsExpr GhcPs
showFunc = mkVar (Orig gHC_SHOW (mkVarOcc "show"))

fromStringFunc :: HsExpr GhcPs
fromStringFunc = mkVar fromString_RDR

dotFunc :: HsExpr GhcPs
dotFunc = mkVar (Orig gHC_BASE (mkVarOcc "."))

pluginSettingsShow :: PluginParserSettings -> PluginSettings
pluginSettingsShow parserSettings = PluginSettings
  { toStringExpression = showFunc
  , parserSettings 
  }

pluginSettingsShowThenFromString :: PluginParserSettings -> PluginSettings
pluginSettingsShowThenFromString parserSettings = PluginSettings
  { toStringExpression =  
      let L _ a = applyFunc2 (l dotFunc) (l fromStringFunc) (l showFunc)
      in a
  , parserSettings 
  }

defaultParserSettings :: PluginParserSettings
defaultParserSettings = PluginParserSettings '#' True

plugin ::  Plugin
plugin = plugin' $ PluginSettings (makeToStringFunction "TextShow" "showt") defaultParserSettings 

plugin' :: PluginSettings -> Plugin
plugin' PluginSettings {..} = defaultPlugin {parsedResultAction = \_ _ -> findAndReplaceModule}
  where
    toStringFunction' = L noSrcSpan toStringExpression

    findAndReplaceModule :: HsParsedModule -> Hsc HsParsedModule
    findAndReplaceModule = Uniplate.transformBiM findAndReplaceDecls

    findAndReplaceDecls :: [LHsDecl GhcPs] -> Hsc [LHsDecl GhcPs]
    findAndReplaceDecls = traverse \case
      L loc expr -> L loc <$> Uniplate.transformBiM findAndReplaceExpr expr

    findAndReplaceExpr :: LHsExpr GhcPs -> Hsc (LHsExpr GhcPs)
    findAndReplaceExpr (L litSpan (HsLit _ (HsString src fs))) = do
      flags <- getDynFlags
  
      parsedString <- parseString parserSettings fs start end fileName flags
      interpolated <- interpolate parsedString src start fileName toStringFunction' flags
      let L _ expr = case interpolated of 
            [] -> mkStringLiteral ""
            (a: as) -> foldl (applyOp concatFunc) a as
      pure $ L litSpan expr

      where

        fileName = case litSpan of
          RealSrcSpan rss ->  srcSpanFile rss
          UnhelpfulSpan _ -> "<no_file>"
        end = case litSpan of
          RealSrcSpan rss -> Loc { line = srcSpanEndLine rss, col = srcSpanEndCol rss }
          UnhelpfulSpan _ -> Loc 0 0
        start = case litSpan of
          RealSrcSpan rss -> Loc { line =srcSpanStartLine rss, col = srcSpanStartCol rss }
          UnhelpfulSpan _ -> Loc 0 0

    findAndReplaceExpr a = pure a

    concatFunc = L noSrcSpan (HsVar NoExtField (l (Orig gHC_BASE (mkVarOcc "<>"))))
     

applyFunc2 :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> GenLocated SrcSpan (HsExpr GhcPs)
applyFunc2 f a = mkApplication (mkApplication f a)



applyOp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> GenLocated SrcSpan (HsExpr GhcPs)
applyOp op a b = l (OpApp NoExtField a op b)

type Parser = P.Parsec ParseError String

newtype ParseError = EmptyExpression Int
  deriving (Show, Eq, Ord)

instance P.ShowErrorComponent ParseError where
  showErrorComponent _ = ""
  

mkApplication :: LHsExpr GhcPs -> LHsExpr GhcPs -> GenLocated SrcSpan (HsExpr GhcPs)
mkApplication a b = l (HsApp NoExtField a b)

mkStringLiteral :: String -> GenLocated SrcSpan (HsExpr GhcPs)
mkStringLiteral s = l (HsLit NoExtField (HsString NoSourceText (fromString s)))

mkVar :: RdrName -> HsExpr GhcPs
mkVar name = HsVar NoExtField $ L noSrcSpan name

l :: e -> GenLocated SrcSpan e
l = L noSrcSpan

interpolate ::
  [Either (String, Int) String]
  -> SourceText
  -> Loc
  -> FastString
  -> LHsExpr GhcPs
  -> DynFlags
  -> Hsc [GenLocated SrcSpan (HsExpr GhcPs)]
interpolate parsedString src loc fileName toStringFunction flags = 
  forM parsedString \case
    Left (bs, i) -> do
      let loc' = getLoc src loc i
      let span' = mkRealSrcLoc fileName (line loc') (col loc')
      let parseInterpolatedExpr = unP 
            (parseExpression >>= runECP_P) 
            (mkPStatePure 
              (mkParserFlags flags) 
              (stringToStringBuffer bs) 
              span'
            ) 
      parsed <- case parseInterpolatedExpr of
            POk _ parsed -> pure parsed
            PFailed ps -> throwErrors $ getErrorMessages ps flags

      pure $ mkApplication toStringFunction parsed
    Right bs -> pure $ mkStringLiteral bs

parseString ::
  PluginParserSettings 
  -> FastString
  -> Loc
  -> Loc
  -> FastString
  -> DynFlags
  -> Hsc [Either (String, Int) String]
parseString settings fs start end fileName flags = case P.parse (parseStringP settings) "" (unpackFS fs) of
  Left P.ParseErrorBundle {..} -> 
    let (e, s) = case firstError of
              P.TrivialError {} -> (mkErrText "failed to parse placeholder", mkSpan (col start) (col end))
              P.FancyError n set -> head $ toList set <&> \case
                  P.ErrorCustom (EmptyExpression len) -> 
                      ( mkErrText "empty expression"
                      , mkSpan (col start + n + 1) (col start + n + len + 1)
                      )
                  _ -> error "impossible"
        mkErrText t =  text $ "Interpolate Plugin: " Prelude.<> t
        mkLoc = mkRealSrcLoc fileName
        mkSpan cs ce =  RealSrcSpan $ mkRealSrcSpan (mkLoc (line start) cs) (mkLoc (line end) ce)
        firstError = NEL.head bundleErrors
    in throwOneError $ mkErrMsg flags s neverQualify e 
  Right v -> pure v

parseStringP :: PluginParserSettings -> Parser [Either (String, Int) String]
parseStringP s@PluginParserSettings {..} = do
  r <- (P.string ['\\', interpolateSymbol] <&> Right)
    <|> Left <$> parsePlaceholderP s
    <|> (P.takeWhileP Nothing (`notElem` ("#\\" :: String)) <&> Right)
  rest <- P.eof $> [] <|> parseStringP s
  pure (r : rest)

parsePlaceholderP :: PluginParserSettings -> Parser (String, Int)
parsePlaceholderP PluginParserSettings {..} = do
  void $ P.single '#'
  
  let omittedBracesParser = if allowOmitBraces 
        then do 
          i <- P.getOffset
          res <- P.takeWhileP Nothing (not . isSpace)
          pure (res, i)
        else Applicative.empty

  let bracesParser = PC.between (P.single '{') (P.single '}') do
        i <- P.getOffset
        res <- P.many $ P.noneOf ['}'] 
        pure (res, i)

  (expr, i) <- bracesParser <|> omittedBracesParser
  when (all isSpace expr) do
    P.parseError $ P.FancyError (i + 1) (fromList [P.ErrorCustom $ EmptyExpression (length expr)])
  pure (expr, i)


data Loc = Loc {line :: Int, col :: Int}
  deriving stock (Show)

getLoc :: SourceText -> Loc ->  Int -> Loc
getLoc src Loc {..} i = case src of
      NoSourceText -> Loc line (col + i)
      SourceText srcText ->
        let Loc line' col' = getCharSourceLoc Loc { line, col } srcText i
        in Loc { line = line', col = col' }

getCharSourceLoc :: Loc -> String -> Int -> Loc
getCharSourceLoc literalLoc source index =
  move literalLoc (path source index)

data Step = NextCol | NextLine 

path :: String -> Int -> [Step]
path str shift = stepOut shift str
  where
    stepOut i ('\"' : s) = NextCol : stepIn i s
    stepOut i ('\\' : s) = NextCol : stepIn i s
    stepOut i (' ' : s) = NextCol : stepOut i s
    stepOut _ (c : _) = error ("Unknown char" ++ show c)
    stepOut _ [] = error "end of input"
    stepIn i ('\\' : '\n' : s) = NextLine : stepOut i s
    stepIn i ('\\' : ' ' : s) = NextCol : NextCol : stepOut i s
    stepIn 0 (_ : _) = []
    stepIn i ('\\' : _ : s) = NextCol : NextCol : stepIn (i - 1) s
    stepIn i (_ : s) = NextCol : stepIn (i - 1) s
    stepIn _ [] = error "end of input"

move :: Loc -> [Step] -> Loc
move = foldl \Loc {..} -> \case
  NextCol -> Loc { line, col = col + 1 }
  NextLine -> Loc { line = line + 1, col =  1 }
