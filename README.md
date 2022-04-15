# Interpolate plugin for GHC

```haskell
main :: IO ()
main = 
  let a = 5
      b = 6
  in putStrLn "#a + #b = #{ a + b }"
```
` >>> 5 + 6 = 11`

Plugin can interpolate values and Haskell expressions into strings (`forall str. IsString str`) with your custom `toString` function.

## Pros and Why not `iii`
- Interpolating with plugin looks like valid Haskell code
- Your custom `toString`
- Simple implementation

## Cons and Why `iii`
- Since it's a plugin, it must be defined outside of your project
- To specify `toString` function you need to create your own plugin based on this one
- String literals not supported inside of placeholders
```haskell
  -- will be parsed by GHC as 3 strings separated by `foo` and `bar`
   "#{ if cond then "foo" else "bar" }" 
```
- HLS shows plugin's errors at the top of file
