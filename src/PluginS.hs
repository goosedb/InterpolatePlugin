module PluginS where

import InterpolatePlugin ( pluginSettingsShow, defaultParserSettings, plugin' )
import GhcPlugins ( Plugin )

plugin :: Plugin
plugin = plugin' $ pluginSettingsShow defaultParserSettings 