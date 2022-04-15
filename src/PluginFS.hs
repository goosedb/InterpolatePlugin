module PluginFS where

import InterpolatePlugin ( defaultParserSettings, plugin', pluginSettingsShowThenFromString )
import GhcPlugins ( Plugin )

plugin :: Plugin
plugin = plugin' $ pluginSettingsShowThenFromString defaultParserSettings 