-- |
-- Module      : Discord.BenjiBot.Plugins.Netrunner
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Discord.BenjiBot.Plugins.Netrunner (netrunner) where

import Discord.BenjiBot.Plugins.Netrunner.Plugin (netrunnerPlugin)
import Discord.BenjiBot.Utility (CompiledPlugin, compilePlugin)

netrunner :: CompiledPlugin
netrunner = compilePlugin netrunnerPlugin
