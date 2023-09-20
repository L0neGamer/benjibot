-- |
-- Module      : Discord.BenjiBot.Plugins
-- Description : Available plugins for Discord.BenjiBot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Here is a collection of existing plugins for Discord.BenjiBot. If you add new plugins
-- to the Plugins directory, include an import here. This means that users only
-- need to import @Discord.BenjiBot.Plugins@ to import individual plugins.
module Discord.BenjiBot.Plugins 
  ( basicPlugins
  , addAdministrationPlugin
  , minusPl
  ) where

import Control.Concurrent.MVar (MVar)
import Data.Text (Text)
import Discord.BenjiBot.Internal.Administration (ShutdownReason)
import Discord.BenjiBot.Internal.Plugins (compilePlugin)
import Discord.BenjiBot.Internal.Types (CompiledPlugin (..))
import Discord.BenjiBot.Plugins.Administration (administrationPlugin)
import Discord.BenjiBot.Plugins.Alias (alias)
import Discord.BenjiBot.Plugins.Basic (basic)
import Discord.BenjiBot.Plugins.Ping (pingpong)

basicPlugins :: [CompiledPlugin]
basicPlugins =
  [ alias
  , basic
  , pingpong
  ]

-- | @addAdministrationPlugin@ is needed to allow the administration plugin to be aware of the list of current plugins
addAdministrationPlugin :: MVar ShutdownReason -> [CompiledPlugin] -> [CompiledPlugin]
addAdministrationPlugin rFlag cps = compilePlugin (administrationPlugin rFlag cps) : cps

-- | @plugs `minusPl` names@ removes all plugins with the given names.
minusPl :: [CompiledPlugin] -> [Text] -> [CompiledPlugin]
minusPl = foldr (\n plugs -> filter ((/= n) . compiledName) plugs)
