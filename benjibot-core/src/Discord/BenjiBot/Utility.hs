-- |
-- Module      : Discord.BenjiBot.Plugin
-- Description : Helpful imports for building plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Imports for when you develop your own plugins. This deliberately hides some
-- functionality as to avoid plugin creation from breaking if the underlying types
-- are ever updated. You should always import this over "Discord.BenjiBot.Plugin.Types".
module Discord.BenjiBot.Utility
  ( module Types,
    module Utils,
    compilePlugin,
    CompiledPlugin,
  )
where

import Discord.BenjiBot.Internal.Plugins (compilePlugin)
import Discord.BenjiBot.Internal.Types (CompiledPlugin)
import Discord.BenjiBot.Utility.Types as Types hiding (Pl)
import Discord.BenjiBot.Utility.Utils as Utils
