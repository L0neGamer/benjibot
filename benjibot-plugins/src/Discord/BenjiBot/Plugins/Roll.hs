-- |
-- Module      : Discord.BenjiBot.Plugins.Roll
-- Description : A command that outputs the result of rolling dice.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs the result of rolling the input dice.
module Discord.BenjiBot.Plugins.Roll (roll) where

import Discord.BenjiBot.Plugins.Roll.Plugin (rollPlugin)
import Discord.BenjiBot.Utility

roll :: CompiledPlugin
roll = compilePlugin rollPlugin
