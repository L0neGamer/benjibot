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
-- need to import @Tablebot.Plugins@ to import individual plugins.
module Discord.BenjiBot.Plugins.Extended (allPlugins) where

import Discord.BenjiBot.Internal.Types (CompiledPlugin (..))
import Discord.BenjiBot.Plugins.Alias (alias)
import Discord.BenjiBot.Plugins.Basic (basic)
import Discord.BenjiBot.Plugins.Cats (cat)
import Discord.BenjiBot.Plugins.Dogs (dog)
import Discord.BenjiBot.Plugins.Flip (flips)
import Discord.BenjiBot.Plugins.Fox (fox)
import Discord.BenjiBot.Plugins.Netrunner (netrunner)
import Discord.BenjiBot.Plugins.Ping (pingpong)
import Discord.BenjiBot.Plugins.Quote (quotes)
import Discord.BenjiBot.Plugins.Reminder (reminder)
import Discord.BenjiBot.Plugins.Roll (roll)
import Discord.BenjiBot.Plugins.Say (says)
import Discord.BenjiBot.Plugins.Shibe (shibe)
import Discord.BenjiBot.Plugins.Suggest (suggests)
import Discord.BenjiBot.Plugins.Welcome (welcome)

-- Use long list format to make additions and removals non-conflicting on git PRs
allPlugins :: [CompiledPlugin]
allPlugins =
  [ pingpong,
    alias,
    basic,
    cat,
    dog,
    shibe,
    flips,
    fox,
    netrunner,
    quotes,
    reminder,
    says,
    suggests,
    roll,
    welcome
  ]
