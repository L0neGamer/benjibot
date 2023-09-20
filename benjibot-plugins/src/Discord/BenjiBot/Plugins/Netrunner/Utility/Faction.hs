-- |
-- Module      : Discord.BenjiBot.Plugins.Netrunner.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner factions in Discord.BenjiBot.
module Discord.BenjiBot.Plugins.Netrunner.Utility.Faction where

import Data.Text
import Discord.BenjiBot.Plugins.Netrunner.Type.Faction (Faction (..))
import Discord.BenjiBot.Plugins.Netrunner.Type.NrApi (NrApi)
import Discord.BenjiBot.Utility
import Discord.BenjiBot.Utility.Discord (formatFromEmojiName)
import Discord.BenjiBot.Utility.Types ()

-- | @toEmoji@ takes a faction and attempts to find its Discord emoji.
toEmoji :: Faction -> EnvDatabaseDiscord NrApi Text
toEmoji Faction {code = code} = case code of
  "haas-bioroid" -> formatFromEmojiName "hb"
  "jinteki" -> formatFromEmojiName "jinteki"
  "nbn" -> formatFromEmojiName "nbn"
  "weyland-consortium" -> formatFromEmojiName "weyland"
  "anarch" -> formatFromEmojiName "anarch"
  "criminal" -> formatFromEmojiName "criminal"
  "shaper" -> formatFromEmojiName "shaper"
  "adam" -> formatFromEmojiName "adam"
  "apex" -> formatFromEmojiName "apex"
  "sunny-lebeau" -> formatFromEmojiName "sunny"
  _ -> formatFromEmojiName "nisei"
