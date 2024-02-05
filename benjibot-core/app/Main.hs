{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Discord.BenjiBot (BotConfig (..), runBenjiBotWithEnv)
import Discord.BenjiBot.Plugins.Administration
import Discord.BenjiBot.Plugins

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main =
  runBenjiBotWithEnv basicPlugins $
    BotConfig
      { gamePlaying = game,
        rootHelpText = rootBody
      }

game :: Text -> Text
game prefix = "with dice. Prefix is `" <> prefix <> "`. Call `" <> prefix <> "help` for help"

rootBody :: Text
rootBody =
  "**BenjiBot**\n\
  \This friendly little bot provides several tools to help with\
  \ the running of this server."
