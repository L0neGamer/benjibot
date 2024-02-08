-- -- |
-- Module      : Discord.BenjiBot.Plugins.Basic
-- Description : A very simple example plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which responds to certain calls with specific responses.
module Discord.BenjiBot.Plugins.Basic (basic) where

import Data.Text as T (Text, toTitle)
import Discord.Types (Message)
import Discord.BenjiBot.Utility
import Discord.BenjiBot.Utility.Discord (sendMessage)
import Discord.BenjiBot.Utility.SmartParser (parseComm)
import Text.Megaparsec (anySingle, skipManyTill)
import Text.Megaparsec.Char (string')
import Text.RawString.QQ (r)

-- * Some types to help clarify what's going on

-- | @MiniHelpPage@ simplifies creating the help pages. You can either provide just the short and long help text and let
-- it autogenerate the formatting, or you can provide a full help page if you want more control
data MiniHelpPage = Simple (Text, Text) | Advanced HelpPage

-- | @BasicCommand@ is a tuple containing the important information for this command.
-- Put the command in the first element, the response in the second element of each tuple,
-- and a 'MiniHelpPage' for your command in the third
type BasicCommand = (Text, Text, MiniHelpPage)

-- | The basic commands.
basicCommands :: [BasicCommand]
basicCommands =
  [ ( "benji",
      "<:benji_sit:920000993721196654>",
      Simple ("the almost mascot", "Though he may sit, when put to test, the gender cube proved it was best")
    ),
    ( "about",
      aboutStr,
      Simple ("some information about the bot", "Some information about the bot, including how you can get involved")
    )
  ]
  where
    aboutStr =
      [r|This bot was created to replace a couple of other bots in its original server.
It's written in Haskell, and you can find the code here: <https://github.com/l0negamer/benjibot>.
If you would like to contribute, there are setup guides and a contributor's guide to help you get started!

If you have found a bug, please report it on Github (<https://github.com/l0negamer/benjibot/issues>) or inform one of the maintainers.|]

-- | @echo@ pulled out to help resolve parser overlapping instances errors.
-- Sends the provided text, regardless of received message.
echo :: Text -> Message -> DatabaseDiscord ()
echo t m = sendMessage m t

-- | Given command text "a", reply with text "b".
baseCommand :: BasicCommand -> Command
baseCommand (a, b, _) =
  Command
    a
    (parseComm $ echo b)
    []

baseHelp :: BasicCommand -> HelpPage
baseHelp (_, _, Advanced help) = help
baseHelp (a, _, Simple (short, long)) = HelpPage a [] short ("**" <> toTitle a <> "**\n" <> long <> "\n\n*Usage:* `" <> a <> "`") [] None

type BasicInlineCommand = (Text, Text)

basicInlineCommands :: [BasicInlineCommand]
basicInlineCommands =
  [ ("thank you benjibot", "You're welcome!")
  ]

baseInlineCommand :: BasicInlineCommand -> InlineCommand
baseInlineCommand (t, rs) = InlineCommand (skipManyTill anySingle (string' t) >> return (`sendMessage` rs))

-- | @basicPlugin@ assembles the call and response commands into a simple command list.
basicPlugin :: Plugin
basicPlugin =
  (plug "basic")
    { commands = map baseCommand basicCommands,
      helpPages = map baseHelp basicCommands,
      inlineCommands = map baseInlineCommand basicInlineCommands
    }

basic :: CompiledPlugin
basic = compilePlugin basicPlugin
