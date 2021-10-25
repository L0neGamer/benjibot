-- |
-- Module      : Tablebot.Plugins.Flip
-- Description : A command that flips a coin, or randomly selects from a list.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that picks one random element from its given arguments.
module Tablebot.Plugins.Flip (flipPlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (pack)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser
import Tablebot.Plugin.Random (chooseOneWithDefault)
import Text.Megaparsec
import Text.RawString.QQ
import Prelude hiding (flip)

-- | @SS@ denotes the type returned by the command setup. Here its unused.
type SS = ()

-- | @flip@ picks one of its arguments at random, or one of "heads" and "tails"
-- if none are provided.
flip :: Command SS
flip = Command "flip" flipcomm
  where
    flipcomm :: Parser (Message -> DatabaseDiscord SS ())
    flipcomm = do
      args <- nonSpaceWord `sepBy` space
      return $ \m -> do
        c <- case length args of
          0 -> liftIO $ chooseOneWithDefault "" ["Heads", "Tails"]
          _ -> liftIO $ chooseOneWithDefault (head args) args
        sendMessage m $ pack c

flipHelp :: HelpPage
flipHelp =
  HelpPage
    "flip"
    "flip a coin, or randomly pick from a given list"
    [r|**Flip**
Randomly picks one element from its arguments or, if none are provided, picks from heads and tails.

*Usage:*
`flip`
`flip first second third`|]
    []
    None

-- | @flipPlugin@ assembles the command into a plugin.
flipPlugin :: Plugin SS
flipPlugin = (plug "flip") {commands = [flip], helpPages = [flipHelp]}
