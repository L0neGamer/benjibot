module Main where

import Data.Default
import Lib
import Plugins.Netrunner
import Tablebot
import Tablebot.Internal.Plugins
import Tablebot.Internal.Types
import Tablebot.Plugins.Ping
import Data.Text

main :: IO ()
main = runTablebotWithEnv [pingpong, compilePlugin netrunnerPlugin] (def {botName=pack "Sahasrara"})
