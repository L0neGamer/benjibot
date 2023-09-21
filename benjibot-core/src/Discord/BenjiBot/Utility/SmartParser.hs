-- |
-- Module      : Discord.BenjiBot.Utility.SmartParser
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Discord.BenjiBot.Utility.SmartParser
  ( module Discord.BenjiBot.Utility.SmartParser.SmartParser,
    module Discord.BenjiBot.Utility.SmartParser.Interactions,
    module Discord.BenjiBot.Utility.SmartParser.Types,
  )
where

import Discord.BenjiBot.Utility.SmartParser.Interactions
import Discord.BenjiBot.Utility.SmartParser.SmartParser
import Discord.BenjiBot.Utility.SmartParser.Types
