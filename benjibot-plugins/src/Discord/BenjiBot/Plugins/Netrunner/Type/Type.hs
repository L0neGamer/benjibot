-- |
-- Module      : Discord.BenjiBot.Plugins.Netrunner.Type.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The Pack and Packs types.
module Discord.BenjiBot.Plugins.Netrunner.Type.Type where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Text (Text)

-- | @Type@ represents a single card type in the NetrunnerDB API.
data Type = Type
  { code :: !Text,
    name :: !Text,
    position :: !Int,
    is_subtype :: !Bool,
    side_code :: !(Maybe Text)
  }
  deriving (Show)

instance FromJSON Type where
  parseJSON = withObject "Type" $ \o ->
    Type <$> o .: "code"
      <*> o .: "name"
      <*> o .: "position"
      <*> o .: "is_subtype"
      <*> o .:? "side_code"
