-- |
-- Module      : Tablebot.Plugins.Netrunner.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of all Netrunner data in Tablebot.
module Tablebot.Plugins.Netrunner.NrApi (getNrApi) where

import Data.Aeson (FromJSON, Value (Object), eitherDecode, parseJSON, (.:))
import Data.Either (fromRight)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import Tablebot.Plugins.Netrunner.Type.Card (Card)
import Tablebot.Plugins.Netrunner.Type.Cycle (Cycle)
import Tablebot.Plugins.Netrunner.Type.Faction (Faction)
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Type.Pack (Pack)

-- | @getNrApi@ is a function that attempts to get the JSON objects containing
-- all required Netrunner data (cards, cycles, and packs) as provided by
-- https://netrunnerdb.com/api/2.0/doc.
getNrApi :: IO NrApi
getNrApi = do
  cardReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/cards"
  cardRes <- httpLBS cardReq
  let cardData = fromRight defaultCards ((eitherDecode $ responseBody cardRes) :: Either String Cards)
  cycleReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/cycles"
  cycleRes <- httpLBS cycleReq
  let cycleData = fromRight defaultCycles ((eitherDecode $ responseBody cycleRes) :: Either String Cycles)
  factionReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/factions"
  factionRes <- httpLBS factionReq
  let factionData = fromRight defaultFactions ((eitherDecode $ responseBody factionRes) :: Either String Factions)
  packReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/packs"
  packRes <- httpLBS packReq
  let packData = fromRight defaultPacks ((eitherDecode $ responseBody packRes) :: Either String Packs)
  return $
    NrApi
      { cards = reverse $ cardContent cardData, -- Reversing the list of cards prioritises newer cards in the search
        cycles = cycleContent cycleData,
        factions = factionContent factionData,
        packs = packContent packData,
        imageTemplate = imageUrlTemplate cardData
      }

-- | @Cards@ represents the full library of cards in Netrunner.
data Cards = Cards
  { cardContent :: ![Card],
    imageUrlTemplate :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Cards where
  parseJSON (Object v) = do
    content <- v .: "data"
    imageUrlTemplate <- v .: "imageUrlTemplate"
    return $ Cards {cardContent = content, imageUrlTemplate = imageUrlTemplate}
  parseJSON _ = return defaultCards

defaultCards :: Cards
defaultCards = Cards {cardContent = [], imageUrlTemplate = ""}

-- | @Cycles@ represents all cycles in the game's history.
data Cycles = Cycles {cycleContent :: [Cycle]} deriving (Show, Generic)

instance FromJSON Cycles where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Cycles {cycleContent = content}
  parseJSON _ = return defaultCycles

defaultCycles :: Cycles
defaultCycles = Cycles {cycleContent = []}

-- | @Factions@ represents all factions in the game's.
data Factions = Factions {factionContent :: [Faction]} deriving (Show, Generic)

instance FromJSON Factions where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Factions {factionContent = content}
  parseJSON _ = return defaultFactions

defaultFactions :: Factions
defaultFactions = Factions {factionContent = []}

-- | @Packs@ represents all data packs in the game's history.
data Packs = Packs {packContent :: [Pack]} deriving (Show, Generic)

instance FromJSON Packs where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Packs {packContent = content}
  parseJSON _ = return defaultPacks

defaultPacks :: Packs
defaultPacks = Packs {packContent = []}
