{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Discord.BenjiBot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the find command.
module Discord.BenjiBot.Plugins.Netrunner.Command.Find (queryCard) where

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isInfixOf, unpack)
import Discord.BenjiBot.Plugins.Netrunner.Type.Card as Card (Card (..))
import Discord.BenjiBot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Discord.BenjiBot.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Discord.BenjiBot.Utility.Utils (standardise)

-- | @queryCard@ searches the given library of cards by title, first checking if
-- the search query is a substring of any cards, then performing a fuzzy search on
-- the cards given, or all of the cards if no cards are found
queryCard :: NrApi -> Text -> Card
queryCard NrApi {cards = cards} txt = findCard (substringSearch pairs txt) txt pairs
  where
    pairs = zip (map (standardise . fromMaybe "" . Card.title) cards) cards
    substringSearch pairs' searchTxt = filter (isInfixOf (standardise searchTxt) . fst) pairs'

-- | @findCard@ finds a card from the given list of pairs that is some subset of a
-- full list. If the sublist is empty, it will fuzzy search the full list. If the sublist
-- has exactly 1 element, it'll return that element. If the sublist has multiple
-- elements, it will fuzzy search the sublist
findCard :: [(Text, Card)] -> Text -> [(Text, Card)] -> Card
findCard [] searchTxt allCards = fuzzyQueryCard allCards searchTxt
findCard [(_, card)] _ _ = card
findCard cards searchTxt _ = fuzzyQueryCard cards searchTxt

-- | @queryCard@ fuzzy searches the given library of cards by title.
fuzzyQueryCard :: [(Text, Card)] -> Text -> Card
fuzzyQueryCard pairs = closestValueWithCosts editCosts unpackedPairs . unpack
  where
    unpackedPairs = fmap (first unpack) pairs
    editCosts =
      FuzzyCosts
        { deletion = 10,
          insertion = 2,
          substitution = 10,
          transposition = 1
        }
