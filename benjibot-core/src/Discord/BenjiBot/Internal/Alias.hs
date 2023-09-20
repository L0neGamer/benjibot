{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Discord.BenjiBot.Internal.Alias
-- Description : Alias management
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Alias management
module Discord.BenjiBot.Internal.Alias where

import Control.Monad.Exception (MonadException (catch), SomeException)
import Data.Text
import Database.Persist.Sqlite (BackendKey (SqlBackendKey))
import qualified Database.Persist.Sqlite as Sql
import Database.Persist.TH
import Discord.Types
import Discord.BenjiBot.Internal.Administration (currentBlacklist)
import Discord.BenjiBot.Internal.Types
import Discord.BenjiBot.Utility.Database (liftSql, selectList)
import Discord.BenjiBot.Utility.Types (EnvDatabaseDiscord)

share
  [mkPersist sqlSettings, mkMigrate "aliasMigration"]
  [persistLowerCase|
Alias
    alias Text
    command Text
    type AliasType
    UniqueAlias alias type
    deriving Show
    deriving Eq
|]

getAliases :: UserId -> EnvDatabaseDiscord d (Maybe [Alias])
getAliases uid = do
  blacklist <- liftSql currentBlacklist
  if "alias" `Prelude.elem` blacklist
    then return Nothing
    else
      (Just . fmap Sql.entityVal <$> selectList [AliasType Sql.<-. [AliasPublic, AliasPrivate uid]] [])
        `catch` (\(_ :: SomeException) -> return Nothing)
