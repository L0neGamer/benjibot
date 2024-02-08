{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : BenjiBot
-- Description : The main runner for the BenjiBot Discord bot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the main runner for BenjiBot. If you're just looking to
-- run the bot with existing plugins, import this and your favourite plugins
-- from "BenjiBot.Plugins".
module Discord.BenjiBot
  ( runBenjiBot,
    runBenjiBotWithEnv,
    BotConfig (..),
  )
where

import Control.Monad.Extra
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Map as M (empty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Sqlite
  ( runMigration,
    runSqlPool,
    withSqlitePool,
  )
import Discord
import Discord.Internal.Rest
import LoadEnv (loadEnv)
import Paths_benjibot_core (version)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import Discord.BenjiBot.Handler (eventHandler, killCron, runCron, submitApplicationCommands)
import Discord.BenjiBot.Internal.Administration
  ( ShutdownReason (Reload),
    adminMigration,
    currentBlacklist,
    gitVersion,
    removeBlacklisted,
    restartAction,
    restartIsTerminal,
  )
import Discord.BenjiBot.Internal.Plugins
import Discord.BenjiBot.Internal.Types
import Discord.BenjiBot.Plugins (addAdministrationPlugin)
import Discord.BenjiBot.Utility
import Discord.BenjiBot.Utility.Help (generateHelp)
import Discord.BenjiBot.Utility.Font (makeFontMap)
import Text.Regex.PCRE ((=~))
import UnliftIO.Concurrent

-- | runBenjiBotWithEnv @plugins@ runs the bot using data found in the .env
-- file with the @[CompiledPlugin]@ given. If you're looking to run the bot as
-- detailed in the README (i.e. using data from .env), you should call this
-- function.
runBenjiBotWithEnv :: [CompiledPlugin] -> BotConfig -> IO ()
runBenjiBotWithEnv plugins config = do
  -- fetch the version info as soon after building to reduce the likelihood that it changes between build and run
  gv <- gitVersion
  let vInfo = VInfo gv version
  rFlag <- newMVar Reload :: IO (MVar ShutdownReason)
  whileM $ do
    _ <- swapMVar rFlag Reload
    loadEnv
    dToken <- pack <$> getEnv "DISCORD_TOKEN"
    unless (encodeUtf8 dToken =~ ("^[A-Za-z0-9_-]{24}[.][A-Za-z0-9_-]{6}[.][A-Za-z0-9_-]{38}$" :: String)) $
      die "Invalid token format. Please check it is a bot token"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    dbpath <- getEnv "SQLITE_FILENAME"
    runBenjiBot vInfo dToken prefix dbpath (addAdministrationPlugin rFlag plugins) config
    exit <- swapMVar rFlag Reload
    restartAction exit
    pure $ not (restartIsTerminal exit)
  putStrLn "Bot closed"

-- | runBenjiBot @vinfo@ @dToken@ @prefix@ @dbpath@ @plugins@ @config@ runs the
-- bot using the given Discord API token @dToken@ and SQLite connection string
-- @dbpath@. In general, you should prefer @runBenjiBotWithEnv@ as it gets all
-- of the required data for you, but this is exported for if you have weird
-- setup requirements or don't want to use the administration plugin.
-- Only the plugins provided by @plugins@ are run, and all commands
-- are prefixed with @prefix@. @config@ details how the bot should present
-- itself to users, allowing programmers to replace the BenjiBot-specific text
-- with their own.
-- The plugins given are combined into a single plugin with their combined
-- functionality. Each migration present in the combined plugin is run, and
-- each cron job and handler is set up.
-- This creates a small pool of database connections used by the event handler,
-- builds an event handler and starts cron jobs. It also kills the cron jobs on
-- bot close.
runBenjiBot :: VersionInfo -> Text -> Text -> FilePath -> [CompiledPlugin] -> BotConfig -> IO ()
runBenjiBot vinfo dToken prefix dbpath plugins config =
  do
    debugPrint ("DEBUG enabled. This is strongly not recommended in production!" :: String)
    -- Create multiple database threads.
    runNoLoggingT . withSqlitePool (pack dbpath) 8 $ \pool -> do
      -- Setup and then apply plugin blacklist from the database
      runSqlPool (runMigration adminMigration) pool
      blacklist <- runResourceT $ runNoLoggingT $ runSqlPool currentBlacklist pool
      let filteredPlugins = removeBlacklisted blacklist plugins
      -- Combine the list of plugins into both a combined plugin
      let !plugin = generateHelp (rootHelpText config) $ combinePlugins filteredPlugins
      -- Run the setup actions of each plugin and collect the plugin actions into a single @PluginActions@ instance
      allActions <- NoLoggingT $ mapM (runResourceT . runNoLoggingT . flip runSqlPool pool) (combinedSetupAction plugin)
      let !actions = combineActions allActions

      -- TODO: this might have issues with duplicates?
      -- TODO: in production, this should probably run once and then never again.
      mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ combinedMigrations plugin
      -- Create a var to kill any ongoing tasks.
      mvar <- newEmptyMVar
      fm <- NoLoggingT makeFontMap
      cacheMVar <- newMVar (Discord.BenjiBot.Utility.Cache M.empty M.empty vinfo fm)
      userFacingError <-
        NoLoggingT $
          runDiscord $
            def
              { discordToken = dToken,
                discordOnEvent =
                  flip runSqlPool pool . flip runReaderT cacheMVar . eventHandler actions prefix,
                discordOnStart = do
                  -- Build list of cron jobs, saving them to the mvar.
                  -- Note that we cannot just use @runSqlPool@ here - this creates
                  -- a single transaction which is reverted in case of exception
                  -- (which can just happen due to databases being unavailable
                  -- sometimes).
                  runReaderT (mapM (runCron pool) (compiledCronJobs actions) >>= liftIO . putMVar mvar) cacheMVar

                  submitApplicationCommands (compiledApplicationCommands actions) cacheMVar

                  liftIO $ putStrLn "The bot lives!"
                  sendCommand (UpdateStatus activityStatus),
                -- Kill every cron job in the mvar.
                discordOnEnd = takeMVar mvar >>= killCron
              }
      liftIO $ putStrLn $ T.unpack userFacingError
  where
    activityStatus =
      UpdateStatusOpts
        { updateStatusOptsSince = Nothing,
          updateStatusOptsActivities = [mkActivity (gamePlaying config prefix) ActivityTypeGame],
          updateStatusOptsNewStatus = UpdateStatusOnline,
          updateStatusOptsAFK = False
        }
