{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)
import Http.Server (runServer)
import UserData.Processors (processors)
import Processor.Process (initConfigs)
import Processor.Config (ProcessorConfig)
import Data.Text (pack)
import System.Environment (getEnv)

import App (AppEnv (..), MailgunSigningKey (..))
import Log (log, HasLog)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

printErr :: (MonadIO m, MonadReader e m, HasLog e) => IOError -> m ()
printErr = log . ("Error configuring processors: " <>) . show

initAll :: (HasLog e, MonadReader e m, MonadIO m) => [ProcessorConfig] -> m ()
initAll ps = do
  log $ "Initialising " <> (show . length $ ps) <> " processors"
  _ <- liftIO (initConfigs ps)
  log "-> Done!"

logger :: String -> IO ()
logger = putStrLn

main :: IO ()
main = do
  env <- flip AppEnv logger . MailgunSigningKey . pack <$> getEnv "MAILGUN_SIGNING_KEY"
  _ <- runReaderT (initAll processors) env
  _ <- logger "Starting server"
  runServer processors env
