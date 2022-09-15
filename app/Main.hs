{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (AppEnv (..), MailgunSigningKey (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Text (pack)
import Http.Server (runServer)
import Log (HasLog, log)
import ProcessorConfig.Init (initConfigs)
import ProcessorConfig.Types (ProcessorConfig)
import Processors (processors)
import System.Environment (getEnv)
import Prelude hiding (log)

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
