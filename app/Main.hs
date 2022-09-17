{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (AppEnv (..), appMToIO, MailgunSigningKey (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack, unpack)
import Http.Server (runServer)
import Log (Logger (..))
import ProcessorConfig.Init (initConfigs)
import ProcessorConfig.Types (ProcessorConfig)
import Processors (processors)
import System.Environment (getEnv)
import Prelude hiding (log)

printErr :: (Logger m, MonadIO m) => IOError -> m ()
printErr = log . ("Error configuring processors: " <>) . pack . show

initAll :: (Logger m, MonadIO m) => [ProcessorConfig] -> m ()
initAll ps = do
  log $ "Initialising " <> (pack . show . length $ ps) <> " processors"
  _ <- liftIO (initConfigs ps)
  log "-> Done!"

logger :: Text -> IO ()
logger = putStrLn . unpack

main :: IO ()
main = do
  env <- flip AppEnv logger . MailgunSigningKey . pack <$> getEnv "MAILGUN_SIGNING_KEY"
  _ <- appMToIO env (initAll processors)
  _ <- logger "Starting server"
  runServer processors env
