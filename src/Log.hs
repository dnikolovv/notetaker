{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Log
  ( Logger (..),
    Log,
    HasLog (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Text (Text)
import Prelude hiding (log)

-- | General logging function type signature
type Log = Text -> IO ()

-- You'd probably be better off using an existing logging library
-- such as katip or monad-logger
class Logger m where
  log :: Text -> m ()

-- | Captures any type which has a logging function in it
class HasLog a where
  getLog :: a -> Log

instance HasLog Log where
  getLog = id
