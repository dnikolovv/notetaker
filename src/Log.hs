{-# LANGUAGE FlexibleInstances #-}

module Log
  ( Logger,
    HasLog (..),
    log,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Prelude hiding (log)

-- | General logging function type signature
type Logger = String -> IO ()

-- | Captures any type which has a logging function in it
class HasLog a where
  getLog :: a -> Logger

instance HasLog Logger where
  getLog = id

-- | Convenience function to call a logger within the right monadic context
log :: (MonadReader env m, MonadIO m, HasLog env) => String -> m ()
log msg = ask >>= \e -> liftIO . getLog e $ msg
