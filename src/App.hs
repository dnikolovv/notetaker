{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module App
  ( AppM (..),
    appMToIO,
    AppEnv (..),
    Has (..),
    MailgunSigningKey (..),
    ProcessingFailure (..),
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import Data.Generics.Internal.VL.Lens (view)
import Data.Generics.Product.Typed (HasType (typed))
import Data.Text (Text)
import GHC.Generics (Generic)
import Log (HasLog (..), Log, Logger (log))

newtype AppM a = AppM {runAppM :: ReaderT AppEnv IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader AppEnv
    )
  deriving anyclass
    ( Has MailgunSigningKey,
      Has Log
    )

appMToIO :: AppEnv -> AppM a -> IO a
appMToIO env = flip runReaderT env . runAppM

instance Logger AppM where
  log msg = getThe @Log >>= \log -> liftIO (log msg)

newtype MailgunSigningKey = MailgunSigningKey Text
  deriving (Show)

data AppEnv = AppEnv
  { _mailgunSigningKey :: MailgunSigningKey,
    _log :: Log
  }
  deriving (Generic)

class Has thing m where
  getThe :: m thing
  default getThe :: HasType thing env => MonadReader env m => m thing
  getThe = asks (view typed)

-- Used by the test suite
instance (HasType thing env) => Has thing (ReaderT env IO) where
  getThe = asks (view typed)

data ProcessingFailure
  = InvalidNote Text
  | IndexCreationFailure
  | IndexTemplateParseFailure
  | NoMatchingProcessor
  deriving (Show, Eq)

instance Exception ProcessingFailure