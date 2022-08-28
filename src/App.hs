module App (
  AppEnv (..)
, HasMailgunSigningKey (..)
, HasLog (..)
, MailgunSigningKey (..)
, Logger
) where

import Data.Text (Text)

import Control.Monad.Reader (ReaderT (..), MonadReader (ask))
import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Log (Logger, HasLog (..))

newtype MailgunSigningKey = MailgunSigningKey Text
  deriving Show

data AppEnv = AppEnv {
  _mailgunSigningKey :: MailgunSigningKey
, _log :: Logger
}

class HasMailgunSigningKey env where
  getMailgunSigningKey :: env -> MailgunSigningKey

instance HasMailgunSigningKey MailgunSigningKey where
  getMailgunSigningKey = id

instance HasMailgunSigningKey AppEnv where
  getMailgunSigningKey = _mailgunSigningKey

instance HasLog AppEnv where
  getLog = _log
