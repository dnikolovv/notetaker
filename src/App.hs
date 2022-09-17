module App
  ( AppM,
    ProcessorM,
    AppEnv (..),
    HasMailgunSigningKey (..),
    MailgunSigningKey (..),
    ProcessingFailure (..),
  )
where

import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Data.Text (Text)
import Log (HasLog (..), Logger)

type ProcessorM = ExceptT ProcessingFailure IO

type AppM e = ReaderT e ProcessorM

newtype MailgunSigningKey = MailgunSigningKey Text
  deriving (Show)

data AppEnv = AppEnv
  { _mailgunSigningKey :: MailgunSigningKey,
    _log :: Logger
  }

class HasMailgunSigningKey env where
  getMailgunSigningKey :: env -> MailgunSigningKey

instance HasMailgunSigningKey MailgunSigningKey where
  getMailgunSigningKey = id

instance HasMailgunSigningKey AppEnv where
  getMailgunSigningKey = _mailgunSigningKey

instance HasLog AppEnv where
  getLog = _log

data ProcessingFailure
  = InvalidNote Text
  | FileAccessFailure
  | IndexCreationFailure
  | IndexTemplateParseFailure
  | NoMatchingProcessor
  deriving (Show, Eq)
