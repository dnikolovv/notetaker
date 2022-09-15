{-# LANGUAGE OverloadedStrings #-}

module Http.Handlers (
  mailgunMessageHandler
) where

import Prelude hiding (log)

-- * Domain specific imports
import App (AppM, ProcessorM, ProcessingFailure (..), HasMailgunSigningKey (..))
import Log (log, HasLog)
import Mailgun.ToNote (mkNote)
import Mailgun.Types ( MailgunEmailBody , MailgunMessage (..) , MessageState (..))
import Mailgun.Validate ( validateMessage , explainValidationError)
import Note.Types (Note)

-- * Data types
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift)
import Data.Time.Clock (getCurrentTime)

-- | Handles incoming messages from the Mailgun routing hook, validating the
-- message according to the Mailgun webhook security documentation.
mailgunMessageHandler :: (HasLog e , HasMailgunSigningKey e)
                      => (Note -> ProcessorM ())
                      -> MailgunEmailBody
                      -> AppM e ()
mailgunMessageHandler processNote message = do
    valid <- validateMessage message
    case valid of
      Left e -> do
        log $ "REJECT: " <> show e
        throwError . InvalidNote . explainValidationError $ e
      Right validated -> do
        now <- liftIO getCurrentTime
        lift . processNote . mkNote now $ validated
