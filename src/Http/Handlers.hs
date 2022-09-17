{-# LANGUAGE OverloadedStrings #-}

module Http.Handlers
  ( mailgunMessageHandler,
  )
where

import App (AppM, HasMailgunSigningKey (..), ProcessingFailure (..), ProcessorM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift)
import Data.Time.Clock (getCurrentTime)
import Log (HasLog, log)
import Mailgun.ToNote (mkNote)
import Mailgun.Types (MailgunEmailBody, MailgunMessage (..), MessageState (..))
import Mailgun.Validate (explainValidationError, validateMessage)
import Note.Types (Note)
import Prelude hiding (log)

-- | Handles incoming messages from the Mailgun routing hook, validating the
-- message according to the Mailgun webhook security documentation.
mailgunMessageHandler ::
  (HasLog e, HasMailgunSigningKey e) =>
  (Note -> ProcessorM ()) ->
  MailgunEmailBody ->
  AppM e ()
mailgunMessageHandler processNote message = do
  valid <- validateMessage message
  case valid of
    Left e -> do
      log $ "REJECT: " <> show e
      throwError . InvalidNote . explainValidationError $ e
    Right validated -> do
      now <- liftIO getCurrentTime
      lift . processNote . mkNote now $ validated
