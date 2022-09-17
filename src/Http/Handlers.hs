{-# LANGUAGE OverloadedStrings #-}

module Http.Handlers
  ( mailgunMessageHandler,
  )
where

import App (AppM, ProcessingFailure (..))
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Log (HasLog, log)
import Mailgun.ToNote (mkNote)
import Mailgun.Types (MailgunEmailBody (..), MailgunMessage (..), MessageState (..))
import Mailgun.Validate (explainValidationError, validateMessage)
import Note.Types (Note)
import Prelude hiding (log)

-- | Handles incoming messages from the Mailgun routing hook, validating the
-- message according to the Mailgun webhook security documentation.
mailgunMessageHandler ::
  (Note -> AppM ()) ->
  MailgunEmailBody ->
  AppM ()
mailgunMessageHandler processNote message = do
  log $
    "Received message: " <> (_from message)
      <> " -> "
      <> (_recipient message)
      <> " ["
      <> (_subject message)
      <> "]"

  valid <- validateMessage message
  case valid of
    Left e -> do
      log $ "REJECT: " <> T.pack (show e)
      throwM . InvalidNote . explainValidationError $ e
    Right validated -> do
      now <- liftIO getCurrentTime
      processNote . mkNote now $ validated
