{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Http.Handlers
  ( mailgunMessageHandler,
    MailgunEmailBody (..),
    MailgunSigningKey,
  )
where

import App (HasMailgunSigningKey (..), MailgunSigningKey (..))
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), MonadError (throwError), withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift)
import Crypto.Hash (digestFromByteString)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC (HMAC, hmacGetDigest), hmac)
import Data.Aeson (FromJSON (..), Object, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.ByteArray (Bytes (..))
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import qualified Data.ByteString.Char8 as BS
import Data.Either (either)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Log (HasLog, log)
import Mailgun.Types (MailgunEmailBody (..), MailgunMessage, MessageState (..), messageBody, validateMessage)
import Processor.Errors (NoteValidationFailure (..), ProcessingFailure (..))
import Processor.Note (Note (..), mkNote)
import Processor.Types (EmailAddress, Processor)
import Servant (Handler (..), err400, err406, err500, errBody)
import Prelude hiding (log)

-- | Handles incoming messages from the Mailgun routing hook, validating the
-- message according to the Mailgun webhook security documentation.
mailgunMessageHandler ::
  (HasLog e, HasMailgunSigningKey e) =>
  (Note -> Processor ()) ->
  MailgunEmailBody ->
  ReaderT e Processor ()
mailgunMessageHandler processNote message = do
  valid <- validateMessage message
  case valid of
    Left e -> do
      log $ "REJECT: " <> show e
      throwError $ InvalidNote e
    Right validated -> do
      now <- liftIO getCurrentTime
      lift . processNote . mkNote now $ validated