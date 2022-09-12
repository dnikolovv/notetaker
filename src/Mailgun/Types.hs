{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailgun.Types
  ( MailgunMessage,
    MessageState (..),
    MailgunEmailBody (..),
    validateMessage,
    messageBody,
  )
where

import App (HasMailgunSigningKey (getMailgunSigningKey), MailgunSigningKey (MailgunSigningKey))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (ask))
import Crypto.Hash (SHA256, digestFromByteString)
import Crypto.MAC.HMAC
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.ByteArray (Bytes)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import qualified Data.ByteString.Char8 as BS
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Processor.Errors (NoteValidationFailure (..))
import Processor.Types (EmailAddress)

data MessageState
  = Raw
  | Validated

data MailgunMessage (st :: MessageState) where
  ValidatedMessage :: MailgunEmailBody -> MailgunMessage 'Validated
  RawMessage :: MailgunEmailBody -> MailgunMessage 'Raw

messageBody :: MailgunMessage any -> MailgunEmailBody
messageBody = \case
  ValidatedMessage b -> b
  RawMessage b -> b

-- | Validates a message from Mailgun using the following rules:
--  1. SHA256 HMAC digest of the (timestamp + token) together should equal the signature
--  2. the timestamp should fall within the last 10 minutes
--  To compute the HMAC digest, we use the Mailgun signing key from the environment.
--  See https://documentation.mailgun.com/en/latest/user_manual.html#webhooks-1.
validateMessage ::
  (MonadReader e m, MonadIO m, HasMailgunSigningKey e) =>
  MailgunEmailBody ->
  m (Either NoteValidationFailure (MailgunMessage 'Validated))
validateMessage m = do
  (MailgunSigningKey k) <- ask <&> getMailgunSigningKey
  let code =
        hmacGetDigest
          ( hmac
              (encodeUtf8 k)
              (encodeUtf8 $ (pack . show . _timestamp $ m) <> _token m) ::
              HMAC SHA256
          )
  now <- liftIO getPOSIXTime <&> floor . nominalDiffTimeToSeconds
  return $ do
    sig <- mapLeft (const DigestFailure) . convertFromBase Base16 . encodeUtf8 . _signature $ m :: Either NoteValidationFailure Bytes
    digest <- maybeToRight DigestFailure $ digestFromByteString sig
    unless (digest == code) (Left CodeMatchFailure)
    unless (abs (now - _timestamp m) < validationWindowInSeconds) $ Left TimeMatchFailure
    Right $ ValidatedMessage m
  where
    -- The maximum difference between the message signing time and now
    validationWindowInSeconds = 600 -- 10 minutes

-- | The request body structure of a request from the Mailgun routing hook.
data MailgunEmailBody = MailgunEmailBody
  { _recipient :: EmailAddress,
    _from :: EmailAddress,
    _subject :: Text,
    _bodyPlain :: Text,
    _attachments :: [BS.ByteString],
    -- | Number of seconds passed since January 1, 1970.
    _timestamp :: Int,
    -- | Randomly generated string with length 50.
    _token :: Text,
    -- | String with hexadecimal digits generate by HMAC algorithm.
    _signature :: Text
  }
  deriving (Show)

instance FromJSON MailgunEmailBody where
  parseJSON =
    withObject
      "Body"
      ( \v ->
          MailgunEmailBody <$> v .: "recipient"
            <*> v .: "from"
            <*> v .: "subject"
            <*> v .: "body-plain"
            <*> ((v .: "attachment-count") >>= \c -> extractAttachments 1 c v)
            <*> v .: "timestamp"
            <*> v .: "token"
            <*> v .: "signature"
      )
    where
      extractAttachments :: Int -> Int -> Object -> Parser [BS.ByteString]
      extractAttachments _ 0 _ = return []
      extractAttachments c r v = do
        x <- v .: fromString ("attachment-" ++ show c)
        xs <- extractAttachments (c + 1) (r - 1) v
        return (BS.pack x : xs)