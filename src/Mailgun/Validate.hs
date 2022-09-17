{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Mailgun.Validate
  ( validateMessage,
    explainValidationError,
    MessageValidationFailure (..),
  )
where

import App
  ( Has (getThe),
    MailgunSigningKey (..),
  )
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader (ask))
import Crypto.Hash (SHA256, digestFromByteString)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.ByteArray (Bytes)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.Either (either)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Mailgun.Types
  ( MailgunEmailBody (..),
    MailgunMessage (..),
    MessageState (Validated),
  )
import Utilities (liftEither)

-- | Different reasons the message validation might fail.
data MessageValidationFailure
  = CodeMatchFailure
  | DigestFailure
  | TimeMatchFailure
  deriving (Show, Eq)

explainValidationError :: MessageValidationFailure -> Text
explainValidationError = \case
  CodeMatchFailure -> "the digest does not match the signature"
  DigestFailure -> "it was not possible to generate a digest"
  TimeMatchFailure -> "the timestamp does not fall within the required window"

-- | Validates a message from Mailgun using the following rules:
--
--  1. SHA256 HMAC digest of the (timestamp + token) together should equal
--     the signature
--  2. the timestamp should fall within the last 10 minutes
--
--  To compute the HMAC digest, we use the Mailgun signing key from the
--  environment.
--
--  See https://documentation.mailgun.com/en/latest/user_manual.html#webhooks-1.
validateMessage ::
  Has MailgunSigningKey m =>
  MonadIO m =>
  MailgunEmailBody ->
  m (Either MessageValidationFailure (MailgunMessage 'Validated))
validateMessage m = do
  (MailgunSigningKey k) <- getThe @MailgunSigningKey
  let code =
        hmacGetDigest
          ( hmac
              (encodeUtf8 k)
              (encodeUtf8 $ (pack . show . _timestamp $ m) <> _token m) ::
              HMAC SHA256
          )
  now <- liftIO getPOSIXTime <&> floor . nominalDiffTimeToSeconds

  pure $ checkSignature now code
  where
    checkSignature now code = do
      sig <- decodeSig . _signature $ m
      digest <- maybeToRight DigestFailure $ digestFromByteString sig
      unless (digest == code) (Left CodeMatchFailure)
      unless (abs (now - _timestamp m) < validationWindowInSeconds) $ Left TimeMatchFailure
      Right $ ValidatedMessage m

    validationWindowInSeconds = 600 -- 10 minutes
    decodeSig :: Text -> Either MessageValidationFailure Bytes
    decodeSig =
      mapLeft (const DigestFailure)
        . convertFromBase Base16
        . encodeUtf8
