{-# LANGUAGE OverloadedStrings #-}

module Http.Handlers (
  mailgunMessageHandler
, MailgunEmailBody (..)
, MailgunSigningKey
, validateMessage
, validationWindowInSeconds
, ValidationFailure (..)
) where

import Prelude hiding (log)

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), withExceptT, MonadError (throwError))
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift)
import App (MailgunSigningKey (..), HasMailgunSigningKey (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (either)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Functor ((<&>))

import Crypto.MAC.HMAC (hmac, HMAC (HMAC, hmacGetDigest))
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson (FromJSON (..), (.:), withObject, Object)
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime, UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Servant (Handler (..), err400, err500, err406, errBody)

import Processor.Errors (ProcessingFailure (..))
import Processor.Note (Note (..))
import Processor.Types (EmailAddress, Processor)
import Crypto.Hash (digestFromByteString)
import Data.ByteArray (Bytes (..))
import Data.ByteArray.Encoding (convertFromBase, Base (Base16))

import Log (log, HasLog)

-- | The request body structure of a request from the Mailgun routing hook.
data MailgunEmailBody = MailgunEmailBody {
  _recipient :: EmailAddress
, _from :: EmailAddress
, _subject :: Text
, _bodyPlain :: Text
, _attachments :: [BS.ByteString]
, _timestamp :: Int  -- ^ Number of seconds passed since January 1, 1970.
, _token :: Text     -- ^ Randomly generated string with length 50.
, _signature :: Text -- ^ String with hexadecimal digits generate by HMAC algorithm.
} deriving Show

instance FromJSON MailgunEmailBody where
  parseJSON = withObject "Body" (\v ->
                MailgunEmailBody <$> v .: "recipient"
                                 <*> v .: "from"
                                 <*> v .: "subject"
                                 <*> v .: "body-plain"
                                 <*> ((v .: "attachment-count") >>= \c -> extractAttachments 1 c v)
                                 <*> v .: "timestamp"
                                 <*> v .: "token"
                                 <*> v .: "signature")
    where extractAttachments :: Int -> Int -> Object -> Parser [BS.ByteString]
          extractAttachments _ 0 _ = return []
          extractAttachments c r v = do
            x <- v .: fromString ("attachment-" ++ show c)
            xs <- extractAttachments (c + 1) (r - 1) v
            return (BS.pack x : xs)

-- | Handles incoming messages from the Mailgun routing hook, validating the
-- message according to the Mailgun webhook security documentation.
mailgunMessageHandler :: ( HasLog e , HasMailgunSigningKey e)
                      => (Note -> Processor ())
                      -> MailgunEmailBody
                      -> ReaderT e Processor ()
mailgunMessageHandler f b = do
    valid <- validateMessage b
    note  <- liftIO $ makeNoteFromBody b
    case valid of
      Left e -> do
        log $ "REJECT: " <> show e
        throwError InvalidNote
      Right _ -> lift . f $ note
  where
    makeNoteFromBody b = getCurrentTime <&> \t ->
      Note (_from b)
        (_recipient b)
        t
        (_subject b)
        (_bodyPlain b)
        (_attachments b)


-- | Different reasons the message validation might fail.
data ValidationFailure = CodeMatchFailure  -- ^ the digest does not match the signature
                       | DigestFailure     -- ^ it was not possible to generate a digest
                       | TimeMatchFailure  -- ^ the timestamp does not fall within the window
  deriving (Show, Eq)

-- | The maximum difference between the message signing time and now
validationWindowInSeconds = 600 -- ^ 10 minutes

{-| Validates a message from Mailgun using the following rules:
  1. SHA256 HMAC digest of the (timestamp + token) together should equal the signature
  2. the timestamp should fall within the last 10 minutes
  To compute the HMAC digest, we use the Mailgun signing key from the environment.
  See https://documentation.mailgun.com/en/latest/user_manual.html#webhooks-1.
-}
validateMessage :: (MonadReader e m, MonadIO m, HasMailgunSigningKey e)
                => MailgunEmailBody -> m (Either ValidationFailure Bool)
validateMessage m = do
  (MailgunSigningKey k) <- ask <&> getMailgunSigningKey
  let code = hmacGetDigest (hmac (encodeUtf8 k)
                  (encodeUtf8 $ (pack . show . _timestamp $ m) <> _token m)
                  :: HMAC SHA256)
  now <- liftIO getPOSIXTime <&> floor . nominalDiffTimeToSeconds
  return $ do
    sig <- mapLeft (const DigestFailure) . convertFromBase Base16 . encodeUtf8 . _signature $ m :: Either ValidationFailure Bytes
    digest <- maybeToRight DigestFailure $ digestFromByteString sig
    unless (digest == code) (Left CodeMatchFailure)
    unless (abs (now - _timestamp m) < validationWindowInSeconds) $ Left TimeMatchFailure
    Right True
