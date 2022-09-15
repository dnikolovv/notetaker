{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mailgun.Types (
  MailgunMessage (..)
, MessageState (..)
, messageBody
, MailgunEmailBody (..)
) where

-- * Domain specific imports
import Processor.Types (EmailAddress)

-- * Data types
import Data.Aeson (FromJSON (..), (.:), withObject, Object)
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS

data MessageState = Raw | Validated
  deriving (Show, Eq)

data MailgunMessage (st :: MessageState) where
  ValidatedMessage :: MailgunEmailBody -> MailgunMessage 'Validated
  RawMessage :: MailgunEmailBody -> MailgunMessage 'Raw

deriving instance Show (MailgunMessage any)
deriving instance Eq (MailgunMessage any)

-- | Get the embedded mailgun email body from inside the message wrapper
messageBody :: MailgunMessage any -> MailgunEmailBody
messageBody = \case
  ValidatedMessage b -> b
  RawMessage b -> b

-- | The request body structure of a request from the Mailgun routing hook.
data MailgunEmailBody = MailgunEmailBody {
  _recipient :: EmailAddress
, _from :: EmailAddress
, _subject :: Text
, _bodyPlain :: Text
, _attachments :: [BS.ByteString]
-- | Number of seconds passed since January 1, 1970.
, _timestamp :: Int
-- | Randomly generated string with length 50.
, _token :: Text
-- | String with hexadecimal digits generate by HMAC algorithm.
, _signature :: Text
} deriving (Show, Eq)

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
