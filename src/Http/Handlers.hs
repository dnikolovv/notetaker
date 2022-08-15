{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}

module Http.Handlers (
  ToHandler (..)
, mailgunMessageHandler
, MailgunEmailBody (..)
) where

import Control.Monad.Except (withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Either (either)
import Data.Functor ((<&>))

import Data.Aeson (FromJSON (..), (.:), withObject, Object)
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Servant (Handler (..), err400, err500, err404, errBody)

import Processor.Errors (ProcessingFailure (..))
import Processor.Note (Note (..))
import Processor.Types (EmailAddress, Processor)

-- | Defines a structure which can be converted into a Servant request handler.
class Monad m => ToHandler m a where
  toHandler :: m a -> Handler a

-- | The request body structure of a request from the Mailgun routing hook.
data MailgunEmailBody = MailgunEmailBody {
  _recipient :: EmailAddress
, _from :: EmailAddress
, _subject :: Text
, _bodyPlain :: Text
, _attachments :: [ByteString]
} deriving Show

instance FromJSON MailgunEmailBody where
  parseJSON = withObject "Body" (\v ->
                MailgunEmailBody <$> v .: "recipient"
                                 <*> v .: "from"
                                 <*> v .: "subject"
                                 <*> v .: "body-plain"
                                 <*> ((v .: "attachment-count") >>= \c -> extractAttachments 1 c v))
    where extractAttachments :: Int -> Int -> Object -> Parser [ByteString]
          extractAttachments _ 0 _ = return []
          extractAttachments c r v = do
            x <- v .: fromString ("attachment-" ++ show c)
            xs <- extractAttachments (c + 1) (r - 1) v
            return (pack x : xs)

instance ToHandler Processor a where
  toHandler = Handler . withExceptT handleError
                where handleError InvalidNote = err400 { errBody = "invalid note" }
                      handleError FileAccessFailure = err500 { errBody = "file access failure" }
                      handleError IndexTemplateParseFailure = err500 { errBody = "failed to parse index template" }
                      handleError IndexCreationFailure = err500 { errBody = "index creation failure" }
                      handleError NoMatchingProcessor = err404 { errBody = "no matching processor" }

-- | Handles incoming messages from the Mailgun routing hook.
mailgunMessageHandler :: (Note -> Processor ()) -> MailgunEmailBody -> Handler ()
mailgunMessageHandler f b =
    liftIO (makeNoteFromBody b) >>= toHandler . f
  where
    makeNoteFromBody b = getCurrentTime <&> \t ->
      Note (_from b)
        (_recipient b)
        t
        (_subject b)
        (_bodyPlain b)
        (_attachments b)
