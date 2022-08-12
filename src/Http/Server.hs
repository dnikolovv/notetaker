{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}

{- This module provides a webserver with endpoints to which notes can be pushed.
 - Currently only a mailgun endpoint is provided. This takes the subject and
 - content of the message and processes the input adding the received date and
 - running the relevant processor.
 -}

module Http.Server (
  runServer
) where

import Control.Applicative ((<*>))

import Data.Aeson (FromJSON (..), (.:), withObject, Object)
import Data.Aeson.Types (Parser)
import Data.Aeson.Key (fromString)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (withExceptT)
import Data.Either (either)
import Data.Time.Clock (getCurrentTime)
import Data.Functor ((<&>))

import Servant ((:>))
import qualified Servant as S
import Network.Wai.Handler.Warp (run)

import Processor.Types (EmailAddress)
import Processor.Note (Note (Note), NoteProcessor)
import Processor.Errors (ProcessingFailure (..))
import Processor.Process (processNote)

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

handleError :: ProcessingFailure -> S.ServerError
handleError InvalidNote = S.err400 { S.errBody = "invalid note" }
handleError FileAccessFailure = S.err500 { S.errBody = "file access failure" }
handleError IndexTemplateParseFailure = S.err500 { S.errBody = "failed to parse index template" }
handleError IndexCreationFailure = S.err500 { S.errBody = "index creation failure" }
handleError NoMatchingProcessor = S.err404 { S.errBody = "no matching processor" }

type MailgunAPI = "mailgun" :> S.ReqBody '[S.JSON] MailgunEmailBody
                            :> S.Post '[S.JSON] FilePath

mailgunAPI :: S.Proxy MailgunAPI
mailgunAPI = S.Proxy

mailgunMessageHandler :: NoteProcessor -> MailgunEmailBody -> S.Handler FilePath
mailgunMessageHandler f b = do
    n <- liftIO (makeNoteFromBody b)
    S.Handler $ withExceptT handleError (f n)
  where
    makeNoteFromBody b = getCurrentTime <&> \t ->
      Note (_from b)
        (_recipient b)
        t
        (_subject b)
        (_bodyPlain b)
        (_attachments b)

makeHandler :: NoteProcessor -> S.Server MailgunAPI
makeHandler = mailgunMessageHandler

app :: NoteProcessor -> S.Application
app = S.serve mailgunAPI . makeHandler

runServer ps = run 8080 $ app (processNote ps)
