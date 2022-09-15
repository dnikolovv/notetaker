{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}

{- This module provides a webserver with endpoints to which notes can be pushed.
 - Currently only a mailgun endpoint is provided. This takes the subject and
 - content of the message and processes the input adding the received date and
 - running the relevant processor.
 -}

module Http.Server (
  runServer
) where

import Prelude hiding (log)

import Control.Applicative ((<*>))

import Servant (ServerT, (:>), Handler (..), err400, err406, err500, errBody)
import qualified Servant as S
import Network.Wai.Handler.Warp (run)

import Http.Handlers (mailgunMessageHandler)
import Mailgun.Types (MailgunEmailBody (..))
import Processor.Types (Processor)
import Processor.Note (Note (Note))
import Processor.Process (processNote)
import Processor.Config (ProcessorConfig)
import Processor.Errors

import App (AppEnv (..), HasMailgunSigningKey (getMailgunSigningKey))
import Log (HasLog, log)
import Control.Monad.Reader (ReaderT (..), ask, MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (withExceptT)

import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)

type CreateNoteRoute = S.ReqBody '[S.JSON] MailgunEmailBody :> S.PostCreated '[S.JSON] ()
type API = "mailgun" :> CreateNoteRoute

api :: S.Proxy API
api = S.Proxy

type AppM e = ReaderT e Processor
type ProcessorFactory = Note -> Processor ()

-- | Convert a Processor to a Handler via an error type morphism
toHandler :: Processor a -> Handler a
toHandler = Handler . withExceptT handleError
              where handleError (InvalidNote e) = err400 { errBody = "invalid note: " <> (fromStrict . encodeUtf8 $ e) }
                    handleError FileAccessFailure = err500 { errBody = "file access failure" }
                    handleError IndexTemplateParseFailure = err500 { errBody = "failed to parse index template" }
                    handleError IndexCreationFailure = err500 { errBody = "index creation failure" }
                    handleError NoMatchingProcessor = err406 { errBody = "no matching processor" }

genHandlers :: (HasLog e, HasMailgunSigningKey e)
            => ProcessorFactory -> S.ServerT API (AppM e)
genHandlers pf = createNote
  where createNote email = do
          log $ "Received message: " <> (unpack . _from $ email)
                                     <> " -> "
                                     <> (unpack . _recipient $ email)
                                     <> " ["
                                     <> (unpack . _subject $ email)
                                     <> "]"
          mailgunMessageHandler pf email

app :: ProcessorFactory -> AppEnv -> S.Application
app pf e = S.serve api $ S.hoistServer api (nt e) $ genHandlers pf
  where nt :: e -> AppM e a -> S.Handler a
        nt e x = toHandler $ runReaderT x e

runServer :: [ProcessorConfig] -> AppEnv -> IO ()
runServer ps e = run 8080 $ app (processNote ps) e
