{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- This module provides a webserver with endpoints to which notes can be pushed.
 - Currently only a mailgun endpoint is provided. This takes the subject and
 - content of the message and processes the input adding the received date and
 - running the relevant processor.
 -}

module Http.Server
  ( runServer,
  )
where

import App (AppEnv (..), HasMailgunSigningKey (getMailgunSigningKey))
import Control.Applicative ((<*>))
import Control.Monad.Except (withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Http.Handlers (MailgunEmailBody (..), MailgunSigningKey, mailgunMessageHandler)
import Log (HasLog, log)
import Network.Wai.Handler.Warp (run)
import Processor.Config (ProcessorConfig)
import Processor.Errors
import Processor.Note (Note)
import Processor.Process (processNote)
import Processor.Types (Processor)
import Servant (Handler (..), ServerT, err400, err406, err500, errBody, (:>))
import qualified Servant as S
import Prelude hiding (log)

type CreateNoteRoute = S.ReqBody '[S.JSON] MailgunEmailBody :> S.PostCreated '[S.JSON] ()

type API = "mailgun" :> CreateNoteRoute

api :: S.Proxy API
api = S.Proxy

type AppM e = ReaderT e Processor

type ProcessorFactory = Note -> Processor ()

-- | Convert a Processor to a Handler via an error type morphism
toHandler :: Processor a -> Handler a
toHandler = Handler . withExceptT handleError
  where
    handleError (InvalidNote err) = err400 {errBody = "invalid note: " <> fromStrict (encodeUtf8 (explainValidationFailure err))}
    handleError FileAccessFailure = err500 {errBody = "file access failure"}
    handleError IndexTemplateParseFailure = err500 {errBody = "failed to parse index template"}
    handleError IndexCreationFailure = err500 {errBody = "index creation failure"}
    handleError NoMatchingProcessor = err406 {errBody = "no matching processor"}

genHandlers ::
  (HasLog e, HasMailgunSigningKey e) =>
  ProcessorFactory ->
  S.ServerT API (AppM e)
genHandlers pf = createNote
  where
    createNote email = do
      log $
        "Received message: " <> (unpack . _from $ email)
          <> " -> "
          <> (unpack . _recipient $ email)
          <> " ["
          <> (unpack . _subject $ email)
          <> "]"
      mailgunMessageHandler pf email

app :: ProcessorFactory -> AppEnv -> S.Application
app pf e = S.serve api $ S.hoistServer api (nt e) $ genHandlers pf
  where
    nt :: e -> AppM e a -> S.Handler a
    nt e x = toHandler $ runReaderT x e

runServer :: [ProcessorConfig] -> AppEnv -> IO ()
runServer ps e = run 8080 $ app (processNote ps) e
