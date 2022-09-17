{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Http.Server
  ( runServer,
  )
where

import App (AppEnv (..), AppM (runAppM), ProcessingFailure (..))
import Control.Applicative ((<*>))
import Control.Monad.Catch (Exception, SomeException, catches, try)
import Control.Monad.Catch qualified as Exception
import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Http.Handlers (mailgunMessageHandler)
import Log (HasLog, log)
import Mailgun.Types (MailgunEmailBody (..))
import Network.Wai.Handler.Warp (run)
import Note.Process (processNote)
import Note.Types (Note (Note))
import ProcessorConfig.Types (ProcessorConfig)
import Servant (Handler (..), ServerError, ServerT, err400, err406, err500, errBody, (:>))
import Servant qualified as S
import Prelude hiding (log)

type API = "mailgun" :> CreateNoteRoute

type CreateNoteRoute = S.ReqBody '[S.JSON] MailgunEmailBody :> S.PostCreated '[S.JSON] ()

api :: S.Proxy API
api = S.Proxy

server :: (Note -> AppM ()) -> S.ServerT API AppM
server processNote = mailgunMessageHandler processNote

-- | Convert a Processor to a Handler via an error type morphism
toHandler :: AppEnv -> AppM a -> Handler a
toHandler env x =
  let result =
        (Right <$> x)
          `catches` [ handleException @ProcessingFailure handleProcessingFailure,
                      handleException @SomeException handleGeneric
                    ]
   in appToHandler result
  where
    appToHandler :: AppM (Either ServerError a) -> Handler a
    appToHandler = Handler . ExceptT . flip runReaderT env . runAppM

    handleException :: forall ex a. Exception ex => (ex -> ServerError) -> Exception.Handler AppM (Either ServerError a)
    handleException toServerErr = Exception.Handler $ \(exception :: ex) -> do
      -- You can do generic stuff such as logging here
      -- this will be run for all exceptions
      log "Exception!"
      log $ T.pack (show exception)
      pure $ Left (toServerErr exception)

    handleProcessingFailure :: ProcessingFailure -> ServerError
    handleProcessingFailure (InvalidNote e) = err400 {errBody = "invalid note: " <> (fromStrict . encodeUtf8 $ e)}
    handleProcessingFailure IndexTemplateParseFailure = err500 {errBody = "failed to parse index template"}
    handleProcessingFailure IndexCreationFailure = err500 {errBody = "index creation failure"}
    handleProcessingFailure NoMatchingProcessor = err406 {errBody = "no matching processor"}

    handleGeneric :: SomeException -> ServerError
    handleGeneric _ =
      -- This will be called for all not explicitly handled exceptions, e.g. IO
      -- Maybe you want to return the exception in development
      -- vs hide it in production, etc.
      err500

app :: [ProcessorConfig] -> AppEnv -> S.Application
app processosConfig e =
  S.serve api $
    S.hoistServer
      api
      (toHandler e)
      (server (processNote processosConfig))

runServer :: [ProcessorConfig] -> AppEnv -> IO ()
runServer ps e = run 8080 $ app ps e