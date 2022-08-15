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

import Servant ((:>))
import qualified Servant as S
import Network.Wai.Handler.Warp (run)

import Http.Handlers (mailgunMessageHandler, MailgunEmailBody)
import Processor.Types (Processor)
import Processor.Note (Note (Note))
import Processor.Process (processNote)

type API = "mailgun" :> S.ReqBody '[S.JSON] MailgunEmailBody
                     :> S.Post '[S.JSON] ()

api :: S.Proxy API
api = S.Proxy

app :: (Note -> Processor ()) -> S.Application
app = S.serve api . mailgunMessageHandler

runServer ps = run 8080 $ app (processNote ps)
