{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}

module Processor.Types (
  EmailAddress
, Processor
) where

import Data.Text (Text)
import Control.Monad.Except (ExceptT (..), withExceptT)
import Processor.Errors (ProcessingFailure (..))
import Http.Handlers (ToHandler (..))
import Servant (Handler (Handler), err400, err500, err404, errBody)

type EmailAddress = Text
type Processor = ExceptT ProcessingFailure IO

instance ToHandler Processor a where
  toHandler = Handler . withExceptT handleError
                where handleError InvalidNote = err400 { errBody = "invalid note" }
                      handleError FileAccessFailure = err500 { errBody = "file access failure" }
                      handleError IndexTemplateParseFailure = err500 { errBody = "failed to parse index template" }
                      handleError IndexCreationFailure = err500 { errBody = "index creation failure" }
                      handleError NoMatchingProcessor = err404 { errBody = "no matching processor" }
