module Processor.Types (
  EmailAddress
, Processor
) where

import Data.Text (Text)
import Control.Monad.Except (ExceptT (..), withExceptT)
import Processor.Errors (ProcessingFailure (..))
import Servant (Handler (Handler), err400, err500, err404, errBody)

type EmailAddress = Text
type Processor = ExceptT ProcessingFailure IO
