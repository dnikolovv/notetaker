module Processor.Errors (
  ProcessingFailure (..)
) where

import Data.Text (Text)

data ProcessingFailure =
      InvalidNote Text
    | FileAccessFailure
    | IndexCreationFailure
    | IndexTemplateParseFailure
    | NoMatchingProcessor
  deriving (Show, Eq)
