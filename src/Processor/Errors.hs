module Processor.Errors (
  ProcessingFailure (..)
) where

data ProcessingFailure =
      InvalidNote
    | FileAccessFailure
    | IndexCreationFailure
    | IndexTemplateParseFailure
    | NoMatchingProcessor
  deriving (Show, Eq)
