{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Processor.Errors
  ( ProcessingFailure (..),
    NoteValidationFailure (..),
    explainValidationFailure,
  )
where

import Data.Text (Text)

-- | Different reasons the message validation might fail.
data NoteValidationFailure
  = CodeMatchFailure
  | DigestFailure
  | TimeMatchFailure
  deriving (Show, Eq)

-- You don't need the comments anymore
explainValidationFailure :: NoteValidationFailure -> Text
explainValidationFailure = \case
  CodeMatchFailure -> "the digest does not match the signature"
  DigestFailure -> "it was not possible to generate a digest"
  TimeMatchFailure -> "the timestamp does not fall within the window"

data ProcessingFailure
  = InvalidNote NoteValidationFailure
  | FileAccessFailure
  | IndexCreationFailure
  | IndexTemplateParseFailure
  | NoMatchingProcessor
  deriving (Show, Eq)