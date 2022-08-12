{-# LANGUAGE OverloadedStrings #-}

{-| Processor.Note contains all the logic for generating notes, including file
 - naming and moving into place.
 -}

module Processor.Note (
  Note (..)
, NoteProcessor
) where

import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Except (ExceptT)

import Text.Mustache (ToMustache (..), (~>), object)

import Processor.Types (EmailAddress)
import Processor.Errors (ProcessingFailure)

data Note = Note {
  noteSender :: EmailAddress
, noteRecipient :: EmailAddress
, noteReceivedTime :: UTCTime
, noteSubject :: Text
, noteContents :: Text
, noteAttachments :: [ByteString]
} deriving (Show)

-- | A NoteProcessor takes a note and processes it, returning the path of the
-- newly created note, or failing with ProcessingFailure
type NoteProcessor = Note -> ExceptT ProcessingFailure IO FilePath

instance ToMustache Note where
  toMustache n = object [ "sender"      ~> noteSender n
                        , "recipient"   ~> noteRecipient n
                        , "receivedAt"  ~> iso8601Show (noteReceivedTime n)
                        , "subject"     ~> noteSubject n
                        , "contents"    ~> noteContents n
                        , "attachments" ~> (show . length . noteAttachments $ n)
                        ]
