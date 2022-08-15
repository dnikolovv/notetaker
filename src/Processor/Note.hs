{-| Processor.Note contains all the logic for generating notes, including file
 - naming and moving into place.
 -}

module Processor.Note (
  Note (..)
) where

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

import Processor.Types (EmailAddress)

-- | A note data record representing all the properties of a note.
data Note = Note {
  noteSender :: EmailAddress
, noteRecipient :: EmailAddress
, noteReceivedTime :: UTCTime
, noteSubject :: Text
, noteContents :: Text
, noteAttachments :: [ByteString]
} deriving (Show)
