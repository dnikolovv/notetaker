{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- | Processor.Note contains all the logic for generating notes, including file
-- - naming and moving into place.
module Processor.Note
  ( Note,
    noteSender,
    noteRecipient,
    noteReceivedTime,
    noteSubject,
    noteContents,
    noteAttachments,
    mkNote,
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Mailgun.Types (MailgunEmailBody (..), MailgunMessage, MessageState (Validated), messageBody)
import Processor.Types (EmailAddress)

-- | A note data record representing all the properties of a note.
data Note = Note
  { noteSender :: EmailAddress,
    noteRecipient :: EmailAddress,
    noteReceivedTime :: UTCTime,
    noteSubject :: Text,
    noteContents :: Text,
    noteAttachments :: [ByteString]
  }
  deriving (Show)

mkNote ::
  UTCTime ->
  MailgunMessage 'Validated ->
  Note
mkNote t (messageBody -> b) =
  Note
    (_from b)
    (_recipient b)
    t
    (_subject b)
    (_bodyPlain b)
    (_attachments b)