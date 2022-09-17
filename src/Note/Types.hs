module Note.Types
  ( Note (..),
    EmailAddress,
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

type EmailAddress = Text

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
