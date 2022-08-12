{-| Processor.Config contains the types which hold processor configurations.
 - These are used to determine how to handle incoming emails.
 -}

module Processor.Config (
  ProcessorConfig (..)
) where

import Processor.Note (Note)
import Processor.Types (EmailAddress)
import Processor.IndexFile (IndexTemplate)

data ProcessorConfig = ProcessorConfig {
  -- | On which email address should this processor listen?
  incomingAddress :: EmailAddress
  -- | Where should incoming notes be stored?
, destinationDirectory :: FilePath
  -- | How should we name the files?
, createNoteName :: Note -> FilePath
  -- | Which template should we use to generate the index file?
, indexTemplate :: IndexTemplate
  -- | Where is the index file located?
, indexFile :: FilePath
}
