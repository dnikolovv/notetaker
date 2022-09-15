module ProcessorConfig.Types (
  ProcessorConfig (..)
) where

import Note.Types (Note, EmailAddress)

-- | Represents the configuration parameters for a note processor.
data ProcessorConfig = ProcessorConfig {
  -- | On which email address should this processor listen?
  incomingAddress :: EmailAddress
  -- | Where should incoming notes be stored?
, destinationDirectory :: FilePath
  -- | How should we name the files?
, createNoteName :: Note -> FilePath
  -- | Which template should we use to generate the index file?
, indexTemplate :: FilePath
  -- | Where is the index file located?
, indexFile :: FilePath
}
