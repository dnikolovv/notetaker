{-# LANGUAGE OverloadedStrings #-}
{-| All processor configurations should be linked here. -}

module UserData.Processors (
  processors
) where

import Processor.Config (ProcessorConfig (..))
import Processor.Note (Note (..))
import Processor.IndexFile (IndexTemplate)
import Data.Text (unpack)

quickNoteProcessor = ProcessorConfig {
  incomingAddress = "quicknote@notetaker.gtf.io"
, destinationDirectory = "notebook"
, createNoteName = (++ ".wiki") . unpack . noteSubject
, indexTemplate = "DefaultIndexTemplate.mustache"
, indexFile = "index.wiki"
}

processors = [quickNoteProcessor]
