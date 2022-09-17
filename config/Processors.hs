{-# LANGUAGE OverloadedStrings #-}

module Processors
  ( processors,
  )
where

import Data.Text (unpack)
import Note.Types (Note (..))
import ProcessorConfig.Types (ProcessorConfig (..))

quickNoteProcessor =
  ProcessorConfig
    { incomingAddress = "quicknote@notetaker.gtf.io",
      destinationDirectory = "notebook",
      createNoteName = (++ ".wiki") . unpack . noteSubject,
      indexTemplate = "DefaultIndexTemplate.mustache",
      indexFile = "index.wiki"
    }

processors = [quickNoteProcessor]
