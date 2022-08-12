{-# LANGUAGE OverloadedStrings #-}

{-| Processor.IndexFile contains all the logic for handling the generation of
 - index files, which link to all the notes in a given configuration.
 -}

module Processor.IndexFile (
  IndexTemplate
, compileIndexFile
, generateIndexContent
) where

import Data.Text (Text)
import Text.Mustache (Template, object, (~>))
import Text.Mustache.Compile (automaticCompile)
import Text.Mustache.Render (substituteValue)
import Text.Parsec (ParseError)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory)

type IndexTemplate = FilePath

-- | Compile index file data from a template in "./templates" using
-- "./templates/partials" for partials.
compileIndexFile :: FilePath -> ExceptT ParseError IO Template
compileIndexFile inputFile = do
    cwd <- liftIO getCurrentDirectory
    ExceptT $ automaticCompile [cwd ++ "/templates/partials"]
                               (cwd ++ "/templates/" ++ inputFile)

-- | Generate index content from index file data and a list of notes
generateIndexContent :: Template -> [FilePath] -> Text
generateIndexContent t notes =
    substituteValue t (object [ "notes" ~> map mk notes ])
    where mk n = object [ "path" ~> show n ]
