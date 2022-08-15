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
import Text.Mustache.Render (checkedSubstituteValue)
import Text.Parsec (ParseError)
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory)

import Processor.Errors (ProcessingFailure (..))

type IndexTemplate = FilePath

-- | Compile index file data from a template in "./templates" using
-- "./templates/partials" for partials.
compileIndexFile :: FilePath -> ExceptT ParseError IO Template
compileIndexFile inputFile = do
    cwd <- liftIO getCurrentDirectory
    ExceptT $ automaticCompile [cwd ++ "/templates/partials"]
                               (cwd ++ "/templates/" ++ inputFile)

-- | Generate index content from index file data and a list of notes
generateIndexContent :: Template -> [FilePath] -> Either ProcessingFailure Text
generateIndexContent t notes =
    let v = object [ "notes" ~> map (\x -> object [ "path" ~> x ]) notes ]
     in case checkedSubstituteValue t v of
      ([], t) -> Right t
      (e, _)  -> Left IndexCreationFailure
