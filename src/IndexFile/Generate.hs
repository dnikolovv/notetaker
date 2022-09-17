{-# LANGUAGE OverloadedStrings #-}

module IndexFile.Generate
  ( compileIndexFile,
    generateIndexContent,
  )
where

import App (ProcessingFailure (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Text (Text)
import System.Directory (getCurrentDirectory)
import Text.Mustache (Template, object, (~>))
import Text.Mustache.Compile (automaticCompile)
import Text.Mustache.Render (checkedSubstituteValue)
import Text.Parsec (ParseError)

-- | Compile index file data from a template in "./templates" using
-- "./templates/partials" for partials.
compileIndexFile :: FilePath -> ExceptT ParseError IO Template
compileIndexFile inputFile = do
  cwd <- liftIO getCurrentDirectory
  ExceptT $
    automaticCompile
      [cwd ++ "/templates/partials"]
      (cwd ++ "/templates/" ++ inputFile)

-- | Generate index content from index file data and a list of notes
generateIndexContent :: Template -> [FilePath] -> Either ProcessingFailure Text
generateIndexContent t notes =
  let v = object ["notes" ~> map (\x -> object ["path" ~> x]) notes]
   in case checkedSubstituteValue t v of
        ([], t) -> Right t
        (e, _) -> Left IndexCreationFailure
