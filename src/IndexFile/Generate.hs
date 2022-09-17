{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IndexFile.Generate
  ( compileIndexFile,
    generateIndexContent,
  )
where

import App (ProcessingFailure (..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Text (Text)
import System.Directory (getCurrentDirectory)
import Text.Mustache (Template, object, (~>))
import Text.Mustache.Compile (automaticCompile)
import Text.Mustache.Render (checkedSubstituteValue)
import Text.Parsec (ParseError)
import Control.Monad.Catch (MonadThrow (throwM))

-- | Compile index file data from a template in "./templates" using
-- "./templates/partials" for partials.
compileIndexFile :: MonadIO m => MonadThrow m => FilePath -> m Template
compileIndexFile inputFile = do
  cwd <- liftIO getCurrentDirectory
  liftIO $
    automaticCompile
      [cwd ++ "/templates/partials"]
      (cwd ++ "/templates/" ++ inputFile) >>= \case
    Right template -> pure template
    Left err -> throwM IndexTemplateParseFailure

-- | Generate index content from index file data and a list of notes
generateIndexContent :: Template -> [FilePath] -> Either ProcessingFailure Text
generateIndexContent t notes =
  let v = object ["notes" ~> map (\x -> object ["path" ~> x]) notes]
   in case checkedSubstituteValue t v of
        ([], t) -> Right t
        (e, _) -> Left IndexCreationFailure
