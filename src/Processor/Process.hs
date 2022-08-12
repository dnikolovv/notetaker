module Processor.Process (
  processNote
, initConfigs
) where

import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)
import Data.Functor ((<&>))
import Data.Either.Combinators (mapBoth)

import qualified Data.Text.IO as TextIO

import Processor.Config (ProcessorConfig (..))
import Processor.Note (Note (..), NoteProcessor)
import Processor.Errors (ProcessingFailure (..))
import Processor.IndexFile (generateIndexContent, compileIndexFile)

import System.Directory (listDirectory, createDirectoryIfMissing, getCurrentDirectory)

type Processor = ExceptT ProcessingFailure IO

useConfig :: ProcessorConfig -> NoteProcessor
useConfig c n =
    writeNote n >>= \notefile -> do
      writeIndexFile notefile
      return notefile
  where
    writeNote :: NoteProcessor
    writeNote n = do
      cwd <- liftIO getCurrentDirectory
      let file = cwd ++ "/" ++ destinationDirectory c ++ "/" ++ createNoteName c n
      ExceptT $ (try (TextIO.writeFile file . noteContents $ n) :: IO (Either IOError ()))
        <&> mapBoth (const FileAccessFailure) (const file)

    writeIndexFile :: FilePath -> Processor ()
    writeIndexFile n = do
      cwd <- liftIO getCurrentDirectory
      noteFiles <- liftIO $ listDirectory ("./" ++ destinationDirectory c)
                      <&> filter (/= indexFile c)
      template <- withExceptT (const IndexTemplateParseFailure)
                    $ compileIndexFile (indexTemplate c)
      let content = generateIndexContent template noteFiles
          indexFP = cwd ++ "/" ++ destinationDirectory c ++ "/" ++ indexFile c
      liftIO . putStrLn $ "writing index file to " ++ indexFP ++ " with notes " ++ show noteFiles
      ExceptT $ (try (TextIO.writeFile indexFP content) :: IO (Either IOError ()))
        <&> mapBoth (const IndexCreationFailure) (const ())

initConfigs :: [ProcessorConfig] -> ExceptT IOError IO [()]
initConfigs = mapM initConfig
  where initConfig = ExceptT . try
                     . createDirectoryIfMissing True
                     . destinationDirectory

processNote :: [ProcessorConfig] -> NoteProcessor
processNote ps n = let xs = filter ((== noteRecipient n) . incomingAddress) ps
                    in (case xs of
                        []    -> ExceptT . return . Left $ NoMatchingProcessor
                        (x:_) -> useConfig x n)
