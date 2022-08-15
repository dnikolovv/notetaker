{-# LANGUAGE LambdaCase #-}

module Processor.Process (
  processNote
, initConfigs
) where

import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.Trans.Except (except)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)
import Data.Functor ((<&>))
import Data.Either.Combinators (mapBoth)

import qualified Data.Text.IO as TextIO

import Processor.Config (ProcessorConfig (..))
import Processor.Note (Note (..))
import Processor.Errors (ProcessingFailure (..))
import Processor.IndexFile (generateIndexContent, compileIndexFile)
import Processor.Types (Processor)

import System.Directory (listDirectory, createDirectoryIfMissing, getCurrentDirectory, doesFileExist)
import System.FilePath.Posix ((</>), takeExtension, dropExtension)

outputDir = getCurrentDirectory

useConfig :: ProcessorConfig -> Note -> Processor ()
useConfig c n =
    writeNote n >>= \notefile -> do
      writeIndexFile notefile
  where
    writeNote :: Note -> Processor FilePath
    writeNote n = do
      file <- liftIO . getNonconflictingPathForFile $ n
      ExceptT $ (try (TextIO.writeFile file . noteContents $ n) :: IO (Either IOError ()))
        <&> mapBoth (const FileAccessFailure) (const file)

    getNonconflictingPathForFile n = do
      cwd <- outputDir
      let canonical = cwd </> destinationDirectory c </> createNoteName c n
          basename = dropExtension canonical
          extension = takeExtension canonical
      findGoodPath basename extension 0
      where findGoodPath b e i =
              let newPath = fname b e i
               in doesFileExist newPath >>= \case True -> findGoodPath b e (i + 1)
                                                  False -> return newPath
            fname b e 0 = b ++ e
            fname b e i = b ++ " (" ++ show i ++ ")" ++ e

    writeIndexFile :: FilePath -> Processor ()
    writeIndexFile n = do
      cwd <- liftIO getCurrentDirectory
      noteFiles <- liftIO $ listDirectory (cwd </> destinationDirectory c)
                      <&> filter (/= indexFile c)
      template <- withExceptT (const IndexTemplateParseFailure)
                    $ compileIndexFile (indexTemplate c)
      let content = generateIndexContent template noteFiles
          indexFP = cwd </> destinationDirectory c </> indexFile c

      except content >>= \c ->
        ExceptT $ (try (TextIO.writeFile indexFP c) :: IO (Either IOError ()))
        <&> mapBoth (const IndexCreationFailure) (const ())

initConfigs :: [ProcessorConfig] -> ExceptT IOError IO [()]
initConfigs = mapM initConfig
  where initConfig = ExceptT . try
                     . createDirectoryIfMissing True
                     . destinationDirectory

processNote :: [ProcessorConfig] -> Note -> Processor ()
processNote ps n = case filter ((== noteRecipient n) . incomingAddress) ps of
                      []    -> ExceptT . return . Left $ NoMatchingProcessor
                      (x:_) -> useConfig x n
