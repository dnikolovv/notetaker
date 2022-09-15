{-# LANGUAGE LambdaCase #-}

module Note.WriteToFile (
  writeNote
) where

-- * Domain specific imports
import Note.Types (Note (..))

-- * File writing functions etc.
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), takeExtension, dropExtension)
import qualified Data.Text.IO as TextIO

-- | Writes a note to a file, adding a `(n)` suffix in case the file
-- already exists. This does not capture potential IO errors, leaving
-- them to bubble higher up.
writeNote :: Note -> FilePath -> IO FilePath
writeNote note path = do
  file <- liftIO . getNonconflictingPathForFile $ path
  TextIO.writeFile file . noteContents $ note
  return file

getNonconflictingPathForFile :: FilePath -> IO FilePath
getNonconflictingPathForFile path = do
  let basename = dropExtension path
      extension = takeExtension path
  findGoodPath basename extension 0
  where findGoodPath b e i =
          let newPath = fname b e i
           in doesFileExist newPath >>= \case
              True -> findGoodPath b e (i + 1)
              False -> return newPath
        fname b e 0 = b ++ e
        fname b e i = b ++ " (" ++ show i ++ ")" ++ e
