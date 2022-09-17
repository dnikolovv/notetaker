module Note.Process
  ( processNote,
  )
where

import App (ProcessingFailure (..), AppM)
import Control.Monad.Except (ExceptT (ExceptT), withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import IndexFile.Generate (compileIndexFile)
import IndexFile.WriteToFile (writeIndexFile)
import Note.Types (Note (noteRecipient))
import Note.WriteToFile (writeNote)
import ProcessorConfig.Types (ProcessorConfig (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath.Posix ((</>))
import Control.Monad.Catch (MonadThrow(..))

processNote :: [ProcessorConfig] -> Note -> AppM ()
processNote configs note =
  case filter ((== noteRecipient note) . incomingAddress) configs of
    [] -> throwM NoMatchingProcessor
    (config : _) -> processWithConfig config note

processWithConfig :: ProcessorConfig -> Note -> AppM ()
processWithConfig config note = do
  rootDir <-
    liftIO getCurrentDirectory <&> \d ->
      d </> destinationDirectory config
  template <- compileIndexFile (indexTemplate config)
  writeNote note (rootDir </> createNoteName config note)
  writeIndexFile rootDir (indexFile config) template
