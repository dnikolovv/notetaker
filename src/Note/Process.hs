module Note.Process
  ( processNote,
  )
where

import App (ProcessingFailure (..), ProcessorM)
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

processNote :: [ProcessorConfig] -> Note -> ProcessorM ()
processNote configs note =
  case filter ((== noteRecipient note) . incomingAddress) configs of
    [] -> ExceptT . return . Left $ NoMatchingProcessor
    (config : _) -> processWithConfig config note

processWithConfig :: ProcessorConfig -> Note -> ProcessorM ()
processWithConfig config note = do
  rootDir <-
    liftIO getCurrentDirectory <&> \d ->
      d </> destinationDirectory config
  template <-
    withExceptT (const IndexTemplateParseFailure) $
      compileIndexFile (indexTemplate config)
  liftIO $ writeNote note (rootDir </> createNoteName config note)
  writeIndexFile rootDir (indexFile config) template
