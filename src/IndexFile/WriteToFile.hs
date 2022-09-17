module IndexFile.WriteToFile
  ( writeIndexFile,
  )
where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Functor ((<&>))
import qualified Data.Text.IO as TextIO
import IndexFile.Generate (generateIndexContent)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import Text.Mustache.Types (Template)
import Control.Monad.Catch (MonadThrow(..))

writeIndexFile ::
  FilePath ->
  FilePath ->
  Template ->
  AppM ()
writeIndexFile rootDir filename template = do
  allNotes <- liftIO $ listDirectory rootDir <&> filter (/= filename)
  case generateIndexContent template allNotes of
    Right contents -> liftIO . TextIO.writeFile (rootDir </> filename) $ contents
    Left err -> throwM err
