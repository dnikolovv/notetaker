module IndexFile.WriteToFile
  ( writeIndexFile,
  )
where

import App (ProcessorM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Functor ((<&>))
import qualified Data.Text.IO as TextIO
import IndexFile.Generate (generateIndexContent)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import Text.Mustache.Types (Template)

writeIndexFile ::
  FilePath ->
  FilePath ->
  Template ->
  ProcessorM ()
writeIndexFile rootDir filename template = do
  allNotes <- liftIO $ listDirectory rootDir <&> filter (/= filename)
  except (generateIndexContent template allNotes)
    >>= liftIO . TextIO.writeFile (rootDir </> filename)
