module IndexFile.WriteToFile (
  writeIndexFile
) where

-- * Domain specific imports
import App (ProcessorM)
import IndexFile.Generate (generateIndexContent)

-- * Control Structures
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Functor ((<&>))

-- * Data types and IO
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>))
import Text.Mustache.Types (Template)
import qualified Data.Text.IO as TextIO

writeIndexFile :: FilePath
               -> FilePath
               -> Template
               -> ProcessorM ()
writeIndexFile rootDir filename template = do
  allNotes <- liftIO $ listDirectory rootDir <&> filter (/= filename)
  except (generateIndexContent template allNotes) >>=
    liftIO . TextIO.writeFile (rootDir </> filename)
