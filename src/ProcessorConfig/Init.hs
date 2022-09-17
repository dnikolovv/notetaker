module ProcessorConfig.Init
  ( initConfigs,
  )
where

import ProcessorConfig.Types (ProcessorConfig (..))
import System.Directory (createDirectoryIfMissing)

initConfigs :: [ProcessorConfig] -> IO ()
initConfigs = mapM_ $ createDirectoryIfMissing True . destinationDirectory
