module Main where

import Http.Server (runServer)
import UserData.Processors (processors)
import Processor.Process (initConfigs)
import Control.Monad.Trans.Except (runExceptT)

printErr :: IOError -> IO ()
printErr = putStrLn . (++ "Error configuring processors: ") . show

main :: IO ()
main = do
  putStrLn $  "Starting notetaker with "
           ++ (show . length $ processors)
           ++ " processor(s)."
  putStrLn "-> Initialising processors"
  runExceptT (initConfigs processors)
    >>= either printErr (const . putStrLn $ "-> Done!")

  putStrLn "-> Starting server on http://localhost:8080"
  runServer processors
