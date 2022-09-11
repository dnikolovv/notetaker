{-# LANGUAGE LambdaCase #-}

module Main where
import System.Environment (getArgs)

import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Crypto.MAC.HMAC (hmac, HMAC (..))
import Crypto.Hash.Algorithms (SHA256)

getNowInSeconds :: IO Integer
getNowInSeconds = getPOSIXTime <&> floor . nominalDiffTimeToSeconds

printUsage :: IO ()
printUsage = do
  putStrLn "Generate a signature from a signing key and a token"
  putStrLn "Prints the timestamp and signature separated by \":\""
  putStrLn "Usage: gensig key token"
  putStrLn "key: signing key for the message"
  putStrLn "token: the token to use to sign the message"

doSigGen :: Text -> Text -> IO ()
doSigGen key token = do
    now <- getNowInSeconds
    let signature = hmacGetDigest (hmac (encodeUtf8 key)
                                        (encodeUtf8 $ (pack . show $ now) <> token)
                                        :: HMAC SHA256)
    putStrLn $ show now <> ":" <> show signature


main :: IO ()
main = do
  getArgs >>= \case
    [key, token] -> doSigGen (pack key) (pack token)
    _                 -> printUsage
