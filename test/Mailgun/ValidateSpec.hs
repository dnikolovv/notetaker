{-# LANGUAGE OverloadedStrings #-}

module Mailgun.ValidateSpec
  ( spec,
  )
where

import App (MailgunSigningKey (..))
import Control.Monad.Reader (runReaderT)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Mailgun.Types (MailgunEmailBody (..), MailgunMessage (..))
import Mailgun.Validate (MessageValidationFailure (..), validateMessage)
import Test.Hspec

getNowInSeconds = getPOSIXTime <&> floor . nominalDiffTimeToSeconds

constructMessage =
  MailgunEmailBody
    "random@address.com"
    "random@address.com"
    "Some Cool Thing"
    "Hello,\nThis is a message!"
    []

spec :: Spec
spec = describe "mailgun message validation" $ do
  let key = "SIGNING_KEY"
      token = "helloworld123"

  it "fails with CodeMatchFailure if the signature is incorrect" $ do
    now <- getNowInSeconds
    let signature =
          hmacGetDigest
            ( hmac
                (encodeUtf8 key)
                (encodeUtf8 "wrong code") ::
                HMAC SHA256
            )
        eml = constructMessage now token (pack . show $ signature)
    res <- runReaderT (validateMessage eml) (MailgunSigningKey key)
    res `shouldBe` Left CodeMatchFailure

  it "fails with CodeMatchFailure if the message is signed with the wrong key" $ do
    now <- getNowInSeconds
    let signature =
          hmacGetDigest
            ( hmac
                (encodeUtf8 "WRONG_KEY")
                (encodeUtf8 $ (pack . show $ now) <> token) ::
                HMAC SHA256
            )
        eml = constructMessage now token (pack . show $ signature)
    res <- runReaderT (validateMessage eml) (MailgunSigningKey key)
    res `shouldBe` Left CodeMatchFailure

  it "fails with TimeMatchFailure if the timestamp is more than 10 minutes old" $ do
    notQuiteNow <- getNowInSeconds <&> \s -> s - 1200 -- twenty minutes > ten minutes
    let signature =
          hmacGetDigest
            ( hmac
                (encodeUtf8 key)
                (encodeUtf8 $ (pack . show $ notQuiteNow) <> token) ::
                HMAC SHA256
            )
        eml = constructMessage notQuiteNow token (pack . show $ signature)
    res <- runReaderT (validateMessage eml) (MailgunSigningKey key)
    res `shouldBe` Left TimeMatchFailure

  it "fails with DigestFailure if the signature is the wrong format" $ do
    now <- getNowInSeconds
    let signature = "invalid signature"
        eml = constructMessage now token (pack signature)
    res <- runReaderT (validateMessage eml) (MailgunSigningKey key)
    res `shouldBe` Left DigestFailure

  it "produces a ValidatedMessage if the message is valid" $ do
    now <- getNowInSeconds
    let signature =
          hmacGetDigest
            ( hmac
                (encodeUtf8 key)
                (encodeUtf8 $ (pack . show $ now) <> token) ::
                HMAC SHA256
            )
        eml = constructMessage now token (pack . show $ signature)
    res <- runReaderT (validateMessage eml) (MailgunSigningKey key)
    res `shouldBe` Right (ValidatedMessage eml)
