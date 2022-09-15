{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Mailgun.ToNote (
  mkNote
) where

-- * Domain specific imports
import Mailgun.Types (
    MailgunMessage
  , MessageState (Validated)
  , MailgunEmailBody (..)
  , messageBody
  )
import Processor.Note (Note (..))

-- * Control structures
import Data.Functor ((<&>))

-- * Data types
import Data.Time.Clock (UTCTime)

mkNote :: UTCTime -> MailgunMessage 'Validated -> Note
mkNote t (messageBody -> b) =
  Note
    (_from b)
    (_recipient b)
    t
    (_subject b)
    (_bodyPlain b)
    (_attachments b)
