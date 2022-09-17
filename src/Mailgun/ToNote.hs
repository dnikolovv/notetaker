{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Mailgun.ToNote
  ( mkNote,
  )
where

import Data.Functor ((<&>))
import Data.Time.Clock (UTCTime)
import Mailgun.Types
  ( MailgunEmailBody (..),
    MailgunMessage,
    MessageState (Validated),
    messageBody,
  )
import Note.Types (Note (..))

mkNote :: UTCTime -> MailgunMessage 'Validated -> Note
mkNote t (messageBody -> b) =
  Note
    (_from b)
    (_recipient b)
    t
    (_subject b)
    (_bodyPlain b)
    (_attachments b)
