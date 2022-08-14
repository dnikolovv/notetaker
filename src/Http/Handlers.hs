{-# LANGUAGE MultiParamTypeClasses #-}

module Http.Handlers (
  ToHandler (..)
) where

import Servant (Handler)

class Monad m => ToHandler m a where
  toHandler :: m a -> Handler a
