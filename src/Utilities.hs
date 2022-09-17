{-# LANGUAGE LambdaCase #-}

module Utilities where

import Control.Monad.Catch (Exception, MonadThrow (throwM))

liftEither ::
  Exception e =>
  MonadThrow m =>
  Either e a ->
  m a
liftEither = \case
  Right a -> pure a
  Left err -> throwM err