module Test.Spec.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Effect.Aff (Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)

expectRight :: forall m e a. MonadThrow Error m => Show e => Either e a -> m a
expectRight (Right a) = pure a
expectRight (Left e) =
  throwError
  $ error
  $ "Expected Right, got " <> show e

expectLeft :: forall m e a. MonadThrow Error m => Either e a -> m e
expectLeft (Right a) =
  throwError
  $ error "Expected Left"
expectLeft (Left e) = pure e
