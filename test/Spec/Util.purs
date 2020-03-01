module Test.Spec.Util where

import Prelude 
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Effect.Aff (error)
import Effect.Aff.Class (class MonadAff, liftAff)

expectRight :: forall m e a. MonadAff m => Show e => Either e a -> m a
expectRight (Right a) = pure a
expectRight (Left e) =
    liftAff
  $ throwError
  $ error
  $ "Expected Right, got " <> show e

expectLeft :: forall m e a. MonadAff m => Either e a -> m e
expectLeft (Right a) =
    liftAff
  $ throwError
  $ error "Expected Left"
expectLeft (Left e) = pure e
