module GraphQL.Link where

import GraphQL.Proxy
import GraphQL.Types
import Pipes.Core
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Error.Control (class ErrorControl, trial)
import Pipes (yield)
import Pipes.Prelude (last)

type GqlPipeline row m = Producer (GqlRes row) m Unit

type GqlLink row row' m =
  forall input r
  . MonadThrow GqlError m
  => GqlOperation input row
  -> Proxy
     (GqlOperation input row')  -- Transformed operation sent upstream
     (GqlRes row')              -- Response from server
     (GqlOperation input row)   -- Original operation
     (GqlRes row)               -- Final result sent to client
     m r

type GqlTermLink row m =
  forall input
  . MonadThrow GqlError m
  => GqlOperation input row
  -> Server (GqlOperation input row) (GqlRes row) m Unit

exec :: forall row m
  .  MonadThrow GqlError m
  => GqlPipeline row m
  -> m (GqlRes row)
exec pipeline = do
  res' <- last pipeline
  case res' of
    Nothing    -> throwError GqlEmptyLink
    (Just res) -> pure res

execE :: forall row m m'
  .  ErrorControl m m' GqlError
  => MonadThrow GqlError m
  => Monad m'
  => GqlPipeline row m
  -> m' (Either GqlError (GqlRes row))
execE = trial <<< exec

op :: forall proxy input row m
  .  MonadThrow GqlError m
  => Rep proxy row
  => GqlOperationType
  -> String
  -> proxy
  -> Record input
  -> Proxy
    (GqlOperation input row)
    (GqlRes row)
    Unit
    (GqlRes row)
    m Unit
op o name rep input = yield =<< (request $ Op o name (toRProxy rep) input)
