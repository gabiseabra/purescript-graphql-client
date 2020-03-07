module GraphQL.Link where

import GraphQL.Parser
import GraphQL.Proxy
import GraphQL.Types
import Pipes.Core
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Error.Control (class ErrorControl, trial)
import Foreign (Foreign)
import Pipes (yield)
import Pipes.Prelude (last)
import Prim.RowList (class RowToList, kind RowList)

{-
type Effect             = Proxy X  () () X
type Producer         b = Proxy X  () () b
type Consumer    a      = Proxy () a  () X
type Pipe        a    b = Proxy () a  () b

type Server        b' b = Proxy X  () b' b 
type Client   a' a      = Proxy a' a  () X

Proxy
  ()
-}

type GqlPipeline ty row m = Producer (GqlRes ty row) m Unit

type GqlOpLink ty row row' =
  forall input m
  .  GqlOperation input ty row'
  -> Proxy
     (GqlOperation input ty row')
     (GqlRes ty row')
     (GqlOperation input ty row)
     (GqlRes ty row)
     m Unit

{-
type GqlResLink row =
  forall e ty row'
  .  GqlBuilder e ty row'
  -> Proxy ()
-}

type GqlTermLink ty row m =
  forall input
  . MonadThrow GqlError m
  => GqlOperation input ty row
  -> Server (GqlOperation input ty row) (GqlRes ty row) m Unit

exec :: forall ty row m
  .  MonadThrow GqlError m
  => GqlPipeline ty row m
  -> m (GqlRes ty row)
exec pipeline = do
  res' <- last pipeline
  case res' of
    Nothing    -> throwError GqlEmptyLink
    (Just res) -> pure res

execE :: forall ty row m m'
  .  ErrorControl m m' GqlError
  => MonadThrow GqlError m
  => Monad m'
  => GqlPipeline ty row m
  -> m' (Either GqlError (GqlRes ty row))
execE = trial <<< exec

op :: forall proxy input ty row m
  .  MonadThrow GqlError m
  => Rep proxy (GqlRow ty row)
  => GqlOperationType
  -> String
  -> proxy
  -> Record input
  -> Proxy
    (GqlOperation input ty row)
    (GqlRes ty row)
    Unit
    (GqlRes ty row)
    m Unit
op o name rep input = yield =<< (request $ Op o name (toRProxy rep) input)
