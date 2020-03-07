module GraphQL.Parser where

import GraphQL.Types
import GraphQL.Proxy
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (lift, runExcept, runExceptT, withExcept)
import Data.Either (Either(..), either)
import Foreign (F, Foreign, renderForeignError)
import Prim.RowList (class RowToList, kind RowList)
import Record.Builder (Builder)
import Simple.JSON (class ReadForeignFields, getFields)
import Type.Data.RowList (RLProxy(..))

class GqlBuilder
  res
  (row :: # Type) where
  gqlBuilder ::
       forall proxy m
     . MonadThrow GqlError m
    => Rep proxy row
    => proxy
    -> res
    -> m (Builder {} {|row})

foreignParserError :: forall m a. MonadThrow GqlError m => F a -> m a
foreignParserError =
  either (throwError <<< GqlParserError <<< map renderForeignError) pure <<< runExcept

instance gqlBuilderForeign ::
  ( ReadForeignFields rl () row
  , RowToList row rl
  ) => GqlBuilder Foreign row where
  gqlBuilder proxy res =
      foreignParserError
    $ getFields (RLProxy :: RLProxy rl) res
