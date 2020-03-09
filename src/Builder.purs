module GraphQL.Builder where

import GraphQL.Types
import GraphQL.Proxy
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Foreign (F, Foreign, renderForeignError)
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (class ReadForeignFields, getFields)
import Type.Data.RowList (RLProxy(..))


build ::
     forall res row proxy m
   . MonadThrow GqlError m
  => Rep proxy row
  => GqlBuilder res row
  => proxy
  -> res
  -> m {|row}
build rep res = Builder.build <$> gqlBuilder rep res <@> {}

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
