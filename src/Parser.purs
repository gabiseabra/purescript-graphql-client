module GraphQL.Parser where

import Prelude

import GraphQL.Types
import Control.Monad.Except (withExcept)
import Data.Symbol (class IsSymbol)
import Foreign (F, Foreign, renderForeignError)
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Simple.JSON (class ReadForeignFields, getFields)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

class GqlResult
  res
  (proxy :: Type)
  (rec :: # Type) | proxy -> rec where
  parseRecord :: proxy -> res -> Gql (Builder {} {|rec})

instance gqlResultFromRec ::
  ( ListToRow rep rep'
  , RowToList rep' rep
  , GqlResult res (RLProxy rep) rec
  ) => GqlResult res (Proxy (Record rep')) rec where
  parseRecord _ = parseRecord (RLProxy :: RLProxy rep)

instance gqlResultFromRow ::
  ( ListToRow rep rep'
  , RowToList rep' rep
  , GqlResult res (RLProxy rep) rec
  ) => GqlResult res (RProxy rep') rec where
  parseRecord _ = parseRecord (RLProxy :: RLProxy rep)

foreignParserError :: forall a. F a -> Gql a
foreignParserError = withExcept (GqlParserError <<< map renderForeignError)

instance gqlResultForeign ::
  ( ReadForeignFields rep () rec
  ) => GqlResult Foreign (RLProxy rep) rec where
  parseRecord proxy res = foreignParserError $ getFields proxy res
