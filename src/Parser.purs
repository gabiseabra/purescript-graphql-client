module GraphQL.Parser where

import GraphQL.Types
import Prelude

import Control.Monad.Except (withExcept)
import Foreign (F, Foreign, renderForeignError)
import Prim.RowList (class RowToList, kind RowList)
import Record.Builder (Builder)
import Simple.JSON (class ReadForeignFields, getFields)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy)
import Type.RowList (class ListToRow)

class RLByProxy proxy (rep :: RowList) | proxy -> rep

instance rlByProxy ::
  ( ListToRow rep rep'
  , RowToList rep' rep
  ) => RLByProxy (Proxy (Record rep')) rep

instance rlByRProxy ::
  ( ListToRow rep rep'
  , RowToList rep' rep
  ) => RLByProxy (RProxy rep') rep

instance rlByRLProxy :: RLByProxy (RLProxy rep) rep

class GqlResult
  res
  (proxy :: Type)
  (rec :: # Type) | proxy -> rec where
  parseRecord :: proxy -> res -> Gql (Builder {} {|rec})

foreignParserError :: forall a. F a -> Gql a
foreignParserError = withExcept (GqlParserError <<< map renderForeignError)

instance gqlResultForeign ::
  ( ReadForeignFields rep () rec
  , RLByProxy proxy rep
  ) => GqlResult Foreign proxy rec where
  parseRecord proxy res =
      foreignParserError
    $ getFields (RLProxy :: RLProxy rep) res
