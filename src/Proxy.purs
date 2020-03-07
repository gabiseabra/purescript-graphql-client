module GraphQL.Proxy where

import Prelude

import Prim.RowList (class RowToList)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy)
import Type.Proxy (Proxy)
import Type.RowList (class ListToRow)

class Rep proxy (row :: # Type) | proxy -> row where
  toRProxy :: proxy -> RProxy row

instance repRProxy :: Rep (RProxy row) row where
  toRProxy = identity

instance repProxy :: Rep (Proxy (Record row)) row where
  toRProxy _ = RProxy

instance repRLProxy :: 
  ( ListToRow rl row
  , RowToList row rl
  ) => Rep (RLProxy rl) row where
  toRProxy _ = RProxy
