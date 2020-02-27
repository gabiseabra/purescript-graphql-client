module Main where

import Prelude

import Data.String.Read (read)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Rec
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

data Typename (typename :: Symbol)

type GqlRow ty rec = (__typename :: Typename ty | rec) :: # Type
type GqlRec ty rec = Record (GqlRow ty rec) :: Type

-- data Gql (ty :: Symbol) (rec :: # Type) = Gql (Record (GqlRow ty rec))

class GqlType (rec :: # Type) (ty :: Symbol) | rec -> ty

instance gqlRow :: GqlType ty rec
-- instance gqlRec :: (TypeEquals (Record rec) (GqlRec ty rec)) => GqlType ty rec

class
  (GqlType rec ty) <= GqlQueryable
  (i :: # Type)
  (rec :: # Type)
  (ty :: Symbol) | rec -> ty where
  query :: forall proxy m 
    .  proxy rec
    -> String
    -> Record i
    -> m Record rec

instance queryable :: 
  ( RowToList rec recRL
  , ListToRow recRL rec
  , TypeEquals (RLProxy recRL) (RLProxy (GqlRow ty recRL))
  ) => GqlQueryable rec i where
  query _ _ _ = buildRecord (RLProxy :: recRL) 

class GqlBuilder
  (recRL :: RL.RowList)
  (resRL :: RL.RowList)
  (rec :: # Type)
  (res :: # Type) | res -> rec where
  buildRecord :: forall proxy m 
    .  proxy recRL
    -> proxy resRL
    -> Record res
    -> m Record rec

{-
class GqlBuilder (rec :: RL.RowList) (res :: # Type) where
  buildQlRec :: forall m
    .  RLProxy rec
    -> Record res
    -> m Record rec

instance gqlBuilderNil ::
  ( IsSymbol name
  , GqlBuilder tail res
  , R.Cons name ty res r
  ) => GqlBuilder RL.Nil res where
  buildQlRec _ res = pure {}

instance gqlBuilderCons ::
  ( IsSymbol name
  , GqlBuilder tail res
  , R.Cons name ty res r
  ) => GqlBuilder (RL.Cons name ty tail) res where
  buildQlRec _ res =
    let nameP = SProxy :: SProxy name
        value = Rec.get nameP res
        tail = buildQlRec (RLProxy :: RLProxy tail) res
    in Rec.insert nameP value <$> tail
-}
--------------------------------------------------------------------------------

type Picture =
  { __typename :: Typename "Person"
  , id  :: Number
  , url :: String
  }

type PictureInput = { fileName :: String }

-- picture :: forall m. PictureInput -> m Picture
-- picture = query (Proxy :: Proxy Picture) "picture"