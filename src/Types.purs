module GraphQL.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign as F
import Simple.JSON (class ReadForeign)
import Type.Data.Row (RProxy)
import Prim.Row as Row

data Typename (ty :: Symbol) = Typename String

derive instance eqTypename :: Eq (Typename ty)

instance showTypename :: IsSymbol ty => Show (Typename ty) where
  show _ = reflectSymbol (SProxy :: SProxy ty)

instance readForeignTypename :: IsSymbol ty => ReadForeign (Typename ty) where
  readImpl f = do
    value <- F.readString f
    let typename = reflectSymbol (SProxy :: SProxy ty)
    if typename == value
    then pure $ Typename typename
    else F.fail $ F.TypeMismatch typename value

class GqlRow (ty :: Symbol) (row :: # Type) | row -> ty

instance gqlRowImpl ::
  ( IsSymbol ty
  , Row.Cons "__typename" (Typename ty) tail row
  ) => GqlRow ty row

data GqlOperationType = Query | Mutation

data GqlOperation input row =
    Op GqlOperationType String (RProxy row) (Record input)

data GqlError = GqlParserError (NonEmptyList String)
              | GqlEmptyLink

derive instance genericGqlError :: Generic GqlError _
instance eqGqlError :: Eq GqlError where eq = genericEq
instance showGqlError :: Show GqlError where show = genericShow

type GqlRes row = { data :: Maybe { | row } }
