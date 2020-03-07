module GraphQL.Types where

import Prelude

import Control.Monad.Except (Except)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign as F
import Simple.JSON (class ReadForeign)
import Type.Data.Row (RProxy)

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

type GqlRow ty row = ( __typename :: Typename ty | row ) :: # Type

type GqlRec ty row = Record (GqlRow ty row) :: Type

data GqlOperationType = Query | Mutation

data GqlOperation input ty row =
    Op GqlOperationType String (RProxy (GqlRow ty row)) (Record input)

{-
data GqlExecOperation input ty row =
    Query String (Proxy (GqlRec ty row)) (Record input)
  | Mutation String (Proxy (GqlRec ty row)) (Record input)

class 
  (IsSymbol ty
  ) <= GqlOperation
  op input ty row | op -> input ty row where
  opRep   :: op -> Proxy (GqlRec ty row)
  opType  :: op -> Typename ty
  opName  :: op -> String
  opInput :: op -> Record input

instance gqlExecOperationImpl ::
  GqlOperation
  (GqlExecOperation input ty row)
  input ty row where
  opRep = snd . snd
  opType = snd
  opName = snd
  opInput = snd
-}

data GqlError = GqlParserError (NonEmptyList String)
              | GqlEmptyLink

derive instance genericGqlError :: Generic GqlError _
instance eqGqlError :: Eq GqlError where eq = genericEq
instance showGqlError :: Show GqlError where show = genericShow

type GqlRes ty row = { data :: Maybe (GqlRec ty row) }
