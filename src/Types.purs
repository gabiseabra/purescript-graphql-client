module GraphQL.Types where

import Prelude

import Control.Monad.Except (Except)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)

data GqlError = GqlParserError (NonEmptyList String)

type Gql a = Except GqlError a

derive instance genericGqlError :: Generic GqlError _
instance eqGqlError :: Eq GqlError where eq = genericEq
instance showGqlError :: Show GqlError where show = genericShow
