module GraphQL.Types where

import Control.Monad.Except (Except)
import Data.List.NonEmpty (NonEmptyList)

data GqlError = GqlParserError (NonEmptyList String)

type Gql a = Except GqlError a
