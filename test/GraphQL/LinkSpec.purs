module Test.GraphQL.LinkSpec where

import GraphQL.Link
import Pipes.Core
import Pipes.Prelude as Pipes
import Prelude

import Control.Monad.Error.Class (class MonadThrow, catchError, throwError)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import Error.Control (class ErrorControl)
import Foreign (Foreign, unsafeToForeign)
import GraphQL.Parser (class GqlBuilder)
import GraphQL.Parser as Parser
import GraphQL.Types (GqlError(..), GqlOperation(..), GqlOperationType(..), GqlRec, GqlRes, GqlRow, Typename(..))
import Record.Builder as Builder
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Util (expectLeft, expectRight)
import Test.Spec.Util (expectLeft, expectRight)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy(..))

type Rec = { __typename :: Typename "Test", x :: Int, y :: String }

termLink :: forall ty row
   . GqlBuilder Foreign (GqlRow ty row)
  => GqlTermLink ty row (ExceptT GqlError Aff)
termLink (Op op name rep input) = do
  res <- Builder.build <$> Parser.gqlBuilder rep (unsafeToForeign { __typename: "Test", x: 123, y: "abc" }) <@> {}
  _   <- respond { data : Just res }
  pure unit

{-
opLink :: forall ty row tail
   . Row.Cons "z" String tail row
  => GqlOpLink ty row tail
opLink (Op op name _rep input) = do
  respond (Op op name (RProxy :: RProxy (GqlRow ty row)) input)
-}

spec :: Spec Unit
spec = do
  describe "Graphql.Link" do
    it "works" do
      res  <- expectRight
              =<< ( execE
                  $ termLink
                  +>> op Query "rec" (Proxy :: Proxy Rec) { }
                  )
      res `shouldEqual` { data : Just { __typename: Typename "Test", x: 123, y: "abc" } }
