module Test.GraphQL.LinkSpec where

import GraphQL.Link
import Pipes.Core
import Prelude

import Control.Monad.Except (ExceptT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import GraphQL.Builder (class GqlBuilder)
import GraphQL.Builder as Builder
import GraphQL.Types (GqlError, GqlOperation(..), GqlOperationType(..), Typename(..))
import Prim.Row as Row
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Util (expectLeft, expectRight)
import Type.Data.Row (RProxy(..))
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Record as Rec

type Rec = { __typename :: Typename "Test", x :: Int, y :: String }
type RecZ = { __typename :: Typename "Test", x :: Int, y :: String, z :: Boolean }

termLink :: forall row
   . GqlBuilder Foreign row
  => GqlTermLink row (ExceptT GqlError Aff)
termLink (Op op name rep input) = do
  res <- Builder.build rep (unsafeToForeign { __typename: "Test", x: 123, y: "abc" })
  _   <- respond { data : Just res }
  pure unit

opLink :: forall row tail m
   . Row.Cons "z" Boolean tail row
  => Row.Lacks "z" tail
  => TypeEquals { z :: Boolean | tail } { | row }
  => GqlLink row tail m
opLink =
  let
    prop = SProxy :: SProxy "z"
    go (Op op name _rep input) = do
      resp <- request $ (Op op name (RProxy :: RProxy tail) input)
      next <- respond $ case resp of
              { data : Nothing } -> { data : Nothing }
              { data : Just d } -> { data : Just (to $ Rec.insert prop true d) }
      go next
  in  go

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

    it "works with intermediry link" do
      res  <- expectRight
              =<< ( execE
                  $ termLink
                  >+> opLink
                  +>> op Query "rec" (Proxy :: Proxy RecZ) { }
                  )
      res `shouldEqual` { data : Just { __typename: Typename "Test", x: 123, y: "abc", z: true } }

