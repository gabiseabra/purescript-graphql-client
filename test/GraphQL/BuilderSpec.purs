module Test.GraphQL.BuilderSpec where

import Prelude

import Control.Monad.Except (runExcept)
import Data.List.NonEmpty (singleton)
import Foreign (ForeignError(..), renderForeignError, unsafeToForeign)
import GraphQL.Builder as Builder
import GraphQL.Types (GqlError(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Util (expectLeft, expectRight)
import Type.Proxy (Proxy(..))

type Rec = { x :: Int, y :: String}

spec :: Spec Unit
spec = do
  describe "gqlResultFromRow" do
    describe "parseResult" do
      it "parses record from Foreign" do
        res <-  expectRight
            <<< runExcept
            <<< Builder.build (Proxy :: Proxy Rec)
            $ unsafeToForeign { x: 123, y: "abc" }
        res `shouldEqual` { x: 123, y: "abc" }

      it "fails with invalid properties" do
        err <-  expectLeft
            <<< runExcept
            <<< Builder.build (Proxy :: Proxy Rec)
            $ unsafeToForeign { x: 123, y: 123 }
        err `shouldEqual` foreignParseError (ErrorAtProperty "y" (TypeMismatch "String" "Number"))

foreignParseError :: ForeignError -> GqlError
foreignParseError e = GqlParserError (singleton $ renderForeignError e)