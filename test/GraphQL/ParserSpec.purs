module Test.GraphQL.ParserSpec where

import Prelude

import Control.Monad.Except (runExcept)
import Data.List.NonEmpty (singleton)
import Effect.Class.Console (log)
import Foreign (ForeignError(..), renderForeignError, unsafeToForeign)
import GraphQL.Parser as Parser
import GraphQL.Types (GqlError(..))
import Record.Builder as Builder
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
        builder <-
                expectRight
            <<< runExcept
            <<< Parser.parseRecord (Proxy :: Proxy Rec)
            $ unsafeToForeign { x: 123, y: "abc" }
        Builder.build builder {} `shouldEqual` { x: 123, y: "abc" }

      it "fails with invalid properties" do
        error <-
                expectLeft
            <<< runExcept
            <<< Parser.parseRecord (Proxy :: Proxy Rec)
            $ unsafeToForeign { x: 123, y: 123 }
        error `shouldEqual` foreignParseError (ErrorAtProperty "y" (TypeMismatch "String" "Number"))

foreignParseError :: ForeignError -> GqlError
foreignParseError e = GqlParserError (singleton $ renderForeignError e)