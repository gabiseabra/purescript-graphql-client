module Test.GraphQL.ProxySpec where

import GraphQL.Proxy
import Prelude

import GraphQL.Types (GqlRow)
import Type.Data.Row (RProxy(..))

{-}
rproxy :: forall (row :: # Type) . Rep row
rproxy = RProxy :: RProxy row

gqlproxy :: forall ty row . Rep (GqlRow ty row)
gqlproxy = RProxy :: RProxy (GqlRow ty row)