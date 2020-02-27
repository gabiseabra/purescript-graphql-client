{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-graphql-proxy-client"
, dependencies =
  [ "affjax"
  , "console"
  , "effect"
  , "psci-support"
  , "read"
  , "record"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
