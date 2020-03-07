{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-graphql-proxy-client"
, dependencies =
  [ "affjax"
  , "console"
  , "coroutines"
  , "effect"
  , "errorcontrol"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "pipes"
  , "psci-support"
  , "read"
  , "record"
  , "simple-json"
  , "spec"
  , "spec-discovery"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
