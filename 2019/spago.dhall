{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "day01"
, dependencies =
    [ "console"
    , "effect"
    , "foldable-traversable"
    , "newtype"
    , "node-fs"
    , "ordered-collections"
    , "prelude"
    , "psci-support"
    , "strings"
    , "unordered-collections"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
