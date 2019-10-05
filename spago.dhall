{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "stringutils"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "either"
    , "functions"
    , "integers"
    , "lists"
    , "maybe"
    , "partial"
    , "prelude"
    , "psci-support"
    , "strings"
    , "strongcheck"
    , "unsafe-coerce"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
