{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "colors"
  , "console"
  , "const"
  , "css"
  , "datetime"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "nonempty"
  , "now"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "transformers"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
