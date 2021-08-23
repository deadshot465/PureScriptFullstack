{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "bifunctors"
  , "colors"
  , "console"
  , "const"
  , "css"
  , "datetime"
  , "effect"
  , "either"
  , "foreign-generic"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "nonempty"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "transformers"
  , "uuid"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "../server/src/Data/Api/**/*.purs"
  , "../server/src/Entity/**/*.purs"
  , "test/**/*.purs"
  ]
}
