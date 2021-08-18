{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "transformers"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
