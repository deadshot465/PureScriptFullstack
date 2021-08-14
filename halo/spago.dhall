{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "const"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
