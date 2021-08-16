{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "const"
  , "control"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "tailrec"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
