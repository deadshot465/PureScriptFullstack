{ name = "my-project"
, dependencies = [ "console", "effect", "halogen", "halogen-css", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
