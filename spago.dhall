{ name =
    "run-profunctor-lenses"
, dependencies =
    [ "effect", "console", "psci-support", "run", "profunctor-lenses" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
