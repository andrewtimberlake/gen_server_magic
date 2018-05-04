# Used by "mix format"
[
  inputs: ["mix.exs", "{config,lib,test}/**/*.{ex,exs}"],
  export: [
    locals_without_parens: [
      defcall: 2,
      defcast: 2,
      defget: 2,
      definifo: 3,
      definit: 2,
      defp: 2,
      defserver: 1,
      defterminate: 3,
      defupdate: 2
    ]
  ]
]
