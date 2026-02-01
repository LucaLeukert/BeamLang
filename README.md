# BeamLang Compiler (Elixir)

This repo contains the BeamLang compiler and standard library. BeamLang is a statically typed language that compiles to BEAM.

## Highlights

- Functions, lambdas, and expression-based blocks
- Struct types with function-valued fields (method syntax via `->`)
- Generics for types and functions
- Optional (`T?`, `?some`, `?none`) and Result (`Ok!Err`, `!ok`, `!err`)
- Modules with `export`/`import` and namespace access (`math::add`)
- Iterators for `for` loops (`Iterator<T>`)
- Standard library implemented in `.bl` files under `stdlib/`

## Run

```sh
mix beamlang examples/hello.bl
```

With debug output:

```sh
mix beamlang examples/hello.bl --print-ast --no-run
```

Or use the helper script:

```sh
bin/beamlang examples/hello.bl --print-ast --no-run
```

Start the language server:

```sh
mix beamlang --lsp
```

For editor integration, build the escript once and point the editor to it:

```sh
mix escript.build
./beamlang --lsp
```

## Tests

```sh
mix test
```

## Examples

See `examples/` for runnable programs, including:

- `examples/hello.bl`
- `examples/generic_fn.bl`
- `examples/optional.bl`
- `examples/result.bl`
- `examples/iterator_methods.bl`
- `examples/lsp/lsp_features.bl`

## Design Docs

- `beamlang_design.md` (original design)
- `beamlang_design_current.md` (current, implemented language)
