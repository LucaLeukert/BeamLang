# BeamLang MVP Compiler (Elixir)

This project implements a minimal BeamLang compiler in Elixir. It supports a single function:

```rust
fn main() -> i32 {
    return 42;
}
```

## Run

```sh
mix beamlang examples/hello.bl
```

With debug output:

```sh
mix beamlang -- examples/hello.bl --print-ast --no-run
```

Or use the helper script (preferred for flags):

```sh
bin/beamlang examples/hello.bl --print-ast --no-run
```

## Tests

```sh
mix test
```

## Limitations

- Only `fn main() -> i32 { return <int>; }` is supported.
- No variables, expressions, or additional types.

## Roadmap

- Add `let` bindings and arithmetic expressions.
- Add multiple functions and parameters.
- Add `Result`/`Option` per BeamLang design.
