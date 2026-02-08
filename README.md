# BeamLang

**A statically typed language that compiles to the BEAM.**  
Functions, generics, optionals, results, modules, and a standard library written in BeamLang.

```beamlang
import math.{Pair, add};

fn main(args: [String]) -> number {
    let pair: Pair = { left = 2, right = 3 };
    return add(pair->left, pair->right);
}
```

---

## Features

| Area | What you get |
|------|----------------|
| **Types** | Primitives, structs, generics, `T?` / `Optional<T>`, `Ok!Err` / `Result<Ok,Err>` |
| **Functions** | Named functions, lambdas, `internal` (module-private), method-style `->` on structs |
| **Modules** | One module per `.bl` file, `export` / `import`, namespace `math::add` |
| **Control flow** | `if`, `match` (exhaustive), `for` over `List<T>` / `Range` |
| **Stdlib** | Implemented in BeamLang under `stdlib/` — core (auto-imported): `math`, `string`, `list`, `optional`, `result`, `range`, `map`, `set`, `vec2`; ext (require import): `args`, `system`, `network` |

---

## Quick start

**Requirements:** Elixir 1.14+

```bash
# Run a program
mix beamlang run examples/basics/hello.bl

# With debug (print AST, don't run)
mix beamlang run --print-ast --no-run examples/basics/hello.bl

# Build a standalone binary
mix beamlang compile examples/basics/hello.bl
./examples/basics/hello

# Or use the helper script (after mix escript.build)
bin/beamlang run examples/basics/hello.bl
```

**Tests:**

```bash
mix test
```

---

## Editor support

- **VS Code:** [beamlang-vscode](beamlang-vscode/) — syntax, LSP (diagnostics, hover, go-to definition, completion).

**LSP (any editor):**

```bash
mix beamlang --lsp
```

Or build the escript and run it:

```bash
mix escript.build
./beamlang --lsp
```

Debug LSP logs: `./beamlang --lsp --lsp-debug`

---

## Examples

| Category | Examples |
|----------|----------|
| **Language tour** | `examples/language_tour.bl` |
| **Hello world** | `examples/basics/hello.bl` |
| **Modules** | `examples/modules/use_math.bl`, `examples/modules/math_ops.bl`, `examples/modules/stdlib_methods.bl` |
| **Apps** | `examples/apps/cat.bl`, `examples/apps/standalone.bl` |
| **LSP** | `examples/lsp/lsp_features.bl` |

---

## Design & docs

- **[beamlang_design.md](beamlang_design.md)** — language spec, types, operators, stdlib, and implementation notes.

---

## License

See repository for license details.
