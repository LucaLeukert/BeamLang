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
| **Control flow** | `if`, `match` (exhaustive), `for` over `Iterator<T>` |
| **Stdlib** | Implemented in BeamLang under `stdlib/` — `math`, `string`, `list`, `optional`, `result`, `iterator`, `range`, `map`, `set`, `args`, `system`, `network`, `vec2` |

---

## Quick start

**Requirements:** Elixir 1.14+

```bash
# Run a program
mix beamlang run examples/basics/hello.bl

# With debug (print AST, don't run)
mix beamlang run --print-ast --no-run examples/hello.bl

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
| **Basics** | `examples/basics/hello.bl`, `args.bl`, `math.bl` |
| **Types** | `optional.bl`, `result.bl`, `generic.bl`, `enum.bl`, `tuples.bl` |
| **Functions** | `generic_fn.bl`, `lambda.bl`, `pattern_params.bl`, `self_method.bl` |
| **Control flow** | `control_flow.bl`, `loops.bl`, `if_expr.bl`, `guard.bl` |
| **Collections** | `iterator_methods.bl`, `list.bl`, `list_methods.bl`, `range_demo.bl` |
| **Modules** | `use_math.bl`, `stdlib_methods.bl` |
| **Apps** | `examples/apps/` — `cat`, `curl`, `grep`, `ls`, `standalone`, `wc` |
| **LSP** | `examples/lsp/lsp_features.bl`, `hover_types.bl` |

---

## Design & docs

- **[beamlang_design.md](beamlang_design.md)** — language spec, types, operators, stdlib, and implementation notes.

---

## License

See repository for license details.
