# Agent Prompt: Implement Minimal BeamLang Compiler in Elixir

## Objective

Implement a basic BeamLang compiler in Elixir that:
1. Parses a minimal BeamLang program.
2. Compiles it to runnable code (Elixir or BEAM).
3. Executes `main/0` and prints the returned value.

This prompt MUST follow the BeamLang design document in `beamlang_design.md`:
- `snake_case` identifiers
- `fn` for function definitions
- `return` for explicit returns
- `match/case` reserved for future matching support
- BEAM-first mindset
- `Result`/`Option` as language primitives (out of scope for MVP)

## Minimal Language Specification (MVP)

### Supported Syntax

```rust
fn main() -> i32 {
    return 42;
}
```

### Required Language Features

1. **Function Declaration**
   - Keyword: `fn`
   - Function name: identifier (snake_case)
   - Parameters: empty `()`
   - Return type annotation: `-> i32`
   - Body: `{ ... }`

2. **Return Statement**
   - Keyword: `return`
   - Expression: integer literal
   - Statement terminator: `;`

3. **Types**
   - `i32` only

4. **Literals**
   - Integer literals, including negatives (e.g., `-5`)

## Compiler Pipeline

Implement the following stages, each as a separate module:

### Phase 1: Lexer (Tokenizer)
**Goal**: Convert source text to tokens

**Input**:
```rust
fn main() -> i32 {
    return 42;
}
```

**Output**:
```elixir
[
  {:fn, "fn"},
  {:identifier, "main"},
  {:lparen, "("},
  {:rparen, ")"},
  {:arrow, "->"},
  {:type, "i32"},
  {:lbrace, "{"},
  {:return, "return"},
  {:integer, 42},
  {:semicolon, ";"},
  {:rbrace, "}"}
]
```

**Tasks**:
- Define token types for keywords, identifiers, symbols, and literals.
- Handle whitespace and newlines.
- Recognize keywords: `fn`, `return`.
- Recognize symbols: `(`, `)`, `{`, `}`, `->`, `;`.
- Recognize integer literals and identifiers.

### Phase 2: Parser (AST Builder)
**Goal**: Convert tokens to AST

**Output**:
```elixir
{:function,
  %{
    name: "main",
    params: [],
    return_type: :i32,
    body: {:block, [
      {:return, {:integer, 42}}
    ]}
  }
}
```

**Tasks**:
- Parse function declarations.
- Parse return statements.
- Parse integer literals.
- Report syntax errors with line/column info.

### Phase 3: Semantic Analysis
**Goal**: Validate AST

**Tasks**:
- Ensure function name is `main` for MVP.
- Ensure `return` exists.
- Ensure return type matches declared type (`i32`).

### Phase 4: Code Generation
**Goal**: Produce runnable output

Choose **one** approach:

#### Option A: Generate Elixir Code (Recommended for MVP)
**Output**:
```elixir
defmodule BeamLangProgram do
  def main() do
    42
  end
end
```

**Tasks**:
- Convert AST to Elixir AST or code string.
- Compile with `Code.compile_string/1`.

#### Option B: Generate BEAM via Erlang Abstract Format (Advanced)
- Build Erlang abstract forms.
- Compile using `:compile.forms/2`.
- Load and execute module.

### Phase 5: Runtime Execution
**Goal**: Execute `main/0` and print output

**Tasks**:
- Load compiled module.
- Call `main/0`.
- Print returned value with `IO.puts/1`.

### Phase 6: CLI Integration
**Goal**: Simple CLI for compiling a file

**Tasks**:
- Accept `.bl` file path.
- Read source.
- Run lexer → parser → semantic → codegen → execute.
- Print errors with file position.

## Project Structure

```
beamlang/
├── mix.exs
├── lib/
│   ├── beamlang.ex
│   ├── lexer.ex
│   ├── parser.ex
│   ├── semantic.ex
│   ├── codegen.ex
│   └── runtime.ex
├── test/
│   ├── lexer_test.exs
│   ├── parser_test.exs
│   └── integration_test.exs
└── examples/
    └── hello.bl
```

## Success Criteria

1. Parses the MVP program.
2. Generates runnable code.
3. Executes `main/0`.
4. Prints the returned integer.
5. Provides syntax error diagnostics.
6. Includes unit tests.
7. CLI works via `mix run lib/beamlang.ex examples/hello.bl`.

## Example Tests

### Test 1: Basic Return
```rust
fn main() -> i32 {
    return 42;
}
```
Expected output: `42`

### Test 2: Zero
```rust
fn main() -> i32 {
    return 0;
}
```
Expected output: `0`

### Test 3: Negative
```rust
fn main() -> i32 {
    return -100;
}
```
Expected output: `-100`

### Test 4: Missing Semicolon
```rust
fn main() -> i32 {
    return 42
}
```
Expected: parse error with location.

## Constraints

- Elixir standard library only.
- Follow BeamLang syntax and naming conventions.
- Keep compiler stages clean and extensible.
- Add concise comments in complex sections.

## Deliverables

1. Working Elixir compiler for MVP.
2. Tests for lexer, parser, and integration.
3. README with usage, limitations, and roadmap.
4. Example `.bl` program.

---

Now implement this minimal compiler following `beamlang_design.md`.
