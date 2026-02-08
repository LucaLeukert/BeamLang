# BeamLang Design (January 2026)

This document describes the BeamLang language design: syntax, type system, semantics, and compilation model. For standard library API documentation, see [`docs/stdlib.md`](docs/stdlib.md).

## Overview

BeamLang is a statically-typed language that compiles to BEAM bytecode via Erlang abstract forms. It emphasizes explicit control flow, typed data, and lightweight interop with the runtime through externals.

Internally, all values are represented as struct literals (similar to how everything in JavaScript is an object). This applies to primitives, optionals/results, and user-defined types.

The standard library is written in BeamLang (`.bl` files) and is loaded automatically.

## Tooling

- The CLI can run as a Language Server Protocol (LSP) server using `beamlang --lsp`.
- The CLI includes `beamlang lint` for token-level lint checks (currently includes single-expression `match` branch style).
- The BeamLang VS Code extension launches the CLI LSP server for diagnostics, hover, go-to definition, and completion.

## Files, Modules, Imports

- Each `.bl` file is a module. The module name is the file name without `.bl`.
- `export` exposes functions and types from a module.
- `internal` marks functions that can only be called from within the same module file.
  Exception: stdlib files (`stdlib/core/*`, `stdlib/ext/*`) may call each other's `internal` functions.

Import forms:

```beamlang
import math.add;
import math.{Pair, add};
import math.*;
import math as m;
```

Namespace access:

```beamlang
math::add(1, 2);
```

## Types

### Primitive Types

- `number`
- `String`
- `char`
- `bool`
- `void`
- `any`

### Generic Types

BeamLang supports generic type parameters for type definitions and for function definitions.

Type definitions:

```beamlang
type Pair<T> {
    left: T,
    right: T
}
```

Function definitions:

```beamlang
fn test<T>(opt: T?) -> T {
    return opt->unwrap(0);
}
```

### Optional and Result

- Optional syntax: `T?` (preferred) or `Optional<T>`
- Result syntax: `Ok!Err` (preferred) or `Result<Ok, Err>`

The shorthand forms are preferred in BeamLang code. They are normalized internally.

Optional literals:

```beamlang
?some 10
?none
```

Result literals:

```beamlang
!ok 10
!err "error"
```

### Struct Types

Struct-like types use `type`:

```beamlang
type User {
    name: String,
    age: number
}
```

Struct literals require a type annotation unless the expected type is clear from context:

```beamlang
let user: User = { name = "Ada", age = 30 };
```

### Operator Overloading

Types can define custom behavior for operators using a two-part system:
1. **Type Definition** declares the operator signature with `operator OP: fn(...)` syntax
2. **Struct Literal** binds the implementation function with `operator OP = func_name` syntax

```beamlang
type Path {
    path: String,
    operator /: fn(Path, String) -> Path  // Operator signature declaration
}

fn path_join(self: Path, segment: String) -> Path {
    return { path = self->path + "/" + segment, operator / = path_join };
}

fn path_new(p: String) -> Path {
    return { path = p, operator / = path_join };  // Bind operator implementation
}
```

The type definition declares the operator's type signature, while the struct literal binds it to an implementation. Supported operators:

| Operator | Internal Name | Description        |
|----------|---------------|--------------------|
| `+`      | `__op_add`    | Addition           |
| `-`      | `__op_sub`    | Subtraction        |
| `*`      | `__op_mul`    | Multiplication     |
| `/`      | `__op_div`    | Division           |
| `%`      | `__op_mod`    | Modulo             |
| `==`     | `__op_eq`     | Equality           |
| `!=`     | `__op_neq`    | Inequality         |
| `<`      | `__op_lt`     | Less than          |
| `>`      | `__op_gt`     | Greater than       |
| `<=`     | `__op_lte`    | Less or equal      |
| `>=`     | `__op_gte`    | Greater or equal   |

**Alternative: Naming Convention**

Instead of struct-level binding, you can use the naming convention `TypeName_op_opname`:

```beamlang
type Path {
    path: String
}

fn Path_op_div(self: Path, segment: String) -> Path {
    return { path = self->path + "/" + segment };
}
```

### Internal Fields

Fields can be marked as `internal` to prevent direct access from outside the type's methods. Internal fields can only be accessed within method functions of the same type:

```beamlang
type List<T> {
    internal data: any,
    get: fn(List<T>, number) -> T
}
```

When a field is marked `internal`:
- Direct field access (`list.data`) from outside the type will result in a compile error
- Methods defined on the type can access internal fields via `self` or any other variable of the same type
- This allows methods like `concat(self, other)` to access `other->data` within the method
- This enables encapsulation of implementation details in stdlib types

### Error Types

Error types are special struct types designed to represent error conditions. They are defined using the `error` keyword and are typically used as the error type in `Result<Ok, Err>`:

```beamlang
error FileError {
    path: String,
    message: String
}

error ParseError {
    line: number,
    column: number,
    message: String
}
```

Error types can be exported:

```beamlang
export error IoError {
    kind: String,
    message: String
}
```

Error types are internally converted to regular type definitions with no generic parameters. They can be instantiated using struct literal syntax with type annotation or in a Result error context:

```beamlang
// Using error in a Result (prefer Ok!Err syntax)
fn parse_number(s: String) -> number!ParseError {
    if (/* invalid */) {
        return !err ParseError { line = 1, column = 0, message = "Invalid" };
    }
    return !ok 42;
}

// Pattern matching on error types
let result = parse_number("abc");
match (result) {
    case!ok n => println(n),
    case!err err => println(err.message)
}
```

## Functions

```beamlang
fn add(a: number, b: number) -> number {
    return a + b;
}
```

- `return` is required for returning values.
- Functions can be exported or internal:

```beamlang
export fn add(a: number, b: number) -> number { return a + b; }
internal fn helper() -> void { return; }
```

### Mutable Parameters

By default, function parameters are immutable. Use the `mut` keyword to allow reassignment:

```beamlang
fn increment(mut value: number) -> number {
    value = value + 1;
    return value;
}

fn process(x: number, mut y: number) -> number {
    // x cannot be reassigned
    // y can be reassigned
    y = y + x;
    return y;
}
```

Note: Erlang/BEAM is immutable by design. Mutable parameters are implemented via variable shadowing - each reassignment creates a new binding that shadows the previous one. The original value passed by the caller is never modified.

### Compound Assignment

Mutable variables and mutable struct fields support shorthand assignment operators:

```beamlang
fn adjust(mut value: number) -> number {
    value += 2;
    value -= 1;
    value *= 3;
    value /= 2;
    value %= 4;
    return value;
}
```

Supported forms: `+=`, `-=`, `*=`, `/=`, `%=`.

### Generics in Functions

Generic parameters are declared after the function name:

```beamlang
fn identity<T>(value: T) -> T {
    return value;
}
```

Type parameters are inferred from arguments when possible. Generic functions can be called with explicit type arguments:

```beamlang
let parsed = parse_args<Args>(args);
```

### Lambdas

```beamlang
let add_one = fn(x: number) -> number {
    return x + 1;
};
```

## Methods (Struct Field Functions)

Methods are function-valued fields in structs. Method call syntax uses `->`:

```beamlang
type Box {
    value: number,
    get: fn(Box) -> number
}

fn box_get(self: Box) -> number {
    return self->value;
}

fn main(args: [String]) -> number {
    let b: Box = { value = 7, get = box_get };
    return b->get();
}
```

Rules:

- A method function must have its first parameter named `self`.
- The name `self` is reserved and cannot be used for other bindings (variables, patterns, loop variables, assignments).
- Method calls can be chained: `list_new()->push(1)->push(2)->first()->unwrap(0)`.

## Comments

### Single-line Comments

```beamlang
// This is a single-line comment
let x = 42; // Inline comment
```

### Block Comments

```beamlang
/**
 * This is a block comment.
 * It can span multiple lines.
 **/
fn main(args: [String]) -> number {
    return 0;
}
```

Note: Block comments use `/**` to open and `**/` to close.

## Control Flow

### If Statements

```beamlang
if (cond) {
    ...
} else if (other) {
    ...
} else {
    ...
}
```

### Guard Statements

```beamlang
guard (cond) else {
    return 1;
}
```

The `else` block must end with `return`.

### Loops

```beamlang
while (cond) { ... }
loop { ... break; }
```

### For Loops

`for` loops iterate over `List<T>` and `Range` values.

```beamlang
for (ch in "hi"->chars()) {
    println(ch);
}
```

## Pattern Matching

```beamlang
match (value) {
    case?some x => x,
    case?none => 0
}
```

Struct patterns:

```beamlang
match (user) {
    case User { age, name } => age,
    case _ => 0
}
```

Match guards use `if` with a comparison expression:

```beamlang
match (user) {
    case User { age } if age > 18 => 1,
    case _ => 0
}
```

Use `=>` for single-expression case bodies and `->` for block bodies:

```beamlang
match (value) {
    case true -> { 1; },
    case false => 0
}
```

Match expressions must be exhaustive. Use `case _` or cover all variants (e.g. `case?some` + `case?none`, `case!ok` + `case!err`, `case true` + `case false`).

## Expressions

- Binary operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`
- Compound assignment operators: `+=`, `-=`, `*=`, `/=`, `%=`
- `if` expressions:

```beamlang
let value = if (cond) { 1; } else { 2; };
```

- String literals use double quotes. Multiline strings use triple quotes and may include newlines and interpolation:

```beamlang
let text = """Line one
Line two ${value}""";
```

## Standard Library Organization

The stdlib is organized into two directories under `stdlib/`:

- **`stdlib/core/`** — Always auto-imported. Contains fundamental types and data structures: `string`, `list`, `range`, `result`, `optional`, `task`, `map`, `math`.
- **`stdlib/ext/`** — Requires explicit `import`. Contains modules for IO, networking, and argument parsing: `system`, `network`, `args`.

```beamlang
// Core types are always available — no import needed
let nums: [number] = [1, 2, 3];
let r = 1..5;

// Ext modules require an import
import system.*;
import args.*;
import network.*;
```

Type alias: `[T]` is equivalent to `List<T>`.

For the full stdlib API reference, see [`docs/stdlib.md`](docs/stdlib.md).

## External Functions

BeamLang uses external function declarations for runtime interop. Both Erlang and Elixir modules are supported:

```beamlang
// Call an Erlang stdlib function directly
@external(erlang, "math", "sqrt")
fn sqrt(n: number) -> number;

// Call an Elixir module function
@external(elixir, "BeamLang.Runtime", "println")
fn println(value: any) -> void;
```

The stdlib prefers `@external(erlang, ...)` for functions that map directly to Erlang/OTP builtins (math, list operations, etc.) and uses `@external(elixir, ...)` only when the BeamLang runtime provides necessary value wrapping or conversion logic.

## Notes

- The language does not have classes. Methods are just function fields in structs.
- The stdlib is implemented in BeamLang and loaded as source, not hard-coded.
- Internal stdlib functions are marked with `internal`, callable within stdlib source files, and not callable from user modules.

## Async

BeamLang supports first-class async tasks backed by Elixir `Task`.

- `async { ... }` creates a `Task<T>` from a block expression.
- `await(task)` returns `T!TaskError` using a default timeout.
- `await(task, timeout_ms)` returns `T!TaskError` with an explicit timeout.

```beamlang
let task = async {
    return 42;
};

let result = await(task, 1000);
match (result) {
    case!ok value => println(value),
    case!err err => println("Task failed: ${err->kind}")
};
```

Task creation is available as a core function:
- `task_spawn`

Task control is method-based on `Task<T>`:
- `await_result`, `await_timeout`
- `poll`, `yield`, `cancel`, `status`

`task->poll()` and `task->yield(timeout_ms)` return optional results:
- `?none` when the task is still running
- `?some(T!TaskError)` when completed
