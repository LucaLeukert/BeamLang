# BeamLang Language Design Document

## Overview

BeamLang is a statically-typed, functional programming language that compiles to the BEAM VM (Erlang Virtual Machine). It combines C-like syntax with functional programming paradigms, Rust-inspired error handling, and seamless Erlang interoperability.

## Design Philosophy

- **Type Safety First**: Strong static typing with exhaustive pattern matching
- **Explicit Over Implicit**: Clear syntax that makes control flow obvious
- **Functional with Familiarity**: Functional paradigms with C-like aesthetics
- **BEAM Native**: First-class support for processes, message passing, and fault tolerance
- **Ergonomic Error Handling**: Result and Optional types with concise syntax as language primitives

## Tooling

- The CLI can run as a Language Server Protocol (LSP) server using `beamlang --lsp`.
- The BeamLang VS Code extension launches the CLI LSP server for diagnostics, hover, go-to definition, and completion.

## Syntax Overview

### Naming Convention
- **Default**: `snake_case` for all identifiers (functions, variables, types)
- Functions: `fn calculate_total()`
- Variables: `let user_name`
- Types: `type UserAccount`

### Function Definitions

```beamlang
fn function_name(param: Type) -> ReturnType {
    // function body
    return value;
}
```

### Variables

```beamlang
let immutable_var = 10;        // Immutable by default
let mut mutable_var = 20;      // Explicitly mutable
```

### Type System

#### Primitive Types
- `number`: Numeric type
- `String`: UTF-8 encoded strings
- `char`: Single Unicode codepoint
- `bool`: Boolean values (`true`, `false`)
- `void`: Unit type for functions with no return

#### Standard Library Types
- `Iterator<T>`: Iterator over values of type `T` (used by `for` loops)
- `Optional<T>`: Optional value type (equivalent to `T?`)
- `Result<Ok, Err>`: Result type (equivalent to `Ok!Err`)

#### Collection Types
- `List<T>`: Linked lists
- `Map<K, V>`: Key-value maps

#### Native Result and Optional Types

Type syntax:
- Optional: `T?`
- Result: `Success!Error`
- Optional (generic): `Optional<T>`
- Result (generic): `Result<Ok, Err>`

These types are built into the language and don't require imports:

```beamlang
// Optional and Result are built-in types.
// Optional literals: `return?some value` / `return?none`.
// Result literals: `return!ok value` / `return!err value`.
```

#### Custom Types

```beamlang
// Struct-like types
type User {
    id: String,
    name: String,
    age: number
}

// Generic types
type Container<T> {
    value: T,
    metadata: Map<String, T>
}

fn get_value(container: Container<T>) -> T {
    return container->value;
}
```

### Struct Literals (JavaScript-like Objects)

BeamLang supports JavaScript-like object literals to construct struct-like types:

```beamlang
let mut user: User = { name = "Peter", id = 2 };
let config: Config = { timeout_ms = 5000, verbose = true };
```

Rules:
- The type annotation is required for struct literals.
- Field names use `snake_case`.
- Fields are assigned with `=`.
- Unknown or missing fields are compile-time errors.

#### Type Inference (Contextual)

Struct literals may omit the explicit type annotation **only when the expected type is unambiguous from context** (e.g., assignment target or function return type). Otherwise, a type annotation is required.

Examples:

```beamlang
fn make_user() -> User {
    return { name = "Peter", id = 2 };
}

let user: User = { name = "Peter", id = 2 };
```

### Pattern Matching

Pattern matching uses `match` with `case` keyword:

```beamlang
match (value) {
    case?some x => {
        // handle some
        return x;
    },
    case?none => {
        // handle none
        return default;
    }
}

// Multiple values
match (result, status) {
    case _ => default_action()
}

// Struct pattern matching
match (user) {
    case User { id = 1, name = "Peter" } => greet(),
    case User { id, name } => {
        println(string.format("User {}: {}", id, name));
    },
    case _ => log_error()
}

// Match guards with comparison operators
match (user) {
    case User { age, name } if age > 18 => greet(),
    case User { age } if age <= 18 => warn(),
    case _ => log_error()
}
```

Guards support comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`.

Match expressions must be exhaustive. Use `case _` or cover all variants (e.g. `case?some` + `case?none`, `case!ok` + `case!err`, `case true` + `case false`).

### Control Flow

```beamlang
// If statements
if (condition) {
    // code
} else if (other_condition) {
    // code
} else {
    // code
}

// Guard statements (Swift-style early exit)
guard (name_len > 0) else {
    return 1;
}

Guard `else` blocks must end with `return`.

// For loops
for (item in collection) {
    // process item
}

// Iteration (MVP)
// For now, `for` supports iterating over Iterator<T> and String values.
// Each `item` is a char when iterating over String.

### Modules, Exports, and Imports

Each `.bl` file is its own module. Module names are the file names without the `.bl` extension.

Use `export` to expose functions or types, and `import` to bring them into scope:

```beamlang
// math.bl
export type Pair {
    left: number,
    right: number
}

export fn add(left: number, right: number) -> number {
    return left + right;
}
```

```beamlang
// use_math.bl
import math.{Pair, add};

fn main(args: [String]) -> number {
    let pair: Pair = { left = 2, right = 3 };
    return add(pair->left, pair->right);
}
```

Namespace access is supported with `module::name`:

```beamlang
fn main(args: [String]) -> number {
    return math::add(1, 2);
}
```

You can import everything from a module with `*`:

```beamlang
import math.*;
```

You can also create aliases for modules:

```beamlang
import math as m;

fn main(args: [String]) -> number {
    return m::add(1, 2);
}
```

// While loops
while (condition) {
    // code
}

// Infinite loops
loop {
    // code
    if (should_break) {
        break;
    }
}

// If expressions
let value = if (condition) { 1; } else { 2; };
```

### Error Handling

Error handling is explicit using `Success!Error` types:

```beamlang
fn divide(a: number, b: number) -> number!String {
    if (b == 0) {
        return!err "Division by zero";
    }
    return!ok (a / b);
}

// Using the result
match (divide(10, 2)) {
    case!ok result => println(result),
    case!err msg => println(string.format("Error: {}", msg))
}
```

### Standard Errors

The standard library defines a base error type that can be used in common APIs:

```beamlang
type StdError {
    Message(String)
}
```

### Concurrency and Processes

BeamLang has first-class support for BEAM processes:

```beamlang
// Spawn a process
fn worker() -> void {
    println("Running in separate process");
    return;
}
let pid = process.spawn(worker);

// Send messages
process.send(pid, "Hello");

// Receive messages
match (process.receive()) {
    case?some message => handle(message),
    case?none => {}
}

// Receive with timeout (milliseconds)
match (process.receive_timeout(5000)) {
    case?some message => handle(message),
    case?none => println("Timeout")
}

// Get current process ID
let self_pid = process.self();
```

### Standard Library

The MVP standard library exposes global functions:

```beamlang
println(message: String) -> void
print(message: String) -> void
parse_args<T>(args: [String]) -> T!ArgsError
```

`parse_args` expects a struct type with `String`, `number`, `bool`, or `char` fields and returns `!err` if the argument count or conversions do not match.

Generic functions can be called with explicit type arguments, for example:

```beamlang
let parsed = parse_args<Args>(args);
```


Common operations use standard library functions:

```beamlang
// String operations
import beam.string;
string.length("hello");
string.concat("hello", "world");
string.format("Value: {}", x);

// List operations
import beam.list;
list.append(my_list, item);
fn map_double(x: number) -> number { return x * 2; }
fn is_positive(x: number) -> bool { return x > 0; }
list.map(my_list, map_double);
list.filter(my_list, is_positive);

// Map operations
import beam.map;
let m = map.new();
map.put(m, key, value);
map.get(m, key);  // Returns V?

// I/O operations
println("Hello");
print("No newline");

// Runtime type inspection
println(typeof("hi"));

// Process operations
import beam.process;
fn start_worker() -> void { return; }
process.spawn(start_worker);
process.send(pid, message);
process.receive();
```

## Key Language Features

### 1. Immutability by Default
Variables are immutable unless explicitly marked with `mut`.

### 2. Exhaustive Pattern Matching
The compiler ensures all cases are handled in match expressions.

### 3. No Null Values
Use `T?` instead of null to prevent null pointer errors.

### 4. Explicit Error Handling
Functions that can fail return `Success!Error`, making error paths visible.

### 5. Process-Oriented Concurrency
Built on BEAM's actor model with lightweight processes and message passing.

### 6. Type Inference
While types are static, the compiler infers types where possible:

```beamlang
let x = 10;  // Type inferred as number
let list = [1, 2, 3];  // Type inferred as List<number>
```

### 7. Expression-Oriented
Most constructs are expressions that return values:

```beamlang
let result = if (x > 0) { "positive" } else { "non-positive" };
```

## Example Program Structure

```beamlang
// Imports
import beam.process;
import beam.list;

// External declarations
@external(erlang, "crypto", "hash")
fn crypto_hash(algorithm: String, data: String) -> String;

// Standard library declarations live in stdlib/beam.bl

// Type definitions
type User {
    id: String,
    name: String
}

type UserError {
    NotFound,
    InvalidData(String)
}

// Functions
fn find_user(id: String) -> User!UserError {
    // Implementation
    return user;
}

fn helper() -> number {
    return 1;
}

// Main entry point
fn main(args: [String]) -> number {
    println("Hello, BeamLang!");
    return 0;
}
```

## Compilation Target

BeamLang compiles to BEAM bytecode, allowing:
- Hot code reloading
- Distribution across nodes
- Fault tolerance through supervision trees
- Integration with existing Erlang/Elixir ecosystems

## Future Considerations

- Macro system for metaprogramming
- Async/await syntax sugar over process communication
- Advanced type features (traits, type classes)
- Property-based testing integration
- Distributed computing primitives
- Pattern guards in match expressions

## Comparison to Similar Languages

| Feature | BeamLang | Gleam | Elixir | Rust |
|---------|----------|-------|--------|------|
| Syntax Style | C-like | ML-like | Ruby-like | C-like |
| Type System | Static | Static | Dynamic | Static |
| Error Handling | Success!Error / T? | Result/Option | try/catch | Result/Option |
| Pattern Matching | `match/case` | `case` | `case` | `match` |
| Mutability | Immutable default | Immutable | Immutable | Explicit mut |
| Target VM | BEAM | BEAM | BEAM | Native/LLVM |

## Conclusion

BeamLang bridges the gap between systems programming aesthetics and functional VM benefits, providing a familiar syntax for developers coming from languages like Rust, C, or Go, while leveraging the BEAM VM's battle-tested concurrency and fault tolerance capabilities.
