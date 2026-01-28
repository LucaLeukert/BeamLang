# BeamLang Language Design Document

## Overview

BeamLang is a statically-typed, functional programming language that compiles to the BEAM VM (Erlang Virtual Machine). It combines C-like syntax with functional programming paradigms, Rust-inspired error handling, and seamless Erlang interoperability.

## Design Philosophy

- **Type Safety First**: Strong static typing with exhaustive pattern matching
- **Explicit Over Implicit**: Clear syntax that makes control flow obvious
- **Functional with Familiarity**: Functional paradigms with C-like aesthetics
- **BEAM Native**: First-class support for processes, message passing, and fault tolerance
- **Ergonomic Error Handling**: Rust-style Result and Option types as language primitives

## Syntax Overview

### Naming Convention
- **Default**: `snake_case` for all identifiers (functions, variables, types)
- Functions: `fn calculate_total()`
- Variables: `let user_name`
- Types: `type UserAccount`

### Function Definitions

```rust
fn function_name(param: Type) -> ReturnType {
    // function body
    return value;
}

// Anonymous functions
fn(x) { return x + 1; }
```

### Variables

```rust
let immutable_var = 10;        // Immutable by default
let mut mutable_var = 20;      // Explicitly mutable
```

### Type System

#### Primitive Types
- `i32`, `i64`, `f32`, `f64`: Numeric types
- `String`: UTF-8 encoded strings
- `bool`: Boolean values (`true`, `false`)
- `void`: Unit type for functions with no return

#### Collection Types
- `List<T>`: Linked lists
- `Map<K, V>`: Key-value maps

#### Native Result and Option Types

These types are built into the language and don't require imports:

```rust
// Result type for error handling
Result<T, E> {
    Ok(T),
    Err(E)
}

// Option type for nullable values
Option<T> {
    Some(T),
    None
}
```

#### Custom Types

```rust
// Struct-like types
type User {
    id: String,
    name: String,
    age: i32
}

// Enum types with variants
type Status {
    Active,
    Inactive,
    Suspended(reason: String)
}

// Generic types
type Container<T> {
    value: T,
    metadata: Map<String, String>
}
```

### Struct Literals (JavaScript-like Objects)

BeamLang supports JavaScript-like object literals to construct struct-like types:

```rust
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

```rust
fn make_user() -> User {
    return { name = "Peter", id = 2 };
}

let user: User = { name = "Peter", id = 2 };
```

### Pattern Matching

Pattern matching uses `match` with `case` keyword:

```rust
match (value) {
    case Some(x) => {
        // handle Some
        return x;
    },
    case None => {
        // handle None
        return default;
    }
}

// Multiple values
match (result, status) {
    case (Ok(data), status = "active") => process(data),
    case (Err(e), _) => handle_error(e),
    case _ => default_action()
}

// Struct pattern matching
match (user) {
    case User { id = 1, name = "Peter" } => greet(),
    case User { id, name } => {
        io.println(string.format("User {}: {}", id, name));
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

### Control Flow

```rust
// If statements
if (condition) {
    // code
} else if (other_condition) {
    // code
} else {
    // code
}

// Guard statements (Swift-style early exit)
guard name_len > 0 else {
    return 1;
}

Guard `else` blocks must end with `return`.

// For loops
for (item in collection) {
    // process item
}

// Iteration (MVP)
// For now, `for` supports iterating over String values. Each `item` is an i32 codepoint.

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

Error handling is explicit using Result types:

```rust
fn divide(a: i32, b: i32) -> Result<i32, String> {
    if (b == 0) {
        return Err("Division by zero");
    }
    return Ok(a / b);
}

// Using the result
match (divide(10, 2)) {
    case Ok(result) => io.println(result),
    case Err(msg) => io.println(string.format("Error: {}", msg))
}
```

### Concurrency and Processes

BeamLang has first-class support for BEAM processes:

```rust
// Spawn a process
let pid = process.spawn(fn() {
    // Process code
    io.println("Running in separate process");
});

// Send messages
process.send(pid, "Hello");

// Receive messages
match (process.receive()) {
    case Some(message) => handle(message),
    case None => {}
}

// Receive with timeout (milliseconds)
match (process.receive_timeout(5000)) {
    case Some(message) => handle(message),
    case None => io.println("Timeout")
}

// Get current process ID
let self_pid = process.self();
```

### Standard Library

The MVP standard library exposes global functions:

```rust
println(message: String) -> void
print(message: String) -> void
```


Common operations use standard library functions:

```rust
// String operations
import beam.string;
string.length("hello");
string.concat("hello", "world");
string.format("Value: {}", x);

// List operations
import beam.list;
list.append(my_list, item);
list.map(my_list, fn(x) { return x * 2; });
list.filter(my_list, fn(x) { return x > 0; });

// Map operations
import beam.map;
let m = map.new();
map.put(m, key, value);
map.get(m, key);  // Returns Option<V>

// I/O operations
import beam.io;
io.println("Hello");
io.print("No newline");

// Process operations
import beam.process;
process.spawn(fn() { });
process.send(pid, message);
process.receive();
```

## Key Language Features

### 1. Immutability by Default
Variables are immutable unless explicitly marked with `mut`.

### 2. Exhaustive Pattern Matching
The compiler ensures all cases are handled in match expressions.

### 3. No Null Values
Use `Option<T>` instead of null to prevent null pointer errors.

### 4. Explicit Error Handling
Functions that can fail return `Result<T, E>`, making error paths visible.

### 5. Process-Oriented Concurrency
Built on BEAM's actor model with lightweight processes and message passing.

### 6. Type Inference
While types are static, the compiler infers types where possible:

```rust
let x = 10;  // Type inferred as i32
let list = [1, 2, 3];  // Type inferred as List<i32>
```

### 7. Expression-Oriented
Most constructs are expressions that return values:

```rust
let result = if (x > 0) { "positive" } else { "non-positive" };
```

## Example Program Structure

```rust
// Imports
import beam.io;
import beam.process;
import beam.list;

// External declarations
@external(erlang, "crypto", "hash")
fn crypto_hash(algorithm: String, data: String) -> String;

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
fn find_user(id: String) -> Result<User, UserError> {
    // Implementation
    return Ok(user);
}

fn helper() -> i32 {
    return 1;
}

// Main entry point
fn main() -> i32 {
    io.println("Hello, BeamLang!");
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
| Error Handling | Result/Option | Result/Option | try/catch | Result/Option |
| Pattern Matching | `match/case` | `case` | `case` | `match` |
| Mutability | Immutable default | Immutable | Immutable | Explicit mut |
| Target VM | BEAM | BEAM | BEAM | Native/LLVM |

## Conclusion

BeamLang bridges the gap between systems programming aesthetics and functional VM benefits, providing a familiar syntax for developers coming from languages like Rust, C, or Go, while leveraging the BEAM VM's battle-tested concurrency and fault tolerance capabilities.
