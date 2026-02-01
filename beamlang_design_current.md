# BeamLang Current Design (January 2026)

This document reflects the current, implemented BeamLang language features and standard library structure.

## Overview

BeamLang is a statically-typed language that compiles to BEAM. It emphasizes explicit control flow, typed data, and lightweight interop with the runtime through externals. The standard library is written in BeamLang (.bl) files and is loaded automatically.

## Files, Modules, Imports

- Each `.bl` file is a module. The module name is the file name without `.bl`.
- `export` exposes functions and types from a module.
- `internal` marks functions that can only be called from within the same module file.

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
fn test<T>(opt: Optional<T>) -> T {
    return opt->unwrap(0);
}
```

### Optional and Result

- Optional syntax: `T?`
- Result syntax: `Ok!Err`

These are normalized to `Optional<T>` and `Result<Ok, Err>`.

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
// Using error in a Result
fn parse_number(s: String) -> Result<number, ParseError> {
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

### Generics in Functions

Generic parameters are declared after the function name:

```beamlang
fn identity<T>(value: T) -> T {
    return value;
}
```

Type parameters are inferred from arguments when possible.

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

## Comments

BeamLang supports two styles of comments:

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

`for` loops iterate over `Iterator<T>` values only.

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

## Expressions

- Binary operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`
- `if` expressions:

```beamlang
let value = if (cond) { 1; } else { 2; };
```

## Standard Library (.bl)

The stdlib is split into multiple `.bl` files under `stdlib/`.

### Iterator

```beamlang
type Iterator<T> {
    data: any,
    next: fn(Iterator<T>) -> Optional<T>,
    map: fn(Iterator<T>, fn(T) -> any) -> Iterator<any>,
    filter: fn(Iterator<T>, fn(T) -> bool) -> Iterator<T>,
    fold: fn(Iterator<T>, any, fn(any, T) -> any) -> any
}
```

### String

```beamlang
type String {
    data: any,
    length: fn(String) -> number,
    chars: fn(String) -> Iterator<char>,
    concat: fn(String, String) -> String
}
```

### Optional

```beamlang
type Optional<T> {
    kind: number,
    tag: number,
    value: any,
    unwrap: fn(Optional<T>, T) -> T,
    map: fn(Optional<T>, fn(T) -> any) -> Optional<any>,
    and_then: fn(Optional<T>, fn(T) -> Optional<any>) -> Optional<any>,
    is_present: fn(Optional<T>) -> bool
}
```

### List

```beamlang
type List<T> {
    data: any,
    length: fn(List<T>) -> number,
    get: fn(List<T>, number) -> T?,
    push: fn(List<T>, T) -> List<T>,
    pop: fn(List<T>) -> List<T>,
    first: fn(List<T>) -> T?,
    last: fn(List<T>) -> T?,
    iter: fn(List<T>) -> Iterator<T>,
    map: fn(List<T>, fn(T) -> any) -> List<any>,
    filter: fn(List<T>, fn(T) -> bool) -> List<T>,
    fold: fn(List<T>, any, fn(any, T) -> any) -> any,
    reverse: fn(List<T>) -> List<T>,
    concat: fn(List<T>, List<T>) -> List<T>
}
```

Type alias: `[T]` is equivalent to `List<T>`.

List literal syntax:

```beamlang
let nums: [number] = [1, 2, 3, 4, 5];
let empty: [String] = [];
let nested: [[number]] = [[1, 2], [3, 4]];
```

Example:

```beamlang
let nums: [number] = [1, 2, 3, 4, 5];
let sum = nums->fold(0, add_nums);
println("Sum: ${sum}");

match (nums->first()) {
    case ?some val => println("First: ${val}"),
    case ?none => println("Empty")
};
```

### Result

```beamlang
type Result<Ok, Err> {
    kind: number,
    tag: number,
    value: any,
    unwrap: fn(Result<Ok, Err>, Ok) -> Ok,
    map: fn(Result<Ok, Err>, fn(Ok) -> any) -> Result<any, Err>,
    and_then: fn(Result<Ok, Err>, fn(Ok) -> Result<any, Err>) -> Result<any, Err>
}
```

### IO

```beamlang
println<T>(value: T) -> void
print<T>(value: T) -> void
```

### Type Inspection

```beamlang
typeof<T>(value: T) -> String
to_string<T>(value: T) -> String
```

## Standard Library Generic Functions

The stdlib is fully generic - all internal methods preserve type information:

```beamlang
// Create typed lists
fn list_new<T>() -> List<T>
fn list_of<T>(items: [T]) -> List<T>

// List methods are generic
fn list_get_method<T>(self: List<T>, index: number) -> T?
fn list_push_method<T>(self: List<T>, item: T) -> List<T>
fn list_map_method<T, U>(self: List<T>, mapper: fn(T) -> U) -> List<U>
fn list_filter_method<T>(self: List<T>, predicate: fn(T) -> bool) -> List<T>
fn list_fold_method<T, U>(self: List<T>, initial: U, folder: fn(U, T) -> U) -> U

// Iterator methods are generic
fn iterator_from_list<T>(data: any) -> Iterator<T>
fn iterator_next<T>(self: Iterator<T>) -> Optional<T>
fn iterator_map<T, U>(self: Iterator<T>, mapper: fn(T) -> U) -> Iterator<U>
fn iterator_filter<T>(self: Iterator<T>, predicate: fn(T) -> bool) -> Iterator<T>
fn iterator_fold<T, U>(self: Iterator<T>, initial: U, folder: fn(U, T) -> U) -> U

// Optional methods are generic
fn optional_some<T>(value: T) -> Optional<T>
fn optional_none<T>() -> Optional<T>
fn optional_unwrap<T>(self: Optional<T>, fallback: T) -> T
fn optional_map<T, U>(self: Optional<T>, mapper: fn(T) -> U) -> Optional<U>

// Result methods are generic
fn result_ok<T, E>(value: T) -> Result<T, E>
fn result_err<T, E>(value: E) -> Result<T, E>
fn result_unwrap<T, E>(self: Result<T, E>, fallback: T) -> T
fn result_map<T, E, U>(self: Result<T, E>, mapper: fn(T) -> U) -> Result<U, E>

// IO functions are generic
fn println<T>(value: T) -> void
fn print<T>(value: T) -> void

// Convert any value to string
fn to_string<T>(value: T) -> String
fn typeof<T>(value: T) -> String
```

## External Functions

BeamLang uses external function declarations for runtime interop:

```beamlang
@external(elixir, "BeamLang.Runtime", "println")
fn println(value: any) -> void;
```

## Notes

- The language does not have classes. Methods are just function fields in structs.
- The stdlib is implemented in BeamLang and loaded as source, not hard-coded.
- Internal stdlib functions are marked with `internal` and are not callable outside their module.
