# BeamLang Design (January 2026)

This document reflects the current, implemented BeamLang language features and standard library structure.

## Overview

BeamLang is a statically-typed language that compiles to BEAM. It emphasizes explicit control flow, typed data, and lightweight interop with the runtime through externals. The standard library is written in BeamLang (.bl) files and is loaded automatically.

Internally, all values are represented as struct literals (similar to how everything in JavaScript is an object). This applies to primitives, optionals/results, and user-defined types.

## Tooling

- The CLI can run as a Language Server Protocol (LSP) server using `beamlang --lsp`.
- The BeamLang VS Code extension launches the CLI LSP server for diagnostics, hover, go-to definition, and completion.

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

Example usage:

```beamlang
type Path {
    path: String,
    operator /: fn(Path, String) -> Path
}

fn path_join(self: Path, segment: String) -> Path {
    let sep = "/";
    let with_sep = self->path->concat(sep);
    let new_path = with_sep->concat(segment);
    return path_new(new_path);
}

fn path_new(p: String) -> Path {
    return { path = p, operator / = path_join };
}

fn main(args: [String]) -> number {
    let base = path_new("/home");
    let full = base / "user" / "documents";
    println("${full->path}");  // /home/user/documents
    return 0;
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
- `if` expressions:

```beamlang
let value = if (cond) { 1; } else { 2; };
```

- String literals use double quotes. Multiline strings use triple quotes and may include newlines and interpolation:

```beamlang
let text = """Line one
Line two ${value}""";
```

## Standard Library (.bl)

The stdlib is organized into two directories under `stdlib/`:

- **`stdlib/core/`** — Always auto-imported. Contains fundamental types and data structures: `string`, `list`, `range`, `result`, `optional`, `map`, `set`, `math`, `vec2`.
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

### String

```beamlang
type String {
    data: any,
    length: fn(String) -> number,
    chars: fn(String) -> [char],
    concat: fn(String, String) -> String
}
```

### Optional

```beamlang
type Optional<T> {
    internal kind: number,
    internal tag: number,
    internal value: any,
    unwrap: fn(T?, T) -> T,
    map: fn(T?, fn(T) -> any) -> any?,
    and_then: fn(T?, fn(T) -> any?) -> any?,
    is_present: fn(T?) -> bool
}
```

### List

```beamlang
type List<T> {
    internal data: any,
    length: fn(List<T>) -> number,
    get: fn(List<T>, number) -> T?,
    push: fn(List<T>, T) -> List<T>,
    pop: fn(List<T>) -> List<T>,
    first: fn(List<T>) -> T?,
    last: fn(List<T>) -> T?,
    map: fn(List<T>, fn(T) -> any) -> List<any>,
    filter: fn(List<T>, fn(T) -> bool) -> List<T>,
    fold: fn(List<T>, any, fn(any, T) -> any) -> any,
    for_each: fn(List<T>, fn(T) -> void) -> void,
    reverse: fn(List<T>) -> List<T>,
    concat: fn(List<T>, List<T>) -> List<T>
}
```

### Args

`parse_args<T>(args: [String]) -> T!ArgsError` parses command-line arguments into a struct literal of `T`.
`T` must be a struct type with `String`, `number`, `bool`, or `char` fields.

Field annotations control parsing behavior:
- `@required()` - Field must be provided (error if missing)
- `@default(value)` - Default value when not provided
- `@description("...")` - Help text description
- `@short("c")` - Short flag form, e.g., `-c`
- `@long("config")` - Long flag form, e.g., `--config`
- `@flag` - Boolean flag (presence = true, absence = false)

Fields without `@short` or `@long` are parsed as positional arguments (matched in field definition order).

Parsing features:
- Named flags: `--verbose`, `-v`, `--count=5`, `-n 5`
- Combined short flags: `-inv` is equivalent to `-i -n -v` (flag-type options only)
- `--` separator: everything after `--` is treated as positional
- Built-in help: `--help` or `-h` returns `ArgsError` with auto-generated usage text

`usage<T>(program: String) -> String` generates a formatted help/usage string from the type's annotations.

```beamlang
error ArgsError {
    message: String,
    missing: [String]
}

fn parse_args<T>(args: [String]) -> T!ArgsError
fn usage<T>(program: String) -> String
```

#### Low-Level Argument Helpers (Pure BeamLang)

The args module also provides pure-BeamLang helper functions for manual/custom argument parsing. These implement the same parsing algorithms as the runtime but without requiring compile-time type metadata.

```beamlang
// Types
type ParsedFlag { name: String, has_value: bool, value: String }
type SplitArgs { before: [String], after: [String] }

// Classification
fn is_help(arg: String) -> bool
fn has_help_flag(args: [String]) -> bool
fn is_long_opt(arg: String) -> bool
fn is_short_opt(arg: String) -> bool
fn is_separator(arg: String) -> bool

// Parsing
fn parse_long_opt(arg: String) -> ParsedFlag?
fn parse_short_opt(arg: String) -> ParsedFlag?
fn parse_combined_flags(arg: String) -> [String]

// Collection
fn split_at_separator(args: [String]) -> SplitArgs
fn find_flag(args: [String], long_name: String, short_name: String) -> bool
fn find_option(args: [String], long_name: String, short_name: String) -> String?
fn positional_args(args: [String]) -> [String]

// Help text formatting
fn format_opt_line(short: String, long: String, hint: String, desc: String) -> String
fn format_pos_line(name: String, hint: String, required: bool, desc: String) -> String
fn format_usage_header(program: String, has_options: bool, names: [String]) -> String
```

Low-level example (no struct type needed):

```beamlang
fn main(args: [String]) -> number {
    if (has_help_flag(args)) {
        let names: [String] = ["file"];
        println(format_usage_header("myapp", true, names));
        println(format_opt_line("v", "verbose", "", "Verbose output"));
        println(format_opt_line("n", "count", "NUMBER", "Item count"));
        return 0;
    }
    let verbose = find_flag(args, "verbose", "v");
    let count = find_option(args, "count", "n");
    let count_val = count->unwrap("10");
    let pos = positional_args(args);
    return 0;
}
```

Example:

```beamlang
type Args {
    @required()
    @description("Input file to process")
    file: String,

    @short("n")
    @long("count")
    @default(10)
    @description("Number of items")
    count: number,

    @flag
    @short("v")
    @long("verbose")
    @description("Enable verbose output")
    verbose: bool
}

fn main(args: [String]) -> number {
    let parsed = parse_args<Args>(args);
    match (parsed) {
        case!ok opts => println(opts->file),
        case!err err -> {
            println("Error: ${err->message}");
            let help = usage<Args>("my_program");
            println(help);
            1;
        }
    }
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
    internal kind: number,
    internal tag: number,
    internal value: any,
    unwrap: fn(Ok!Err, Ok) -> Ok,
    map: fn(Ok!Err, fn(Ok) -> any) -> any!Err,
    and_then: fn(Ok!Err, fn(Ok) -> any!Err) -> any!Err
}
```

### IO

```beamlang
println<T>(value: T) -> void
print<T>(value: T) -> void
```

### System

The system module provides file and environment operations with proper error types:

```beamlang
// Error type for IO operations
export error IoError {
    kind: String,
    message: String
}

fn read_file(path: String) -> String!IoError
fn write_file(path: String, contents: String) -> bool!IoError
fn file_exists(path: String) -> bool
fn get_env(name: String) -> String?
```

Example:

```beamlang
let result = read_file("config.txt");
match (result) {
    case!ok content => println(content),
    case!err err => println("Error: ${err->message}")
};
```

### Networking (HTTP)

```beamlang
export error NetError {
    kind: String,
    message: String
}

type HttpResponse {
    status: number,
    body: String,
    headers: [String]
}

fn http_request(method: String, url: String, headers: [String], body: String, timeout_ms: number, follow_redirects: bool) -> HttpResponse!NetError
fn http_get(url: String) -> HttpResponse!NetError
fn http_head(url: String) -> HttpResponse!NetError
```

Example:

```beamlang
let result = http_get("https://example.com");
match (result) {
    case!ok resp => println(resp->body),
    case!err err => println("Network error: ${err->message}")
};
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
fn list_for_each_method<T>(self: List<T>, callback: fn(T) -> void) -> void

// Optional methods are generic
fn optional_some<T>(value: T) -> T?
fn optional_none<T>() -> T?
fn optional_unwrap<T>(self: T?, fallback: T) -> T
fn optional_map<T, U>(self: T?, mapper: fn(T) -> U) -> U?

// Result methods are generic
fn result_ok<T, E>(value: T) -> T!E
fn result_err<T, E>(value: E) -> T!E
fn result_unwrap<T, E>(self: T!E, fallback: T) -> T
fn result_map<T, E, U>(self: T!E, mapper: fn(T) -> U) -> U!E

// IO functions are generic
fn println<T>(value: T) -> void
fn print<T>(value: T) -> void

// Convert any value to string
fn to_string<T>(value: T) -> String
fn typeof<T>(value: T) -> String
```

### Vec2 (2D Vector with Operators)

```beamlang
type Vec2 {
    x: number,
    y: number,
    length: fn(Vec2) -> number,
    normalize: fn(Vec2) -> Vec2,
    dot: fn(Vec2, Vec2) -> number,
    operator +: fn(Vec2, Vec2) -> Vec2,
    operator -: fn(Vec2, Vec2) -> Vec2,
    operator *: fn(Vec2, number) -> Vec2,
    operator /: fn(Vec2, number) -> Vec2,
    operator ==: fn(Vec2, Vec2) -> bool,
    operator !=: fn(Vec2, Vec2) -> bool
}

fn vec2(x: number, y: number) -> Vec2
fn vec2_zero() -> Vec2
fn vec2_one() -> Vec2
fn vec2_distance(a: Vec2, b: Vec2) -> number
fn vec2_lerp(a: Vec2, b: Vec2, t: number) -> Vec2
```

### Range (Iteration Support)

The `..` operator creates a Range value. Ranges can be iterated directly in for loops or used with methods like `contains()` and `length()`.

```beamlang
// Range literal syntax
for (i in 0..10) {
    println(to_string(i));  // 0, 1, 2, ..., 9
}

// Assign to variable
let r = 1..5;
println(to_string(r->contains(3)));  // true
println(to_string(r->length()));     // 4.0

// Using range() function (equivalent to 0..5)
let r2 = range(0, 5);

// Range with custom step
let r3 = range_step(0, 10, 2);  // 0, 2, 4, 6, 8
```

```beamlang
type Range {
    internal start: number,
    internal end: number,
    internal step: number,
    contains: fn(Range, number) -> bool,
    length: fn(Range) -> number
}

fn range(start: number, end: number) -> Range
fn range_step(start: number, end: number, step: number) -> Range
```

### Map (Key-Value Dictionary)

```beamlang
type Map<K, V> {
    internal data: any,
    get: fn(Map<K, V>, K) -> V?,
    put: fn(Map<K, V>, K, V) -> Map<K, V>,
    remove: fn(Map<K, V>, K) -> Map<K, V>,
    contains: fn(Map<K, V>, K) -> bool,
    size: fn(Map<K, V>) -> number,
    keys: fn(Map<K, V>) -> List<K>,
    values: fn(Map<K, V>) -> List<V>
}

fn map_new<K, V>() -> Map<K, V>
```

### Set (Unique Elements)

```beamlang
type Set<T> {
    internal data: any,
    add: fn(Set<T>, T) -> Set<T>,
    remove: fn(Set<T>, T) -> Set<T>,
    contains: fn(Set<T>, T) -> bool,
    size: fn(Set<T>) -> number,
    to_list: fn(Set<T>) -> List<T>,
    union: fn(Set<T>, Set<T>) -> Set<T>,
    intersection: fn(Set<T>, Set<T>) -> Set<T>,
    difference: fn(Set<T>, Set<T>) -> Set<T>
}

fn set_new<T>() -> Set<T>
```

### Math Functions

```beamlang
fn sqrt(n: number) -> number
fn abs(n: number) -> number
fn floor(n: number) -> number
fn ceil(n: number) -> number
fn round(n: number) -> number
fn sin(n: number) -> number
fn cos(n: number) -> number
fn tan(n: number) -> number
fn asin(n: number) -> number
fn acos(n: number) -> number
fn atan(n: number) -> number
fn atan2(y: number, x: number) -> number
fn pow(base: number, exp: number) -> number
fn log(n: number) -> number
fn log10(n: number) -> number
fn exp(n: number) -> number
fn pi() -> number
fn e() -> number
fn min(a: number, b: number) -> number
fn max(a: number, b: number) -> number
fn clamp(value: number, min_val: number, max_val: number) -> number
fn lerp(a: number, b: number, t: number) -> number
fn deg_to_rad(deg: number) -> number
fn rad_to_deg(rad: number) -> number
fn sign(n: number) -> number
```

Generic functions can be called with explicit type arguments:

```beamlang
let parsed = parse_args<Args>(args);
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

## Async (Proposed)

Not implemented yet. Proposed approach:
- `Task<T>` type backed by a BEAM process
- `spawn(fn() -> T)` returns `Task<T>`
- `await(task: Task<T>) -> T` blocks until completion
- `poll(task: Task<T>) -> T?` returns `?none` if not finished

The runtime would spawn a process, send the result back to the parent,
and store a mailbox reference inside `Task<T>`.
