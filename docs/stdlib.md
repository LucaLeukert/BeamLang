# BeamLang Standard Library Reference

This document covers the public API of every module in the BeamLang standard library. For the language design (syntax, types, semantics), see [`../beamlang_design.md`](../beamlang_design.md).

## Organization

| Directory | Auto-imported | Modules |
|-----------|---------------|---------|
| `stdlib/core/` | Yes | `string`, `list`, `optional`, `result`, `task`, `map`, `range`, `math` |
| `stdlib/ext/` | No (use `import`) | `system`, `network`, `args` |

---

## Core Modules

### String

Strings in BeamLang are struct objects wrapping a charlist. They are created with double-quoted literals (`"hello"`) and support interpolation (`"value: ${x}"`).

#### Type

```beamlang
type String {
    data: any,
    length: fn(String) -> number,
    chars: fn(String) -> [char],
    concat: fn(String, String) -> String,
    split: fn(String, String) -> [String],
    contains: fn(String, String) -> bool,
    starts_with: fn(String, String) -> bool,
    ends_with: fn(String, String) -> bool,
    trim: fn(String) -> String,
    trim_start: fn(String) -> String,
    trim_end: fn(String) -> String,
    replace: fn(String, String, String) -> String,
    to_upper: fn(String) -> String,
    to_lower: fn(String) -> String,
    substring: fn(String, number, number) -> String,
    index_of: fn(String, String) -> number?
}
```

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `length` | `fn(String) -> number` | Number of characters |
| `chars` | `fn(String) -> [char]` | List of individual characters |
| `concat` | `fn(String, String) -> String` | Concatenate two strings |
| `split` | `fn(String, String) -> [String]` | Split by separator |
| `contains` | `fn(String, String) -> bool` | Check if substring exists |
| `starts_with` | `fn(String, String) -> bool` | Check prefix |
| `ends_with` | `fn(String, String) -> bool` | Check suffix |
| `trim` | `fn(String) -> String` | Remove leading/trailing whitespace |
| `trim_start` | `fn(String) -> String` | Remove leading whitespace |
| `trim_end` | `fn(String) -> String` | Remove trailing whitespace |
| `replace` | `fn(String, String, String) -> String` | Replace all occurrences of a pattern |
| `to_upper` | `fn(String) -> String` | Convert to uppercase |
| `to_lower` | `fn(String) -> String` | Convert to lowercase |
| `substring` | `fn(String, number, number) -> String` | Extract substring by start/end index |
| `index_of` | `fn(String, String) -> number?` | Find first index of substring, or `?none` |

#### Free Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `to_string` | `fn<T>(T) -> String` | Convert any value to its string representation |
| `typeof` | `fn<T>(T) -> String` | Get the type name of a value as a string |

#### IO

| Function | Signature | Description |
|----------|-----------|-------------|
| `println` | `fn<T>(T) -> void` | Print a value followed by a newline |
| `print` | `fn<T>(T) -> void` | Print a value without a trailing newline |

#### Example

```beamlang
let greeting = "Hello, World!";
println(greeting->length());           // 13
println(greeting->contains("World"));  // true
println(greeting->to_upper());         // HELLO, WORLD!
println(greeting->substring(0, 5));    // Hello
println(greeting->replace("World", "BeamLang")); // Hello, BeamLang!

let idx = greeting->index_of("World");
match (idx) {
    case?some i => println(i),  // 7
    case?none => println("not found")
};
```

---

### List

Lists are generic, ordered collections. The shorthand `[T]` is equivalent to `List<T>`.

#### Type

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
    concat: fn(List<T>, List<T>) -> List<T>,
    join: fn(List<T>, String) -> String
}
```

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `length` | `fn(List<T>) -> number` | Number of elements |
| `get` | `fn(List<T>, number) -> T?` | Element at index, or `?none` if out of bounds |
| `push` | `fn(List<T>, T) -> List<T>` | Append an element (returns new list) |
| `pop` | `fn(List<T>) -> List<T>` | Remove last element (returns new list) |
| `first` | `fn(List<T>) -> T?` | First element, or `?none` if empty |
| `last` | `fn(List<T>) -> T?` | Last element, or `?none` if empty |
| `map` | `fn(List<T>, fn(T) -> U) -> List<U>` | Transform each element |
| `filter` | `fn(List<T>, fn(T) -> bool) -> List<T>` | Keep elements matching predicate |
| `fold` | `fn(List<T>, U, fn(U, T) -> U) -> U` | Reduce to a single value |
| `for_each` | `fn(List<T>, fn(T) -> void) -> void` | Execute a side effect for each element |
| `reverse` | `fn(List<T>) -> List<T>` | Reverse element order |
| `concat` | `fn(List<T>, List<T>) -> List<T>` | Concatenate two lists |
| `join` | `fn(List<T>, String) -> String` | Join elements into a string with separator |

#### Constructors

| Function | Signature | Description |
|----------|-----------|-------------|
| `list_new` | `fn<T>() -> List<T>` | Create an empty list |
| `list_of` | `fn<T>([T]) -> List<T>` | Create a list from a literal |

#### List Literals

```beamlang
let nums: [number] = [1, 2, 3, 4, 5];
let empty: [String] = [];
let nested: [[number]] = [[1, 2], [3, 4]];
```

#### Example

```beamlang
let nums: [number] = [1, 2, 3, 4, 5];
println(nums->length());    // 5
println(nums->push(6)->push(7)->length()); // 7

let doubled = nums->map(fn(x: number) -> number { return x * 2; });
let evens = nums->filter(fn(x: number) -> bool { return x % 2 == 0; });
let sum = nums->fold(0, fn(acc: number, x: number) -> number { return acc + x; });
println(sum);  // 15

match (nums->first()) {
    case?some val => println(val),  // 1
    case?none => println("empty")
};
```

---

### Optional

Represents a value that may or may not be present. Use the shorthand `T?` (preferred over `Optional<T>`).

#### Type

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

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `unwrap` | `fn(T?, T) -> T` | Extract value or return fallback |
| `map` | `fn(T?, fn(T) -> U) -> U?` | Transform the inner value if present |
| `and_then` | `fn(T?, fn(T) -> U?) -> U?` | Chain optional-returning operations |
| `is_present` | `fn(T?) -> bool` | Check if value is present |

#### Literals

```beamlang
?some 42    // Optional containing 42
?none       // Empty optional
```

#### Pattern Matching

```beamlang
let opt: number? = ?some 10;
match (opt) {
    case?some x => println(x),
    case?none => println("nothing")
};
```

---

### Result

Represents a computation that can succeed or fail. Use the shorthand `Ok!Err` (preferred over `Result<Ok, Err>`).

#### Type

```beamlang
type Result<Ok, Err> {
    internal kind: number,
    internal tag: number,
    internal value: any,
    unwrap: fn(Ok!Err, Ok) -> Ok,
    unwrap_err: fn(Ok!Err) -> Err,
    map: fn(Ok!Err, fn(Ok) -> any) -> any!Err,
    and_then: fn(Ok!Err, fn(Ok) -> any!Err) -> any!Err,
    is_ok: fn(Ok!Err) -> bool,
    is_err: fn(Ok!Err) -> bool
}
```

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `unwrap` | `fn(Ok!Err, Ok) -> Ok` | Extract ok value or return fallback |
| `unwrap_err` | `fn(Ok!Err) -> Err` | Extract error value (use after `is_err()` check) |
| `map` | `fn(Ok!Err, fn(Ok) -> U) -> U!Err` | Transform the ok value |
| `and_then` | `fn(Ok!Err, fn(Ok) -> U!Err) -> U!Err` | Chain result-returning operations |
| `is_ok` | `fn(Ok!Err) -> bool` | Check if result is ok |
| `is_err` | `fn(Ok!Err) -> bool` | Check if result is an error |

#### Literals

```beamlang
!ok 42          // Successful result
!err "failed"   // Error result
```

#### Pattern Matching

```beamlang
let result: number!String = !ok 42;
match (result) {
    case!ok value => println(value),
    case!err msg => println(msg)
};
```

---

### Task

Asynchronous computation backed by Elixir `Task`.

#### Types

```beamlang
export error TaskError {
    kind: String,
    message: String
}

export enum TaskStatus {
    Running,
    Succeeded,
    Failed,
    Cancelled
}

export type Task<T> {
    internal data: any,
    await_result: fn(Task<T>) -> T!TaskError,
    await_timeout: fn(Task<T>, number) -> T!TaskError,
    poll: fn(Task<T>) -> (T!TaskError)?,
    yield: fn(Task<T>, number) -> (T!TaskError)?,
    cancel: fn(Task<T>) -> bool,
    status: fn(Task<T>) -> TaskStatus
}
```

#### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `task_spawn` | `fn<T>(fn() -> T) -> Task<T>` | Start a task from a callback |

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `await_result` | `fn(Task<T>) -> T!TaskError` | Wait using default timeout |
| `await_timeout` | `fn(Task<T>, number) -> T!TaskError` | Wait with explicit timeout (ms) |
| `poll` | `fn(Task<T>) -> (T!TaskError)?` | Non-blocking check (`?none` if running) |
| `yield` | `fn(Task<T>, number) -> (T!TaskError)?` | Timed non-blocking yield |
| `cancel` | `fn(Task<T>) -> bool` | Cancel a running task |
| `status` | `fn(Task<T>) -> TaskStatus` | Current status |

#### Language Support

| Syntax | Type |
|--------|------|
| `async { ... }` | `Task<T>` |
| `await(task)` | `T!TaskError` |
| `await(task, timeout_ms)` | `T!TaskError` |

#### Example

```beamlang
fn main(args: [String]) -> number {
    let task = async { return 42; };
    let result = task->await_timeout(1000);

    return match (result) {
        case!ok value => value,
        case!err err => {
            println("Task error: ${err->kind}");
            0;
        }
    };
}
```

---

### Map

A generic key-value dictionary.

#### Type

```beamlang
type Map<K, V> {
    internal data: any,
    get: fn(Map<K, V>, K) -> V?,
    put: fn(Map<K, V>, K, V) -> Map<K, V>,
    remove: fn(Map<K, V>, K) -> Map<K, V>,
    contains: fn(Map<K, V>, K) -> bool,
    size: fn(Map<K, V>) -> number,
    keys: fn(Map<K, V>) -> List<K>,
    values: fn(Map<K, V>) -> List<V>,
    entries: fn(Map<K, V>) -> List<Pair<K, V>>
}

type Pair<A, B> {
    first: A,
    second: B
}
```

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `get` | `fn(Map<K, V>, K) -> V?` | Look up value by key, or `?none` |
| `put` | `fn(Map<K, V>, K, V) -> Map<K, V>` | Insert or update a key-value pair (returns new map) |
| `remove` | `fn(Map<K, V>, K) -> Map<K, V>` | Remove a key (returns new map) |
| `contains` | `fn(Map<K, V>, K) -> bool` | Check if key exists |
| `size` | `fn(Map<K, V>) -> number` | Number of entries |
| `keys` | `fn(Map<K, V>) -> List<K>` | All keys as a list |
| `values` | `fn(Map<K, V>) -> List<V>` | All values as a list |
| `entries` | `fn(Map<K, V>) -> List<Pair<K, V>>` | All key-value pairs as a list of `Pair` |

#### Constructors

| Function | Signature | Description |
|----------|-----------|-------------|
| `map_new` | `fn<K, V>() -> Map<K, V>` | Create an empty map |
| `pair` | `fn<A, B>(A, B) -> Pair<A, B>` | Create a key-value pair |

#### Example

```beamlang
let mut m = map_new();
m = m->put("name", "Ada");
m = m->put("lang", "BeamLang");

println(m->size());              // 2
println(m->contains("name"));   // true

match (m->get("name")) {
    case?some v => println(v),   // Ada
    case?none => println("not found")
};
```

---

### Range

Ranges represent a sequence of numbers with a start, end, and step. The `..` operator creates a range.

#### Type

```beamlang
type Range {
    internal start: number,
    internal end: number,
    internal step: number,
    contains: fn(Range, number) -> bool,
    length: fn(Range) -> number
}
```

#### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `contains` | `fn(Range, number) -> bool` | Check if a number is within the range |
| `length` | `fn(Range) -> number` | Number of elements in the range |

#### Constructors

| Function | Signature | Description |
|----------|-----------|-------------|
| `range` | `fn(number, number) -> Range` | Create a range with step 1 |
| `range_step` | `fn(number, number, number) -> Range` | Create a range with custom step |

#### Range Literals

The `..` operator is shorthand for `range(start, end)`:

```beamlang
for (i in 0..10) {
    println(to_string(i));  // 0, 1, 2, ..., 9
}

let r = 1..5;
println(to_string(r->contains(3)));  // true
println(to_string(r->length()));     // 4.0

let evens = range_step(0, 10, 2);   // 0, 2, 4, 6, 8
```

---

### Math

Mathematical constants and functions. All trigonometric and transcendental functions call Erlang's `:math` module directly.

#### Constants

| Function | Description |
|----------|-------------|
| `pi() -> number` | The constant pi (3.14159...) |
| `e() -> number` | Euler's number (2.71828...) |

#### Basic

| Function | Description |
|----------|-------------|
| `abs(n) -> number` | Absolute value |
| `floor(n) -> number` | Round down |
| `ceil(n) -> number` | Round up |
| `round(n) -> number` | Round to nearest integer |
| `min(a, b) -> number` | Smaller of two numbers |
| `max(a, b) -> number` | Larger of two numbers |
| `clamp(value, min, max) -> number` | Constrain value to range |
| `sign(n) -> number` | Sign of a number (-1, 0, or 1) |

#### Powers and Logarithms

| Function | Description |
|----------|-------------|
| `pow(base, exp) -> number` | Exponentiation |
| `sqrt(n) -> number` | Square root |
| `exp(n) -> number` | e raised to the power n |
| `log(n) -> number` | Natural logarithm |
| `log10(n) -> number` | Base-10 logarithm |

#### Trigonometry

| Function | Description |
|----------|-------------|
| `sin(n) -> number` | Sine |
| `cos(n) -> number` | Cosine |
| `tan(n) -> number` | Tangent |
| `asin(n) -> number` | Arc sine |
| `acos(n) -> number` | Arc cosine |
| `atan(n) -> number` | Arc tangent |
| `atan2(y, x) -> number` | Two-argument arc tangent |

#### Conversion

| Function | Description |
|----------|-------------|
| `deg_to_rad(deg) -> number` | Degrees to radians |
| `rad_to_deg(rad) -> number` | Radians to degrees |
| `lerp(a, b, t) -> number` | Linear interpolation between a and b |

#### Example

```beamlang
println(sqrt(16.0));          // 4.0
println(abs(-5));             // 5.0
println(pow(2.0, 8.0));      // 256.0
println(sin(pi() / 2.0));    // 1.0
println(clamp(10, 0, 5));    // 5
println(lerp(0, 100, 0.25)); // 25.0
```

---

## Extension Modules

These require an explicit `import` statement.

### System (`import system.*`)

File I/O, environment variables, stdin, directory listing, and timing.

#### Types

```beamlang
export error IoError {
    kind: String,
    message: String
}

export type FileEntry {
    name: String,
    is_dir: bool,
    size: number
}

export type Path {
    segments: [String],
    operator /: fn(Path, String) -> Path,
    push: fn(Path, String) -> Path,
    to_string: fn(Path) -> String
}

export type Clock {
    internal start_time: number,
    internal stop_time: number,
    start: fn(Clock) -> Clock,
    stop: fn(Clock) -> Clock,
    elapsed: fn(Clock) -> number,
    now: fn(Clock) -> number
}
```

#### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `read_file` | `fn(String) -> String!IoError` | Read file contents |
| `write_file` | `fn(String, String) -> bool!IoError` | Write string to file |
| `file_exists` | `fn(String) -> bool` | Check if a file exists |
| `read_stdin` | `fn() -> String` | Read all data from stdin |
| `get_env` | `fn(String) -> String?` | Get environment variable, or `?none` |
| `list_directory` | `fn(String) -> List<FileEntry>!IoError` | List directory entries |
| `clock_new` | `fn() -> Clock` | Create a new stopwatch |
| `clock_now` | `fn() -> number` | Current monotonic time in milliseconds |
| `path_new` | `fn() -> Path` | Create an empty path |
| `path_from_string` | `fn(String) -> Path` | Create a path from slash-separated text |

#### Example

```beamlang
import system.*;

fn main(args: [String]) -> number {
    let result = read_file("config.txt");
    match (result) {
        case!ok content => println(content),
        case!err err => println("Error: ${err->message}")
    };

    let home = get_env("HOME");
    println(home->unwrap("unknown"));

    let clock = clock_new()->start();
    // ... do work ...
    let clock2 = clock->stop();
    println("Elapsed: ${to_string(clock2->elapsed())} ms");

    let full = path_from_string("usr/local") / "bin" / "beamlang";
    println(full->to_string()); // usr/local/bin/beamlang

    return 0;
}
```

---

### Network (`import network.*`)

HTTP client built on Erlang's `:httpc`.

#### Types

```beamlang
export error NetError {
    kind: String,
    message: String
}

export type HttpResponse {
    status: number,
    body: String,
    headers: [String]
}
```

#### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_get` | `fn(String) -> HttpResponse!NetError` | GET request with defaults |
| `http_head` | `fn(String) -> HttpResponse!NetError` | HEAD request with defaults |
| `http_post` | `fn(String, String, [String]) -> HttpResponse!NetError` | POST with body and headers |
| `http_get_with` | `fn(String, [String], number, bool) -> HttpResponse!NetError` | GET with custom headers, timeout, and redirect control |
| `http_request` | `fn(String, String, [String], String, number, bool) -> HttpResponse!NetError` | Full control: method, url, headers, body, timeout_ms, follow_redirects |

#### Example

```beamlang
import network.*;

fn main(args: [String]) -> number {
    let result = http_get("https://example.com");
    match (result) {
        case!ok resp => println(resp->body),
        case!err err => println("Network error: ${err->message}")
    };
    return 0;
}
```

---

### Args (`import args.*`)

Command-line argument parsing. Provides both a high-level type-driven API and low-level pure-BeamLang helpers.

#### Error Type

```beamlang
export error ArgsError {
    message: String,
    missing: [String]
}
```

#### High-Level API

These use compile-time type metadata to parse arguments into typed structs.

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse_args` | `fn<T>([String]) -> T!ArgsError` | Parse args into struct `T` |
| `usage` | `fn<T>(String) -> String` | Generate help text from type annotations |

`T` must be a struct type with `String`, `number`, `bool`, or `char` fields.

**Field annotations** control parsing behavior:

| Annotation | Description |
|------------|-------------|
| `@required()` | Field must be provided (error if missing) |
| `@default(value)` | Default value when not provided |
| `@description("...")` | Help text description |
| `@short("c")` | Short flag form, e.g., `-c` |
| `@long("config")` | Long flag form, e.g., `--config` |
| `@flag` | Boolean flag (presence = true, absence = false) |

Fields without `@short` or `@long` are parsed as **positional arguments** (matched in field definition order).

**Parsing features:**
- Named flags: `--verbose`, `-v`, `--count=5`, `-n 5`
- Combined short flags: `-inv` is equivalent to `-i -n -v` (flag-type options only)
- `--` separator: everything after `--` is treated as positional
- Built-in help: `--help` or `-h` returns `ArgsError` with auto-generated usage text

#### Example (High-Level)

```beamlang
import args.*;

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
            println(usage<Args>("my_program"));
            1;
        }
    }
}
```

#### Low-Level API

Pure-BeamLang helpers for manual argument parsing without struct type metadata.

**Types:**

```beamlang
type ParsedFlag { name: String, has_value: bool, value: String }
type SplitArgs { before: [String], after: [String] }
```

**Classification:**

| Function | Signature | Description |
|----------|-----------|-------------|
| `is_help` | `fn(String) -> bool` | Is `--help` or `-h`? |
| `has_help_flag` | `fn([String]) -> bool` | Any arg is a help flag? |
| `is_long_opt` | `fn(String) -> bool` | Starts with `--`? |
| `is_short_opt` | `fn(String) -> bool` | Starts with `-` (not `--`)? |
| `is_separator` | `fn(String) -> bool` | Is `--`? |

**Parsing:**

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse_long_opt` | `fn(String) -> ParsedFlag?` | Parse `--name` or `--name=value` |
| `parse_short_opt` | `fn(String) -> ParsedFlag?` | Parse `-n` or `-n=value` |
| `parse_combined_flags` | `fn(String) -> [String]` | Split `-inv` into `["i", "n", "v"]` |

**Collection:**

| Function | Signature | Description |
|----------|-----------|-------------|
| `split_at_separator` | `fn([String]) -> SplitArgs` | Split at `--` separator |
| `find_flag` | `fn([String], String, String) -> bool` | Check for boolean flag by long/short name |
| `find_option` | `fn([String], String, String) -> String?` | Find option value by long/short name |
| `positional_args` | `fn([String]) -> [String]` | Collect non-flag arguments |

**Help text formatting:**

| Function | Signature | Description |
|----------|-----------|-------------|
| `format_opt_line` | `fn(String, String, String, String) -> String` | Format an option help line |
| `format_pos_line` | `fn(String, String, bool, String) -> String` | Format a positional arg help line |
| `format_usage_header` | `fn(String, bool, [String]) -> String` | Format the usage header line |

#### Example (Low-Level)

```beamlang
import args.*;

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
