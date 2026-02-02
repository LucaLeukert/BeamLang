# ls Tool

A BeamLang implementation of the Unix `ls` command for listing directory contents.

## Features

- List files and directories with indicators (directories show trailing `/`)
- Long format (`-l`) showing file sizes in human-readable format (K, M, G)
- Show hidden files (`-a`) starting with `.`
- Error handling for non-existent directories

## Usage

```bash
# List current directory
beamlang examples/apps/ls.bl

# List specific directory
beamlang examples/apps/ls.bl /path/to/directory

# Long format with sizes
beamlang examples/apps/ls.bl -l

# Show all files including hidden
beamlang examples/apps/ls.bl -a

# Combine options
beamlang examples/apps/ls.bl -la /home/user
```

## Options

- `path` - Directory to list (default: current directory `.`)
- `-l, --long` - Use long listing format with file sizes
- `-a, --all` - Show hidden files (starting with `.`)

## Implementation Details

This tool demonstrates several BeamLang language features:

1. **External Functions** - Uses `@external` to call Elixir runtime for directory listing
2. **Result Types** - Proper error handling with `Result<T, E>` (syntax: `T!E`)
3. **Pattern Matching** - Uses `match` expressions to handle Ok/Err cases
4. **Struct Types** - `FileEntry` type with `name`, `is_dir`, and `size` fields
5. **Method Syntax** - String methods via `->` operator (e.g., `name->starts_with(".")`)
6. **Recursive Functions** - Iterates through entries using tail recursion
7. **Argument Parsing** - Uses `parse_args<T>()` with `@flag`, `@default`, `@description` annotations

## Example Output

```
$ beamlang examples/apps/ls.bl
cat.bl
grep.bl
ls.bl

$ beamlang examples/apps/ls.bl -l
    4K  cat.bl
    5K  grep.bl
    4K  ls.bl
```
