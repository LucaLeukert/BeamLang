# BeamLang VS Code Extension

A Visual Studio Code extension that provides syntax highlighting and LSP-powered language features for BeamLang (.bl files).

## Features

- **Syntax Highlighting** for all BeamLang language constructs:
  - Keywords: `fn`, `type`, `let`, `import`, `export`, `internal`, `if`, `else`, `match`, `case`, `while`, `loop`, `for`, `return`, `guard`
  - Primitive types: `number`, `String`, `char`, `bool`, `void`, `any`
  - Built-in types: `Optional`, `Result`, `Iterator`
  - Optional syntax: `T?`, `?some`, `?none`
  - Result syntax: `Ok!Err`, `!ok`, `!err`
  - Method call operator: `->`
  - Namespace operator: `::`
  - Function definitions and calls
  - Generics with `<T>` syntax
  - External function attributes: `@external`
  - Comments (line and block)
  - String and character literals
  - Numbers and booleans

- **Code Folding** for functions, types, and control structures
- **Auto-closing** brackets, quotes, and parentheses
- **Bracket Matching** for `{}`, `[]`, `()`
- **Language Server** features (requires the BeamLang CLI in your PATH):
  - Diagnostics
  - Hover
  - Go-to definition
  - Completion
  - Document symbols
  - Workspace symbols
  - Signature help
  - Local/parameter hover and definition support

## Installation

### From Source

1. Copy the `beamlang-vscode` folder to your VS Code extensions directory:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\`
   - **macOS/Linux**: `~/.vscode/extensions/`

2. Restart VS Code

3. Open any `.bl` file to see syntax highlighting and language features

### Manual Installation (VSIX)

If you have a `.vsix` package:

1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
3. Click the "..." menu at the top
4. Select "Install from VSIX..."
5. Choose the `.vsix` file

## Usage

Once installed, the extension automatically activates for any file with the `.bl` extension. You can also manually set the language mode:

1. Open a file
2. Click the language indicator in the bottom-right corner
3. Select "BeamLang" from the list

To enable LSP features, ensure the `beamlang` CLI is on your PATH (or configure the path in settings). For the most stable startup, build the escript and point the extension to it. If you use the repo script, set the working directory too.

Suggested settings:

```json
{
  "beamlang.lsp.serverPath": "/Users/lucaleukert/src/BeamLang/beamlang",
  "beamlang.lsp.serverArgs": ["--lsp"],
  "beamlang.lsp.serverCwd": "/Users/lucaleukert/src/BeamLang",
  "beamlang.lsp.debug": false
}
```

## Example

See `example.bl` for a sample BeamLang file showcasing various language features.

## Language Support

This extension supports BeamLang as defined in the January 2026 design specification, including:

- Module system with imports and exports
- Generic types and functions
- Struct types with methods
- Pattern matching with guards
- Optional and Result types
- Iterator-based for loops
- External function declarations

## Development

To modify this extension:

1. Edit `syntaxes/beamlang.tmLanguage.json` to change syntax highlighting rules
2. Edit `language-configuration.json` to change bracket matching and auto-closing behavior
3. Edit `extension.js` to change LSP client behavior
4. Reload VS Code window (Ctrl+R / Cmd+R) to see changes

## License

MIT

## Version History

### 1.1.0
- Add LSP client support for diagnostics, hover, go-to definition, and completion
- Configurable LSP server path and arguments

### 1.0.0
- Initial release
- Complete syntax highlighting for BeamLang
- Support for all language features as of January 2026
