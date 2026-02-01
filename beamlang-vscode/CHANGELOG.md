# Change Log

All notable changes to the "beamlang" extension will be documented in this file.

## [1.1.0] - 2026-02-01

### Added
- Language server client integration for BeamLang
- Diagnostics, hover, go-to definition, and completion
- Configurable LSP server path and arguments

## [1.0.0] - 2026-01-31

### Added
- Initial release of BeamLang syntax highlighter
- Syntax highlighting for all BeamLang language constructs
- Support for keywords: fn, type, let, import, export, internal, if, else, match, case, while, loop, for, return, guard
- Support for primitive types: number, String, char, bool, void, any
- Support for built-in types: Optional, Result, Iterator
- Support for Optional syntax: T?, ?some, ?none
- Support for Result syntax: Ok!Err, !ok, !err
- Support for method call operator: ->
- Support for namespace operator: ::
- Support for function definitions and calls
- Support for generics with <T> syntax
- Support for external function attributes: @external
- Support for comments (line and block)
- Support for string and character literals
- Support for numbers and booleans
- Auto-closing brackets, quotes, and parentheses
- Bracket matching for {}, [], ()
- Code folding support
