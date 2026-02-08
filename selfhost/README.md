# Selfhost Bootstrap

This directory contains the first bootstrap for a BeamLang selfhosted compiler effort.

## Layout

- `src/` - bootstrap BeamLang source modules (CLI/args/lexer scaffolding)
- `test/` - runnable BeamLang checks for CLI parsing and lexer behavior
- `examples/` - small BeamLang programs for lexing and smoke runs

## CLI

From the repository root:

```bash
./bin/selfhost --help
./bin/selfhost --version
./bin/selfhost lex selfhost/examples/minimal.bl
cat selfhost/examples/minimal.bl | ./bin/selfhost lex --stdin
```

Output format is one token per line:

```text
TYPE VALUE line:col
```

Errors use:

```text
LEX_ERROR message line:col
```

## Token Set (v0)

- `IDENTIFIER`
- `INTEGER`
- `STRING`
- keywords: `FN`, `LET`, `RETURN`
- delimiters/operators covered by bootstrap output include: `(` `)` `{` `}` `[` `]` `,` `;` `:` `=` `+` `-` `*` `/` `->`

## Tests

Run selfhost bootstrap checks from repo root:

```bash
./bin/selfhost-check
```

This runs:

- `selfhost/test/cli_parse_test.bl`
- `selfhost/test/lexer_test.bl`
