# Agent Instructions

- Follow the BeamLang design document (`beamlang_design.md`) and keep implementations minimal unless requested otherwise.
- Keep the following files in sync with language features:
  - `beamlang_design.md` — language design (syntax, types, semantics)
  - `docs/stdlib.md` — standard library API reference
  - `beamlang-vscode` — VS Code extension
- Prefer compiling BeamLang to BEAM bytecode via Erlang abstract forms.
- Prefer `@external(erlang, ...)` over `@external(elixir, ...)` in the stdlib when the function maps directly to an Erlang/OTP builtin.
- Work is only done when each new feature includes a parsing test and a runnable example, both pass, and the change is committed and pushed.
- For any task (not only new features), once implementation is finished and relevant end-to-end tests pass, commit the changes before reporting completion; push when requested or when this instruction set requires it.
- Always run the relevant tests after substantive changes, unless the user explicitly opts out.
- When the user explicitly requests a commit and/or push, perform it.
