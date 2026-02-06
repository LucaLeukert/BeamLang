# Agent Instructions

- Follow the BeamLang design document (`beamlang_design.md`) and keep implementations minimal unless requested otherwise.
- Keep the design doc (`beamlang_design.md`) and the VS Code extension in `beamlang-vscode` in sync with language features.
- Prefer compiling BeamLang to BEAM bytecode via Erlang abstract forms.
- Work is only done when each new feature includes a parsing test and a runnable example, both pass, and the change is committed and pushed.
- Always run the relevant tests after substantive changes, unless the user explicitly opts out.
- When the user explicitly requests a commit and/or push, perform it.
