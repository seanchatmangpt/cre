# Project Rules

- Do what has been asked; nothing more, nothing less.
- NEVER create files unless absolutely necessary for achieving the goal.
- ALWAYS prefer editing an existing file to creating a new one.
- NEVER proactively create documentation files (*.md) or README files unless explicitly requested.
- NEVER save working files, tests, or markdown to the project root folder.
- Source code goes in `src/`. Tests go in `test/`. Documentation goes in `docs/`.
- MUST read existing code before modifying it. Understand the patterns in use.
- Avoid over-engineering. Keep changes minimal and focused on the request.
- MUST run `rebar3 compile` after any `.erl` or `.hrl` modification to verify correctness.
