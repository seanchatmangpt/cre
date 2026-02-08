---
paths:
  - "src/**/*.erl"
  - "src/**/*.hrl"
  - "test/**/*.erl"
---

# Erlang Conventions

## OTP Behaviors
- MUST follow OTP design principles. Use gen_server, gen_statem, supervisor behaviors.
- This project uses `gen_pnet` (Petri Net) behavior for workflow pattern modules.
- All gen_server handle_call/handle_cast clauses that destructure state records MUST bind `= State` and return the full `State`, not a partial record reconstruction.

## Type System
- MUST use `-spec` type annotations on all exported functions.
- MUST use `-type` definitions for complex data structures.

## Code Style
- Prefer `maps` over `proplists` for key-value data.
- Use `logger` module for logging. NEVER use `io:format` or `error_logger` in production code.
- Use `list_to_atom/1` (not `list_to_existing_atom/1`) when the atom may not exist yet.
- All `case` expressions on user-facing format/type atoms MUST include a catch-all `_ ->` clause.
- ETS tables: use `named_table` and `ets:whereis/1` for existence checks.
