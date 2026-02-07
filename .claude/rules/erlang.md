---
paths:
  - "src/**/*.erl"
  - "src/**/*.hrl"
  - "test/**/*.erl"
---

# Erlang Conventions

- Follow OTP design principles. Use gen_server, gen_statem, supervisor behaviors.
- This project uses `gen_pnet` (Petri Net) behavior from the gen_pnet library for workflow pattern modules.
- All gen_server handle_call clauses that destructure state records MUST bind `= State` and return the full `State`, not a partial record reconstruction.
- Use `-spec` type annotations on all exported functions.
- Prefer `maps` over `proplists` for key-value data.
- Use `logger` module (not `error_logger`) for logging.
- ETS tables: use `named_table` and `ets:whereis/1` for existence checks.
- Use `list_to_atom/1` (not `list_to_existing_atom/1`) when the atom may not exist yet.
- All `case` expressions on user-facing format/type atoms must include a catch-all `_ ->` clause.
- Never use `io:format` for logging in production code.
