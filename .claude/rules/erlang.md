---
paths:
  - "src/**/*.erl"
  - "src/**/*.hrl"
  - "test/**/*.erl"
---

# Erlang/OTP Conventions

## Type System
- MUST use `-spec` annotations on all exported functions
- MUST use `-type` definitions for complex structures
- Use Dialyzer for static type analysis

## Code Style
- Prefer `maps` over `proplists` for key-value data
- Use `logger` module for logging (NEVER `io:format` in production)
- Use `list_to_atom/1` when atom may not exist yet
- All `case` expressions on user-facing atoms MUST include catch-all `_ ->` clause

## OTP Behaviors
- Follow gen_server, gen_statem, supervisor design principles
- All handle_call/handle_cast clauses MUST bind `= State` and return full State
- This project uses `gen_pnet` behavior for workflow patterns

## ETS Tables
- Use `named_table` option
- Use `ets:whereis/1` for existence checks
