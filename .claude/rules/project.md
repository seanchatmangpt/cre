# Project Organization

## File Structure
- Source code: `src/`
- Tests: `test/`
- Documentation: `docs/`
- NEVER put files in project root

## Development Workflow
- MUST run `rebar3 compile` after ANY `.erl` or `.hrl` modification
- Read existing code before modifying
- Keep changes minimal and focused
- Avoid over-engineering

## Critical Constraints
- `src/core/` contains gen_pnet runtime - understand callback chain before modifying
- All workflow pattern modules implement gen_pnet behavior
- Never proactively create documentation unless requested
