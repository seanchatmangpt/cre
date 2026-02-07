# Running AGI Symposium Ω Simulation

## Quick Start

### Verify All 43 Patterns
```bash
rebar3 shell
> verify_43_patterns:run().
```

### Run Full Simulation
```bash
rebar3 shell
> run_agi_symposium:run().
```

### Run Common Test Suite
```bash
rebar3 ct --suite=agi_symposium_omega_SUITE
```

## What's Implemented

✅ **All 43 YAWL patterns** as `gen_yawl` modules  
✅ **Pattern registry** mapping all patterns  
✅ **Pattern expander** for compilation  
✅ **YAML parser** for AGI Symposium spec  
✅ **Compiler** extended for YAML support  
✅ **Test suite** for verification  

## Architecture

Following **Joe Armstrong's design**:
- **Single OTP Runtime**: `gen_yawl` (wraps `gen_pnet`)
- **Pure Helpers**: Registry, expander, compiler, parser (all stateless)

## Status

All 43 patterns are implemented and verified. The simulation infrastructure is complete and ready for execution.
