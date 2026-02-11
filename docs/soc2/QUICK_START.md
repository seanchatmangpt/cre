# SOC 2 Evidence Initialization - Quick Start

## What It Does

The `soc2_evidence_init` module ensures SOC 2 evidence collection directories exist during CRE application startup.

## Directory Structure

```
evidence/
  ├── uptime/
  ├── load_tests/
  ├── chaos/
  └── period/
receipts/
```

Each directory gets a `.gitkeep` file for version control tracking.

## One-Line Integration

Add this to `src/app/cre.erl` in the `start/2` function:

```erlang
ok = soc2_evidence_init:ensure_directories(),
```

## API Reference

### Main Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `ensure_directories()` | Create all required directories | `ok \| {error, Reason}` |
| `ensure_directory(Path)` | Create specific directory | `ok \| {error, Reason}` |
| `create_gitkeep(Path)` | Create .gitkeep file | `ok \| {error, Reason}` |
| `init_directories()` | Supervisor-friendly init | `ok` (or exits) |
| `get_evidence_base_dir()` | Get base dir name | `"evidence"` |
| `get_evidence_directories()` | Get all subdirs | `[string()]` |

## Basic Usage

```erlang
% Ensure all directories exist
ok = soc2_evidence_init:ensure_directories().

% Check what directories exist
Dirs = soc2_evidence_init:get_evidence_directories().
% => ["evidence/uptime","evidence/load_tests","evidence/chaos","evidence/period"]

% Create specific directory
ok = soc2_evidence_init:ensure_directory("evidence/custom").

% Create .gitkeep file
ok = soc2_evidence_init:create_gitkeep("evidence/custom").
```

## Integration Examples

### Option 1: Direct Call (Recommended)

```erlang
start(_Type, _Args) ->
    ensure_cre_gen_pnet_loaded(),
    ok = cre_config:init(),

    % Add this line:
    ok = soc2_evidence_init:ensure_directories(),

    % Continue with startup...
    DefaultPort = cre_config:get(cre_default_port, 4142),
    case start_cre_webservice(DefaultPort) of
        {ok, Port} ->
            logger:info("Starting CRE: vsn=~p node=~p port=~p",
                        [?VSN, node(), Port]),
            cre_sup:start_link();
        % ...
    end.
```

### Option 2: Supervisor Child

```erlang
EvidenceInitSpec = #{
    id => soc2_evidence_init,
    start => {soc2_evidence_init, init_directories, []},
    restart => temporary,
    shutdown => 5000,
    type => worker,
    modules => [soc2_evidence_init]
},
```

### Option 3: Manual Call

```erlang
% In shell or script
1> soc2_evidence_init:ensure_directories().
ok
```

## Error Handling

The module gracefully handles errors:

- **Missing parent directories** → Created automatically
- **Directory already exists** → Skipped (idempotent)
- **Permission denied** → Logged as warning, continues
- **Other errors** → Logged and returned as error tuple

## Testing

```bash
# Run test suite
rebar3 ct --suite=test/soc2_evidence_init_SUITE

# Run in shell
1> soc2_evidence_init:ensure_directories().
ok
2> filelib:is_dir("evidence/uptime").
true
```

## Features

✓ **Idempotent** - Safe to call multiple times
✓ **Error Tolerant** - Graceful degradation on permission issues
✓ **Well Logged** - All operations logged with logger
✓ **Type Safe** - Full Erlang type specifications
✓ **Tested** - Comprehensive test suite included
✓ **OTP Compliant** - Follows OTP principles
✓ **Documented** - Complete API documentation

## Directory Permissions

- Directories: `0755` (rwxr-xr-x)
- .gitkeep files: `0644` (rw-r--r--)

## Files

| File | Purpose |
|------|---------|
| `src/soc2/soc2_evidence_init.erl` | Main module |
| `test/soc2_evidence_init_SUITE.erl` | Test suite |
| `docs/soc2/EVIDENCE_INITIALIZATION.md` | Complete documentation |
| `docs/soc2/INTEGRATION_EXAMPLE.md` | Integration guide |

## Troubleshooting

### Check if directories were created

```erlang
1> filelib:is_dir("evidence/uptime").
true
```

### Check .gitkeep files

```erlang
1> filelib:is_file("evidence/uptime/.gitkeep").
true
```

### Verify module loads

```erlang
1> code:load_file(soc2_evidence_init).
{module, soc2_evidence_init}
```

### Check logs

Look for these log messages during startup:

```
INFO: Initializing SOC 2 evidence directories
DEBUG: Created directory: evidence/uptime
DEBUG: Created gitkeep file: evidence/uptime/.gitkeep
INFO: All SOC 2 evidence directories initialized successfully
```

## Next Steps

1. Choose integration option (Option 1 recommended)
2. Add initialization call to application startup
3. Run tests to verify
4. Deploy and check logs
5. Verify directories created with correct permissions

## Questions?

- Complete docs: See `docs/soc2/EVIDENCE_INITIALIZATION.md`
- Integration guide: See `docs/soc2/INTEGRATION_EXAMPLE.md`
- Module source: See `src/soc2/soc2_evidence_init.erl`
- Test suite: See `test/soc2_evidence_init_SUITE.erl`

---

**Status:** ✓ Production Ready
