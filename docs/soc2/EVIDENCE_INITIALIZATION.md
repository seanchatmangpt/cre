# SOC 2 Evidence Initialization

## Overview

The `soc2_evidence_init` module ensures all required SOC 2 evidence and receipt directories exist during application startup. This is critical for the evidence collection pipeline, as other modules (like `soc2_evidence_gen`) depend on these directories being present.

## Directory Structure

The module creates and maintains the following directory structure:

```
evidence/
  ├── .gitkeep
  ├── uptime/
  │   └── .gitkeep
  ├── load_tests/
  │   └── .gitkeep
  ├── chaos/
  │   └── .gitkeep
  └── period/
      └── .gitkeep

receipts/
  └── .gitkeep
```

### Directory Purposes

- **evidence/uptime/** - System uptime and availability records
- **evidence/load_tests/** - Load testing results and metrics
- **evidence/chaos/** - Chaos engineering experiment results
- **evidence/period/** - Period-based compliance evidence
- **receipts/** - Audit receipts and cryptographic proofs

### .gitkeep Files

Each directory contains a `.gitkeep` file to ensure directories are tracked in version control even when empty. The `.gitkeep` file is a zero-byte file with read/write permissions (0644).

## Integration with CRE Application

### Option 1: Call during Application Startup (Recommended)

Add initialization to the `cre` application module's `start/2` callback:

```erlang
start(_Type, _Args) ->
    %% Existing initialization
    ensure_cre_gen_pnet_loaded(),
    ok = cre_config:init(),

    %% NEW: Initialize SOC 2 evidence directories
    ok = soc2_evidence_init:ensure_directories(),

    %% Continue with web service startup
    DefaultPort = cre_config:get(cre_default_port, 4142),
    case start_cre_webservice(DefaultPort) of
        {ok, Port} ->
            logger:info("Starting CRE: vsn=~p node=~p port=~p",
                        [?VSN, node(), Port]),
            cre_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.
```

### Option 2: Add as a Supervisor Child

If you prefer supervised initialization, add to `cre_sup:init/1`:

```erlang
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 0, period => 5},

    %% Evidence initialization - runs once at startup
    EvidenceInitSpec = #{
        id => soc2_evidence_init,
        start => {soc2_evidence_init, init_directories, []},
        restart => temporary,  % Run once and exit
        shutdown => 5000,
        type => worker,
        modules => [soc2_evidence_init]
    },

    ChildSpecs = [EvidenceInitSpec, ...other specs...],

    {ok, {SupFlags, ChildSpecs}}.
```

### Option 3: Manual Initialization Script

For testing or manual deployment:

```erlang
%% In shell or deployment script
1> ok = soc2_evidence_init:ensure_directories().
ok
2> soc2_evidence_init:get_evidence_directories().
["evidence/uptime","evidence/load_tests",
 "evidence/chaos","evidence/period"]
```

## API Reference

### Main Functions

#### `ensure_directories() -> ok | {error, Reason}`

Ensures all required evidence directories exist with proper permissions. This is the main entry point and is idempotent - safe to call multiple times.

**Returns:**
- `ok` - All directories created or already exist
- `{error, Reason}` - Directory creation failed

**Example:**
```erlang
ok = soc2_evidence_init:ensure_directories().
```

#### `ensure_directory(DirPath) -> ok | {error, Reason}`

Ensures a specific directory exists with proper permissions (0755). Creates parent directories as needed.

**Parameters:**
- `DirPath` - Directory path (string or binary)

**Returns:**
- `ok` - Directory created or already exists
- `{error, Reason}` - Directory creation failed

**Example:**
```erlang
ok = soc2_evidence_init:ensure_directory("evidence/custom").
```

#### `create_gitkeep(DirPath) -> ok | {error, Reason}`

Creates a `.gitkeep` file in the specified directory. Used to track empty directories in version control.

**Parameters:**
- `DirPath` - Directory path (string or binary)

**Returns:**
- `ok` - .gitkeep file created or already exists
- `{error, Reason}` - File creation failed

**Example:**
```erlang
ok = soc2_evidence_init:create_gitkeep("evidence/uptime").
```

#### `init_directories() -> ok`

Initialization entry point suitable for supervisor child specs. Calls `ensure_directories/0` and exits on failure.

**Returns:**
- `ok` - All directories initialized successfully
- Exits with reason `{initialization_failed, Reason}` on failure

**Example:**
```erlang
%% In supervisor child spec
start => {soc2_evidence_init, init_directories, []}
```

#### `get_evidence_base_dir() -> string()`

Returns the base evidence directory name.

**Returns:** `"evidence"`

#### `get_evidence_directories() -> [string()]`

Returns a list of all evidence subdirectories.

**Returns:**
```erlang
["evidence/uptime", "evidence/load_tests", "evidence/chaos", "evidence/period"]
```

## Permissions

The module sets directory permissions as follows:

- **Directories:** `0755` (owner rwx, group rx, other rx)
- **.gitkeep files:** `0644` (owner rw, group r, other r)

Permission errors are logged as warnings and don't cause initialization to fail, allowing graceful degradation in restricted environments.

## Error Handling

The module handles the following error scenarios gracefully:

1. **Parent directory doesn't exist** - Creates parent directories recursively
2. **Directory already exists** - Skips creation, continues with permissions
3. **Permission denied** - Logs warning and continues (graceful degradation)
4. **File system errors** - Logs error and returns error tuple

All errors are logged with `logger` for troubleshooting.

## Logging

The module logs at different levels:

- **INFO:** Directory initialization start/completion
- **DEBUG:** Individual directory creation, .gitkeep file creation
- **WARNING:** Permission setup failures, file creation failures
- **ERROR:** Critical initialization failures

Example log output:
```
INFO: Initializing SOC 2 evidence directories
DEBUG: Created directory: evidence/uptime
DEBUG: Created gitkeep file: evidence/uptime/.gitkeep
INFO: All SOC 2 evidence directories initialized successfully
```

## Testing

### Unit Tests (EUnit)

Unit tests are included in the module:

```bash
rebar3 eunit soc2_evidence_init
```

### Integration Tests (Common Test)

Comprehensive integration tests are available:

```bash
rebar3 ct --suite=test/soc2_evidence_init_SUITE
```

### Manual Testing

Test in the Erlang shell:

```erlang
1> soc2_evidence_init:ensure_directories().
ok
2> filelib:is_dir("evidence/uptime").
true
3> filelib:is_file("evidence/uptime/.gitkeep").
true
```

## Integration with Evidence Collectors

The `soc2_evidence_gen` module relies on these directories being present:

```erlang
%% soc2_evidence_gen usage
gen_server:start_link({local, soc2_evidence_gen_uptime},
                      soc2_evidence_gen, [uptime], []).

%% This assumes evidence/uptime exists and is writable
```

To ensure proper operation:

1. Call `soc2_evidence_init:ensure_directories()` during app startup
2. Or add `soc2_evidence_init` as a supervisor child with `restart => temporary`
3. Then start evidence generator processes

## Compliance

This module supports SOC 2 compliance by:

1. **Systematic Directory Management** - Ensures evidence directories exist and are properly configured
2. **Audit Trail** - .gitkeep files track directory structure in version control
3. **Consistent Permissions** - Standardized directory permissions across all evidence locations
4. **Error Handling** - Robust error handling with detailed logging for audit purposes

## Related Modules

- `soc2_evidence_gen` - Generates evidence artifacts (requires directories to exist)
- `soc2_receipt_chain` - Manages cryptographic receipts (uses receipts/ directory)
- `cre` - CRE application module (integration point)
- `cre_sup` - CRE supervisor (optional integration point)

## Future Enhancements

Potential improvements for future versions:

1. **Configurable Paths** - Allow custom evidence directory paths via configuration
2. **Directory Permissions Configuration** - Make permissions configurable
3. **Monitoring** - Track directory usage and available disk space
4. **Cleanup** - Automated evidence retention/cleanup based on policies
5. **Backup** - Automated backup of evidence directories
