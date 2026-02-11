# SOC 2 Evidence Initialization - Integration Examples

This document provides practical examples for integrating `soc2_evidence_init` into your CRE application.

## Quick Start

The simplest integration is to call initialization during application startup:

```erlang
%% In src/app/cre.erl, in the start/2 callback

start(_Type, _Args) ->
    %% Existing initialization code
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
        {error, {already_started, Port}} ->
            logger:info("CRE web service already running on port ~p", [Port]),
            cre_sup:start_link()
    end.
```

## Integration Option 1: Direct Call in cre:start/2

**Pros:**
- Simple and straightforward
- Fails fast if initialization fails
- No additional process overhead

**Cons:**
- Blocks application startup on slow filesystems
- No separate restart management

**Implementation:**

```erlang
start(_Type, _Args) ->
    ensure_cre_gen_pnet_loaded(),
    ok = cre_config:init(),

    %% Initialize SOC 2 evidence directories
    case soc2_evidence_init:ensure_directories() of
        ok ->
            logger:info("SOC 2 evidence directories initialized"),
            DefaultPort = cre_config:get(cre_default_port, 4142),
            case start_cre_webservice(DefaultPort) of
                {ok, Port} ->
                    logger:info("Starting CRE: vsn=~p node=~p port=~p",
                                [?VSN, node(), Port]),
                    cre_sup:start_link();
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:error("Failed to initialize SOC 2 directories: ~p", [Reason]),
            {error, {soc2_init_failed, Reason}}
    end.
```

## Integration Option 2: Supervisor Child Process

**Pros:**
- Separate process management
- Can be restarted independently
- Better error isolation

**Cons:**
- Slightly more complex
- Requires supervisor integration

**Implementation in cre_sup:init/1:**

```erlang
init(_Args) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 5
    },

    %% Evidence initialization - runs once at startup
    EvidenceInitSpec = #{
        id => soc2_evidence_init,
        start => {soc2_evidence_init, init_directories, []},
        restart => temporary,  % One-time initialization
        shutdown => 5000,
        type => worker,
        modules => [soc2_evidence_init]
    },

    ChildSpecs = [
        EvidenceInitSpec,  % First child - run initialization
        CreMasterSpec,
        TimeoutSpec,
        XesSpec,
        ApprovalSpec,
        WorkflowSupSpec,
        WorklistSpec,
        RegistrySpec
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Integration Option 3: Hook Before Supervisor Startup

**Pros:**
- Guarantees initialization before all other services
- No process overhead
- Clean separation of concerns

**Cons:**
- Requires modification of cre.erl

**Implementation:**

```erlang
start(_Type, _Args) ->
    ensure_cre_gen_pnet_loaded(),
    ok = cre_config:init(),

    %% Initialize SOC 2 evidence directories before starting supervisor
    ok = soc2_evidence_init:init_directories(),

    DefaultPort = cre_config:get(cre_default_port, 4142),
    case start_cre_webservice(DefaultPort) of
        {ok, Port} ->
            logger:info("Starting CRE: vsn=~p node=~p port=~p",
                        [?VSN, node(), Port]),
            cre_sup:start_link();
        {error, {already_started, Port}} ->
            logger:info("CRE web service already running on port ~p", [Port]),
            cre_sup:start_link()
    end.
```

Note: `init_directories/0` will exit with error tuple if initialization fails.

## Integration Option 4: Explicit Manual Initialization

For testing or specific deployment scenarios:

```erlang
%% In a deployment script or test setup
1> soc2_evidence_init:ensure_directories().
ok

%% Or with error handling
case soc2_evidence_init:ensure_directories() of
    ok ->
        io:format("Directories ready~n");
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.
```

## Recommended Approach

For production CRE deployments, **Option 1 (Direct Call)** is recommended because:

1. Simple and explicit
2. Fails fast at application startup
3. No additional process management
4. Evidence directories are initialized before any service starts

Here's the complete recommended implementation:

```erlang
%% In src/app/cre.erl

start(_Type, _Args) ->
    %% Load cre's gen_pnet (inject/step/drain) before dep's version
    ensure_cre_gen_pnet_loaded(),

    %% Initialize persistent_term configuration (OTP 21+ optimization)
    ok = cre_config:init(),

    %% Initialize SOC 2 evidence directories
    case soc2_evidence_init:ensure_directories() of
        ok ->
            logger:info("SOC 2 evidence directories ready");
        {error, DirError} ->
            logger:warning("SOC 2 directory initialization warning: ~p", [DirError])
            %% Note: We log but don't fail - evidence collection is optional
    end,

    %% Use persistent_term for O(1) access to default port
    DefaultPort = cre_config:get(cre_default_port, 4142),
    case start_cre_webservice(DefaultPort) of
        {ok, Port} ->
            logger:info("Starting CRE: vsn=~p node=~p port=~p",
                        [?VSN, node(), Port],
                        [{info, "starting cre"}, {application, cre}]),
            cre_sup:start_link();
        {error, {already_started, Port}} ->
            logger:info("CRE web service already running on port ~p", [Port]),
            cre_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.
```

## Verification

After integration, verify directories are created correctly:

### During Application Startup

```erlang
1> application:start(cre).
INFO: Initializing SOC 2 evidence directories
DEBUG: Created directory: evidence/uptime
DEBUG: Created gitkeep file: evidence/uptime/.gitkeep
... (more directories)
INFO: All SOC 2 evidence directories initialized successfully
{ok, <0.456.0>}

2> filelib:is_dir("evidence/uptime").
true

3> filelib:is_file("evidence/uptime/.gitkeep").
true
```

### Via Module API

```erlang
1> soc2_evidence_init:ensure_directories().
ok

2> soc2_evidence_init:get_evidence_directories().
["evidence/uptime","evidence/load_tests",
 "evidence/chaos","evidence/period"]

3> filelib:is_dir("receipts").
true
```

## Troubleshooting

### Permissions Error

If you see `{error, eacces}` warnings:

```
WARNING: Cannot set permissions on evidence/uptime: access denied
```

This is non-fatal. The module logs and continues. Ensure the directory is writable:

```bash
chmod 755 evidence/uptime
```

### Directory Already Exists

If a directory already exists, the module skips creation and returns `ok`:

```erlang
1> soc2_evidence_init:ensure_directory("evidence/uptime").
ok
```

### Missing Parent Directory

The module creates parent directories automatically:

```erlang
1> soc2_evidence_init:ensure_directory("evidence/custom/deep/path").
ok
%% All parent directories created automatically
```

## Related Modules

- **soc2_evidence_gen** - Evidence artifact generation (depends on directories)
- **soc2_receipt_chain** - Receipt management (uses receipts/ directory)
- **cre** - Application module (integration point)
- **cre_sup** - Supervisor (optional integration point)

## Testing

Run the comprehensive test suite:

```bash
# Common Test suite
rebar3 ct --suite=test/soc2_evidence_init_SUITE

# Run specific test
rebar3 ct --suite=test/soc2_evidence_init_SUITE --case test_ensure_directories_all

# EUnit tests (embedded in module)
rebar3 eunit soc2_evidence_init
```

## Files Modified/Created

For production integration, these files would be modified:

- **src/app/cre.erl** - Add `soc2_evidence_init:ensure_directories()` to `start/2`

These files are already created:

- **src/soc2/soc2_evidence_init.erl** - Main module
- **test/soc2_evidence_init_SUITE.erl** - Test suite
- **docs/soc2/EVIDENCE_INITIALIZATION.md** - Complete documentation

## Next Steps

1. Choose an integration option above
2. Apply the code changes
3. Run tests to verify
4. Deploy and monitor logs
5. Verify evidence directories are created correctly
