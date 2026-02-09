# CRE Migration Guide

This guide helps you migrate between versions of CRE (Common Runtime Environment). It covers breaking changes, API migrations, configuration updates, and data migration steps.

## Table of Contents

- [Version Support Policy](#version-support-policy)
- [Quick Migration Decision Tree](#quick-migration-decision-tree)
- [Version History](#version-history)
- [OTP 25 to 28 Migration](#otp-25-to-28-migration)
- [API Migrations](#api-migrations)
- [Configuration Changes](#configuration-changes)
- [Data Migration](#data-migration)
- [Testing After Migration](#testing-after-migration)
- [Troubleshooting](#troubleshooting)

---

## Version Support Policy

| Version | Support Status | EOL Date | Notes |
|---------|----------------|----------|-------|
| 0.3.x | Current | Active | Primary development branch |
| 0.2.x | Maintenance | TBD | Security fixes only |
| 0.1.x | End of Life | 2025-02-04 | No longer supported |

### Minimum Requirements

- **OTP Version**: 25.0 minimum (OTP 19-24 dropped in v0.2.0)
- **Rebar3**: 3.0.0 or higher
- **Tested OTP Versions**: 25, 26, 27, 28

---

## Quick Migration Decision Tree

```
Are you upgrading from...
    |
    +--- v0.1.x? -> Go to [Migrating from v0.1.x to v0.2.0](#migrating-from-v01x-to-v020)
    |
    +--- v0.2.0? -> Go to [Migrating from v0.2.0 to v0.3.0](#migrating-from-v020-to-v030)
    |
    +--- v0.2.1? -> Go to [Migrating from v0.2.1 to v0.3.0](#migrating-from-v021-to-v030)
```

---

## Version History

### v0.3.0 (2026-02-06) - Current

**Major Release: 43 YAWL Patterns Complete**

#### Breaking Changes

1. **GenPNet Callback Interface Changes**
   - `fire/3` now returns `{produce, Map}` instead of `{produce, Map, State}`
   - `init/1` now returns plain `State` instead of `{ok, State}`
   - `terminate/2` and `trigger/3` must extract `usr_info` from `#net_state{}` record

2. **Module Renaming**
   - `wf/yawl_executor.erl` -> `wf/wf_yawl_executor.erl`
   - `wf/yawl_persistence.erl` -> `wf/wf_yawl_persistence.erl`
   - `wf/yawl_schema.erl` -> `wf/wf_yawl_schema.erl`
   - `wf/yawl_telemetry.erl` -> `wf/wf_yawl_telemetry.erl`
   - `integration/yawl_claude_bridge.erl` -> `yawl_claude_bridge.erl`

3. **Dependency Changes**
   - All dependencies switched from hex to git sources
   - Cowboy: 2.14.2
   - Cowlib: 2.16.0 (override for OTP 28 compatibility)
   - Ranch: 2.1.0
   - JSX: v3.1.0
   - JSONE: 1.9.0

#### New Features

- Complete implementation of all 43 YAWL workflow patterns
- 14 new advanced pattern modules (critical_section, data_accumulate, etc.)
- `gen_yawl` wrapper behavior for YAWL workflows
- Enhanced Petri net validation and soundness verification
- Process mining capabilities with Alpha algorithm
- RNN-based predictive mining

### v0.2.1 (2026-02-05)

**Major Feature Release**

#### Breaking Changes
- None (additive release)

#### New Features
- Human-in-the-Loop approval workflows
- OpenTelemetry integration
- XES logging support
- Web dashboard
- 36 YAWL patterns implemented

### v0.2.0 (2025-02-04)

**OTP 25+ Migration Release**

#### Breaking Changes

1. **OTP Version Requirement**
   - Dropped support for OTP 19-24
   - Minimum OTP version is now 25.0

2. **Logging API Migration**
   - Migrated from `error_logger` to `logger`
   - `error_logger:info_report/1` -> `logger:info/2`
   - `error_logger:error_report/1` -> `logger:error/2`
   - `error_logger:warning_report/1` -> `logger:warning/2`

3. **GenPNet Interface Changes**
   - Fixed undefined `gen_pnet:set_usr_info/2` calls
   - Pattern state now uses process dictionary

4. **Code:lib_dir Deprecation**
   - `code:lib_dir(cre, priv)` -> `code:lib_dir(cre) ++ "/priv"`

### v0.1.10 (Previous Release)

- Initial YAWL workflow patterns implementation
- Stateless execution support
- Basic persistence layer with Mnesia

---

## OTP 25 to 28 Migration

### Type Specification Changes

OTP 28 introduced stricter type checking. CRE v0.3.0 includes updated type specifications.

#### Before (OTP 25-style types)

```erlang
-spec my_function(atom()) -> {ok, term()} | {error, term()}.
```

#### After (OTP 28-style types)

```erlang
-spec my_function(atom()) -> {ok, term()} | {error, term()}.
%% No changes needed for basic types, but Dialyzer warnings now
%% require more explicit type unions in some cases
```

### Supervisor Updates

OTP 28 updated supervisor behavior. CRE uses `one_for_one` strategy throughout.

#### Example supervisor migration

```erlang
%% OTP 25-27 (still works in 28)
init([]) ->
    ChildSpecs = [
        #{id => my_worker,
          start => {my_worker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [my_worker]}
    ],
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.

%% OTP 28 preferred (same, but more explicit)
init([]) ->
    ChildSpecs = [
        #{id => my_worker,
          start => {my_worker, start_link, []},
          restart => permanent,
          significant => false,
          shutdown => 5000,
          type => worker,
          modules => [my_worker]}
    ],
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 60},
    {ok, {SupFlags, ChildSpecs}}.
```

### Logger vs error_logger

OTP 28 fully deprecated `error_logger` in favor of `logger`.

#### Migration Table

| Old API | New API | Notes |
|---------|---------|-------|
| `error_logger:info_report(Report)` | `logger:info("~p", [Report])` | Use format string |
| `error_logger:error_report(Report)` | `logger:error("~p", [Report])` | Use format string |
| `error_logger:warning_report(Report)` | `logger:warning("~p", [Report])` | Use format string |
| `error_logger:info_msg(Format, Args)` | `logger:info(Format, Args)` | Direct mapping |
| `error_logger:error_msg(Format, Args)` | `logger:error(Format, Args)` | Direct mapping |
| `error_logger:warning_msg(Format, Args)` | `logger:warning(Format, Args)` | Direct mapping |

---

## API Migrations

### GenPNet Callback Changes (v0.2.0 -> v0.3.0)

The most significant API change in v0.3.0 is the GenPNet callback interface.

#### fire/3 Return Value

**Before (v0.2.x):**
```erlang
fire(Trsn, Mode, US) ->
    %% Returns 3-tuple with state
    {produce, #{p_out => [token]}, US}.
```

**After (v0.3.0):**
```erlang
fire(Trsn, Mode, US) ->
    %% Returns 2-tuple only
    {produce, #{p_out => [token]}}.
    %% State is immutable, stored in usr_info
```

#### init/1 Return Value

**Before (v0.2.x):**
```erlang
init(Args) ->
    {ok, #{
        my_state => initialize(Args)
    }}.
```

**After (v0.3.0):**
```erlang
init(Args) ->
    #{
        my_state => initialize(Args)
    }.
    %% No {ok, ...} wrapper
```

#### terminate/2 and trigger/3

**Before (v0.2.x):**
```erlang
terminate(Reason, UsrInfo) ->
    cleanup(UsrInfo),
    ok.

trigger(Trsn, Mode, UsrInfo) ->
    execute_trigger(Trsn, UsrInfo),
    pass.
```

**After (v0.3.0):**
```erlang
terminate(Reason, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    cleanup(UsrInfo),
    ok.

trigger(Trsn, Mode, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    execute_trigger(Trsn, UsrInfo),
    pass.
```

### cre_yawl_patterns API Changes

Several undefined function guards were added in v0.3.0:

```erlang
%% New guards for unimplemented patterns
-ifdef(OTP_25_PLUS).
-define(IS_WCP18(P), (P =:= 'WCP-18-implicit-termination')).
-define(IS_WCP19(P), (P =:= 'WCP-19-multiple-instance-without-synchronization')).
-endif.
```

---

## Configuration Changes

### rebar.config Changes

#### v0.2.0 to v0.3.0

**Before (v0.2.0):**
```erlang
{deps, [
        {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {branch, "master"}}},
        {cowboy, "2.12.0"},
        {jsone, "1.9.0"}
]}.
```

**After (v0.3.0):**
```erlang
{deps, [
        {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {branch, "master"}}},
        {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.14.2"}}},
        {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}},
        {ranch, {git, "https://github.com/ninenines/ranch.git", {tag, "2.1.0"}}},
        {jsx, {git, "https://github.com/talentdeficit/jsx.git", {tag, "v3.1.0"}}},
        {jsone, {git, "https://github.com/sile/jsone.git", {tag, "1.9.0"}}}
]}.

{overrides, [
    {override, cowboy, [
        {deps, [
            {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}},
            {ranch, {git, "https://github.com/ninenines/ranch.git", {tag, "2.1.0"}}}
        ]}
    ]}
]}.
```

### OTP Platform Defines

v0.3.0 adds platform-specific compilation:

```erlang
{erl_opts, [debug_info, bin_opt_info,
              {platform_define, "^[0-9]+", 'OTP_25_PLUS'},
              {doc, "excerpt"},
              {src_dirs, [...]}]}.
```

---

## Data Migration

### Mnesia Schema Changes

No Mnesia schema changes between v0.2.x and v0.3.0. Existing databases should be compatible.

However, if you're upgrading from v0.1.x:

#### Backup Before Migration

```bash
# Backup Mnesia data
erl -sname backup -mnesia dir "\"/path/to/your/mnesia\"" \
    -eval "mnesia:backup(\"\"/tmp/mnesia.backup\"\")." \
    -s init stop
```

#### Migration Script

```erlang
%% migrate_mnesia.erl
-module(migrate_mnesia).

-export([migrate/0]).

migrate() ->
    %% Ensure Mnesia is started
    application:ensure_all_started(mnesia),

    %% Check existing tables
    Tables = mnesia:system_info(tables),
    io:format("Existing tables: ~p~n", [Tables]),

    %% Add new columns if needed (example)
    case lists:member(cre_workflow, Tables) of
        true ->
            %% Add new attributes to existing table
            mnesia:transform_table(
                cre_workflow,
                fun({Rec, Id, Name, OldData}) ->
                    %% Transform old record to new format
                    {Rec, Id, Name, transform_data(OldData)}
                end,
                record_info(fields, cre_workflow),
                cre_workflow
            );
        false ->
            ok
    end,
    ok.

transform_data(OldData) ->
    %% Apply data transformation logic here
    OldData.
```

### Migration Execution

```bash
# 1. Stop current CRE instance
rebar3 shell --eval "init:stop()."

# 2. Backup data
erl -sname backup -eval "
    case mnesia:backup(\"/tmp/cre_backup\") of
        ok -> io:format('Backup successful~n');
        {error, Reason} -> io:format('Backup failed: ~p~n', [Reason])
    end,
    init:stop().
"

# 3. Update dependencies
rebar3 upgrade

# 4. Compile with new version
rebar3 compile

# 5. Run migration if needed
rebar3 shell --eval "migrate_mnesia:migrate()."

# 6. Start new instance
rebar3 shell
```

---

## Testing After Migration

### Pre-Migration Checklist

- [ ] Current version documented
- [ ] All custom pattern modules identified
- [ ] Mnesia data backed up
- [ ] Configuration files backed up
- [ ] Test suite documented

### Post-Migration Verification

#### 1. Compilation Test

```bash
# Should complete without errors
rebar3 compile
```

#### 2. Unit Tests

```bash
# Run EUnit tests
rebar3 eunit

# Expected: All tests pass (v0.3.0: 96%+ pass rate)
```

#### 3. Integration Tests

```bash
# Run Common Test suite
rebar3 ct

# Check for any regressions
```

#### 4. Dialyzer Type Check

```bash
# Build PLT and run Dialyzer
rebar3 dialyzer

# Should show no new warnings
```

#### 5. Custom Pattern Verification

For each custom gen_pnet pattern module:

```erlang
%% Verify callback signatures
1> c(my_pattern, [debug_info]).
{ok, my_pattern}

2> my_pattern:place_lst().
[p1, p2, ...]

3> my_pattern:trsn_lst().
[t1, t2, ...]

4> gen_pnet:start_link({local, test}, my_pattern, []).
{ok, Pid}
```

#### 6. Workflow Execution Test

```erlang
%% Test a simple workflow
1> application:ensure_all_started(cre).
ok

2> {ok, WF} = cre_yawl:new_workflow(<<"test">>).
{ok, {...}}

3> cre_yawl:add_task(WF, <<"task1">>, [{type, atomic}]).
{ok, {...}}

4> cre_yawl:execute(WF).
{ok, {...}}
```

### Performance Validation

After migration, validate performance characteristics:

```bash
# Run performance benchmarks
rebar3 shell --eval "
    {ok, Pid} = cre:start(),
    {ok, _} = cre_master:add_worker(Pid, worker_node),
    %% Execute test workflow
    timer:sleep(1000),
    init:stop().
"
```

---

## Troubleshooting

### Common Migration Issues

#### Issue: `undefined function gen_pnet:set_usr_info/2`

**Solution:** This function was removed in v0.3.0. Use process dictionary or immutable state:

```erlang
%% Old (v0.2.x)
gen_pnet:set_usr_info(Pid, NewState).

%% New (v0.3.x)
%% State is managed in usr_info field, passed via callbacks
```

#### Issue: `unbound type variable` on OTP 28

**Solution:** Ensure cowlib 2.16.0 is used via overrides in rebar.config:

```erlang
{overrides, [
    {override, cowboy, [
        {deps, [
            {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}}
        ]}
    ]}
]}.
```

#### Issue: Pattern module fire/3 returns wrong format

**Solution:** Update fire/3 to return 2-tuple:

```erlang
%% Wrong (v0.2.x style)
fire(Trsn, Mode, US) ->
    {produce, #{p_out => []}, US}.

%% Correct (v0.3.x)
fire(Trsn, Mode, US) ->
    {produce, #{p_out => []}}.
```

#### Issue: Module not found after renaming

**Solution:** Update references to renamed modules:

```erlang
%% Old
wf:yawl_executor

%% New
wf:wf_yawl_executor
```

### Getting Help

If you encounter issues not covered here:

1. Check the [CHANGELOG.md](/Users/sac/cre/CHANGELOG.md) for detailed version notes
2. Review [ARCHITECTURE.md](/Users/sac/cre/docs/ARCHITECTURE.md) for API details
3. Run `rebar3 help` for build tool assistance
4. Check test files for usage examples

---

## Version-Specific Migration Guides

### Migrating from v0.1.x to v0.2.0

1. **Upgrade OTP to 25.0+**
   - v0.2.0 requires OTP 25.0 or later
   - OTP 19-24 are no longer supported

2. **Update rebar.config**
   ```erlang
   {deps, [{cre, "0.2.0"}]}.
   ```

3. **Update logging calls**
   ```erlang
   %% Find all error_logger calls
   %% Replace with logger equivalents
   ```

4. **Fix code:lib_dir/2 usage**
   ```erlang
   %% Before
   PrivDir = code:lib_dir(cre, priv),

   %% After
   PrivDir = code:lib_dir(cre) ++ "/priv",
   ```

5. **Test thoroughly**
   ```bash
   rebar3 compile
   rebar3 eunit
   rebar3 ct
   ```

### Migrating from v0.2.0 to v0.3.0

1. **Update GenPNet callbacks in custom patterns**

   For each pattern module implementing `gen_pnet`:

   ```erlang
   %% Update fire/3
   fire(Trsn, Mode, _US) ->
       {produce, #{p_out => []}}.  %% Remove _US from return

   %% Update init/1
   init(_Args) ->
       #{state => initial}.  %% Remove {ok, ...} wrapper

   %% Update terminate/2
   terminate(_Reason, NetState) ->
       US = NetState#net_state.usr_info,
       cleanup(US),
       ok.

   %% Update trigger/3
   trigger(_Trsn, _Mode, NetState) ->
       US = NetState#net_state.usr_info,
       execute(US),
       pass.
   ```

2. **Update module references**
   - `yawl_executor` -> `wf_yawl_executor`
   - `yawl_persistence` -> `wf_yawl_persistence`
   - `yawl_schema` -> `wf_yawl_schema`
   - `yawl_telemetry` -> `wf_yawl_telemetry`

3. **Update rebar.config dependencies**
   - Switch from hex to git sources
   - Add cowlib override for OTP 28

4. **Compile and test**
   ```bash
   rebar3 compile
   rebar3 eunit
   rebar3 dialyzer
   ```

### Migrating from v0.2.1 to v0.3.0

Same as v0.2.0 to v0.3.0 above (v0.2.1 had no breaking changes from v0.2.0).

---

## Rollback Procedure

If migration fails:

1. **Stop CRE**
   ```bash
   rebar3 shell --eval "init:stop()."
   ```

2. **Restore from backup**
   ```bash
   cp -r /path/to/backup/* /path/to/cre/
   ```

3. **Restore Mnesia data** (if needed)
   ```erl -sname restore -eval "
       mnesia:start(),
       {ok, _} = mnesia:restore('/tmp/cre_backup', []),
       init:stop().
   "
   ```

4. **Verify rollback**
   ```bash
   rebar3 compile
   rebar3 eunit
   ```

---

## Appendix: Complete API Reference Changes

### GenPNet Callback Signatures

| Callback | v0.2.x Signature | v0.3.0 Signature | Change |
|----------|------------------|------------------|--------|
| `init/1` | `init(Args) -> {ok, UsrInfo}` | `init(Args) -> UsrInfo` | Removed `{ok, ...}` wrapper |
| `fire/3` | `fire(Trsn, Mode, UsrInfo) -> {produce, Marking, UsrInfo}` | `fire(Trsn, Mode, UsrInfo) -> {produce, Marking}` | State no longer returned |
| `terminate/2` | `terminate(Reason, UsrInfo)` | `terminate(Reason, NetState)` | Receives full `#net_state{}` |
| `trigger/3` | `trigger(Trsn, Mode, UsrInfo)` | `trigger(Trsn, Mode, NetState)` | Receives full `#net_state{}` |

### Module Rename Mapping

| Old Module Path | New Module Path |
|-----------------|-----------------|
| `wf/yawl_executor` | `wf/wf_yawl_executor` |
| `wf/yawl_persistence` | `wf/wf_yawl_persistence` |
| `wf/yawl_schema` | `wf/wf_yawl_schema` |
| `wf/yawl_telemetry` | `wf/wf_yawl_telemetry` |
| `integration/yawl_claude_bridge` | `yawl_claude_bridge` |

---

## Related Documentation

- **[CHANGELOG.md](/Users/sac/cre/CHANGELOG.md)** - Detailed version history
- **[otp_25_28.md](otp_25_28.md)** - Deep dive on GenPNet callback changes
- **[ARCHITECTURE.md](/Users/sac/cre/docs/ARCHITECTURE.md)** - System design overview
- **[GEN_PNET_USER_GUIDE.md](/Users/sac/cre/docs/GEN_PNET_USER_GUIDE.md)** - Petri net behavior guide

---

*This migration guide is maintained alongside the CRE project. For the latest updates, always refer to the [CHANGELOG.md](/Users/sac/cre/CHANGELOG.md).*
