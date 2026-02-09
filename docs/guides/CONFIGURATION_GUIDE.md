# CRE Configuration Guide

This guide provides comprehensive documentation for configuring the Common Runtime Environment (CRE) for both development and production deployments.

## Table of Contents

1. [Environment Variables](#environment-variables)
2. [Application Configuration (sys.config)](#application-configuration-sysconfig)
3. [Build Configuration (rebar.config)](#build-configuration-rebarconfig)
4. [Runtime Configuration](#runtime-configuration)
5. [Feature Flags](#feature-flags)
6. [Performance Tuning](#performance-tuning)
7. [Development vs Production Settings](#development-vs-production-settings)

---

## Environment Variables

CRE supports environment variable-based configuration for secrets and runtime options. Environment variables override application configuration values.

### CRE Application Environment Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ZAI_API_KEY` | string | - | Z.AI (Zhipu AI) API key for chat completion |
| `ZAI_MODEL` | string | `glm-4.7-flash` | Default model for Z.AI API calls |
| `CRE_COOKIE` | string | - | Distribution cookie for node clustering |
| `CRE_DB_PASSWORD` | string | - | Database password (if using external DB) |
| `CRE_API_KEY` | string | - | API authentication key |

### YAWL Workflow Environment Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `DEMO_OMEGA_DEBUG` | boolean | `false` | Enable Omega demo debug output (set to `1`) |
| `DEMO_DEBUG` | boolean | `false` | Enable general demo debug output (set to `1`) |

### MAPEK Environment Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `MAPEK_GOAL` | string | - | MAPE-K loop optimization goal |
| `MAPEK_MAX_ITER` | integer | `5` | Maximum MAPE-K loop iterations |

### Temporary Directories

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `TMPDIR` | path | `/tmp` | Temporary directory for file operations |

---

## Application Configuration (sys.config)

The `sys.config` file (or `sys.config.src` template) contains application environment variables used by OTP applications at runtime.

### Configuration File Location

```
/Users/sac/cre/config/sys.config
```

### Example Configuration

```erlang
[
 {cre, [
   %% Z.AI API Configuration
   {zai_api_key, "YOUR_ZAI_API_KEY"},
   {zai_model, "glm-4.7-flash"},

   %% Debug Options
   {debug, false},
   {omega_debug, false}
 ]},

 {yawl, [
   %% Persistence
   {persistence_enabled, false},

   %% Timeout Configuration (see timeout_configuration.md)
   {default_timeout, 30000},
   {deadlock_interval, 5000},
   {resource_check_interval, 60000}
 ]},

 {kernel, [
   %% Distribution configuration
   {sync_nodes_optional, []},
   {sync_nodes_timeout, 5000}
 ]},

 {logger, [
   %% Logger configuration
   {level, info},
   {handler, default, logger_std_h,
    #{config => #{file => "log/cre.log"}}}
 ]}
].
```

### CRE Application Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `zai_api_key` | string | - | Z.AI API key (or use `ZAI_API_KEY` env) |
| `zai_model` | string | `glm-4.7-flash` | Model name (or use `ZAI_MODEL` env) |
| `debug` | boolean | `false` | General debug mode |
| `omega_debug` | boolean | `false` | Omega demo blocked state debugging |

### YAWL Application Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `persistence_enabled` | boolean | `false` | Enable Mnesia-based workflow persistence |
| `default_timeout` | integer | `30000` | Default pattern execution timeout (ms) |
| `deadlock_interval` | integer | `5000` | Deadlock detection check interval (ms) |
| `resource_check_interval` | integer | `60000` | Resource leak check interval (ms) |

### Kernel Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `sync_nodes_optional` | list | `[]` | Nodes to synchronize with at startup |
| `sync_nodes_timeout` | integer | `5000` | Timeout for node synchronization (ms) |

---

## Build Configuration (rebar.config)

The `rebar.config` file controls how CRE is compiled and dependencies are managed.

### File Location

```
/Users/sac/cre/rebar.config
```

### Key Configuration Sections

#### Source Directories

```erlang
{erl_opts, [
    debug_info,           % Include debug info in BEAM files
    bin_opt_info,        % Binary optimization info
    {platform_define, "^[0-9]+", 'OTP_25_PLUS'},  % OTP 25+ flag
    {doc, "excerpt"},    % Enable documentation for doctests
    {src_dirs, [
        "src",
        "src/core",      % gen_pnet runtime
        "src/pnet",      % Petri net algebra
        "src/wf",        % Workflow utilities
        "src/yawl",      % YAWL compilation/execution
        "src/patterns",  % Workflow control-flow patterns
        "src/api",
        "src/integration",
        "src/http",
        "src/app",
        "src/nato",
        "src/mining",
        "src/rust_nifs", % Rust NIF bindings
        "src/rust_implementations/paper_algorithms"
    ]}
]}.
```

#### Dependencies

```erlang
{deps, [
    {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {branch, "master"}}},
    {lib_combin, {git, "https://github.com/joergen7/lib_combin.git", {ref, "953273d875ce4eb4119219bb0d1855acc258586c"}}},
    {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.14.2"}}},
    {jsx, {git, "https://github.com/talentdeficit/jsx.git", {tag, "v3.1.0"}}},
    {jsone, {git, "https://github.com/sile/jsone.git", {tag, "1.9.0"}}},
    {yamerl, {git, "https://github.com/yakaz/yamerl.git", {tag, "0.10.0"}}}
]}.
```

#### Build Profiles

```erlang
{profiles, [
    {test, [
        {cover_enabled, false},
        {erl_opts, [debug_info, {doc, "excerpt"}, {d, 'TEST'}]},
        {deps, [{meck, "0.9.2"}]}
    ]},
    {debug, [
        {deps, [
            {recon, {git, "https://github.com/ferd/recon.git", {tag, "2.5.1"}}},
            {redbug, {git, "https://github.com/massemanet/redbug.git", {tag, "2.0.6"}}},
            {eflame, {git, "https://github.com/proger/eflame.git", {tag, "1.0.0"}}}
        ]}
    ]}
]}.
```

#### Dialyzer Configuration

```erlang
{dialyzer, [
    {warnings, [
        unmatched_returns,
        error_handling,
        underspecs
    ]},
    {plt_extra_apps, [lib_combin, gen_pnet, jsone, xmerl]},
    {get_warnings, true},
    {include_dirs, ["include"]}
]}.
```

---

## Runtime Configuration

### Persistent Term Configuration (cre_config)

CRE uses `persistent_term` (OTP 21+) for O(1) access to frequently accessed configuration values. These are initialized at application startup via `cre_config:init/0`.

#### Authentication Configuration

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `cre_auth_pbkdf2_iterations` | integer | `100000` | PBKDF2-HMAC-SHA256 iterations for password hashing |
| `cre_auth_default_session_timeout` | integer | `3600` | Default session timeout in seconds (1 hour) |
| `cre_auth_min_password_length` | integer | `8` | Minimum password length requirement |

#### YAWL Stateless Configuration

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `yawl_stateless_checkpoint_dir` | path | `priv/checkpoints` | Checkpoint directory for stateless workflows |
| `yawl_stateless_max_executions` | integer | `1000` | Maximum concurrent stateless executions |
| `yawl_stateless_execution_ttl` | integer | `3600000` | Execution TTL in milliseconds (1 hour) |
| `yawl_stateless_ttl_cleanup_interval` | integer | `60000` | TTL cleanup interval in milliseconds (1 minute) |

#### YAWL Patterns Configuration

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `yawl_patterns_place_lst` | list | (see source) | Static place list for patterns Petri net |
| `yawl_patterns_trsn_lst` | list | (see source) | Static transition list for patterns Petri net |

#### YAWL Timeout Configuration

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `yawl_timeout_checkpoint_dir` | path | `priv/yawl_checkpoints` | Checkpoint directory for timeout state |
| `yawl_timeout_default_timeout` | integer | `30000` | Default pattern execution timeout (30 seconds) |
| `yawl_timeout_deadlock_interval` | integer | `5000` | Deadlock detection interval (5 seconds) |
| `yawl_timeout_resource_check_interval` | integer | `60000` | Resource leak check interval (1 minute) |

#### Web Server Configuration

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `cre_default_port` | integer | `4142` | Default HTTP port for CRE status service |
| `cre_status_route` | string | `"/[status.json]"` | Status endpoint route |
| `cre_history_route` | string | `"/history.json"` | History endpoint route |

#### Client Configuration

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `cre_client_poll_interval` | integer | `250` | Client poll interval in milliseconds |

### Modifying Persistent Term Configuration at Runtime

```erlang
%% Get a value
Port = cre_config:get(cre_default_port).  % Returns 4142

%% Get with default
Value = cre_config:get(undefined_key, default_value).

%% Set a value (use sparingly)
ok = cre_config:set(custom_key, custom_value).

%% Reload all configuration (re-initializes to defaults)
ok = cre_config:reload().

%% Get all configuration
AllConfig = cre_config:get_all().
```

---

## Feature Flags

### YAWL Persistence

Enable/disable workflow case and work item persistence to Mnesia.

**Application Config:**
```erlang
{yawl, [
    {persistence_enabled, true}
]}
```

**Runtime API:**
```erlang
%% Enable persistence
ok = yawl_engine:enable_persistence().

%% Disable persistence
ok = yawl_engine:disable_persistence().

%% Check status
true = yawl_engine:is_persistence_enabled().
```

### Debug Modes

**Application Config:**
```erlang
{cre, [
    {debug, true},
    {omega_debug, true}
]}
```

**Environment Variables:**
```bash
export DEMO_OMEGA_DEBUG=1
export DEMO_DEBUG=1
```

### OTP Version Features

CRE automatically enables OTP 25+ features when built on compatible versions:

```erlang
-ifdef(OTP_25_PLUS).
%% OTP 25+ specific code
-endif.
```

---

## Performance Tuning

### VM Flags

Start CRE with optimized VM flags for your workload:

#### Development (fast startup, low memory)

```bash
erl -name cre@localhost \
    -pa _build/default/lib/*/ebin \
    -s cre
```

#### Production (high performance, SMP)

```bash
erl -name cre@localhost \
    +P 256000 \                    % Maximum number of processes
    +Q 65536 \                     % Maximum number of ports
    +K true \                      % Enable kernel poll
    +A 128 \                       % Async thread pool size
    +SDio 100 \                    % Scheduler dirty I/O workers
    +SDcpu 50 \                    % Scheduler dirty CPU workers
    +sbt db \                      % Scheduler bind type (dirty CPU bind type)
    +swt very_low \                % Scheduler wake threshold
    +MBas aobf \                   % Allocator settings
    -env ERL_MAX_PORTS 65536 \
    -env ERL_MAX_ETS_TABLES 2000 \
    -s cre
```

#### Memory-Optimized

```bash
erl -name cre@localhost \
    +MBacul 0 \                    % Disable aullet carrier ul
    +MBsacbp 256 \                 % Small allocator carrier block size
    +MBsbacp 256 \                 % Small allocator carrier private
    +hms 8192 \                    % Hidden memory saver
    -s cre
```

### Memory Settings

| Setting | Environment Variable | Default | Recommended Production |
|---------|---------------------|---------|------------------------|
| Max processes | `+P` | 262144 | 256000+ |
| Max ports | `ERL_MAX_PORTS` | 65536 | 65536 |
| Max ETS tables | `ERL_MAX_ETS_TABLES` | 1400 | 2000+ |
| Fragmentation | `+MB` | defaults | `+MBas aobf` |

### Scheduler Settings

| Setting | Flag | Development | Production |
|---------|------|-------------|------------|
| SMP | `-smp` | enable | enable |
| Scheduler threads | `+S` | auto | cores count |
| Kernel poll | `+K` | false | true |
| Async threads | `+A` | 10 | 128 |
| Dirty I/O workers | `+SDio` | 10 | 100 |
| Dirty CPU workers | `+SDcpu` | 10 | 50 |

### gen_server Configuration

#### Heap Configuration

```erlang
%% In gen_server init/1
init(_Args) ->
    %% Set min heap size for process
    erlang:process_flag(min_heap_size, 1000),  % words
    %% Set min bin vheap size
    erlang:process_flag(min_bin_vheap_size, 1000),  % words
    %% Set max heap size
    erlang:process_flag(max_heap_size, #{size => 10000000, kill => true}),
    {ok, #state{}}.
```

#### Message Queue Limits

```erlang
init(_Args) ->
    %% Set high message queue watermark
    erlang:process_flag(message_queue_data, off_heap),
    erlang:process_flag(max_heap_size, #{size => 1000000, kill => true}),
    {ok, #state{}}.
```

### Pool Configuration

CRE uses `poolboy` for worker pools (if available):

```erlang
%% Example pool configuration
{pool_size, 10},          % Number of workers
{max_overflow, 20},       % Max overflow workers
},
```

### Persistent Timer Configuration

The `wf_persistent_timer` gen_server supports configuration at startup:

```erlang
wf_persistent_timer:start_link([
    {timezone, <<"America/New_York">>},
    {work_start, {9, 0}},      % 9 AM
    {work_end, {17, 0}},       % 5 PM
    {weekend_days, [6, 0]}     % Saturday, Sunday
]).
```

---

## Development vs Production Settings

### Development Configuration

**sys.config:**
```erlang
[
 {cre, [
   {debug, true},
   {zai_model, "glm-4.7-flash"}
 ]},

 {yawl, [
   {persistence_enabled, false},
   {default_timeout, 30000}
 ]},

 {logger, [
   {level, debug},
   {handler, default, logger_std_h,
    #{config => #{file => "log/cre.log",
                  max_no_files => 10,
                  max_no_bytes => 10485760}}  % 10MB
   }
 ]}
].
```

**Environment:**
```bash
export ERL_AFLAGS="-kernel shell_history enabled"
export REBAR_PROFILE=debug
```

### Production Configuration

**sys.config:**
```erlang
[
 {cre, [
   {debug, false},
   {zai_model, "glm-4-plus"}  % Production model
 ]},

 {yawl, [
   {persistence_enabled, true},
   {default_timeout, 60000}     % Longer timeout for production
 ]},

 {kernel, [
   {logger_level, info},
   {logger_format, html}
 ]},

 {logger, [
   {level, info},
   {handler, default, logger_std_h,
    #{config => #{file => "/var/log/cre/cre.log",
                  max_no_files => 100,
                  max_no_bytes => 52428800}}  % 50MB
   }
 ]}
].
```

**Production VM Startup:**
```bash
#!/bin/bash
# production/cre.sh

erl -detached \
    -name cre@${HOSTNAME} \
    -setcookie ${CRE_COOKIE} \
    +P 256000 \
    +Q 65536 \
    +K true \
    +A 128 \
    +SDio 100 \
    +SDcpu 50 \
    +MBas aobf \
    -env ERL_MAX_PORTS 65536 \
    -env ERL_MAX_ETS_TABLES 2000 \
    -config /etc/cre/sys.config \
    -s cre
```

### Configuration Comparison

| Setting | Development | Production |
|---------|-------------|------------|
| Debug mode | `true` | `false` |
| Persistence | `false` | `true` |
| Log level | `debug` | `info` |
| Timeout (ms) | 30000 | 60000 |
| Log rotation | 10 files @ 10MB | 100 files @ 50MB |
| Model | `glm-4.7-flash` | `glm-4-plus` |
| Kernel poll | disabled | enabled |
| Process limit | default | 256000 |

---

## Configuration Validation

CRE provides configuration validation functions:

```erlang
%% Validate all required secrets
case cre_config:validate_secrets() of
    ok -> ok;
    {error, Missing} -> error({missing_secrets, Missing})
end.

%% Check configuration values
Port = cre_config:get(cre_default_port, 4142).

%% List all configuration
AllConfig = cre_config:get_all().
```

---

## Secrets Management

### Using Environment Variables

```bash
export CRE_COOKIE="your-secure-cookie"
export CRE_API_KEY="your-api-key"
export ZAI_API_KEY="your-zai-api-key"
```

### Runtime Access

```erlang
%% Get secret from environment
{ok, ApiKey} = cre_config:get_secret(api_key).

%% Get with default
{ok, Key} = cre_config:get_secret(api_key, <<"default_key">>).

%% List all available secrets
Secrets = cre_config:list_secrets().
```

---

## Reload Configuration

Some configuration changes require application restart. However, certain values can be updated at runtime:

```erlang
%% Reload persistent_term configuration
ok = cre_config:reload().

%% Reload application environment
application:stop(cre),
application:start(cre).

%% Hot code update
l(Module).
```

---

## File References

- **Configuration Example**: `/Users/sac/cre/config/sys.config.example`
- **Build Configuration**: `/Users/sac/cre/rebar.config`
- **Application Definition**: `/Users/sac/cre/src/cre.app.src`
- **Config Module**: `/Users/sac/cre/src/cre_config.erl`

---

## See Also

- [Timeout Configuration](./timeout_configuration.md) - Detailed timeout settings
- [Tool Configuration](./tool_configuration.md) - External tool configuration
- [Telemetry Guide](./telemetry.md) - Observability and monitoring
- [Error Handling](./ERROR_HANDLING.md) - Error configuration
