%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author CRE Performance Optimization Team
%% @copyright 2025
%%
%% @doc CRE Persistent Term Configuration Module (OTP 21+)
%%
%% This module manages persistent_term storage for frequently accessed
%% constant data in CRE. persistent_term provides O(1) access time
%% and is optimized for read-heavy workloads.
%%
%% <h3>Performance Benefits</h3>
%% <ul>
%%   <li>O(1) constant time access (vs process dictionary or ETS)</li>
%%   <li>No copying on read (shared memory)</li>
%%   <li>Global access without message passing</li>
%%   <li>Atomic updates process-wide</li>
%% </ul>
%%
%% <h3>Usage Pattern</h3>
%% Call cre_config:init/0 during application start to initialize
%% all persistent terms. Use cre_config:get/1 to retrieve values.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_config).
-behaviour(application).

%% Disable auto-import warnings for get/1 since we export get/1, get/2
-compile({no_auto_import, [get/1]}).

-moduledoc("""
CRE Persistent Term Configuration Module.

This module manages persistent_term storage for frequently accessed
constant data in CRE. persistent_term provides O(1) access time
and is optimized for read-heavy workloads.

## Performance Benefits

- **O(1) constant time access**: vs process dictionary or ETS
- **No copying on read**: shared memory access
- **Global access**: no message passing required
- **Atomic updates**: process-wide consistency

## Usage Pattern

Initialize during application start, then retrieve values:

```erlang
1> cre_config:init().
ok
2> cre_config:get(cre_auth_pbkdf2_iterations).
100000
3> cre_config:get(cre_default_port).
4142
```

## Examples

Get configuration values:

```erlang
1> cre_config:init().
ok
2> cre_config:get(cre_auth_pbkdf2_iterations).
100000
3> cre_config:get(cre_default_port).
4142
4> cre_config:get(cre_status_route).
"/[status.json]"
```

Get configuration with default value:

```erlang
1> cre_config:init().
ok
2> cre_config:get(nonexistent_key, default_value).
default_value
```

Set configuration value:

```erlang
1> cre_config:set(custom_key, custom_value).
true
2> cre_config:get(custom_key).
custom_value
```

Get all configuration:

```erlang
1> cre_config:init().
ok
2> AllConfig = cre_config:get_all().
[{cre_auth_pbkdf2_iterations,100000},
 {cre_auth_default_session_timeout,3600},
 ...]
```

Run doctests:

```erlang
1> cre_config:doctest_test().
ok
```
""").

%%====================================================================
%% Exports
%%====================================================================

-export([init/0, reload/0]).
-export([get/1, get/2, set/2]).
-export([get_all/0]).
-export([doctest_test/0]).

%%====================================================================
%% Persistent Term Keys
%%====================================================================

%% Authentication keys
-define(AUTH_PBKDF2_ITERATIONS, cre_auth_pbkdf2_iterations).
-define(AUTH_DEFAULT_SESSION_TIMEOUT, cre_auth_default_session_timeout).
-define(AUTH_MIN_PASSWORD_LENGTH, cre_auth_min_password_length).

%% YAWL Stateless keys
-define(YAWL_STATELESS_CHECKPOINT_DIR, yawl_stateless_checkpoint_dir).
-define(YAWL_STATELESS_MAX_EXECUTIONS, yawl_stateless_max_executions).
-define(YAWL_STATELESS_EXECUTION_TTL, yawl_stateless_execution_ttl).
-define(YAWL_STATELESS_TTL_CLEANUP_INTERVAL, yawl_stateless_ttl_cleanup_interval).

%% YAWL Patterns keys
-define(YAWL_PATTERNS_PLACE_LST, yawl_patterns_place_lst).
-define(YAWL_PATTERNS_TRSN_LST, yawl_patterns_trsn_lst).

%% YAWL Timeout keys
-define(YAWL_TIMEOUT_CHECKPOINT_DIR, yawl_timeout_checkpoint_dir).
-define(YAWL_TIMEOUT_DEFAULT_TIMEOUT, yawl_timeout_default_timeout).
-define(YAWL_TIMEOUT_DEADLOCK_INTERVAL, yawl_timeout_deadlock_interval).
-define(YAWL_TIMEOUT_RESOURCE_CHECK_INTERVAL, yawl_timeout_resource_check_interval).

%% CRE Client keys
-define(CRE_CLIENT_POLL_INTERVAL, cre_client_poll_interval).

%% CRE Web keys
-define(CRE_DEFAULT_PORT, cre_default_port).
-define(CRE_STATUS_ROUTE, cre_status_route).
-define(CRE_HISTORY_ROUTE, cre_history_route).

%% CRE Master keys
-define(CRE_MASTER_LOG_MODULE, cre_master_log_module).

%%====================================================================
%% Types
%%====================================================================

-type key() :: atom().
-type value() :: term().

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes all persistent terms for the CRE application.
%%      Should be called during application start.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Initializes all persistent terms for the CRE application.

This function sets up all configuration values in persistent_term storage.
It should be called during application start.

## Examples

```erlang
1> cre_config:init().
ok
2> cre_config:get(cre_auth_pbkdf2_iterations).
100000
```
""").
-spec init() -> ok.

init() ->
    %% Initialize authentication constants
    init_auth_terms(),

    %% Initialize YAWL stateless constants
    init_yawl_stateless_terms(),

    %% Initialize YAWL patterns constants
    init_yawl_patterns_terms(),

    %% Initialize YAWL timeout constants
    init_yawl_timeout_terms(),

    %% Initialize CRE client constants
    init_cre_client_terms(),

    %% Initialize CRE web constants
    init_cre_web_terms(),

    %% Initialize CRE master constants
    init_cre_master_terms(),

    ok.

%%--------------------------------------------------------------------
%% @doc Reloads all persistent terms with current values.
%%      Useful for runtime configuration updates.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Reloads all persistent terms with current values.

This function re-initializes all configuration values. Useful for
runtime configuration updates.

## Examples

```erlang
1> cre_config:init().
ok
2> cre_config:set(cre_auth_pbkdf2_iterations, 50000).
true
3> cre_config:reload().
ok
4> cre_config:get(cre_auth_pbkdf2_iterations).
100000
```
""").
-spec reload() -> ok.

reload() ->
    init().

%%--------------------------------------------------------------------
%% @doc Gets a persistent term value by key.
%%      Returns default if key doesn't exist.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets a persistent term value by key.

Returns the value stored for the given key. Raises `badarg` if the
key does not exist.

## Examples

```erlang
1> cre_config:init().
ok
2> cre_config:get(cre_default_port).
4142
3> cre_config:get(cre_auth_pbkdf2_iterations).
100000
```
""").
-spec get(key()) -> value().

get(Key) ->
    persistent_term:get(Key).

%%--------------------------------------------------------------------
%% @doc Gets a persistent term value by key with explicit default.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets a persistent term value by key with explicit default.

Returns the value stored for the given key, or `Default` if the key
does not exist.

## Examples

```erlang
1> cre_config:init().
ok
2> cre_config:get(cre_default_port, 8080).
4142
3> cre_config:get(nonexistent_key, default_value).
default_value
```
""").
-spec get(key(), value()) -> value().

get(Key, Default) ->
    try
        persistent_term:get(Key)
    catch
        error:badarg -> Default;
        error:{badarg, _} -> Default
    end.

%%--------------------------------------------------------------------
%% @doc Sets a persistent term key to a value.
%%      Use sparingly - persistent_term is for constants.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Sets a persistent term key to a value.

Sets the value for the given key in persistent_term storage.
Use sparingly - persistent_term is intended for constants.

## Examples

```erlang
1> cre_config:set(custom_key, custom_value).
true
2> cre_config:get(custom_key).
custom_value
```
""").
-spec set(key(), value()) -> true.

set(Key, Value) ->
    persistent_term:put(Key, Value),
    true.

%%--------------------------------------------------------------------
%% @doc Returns all CRE persistent term keys and their values.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Returns all CRE persistent term keys and their values.

Returns a proplist of all known configuration keys and their current
values.

## Examples

```erlang
1> cre_config:init().
ok
2> AllConfig = cre_config:get_all().
3> proplists:get_value(cre_default_port, AllConfig).
4142
```
""").
-spec get_all() -> [{key(), value()}].

get_all() ->
    Keys = [
        ?AUTH_PBKDF2_ITERATIONS,
        ?AUTH_DEFAULT_SESSION_TIMEOUT,
        ?AUTH_MIN_PASSWORD_LENGTH,
        ?YAWL_STATELESS_CHECKPOINT_DIR,
        ?YAWL_STATELESS_MAX_EXECUTIONS,
        ?YAWL_STATELESS_EXECUTION_TTL,
        ?YAWL_STATELESS_TTL_CLEANUP_INTERVAL,
        ?YAWL_PATTERNS_PLACE_LST,
        ?YAWL_PATTERNS_TRSN_LST,
        ?YAWL_TIMEOUT_CHECKPOINT_DIR,
        ?YAWL_TIMEOUT_DEFAULT_TIMEOUT,
        ?YAWL_TIMEOUT_DEADLOCK_INTERVAL,
        ?YAWL_TIMEOUT_RESOURCE_CHECK_INTERVAL,
        ?CRE_CLIENT_POLL_INTERVAL,
        ?CRE_DEFAULT_PORT,
        ?CRE_STATUS_ROUTE,
        ?CRE_HISTORY_ROUTE,
        ?CRE_MASTER_LOG_MODULE
    ],
    [{Key, get(Key, undefined)} || Key <- Keys].

%%--------------------------------------------------------------------
%% @doc Runs doctests for the cre_config module.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Run doctests for the cre_config module.

This function provides a simple test entry point that verifies
basic configuration functionality including:
- Initializing configuration values
- Getting configuration values
- Getting configuration with default values
- Setting custom configuration values
- Getting all configuration

## Examples

```erlang
1> cre_config:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Initialize configuration
    ok = init(),

    %% Test 2: Get authentication configuration
    100000 = get(cre_auth_pbkdf2_iterations),
    3600 = get(cre_auth_default_session_timeout),
    8 = get(cre_auth_min_password_length),

    %% Test 3: Get YAWL stateless configuration
    1000 = get(yawl_stateless_max_executions),
    3600000 = get(yawl_stateless_execution_ttl),

    %% Test 4: Get YAWL timeout configuration
    30000 = get(yawl_timeout_default_timeout),
    5000 = get(yawl_timeout_deadlock_interval),

    %% Test 5: Get web configuration
    4142 = get(cre_default_port),
    "/[status.json]" = get(cre_status_route),
    "/history.json" = get(cre_history_route),

    %% Test 6: Get with default value for nonexistent key
    default_value = get(nonexistent_config_key, default_value),

    %% Test 7: Set and get custom value
    true = set(doctest_custom_key, doctest_custom_value),
    doctest_custom_value = get(doctest_custom_key),

    %% Test 8: Get all configuration
    AllConfig = get_all(),
    true = is_list(AllConfig),
    true = length(AllConfig) >= 10,

    %% Test 9: Verify place list is a list of atoms
    PlaceList = get(yawl_patterns_place_lst),
    true = is_list(PlaceList),
    true = lists:all(fun is_atom/1, PlaceList),

    %% Test 10: Verify transition list is a list of atoms
    TrsnList = get(yawl_patterns_trsn_lst),
    true = is_list(TrsnList),
    true = lists:all(fun is_atom/1, TrsnList),

    %% Test 11: Reload configuration
    ok = reload(),

    %% Test 12: Verify values after reload
    4142 = get(cre_default_port),

    %% Clean up test values
    persistent_term:erase(doctest_custom_key),

    ok.

%%====================================================================
%% Internal Initialization Functions
%%====================================================================

%% @private
init_auth_terms() ->
    %% Password hashing iterations - PBKDF2-HMAC-SHA256
    %% 100,000 iterations provides strong security while maintaining performance
    persistent_term:put(?AUTH_PBKDF2_ITERATIONS, 100000),

    %% Default session timeout in seconds (1 hour)
    persistent_term:put(?AUTH_DEFAULT_SESSION_TIMEOUT, 3600),

    %% Minimum password length requirement
    persistent_term:put(?AUTH_MIN_PASSWORD_LENGTH, 8),

    ok.

%% @private
init_yawl_stateless_terms() ->
    %% Default checkpoint directory
    CheckpointDir = case code:lib_dir(cre) of
        {error, bad_name} -> "priv/checkpoints";
        Dir -> Dir ++ "/priv/checkpoints"
    end,
    persistent_term:put(?YAWL_STATELESS_CHECKPOINT_DIR, CheckpointDir),

    %% Maximum concurrent stateless executions
    persistent_term:put(?YAWL_STATELESS_MAX_EXECUTIONS, 1000),

    %% Execution TTL in milliseconds (1 hour)
    persistent_term:put(?YAWL_STATELESS_EXECUTION_TTL, 3600000),

    %% TTL cleanup interval in milliseconds (1 minute)
    persistent_term:put(?YAWL_STATELESS_TTL_CLEANUP_INTERVAL, 60000),

    ok.

%% @private
init_yawl_patterns_terms() ->
    %% Static place list for YAWL patterns Petri net
    PlaceList = [
        %% Implicit Termination (WCP-11)
        'p_start', 'p_active', 'p_work', 'p_work_pending', 'p_terminate',

        %% Multiple Instances (WCP-12, WCP-13, WCP-14, WCP-15)
        'p_instance_pool', 'p_ready', 'p_running', 'p_done', 'p_complete',
        'p_spawn_pending', 'p_all_spawned', 'p_eval', 'p_data_source', 'p_final',

        %% Deferred Choice (WCP-16)
        'p_choice_pending', 'p_option_a', 'p_option_b', 'p_options',
        'p_selected', 'p_discarded', 'p_choice_complete',

        %% Interleaved Routing (WCP-17)
        'p_branch_pool', 'p_next_branch', 'p_executing', 'p_branch_done',
        'p_all_done', 'p_interleave_complete'
    ],
    persistent_term:put(?YAWL_PATTERNS_PLACE_LST, PlaceList),

    %% Static transition list for YAWL patterns Petri net
    TrsnList = [
        %% Implicit Termination (WCP-11)
        't_activate', 't_queue_work', 't_dequeue_work', 't_implicit_term',

        %% Multiple Instances - No Sync (WCP-12)
        't_spawn_no_sync', 't_execute_no_sync', 't_complete_no_sync',

        %% Multiple Instances - Static (Design Time) (WCP-13)
        't_spawn_all_static', 't_execute_static', 't_collect_static', 't_join_static',

        %% Multiple Instances - Runtime (WCP-14)
        't_eval_count', 't_spawn_runtime', 't_execute_runtime', 't_collect_runtime',
        't_join_runtime',

        %% Multiple Instances - Dynamic (WCP-15)
        't_spawn_dynamic', 't_fetch_data', 't_execute_dynamic', 't_collect_dynamic',
        't_check_done', 't_terminate_dynamic',

        %% Deferred Choice (WCP-16)
        't_offer_choice', 't_select_a', 't_select_b', 't_discard_a', 't_discard_b',
        't_complete_choice',

        %% Interleaved Routing (WCP-17)
        't_distribute_branches', 't_pick_branch', 't_execute_branch',
        't_return_branch', 't_complete_interleaved'
    ],
    persistent_term:put(?YAWL_PATTERNS_TRSN_LST, TrsnList),

    ok.

%% @private
init_yawl_timeout_terms() ->
    %% Checkpoint directory for timeout state persistence
    CheckpointDir = case code:lib_dir(cre) of
        {error, bad_name} -> "priv/yawl_checkpoints";
        Dir -> Dir ++ "/priv/yawl_checkpoints"
    end,
    persistent_term:put(?YAWL_TIMEOUT_CHECKPOINT_DIR, CheckpointDir),

    %% Default timeout for pattern execution (30 seconds)
    persistent_term:put(?YAWL_TIMEOUT_DEFAULT_TIMEOUT, 30000),

    %% Deadlock detection interval (5 seconds)
    persistent_term:put(?YAWL_TIMEOUT_DEADLOCK_INTERVAL, 5000),

    %% Resource leak check interval (1 minute)
    persistent_term:put(?YAWL_TIMEOUT_RESOURCE_CHECK_INTERVAL, 60000),

    ok.

%% @private
init_cre_client_terms() ->
    %% Client poll interval in milliseconds
    persistent_term:put(?CRE_CLIENT_POLL_INTERVAL, 250),

    ok.

%% @private
init_cre_web_terms() ->
    %% Default HTTP port for CRE status service
    persistent_term:put(?CRE_DEFAULT_PORT, 4142),

    %% Status endpoint route
    persistent_term:put(?CRE_STATUS_ROUTE, "/[status.json]"),

    %% History endpoint route
    persistent_term:put(?CRE_HISTORY_ROUTE, "/history.json"),

    ok.

%% @private
init_cre_master_terms() ->
    %% Module for CRE master logging
    persistent_term:put(?CRE_MASTER_LOG_MODULE, cre),

    ok.
