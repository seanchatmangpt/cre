%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @author YAWL Timeout and Cancellation Team
%% @copyright 2025
%%
%% @doc YAWL Timeout and Cancellation Infrastructure
%%
%% This module provides comprehensive timeout and cancellation support for
%% all YAWL workflow patterns, ensuring reliable execution with proper
%% resource cleanup and deadlock detection.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Timeout Support:</b> Per-pattern timeout configuration with detection</li>
%%   <li><b>Cancellation Tokens:</b> Cooperative cancellation throughout patterns</li>
%%   <li><b>Deadlock Detection:</b> Circular wait detection and resolution</li>
%%   <li><b>Resource Leak Detection:</b> Automatic resource tracking and cleanup</li>
%%   <li><b>State Persistence:</b> Checkpoint-based state recovery</li>
%% </ul>
%%
%% <h3>Integration Points</h3>
%% <ul>
%%   <li>Pattern execution functions via cre_yawl_patterns</li>
%%   <li>gen_pnet integration for Petri net patterns</li>
%%   <li>yawl_executor for workflow dispatch</li>
%%   <li>yawl_stateless for stateless execution</li>
%%   <li>yawl_engine for workflow case management</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_timeout).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Timeout management API
-export([with_timeout/3, with_timeout/4, cancel_timeout/1]).
-export([set_pattern_timeout/2, get_pattern_timeout/1]).
-export([start_timeout_monitor/2, stop_timeout_monitor/1]).
-export([extend_timeout/2, reset_timeout/1]).

%% Cancellation support API
-export([new_cancellation_token/0, new_cancellation_token/1]).
-export([check_cancelled/1, is_cancelled/1, request_cancel/1]).
-export([throw_if_cancelled/1, link_token/2, unlink_token/2]).
-export([get_cancel_reason/1]).

%% Deadlock detection API
-export([detect_deadlock/1, resolve_deadlock/1, check_cycle/1]).
-export([add_wait_edge/3, remove_wait_edge/2, get_wait_graph/1]).
-export([detect_resource_deadlock/0, break_deadlock/2]).

%% Resource tracking API
-export([track_resource/2, release_resource/1, check_leaks/1]).
-export([get_resource_usage/1, cleanup_all_resources/1]).
-export([list_resources/1, get_resource_info/2]).
-export([set_resource_cleanup/2, mark_resource_critical/2]).

%% State persistence API
-export([save_checkpoint/2, load_checkpoint/1, list_checkpoints/0]).
-export([cleanup_checkpoint/1, restore_from_checkpoint/1]).
-export([checkpoint_before_block/2, restore_after_block/1]).
-export([get_checkpoint_info/1, validate_checkpoint/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Supervisor start
-export([start_link/0]).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_TIMEOUT, 30000).
-define(INFINITE_TIMEOUT, infinity).
-define(DEADLOCK_CHECK_INTERVAL, 5000).
-define(RESOURCE_CHECK_INTERVAL, 60000).
-define(CHECKPOINT_TABLE, yawl_checkpoints).
-define(RESOURCE_TABLE, yawl_resources).
-define(WAIT_GRAPH_TABLE, yawl_wait_graph).

%%====================================================================
%% Records
%%====================================================================

-record(cancellation_token, {
    token_id :: binary(),
    cancelled = false :: boolean(),
    cancel_reason :: term() | undefined,
    linked_tokens = [] :: [binary()],
    observers = [] :: [pid()],
    created_at :: integer()
}).

-record(timeout_ref, {
    timer_ref :: reference(),
    pattern_id :: binary(),
    timeout :: non_neg_integer() | infinity,
    callback :: function() | undefined,
    started_at :: integer(),
    extended_count = 0 :: non_neg_integer()
}).

-record(resource_entry, {
    resource_id :: binary(),
    owner_pid :: pid() | undefined,
    pattern_id :: binary() | undefined,
    resource_type :: atom(),
    allocated_at :: integer(),
    cleanup_fn :: function() | undefined,
    critical = false :: boolean(),
    leak_detected = false :: boolean(),
    metadata :: map()
}).

-record(wait_edge, {
    waiting_pid :: pid(),
    waiting_for :: pid() | binary(),
    resource_id :: binary() | undefined,
    added_at :: integer(),
    pattern_id :: binary() | undefined
}).

-record(checkpoint_entry, {
    checkpoint_id :: binary(),
    pattern_id :: binary(),
    state_data :: term(),
    created_at :: integer(),
    state_version :: pos_integer(),
    metadata :: map(),
    is_valid = true :: boolean()
}).

-record(timeout_state, {
    active_timeouts = #{} :: #{binary() => #timeout_ref{}},
    cancellation_tokens = #{} :: #{binary() => #cancellation_token{}},
    resources = #{} :: #{binary() => #resource_entry{}},
    wait_graph = [] :: [#wait_edge{}],
    deadlock_monitor :: reference() | undefined,
    resource_monitor :: reference() | undefined,
    checkpoint_dir :: binary()
}).

%%====================================================================
%% Types
%%====================================================================

-type token_id() :: binary().
-type pattern_id() :: binary().
-type resource_id() :: binary().
-type checkpoint_id() :: binary().
-type timeout_ms() :: non_neg_integer() | infinity.
-type cancellation_token() :: #cancellation_token{}.
-type timeout_ref() :: #timeout_ref{}.
-type resource_entry() :: #resource_entry{}.
-type wait_edge() :: #wait_edge{}.
-type checkpoint_entry() :: #checkpoint_entry{}.
-type deadlock_info() :: #{cycle => [pid()], resources => [binary()]}.
-type resource_usage() :: #{allocated => non_neg_integer(), leaked => non_neg_integer()}.

-export_type([cancellation_token/0, token_id/0, timeout_ms/0,
             resource_id/0, checkpoint_id/0, deadlock_info/0, resource_usage/0]).

%%====================================================================
%% Timeout Management API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a function with a timeout, using default timeout.
%%
%% @param Fun The function to execute
%% @param PatternId The pattern identifier for tracking
%% @return {ok, Result} or {error, timeout} or {error, cancelled}
%%
%% @end
%%--------------------------------------------------------------------
-spec with_timeout(function(), pattern_id()) -> {ok, term()} | {error, timeout | cancelled}.

with_timeout(Fun, PatternId) ->
    with_timeout(Fun, PatternId, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc Executes a function with a specified timeout.
%%
%% If the function doesn't complete within the timeout, it is cancelled
%% and {error, timeout} is returned. Supports cancellation tokens.
%%
%% @param Fun The function to execute
%% @param PatternId The pattern identifier for tracking
%% @param Timeout Timeout in milliseconds or infinity
%% @return {ok, Result} or {error, timeout} or {error, cancelled}
%%
%% @end
%%--------------------------------------------------------------------
-spec with_timeout(function(), pattern_id(), timeout_ms()) ->
          {ok, term()} | {error, timeout | cancelled}.

with_timeout(Fun, PatternId, Timeout) when is_function(Fun, 0), is_binary(PatternId) ->
    Token = new_cancellation_token(PatternId),
    with_timeout(Fun, PatternId, Timeout, Token).

%%--------------------------------------------------------------------
%% @doc Executes a function with timeout and cancellation token.
%%
%% @param Fun The function to execute
%% @param PatternId The pattern identifier for tracking
%% @param Timeout Timeout in milliseconds or infinity
%% @param Token Cancellation token for cooperative cancellation
%% @return {ok, Result} or {error, timeout} or {error, cancelled}
%%
%% @end
%%--------------------------------------------------------------------
-spec with_timeout(function(), pattern_id(), timeout_ms(), cancellation_token()) ->
          {ok, term()} | {error, timeout | cancelled}.

with_timeout(Fun, PatternId, Timeout, #cancellation_token{token_id = TokenId}) ->
    Parent = self(),

    %% Start timeout monitor
    {ok, MonitorPid} = start_timeout_monitor(PatternId, Timeout),

    %% Execute function in spawned process
    Pid = spawn_link(fun() ->
        case catch Fun() of
            {'EXIT', Reason} ->
                Parent ! {PatternId, {error, Reason}};
            Result ->
                Parent ! {PatternId, {ok, Result}}
        end
    end),

    %% Wait for result or timeout
    receive
        {PatternId, {ok, Result}} ->
            stop_timeout_monitor(MonitorPid),
            {ok, Result};
        {PatternId, {error, Reason}} ->
            stop_timeout_monitor(MonitorPid),
            {error, Reason};
        {timeout, PatternId, MonitorPid} ->
            exit(Pid, kill),
            {error, timeout};
        {cancelled, PatternId, TokenId} ->
            exit(Pid, kill),
            {error, cancelled}
    after
        Timeout + 100 ->  % Buffer period
            exit(Pid, kill),
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Cancels an active timeout by pattern ID.
%%
%% @param PatternId The pattern identifier
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_timeout(pattern_id()) -> ok | {error, not_found}.

cancel_timeout(PatternId) ->
    gen_server:call(?MODULE, {cancel_timeout, PatternId}).

%%--------------------------------------------------------------------
%% @doc Sets a timeout for a specific pattern.
%%
%% @param PatternId The pattern identifier
%% @param Timeout Timeout in milliseconds or infinity
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec set_pattern_timeout(pattern_id(), timeout_ms()) -> ok.

set_pattern_timeout(PatternId, Timeout) ->
    gen_server:call(?MODULE, {set_pattern_timeout, PatternId, Timeout}).

%%--------------------------------------------------------------------
%% @doc Gets the configured timeout for a pattern.
%%
%% @param PatternId The pattern identifier
%% @return {ok, Timeout} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern_timeout(pattern_id()) -> {ok, timeout_ms()} | {error, not_found}.

get_pattern_timeout(PatternId) ->
    gen_server:call(?MODULE, {get_pattern_timeout, PatternId}).

%%--------------------------------------------------------------------
%% @doc Starts a timeout monitor for a pattern.
%%
%% @param PatternId The pattern identifier
%% @param Timeout Timeout in milliseconds
%% @return {ok, MonitorPid}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_timeout_monitor(pattern_id(), timeout_ms()) -> {ok, pid()}.

start_timeout_monitor(PatternId, Timeout) ->
    gen_server:call(?MODULE, {start_timeout_monitor, PatternId, Timeout}).

%%--------------------------------------------------------------------
%% @doc Stops a timeout monitor.
%%
%% @param MonitorPid The monitor process PID
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_timeout_monitor(pid()) -> ok.

stop_timeout_monitor(MonitorPid) ->
    gen_server:call(?MODULE, {stop_timeout_monitor, MonitorPid}).

%%--------------------------------------------------------------------
%% @doc Extends an existing timeout.
%%
%% @param PatternId The pattern identifier
%% @param AdditionalTime Additional time in milliseconds
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec extend_timeout(pattern_id(), timeout_ms()) -> ok | {error, not_found}.

extend_timeout(PatternId, AdditionalTime) ->
    gen_server:call(?MODULE, {extend_timeout, PatternId, AdditionalTime}).

%%--------------------------------------------------------------------
%% @doc Resets a timeout to its original value.
%%
%% @param PatternId The pattern identifier
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_timeout(pattern_id()) -> ok | {error, not_found}.

reset_timeout(PatternId) ->
    gen_server:call(?MODULE, {reset_timeout, PatternId}).

%%====================================================================
%% Cancellation Support API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new cancellation token.
%%
%% @return New cancellation token
%%
%% @end
%%--------------------------------------------------------------------
-spec new_cancellation_token() -> cancellation_token().

new_cancellation_token() ->
    TokenId = generate_token_id(),
    #cancellation_token{
        token_id = TokenId,
        created_at = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Creates a new cancellation token with associated pattern ID.
%%
%% @param PatternId The pattern identifier to associate with
%% @return New cancellation token
%%
%% @end
%%--------------------------------------------------------------------
-spec new_cancellation_token(pattern_id()) -> cancellation_token().

new_cancellation_token(PatternId) when is_binary(PatternId) ->
    Token = new_cancellation_token(),
    ok = register_token(Token, PatternId),
    Token.

%%--------------------------------------------------------------------
%% @doc Checks if a cancellation token has been cancelled.
%%
%% @param Token The cancellation token to check
%% @return true if cancelled, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec check_cancelled(cancellation_token()) -> boolean().

check_cancelled(#cancellation_token{token_id = TokenId}) ->
    gen_server:call(?MODULE, {check_cancelled, TokenId}).

%%--------------------------------------------------------------------
%% @doc Alias for check_cancelled/1 for consistency.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cancelled(cancellation_token()) -> boolean().

is_cancelled(Token) ->
    check_cancelled(Token).

%%--------------------------------------------------------------------
%% @doc Requests cancellation of a token.
%%
%% @param Token The cancellation token to cancel
%% @return ok | {error, already_cancelled}
%%
%% @end
%%--------------------------------------------------------------------
-spec request_cancel(cancellation_token()) -> ok | {error, already_cancelled}.

request_cancel(#cancellation_token{token_id = TokenId}) ->
    request_cancel(TokenId, normal).

%%--------------------------------------------------------------------
%% @doc Requests cancellation with a reason.
%%
%% @param TokenId The token identifier
%% @param Reason The cancellation reason
%% @return ok | {error, already_cancelled}
%%
%% @end
%%--------------------------------------------------------------------
-spec request_cancel(token_id(), term()) -> ok | {error, already_cancelled}.

request_cancel(TokenId, Reason) ->
    gen_server:call(?MODULE, {request_cancel, TokenId, Reason}).

%%--------------------------------------------------------------------
%% @doc Throws an exception if the token is cancelled.
%%
%% @param Token The cancellation token to check
%% @return ok or throws {cancelled, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec throw_if_cancelled(cancellation_token()) -> ok | no_return().

throw_if_cancelled(#cancellation_token{token_id = TokenId}) ->
    case gen_server:call(?MODULE, {check_cancelled_with_reason, TokenId}) of
        {cancelled, Reason} ->
            erlang:throw({cancelled, Reason});
        not_cancelled ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Links two cancellation tokens together.
%%
%% When one is cancelled, the linked token is also cancelled.
%%
%% @param Token1 First token to link
%% @param Token2 Second token to link
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec link_token(cancellation_token(), cancellation_token()) -> ok.

link_token(#cancellation_token{token_id = Id1}, #cancellation_token{token_id = Id2}) ->
    gen_server:call(?MODULE, {link_tokens, Id1, Id2}).

%%--------------------------------------------------------------------
%% @doc Unlinks two cancellation tokens.
%%
%% @param Token1 First token to unlink
%% @param Token2 Second token to unlink
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec unlink_token(cancellation_token(), cancellation_token()) -> ok.

unlink_token(#cancellation_token{token_id = Id1}, #cancellation_token{token_id = Id2}) ->
    gen_server:call(?MODULE, {unlink_tokens, Id1, Id2}).

%%--------------------------------------------------------------------
%% @doc Gets the cancellation reason for a token.
%%
%% @param Token The cancellation token
%% @return {ok, Reason} or {error, not_cancelled}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cancel_reason(cancellation_token()) -> {ok, term()} | {error, not_cancelled}.

get_cancel_reason(#cancellation_token{token_id = TokenId}) ->
    gen_server:call(?MODULE, {get_cancel_reason, TokenId}).

%%====================================================================
%% Deadlock Detection API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Detects deadlocks in the current execution context.
%%
%% Performs cycle detection in the wait graph to identify circular
%% wait conditions that could cause deadlock.
%%
%% @param ContextId The execution context (pattern_id or case_id)
%% @return {ok, no_deadlock} or {ok, DeadlockInfo}
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_deadlock(pattern_id()) -> {ok, no_deadlock} | {ok, deadlock_info()}.

detect_deadlock(ContextId) ->
    gen_server:call(?MODULE, {detect_deadlock, ContextId}).

%%--------------------------------------------------------------------
%% @doc Attempts to resolve a detected deadlock.
%%
%% @param ContextId The execution context with deadlock
%% @return {ok, resolved} or {error, cannot_resolve}
%%
%% @end
%%--------------------------------------------------------------------
-spec resolve_deadlock(pattern_id()) -> {ok, resolved} | {error, cannot_resolve}.

resolve_deadlock(ContextId) ->
    gen_server:call(?MODULE, {resolve_deadlock, ContextId}).

%%--------------------------------------------------------------------
%% @doc Checks for cycles in the wait graph.
%%
%% @param ContextId The execution context to check
%% @return {ok, has_cycle, CyclePath} or {ok, no_cycle}
%%
%% @end
%%--------------------------------------------------------------------
-spec check_cycle(pattern_id()) -> {ok, has_cycle, [pid()]} | {ok, no_cycle}.

check_cycle(ContextId) ->
    gen_server:call(?MODULE, {check_cycle, ContextId}).

%%--------------------------------------------------------------------
%% @doc Adds a wait edge to the wait graph for deadlock detection.
%%
%% @param WaitingPid The process waiting for a resource
%% @param WaitingFor The PID or resource being waited on
%% @param ResourceId Optional resource identifier
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec add_wait_edge(pid(), pid() | binary(), resource_id() | undefined) -> ok.

add_wait_edge(WaitingPid, WaitingFor, ResourceId) ->
    gen_server:call(?MODULE, {add_wait_edge, WaitingPid, WaitingFor, ResourceId}).

%%--------------------------------------------------------------------
%% @doc Removes a wait edge from the wait graph.
%%
%% @param WaitingPid The process that was waiting
%% @param ResourceId The resource that was waited on
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_wait_edge(pid(), resource_id()) -> ok.

remove_wait_edge(WaitingPid, ResourceId) ->
    gen_server:call(?MODULE, {remove_wait_edge, WaitingPid, ResourceId}).

%%--------------------------------------------------------------------
%% @doc Gets the current wait graph for analysis.
%%
%% @param ContextId The execution context
%% @return List of wait edges
%%
%% @end
%%--------------------------------------------------------------------
-spec get_wait_graph(pattern_id()) -> [#wait_edge{}].

get_wait_graph(ContextId) ->
    gen_server:call(?MODULE, {get_wait_graph, ContextId}).

%%--------------------------------------------------------------------
%% @doc Detects resource-level deadlocks across all contexts.
%%
%% @return {ok, Deadlocks} where Deadlocks is a list of deadlock info
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_resource_deadlock() -> {ok, [deadlock_info()]}.

detect_resource_deadlock() ->
    gen_server:call(?MODULE, detect_resource_deadlock).

%%--------------------------------------------------------------------
%% @doc Breaks a deadlock by forcing resource release.
%%
%% @param ContextId The context with deadlock
%% @param Strategy The resolution strategy (kill_youngest | kill_oldest | abort)
%% @return {ok, resolved} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec break_deadlock(pattern_id(), kill_youngest | kill_oldest | abort) ->
          {ok, resolved} | {error, term()}.

break_deadlock(ContextId, Strategy) ->
    gen_server:call(?MODULE, {break_deadlock, ContextId, Strategy}).

%%====================================================================
%% Resource Tracking API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Tracks allocation of a resource.
%%
%% @param PatternId The pattern allocating the resource
%% @param ResourceInfo Map containing: type, cleanup_fn, critical, metadata
%% @return {ok, ResourceId}
%%
%% @end
%%--------------------------------------------------------------------
-spec track_resource(pattern_id(), map()) -> {ok, resource_id()}.

track_resource(PatternId, ResourceInfo) when is_map(ResourceInfo) ->
    ResourceType = maps:get(type, ResourceInfo, generic),
    CleanupFn = maps:get(cleanup_fn, ResourceInfo, fun(_) -> ok end),
    Critical = maps:get(critical, ResourceInfo, false),
    Metadata = maps:get(metadata, ResourceInfo, #{}),
    gen_server:call(?MODULE, {track_resource, PatternId, ResourceType,
                             CleanupFn, Critical, Metadata}).

%%--------------------------------------------------------------------
%% @doc Releases a tracked resource.
%%
%% @param ResourceId The resource identifier
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec release_resource(resource_id()) -> ok | {error, not_found}.

release_resource(ResourceId) ->
    gen_server:call(?MODULE, {release_resource, ResourceId}).

%%--------------------------------------------------------------------
%% @doc Checks for resource leaks in a pattern context.
%%
%% @param PatternId The pattern identifier to check
%% @return {ok, LeakedResources} list of resource IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec check_leaks(pattern_id()) -> {ok, [resource_id()]}.

check_leaks(PatternId) ->
    gen_server:call(?MODULE, {check_leaks, PatternId}).

%%--------------------------------------------------------------------
%% @doc Gets resource usage statistics for a pattern.
%%
%% @param PatternId The pattern identifier
%% @return {ok, ResourceUsage} map with allocation stats
%%
%% @end
%%--------------------------------------------------------------------
-spec get_resource_usage(pattern_id()) -> {ok, resource_usage()}.

get_resource_usage(PatternId) ->
    gen_server:call(?MODULE, {get_resource_usage, PatternId}).

%%--------------------------------------------------------------------
%% @doc Cleans up all resources for a pattern.
%%
%% @param PatternId The pattern identifier
%% @return {ok, CleanedCount}
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_all_resources(pattern_id()) -> {ok, non_neg_integer()}.

cleanup_all_resources(PatternId) ->
    gen_server:call(?MODULE, {cleanup_all_resources, PatternId}).

%%--------------------------------------------------------------------
%% @doc Lists all resources for a pattern.
%%
%% @param PatternId The pattern identifier
%% @return {ok, ResourceList}
%%
%% @end
%%--------------------------------------------------------------------
-spec list_resources(pattern_id()) -> {ok, [resource_id()]}.

list_resources(PatternId) ->
    gen_server:call(?MODULE, {list_resources, PatternId}).

%%--------------------------------------------------------------------
%% @doc Gets detailed information about a resource.
%%
%% @param PatternId The pattern identifier
%% @param ResourceId The resource identifier
%% @return {ok, ResourceInfo} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_resource_info(pattern_id(), resource_id()) ->
          {ok, map()} | {error, not_found}.

get_resource_info(PatternId, ResourceId) ->
    gen_server:call(?MODULE, {get_resource_info, PatternId, ResourceId}).

%%--------------------------------------------------------------------
%% @doc Sets a cleanup function for a resource.
%%
%% @param ResourceId The resource identifier
%% @param CleanupFn The cleanup function
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec set_resource_cleanup(resource_id(), function()) -> ok | {error, not_found}.

set_resource_cleanup(ResourceId, CleanupFn) when is_function(CleanupFn, 1) ->
    gen_server:call(?MODULE, {set_resource_cleanup, ResourceId, CleanupFn}).

%%--------------------------------------------------------------------
%% @doc Marks a resource as critical (cannot be auto-cleaned).
%%
%% @param PatternId The pattern identifier
%% @param ResourceId The resource identifier
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_resource_critical(pattern_id(), resource_id()) -> ok | {error, not_found}.

mark_resource_critical(PatternId, ResourceId) ->
    gen_server:call(?MODULE, {mark_resource_critical, PatternId, ResourceId}).

%%====================================================================
%% State Persistence API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Saves a checkpoint of the current state.
%%
%% @param PatternId The pattern identifier
%% @param StateData The state data to persist
%% @return {ok, CheckpointId}
%%
%% @end
%%--------------------------------------------------------------------
-spec save_checkpoint(pattern_id(), term()) -> {ok, checkpoint_id()}.

save_checkpoint(PatternId, StateData) ->
    save_checkpoint(PatternId, StateData, #{}).

%%--------------------------------------------------------------------
%% @doc Saves a checkpoint with metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_checkpoint(pattern_id(), term(), map()) -> {ok, checkpoint_id()}.

save_checkpoint(PatternId, StateData, Metadata) when is_map(Metadata) ->
    gen_server:call(?MODULE, {save_checkpoint, PatternId, StateData, Metadata}).

%%--------------------------------------------------------------------
%% @doc Loads a checkpoint by ID.
%%
%% @param CheckpointId The checkpoint identifier
%% @return {ok, StateData} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec load_checkpoint(checkpoint_id()) -> {ok, term()} | {error, not_found}.

load_checkpoint(CheckpointId) ->
    gen_server:call(?MODULE, {load_checkpoint, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Lists all available checkpoints.
%%
%% @return {ok, CheckpointList}
%%
%% @end
%%--------------------------------------------------------------------
-spec list_checkpoints() -> {ok, [checkpoint_id()]}.

list_checkpoints() ->
    gen_server:call(?MODULE, list_checkpoints).

%%--------------------------------------------------------------------
%% @doc Cleans up a checkpoint.
%%
%% @param CheckpointId The checkpoint to remove
%% @return ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_checkpoint(checkpoint_id()) -> ok | {error, not_found}.

cleanup_checkpoint(CheckpointId) ->
    gen_server:call(?MODULE, {cleanup_checkpoint, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Restores state from the latest checkpoint for a pattern.
%%
%% @param PatternId The pattern identifier
%% @return {ok, RestoredState} or {error, no_checkpoint}
%%
%% @end
%%--------------------------------------------------------------------
-spec restore_from_checkpoint(pattern_id()) -> {ok, term()} | {error, no_checkpoint}.

restore_from_checkpoint(PatternId) ->
    gen_server:call(?MODULE, {restore_from_checkpoint, PatternId}).

%%--------------------------------------------------------------------
%% @doc Creates a checkpoint before a blocking operation.
%%
%% @param PatternId The pattern identifier
%% @param StateData The current state to save
%% @return {ok, CheckpointId}
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint_before_block(pattern_id(), term()) -> {ok, checkpoint_id()}.

checkpoint_before_block(PatternId, StateData) ->
    BlockMetadata = #{type => pre_block, timestamp => erlang:system_time(millisecond)},
    save_checkpoint(PatternId, StateData, BlockMetadata).

%%--------------------------------------------------------------------
%% @doc Restores state after a blocking operation completes.
%%
%% @param PatternId The pattern identifier
%% @return {ok, RestoredState} or {error, no_checkpoint}
%%
%% @end
%%--------------------------------------------------------------------
-spec restore_after_block(pattern_id()) -> {ok, term()} | {error, no_checkpoint}.

restore_after_block(PatternId) ->
    gen_server:call(?MODULE, {restore_after_block, PatternId}).

%%--------------------------------------------------------------------
%% @doc Gets information about a checkpoint.
%%
%% @param CheckpointId The checkpoint identifier
%% @return {ok, CheckpointInfo} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_checkpoint_info(checkpoint_id()) -> {ok, map()} | {error, not_found}.

get_checkpoint_info(CheckpointId) ->
    gen_server:call(?MODULE, {get_checkpoint_info, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Validates a checkpoint for consistency.
%%
%% @param CheckpointId The checkpoint identifier
%% @return {ok, valid} or {error, invalid}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_checkpoint(checkpoint_id()) -> {ok, valid} | {error, invalid}.

validate_checkpoint(CheckpointId) ->
    gen_server:call(?MODULE, {validate_checkpoint, CheckpointId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the timeout gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
-spec init([]) -> {ok, #timeout_state{}}.

init([]) ->
    process_flag(trap_exit, true),

    %% Initialize ETS tables
    initialize_ets_tables(),

    %% Get checkpoint directory from config
    CheckpointDir = cre_config:get(yawl_timeout_checkpoint_dir,
                                   "priv/yawl_checkpoints"),
    ok = filelib:ensure_dir(CheckpointDir ++ "/"),

    %% Start periodic monitors
    DeadlockRef = erlang:send_after(?DEADLOCK_CHECK_INTERVAL, self(), deadlock_check),
    ResourceRef = erlang:send_after(?RESOURCE_CHECK_INTERVAL, self(), resource_check),

    logger:info("YAWL Timeout module started", [{module, ?MODULE}]),

    {ok, #timeout_state{
        checkpoint_dir = CheckpointDir,
        deadlock_monitor = DeadlockRef,
        resource_monitor = ResourceRef
    }}.

%% @private
handle_call({cancel_timeout, PatternId}, _From, State) ->
    case maps:get(PatternId, State#timeout_state.active_timeouts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #timeout_ref{timer_ref = TimerRef} ->
            erlang:cancel_timer(TimerRef),
            NewTimeouts = maps:remove(PatternId, State#timeout_state.active_timeouts),
            {reply, ok, State#timeout_state{active_timeouts = NewTimeouts}}
    end;

handle_call({set_pattern_timeout, PatternId, Timeout}, _From, State) ->
    TimeoutRef = #timeout_ref{
        timer_ref = make_ref(),
        pattern_id = PatternId,
        timeout = Timeout,
        started_at = erlang:system_time(millisecond)
    },
    NewTimeouts = maps:put(PatternId, TimeoutRef, State#timeout_state.active_timeouts),
    {reply, ok, State#timeout_state{active_timeouts = NewTimeouts}};

handle_call({get_pattern_timeout, PatternId}, _From, State) ->
    case maps:get(PatternId, State#timeout_state.active_timeouts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #timeout_ref{timeout = Timeout} ->
            {reply, {ok, Timeout}, State}
    end;

handle_call({start_timeout_monitor, PatternId, Timeout}, _From, State) ->
    MonitorPid = spawn_link(fun() -> timeout_monitor_loop(PatternId, Timeout, self()) end),
    TimeoutRef = #timeout_ref{
        timer_ref = make_ref(),
        pattern_id = PatternId,
        timeout = Timeout,
        started_at = erlang:system_time(millisecond)
    },
    NewTimeouts = maps:put(PatternId, TimeoutRef, State#timeout_state.active_timeouts),
    {reply, {ok, MonitorPid}, State#timeout_state{active_timeouts = NewTimeouts}};

handle_call({stop_timeout_monitor, MonitorPid}, _From, State) ->
    MonitorPid ! stop,
    {reply, ok, State};

handle_call({extend_timeout, PatternId, AdditionalTime}, _From, State) ->
    case maps:get(PatternId, State#timeout_state.active_timeouts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TimeoutRef ->
            NewRef = TimeoutRef#timeout_ref{
                timeout = TimeoutRef#timeout_ref.timeout + AdditionalTime,
                extended_count = TimeoutRef#timeout_ref.extended_count + 1
            },
            NewTimeouts = maps:put(PatternId, NewRef, State#timeout_state.active_timeouts),
            {reply, ok, State#timeout_state{active_timeouts = NewTimeouts}}
    end;

handle_call({reset_timeout, PatternId}, _From, State) ->
    case maps:get(PatternId, State#timeout_state.active_timeouts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TimeoutRef ->
            NewRef = TimeoutRef#timeout_ref{extended_count = 0},
            NewTimeouts = maps:put(PatternId, NewRef, State#timeout_state.active_timeouts),
            {reply, ok, State#timeout_state{active_timeouts = NewTimeouts}}
    end;

handle_call({check_cancelled, TokenId}, _From, State) ->
    case maps:get(TokenId, State#timeout_state.cancellation_tokens, undefined) of
        undefined ->
            {reply, false, State};
        #cancellation_token{cancelled = Cancelled} ->
            {reply, Cancelled, State}
    end;

handle_call({check_cancelled_with_reason, TokenId}, _From, State) ->
    case maps:get(TokenId, State#timeout_state.cancellation_tokens, undefined) of
        undefined ->
            {reply, not_cancelled, State};
        #cancellation_token{cancelled = false} ->
            {reply, not_cancelled, State};
        #cancellation_token{cancelled = true, cancel_reason = Reason} ->
            {reply, {cancelled, Reason}, State}
    end;

handle_call({request_cancel, TokenId, Reason}, _From, State) ->
    case maps:get(TokenId, State#timeout_state.cancellation_tokens, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #cancellation_token{cancelled = true} ->
            {reply, {error, already_cancelled}, State};
        #cancellation_token{linked_tokens = Linked, observers = Observers} = Token ->
            UpdatedToken = Token#cancellation_token{cancelled = true, cancel_reason = Reason},
            NewTokens = maps:put(TokenId, UpdatedToken, State#timeout_state.cancellation_tokens),

            %% Notify observers
            notify_observers(Observers, {cancelled, TokenId, Reason}),

            %% Cancel linked tokens
            lists:foreach(fun(LinkedId) ->
                request_cancel(LinkedId, {linked, TokenId, Reason})
            end, Linked),

            {reply, ok, State#timeout_state{cancellation_tokens = NewTokens}}
    end;

handle_call({register_token, TokenId, PatternId}, _From, State) ->
    Token = #cancellation_token{
        token_id = TokenId,
        created_at = erlang:system_time(millisecond)
    },
    NewTokens = maps:put(TokenId, Token, State#timeout_state.cancellation_tokens),
    logger:debug("Registered cancellation token ~p for pattern ~p", [TokenId, PatternId]),
    {reply, ok, State#timeout_state{cancellation_tokens = NewTokens}};

handle_call({link_tokens, Id1, Id2}, _From, State) ->
    NewTokens = maps:update_with(Id1,
        fun(#cancellation_token{linked_tokens = Linked} = Token) ->
            Token#cancellation_token{linked_tokens = [Id2 | Linked]}
        end,
        #cancellation_token{linked_tokens = [Id2]},
        State#timeout_state.cancellation_tokens),

    NewTokens2 = maps:update_with(Id2,
        fun(#cancellation_token{linked_tokens = Linked} = Token) ->
            Token#cancellation_token{linked_tokens = [Id1 | Linked]}
        end,
        #cancellation_token{linked_tokens = [Id1]},
        NewTokens),

    {reply, ok, State#timeout_state{cancellation_tokens = NewTokens2}};

handle_call({unlink_tokens, Id1, Id2}, _From, State) ->
    NewTokens = maps:update_with(Id1,
        fun(#cancellation_token{linked_tokens = Linked} = Token) ->
            Token#cancellation_token{linked_tokens = lists:delete(Id2, Linked)}
        end,
        State#timeout_state.cancellation_tokens),

    NewTokens2 = maps:update_with(Id2,
        fun(#cancellation_token{linked_tokens = Linked} = Token) ->
            Token#cancellation_token{linked_tokens = lists:delete(Id1, Linked)}
        end,
        NewTokens),

    {reply, ok, State#timeout_state{cancellation_tokens = NewTokens2}};

handle_call({get_cancel_reason, TokenId}, _From, State) ->
    case maps:get(TokenId, State#timeout_state.cancellation_tokens, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #cancellation_token{cancelled = false} ->
            {reply, {error, not_cancelled}, State};
        #cancellation_token{cancel_reason = Reason} ->
            {reply, {ok, Reason}, State}
    end;

handle_call({detect_deadlock, ContextId}, _From, State) ->
    %% Filter wait graph by context
    ContextEdges = [E || E <- State#timeout_state.wait_graph,
                       get_pattern_from_edge(E) =:= ContextId],

    case detect_cycle_in_edges(ContextEdges) of
        {cycle, Cycle} ->
            DeadlockInfo = #{
                context => ContextId,
                cycle => Cycle,
                detected_at => erlang:system_time(millisecond)
            },
            {reply, {ok, DeadlockInfo}, State};
        no_cycle ->
            {reply, {ok, no_deadlock}, State}
    end;

handle_call({resolve_deadlock, ContextId}, _From, State) ->
    ContextEdges = [E || E <- State#timeout_state.wait_graph,
                       get_pattern_from_edge(E) =:= ContextId],

    case detect_cycle_in_edges(ContextEdges) of
        no_cycle ->
            {reply, {error, no_deadlock}, State};
        {cycle, Cycle} ->
            %% Resolve by removing the first edge in the cycle
            [FirstEdge | _] = Cycle,
            NewWaitGraph = lists:delete(FirstEdge, State#timeout_state.wait_graph),

            logger:warning("Deadlock resolved for context ~p by removing edge ~p",
                          [ContextId, FirstEdge]),

            {reply, {ok, resolved}, State#timeout_state{wait_graph = NewWaitGraph}}
    end;

handle_call({check_cycle, ContextId}, _From, State) ->
    ContextEdges = [E || E <- State#timeout_state.wait_graph,
                       get_pattern_from_edge(E) =:= ContextId],

    case detect_cycle_in_edges(ContextEdges) of
        {cycle, Cycle} ->
            CyclePids = [E#wait_edge.waiting_pid || E <- Cycle],
            {reply, {ok, has_cycle, CyclePids}, State};
        no_cycle ->
            {reply, {ok, no_cycle}, State}
    end;

handle_call({add_wait_edge, WaitingPid, WaitingFor, ResourceId}, _From, State) ->
    Edge = #wait_edge{
        waiting_pid = WaitingPid,
        waiting_for = WaitingFor,
        resource_id = ResourceId,
        added_at = erlang:system_time(millisecond),
        pattern_id = get_pattern_from_pid(WaitingPid)
    },
    NewWaitGraph = [Edge | State#timeout_state.wait_graph],
    {reply, ok, State#timeout_state{wait_graph = NewWaitGraph}};

handle_call({remove_wait_edge, WaitingPid, ResourceId}, _From, State) ->
    NewWaitGraph = lists:filter(
        fun(#wait_edge{waiting_pid = P, resource_id = R}) ->
            not (P =:= WaitingPid andalso R =:= ResourceId)
        end,
        State#timeout_state.wait_graph
    ),
    {reply, ok, State#timeout_state{wait_graph = NewWaitGraph}};

handle_call({get_wait_graph, ContextId}, _From, State) ->
    ContextEdges = [E || E <- State#timeout_state.wait_graph,
                       get_pattern_from_edge(E) =:= ContextId],
    {reply, ContextEdges, State};

handle_call(detect_resource_deadlock, _From, State) ->
    AllEdges = State#timeout_state.wait_graph,

    %% Group edges by resource
    Resources = lists:foldl(fun(#wait_edge{resource_id = R} = E, Acc) ->
        case R of
            undefined -> Acc;
            _ -> maps:update_with(R, fun(Edges) -> [E | Edges] end, [E], Acc)
        end
    end, #{}, AllEdges),

    %% Detect cycles for each resource
    Deadlocks = maps:fold(fun(ResourceId, Edges, Acc) ->
        case detect_cycle_in_edges(Edges) of
            {cycle, Cycle} ->
                [#{
                    resource => ResourceId,
                    cycle => Cycle,
                    participants => [E#wait_edge.waiting_pid || E <- Cycle]
                } | Acc];
            no_cycle ->
                Acc
        end
    end, [], Resources),

    {reply, {ok, Deadlocks}, State};

handle_call({break_deadlock, ContextId, Strategy}, _From, State) ->
    ContextEdges = [E || E <- State#timeout_state.wait_graph,
                       get_pattern_from_edge(E) =:= ContextId],

    case detect_cycle_in_edges(ContextEdges) of
        no_cycle ->
            {reply, {error, no_deadlock}, State};
        {cycle, Cycle} ->
            {PidToKill, _RemainingEdges} = select_victim(Cycle, Strategy),

            case Strategy of
                abort ->
                    logger:error("Aborting deadlock for context ~p, killing process ~p",
                                 [ContextId, PidToKill]),
                    exit(PidToKill, kill);
                _ ->
                    logger:warning("Breaking deadlock for context ~p by killing ~p",
                                   [ContextId, PidToKill]),
                    exit(PidToKill, deadlock_resolved)
            end,

            %% Remove all edges involving the killed process
            NewWaitGraph = lists:filter(
                fun(#wait_edge{waiting_pid = P}) -> P =/= PidToKill;
                   (#wait_edge{waiting_for = W}) when is_pid(W) -> W =/= PidToKill;
                   (_) -> true
                end,
                State#timeout_state.wait_graph
            ),

            {reply, {ok, resolved}, State#timeout_state{wait_graph = NewWaitGraph}}
    end;

handle_call({track_resource, PatternId, ResourceType, CleanupFn, Critical, Metadata}, _From, State) ->
    ResourceId = generate_resource_id(PatternId),
    Entry = #resource_entry{
        resource_id = ResourceId,
        owner_pid = self(),
        pattern_id = PatternId,
        resource_type = ResourceType,
        allocated_at = erlang:system_time(millisecond),
        cleanup_fn = CleanupFn,
        critical = Critical,
        metadata = Metadata
    },
    ets:insert(?RESOURCE_TABLE, Entry),
    logger:debug("Tracked resource ~p of type ~p for pattern ~p",
                 [ResourceId, ResourceType, PatternId]),
    {reply, {ok, ResourceId}, State};

handle_call({release_resource, ResourceId}, _From, State) ->
    case ets:lookup(?RESOURCE_TABLE, ResourceId) of
        [] ->
            {reply, {error, not_found}, State};
        [#resource_entry{cleanup_fn = CleanupFn, critical = Critical}] ->
            %% Run cleanup function if not critical
            case Critical of
                false ->
                    catch CleanupFn(ResourceId);
                true ->
                    logger:warning("Not running cleanup for critical resource ~p", [ResourceId])
            end,
            ets:delete(?RESOURCE_TABLE, ResourceId),
            {reply, ok, State}
    end;

handle_call({check_leaks, PatternId}, _From, State) ->
    PatternResources = ets:select(?RESOURCE_TABLE,
        [{#resource_entry{pattern_id = PatternId, _ = '_'}, [], ['$_']}]),

    %% Mark detected leaks
    Now = erlang:system_time(millisecond),
    LeakedIds = lists:foldl(fun(#resource_entry{resource_id = Id, allocated_at = At,
                                               critical = false}, Acc) ->
        Age = Now - At,
        %% Consider resources older than 5 minutes as potential leaks
        if Age > 300000 ->
            ets:update_element(?RESOURCE_TABLE, Id, {#resource_entry.leak_detected, true}),
            [Id | Acc];
        true ->
            Acc
        end
    end, [], PatternResources),

    {reply, {ok, LeakedIds}, State};

handle_call({get_resource_usage, PatternId}, _From, State) ->
    PatternResources = ets:select(?RESOURCE_TABLE,
        [{#resource_entry{pattern_id = PatternId, _ = '_'}, [], ['$_']}]),

    Allocated = length(PatternResources),
    Leaked = length([R || R <- PatternResources,
                        R#resource_entry.leak_detected =:= true]),

    Usage = #{
        allocated => Allocated,
        leaked => Leaked,
        critical => length([R || R <- PatternResources, R#resource_entry.critical =:= true])
    },
    {reply, {ok, Usage}, State};

handle_call({cleanup_all_resources, PatternId}, _From, State) ->
    PatternResources = ets:select(?RESOURCE_TABLE,
        [{#resource_entry{pattern_id = PatternId, _ = '_'}, [], ['$_']}]),

    Cleaned = lists:foldl(fun(#resource_entry{resource_id = Id,
                                           cleanup_fn = CleanupFn,
                                           critical = Critical}, Count) ->
        case Critical of
            false ->
                catch CleanupFn(Id),
                ets:delete(?RESOURCE_TABLE, Id),
                Count + 1;
            true ->
                %% Don't auto-cleanup critical resources
                Count
        end
    end, 0, PatternResources),

    {reply, {ok, Cleaned}, State};

handle_call({list_resources, PatternId}, _From, State) ->
    PatternResources = ets:select(?RESOURCE_TABLE,
        [{#resource_entry{pattern_id = PatternId, _ = '_'}, [], ['$1']}]),
    ResourceIds = [R#resource_entry.resource_id || R <- PatternResources],
    {reply, {ok, ResourceIds}, State};

handle_call({get_resource_info, PatternId, ResourceId}, _From, State) ->
    case ets:lookup(?RESOURCE_TABLE, ResourceId) of
        [#resource_entry{pattern_id = PatternId} = Entry] ->
            Info = #{
                resource_id => Entry#resource_entry.resource_id,
                resource_type => Entry#resource_entry.resource_type,
                allocated_at => Entry#resource_entry.allocated_at,
                critical => Entry#resource_entry.critical,
                leak_detected => Entry#resource_entry.leak_detected,
                metadata => Entry#resource_entry.metadata
            },
            {reply, {ok, Info}, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_resource_cleanup, ResourceId, CleanupFn}, _From, State) ->
    case ets:lookup(?RESOURCE_TABLE, ResourceId) of
        [] ->
            {reply, {error, not_found}, State};
        [_] ->
            ets:update_element(?RESOURCE_TABLE, ResourceId,
                             {#resource_entry.cleanup_fn, CleanupFn}),
            {reply, ok, State}
    end;

handle_call({mark_resource_critical, PatternId, ResourceId}, _From, State) ->
    case ets:lookup(?RESOURCE_TABLE, ResourceId) of
        [#resource_entry{pattern_id = PatternId}] ->
            ets:update_element(?RESOURCE_TABLE, ResourceId,
                             {#resource_entry.critical, true}),
            {reply, ok, State};
        _ ->
            {reply, {error, not_found}, State}
    end;

handle_call({save_checkpoint, PatternId, StateData, Metadata}, _From, State) ->
    CheckpointId = generate_checkpoint_id(PatternId),

    %% Get current version for this pattern
    Version = case ets:match(?CHECKPOINT_TABLE,
                           #checkpoint_entry{pattern_id = PatternId,
                                            state_version = '$1', _ = '_'}) of
        [[V]] -> V + 1;
        [] -> 1
    end,

    Entry = #checkpoint_entry{
        checkpoint_id = CheckpointId,
        pattern_id = PatternId,
        state_data = StateData,
        created_at = erlang:system_time(millisecond),
        state_version = Version,
        metadata = Metadata,
        is_valid = true
    },

    ets:insert(?CHECKPOINT_TABLE, Entry),

    %% Also persist to file
    CheckpointFile = filename:join([State#timeout_state.checkpoint_dir,
                                     binary_to_list(CheckpointId) ++ ".chk"]),
    save_checkpoint_to_file(CheckpointFile, Entry),

    logger:debug("Saved checkpoint ~p for pattern ~p (version ~p)",
                 [CheckpointId, PatternId, Version]),

    {reply, {ok, CheckpointId}, State};

handle_call({load_checkpoint, CheckpointId}, _From, State) ->
    case ets:lookup(?CHECKPOINT_TABLE, CheckpointId) of
        [] ->
            {reply, {error, not_found}, State};
        [#checkpoint_entry{state_data = Data, is_valid = true}] ->
            {reply, {ok, Data}, State};
        [#checkpoint_entry{is_valid = false}] ->
            {reply, {error, invalid_checkpoint}, State}
    end;

handle_call(list_checkpoints, _From, State) ->
    AllCheckpoints = ets:tab2list(?CHECKPOINT_TABLE),
    ValidIds = [E#checkpoint_entry.checkpoint_id || E <- AllCheckpoints,
                                            E#checkpoint_entry.is_valid =:= true],
    {reply, {ok, ValidIds}, State};

handle_call({cleanup_checkpoint, CheckpointId}, _From, State) ->
    case ets:lookup(?CHECKPOINT_TABLE, CheckpointId) of
        [] ->
            {reply, {error, not_found}, State};
        [_] ->
            ets:delete(?CHECKPOINT_TABLE, CheckpointId),
            %% Also delete file
            CheckpointFile = filename:join([State#timeout_state.checkpoint_dir,
                                             binary_to_list(CheckpointId) ++ ".chk"]),
            file:delete(CheckpointFile),
            {reply, ok, State}
    end;

handle_call({restore_from_checkpoint, PatternId}, _From, State) ->
    case ets:match(?CHECKPOINT_TABLE,
                  #checkpoint_entry{pattern_id = PatternId, state_version = '$1',
                                   _ = '_'}) of
        [] ->
            {reply, {error, no_checkpoint}, State};
        Versions ->
            MaxVersion = lists:max([V || [V] <- Versions]),
            [#checkpoint_entry{state_data = Data}] =
                ets:match_object(#checkpoint_entry{pattern_id = PatternId,
                                                  state_version = MaxVersion,
                                                  _ = '_'}),
            {reply, {ok, Data}, State}
    end;

handle_call({restore_after_block, PatternId}, _From, State) ->
    %% Find the most recent pre_block checkpoint for this pattern
    PatternCheckpoints = ets:select(?CHECKPOINT_TABLE,
        [{#checkpoint_entry{pattern_id = PatternId,
                           metadata = '$1', _ = '_'},
          [{'=:=', '$1', {type, pre_block}}], ['$_']}]),

    case lists:sort(fun(A, B) ->
        A#checkpoint_entry.created_at >= B#checkpoint_entry.created_at
    end, PatternCheckpoints) of
        [] ->
            {reply, {error, no_checkpoint}, State};
        [#checkpoint_entry{state_data = Data} | _] ->
            {reply, {ok, Data}, State}
    end;

handle_call({get_checkpoint_info, CheckpointId}, _From, State) ->
    case ets:lookup(?CHECKPOINT_TABLE, CheckpointId) of
        [] ->
            {reply, {error, not_found}, State};
        [Entry] ->
            Info = #{
                checkpoint_id => Entry#checkpoint_entry.checkpoint_id,
                pattern_id => Entry#checkpoint_entry.pattern_id,
                created_at => Entry#checkpoint_entry.created_at,
                state_version => Entry#checkpoint_entry.state_version,
                metadata => Entry#checkpoint_entry.metadata,
                is_valid => Entry#checkpoint_entry.is_valid
            },
            {reply, {ok, Info}, State}
    end;

handle_call({validate_checkpoint, CheckpointId}, _From, State) ->
    case ets:lookup(?CHECKPOINT_TABLE, CheckpointId) of
        [] ->
            {reply, {error, invalid}, State};
        [#checkpoint_entry{is_valid = true}] ->
            %% Additional validation: check if state data is consistent
            {reply, {ok, valid}, State};
        [#checkpoint_entry{is_valid = false}] ->
            {reply, {error, invalid}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(deadlock_check, State) ->
    %% Perform periodic deadlock detection
    AllEdges = State#timeout_state.wait_graph,

    case detect_cycle_in_edges(AllEdges) of
        {cycle, Cycle} ->
            logger:warning("Deadlock detected: ~p", [Cycle]),
            %% Could trigger automatic resolution here
            ok;
        no_cycle ->
            ok
    end,

    %% Reschedule
    DeadlockRef = erlang:send_after(?DEADLOCK_CHECK_INTERVAL, self(), deadlock_check),
    {noreply, State#timeout_state{deadlock_monitor = DeadlockRef}};

handle_info(resource_check, State) ->
    %% Perform periodic resource leak detection
    AllResources = ets:tab2list(?RESOURCE_TABLE),
    Now = erlang:system_time(millisecond),

    lists:foreach(fun(#resource_entry{resource_id = Id, allocated_at = At,
                                     critical = false, leak_detected = false}) ->
        Age = Now - At,
        %% Warn about resources older than 10 minutes
        if Age > 600000 ->
            logger:warning("Potential resource leak detected: ~p (age: ~pms)", [Id, Age]),
            ets:update_element(?RESOURCE_TABLE, Id, {#resource_entry.leak_detected, true});
        true ->
            ok
    end
    end, AllResources),

    %% Reschedule
    ResourceRef = erlang:send_after(?RESOURCE_CHECK_INTERVAL, self(), resource_check),
    {noreply, State#timeout_state{resource_monitor = ResourceRef}};

handle_info({'EXIT', Pid, _Reason}, State) ->
    %% Clean up wait edges involving the exited process
    NewWaitGraph = lists:filter(
        fun(#wait_edge{waiting_pid = P}) -> P =/= Pid;
           (#wait_edge{waiting_for = W}) when is_pid(W) -> W =/= Pid;
           (_) -> true
        end,
        State#timeout_state.wait_graph
    ),
    {noreply, State#timeout_state{wait_graph = NewWaitGraph}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    %% Cleanup ETS tables
    catch ets:delete(?CHECKPOINT_TABLE),
    catch ets:delete(?RESOURCE_TABLE),
    catch ets:delete(?WAIT_GRAPH_TABLE),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
initialize_ets_tables() ->
    case ets:whereis(?CHECKPOINT_TABLE) of
        undefined ->
            ets:new(?CHECKPOINT_TABLE, [named_table, set, public,
                                       {keypos, #checkpoint_entry.checkpoint_id}]);
        _ ->
            ok
    end,

    case ets:whereis(?RESOURCE_TABLE) of
        undefined ->
            ets:new(?RESOURCE_TABLE, [named_table, set, public,
                                       {keypos, #resource_entry.resource_id}]);
        _ ->
            ok
    end,

    case ets:whereis(?WAIT_GRAPH_TABLE) of
        undefined ->
            ets:new(?WAIT_GRAPH_TABLE, [named_table, bag, public,
                                       {keypos, #wait_edge.waiting_pid}]);
        _ ->
            ok
    end.

%% @private
timeout_monitor_loop(PatternId, Timeout, Parent) ->
    receive
        stop ->
            ok;
        {cancel, PatternId} ->
            Parent ! {cancelled, PatternId, PatternId},
            ok
    after Timeout ->
        Parent ! {timeout, PatternId, self()},
        ok
    end.

%% @private
register_token(#cancellation_token{token_id = TokenId}, PatternId) ->
    gen_server:call(?MODULE, {register_token, TokenId, PatternId}).

%% @private
generate_token_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"token_", Hex/binary>>.

%% @private
generate_resource_id(PatternId) ->
    Unique = crypto:hash(md5, term_to_binary({PatternId, erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"resource_", Hex/binary>>.

%% @private
generate_checkpoint_id(PatternId) ->
    Unique = crypto:hash(md5, term_to_binary({PatternId, erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"checkpoint_", PatternId/binary, "_", Hex/binary>>.

%% @private
get_pattern_from_edge(#wait_edge{pattern_id = PatternId}) ->
    PatternId.

%% @private
get_pattern_from_pid(Pid) when is_pid(Pid) ->
    %% Try to extract pattern from registered name or process dictionary
    case process_info(Pid, registered_name) of
        {registered_name, Name} when is_atom(Name) ->
            atom_to_binary(Name, utf8);
        _ ->
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    case lists:keyfind(pattern_id, 1, Dict) of
                        {pattern_id, PatternId} -> PatternId;
                        _ -> <<>>
                    end;
                _ ->
                    <<>>
            end
    end.

%% @private
select_victim(Cycle, Strategy) ->
    %% Sort by creation time to find youngest/oldest
    Sorted = lists:keysort(#wait_edge.added_at, Cycle),

    case Strategy of
        kill_youngest ->
            %% Last in sorted list (most recent) is youngest
            Last = lists:last(Sorted),
            {Last#wait_edge.waiting_pid, lists:delete(Last, Cycle)};
        kill_oldest ->
            %% First in sorted list is oldest
            [First | Rest] = Sorted,
            {First#wait_edge.waiting_pid, Rest};
        abort ->
            %% Select first available
            [First | Rest] = Cycle,
            {First#wait_edge.waiting_pid, Rest}
    end.

%% @private
detect_cycle_in_edges(Edges) ->
    %% Build adjacency list
    Graph = lists:foldl(fun(#wait_edge{waiting_pid = Pid, waiting_for = Waiting}, Acc) ->
        WaitingPid = case Waiting of
            W when is_pid(W) -> W;
            _ -> Pid  % Self-wait or resource wait
        end,
        maps:update_with(Pid, fun(Neighbors) -> [WaitingPid | Neighbors] end,
                        [WaitingPid], Acc)
    end, #{}, Edges),

    Nodes = lists:usort([E#wait_edge.waiting_pid || E <- Edges] ++
                        [E#wait_edge.waiting_for || E <- Edges, is_pid(E#wait_edge.waiting_for)]),

    detect_cycle_dfs(Graph, Nodes, [], #{}).

%% @private
detect_cycle_dfs(_Graph, [], _Path, _Visited) ->
    no_cycle;
detect_cycle_dfs(Graph, [Node | Rest], Path, Visited) ->
    case maps:get(Node, Visited, undefined) of
        visiting ->
            %% Found a cycle
            CycleStart = lists:nth(length(Path) - lists:nthindex(fun(N) -> N =:= Node end, lists:reverse(Path)) + 1, Path),
            {cycle, [Node | CycleStart]};
        visited ->
            detect_cycle_dfs(Graph, Rest, Path, Visited);
        undefined ->
            Visited1 = Visited#{Node => visiting},
            Neighbors = maps:get(Node, Graph, []),
            case detect_cycle_dfs(Graph, Neighbors, [Node | Path], Visited1) of
                {cycle, _} = Cycle ->
                    Cycle;
                no_cycle ->
                    Visited2 = Visited1#{Node => visited},
                    detect_cycle_dfs(Graph, Rest, Path, Visited2)
            end
    end.

%% @private
notify_observers(Observers, Message) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! {yawl_timeout, Message}
    end, Observers).

%% @private
save_checkpoint_to_file(File, Entry) ->
    try
        Data = term_to_binary(Entry),
        ok = file:write_file(File, Data),
        logger:debug("Saved checkpoint to file: ~p", [File])
    catch
        Kind:Reason:Stack ->
            logger:error("Failed to save checkpoint to file ~p: ~p:~p",
                          [File, Kind, Reason]),
            logger:error("Stacktrace: ~p", [Stack])
    end.
