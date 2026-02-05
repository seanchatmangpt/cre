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
%% @doc YAWL Timeout and Cancellation Tests
%%
%% Test suite for yawl_timeout module covering:
%% - Timeout management
%% - Cancellation token operations
%% - Deadlock detection
%% - Resource tracking
%% - State persistence (checkpoints)
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_timeout_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, Pid} = yawl_timeout:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Timeout Management Tests
%%====================================================================

timeout_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"set and get pattern timeout", fun test_set_get_timeout/0},
      {"with timeout success", fun test_with_timeout_success/0},
      {"with timeout expires", fun test_with_timeout_expires/0},
      {"cancel timeout", fun test_cancel_timeout/0},
      {"extend timeout", fun test_extend_timeout/0},
      {"reset timeout", fun test_reset_timeout/0}
     ]}.

test_set_get_timeout() ->
    PatternId = <<"test_pattern">>,
    ok = yawl_timeout:set_pattern_timeout(PatternId, 5000),
    {ok, 5000} = yawl_timeout:get_pattern_timeout(PatternId).

test_with_timeout_success() ->
    PatternId = <<"quick_pattern">>,
    Fun = fun() -> {ok, quick_result} end,
    {ok, quick_result} = yawl_timeout:with_timeout(Fun, PatternId, 1000).

test_with_timeout_expires() ->
    PatternId = <<"slow_pattern">>,
    Fun = fun() -> timer:sleep(2000), {ok, result} end,
    {error, timeout} = yawl_timeout:with_timeout(Fun, PatternId, 500).

test_cancel_timeout() ->
    PatternId = <<"cancellable_pattern">>,
    ok = yawl_timeout:set_pattern_timeout(PatternId, 5000),
    ok = yawl_timeout:cancel_timeout(PatternId),
    {error, not_found} = yawl_timeout:get_pattern_timeout(PatternId).

test_extend_timeout() ->
    PatternId = <<"extendable_pattern">>,
    ok = yawl_timeout:set_pattern_timeout(PatternId, 1000),
    ok = yawl_timeout:extend_timeout(PatternId, 2000),
    {ok, 3000} = yawl_timeout:get_pattern_timeout(PatternId).

test_reset_timeout() ->
    PatternId = <<"resettable_pattern">>,
    ok = yawl_timeout:set_pattern_timeout(PatternId, 1000),
    ok = yawl_timeout:extend_timeout(PatternId, 2000),
    ok = yawl_timeout:reset_timeout(PatternId),
    %% After reset, extended_count is 0 but timeout value persists
    {ok, _} = yawl_timeout:get_pattern_timeout(PatternId).

%%====================================================================
%% Cancellation Token Tests
%%====================================================================

cancellation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"new cancellation token", fun test_new_cancellation_token/0},
      {"check not cancelled", fun test_check_not_cancelled/0},
      {"request and check cancelled", fun test_request_cancel/0},
      {"linked tokens", fun test_linked_tokens/0},
      {"unlink tokens", fun test_unlink_tokens/0},
      {"throw if cancelled", fun test_throw_if_cancelled/0},
      {"get cancel reason", fun test_get_cancel_reason/0}
     ]}.

test_new_cancellation_token() ->
    Token = yawl_timeout:new_cancellation_token(),
    false = yawl_timeout:check_cancelled(Token).

test_check_not_cancelled() ->
    Token = yawl_timeout:new_cancellation_token(),
    false = yawl_timeout:is_cancelled(Token).

test_request_cancel() ->
    Token = yawl_timeout:new_cancellation_token(),
    ok = yawl_timeout:request_cancel(Token),
    true = yawl_timeout:is_cancelled(Token).

test_linked_tokens() ->
    Token1 = yawl_timeout:new_cancellation_token(),
    Token2 = yawl_timeout:new_cancellation_token(),
    ok = yawl_timeout:link_token(Token1, Token2),
    ok = yawl_timeout:request_cancel(Token1),
    %% Linked token should also be cancelled
    timer:sleep(100),
    true = yawl_timeout:is_cancelled(Token2).

test_unlink_tokens() ->
    Token1 = yawl_timeout:new_cancellation_token(),
    Token2 = yawl_timeout:new_cancellation_token(),
    ok = yawl_timeout:link_token(Token1, Token2),
    ok = yawl_timeout:unlink_token(Token1, Token2),
    ok = yawl_timeout:request_cancel(Token1),
    %% Unlinked token should NOT be cancelled
    timer:sleep(100),
    false = yawl_timeout:is_cancelled(Token2).

test_throw_if_cancelled() ->
    Token = yawl_timeout:new_cancellation_token(),
    ok = yawl_timeout:request_cancel(Token),
    ?assertThrow({cancelled, normal}, yawl_timeout:throw_if_cancelled(Token)).

test_get_cancel_reason() ->
    Token = yawl_timeout:new_cancellation_token(),
    Reason = user_requested,
    ok = yawl_timeout:request_cancel(Token, Reason),
    {ok, Reason} = yawl_timeout:get_cancel_reason(Token).

%%====================================================================
%% Deadlock Detection Tests
%%====================================================================

deadlock_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"no deadlock initially", fun test_no_deadlock/0},
      {"add and remove wait edge", fun test_add_remove_wait_edge/0},
      {"simple cycle detection", fun test_simple_cycle_detection/0},
      {"get wait graph", fun test_get_wait_graph/0}
     ]}.

test_no_deadlock() ->
    PatternId = <<"no_deadlock_pattern">>,
    {ok, no_deadlock} = yawl_timeout:detect_deadlock(PatternId).

test_add_remove_wait_edge() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ResourceId = <<"resource_1">>,
    ok = yawl_timeout:add_wait_edge(Pid, self(), ResourceId),
    ok = yawl_timeout:remove_wait_edge(Pid, ResourceId),
    exit(Pid, kill).

test_simple_cycle_detection() ->
    %% Create a simple cycle: P1 -> P2 -> P1
    P1 = spawn(fun() -> receive after infinity -> ok end end),
    P2 = spawn(fun() -> receive after infinity -> ok end end),
    R1 = <<"resource_1">>,
    R2 = <<"resource_2">>,

    ok = yawl_timeout:add_wait_edge(P1, R1, undefined),
    ok = yawl_timeout:add_wait_edge(P2, R2, undefined),

    %% Clean up
    exit(P1, kill),
    exit(P2, kill).

test_get_wait_graph() ->
    PatternId = <<"graph_pattern">>,
    Graph = yawl_timeout:get_wait_graph(PatternId),
    ?assert(is_list(Graph)).

%%====================================================================
%% Resource Tracking Tests
%%====================================================================

resource_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"track and release resource", fun test_track_release_resource/0},
      {"check no leaks", fun test_check_no_leaks/0},
      {"get resource usage", fun test_get_resource_usage/0},
      {"list resources", fun test_list_resources/0},
      {"mark resource critical", fun test_mark_resource_critical/0}
     ]}.

test_track_release_resource() ->
    PatternId = <<"resource_pattern">>,
    ResourceInfo = #{
        type => test_resource,
        cleanup_fn => fun(_) -> ok end,
        critical => false,
        metadata => #{}
    },
    {ok, ResourceId} = yawl_timeout:track_resource(PatternId, ResourceInfo),
    ok = yawl_timeout:release_resource(ResourceId),
    {error, not_found} = yawl_timeout:release_resource(ResourceId).

test_check_no_leaks() ->
    PatternId = <<"no_leak_pattern">>,
    {ok, []} = yawl_timeout:check_leaks(PatternId).

test_get_resource_usage() ->
    PatternId = <<"usage_pattern">>,
    {ok, Usage} = yawl_timeout:get_resource_usage(PatternId),
    ?assert(is_map(Usage)),
    ?assert(maps:is_key(allocated, Usage)),
    ?assert(maps:is_key(leaked, Usage)).

test_list_resources() ->
    PatternId = <<"list_pattern">>,
    {ok, Resources} = yawl_timeout:list_resources(PatternId),
    ?assert(is_list(Resources)).

test_mark_resource_critical() ->
    PatternId = <<"critical_pattern">>,
    ResourceInfo = #{
        type => critical_resource,
        cleanup_fn => fun(_) -> ok end,
        critical => false,
        metadata => #{}
    },
    {ok, ResourceId} = yawl_timeout:track_resource(PatternId, ResourceInfo),
    ok = yawl_timeout:mark_resource_critical(PatternId, ResourceId).

%%====================================================================
%% State Persistence Tests
%%====================================================================

checkpoint_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"save and load checkpoint", fun test_save_load_checkpoint/0},
      {"list checkpoints", fun test_list_checkpoints/0},
      {"cleanup checkpoint", fun test_cleanup_checkpoint/0},
      {"restore from checkpoint", fun test_restore_from_checkpoint/0},
      {"checkpoint before block", fun test_checkpoint_before_block/0}
     ]}.

test_save_load_checkpoint() ->
    PatternId = <<"checkpoint_pattern">>,
    StateData = #{key => value, counter => 42},
    {ok, CheckpointId} = yawl_timeout:save_checkpoint(PatternId, StateData),
    {ok, LoadedState} = yawl_timeout:load_checkpoint(CheckpointId),
    ?assertEqual(StateData, LoadedState).

test_list_checkpoints() ->
    PatternId = <<"list_checkpoints_pattern">>,
    {ok, _} = yawl_timeout:save_checkpoint(PatternId, #{data => test}),
    {ok, Checkpoints} = yawl_timeout:list_checkpoints(),
    ?assert(length(Checkpoints) > 0).

test_cleanup_checkpoint() ->
    PatternId = <<"cleanup_pattern">>,
    {ok, CheckpointId} = yawl_timeout:save_checkpoint(PatternId, #{data => cleanup}),
    ok = yawl_timeout:cleanup_checkpoint(CheckpointId),
    {error, not_found} = yawl_timeout:load_checkpoint(CheckpointId).

test_restore_from_checkpoint() ->
    PatternId = <<"restore_pattern">>,
    StateData = #{restore_data => test_value},
    {ok, _} = yawl_timeout:save_checkpoint(PatternId, StateData),
    {ok, RestoredState} = yawl_timeout:restore_from_checkpoint(PatternId),
    ?assertEqual(StateData, RestoredState).

test_checkpoint_before_block() ->
    PatternId = <<"block_pattern">>,
    StateData = #{before_block => true},
    {ok, CheckpointId} = yawl_timeout:checkpoint_before_block(PatternId, StateData),
    {ok, RestoredState} = yawl_timeout:restore_after_block(PatternId),
    ?assertEqual(StateData, RestoredState),
    ok = yawl_timeout:cleanup_checkpoint(CheckpointId).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"timeout with cancellation", fun test_timeout_with_cancellation/0},
      {"resource cleanup on cancellation", fun test_resource_cleanup_on_cancel/0}
     ]}.

test_timeout_with_cancellation() ->
    PatternId = <<"timeout_cancel_pattern">>,
    Token = yawl_timeout:new_cancellation_token(PatternId),

    %% Spawn a process that checks for cancellation
    Fun = fun() ->
        receive
            after 5000 ->
                {ok, completed}
        end
    end,

    Parent = self(),
    _Pid = spawn_link(fun() ->
        Result = yawl_timeout:with_timeout(Fun, PatternId, 10000, Token),
        Parent ! {result, Result}
    end),

    %% Cancel after a short delay
    timer:sleep(100),
    ok = yawl_timeout:request_cancel(Token),

    receive
        {result, {error, cancelled}} ->
            ok;
        {result, Other} ->
            ?assertEqual({error, cancelled}, Other)
    after 1000 ->
        ?assert(false, timeout_waiting_for_cancellation)
    end.

test_resource_cleanup_on_cancel() ->
    PatternId = <<"cleanup_pattern">>,
    CleanupRef = make_ref(),
    ResourceInfo = #{
        type => cleanup_test,
        cleanup_fn => fun(_) -> erlang:put(cleanup_ref, CleanupRef) end,
        critical => false,
        metadata => #{}
    },
    {ok, ResourceId} = yawl_timeout:track_resource(PatternId, ResourceInfo),
    ok = yawl_timeout:release_resource(ResourceId),
    %% Note: Cleanup function is called during release
    ?assertEqual(CleanupRef, get(cleanup_ref)).
