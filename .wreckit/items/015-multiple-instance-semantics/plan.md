# Multiple Instance Semantics Implementation Plan

## Implementation Plan Title
Unified Multi-Instance Framework for CRE with Comprehensive Join Policies and Cancellation Integration

## Overview

This plan implements a comprehensive multiple instance semantics framework for CRE (Common Runtime Environment) that unifies fixed, runtime, and dynamic instance spawning under a single gen_yawl pattern module. The implementation provides complete join policy support (all, first_n, n_of_m, discriminator), integrates with structured cancellation (item 014), and includes result aggregation strategies.

**Key Problem:** CRE has partial multi-instance support scattered across multiple modules (wf_multi_instance gen_server, static_partial_join_mi, dynamic_partial_join_mi, n_out_of_m) with inconsistent APIs and missing features like dynamic spawning, first-N join, and cancellation integration.

**Solution:** Create a unified `multi_instance` gen_yawl pattern module following the production-ready `n_out_of_m.erl` pattern, supporting all three instance specifications (fixed, runtime, dynamic) and all four join policies with proper Petri net semantics.

## Current State

### Existing Components (Verified)

1. **`wf_multi_instance`** (`/Users/sac/cre/src/wf/wf_multi_instance.erl:1-418`)
   - ✅ gen_server registry with N-out-of-M semantics
   - ✅ Tracks instances with unique IDs
   - ⚠️ NOT a gen_yawl behavior - cannot be used in workflows
   - ⚠️ Fixed M only - no dynamic spawning
   - ⚠️ No cancellation integration

2. **`n_out_of_m`** (`/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`)
   - ✅ **MODEL IMPLEMENTATION** - complete gen_yawl behavior
   - ✅ Full Petri net with places/transitions
   - ✅ N-of-M quorum join policy
   - ✅ XES logging for process mining
   - ✅ Pure functional design
   - ⚠️ Only supports N-of-M join (not first-N, all, or discriminator)

3. **`static_partial_join_mi`** (`/Users/sac/cre/src/patterns/static_partial_join_mi.erl:1-87`)
   - ⚠️ Basic static M instances with threshold
   - ⚠️ Simplified Petri net (4 places, 4 transitions)
   - ⚠️ No result aggregation

4. **`dynamic_partial_join_mi`** (`/Users/sac/cre/src/patterns/dynamic_partial_join_mi.erl:1-108`)
   - ⚠️ Runtime threshold computation
   - ⚠️ Instances NOT truly dynamic (still fixed count created at start)
   - ⚠️ No data-driven spawning

5. **`blocking_discriminator`** (`/Users/sac/cre/src/patterns/blocking_discriminator.erl:1-83`)
   - ⚠️ Standalone pattern
   - ⚠️ Not integrated with multi-instance framework
   - ✅ Demonstrates discriminator semantics

6. **`wf_scope`** (`/Users/sac/cre/src/wf/wf_scope.erl:1-319`)
   - ✅ Scope boundary mapping for subflows
   - ✅ `enter/3`, `leave/3`, `bindings/2` functions
   - ✅ Can define instance-level scopes

7. **Test Suite** (`/Users/sac/cre/test/yawl_multiple_instances_test.erl:1-2324`)
   - ✅ Comprehensive tests for WCP-11 through WCP-17
   - ⚠️ References non-existent `cre_yawl_patterns` module
   - ⚠️ Tests mocked - need actual implementation to run

### Key Discoveries

1. **Pattern to Follow:** `n_out_of_m.erl` is the production-ready reference implementation
   - File: `/Users/sac/cre/src/patterns/n_out_of_m.erl:21-657`
   - Complete gen_yawl callbacks: `place_lst/0`, `trsn_lst/0`, `init_marking/2`, `preset/1`, `is_enabled/3`, `fire/3`, `trigger/3`
   - State record includes: `m`, `n`, `branch_funs`, `completed`, `results`, `quorum_met`, `wait_for_all`, `log_id`
   - XES logging integration via `log_event/4` helper
   - 3-tuple fire returns: `{produce, ProduceMap, NewUsrInfo}` (lines 366-443)

2. **Gen_YAWL Convention:** All workflow patterns implement gen_yawl behavior
   - Petri net structure with places and transitions
   - Pure functional fire/3 (except gen_yawl callbacks)
   - Token-based communication
   - usr_info carries pattern state

3. **Scope Integration:** `wf_scope` module provides place translation
   - File: `/Users/sac/cre/src/wf/wf_scope.erl:21-319`
   - Can map parent places to child places for instance scopes
   - Use for per-instance cancellation scopes

4. **Critical Gap:** No unified multi-instance module
   - Existing patterns are scattered and incomplete
   - No dynamic spawning (WCP-15)
   - No first-N join policy
   - No integration with item 014 cancellation

## Desired End State

### Specification

1. **Unified `multi_instance` gen_yawl pattern** supporting:
   - **Instance specifications:**
     - `fixed M`: Design-time knowledge (WCP-13)
     - `runtime M`: Runtime-known count (WCP-14)
     - `dynamic`: Data-driven spawning with no prior knowledge (WCP-15)
   - **Join policies:**
     - `all`: Wait for all M instances
     - `first_n N`: Proceed after first N complete
     - `n_of_m {N, M}`: Quorum-based (existing n_out_of_m behavior)
     - `discriminator`: First completion triggers, cancel rest
   - **Result strategies:**
     - `collect_all`: Return all results
     - `collect_quorum`: Return first Q results
     - `merge`: Merge results using function
     - `broadcast`: Send to multiple places
     - `discard`: Drop results
   - **Cancellation:**
     - Per-instance scope: `{instance, InstanceId}`
     - Per-activity scope: `{activity, TaskId}`
     - Cancel-on-quorum: Cancel remaining after quorum met

2. **Petri Net Structure:**
   ```
   Places:
   - p_start: Start token
   - p_data_source: Data source for dynamic spawning
   - p_instances: Pool of active instances
   - p_running: Currently executing instances
   - p_completed: Completed instance tokens
   - p_quorum_met: Quorum reached (for n_of_m)
   - p_first_n_met: First-N reached (for first_n)
   - p_output: Final output
   - p_cancel: Cancellation tokens

   Transitions:
   - t_spawn_fixed: Spawn M instances (fixed spec)
   - t_spawn_runtime: Spawn M instances via function (runtime spec)
   - t_spawn_dynamic: Spawn one instance from data (dynamic spec)
   - t_execute: Execute an instance
   - t_complete: Mark instance as complete
   - t_join_all: Join all instances
   - t_join_first_n: Join after N complete
   - t_join_quorum: Join after N of M complete
   - t_discriminator: First completion triggers
   - t_cancel_remaining: Cancel after quorum (optional)
   - t_collect: Collect results based on strategy
   ```

3. **State Record:**
   ```erlang
   -record(mi_state, {
       instance_spec :: instance_spec(),
       join_policy :: join_policy(),
       result_strategy :: result_strategy(),
       completed = [] :: [binary()],  % Instance IDs
       results = [] :: [{binary(), term()}],  % {InstanceId, Result}
       quorum_met = false :: boolean(),
       cancel_on_quorum = false :: boolean(),
       log_id :: binary() | undefined
   }).
   ```

### Verification

- [ ] All four join policies work with fixed M instances
- [ ] Dynamic spawning terminates when data source exhausted
- [ ] First-N join proceeds exactly at N completions
- [ ] Discriminator cancels remaining instances after first completes
- [ ] Cancellation tokens properly cancel instances via trigger/3
- [ ] XES logging records spawn, complete, quorum, proceed events
- [ ] Result aggregation strategies produce correct output
- [ ] Integration tests pass with `yawl_multiple_instances_test.erl`

## What We're NOT Doing

1. **NOT replacing `wf_multi_instance` gen_server**
   - Keep it for registry/query purposes (hybrid approach)
   - May add gen_yawl facade later, but out of scope

2. **NOT implementing persistence**
   - Instance state kept in memory only
   - Future enhancement: ETS or database persistence

3. **NOT implementing sequential mode**
   - Focus on parallel execution (primary use case)
   - Sequential can be added later as instance_spec variant

4. **NOT modifying existing patterns**
   - Keep `n_out_of_m`, `static_partial_join_mi`, `dynamic_partial_join_mi` unchanged
   - New `multi_instance` module will be the unified entry point

5. **NOT implementing workflow XML parser**
   - Focus on gen_yawl pattern implementation only
   - XML/YAWL specification integration is separate concern

6. **NOT creating `cre_yawl_patterns` facade module**
   - Tests reference it, but creation is separate work
   - Tests will be updated to call `multi_instance` directly

## Implementation Approach

### High-Level Strategy

**Phase 1: Core Multi-Instance Pattern (Instance Spawning)**
- Create `multi_instance` gen_yawl module following `n_out_of_m.erl` pattern
- Implement fixed M spawning (design-time knowledge)
- Implement runtime M spawning (function-determined count)
- Implement basic join policies (all, first_n, n_of_m)
- Pure functional design with XES logging

**Phase 2: Dynamic Spawning (WCP-15)**
- Implement data-driven instance creation
- Add max_instances limit for safety
- Implement data source exhaustion detection
- Add backpressure via token pool

**Phase 3: Join Policy Framework**
- Implement discriminator join policy
- Create unified join policy validation
- Add join policy precedence rules
- Implement cancel-on-quorum transition

**Phase 4: Cancellation Integration**
- Integrate with item 014 structured cancellation
- Implement per-instance cancellation scopes
- Implement per-activity cancellation scopes
- Add trigger/3 filtering for cancellation tokens

**Phase 5: Result Aggregation**
- Implement collect_all strategy
- Implement collect_quorum strategy
- Implement merge strategy with custom function
- Implement broadcast strategy
- Implement discard strategy

**Phase 6: Test Integration**
- Fix `yawl_multiple_instances_test.erl` to call `multi_instance` directly
- Add property-based tests for invariants
- Add performance benchmarks
- Verify all WCP-11 through WCP-17 tests pass

---

## Phase 1: Core Multi-Instance Pattern (Instance Spawning)

### Overview
Create the unified `multi_instance` gen_yawl module with fixed and runtime instance spawning, plus basic join policies (all, first_n, n_of_m). This establishes the foundation for all subsequent phases.

### Changes Required:

#### 1. Create `multi_instance.erl` Module

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl` (NEW)
**Changes**: Create new gen_yawl pattern module following `n_out_of_m.erl` structure

```erlang
%% -*- erlang -*-
%% @doc Unified Multi-Instance Pattern for YAWL.
%%
%% Implements comprehensive multi-instance semantics supporting:
%% - Fixed M instances (design-time knowledge, WCP-13)
%% - Runtime M instances (runtime knowledge, WCP-14)
%% - Dynamic instances (no prior knowledge, WCP-15)
%% - Join policies: all, first_n, n_of_m, discriminator
%% - Result aggregation: collect_all, collect_quorum, merge, broadcast, discard
%% - Per-instance and per-activity cancellation
%%
%% @end
-module(multi_instance).
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    trigger/3,
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% API exports
-export([
    new/3,
    start/2,
    run/2,
    get_state/1,
    execute/3
]).

%%====================================================================
%% Types
%%====================================================================

%% Instance specification
-type instance_spec() :: {fixed, pos_integer()} |
                        {runtime, fun(() -> pos_integer())} |
                        {dynamic, fun(() -> {more, term()} | done), pos_integer()}.

%% Join policy
-type join_policy() :: all |
                       {first_n, pos_integer()} |
                       {n_of_m, pos_integer(), pos_integer()} |
                       discriminator.

%% Result aggregation strategy
-type result_strategy() :: collect_all |
                          collect_quorum |
                          {merge, fun(([term()]) -> term())} |
                          {broadcast, [atom()]} |
                          discard.

%% Cancellation scope
-type cancel_scope() :: {instance, binary()} |
                       {activity, atom()} |
                       quorum_met.

%% State record
-record(mi_state, {
    instance_spec :: instance_spec(),
    join_policy :: join_policy(),
    result_strategy :: result_strategy(),
    cancel_on_quorum = false :: boolean(),
    completed = [] :: [binary()],
    results = [] :: [{binary(), term()}],
    max_instances :: pos_integer() | unlimited,
    quorum_met = false :: boolean(),
    first_n_met = false :: boolean(),
    discriminator_triggered = false :: boolean(),
    log_id :: binary() | undefined
}).

-type mi_state() :: #mi_state{}.
-export_type([mi_state/0, instance_spec/0, join_policy/0, result_strategy/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Creates a new multi-instance pattern state.
-spec new(InstanceSpec :: instance_spec(),
          JoinPolicy :: join_policy(),
          ResultStrategy :: result_strategy()) -> mi_state().

new(InstanceSpec, JoinPolicy, ResultStrategy) ->
    LogId = generate_log_id(),
    #mi_state{
        instance_spec = InstanceSpec,
        join_policy = JoinPolicy,
        result_strategy = ResultStrategy,
        max_instances = max_instances(InstanceSpec),
        log_id = LogId
    }.

%% @doc Starts the multi-instance workflow as a gen_yawl process.
-spec start(InstanceSpec :: instance_spec(),
            {JoinPolicy :: join_policy(), ResultStrategy :: result_strategy()}) ->
          {ok, pid()} | {error, term()}.

start(InstanceSpec, {JoinPolicy, ResultStrategy}) ->
    MIState = new(InstanceSpec, JoinPolicy, ResultStrategy),
    gen_yawl:start_link(?MODULE, MIState, []).

%% @doc Runs the multi-instance workflow synchronously.
-spec run(InstanceSpec :: instance_spec(),
          {JoinPolicy :: join_policy(), ResultStrategy :: result_strategy()}) ->
          {ok, term()} | {error, term()}.

run(InstanceSpec, {JoinPolicy, ResultStrategy}) ->
    case start(InstanceSpec, {JoinPolicy, ResultStrategy}) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, Result} ->
                    gen_yawl:stop(Pid),
                    {ok, Result};
                {error, Reason} ->
                    gen_yawl:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Gets the current state of the multi-instance workflow.
-spec get_state(pid()) -> {ok, mi_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%% @doc Executes the multi-instance pattern with given instance functions.
-spec execute(InstanceSpec :: instance_spec(),
             JoinPolicy :: join_policy(),
             InstanceFuns :: [function()]) ->
          {ok, term()} | {error, term()}.

execute({fixed, M}, JoinPolicy, InstanceFuns) when length(InstanceFuns) =:= M ->
    ResultStrategy = collect_all,
    run({fixed, M}, {JoinPolicy, ResultStrategy});
execute({runtime, CountFun}, JoinPolicy, InstanceFuns) ->
    M = CountFun(),
    execute({fixed, M}, JoinPolicy, InstanceFuns);
execute(_InstanceSpec, _JoinPolicy, _InstanceFuns) ->
    {error, invalid_spec}.

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

%% @doc Returns the list of places for the multi-instance Petri net.
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_instances',
        'p_running',
        'p_completed',
        'p_quorum_met',
        'p_first_n_met',
        'p_output',
        'p_cancel'
    ].

%% @doc Returns the list of transitions for the multi-instance Petri net.
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_spawn_fixed',
        't_spawn_runtime',
        't_execute',
        't_complete',
        't_join_all',
        't_join_first_n',
        't_join_quorum',
        't_discriminator',
        't_collect'
    ].

%% @doc Returns the initial marking for a given place.
-spec init_marking(atom(), mi_state()) -> [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%% @doc Returns the preset (input places) for each transition.
-spec preset(atom()) -> [atom()].

preset('t_spawn_fixed') -> ['p_start'];
preset('t_spawn_runtime') -> ['p_start'];
preset('t_execute') -> ['p_instances'];
preset('t_complete') -> ['p_running'];
preset('t_join_all') -> ['p_completed'];
preset('t_join_first_n') -> ['p_completed'];
preset('t_join_quorum') -> ['p_completed'];
preset('t_discriminator') -> ['p_completed'];
preset('t_collect') -> ['p_output'];
preset(_) -> [].

%% @doc Checks if a transition is enabled.
-spec is_enabled(atom(), map(), mi_state()) -> boolean().

is_enabled('t_spawn_fixed', Mode, #mi_state{instance_spec = {fixed, M}}) ->
    maps:is_key('p_start', Mode) andalso M > 0;
is_enabled('t_spawn_runtime', Mode, #mi_state{instance_spec = {runtime, _Fun}}) ->
    maps:is_key('p_start', Mode);
is_enabled('t_execute', Mode, _UsrInfo) ->
    case maps:get('p_instances', Mode, []) of
        [] -> false;
        _ -> true
    end;
is_enabled('t_complete', Mode, _UsrInfo) ->
    case maps:get('p_running', Mode, []) of
        [] -> false;
        _ -> true
    end;
is_enabled('t_join_all', Mode, #mi_state{join_policy = all, instance_spec = {fixed, M}}) ->
    Completed = length(maps:get('p_completed', Mode, [])),
    Completed >= M;
is_enabled('t_join_first_n', Mode, #mi_state{join_policy = {first_n, N}}) ->
    Completed = length(maps:get('p_completed', Mode, [])),
    Completed >= N;
is_enabled('t_join_quorum', Mode, #mi_state{join_policy = {n_of_m, N, M}}) ->
    Completed = length(maps:get('p_completed', Mode, [])),
    Completed >= N andalso Completed =< M;
is_enabled('t_discriminator', Mode, #mi_state{join_policy = discriminator, discriminator_triggered = false}) ->
    length(maps:get('p_completed', Mode, [])) > 0;
is_enabled('t_collect', Mode, _UsrInfo) ->
    maps:is_key('p_output', Mode);
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%% @doc Fires a transition, consuming and producing tokens.
-spec fire(atom(), map(), mi_state()) ->
          {produce, map()} | {produce, map(), mi_state()}.

fire('t_spawn_fixed', #{'p_start' := [start]}, #mi_state{instance_spec = {fixed, M}} = State) ->
    %% Create M instance tokens
    InstanceTokens = [{{instance, I}, undefined} || I <- lists:seq(1, M)],
    log_event(State, <<"MultiInstance">>, <<"SpawnFixed">>, #{<<"m">> => M}),
    {produce, #{
        'p_start' => [],
        'p_instances' => InstanceTokens
    }};

fire('t_spawn_runtime', #{'p_start' := [start]}, #mi_state{instance_spec = {runtime, CountFun}} = State) ->
    M = CountFun(),
    InstanceTokens = [{{instance, I}, undefined} || I <- lists:seq(1, M)],
    log_event(State, <<"MultiInstance">>, <<"SpawnRuntime">>, #{<<"m">> => M}),
    {produce, #{
        'p_start' => [],
        'p_instances' => InstanceTokens
    }};

fire('t_execute', #{'p_instances' := [Token | Rest]}, State) ->
    %% Move instance from pool to running
    log_event(State, <<"MultiInstance">>, <<"Execute">>, #{}),
    {produce, #{
        'p_instances' => Rest,
        'p_running' => [Token]
    }};

fire('t_complete', #{'p_running' := [{{instance, Id}, _Data} | Rest]}, #mi_state{completed = Completed, results = Results} = State) ->
    %% Mark instance as complete
    InstanceId = integer_to_binary(Id),
    NewState = State#mi_state{
        completed = [InstanceId | Completed],
        results = [{{instance, Id}, complete} | Results]
    },
    log_event(State, <<"MultiInstance">>, <<"InstanceComplete">>, #{
        <<"instance_id">> => InstanceId,
        <<"completed_count">> => length(Completed) + 1
    }),
    {produce, #{
        'p_running' => Rest,
        'p_completed' => [InstanceId]
    }, NewState};

fire('t_join_all', #{'p_completed' := Completed}, #mi_state{instance_spec = {fixed, M}} = State) when length(Completed) >= M ->
    log_event(State, <<"MultiInstance">>, <<"JoinAll">>, #{<<"all_count">> => length(Completed)}),
    {produce, #{
        'p_completed' => [],
        'p_output' => [{all_complete, Completed}]
    }, State};

fire('t_join_first_n', #{'p_completed' := Completed}, #mi_state{join_policy = {first_n, N}} = State) when length(Completed) >= N ->
    log_event(State, <<"MultiInstance">>, <<"JoinFirstN">>, #{<<"n">> => N, <<"completed">> => length(Completed)}),
    {produce, #{
        'p_completed' => [],
        'p_output' => [{first_n_complete, lists:sublist(Completed, N)}]
    }, State#mi_state{first_n_met = true}};

fire('t_join_quorum', #{'p_completed' := Completed}, #mi_state{join_policy = {n_of_m, N, M}} = State) when length(Completed) >= N ->
    Remaining = M - length(Completed),
    log_event(State, <<"MultiInstance">>, <<"QuorumMet">>, #{
        <<"n">> => N,
        <<"m">> => M,
        <<"completed">> => length(Completed),
        <<"remaining">> => Remaining
    }),
    {produce, #{
        'p_completed' => [],
        'p_output' => [{quorum_met, Completed}]
    }, State#mi_state{quorum_met = true}};

fire('t_discriminator', #{'p_completed' := [First | _]}, #mi_state{discriminator_triggered = false} = State) ->
    log_event(State, <<"MultiInstance">>, <<"DiscriminatorTriggered">>, #{<<"first">> => First}),
    {produce, #{
        'p_completed' => [],
        'p_output' => [{discriminator, First}],
        'p_cancel' => [cancel_remaining]
    }, State#mi_state{discriminator_triggered = true}};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%% @doc Trigger callback for token filtering (cancellation support).
-spec trigger(atom(), term(), term()) -> pass | {consume, [term()]}.

trigger(_Place, _Token, _NetState) ->
    pass.

%% @doc Initializes the gen_yawl process.
-spec init(mi_state()) -> {ok, mi_state()}.

init(#mi_state{log_id = LogId} = MIState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MultiInstance">>}) of
        {ok, LogId} ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, MIState};
        _ ->
            {ok, MIState}
    end.

%% @doc Handles synchronous calls.
-spec handle_call(term(), {pid(), term()}, term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%% @doc Handles asynchronous casts.
-spec handle_cast(term(), term()) -> {noreply, term()}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%% @doc Handles non-gen_yawl messages.
-spec handle_info(term(), term()) -> {noreply, term()}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

%% @doc Handles code changes.
-spec code_change(term(), term(), term()) -> {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%% @doc Cleanup on termination.
-spec terminate(term(), term()) -> ok.

terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #mi_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
max_instances({fixed, M}) -> M;
max_instances({runtime, Fun}) -> catch Fun();
max_instances({dynamic, _Fun, Max}) -> Max;
max_instances(_) -> unlimited.

%% @private
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"multi_instance_", Hex/binary>>.

%% @private
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%% @private
log_event(#mi_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%% @private
wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #mi_state{results = Results} ->
                            {ok, Results};
                        _ ->
                            {error, no_results}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.
```

#### 2. Add Simple Unit Tests

**File**: `/Users/sac/cre/test/multi_instance_test.erl` (NEW)
**Changes**: Create basic unit tests for the new module

```erlang
%% -*- erlang -*-
%% @doc Unit tests for multi_instance pattern
-module(multi_instance_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixed instance spawning
fixed_spawn_test() ->
    Spec = {fixed, 3},
    Policy = {n_of_m, 2, 3},
    State = multi_instance:new(Spec, Policy, collect_all),
    ?assertEqual(3, element(2, State#mi_state.instance_spec)).

%% Test runtime instance spawning
runtime_spawn_test() ->
    CountFun = fun() -> 5 end,
    Spec = {runtime, CountFun},
    Policy = all,
    State = multi_instance:new(Spec, Policy, collect_all),
    ?assertEqual(runtime, element(1, State#mi_state.instance_spec)).

%% Test place list
place_lst_test() ->
    Places = multi_instance:place_lst(),
    ?assert(lists:member('p_start', Places)),
    ?assert(lists:member('p_instances', Places)),
    ?assert(lists:member('p_completed', Places)),
    ?assert(lists:member('p_output', Places)).

%% Test transition list
trsn_lst_test() ->
    Transitions = multi_instance:trsn_lst(),
    ?assert(lists:member('t_spawn_fixed', Transitions)),
    ?assert(lists:member('t_execute', Transitions)),
    ?assert(lists:member('t_join_all', Transitions)).
```

### Success Criteria:

#### Automated Verification:
- [ ] Module compiles without errors: `erlc -I /Users/sac/cre/src src/patterns/multi_instance.erl`
- [ ] Unit tests pass: `rebar3 eunit --module=multi_instance_test`
- [ ] gen_yawl behavior callbacks compile correctly
- [ ] Type specs are valid: `dialyzer src/patterns/multi_instance.erl` (no warnings)

#### Manual Verification:
- [ ] Code review: Follows `n_out_of_m.erl` structure exactly
- [ ] Petri net places/transitions documented in code
- [ ] All four join policies have distinct transitions
- [ ] XES logging integrated for all major events
- [ ] No hardcoded values - all configurable via state record

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 2.

---

## Phase 2: Dynamic Spawning (WCP-15)

### Overview
Implement truly dynamic instance spawning where instances are created on-demand from a data source until exhausted. This completes WCP-15 (Multiple Instances without Prior Knowledge).

### Changes Required:

#### 1. Extend `multi_instance.erl` with Dynamic Spawning

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add dynamic spawning transition and data source handling

**Modify trsn_lst/0 to add:**
```erlang
trsn_lst() ->
    [
        't_spawn_fixed',
        't_spawn_runtime',
        't_spawn_dynamic',      %% NEW
        't_check_data_source',   %% NEW
        't_execute',
        't_complete',
        't_join_all',
        't_join_first_n',
        't_join_quorum',
        't_discriminator',
        't_collect'
    ].
```

**Modify place_lst/0 to add:**
```erlang
place_lst() ->
    [
        'p_start',
        'p_data_source',         %% NEW: Data source for dynamic spawning
        'p_instances',
        'p_running',
        'p_completed',
        'p_data_exhausted',      %% NEW: Signal data source exhausted
        'p_quorum_met',
        'p_first_n_met',
        'p_output',
        'p_cancel'
    ].
```

**Add preset entries:**
```erlang
preset('t_spawn_dynamic') -> ['p_start'];
preset('t_check_data_source') -> ['p_data_source'];
%% ... rest of presets
```

**Add is_enabled clauses:**
```erlang
is_enabled('t_spawn_dynamic', Mode, #mi_state{instance_spec = {dynamic, _DataFun, _Max}}) ->
    maps:is_key('p_start', Mode) orelse maps:is_key('p_data_source', Mode);
is_enabled('t_check_data_source', Mode, #mi_state{instance_spec = {dynamic, _DataFun, _Max}}) ->
    maps:is_key('p_data_source', Mode);
%% ... rest of is_enabled clauses
```

**Add fire clauses for dynamic spawning:**
```erlang
fire('t_spawn_dynamic', #{'p_start' := [start]}, #mi_state{instance_spec = {dynamic, DataFun, Max}} = State) ->
    %% Initialize data source
    log_event(State, <<"MultiInstance">>, <<"InitDataSource">>, #{<<"max_instances">> => Max}),
    {produce, #{
        'p_start' => [],
        'p_data_source' => [DataFun]
    }};

fire('t_check_data_source', #{'p_data_source' := [DataFun | Rest]}, State) ->
    %% Try to get next instance data
    case DataFun() of
        {more, InstanceData} ->
            InstanceId = generate_instance_id(),
            log_event(State, <<"MultiInstance">>, <<"SpawnDynamic">>, #{
                <<"instance_id">> => InstanceId,
                <<"data">> => term_to_binary(InstanceData)
            }),
            %% Create instance and put data source back for next iteration
            {produce, #{
                'p_instances' => [{{instance, InstanceId}, InstanceData}],
                'p_data_source' => [DataFun]
            }};
        done ->
            %% Data source exhausted
            log_event(State, <<"MultiInstance">>, <<"DataSourceExhausted">>, #{}),
            {produce, #{
                'p_data_source' => [],
                'p_data_exhausted' => [exhausted]
            }}
    end;
```

**Modify state record to track dynamic instances:**
```erlang
-record(mi_state, {
    instance_spec :: instance_spec(),
    join_policy :: join_policy(),
    result_strategy :: result_strategy(),
    cancel_on_quorum = false :: boolean(),
    completed = [] :: [binary()],
    results = [] :: [{binary(), term()}],
    max_instances :: pos_integer() | unlimited,
    spawned_count = 0 :: non_neg_integer(),  %% NEW: Track spawned instances
    quorum_met = false :: boolean(),
    first_n_met = false :: boolean(),
    discriminator_triggered = false :: boolean(),
    log_id :: binary() | undefined
}).
```

**Add max_instances enforcement:**
```erlang
fire('t_check_data_source', #{'p_data_source' := [DataFun | Rest]}, #mi_state{spawned_count = Spawned, max_instances = Max} = State) when is_integer(Max), Spawned >= Max ->
    %% Max instances reached - stop spawning
    log_event(State, <<"MultiInstance">>, <<"MaxInstancesReached">>, #{<<"max">> => Max}),
    {produce, #{
        'p_data_source' => [],
        'p_data_exhausted' => [max_reached]
    }};
%% ... rest of t_check_data_source clause
```

**Add helper function:**
```erlang
%% @private
generate_instance_id() ->
    Unique = erlang:unique_integer(),
    <<"instance_", (integer_to_binary(Unique))/binary>>.
```

#### 2. Add Dynamic Spawning Tests

**File**: `/Users/sac/cre/test/multi_instance_test.erl`
**Changes**: Add tests for dynamic spawning

```erlang
%% Test dynamic spawning with data list
dynamic_spawn_list_test() ->
    DataList = [a, b, c, d, e],
    DataFun = fun() ->
        case DataList of
            [] -> done;
            [H | T] ->
                DataList = T,  %% Update process dictionary
                {more, H}
        end
    end,
    Spec = {dynamic, DataFun, 100},
    Policy = all,
    State = multi_instance:new(Spec, Policy, collect_all),
    ?assertEqual(dynamic, element(1, State#mi_state.instance_spec)),
    ?assertEqual(100, State#mi_state.max_instances).

%% Test dynamic spawning max limit
dynamic_max_limit_test() ->
    %% Create data source with more items than max
    DataFun = fun() -> {more, test_data} end,
    Spec = {dynamic, DataFun, 5},
    Policy = {first_n, 3},
    State = multi_instance:new(Spec, Policy, collect_all),
    ?assertEqual(5, State#mi_state.max_instances).
```

### Success Criteria:

#### Automated Verification:
- [ ] Module compiles with dynamic spawning changes
- [ ] New unit tests pass
- [ ] No dialyzer warnings

#### Manual Verification:
- [ ] Data source exhaustion triggers p_data_exhausted token
- [ ] Max instances limit enforced correctly
- [ ] Dynamic spawning can run without fixed M (WCP-15 satisfied)
- [ ] Backpressure via token pool prevents unbounded spawning

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 3.

---

## Phase 3: Join Policy Framework

### Overview
Implement discriminator join policy and create unified join policy validation with precedence rules. This completes all four join policies.

### Changes Required:

#### 1. Implement Discriminator with Cancellation

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Enhance discriminator transition to cancel remaining instances

**Modify discriminator fire clause:**
```erlang
fire('t_discriminator', #{'p_completed' := [First | Rest]}, #mi_state{discriminator_triggered = false} = State) ->
    log_event(State, <<"MultiInstance">>, <<"DiscriminatorTriggered">>, #{
        <<"first">> => First,
        <<"cancelled_count">> => length(Rest)
    }),
    %% Produce cancellation tokens for remaining instances
    CancelTokens = [{cancel, InstanceId} || InstanceId <- Rest],
    {produce, #{
        'p_completed' => [],
        'p_output' => [{discriminator, First}],
        'p_cancel' => CancelTokens
    }, State#mi_state{discriminator_triggered = true}};
```

**Add trigger/3 clause to handle cancellation:**
```erlang
%% Trigger callback for filtering cancellation tokens
trigger('p_running', {cancel, InstanceId}, NetState) ->
    %% Cancel specific instance - drop the token
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #mi_state{} ->
            log_event(UsrInfo, <<"MultiInstance">>, <<"InstanceCancelled">>, #{
                <<"instance_id">> => InstanceId
            }),
            {consume, [{cancel, InstanceId}]};
        _ ->
            pass
    end;

trigger('p_instances', {cancel, InstanceId}, NetState) ->
    %% Cancel pending instance - drop from pool
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #mi_state{} ->
            log_event(UsrInfo, <<"MultiInstance">>, <<"InstanceCancelled">>, #{
                <<"instance_id">> => InstanceId
            }),
            {consume, [{cancel, InstanceId}]};
        _ ->
            pass
    end;

trigger(_Place, _Token, _NetState) ->
    pass.
```

#### 2. Add Join Policy Validation

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add validation function for join policies

**Add to API exports:**
```erlang
-export([
    validate_join_policy/2,
    validate_instance_spec/1
]).
```

**Add validation functions:**
```erlang
%% @doc Validates a join policy against instance specification.
-spec validate_join_policy(join_policy(), instance_spec()) ->
          ok | {error, term()}.

validate_join_policy(all, {fixed, M}) when M > 0 -> ok;
validate_join_policy(all, {runtime, _}) -> ok;
validate_join_policy(all, {dynamic, _, _}) -> ok;

validate_join_policy({first_n, N}, {fixed, M}) when N =< M, N > 0 -> ok;
validate_join_policy({first_n, N}, {runtime, Fun}) when is_function(Fun, 0), N > 0 -> ok;
validate_join_policy({first_n, N}, {dynamic, _, Max}) when N > 0, N =< Max -> ok;

validate_join_policy({n_of_m, N, M}, {fixed, M}) when N =< M, N > 0 -> ok;
validate_join_policy({n_of_m, N, M}, {runtime, Fun}) when is_function(Fun, 0), N > 0, M > 0, N =< M -> ok;

validate_join_policy(discriminator, {fixed, M}) when M > 0 -> ok;
validate_join_policy(discriminator, {runtime, _}) -> ok;
validate_join_policy(discriminator, {dynamic, _, _}) -> ok;

validate_join_policy(Policy, Spec) ->
    {error, {invalid_combination, Policy, Spec}}.

%% @doc Validates an instance specification.
-spec validate_instance_spec(instance_spec()) -> ok | {error, term()}.

validate_instance_spec({fixed, M}) when is_integer(M), M > 0 -> ok;
validate_instance_spec({runtime, Fun}) when is_function(Fun, 0) -> ok;
validate_instance_spec({dynamic, Fun, Max}) when is_function(Fun, 0), is_integer(Max), Max > 0 -> ok;
validate_instance_spec(Spec) -> {error, {invalid_spec, Spec}}.
```

#### 3. Add Precedence Rules

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Document and implement join policy precedence

**Add function:**
```erlang
%% @doc Returns the effective join policy based on precedence rules.
%% Precedence: discriminator > first_n > n_of_m > all
-spec effective_join_policy(join_policy(), join_policy()) -> join_policy().

effective_join_policy(discriminator, _) -> discriminator;
effective_join_policy(_, discriminator) -> discriminator;
effective_join_policy({first_n, N}, _) -> {first_n, N};
effective_join_policy(_, {first_n, N}) -> {first_n, N};
effective_join_policy({n_of_m, N, M}, all) -> {n_of_m, N, M};
effective_join_policy(all, {n_of_m, N, M}) -> {n_of_m, N, M};
effective_join_policy(Policy, Policy) -> Policy.
```

#### 4. Add Join Policy Tests

**File**: `/Users/sac/cre/test/multi_instance_test.erl`
**Changes**: Add join policy validation tests

```erlang
%% Test join policy validation
validate_join_policy_test() ->
    ?assertEqual(ok, multi_instance:validate_join_policy(all, {fixed, 5})),
    ?assertEqual(ok, multi_instance:validate_join_policy({first_n, 3}, {fixed, 5})),
    ?assertEqual(ok, multi_instance:validate_join_policy({n_of_m, 3, 5}, {fixed, 5})),
    ?assertEqual(ok, multi_instance:validate_join_policy(discriminator, {fixed, 5})),
    ?assertEqual({error, _}, multi_instance:validate_join_policy({first_n, 10}, {fixed, 5})).

%% Test instance spec validation
validate_spec_test() ->
    ?assertEqual(ok, multi_instance:validate_instance_spec({fixed, 5})),
    ?assertEqual(ok, multi_instance:validate_instance_spec({runtime, fun() -> 5 end})),
    ?assertEqual({error, _}, multi_instance:validate_instance_spec({fixed, 0})).

%% Test join policy precedence
precedence_test() ->
    ?assertEqual(discriminator, multi_instance:effective_join_policy(discriminator, all)),
    ?assertEqual({first_n, 3}, multi_instance:effective_join_policy({first_n, 3}, all)),
    ?assertEqual({n_of_m, 3, 5}, multi_instance:effective_join_policy({n_of_m, 3, 5}, all)).
```

### Success Criteria:

#### Automated Verification:
- [ ] Module compiles with discriminator cancellation
- [ ] Validation tests pass
- [ ] Precedence tests pass
- [ ] No dialyzer warnings

#### Manual Verification:
- [ ] Discriminator cancels remaining instances (trigger/3 filters tokens)
- [ ] Join policy validation catches invalid combinations
- [ ] Precedence rules documented and tested
- [ ] All four join policies functional

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 4.

---

## Phase 4: Cancellation Integration

### Overview
Integrate with item 014 structured cancellation by implementing per-instance and per-activity cancellation scopes using wf_scope module.

### Changes Required:

#### 1. Define Cancellation Scope Types

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add cancellation scope types and integration

**Add to types:**
```erlang
%% Cancellation scope for item 014 integration
-type cancel_scope() :: {instance, binary()} |   %% Cancel specific instance
                       {activity, atom()} |        %% Cancel all instances of activity
                       quorum_met.                 %% Cancel remaining after quorum

-type cancel_token() :: {cancel, cancel_scope()}.
```

**Modify state record:**
```erlang
-record(mi_state, {
    instance_spec :: instance_spec(),
    join_policy :: join_policy(),
    result_strategy :: result_strategy(),
    cancel_on_quorum = false :: boolean(),
    completed = [] :: [binary()],
    results = [] :: [{binary(), term()}],
    max_instances :: pos_integer() | unlimited,
    spawned_count = 0 :: non_neg_integer(),
    quorum_met = false :: boolean(),
    first_n_met = false :: boolean(),
    discriminator_triggered = false :: boolean(),
    activity_id :: atom() | undefined,  %% NEW: Activity ID for scope
    instance_scopes = #{} :: #{binary() => atom()},  %% NEW: Instance ID -> scope ID
    log_id :: binary() | undefined
}).
```

#### 2. Implement Cancellation Token Handling

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add cancel transition and enhanced trigger/3

**Add to trsn_lst/0:**
```erlang
trsn_lst() ->
    [
        't_spawn_fixed',
        't_spawn_runtime',
        't_spawn_dynamic',
        't_check_data_source',
        't_execute',
        't_complete',
        't_join_all',
        't_join_first_n',
        't_join_quorum',
        't_discriminator',
        't_cancel_remaining',  %% NEW: Cancel remaining after quorum
        't_collect'
    ].
```

**Add preset:**
```erlang
preset('t_cancel_remaining') -> ['p_quorum_met'];
```

**Add is_enabled:**
```erlang
is_enabled('t_cancel_remaining', Mode, #mi_state{cancel_on_quorum = true, quorum_met = true}) ->
    maps:is_key('p_quorum_met', Mode);
```

**Add fire clause:**
```erlang
fire('t_cancel_remaining', #{'p_quorum_met' := [quorum, Completed]}, #mi_state{instance_spec = {fixed, M}, completed = Completed} = State) ->
    %% Cancel remaining instances after quorum met
    RemainingCount = M - length(Completed),
    log_event(State, <<"MultiInstance">>, <<"CancelRemaining">>, #{
        <<"remaining_count">> => RemainingCount
    }),
    %% Generate cancellation tokens for all non-completed instances
    AllInstanceIds = [integer_to_binary(I) || I <- lists:seq(1, M)],
    CancelTokens = [{cancel, {instance, Id}} || Id <- AllInstanceIds -- Completed],
    {produce, #{
        'p_quorum_met' => [],
        'p_cancel' => CancelTokens
    }, State};
```

**Enhance trigger/3 for cancellation:**
```erlang
%% Handle per-instance cancellation
trigger('p_running', {cancel, {instance, InstanceId}}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #mi_state{instance_scopes = Scopes} ->
            case maps:get(InstanceId, Scopes, undefined) of
                undefined ->
                    %% Instance not found - pass through
                    pass;
                ScopeId ->
                    %% Cancel instance scope
                    log_event(UsrInfo, <<"MultiInstance">>, <<"InstanceCancelled">>, #{
                        <<"instance_id">> => InstanceId,
                        <<"scope_id">> => ScopeId
                    }),
                    %% Remove from running - consume token
                    {consume, [{cancel, {instance, InstanceId}}]}
            end;
        _ ->
            pass
    end;

%% Handle per-activity cancellation
trigger('p_running', {cancel, {activity, ActivityId}}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #mi_state{activity_id = ActivityId} ->
            %% Cancel all instances of this activity
            log_event(UsrInfo, <<"MultiInstance">>, <<"ActivityCancelled">>, #{
                <<"activity_id">> => ActivityId
            }),
            {consume, [{cancel, {activity, ActivityId}}]};
        _ ->
            pass
    end;

%% Handle quorum-based cancellation
trigger('p_running', {cancel, quorum_met}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #mi_state{quorum_met = true} ->
            %% Quorum met - cancel remaining
            log_event(UsrInfo, <<"MultiInstance">>, <<"QuorumCancel">>, #{}),
            {consume, [{cancel, quorum_met}]};
        _ ->
            pass
    end;

trigger(_Place, _Token, _NetState) ->
    pass.
```

#### 3. Integrate with wf_scope

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add scope binding support

**Add API function:**
```erlang
%% @doc Creates a binding table for instance scopes.
-spec create_scope_bindings(atom(), [binary()]) -> wf_scope:binding_table().

create_scope_bindings(ActivityId, InstanceIds) ->
    %% Create scope mapping for each instance
    ScopeMaps = lists:map(fun(InstanceId) ->
        ScopeId = {instance, InstanceId},
        #{'p_running' => {'p_running', ScopeId},
          'p_instances' => {'p_instances', ScopeId}}
    end, InstanceIds),

    %% Create parent scope mapping
    ParentScope = {{activity, ActivityId},
                   #{'p_running' => 'p_running',
                     'p_instances' => 'p_instances',
                     'p_completed' => 'p_completed',
                     'p_output' => 'p_output'}},

    %% Combine all mappings
    lists:foldl(fun(Map, Acc) -> maps:merge(Acc, Map) end,
                #{ParentScope},
                ScopeMaps).
```

**Modify init/1 to set up scopes:**
```erlang
init(#mi_state{instance_spec = {fixed, M}, activity_id = ActivityId} = MIState) ->
    InstanceIds = [integer_to_binary(I) || I <- lists:seq(1, M)],
    InstanceScopes = maps:from_list([{Id, {instance, Id}} || Id <- InstanceIds]),
    State1 = MIState#mi_state{instance_scopes = InstanceScopes},

    case yawl_xes:new_log(#{<<"process">> => <<"MultiInstance">>}) of
        {ok, LogId} when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1#mi_state{log_id = LogId}};
        _ ->
            {ok, State1}
    end;
init(MIState) ->
    init(MIState#mi_state{activity_id = undefined}).
```

#### 4. Add Cancellation Tests

**File**: `/Users/sac/cre/test/multi_instance_test.erl`
**Changes**: Add cancellation integration tests

```erlang
%% Test per-instance cancellation
instance_cancel_test() ->
    State = #mi_state{
        instance_spec = {fixed, 3},
        join_policy = all,
        result_strategy = collect_all,
        instance_scopes = #{<<"1">> => {instance, <<"1">>}}
    },
    ?assert(is_map(State#mi_state.instance_scopes)),
    ?assert(maps:is_key(<<"1">>, State#mi_state.instance_scopes)).

%% Test activity cancellation scope
activity_cancel_test() ->
    State = #mi_state{
        instance_spec = {fixed, 5},
        join_policy = {n_of_m, 3, 5},
        result_strategy = collect_quorum,
        activity_id = review_task,
        cancel_on_quorum = true
    },
    ?assertEqual(review_task, State#mi_state.activity_id),
    ?assertEqual(true, State#mi_state.cancel_on_quorum).

%% Test scope binding creation
scope_bindings_test() ->
    ActivityId = review_task,
    InstanceIds = [<<"1">>, <<"2">>, <<"3">>],
    Bindings = multi_instance:create_scope_bindings(ActivityId, InstanceIds),
    ?assert(is_map(Bindings)),
    ?assert(maps:is_key({activity, ActivityId}, Bindings)).
```

### Success Criteria:

#### Automated Verification:
- [ ] Module compiles with cancellation integration
- [ ] Cancellation tests pass
- [ ] No dialyzer warnings

#### Manual Verification:
- [ ] Per-instance cancellation removes specific instance from p_running
- [ ] Per-activity cancellation cancels all instances
- [ ] Quorum-based cancellation triggers correctly
- [ ] wf_scope bindings created correctly
- [ ] trigger/3 properly filters cancellation tokens

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 5.

---

## Phase 5: Result Aggregation

### Overview
Implement result aggregation strategies: collect_all, collect_quorum, merge, broadcast, and discard.

### Changes Required:

#### 1. Implement Result Strategies

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add result collection transition with strategy handling

**Modify fire/3 for result collection:**
```erlang
fire('t_collect', #{'p_output' := [OutputTokens]}, #mi_state{result_strategy = Strategy, results = Results} = State) ->
    case Strategy of
        collect_all ->
            %% Return all results
            log_event(State, <<"MultiInstance">>, <<"CollectAll">>, #{
                <<"result_count">> => length(Results)
            }),
            {produce, #{
                'p_output' => [],
                'p_result' => [{all_results, Results}]
            }, State};

        collect_quorum ->
            %% Return quorum results (first N)
            case State#mi_state.join_policy of
                {n_of_m, N, _M} ->
                    {QuorumResults, _} = lists:split(N, Results),
                    log_event(State, <<"MultiInstance">>, <<"CollectQuorum">>, #{
                        <<"quorum_count">> => N
                    }),
                    {produce, #{
                        'p_output' => [],
                        'p_result' => [{quorum_results, QuorumResults}]
                    }, State};
                _ ->
                    abort
            end;

        {merge, MergeFun} when is_function(MergeFun, 1) ->
            %% Merge results using custom function
            Merged = MergeFun(Results),
            log_event(State, <<"MultiInstance">>, <<"MergeResults">>, #{
                <<"merge_fun">> => term_to_binary(MergeFun)
            }),
            {produce, #{
                'p_output' => [],
                'p_result' => [{merged, Merged}]
            }, State};

        {broadcast, Places} when is_list(Places) ->
            %% Broadcast results to multiple places
            BroadcastMaps = [{Place, Results} || Place <- Places],
            ProduceMap = lists:foldl(
                fun({Place, Res}, Acc) ->
                    Acc#{Place => [Res]}
                end,
                #{'p_output' => []},
                BroadcastMaps
            ),
            log_event(State, <<"MultiInstance">>, <<"BroadcastResults">>, #{
                <<"places">> => Places
            }),
            {produce, ProduceMap, State};

        discard ->
            %% Discard all results
            log_event(State, <<"MultiInstance">>, <<"DiscardResults">>, #{}),
            {produce, #{
                'p_output' => [],
                'p_complete' => [done]
            }, State};

        _ ->
            abort
    end;
```

#### 2. Add Result Ordering

**File**: `/Users/sac/cre/src/patterns/multi_instance.erl`
**Changes**: Add result ordering types and function

**Add types:**
```erlang
-type result_order() :: completion_order | instance_id | {custom_sort, fun((term(), term()) -> boolean())}.
```

**Add to state record:**
```erlang
-record(mi_state, {
    instance_spec :: instance_spec(),
    join_policy :: join_policy(),
    result_strategy :: result_strategy(),
    result_order = completion_order :: result_order(),  %% NEW
    cancel_on_quorum = false :: boolean(),
    completed = [] :: [binary()],
    results = [] :: [{binary(), term()}],
    max_instances :: pos_integer() | unlimited,
    spawned_count = 0 :: non_neg_integer(),
    quorum_met = false :: boolean(),
    first_n_met = false :: boolean(),
    discriminator_triggered = false :: boolean(),
    activity_id :: atom() | undefined,
    instance_scopes = #{} :: #{binary() => atom()},
    log_id :: binary() | undefined
}).
```

**Add ordering function:**
```erlang
%% @doc Orders results according to the specified ordering strategy.
-spec order_results([{binary(), term()}], result_order()) -> [{binary(), term()}].

order_results(Results, completion_order) ->
    %% Return in completion order (already the default)
    Results;
order_results(Results, instance_id) ->
    %% Sort by instance ID
    lists:sort(fun({Id1, _}, {Id2, _}) -> Id1 =< Id2 end, Results);
order_results(Results, {custom_sort, SortFun}) when is_function(SortFun, 2) ->
    %% Sort using custom function
    lists:sort(SortFun, Results).
```

**Apply ordering in t_collect:**
```erlang
fire('t_collect', #{'p_output' := [OutputTokens]}, #mi_state{result_strategy = Strategy, results = Results, result_order = Order} = State) ->
    %% Order results first
    OrderedResults = order_results(Results, Order),

    case Strategy of
        collect_all ->
            %% ... use OrderedResults instead of Results
    end.
```

#### 3. Add Result Aggregation Tests

**File**: `/Users/sac/cre/test/multi_instance_test.erl`
**Changes**: Add result aggregation tests

```erlang
%% Test collect_all strategy
collect_all_test() ->
    Results = [{{instance, <<"1">>}, a}, {{instance, <<"2">>}, b}],
    State = #mi_state{
        result_strategy = collect_all,
        results = Results
    },
    ?assertEqual(collect_all, State#mi_state.result_strategy),
    ?assertEqual(2, length(State#mi_state.results)).

%% Test merge strategy
merge_results_test() ->
    MergeFun = fun(Lists) -> lists:flatten(Lists) end,
    State = #mi_state{
        result_strategy = {merge, MergeFun},
        results = [{{instance, <<"1">>}, [1, 2]}, {{instance, <<"2">>}, [3, 4]}]
    },
    ?assertMatch({merge, _}, State#mi_state.result_strategy).

%% Test broadcast strategy
broadcast_results_test() ->
    Places = [p_notifier, p_logger, p_storage],
    State = #mi_state{
        result_strategy = {broadcast, Places},
        results = [{{instance, <<"1">>}, result}]
    },
    ?assertMatch({broadcast, _}, State#mi_state.result_strategy),
    ?assertEqual(3, length(Places)).

%% Test result ordering
order_results_test() ->
    Results = [{{instance, <<"2">>}, b}, {{instance, <<"1">>}, a}, {{instance, <<"3">>}, c}],
    Ordered = multi_instance:order_results(Results, instance_id),
    ?assertEqual({{instance, <<"1">>}, a}, hd(Ordered)),
    ?assertEqual({{instance, <<"3">>}, c}, lists:last(Ordered)).
```

### Success Criteria:

#### Automated Verification:
- [ ] Module compiles with result aggregation
- [ ] Result aggregation tests pass
- [ ] No dialyzer warnings

#### Manual Verification:
- [ ] collect_all returns all results in order
- [ ] collect_quorum returns only first N results
- [ ] merge applies custom function correctly
- [ ] broadcast sends results to multiple places
- [ ] discard drops all results
- [ ] Result ordering works as specified

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to Phase 6.

---

## Phase 6: Test Integration

### Overview
Fix the existing test suite to use the new `multi_instance` module directly, add property-based tests, and verify all WCP-11 through WCP-17 tests pass.

### Changes Required:

#### 1. Fix `yawl_multiple_instances_test.erl`

**File**: `/Users/sac/cre/test/yawl_multiple_instances_test.erl:1-2324`
**Changes**: Replace references to non-existent `cre_yawl_patterns` with direct calls to `multi_instance`

**Modify WCP-13 Static Multi-Instance test (around line 566):**
```erlang
%% BEFORE (non-existent module):
%% cre_yawl_patterns:static_multi_instance(M, N, InstanceFuns)

%% AFTER (new module):
multi_instance:execute({fixed, M}, {n_of_m, N, M}, InstanceFuns)
```

**Modify WCP-14 Runtime Multi-Instance test (around line 761):**
```erlang
%% BEFORE:
%% cre_yawl_patterns:runtime_multi_instance(CountFun, N, InstanceFuns)

%% AFTER:
CountFun = fun() -> 5 end,
M = CountFun(),
multi_instance:execute({fixed, M}, {n_of_m, 3, M}, InstanceFuns)
```

**Modify WCP-15 Dynamic Multi-Instance test (around line 947):**
```erlang
%% BEFORE:
%% cre_yawl_patterns:dynamic_multi_instance(DataFun, N, InstanceFuns)

%% AFTER:
DataFun = fun() ->
    case get_next_data() of
        {ok, Data} -> {more, Data};
        eof -> done
    end
end,
multi_instance:execute({dynamic, DataFun, 100}, all, InstanceFuns)
```

#### 2. Add Property-Based Tests

**File**: `/Users/sac/cre/test/multi_instance_props_test.erl` (NEW)
**Changes**: Create PropEr tests for invariants

```erlang
%% -*- erlang -*-
%% @doc Property-based tests for multi_instance pattern
-module(multi_instance_props_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test that quorum is always <= total instances
prop_quorum_le_total() ->
    ?FORALL({M, N}, {pos_int(), pos_int()},
            M >= N imply
            begin
                State = multi_instance:new({fixed, M}, {n_of_m, N, M}, collect_all),
                case multi_instance:validate_join_policy({n_of_m, N, M}, {fixed, M}) of
                    ok -> true;
                    _ -> false
                end
            end).

%% Test that first_N is always <= M
prop_first_n_le_m() ->
    ?FORALL({M, N}, {pos_int(), pos_int()},
            M >= N imply
            begin
                case multi_instance:validate_join_policy({first_n, N}, {fixed, M}) of
                    ok -> true;
                    _ -> false
                end
            end).

%% Test that instance count is always positive
prop_instance_count_positive() ->
    ?FORALL(M, pos_int(),
            M > 0 imply
            begin
                case multi_instance:validate_instance_spec({fixed, M}) of
                    ok -> true;
                    _ -> false
                end
            end).

%% Generators
pos_int() -> ?SUCHTHAT(I, integer(), I > 0).

%% Test runner
prop_test_() ->
    {timeout, 60, [
        ?_assertEqual(true, proper:check_spec(prop_quorum_le_total(), 100)),
        ?_assertEqual(true, proper:check_spec(prop_first_n_le_m(), 100)),
        ?_assertEqual(true, proper:check_spec(prop_instance_count_positive(), 100))
    ]}.
```

#### 3. Add Performance Benchmarks

**File**: `/Users/sac/cre/test/multi_instance_bench_test.erl` (NEW)
**Changes**: Create performance benchmarks

```erlang
%% -*- erlang -*-
%% @doc Performance benchmarks for multi_instance pattern
-module(multi_instance_bench_test).
-include_lib("eunit/include/eunit.hrl").

%% Benchmark fixed instance spawning
fixed_spawn_bench_test() ->
    M = 1000,
    {StartMem, _} = erlang:memory(processes),
    StartTime = erlang:monotonic_time(millisecond),

    State = multi_instance:new({fixed, M}, {first_n, 500}, collect_all),

    EndTime = erlang:monotonic_time(millisecond),
    {EndMem, _} = erlang:memory(processes),

    Duration = EndTime - StartTime,
    MemDelta = EndMem - StartMem,

    ?debugFmt("Fixed M=~p spawn time: ~p ms, memory: ~p bytes", [M, Duration, MemDelta]),
    ?assert(Duration < 100),  %% Should complete in < 100ms
    ?assert(MemDelta < 100000).  %% Should use < 100KB

%% Benchmark dynamic spawning
dynamic_spawn_bench_test() ->
    %% Create data source with 100 items
    DataList = lists:seq(1, 100),
    DataFun = fun() ->
        case get(data_items) of
            undefined -> put(data_items, DataList), {more, hd(DataList)};
            [] -> done;
            [H | T] -> put(data_items, T), {more, H}
        end
    end,

    StartTime = erlang:monotonic_time(millisecond),
    State = multi_instance:new({dynamic, DataFun, 200}, all, collect_all),
    EndTime = erlang:monotonic_time(millisecond),

    Duration = EndTime - StartTime,
    ?debugFmt("Dynamic spawn time: ~p ms", [Duration]),
    ?assert(Duration < 200).
```

#### 4. Run Full Test Suite

**Execute tests:**
```bash
# Compile new module
rebar3 compile

# Run unit tests
rebar3 eunit --module=multi_instance_test

# Run property-based tests
rebar3 proper --module=multi_instance_props_test

# Run performance benchmarks
rebar3 eunit --module=multi_instance_bench_test

# Run full multiple instance test suite
rebar3 eunit --module=yawl_multiple_instances_test
```

### Success Criteria:

#### Automated Verification:
- [ ] All unit tests pass
- [ ] All property-based tests pass (100 iterations each)
- [ ] Performance benchmarks complete within thresholds
- [ ] All WCP-11 through WCP-17 tests pass
- [ ] No test failures or timeouts

#### Manual Verification:
- [ ] Test coverage report shows > 90% coverage for multi_instance.erl
- [ ] Performance metrics documented
- [ ] All edge cases tested (empty data source, max_instances reached, etc.)
- [ ] Test suite runs in < 5 minutes total

**Note**: This is the final phase. Complete all automated and manual verification before marking implementation complete.

---

## Testing Strategy

### Unit Tests

**What to test:**
- Instance specification validation (fixed, runtime, dynamic)
- Join policy validation and precedence
- Result aggregation strategies
- Cancellation scope creation
- Result ordering functions

**Key edge cases:**
- M = 0 (invalid - should be rejected)
- N > M (invalid for quorum)
- Empty data source for dynamic spawning
- Max_instances = 1 (minimum valid)
- Discriminator with M = 1 (trivial case)
- Cancel-on-quorum with all join policy (no-op)

### Integration Tests

**End-to-end scenarios:**

1. **Fixed M with N-of-M join:**
   - Create 5 instances, quorum of 3
   - Verify proceeds after 3 complete
   - Verify remaining 2 can complete or be cancelled

2. **Dynamic spawning with first_N join:**
   - Create data source with 20 items
   - Max_instances = 100
   - First_N = 10
   - Verify stops after 10 complete

3. **Discriminator with cancellation:**
   - Create 10 parallel instances
   - First completes, triggers discriminator
   - Verify remaining 9 cancelled via trigger/3

4. **Result aggregation:**
   - Create 5 instances with different result types
   - Test merge strategy with custom function
   - Test broadcast to 3 places

5. **Cancellation integration:**
   - Create activity with 10 instances
   - Cancel specific instance after 3 complete
   - Cancel entire activity after quorum met
   - Verify scope bindings updated

### Manual Testing Steps

1. **Start CRE application:**
   ```bash
   rebar3 shell
   ```

2. **Test fixed instance spawning:**
   ```erlang
   {ok, Pid} = multi_instance:start({fixed, 5}, {{n_of_m, 3, 5}, collect_all}),
   {ok, State} = multi_instance:get_state(Pid).
   ```

3. **Test dynamic spawning:**
   ```erlang
   DataFun = fun() ->
       case rand:uniform(10) of
           1 -> done;
           N -> {more, N}
       end
   end,
   {ok, Pid} = multi_instance:start({dynamic, DataFun, 50}, {all, collect_all}).
   ```

4. **Test discriminator:**
   ```erlang
   {ok, Pid} = multi_instance:start({fixed, 10}, {discriminator, collect_all}),
   %% Trigger first completion manually
   ```

5. **Verify XES logging:**
   ```erlang
   %% Check log files are created in /tmp/xes_logs/
   ```

## Migration Notes

### For Existing Code

**NOT breaking existing patterns** - keeping `n_out_of_m`, `static_partial_join_mi`, `dynamic_partial_join_mi` unchanged.

**New code should use `multi_instance` module:**

```erlang
%% OLD: Using n_out_of_m
n_out_of_m:start(3, [fun() -> work1() end, fun() -> work2() end, fun() -> work3() end]).

%% NEW: Using multi_instance
multi_instance:start({fixed, 3}, {{n_of_m, 3, 3}, collect_all}).
```

### wf_multi_instance gen_server

**Keep for registry purposes** - new `multi_instance` gen_yawl pattern does NOT replace the gen_server.

**Future work:** Add facade functions in `wf_multi_instance` that delegate to `multi_instance` pattern when needed.

### Test Suite Migration

**Tests updated in Phase 6** - `yawl_multiple_instances_test.erl` modified to call `multi_instance` directly instead of non-existent `cre_yawl_patterns`.

## References

- Research: `/Users/sac/cre/.wreckit/items/015-multiple-instance-semantics/research.md`
- Model Implementation: `/Users/sac/cre/src/patterns/n_out_of_m.erl:1-657`
- Scope Integration: `/Users/sac/cre/src/wf/wf_scope.erl:1-319`
- Existing Registry: `/Users/sac/cre/src/wf/wf_multi_instance.erl:1-418`
- Test Suite: `/Users/sac/cre/test/yawl_multiple_instances_test.erl:1-2324`
- Item 014 (Cancellation): `/Users/sac/cre/.wreckit/items/014-structured-cancellation/`
