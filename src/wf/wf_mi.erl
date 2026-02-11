%%% @doc WF Multiple Instances - MI Pattern Implementation
%%%
%%% This module implements multiple instance (MI) semantics for the WF Substrate.
%%% MI allows spawning and managing N instances of a workflow pattern, with
%%% configurable join policies.
%%%
%%% MI policies:
%%% - {fixed, N}: spawn exactly N instances
%%% - {dynamic, CollectorFun}: spawn instances dynamically using collector
%%%
%%% MI join policies determine when to proceed:
%%% - all: wait for all instances
%%% - {first_n, N}: wait for first N instances
%%% - {threshold, Threshold}: wait for threshold count
%%%
%%% @end
-module(wf_mi).

-export([
    %% MI policy evaluation
    spawn_instances/2,
    should_join/3,

    %% Instance tracking
    track_instance/3,
    get_instance_count/1,
    get_completed_count/1,

    %% Context distribution
    distribute_context/2,
    collect_contexts/1
]).

-export_type([
    mi_policy/0,
    mi_state/0,
    instance_id/0,
    instance_status/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% MI policy from wf_term
-type mi_policy() ::
      {fixed, non_neg_integer()}
    | {dynamic, fun((wf_term:context()) -> {next, wf_term:context()} | done)}.

%% MI execution state (tracked in frame data)
-type mi_state() :: #{
    policy := mi_policy(),
    total := non_neg_integer(),
    completed := non_neg_integer(),
    instances := #{instance_id() => instance_status()},
    contexts := [wf_term:context()]
}.

-type instance_id() :: non_neg_integer().

-type instance_status() ::
      pending
    | running
    | completed
    | failed.

%%% API =====================================================================

%% @doc Determine how many instances to spawn based on policy and context.
%%
%% For fixed policy, returns the fixed count.
%% For dynamic policy, evaluates collector function until 'done'.
%%
-spec spawn_instances(Policy :: mi_policy(), Ctx :: wf_term:context()) ->
    {ok, non_neg_integer(), [wf_term:context()]} | {error, term()}.
spawn_instances({fixed, N}, Ctx) when is_integer(N), N >= 1 ->
    %% Fixed count: distribute context to N instances
    Contexts = replicate_context(Ctx, N),
    {ok, N, Contexts};

spawn_instances({dynamic, CollectorFun}, InitCtx) when is_function(CollectorFun, 1) ->
    %% Dynamic collection: call collector until done
    case collect_dynamic_instances(CollectorFun, InitCtx, [], 0) of
        {ok, Count, Contexts} ->
            {ok, Count, Contexts};
        {error, Reason} ->
            {error, {collector_failed, Reason}}
    end;

spawn_instances(Policy, _Ctx) ->
    {error, {invalid_mi_policy, Policy}}.

%% @doc Check if join condition is met based on MI policy.
%%
%% Determines if enough instances have completed to proceed.
%%
-spec should_join(
    Policy :: mi_policy(),
    Total :: non_neg_integer(),
    Completed :: non_neg_integer()
) -> boolean().
should_join({fixed, N}, Total, Completed) when Total == N ->
    %% Fixed MI: wait for all instances by default
    Completed >= Total;

should_join({dynamic, _CollectorFun}, Total, Completed) ->
    %% Dynamic MI: wait for all collected instances
    Completed >= Total;

should_join(_Policy, _Total, _Completed) ->
    false.

%% @doc Track an instance's status in MI state.
-spec track_instance(
    MIState :: mi_state(),
    InstanceId :: instance_id(),
    Status :: instance_status()
) -> mi_state().
track_instance(#{instances := Instances} = MIState, InstanceId, Status) ->
    NewInstances = maps:put(InstanceId, Status, Instances),
    NewCompleted = case Status of
        completed ->
            maps:get(completed, MIState, 0) + 1;
        _ ->
            maps:get(completed, MIState, 0)
    end,
    MIState#{
        instances => NewInstances,
        completed => NewCompleted
    }.

%% @doc Get total instance count from MI state.
-spec get_instance_count(MIState :: mi_state()) -> non_neg_integer().
get_instance_count(#{total := Total}) ->
    Total.

%% @doc Get completed instance count from MI state.
-spec get_completed_count(MIState :: mi_state()) -> non_neg_integer().
get_completed_count(#{completed := Completed}) ->
    Completed.

%% @doc Distribute a context to multiple instances.
%%
%% Each instance gets a copy of the context with instance-specific data.
%%
-spec distribute_context(
    Ctx :: wf_term:context(),
    N :: non_neg_integer()
) -> [wf_term:context()].
distribute_context(Ctx, N) when is_integer(N), N >= 1 ->
    [add_instance_id(Ctx, InstanceId) || InstanceId <- lists:seq(0, N - 1)];
distribute_context(_Ctx, _N) ->
    [].

%% @doc Collect contexts from completed instances.
%%
%% Merges instance contexts into a single context, preserving results.
%%
-spec collect_contexts(Contexts :: [wf_term:context()]) -> wf_term:context().
collect_contexts([]) ->
    #{};
collect_contexts([FirstCtx | RestContexts]) ->
    %% Merge all contexts, collecting results
    lists:foldl(fun merge_contexts/2, FirstCtx, RestContexts).

%%% INTERNAL FUNCTIONS ======================================================

%% @doc Replicate a context N times for fixed MI.
-spec replicate_context(Ctx :: wf_term:context(), N :: non_neg_integer()) ->
    [wf_term:context()].
replicate_context(Ctx, N) when is_integer(N), N >= 1 ->
    [add_instance_id(Ctx, I) || I <- lists:seq(0, N - 1)];
replicate_context(_Ctx, _N) ->
    [].

%% @doc Add instance ID to context token data.
-spec add_instance_id(
    Ctx :: wf_term:context(),
    InstanceId :: instance_id()
) -> wf_term:context().
add_instance_id(Ctx, InstanceId) ->
    TokenData = maps:get(token_data, Ctx, #{}),
    NewTokenData = TokenData#{instance_id => InstanceId},
    Ctx#{token_data => NewTokenData}.

%% @doc Dynamically collect instances using collector function.
-spec collect_dynamic_instances(
    CollectorFun :: fun((wf_term:context()) -> {next, wf_term:context()} | done),
    CurrentCtx :: wf_term:context(),
    Acc :: [wf_term:context()],
    Count :: non_neg_integer()
) -> {ok, non_neg_integer(), [wf_term:context()]} | {error, term()}.
collect_dynamic_instances(CollectorFun, CurrentCtx, Acc, Count) ->
    case catch CollectorFun(CurrentCtx) of
        {next, NextCtx} ->
            %% Collector returned next instance context
            InstanceCtx = add_instance_id(NextCtx, Count),
            collect_dynamic_instances(CollectorFun, NextCtx, [InstanceCtx | Acc], Count + 1);

        done ->
            %% Collector finished
            {ok, Count, lists:reverse(Acc)};

        Other ->
            {error, {invalid_collector_return, Other}}
    end.

%% @doc Merge two contexts, combining their data.
-spec merge_contexts(
    Ctx1 :: wf_term:context(),
    Ctx2 :: wf_term:context()
) -> wf_term:context().
merge_contexts(Ctx1, Ctx2) ->
    %% Merge data maps
    Data1 = maps:get(data, Ctx1, #{}),
    Data2 = maps:get(data, Ctx2, #{}),
    MergedData = maps:merge(Data1, Data2),

    %% Merge results maps
    Results1 = maps:get(results, Ctx1, #{}),
    Results2 = maps:get(results, Ctx2, #{}),
    MergedResults = maps:merge(Results1, Results2),

    %% Combine signals (order preserved)
    Signals1 = maps:get(signals, Ctx1, []),
    Signals2 = maps:get(signals, Ctx2, []),
    MergedSignals = Signals1 ++ Signals2,

    #{
        data => MergedData,
        signals => MergedSignals,
        results => MergedResults
    }.

%%% TESTS ===================================================================

%% Test spawn_instances with fixed policy
spawn_instances_fixed_test() ->
    Policy = {fixed, 3},
    Ctx = #{data => #{value => 42}},
    {ok, Count, Contexts} = spawn_instances(Policy, Ctx),
    ?assertEqual(3, Count),
    ?assertEqual(3, length(Contexts)),
    %% Verify each context has an instance ID
    [Ctx0, Ctx1, Ctx2] = Contexts,
    ?assertEqual(0, maps:get(instance_id, maps:get(token_data, Ctx0))),
    ?assertEqual(1, maps:get(instance_id, maps:get(token_data, Ctx1))),
    ?assertEqual(2, maps:get(instance_id, maps:get(token_data, Ctx2))).

%% Test spawn_instances with dynamic policy
spawn_instances_dynamic_test() ->
    %% Collector that generates 5 instances
    Collector = fun(Ctx) ->
        Count = maps:get(count, maps:get(data, Ctx, #{}), 0),
        if
            Count < 5 ->
                NewCtx = Ctx#{data => #{count => Count + 1}},
                {next, NewCtx};
            true ->
                done
        end
    end,
    Policy = {dynamic, Collector},
    InitCtx = #{data => #{count => 0}},
    {ok, Count, Contexts} = spawn_instances(Policy, InitCtx),
    ?assertEqual(5, Count),
    ?assertEqual(5, length(Contexts)).

%% Test spawn_instances with invalid policy
spawn_instances_invalid_test() ->
    ?assertMatch({error, {invalid_mi_policy, _}}, spawn_instances(invalid, #{})),
    ?assertMatch({error, {invalid_mi_policy, _}}, spawn_instances({fixed, 0}, #{})).

%% Test should_join for fixed policy
should_join_fixed_test() ->
    Policy = {fixed, 3},
    ?assertEqual(false, should_join(Policy, 3, 0)),
    ?assertEqual(false, should_join(Policy, 3, 2)),
    ?assertEqual(true, should_join(Policy, 3, 3)),
    ?assertEqual(true, should_join(Policy, 3, 4)).

%% Test should_join for dynamic policy
should_join_dynamic_test() ->
    Collector = fun(_) -> done end,
    Policy = {dynamic, Collector},
    ?assertEqual(false, should_join(Policy, 5, 3)),
    ?assertEqual(true, should_join(Policy, 5, 5)).

%% Test track_instance
track_instance_test() ->
    MIState = #{
        policy => {fixed, 3},
        total => 3,
        completed => 0,
        instances => #{},
        contexts => []
    },
    MIState1 = track_instance(MIState, 0, running),
    ?assertEqual(running, maps:get(0, maps:get(instances, MIState1))),
    ?assertEqual(0, maps:get(completed, MIState1)),

    MIState2 = track_instance(MIState1, 0, completed),
    ?assertEqual(completed, maps:get(0, maps:get(instances, MIState2))),
    ?assertEqual(1, maps:get(completed, MIState2)).

%% Test get_instance_count
get_instance_count_test() ->
    MIState = #{total => 5, completed => 0, instances => #{}},
    ?assertEqual(5, get_instance_count(MIState)).

%% Test get_completed_count
get_completed_count_test() ->
    MIState = #{total => 5, completed => 3, instances => #{}},
    ?assertEqual(3, get_completed_count(MIState)).

%% Test distribute_context
distribute_context_test() ->
    Ctx = #{data => #{value => 100}},
    Contexts = distribute_context(Ctx, 3),
    ?assertEqual(3, length(Contexts)),
    %% Check instance IDs
    [C0, C1, C2] = Contexts,
    ?assertEqual(0, maps:get(instance_id, maps:get(token_data, C0))),
    ?assertEqual(1, maps:get(instance_id, maps:get(token_data, C1))),
    ?assertEqual(2, maps:get(instance_id, maps:get(token_data, C2))).

%% Test collect_contexts
collect_contexts_test() ->
    Ctx1 = #{data => #{a => 1}, results => #{r1 => ok}, signals => [s1]},
    Ctx2 = #{data => #{b => 2}, results => #{r2 => ok}, signals => [s2]},
    Ctx3 = #{data => #{c => 3}, results => #{r3 => ok}, signals => [s3]},

    Merged = collect_contexts([Ctx1, Ctx2, Ctx3]),

    %% Check merged data
    Data = maps:get(data, Merged),
    ?assertEqual(1, maps:get(a, Data)),
    ?assertEqual(2, maps:get(b, Data)),
    ?assertEqual(3, maps:get(c, Data)),

    %% Check merged results
    Results = maps:get(results, Merged),
    ?assertEqual(ok, maps:get(r1, Results)),
    ?assertEqual(ok, maps:get(r2, Results)),
    ?assertEqual(ok, maps:get(r3, Results)),

    %% Check merged signals (order preserved)
    Signals = maps:get(signals, Merged),
    ?assertEqual([s1, s2, s3], Signals).

%% Test collect_contexts with empty list
collect_contexts_empty_test() ->
    ?assertEqual(#{}, collect_contexts([])).

%% Test merge_contexts
merge_contexts_test() ->
    Ctx1 = #{data => #{a => 1}, results => #{r1 => val1}},
    Ctx2 = #{data => #{b => 2}, results => #{r2 => val2}},
    Merged = merge_contexts(Ctx1, Ctx2),

    Data = maps:get(data, Merged),
    ?assertEqual(1, maps:get(a, Data)),
    ?assertEqual(2, maps:get(b, Data)),

    Results = maps:get(results, Merged),
    ?assertEqual(val1, maps:get(r1, Results)),
    ?assertEqual(val2, maps:get(r2, Results)).

%% Test replicate_context
replicate_context_test() ->
    Ctx = #{data => #{x => 10}},
    Contexts = replicate_context(Ctx, 4),
    ?assertEqual(4, length(Contexts)),
    %% Verify instance IDs
    lists:foreach(
        fun({I, C}) ->
            TokenData = maps:get(token_data, C),
            ?assertEqual(I, maps:get(instance_id, TokenData))
        end,
        lists:zip(lists:seq(0, 3), Contexts)
    ).

%% Test add_instance_id
add_instance_id_test() ->
    Ctx = #{data => #{value => 5}},
    CtxWithId = add_instance_id(Ctx, 7),
    TokenData = maps:get(token_data, CtxWithId),
    ?assertEqual(7, maps:get(instance_id, TokenData)),
    %% Original data preserved
    ?assertEqual(5, maps:get(value, maps:get(data, CtxWithId))).

%% Test collect_dynamic_instances
collect_dynamic_instances_test() ->
    Collector = fun(Ctx) ->
        N = maps:get(n, maps:get(data, Ctx, #{}), 0),
        if
            N < 3 ->
                {next, Ctx#{data => #{n => N + 1}}};
            true ->
                done
        end
    end,
    InitCtx = #{data => #{n => 0}},
    {ok, Count, Contexts} = collect_dynamic_instances(Collector, InitCtx, [], 0),
    ?assertEqual(3, Count),
    ?assertEqual(3, length(Contexts)).

%% Test collector error handling
collect_dynamic_instances_error_test() ->
    BadCollector = fun(_) -> invalid_return end,
    InitCtx = #{},
    Result = collect_dynamic_instances(BadCollector, InitCtx, [], 0),
    ?assertMatch({error, {invalid_collector_return, _}}, Result).
