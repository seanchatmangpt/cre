%%% @doc WF Trace - Structured Events and Replay
%%%
%%% This module provides structured event tracing and deterministic replay
%%% capabilities for workflow execution. Every reduction step produces a
%%% trace event that captures the complete execution state at that point.
%%%
%%% Key features:
%%% - Emit trace events at each execution step
%%% - Store trace logs for observability and debugging
%%% - Deterministic replay from trace logs
%%% - Trace comparison for testing and validation
%%% - Trace persistence and serialization
%%%
%%% @end
-module(wf_trace).

%% API exports
-export([
    %% Trace event creation
    event/6,
    event/7,

    %% Trace log operations
    new_log/0,
    append_event/2,
    get_events/1,
    log_size/1,

    %% Trace querying
    filter_by_type/2,
    filter_by_opcode/2,
    filter_by_scope/2,
    get_last_event/1,
    get_event_at/2,

    %% Replay capability
    replay/2,
    replay_validate/3,

    %% Trace comparison
    compare_traces/2,
    traces_match/2,
    diff_traces/2,

    %% Serialization
    to_list/1,
    from_list/1,
    to_binary/1,
    from_binary/1,

    %% Statistics and analysis
    trace_stats/1,
    execution_timeline/1,
    scope_depth_analysis/1
]).

%% Type exports
-export_type([
    trace_event/0,
    trace_log/0,
    event_type/0,
    trace_diff/0,
    trace_stats/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% Trace event structure (matches wf_vm:trace_event/0)
-type trace_event() :: {
    Seq :: non_neg_integer(),        % Global sequence number
    Type :: event_type(),             % Event type
    Opcode :: atom(),                 % Opcode executed
    Ctx :: wf_term:context(),         % Context after step
    Timestamp :: non_neg_integer(),   % Microseconds (monotonic)
    Scope :: [atom()],                % Scope nesting path
    CancelSignal :: boolean()         % Was this step cancelled?
}.

%% Event types
-type event_type() ::
      step_exec        % Generic step execution
    | task_enter       % Task entry
    | task_ok          % Task success
    | task_error       % task failure
    | task_exit        % Task exit
    | seq_enter        % Sequence entry
    | seq_exit         % Sequence exit
    | par_fork         % Parallel fork
    | par_join         % Parallel join (all)
    | par_join_wait    % Parallel join waiting
    | par_join_xor     % XOR merge
    | par_join_sync    % Synchronizing merge
    | par_join_first_n % First-N join
    | xor_choose       % Exclusive choice
    | loop_back        % Loop back edge
    | defer_race       % Deferred choice race
    | join_wait        % General join wait
    | join_wait_done   % Join completed
    | effect_yield     % Effect yielded
    | effect_resume    % Effect resumed
    | cancel_enter     % Cancellation scope entry
    | cancel_exit      % Cancellation scope exit
    | mi_spawn         % Multiple instance spawn
    | mi_join          % Multiple instance join
    | halt             % Execution halt
    | error.           % Error termination

%% Trace log (ordered sequence of events)
-type trace_log() :: [trace_event()].

%% Trace comparison result
-type trace_diff() :: #{
    matching => non_neg_integer(),
    mismatched => [mismatch()],
    length_diff => {non_neg_integer(), non_neg_integer()}
}.

-type mismatch() :: #{
    seq => non_neg_integer(),
    field => atom(),
    expected => term(),
    actual => term()
}.

%% Trace statistics
-type trace_stats() :: #{
    total_events => non_neg_integer(),
    by_type => #{event_type() => non_neg_integer()},
    by_opcode => #{atom() => non_neg_integer()},
    max_scope_depth => non_neg_integer(),
    execution_time_us => non_neg_integer(),
    cancelled_steps => non_neg_integer()
}.

%%% TRACE EVENT CREATION ====================================================

%% @doc Create a trace event.
-spec event(
    Seq :: non_neg_integer(),
    Type :: event_type(),
    Opcode :: atom(),
    Ctx :: wf_term:context(),
    Scope :: [atom()],
    CancelSignal :: boolean()
) -> trace_event().
event(Seq, Type, Opcode, Ctx, Scope, CancelSignal) ->
    event(Seq, Type, Opcode, Ctx, erlang:monotonic_time(microsecond), Scope, CancelSignal).

%% @doc Create a trace event with explicit timestamp.
-spec event(
    Seq :: non_neg_integer(),
    Type :: event_type(),
    Opcode :: atom(),
    Ctx :: wf_term:context(),
    Timestamp :: non_neg_integer(),
    Scope :: [atom()],
    CancelSignal :: boolean()
) -> trace_event().
event(Seq, Type, Opcode, Ctx, Timestamp, Scope, CancelSignal)
  when is_integer(Seq), Seq >= 0,
       is_atom(Type),
       is_atom(Opcode),
       is_map(Ctx),
       is_integer(Timestamp),
       is_list(Scope),
       is_boolean(CancelSignal) ->
    {Seq, Type, Opcode, Ctx, Timestamp, Scope, CancelSignal}.

%%% TRACE LOG OPERATIONS ====================================================

%% @doc Create a new empty trace log.
-spec new_log() -> trace_log().
new_log() ->
    [].

%% @doc Append an event to a trace log.
-spec append_event(Log :: trace_log(), Event :: trace_event()) -> trace_log().
append_event(Log, Event) when is_list(Log) ->
    Log ++ [Event].

%% @doc Get all events from a trace log.
-spec get_events(Log :: trace_log()) -> [trace_event()].
get_events(Log) when is_list(Log) ->
    Log.

%% @doc Get the number of events in a trace log.
-spec log_size(Log :: trace_log()) -> non_neg_integer().
log_size(Log) when is_list(Log) ->
    length(Log).

%%% TRACE QUERYING ==========================================================

%% @doc Filter events by type.
-spec filter_by_type(Log :: trace_log(), Type :: event_type()) -> [trace_event()].
filter_by_type(Log, Type) when is_list(Log), is_atom(Type) ->
    [Event || Event = {_, EventType, _, _, _, _, _} <- Log, EventType == Type].

%% @doc Filter events by opcode.
-spec filter_by_opcode(Log :: trace_log(), Opcode :: atom()) -> [trace_event()].
filter_by_opcode(Log, Opcode) when is_list(Log), is_atom(Opcode) ->
    [Event || Event = {_, _, EventOpcode, _, _, _, _} <- Log, EventOpcode == Opcode].

%% @doc Filter events by scope (events within a specific scope).
-spec filter_by_scope(Log :: trace_log(), ScopeId :: atom()) -> [trace_event()].
filter_by_scope(Log, ScopeId) when is_list(Log), is_atom(ScopeId) ->
    [Event || Event = {_, _, _, _, _, Scope, _} <- Log, lists:member(ScopeId, Scope)].

%% @doc Get the last event in the trace log.
-spec get_last_event(Log :: trace_log()) -> {ok, trace_event()} | {error, empty}.
get_last_event([]) ->
    {error, empty};
get_last_event(Log) when is_list(Log) ->
    {ok, lists:last(Log)}.

%% @doc Get the event at a specific sequence number.
-spec get_event_at(Log :: trace_log(), Seq :: non_neg_integer()) ->
    {ok, trace_event()} | {error, not_found}.
get_event_at(Log, Seq) when is_list(Log), is_integer(Seq), Seq >= 0 ->
    case [Event || Event = {EventSeq, _, _, _, _, _, _} <- Log, EventSeq == Seq] of
        [Event] -> {ok, Event};
        [] -> {error, not_found};
        _ -> {error, duplicate_seq}
    end.

%%% REPLAY CAPABILITY =======================================================

%% @doc Replay execution from a compiled pattern using a trace log.
%%
%% This function executes the pattern and verifies that each step produces
%% the same trace event as recorded in the provided trace log. This enables
%% deterministic replay validation.
%%
%% Returns {ok, FinalState} if replay matches, or {error, Reason} on mismatch.
%%
%% @end
-spec replay(
    Compiled :: wf_compile:compiled(),
    TraceLog :: trace_log()
) -> {ok, wf_vm:exec_state()} | {error, term()}.
replay(Compiled, TraceLog) when is_list(TraceLog) ->
    InitCtx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    State = wf_exec:exec_init(Compiled, InitCtx),
    replay_step(State, TraceLog, 0).

-spec replay_step(
    State :: wf_vm:exec_state(),
    RemainingTrace :: trace_log(),
    StepCount :: non_neg_integer()
) -> {ok, wf_vm:exec_state()} | {error, term()}.
replay_step(State, [], _StepCount) ->
    %% All trace events matched
    {ok, State};
replay_step(State, [ExpectedEvent | RestTrace], StepCount) ->
    case wf_exec:exec_step(State) of
        {continue, NewState} ->
            case validate_event(NewState, ExpectedEvent, StepCount) of
                ok ->
                    replay_step(NewState, RestTrace, StepCount + 1);
                {error, Reason} ->
                    {error, {replay_mismatch, StepCount, Reason}}
            end;
        {halt, _Status, NewState} ->
            case RestTrace of
                [] ->
                    {ok, NewState};
                _ ->
                    {error, {early_halt, StepCount, length(RestTrace)}}
            end;
        {error, Reason, _NewState} ->
            {error, {execution_error, StepCount, Reason}};
        {yield, _Spec, _NewState} ->
            %% For now, we don't support effect replay
            {error, {effect_not_supported, StepCount}}
    end.

%% @doc Validate that the current state matches the expected trace event.
-spec validate_event(
    State :: wf_vm:exec_state(),
    Expected :: trace_event(),
    StepCount :: non_neg_integer()
) -> ok | {error, term()}.
validate_event(State, {ExpSeq, ExpType, ExpOpcode, _ExpCtx, _ExpTs, ExpScope, ExpCancel}, StepCount) ->
    Trace = wf_vm:exec_trace(State),
    case lists:nth(StepCount + 1, Trace, undefined) of
        undefined ->
            {error, no_event_at_step};
        {ActSeq, ActType, ActOpcode, _ActCtx, _ActTs, ActScope, ActCancel} ->
            %% Compare key fields (relaxed context and timestamp comparison)
            if
                ExpSeq =/= ActSeq ->
                    {error, {seq_mismatch, ExpSeq, ActSeq}};
                ExpType =/= ActType ->
                    {error, {type_mismatch, ExpType, ActType}};
                ExpOpcode =/= ActOpcode ->
                    {error, {opcode_mismatch, ExpOpcode, ActOpcode}};
                ExpScope =/= ActScope ->
                    {error, {scope_mismatch, ExpScope, ActScope}};
                ExpCancel =/= ActCancel ->
                    {error, {cancel_mismatch, ExpCancel, ActCancel}};
                true ->
                    ok
            end
    end.

%% @doc Replay and validate execution against a reference trace.
%%
%% This is a convenience function that compiles, executes, and validates
%% in a single call.
%%
%% @end
-spec replay_validate(
    Pattern :: wf_term:wf_term(),
    InitCtx :: wf_term:context(),
    ReferenceTrace :: trace_log()
) -> ok | {error, term()}.
replay_validate(Pattern, InitCtx, ReferenceTrace) ->
    case wf_compile:compile(Pattern) of
        {ok, Compiled} ->
            State = wf_exec:exec_init(Compiled, InitCtx),
            case wf_exec:exec_until_halt(State) of
                {halt, ok, FinalState} ->
                    ActualTrace = wf_vm:exec_trace(FinalState),
                    case traces_match(ReferenceTrace, ActualTrace) of
                        true -> ok;
                        false -> {error, {trace_mismatch, diff_traces(ReferenceTrace, ActualTrace)}}
                    end;
                {error, Reason, _} ->
                    {error, {execution_failed, Reason}};
                Other ->
                    {error, {unexpected_result, Other}}
            end;
        {error, Reason} ->
            {error, {compile_failed, Reason}}
    end.

%%% TRACE COMPARISON ========================================================

%% @doc Compare two trace logs and return a detailed diff.
-spec compare_traces(Expected :: trace_log(), Actual :: trace_log()) -> trace_diff().
compare_traces(Expected, Actual) when is_list(Expected), is_list(Actual) ->
    Diff = diff_traces(Expected, Actual),
    LenExp = length(Expected),
    LenAct = length(Actual),
    Matching = count_matching_events(Expected, Actual),
    #{
        matching => Matching,
        mismatched => maps:get(mismatches, Diff, []),
        length_diff => {LenExp, LenAct}
    }.

%% @doc Check if two trace logs match exactly.
-spec traces_match(Log1 :: trace_log(), Log2 :: trace_log()) -> boolean().
traces_match(Log1, Log2) when length(Log1) =/= length(Log2) ->
    false;
traces_match(Log1, Log2) ->
    lists:all(fun({E1, E2}) -> events_match(E1, E2) end,
              lists:zip(Log1, Log2)).

%% @doc Compute a detailed diff between two trace logs.
-spec diff_traces(Expected :: trace_log(), Actual :: trace_log()) -> #{
    matching => non_neg_integer(),
    mismatches => [mismatch()],
    expected_length => non_neg_integer(),
    actual_length => non_neg_integer()
}.
diff_traces(Expected, Actual) ->
    Pairs = lists:zip(
        Expected ++ lists:duplicate(max(0, length(Actual) - length(Expected)), undefined),
        Actual ++ lists:duplicate(max(0, length(Expected) - length(Actual)), undefined)
    ),
    Mismatches = lists:filtermap(
        fun({E, A}) ->
            case {E, A} of
                {undefined, _} -> {true, #{seq => length(Expected), field => missing, expected => E, actual => A}};
                {_, undefined} -> {true, #{seq => length(Actual), field => extra, expected => E, actual => A}};
                _ ->
                    case event_diff(E, A) of
                        [] -> false;
                        Diffs -> {true, Diffs}
                    end
            end
        end,
        Pairs
    ),
    #{
        matching => count_matching_events(Expected, Actual),
        mismatches => lists:flatten(Mismatches),
        expected_length => length(Expected),
        actual_length => length(Actual)
    }.

%% Helper: check if two events match (relaxed comparison)
-spec events_match(trace_event(), trace_event()) -> boolean().
events_match(
    {Seq1, Type1, Opcode1, _Ctx1, _Ts1, Scope1, Cancel1},
    {Seq2, Type2, Opcode2, _Ctx2, _Ts2, Scope2, Cancel2}
) ->
    %% Relaxed comparison: ignore context and timestamp
    Seq1 == Seq2 andalso
    Type1 == Type2 andalso
    Opcode1 == Opcode2 andalso
    Scope1 == Scope2 andalso
    Cancel1 == Cancel2.

%% Helper: get diff for a single event
-spec event_diff(trace_event(), trace_event()) -> [mismatch()].
event_diff(
    {Seq1, Type1, Opcode1, _Ctx1, _Ts1, Scope1, Cancel1},
    {Seq2, Type2, Opcode2, _Ctx2, _Ts2, Scope2, Cancel2}
) ->
    lists:flatten([
        if Seq1 =/= Seq2 -> [#{seq => Seq1, field => seq, expected => Seq1, actual => Seq2}]; true -> [] end,
        if Type1 =/= Type2 -> [#{seq => Seq1, field => type, expected => Type1, actual => Type2}]; true -> [] end,
        if Opcode1 =/= Opcode2 -> [#{seq => Seq1, field => opcode, expected => Opcode1, actual => Opcode2}]; true -> [] end,
        if Scope1 =/= Scope2 -> [#{seq => Seq1, field => scope, expected => Scope1, actual => Scope2}]; true -> [] end,
        if Cancel1 =/= Cancel2 -> [#{seq => Seq1, field => cancel, expected => Cancel1, actual => Cancel2}]; true -> [] end
    ]).

%% Helper: count matching events
-spec count_matching_events(trace_log(), trace_log()) -> non_neg_integer().
count_matching_events(Log1, Log2) ->
    MinLen = min(length(Log1), length(Log2)),
    Pairs = lists:zip(lists:sublist(Log1, MinLen), lists:sublist(Log2, MinLen)),
    length([ok || {E1, E2} <- Pairs, events_match(E1, E2)]).

%%% SERIALIZATION ===========================================================

%% @doc Convert trace log to a list representation.
-spec to_list(Log :: trace_log()) -> [trace_event()].
to_list(Log) when is_list(Log) ->
    Log.

%% @doc Create trace log from a list representation.
-spec from_list(List :: [trace_event()]) -> trace_log().
from_list(List) when is_list(List) ->
    List.

%% @doc Serialize trace log to binary.
-spec to_binary(Log :: trace_log()) -> binary().
to_binary(Log) when is_list(Log) ->
    term_to_binary(Log, [compressed]).

%% @doc Deserialize trace log from binary.
-spec from_binary(Bin :: binary()) -> {ok, trace_log()} | {error, term()}.
from_binary(Bin) when is_binary(Bin) ->
    try
        Log = binary_to_term(Bin, [safe]),
        {ok, Log}
    catch
        error:Reason ->
            {error, {deserialization_failed, Reason}}
    end.

%%% STATISTICS AND ANALYSIS =================================================

%% @doc Compute statistics from a trace log.
-spec trace_stats(Log :: trace_log()) -> trace_stats().
trace_stats([]) ->
    #{
        total_events => 0,
        by_type => #{},
        by_opcode => #{},
        max_scope_depth => 0,
        execution_time_us => 0,
        cancelled_steps => 0
    };
trace_stats(Log) when is_list(Log) ->
    ByType = count_by_field(Log, 2),  % Field 2 is Type
    ByOpcode = count_by_field(Log, 3),  % Field 3 is Opcode
    MaxDepth = lists:max([length(Scope) || {_, _, _, _, _, Scope, _} <- Log]),
    {_, _, _, _, FirstTs, _, _} = hd(Log),
    {_, _, _, _, LastTs, _, _} = lists:last(Log),
    CancelledSteps = length([ok || {_, _, _, _, _, _, Cancel} <- Log, Cancel]),
    #{
        total_events => length(Log),
        by_type => ByType,
        by_opcode => ByOpcode,
        max_scope_depth => MaxDepth,
        execution_time_us => LastTs - FirstTs,
        cancelled_steps => CancelledSteps
    }.

%% Helper: count occurrences by field position
-spec count_by_field(trace_log(), pos_integer()) -> #{atom() => non_neg_integer()}.
count_by_field(Log, FieldPos) ->
    lists:foldl(
        fun(Event, Acc) ->
            Key = element(FieldPos, Event),
            maps:update_with(Key, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        Log
    ).

%% @doc Extract execution timeline (sequence of timestamps and events).
-spec execution_timeline(Log :: trace_log()) -> [{non_neg_integer(), event_type(), atom()}].
execution_timeline(Log) when is_list(Log) ->
    [{Ts, Type, Opcode} || {_, Type, Opcode, _, Ts, _, _} <- Log].

%% @doc Analyze scope depth over time.
-spec scope_depth_analysis(Log :: trace_log()) -> [{non_neg_integer(), non_neg_integer()}].
scope_depth_analysis(Log) when is_list(Log) ->
    [{Seq, length(Scope)} || {Seq, _, _, _, _, Scope, _} <- Log].

%%% TESTS ===================================================================

event_creation_test_() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    Event = event(0, task_enter, task_enter, Ctx, [], false),
    [
        ?_assertMatch({0, task_enter, task_enter, _, _, [], false}, Event)
    ].

trace_log_operations_test_() ->
    Log = new_log(),
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    Event1 = event(0, task_enter, task_enter, Ctx, [], false),
    Event2 = event(1, task_ok, task_call, Ctx, [], false),
    Log1 = append_event(Log, Event1),
    Log2 = append_event(Log1, Event2),
    [
        ?_assertEqual(0, log_size(Log)),
        ?_assertEqual(1, log_size(Log1)),
        ?_assertEqual(2, log_size(Log2)),
        ?_assertEqual([Event1, Event2], get_events(Log2))
    ].

filter_test_() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = event(0, task_enter, task_enter, Ctx, [], false),
    E2 = event(1, task_ok, task_call, Ctx, [], false),
    E3 = event(2, task_exit, task_exit, Ctx, [], false),
    Log = [E1, E2, E3],
    [
        ?_assertEqual([E1], filter_by_type(Log, task_enter)),
        ?_assertEqual([E2], filter_by_opcode(Log, task_call)),
        ?_assertEqual({ok, E3}, get_last_event(Log)),
        ?_assertEqual({ok, E2}, get_event_at(Log, 1))
    ].

trace_comparison_test_() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = event(1, task_ok, task_call, Ctx, 2000, [], false),
    Log1 = [E1, E2],
    Log2 = [E1, E2],
    E3 = event(1, task_error, task_call, Ctx, 2000, [], false),  % Different type
    Log3 = [E1, E3],
    [
        ?_assert(traces_match(Log1, Log2)),
        ?_assertNot(traces_match(Log1, Log3)),
        ?_assertEqual(2, maps:get(matching, compare_traces(Log1, Log2))),
        ?_assert(length(maps:get(mismatched, compare_traces(Log1, Log3))) > 0)
    ].

serialization_test_() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = event(0, task_enter, task_enter, Ctx, [], false),
    Log = [E1],
    Bin = to_binary(Log),
    [
        ?_assert(is_binary(Bin)),
        ?_assertMatch({ok, _}, from_binary(Bin)),
        ?_assertEqual(Log, to_list(Log))
    ].

trace_stats_test_() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = event(0, task_enter, task_enter, Ctx, 1000, [{seq, 1}], false),
    E2 = event(1, task_ok, task_call, Ctx, 2000, [{seq, 1}], false),
    E3 = event(2, task_exit, task_exit, Ctx, 3000, [{seq, 1}], true),
    Log = [E1, E2, E3],
    Stats = trace_stats(Log),
    [
        ?_assertEqual(3, maps:get(total_events, Stats)),
        ?_assertEqual(1, maps:get(cancelled_steps, Stats)),
        ?_assertEqual(1, maps:get(max_scope_depth, Stats)),
        ?_assertEqual(2000, maps:get(execution_time_us, Stats))
    ].

timeline_test_() ->
    Ctx = #{data => #{}, signals => [], results => #{}, token_data => #{}},
    E1 = event(0, task_enter, task_enter, Ctx, 1000, [], false),
    E2 = event(1, task_ok, task_call, Ctx, 2000, [], false),
    Log = [E1, E2],
    Timeline = execution_timeline(Log),
    [
        ?_assertEqual(2, length(Timeline)),
        ?_assertMatch([{1000, task_enter, task_enter}, {2000, task_ok, task_call}], Timeline)
    ].
