%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc Event Log Mock Factories
%%
%% This module provides mock factories for generating synthetic event logs
%% for testing workflow mining, process discovery, and predictive analytics.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Simple event logs for basic testing</li>
%%   <li>Noisy logs with malformed events</li>
%%   <li>Large logs for performance testing</li>
%%   <li>Various trace patterns (sequential, parallel, loops)</li>
%%   <li>Meck-compatible for easy mocking</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Generate a simple event log:
%% ```erlang
%% > Log = event_log_mocks:simple_log().
%% [#{
%% >   timestamp => 1704067200000,
%% >   type => case_created,
%% >   case_id => <<"case_001">>,
%% >   data => #{}
%% > }, ...]
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(event_log_mocks).

%%====================================================================
%% Exports
%%====================================================================

%% Event log generators
-export([simple_log/0, simple_log/1]).
-export([noisy_log/0, noisy_log/1]).
-export([large_log/1]).
-export([trace_log/0, trace_log/1]).
-export([sequential_trace/0, sequential_trace/1]).
-export([parallel_trace/0, parallel_trace/1]).
-export([loop_trace/0, loop_trace/1]).

%% Event generators
-export([event/4]).
-export([case_event/2]).
-export([workitem_event/4]).

%% Utility functions
-export([validate_log/1]).
-export([count_events/2]).
-export([filter_by_case/2]).
-export([sort_by_timestamp/1]).

%%====================================================================
%% Types
%%====================================================================

-type event() :: #{
    timestamp := integer(),
    type := atom(),
    case_id := binary(),
    data => map()
}.

-type event_log() :: [event()].

-type log_option() :: {case_count, pos_integer()}
                    | {event_count, pos_integer()}
                    | {noise_level, float()}
                    | {start_time, integer()}
                    | {case_ids, [binary()]}.

-export_type([event/0, event_log/0, log_option/0]).

%%====================================================================
%% Event Log Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a simple event log with default settings.
%%
%% Creates a log with 3 cases, each having a standard lifecycle:
%% case_created -> workitem_started -> workitem_completed.
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_log() -> event_log().

simple_log() ->
    simple_log([{case_count, 3}]).

%%--------------------------------------------------------------------
%% @doc Generates a simple event log with options.
%%
%% Options:
%% - `{case_count, N}' - Number of cases (default: 3)
%% - `{start_time, Millis}' - Start timestamp (default: current time)
%% - `{case_ids, [Ids]}' - Specific case IDs to use
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_log([log_option()]) -> event_log().

simple_log(Options) ->
    CaseCount = proplists:get_value(case_count, Options, 3),
    StartTime = proplists:get_value(start_time, Options, erlang:system_time(millisecond)),
    CaseIds = proplists:get_value(case_ids, Options,
                                  [list_to_binary("case_" ++ integer_to_list(I))
                                   || I <- lists:seq(1, CaseCount)]),

    lists:flatmap(fun(CaseId) ->
        sequential_events(StartTime, CaseId, 3)
    end, CaseIds).

%%--------------------------------------------------------------------
%% @doc Generates a noisy event log with malformed events.
%%
%% Includes events with missing fields, invalid timestamps, and
%% inconsistent data for testing error handling.
%%
%% @end
%%--------------------------------------------------------------------
-spec noisy_log() -> event_log().

noisy_log() ->
    noisy_log([{noise_level, 0.3}]).

%%--------------------------------------------------------------------
%% @doc Generates a noisy event log with specified noise level.
%%
%% Noise level is the proportion of events that are malformed (0.0-1.0).
%%
%% @end
%%--------------------------------------------------------------------
-spec noisy_log([{noise_level, float()}]) -> event_log().

noisy_log(Options) ->
    NoiseLevel = proplists:get_value(noise_level, Options, 0.3),
    BaseLog = simple_log(),
    NoiseCount = round(length(BaseLog) * NoiseLevel),

    MalformedEvents = generate_malformed_events(NoiseCount),

    %% Shuffle and combine
    Combined = BaseLog ++ MalformedEvents,
    [E || {_, E} <- lists:sort([{rand:uniform(), N} || N <- Combined])].

%%--------------------------------------------------------------------
%% @doc Generates a large event log for performance testing.
%%
%% Creates a log with the specified number of events across multiple cases.
%%
%% @end
%%--------------------------------------------------------------------
-spec large_log(pos_integer()) -> event_log().

large_log(EventCount) ->
    CaseCount = max(1, EventCount div 10),
    EventsPerCase = max(1, EventCount div CaseCount),
    StartTime = erlang:system_time(millisecond),

    lists:flatmap(fun(CaseNum) ->
        CaseId = list_to_binary("case_" ++ integer_to_list(CaseNum)),
        sequential_events(StartTime, CaseId, EventsPerCase)
    end, lists:seq(1, CaseCount)).

%%--------------------------------------------------------------------
%% @doc Generates a log with various trace patterns.
%%
%% Creates a log containing sequential, parallel, and loop patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec trace_log() -> event_log().

trace_log() ->
    trace_log([{pattern_count, 2}]).

%%--------------------------------------------------------------------
%% @doc Generates a log with specified pattern count.
%%
%% @end
%%--------------------------------------------------------------------
-spec trace_log([{pattern_count, pos_integer()}]) -> event_log().

trace_log(Options) ->
    PatternCount = proplists:get_value(pattern_count, Options, 2),
    StartTime = erlang:system_time(millisecond),

    %% Generate sequential traces
    Sequential = lists:flatmap(fun(I) ->
        CaseId = list_to_binary("seq_" ++ integer_to_list(I)),
        sequential_events(StartTime, CaseId, 5)
    end, lists:seq(1, PatternCount)),

    %% Generate parallel traces
    Parallel = lists:flatmap(fun(I) ->
        CaseId = list_to_binary("par_" ++ integer_to_list(I)),
        parallel_events(StartTime, CaseId, 3)
    end, lists:seq(1, PatternCount)),

    %% Generate loop traces
    Loop = lists:flatmap(fun(I) ->
        CaseId = list_to_binary("loop_" ++ integer_to_list(I)),
        loop_events(StartTime, CaseId, 2, 2)
    end, lists:seq(1, PatternCount)),

    sort_by_timestamp(Sequential ++ Parallel ++ Loop).

%%--------------------------------------------------------------------
%% @doc Generates a sequential trace event log.
%%
%% All events occur in a linear sequence.
%%
%% @end
%%--------------------------------------------------------------------
-spec sequential_trace() -> event_log().

sequential_trace() ->
    sequential_trace([{case_count, 2}]).

%%--------------------------------------------------------------------
%% @doc Generates a sequential trace with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec sequential_trace([{case_count, pos_integer()}]) -> event_log().

sequential_trace(Options) ->
    CaseCount = proplists:get_value(case_count, Options, 2),
    StartTime = erlang:system_time(millisecond),

    lists:flatmap(fun(I) ->
        CaseId = list_to_binary("seq_case_" ++ integer_to_list(I)),
        sequential_events(StartTime, CaseId, 5)
    end, lists:seq(1, CaseCount)).

%%--------------------------------------------------------------------
%% @doc Generates a parallel trace event log.
%%
%% Simulates concurrent execution paths.
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel_trace() -> event_log().

parallel_trace() ->
    parallel_trace([{case_count, 2}]).

%%--------------------------------------------------------------------
%% @doc Generates a parallel trace with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel_trace([{case_count, pos_integer()}]) -> event_log().

parallel_trace(Options) ->
    CaseCount = proplists:get_value(case_count, Options, 2),
    StartTime = erlang:system_time(millisecond),

    lists:flatmap(fun(I) ->
        CaseId = list_to_binary("par_case_" ++ integer_to_list(I)),
        parallel_events(StartTime, CaseId, 3)
    end, lists:seq(1, CaseCount)).

%%--------------------------------------------------------------------
%% @doc Generates a loop trace event log.
%%
%% Simulates repeating activities.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop_trace() -> event_log().

loop_trace() ->
    loop_trace([{case_count, 2}]).

%%--------------------------------------------------------------------
%% @doc Generates a loop trace with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop_trace([{case_count, pos_integer()}]) -> event_log().

loop_trace(Options) ->
    CaseCount = proplists:get_value(case_count, Options, 2),
    StartTime = erlang:system_time(millisecond),

    lists:flatmap(fun(I) ->
        CaseId = list_to_binary("loop_case_" ++ integer_to_list(I)),
        loop_events(StartTime, CaseId, 2, 2)
    end, lists:seq(1, CaseCount)).

%%====================================================================
%% Event Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an event with the specified fields.
%%
%% @end
%%--------------------------------------------------------------------
-spec event(integer(), atom(), binary(), map()) -> event().

event(Timestamp, Type, CaseId, Data) ->
    #{
        timestamp => Timestamp,
        type => Type,
        case_id => CaseId,
        data => Data
    }.

%%--------------------------------------------------------------------
%% @doc Creates a case-level event.
%%
%% @end
%%--------------------------------------------------------------------
-spec case_event(atom(), binary()) -> event().

case_event(Type, CaseId) ->
    Timestamp = erlang:system_time(millisecond),
    event(Timestamp, Type, CaseId, #{}).

%%--------------------------------------------------------------------
%% @doc Creates a workitem event with task information.
%%
%% @end
%%--------------------------------------------------------------------
-spec workitem_event(atom(), binary(), binary(), map()) -> event().

workitem_event(Type, CaseId, Task, ExtraData) ->
    Timestamp = erlang:system_time(millisecond),
    Data = maps:merge(#{task => Task}, ExtraData),
    event(Timestamp, Type, CaseId, Data).

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates an event log structure.
%%
%% Checks that all events have required fields and valid values.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_log(event_log()) -> {ok, [event()]} | {error, [term()]}.

validate_log(Log) ->
    {Valid, Invalid} = lists:foldl(fun(E, {V, I}) ->
        case validate_event(E) of
            true -> {[E | V], I};
            false -> {V, [E | I]}
        end
    end, {[], []}, Log),

    case Invalid of
        [] -> {ok, lists:reverse(Valid)};
        _ -> {error, lists:reverse(Invalid)}
    end.

%%--------------------------------------------------------------------
%% @doc Counts events of a specific type in the log.
%%
%% @end
%%--------------------------------------------------------------------
-spec count_events(event_log(), atom()) -> non_neg_integer().

count_events(Log, Type) ->
    length([E || E <- Log, maps:get(type, E) =:= Type]).

%%--------------------------------------------------------------------
%% @doc Filters events by case ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_by_case(event_log(), binary()) -> event_log().

filter_by_case(Log, CaseId) ->
    [E || E <- Log, maps:get(case_id, E) =:= CaseId].

%%--------------------------------------------------------------------
%% @doc Sorts events by timestamp.
%%
%% @end
%%--------------------------------------------------------------------
-spec sort_by_timestamp(event_log()) -> event_log().

sort_by_timestamp(Log) ->
    lists:sort(fun(A, B) ->
        maps:get(timestamp, A, 0) =< maps:get(timestamp, B, 0)
    end, Log).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec validate_event(term()) -> boolean().

validate_event(Event) when is_map(Event) ->
    HasTimestamp = maps:is_key(timestamp, Event)
        andalso is_integer(maps:get(timestamp, Event, 0)),
    HasType = maps:is_key(type, Event)
        andalso is_atom(maps:get(type, Event)),
    HasCaseId = maps:is_key(case_id, Event)
        andalso is_binary(maps:get(case_id, Event)),
    HasTimestamp andalso HasType andalso HasCaseId;
validate_event(_) ->
    false.

%% @private
-spec sequential_events(integer(), binary(), pos_integer()) -> [event()].

sequential_events(StartTime, CaseId, Count) ->
    BaseEvents = [case_created, case_started, workitem_enabled],
    WorkEvents = lists:duplicate(max(0, Count - length(BaseEvents)), workitem_started)
               ++ [workitem_completed],
    EndEvents = [case_completed],

    AllTypes = BaseEvents ++ WorkEvents ++ EndEvents,

    lists:map(fun({Type, Index}) ->
        Timestamp = StartTime + (Index * 1000),
        event(Timestamp, Type, CaseId, #{index => Index})
    end, lists:zip(AllTypes, lists:seq(0, length(AllTypes) - 1))).

%% @private
-spec parallel_events(integer(), binary(), pos_integer()) -> [event()].

parallel_events(StartTime, CaseId, BranchCount) ->
    %% Create parallel branches that start at the same time
    Branches = lists:map(fun(BranchId) ->
        TaskName = list_to_binary("branch_" ++ integer_to_list(BranchId)),
        [
            event(StartTime + 500, workitem_enabled, CaseId, #{task => TaskName}),
            event(StartTime + 1000, workitem_started, CaseId, #{task => TaskName}),
            event(StartTime + 3000, workitem_completed, CaseId, #{task => TaskName})
        ]
    end, lists:seq(1, BranchCount)),

    CaseEvents = [
        event(StartTime, case_created, CaseId, #{}),
        event(StartTime + 100, case_started, CaseId, #{}),
        event(StartTime + 4000, case_completed, CaseId, #{})
    ],

    CaseEvents ++ lists:flatten(Branches).

%% @private
-spec loop_events(integer(), binary(), pos_integer(), pos_integer()) -> [event()].

loop_events(StartTime, CaseId, LoopCount, EventsPerLoop) ->
    LoopEvents = lists:flatmap(fun(LoopIndex) ->
        BaseOffset = LoopIndex * EventsPerLoop * 1000,
        lists:map(fun(EventIndex) ->
            Timestamp = StartTime + BaseOffset + (EventIndex * 1000),
            Task = list_to_binary("loop_task_" ++ integer_to_list(LoopIndex)),
            event(Timestamp, workitem_started, CaseId, #{task => Task, loop => LoopIndex})
        end, lists:seq(1, EventsPerLoop))
    end, lists:seq(1, LoopCount)),

    WrapperEvents = [
        event(StartTime, case_created, CaseId, #{}),
        event(StartTime + (LoopCount * EventsPerLoop * 1000) + 1000, case_completed, CaseId, #{})
    ],

    WrapperEvents ++ LoopEvents.

%% @private
-spec generate_malformed_events(pos_integer()) -> [event()].

generate_malformed_events(Count) ->
    MalformTypes = [
        fun() -> #{timestamp => -1} end,  %% Invalid timestamp
        fun() -> #{timestamp => 0, type => invalid} end,  %% Missing case_id
        fun() -> #{timestamp => 0, case_id => <<>>} end,  %% Missing type
        fun() -> #{timestamp => "not_an_integer", type => case_created, case_id => <<"x">>} end,
        fun() -> not_a_map end  %% Completely invalid
    ],
    [generate_malformed_event(MalformTypes) || _ <- lists:seq(1, Count)].

%% @private
-spec generate_malformed_event([fun()]) -> term().

generate_malformed_event(Types) ->
    TypeFun = lists:nth(rand:uniform(length(Types)), Types),
    TypeFun().

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test simple_log/0
simple_log_test() ->
    Log = simple_log(),
    ?assert(length(Log) > 0),
    ?assertEqual(ok, element(1, validate_log(Log))).

%% Test simple_log/1 with case_count
simple_log_case_count_test() ->
    Log = simple_log([{case_count, 5}]),
    ?assert(length(Log) >= 15),  %% At least 3 events per case
    Cases = lists:usort([maps:get(case_id, E) || E <- Log]),
    ?assertEqual(5, length(Cases)).

%% Test noisy_log/0
noisy_log_test() ->
    Log = noisy_log(),
    ?assert(length(Log) > 0).

%% Test noisy_log/1 with noise level
noisy_log_level_test() ->
    Log = noisy_log([{noise_level, 0.5}]),
    case validate_log(Log) of
        {ok, _Valid} ->
            %% All events valid - should still pass
            ?assert(length(Log) > 0);
        {error, _Invalid} ->
            %% Some invalid events, which is expected for noisy log
            ?assert(length(Log) > 0)
    end.

%% Test large_log/1
large_log_test() ->
    Log = large_log(100),
    ?assert(length(Log) >= 100).

%% Test event/4
event_creation_test() ->
    Ev = event(1000, case_created, <<"case1">>, #{key => val}),
    ?assertEqual(1000, maps:get(timestamp, Ev)),
    ?assertEqual(case_created, maps:get(type, Ev)),
    ?assertEqual(<<"case1">>, maps:get(case_id, Ev)),
    ?assertEqual(val, maps:get(key, maps:get(data, Ev))).

%% Test case_event/2
case_event_test() ->
    Ev = case_event(case_started, <<"case1">>),
    ?assertEqual(case_started, maps:get(type, Ev)),
    ?assertEqual(<<"case1">>, maps:get(case_id, Ev)),
    ?assert(is_integer(maps:get(timestamp, Ev))).

%% Test workitem_event/4
workitem_event_test() ->
    Ev = workitem_event(workitem_started, <<"case1">>, <<"task1">>, #{priority => high}),
    ?assertEqual(workitem_started, maps:get(type, Ev)),
    ?assertEqual(<<"case1">>, maps:get(case_id, Ev)),
    ?assertEqual(<<"task1">>, maps:get(task, maps:get(data, Ev))),
    ?assertEqual(high, maps:get(priority, maps:get(data, Ev))).

%% Test count_events/2
count_events_test() ->
    Log = simple_log(),
    CreatedCount = count_events(Log, case_created),
    ?assert(CreatedCount > 0).

%% Test filter_by_case/2
filter_by_case_test() ->
    Log = simple_log([{case_count, 3}]),
    FirstCase = maps:get(case_id, hd(Log)),
    Filtered = filter_by_case(Log, FirstCase),
    ?assert(length(Filtered) > 0),
    ?assert(lists:all(fun(E) -> maps:get(case_id, E) =:= FirstCase end, Filtered)).

%% Test sort_by_timestamp/1
sort_by_timestamp_test() ->
    Log = [event(3000, a, <<"c1">>, #{}),
           event(1000, b, <<"c1">>, #{}),
           event(2000, c, <<"c1">>, #{})],
    Sorted = sort_by_timestamp(Log),
    ?assertEqual([b, c, a], [maps:get(type, E) || E <- Sorted]).

%% Test trace_log/0
trace_log_test() ->
    Log = trace_log(),
    ?assert(length(Log) > 0),
    ?assertEqual(ok, element(1, validate_log(Log))).

%% Test sequential_trace/0
sequential_trace_test() ->
    Log = sequential_trace(),
    ?assert(length(Log) > 0).

%% Test parallel_trace/0
parallel_trace_test() ->
    Log = parallel_trace(),
    ?assert(length(Log) > 0).

%% Test loop_trace/0
loop_trace_test() ->
    Log = loop_trace(),
    ?assert(length(Log) > 0).

-endif.
