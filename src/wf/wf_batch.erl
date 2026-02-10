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

-module(wf_batch).

-moduledoc """
Batch workflow operations for bulk start/stop of cases.

Provides efficient batch operations for workflow case and work item
management, enabling bulk start, stop, allocate, and complete operations
with comprehensive error tracking.

== Doctests ==

Starting multiple cases in a batch:
```erlang
> Engine = spawn(fun() -> ok end),
> Cases = [#{data => #{amount => 10}}, #{data => #{amount => 20}}],
> {ok, CaseIds, Errors} = wf_batch:start_cases(Engine, Cases, 0),
> is_list(CaseIds) andalso is_list(Errors).
true
```

Cancelling multiple cases in a batch:
```erlang
> {ok, Results, Errors} = wf_batch:cancel_cases(Engine, CaseIds, 1),
> length(Results) =:= length(CaseIds).
true
```

Batch allocating work items:
```erlang
> WiIds = [<<"wi_1">>, <<"wi_2">>],
> {ok, Success, Failed} = wf_batch:allocate_workitems(
..   Engine, WiIds, alice, 0
.. ),
> is_list(Success) andalso is_list(Failed).
true
```

Batch completing work items:
```erlang
> Data = [#{approved => true}, #{approved => false}],
> {ok, Completed, Errors} = wf_batch:complete_workitems(
..   Engine, WiIds, alice, Data, 0
.. ),
> is_list(Completed) andalso is_list(Errors).
true
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Batch case operations
-export([start_cases/3, cancel_cases/3, suspend_cases/3, resume_cases/3]).

%% Batch work item operations
-export([allocate_workitems/4, start_workitems/4, complete_workitems/5]).

%% Utility functions
-export([batch_result/2, filter_successes/1, filter_errors/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Result of a batch operation.
%%
%% Contains successful operations and errors.
%%--------------------------------------------------------------------
-type batch_result() :: {ok, [any()], [{any(), term()}]}.

%%--------------------------------------------------------------------
%% @doc Case configuration for batch operations.
%%--------------------------------------------------------------------
-type case_config() :: #{data => map()}.

%%--------------------------------------------------------------------
%% @doc Work item operation result.
%%--------------------------------------------------------------------
-type wi_result() :: ok | {error, term()}.

%%--------------------------------------------------------------------
%% @doc Tagged result for filtering.
%%--------------------------------------------------------------------
-type tagged_result() :: {ok, any()} | {error, any(), term()}.

%% Export types
-export_type([batch_result/0, case_config/0, wi_result/0, tagged_result/0]).

%%====================================================================
%% Batch Case Operations
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts multiple workflow cases in batch.
%%
%% Processes a list of case configurations and returns successful
%% case IDs and errors. Each configuration should contain at least
%% the 'data' key.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Ids, Errors} = wf_batch:start_cases(
%%     Engine,
%%     [#{data => #{x => 1}}, #{data => #{y => 2}}],
%%     0
%% ).
%% {ok, [<<"case_...">>, <<"case_...">>], []}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param Cases List of case configurations
%% @param Now Current timestamp
%% @return {ok, CaseIds, Errors} where Errors = [{Config, Reason}]
%%
%% @end
%%--------------------------------------------------------------------
-spec start_cases(Engine :: pid() | atom(), Cases :: [case_config()],
                  Now :: integer()) ->
          batch_result().

start_cases(Engine, Cases, Now) when is_list(Cases) ->
    Results = lists:map(
        fun(CaseConfig) ->
            case wf_engine:start_case(Engine, CaseConfig, Now) of
                {ok, CaseId} -> {ok, CaseId};
                {error, Reason} -> {error, CaseConfig, Reason}
            end
        end,
        Cases
    ),
    {Successes, Failures} = partition_results(Results),
    {ok, Successes, Failures}.

%%--------------------------------------------------------------------
%% @doc Cancels multiple workflow cases in batch.
%%
%% Processes a list of case IDs and sends cancel requests. Returns
%% successful cancellations and errors for cases that failed.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Results, Errors} = wf_batch:cancel_cases(
%%     Engine,
%%     [CaseId1, CaseId2],
%%     1
%% ).
%% {ok, [ok, ok], []}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param CaseIds List of case IDs to cancel
%% @param Now Current timestamp
%% @return {ok, Results, Errors} where Results = [ok] and Errors = [{CaseId, Reason}]
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_cases(Engine :: pid() | atom(), CaseIds :: [binary()],
                   Now :: integer()) ->
          batch_result().

cancel_cases(Engine, CaseIds, Now) when is_list(CaseIds) ->
    Results = lists:map(
        fun(CaseId) ->
            case wf_engine:cancel_case(Engine, CaseId, Now) of
                ok -> {ok, ok};
                {error, Reason} -> {error, CaseId, Reason}
            end
        end,
        CaseIds
    ),
    {Successes, Failures} = partition_results(Results),
    {ok, Successes, Failures}.

%%--------------------------------------------------------------------
%% @doc Suspends multiple workflow cases in batch.
%%
%% Suspends a batch of running cases. Returns successful suspensions
%% and errors for cases that could not be suspended.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Results, Errors} = wf_batch:suspend_cases(
%%     Engine,
%%     [CaseId1, CaseId2],
%%     1
%% ).
%% {ok, [ok, ok], []}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param CaseIds List of case IDs to suspend
%% @param Now Current timestamp
%% @return {ok, Results, Errors}
%%
%% @end
%%--------------------------------------------------------------------
-spec suspend_cases(Engine :: pid() | atom(), CaseIds :: [binary()],
                    Now :: integer()) ->
          batch_result().

suspend_cases(Engine, CaseIds, Now) when is_list(CaseIds) ->
    Results = lists:map(
        fun(CaseId) ->
            case wf_engine:suspend_case(Engine, CaseId, Now) of
                ok -> {ok, ok};
                {error, Reason} -> {error, CaseId, Reason}
            end
        end,
        CaseIds
    ),
    {Successes, Failures} = partition_results(Results),
    {ok, Successes, Failures}.

%%--------------------------------------------------------------------
%% @doc Resumes multiple workflow cases in batch.
%%
%% Resumes a batch of suspended cases. Returns successful resumptions
%% and errors for cases that could not be resumed.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Results, Errors} = wf_batch:resume_cases(
%%     Engine,
%%     [CaseId1, CaseId2],
%%     2
%% ).
%% {ok, [ok, ok], []}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param CaseIds List of case IDs to resume
%% @param Now Current timestamp
%% @return {ok, Results, Errors}
%%
%% @end
%%--------------------------------------------------------------------
-spec resume_cases(Engine :: pid() | atom(), CaseIds :: [binary()],
                   Now :: integer()) ->
          batch_result().

resume_cases(Engine, CaseIds, Now) when is_list(CaseIds) ->
    Results = lists:map(
        fun(CaseId) ->
            case wf_engine:resume_case(Engine, CaseId, Now) of
                ok -> {ok, ok};
                {error, Reason} -> {error, CaseId, Reason}
            end
        end,
        CaseIds
    ),
    {Successes, Failures} = partition_results(Results),
    {ok, Successes, Failures}.

%%====================================================================
%% Batch Work Item Operations
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Allocates multiple work items to a user in batch.
%%
%% Processes a list of work item IDs and allocates them to the
%% specified user. Returns successfully allocated items and errors.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Success, Failed} = wf_batch:allocate_workitems(
%%     Engine,
%%     [WiId1, WiId2, WiId3],
%%     alice,
%%     0
%% ).
%% {ok, [WiId1, WiId2], [{WiId3, invalid_status}]}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param WiIds List of work item IDs to allocate
%% @param User User or resource to allocate to
%% @param Now Current timestamp
%% @return {ok, Allocated, Failed} where Failed = [{WiId, Reason}]
%%
%% @end
%%--------------------------------------------------------------------
-spec allocate_workitems(Engine :: pid() | atom(), WiIds :: [binary()],
                        User :: atom() | binary(), Now :: integer()) ->
          batch_result().

allocate_workitems(Engine, WiIds, User, Now) when is_list(WiIds) ->
    Results = lists:map(
        fun(WiId) ->
            case wf_engine:allocate(Engine, WiId, User, Now) of
                ok -> {ok, WiId};
                {error, Reason} -> {error, WiId, Reason}
            end
        end,
        WiIds
    ),
    {Successes, Failures} = partition_results(Results),
    {ok, Successes, Failures}.

%%--------------------------------------------------------------------
%% @doc Starts work on multiple work items in batch.
%%
%% Transitions a batch of allocated work items to started status.
%% Returns successfully started items and errors.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Started, Failed} = wf_batch:start_workitems(
%%     Engine,
%%     [WiId1, WiId2],
%%     alice,
%%     0
%% ).
%% {ok, [WiId1, WiId2], []}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param WiIds List of work item IDs to start
%% @param User User resource associated with work items
%% @param Now Current timestamp
%% @return {ok, Started, Failed} where Failed = [{WiId, Reason}]
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workitems(Engine :: pid() | atom(), WiIds :: [binary()],
                     User :: atom() | binary(), Now :: integer()) ->
          batch_result().

start_workitems(Engine, WiIds, User, Now) when is_list(WiIds) ->
    Results = lists:map(
        fun(WiId) ->
            case wf_engine:start_work(Engine, WiId, User, Now) of
                ok -> {ok, WiId};
                {error, Reason} -> {error, WiId, Reason}
            end
        end,
        WiIds
    ),
    {Successes, Failures} = partition_results(Results),
    {ok, Successes, Failures}.

%%--------------------------------------------------------------------
%% @doc Completes multiple work items in batch.
%%
%% Completes a batch of started work items with their result data.
%% Data list must correspond 1:1 with WiIds list. Returns successfully
%% completed items and errors.
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Completed, Errors} = wf_batch:complete_workitems(
%%     Engine,
%%     [WiId1, WiId2],
%%     alice,
%%     [#{approved => true}, #{approved => false}],
%%     1
%% ).
%% {ok, [WiId1, WiId2], []}
%% ```
%%
%% The data list can contain arbitrary maps with task output:
%%
%% ```erlang
%% > Data = [
%%     #{decision => approve, amount => 1000},
%%     #{decision => reject, reason => insufficient_funds}
%% ],
%% > wf_batch:complete_workitems(Engine, [Wi1, Wi2], bob, Data, 2).
%% {ok, [Wi1, Wi2], []}
%% ```
%%
%% @param Engine Engine pid or registered name
%% @param WiIds List of work item IDs to complete
%% @param User User completing the work items
%% @param Data List of result data (must match WiIds length)
%% @param Now Current timestamp
%% @return {ok, Completed, Failed} where Failed = [{WiId, Reason}]
%% @throws badarg if Data length doesn't match WiIds length
%%
%% @end
%%--------------------------------------------------------------------
-spec complete_workitems(Engine :: pid() | atom(), WiIds :: [binary()],
                        User :: atom() | binary(), Data :: [map()],
                        Now :: integer()) ->
          batch_result().

complete_workitems(Engine, WiIds, User, Data, Now)
        when is_list(WiIds), is_list(Data) ->
    case length(WiIds) =:= length(Data) of
        false ->
            error(badarg, [Engine, WiIds, User, Data, Now]);
        true ->
            Results = lists:map(
                fun({WiId, ResultData}) ->
                    case wf_engine:complete(Engine, WiId, User, ResultData, Now) of
                        ok -> {ok, WiId};
                        {error, Reason} -> {error, WiId, Reason}
                    end
                end,
                lists:zip(WiIds, Data)
            ),
            {Successes, Failures} = partition_results(Results),
            {ok, Successes, Failures}
    end.

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a batch result tuple from successes and failures.
%%
%% Convenience function for constructing consistent batch results.
%%
%% == Examples ==
%%
%% ```erlang
%% > wf_batch:batch_result([a, b], [{c, error}]).
%% {ok, [a, b], [{c, error}]}
%% ```
%%
%% @param Successes List of successful operation results
%% @param Failures List of {Item, Reason} failure tuples
%% @return Normalized batch result
%%
%% @end
%%--------------------------------------------------------------------
-spec batch_result(Successes :: [any()], Failures :: [{any(), term()}]) ->
          batch_result().

batch_result(Successes, Failures) when is_list(Successes), is_list(Failures) ->
    {ok, Successes, Failures}.

%%--------------------------------------------------------------------
%% @doc Filters a batch result to extract only successful items.
%%
%% Extracts the list of successful operation results from a
%% batch result tuple.
%%
%% == Examples ==
%%
%% ```erlang
%% > Result = {ok, [id1, id2], [{id3, error}]},
%% > wf_batch:filter_successes(Result).
%% [id1, id2]
%% ```
%%
%% @param BatchResult Result tuple from batch operation
%% @return List of successful items
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_successes(BatchResult :: batch_result()) -> [any()].

filter_successes({ok, Successes, _Failures}) ->
    Successes.

%%--------------------------------------------------------------------
%% @doc Filters a batch result to extract only failed items.
%%
%% Extracts the list of failed operation results from a
%% batch result tuple.
%%
%% == Examples ==
%%
%% ```erlang
%% > Result = {ok, [id1, id2], [{id3, error}]},
%% > wf_batch:filter_errors(Result).
%% [{id3, error}]
%% ```
%%
%% @param BatchResult Result tuple from batch operation
%% @return List of {Item, Reason} failure tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_errors(BatchResult :: batch_result()) -> [{any(), term()}].

filter_errors({ok, _Successes, Failures}) ->
    Failures.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Partitions results into successes and failures.
%%
%% Separates tagged results into two lists for successes {ok, Value}
%% and failures {error, Item, Reason}.
%%
%% @end
%%--------------------------------------------------------------------
-spec partition_results([tagged_result()]) ->
          {[any()], [{any(), term()}]}.

partition_results(Results) ->
    lists:foldl(
        fun({ok, Value}, {Successes, Failures}) ->
                {[Value | Successes], Failures};
           ({error, Item, Reason}, {Successes, Failures}) ->
                {Successes, [{Item, Reason} | Failures]}
        end,
        {[], []},
        Results
    ).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit test runner for the module.
%% Tests the doctest examples from the moduledoc.
%%--------------------------------------------------------------------
doctest_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc Test batch_result/2 creates correct tuple.
%%--------------------------------------------------------------------
batch_result_test() ->
    Result = batch_result([a, b], [{c, error}]),
    ?assertEqual({ok, [a, b], [{c, error}]}, Result).

%%--------------------------------------------------------------------
%% @doc Test filter_successes/1 extracts successes.
%%--------------------------------------------------------------------
filter_successes_test() ->
    Result = {ok, [id1, id2], [{id3, error}]},
    ?assertEqual([id1, id2], filter_successes(Result)).

%%--------------------------------------------------------------------
%% @doc Test filter_errors/1 extracts failures.
%%--------------------------------------------------------------------
filter_errors_test() ->
    Result = {ok, [id1, id2], [{id3, error}]},
    ?assertEqual([{id3, error}], filter_errors(Result)).

%%--------------------------------------------------------------------
%% @doc Test partition_results/1 with mixed results.
%%--------------------------------------------------------------------
partition_results_test() ->
    Results = [
        {ok, a},
        {error, b, reason1},
        {ok, c},
        {error, d, reason2}
    ],
    {Successes, Failures} = partition_results(Results),
    ?assert(lists:member(a, Successes)),
    ?assert(lists:member(c, Successes)),
    ?assert(lists:member({b, reason1}, Failures)),
    ?assert(lists:member({d, reason2}, Failures)).

%%--------------------------------------------------------------------
%% @doc Test partition_results with empty list.
%%--------------------------------------------------------------------
partition_results_empty_test() ->
    {Successes, Failures} = partition_results([]),
    ?assertEqual([], Successes),
    ?assertEqual([], Failures).

%%--------------------------------------------------------------------
%% @doc Test partition_results with only successes.
%%--------------------------------------------------------------------
partition_results_successes_only_test() ->
    Results = [{ok, a}, {ok, b}, {ok, c}],
    {Successes, Failures} = partition_results(Results),
    ?assertEqual(3, length(Successes)),
    ?assertEqual([], Failures).

%%--------------------------------------------------------------------
%% @doc Test partition_results with only failures.
%%--------------------------------------------------------------------
partition_results_failures_only_test() ->
    Results = [{error, a, err1}, {error, b, err2}],
    {Successes, Failures} = partition_results(Results),
    ?assertEqual([], Successes),
    ?assertEqual(2, length(Failures)).

%%--------------------------------------------------------------------
%% @doc Test complete_workitems with mismatched data length.
%%--------------------------------------------------------------------
complete_workitems_badarg_test() ->
    Engine = spawn(fun() -> ok end),
    WiIds = [<<"wi_1">>, <<"wi_2">>],
    Data = [#{approved => true}],  % Only 1 item vs 2 WiIds
    ?assertError(badarg, complete_workitems(Engine, WiIds, alice, Data, 0)).

-endif.
