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
%% @doc CRE YAWL Client Test Suite
%%
%% Comprehensive test suite for the cre_yawl_client module.
%% Tests cover client lifecycle, workflow execution, pattern handling,
%% state management, and error conditions.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_yawl_client_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    test_init_/0,
    test_step_/0,
    test_recv_/0,
    test_load_/0,
    test_unload_/0,
    test_compose_patterns_/0,
    test_is_pattern_record_/0,
    test_get_pattern_type_/0,
    test_evaluate_condition_/0,
    test_select_branch_/0,
    test_select_branches_/0,
    test_code_change_/0,
    test_terminate_/0,
    setup/0,
    cleanup/1,
    simple_workflow/0,
    simple_pattern/0
]).

-compile(export_all).

%% Record definition from cre_yawl_client module
-record(yawl_client_state, {
    workflow :: undefined | term(),
    pattern :: undefined | term(),
    execution_state :: running | completed | failed | terminated,
    active_tasks = [] :: list(),
    completed_tasks = [] :: list(),
    pending_tasks = [] :: list(),
    results = #{} :: map(),
    errors = [] :: list(),
    start_time :: undefined | erlang:timestamp(),
    end_time :: undefined | erlang:timestamp(),
    metadata = #{} :: map()
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Initialize persistent terms for configuration
    cre_config:init(),

    %% Start CRE master if not running
    case whereis(cre_master) of
        undefined ->
            {ok, _Pid} = cre_master:start_link();
        _ ->
            ok
    end,
    timer:sleep(10).

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_Arg) ->
    %% Flush messages
    receive
        _ -> ok
    after 0 -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Creates a simple workflow for testing.
%% @end
%%--------------------------------------------------------------------
simple_workflow() ->
    cre_yawl:new_workflow(<<"test_workflow">>).

%%--------------------------------------------------------------------
%% @doc Creates a simple pattern for testing.
%% @end
%%--------------------------------------------------------------------
simple_pattern() ->
    {sequence, [<<"task1">>, <<"task2">>]}.

%%====================================================================
%% cre_client Callback Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test init with valid workflow.
%% @end
%%--------------------------------------------------------------------
test_init_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Workflow = simple_workflow(),
                    State = cre_yawl_client:init(Workflow),
                    ?assert(is_map(State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test init with valid pattern.
%% @end
%%--------------------------------------------------------------------
test_step_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Pattern = simple_pattern(),
                    State = cre_yawl_client:init(Pattern),
                    {ok, NewState, Tasks} = cre_yawl_client:step(undefined, State),
                    ?assert(is_map(NewState)),
                    ?assert(is_list(Tasks))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test recv processes results.
%% @end
%%--------------------------------------------------------------------
test_recv_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Create a simple state
                    State = cre_yawl_client:init(simple_pattern()),
                    ReplyList = [{<<"task1">>, #{result => ok}}],
                    NewState = cre_yawl_client:recv(undefined, ReplyList, State),
                    ?assert(is_map(NewState))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test load with workflow.
%% @end
%%--------------------------------------------------------------------
test_load_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Workflow = simple_workflow(),
                    InitialState = cre_yawl_client:init(Workflow),
                    State = cre_yawl_client:load(Workflow, InitialState),
                    ?assert(is_map(State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unload callback.
%% @end
%%--------------------------------------------------------------------
test_unload_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = cre_yawl_client:init(simple_workflow()),
                    Result = cre_yawl_client:unload(simple_workflow(), State),
                    case Result of
                        ok -> ok;
                        {ok, _} -> ok;
                        _ -> ?assert(false, "Unexpected result")
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test compose_patterns.
%% @end
%%--------------------------------------------------------------------
test_compose_patterns_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Patterns = [{sequence, []}, {parallel_split, [], []}],
                    Options = #{mode => sequence},
                    Result = cre_yawl_client:compose_patterns(Patterns, Options),
                    ?assertMatch({sequence, _}, Result)
                end)]
     end}.

%%====================================================================
%% Internal Function Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test is_pattern_record with valid patterns.
%% @end
%%--------------------------------------------------------------------
test_is_pattern_record_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Test valid pattern records
                    ?assert(cre_yawl_client:is_pattern_record(sequence)),
                    ?assert(cre_yawl_client:is_pattern_record(parallel_split)),
                    ?assert(cre_yawl_client:is_pattern_record(synchronization)),
                    ?assert(cre_yawl_client:is_pattern_record(exclusive_choice)),
                    ?assert(cre_yawl_client:is_pattern_record(simple_merge)),

                    %% Test invalid pattern
                    ?assertNot(cre_yawl_client:is_pattern_record(invalid_pattern))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test get_pattern_type.
%% @end
%%--------------------------------------------------------------------
test_get_pattern_type_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    ?assertEqual(sequence, cre_yawl_client:get_pattern_type({sequence, []})),
                    ?assertEqual(parallel_split, cre_yawl_client:get_pattern_type({parallel_split, [], []})),
                    ?assertEqual(undefined, cre_yawl_client:get_pattern_type(not_a_tuple)),
                    ?assertEqual(undefined, cre_yawl_client:get_pattern_type({}))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test evaluate_condition.
%% @end
%%--------------------------------------------------------------------
test_evaluate_condition_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    ?assert(cre_yawl_client:evaluate_condition(true)),
                    ?assertNot(cre_yawl_client:evaluate_condition(false)),
                    ?assert(cre_yawl_client:evaluate_condition(fun() -> true end)),
                    ?assertNot(cre_yawl_client:evaluate_condition(fun() -> false end))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test select_branch.
%% @end
%%--------------------------------------------------------------------
test_select_branch_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Test with first true branch
                    Branches = [
                        {task1, true},
                        {task2, false},
                        {task3, true}
                    ],
                    ?assertEqual({task1, true}, cre_yawl_client:select_branch(Branches)),

                    %% Test with all false branches
                    ?assertEqual(none, cre_yawl_client:select_branch([{task1, false}])),

                    %% Test empty list
                    ?assertEqual(none, cre_yawl_client:select_branch([]))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test select_branches.
%% @end
%%--------------------------------------------------------------------
test_select_branches_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Test with multiple true branches
                    Branches = [
                        {task1, true},
                        {task2, true},
                        {task3, false}
                    ],
                    Result = cre_yawl_client:select_branches(Branches, []),
                    ?assert(length(Result) >= 2),

                    %% Test empty list
                    ?assertEqual([], cre_yawl_client:select_branches([], []))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test code_change callback.
%% @end
%%--------------------------------------------------------------------
test_code_change_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    ?assertEqual({ok, State}, cre_yawl_client:code_change(old_vsn, State, extra))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test terminate callback.
%% @end
%%--------------------------------------------------------------------
test_terminate_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    ?assertEqual(ok, cre_yawl_client:terminate(normal, State))
                end)]
     end}.

%%====================================================================
%% EUnit Test Export
%%====================================================================

%% @doc Returns all test generators for EUnit discovery.
%% @end
%%--------------------------------------------------------------------
test() ->
    [
        test_init_(),
        test_step_(),
        test_recv_(),
        test_load_(),
        test_unload_(),
        test_compose_patterns_(),
        test_is_pattern_record_(),
        test_get_pattern_type_(),
        test_evaluate_condition_(),
        test_select_branch_(),
        test_select_branches_(),
        test_code_change_(),
        test_terminate_()
    ].
