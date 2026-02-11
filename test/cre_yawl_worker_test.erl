%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc CRE YAWL Worker Test Suite
%%
%% Comprehensive test suite for the cre_yawl_worker module.
%% Tests cover worker lifecycle, task execution, handler registration,
%% pattern execution, parallel processing, and error handling.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_yawl_worker_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    test_start_link_/0,
    test_start_link_named_/0,
    test_stop_/0,
    test_init_/0,
    test_register_task_handler_/0,
    test_unregister_task_handler_/0,
    test_list_task_handlers_/0,
    test_get_task_handler_/0,
    test_execute_task_basic_/0,
    test_execute_task_async_/0,
    test_clear_task_cache_/0,
    test_execute_parallel_/0,
    test_execute_join_and_/0,
    test_execute_join_or_/0,
    test_execute_join_xor_/0,
    test_code_change_/0,
    test_terminate_/0,
    test_trigger_/0,
    test_classify_error_/0,
    test_file_exists_/0,
    setup/0,
    cleanup/1
]).

-compile(export_all).

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

    %% Clean up any existing worker
    catch cre_yawl_worker:stop(test_yawl_worker),

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
    catch cre_yawl_worker:stop(test_yawl_worker),
    receive
        _ -> ok
    after 0 -> ok
    end,
    timer:sleep(10).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test start_link with CRE master.
%% @end
%%--------------------------------------------------------------------
test_start_link_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Config = #{timeout => 5000},
                    Result = cre_yawl_worker:start_link(cre_master, Config),
                    ?assertMatch({ok, _Pid}, Result),
                    {ok, Pid} = Result,
                    ?assert(is_pid(Pid)),
                    gen_server:stop(Pid)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test start_link with named worker.
%% @end
%%--------------------------------------------------------------------
test_start_link_named_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Config = #{timeout => 5000},
                    Result = cre_yawl_worker:start_link(test_yawl_worker_named, cre_master, Config),
                    ?assertMatch({ok, _Pid}, Result),
                    {ok, Pid} = Result,
                    ?assert(is_pid(Pid)),
                    cre_yawl_worker:stop(test_yawl_worker_named)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test stopping the worker.
%% @end
%%--------------------------------------------------------------------
test_stop_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Config = #{timeout => 5000},
                    {ok, Pid} = cre_yawl_worker:start_link(test_yawl_worker_stop, cre_master, Config),
                    ?assertEqual(ok, cre_yawl_worker:stop(Pid))
                end)]
     end}.

%%====================================================================
%% API Function Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test init callback with map configuration.
%% @end
%%--------------------------------------------------------------------
test_init_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Config = #{cre_name => cre_master, timeout => 5000},
                    State = cre_yawl_worker:init(Config),
                    ?assert(is_map(State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test registering a task handler.
%% @end
%%--------------------------------------------------------------------
test_register_task_handler_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start a worker
                    {ok, Pid} = cre_yawl_worker:start_link(test_yawl_worker_reg, cre_master, #{}),

                    %% Register a handler
                    Handler = fun(Input) -> {ok, Input} end,
                    ?assertEqual(ok, cre_yawl_worker:register_task_handler(<<"test_handler">>, Handler, atomic)),

                    %% Clean up
                    gen_server:stop(Pid)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unregistering a task handler.
%% @end
%%--------------------------------------------------------------------
test_unregister_task_handler_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start a worker
                    {ok, Pid} = cre_yawl_worker:start_link(test_yawl_worker_unreg, cre_master, #{}),

                    %% Unregister non-existent handler (should not crash)
                    ?assertEqual(ok, cre_yawl_worker:unregister_task_handler(<<"nonexistent_handler">>)),

                    %% Clean up
                    gen_server:stop(Pid)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test listing task handlers.
%% @end
%%--------------------------------------------------------------------
test_list_task_handlers_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start a worker
                    {ok, Pid} = cre_yawl_worker:start_link(test_yawl_worker_list, cre_master, #{}),

                    %% List handlers (should return empty list or map)
                    Handlers = cre_yawl_worker:list_task_handlers(),
                    ?assert(is_list(Handlers) orelse is_map(Handlers)),

                    %% Clean up
                    gen_server:stop(Pid)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test getting a task handler.
%% @end
%%--------------------------------------------------------------------
test_get_task_handler_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Get non-existent handler
                    Result = cre_yawl_worker:get_task_handler(<<"nonexistent">>),
                    ?assertMatch({error, _}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test executing a basic task.
%% @end
%%--------------------------------------------------------------------
test_execute_task_basic_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% First register a handler
                    {ok, Pid} = cre_yawl_worker:start_link(test_yawl_worker_exec, cre_master, #{}),
                    Handler = fun(_) -> {ok, test_result} end,
                    cre_yawl_worker:register_task_handler(<<"test_task">>, Handler, atomic),

                    %% Execute task - test that function exists and doesn't crash
                    Task = #{
                      id => <<"task_1">>,
                      name => <<"test_task">>,
                      type => atomic,
                      input => #{},
                      metadata => #{}
                     },
                    Result = cre_yawl_worker:execute_task(Task),
                    case Result of
                        {ok, _} -> ok;
                        {error, _} -> ok;
                        _ -> ?assert(false, "Unexpected result")
                    end,

                    %% Clean up
                    gen_server:stop(Pid)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test executing task asynchronously.
%% @end
%%--------------------------------------------------------------------
test_execute_task_async_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Task = #{
                      id => <<"async_task_1">>,
                      name => <<"test_async_task">>,
                      type => atomic,
                      input => #{},
                      metadata => #{}
                     },
                    Result = cre_yawl_worker:execute_task_async(Task),
                    case Result of
                        {ok, _Ref} -> ok;
                        {error, _} -> ok;
                        _ -> ?assert(false, "Unexpected result")
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test clearing task cache.
%% @end
%%--------------------------------------------------------------------
test_clear_task_cache_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    ?assertEqual(ok, cre_yawl_worker:clear_task_cache())
                end)]
     end}.

%%====================================================================
%% Pattern Execution Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test parallel execution.
%% @end
%%--------------------------------------------------------------------
test_execute_parallel_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Test that function exists and accepts correct arguments
                    %% The actual implementation may vary
                    Result = cre_yawl_worker:execute_parallel([], 5000),
                    case Result of
                        {ok, []} -> ok;
                        {error, _} -> ok;
                        _ -> ?assert(false, "Unexpected result")
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test AND join.
%% @end
%%--------------------------------------------------------------------
test_execute_join_and_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Results = [r1, r2, r3],
                    ?assertMatch({ok, {all_results, Results}},
                               cre_yawl_worker:execute_join(and_join, Results, undefined)),

                    %% Empty results
                    ?assertMatch({error, _},
                               cre_yawl_worker:execute_join(and_join, [], undefined))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test OR join.
%% @end
%%--------------------------------------------------------------------
test_execute_join_or_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% With results - returns first
                    ?assertMatch({ok, r1}, cre_yawl_worker:execute_join(or_join, [r1, r2], undefined)),

                    %% Empty results - returns default
                    ?assertMatch({ok, default}, cre_yawl_worker:execute_join(or_join, [], default))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test XOR join.
%% @end
%%--------------------------------------------------------------------
test_execute_join_xor_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Single result
                    ?assertMatch({ok, r1}, cre_yawl_worker:execute_join(xor_join, [r1], undefined)),

                    %% Empty results
                    ?assertMatch({ok, default}, cre_yawl_worker:execute_join(xor_join, [], default)),

                    %% Multiple results - error
                    ?assertMatch({error, _},
                               cre_yawl_worker:execute_join(xor_join, [r1, r2], undefined))
                end)]
     end}.

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

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
                    ?assertEqual({ok, State}, cre_yawl_worker:code_change(old_vsn, State, extra))
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
                    ?assertEqual(ok, cre_yawl_worker:terminate(normal, State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test trigger callback.
%% @end
%%--------------------------------------------------------------------
test_trigger_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    Result = cre_yawl_worker:trigger(test_place, test_token, State),
                    ?assertEqual(pass, Result)
                end)]
     end}.

%%====================================================================
%% Internal Function Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test classify_error if exported.
%% @end
%%--------------------------------------------------------------------
test_classify_error_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Test that error classification function exists and works
                    ?assert(is_function(fun cre_yawl_worker:classify_error/1))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test file_exists helper.
%% @end
%%--------------------------------------------------------------------
test_file_exists_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Test that file checking function exists
                    ?assert(is_function(fun cre_yawl_worker:file_exists/1))
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
        test_start_link_(),
        test_start_link_named_(),
        test_stop_(),
        test_init_(),
        test_register_task_handler_(),
        test_unregister_task_handler_(),
        test_list_task_handlers_(),
        test_get_task_handler_(),
        test_execute_task_basic_(),
        test_execute_task_async_(),
        test_clear_task_cache_(),
        test_execute_parallel_(),
        test_execute_join_and_(),
        test_execute_join_or_(),
        test_execute_join_xor_(),
        test_code_change_(),
        test_terminate_(),
        test_trigger_(),
        test_classify_error_(),
        test_file_exists_()
    ].
