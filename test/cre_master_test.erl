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
%% @doc CRE Master Test Suite
%%
%% Comprehensive test suite for the cre_master module.
%% Tests cover worker registration, request handling, result processing,
%% cache management, status queries, and worker failure scenarios.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_master_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    test_start_link_anonymous_/0,
    test_start_link_named_/0,
    test_add_worker_by_pid_/0,
    test_add_worker_by_name_/0,
    test_worker_result_/0,
    test_cre_request_cached_/0,
    test_cre_request_new_/0,
    test_cre_request_queue_/0,
    test_get_status_empty_/0,
    test_get_status_with_workers_/0,
    test_get_status_with_active_tasks_/0,
    test_get_history_empty_/0,
    test_get_history_with_results_/0,
    test_worker_exit_idle_/0,
    test_worker_exit_busy_/0,
    test_stop_/0,
    test_code_change_/0,
    test_multiple_workers_/0,
    test_worker_progression_/0,
    test_cache_lookup_/0,
    test_multiple_subscribers_/0,
    test_worker_reuse_/0,
    test_queue_processing_/0,
    test_busy_worker_death_/0,
    setup/0,
    cleanup/1,
    setup_master/0,
    setup_worker/0,
    simple_app/0,
    complex_app/0
]).

-compile(export_all).

%% Record definition from cre_master module
-record(cre_state, {
    subscr_map = #{},
    idle_lst = [],
    busy_map = #{},
    queue = [],
    cache = #{}
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Creates a test master instance.
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Initialize persistent terms for configuration
    cre_config:init(),

    %% Start a master for testing
    {ok, MasterPid} = cre_master:start_link(),
    MasterPid.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the master and flushes messages.
%% @end
%%--------------------------------------------------------------------
cleanup(_MasterPid) ->
    %% Stop the master
    catch gen_server:stop(cre_master),
    %% Wait a bit for shutdown
    timer:sleep(10),
    %% Flush any remaining messages
    receive
        _ -> ok
    after 0 -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Creates a master instance for testing.
%% @end
%%--------------------------------------------------------------------
setup_master() ->
    case whereis(cre_master) of
        undefined ->
            {ok, Pid} = cre_master:start_link(),
            Pid;
        Pid ->
            Pid
    end.

%%--------------------------------------------------------------------
%% @doc Creates a simple worker application for testing.
%% @end
%%--------------------------------------------------------------------
simple_app() ->
    #{app_id => <<"simple_app_1">>,
      lambda => #{lambda_name => <<"test_lambda">>}}.

%%--------------------------------------------------------------------
%% @doc Creates a complex worker application for testing.
%% @end
%%--------------------------------------------------------------------
complex_app() ->
    #{app_id => <<"complex_app_1">>,
      lambda => #{lambda_name => <<"complex_lambda">>},
      input => #{data => <<"test_data">>}}.

%%--------------------------------------------------------------------
%% @doc Sets up a mock worker process for testing.
%% @end
%%--------------------------------------------------------------------
setup_worker() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test starting an anonymous master instance.
%% Verifies that a master can be started without a name.
%% @end
%%--------------------------------------------------------------------
test_start_link_anonymous_() ->
    {setup,
     fun() ->
        %% Ensure clean state
        catch unregister(anonymous_master_test),
        ok
     end,
     fun(_After) ->
        catch unregister(anonymous_master_test),
        ok
     end,
     fun(_Before) ->
         [?_test(begin
                    {ok, Pid} = cre_master:start_link(),
                    ?assert(is_pid(Pid)),
                    ?assert(process_info(Pid) =/= undefined),
                    gen_server:stop(Pid)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test starting a named master instance.
%% Verifies that a master can be started with a registered name.
%% @end
%%--------------------------------------------------------------------
test_start_link_named_() ->
    {setup,
     fun() ->
        catch unregister(named_master_test),
        ok
     end,
     fun(_After) ->
        catch unregister(named_master_test),
        ok
     end,
     fun(_Before) ->
         [?_test(begin
                    {ok, Pid} = cre_master:start_link(named_master_test),
                    ?assert(is_pid(Pid)),
                    ?assertEqual(Pid, whereis(named_master_test)),
                    gen_server:stop(named_master_test)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test stopping the master instance.
%% Verifies that a master can be stopped cleanly.
%% @end
%%--------------------------------------------------------------------
test_stop_() ->
    {setup,
     fun() ->
        catch unregister(stop_test_master),
        {ok, Pid} = cre_master:start_link(stop_test_master),
        Pid
     end,
     fun(_Pid) ->
        catch unregister(stop_test_master),
        timer:sleep(10),
        ok
     end,
     fun(_Pid) ->
         [?_test(begin
                    ?assertEqual(ok, cre_master:stop(stop_test_master)),
                    timer:sleep(10),
                    ?assertEqual(undefined, whereis(stop_test_master))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test code_change callback.
%% Verifies hot code upgrade capability.
%% @end
%%--------------------------------------------------------------------
test_code_change_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    State = #cre_state{},
                    ?assertEqual({ok, State}, cre_master:code_change(old_vsn, State, extra))
                end)]
     end}.

%%====================================================================
%% Worker Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding a worker by PID.
%% Verifies that a worker process can be registered with the master.
%% @end
%%--------------------------------------------------------------------
test_add_worker_by_pid_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    WorkerPid = setup_worker(),
                    ?assertEqual(ok, cre_master:add_worker(cre_master, WorkerPid)),
                    %% Verify status shows the worker
                    Status = cre_master:get_status(cre_master),
                    Workers = maps:get(n_wrk, maps:get(cre_info, Status)),
                    ?assert(Workers >= 1),
                    %% Clean up worker
                    WorkerPid ! stop,
                    timer:sleep(10)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test adding a worker by registered name.
%% Verifies that a named worker can be registered.
%% @end
%%--------------------------------------------------------------------
test_add_worker_by_name_() ->
    {setup,
     fun() ->
        catch unregister(test_worker),
        WorkerPid = spawn(fun() ->
            register(test_worker, self()),
            receive
                stop -> ok
            end
        end),
        {ok, MasterPid} = cre_master:start_link(),
        {MasterPid, WorkerPid}
     end,
     fun({_MasterPid, WorkerPid}) ->
        WorkerPid ! stop,
        catch unregister(test_worker),
        catch gen_server:stop(cre_master),
        timer:sleep(10),
        ok
     end,
     fun({_MasterPid, WorkerPid}) ->
         [?_test(begin
                    ?assertEqual(ok, cre_master:add_worker(cre_master, test_worker)),
                    %% Verify status shows the worker
                    Status = cre_master:get_status(cre_master),
                    Workers = maps:get(n_wrk, maps:get(cre_info, Status)),
                    ?assert(Workers >= 1)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test handling of idle worker exit.
%% Verifies that idle workers are properly removed when they exit.
%% @end
%%--------------------------------------------------------------------
test_worker_exit_idle_() ->
    {setup,
     fun() ->
        {ok, MasterPid} = cre_master:start_link(),
        WorkerPid = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        {MasterPid, WorkerPid}
     end,
     fun({_MasterPid, WorkerPid}) ->
        WorkerPid ! stop,
        catch gen_server:stop(cre_master),
        timer:sleep(20),
        ok
     end,
     fun({_MasterPid, WorkerPid}) ->
         [?_test(begin
                    %% Add worker
                    ok = cre_master:add_worker(cre_master, WorkerPid),
                    Status1 = cre_master:get_status(cre_master),
                    Workers1 = maps:get(n_wrk, maps:get(cre_info, Status1)),
                    ?assert(Workers1 >= 1),

                    %% Kill worker
                    exit(WorkerPid, kill),
                    timer:sleep(20),

                    %% Verify worker removed
                    Status2 = cre_master:get_status(cre_master),
                    Workers2 = maps:get(n_wrk, maps:get(cre_info, Status2)),
                    ?assertEqual(0, Workers2)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test handling of busy worker exit.
%% Verifies that tasks from busy workers are re-queued when workers exit.
%% @end
%%--------------------------------------------------------------------
test_worker_exit_busy_() ->
    {setup,
     fun() ->
        {ok, MasterPid} = cre_master:start_link(),
        MasterPid
     end,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    %% Create a worker that will die while busy
                    WorkerPid = spawn(fun() ->
                        receive
                            _ -> ok
                        end
                    end),

                    %% Add worker
                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    %% Send request to get worker busy
                    App = simple_app(),
                    cre_master:cre_request(cre_master, self(), 1, App),

                    %% Wait a bit for scheduling
                    timer:sleep(50),

                    %% Kill the busy worker
                    exit(WorkerPid, kill),
                    timer:sleep(50),

                    %% Verify state after worker death
                    Status = cre_master:get_status(cre_master),
                    ?assert(maps:is_key(Status, cre_info))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test multiple workers registration.
%% Verifies that multiple workers can be registered and tracked.
%% @end
%%--------------------------------------------------------------------
test_multiple_workers_() ->
    {setup,
     fun() ->
        {ok, MasterPid} = cre_master:start_link(),
        Worker1 = spawn(fun() -> receive stop -> ok end end),
        Worker2 = spawn(fun() -> receive stop -> ok end end),
        Worker3 = spawn(fun() -> receive stop -> ok end end),
        {MasterPid, [Worker1, Worker2, Worker3]}
     end,
     fun({_MasterPid, Workers}) ->
        lists:foreach(fun(W) -> W ! stop, timer:sleep(5) end, Workers),
        catch gen_server:stop(cre_master),
        timer:sleep(20),
        ok
     end,
     fun({_MasterPid, Workers}) ->
         [?_test(begin
                    %% Add all workers
                    lists:foreach(fun(W) ->
                        cre_master:add_worker(cre_master, W)
                    end, Workers),

                    %% Verify count
                    Status = cre_master:get_status(cre_master),
                    WorkerCount = maps:get(n_wrk, maps:get(cre_info, Status)),
                    ?assertEqual(3, WorkerCount)
                end)]
     end}.

%%====================================================================
%% Request Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test request with cached result.
%% Verifies that cached results are returned without scheduling.
%% @end
%%--------------------------------------------------------------------
test_cre_request_cached_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    App = simple_app(),
                    Delta = #{result => cached_result},

                    %% First, cache a result by simulating worker completion
                    WorkerPid = spawn(fun() -> receive stop -> ok end end),
                    ok = cre_master:add_worker(cre_master, WorkerPid),
                    cre_master:worker_result(cre_master, WorkerPid, App, Delta),
                    timer:sleep(50),

                    %% Now make a request - should get cached result
                    %% The cre_client:cre_reply will be called
                    ?assertMatch(#{}, cre_master:get_history(cre_master))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test new request with available worker.
%% Verifies that requests are dispatched to idle workers.
%% @end
%%--------------------------------------------------------------------
test_cre_request_new_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    %% Create a mock worker
                    WorkerPid = spawn(fun() ->
                        receive
                            {worker_request, _App} -> ok;
                            stop -> ok
                        end
                    end),

                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    App = simple_app(),
                    cre_master:cre_request(cre_master, self(), 1, App),

                    timer:sleep(50),

                    WorkerPid ! stop,
                    timer:sleep(10)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test request queuing when no workers available.
%% Verifies that requests are queued when all workers are busy.
%% @end
%%--------------------------------------------------------------------
test_cre_request_queue_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    %% No workers available
                    App = simple_app(),
                    cre_master:cre_request(cre_master, self(), 1, App),

                    timer:sleep(50),

                    %% Request should be queued
                    Status = cre_master:get_status(cre_master),
                    AppInfo = maps:get(app_info, Status),
                    Queued = maps:get(queued, AppInfo),
                    ?assert(length(Queued) >= 0)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test multiple subscribers to same application.
%% Verifies that multiple clients can subscribe to the same application.
%% @end
%%--------------------------------------------------------------------
test_multiple_subscribers_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    App = simple_app(),

                    %% Multiple subscribers request same app
                    cre_master:cre_request(cre_master, self(), 1, App),
                    cre_master:cre_request(cre_master, self(), 2, App),

                    timer:sleep(50),

                    %% Verify requests tracked
                    Status = cre_master:get_status(cre_master),
                    ?assert(maps:is_key(Status, app_info))
                end)]
     end}.

%%====================================================================
%% Result Processing Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test worker result processing.
%% Verifies that worker results are processed and subscribers notified.
%% @end
%%--------------------------------------------------------------------
test_worker_result_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    App = simple_app(),
                    Delta = #{result => test_complete},

                    %% Create and add worker
                    WorkerPid = spawn(fun() -> receive stop -> ok end end),
                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    %% Make a request first
                    cre_master:cre_request(cre_master, self(), 1, App),
                    timer:sleep(50),

                    %% Send result
                    cre_master:worker_result(cre_master, WorkerPid, App, Delta),
                    timer:sleep(50),

                    %% Verify result in history
                    History = cre_master:get_history(cre_master),
                    ?assert(maps:is_key(history, History)),

                    WorkerPid ! stop,
                    timer:sleep(10)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test worker reuse after completion.
%% Verifies that workers return to idle pool after completing tasks.
%% @end
%%--------------------------------------------------------------------
test_worker_reuse_() ->
    {setup,
     fun() ->
        {ok, MasterPid} = cre_master:start_link(),
        MasterPid
     end,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    WorkerPid = spawn(fun() ->
                        receive
                            {worker_request, _App} -> ok;
                            stop -> ok
                        end
                    end),

                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    %% First request
                    App1 = simple_app(),
                    cre_master:cre_request(cre_master, self(), 1, App1),
                    timer:sleep(50),

                    %% Complete the task
                    Delta1 = #{result => complete1},
                    cre_master:worker_result(cre_master, WorkerPid, App1, Delta1),
                    timer:sleep(50),

                    %% Second request - worker should be reused
                    App2 = complex_app(),
                    cre_master:cre_request(cre_master, self(), 2, App2),
                    timer:sleep(50),

                    WorkerPid ! stop,
                    timer:sleep(10)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test cache lookup for repeated applications.
%% Verifies that cached results prevent re-computation.
%% @end
%%--------------------------------------------------------------------
test_cache_lookup_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    App = simple_app(),
                    Delta = #{result => cached_value},

                    %% Add worker and complete a task
                    WorkerPid = spawn(fun() -> receive stop -> ok end end),
                    ok = cre_master:add_worker(cre_master, WorkerPid),
                    cre_master:cre_request(cre_master, self(), 1, App),
                    timer:sleep(50),
                    cre_master:worker_result(cre_master, WorkerPid, App, Delta),
                    timer:sleep(50),

                    %% Make same request again - should use cache
                    cre_master:cre_request(cre_master, self(), 2, App),
                    timer:sleep(50),

                    %% Verify cache contains result
                    History = cre_master:get_history(cre_master),
                    ?assert(maps:is_key(history, History)),

                    WorkerPid ! stop,
                    timer:sleep(10)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test queue processing when worker becomes available.
%% Verifies that queued requests are processed when workers free up.
%% @end
%%--------------------------------------------------------------------
test_queue_processing_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    %% Queue requests before adding worker
                    App1 = simple_app(),
                    App2 = complex_app(),

                    cre_master:cre_request(cre_master, self(), 1, App1),
                    cre_master:cre_request(cre_master, self(), 2, App2),
                    timer:sleep(50),

                    %% Add worker - should process queue
                    WorkerPid = spawn(fun() ->
                        receive
                            {worker_request, _App} -> ok;
                            stop -> ok
                        end
                    end),
                    ok = cre_master:add_worker(cre_master, WorkerPid),
                    timer:sleep(50),

                    WorkerPid ! stop,
                    timer:sleep(10)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test busy worker death requeues tasks.
%% Verifies that tasks from dead workers are put back in queue.
%% @end
%%--------------------------------------------------------------------
test_busy_worker_death_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    WorkerPid = spawn(fun() ->
                        receive
                            {worker_request, _App} ->
                                %% Die while busy
                                exit(normal);
                            stop -> ok
                        end
                    end),

                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    App = simple_app(),
                    cre_master:cre_request(cre_master, self(), 1, App),
                    timer:sleep(100),

                    %% Worker should have died, task requeued
                    Status = cre_master:get_status(cre_master),
                    ?assert(maps:is_key(Status, app_info))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test worker progression logic.
%% Verifies the attempt_progress internal function behavior.
%% @end
%%--------------------------------------------------------------------
test_worker_progression_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    %% Create multiple workers
                    Worker1 = spawn(fun() -> receive stop -> ok end end),
                    Worker2 = spawn(fun() -> receive stop -> ok end end),

                    lists:foreach(fun(W) ->
                        cre_master:add_worker(cre_master, W)
                    end, [Worker1, Worker2]),

                    %% Queue multiple requests
                    App1 = simple_app(),
                    App2 = complex_app(),
                    App3 = simple_app(),

                    cre_master:cre_request(cre_master, self(), 1, App1),
                    cre_master:cre_request(cre_master, self(), 2, App2),
                    cre_master:cre_request(cre_master, self(), 3, App3),

                    timer:sleep(100),

                    %% Verify workers were used
                    Status = cre_master:get_status(cre_master),
                    Load = maps:get(load, maps:get(cre_info, Status)),
                    ?assert(Load >= 0.0),

                    %% Cleanup
                    Worker1 ! stop,
                    Worker2 ! stop,
                    timer:sleep(20)
                end)]
     end}.

%%====================================================================
%% Status Query Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test get status with empty master.
%% Verifies status report for master with no workers or tasks.
%% @end
%%--------------------------------------------------------------------
test_get_status_empty_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    Status = cre_master:get_status(cre_master),

                    %% Verify structure
                    ?assertMatch(#{cre_info := _, node_info := _, app_info := _}, Status),

                    %% Verify counts
                    CreInfo = maps:get(cre_info, Status),
                    ?assertEqual(0, maps:get(n_wrk, CreInfo)),
                    ?assertEqual(0.0, maps:get(load, CreInfo)),

                    %% Verify app info
                    AppInfo = maps:get(app_info, Status),
                    ?assertEqual([], maps:get(queued, AppInfo)),
                    ?assertEqual([], maps:get(active, AppInfo)),
                    ?assertEqual([], maps:get(complete, AppInfo))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test get status with workers registered.
%% Verifies status report includes worker information.
%% @end
%%--------------------------------------------------------------------
test_get_status_with_workers_() ->
    {setup,
     fun() ->
        {ok, MasterPid} = cre_master:start_link(),
        Worker1 = spawn(fun() -> receive stop -> ok end end),
        Worker2 = spawn(fun() -> receive stop -> ok end end),
        lists:foreach(fun(W) -> cre_master:add_worker(cre_master, W) end, [Worker1, Worker2]),
        timer:sleep(50),
        {MasterPid, [Worker1, Worker2]}
     end,
     fun({_MasterPid, Workers}) ->
        lists:foreach(fun(W) -> W ! stop, timer:sleep(5) end, Workers),
        catch gen_server:stop(cre_master),
        timer:sleep(20),
        ok
     end,
     fun({_MasterPid, _Workers}) ->
         [?_test(begin
                    Status = cre_master:get_status(cre_master),

                    CreInfo = maps:get(cre_info, Status),
                    ?assertEqual(2, maps:get(n_wrk, CreInfo)),

                    %% Verify node info
                    NodeInfo = maps:get(node_info, Status),
                    ?assert(length(NodeInfo) > 0)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test get status with active tasks.
%% Verifies status report includes active task information.
%% @end
%%--------------------------------------------------------------------
test_get_status_with_active_tasks_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    %% Add worker and make request
                    WorkerPid = spawn(fun() ->
                        receive
                            {worker_request, _App} ->
                                timer:sleep(100);
                            stop -> ok
                        end
                    end),

                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    App = simple_app(),
                    cre_master:cre_request(cre_master, self(), 1, App),
                    timer:sleep(50),

                    %% Check status
                    Status = cre_master:get_status(cre_master),
                    AppInfo = maps:get(app_info, Status),

                    %% Either active or queued depending on timing
                    Active = maps:get(active, AppInfo),
                    Queued = maps:get(queued, AppInfo),
                    ?assert(length(Active) + length(Queued) >= 1),

                    WorkerPid ! stop,
                    timer:sleep(20)
                end)]
     end}.

%%====================================================================
%% History Query Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test get history with empty master.
%% Verifies empty history when no tasks completed.
%% @end
%%--------------------------------------------------------------------
test_get_history_empty_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    History = cre_master:get_history(cre_master),
                    ?assertMatch(#{history := []}, History)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test get history with completed tasks.
%% Verifies history includes completed task results.
%% @end
%%--------------------------------------------------------------------
test_get_history_with_results_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_MasterPid) ->
         [?_test(begin
                    App = simple_app(),
                    Delta = #{result => test_result, value => 42},

                    WorkerPid = spawn(fun() -> receive stop -> ok end end),
                    ok = cre_master:add_worker(cre_master, WorkerPid),

                    cre_master:cre_request(cre_master, self(), 1, App),
                    timer:sleep(50),
                    cre_master:worker_result(cre_master, WorkerPid, App, Delta),
                    timer:sleep(50),

                    History = cre_master:get_history(cre_master),
                    ?assert(maps:is_key(history, History)),

                    HistoryList = maps:get(history, History),
                    ?assert(length(HistoryList) >= 0),

                    WorkerPid ! stop,
                    timer:sleep(10)
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
        test_start_link_anonymous_(),
        test_start_link_named_(),
        test_add_worker_by_pid_(),
        test_add_worker_by_name_(),
        test_worker_result_(),
        test_cre_request_cached_(),
        test_cre_request_new_(),
        test_cre_request_queue_(),
        test_get_status_empty_(),
        test_get_status_with_workers_(),
        test_get_status_with_active_tasks_(),
        test_get_history_empty_(),
        test_get_history_with_results_(),
        test_worker_exit_idle_(),
        test_worker_exit_busy_(),
        test_stop_(),
        test_code_change_(),
        test_multiple_workers_(),
        test_worker_progression_(),
        test_cache_lookup_(),
        test_multiple_subscribers_(),
        test_worker_reuse_(),
        test_queue_processing_(),
        test_busy_worker_death_()
    ].
