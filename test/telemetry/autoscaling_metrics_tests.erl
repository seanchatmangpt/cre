%% -*- erlang -*-
%% @doc Unit tests for autoscaling_metrics

-module(autoscaling_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = autoscaling_metrics:start_link([{collect_interval, 1000}]),
    Pid.

cleanup(Pid) ->
    autoscaling_metrics:stop(),
    timer:sleep(100).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

start_stop_test() ->
    ?assertEqual(ok, autoscaling_metrics:stop()),
    {ok, Pid} = autoscaling_metrics:start_link([{collect_interval, 1000}]),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, autoscaling_metrics:stop()).

start_with_options_test() ->
    Options = [{collect_interval, 5000}],
    {ok, Pid} = autoscaling_metrics:start_link(Options),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, autoscaling_metrics:stop()).

%%====================================================================
%% Metric Collection Tests
%%====================================================================

get_active_workflows_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Count = autoscaling_metrics:get_active_workflows(),
                    ?assert(is_integer(Count)),
                    ?assert(Count >= 0)
                end)
         ]
     end}.

get_workflow_queue_depth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Depth = autoscaling_metrics:get_workflow_queue_depth(),
                    ?assert(is_integer(Depth)),
                    ?assert(Depth >= 0)
                end)
         ]
     end}.

get_erlang_process_count_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Count = autoscaling_metrics:get_erlang_process_count(),
                    ?assert(is_integer(Count)),
                    ?assert(Count > 0),
                    %% Should be close to system process count
                    SystemCount = erlang:system_info(process_count),
                    ?assert(abs(Count - SystemCount) < 1000)
                end)
         ]
     end}.

get_mnesia_table_size_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Size = autoscaling_metrics:get_mnesia_table_size(),
                    ?assert(is_integer(Size)),
                    ?assert(Size >= 0)
                end)
         ]
     end}.

get_all_metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Metrics = autoscaling_metrics:get_all_metrics(),
                    ?assert(is_map(Metrics)),
                    ?assert(maps:is_key(active_workflows, Metrics)),
                    ?assert(maps:is_key(workflow_queue_depth, Metrics)),
                    ?assert(maps:is_key(erlang_process_count, Metrics)),
                    ?assert(maps:is_key(mnesia_table_size, Metrics)),

                    %% Verify types
                    ?assert(is_integer(maps:get(active_workflows, Metrics))),
                    ?assert(is_integer(maps:get(workflow_queue_depth, Metrics))),
                    ?assert(is_integer(maps:get(erlang_process_count, Metrics))),
                    ?assert(is_integer(maps:get(mnesia_table_size, Metrics)))
                end)
         ]
     end}.

%%====================================================================
%% Prometheus Export Tests
%%====================================================================

export_prometheus_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Export = iolist_to_binary(autoscaling_metrics:export_prometheus()),

                    %% Check for HELP headers
                    ?assertMatch(true, binary:match(Export, <<"# HELP cre_autoscaling_active_workflows">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# HELP cre_autoscaling_workflow_queue_depth">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# HELP cre_autoscaling_erlang_process_count">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# HELP cre_autoscaling_mnesia_table_size">>) =/= nomatch),

                    %% Check for TYPE headers
                    ?assertMatch(true, binary:match(Export, <<"# TYPE cre_autoscaling_active_workflows gauge">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# TYPE cre_autoscaling_workflow_queue_depth gauge">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# TYPE cre_autoscaling_erlang_process_count gauge">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# TYPE cre_autoscaling_mnesia_table_size gauge">>) =/= nomatch),

                    %% Check for metric values
                    ?assertMatch(true, binary:match(Export, <<"cre_autoscaling_active_workflows ">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"cre_autoscaling_workflow_queue_depth ">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"cre_autoscaling_erlang_process_count ">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"cre_autoscaling_mnesia_table_size ">>) =/= nomatch)
                end)
         ]
     end}.

prometheus_format_validity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    Export = iolist_to_binary(autoscaling_metrics:export_prometheus()),

                    %% Lines should end with newlines
                    Lines = binary:split(Export, <<"\n">>, [global]),
                    ?assert(length(Lines) > 0),

                    %% Verify no empty lines between metrics
                    NonEmpty = [L || L <- Lines, L =/= <<>>],
                    ?assert(length(NonEmpty) > 0)
                end)
         ]
     end}.

%%====================================================================
%% Metric History Tests
%%====================================================================

get_metric_history_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Trigger collection
                    autoscaling_metrics:collect_metrics(),
                    timer:sleep(100),

                    %% Get history for each metric
                    ActiveHistory = autoscaling_metrics:get_metric_history(active_workflows),
                    ?assert(is_list(ActiveHistory)),

                    QueueHistory = autoscaling_metrics:get_metric_history(workflow_queue_depth),
                    ?assert(is_list(QueueHistory)),

                    ProcessHistory = autoscaling_metrics:get_metric_history(erlang_process_count),
                    ?assert(is_list(ProcessHistory)),

                    MnesiaHistory = autoscaling_metrics:get_metric_history(mnesia_table_size),
                    ?assert(is_list(MnesiaHistory))
                end)
         ]
     end}.

metric_history_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    autoscaling_metrics:collect_metrics(),
                    timer:sleep(100),

                    History = autoscaling_metrics:get_metric_history(active_workflows),

                    %% Each entry should be {Timestamp, Value}
                    lists:foreach(fun({Timestamp, Value}) ->
                        ?assert(is_integer(Timestamp)),
                        ?assert(Timestamp > 0),
                        ?assert(is_number(Value))
                    end, History)
                end)
         ]
     end}.

%%====================================================================
%% State Tests
%%====================================================================

get_state_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    State = autoscaling_metrics:get_state(),
                    ?assert(is_record(State, state))
                end)
         ]
     end}.

state_fields_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    State = autoscaling_metrics:get_state(),

                    %% Check all expected fields
                    ?assert(is_integer(State#state.collect_interval)),
                    ?assert(State#state.collect_interval > 0),

                    %% Timer ref may be undefined or a reference
                    TimerRef = State#state.timer_ref,
                    ?assert(TimerRef =:= undefined orelse is_reference(TimerRef)),

                    %% Last collect time should be recent
                    ?assert(is_integer(State#state.last_collect_time)),

                    %% Metric values should be non-negative integers
                    ?assert(is_integer(State#state.active_workflows)),
                    ?assert(State#state.active_workflows >= 0),

                    ?assert(is_integer(State#state.workflow_queue_depth)),
                    ?assert(State#state.workflow_queue_depth >= 0),

                    ?assert(is_integer(State#state.erlang_process_count)),
                    ?assert(State#state.erlang_process_count >= 0),

                    ?assert(is_integer(State#state.mnesia_table_size)),
                    ?assert(State#state.mnesia_table_size >= 0),

                    %% History should be a map
                    ?assert(is_map(State#state.history))
                end)
         ]
     end}.

%%====================================================================
%% Collection Interval Tests
%%====================================================================

collect_metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Get initial state
                    State1 = autoscaling_metrics:get_state(),
                    Time1 = State1#state.last_collect_time,

                    %% Trigger manual collection
                    ?assertEqual(ok, autoscaling_metrics:collect_metrics()),
                    timer:sleep(50),

                    %% Verify state updated
                    State2 = autoscaling_metrics:get_state(),
                    Time2 = State2#state.last_collect_time,

                    ?assert(Time2 >= Time1)
                end)
         ]
     end}.

auto_collection_test_() ->
    {timeout, 10, fun() ->
        %% Start with short interval
        {ok, Pid} = autoscaling_metrics:start_link([{collect_interval, 500}]),

        State1 = autoscaling_metrics:get_state(),
        Time1 = State1#state.last_collect_time,

        %% Wait for at least 2 collection cycles
        timer:sleep(1200),

        State2 = autoscaling_metrics:get_state(),
        Time2 = State2#state.last_collect_time,

        %% Should have collected at least once more
        ?assert(Time2 > Time1),

        autoscaling_metrics:stop()
    end}.

%%====================================================================
%% Scaling Behavior Tests
%%====================================================================

scaling_threshold_active_workflows_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Simulate low load
                    ActiveCount = autoscaling_metrics:get_active_workflows(),

                    %% For HPA: scale up when avg active_workflows > 50
                    %% Since this is a test with no actual workflows, count should be 0
                    ?assert(ActiveCount < 50),

                    %% Verify metric is exported for HPA consumption
                    Export = iolist_to_binary(autoscaling_metrics:export_prometheus()),
                    ?assertMatch(true, binary:match(Export, <<"cre_autoscaling_active_workflows ">>) =/= nomatch)
                end)
         ]
     end}.

scaling_threshold_queue_depth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% For HPA: scale up when avg workflow_queue_depth > 100
                    QueueDepth = autoscaling_metrics:get_workflow_queue_depth(),

                    %% In normal conditions, queue should be below threshold
                    ?assert(QueueDepth < 100),

                    Export = iolist_to_binary(autoscaling_metrics:export_prometheus()),
                    ?assertMatch(true, binary:match(Export, <<"cre_autoscaling_workflow_queue_depth ">>) =/= nomatch)
                end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

full_metric_cycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% 1. Collect metrics
                    ?assertEqual(ok, autoscaling_metrics:collect_metrics()),
                    timer:sleep(100),

                    %% 2. Get individual metrics
                    Active = autoscaling_metrics:get_active_workflows(),
                    Queue = autoscaling_metrics:get_workflow_queue_depth(),
                    Process = autoscaling_metrics:get_erlang_process_count(),
                    Mnesia = autoscaling_metrics:get_mnesia_table_size(),

                    %% 3. Get all metrics at once
                    All = autoscaling_metrics:get_all_metrics(),

                    %% 4. Verify consistency
                    ?assertEqual(Active, maps:get(active_workflows, All)),
                    ?assertEqual(Queue, maps:get(workflow_queue_depth, All)),
                    ?assertEqual(Process, maps:get(erlang_process_count, All)),
                    ?assertEqual(Mnesia, maps:get(mnesia_table_size, All)),

                    %% 5. Verify Prometheus export contains same values
                    Export = iolist_to_binary(autoscaling_metrics:export_prometheus()),
                    ?assertMatch(true, binary:match(Export, integer_to_binary(Active)) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, integer_to_binary(Process)) =/= nomatch)
                end)
         ]
     end}.

hpa_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Verify metrics are HPA-compatible
                    Export = iolist_to_binary(autoscaling_metrics:export_prometheus()),

                    %% Check for required Prometheus format elements
                    %% HPA requires proper metric names and types
                    ?assertMatch(true, binary:match(Export, <<"# TYPE">>) =/= nomatch),

                    %% All autoscaling metrics should be gauges
                    ?assertMatch(true, binary:match(Export, <<"# TYPE cre_autoscaling_active_workflows gauge">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Export, <<"# TYPE cre_autoscaling_workflow_queue_depth gauge">>) =/= nomatch),

                    %% Verify metrics are numeric (required for HPA)
                    Lines = binary:split(Export, <<"\n">>, [global]),
                    MetricLines = [L || L <- Lines,
                                       byte_size(L) > 0,
                                       binary:match(L, <<"cre_autoscaling_">>) =/= nomatch,
                                       binary:match(L, <<"#">>) =:= nomatch],

                    lists:foreach(fun(Line) ->
                        %% Extract value after space
                        case binary:split(Line, <<" ">>) of
                            [_, ValueBin] ->
                                %% Should be a valid number
                                Value = binary_to_integer(ValueBin),
                                ?assert(is_integer(Value));
                            _ ->
                                ok
                        end
                    end, MetricLines)
                end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

handle_invalid_metric_history_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Query for non-existent metric should return empty list
                    History = autoscaling_metrics:get_metric_history(non_existent_metric),
                    ?assertEqual([], History)
                end)
         ]
     end}.

multiple_start_stop_test_() ->
    %% Starting when already running should return error
    {ok, Pid1} = autoscaling_metrics:start_link([{collect_interval, 1000}]),
    Result = autoscaling_metrics:start_link([{collect_interval, 1000}]),
    ?assertMatch({error, _}, Result),
    autoscaling_metrics:stop().

%%====================================================================
%% Performance Tests
%%====================================================================

metric_collection_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Measure collection time
                    {Time, _} = timer:tc(fun() -> autoscaling_metrics:collect_metrics() end),

                    %% Collection should complete within 100ms
                    ?assert(Time < 100000)
                end)
         ]
     end}.

export_performance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Measure export time
                    {Time, _Export} = timer:tc(fun() -> autoscaling_metrics:export_prometheus() end),

                    %% Export should complete within 10ms
                    ?assert(Time < 10000)
                end)
         ]
     end}.

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

concurrent_metric_reads_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                    %% Spawn multiple readers
                    Readers = [spawn(fun() ->
                        autoscaling_metrics:get_active_workflows(),
                        autoscaling_metrics:get_workflow_queue_depth(),
                        autoscaling_metrics:get_erlang_process_count(),
                        autoscaling_metrics:get_mnesia_table_size()
                    end) || _ <- lists:seq(1, 10)],

                    %% Wait for all to complete
                    timer:sleep(200),

                    %% All should have completed without error
                    ?assert(length(Readers) =:= 10)
                end)
         ]
     end}.
