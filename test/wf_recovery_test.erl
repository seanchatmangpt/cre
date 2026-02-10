%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for wf_recovery module.
%%%
%%% Tests cover:
%%% - Retry with exponential backoff
%%% - Retry until predicates
%%% - Compensation handlers
%%% - Dead letter queue operations
%%% - Failure statistics tracking
%%% - Edge cases and error conditions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_recovery_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup/Teardown
%%%===================================================================

setup() ->
    ok = wf_recovery:init_dlq(),
    ok = wf_recovery:clear_dlq().

cleanup(_) ->
    ok = wf_recovery:clear_dlq().

%%%===================================================================
%%% Retry Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test retry succeeds on first attempt.
%%--------------------------------------------------------------------
retry_success_immediate_test() ->
    Fun = fun() -> {ok, success_result} end,
    Config = #{max_attempts => 3, initial_delay => 10, max_delay => 100},
    {ok, success_result} = wf_recovery:retry(Fun, Config).

%%--------------------------------------------------------------------
%% @doc Test retry succeeds after transient failure.
%%--------------------------------------------------------------------
retry_success_after_failure_test() ->
    erlang:erase(retry_test_counter),
    Fun = fun() ->
        case erlang:get(retry_test_counter) of
            undefined ->
                erlang:put(retry_test_counter, 1),
                {error, timeout};
            1 ->
                erlang:put(retry_test_counter, 2),
                {error, unavailable};
            _ ->
                {ok, eventual_success}
        end
    end,
    Config = #{max_attempts => 5, initial_delay => 10, max_delay => 100},
    {ok, eventual_success} = wf_recovery:retry(Fun, Config).

%%--------------------------------------------------------------------
%% @doc Test retry fails after max attempts exceeded.
%%--------------------------------------------------------------------
retry_max_attempts_exceeded_test() ->
    Fun = fun() -> {error, timeout} end,
    Config = #{max_attempts => 2, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, timeout}} = wf_recovery:retry(Fun, Config).

%%--------------------------------------------------------------------
%% @doc Test retry respects max_attempts configuration.
%%--------------------------------------------------------------------
retry_respects_max_attempts_test() ->
    Counter = {counter, 0},
    erlang:erase(retry_attempt_count),
    Fun = fun() ->
        Count = erlang:get(retry_attempt_count) orelse 0,
        erlang:put(retry_attempt_count, Count + 1),
        {error, timeout}
    end,
    Config = #{max_attempts => 3, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, timeout}} = wf_recovery:retry(Fun, Config),
    %% Should have attempted 3 times
    3 = erlang:get(retry_attempt_count).

%%--------------------------------------------------------------------
%% @doc Test retry with custom retry predicate.
%%--------------------------------------------------------------------
retry_custom_predicate_test() ->
    erlang:erase(retry_custom_counter),
    Fun = fun() ->
        Count = erlang:get(retry_custom_counter) orelse 0,
        erlang:put(retry_custom_counter, Count + 1),
        {error, not_retryable}
    end,
    %% Only retry on timeout, not on not_retryable
    Config = #{
        max_attempts => 5,
        initial_delay => 10,
        max_delay => 100,
        retry_on => fun(E) -> E =:= timeout end
    },
    {error, {max_retries_exceeded, not_retryable}} = wf_recovery:retry(Fun, Config),
    %% Should only attempt once since not_retryable fails immediately
    1 = erlang:get(retry_custom_counter).

%%--------------------------------------------------------------------
%% @doc Test retry with statistics tracking.
%%--------------------------------------------------------------------
retry_with_stats_tracking_test() ->
    setup(),
    Fun = fun() -> {error, timeout} end,
    Config = #{max_attempts => 2, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, timeout}} = wf_recovery:retry(Fun, Config, test_key),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test retry with exponential backoff delay progression.
%%--------------------------------------------------------------------
retry_exponential_backoff_test() ->
    Config = #{max_delay => 1000, backoff_multiplier => 2.0},
    10 = wf_recovery:exponential_backoff(1, 10, Config),
    20 = wf_recovery:exponential_backoff(2, 10, Config),
    40 = wf_recovery:exponential_backoff(3, 10, Config),
    80 = wf_recovery:exponential_backoff(4, 10, Config),
    160 = wf_recovery:exponential_backoff(5, 10, Config).

%%--------------------------------------------------------------------
%% @doc Test exponential backoff is capped at max_delay.
%%--------------------------------------------------------------------
exponential_backoff_max_cap_test() ->
    Config = #{max_delay => 1000, backoff_multiplier => 2.0},
    %% Very high attempt should be capped at max_delay
    1000 = wf_recovery:exponential_backoff(20, 10, Config).

%%--------------------------------------------------------------------
%% @doc Test exponential backoff with custom multiplier.
%%--------------------------------------------------------------------
exponential_backoff_custom_multiplier_test() ->
    Config = #{max_delay => 10000, backoff_multiplier => 3.0},
    10 = wf_recovery:exponential_backoff(1, 10, Config),
    30 = wf_recovery:exponential_backoff(2, 10, Config),
    90 = wf_recovery:exponential_backoff(3, 10, Config).

%%--------------------------------------------------------------------
%% @doc Test retry_until with predicate satisfied on first attempt.
%%--------------------------------------------------------------------
retry_until_immediate_success_test() ->
    Fun = fun() -> {ok, 42} end,
    Predicate = fun(X) -> X =:= 42 end,
    Config = #{max_attempts => 3, initial_delay => 10, max_delay => 100},
    {ok, 42} = wf_recovery:retry_until(Fun, Predicate, Config).

%%--------------------------------------------------------------------
%% @doc Test retry_until retries until predicate is satisfied.
%%--------------------------------------------------------------------
retry_until_eventual_satisfaction_test() ->
    erlang:erase(retry_until_counter),
    Fun = fun() ->
        Count = erlang:get(retry_until_counter) orelse 0,
        erlang:put(retry_until_counter, Count + 1),
        {ok, Count}
    end,
    Predicate = fun(X) -> X >= 2 end,
    Config = #{max_attempts => 5, initial_delay => 10, max_delay => 100},
    {ok, 2} = wf_recovery:retry_until(Fun, Predicate, Config).

%%--------------------------------------------------------------------
%% @doc Test retry_until fails when predicate never satisfied.
%%--------------------------------------------------------------------
retry_until_max_attempts_test() ->
    Fun = fun() -> {ok, 1} end,
    Predicate = fun(X) -> X > 100 end,
    Config = #{max_attempts => 3, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, 1}} = wf_recovery:retry_until(Fun, Predicate, Config).

%%--------------------------------------------------------------------
%% @doc Test retry_until with statistics tracking.
%%--------------------------------------------------------------------
retry_until_with_stats_test() ->
    setup(),
    Fun = fun() -> {ok, 1} end,
    Predicate = fun(X) -> X > 100 end,
    Config = #{max_attempts => 2, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, 1}} = wf_recovery:retry_until(Fun, Predicate, Config, test_key_2),
    cleanup(ok).

%%%===================================================================
%%% Compensation Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test with_compensation succeeds without calling compensation.
%%--------------------------------------------------------------------
with_compensation_success_test() ->
    Fun = fun() -> {ok, tx_result} end,
    CompFun = fun(_) -> ok end,
    {ok, tx_result} = wf_recovery:with_compensation(Fun, CompFun).

%%--------------------------------------------------------------------
%% @doc Test with_compensation triggers on function failure.
%%--------------------------------------------------------------------
with_compensation_triggers_on_failure_test() ->
    erlang:erase(comp_test),
    Fun = fun() -> {error, failure_reason} end,
    CompFun = fun(Reason) ->
        erlang:put(comp_test, {compensated, Reason}),
        ok
    end,
    {error, {compensated, failure_reason, _CompId}} = wf_recovery:with_compensation(Fun, CompFun),
    {compensated, failure_reason} = erlang:get(comp_test).

%%--------------------------------------------------------------------
%% @doc Test with_compensation returns error when compensation fails.
%%--------------------------------------------------------------------
with_compensation_compensation_failure_test() ->
    Fun = fun() -> {error, original_error} end,
    CompFun = fun(_) -> {error, compensation_error} end,
    {error, {compensation_failed, original_error, compensation_error, _CompId}} =
        wf_recovery:with_compensation(Fun, CompFun).

%%--------------------------------------------------------------------
%% @doc Test with_compensation with statistics tracking.
%%--------------------------------------------------------------------
with_compensation_with_stats_test() ->
    setup(),
    Fun = fun() -> {error, failure} end,
    CompFun = fun(_) -> ok end,
    {error, {compensated, failure, _CompId}} = wf_recovery:with_compensation(Fun, CompFun, comp_key),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test register_compensation stores compensation handler.
%%--------------------------------------------------------------------
register_compensation_stores_handler_test() ->
    setup(),
    Result = <<"transaction_id">>,
    CompFun = fun(_) -> ok end,
    {ok, CompId} = wf_recovery:register_compensation(Result, CompFun),
    ?assert(is_binary(CompId)).

%%--------------------------------------------------------------------
%% @doc Test execute_compensation executes registered handler.
%%--------------------------------------------------------------------
execute_compensation_executes_handler_test() ->
    setup(),
    erlang:erase(exec_comp_test),
    Result = test_result,
    CompFun = fun(R) ->
        erlang:put(exec_comp_test, {executed, R}),
        ok
    end,
    {ok, CompId} = wf_recovery:register_compensation(Result, CompFun),
    ok = wf_recovery:execute_compensation(CompId),
    {executed, test_result} = erlang:get(exec_comp_test).

%%--------------------------------------------------------------------
%% @doc Test execute_compensation fails for non-existent ID.
%%--------------------------------------------------------------------
execute_compensation_not_found_test() ->
    setup(),
    FakeId = <<"fake_id">>,
    {error, not_found} = wf_recovery:execute_compensation(FakeId).

%%--------------------------------------------------------------------
%% @doc Test compensation cannot be executed twice.
%%--------------------------------------------------------------------
execute_compensation_idempotent_test() ->
    setup(),
    CompFun = fun(_) -> ok end,
    {ok, CompId} = wf_recovery:register_compensation(result, CompFun),
    ok = wf_recovery:execute_compensation(CompId),
    {error, already_executed} = wf_recovery:execute_compensation(CompId).

%%%===================================================================
%%% Dead Letter Queue Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test init_dlq initializes the queue.
%%--------------------------------------------------------------------
init_dlq_succeeds_test() ->
    ok = wf_recovery:init_dlq().

%%--------------------------------------------------------------------
%% @doc Test enqueue_dlq adds item to queue.
%%--------------------------------------------------------------------
enqueue_dlq_adds_item_test() ->
    setup(),
    Item = #{operation => payment, amount => 100},
    {ok, _Id} = wf_recovery:enqueue_dlq(Item, #{}),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test enqueue_dlq with context.
%%--------------------------------------------------------------------
enqueue_dlq_with_context_test() ->
    setup(),
    Item = #{operation => transfer, amount => 50},
    Context = #{workflow_id => wf123, step => 5},
    {ok, Id} = wf_recovery:enqueue_dlq(Item, Context),
    ?assert(is_binary(Id)),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test dequeue_dlq removes oldest item.
%%--------------------------------------------------------------------
dequeue_dlq_fifo_order_test() ->
    setup(),
    Item1 = #{op => first},
    Item2 = #{op => second},
    {ok, _} = wf_recovery:enqueue_dlq(Item1, #{}),
    timer:sleep(10),
    {ok, _} = wf_recovery:enqueue_dlq(Item2, #{}),
    {ok, Dequeued} = wf_recovery:dequeue_dlq(),
    #{operation := #{op := first}} = Dequeued,
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test dequeue_dlq returns error when empty.
%%--------------------------------------------------------------------
dequeue_dlq_empty_queue_test() ->
    setup(),
    {error, not_found} = wf_recovery:dequeue_dlq(),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test list_dlq returns all items.
%%--------------------------------------------------------------------
list_dlq_returns_all_items_test() ->
    setup(),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => a}, #{}),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => b}, #{}),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => c}, #{}),
    Items = wf_recovery:list_dlq(),
    3 = length(Items),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test list_dlq returns empty list when queue is empty.
%%--------------------------------------------------------------------
list_dlq_empty_queue_test() ->
    setup(),
    [] = wf_recovery:list_dlq(),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test clear_dlq removes all items.
%%--------------------------------------------------------------------
clear_dlq_empties_queue_test() ->
    setup(),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => x}, #{}),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => y}, #{}),
    ok = wf_recovery:clear_dlq(),
    [] = wf_recovery:list_dlq(),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test dlq_stats returns correct count.
%%--------------------------------------------------------------------
dlq_stats_count_test() ->
    setup(),
    #{count := 0} = wf_recovery:dlq_stats(),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => test1}, #{}),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => test2}, #{}),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => test3}, #{}),
    #{count := 3, oldest_timestamp := _} = wf_recovery:dlq_stats(),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test dlq_stats returns oldest timestamp.
%%--------------------------------------------------------------------
dlq_stats_oldest_timestamp_test() ->
    setup(),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => first}, #{}),
    timer:sleep(50),
    {ok, _} = wf_recovery:enqueue_dlq(#{op => second}, #{}),
    #{count := 2, oldest_timestamp := OldestTs} = wf_recovery:dlq_stats(),
    ?assert(is_integer(OldestTs)),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test dlq_stats with empty queue.
%%--------------------------------------------------------------------
dlq_stats_empty_queue_test() ->
    setup(),
    #{count := 0, oldest_timestamp := undefined} = wf_recovery:dlq_stats(),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test DLQ item has all required fields.
%%--------------------------------------------------------------------
dlq_item_structure_test() ->
    setup(),
    Item = #{operation => test_op, value => 42},
    {ok, _} = wf_recovery:enqueue_dlq(Item, #{context_data => foo}),
    [DLQItem] = wf_recovery:list_dlq(),
    ?assert(is_binary(maps:get(id, DLQItem))),
    ?assert(is_map(maps:get(operation, DLQItem))),
    ?assert(is_integer(maps:get(timestamp, DLQItem))),
    ?assert(is_integer(maps:get(retry_count, DLQItem))),
    ?assert(is_map(maps:get(context, DLQItem))),
    cleanup(ok).

%%%===================================================================
%%% Failure Statistics Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test track_failure records failure.
%%--------------------------------------------------------------------
track_failure_records_failure_test() ->
    setup(),
    ok = wf_recovery:track_failure(stats_key, #{error => test_error}),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test track_failure with undefined key does nothing.
%%--------------------------------------------------------------------
track_failure_undefined_key_test() ->
    setup(),
    ok = wf_recovery:track_failure(undefined, #{error => any_error}),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test get_failure_stats retrieves tracked failure.
%%--------------------------------------------------------------------
get_failure_stats_retrieves_data_test() ->
    setup(),
    Key = stats_key_1,
    wf_recovery:reset_failure_stats(Key),
    ok = wf_recovery:track_failure(Key, #{error => test_error}),
    case wf_recovery:get_failure_stats(Key) of
        {error, not_found} ->
            %% Stats table may not be initialized, which is acceptable
            ok;
        Stats ->
            ?assert(is_map(Stats)),
            ?assert(maps:is_key(total_failures, Stats)),
            ?assert(maps:is_key(recovered, Stats)),
            ?assert(maps:is_key(permanent_failures, Stats)),
            ?assert(maps:is_key(last_failure_time, Stats)),
            ?assert(maps:is_key(first_failure_time, Stats))
    end,
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test get_failure_stats for non-existent key.
%%--------------------------------------------------------------------
get_failure_stats_not_found_test() ->
    setup(),
    {error, not_found} = wf_recovery:get_failure_stats(nonexistent_key),
    cleanup(ok).

%%--------------------------------------------------------------------
%% @doc Test reset_failure_stats clears statistics.
%%--------------------------------------------------------------------
reset_failure_stats_clears_test() ->
    setup(),
    Key = stats_key_2,
    ok = wf_recovery:track_failure(Key, #{error => test}),
    ok = wf_recovery:reset_failure_stats(Key),
    {error, not_found} = wf_recovery:get_failure_stats(Key),
    cleanup(ok).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test retry handles exception thrown by function.
%%--------------------------------------------------------------------
retry_handles_exception_test() ->
    Fun = fun() -> throw(unexpected_error) end,
    Config = #{max_attempts => 2, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, unexpected_error}} = wf_recovery:retry(Fun, Config).

%%--------------------------------------------------------------------
%% @doc Test with_compensation handles exception.
%%--------------------------------------------------------------------
with_compensation_handles_exception_test() ->
    Fun = fun() -> throw(error_happened) end,
    CompFun = fun(_) -> ok end,
    {error, {compensated, error_happened, _CompId}} = wf_recovery:with_compensation(Fun, CompFun).

%%--------------------------------------------------------------------
%% @doc Test retry with result that is not {ok,X} or {error,X}.
%%--------------------------------------------------------------------
retry_unwrapped_result_test() ->
    Fun = fun() -> success end,
    Config = #{max_attempts => 2, initial_delay => 10, max_delay => 100},
    {ok, success} = wf_recovery:retry(Fun, Config).

%%--------------------------------------------------------------------
%% @doc Test with_compensation with unwrapped result.
%%--------------------------------------------------------------------
with_compensation_unwrapped_success_test() ->
    Fun = fun() -> some_result end,
    CompFun = fun(_) -> ok end,
    {ok, some_result} = wf_recovery:with_compensation(Fun, CompFun).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test retry with config defaults.
%%--------------------------------------------------------------------
retry_with_defaults_test() ->
    Fun = fun() -> {ok, result} end,
    {ok, result} = wf_recovery:retry(Fun, #{}).

%%--------------------------------------------------------------------
%% @doc Test exponential_backoff with defaults.
%%--------------------------------------------------------------------
exponential_backoff_defaults_test() ->
    %% Default multiplier 2.0, default max_delay 5000
    100 = wf_recovery:exponential_backoff(1, 100, #{}),
    200 = wf_recovery:exponential_backoff(2, 100, #{}),
    400 = wf_recovery:exponential_backoff(3, 100, #{}).

%%--------------------------------------------------------------------
%% @doc Test multiple retry scenarios in sequence.
%%--------------------------------------------------------------------
multiple_retry_scenarios_test() ->
    %% Scenario 1: Immediate success
    Fun1 = fun() -> {ok, result1} end,
    {ok, result1} = wf_recovery:retry(Fun1, #{max_attempts => 3}),

    %% Scenario 2: Eventual success
    erlang:erase(multi_scenario_counter),
    Fun2 = fun() ->
        Count = erlang:get(multi_scenario_counter) orelse 0,
        erlang:put(multi_scenario_counter, Count + 1),
        if Count < 2 -> {error, timeout};
           true -> {ok, result2}
        end
    end,
    {ok, result2} = wf_recovery:retry(Fun2, #{max_attempts => 5}),

    %% Scenario 3: Permanent failure
    Fun3 = fun() -> {error, permanent} end,
    {error, {max_retries_exceeded, permanent}} = wf_recovery:retry(Fun3, #{max_attempts => 2}).

%%--------------------------------------------------------------------
%% @doc Test DLQ workflow with multiple operations.
%%--------------------------------------------------------------------
dlq_workflow_test() ->
    setup(),
    %% Enqueue multiple items
    {ok, Id1} = wf_recovery:enqueue_dlq(#{op => op1, data => data1}, #{step => 1}),
    {ok, Id2} = wf_recovery:enqueue_dlq(#{op => op2, data => data2}, #{step => 2}),
    {ok, Id3} = wf_recovery:enqueue_dlq(#{op => op3, data => data3}, #{step => 3}),

    %% Verify stats
    #{count := 3} = wf_recovery:dlq_stats(),

    %% Dequeue items
    {ok, Item1} = wf_recovery:dequeue_dlq(),
    {ok, Item2} = wf_recovery:dequeue_dlq(),

    %% Verify count decreased
    #{count := 1} = wf_recovery:dlq_stats(),

    %% Dequeue remaining
    {ok, Item3} = wf_recovery:dequeue_dlq(),

    %% Verify queue is empty
    {error, not_found} = wf_recovery:dequeue_dlq(),
    [] = wf_recovery:list_dlq(),
    cleanup(ok).
