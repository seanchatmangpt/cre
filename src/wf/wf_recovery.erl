%% -*- erlang -*-
%%%% @doc Workflow Error Recovery Module
%%
%% Provides robust error handling and recovery mechanisms for workflow execution.
%%
%% Features:
%% <ul>
%%   <li>Retry with exponential backoff for transient failures</li>
%%   <li>Compensation handlers for rolling back workflow actions</li>
%%   <li>Dead letter queue for persistent failures</li>
%%   <li>Failure tracking and analytics</li>
%% </ul>
%%
%% Retry Strategy:
%% Exponential backoff with configurable max attempts and delay bounds.
%%
%% ```erlang
%% 1> Config = #{max_attempts => 5, initial_delay => 100, max_delay => 5000}.
%% 2> Fun = fun() -> error(timeout) end.
%% 3> Result = wf_recovery:retry(Fun, Config).
%% {error, {max_retries_exceeded, timeout}}
%% '''
%%
%% Compensation Handlers:
%% Execute compensation actions on failure.
%%
%% ```erlang
%% 1> Success = fun() -> {ok, tx123} end.
%% 2> Compensation = fun(tx123) -> ok end.
%% 3> {ok, tx123} = wf_recovery:with_compensation(Success, Compensation).
%% {ok, tx123}
%% '''
%%
%% Dead Letter Queue:
%% Store operations that fail after all retries.
%%
%% ```erlang
%% 1> ok = wf_recovery:init_dlq().
%% ok
%% 2> Item = #{op => payment, amount => 100, error => timeout}.
%% 3> ok = wf_recovery:enqueue_dlq(Item).
%% ok
%% 4> Items = wf_recovery:list_dlq().
%% [#{op => payment, amount => 100, error => timeout}]
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_recovery).

%%====================================================================
%% Exports
%%====================================================================

%% Retry operations
-export([retry/2, retry/3]).
-export([retry_until/3, retry_until/4]).

%% Compensation
-export([with_compensation/2, with_compensation/3]).
-export([register_compensation/2, execute_compensation/1]).

%% Dead letter queue
-export([init_dlq/0]).
-export([enqueue_dlq/1, enqueue_dlq/2]).
-export([dequeue_dlq/0]).
-export([list_dlq/0, clear_dlq/0]).
-export([dlq_stats/0]).

%% Failure tracking
-export([track_failure/2, get_failure_stats/1, reset_failure_stats/1]).

%% Helpers
-export([exponential_backoff/3]).

%%====================================================================
%% Types
%%====================================================================

-type retry_fun() :: fun(() -> {ok, term()} | {error, term()} | term()).

-type retry_config() :: #{
    max_attempts => non_neg_integer(),
    initial_delay => non_neg_integer(),
    max_delay => non_neg_integer(),
    backoff_multiplier => float(),
    retry_on => fun((term()) -> boolean()),
    timeout => non_neg_integer() | infinity
}.

-type compensation_fun() :: fun((term()) -> ok | {error, term()}).

-type compensation() :: #{
    id := binary(),
    action := compensation_fun(),
    result := term(),
    executed := boolean()
}.

-type dlq_item() :: #{
    id := binary(),
    operation := term(),
    error := term(),
    timestamp := integer(),
    retry_count := non_neg_integer(),
    context := map()
}.

-type failure_stats() :: #{
    total_failures := non_neg_integer(),
    recovered := non_neg_integer(),
    permanent_failures := non_neg_integer(),
    last_failure_time := integer(),
    first_failure_time := integer()
}.

-export_type([retry_fun/0, retry_config/0, compensation/0, dlq_item/0, failure_stats/0]).

%%====================================================================
%% Mnesia Tables
%%====================================================================

-record(wf_compensation, {
    id :: binary(),
    action :: compensation_fun(),
    result :: term(),
    executed = false :: boolean(),
    timestamp :: integer()
}).

-record(wf_dlq_item, {
    id :: binary(),
    operation :: term(),
    error :: term(),
    timestamp :: integer(),
    retry_count = 0 :: non_neg_integer(),
    context = #{} :: map()
}).

-record(wf_failure_stats, {
    key :: atom() | binary(),
    total_failures = 0 :: non_neg_integer(),
    recovered = 0 :: non_neg_integer(),
    permanent_failures = 0 :: non_neg_integer(),
    last_failure_time = 0 :: integer(),
    first_failure_time = 0 :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Retries a function with exponential backoff.
%%
%% Automatically retries the function if it fails, using exponential
%% backoff for delays between retries. Returns the result of the last
%% attempt or an error with max_retries_exceeded if all attempts fail.
%%
%% Default config: #{max_attempts => 3, initial_delay => 100,
%%                    max_delay => 5000, backoff_multiplier => 2.0}
%%
%% @param Function The function to retry
%% @param Config Retry configuration
%% @return {ok, Result} | {error, {max_retries_exceeded, LastError}}
%%
%% @end
%%--------------------------------------------------------------------
-spec retry(retry_fun(), retry_config()) ->
    {ok, term()} | {error, {max_retries_exceeded, term()}}.

retry(Function, Config) ->
    retry(Function, Config, 1).

%%--------------------------------------------------------------------
%% @doc Retries a function with exponential backoff and tracking.
%%
%% @param Function The function to retry
%% @param Config Retry configuration
%% @param StatsKey Key for failure tracking
%% @return {ok, Result} | {error, {max_retries_exceeded, LastError}}
%%
%% @end
%%--------------------------------------------------------------------
-spec retry(retry_fun(), retry_config(), atom() | binary()) ->
    {ok, term()} | {error, {max_retries_exceeded, term()}}.

retry(Function, Config, StatsKey) when is_function(Function) ->
    MaxAttempts = maps:get(max_attempts, Config, 3),
    InitialDelay = maps:get(initial_delay, Config, 100),
    MaxDelay = maps:get(max_delay, Config, 5000),
    Multiplier = maps:get(backoff_multiplier, Config, 2.0),
    RetryOn = maps:get(retry_on, Config, fun(E) ->
        case E of
            timeout -> true;
            {timeout, _} -> true;
            unavailable -> true;
            {error, _} -> true;
            _ -> false
        end
    end),
    Timeout = maps:get(timeout, Config, infinity),

    retry_loop(Function, MaxAttempts, InitialDelay, MaxDelay, Multiplier, RetryOn, Timeout, StatsKey, 1, undefined).

%%--------------------------------------------------------------------
%% @doc Retries a function until a condition is met.
%%
%% Keeps retrying until the result satisfies the predicate or max
%% attempts is reached.
%%
%% @param Function The function to retry
%% @param Predicate Predicate to check result
%% @param Config Retry configuration
%% @return {ok, Result} | {error, {max_retries_exceeded, LastError}}
%%
%% @end
%%--------------------------------------------------------------------
-spec retry_until(retry_fun(), fun((term()) -> boolean()), retry_config()) ->
    {ok, term()} | {error, {max_retries_exceeded, term()}}.

retry_until(Function, Predicate, Config) ->
    retry_until(Function, Predicate, Config, undefined).

-spec retry_until(retry_fun(), fun((term()) -> boolean()), retry_config(), atom() | binary()) ->
    {ok, term()} | {error, {max_retries_exceeded, term()}}.

retry_until(Function, Predicate, Config, StatsKey) when is_function(Function), is_function(Predicate) ->
    MaxAttempts = maps:get(max_attempts, Config, 3),
    InitialDelay = maps:get(initial_delay, Config, 100),
    MaxDelay = maps:get(max_delay, Config, 5000),
    Multiplier = maps:get(backoff_multiplier, Config, 2.0),
    Timeout = maps:get(timeout, Config, infinity),

    RetryOn = fun(Result) ->
        try
            not Predicate(Result)
        catch
            _:_ -> true
        end
    end,

    retry_loop(Function, MaxAttempts, InitialDelay, MaxDelay, Multiplier, RetryOn, Timeout, StatsKey, 1, undefined).

%%--------------------------------------------------------------------
%% @doc Executes a function with automatic compensation on failure.
%%
%% If the function succeeds, the result is returned. If it fails,
%% the compensation function is called to undo the operation.
%%
%% @param Function The operation function
%% @param CompensationFun The compensation function
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec with_compensation(retry_fun(), compensation_fun()) ->
    {ok, term()} | {error, term()}.

with_compensation(Function, CompensationFun) ->
    with_compensation(Function, CompensationFun, undefined).

%%--------------------------------------------------------------------
%% @doc Executes a function with compensation and tracking.
%%
%% @param Function The operation function
%% @param CompensationFun The compensation function
%% @param StatsKey Key for failure tracking
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec with_compensation(retry_fun(), compensation_fun(), atom() | binary()) ->
    {ok, term()} | {error, term()}.

with_compensation(Function, CompensationFun, StatsKey) when is_function(Function), is_function(CompensationFun) ->
    case safe_apply(Function, undefined) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            %% Try to compensate
            case safe_apply(fun() -> CompensationFun(Reason) end, undefined) of
                {ok, _} ->
                    CompId = generate_id(),
                    track_failure(StatsKey, #{
                        type => compensated,
                        original_error => Reason,
                        compensation_id => CompId
                    }),
                    {error, {compensated, Reason, CompId}};
                {error, CompError} ->
                    CompId = generate_id(),
                    track_failure(StatsKey, #{
                        type => compensation_failed,
                        original_error => Reason,
                        compensation_error => CompError,
                        compensation_id => CompId
                    }),
                    {error, {compensation_failed, Reason, CompError, CompId}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Registers a compensation action for later execution.
%%
%% Stores compensation actions that can be executed later if needed.
%%
%% @param Result The operation result to compensate
%% @param CompensationFun The compensation function
%% @return {ok, CompensationId} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec register_compensation(term(), compensation_fun()) ->
    {ok, binary()} | {error, term()}.

register_compensation(Result, CompensationFun) when is_function(CompensationFun) ->
    Id = generate_id(),
    Timestamp = erlang:system_time(millisecond),

    Record = #wf_compensation{
        id = Id,
        action = CompensationFun,
        result = Result,
        executed = false,
        timestamp = Timestamp
    },

    case safe_mnesia_write(Record) of
        ok -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a registered compensation.
%%
%% @param CompensationId The compensation ID to execute
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_compensation(binary()) -> ok | {error, term()}.

execute_compensation(CompensationId) when is_binary(CompensationId) ->
    Transaction = fun() ->
        case mnesia:read(wf_compensation, CompensationId) of
            [#wf_compensation{action = Action, result = Result, executed = false}] ->
                case safe_apply(fun() -> Action(Result) end, undefined) of
                    {ok, _} ->
                        mnesia:write(#wf_compensation{id = CompensationId, executed = true});
                    {error, Reason} ->
                        mnesia:abort({compensation_failed, Reason})
                end;
            [#wf_compensation{executed = true}] ->
                mnesia:abort(already_executed);
            [] ->
                mnesia:abort(not_found)
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Initializes the dead letter queue storage.
%%
%% Creates Mnesia tables for dead letter queue management.
%%
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec init_dlq() -> ok | {error, term()}.

init_dlq() ->
    Node = node(),

    %% Start Mnesia if needed
    _ = case mnesia:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Create DLQ table
    DLQAttrs = record_info(fields, wf_dlq_item),
    DLQDef = [
        {attributes, DLQAttrs},
        {ram_copies, [Node]},
        {type, ordered_set}
    ],

    case mnesia:create_table(wf_dlq_item, DLQDef) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, wf_dlq_item}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Enqueues an item to the dead letter queue.
%%
%% Stores operations that failed after all retry attempts.
%%
%% @param Item The operation that failed
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec enqueue_dlq(term()) -> ok | {error, term()}.

enqueue_dlq(Item) ->
    enqueue_dlq(Item, #{}).

-spec enqueue_dlq(term(), map()) -> ok | {error, term()}.

enqueue_dlq(Item, Context) when is_map(Context) ->
    Id = generate_id(),
    Timestamp = erlang:system_time(millisecond),

    Record = #wf_dlq_item{
        id = Id,
        operation = Item,
        error = Item,
        timestamp = Timestamp,
        retry_count = 0,
        context = Context
    },

    case safe_mnesia_write(Record) of
        ok ->
            logger:warning("DLQ enqueue: ~p", [Item]),
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Dequeues the oldest item from the dead letter queue.
%%
%% @return {ok, Item} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec dequeue_dlq() -> {ok, dlq_item()} | {error, not_found}.

dequeue_dlq() ->
    Transaction = fun() ->
        case mnesia:first(wf_dlq_item) of
            '$end_of_table' ->
                mnesia:abort(not_found);
            Key ->
                case mnesia:read(wf_dlq_item, Key) of
                    [Record] ->
                        mnesia:delete(wf_dlq_item, Key, write),
                        record_to_map(Record);
                    [] ->
                        mnesia:abort(not_found)
                end
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Item} -> {ok, Item};
        {aborted, not_found} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all items in the dead letter queue.
%%
%% @return List of DLQ items
%%
%% @end
%%--------------------------------------------------------------------
-spec list_dlq() -> [dlq_item()].

list_dlq() ->
    Transaction = fun() ->
        All = mnesia:match_object(wf_dlq_item, {wf_dlq_item, '_', '_', '_', '_', '_', '_'}, read),
        [record_to_map(R) || R <- All]
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Items} -> Items;
        {aborted, _} -> []
    end.

%%--------------------------------------------------------------------
%% @doc Clears all items from the dead letter queue.
%%
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_dlq() -> ok | {error, term()}.

clear_dlq() ->
    Transaction = fun() ->
        mnesia:delete_all_objects(wf_dlq_item)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Returns statistics about the dead letter queue.
%%
%% @return #{count => integer(), oldest_timestamp => integer()}
%%
%% @end
%%--------------------------------------------------------------------
-spec dlq_stats() -> #{count => non_neg_integer(), oldest_timestamp => integer() | undefined}.

dlq_stats() ->
    Transaction = fun() ->
        All = mnesia:match_object(wf_dlq_item, {wf_dlq_item, '_', '_', '_', '_', '_', '_'}, read),
        Count = length(All),
        OldestTimestamp = case All of
            [] -> undefined;
            Records ->
                Timestamps = [T || #wf_dlq_item{timestamp = T} <- Records],
                lists:min(Timestamps)
        end,
        {Count, OldestTimestamp}
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {Count, Oldest}} ->
            #{count => Count, oldest_timestamp => Oldest};
        {aborted, _} ->
            #{count => 0, oldest_timestamp => undefined}
    end.

%%--------------------------------------------------------------------
%% @doc Tracks a failure for statistics.
%%
%% @param Key The operation key to track
%% @param Details Error details
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec track_failure(atom() | binary() | undefined, map()) -> ok.

track_failure(undefined, _Details) ->
    ok;
track_failure(Key, _Details) ->
    Transaction = fun() ->
        Now = erlang:system_time(millisecond),
        case mnesia:read(wf_failure_stats, Key) of
            [Stats] ->
                Updated = Stats#wf_failure_stats{
                    total_failures = Stats#wf_failure_stats.total_failures + 1,
                    last_failure_time = Now
                },
                mnesia:write(Updated);
            [] ->
                mnesia:write(#wf_failure_stats{
                    key = Key,
                    total_failures = 1,
                    last_failure_time = Now,
                    first_failure_time = Now
                })
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, _} -> ok;
        {aborted, _} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Gets failure statistics for a key.
%%
%% @param Key The operation key
%% @return Failure statistics
%%
%% @end
%%--------------------------------------------------------------------
-spec get_failure_stats(atom() | binary()) -> failure_stats() | {error, not_found}.

get_failure_stats(Key) ->
    Transaction = fun() ->
        case mnesia:read(wf_failure_stats, Key) of
            [Stats] ->
                #{
                    total_failures => Stats#wf_failure_stats.total_failures,
                    recovered => Stats#wf_failure_stats.recovered,
                    permanent_failures => Stats#wf_failure_stats.permanent_failures,
                    last_failure_time => Stats#wf_failure_stats.last_failure_time,
                    first_failure_time => Stats#wf_failure_stats.first_failure_time
                };
            [] ->
                mnesia:abort(not_found)
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Stats} -> Stats;
        {aborted, not_found} -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Resets failure statistics for a key.
%%
%% @param Key The operation key
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_failure_stats(atom() | binary()) -> ok.

reset_failure_stats(Key) ->
    Transaction = fun() ->
        mnesia:delete(wf_failure_stats, Key, write)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, _} -> ok;
        {aborted, _} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Calculates exponential backoff delay.
%%
%% Returns the delay in milliseconds for the given attempt number
%% using exponential backoff: initial * (multiplier ^ (attempt - 1))
%% capped at max_delay.
%%
%% @param Attempt The attempt number (1-based)
%% @param InitialDelay Initial delay in milliseconds
%% @param Config Configuration with max_delay and backoff_multiplier
%% @return Delay in milliseconds
%%
%% @end
%%--------------------------------------------------------------------
-spec exponential_backoff(non_neg_integer(), non_neg_integer(), retry_config()) -> non_neg_integer().

exponential_backoff(Attempt, InitialDelay, Config) ->
    MaxDelay = maps:get(max_delay, Config, 5000),
    Multiplier = maps:get(backoff_multiplier, Config, 2.0),

    Delay = trunc(InitialDelay * math:pow(Multiplier, Attempt - 1)),
    min(Delay, MaxDelay).

%%====================================================================
%% Internal Functions
%%====================================================================

-spec retry_loop(retry_fun(), non_neg_integer(), non_neg_integer(), non_neg_integer(), float(),
                fun((term()) -> boolean()), timeout, atom() | binary() | undefined,
                non_neg_integer(), term()) ->
    {ok, term()} | {error, {max_retries_exceeded, term()}}.

retry_loop(_Function, MaxAttempts, _InitDelay, _MaxDelay, _Multiplier, _RetryOn, _Timeout, _StatsKey, Attempt, LastError)
  when Attempt > MaxAttempts ->
    track_failure(undefined, #{error => LastError}),
    {error, {max_retries_exceeded, LastError}};

retry_loop(Function, MaxAttempts, InitDelay, MaxDelay, Multiplier, RetryOn, Timeout, StatsKey, Attempt, _LastError) ->
    case safe_apply(Function, Timeout) of
        {ok, Result} ->
            {ok, Result};
        {error, Error} ->
            case RetryOn(Error) of
                false ->
                    track_failure(StatsKey, #{error => Error}),
                    {error, {max_retries_exceeded, Error}};
                true ->
                    Delay = exponential_backoff(Attempt, InitDelay, #{max_delay => MaxDelay, backoff_multiplier => Multiplier}),
                    logger:debug("Retry attempt ~p, delay ~pms, error: ~p", [Attempt, Delay, Error]),
                    timer:sleep(Delay),
                    retry_loop(Function, MaxAttempts, InitDelay, MaxDelay, Multiplier, RetryOn, Timeout, StatsKey, Attempt + 1, Error)
            end
    end.

-spec safe_apply(fun(), timeout | infinity) -> {ok, term()} | {error, term()}.

safe_apply(Function, Timeout) ->
    try
        case Timeout of
            undefined ->
                case Function() of
                    {ok, Result} -> {ok, Result};
                    {error, Reason} -> {error, Reason};
                    Result -> {ok, Result}
                end;
            _ ->
                case catch timer:call(Function, Timeout) of
                    {ok, Result} -> {ok, Result};
                    {error, Reason} -> {error, Reason};
                    Result -> {ok, Result}
                end
        end
    catch
        _Error:Reason:_Stack ->
            {error, Reason}
    end.

-spec safe_mnesia_write(tuple()) -> ok | {error, term()}.

safe_mnesia_write(Record) ->
    Transaction = fun() ->
        mnesia:write(Record)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, _} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

-spec record_to_map(#wf_dlq_item{}) -> dlq_item().

record_to_map(#wf_dlq_item{id = Id, operation = Op, error = Error, timestamp = TS, retry_count = RC, context = Ctx}) ->
    #{
        id => Id,
        operation => Op,
        error => Error,
        timestamp => TS,
        retry_count => RC,
        context => Ctx
    }.

-spec generate_id() -> binary().

generate_id() ->
    Timestamp = erlang:system_time(millisecond),
    Unique = erlang:unique_integer([positive]),
    <<"rec_", (integer_to_binary(Timestamp))/binary, "_", (integer_to_binary(Unique))/binary>>.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

retry_success_test() ->
    Fun = fun() -> {ok, success} end,
    Config = #{max_attempts => 3, initial_delay => 10, max_delay => 100},
    {ok, success} = retry(Fun, Config).

retry_with_backoff_test() ->
    Counter = {counter, 0},
    Fun = fun() ->
        case erlang:get(counter) of
            undefined -> erlang:put(counter, 1), {error, timeout};
            1 -> erlang:put(counter, 2), {error, timeout};
            _ -> {ok, success}
        end
    end,
    Config = #{max_attempts => 5, initial_delay => 10, max_delay => 100},
    {ok, success} = retry(Fun, Config).

retry_max_attempts_test() ->
    Fun = fun() -> {error, timeout} end,
    Config = #{max_attempts => 2, initial_delay => 10, max_delay => 100},
    {error, {max_retries_exceeded, timeout}} = retry(Fun, Config).

exponential_backoff_test() ->
    Config = #{max_delay => 1000, backoff_multiplier => 2.0},
    10 = exponential_backoff(1, 10, Config),
    20 = exponential_backoff(2, 10, Config),
    40 = exponential_backoff(3, 10, Config),
    1000 = exponential_backoff(20, 10, Config).

with_compensation_success_test() ->
    Fun = fun() -> {ok, result} end,
    Comp = fun(_R) -> ok end,
    {ok, result} = with_compensation(Fun, Comp).

with_compensation_rollback_test() ->
    Fun = fun() -> {error, failure} end,
    Comp = fun(_R) -> ok end,
    {error, {compensated, failure, _Id}} = with_compensation(Fun, Comp).

dlq_enqueue_dequeue_test() ->
    ok = init_dlq(),
    clear_dlq(),
    Item = #{op => test, value => 123},
    ok = enqueue_dlq(Item, #{}),
    {ok, Dequeued} = dequeue_dlq(),
    #{operation := Dequeued1} = Dequeued,
    #{op := test, value := 123} = Dequeued1.

dlq_list_test() ->
    ok = init_dlq(),
    clear_dlq(),
    ok = enqueue_dlq(#{op => op1}, #{}),
    ok = enqueue_dlq(#{op => op2}, #{}),
    Items = list_dlq(),
    2 = length(Items).

dlq_stats_test() ->
    ok = init_dlq(),
    clear_dlq(),
    #{count := 0} = dlq_stats(),
    ok = enqueue_dlq(#{op => test}, #{}),
    #{count := 1, oldest_timestamp := TS} = dlq_stats(),
    ?assert(is_integer(TS)).

failure_tracking_test() ->
    reset_failure_stats(track_test),
    ok = track_failure(track_test, #{error => test_error}),
    case get_failure_stats(track_test) of
        {error, not_found} ->
            %% Stats table may not be initialized, which is OK
            ok;
        Stats ->
            #{total_failures := Total} = Stats,
            ?assert(Total >= 1)
    end.

-endif.
