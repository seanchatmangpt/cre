%%%-------------------------------------------------------------------
%%% @doc ln_loop - Loop semantics for workflow patterns.
%%%
%%% Supports while, until, and fixed-count loops with timeout
%%% and retry policies.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_loop).

%% API
-export([init_while/2, init_until/2, init_times/2]).
-export([check_continue/2]).
-export([record_iteration/1, record_iteration/2]).
-export([check_timeout/1]).
-export([can_retry/1, calculate_backoff/1, record_retry/1]).
-export([get_iteration/1, get_id/1]).

%% Types
-export_type([state/0, variant/0, loop_policy/0, break_reason/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type loop_id() :: reference().
-type iteration() :: non_neg_integer().

-type variant() :: while | until | times.

-type condition() :: fun((map()) -> boolean()).

-type retry_policy() :: #{
    max := pos_integer(),
    backoff_ms := pos_integer(),
    jitter := boolean()
}.

-type loop_policy() :: #{
    timeout_ms := pos_integer() | infinity,
    max_iterations := pos_integer() | infinity,
    retry := retry_policy() | undefined
}.

-record(loop_state, {
    id :: loop_id(),
    variant :: variant(),
    condition :: condition() | undefined,
    target_count :: iteration() | undefined,
    iteration :: iteration(),
    started_at :: integer(),
    last_check_at :: integer(),
    policy :: loop_policy(),
    attempt_count :: non_neg_integer(),
    history :: [map()]
}).

-opaque state() :: #loop_state{}.

-type break_reason() ::
    {condition_false, state()}
    | {condition_true, state()}
    | {timeout, iteration(), pos_integer()}
    | {max_iterations_exceeded, iteration()}.

-type continue_result() :: {continue, state()} | {break, break_reason()}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Initialize a while loop.
-spec init_while(condition(), loop_policy()) -> state().
init_while(Condition, Policy) ->
    #loop_state{
        id = make_ref(),
        variant = while,
        condition = Condition,
        target_count = undefined,
        iteration = 0,
        started_at = erlang:monotonic_time(millisecond),
        last_check_at = erlang:monotonic_time(millisecond),
        policy = normalize_policy(Policy),
        attempt_count = 0,
        history = []
    }.

%% @doc Initialize an until loop.
-spec init_until(condition(), loop_policy()) -> state().
init_until(Condition, Policy) ->
    #loop_state{
        id = make_ref(),
        variant = until,
        condition = Condition,
        target_count = undefined,
        iteration = 0,
        started_at = erlang:monotonic_time(millisecond),
        last_check_at = erlang:monotonic_time(millisecond),
        policy = normalize_policy(Policy),
        attempt_count = 0,
        history = []
    }.

%% @doc Initialize a fixed-count loop (times).
-spec init_times(pos_integer(), loop_policy()) -> state().
init_times(TargetCount, Policy) ->
    #loop_state{
        id = make_ref(),
        variant = times,
        condition = undefined,
        target_count = TargetCount,
        iteration = 0,
        started_at = erlang:monotonic_time(millisecond),
        last_check_at = erlang:monotonic_time(millisecond),
        policy = normalize_policy(Policy),
        attempt_count = 0,
        history = []
    }.

%% @doc Check if loop should continue.
-spec check_continue(state(), map()) -> continue_result().
check_continue(#loop_state{variant = while, condition = Condition, iteration = Iter,
                            policy = #{max_iterations := MaxIter}} = Loop, Ctx) ->
    case Iter >= MaxIter of
        true ->
            {break, {max_iterations_exceeded, Iter}};
        false ->
            case eval_condition(Condition, Ctx) of
                {ok, true} ->
                    {continue, Loop#loop_state{iteration = Iter + 1, last_check_at = erlang:monotonic_time(millisecond)}};
                {ok, false} ->
                    {break, {condition_false, Loop}};
                {error, _} ->
                    {break, {condition_false, Loop}}
            end
    end;
check_continue(#loop_state{variant = until, condition = Condition, iteration = Iter,
                           policy = #{max_iterations := MaxIter}} = Loop, Ctx) ->
    case Iter >= MaxIter of
        true ->
            {break, {max_iterations_exceeded, Iter}};
        false ->
            case eval_condition(Condition, Ctx) of
                {ok, true} ->
                    {break, {condition_true, Loop}};
                {ok, false} ->
                    {continue, Loop#loop_state{iteration = Iter + 1, last_check_at = erlang:monotonic_time(millisecond)}};
                {error, _} ->
                    {continue, Loop#loop_state{iteration = Iter + 1, last_check_at = erlang:monotonic_time(millisecond)}}
            end
    end;
check_continue(#loop_state{variant = times, target_count = Target, iteration = Iter,
                           policy = #{max_iterations := MaxIter}} = Loop, _Ctx) ->
    case Iter >= MaxIter of
        true ->
            {break, {max_iterations_exceeded, Iter}};
        false when Iter >= Target ->
            {break, {condition_false, Loop}};
        false ->
            {continue, Loop#loop_state{iteration = Iter + 1, last_check_at = erlang:monotonic_time(millisecond)}}
    end.

%% @doc Record an iteration.
-spec record_iteration(state()) -> state().
record_iteration(#loop_state{iteration = Iter} = Loop) ->
    Loop#loop_state{iteration = Iter + 1}.

%% @doc Record an iteration with context snapshot.
-spec record_iteration(state(), map()) -> state().
record_iteration(#loop_state{iteration = Iter, history = History} = Loop, Ctx) ->
    Loop#loop_state{
        iteration = Iter + 1,
        history = [#{iteration => Iter, context => Ctx, timestamp => erlang:monotonic_time(millisecond)} | History]
    }.

%% @doc Check if timeout has been exceeded.
-spec check_timeout(state()) -> ok | {timeout, iteration(), pos_integer()}.
check_timeout(#loop_state{started_at = Start, iteration = Iter, policy = #{timeout_ms := Timeout}}) ->
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    case Elapsed >= Timeout of
        true -> {timeout, Iter, Elapsed};
        false -> ok
    end;
check_timeout(_) ->
    ok.

%% @doc Check if retry is available.
-spec can_retry(state()) -> boolean().
can_retry(#loop_state{policy = #{retry := #{max := Max}}, attempt_count = Count}) ->
    Count < Max;
can_retry(_) ->
    false.

%% @doc Calculate backoff delay with optional jitter.
-spec calculate_backoff(state()) -> pos_integer().
calculate_backoff(#loop_state{policy = #{retry := #{backoff_ms := Base, jitter := true}}, attempt_count = Count}) ->
    %% Exponential backoff with jitter
    Backoff = Base * (1 bsl Count),
    Jitter = rand:uniform(Backoff div 4),
    Backoff + Jitter;
calculate_backoff(#loop_state{policy = #{retry := #{backoff_ms := Base}}, attempt_count = Count}) ->
    %% Exponential backoff
    Base * (1 bsl Count);
calculate_backoff(_) ->
    1000.

%% @doc Record a retry attempt.
-spec record_retry(state()) -> state().
record_retry(#loop_state{attempt_count = Count} = Loop) ->
    Loop#loop_state{attempt_count = Count + 1}.

%% @doc Get current iteration count.
-spec get_iteration(state()) -> iteration().
get_iteration(#loop_state{iteration = Iter}) ->
    Iter.

%% @doc Get loop ID.
-spec get_id(state()) -> loop_id().
get_id(#loop_state{id = Id}) ->
    Id.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Normalize policy with defaults.
normalize_policy(Policy) ->
    Default = #{
        timeout_ms => infinity,
        max_iterations => infinity,
        retry => undefined
    },
    maps:merge(Default, Policy).

%% @doc Evaluate condition function safely.
eval_condition(Condition, Ctx) when is_function(Condition, 1) ->
    try
        {ok, Condition(Ctx)}
    catch
        _:Error ->
            {error, Error}
    end;
eval_condition(_, _) ->
    {error, bad_condition}.
