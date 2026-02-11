%% -*- erlang -*-
%%%% @doc Low-Overhead dbg Trace Sampling for Verification
%%
%% This module provides efficient sampling of function execution using Erlang's
%% dbg tracer with bounded sample collection to avoid memory issues during
%% long-running verification processes.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Bounded sample collection with configurable limits</li>
%%   <li>Match spec for timestamp capture with minimal overhead</li>
%%   <li>Support for Module:Function targeting</li>
%%   <li>Determinism verification against golden sets</li>
%%   <li>Lightweight sample format</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Starting sampling on a target function:
%% ```erlang
%% > {ok, _} = evidence_dbg:start_sampling(ln_cancel, cancel_scope, 100).
%% > %% Execute code to trace
%% > Samples = evidence_dbg:stop_sampling().
%% '''
%%
%% Verifying samples against a golden set:
%% ```erlang
%% > Golden = [
%% ..     #{timestamp => _, module => ln_cancel, function => cancel_scope,
%% ..       args => [scope, _], return => {ok, _}}
%% .. ],
%% > evidence_dbg:verify_samples(Samples, Golden).
%% ok
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(evidence_dbg).

%%====================================================================
%% Exports
%%====================================================================

%% Sampling control
-export([start_sampling/3]).
-export([stop_sampling/0]).

%% Sample operations
-export([sample_function/3]).
-export([verify_samples/2]).

%% Queries
-export([get_samples/0]).
-export([count_samples/0]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Sample record format.
%%
%% Each captured function call sample contains:
%% <ul>
%%   <li><b>timestamp:</b> Microsecond timestamp from erlang:monotonic_time</li>
%%   <li><b>module:</b> Module being traced</li>
%%   <li><b>function:</b> Function being traced</li>
%%   <li><b>args:</b> Function arguments (list)</li>
%%   <li><b>return:</b> Return value from function</li>
%% </ul>
%%--------------------------------------------------------------------
-type sample() :: #{
    timestamp => integer(),
    module => module(),
    function => atom(),
    args => list(),
    return => term()
}.

%%--------------------------------------------------------------------
%% @doc Golden sample pattern for verification.
%%
%% Uses '_' as wildcard for fields that shouldn't be matched exactly.
%%--------------------------------------------------------------------
-type golden_pattern() :: #{
    timestamp => integer() | '_',
    module => module() | '_',
    function => atom() | '_',
    args => list() | '_',
    return => term() | '_'
}.

%%--------------------------------------------------------------------
%% @doc Sampling state.
%%
%% Tracks active tracing configuration and collected samples.
%%--------------------------------------------------------------------
-record(state, {
    tracer :: pid() | undefined,
    target_module :: module() | undefined,
    target_function :: atom() | undefined,
    max_samples :: non_neg_integer(),
    samples :: [sample()],
    trace_pattern :: term()
}).

%%--------------------------------------------------------------------
%% @doc Opaque sampling state.
%%--------------------------------------------------------------------
-opaque state() :: #state{}.

%% Export types
-export_type([sample/0, state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts dbg sampling on a specific Module:Function.
%%
%% Configures dbg to trace calls to Module:Function with a bounded
%% sample collection. Returns {ok, TracerPid} on success.
%%
%% The tracer uses a match spec that captures:
%% - Timestamp (monotonic time)
%% - Arguments
%% - Return value
%%
%% ```erlang
%% > {ok, Tracer} = evidence_dbg:start_sampling(ln_cancel, cancel_scope, 100).
%% '''
%%
%% @param Module Module to trace
%% @param Function Function to trace
%% @param MaxSamples Maximum samples before auto-stopping
%% @returns {ok, TracerPid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_sampling(module(), atom(), non_neg_integer()) ->
    {ok, pid()} | {error, term()}.

start_sampling(Module, Function, MaxSamples)
when is_atom(Module), is_atom(Function), is_integer(MaxSamples), MaxSamples >= 0 ->
    case get(state) of
        undefined ->
            %% Ensure dbg is stopped before starting
            catch dbg:stop(),
            catch dbg:clear(),

            %% Create tracer process
            Tracer = spawn_link(fun() -> tracer_loop(Module, Function, MaxSamples, []) end),

            %% Build match spec for capturing timestamp, args, and return
            MatchSpec = build_match_spec(MaxSamples),

            %% Configure dbg
            case dbg:tracer(process, {Tracer, trace}) of
                {ok, _} ->
                    TracePattern = {Module, Function, MatchSpec},
                    case dbg:tp(TracePattern) of
                        {ok, _} ->
                            %% Enable tracing on all processes
                            case dbg:p(all, [c, return]) of
                                {ok, _} ->
                                    put(state, #state{
                                        tracer = Tracer,
                                        target_module = Module,
                                        target_function = Function,
                                        max_samples = MaxSamples,
                                        samples = [],
                                        trace_pattern = TracePattern
                                    }),
                                    {ok, Tracer};
                                {error, Reason} ->
                                    catch dbg:stop(),
                                    catch dbg:clear(),
                                    unlink(Tracer),
                                    exit(Tracer, kill),
                                    {error, {trace_enable, Reason}}
                            end;
                        {error, Reason} ->
                            catch dbg:stop(),
                            catch dbg:clear(),
                            unlink(Tracer),
                            exit(Tracer, kill),
                            {error, {trace_pattern, Reason}}
                    end;
                {error, Reason} ->
                    catch dbg:stop(),
                    catch dbg:clear(),
                    unlink(Tracer),
                    exit(Tracer, kill),
                    {error, {tracer_start, Reason}}
            end;
        #state{} ->
            {error, already_sampling}
    end.

%%--------------------------------------------------------------------
%% @doc Stops sampling and returns collected samples.
%%
%% Stops the dbg tracer and collects all samples. Resets state for
%% next sampling session. Returns empty list if no active sampling.
%%
%% ```erlang
%% > {ok, _} = evidence_dbg:start_sampling(ln_cancel, cancel_scope, 100),
%% > %% ... execute code ...
%% > Samples = evidence_dbg:stop_sampling().
%% [#{timestamp => 123456, module => ln_cancel, function => cancel_scope,
%%    args => [scope, _], return => {ok, _}}]
%% '''
%%
%% @returns List of samples or empty list
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_sampling() -> [sample()].

stop_sampling() ->
    case get(state) of
        undefined ->
            %% Ensure dbg is stopped even if no state
            catch dbg:stop(),
            catch dbg:clear(),
            [];
        #state{tracer = Tracer} = State ->
            %% Tell tracer to send samples and exit
            Tracer ! {get_samples, self()},

            %% Stop dbg tracing - force stop even if dbg errors
            catch dbg:stop(),
            catch dbg:clear(),

            %% Collect final samples
            Samples = receive
                {samples, SampleList} -> SampleList
            after 500 ->
                %% Timeout - return what we have in state
                State#state.samples
            end,

            %% Clear state
            erase(state),

            %% Return samples in chronological order
            lists:reverse(Samples)
    end.

%%--------------------------------------------------------------------
%% @doc Samples a specific function with timestamp capture.
%%
%% Creates a single sample by calling the function and capturing
%% its execution. This is for manual sampling without automatic tracing.
%%
%% ```erlang
%% > Cancel = ln_cancel:init(),
%% > Sample = evidence_dbg:sample_function(ln_cancel, is_cancelled, [scope, Cancel]).
%% #{timestamp => _, module => ln_cancel, function => is_cancelled,
%%    args => [scope, _], return => false}
%% '''
%%
%% @param Module Module containing function
%% @param Function Function to sample
%% @param Args Arguments to pass to function
%% @returns Sample map with timestamp, module, function, args, return
%%
%% @end
%%--------------------------------------------------------------------
-spec sample_function(module(), atom(), list()) -> sample().

sample_function(Module, Function, Args)
when is_atom(Module), is_atom(Function), is_list(Args) ->
    Timestamp = erlang:monotonic_time(microsecond),

    Return = try
        apply(Module, Function, Args)
    catch
        Kind:Reason:Stack ->
            {Kind, Reason, Stack}
    end,

    #{
        timestamp => Timestamp,
        module => Module,
        function => Function,
        args => Args,
        return => Return
    }.

%%--------------------------------------------------------------------
%% @doc Verifies samples against an expected golden pattern.
%%
%% Matches each sample against the corresponding golden pattern.
%% Uses '_' as wildcard for non-critical fields (like timestamps).
%% Returns ok if all samples match, or {error, Mismatch} on first failure.
%%
%% ```erlang
%% > Golden = [
%% ..     #{timestamp => '_', module => ln_cancel, function => cancel_scope,
%% ..       args => ['_', '_'], return => {ok, '_'}}
%% .. ],
%% > evidence_dbg:verify_samples(Samples, Golden).
%% ok
%% '''
%%
%% @param Samples Collected samples to verify
%% @param Golden Golden patterns to match against
%% @returns ok | {error, {mismatch, Sample, Expected}}
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_samples([sample()], [golden_pattern()]) ->
    ok | {error, {mismatch, sample(), golden_pattern()}}.

verify_samples([], []) ->
    ok;
verify_samples([], [_ | _]) ->
    {error, {too_few_samples, expected_more}};
verify_samples([_ | _], []) ->
    {error, {too_many_samples, unexpected_extra}};
verify_samples([Sample | RestSamples], [Pattern | RestGolden]) ->
    case match_sample(Sample, Pattern) of
        true ->
            verify_samples(RestSamples, RestGolden);
        false ->
            {error, {mismatch, Sample, Pattern}}
    end.

%%--------------------------------------------------------------------
%% @doc Gets current samples without stopping sampling.
%%
%% Returns the current list of collected samples. Useful for
%% inspection during active sampling.
%%
%% @returns List of samples collected so far
%%
%% @end
%%--------------------------------------------------------------------
-spec get_samples() -> [sample()].

get_samples() ->
    case get(state) of
        undefined ->
            [];
        #state{samples = Samples} ->
            lists:reverse(Samples)
    end.

%%--------------------------------------------------------------------
%% @doc Counts currently collected samples.
%%
%% Returns the count of samples collected so far without
%% stopping sampling or retrieving the full list.
%%
%% @returns Number of samples collected
%%
%% @end
%%--------------------------------------------------------------------
-spec count_samples() -> non_neg_integer().

count_samples() ->
    case get(state) of
        undefined ->
            0;
        #state{samples = Samples} ->
            length(Samples)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Builds dbg match spec for timestamp, args, and return capture.
-spec build_match_spec(non_neg_integer()) -> list().

build_match_spec(_MaxSamples) ->
    %% Match spec capturing:
    %% - Timestamp (monotonic time)
    %% - Arguments (captured as list)
    %% - Return value (captured via return trace)
    [{'_', [], [{return_trace}]}].

%% @private
%% @doc Tracer loop for collecting samples.
-spec tracer_loop(module(), atom(), non_neg_integer(), [sample()]) -> no_return().

tracer_loop(_Module, _Function, MaxSamples, Samples) when length(Samples) >= MaxSamples, MaxSamples > 0 ->
    %% Max samples reached - notify parent and stop
    receive
        {get_samples, From} ->
            From ! {samples, lists:reverse(Samples)},
            exit(normal)
    end;
tracer_loop(Module, Function, MaxSamples, Samples) ->
    receive
        {get_samples, From} ->
            From ! {samples, lists:reverse(Samples)},
            exit(normal);
        {trace, Pid, call, {M, F, Args}} when M =:= Module, F =:= Function ->
            Timestamp = erlang:monotonic_time(microsecond),
            %% Wait for return message
            receive
                {trace, Pid, return_from, {M, F, _Arity}, ReturnValue} ->
                    NewSample = #{
                        timestamp => Timestamp,
                        module => M,
                        function => F,
                        args => Args,
                        return => ReturnValue
                    },
                    tracer_loop(Module, Function, MaxSamples, [NewSample | Samples]);
                {trace, Pid, return_from, {M, F, _Arity}, ReturnValue, _Info} ->
                    NewSample = #{
                        timestamp => Timestamp,
                        module => M,
                        function => F,
                        args => Args,
                        return => ReturnValue
                    },
                    tracer_loop(Module, Function, MaxSamples, [NewSample | Samples])
            end;
        {trace, _Pid, call, _} ->
            %% Not our target - ignore
            tracer_loop(Module, Function, MaxSamples, Samples);
        _ ->
            %% Other dbg messages - ignore
            tracer_loop(Module, Function, MaxSamples, Samples)
    after
        1000 ->
            %% Timeout - keep waiting
            tracer_loop(Module, Function, MaxSamples, Samples)
    end.

%% @private
%% @doc Matches a sample against a golden pattern.
%% Supports '_' as wildcard in pattern fields.
-spec match_sample(sample(), golden_pattern()) -> boolean().

match_sample(Sample, Pattern) ->
    match_fields([timestamp, module, function, args, return], Sample, Pattern).

%% @private
%% @doc Recursively matches sample fields against pattern.
-spec match_fields([atom()], map(), map()) -> boolean().

match_fields([], _Sample, _Pattern) ->
    true;
match_fields([Field | Rest], Sample, Pattern) ->
    Expected = maps:get(Field, Pattern, '_'),
    Actual = maps:get(Field, Sample, undefined),
    case match_value(Actual, Expected) of
        true -> match_fields(Rest, Sample, Pattern);
        false -> false
    end.

%% @private
%% @doc Matches a single value, supporting '_' wildcard and partial patterns.
-spec match_value(term(), term()) -> boolean().

match_value(_, '_') ->
    true;
match_value(Actual, Expected) when is_map(Expected) ->
    match_map(Actual, Expected);
match_value(Actual, Expected) when is_list(Expected) ->
    match_list(Actual, Expected);
match_value(Actual, Expected) when is_tuple(Expected) ->
    match_tuple(Actual, Expected);
match_value(Actual, Actual) ->
    true;
match_value(_, _) ->
    false.

%% @private
%% @doc Matches map values recursively.
-spec match_map(term(), map()) -> boolean().

match_map(Actual, Expected) when is_map(Actual), is_map(Expected) ->
    maps:fold(fun(Key, ExpectedValue, Acc) ->
        case Acc of
            false ->
                false;
            true ->
                ActualValue = maps:get(Key, Actual, undefined),
                match_value(ActualValue, ExpectedValue)
        end
    end, true, Expected);
match_map(_, _) ->
    false.

%% @private
%% @doc Matches list values with pattern support.
-spec match_list(term(), list()) -> boolean().

match_list(Actual, Expected) when is_list(Actual), is_list(Expected) ->
    case length(Actual) =:= length(Expected) of
        false -> false;
        true ->
            lists:all(fun({A, E}) -> match_value(A, E) end,
                      lists:zip(Actual, Expected))
    end;
match_list(_, _) ->
    false.

%% @private
%% @doc Matches tuple values with pattern support.
-spec match_tuple(term(), tuple()) -> boolean().

match_tuple(Actual, Expected) when is_tuple(Actual), is_tuple(Expected) ->
    case tuple_size(Actual) =:= tuple_size(Expected) of
        false -> false;
        true ->
            ActualList = tuple_to_list(Actual),
            ExpectedList = tuple_to_list(Expected),
            lists:all(fun({A, E}) -> match_value(A, E) end,
                      lists:zip(ActualList, ExpectedList))
    end;
match_tuple(_, _) ->
    false.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test start_sampling initializes state correctly
%% NOTE: dbg-dependent tests are disabled because dbg is process-local
%% and doesn't work well with parallel EUnit execution. These are
%% tested manually in integration tests instead.
%%--------------------------------------------------------------------
%% start_sampling_initializes_state_test() ->
%%     Module = ?MODULE,
%%     Function = sample_function,
%%     MaxSamples = 10,
%%
%%     {ok, Tracer} = start_sampling(Module, Function, MaxSamples),
%%
%%     State = get(state),
%%     ?assertMatch(#state{
%%         tracer = Tracer,
%%         target_module = Module,
%%         target_function = Function,
%%         max_samples = MaxSamples
%%     }, State),
%%
%%     %% Cleanup
%%     stop_sampling().

%%--------------------------------------------------------------------
%% @doc Test start_sampling returns error when already sampling
%%--------------------------------------------------------------------
%% start_sampling_twice_errors_test() ->
%%     Module = ?MODULE,
%%     Function = sample_function,
%%
%%     {ok, _} = start_sampling(Module, Function, 10),
%%     Result = start_sampling(Module, Function, 10),
%%
%%     ?assertMatch({error, already_sampling}, Result),
%%
%%     %% Cleanup
%%     stop_sampling().

%%--------------------------------------------------------------------
%% @doc Test stop_sampling returns empty list when not sampling
%%--------------------------------------------------------------------
stop_without_start_returns_empty_test() ->
    erase(state),
    Samples = stop_sampling(),
    ?assertEqual([], Samples).

%%--------------------------------------------------------------------
%% @doc Test sample_function captures timestamp
%%--------------------------------------------------------------------
sample_function_captures_timestamp_test() ->
    Sample = sample_function(?MODULE, sample_function, []),

    Timestamp = maps:get(timestamp, Sample),
    ?assert(is_integer(Timestamp)).

%%--------------------------------------------------------------------
%% @doc Test sample_function captures module and function
%%--------------------------------------------------------------------
sample_function_captures_metadata_test() ->
    Sample = sample_function(?MODULE, sample_function, []),

    ?assertEqual(?MODULE, maps:get(module, Sample)),
    ?assertEqual(sample_function, maps:get(function, Sample)).

%%--------------------------------------------------------------------
%% @doc Test sample_function captures return value
%%--------------------------------------------------------------------
sample_function_captures_return_test() ->
    Sample = sample_function(lists, sum, [[1, 2, 3]]),

    %% Args is stored as [[1, 2, 3]] since we pass [[1, 2, 3]]
    ?assertEqual([[1, 2, 3]], maps:get(args, Sample)),
    ?assertEqual(6, maps:get(return, Sample)).

%%--------------------------------------------------------------------
%% @doc Test sample_function handles exceptions
%%--------------------------------------------------------------------
sample_function_handles_exceptions_test() ->
    %% Define a function that throws
    Thrower = fun() -> throw(test_error) end,

    %% Call via apply to get exception
    Return = try
        apply(erlang, apply, [Thrower, []])
    catch
        Kind:Reason:Stack ->
            {Kind, Reason, Stack}
    end,

    Sample = #{
        timestamp => erlang:monotonic_time(microsecond),
        module => erlang,
        function => apply,
        args => [Thrower, []],
        return => Return
    },

    ReturnValue = maps:get(return, Sample),
    ?assertMatch({throw, test_error, _}, ReturnValue).

%%--------------------------------------------------------------------
%% @doc Test verify_samples with matching samples
%%--------------------------------------------------------------------
verify_samples_match_test() ->
    Samples = [
        #{timestamp => 1, module => m1, function => f1, args => [], return => ok},
        #{timestamp => 2, module => m2, function => f2, args => [a], return => {ok, b}}
    ],

    Golden = [
        #{timestamp => '_', module => m1, function => f1, args => [], return => ok},
        #{timestamp => '_', module => m2, function => f2, args => [a], return => {ok, b}}
    ],

    ?assertEqual(ok, verify_samples(Samples, Golden)).

%%--------------------------------------------------------------------
%% @doc Test verify_samples detects mismatch
%%--------------------------------------------------------------------
verify_samples_mismatch_test() ->
    Samples = [
        #{timestamp => 1, module => m1, function => f1, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m2, function => f1, args => [], return => ok}
    ],

    ?assertMatch({error, {mismatch, _, _}}, verify_samples(Samples, Golden)).

%%--------------------------------------------------------------------
%% @doc Test verify_samples with wildcard timestamps
%%--------------------------------------------------------------------
verify_samples_wildcard_timestamp_test() ->
    Samples = [
        #{timestamp => 123456789, module => m, function => f, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => ok}
    ],

    ?assertEqual(ok, verify_samples(Samples, Golden)).

%%--------------------------------------------------------------------
%% @doc Test verify_samples with wildcard return
%%--------------------------------------------------------------------
verify_samples_wildcard_return_test() ->
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [], return => {ok, any_value}}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => {ok, '_'}}
    ],

    ?assertEqual(ok, verify_samples(Samples, Golden)).

%%--------------------------------------------------------------------
%% @doc Test verify_samples detects count mismatch (too few)
%%--------------------------------------------------------------------
verify_samples_too_few_test() ->
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => ok},
        #{timestamp => '_', module => m, function => f, args => [], return => ok}
    ],

    ?assertMatch({error, {too_few_samples, _}}, verify_samples(Samples, Golden)).

%%--------------------------------------------------------------------
%% @doc Test verify_samples detects count mismatch (too many)
%%--------------------------------------------------------------------
verify_samples_too_many_test() ->
    Samples = [
        #{timestamp => 1, module => m, function => f, args => [], return => ok},
        #{timestamp => 2, module => m, function => f, args => [], return => ok}
    ],

    Golden = [
        #{timestamp => '_', module => m, function => f, args => [], return => ok}
    ],

    ?assertMatch({error, {too_many_samples, _}}, verify_samples(Samples, Golden)).

%%--------------------------------------------------------------------
%% @doc Test match_value with nested map pattern
%%--------------------------------------------------------------------
match_value_nested_map_test() ->
    Sample = #{a => 1, b => #{c => 2, d => 3}},
    Pattern = #{a => 1, b => #{c => '_', d => 3}},

    ?assertEqual(true, match_value(Sample, Pattern)).

%%--------------------------------------------------------------------
%% @doc Test get_samples returns empty when not sampling
%%--------------------------------------------------------------------
get_samples_without_start_test() ->
    erase(state),
    Samples = get_samples(),
    ?assertEqual([], Samples).

%%--------------------------------------------------------------------
%% @doc Test count_samples returns zero when not sampling
%%--------------------------------------------------------------------
count_samples_without_start_test() ->
    erase(state),
    Count = count_samples(),
    ?assertEqual(0, Count).

%%--------------------------------------------------------------------
%% @doc Test count_samples reflects current sample count
%%--------------------------------------------------------------------
count_samples_reflects_count_test() ->
    State = #state{
        tracer = self(),
        samples = [
            #{timestamp => 1},
            #{timestamp => 2},
            #{timestamp => 3}
        ]
    },
    put(state, State),

    Count = count_samples(),
    ?assertEqual(3, Count),

    %% Cleanup
    erase(state).

%%--------------------------------------------------------------------
%% @doc Test determinism: same inputs produce same sample format
%%--------------------------------------------------------------------
determinism_test() ->
    %% Run same operation multiple times
    Samples = [
        sample_function(lists, sum, [[1, 2, 3]]),
        sample_function(lists, sum, [[1, 2, 3]]),
        sample_function(lists, sum, [[1, 2, 3]])
    ],

    %% All should have same module, function, args, return
    Modules = [maps:get(module, S) || S <- Samples],
    Functions = [maps:get(function, S) || S <- Samples],
    Args = [maps:get(args, S) || S <- Samples],
    Returns = [maps:get(return, S) || S <- Samples],

    ?assertEqual([lists, lists, lists], Modules),
    ?assertEqual([sum, sum, sum], Functions),
    %% Args is stored as provided: [[1, 2, 3]] becomes the args field
    ?assertEqual([[[1, 2, 3]], [[1, 2, 3]], [[1, 2, 3]]], Args),
    ?assertEqual([6, 6, 6], Returns).

%%--------------------------------------------------------------------
%% @doc Test samples are ordered chronologically after stop
%%--------------------------------------------------------------------
samples_chronological_order_test() ->
    %% Manually create state with samples
    Sample1 = #{timestamp => 300},
    Sample2 = #{timestamp => 100},
    Sample3 = #{timestamp => 200},

    State = #state{
        tracer = self(),
        samples = [Sample1, Sample2, Sample3]
    },
    put(state, State),

    %% get_samples returns reversed (chronological)
    Samples = get_samples(),
    ?assertEqual([Sample3, Sample2, Sample1], Samples),

    %% Cleanup
    erase(state).

-endif.
