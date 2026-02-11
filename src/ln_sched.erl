%%%-------------------------------------------------------------------
%%% @doc ln_sched - Scheduler for deterministic/nondeterministic/replay modes.
%%%
%%% Provides deterministic scheduling for reproducible execution,
%%% nondeterministic with choice logging, and replay from logs.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_sched).

%% API
-export([init/1, init/2]).
-export([choose/2]).
-export([record_choice/3]).
-export([get_log/1, get_mode/1]).
-export([verify_choice/3]).

%% Types
-export_type([mode/0, sched_state/0, choice/0, choice_type/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type mode() :: deterministic | nondeterministic | replay.

-type choice_type() :: xor_selection
                     | defer_race
                     | task_selection
                     | join_order.

-record(choice, {
    type :: choice_type(),
    value :: term(),
    metadata :: term(),
    timestamp :: integer()
}).

-type choice() :: #choice{}.
-type choice_log() :: [choice()].

-record(sched_state, {
    mode :: mode(),
    seed :: undefined | rand:state(),
    choices :: choice_log(),
    position :: non_neg_integer()
}).

-opaque sched_state() :: sched_state().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Initialize scheduler in deterministic or nondeterministic mode.
init(deterministic) ->
    #sched_state{
        mode = deterministic,
        seed = undefined,
        choices = [],
        position = 0
    };
init(nondeterministic) ->
    init(nondeterministic, rand:seed_s(exrop)).

%% @doc Initialize scheduler in nondeterministic mode with seed.
init(nondeterministic, Seed) ->
    #sched_state{
        mode = nondeterministic,
        seed = Seed,
        choices = [],
        position = 0
    };

%% @doc Initialize scheduler in replay mode with choice log.
init(replay, Choices) when is_list(Choices) ->
    #sched_state{
        mode = replay,
        seed = undefined,
        choices = Choices,
        position = 0
    }.

%% @doc Choose an item from a list based on scheduler mode.
choose([], _State) ->
    {error, no_choices};
choose([{_Item, _} = Pair], State) ->
    {Pair, State};
choose(Candidates, #sched_state{mode = deterministic} = State) ->
    %% Deterministic: select first by term ordering
    Sorted = lists:keysort(1, Candidates),
    {Selected, _} = lists:split(1, Sorted),
    {hd(Selected), State};
choose(Candidates, #sched_state{mode = nondeterministic} = State) ->
    %% Nondeterministic: random selection
    Index = rand:uniform(length(Candidates)),
    Selected = lists:nth(Index, Candidates),
    Choice = #choice{
        type = task_selection,
        value = element(1, Selected),
        metadata = #{count => length(Candidates)},
        timestamp = erlang:monotonic_time(millisecond)
    },
    {Selected, State#sched_state{choices = [Choice | State#sched_state.choices]}};
choose(Candidates, #sched_state{mode = replay, choices = Choices, position = Pos} = State) ->
    %% Replay: consume from choice log
    case Pos < length(Choices) of
        true ->
            Choice = lists:nth(Pos + 1, Choices),
            case lists:keyfind(element(1, Choice#choice.value), 1, Candidates) of
                false ->
                    {error, {choice_not_available, Choice#choice.value}};
                Selected ->
                    NewState = State#sched_state{position = Pos + 1},
                    {Selected, NewState}
            end;
        false ->
            {error, choice_log_exhausted}
    end.

%% @doc Record a choice point (for nondeterministic mode).
record_choice(Type, Value, #sched_state{mode = nondeterministic} = State) ->
    Choice = #choice{
        type = Type,
        value = Value,
        metadata = #{},
        timestamp = erlang:monotonic_time(millisecond)
    },
    State#sched_state{choices = [Choice | State#sched_state.choices]};
record_choice(_Type, _Value, State) ->
    State.

%% @doc Get the choice log.
get_log(#sched_state{choices = Choices}) ->
    lists:reverse(Choices).

%% @doc Get the current scheduler mode.
get_mode(#sched_state{mode = Mode}) ->
    Mode.

%% @doc Verify a choice matches expected type and value is available.
verify_choice(ExpectedType, Candidates, #choice{type = Type, value = Value}) ->
    case Type of
        ExpectedType ->
            case lists:member(Value, Candidates) of
                true -> {ok, Value};
                false -> {error, not_available}
            end;
        _ ->
            {error, mismatch}
    end.
