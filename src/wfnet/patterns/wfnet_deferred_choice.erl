%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @author CRE Team
%% @version 0.3.0
%% @doc Deferred Choice Workflow Pattern (WCP-16)
%%
%% Implements the Deferred Choice pattern where the exact branch to
%% execute is not determined at workflow design time, but is instead
%% decided at runtime based on which branch becomes available first.
%% Unlike exclusive choice, the decision is deferred until execution.
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Entry point to the deferred choice
%%   - defer: Place where deferral occurs (waiting for first trigger)
%%   - option1, option2, ..., optionN: Alternative option places
%%   - 'end': Final place after chosen option completes
%%
%% Transitions:
%%   - trigger: The first option to trigger wins
%%   - complete: Completes the chosen option
%%
%% Flow: start -> defer -> (first option triggered) -> complete -> end
%%
%% <h3>Deferred Choice Semantics</h3>
%%
%% - Multiple options are made available simultaneously
%% - The FIRST option to become active/triggered is chosen
%% - Once chosen, other options are discarded
%% - This is a "race" pattern - fastest responder wins
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a deferred choice between alternatives
%% {ok, WF} = wfnet_deferred_choice:start_link([
%%     {automatic, #{handler => fun() -> timer:sleep(100) end}},
%%     {manual, #{handler => fun() -> timer:sleep(5000) end}}
%% ]).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_deferred_choice:new([option_a, option_b, option_c]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_deferred_choice).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    trigger_option/2,
    get_chosen_option/1
]).

%% gen_wfnet callbacks
-export([
    workflow_spec/0,
    init_marking/2,
    fire/3,
    is_enabled/3,
    init/1
]).

%% Include records
-include_lib("gen_pnet.hrl").
-include_lib("gen_wfnet.hrl").

%% Types
-type option() :: atom() | {atom(), option_config()}.
-type option_config() :: #{
    handler => function(),           %% Function to execute when option is chosen
    timeout => timeout(),             %% Optional timeout for handler
    priority => non_neg_integer()     %% Priority for tie-breaking (0 = highest)
}.
-type options() :: [option()].
-type option_name() :: atom().
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(deferred_state, {
    options :: options(),
    option_count :: pos_integer(),
    chosen = undefined :: undefined | option_name(),
    triggered = false :: boolean(),
    pending = [] :: [option_name()]
}).

%% Export types
-export_type([option/0, option_config/0, options/0, option_name/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a deferred choice workflow process.
%%
%% @param Options List of option definitions
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_list(Options) ->
    gen_wfnet:start_link(?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Start a named deferred choice workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, options()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Options) ->
    gen_wfnet:start_link(Name, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Create a deferred choice workflow specification.
%%
%% Returns a workflow spec map that can be used with other
%% composition operators.
%%
%% @param Options List of option definitions
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(options()) -> wfnet_types:workflow_spec().
new(Options) when is_list(Options) ->
    new(Options, #{}).

%%--------------------------------------------------------------------
%% @doc Create a deferred choice workflow specification with options.
%%
%% @param Options List of option definitions
%% @param Config Configuration options
%% @returns workflow_spec()
%%
%% Supported configuration options:
%% - timeout: timeout() - Maximum time to wait for any option to trigger
%% - tie_breaker: priority | random | first - How to handle simultaneous triggers
%%
%% @end
%%--------------------------------------------------------------------
-spec new(options(), map()) -> wfnet_types:workflow_spec().
new(Options, Config) when is_list(Options), is_map(Config) ->
    case Options of
        [] -> error(empty_options);
        [_] -> error(single_option_use_direct);
        _ -> build_deferred_spec(Options, Config)
    end.

%%--------------------------------------------------------------------
%% @doc Manually trigger a specific option.
%%
%% This allows external triggering of an option. The first option
%% to be triggered (either manually or automatically) will be chosen.
%%
%% @param Pid Process pid or registered name
%% @param OptionName Name of the option to trigger
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_option(gen_wfnet:name(), option_name()) -> ok | {error, term()}.
trigger_option(Name, OptionName) ->
    gen_wfnet:call(Name, {trigger_option, OptionName}).

%%--------------------------------------------------------------------
%% @doc Get the option that was chosen.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, OptionName} | {error, not_chosen}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_chosen_option(gen_wfnet:name()) -> {ok, option_name()} | {error, not_chosen}.
get_chosen_option(Name) ->
    case gen_wfnet:usr_info(Name) of
        #deferred_state{chosen = undefined} ->
            {error, not_chosen};
        #deferred_state{chosen = Option} ->
            {ok, Option};
        Other ->
            {error, {invalid_state, Other}}
    end.

%%====================================================================
%% gen_wfnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Return the workflow specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec workflow_spec() -> wfnet_types:workflow_spec().
workflow_spec() ->
    %% Placeholder - actual spec built from state during init
    #{}.

%%--------------------------------------------------------------------
%% @doc Initialize the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(options()) -> {ok, #deferred_state{}}.
init(Options) ->
    Normalized = normalize_options(Options),
    OptionNames = [Name || {Name, _} <- Normalized],
    State = #deferred_state{
        options = Normalized,
        option_count = length(Normalized),
        chosen = undefined,
        triggered = false,
        pending = OptionNames
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #deferred_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #deferred_state{}) -> boolean().
is_enabled(begin_defer, #{start := [init]}, #deferred_state{triggered = false}) ->
    true;
is_enabled(choose, Mode, #deferred_state{triggered = false, pending = Pending}) ->
    %% Check if any option has a token
    lists:any(fun(Option) ->
        Place = option_place(Option),
        case maps:get(Place, Mode, []) of
            [] -> false;
            _ -> true
        end
    end, Pending);
is_enabled(complete, #{chosen := _}, #deferred_state{chosen = Option}) when Option =/= undefined ->
    true;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #deferred_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #deferred_state{}}.
fire(begin_defer, _Mode, #deferred_state{pending = Pending} = State) ->
    %% Enter deferral state - make all options available
    ProduceMap = lists:foldl(fun(Option, Acc) ->
        Acc#{option_place(Option) => [available]}
    end, #{start => []}, Pending),
    NewState = State#deferred_state{triggered = true},
    {produce, ProduceMap, NewState};

fire(choose, Mode, #deferred_state{pending = Pending, options = Options} = State) ->
    %% Find first available option (could use priority or random)
    Chosen = find_chosen_option(Pending, Mode, Options),
    ChosenPlace = option_place(Chosen),
    NewState = State#deferred_state{
        chosen = Chosen,
        pending = []
    },
    %% Mark chosen option, clear others
    ProduceMap = lists:foldl(fun(Option, Acc) ->
        Place = option_place(Option),
        case Option of
            Chosen -> Acc#{Place => [chosen]};
            _ -> Acc#{Place => []}
        end
    end, #{}, Pending),
    {produce, ProduceMap#{chosen => [Chosen]}, NewState};

fire(complete, Mode, #deferred_state{chosen = Chosen} = State) ->
    %% Complete the chosen option
    ChosenPlace = option_place(Chosen),
    NewState = State#deferred_state{pending = []},
    {produce, #{
        ChosenPlace => [],
        chosen => [],
        'end' => [{deferred_choice_complete, Chosen}]
    }, NewState};

fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build deferred choice workflow specification.
%%--------------------------------------------------------------------
build_deferred_spec(Options, Config) ->
    Normalized = normalize_options(Options),
    N = length(Normalized),
    OptionNames = [Name || {Name, _} <- Normalized],

    %% Generate place names
    Start = start,
    End = 'end',
    ChosenPlace = chosen,
    OptionPlaces = [option_place(Name) || Name <- OptionNames],

    %% Generate transitions
    BeginDeferTrans = begin_defer,
    ChooseTrans = choose,
    CompleteTrans = complete,

    %% Build places list
    Places = [Start, End, ChosenPlace | OptionPlaces],

    %% Build preset (transition -> input places)
    Preset = #{
        BeginDeferTrans => [Start],
        ChooseTrans => OptionPlaces,
        CompleteTrans => [ChosenPlace]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        BeginDeferTrans => OptionPlaces,
        ChooseTrans => [ChosenPlace],
        CompleteTrans => [End]
    },

    %% Get config options
    TieBreaker = maps:get(tie_breaker, Config, first),
    Timeout = maps:get(timeout, Config, infinity),

    #{
        places => Places,
        transitions => [BeginDeferTrans, ChooseTrans, CompleteTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => deferred_choice,
            option_count => N,
            tie_breaker => TieBreaker,
            timeout => Timeout
        }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize options to {Name, Config} tuples.
%%--------------------------------------------------------------------
normalize_options(Options) ->
    lists:map(fun
        ({Name, Config}) when is_atom(Name), is_map(Config) -> {Name, Config};
        (Name) when is_atom(Name) -> {Name, #{}}
    end, Options).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate option place name.
%%--------------------------------------------------------------------
option_place(OptionName) when is_atom(OptionName) ->
    list_to_atom(atom_to_list(OptionName) ++ "_option").

%%--------------------------------------------------------------------
%% @private
%% @doc Find the chosen option based on availability and priority.
%%--------------------------------------------------------------------
find_chosen_option(Pending, Mode, Options) ->
    %% Find options with tokens and select based on priority
    Available = lists:filter(fun(Option) ->
        Place = option_place(Option),
        case maps:get(Place, Mode, []) of
            [] -> false;
            _ -> true
        end
    end, Pending),

    case Available of
        [] -> error(no_available_options);
        _ ->
            %% Sort by priority
            Sorted = lists:sort(fun(A, B) ->
                PriorityA = get_priority(A, Options),
                PriorityB = get_priority(B, Options),
                PriorityA =< PriorityB
            end, Available),
            hd(Sorted)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Get priority for an option (lower = higher priority).
%%--------------------------------------------------------------------
get_priority(Option, Options) ->
    case lists:keyfind(Option, 1, Options) of
        {Option, Config} ->
            maps:get(priority, Config, 0);
        false ->
            0
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Spec = new([a, b, c]),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(3, length(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)).

%% error cases test
new_error_test() ->
    ?assertError(empty_options, new([])),
    ?assertError(single_option_use_direct, new([single])).

%% normalize_options test
normalize_options_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], normalize_options([a, b])),
    ?assertEqual([{a, #{priority => 1}}, {b, #{}}], normalize_options([{a, #{priority => 1}}, b])).

%% option_place test
option_place_test() ->
    ?assertEqual(a_option, option_place(a)),
    ?assertEqual(my_option_option, option_place(my_option)).

%% init test
init_test() ->
    {ok, State} = init([a, b, c]),
    ?assertEqual(3, State#deferred_state.option_count),
    ?assertEqual(undefined, State#deferred_state.chosen),
    ?assertEqual(false, State#deferred_state.triggered),
    ?assertEqual([a, b, c], lists:sort(State#deferred_state.pending)).

%% is_enabled test
is_enabled_begin_defer_test() ->
    State = #deferred_state{triggered = false},
    Mode = #{start => [init]},
    ?assert(is_enabled(begin_defer, Mode, State)),

    %% After triggered, should not enable again
    State2 = State#deferred_state{triggered = true},
    ?assertNot(is_enabled(begin_defer, Mode, State2)).

is_enabled_choose_test() ->
    State = #deferred_state{triggered = false, pending = [a, b]},
    Mode = #{a_option => [available], b_option => []},
    ?assert(is_enabled(choose, Mode, State)).

is_enabled_complete_test() ->
    State = #deferred_state{chosen = a},
    Mode = #{chosen => [a]},
    ?assert(is_enabled(complete, Mode, State)).

%% fire begin_defer test
fire_begin_defer_test() ->
    State = #deferred_state{pending = [a, b]},
    Result = fire(begin_defer, #{start => [init]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual([available], maps:get(a_option, ProduceMap)),
    ?assertEqual([available], maps:get(b_option, ProduceMap)),
    ?assertEqual(true, NewState#deferred_state.triggered).

%% fire choose test
fire_choose_test() ->
    Options = [{a, #{priority => 0}}, {b, #{priority => 1}}],
    State = #deferred_state{pending = [a, b], options = Options},
    Mode = #{a_option => [available], b_option => [available]},

    Result = fire(choose, Mode, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual(a, NewState#deferred_state.chosen),
    ?assertEqual([chosen], maps:get(a_option, ProduceMap)),
    ?assertEqual([], maps:get(b_option, ProduceMap)).

%% fire complete test
fire_complete_test() ->
    State = #deferred_state{chosen = a, pending = []},
    Result = fire(complete, #{chosen => [a]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertMatch([{deferred_choice_complete, a}], maps:get('end', ProduceMap)).

%% find_chosen_option test
find_chosen_option_test() ->
    Options = [{a, #{priority => 1}}, {b, #{priority => 0}}, {c, #{priority => 2}}],
    Mode = #{a_option => [avail], b_option => [avail], c_option => []},

    %% b has highest priority (0)
    ?assertEqual(b, find_chosen_option([a, b, c], Mode, Options)).

%% get_priority test
get_priority_test() ->
    Options = [{a, #{priority => 5}}, {b, #{}}],
    ?assertEqual(5, get_priority(a, Options)),
    ?assertEqual(0, get_priority(b, Options)).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Spec = new([option1, option2]),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := deferred_choice}}, Spec).

%% new with tie_breaker test
new_with_tie_breaker_test() ->
    Spec = new([a, b], #{tie_breaker => priority}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(priority, maps:get(tie_breaker, Optional)).

%% new with timeout test
new_with_timeout_test() ->
    Spec = new([a, b], #{timeout => 5000}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(5000, maps:get(timeout, Optional)).

-endif.
