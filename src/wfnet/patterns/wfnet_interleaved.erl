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
%% @doc Interleaved Parallel Routing Workflow Pattern (WCP-17)
%%
%% Implements the Interleaved Parallel Routing pattern where multiple
%% branches are executed in an interleaved (round-robin) fashion rather
%% than truly concurrently. This maintains ordering constraints while
%% allowing progress across multiple branches.
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Entry point to the interleaved routing
%%   - route: Controls which branch is currently active
%%   - branch1, branch2, ..., branchN: Individual branch places
%%   - 'end': Final place after all branches complete
%%
%% Transitions:
%%   - select_next: Selects the next branch to execute
%%   - advance: Moves to the next branch after current completes
%%   - complete_all: Finalizes when all branches have executed
%%
%% Flow: start -> branch1 -> branch2 -> ... -> branchN -> end
%% (but branches can be interleaved with other workflows)
%%
%% <h3>Interleaved Semantics</h3>
%%
%% - Branches execute one at a time in a specific order
%% - Each branch must complete before moving to the next
%% - Other workflows can interleave between branch executions
%% - Maintains strict ordering between branches
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create interleaved routing for 3 branches
%% {ok, WF} = wfnet_interleaved:start_link([step_a, step_b, step_c]).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_interleaved:new([task1, task2, task3]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_interleaved).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    get_current_branch/1,
    get_completed_branches/1,
    get_remaining_branches/1
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
-type branch() :: atom() | {atom(), map()}.
-type branches() :: [branch()].
-type branch_name() :: atom().
-type branch_order() :: [branch_name()].
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(interleaved_state, {
    branches :: branches(),
    branch_order :: branch_order(),
    branch_count :: pos_integer(),
    current_index = 1 :: pos_integer(),
    completed = [] :: [branch_name()],
    all_complete = false :: boolean()
}).

%% Export types
-export_type([branch/0, branches/0, branch_name/0, branch_order/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start an interleaved parallel routing workflow process.
%%
%% @param Branches List of branch atoms to execute in order
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(branches()) -> {ok, pid()} | {error, term()}.
start_link(Branches) when is_list(Branches) ->
    gen_wfnet:start_link(?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Start a named interleaved parallel routing workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, branches()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Branches) ->
    gen_wfnet:start_link(Name, ?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Create an interleaved parallel routing workflow specification.
%%
%% Returns a workflow spec map that can be used with other
%% composition operators.
%%
%% @param Branches List of branch atoms
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches()) -> wfnet_types:workflow_spec().
new(Branches) when is_list(Branches) ->
    new(Branches, #{}).

%%--------------------------------------------------------------------
%% @doc Create an interleaved parallel routing workflow specification with options.
%%
%% @param Branches List of branch atoms
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% Supported options:
%% - order: [atom()] - Custom execution order (default: as provided)
%% - allow_skip: boolean() - If true, allows skipping completed branches
%% - cyclic: boolean() - If true, cycles back to first branch after completion
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches(), map()) -> wfnet_types:workflow_spec().
new(Branches, Options) when is_list(Branches), is_map(Options) ->
    case Branches of
        [] -> error(empty_branches);
        [_] -> error(single_branch_use_sequence);
        _ -> build_interleaved_spec(Branches, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Get the currently active branch.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, BranchName} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_current_branch(gen_wfnet:name()) -> {ok, branch_name()} | {error, term()}.
get_current_branch(Name) ->
    case gen_wfnet:usr_info(Name) of
        #interleaved_state{branch_order = [Current | _], all_complete = false} ->
            {ok, Current};
        #interleaved_state{all_complete = true} ->
            {error, all_complete};
        Other ->
            {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Get the list of completed branches.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, [BranchName]} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_completed_branches(gen_wfnet:name()) -> {ok, [branch_name()]} | {error, term()}.
get_completed_branches(Name) ->
    case gen_wfnet:usr_info(Name) of
        #interleaved_state{completed = Completed} -> {ok, Completed};
        Other -> {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Get the list of remaining branches to execute.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, [BranchName]} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_remaining_branches(gen_wfnet:name()) -> {ok, [branch_name()]} | {error, term()}.
get_remaining_branches(Name) ->
    case gen_wfnet:usr_info(Name) of
        #interleaved_state{branch_order = Order, completed = Completed} ->
            Remaining = Order -- Completed,
            {ok, Remaining};
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
-spec init(branches()) -> {ok, #interleaved_state{}}.
init(Branches) ->
    Normalized = normalize_branches(Branches),
    Order = [Name || {Name, _} <- Normalized],
    State = #interleaved_state{
        branches = Normalized,
        branch_order = Order,
        branch_count = length(Order),
        current_index = 1,
        completed = [],
        all_complete = false
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #interleaved_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #interleaved_state{}) -> boolean().
is_enabled(select_next, #{start := [init]}, #interleaved_state{all_complete = false}) ->
    true;
is_enabled(advance, Mode, #interleaved_state{branch_order = [Current | _], completed = Completed, all_complete = false}) ->
    %% Current branch must have a token to advance
    Place = branch_place(Current),
    case maps:get(Place, Mode, []) of
        [] -> false;
        _ -> true
    end;
is_enabled(complete_all, _Mode, #interleaved_state{all_complete = true}) ->
    true;
is_enabled(complete_all, _Mode, #interleaved_state{}) ->
    false;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #interleaved_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #interleaved_state{}}.
fire(select_next, _Mode, #interleaved_state{branch_order = [Current | _]} = State) ->
    %% Select and activate the next branch
    CurrentPlace = branch_place(Current),
    {produce, #{
        start => [],
        route => [{selected, Current}]
    }, State};

fire(advance, Mode, #interleaved_state{branch_order = [Current | Rest], completed = Completed, current_index = Index} = State) ->
    %% Mark current branch as complete and move to next
    CurrentPlace = branch_place(Current),
    NewCompleted = [Current | Completed],
    NewIndex = Index + 1,

    case Rest of
        [] ->
            %% All branches complete
            NewState = State#interleaved_state{
                completed = NewCompleted,
                current_index = NewIndex,
                all_complete = true
            },
            {produce, #{
                CurrentPlace => [],
                route => [{all_done, NewCompleted}]
            }, NewState};
        [_Next | _] ->
            %% Move to next branch
            NewState = State#interleaved_state{
                branch_order = Rest ++ [Current],  %% Cycle current to end
                completed = NewCompleted,
                current_index = NewIndex
            },
            {produce, #{
                CurrentPlace => [],
                route => [{advanced, Current}]
            }, NewState}
    end;

fire(complete_all, _Mode, #interleaved_state{completed = Completed} = State) ->
    %% Finalize the interleaved execution
    NewState = State#interleaved_state{all_complete = true},
    {produce, #{
        route => [],
        'end' => [{interleaved_complete, Completed}]
    }, NewState};

fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build interleaved parallel routing workflow specification.
%%--------------------------------------------------------------------
build_interleaved_spec(Branches, Options) ->
    Normalized = normalize_branches(Branches),
    N = length(Normalized),
    BranchNames = [Name || {Name, _} <- Normalized],

    %% Use custom order if provided
    Order = case maps:get(order, Options, undefined) of
        undefined -> BranchNames;
        CustomOrder when is_list(CustomOrder) -> CustomOrder
    end,

    %% Generate place names
    Start = start,
    End = 'end',
    RoutePlace = route,
    BranchPlaces = [branch_place(Name) || Name <- BranchNames],

    %% Generate transitions
    SelectNextTrans = select_next,
    AdvanceTrans = advance,
    CompleteAllTrans = complete_all,

    %% Build places list
    Places = [Start, End, RoutePlace | BranchPlaces],

    %% Build preset (transition -> input places)
    Preset = #{
        SelectNextTrans => [Start],
        AdvanceTrans => [RoutePlace | BranchPlaces],
        CompleteAllTrans => [RoutePlace]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        SelectNextTrans => [RoutePlace],
        AdvanceTrans => [RoutePlace],
        CompleteAllTrans => [End]
    },

    %% Get options
    AllowSkip = maps:get(allow_skip, Options, false),
    Cyclic = maps:get(cyclic, Options, false),

    #{
        places => Places,
        transitions => [SelectNextTrans, AdvanceTrans, CompleteAllTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => interleaved_routing,
            branch_count => N,
            order => Order,
            allow_skip => AllowSkip,
            cyclic => Cyclic
        }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize branches to {Name, Options} tuples.
%%--------------------------------------------------------------------
normalize_branches(Branches) ->
    lists:map(fun
        ({Name, Opts}) when is_atom(Name), is_map(Opts) -> {Name, Opts};
        (Name) when is_atom(Name) -> {Name, #{}}
    end, Branches).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate branch place name.
%%--------------------------------------------------------------------
branch_place(BranchName) when is_atom(BranchName) ->
    list_to_atom(atom_to_list(BranchName) ++ "_branch").

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
    ?assertError(empty_branches, new([])),
    ?assertError(single_branch_use_sequence, new([single])).

%% normalize_branches test
normalize_branches_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], normalize_branches([a, b])),
    ?assertEqual([{a, #{opt => 1}}, {b, #{}}], normalize_branches([{a, #{opt => 1}}, b])).

%% branch_place test
branch_place_test() ->
    ?assertEqual(a_branch, branch_place(a)),
    ?assertEqual(my_branch_branch, branch_place(my_branch)).

%% init test
init_test() ->
    {ok, State} = init([a, b, c]),
    ?assertEqual(3, State#interleaved_state.branch_count),
    ?assertEqual([a, b, c], State#interleaved_state.branch_order),
    ?assertEqual(1, State#interleaved_state.current_index),
    ?assertEqual([], State#interleaved_state.completed),
    ?assertEqual(false, State#interleaved_state.all_complete).

%% is_enabled test
is_enabled_select_next_test() ->
    State = #interleaved_state{all_complete = false},
    Mode = #{start => [init]},
    ?assert(is_enabled(select_next, Mode, State)).

is_enabled_advance_test() ->
    State = #interleaved_state{branch_order => [a, b, c], completed => []},
    Mode = #{a_branch => [done], route => [selected]},
    ?assert(is_enabled(advance, Mode, State)).

is_enabled_complete_all_test() ->
    State = #interleaved_state{all_complete => true},
    ?assert(is_enabled(complete_all, #{route => [all_done]}, State)),

    State2 = State#interleaved_state{all_complete => false},
    ?assertNot(is_enabled(complete_all, #{}, State2)).

%% fire select_next test
fire_select_next_test() ->
    State = #interleaved_state{branch_order => [a, b, c]},
    Result = fire(select_next, #{start => [init]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([], maps:get(start, ProduceMap)),
    ?assertMatch([{selected, a}], maps:get(route, ProduceMap)).

%% fire advance test - not last branch
fire_advance_middle_test() ->
    State = #interleaved_state{branch_order => [a, b, c], completed => [], current_index => 1},
    Mode = #{a_branch => [done], route => [selected]},
    Result = fire(advance, Mode, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual([a], NewState#interleaved_state.completed),
    ?assertEqual(2, NewState#interleaved_state.current_index),
    ?assertEqual(false, NewState#interleaved_state.all_complete).

%% fire advance test - last branch
fire_advance_last_test() ->
    State = #interleaved_state{branch_order => [c], completed => [a, b], current_index => 3},
    Mode = #{c_branch => [done], route => [selected]},
    Result = fire(advance, Mode, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual([c, a, b], lists:sort(NewState#interleaved_state.completed)),
    ?assertEqual(4, NewState#interleaved_state.current_index),
    ?assertEqual(true, NewState#interleaved_state.all_complete).

%% fire complete_all test
fire_complete_all_test() ->
    State = #interleaved_state{completed => [a, b, c]},
    Result = fire(complete_all, #{route => [all_done]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertMatch([{interleaved_complete, [a, b, c]}], maps:get('end', ProduceMap)).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Spec = new([branch1, branch2]),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := interleaved_routing}}, Spec).

%% new with custom order test
new_with_order_test() ->
    Spec = new([a, b, c], #{order => [c, b, a]}),
    Optional = maps:get(optional, Spec),
    ?assertEqual([c, b, a], maps:get(order, Optional)).

%% new with allow_skip test
new_with_allow_skip_test() ->
    Spec = new([a, b], #{allow_skip => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(allow_skip, Optional)).

%% new with cyclic test
new_with_cyclic_test() ->
    Spec = new([a, b], #{cyclic => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(cyclic, Optional)).

-endif.
