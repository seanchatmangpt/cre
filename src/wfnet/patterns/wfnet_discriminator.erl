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
%% @doc Discriminator Workflow Pattern (WCP-09)
%%
%% Implements the Discriminator pattern where multiple concurrent paths
%% merge, but execution continues as soon as the FIRST path completes.
%% Subsequent completions are ignored until reset.
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Entry point (optional, for workflows with split)
%%   - branch1, branch2, ..., branchN: Concurrent branch places
%%   - waiting: Place waiting for first completion
%%   - 'end': Final place after first branch completes
%%
%% Transitions:
%%   - complete: Fires when first branch completes
%%   - reset: Resets the discriminator for next use
%%
%% Flow: (branch1 OR branch2 OR ... OR branchN) -> complete -> end
%%
%% <h3>Discriminator Semantics</h3>
%%
%% - Activates waiting for all branches to start
%% - Completes when the FIRST branch finishes
%% - Ignores subsequent branch completions
%% - Can be reset for reuse
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a discriminator for 3 concurrent branches
%% {ok, WF} = wfnet_discriminator:start_link([branch_a, branch_b, branch_c]).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_discriminator:new([task1, task2, task3]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_discriminator).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    get_completed_branch/1,
    reset/1,
    is_waiting/1
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
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(discriminator_state, {
    branches :: branches(),
    branch_count :: pos_integer(),
    completed = undefined :: undefined | branch_name(),
    waiting = false :: boolean(),
    seen_completions = [] :: [branch_name()]
}).

%% Export types
-export_type([branch/0, branches/0, branch_name/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a discriminator workflow process.
%%
%% @param Branches List of branch atoms to discriminate
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(branches()) -> {ok, pid()} | {error, term()}.
start_link(Branches) when is_list(Branches) ->
    gen_wfnet:start_link(?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Start a named discriminator workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, branches()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Branches) ->
    gen_wfnet:start_link(Name, ?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Create a discriminator workflow specification.
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
%% @doc Create a discriminator workflow specification with options.
%%
%% @param Branches List of branch atoms
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% Supported options:
%% - auto_reset: boolean() - If true, automatically resets after completion
%% - track_all: boolean() - If true, tracks all completions even after first
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches(), map()) -> wfnet_types:workflow_spec().
new(Branches, Options) when is_list(Branches), is_map(Options) ->
    case Branches of
        [] -> error(empty_branches);
        [_] -> error(single_branch_use_direct);
        _ -> build_discriminator_spec(Branches, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Get the name of the branch that completed first.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, BranchName} | {error, not_completed}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_completed_branch(gen_wfnet:name()) -> {ok, branch_name()} | {error, not_completed}.
get_completed_branch(Name) ->
    case gen_wfnet:usr_info(Name) of
        #discriminator_state{completed = undefined} ->
            {error, not_completed};
        #discriminator_state{completed = Branch} ->
            {ok, Branch};
        Other ->
            {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Reset the discriminator for reuse.
%%
%% Clears the completed state and allows the discriminator to
%% wait for the next completion.
%%
%% @param Pid Process pid or registered name
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(gen_wfnet:name()) -> ok | {error, term()}.
reset(Name) ->
    gen_wfnet:call(Name, reset_discriminator).

%%--------------------------------------------------------------------
%% @doc Check if the discriminator is currently waiting for completion.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, boolean()} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec is_waiting(gen_wfnet:name()) -> {ok, boolean()} | {error, term()}.
is_waiting(Name) ->
    case gen_wfnet:usr_info(Name) of
        #discriminator_state{waiting = Waiting} ->
            {ok, Waiting};
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
-spec init(branches()) -> {ok, #discriminator_state{}}.
init(Branches) ->
    State = #discriminator_state{
        branches = normalize_branches(Branches),
        branch_count = length(Branches),
        completed = undefined,
        waiting = false,
        seen_completions = []
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #discriminator_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #discriminator_state{}) -> boolean().
is_enabled(complete, _Mode, #discriminator_state{completed = undefined}) ->
    true;
is_enabled(complete, _Mode, #discriminator_state{}) ->
    false;
is_enabled(reset, _Mode, _State) ->
    true;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #discriminator_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #discriminator_state{}}.
fire(complete, Mode, #discriminator_state{branches = Branches, seen_completions = Seen} = State) ->
    %% Find first completed branch that hasn't been seen yet
    case find_first_completion(Branches, Mode, Seen) of
        {ok, Branch} ->
            BranchPlace = branch_place(Branch),
            NewSeen = [Branch | Seen],
            NewState = State#discriminator_state{
                completed = Branch,
                waiting = false,
                seen_completions = NewSeen
            },
            {produce, #{
                BranchPlace => [],
                waiting => [],
                'end' => [{first_complete, Branch}]
            }, NewState};
        error ->
            abort
    end;

fire(reset, _Mode, State) ->
    %% Reset the discriminator
    NewState = State#discriminator_state{
        completed = undefined,
        waiting = true,
        seen_completions = []
    },
    {produce, #{
        'end' => [],
        waiting => [reset]
    }, NewState};

fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build discriminator workflow specification.
%%--------------------------------------------------------------------
build_discriminator_spec(Branches, Options) ->
    Normalized = normalize_branches(Branches),
    N = length(Normalized),
    BranchNames = [Name || {Name, _} <- Normalized],

    %% Generate place names
    Start = start,
    End = 'end',
    WaitingPlace = waiting,
    BranchPlaces = [branch_place(Name) || Name <- BranchNames],

    %% Generate transitions
    CompleteTrans = complete,
    ResetTrans = reset,

    %% Build places list
    Places = [Start, End, WaitingPlace | BranchPlaces],

    %% Build preset (transition -> input places)
    Preset = #{
        CompleteTrans => [WaitingPlace | BranchPlaces],
        ResetTrans => [End]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        CompleteTrans => [End],
        ResetTrans => [WaitingPlace]
    },

    %% Get options
    AutoReset = maps:get(auto_reset, Options, false),
    TrackAll = maps:get(track_all, Options, false),

    BaseSpec = #{
        places => Places,
        transitions => [CompleteTrans, ResetTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => discriminator,
            branch_count => N,
            auto_reset => AutoReset,
            track_all => TrackAll
        }
    },

    BaseSpec.

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

%%--------------------------------------------------------------------
%% @private
%% @doc Find first branch that has completed.
%%--------------------------------------------------------------------
find_first_completion(Branches, Mode, Seen) ->
    lists:foldl(fun({Branch, _Opts}, Acc) ->
        case Acc of
            {ok, _} -> Acc;
            error ->
                Place = branch_place(Branch),
                HasToken = case maps:get(Place, Mode, []) of
                    [] -> false;
                    _ -> true
                end,
                AlreadySeen = lists:member(Branch, Seen),
                case HasToken andalso not AlreadySeen of
                    true -> {ok, Branch};
                    false -> error
                end
        end
    end, error, Branches).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Spec = new([a, b, c]),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(2, length(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)).

%% error cases test
new_error_test() ->
    ?assertError(empty_branches, new([])),
    ?assertError(single_branch_use_direct, new([single])).

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
    ?assertEqual(3, State#discriminator_state.branch_count),
    ?assertEqual(undefined, State#discriminator_state.completed),
    ?assertEqual(false, State#discriminator_state.waiting).

%% is_enabled test
is_enabled_complete_test() ->
    State = #discriminator_state{completed = undefined},
    Mode = #{a_branch => [done]},
    ?assert(is_enabled(complete, Mode, State)),

    %% After completion, should not enable again
    State2 = State#discriminator_state{completed = a},
    ?assertNot(is_enabled(complete, Mode, State2)).

is_enabled_reset_test() ->
    State = #discriminator_state{},
    ?assert(is_enabled(reset, #{}, State)).

%% fire complete test
fire_complete_test() ->
    Branches = [{a, #{}}, {b, #{}}, {c, #{}}],
    State = #discriminator_state{branches = Branches, seen_completions = []},
    Mode = #{a_branch => [done], b_branch => [done], waiting => [wait]},

    Result = fire(complete, Mode, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual(a, NewState#discriminator_state.completed),
    ?assertEqual([a], NewState#discriminator_state.seen_completions).

%% fire reset test
fire_reset_test() ->
    State = #discriminator_state{completed = a, seen_completions = [a]},
    Result = fire(reset, #{}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual(undefined, NewState#discriminator_state.completed),
    ?assertEqual([], NewState#discriminator_state.seen_completions),
    ?assertEqual(true, NewState#discriminator_state.waiting).

%% find_first_completion test
find_first_completion_test() ->
    Branches = [{a, #{}}, {b, #{}}, {c, #{}}],
    Mode = #{a_branch => [], b_branch => [done], c_branch => [done]},
    ?assertEqual({ok, b}, find_first_completion(Branches, Mode, [])),
    %% With b already seen, should find c
    ?assertEqual({ok, c}, find_first_completion(Branches, Mode, [b])),
    %% All seen
    ?assertEqual(error, find_first_completion(Branches, Mode, [b, c])).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Spec = new([branch1, branch2]),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := discriminator}}, Spec).

%% new with options test
new_with_auto_reset_test() ->
    Spec = new([a, b], #{auto_reset => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(auto_reset, Optional)).

new_with_track_all_test() ->
    Spec = new([a, b], #{track_all => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(track_all, Optional)).

-endif.
