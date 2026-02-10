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
%% @doc Parallel Split Workflow Pattern (WCP-02)
%%
%% Implements the Parallel Split pattern where multiple branches
%% execute concurrently. Uses AND-split semantics.
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a parallel split into 3 branches
%% {ok, WF} = wfnet_parallel_split:start_link([branch_a, branch_b, branch_c]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_parallel_split).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2
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
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(parallel_state, {
    branches :: branches(),
    branch_count :: pos_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a parallel split workflow process.
%%
%% @param Branches List of branch atoms to execute in parallel
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(branches()) -> {ok, pid()} | {error, term()}.
start_link(Branches) when is_list(Branches) ->
    gen_wfnet:start_link(?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Start a named parallel split workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, branches()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Branches) ->
    gen_wfnet:start_link(Name, ?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Create a parallel split workflow specification.
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
%% @doc Create a parallel split workflow specification with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches(), map()) -> wfnet_types:workflow_spec().
new(Branches, Options) when is_list(Branches), is_map(Options) ->
    case Branches of
        [] -> error(empty_branches);
        [_] -> error(single_branch_use_sequence);
        _ -> build_parallel_spec(Branches, Options)
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
    #{}.

%%--------------------------------------------------------------------
%% @doc Initialize the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(branches()) -> {ok, #parallel_state{}}.
init(Branches) ->
    State = #parallel_state{
        branches = normalize_branches(Branches),
        branch_count = length(Branches)
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #parallel_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #parallel_state{}) -> boolean().
is_enabled(_Transition, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #parallel_state{}) ->
    abort | {produce, wfnet_types:produce_map()}.
fire(split, _Mode, #parallel_state{branches = Branches}) ->
    %% Produce tokens to all branch places
    BranchPlaces = [branch_place(B) || B <- Branches],
    ProduceMap = lists:foldl(fun(P, Acc) ->
        Acc#{P => [split]}
    end, #{}, BranchPlaces),
    {produce, ProduceMap};
fire(sync, _Mode, #parallel_state{}) ->
    %% All branches completed, proceed to end
    {produce, #{'end' => [all_completed]}};
fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build parallel split workflow specification.
%%--------------------------------------------------------------------
build_parallel_spec(Branches, _Options) ->
    Normalized = normalize_branches(Branches),
    N = length(Normalized),

    %% Generate place names
    Start = start,
    End = 'end',
    BranchPlaces = [branch_place(B) || B <- Normalized],
    SyncPlace = sync,

    %% Generate transitions
    SplitTrans = split,
    SyncTrans = sync,

    %% Build places list
    Places = [Start, End, SyncPlace | BranchPlaces],

    %% Build preset
    Preset = #{
        SplitTrans => [Start],
        SyncTrans => [SyncPlace | BranchPlaces]
    },

    %% Build postset
    Postset = #{
        SplitTrans => BranchPlaces,
        SyncTrans => [End]
    },

    #{
        places => Places,
        transitions => [SplitTrans, SyncTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{pattern => parallel_split, branch_count => N}
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
branch_place({Branch, _Opts}) ->
    list_to_atom(atom_to_list(Branch) ++ "_branch");
branch_place(Branch) ->
    list_to_atom(atom_to_list(Branch) ++ "_branch").

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Spec = new([a, b, c]),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(2, length(maps:get(transitions, Spec)).

%% error cases test
new_error_test() ->
    ?assertError(empty_branches, new([])),
    ?assertError(single_branch_use_sequence, new([single])).

-endif.
