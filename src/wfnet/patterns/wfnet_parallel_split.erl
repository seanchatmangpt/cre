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
%% execute concurrently. Uses AND-split semantics with synchronization.
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Initial place (workflow entry)
%%   - branch1, branch2, ..., branchN: Parallel branch execution places
%%   - sync: Synchronization place for AND-join
%%   - 'end': Final place (workflow exit)
%%
%% Transitions:
%%   - split: AND-split transition that activates all branches
%%   - join_branchN: Individual branch completion transitions
%%   - sync: AND-join transition that waits for all branches
%%
%% Flow: start -> split -> (branch1 | branch2 | ... | branchN) -> sync -> end
%%
%% <h3>AND-Split Semantics</h3>
%%
%% When the split transition fires:
%% - It consumes one token from start
%% - It produces one token to EACH branch place
%% - All branches become active concurrently
%%
%% <h3>Synchronization</h3>
%%
%% The sync transition uses AND-join semantics:
%% - It requires tokens from ALL branch places to fire
%% - Individual branches complete via join_branchN transitions
%% - When all branches complete, sync fires and produces to end
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a parallel split into 3 branches
%% {ok, WF} = wfnet_parallel_split:start_link([branch_a, branch_b, branch_c]).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_parallel_split:new([a, b, c]).
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
    new/2,
    execute/2,
    get_branch_count/1
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
-type branch_index() :: pos_integer().
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(parallel_state, {
    branches :: branches(),
    branch_count :: pos_integer(),
    completed = [] :: [branch_index()],
    results = #{} :: #{branch_index() => term()}
}).

%% Export types
-export_type([branch/0, branches/0, branch_index/0]).

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
%% @doc Create a parallel split workflow specification with options.
%%
%% @param Branches List of branch atoms
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% Supported options:
%% - sync_type: simple | counter - How synchronization is handled
%% - timeout: pos_integer() - Maximum time to wait for sync (ms)
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

%%--------------------------------------------------------------------
%% @doc Execute a parallel split synchronously.
%%
%% Executes all branch functions in parallel and waits for all
%% to complete before returning.
%%
%% @param BranchFuns List of functions to execute in parallel
%% @param InputData Input data passed to each branch function
%% @returns {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute([function()], term()) -> {ok, #{branch_index() => term()}} | {error, term()}.
execute(BranchFuns, InputData) when is_list(BranchFuns), length(BranchFuns) >= 2 ->
    BranchCount = length(BranchFuns),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all branches in parallel
    Pids = lists:map(fun({Fun, Index}) ->
        spawn(fun() ->
            try
                Result = Fun(InputData),
                Parent ! {Ref, {branch_complete, Index}, Result}
            catch
                Class:Reason:Stack ->
                    Parent ! {Ref, {branch_error, Index}, {Class, Reason, Stack}}
            end
        end)
    end, lists:zip(BranchFuns, lists:seq(1, BranchCount))),

    %% Wait for all branches to complete
    wait_all_branches(Ref, Pids, BranchCount, 30000, #{}).

%%--------------------------------------------------------------------
%% @doc Get the number of branches in a running parallel split.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, BranchCount}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_branch_count(gen_wfnet:name()) -> {ok, pos_integer()}.
get_branch_count(Name) ->
    case gen_wfnet:usr_info(Name) of
        #parallel_state{branch_count = Count} -> {ok, Count};
        Other -> {error, {invalid_state, Other}}
    end.

%%====================================================================
%% gen_wfnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Return the workflow specification.
%%
%% This returns a placeholder - the actual spec is built from state
%% during initialization.
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
-spec init(branches()) -> {ok, #parallel_state{}}.
init(Branches) ->
    Normalized = normalize_branches(Branches),
    State = #parallel_state{
        branches = Normalized,
        branch_count = length(Normalized),
        completed = [],
        results = #{}
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
is_enabled(split, _Mode, _State) ->
    true;
is_enabled(join, _Mode, _State) ->
    true;
is_enabled(sync, Mode, #parallel_state{branch_count = Count, completed = Completed}) ->
    %% Sync is enabled when all branches have completed
    %% Check that we have completion tokens equal to branch count
    length(Completed) =:= Count andalso has_all_branch_tokens(Mode, Count);
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #parallel_state{}) ->
    abort | {produce, wfnet_types:produce_map()}.
fire(split, _Mode, #parallel_state{branches = Branches}) ->
    %% Produce tokens to all branch places (AND-split)
    BranchPlaces = [branch_place(B) || B <- Branches],
    ProduceMap = lists:foldl(fun(P, Acc) ->
        Acc#{P => [split]}
    end, #{}, BranchPlaces),
    {produce, ProduceMap};
fire(join, #{branch_place := [Token]}, #parallel_state{completed = Completed}) ->
    %% Individual branch completion - move to sync place
    %% Extract branch index from token if present
    BranchIndex = case Token of
        {branch, N} -> N;
        _ -> length(Completed) + 1
    end,
    {produce, #{
        branch_place => [],
        sync => [{branch_completed, BranchIndex}]
    }};
fire(sync, Mode, #parallel_state{branches = Branches}) ->
    %% All branches completed - produce to end
    %% Collect results from mode if available
    Results = collect_branch_results(Mode, Branches),
    {produce, #{
        sync => [],
        'end' => [{all_completed, Results}]
    }};
fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build parallel split workflow specification.
%%--------------------------------------------------------------------
build_parallel_spec(Branches, Options) ->
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

    %% Build preset (transition -> input places)
    Preset = #{
        SplitTrans => [Start],
        SyncTrans => [SyncPlace | BranchPlaces]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        SplitTrans => BranchPlaces,
        SyncTrans => [End]
    },

    %% Get sync type option
    SyncType = maps:get(sync_type, Options, simple),

    BaseSpec = #{
        places => Places,
        transitions => [SplitTrans, SyncTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => parallel_split,
            branch_count => N,
            sync_type => SyncType
        }
    },

    %% Add timeout to optional if specified
    case maps:get(timeout, Options, undefined) of
        undefined -> BaseSpec;
        Timeout -> BaseSpec#{
            optional => (maps:get(optional, BaseSpec))#{timeout => Timeout}
        }
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc Check if mode has tokens from all branches.
%%--------------------------------------------------------------------
has_all_branch_tokens(Mode, Count) ->
    %% Count branch_completed tokens in sync place
    case maps:get(sync, Mode, []) of
        Tokens when is_list(Tokens) ->
            CompletedCount = length(lists:filter(fun
                ({branch_completed, _BranchIndex}) -> true;
                (_Other) -> false
            end, Tokens)),
            CompletedCount >= Count;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collect results from all branches.
%%--------------------------------------------------------------------
collect_branch_results(Mode, Branches) ->
    lists:foldl(fun(Branch, Acc) ->
        Place = branch_place(Branch),
        case maps:get(Place, Mode, []) of
            [] -> Acc;
            Tokens ->
                BranchName = element(1, Branch),
                Acc#{BranchName => lists:last(Tokens)}
        end
    end, #{}, Branches).

%%--------------------------------------------------------------------
%% @private
%% @doc Wait for all branches to complete.
%%--------------------------------------------------------------------
wait_all_branches(_Ref, _Pids, 0, _Timeout, Acc) ->
    {ok, Acc};
wait_all_branches(Ref, Pids, Remaining, Timeout, Acc) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            wait_all_branches(Ref, Pids, Remaining - 1, Timeout, Acc#{Index => Result});
        {Ref, {branch_error, Index}, {Class, Reason, _Stack}} ->
            {error, {branch_error, Index, Class, Reason}}
    after Timeout ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        {error, timeout}
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
    ?assertEqual(2, length(maps:get(transitions, Spec))),
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
    ?assertEqual(branch_branch, branch_place({branch, #{}})).

%% init test
init_test() ->
    {ok, State} = init([a, b, c]),
    ?assertEqual(3, State#parallel_state.branch_count),
    ?assertEqual([], State#parallel_state.completed),
    ?assertEqual(#{}, State#parallel_state.results).

%% init_marking test
init_marking_start_test() ->
    State = #parallel_state{},
    ?assertEqual([init], init_marking(start, State)).

init_marking_other_test() ->
    State = #parallel_state{},
    ?assertEqual([], init_marking('end', State)),
    ?assertEqual([], init_marking(sync, State)),
    ?assertEqual([], init_marking(a_branch, State)).

%% is_enabled test
is_enabled_split_test() ->
    State = #parallel_state{},
    ?assert(is_enabled(split, #{}, State)).

is_enabled_sync_test() ->
    State = #parallel_state{branch_count = 2, completed = [1, 2]},
    ModeWithTokens = #{sync => [{branch_completed, 1}, {branch_completed, 2}]},
    ?assert(is_enabled(sync, ModeWithTokens, State)),

    %% Not all completed
    State2 = #parallel_state{branch_count = 2, completed = [1]},
    ?assertNot(is_enabled(sync, ModeWithTokens, State2)).

%% fire split test
fire_split_test() ->
    State = #parallel_state{branches = [{a, #{}}, {b, #{}}]},
    Result = fire(split, #{}, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap} = Result,
    ?assertEqual([split], maps:get(a_branch, ProduceMap)),
    ?assertEqual([split], maps:get(b_branch, ProduceMap)).

%% fire sync test
fire_sync_test() ->
    State = #parallel_state{branches = [{a, #{}}, {b, #{}}]},
    Mode = #{a_branch => [result_a], b_branch => [result_b], sync => []},
    Result = fire(sync, Mode, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap} = Result,
    ?assertEqual([], maps:get(sync, ProduceMap)),
    ?assertMatch([{all_completed, _}], maps:get('end', ProduceMap)).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Spec = new([branch1, branch2]),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := parallel_split}}, Spec).

%% has_all_branch_tokens test
has_all_branch_tokens_true_test() ->
    Mode = #{sync => [{branch_completed, 1}, {branch_completed, 2}]},
    ?assert(has_all_branch_tokens(Mode, 2)).

has_all_branch_tokens_false_test() ->
    Mode = #{sync => [{branch_completed, 1}]},
    ?assertNot(has_all_branch_tokens(Mode, 2)).

%% execute test (synchronous execution)
execute_success_test() ->
    Fun1 = fun(X) -> X + 1 end,
    Fun2 = fun(X) -> X * 2 end,
    ?assertMatch({ok, #{1 := 11, 2 := 20}}, execute([Fun1, Fun2], 10)).

execute_error_test() ->
    Fun1 = fun(_) -> ok end,
    Fun2 = fun(_) -> error(bad) end,
    Result = execute([Fun1, Fun2], input),
    ?assertMatch({error, {branch_error, 2, _, _, _}}, Result).

%% multiple branch test
new_5_branches_test() ->
    Spec = new([a, b, c, d, e]),
    ?assertEqual(5, maps:get(branch_count, maps:get(optional, Spec))),
    ?assertEqual(7, length(maps:get(places, Spec))).

%% options test
new_with_timeout_test() ->
    Spec = new([a, b], #{timeout => 5000}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(5000, maps:get(timeout, Optional)).

new_with_sync_type_test() ->
    Spec = new([a, b], #{sync_type => counter}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(counter, maps:get(sync_type, Optional)).

-endif.
