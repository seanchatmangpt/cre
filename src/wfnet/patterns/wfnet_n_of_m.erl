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
%% @doc N-out-of-M Merge Workflow Pattern (WCP-10)
%%
%% Implements the N-out-of-M merge pattern where execution continues
%% after N out of M branches complete. This is a generalization of
%% discriminator (N=1) and synchronous merge (N=M).
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Entry point (optional)
%%   - branch1, branch2, ..., branchM: Concurrent branch places
%%   - waiting: Place tracking completion count
%%   - 'end': Final place after N branches complete
%%
%% Transitions:
%%   - complete_branch: Tracks individual branch completion
%%   - merge_n: Fires when N branches have completed
%%
%% Flow: Wait for N of M branches -> merge_n -> end
%%
%% <h3>N-out-of-M Semantics</h3>
%%
%% - Waits for exactly N branches to complete (where 1 <= N <= M)
%% - Completions are tracked as they arrive
%% - When the Nth branch completes, merge fires immediately
%% - Remaining branch completions are discarded
%% - Can be reset for reuse
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a 2-out-of-5 merge (wait for any 2 of 5 branches)
%% {ok, WF} = wfnet_n_of_m:start_link(5, #{n => 2}).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_n_of_m:new([a, b, c, d, e], #{n => 3}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_n_of_m).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/2,
    start_link/3,
    new/2,
    new/3,
    get_completion_count/1,
    get_n_value/1,
    reset/1
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
-type n_value() :: pos_integer().
-type m_value() :: pos_integer().
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(n_of_m_state, {
    branches :: branches(),
    m :: m_value(),
    n :: n_value(),
    completed = [] :: [branch_name()],
    completion_count = 0 :: non_neg_integer(),
    merged = false :: boolean()
}).

%% Export types
-export_type([branch/0, branches/0, branch_name/0, n_value/0, m_value/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start an N-out-of-M merge workflow process.
%%
%% @param M Total number of branches
%% @param Options Configuration map with n key
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(m_value(), map()) -> {ok, pid()} | {error, term()}.
start_link(M, Options) when is_integer(M), M > 0, is_map(Options) ->
    N = maps:get(n, Options, 1),
    Branches = [list_to_atom("branch_" ++ integer_to_list(I)) || I <- lists:seq(1, M)],
    gen_wfnet:start_link(?MODULE, {Branches, N}, []).

%%--------------------------------------------------------------------
%% @doc Start a named N-out-of-M merge workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, m_value(), map()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, M, Options) ->
    N = maps:get(n, Options, 1),
    Branches = [list_to_atom("branch_" ++ integer_to_list(I)) || I <- lists:seq(1, M)],
    gen_wfnet:start_link(Name, ?MODULE, {Branches, N}, []).

%%--------------------------------------------------------------------
%% @doc Create an N-out-of-M merge workflow specification.
%%
%% @param M Total number of branches (generates branch1..branchM)
%% @param N Number of branches needed to proceed
%% @param Options Additional configuration options
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(m_value(), n_value(), map()) -> wfnet_types:workflow_spec().
new(M, N, Options) when is_integer(M), M > 0, is_integer(N), N > 0, N =< M, is_map(Options) ->
    Branches = [list_to_atom("branch_" ++ integer_to_list(I)) || I <- lists:seq(1, M)],
    new(Branches, Options#{n => N}).

%%--------------------------------------------------------------------
%% @doc Create an N-out-of-M merge workflow specification.
%%
%% @param Branches List of branch atoms
%% @param Options Configuration map with required n key
%% @returns workflow_spec()
%%
%% Supported options:
%% - n: pos_integer() (required) - Number of branches needed to proceed
%% - auto_reset: boolean() - If true, automatically resets after merge
%% - discard_remaining: boolean() - If true, discards tokens from remaining branches
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches(), map()) -> wfnet_types:workflow_spec().
new(Branches, Options) when is_list(Branches), is_map(Options) ->
    case Branches of
        [] -> error(empty_branches);
        _ ->
            N = maps:get(n, Options, 1),
            M = length(Branches),
            case N > M of
                true -> error({n_greater_than_m, {N, M}});
                false -> build_n_of_m_spec(Branches, Options)
            end
    end.

%%--------------------------------------------------------------------
%% @doc Get the current completion count.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, Count} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_completion_count(gen_wfnet:name()) -> {ok, non_neg_integer()} | {error, term()}.
get_completion_count(Name) ->
    case gen_wfnet:usr_info(Name) of
        #n_of_m_state{completion_count = Count} -> {ok, Count};
        Other -> {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Get the N value (required completions).
%%
%% @param Pid Process pid or registered name
%% @returns {ok, N} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_n_value(gen_wfnet:name()) -> {ok, n_value()} | {error, term()}.
get_n_value(Name) ->
    case gen_wfnet:usr_info(Name) of
        #n_of_m_state{n = N} -> {ok, N};
        Other -> {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Reset the N-out-of-M merge for reuse.
%%
%% Clears the completion state and allows waiting for N completions again.
%%
%% @param Pid Process pid or registered name
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(gen_wfnet:name()) -> ok | {error, term()}.
reset(Name) ->
    gen_wfnet:call(Name, reset_n_of_m).

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
-spec init({branches(), n_value()}) -> {ok, #n_of_m_state{}}.
init({Branches, N}) when is_list(Branches), is_integer(N), N > 0 ->
    M = length(Branches),
    Normalized = normalize_branches(Branches),
    State = #n_of_m_state{
        branches = Normalized,
        m = M,
        n = N,
        completed = [],
        completion_count = 0,
        merged = false
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #n_of_m_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #n_of_m_state{}) -> boolean().
is_enabled(complete_branch, _Mode, #n_of_m_state{merged = false}) ->
    true;
is_enabled(complete_branch, _Mode, #n_of_m_state{merged = true}) ->
    false;
is_enabled(merge_n, _Mode, #n_of_m_state{n = N, completion_count = Count, merged = false}) ->
    Count >= N;
is_enabled(merge_n, _Mode, #n_of_m_state{}) ->
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
-spec fire(atom(), wfnet_types:mode(), #n_of_m_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #n_of_m_state{}}.
fire(complete_branch, Mode, #n_of_m_state{branches = Branches, completed = Completed, completion_count = Count} = State) ->
    %% Find a branch that has completed but isn't yet tracked
    case find_new_completion(Branches, Mode, Completed) of
        {ok, Branch} ->
            BranchPlace = branch_place(Branch),
            NewCompleted = [Branch | Completed],
            NewCount = Count + 1,
            NewState = State#n_of_m_state{
                completed = NewCompleted,
                completion_count = NewCount
            },
            %% Produce to tracking place
            {produce, #{
                BranchPlace => [],
                waiting => [{branch_completed, Branch}]
            }, NewState};
        error ->
            abort
    end;

fire(merge_n, Mode, #n_of_m_state{completed = Completed, n = N} = State) ->
    %% N branches have completed - merge
    %% Take first N completions
    MergedBranches = lists:sublist(Completed, N),
    NewState = State#n_of_m_state{merged = true},
    {produce, #{
        waiting => [],
        'end' => [{n_complete, N, MergedBranches}]
    }, NewState};

fire(reset, _Mode, State) ->
    %% Reset for reuse
    NewState = State#n_of_m_state{
        completed = [],
        completion_count = 0,
        merged = false
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
%% @doc Build N-out-of-M workflow specification.
%%--------------------------------------------------------------------
build_n_of_m_spec(Branches, Options) ->
    Normalized = normalize_branches(Branches),
    M = length(Normalized),
    N = maps:get(n, Options, 1),
    BranchNames = [Name || {Name, _} <- Normalized],

    %% Validate N
    true = N =< M orelse error({n_greater_than_m, {N, M}}),
    true = N > 0 orelse error({n_must_be_positive, N}),

    %% Generate place names
    Start = start,
    End = 'end',
    WaitingPlace = waiting,
    BranchPlaces = [branch_place(Name) || Name <- BranchNames],

    %% Generate transitions
    CompleteTrans = complete_branch,
    MergeNTrans = merge_n,
    ResetTrans = reset,

    %% Build places list
    Places = [Start, End, WaitingPlace | BranchPlaces],

    %% Build preset (transition -> input places)
    Preset = #{
        CompleteTrans => [WaitingPlace | BranchPlaces],
        MergeNTrans => [WaitingPlace],
        ResetTrans => [End]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        CompleteTrans => [WaitingPlace],
        MergeNTrans => [End],
        ResetTrans => [WaitingPlace]
    },

    %% Get options
    AutoReset = maps:get(auto_reset, Options, false),
    DiscardRemaining = maps:get(discard_remaining, Options, true),

    BaseSpec = #{
        places => Places,
        transitions => [CompleteTrans, MergeNTrans, ResetTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => n_of_m,
            m => M,
            n => N,
            auto_reset => AutoReset,
            discard_remaining => DiscardRemaining
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
%% @doc Find a newly completed branch not yet in the completed list.
%%--------------------------------------------------------------------
find_new_completion(Branches, Mode, Completed) ->
    lists:foldl(fun({Branch, _Opts}, Acc) ->
        case Acc of
            {ok, _} -> Acc;
            error ->
                AlreadyCompleted = lists:member(Branch, Completed),
                case AlreadyCompleted of
                    true -> error;
                    false ->
                        Place = branch_place(Branch),
                        case maps:get(Place, Mode, []) of
                            [] -> error;
                            _ -> {ok, Branch}
                        end
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
    Spec = new([a, b, c], #{n => 2}),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(3, length(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)).

%% error cases test
new_error_test() ->
    ?assertError(empty_branches, new([], #{n => 1})),
    ?assertError({n_greater_than_m, _}, new([a, b], #{n => 5})).

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
    {ok, State} = init({[a, b, c], 2}),
    ?assertEqual(3, State#n_of_m_state.m),
    ?assertEqual(2, State#n_of_m_state.n),
    ?assertEqual(0, State#n_of_m_state.completion_count),
    ?assertEqual(false, State#n_of_m_state.merged).

%% is_enabled test
is_enabled_complete_test() ->
    State = #n_of_m_state{n = 2, completion_count = 1, merged = false},
    Mode = #{a_branch => [done], waiting => []},
    ?assert(is_enabled(complete_branch, Mode, State)),

    %% After merge, complete should be disabled
    State2 = State#n_of_m_state{merged = true},
    ?assertNot(is_enabled(complete_branch, Mode, State2)).

is_enabled_merge_n_test() ->
    State = #n_of_m_state{n = 2, completion_count = 2, merged = false},
    ?assert(is_enabled(merge_n, #{waiting => [a, b]}, State)),

    %% Not enough completions
    State2 = State#n_of_m_state{completion_count = 1},
    ?assertNot(is_enabled(merge_n, #{waiting => [a]}, State2)).

%% fire complete_branch test
fire_complete_test() ->
    Branches = [{a, #{}}, {b, #{}}, {c, #{}}],
    State = #n_of_m_state{branches = Branches, completed = [], completion_count = 0},
    Mode = #{a_branch => [done], b_branch => [done], waiting => []},

    Result = fire(complete_branch, Mode, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual(1, NewState#n_of_m_state.completion_count),
    ?assertEqual([a], NewState#n_of_m_state.completed).

%% fire merge_n test
fire_merge_n_test() ->
    State = #n_of_m_state{n = 2, completed = [a, b], completion_count = 2},
    Result = fire(merge_n, #{waiting => [a, b]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual(true, NewState#n_of_m_state.merged),
    ?assertMatch([{n_complete, 2, _}], maps:get('end', ProduceMap)).

%% fire reset test
fire_reset_test() ->
    State = #n_of_m_state{n = 2, completed = [a, b], completion_count = 2, merged = true},
    Result = fire(reset, #{}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, _ProduceMap, NewState} = Result,
    ?assertEqual([], NewState#n_of_m_state.completed),
    ?assertEqual(0, NewState#n_of_m_state.completion_count),
    ?assertEqual(false, NewState#n_of_m_state.merged).

%% find_new_completion test
find_new_completion_test() ->
    Branches = [{a, #{}}, {b, #{}}, {c, #{}}],
    Mode = #{a_branch => [done], b_branch => [done], c_branch => []},

    ?assertEqual({ok, a}, find_new_completion(Branches, Mode, [])),
    ?assertEqual({ok, b}, find_new_completion(Branches, Mode, [a])),
    ?assertEqual(error, find_new_completion(Branches, Mode, [a, b])),
    ?assertEqual(error, find_new_completion(Branches, Mode, [a, b, c])).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Spec = new([a, b, c], #{n => 2}),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := n_of_m}}, Spec),
    ?assertEqual(3, maps:get(m, maps:get(optional, Spec))),
    ?assertEqual(2, maps:get(n, maps:get(optional, Spec))).

%% discriminator edge case (N=1)
new_discriminator_test() ->
    Spec = new([a, b, c], #{n => 1}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(1, maps:get(n, Optional)).

%% sync merge edge case (N=M)
new_sync_merge_test() ->
    Spec = new([a, b, c], #{n => 3}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(3, maps:get(n, Optional)),
    ?assertEqual(3, maps:get(m, Optional)).

-endif.
