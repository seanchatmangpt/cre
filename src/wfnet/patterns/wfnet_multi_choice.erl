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
%% @doc Multi-Choice Workflow Pattern (WCP-07)
%%
%% Implements the Multi-Choice pattern (OR-split) where multiple
%% branches can be selected simultaneously based on runtime conditions.
%% Unlike exclusive choice which selects exactly one branch, multi-choice
%% can select any subset of branches (including none or all).
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Initial place (workflow entry)
%%   - evaluate: Place where branch conditions are evaluated
%%   - branch1, branch2, ..., branchN: Individual branch places
%%   - merge: Synchronization place for merging selected branches
%%   - 'end': Final place (workflow exit)
%%
%% Transitions:
%%   - evaluate: Evaluates branch conditions and produces tokens to selected branches
%%   - complete_branchN: Marks a branch as complete
%%   - merge: Merges all completed branches and produces to end
%%
%% Flow: start -> evaluate -> (branch1 OR branch2 OR ... OR branchN) -> merge -> end
%%
%% <h3>Selection Semantics</h3>
%%
%% The multi-choice pattern uses OR-split semantics:
%% - Each branch has an associated condition function
%% - The evaluate transition checks all conditions
%% - Tokens are produced to ALL branches whose conditions evaluate to true
%% - At least one branch must be selected (default behavior)
%% - All selected branches must complete before merge
%%
%% <h3>Configuration Options</h3>
%%
%% - allow_none: boolean() - If true, allows no branches to be selected
%% - selection_mode: all | some | one - Controls branch selection behavior
%%   - all: All branches must be selected (AND-split)
%%   - some: Any subset of branches can be selected (default)
%%   - one: Exactly one branch must be selected (exclusive choice)
%% - merge_mode: sync | async - How branches are merged
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Define branches with conditions
%% Branches = [
%%     {branch_a, #{condition => fun(Data) -> Data > 0 end}},
%%     {branch_b, #{condition => fun(Data) -> Data < 10 end}},
%%     {branch_c, #{condition => fun(Data) -> Data rem 2 =:= 0 end}}
%% ],
%%
%% %% Create a multi-choice workflow
%% {ok, WF} = wfnet_multi_choice:start_link(Branches).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_multi_choice:new(Branches, #{allow_none => false}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_multi_choice).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    execute/2,
    get_selected_branches/1,
    get_completed_branches/1,
    select_branches/2
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
-include("wfnet_multi_choice.hrl").

%% Types
-type branch() :: atom() | {atom(), branch_config()}.
-type branch_config() :: #{
    condition => function(),           %% Function that evaluates to boolean
    handler => function(),             %% Function to execute when branch is selected
    timeout => timeout()               %% Optional timeout for branch execution
}.
-type branches() :: [branch()].
-type branch_name() :: atom().
-type selection_mode() :: all | some | one.
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% Export types
-export_type([branch/0, branch_config/0, branches/0, branch_name/0,
             selection_mode/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a multi-choice workflow process.
%%
%% @param Branches List of branch definitions with conditions
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(branches()) -> {ok, pid()} | {error, term()}.
start_link(Branches) when is_list(Branches) ->
    gen_wfnet:start_link(?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Start a named multi-choice workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, branches()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Branches) ->
    gen_wfnet:start_link(Name, ?MODULE, Branches, []).

%%--------------------------------------------------------------------
%% @doc Create a multi-choice workflow specification.
%%
%% Returns a workflow spec map that can be used with other
%% composition operators.
%%
%% @param Branches List of branch definitions
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches()) -> wfnet_types:workflow_spec().
new(Branches) when is_list(Branches) ->
    new(Branches, #{}).

%%--------------------------------------------------------------------
%% @doc Create a multi-choice workflow specification with options.
%%
%% @param Branches List of branch definitions
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% Supported options:
%% - allow_none: boolean() - Allow no branches to be selected (default: false)
%% - selection_mode: all | some | one - Branch selection behavior (default: some)
%% - merge_mode: sync | async - How branches are merged (default: sync)
%%
%% @end
%%--------------------------------------------------------------------
-spec new(branches(), map()) -> wfnet_types:workflow_spec().
new(Branches, Options) when is_list(Branches), is_map(Options) ->
    case Branches of
        [] -> error(empty_branches);
        _ -> build_multi_choice_spec(Branches, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Execute a multi-choice selection synchronously.
%%
%% Evaluates all branch conditions and executes handlers for
%% branches whose conditions return true.
%%
%% @param Branches List of {BranchName, Config} tuples
%% @param InputData Input data passed to condition functions
%% @returns {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(branches(), term()) -> {ok, #{branch_name() => term()}} | {error, term()}.
execute(Branches, InputData) when is_list(Branches) ->
    Ref = make_ref(),
    Parent = self(),

    %% Normalize branches
    Normalized = normalize_branches(Branches),

    %% Evaluate conditions
    Selected = lists:filter(fun({_Name, Config}) ->
        case maps:get(condition, Config, fun(_) -> true end) of
            CondFun when is_function(CondFun) ->
                try CondFun(InputData) of
                    true -> true;
                    _ -> false
                catch
                    _:_ -> false
                end;
            _ ->
                true
        end
    end, Normalized),

    case Selected of
        [] ->
            {ok, #{}};
        _ ->
            %% Execute selected branches
            _Pids = lists:map(fun({Name, Config}) ->
                Handler = maps:get(handler, Config, fun(_) -> ok end),
                Timeout = maps:get(timeout, Config, 5000),
                spawn(fun() ->
                    try
                        Result = Handler(InputData),
                        Parent ! {Ref, {branch_complete, Name}, Result}
                    catch
                        Class:Reason:Stack ->
                            Parent ! {Ref, {branch_error, Name}, {Class, Reason, Stack}}
                    end,
                    timer:sleep(Timeout)  %% Ensure unique message ordering
                end)
            end, Selected),

            wait_all_branches(Ref, length(Selected), 30000, #{})
    end.

%%--------------------------------------------------------------------
%% @doc Get the list of selected branches from a running workflow.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, [branch_name()]}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_selected_branches(gen_wfnet:name()) -> {ok, [branch_name()]}.
get_selected_branches(Name) ->
    case gen_wfnet:usr_info(Name) of
        #multi_choice_state{selected = Selected} -> {ok, Selected};
        Other -> {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Get the list of completed branches from a running workflow.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, [branch_name()]}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_completed_branches(gen_wfnet:name()) -> {ok, [branch_name()]}.
get_completed_branches(Name) ->
    case gen_wfnet:usr_info(Name) of
        #multi_choice_state{completed = Completed} -> {ok, Completed};
        Other -> {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Manually select branches for execution.
%%
%% This allows dynamic branch selection based on external factors.
%% Can be called instead of relying on condition functions.
%%
%% @param Pid Process pid or registered name
%% @param BranchNames List of branch names to select
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec select_branches(gen_wfnet:name(), [branch_name()]) -> ok | {error, term()}.
select_branches(Name, BranchNames) when is_list(BranchNames) ->
    gen_wfnet:call(Name, {select_branches, BranchNames}).

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
-spec init(branches()) -> {ok, #multi_choice_state{}}.
init(Branches) ->
    Normalized = normalize_branches(Branches),
    State = #multi_choice_state{
        branches = maps:from_list(Normalized),
        branch_count = length(Normalized),
        selected = [],
        completed = [],
        selection_mode = some,
        allow_none = false,
        merge_mode = sync,
        results = #{}
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #multi_choice_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #multi_choice_state{}) -> boolean().
is_enabled(evaluate, #{start := [init]}, #multi_choice_state{}) ->
    true;
is_enabled(evaluate, #{start := [evaluate]}, #multi_choice_state{}) ->
    true;
is_enabled(complete_branch, #{branch_place := [_Token]}, #multi_choice_state{}) ->
    true;
is_enabled(merge, _Mode, #multi_choice_state{selected = Selected, completed = Completed}) ->
    %% Merge is enabled when all selected branches have completed
    length(Completed) =:= length(Selected) andalso length(Selected) > 0;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #multi_choice_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #multi_choice_state{}}.
fire(evaluate, #{start := [init]}, #multi_choice_state{branches = Branches} = State) ->
    %% Evaluate conditions and select branches
    Selected = maps:fold(fun(Name, Config, Acc) ->
        case maps:get(condition, Config, fun(_) -> true end) of
            CondFun when is_function(CondFun) ->
                case evaluate_condition(CondFun, init) of
                    true -> [Name | Acc];
                    false -> Acc
                end;
            _ ->
                [Name | Acc]
        end
    end, [], Branches),

    case Selected of
        [] when State#multi_choice_state.allow_none =:= false ->
            %% At least one branch must be selected - select first
            [First | _] = maps:keys(Branches),
            {produce, #{start => [], evaluate => [ready]}, State#multi_choice_state{selected = [First]}};
        _ ->
            %% Produce tokens to selected branch places
            ProduceMap = lists:foldl(fun(Name, Acc) ->
                Acc#{branch_place(Name) => [selected]}
            end, #{start => [], evaluate => [ready]}, Selected),
            {produce, ProduceMap, State#multi_choice_state{selected = Selected}}
    end;

fire(complete_branch, Mode, #multi_choice_state{completed = Completed} = State) ->
    %% Find which branch completed by checking mode
    CompletedBranch = find_completed_branch(Mode, State),
    NewCompleted = case lists:member(CompletedBranch, Completed) of
        true -> Completed;
        false -> [CompletedBranch | Completed]
    end,

    %% Check if all selected branches are complete
    NewState = State#multi_choice_state{completed = NewCompleted},
    case length(NewCompleted) =:= length(State#multi_choice_state.selected) of
        true ->
            %% All branches complete - produce to merge
            {produce, #{merge => [all_done]}, NewState};
        false ->
            %% More branches pending
            {produce, #{}, NewState}
    end;

fire(merge, #{merge := [all_done]}, #multi_choice_state{selected = Selected} = State) ->
    %% All branches merged - produce to end
    {produce, #{
        merge => [],
        'end' => [{multi_choice_complete, Selected}]
    }, State};

fire(_Transition, _Mode, _State) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Handle synchronous calls for custom operations.
%%
%% This is an optional callback that gen_wfnet delegates to.
%% To enable branch selection via gen_wfnet:call/2, uncomment the export.
%%
%% @end
%%--------------------------------------------------------------------
%% Note: The following handle_call/3 is an optional callback.
%% To enable custom call handling, uncomment the code below:
%%
%% -export([handle_call/3]).
%%
%% -spec handle_call(term(), {pid(), term()}, #multi_choice_state{}) ->
%%         {reply, term(), #multi_choice_state{}}.
%% handle_call({select_branches, BranchNames}, _From, State) ->
%%     #multi_choice_state{branches = Branches} = State,
%%     %% Validate branch names exist
%%     ValidBranches = maps:keys(Branches),
%%     Invalid = lists:filter(fun(N) -> not lists:member(N, ValidBranches) end, BranchNames),
%%
%%     case Invalid of
%%         [] ->
%%             NewState = State#multi_choice_state{selected = BranchNames},
%%             {reply, ok, NewState};
%%         _ ->
%%             {reply, {error, {invalid_branches, Invalid}}, State}
%%     end;
%% handle_call(_Request, _From, State) ->
%%     {reply, {error, unknown_request}, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build multi-choice workflow specification.
%%--------------------------------------------------------------------
build_multi_choice_spec(Branches, Options) ->
    Normalized = normalize_branches(Branches),
    N = length(Normalized),
    BranchNames = [Name || {Name, _} <- Normalized],

    %% Generate place names
    Start = start,
    End = 'end',
    EvaluatePlace = evaluate,
    BranchPlaces = [branch_place(Name) || Name <- BranchNames],
    MergePlace = merge,

    %% Generate transitions
    EvaluateTrans = evaluate,
    CompleteTrans = complete_branch,
    MergeTrans = merge,

    %% Build places list
    Places = [Start, End, EvaluatePlace, MergePlace | BranchPlaces],

    %% Build preset (transition -> input places)
    Preset = #{
        EvaluateTrans => [Start],
        CompleteTrans => BranchPlaces,
        MergeTrans => [MergePlace]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        EvaluateTrans => [EvaluatePlace | BranchPlaces],
        CompleteTrans => [MergePlace],
        MergeTrans => [End]
    },

    %% Get options
    SelectionMode = maps:get(selection_mode, Options, some),
    AllowNone = maps:get(allow_none, Options, false),
    MergeMode = maps:get(merge_mode, Options, sync),

    #{
        places => Places,
        transitions => [EvaluateTrans, CompleteTrans, MergeTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => multi_choice,
            branch_count => N,
            selection_mode => SelectionMode,
            allow_none => AllowNone,
            merge_mode => MergeMode
        }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize branches to {Name, Config} tuples.
%%--------------------------------------------------------------------
normalize_branches(Branches) ->
    lists:map(fun
        ({Name, Config}) when is_atom(Name), is_map(Config) -> {Name, Config};
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
%% @doc Evaluate a condition function safely.
%%--------------------------------------------------------------------
evaluate_condition(CondFun, Input) when is_function(CondFun) ->
    try
        CondFun(Input)
    catch
        _:_ -> false
    end;
evaluate_condition(_CondFun, _Input) ->
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc Find which branch completed by examining mode.
%%--------------------------------------------------------------------
find_completed_branch(Mode, #multi_choice_state{selected = Selected}) ->
    %% Look for a branch place with tokens
    lists:foldl(fun(Name, Acc) ->
        Place = branch_place(Name),
        case maps:get(Place, Mode, []) of
            [_] -> Name;
            _ -> Acc
        end
    end, hd(Selected), Selected).

%%--------------------------------------------------------------------
%% @private
%% @doc Wait for all branches to complete.
%%--------------------------------------------------------------------
wait_all_branches(_Ref, 0, _Timeout, Acc) ->
    {ok, Acc};
wait_all_branches(Ref, Remaining, Timeout, Acc) ->
    receive
        {Ref, {branch_complete, Name}, Result} ->
            wait_all_branches(Ref, Remaining - 1, Timeout, Acc#{Name => Result});
        {Ref, {branch_error, Name}, {Class, Reason, _Stack}} ->
            {error, {branch_error, Name, Class, Reason}}
    after Timeout ->
        {error, timeout}
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% normalize_branches test
normalize_branches_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], normalize_branches([a, b])),
    ?assertEqual([{a, #{cond => 1}}, {b, #{}}], normalize_branches([{a, #{cond => 1}}, b])).

%% branch_place test
branch_place_test() ->
    ?assertEqual(a_branch, branch_place(a)),
    ?assertEqual(my_branch_branch, branch_place(my_branch)).

%% new test
new_test() ->
    Spec = new([a, b, c]),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(3, length(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)).

%% error cases test
new_error_test() ->
    ?assertError(empty_branches, new([])).

%% init test
init_test() ->
    {ok, State} = init([a, b, c]),
    ?assertEqual(3, State#multi_choice_state.branch_count),
    ?assertEqual([], State#multi_choice_state.selected),
    ?assertEqual([], State#multi_choice_state.completed).

%% init_marking test
init_marking_start_test() ->
    State = #multi_choice_state{},
    ?assertEqual([init], init_marking(start, State)).

init_marking_other_test() ->
    State = #multi_choice_state{},
    ?assertEqual([], init_marking('end', State)),
    ?assertEqual([], init_marking(evaluate, State)).

%% is_enabled test
is_enabled_evaluate_test() ->
    State = #multi_choice_state{},
    Mode = #{start => [init]},
    ?assert(is_enabled(evaluate, Mode, State)).

is_enabled_merge_test() ->
    State = #multi_choice_state{selected = [a, b], completed = [a, b]},
    Mode = #{merge => [all_done]},
    ?assert(is_enabled(merge, Mode, State)),

    %% Not all completed
    State2 = State#multi_choice_state{completed = [a]},
    ?assertNot(is_enabled(merge, Mode, State2)).

%% fire evaluate test
fire_evaluate_test() ->
    Branches = #{
        a => #{condition => fun(_) -> true end},
        b => #{condition => fun(_) -> false end}
    },
    State = #multi_choice_state{branches = Branches, allow_none = false},
    Result = fire(evaluate, #{start => [init]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual([a], NewState#multi_choice_state.selected),
    ?assertEqual([selected], maps:get(a_branch, ProduceMap)).

%% fire evaluate with all selected test
fire_evaluate_all_test() ->
    Branches = #{
        a => #{condition => fun(_) -> true end},
        b => #{condition => fun(_) -> true end}
    },
    State = #multi_choice_state{branches = Branches},
    Result = fire(evaluate, #{start => [init]}, State),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual([a, b], lists:sort(NewState#multi_choice_state.selected)),
    ?assertEqual([selected], maps:get(a_branch, ProduceMap)),
    ?assertEqual([selected], maps:get(b_branch, ProduceMap)).

%% fire merge test
fire_merge_test() ->
    State = #multi_choice_state{selected = [a, b]},
    Result = fire(merge, #{merge => [all_done]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _} = Result,
    ?assertEqual([], maps:get(merge, ProduceMap)),
    ?assertMatch([{multi_choice_complete, [a, b]}], maps:get('end', ProduceMap)).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Spec = new([branch1, branch2]),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := multi_choice}}, Spec).

%% new with options test
new_with_allow_none_test() ->
    Spec = new([a, b], #{allow_none => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(allow_none, Optional)).

new_with_selection_mode_test() ->
    Spec = new([a, b], #{selection_mode => all}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(all, maps:get(selection_mode, Optional)).

%% execute test
execute_success_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 0 end, handler => fun(X) -> X * 2 end}},
        {b, #{condition => fun(X) -> X < 10 end, handler => fun(X) -> X + 1 end}}
    ],
    ?assertMatch({ok, #{a := 10, b := 6}}, execute(Branches, 5)).

execute_single_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 100 end}},
        {b, #{condition => fun(X) -> X < 10 end, handler => fun(X) -> X + 1 end}}
    ],
    ?assertMatch({ok, #{b := 6}}, execute(Branches, 5)).

execute_none_test() ->
    Branches = [
        {a, #{condition => fun(X) -> X > 100 end}}
    ],
    ?assertMatch({ok, #{}}, execute(Branches, 5)).

%% evaluate_condition test
evaluate_condition_true_test() ->
    Cond = fun(X) -> X > 0 end,
    ?assert(evaluate_condition(Cond, 5)).

evaluate_condition_false_test() ->
    Cond = fun(X) -> X > 10 end,
    ?assertNot(evaluate_condition(Cond, 5)).

evaluate_condition_crash_test() ->
    Cond = fun(_) -> error(bad) end,
    ?assertNot(evaluate_condition(Cond, input)).

%% find_completed_branch test
find_completed_branch_test() ->
    State = #multi_choice_state{selected = [a, b]},
    Mode = #{a_branch => [selected], b_branch => []},
    ?assertEqual(a, find_completed_branch(Mode, State)).

%% workflow spec with merge_mode test
new_with_merge_mode_test() ->
    Spec = new([a, b], #{merge_mode => async}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(async, maps:get(merge_mode, Optional)).

%% multiple branch test
new_5_branches_test() ->
    Spec = new([a, b, c, d, e]),
    ?assertEqual(5, maps:get(branch_count, maps:get(optional, Spec))),
    ?assertEqual(9, length(maps:get(places, Spec))).

%% preset and postset structure test
preset_postset_test() ->
    Spec = new([a, b]),
    Preset = maps:get(preset, Spec),
    Postset = maps:get(postset, Spec),

    %% Check preset
    ?assertEqual([start], maps:get(evaluate, Preset)),
    ?assertEqual([a_branch, b_branch], lists:sort(maps:get(complete_branch, Preset))),
    ?assertEqual([merge], maps:get(merge, Preset)),

    %% Check postset
    ?assertEqual([evaluate, a_branch, b_branch], lists:sort(maps:get(evaluate, Postset))),
    ?assertEqual([merge], maps:get(complete_branch, Postset)),
    ?assertEqual(['end'], maps:get(merge, Postset)).

-endif.
