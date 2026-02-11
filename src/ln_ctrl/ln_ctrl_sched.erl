%%% @doc ln_ctrl_sched: Scheduler policies and replay
%%%
%%% Deterministic mode: always pick first enabled choice.
%%% Nondeterministic mode: record every non-det choice.
%%% Replay mode: use recorded choice log, produce identical trace.
%%%
%%% @end
-module(ln_ctrl_sched).

-export([
    new_deterministic/0,
    new_nondeterministic/0,
    new_replay/1,
    apply_policy/3
]).

-export_type([
    policy/0,
    choice_log/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-record(deterministic, {
    name = deterministic :: atom()
}).

-record(nondeterministic, {
    name = nondeterministic :: atom(),
    choice_log = [] :: choice_log()
}).

-record(replay, {
    name = replay :: atom(),
    choice_log :: choice_log(),
    index = 0 :: non_neg_integer()
}).

-type policy() ::
      #deterministic{}
    | #nondeterministic{}
    | #replay{}.

-type choice_log() :: [{non_neg_integer(), non_neg_integer()}].
    %% [{branch_index, choice_point_id}]

%%% API =====================================================================

%% @doc Create a deterministic scheduling policy.
%%
%% Always selects the first enabled choice. Produces stable, reproducible
%% execution traces.
%%
%% @end
-spec new_deterministic() -> policy().
new_deterministic() ->
    #deterministic{}.

%% @doc Create a nondeterministic scheduling policy.
%%
%% Records every choice made during execution. Can be converted to a
%% replay policy to reproduce the same trace.
%%
%% @end
-spec new_nondeterministic() -> policy().
new_nondeterministic() ->
    #nondeterministic{}.

%% @doc Create a replay scheduling policy.
%%
%% Uses the provided choice log to make the same decisions as a previous run.
%%
%% @end
-spec new_replay(ChoiceLog :: choice_log()) -> policy().
new_replay(ChoiceLog) when is_list(ChoiceLog) ->
    #replay{choice_log = ChoiceLog}.

%% @doc Apply the policy to make a choice.
%%
%% Given a list of possible branches, returns {ChosenBranch, UpdatedPolicy}.
%%
%% @end
-spec apply_policy(
    Policy :: policy(),
    State :: wf_vm:exec_state(),
    Choices :: [any()]
) -> {ChosenBranch :: any(), UpdatedPolicy :: policy()}.

%% Deterministic: always pick first
apply_policy(Policy = #deterministic{}, _State, Choices) when is_list(Choices), length(Choices) > 0 ->
    {hd(Choices), Policy};

%% Nondeterministic: pick first but record it
apply_policy(Policy = #nondeterministic{}, _State, Choices) when is_list(Choices), length(Choices) > 0 ->
    ChosenIdx = 0,  % First for now
    ChosenBranch = lists:nth(ChosenIdx + 1, Choices),
    ChoicePoint = erlang:unique_integer([positive]),
    NewChoiceLog = Policy#nondeterministic.choice_log ++ [{ChosenIdx, ChoicePoint}],
    UpdatedPolicy = Policy#nondeterministic{choice_log = NewChoiceLog},
    {ChosenBranch, UpdatedPolicy};

%% Replay: use recorded choices
apply_policy(Policy = #replay{}, _State, Choices) when is_list(Choices), length(Choices) > 0 ->
    Index = Policy#replay.index,
    ChoiceLog = Policy#replay.choice_log,
    case lists:nth(Index + 1, ChoiceLog, undefined) of
        {ChosenIdx, _ChoicePoint} ->
            case lists:nth(ChosenIdx + 1, Choices, undefined) of
                undefined ->
                    %% Fallback to first
                    {hd(Choices), Policy#replay{index = Index + 1}};
                ChosenBranch ->
                    {ChosenBranch, Policy#replay{index = Index + 1}}
            end;
        undefined ->
            %% No more recorded choices, use first
            {hd(Choices), Policy#replay{index = Index + 1}}
    end.

%%% TESTS ===================================================================

deterministic_test_() ->
    Policy = new_deterministic(),
    Choices = [branch1, branch2, branch3],
    {Branch, _} = apply_policy(Policy, undefined, Choices),
    [
        ?_assertEqual(branch1, Branch)
    ].

nondeterministic_test_() ->
    Policy = new_nondeterministic(),
    Choices = [branch1, branch2, branch3],
    {Branch1, Policy1} = apply_policy(Policy, undefined, Choices),
    {Branch2, Policy2} = apply_policy(Policy1, undefined, Choices),
    [
        ?_assertEqual(branch1, Branch1),
        ?_assertEqual(branch1, Branch2),
        ?_assert(length(Policy2#nondeterministic.choice_log) == 2)
    ].

replay_test_() ->
    ChoiceLog = [{0, 1}, {0, 2}],
    Policy = new_replay(ChoiceLog),
    Choices = [branch1, branch2, branch3],
    {Branch1, Policy1} = apply_policy(Policy, undefined, Choices),
    {Branch2, _Policy2} = apply_policy(Policy1, undefined, Choices),
    [
        ?_assertEqual(branch1, Branch1),
        ?_assertEqual(branch1, Branch2)
    ].
