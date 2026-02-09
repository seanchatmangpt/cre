%% -*- erlang -*-
%% @doc UCB1 Strategy for Branch Selection
%%
%% Upper Confidence Bound algorithm for multi-armed bandits.
%%
%% @end

-module(strategy_ucb).

%%====================================================================
%% Records
%%====================================================================

-record(ucb_state, {
    n :: pos_integer(),         %% Number of arms
    m :: pos_integer(),         %% Number of arms to select (1 = standard UCB1)
    arms :: [ucb_arm()],
    total_pulls = 0 :: non_neg_integer(),
    c = 1.41 :: float()         %% Exploration parameter (sqrt(2))
}).

-record(ucb_arm, {
    id :: pos_integer(),
    pulls = 0 :: non_neg_integer(),
    total_reward = 0.0 :: float(),
    avg_reward = 0.0 :: float()
}).

%%====================================================================
%% Exports
%%====================================================================

-export([new/2, select_arm/1, record_result/3, get_stats/1]).

-type ucb_state() :: #ucb_state{}.
-type ucb_arm() :: #ucb_arm{}.

%%====================================================================
%% API
%%====================================================================

%% @doc Create new UCB1 strategy state.
-spec new(pos_integer(), map()) -> ucb_state().
new(N, Options) when is_integer(N), N > 0 ->
    C = maps:get(c, Options, 1.41),
    M = maps:get(m, Options, 1),
    Arms = [#ucb_arm{id = I} || I <- lists:seq(1, N)],
    #ucb_state{n = N, m = M, arms = Arms, c = C}.

%% @doc Select arm using UCB1 algorithm.
-spec select_arm(ucb_state()) -> {pos_integer(), ucb_state()}.
select_arm(#ucb_state{arms = Arms, total_pulls = Total, c = C} = State) ->
    {ArmId, UpdatedArms} = select_arm_ucb(Arms, Total, C),
    {ArmId, State#ucb_state{arms = UpdatedArms}}.

%% @doc Record result of arm pull.
-spec record_result(ucb_state(), pos_integer(), float()) -> ucb_state().
record_result(#ucb_state{arms = Arms} = State, ArmId, Reward) when is_float(Reward) ->
    UpdatedArms = lists:map(fun(Arm) ->
        case Arm#ucb_arm.id =:= ArmId of
            true ->
                NewPulls = Arm#ucb_arm.pulls + 1,
                NewTotal = Arm#ucb_arm.total_reward + Reward,
                Arm#ucb_arm{
                    pulls = NewPulls,
                    total_reward = NewTotal,
                    avg_reward = NewTotal / NewPulls
                };
            false ->
                Arm
        end
    end, Arms),
    State#ucb_state{arms = UpdatedArms, total_pulls = State#ucb_state.total_pulls + 1}.

%% @doc Get current statistics.
-spec get_stats(ucb_state()) -> map().
get_stats(#ucb_state{arms = Arms, total_pulls = Total}) ->
    ArmStats = [{A#ucb_arm.id, #{
        pulls => A#ucb_arm.pulls,
        avg_reward => A#ucb_arm.avg_reward
    }} || A <- Arms],
    #{
        arms => maps:from_list(ArmStats),
        total_pulls => Total
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec select_arm_ucb([ucb_arm()], non_neg_integer(), float()) -> {pos_integer(), [ucb_arm()]}.
select_arm_ucb(Arms, Total, C) ->
    %% Find arm with highest UCB value
    {BestArm, _UCB} = lists:foldl(fun(Arm, {BestAcc, BestUCBAcc}) ->
        UCB = calculate_ucb(Arm, Total, C),
        case UCB > BestUCBAcc of
            true -> {Arm, UCB};
            false -> {BestAcc, BestUCBAcc}
        end
    end, {hd(Arms), -1.0}, Arms),
    {BestArm#ucb_arm.id, Arms}.

%% @private
-spec calculate_ucb(ucb_arm(), non_neg_integer(), float()) -> float() | infinity.
calculate_ucb(#ucb_arm{pulls = 0}, _Total, _C) ->
    infinity;
calculate_ucb(#ucb_arm{avg_reward = Avg, pulls = Pulls}, Total, C) ->
    Avg + C * math:sqrt(math:log(Total + 1) / Pulls).
