%% -*- erlang -*-
%%%% @doc strategy_thompson_sampling - Multi-armed bandit strategy.
%%
%% This strategy uses Thompson Sampling for the N-of-M pattern, treating
%% each branch as an armed bandit. It balances exploration and exploitation
%% to maximize long-term performance.
%%
%% <h3>Algorithm</h3>
%%
%% For each branch, maintain Beta distribution parameters (alpha, beta):
%% <ul>
%%   <li>alpha = 1 + success_count</li>
%%   <li>beta = 1 + failure_count</li>
%% </ul>
%%
%% Sample from each Beta distribution and select the highest value.
%%
%% @end
%% -------------------------------------------------------------------

-module(strategy_thompson_sampling).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Strategy behavior
-export([init/2]).
-export([should_complete/2]).
-export([on_branch_complete/2]).
-export([get_result/1]).
-export([select_branch/1]).
-export([record_outcome/3]).

%%====================================================================
%% Records
%%====================================================================

-record(bandit_arm, {
    branch_id :: pos_integer(),
    alpha :: pos_integer(),
    beta :: pos_integer(),
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer()
}).

-record(thompson_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    arms :: [#bandit_arm{}],
    completed :: [pos_integer()],
    results :: map()
}).

%%====================================================================
%% Types
%%====================================================================

-type thompson_state() :: #thompson_state{}.
-type bandit_arm() :: #bandit_arm{}.
-export_type([thompson_state/0, bandit_arm/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the Thompson sampling strategy.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(pos_integer(), pos_integer()) -> {ok, thompson_state()}.

init(N, M) when N =< M, N > 0, M > 0 ->
    %% Initialize arms for all branches
    Arms = [begin
        #bandit_arm{
            branch_id = I,
            alpha = 1,
            beta = 1,
            success_count = 0,
            failure_count = 0
        }
    end || I <- lists:seq(1, M)],
    {ok, #thompson_state{
        n = N,
        m = M,
        arms = Arms
    }}.

%%--------------------------------------------------------------------
%% @doc Determines if the pattern should complete.
%%
%% @end
%%--------------------------------------------------------------------
-spec should_complete(thompson_state(), map()) -> boolean().

should_complete(#thompson_state{n = N, completed = Completed}, _Context) ->
    length(Completed) >= N.

%%--------------------------------------------------------------------
%% @doc Called when a branch completes.
%%
%% Records the outcome for learning.
%%
%% @end
%%--------------------------------------------------------------------
-spec on_branch_complete(thompson_state(), {pos_integer(), term()}) ->
          thompson_state().

on_branch_complete(State = #thompson_state{completed = Completed, results = Results},
                   {BranchIndex, Result}) ->
    State#thompson_state{
        completed = [BranchIndex | Completed],
        results = maps:put(BranchIndex, Result, Results)
    }.

%%--------------------------------------------------------------------
%% @doc Gets the final result.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_result(thompson_state()) -> {ok, map()}.

get_result(#thompson_state{results = Results}) ->
    {ok, Results}.

%%--------------------------------------------------------------------
%% @doc Selects a branch using Thompson sampling.
%%
%% Samples from Beta distributions and returns the branch with highest value.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_branch(thompson_state()) -> pos_integer().

select_branch(#thompson_state{arms = Arms}) ->
    %% Sample from Beta distribution for each arm
    Samples = [{Arm#bandit_arm.branch_id, sample_beta(Arm#bandit_arm.alpha, Arm#bandit_arm.beta)}
               || Arm <- Arms],
    %% Select arm with highest sample
    {BranchId, _Sample} = lists:max(
        fun({_, A}, {_, B}) -> A > B end,
        Samples
    ),
    BranchId.

%%--------------------------------------------------------------------
%% @doc Records an outcome for a branch (success/failure).
%%
%% Updates the Beta distribution parameters.
%%
%% @end
%%--------------------------------------------------------------------
-spec record_outcome(thompson_state(), pos_integer(), success | failure) ->
          thompson_state().

record_outcome(State = #thompson_state{arms = Arms}, BranchId, Outcome) ->
    Arms1 = lists:map(
        fun(Arm) when Arm#bandit_arm.branch_id =:= BranchId ->
                case Outcome of
                    success ->
                        Arm#bandit_arm{
                            alpha = Arm#bandit_arm.alpha + 1,
                            success_count = Arm#bandit_arm.success_count + 1
                        };
                    failure ->
                        Arm#bandit_arm{
                            beta = Arm#bandit_arm.beta + 1,
                            failure_count = Arm#bandit_arm.failure_count + 1
                        }
                end;
           (Arm) ->
                Arm
        end,
        Arms
    ),
    State#thompson_state{arms = Arms1}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Samples from a Beta distribution using the algorithm:
%% X ~ Gamma(alpha, 1)
%% Y ~ Gamma(beta, 1)
%% return X / (X + Y)
-spec sample_beta(pos_integer(), pos_integer()) -> float().

sample_beta(Alpha, Beta) when Alpha > 0, Beta > 0 ->
    X = sample_gamma(Alpha, 1.0),
    Y = sample_gamma(Beta, 1.0),
    X / (X + Y).

%% @private
%% Samples from a Gamma distribution using Marsaglia and Tsang's method
-spec sample_gamma(number(), float()) -> float().

sample_gamma(Alpha, Theta) when Alpha > 0, Theta > 0 ->
    if
        Alpha >= 1 ->
            %% Marsaglia and Tsang's method
            D = Alpha - 1.0 / 3.0,
            C = 1.0 / math:sqrt(9.0 * D),
            sample_gamma_main(Alpha, Theta, D, C);
        true ->
            %% For alpha < 1, use: Gamma(alpha) = Gamma(alpha+1) * U^(1/alpha)
            U = rand:uniform(),
            X = sample_gamma_main(Alpha + 1.0, Theta, Alpha + 1.0 - 1.0/3.0,
                                 1.0 / math:sqrt(9.0 * (Alpha + 1.0 - 1.0/3.0))),
            X * math:pow(U, 1.0 / Alpha)
    end.

%% @private
-spec sample_gamma_main(float(), float(), float(), float()) -> float().

sample_gamma_main(_Alpha, Theta, D, C) ->
    sample_gamma_loop(Theta, D, C).

%% @private
-spec sample_gamma_loop(float(), float(), float()) -> float().

sample_gamma_loop(Theta, D, C) ->
    X = rand:normal(),
    V = 1.0 + C * X,
    if
        V =< 0.0 ->
            sample_gamma_loop(Theta, D, C);
        true ->
            V3 = V * V * V,
            U = rand:uniform(),
            if
                U < 1.0 - 0.0331 * (X * X) * (X * X) ->
                    (V3 * D * Theta);
                true ->
                    LogTerm = 0.5 * X * X + D * (1.0 - V3 + math:log(V3)),
                    case U < math:exp(LogTerm) of
                        true ->
                            (V3 * D * Theta);
                        false ->
                            sample_gamma_loop(Theta, D, C)
                    end
            end
    end.
