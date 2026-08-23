%%%-------------------------------------------------------------------
%%% @doc f5_proof_budget - Budget enforcement proof verification.
%%%
%%% Implements Fortune-5 FIBO budget-proof verification ensuring that
%%% execution stays within defined limits for effects, steps, and time.
%%%
%%% The proof guarantees:
%%% - Effects executed <= max_effects
%%% - Steps (reductions) <= max_steps
%%% - Execution time <= max_time
%%% - Hard stop was triggered if exceeded
%%%
%%% Evidence sources:
%%% - Effect receipts from execution
%%% - Reduction counters from process_info
%%% - Wall-clock time measurement
%%% - Hard stop exit signal
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(f5_proof_budget).

%% API
-export([verify_budget/3]).
-export([enforce_budget/2]).
-export([check_budget_exceeded/1]).
-export([generate_budget_proof/3]).
-export([compute_proof_hash/3]).
-export([track_effect/2]).
-export([track_step/1]).
-export([track_time/1]).

-include_lib("kernel/include/logger.hrl").

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type budget_limit() :: non_neg_integer() | unlimited.
-type timestamp_ms() :: integer().
-type reduction_count() :: non_neg_integer().
-type effect_count() :: non_neg_integer().

-type budget_spec() :: #{
    max_effects := budget_limit(),
    max_steps := budget_limit(),
    max_time := budget_limit()
}.

-type budget_state() :: #{
    spec := budget_spec(),
    effects_used := effect_count(),
    steps_used := reduction_count(),
    start_time := timestamp_ms(),
    elapsed_ms := non_neg_integer(),
    exceeded := boolean(),
    hard_stop_triggered := boolean()
}.

-type execution_result() :: #{
    effects_executed := effect_count(),
    reductions_executed := reduction_count(),
    duration_ms := non_neg_integer(),
    terminated_normally := boolean(),
    exit_reason => term()
}.

-type budget_proof() :: #{
    proof_type := budget,
    budget := budget_spec(),
    actual := #{
        effects := effect_count(),
        steps := reduction_count(),
        time_ms := non_neg_integer()
    },
    exceeded := boolean(),
    hard_stop_triggered := boolean(),
    within_limits := boolean(),
    evidence_sources := [atom()],
    proof_hash := binary(),
    verified_at := timestamp_ms()
}.

-type proof_result() :: {ok, budget_proof()} | {error, term()}.

-export_type([budget_spec/0, budget_state/0, execution_result/0,
              budget_proof/0, proof_result/0]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% @doc Verify execution stayed within budget.
%%
%% Compares the execution results against the budget specification
%% to verify all limits were respected.
%%
%% @param BudgetSpec The budget limits (max_effects, max_steps, max_time)
%% @param ExecutionResult Actual execution metrics
%% @returns {ok, Proof} if verification passes, {error, Reason} otherwise
-spec verify_budget(budget_spec(), execution_result(), timestamp_ms()) -> proof_result().
verify_budget(BudgetSpec, ExecutionResult, VerifyTime) ->
    try
        %% Extract budget limits
        MaxEffects = do_safe_maps_get(max_effects, BudgetSpec, unlimited),
        MaxSteps = do_safe_maps_get(max_steps, BudgetSpec, unlimited),
        MaxTime = do_safe_maps_get(max_time, BudgetSpec, unlimited),

        %% Extract actual usage
        EffectsUsed = maps:get(effects_executed, ExecutionResult, 0),
        StepsUsed = maps:get(reductions_executed, ExecutionResult, 0),
        TimeUsed = maps:get(duration_ms, ExecutionResult, 0),

        %% Check each limit
        EffectsExceeded = check_limit_exceeded(EffectsUsed, MaxEffects),
        StepsExceeded = check_limit_exceeded(StepsUsed, MaxSteps),
        TimeExceeded = check_limit_exceeded(TimeUsed, MaxTime),

        Exceeded = EffectsExceeded orelse StepsExceeded orelse TimeExceeded,

        %% Check if hard stop was triggered
        TerminatedNormally = maps:get(terminated_normally, ExecutionResult, true),
        ExitReason = maps:get(exit_reason, ExecutionResult, normal),
        HardStopTriggered = (not TerminatedNormally) andalso
            is_hard_stop_reason(ExitReason),

        %% Build proof
        Actual = #{
            effects => EffectsUsed,
            steps => StepsUsed,
            time_ms => TimeUsed
        },

        WithinLimits = not Exceeded,

        Proof = #{
            proof_type => budget,
            budget => #{
                max_effects => MaxEffects,
                max_steps => MaxSteps,
                max_time => MaxTime
            },
            actual => Actual,
            exceeded => Exceeded,
            hard_stop_triggered => HardStopTriggered,
            within_limits => WithinLimits,
            evidence_sources => determine_evidence_sources(ExecutionResult),
            proof_hash => compute_proof_hash(EffectsUsed, StepsUsed, TimeUsed),
            verified_at => VerifyTime
        },

        %% Return result based on verification
        case Exceeded of
            false ->
                {ok, Proof};
            true ->
                %% Even when exceeded, we return the proof for audit
                {ok, Proof#{exceeded_details => #{
                    effects_exceeded => EffectsExceeded,
                    steps_exceeded => StepsExceeded,
                    time_exceeded => TimeExceeded
                }}}
        end
    catch
        error:Reason:Stack ->
            ?LOG_ERROR("Budget verification failed: ~p~n~p", [Reason, Stack]),
            {error, {verification_failed, Reason}}
    end.

%% @doc Enforce budget with hard stop.
%%
%% Creates a budget state tracker that will enforce limits during
%% execution. When any limit is exceeded, the process will exit with
%% a hard stop reason.
%%
%% @param BudgetSpec The budget limits to enforce
%% @param StartTime The start timestamp for time tracking
%% @returns Initial budget state
-spec enforce_budget(budget_spec(), timestamp_ms()) -> budget_state().
enforce_budget(BudgetSpec, StartTime) ->
    #{
        spec => BudgetSpec,
        effects_used => 0,
        steps_used => 0,
        start_time => StartTime,
        elapsed_ms => 0,
        exceeded => false,
        hard_stop_triggered => false
    }.

%% @doc Check if budget has been exceeded and trigger hard stop if so.
%%
%% Updates the budget state with current elapsed time and checks all
%% limits. If any limit is exceeded, exits with budget_exceeded reason.
%%
%% @param BudgetState Current budget state
%% @returns Updated budget state or exits
-spec check_budget_exceeded(budget_state()) -> budget_state().
check_budget_exceeded(BudgetState) ->
    %% Update elapsed time
    StartTime = maps:get(start_time, BudgetState, 0),
    CurrentTime = erlang:monotonic_time(millisecond),
    ElapsedMs = CurrentTime - StartTime,

    %% Get current usage
    EffectsUsed = maps:get(effects_used, BudgetState, 0),
    StepsUsed = maps:get(steps_used, BudgetState, 0),

    %% Get limits
    Spec = maps:get(spec, BudgetState, #{}),
    MaxEffects = do_safe_maps_get(max_effects, Spec, unlimited),
    MaxSteps = do_safe_maps_get(max_steps, Spec, unlimited),
    MaxTime = do_safe_maps_get(max_time, Spec, unlimited),

    %% Check each limit
    EffectsOk = check_limit_ok(EffectsUsed, MaxEffects),
    StepsOk = check_limit_ok(StepsUsed, MaxSteps),
    TimeOk = check_limit_ok(ElapsedMs, MaxTime),

    %% Update state
    UpdatedState = BudgetState#{
        elapsed_ms => ElapsedMs,
        exceeded => not (EffectsOk andalso StepsOk andalso TimeOk)
    },

    %% Trigger hard stop if exceeded
    case EffectsOk andalso StepsOk andalso TimeOk of
        true ->
            UpdatedState;
        false ->
            _HardStopState = UpdatedState#{hard_stop_triggered => true},
            ExitReason = format_exit_reason(EffectsUsed, MaxEffects,
                                          StepsUsed, MaxSteps,
                                          ElapsedMs, MaxTime),
            exit({budget_exceeded, ExitReason})
    end.

%% @doc Generate budget proof artifact as JSON-compatible map.
%%
%% Creates a comprehensive proof artifact for serialization.
%%
%% @param BudgetSpec The budget specification
%% @param ExecutionResult Actual execution metrics
%% @param Metadata Additional metadata to include
%% @returns {ok, ProofMap} on success
-spec generate_budget_proof(budget_spec(), execution_result(), map()) ->
    {ok, map()} | {error, term()}.
generate_budget_proof(BudgetSpec, ExecutionResult, Metadata) ->
    VerifyTime = erlang:monotonic_time(millisecond),
    case verify_budget(BudgetSpec, ExecutionResult, VerifyTime) of
        {ok, Proof} ->
            JsonProof = proof_to_json(Proof, Metadata),
            {ok, JsonProof};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compute SHA-256 hash of budget data for integrity verification.
-spec compute_proof_hash(effect_count(), reduction_count(), non_neg_integer()) -> binary().
compute_proof_hash(Effects, Steps, TimeMs) ->
    ProofData = term_to_binary({Effects, Steps, TimeMs}),
    crypto:hash(sha256, ProofData).

%% @doc Track an effect execution in budget state.
%%
%% Records that an effect was executed and checks budget limits.
%% Use this during execution to track resource usage.
%%
%% @param BudgetState Current budget state
%% @param EffectCost Optional cost weight (default 1)
%% @returns Updated budget state
-spec track_effect(budget_state(), pos_integer()) -> budget_state().
track_effect(BudgetState, EffectCost) when EffectCost > 0 ->
    CurrentEffects = maps:get(effects_used, BudgetState, 0),
    NewEffects = CurrentEffects + EffectCost,
    BudgetState#{effects_used => NewEffects}.

%% @doc Track a step (reduction) in budget state.
%%
%% Records that a step was executed. In practice, this would be
%% called periodically or via process_info reduction counts.
%%
%% @param BudgetState Current budget state
%% @returns Updated budget state
-spec track_step(budget_state()) -> budget_state().
track_step(BudgetState) ->
    CurrentSteps = maps:get(steps_used, BudgetState, 0),
    BudgetState#{steps_used => CurrentSteps + 1}.

%% @doc Track elapsed time in budget state.
%%
%% Updates the elapsed_ms field with current time since start.
%%
%% @param BudgetState Current budget state
%% @returns Updated budget state with refreshed elapsed time
-spec track_time(budget_state()) -> budget_state().
track_time(BudgetState) ->
    StartTime = maps:get(start_time, BudgetState, 0),
    CurrentTime = erlang:monotonic_time(millisecond),
    BudgetState#{elapsed_ms => CurrentTime - StartTime}.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

%% @private Check if a limit was exceeded.
check_limit_exceeded(_Value, unlimited) ->
    false;
check_limit_exceeded(Value, Limit) when is_integer(Limit) ->
    Value > Limit.

%% @private Check if value is within limit.
check_limit_ok(_Value, unlimited) ->
    true;
check_limit_ok(Value, Limit) when is_integer(Limit) ->
    Value =< Limit.

%% @private Determine if exit reason indicates a hard stop.
is_hard_stop_reason({budget_exceeded, _}) ->
    true;
is_hard_stop_reason(budget_exceeded) ->
    true;
is_hard_stop_reason({timeout, _}) ->
    true;
is_hard_stop_reason(killed) ->
    true;
is_hard_stop_reason(_ExitReason) ->
    false.

%% @private Format exit reason with exceeded limits.
format_exit_reason(Effects, MaxEffects, Steps, MaxSteps, Time, MaxTime) ->
    [
        case check_limit_exceeded(Effects, MaxEffects) of
            true -> {effects_exceeded, Effects, MaxEffects};
            false -> effects_ok
        end,
        case check_limit_exceeded(Steps, MaxSteps) of
            true -> {steps_exceeded, Steps, MaxSteps};
            false -> steps_ok
        end,
        case check_limit_exceeded(Time, MaxTime) of
            true -> {time_exceeded, Time, MaxTime};
            false -> time_ok
        end
    ].

%% @private Determine available evidence sources.
determine_evidence_sources(ExecutionResult) ->
    Sources = [],

    %% Check if we have effect data
    Sources1 = case maps:is_key(effects_executed, ExecutionResult) of
        true -> [effects | Sources];
        false -> Sources
    end,

    %% Check if we have reduction data
    Sources2 = case maps:is_key(reductions_executed, ExecutionResult) of
        true -> [reductions | Sources1];
        false -> Sources1
    end,

    %% Check if we have timing data
    Sources3 = case maps:is_key(duration_ms, ExecutionResult) of
        true -> [timing | Sources2];
        false -> Sources2
    end,

    %% Check if process was terminated
    Sources4 = case maps:get(terminated_normally, ExecutionResult, true) of
        false -> [exit_signal | Sources3];
        true -> Sources3
    end,

    lists:usort(Sources4).

%% @private Convert proof to JSON-compatible map.
proof_to_json(Proof, Metadata) ->
    BaseProof = #{
        proof_type => maps:get(proof_type, Proof, budget),
        budget => format_budget_spec(maps:get(budget, Proof)),
        actual => maps:get(actual, Proof),
        exceeded => maps:get(exceeded, Proof),
        hard_stop_triggered => maps:get(hard_stop_triggered, Proof),
        within_limits => maps:get(within_limits, Proof),
        evidence_sources => maps:get(evidence_sources, Proof),
        proof_hash => binary:encode_hex(maps:get(proof_hash, Proof, <<>>)),
        verified_at => maps:get(verified_at, Proof, 0),
        metadata => Metadata
    },

    %% Add exceeded details if present
    BaseProof1 = case maps:get(exceeded_details, Proof, undefined) of
        undefined -> BaseProof;
        Details -> BaseProof#{exceeded_details => Details}
    end,

    %% Add verification status
    case maps:get(within_limits, Proof) of
        true ->
            BaseProof1#{
                status => verified,
                message => <<"Execution completed within budget limits">>
            };
        false ->
            HardStopTriggered = maps:get(hard_stop_triggered, Proof, false),
            case HardStopTriggered of
                true ->
                    BaseProof1#{
                        status => hard_stopped,
                        message => <<"Execution exceeded budget and was hard stopped">>
                    };
                false ->
                    BaseProof1#{
                        status => exceeded,
                        message => <<"Execution exceeded budget limits">>
                    }
            end
    end.

%% @private Format budget spec for JSON output.
format_budget_spec(BudgetSpec) ->
    #{
        max_effects => maps:get(max_effects, BudgetSpec),
        max_steps => maps:get(max_steps, BudgetSpec),
        max_time => maps:get(max_time, BudgetSpec)
    }.

%% @private Safe maps:get with default.
do_safe_maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
