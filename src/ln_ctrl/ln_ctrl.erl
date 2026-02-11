%%% @doc ln_ctrl: Public API for line execution runtime
%%%
%%% Manages the lifecycle of execution "cases" with cancellation, budgets,
%%% and determinism. Delegates to gen_server case runner for each case.
%%%
%%% @end
-module(ln_ctrl).

%% Public API
-export([
    new_case/3,
    signal/2,
    cancel/1,
    cancel_scope/2,
    await/2,
    status/1,
    trace/3,
    validate/2
]).

%% Type exports
-export_type([
    case_id/0,
    case_options/0,
    case_status/0,
    trace_event/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-type case_id() :: atom() | reference().

-type case_options() :: #{
    budget => ln_ctrl_budget:budget(),
    scheduler => ln_ctrl_sched:policy(),
    timeout_ms => non_neg_integer(),
    trace_level => none | basic | full
}.

-type case_status() :: #{
    state := running | halted | cancelled | error,
    steps := non_neg_integer(),
    current_activity := atom() | undefined,
    pc := non_neg_integer(),
    stack_depth := non_neg_integer(),
    effects_issued := non_neg_integer(),
    budget_status := ln_ctrl_budget:budget_status()
}.

-type trace_event() :: {
    seq,
    non_neg_integer(),
    atom(),
    atom(),
    non_neg_integer(),
    [atom()],
    boolean()
}.

%%% API =====================================================================

%% @doc Start execution of a new case.
%%
%% Creates a gen_server case runner for the given process term with
%% initial context and options. Returns a case ID for reference.
%%
%% @end
-spec new_case(
    ProcTerm :: wf_term:wf_term(),
    InitCtx :: wf_term:context(),
    Options :: case_options()
) -> {ok, case_id()} | {error, term()}.
new_case(ProcTerm, InitCtx, Options) ->
    %% Validate pattern
    case wf_term:is_valid(ProcTerm) of
        false ->
            {error, {invalid_pattern, ProcTerm}};
        true ->
            %% Compile pattern
            case wf_compile:compile(ProcTerm) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Compiled} ->
                    %% Start case runner
                    case ln_ctrl_case_runner:start_link(Compiled, InitCtx, Options) of
                        {ok, Pid} ->
                            {ok, Pid};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc Send an inbound signal to a running case.
%%
%% Signals are typically responses from external effects or user inputs.
%% The signal is queued and processed by the case runner.
%%
%% @end
-spec signal(case_id(), any()) -> ok | {error, term()}.
signal(CaseID, Msg) ->
    case is_pid(CaseID) of
        true ->
            case ln_ctrl_case_runner:signal(CaseID, Msg) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {error, invalid_case_id}
    end.

%% @doc Cancel a running case (entire case).
%%
%% Sends cancel signal to root scope. Case transitions to cancelled state.
%%
%% @end
-spec cancel(case_id()) -> ok | {error, term()}.
cancel(CaseID) ->
    case is_pid(CaseID) of
        true ->
            ln_ctrl_case_runner:cancel(CaseID);
        false ->
            {error, invalid_case_id}
    end.

%% @doc Cancel a specific scope within a case.
%%
%% Cancels only the activities within the named scope.
%% Case continues execution outside the scope.
%%
%% @end
-spec cancel_scope(case_id(), atom()) -> ok | {error, term()}.
cancel_scope(CaseID, ScopeID) ->
    case is_pid(CaseID) of
        true ->
            ln_ctrl_case_runner:cancel_scope(CaseID, ScopeID);
        false ->
            {error, invalid_case_id}
    end.

%% @doc Wait for a case to complete.
%%
%% Blocks until the case halts (successfully or with error) or timeout.
%% Returns the final context on success, error reason on failure.
%%
%% @end
-spec await(case_id(), non_neg_integer() | infinity) ->
    {ok, wf_term:context()} | {error, term()} | timeout.
await(CaseID, Timeout) ->
    case is_pid(CaseID) of
        true ->
            ln_ctrl_case_runner:await(CaseID, Timeout);
        false ->
            {error, invalid_case_id}
    end.

%% @doc Get current status of a running case.
%%
%% Returns snapshot of execution state: PC, stack depth, current activity,
%% and budget status.
%%
%% @end
-spec status(case_id()) -> case_status() | {error, term()}.
status(CaseID) ->
    case is_pid(CaseID) of
        true ->
            ln_ctrl_case_runner:status(CaseID);
        false ->
            {error, invalid_case_id}
    end.

%% @doc Extract trace events from a case execution.
%%
%% Returns trace events between sequence numbers FromSeq and ToSeq.
%% Useful for debugging and test analysis.
%%
%% @end
-spec trace(case_id(), non_neg_integer(), non_neg_integer()) ->
    [trace_event()] | {error, term()}.
trace(CaseID, FromSeq, ToSeq) ->
    case is_pid(CaseID) of
        true ->
            ln_ctrl_case_runner:trace(CaseID, FromSeq, ToSeq);
        false ->
            {error, invalid_case_id}
    end.

%% @doc Validate a pattern before execution.
%%
%% Compiles and checks pattern for correctness. Returns ok or list of issues.
%%
%% @end
-spec validate(wf_term:wf_term(), case_options()) ->
    ok | {error, term()}.
validate(ProcTerm, _Options) ->
    case wf_term:is_valid(ProcTerm) of
        false ->
            {error, {invalid_pattern, ProcTerm}};
        true ->
            case wf_compile:compile(ProcTerm) of
                {ok, _Compiled} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%% TESTS ===================================================================

basic_test_() ->
    [
        ?_assert(wf_term:is_valid(wf_term:task(simple_task, fun(_) -> {ok, #{}} end)))
    ].
