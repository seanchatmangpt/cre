%%% @doc WF Case Runner - Gen Server for Case Execution
%%%
%%% This module implements a gen_server for running a single workflow case.
%%% Each case runner maintains execution state, handles reduction steps,
%%% processes effects, manages timers, and tracks case lifecycle.
%%%
%%% The runner executes bytecode in quanta (N steps per timer tick) to
%%% avoid monopolizing the Erlang scheduler and to allow interleaving
%%% multiple cases.
%%%
%%% @end
-module(wf_case_runner).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    start_link/3,
    reduce/2,
    signal/2,
    effect_result/3,
    cancel/1,
    get_status/1,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export_type([
    case_id/0,
    case_state/0,
    case_status/0,
    reduction_quantum/0
]).

%%% TYPES ===================================================================

-type case_id() :: atom() | {atom(), term()}.

-type reduction_quantum() :: non_neg_integer().

%% Case state maintained by the gen_server
-type case_state() :: #{
    case_id := case_id(),
    exec_state := wf_vm:exec_state(),
    user_ctx := wf_term:context(),
    effects := #{term() => {pending | completed | failed, term()}},
    effect_timeouts := #{term() => reference()},
    committed := non_neg_integer(),
    cancelled := boolean(),
    status := running | halted | error | cancelled,
    started_at := integer(),
    quantum := reduction_quantum(),
    periodic_timer := reference() | undefined
}.

%% Status response
-type case_status() :: #{
    case_id := case_id(),
    status := running | halted | error | cancelled,
    steps := non_neg_integer(),
    pc := non_neg_integer(),
    stack_depth := non_neg_integer(),
    effects := #{term() => {pending | completed | failed, term()}},
    uptime_ms := non_neg_integer()
}.

-define(DEFAULT_QUANTUM, 100).
-define(DEFAULT_PERIODIC_MS, 10).
-define(DEFAULT_EFFECT_TIMEOUT_MS, 30000).

%%% API =====================================================================

%% @doc Start a case runner with a compiled pattern and initial context.
-spec start_link(
    CaseId :: case_id(),
    Compiled :: wf_compile:compiled()
) -> {ok, pid()} | {error, term()}.
start_link(CaseId, Compiled) ->
    start_link(CaseId, Compiled, #{}).

%% @doc Start a case runner with a compiled pattern, context, and options.
-spec start_link(
    CaseId :: case_id(),
    Compiled :: wf_compile:compiled(),
    InitCtx :: wf_term:context()
) -> {ok, pid()} | {error, term()}.
start_link(CaseId, Compiled, InitCtx) ->
    gen_server:start_link(?MODULE, {CaseId, Compiled, InitCtx}, []).

%% @doc Request N reduction steps to be executed.
-spec reduce(Pid :: pid(), NumSteps :: non_neg_integer()) -> ok.
reduce(Pid, NumSteps) ->
    gen_server:cast(Pid, {reduce, NumSteps}).

%% @doc Send a signal to the case (for deferred choice, external events).
-spec signal(Pid :: pid(), Signal :: term()) -> ok.
signal(Pid, Signal) ->
    gen_server:cast(Pid, {signal, Signal}).

%% @doc Provide the result of an effect execution.
-spec effect_result(Pid :: pid(), EffectId :: term(), Result :: term()) -> ok.
effect_result(Pid, EffectId, Result) ->
    gen_server:cast(Pid, {effect_result, EffectId, Result}).

%% @doc Cancel the case execution.
-spec cancel(Pid :: pid()) -> ok.
cancel(Pid) ->
    gen_server:cast(Pid, {cancel}).

%% @doc Get the current status of the case.
-spec get_status(Pid :: pid()) -> {ok, case_status()} | {error, term()}.
get_status(Pid) ->
    gen_server:call(Pid, {get_status}).

%% @doc Stop the case runner gracefully.
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%% GEN_SERVER CALLBACKS ====================================================

%% @private
-spec init({case_id(), wf_compile:compiled(), wf_term:context()}) ->
    {ok, case_state()} | {stop, term()}.
init({CaseId, Compiled, InitCtx}) ->
    %% Initialize execution state from compiled pattern
    ExecState = wf_exec:exec_init(Compiled, InitCtx),

    %% Start periodic reduction timer
    PeriodicTimer = erlang:send_after(?DEFAULT_PERIODIC_MS, self(), {periodic_reduce}),

    State = #{
        case_id => CaseId,
        exec_state => ExecState,
        user_ctx => InitCtx,
        effects => #{},
        effect_timeouts => #{},
        committed => 0,
        cancelled => false,
        status => running,
        started_at => erlang:monotonic_time(millisecond),
        quantum => ?DEFAULT_QUANTUM,
        periodic_timer => PeriodicTimer
    },

    {ok, State}.

%% @private
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: case_state()) ->
    {reply, Reply :: term(), NewState :: case_state()} |
    {noreply, NewState :: case_state()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: case_state()}.

handle_call({get_status}, _From, State = #{
    case_id := CaseId,
    exec_state := ExecState,
    status := Status,
    effects := Effects,
    started_at := StartedAt
}) ->
    ExecStatus = wf_exec:exec_status(ExecState),
    Uptime = erlang:monotonic_time(millisecond) - StartedAt,

    StatusReply = #{
        case_id => CaseId,
        status => Status,
        steps => maps:get(steps, ExecStatus),
        pc => maps:get(pc, ExecStatus),
        stack_depth => maps:get(stack_depth, ExecStatus),
        effects => Effects,
        uptime_ms => Uptime
    },

    {reply, {ok, StatusReply}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request :: term(), State :: case_state()) ->
    {noreply, NewState :: case_state()} |
    {stop, Reason :: term(), NewState :: case_state()}.

handle_cast({reduce, NumSteps}, State = #{status := running}) ->
    NewState = do_reduce(NumSteps, State),
    {noreply, NewState};

handle_cast({reduce, _NumSteps}, State) ->
    %% Ignore reduction requests if not running
    {noreply, State};

handle_cast({signal, Signal}, State = #{exec_state := ExecState}) ->
    %% Add signal to context
    Ctx = wf_vm:exec_ctx(ExecState),
    Signals = maps:get(signals, Ctx, []),
    NewCtx = maps:put(signals, [Signal | Signals], Ctx),
    NewExecState = wf_vm:exec_set_ctx(ExecState, NewCtx),
    NewState = maps:put(exec_state, NewExecState, State),
    {noreply, NewState};

handle_cast({effect_result, EffectId, Result}, State = #{
    effects := Effects,
    effect_timeouts := Timeouts
}) ->
    %% Cancel timeout timer if it exists
    NewTimeouts = case maps:get(EffectId, Timeouts, undefined) of
        undefined -> Timeouts;
        TRef ->
            erlang:cancel_timer(TRef),
            maps:remove(EffectId, Timeouts)
    end,

    %% Update effect status
    NewEffects = maps:put(EffectId, {completed, Result}, Effects),

    %% Store result in context
    ExecState = maps:get(exec_state, State),
    Ctx = wf_vm:exec_ctx(ExecState),
    Results = maps:get(results, Ctx, #{}),
    NewResults = maps:put(EffectId, Result, Results),
    NewCtx = maps:put(results, NewResults, Ctx),
    NewExecState = wf_vm:exec_set_ctx(ExecState, NewCtx),

    NewState = State#{
        exec_state => NewExecState,
        effects => NewEffects,
        effect_timeouts => NewTimeouts
    },

    %% Resume execution after effect completion
    FinalState = do_reduce(1, NewState),
    {noreply, FinalState};

handle_cast({cancel}, State = #{exec_state := ExecState}) ->
    %% Set cancel flag on root scope
    Cancel = wf_vm:exec_cancel(ExecState),
    NewCancel = maps:put(root, true, Cancel),
    NewExecState = wf_vm:exec_set_cancel(ExecState, NewCancel),
    NewState = State#{
        exec_state => NewExecState,
        cancelled => true,
        status => cancelled
    },
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info :: term(), State :: case_state()) ->
    {noreply, NewState :: case_state()} |
    {stop, Reason :: term(), NewState :: case_state()}.

handle_info({periodic_reduce}, State = #{
    status := running,
    quantum := Quantum
}) ->
    %% Execute one quantum of reduction steps
    NewState = do_reduce(Quantum, State),

    %% Schedule next periodic reduction
    PeriodicTimer = erlang:send_after(?DEFAULT_PERIODIC_MS, self(), {periodic_reduce}),
    FinalState = maps:put(periodic_timer, PeriodicTimer, NewState),

    {noreply, FinalState};

handle_info({periodic_reduce}, State) ->
    %% Case is not running, reschedule anyway
    PeriodicTimer = erlang:send_after(?DEFAULT_PERIODIC_MS, self(), {periodic_reduce}),
    NewState = maps:put(periodic_timer, PeriodicTimer, State),
    {noreply, NewState};

handle_info({effect_timeout, EffectId}, State = #{effects := Effects}) ->
    %% Mark effect as failed due to timeout
    NewEffects = maps:put(EffectId, {failed, timeout}, Effects),
    NewState = State#{
        effects => NewEffects,
        status => error
    },
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason :: term(), State :: case_state()) -> ok.
terminate(_Reason, #{periodic_timer := PeriodicTimer}) when PeriodicTimer =/= undefined ->
    erlang:cancel_timer(PeriodicTimer),
    ok;
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(OldVsn :: term(), State :: case_state(), Extra :: term()) ->
    {ok, NewState :: case_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% INTERNAL FUNCTIONS ======================================================

%% @doc Execute N reduction steps and update state.
-spec do_reduce(NumSteps :: non_neg_integer(), State :: case_state()) -> case_state().
do_reduce(0, State) ->
    State;
do_reduce(_NumSteps, State = #{status := Status}) when Status =/= running ->
    State;
do_reduce(NumSteps, State = #{exec_state := ExecState}) ->
    case wf_exec:exec_step(ExecState) of
        {continue, NewExecState} ->
            NewState = maps:put(exec_state, NewExecState, State),
            do_reduce(NumSteps - 1, NewState);

        {yield, EffectSpec, NewExecState} ->
            %% Effect yielded - store it and wait for result
            EffectId = make_effect_id(EffectSpec),

            %% Set up effect timeout
            TimeoutRef = erlang:send_after(
                ?DEFAULT_EFFECT_TIMEOUT_MS,
                self(),
                {effect_timeout, EffectId}
            ),

            Effects = maps:get(effects, State),
            NewEffects = maps:put(EffectId, {pending, EffectSpec}, Effects),

            Timeouts = maps:get(effect_timeouts, State),
            NewTimeouts = maps:put(EffectId, TimeoutRef, Timeouts),

            %% TODO: Trigger effect execution (send to effect executor)
            %% For now, effects must be completed via effect_result/3

            State#{
                exec_state => NewExecState,
                effects => NewEffects,
                effect_timeouts => NewTimeouts
            };

        {halt, ok, NewExecState} ->
            %% Normal termination
            State#{
                exec_state => NewExecState,
                status => halted
            };

        {error, Reason, NewExecState} ->
            %% Error termination
            logger:error("Case execution error: ~p", [Reason]),
            State#{
                exec_state => NewExecState,
                status => error
            }
    end.

%% @doc Generate a unique effect ID from an effect spec.
-spec make_effect_id(EffectSpec :: term()) -> term().
make_effect_id(EffectSpec) ->
    %% If effect spec has an ID, use it; otherwise generate one
    case EffectSpec of
        {effect, _Type, _Payload, Id} -> Id;
        _ -> erlang:unique_integer([positive])
    end.
