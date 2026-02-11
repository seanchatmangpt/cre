%%% @doc ln_ctrl_case_runner: gen_server for case execution
%%%
%%% Holds exec_state and steps through bytecode using wf_exec.
%%% Handles inbound signals, cancel requests, and effect results.
%%%
%%% @end
-module(ln_ctrl_case_runner).

-behaviour(gen_server).

%% Public API
-export([
    start_link/3,
    signal/2,
    cancel/1,
    cancel_scope/2,
    await/2,
    status/1,
    trace/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-record(state, {
    exec_state :: wf_vm:exec_state(),
    compiled :: wf_compile:compiled(),
    case_status :: running | halted | cancelled | error,
    error_reason :: term() | undefined,
    budget :: ln_ctrl_budget:budget(),
    scheduler :: ln_ctrl_sched:policy(),
    effects_queue :: [term()],
    effect_results :: #{atom() => any()},
    receipt_log :: [ln_ctrl_receipt:receipt()],
    trace_level :: none | basic | full,
    start_time :: integer(),
    awaiting_clients :: [from()]
}).

-type from() :: {pid(), reference()}.

%%% PUBLIC API ===============================================================

%% @doc Start a new case runner.
-spec start_link(
    Compiled :: wf_compile:compiled(),
    InitCtx :: wf_term:context(),
    Options :: ln_ctrl:case_options()
) -> {ok, pid()} | {error, term()}.
start_link(Compiled, InitCtx, Options) ->
    gen_server:start_link(?MODULE, {Compiled, InitCtx, Options}, []).

%% @doc Send a signal to the case.
-spec signal(pid(), any()) -> ok | {error, term()}.
signal(Pid, Msg) ->
    gen_server:call(Pid, {signal, Msg}).

%% @doc Cancel the entire case.
-spec cancel(pid()) -> ok | {error, term()}.
cancel(Pid) ->
    gen_server:call(Pid, cancel_case).

%% @doc Cancel a specific scope.
-spec cancel_scope(pid(), atom()) -> ok | {error, term()}.
cancel_scope(Pid, ScopeID) ->
    gen_server:call(Pid, {cancel_scope, ScopeID}).

%% @doc Wait for case completion.
-spec await(pid(), non_neg_integer() | infinity) ->
    {ok, wf_term:context()} | {error, term()} | timeout.
await(Pid, Timeout) ->
    gen_server:call(Pid, await_completion, Timeout).

%% @doc Get case status.
-spec status(pid()) -> ln_ctrl:case_status() | {error, term()}.
status(Pid) ->
    gen_server:call(Pid, get_status).

%% @doc Get trace events.
-spec trace(pid(), non_neg_integer(), non_neg_integer()) ->
    [ln_ctrl:trace_event()] | {error, term()}.
trace(Pid, FromSeq, ToSeq) ->
    gen_server:call(Pid, {get_trace, FromSeq, ToSeq}).

%%% GEN_SERVER CALLBACKS =====================================================

-spec init({wf_compile:compiled(), wf_term:context(), ln_ctrl:case_options()}) ->
    {ok, #state{}} | {stop, term()}.
init({Compiled, InitCtx, Options}) ->
    %% Initialize execution state
    ExecState = wf_exec:exec_init(Compiled, InitCtx),

    %% Get options
    Budget = maps:get(budget, Options, ln_ctrl_budget:new_budget(unlimited, unlimited, unlimited)),
    Scheduler = maps:get(scheduler, Options, ln_ctrl_sched:new_deterministic()),
    TraceLevel = maps:get(trace_level, Options, basic),

    State = #state{
        exec_state = ExecState,
        compiled = Compiled,
        case_status = running,
        error_reason = undefined,
        budget = Budget,
        scheduler = Scheduler,
        effects_queue = [],
        effect_results = #{},
        receipt_log = [],
        trace_level = TraceLevel,
        start_time = erlang:monotonic_time(millisecond),
        awaiting_clients = []
    },

    %% Schedule first reduction
    gen_server:cast(self(), execute_step),
    {ok, State}.

%% Handle reduction loop
-spec handle_cast(atom(), #state{}) -> {noreply, #state{}} | {stop, normal, #state{}}.
handle_cast(execute_step, State) ->
    case State#state.case_status of
        running ->
            case step_execution(State) of
                {NewState, continue} ->
                    %% Schedule next step
                    gen_server:cast(self(), execute_step),
                    {noreply, NewState};
                {NewState, halt} ->
                    %% Notify waiting clients
                    notify_awaiting(NewState),
                    {noreply, NewState}
            end;
        _ ->
            {noreply, State}
    end.

%% Handle calls
-spec handle_call(any(), from(), #state{}) ->
    {reply, any(), #state{}} | {noreply, #state{}}.

handle_call({signal, _Msg}, _From, State) ->
    %% Queue signal for processing
    {reply, ok, State};

handle_call(cancel_case, _From, State) ->
    %% Mark root scope as cancelled
    NewState = cancel_root_scope(State),
    {reply, ok, NewState};

handle_call({cancel_scope, ScopeID}, _From, State) ->
    %% Mark specific scope as cancelled
    NewState = mark_scope_cancelled(State, ScopeID),
    {reply, ok, NewState};

handle_call(await_completion, From, State) ->
    case State#state.case_status of
        running ->
            %% Add to waiting list
            NewAwaiters = [From | State#state.awaiting_clients],
            {noreply, State#state{awaiting_clients = NewAwaiters}};
        halted ->
            ExecCtx = wf_vm:exec_ctx(State#state.exec_state),
            {reply, {ok, ExecCtx}, State};
        cancelled ->
            {reply, {error, cancelled}, State};
        error ->
            {reply, {error, State#state.error_reason}, State}
    end;

handle_call(get_status, _From, State) ->
    Status = build_status(State),
    {reply, Status, State};

handle_call({get_trace, FromSeq, ToSeq}, _From, State) ->
    ExecState = State#state.exec_state,
    Trace = wf_vm:exec_trace(ExecState),
    FilteredTrace = filter_trace(Trace, FromSeq, ToSeq),
    {reply, FilteredTrace, State};

handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% Handle info
-spec handle_info(any(), #state{}) -> {noreply, #state{}} | {stop, normal, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% Terminate
-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%%% INTERNAL FUNCTIONS ======================================================

%% Execute one step of the bytecode
-spec step_execution(#state{}) -> {#state{}, continue | halt}.
step_execution(State) ->
    ExecState = State#state.exec_state,

    case wf_exec:exec_step(ExecState) of
        {continue, NewExecState} ->
            {State#state{exec_state = NewExecState}, continue};
        {yield, Spec, NewExecState} ->
            %% Effect issued, execute it and collect result
            NewState = execute_effect(State#state{exec_state = NewExecState}, Spec),
            {NewState, continue};
        {halt, ok, NewExecState} ->
            NewState = State#state{
                exec_state = NewExecState,
                case_status = halted
            },
            {NewState, halt};
        {error, Reason, NewExecState} ->
            NewState = State#state{
                exec_state = NewExecState,
                case_status = error,
                error_reason = Reason
            },
            {NewState, halt}
    end.

%% Execute an effect and update state
-spec execute_effect(#state{}, term()) -> #state{}.
execute_effect(State, Spec) ->
    %% Extract effect details
    EffectID = extract_effect_id(Spec),
    Connector = extract_connector(Spec),
    Params = extract_params(Spec),

    %% Check budget before executing
    Budget = State#state.budget,
    case ln_ctrl_budget:check_effect(Budget, Params) of
        {ok, NewBudget} ->
            %% Execute effect with idempotency check
            StartTime = erlang:monotonic_time(millisecond),
            Handle = ln_receipt_effect:start_effect(EffectID, Connector, Params),

            case ln_receipt_effect:idempotent_receipt(EffectID, Params) of
                {ok, CachedReceipt} ->
                    %% Idempotent: return cached result
                    Result = maps:get(result, CachedReceipt, ok),
                    update_with_effect_result(State, EffectID, Result, NewBudget);
                not_found ->
                    %% Execute effect via connector
                    case execute_connector_call(Connector, Params) of
                        {ok, Result} ->
                            Latency = erlang:monotonic_time(millisecond) - StartTime,
                            {ok, Receipt} = ln_receipt_effect:complete(Handle, Result, Latency),
                            ln_receipt_andon:set_green(State#state.receipt_log),
                            update_with_effect_result(State, EffectID, Result, NewBudget);
                        {error, Reason} ->
                            Latency = erlang:monotonic_time(millisecond) - StartTime,
                            {ok, Receipt} = ln_receipt_effect:failed(Handle, Reason, Latency),
                            ln_receipt_andon:set_red(State#state.receipt_log, Reason),
                            State#state{
                                case_status = error,
                                error_reason = {effect_failed, EffectID, Reason},
                                budget = NewBudget
                            }
                    end
            end;
        {budget_exceeded, Details, NewBudget} ->
            %% Budget exceeded, halt execution
            ln_receipt_andon:set_red(State#state.receipt_log, Details),
            State#state{
                case_status = error,
                error_reason = {budget_exceeded, Details},
                budget = NewBudget
            }
    end.

%% Update state with effect result
-spec update_with_effect_result(#state{}, atom(), term(), ln_ctrl_budget:budget()) -> #state{}.
update_with_effect_result(State, EffectID, Result, NewBudget) ->
    NewEffectResults = maps:put(EffectID, Result, State#state.effect_results),
    ExecState = State#state.exec_state,
    %% Update execution context with result
    ExecCtx = wf_vm:exec_ctx(ExecState),
    NewExecCtx = maps:put(EffectID, Result, ExecCtx),
    NewExecState = wf_vm:exec_set_ctx(ExecState, NewExecCtx),
    State#state{
        exec_state = NewExecState,
        effect_results = NewEffectResults,
        budget = NewBudget
    }.

%% Execute connector call
-spec execute_connector_call(atom(), map()) -> {ok, term()} | {error, term()}.
execute_connector_call(Connector, Params) ->
    case Connector of
        siem -> incident_connector_siem:ingest(maps:get(alert, Params, #{}));
        edr -> incident_connector_edr:quarantine(maps:get(device_id, Params, unknown));
        ticket -> incident_connector_ticket:create(maps:get(ticket_data, Params, #{}));
        notify -> incident_connector_notify:notify(maps:get(recipient, Params, unknown), maps:get(message, Params, <<>>));
        _ -> {error, unknown_connector}
    end.

%% Extract effect ID from spec
-spec extract_effect_id(term()) -> atom().
extract_effect_id(Spec) when is_map(Spec) ->
    maps:get(effect_id, Spec, erlang:unique_integer([positive]));
extract_effect_id({effect, ID, _Connector, _Params}) ->
    ID;
extract_effect_id(_) ->
    erlang:unique_integer([positive]).

%% Extract connector from spec
-spec extract_connector(term()) -> atom().
extract_connector(Spec) when is_map(Spec) ->
    maps:get(connector, Spec, unknown);
extract_connector({effect, _ID, Connector, _Params}) ->
    Connector;
extract_connector(_) ->
    unknown.

%% Extract params from spec
-spec extract_params(term()) -> map().
extract_params(Spec) when is_map(Spec) ->
    maps:get(params, Spec, #{});
extract_params({effect, _ID, _Connector, Params}) when is_map(Params) ->
    Params;
extract_params(_) ->
    #{}.

%% Cancel root scope
-spec cancel_root_scope(#state{}) -> #state{}.
cancel_root_scope(State) ->
    ExecState = State#state.exec_state,
    CancelFlags = wf_vm:exec_cancel(ExecState),
    NewCancelFlags = maps:put(root_case, true, CancelFlags),
    NewExecState = wf_vm:exec_set_cancel(ExecState, NewCancelFlags),
    State#state{
        exec_state = NewExecState,
        case_status = cancelled
    }.

%% Mark a specific scope as cancelled
-spec mark_scope_cancelled(#state{}, atom()) -> #state{}.
mark_scope_cancelled(State, ScopeID) ->
    ExecState = State#state.exec_state,
    CancelFlags = wf_vm:exec_cancel(ExecState),
    NewCancelFlags = maps:put(ScopeID, true, CancelFlags),
    NewExecState = wf_vm:exec_set_cancel(ExecState, NewCancelFlags),
    State#state{exec_state = NewExecState}.

%% Build status map
-spec build_status(#state{}) -> ln_ctrl:case_status().
build_status(State) ->
    ExecState = State#state.exec_state,
    ExecStatus = wf_exec:exec_status(ExecState),
    Budget = State#state.budget,
    BudgetStatus = ln_ctrl_budget:status(Budget),

    #{
        state => State#state.case_status,
        steps => maps:get(steps, ExecStatus),
        current_activity => maps:get(last_opcode, ExecStatus),
        pc => maps:get(pc, ExecStatus),
        stack_depth => maps:get(stack_depth, ExecStatus),
        effects_issued => length(State#state.effects_queue),
        budget_status => BudgetStatus
    }.

%% Notify all awaiting clients
-spec notify_awaiting(#state{}) -> ok.
notify_awaiting(State) ->
    ExecCtx = wf_vm:exec_ctx(State#state.exec_state),
    Result = case State#state.case_status of
        halted -> {ok, ExecCtx};
        cancelled -> {error, cancelled};
        error -> {error, State#state.error_reason};
        _ -> {error, unknown}
    end,

    lists:foreach(
        fun({Pid, Ref}) ->
            gen_server:reply({Pid, Ref}, Result)
        end,
        State#state.awaiting_clients
    ).

%% Filter trace events by sequence number
-spec filter_trace([term()], non_neg_integer(), non_neg_integer()) ->
    [ln_ctrl:trace_event()].
filter_trace(Trace, FromSeq, ToSeq) ->
    lists:filter(
        fun({Seq, _Type, _Op, _Ctx, _Time, _Scope, _Cancel}) ->
            Seq >= FromSeq andalso Seq =< ToSeq
        end,
        Trace
    ).
