%%%-------------------------------------------------------------------
%%% @doc ln_ctrl - OTP behavior for reliable choreography.
%%%
%%% ln_ctrl is a behavior for orchestrating complex, multi-step workflows
%%% with deterministic scheduling, effect tracking, and structured
%%% cancellation. It combines gen_server ergonomics with workflow-specific
%%% capabilities.
%%%
%%% == Quick Start ==
%%%
%%% ```erlang
%%% -module(my_workflow).
%%% -behaviour(ln_ctrl).
%%%
%%% -export([start_link/1, init/1, plan/1, task/3]).
%%%
%%% start_link(Args) ->
%%%     ln_ctrl:start_link(?MODULE, Args, []).
%%%
%%% init(Args) ->
%%%     {ok, #{}, ln_plan:seq([ln_plan:task(validate),
%%%                           ln_plan:task(process)])}.
%%%
%%% plan(_Ctx) ->
%%%     %% Plan can be dynamic based on context
%%%     ln_plan:seq([ln_plan:task(step1)]).
%%%
%%% task(validate, _Input, Ctx) ->
%%%     {ok, validated, Ctx#{validated => true}};
%%% task(process, _Input, Ctx) ->
%%%     {ok, result, Ctx#{processed => true}}.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_ctrl).

-behaviour(gen_server).

%% Public API
-export([start_link/3, start_link/4]).
-export([signal/2, call/2, call/3]).
-export([cancel/1, cancel_scope/2]).
-export([await/2, status/1, trace/1, trace/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type name() :: pid() | atom() | {via, module(), term()} |
               {global, term()} | {atom(), atom()}.

-type option() :: {scheduler, ln_sched:mode()} |
                 {step_quanta, pos_integer()} |
                 {trace, none | min | full} |
                 {budget, ln_budget:budget()} |
                 {effect_handler, module()} |
                 {checkpoint, boolean()} |
                 {timeout, timeout()} |
                 {debug, [term()]}.

-type status() :: #{
    state := init | running | completing | cancelling | terminated,
    steps := non_neg_integer(),
    effects := #{completed => non_neg_integer(), pending => non_neg_integer()},
    active_scopes := [term()],
    waiting_conditions := [term()],
    budget_usage := term()
}.

-type trace_event() :: #{
    timestamp := integer(),
    type := atom(),
    data => map()
}.

-export_type([name/0, option/0, status/0, trace_event/0]).

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

%% @doc Starts a ln_ctrl instance without registration.
-spec start_link(module(), term(), [option()]) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(CallbackMod, CaseArg, Options) ->
    gen_server:start_link(?MODULE, {CallbackMod, CaseArg, Options}, []).

%% @doc Starts a ln_ctrl instance with registration.
-spec start_link(name(), module(), term(), [option()]) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(ServerName, CallbackMod, CaseArg, Options) ->
    gen_server:start_link(ServerName, ?MODULE, {CallbackMod, CaseArg, Options}, []).

%% @doc Sends an asynchronous signal to the ln_ctrl instance.
-spec signal(name(), term()) -> ok.
signal(Name, Msg) ->
    gen_server:cast(Name, {signal, Msg}).

%% @doc Makes a synchronous call to the ln_ctrl instance.
-spec call(name(), term()) -> term().
call(Name, Request) ->
    gen_server:call(Name, {ln_call, Request}, infinity).

%% @doc Makes a synchronous call with timeout.
-spec call(name(), term(), timeout()) -> term().
call(Name, Request, Timeout) ->
    gen_server:call(Name, {ln_call, Request}, Timeout).

%% @doc Cancels the entire workflow execution.
-spec cancel(name()) -> ok.
cancel(Name) ->
    gen_server:cast(Name, cancel_case).

%% @doc Cancels a specific scope within the workflow.
-spec cancel_scope(name(), term()) -> ok.
cancel_scope(Name, ScopeId) ->
    gen_server:cast(Name, {cancel_scope, ScopeId}).

%% @doc Waits for workflow completion and returns final context.
-spec await(name(), timeout()) ->
    {ok, term()} | {error, term()} | timeout.
await(Name, Timeout) ->
    gen_server:call(Name, await, Timeout).

%% @doc Returns current execution status.
-spec status(name()) -> status().
status(Name) ->
    gen_server:call(Name, status, infinity).

%% @doc Returns trace history.
-spec trace(name()) -> [trace_event()].
trace(Name) ->
    gen_server:call(Name, trace, infinity).

%% @doc Returns trace history filtered by step range.
-spec trace(name(), #{from := non_neg_integer(), to := non_neg_integer()}) ->
    [trace_event()].
trace(Name, #{from := From, to := To}) ->
    gen_server:call(Name, {trace, From, To}, infinity).

%% @doc Stops the ln_ctrl instance gracefully.
-spec stop(name()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

%% @private
init({CallbackMod, CaseArg, Options}) ->
    process_flag(trap_exit, true),

    %% Parse options
    SchedulerMode = proplists:get_value(scheduler, Options, nondeterministic),
    TraceLevel = proplists:get_value(trace, Options, min),
    BudgetConfig = proplists:get_value(budget, Options, #{}),
    EffectHandler = proplists:get_value(effect_handler, Options, ln_effect),
    StepQuanta = proplists:get_value(step_quanta, Options, 1),
    Debug = proplists:get_value(debug, Options, []),

    %% Initialize components
    Sched = ln_sched:init(SchedulerMode),
    Trace = ln_trace:new(#{level => TraceLevel, max_events => infinity}),
    Budget = ln_budget:init(BudgetConfig),
    Cancel = ln_cancel:init(),
    Effect = ln_effect:init(EffectHandler),

    %% Call user init
    case CallbackMod:init(CaseArg) of
        {ok, InitialCtx, Plan} ->
            case ln_plan:validate(Plan) of
                ok ->
                    case ln_compile:compile(Plan) of
                        {ok, Bytecode} ->
                            State = #{
                                callback => CallbackMod,
                                context => InitialCtx,
                                plan => Plan,
                                bytecode => Bytecode,
                                vm_state => init_vm_state(Bytecode),
                                scheduler => Sched,
                                trace => Trace,
                                budget => Budget,
                                cancel => Cancel,
                                effect => Effect,
                                awaiters => [],
                                signal_queue => [],
                                call_queue => [],
                                status => running,
                                step_quanta => StepQuanta,
                                steps_this_quanta => 0,
                                debug => Debug
                            },
                            ln_trace:emit({case_started, #{}}, Trace),
                            {ok, State};
                        {error, Reason} ->
                            {stop, {compile_error, Reason}}
                    end;
                {error, Reason} ->
                    {stop, {validation_error, Reason}}
            end;
        {ok, InitialCtx} ->
            %% No plan returned - use empty plan
            State = #{
                callback => CallbackMod,
                context => InitialCtx,
                plan => ln_plan:seq([]),
                bytecode => undefined,
                vm_state => undefined,
                scheduler => Sched,
                trace => Trace,
                budget => Budget,
                cancel => Cancel,
                effect => Effect,
                awaiters => [],
                signal_queue => [],
                call_queue => [],
                call_queue => [],
                status => running,
                step_quanta => StepQuanta,
                steps_this_quanta => 0,
                debug => Debug
            },
            {ok, State};
        {stop, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call({ln_call, Request}, From, State) ->
    %% Forward to user callback
    #{callback := Mod, context := Ctx} = State,
    case Mod:handle_call(Request, From, Ctx) of
        {reply, Reply, NewCtx} ->
            {reply, Reply, State#{context => NewCtx}};
        {noreply, NewCtx} ->
            {noreply, State#{context => NewCtx}};
        {stop, Reason, Reply, NewCtx} ->
            {stop, Reason, Reply, State#{context => NewCtx}};
        {stop, Reason, NewCtx} ->
            {stop, Reason, State#{context => NewCtx}}
    end;

handle_call(status, _From, #{trace := Trace} = State) ->
    Status = #{
        state => maps:get(status, State, unknown),
        steps => maps:get(steps, State, 0),
        effects => #{completed => 0, pending => 0},
        active_scopes => [],
        waiting_conditions => [],
        budget_usage => ln_budget:status(maps:get(budget, State, undefined))
    },
    {reply, Status, State};

handle_call(trace, _From, #{trace := Trace} = State) ->
    {reply, ln_trace:get_all(Trace), State};

handle_call({trace, From, To}, _From, #{trace := Trace} = State) ->
    {reply, ln_trace:get_range(Trace, From, To), State};

handle_call(await, From, #{status := Status} = State) ->
    case Status of
        completed ->
            {reply, {ok, maps:get(context, State)}, State};
        failed ->
            {reply, {error, maps:get(failure_reason, State, unknown)}, State};
        cancelled ->
            {reply, {error, cancelled}, State};
        _ ->
            %% Add to awaiters
            Awaiters = maps:get(awaiters, State, []),
            {noreply, State#{awaiters => [{From, erlang:monotonic_time(millisecond)} | Awaiters]}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({signal, Msg}, #{callback := Mod, context := Ctx, signal_queue := Q} = State) ->
    case Mod:handle_signal(Msg, Ctx) of
        {noreply, NewCtx} ->
            {noreply, State#{context => NewCtx}};
        {stop, Reason, NewCtx} ->
            {stop, Reason, State#{context => NewCtx}}
    end;

handle_cast(cancel_case, #{status := running} = State) ->
    {ok, NewCancel} = ln_cancel:cancel_scope(root, maps:get(cancel, State)),
    {noreply, State#{cancel => NewCancel, status => cancelled}};

handle_cast(cancel_case, State) ->
    {noreply, State};

handle_cast({cancel_scope, ScopeId}, #{cancel := Cancel} = State) ->
    {ok, NewCancel} = ln_cancel:cancel_scope(ScopeId, Cancel),
    {noreply, State#{cancel => NewCancel}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Initialize VM state from bytecode.
init_vm_state(#{program := Program}) ->
    #{
        pc => 0,
        frames => [],
        stack => [],
        scopes => #{},
        result => undefined
    }.
