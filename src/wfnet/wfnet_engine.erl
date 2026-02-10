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
%% @doc Workflow Net Execution Engine
%%
%% Step-based, event-driven runtime for executing workflow specifications.
%% Provides fine-grained control over workflow execution with full
%% observability through event emission.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Step-based execution:</b> Execute one transition at a time</li>
%%   <li><b>Event-driven:</b> Subscribe to workflow events</li>
%%   <li><b>Suspend/Resume:</b> Pause and resume long-running workflows</li>
%%   <li><b>Case management:</b> Multiple workflow cases per engine</li>
%%   <li><b>Observability:</b> Full event history and metrics</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% ```erlang
%% %% Start the engine with a workflow spec
%% {ok, Engine} = wfnet_engine:start_link(Spec).
%%
%% %% Start a new workflow case
%% {ok, CaseId} = wfnet_engine:start_case(Engine, #{input => data}).
%%
%% %% Execute single steps
%% {ok, StepResult} = wfnet_engine:execute_step(Engine, CaseId).
%%
%% %% Run to completion
%% {ok, StepsCompleted} = wfnet_engine:run_to_completion(Engine, CaseId).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_engine).

-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    start_link/2,
    stop/1,

    %% Case management
    start_case/2,
    get_case_state/2,
    get_case_marking/2,
    list_cases/1,
    list_cases/2,
    cancel_case/2,

    %% Step-based execution
    execute_step/1,
    execute_step/2,
    execute_steps/2,
    execute_steps/3,
    run_to_completion/1,
    run_to_completion/2,
    set_mode/2,

    %% Event subscription
    subscribe_events/2,
    subscribe_events/3,
    unsubscribe_events/2,

    %% Suspend/Resume
    suspend_case/2,
    resume_case/2,
    checkpoint/2,
    restore/2,

    %% Query functions
    get_metrics/1,
    get_enabled_transitions/2,
    get_receipts/2,

    %% Status
    get_status/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

%% Include types and records
-include_lib("kernel/include/logger.hrl").

%% Records

-record(case_state, {
    case_id :: wfnet_types:case_id(),
    status :: wfnet_types:case_status(),
    marking :: wfnet_types:marking(),
    usr_info :: map(),
    receipts = [] :: [term()],
    created_at :: integer(),
    updated_at :: integer(),
    parent_case :: wfnet_types:case_id() | undefined,
    context :: map()
}).

-record(engine_state, {
    spec_id :: binary(),
    spec :: wfnet_types:workflow_spec(),
    cases = #{} :: #{wfnet_types:case_id() => #case_state{}},
    next_case_seq = 1 :: pos_integer(),
    event_subscribers = [] :: [{pid(), wfnet_events:event_filter()}],
    event_buffer = [] :: [wfnet_events:event()],
    max_buffer = 1000 :: pos_integer(),
    execution_mode :: auto | step | paused,
    metrics :: map()
}).

%% Types

-type engine() :: pid().
-type case_id() :: wfnet_types:case_id().
-type case_status() :: wfnet_types:case_status().
-type marking() :: wfnet_types:marking().

-type step_result() :: #{
    case_id := case_id(),
    transition_fired => atom() | undefined,
    marking_before => marking(),
    marking_after => marking(),
    steps_remaining => non_neg_integer()
}.

-type metrics() :: #{
    total_cases => non_neg_integer(),
    active_cases => non_neg_integer(),
    completed_cases => non_neg_integer(),
    steps_executed => non_neg_integer(),
    events_emitted => non_neg_integer()
}.

-export_type([engine/0, step_result/0, metrics/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a workflow engine with a specification.
%%
%% === Example ===
%% ```erlang
%% Spec = #{places => [...], transitions => [...], ...},
%% {ok, Engine} = wfnet_engine:start_link(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(wfnet_types:workflow_spec()) -> {ok, pid()} | {error, term()}.
start_link(Spec) ->
    start_link(Spec, []).

%%--------------------------------------------------------------------
%% @doc Start a workflow engine with options.
%%
%% Options:
%% - `{spec_id, binary()}` - Specification identifier
%% - `{event_buffer_size, pos_integer()}` - Max events to buffer
%% - `{execution_mode, auto | step | paused}` - Initial execution mode
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(wfnet_types:workflow_spec(), proplists:proplist()) ->
    {ok, pid()} | {error, term()}.
start_link(Spec, Options) when is_map(Spec), is_list(Options) ->
    SpecId = proplists:get_value(spec_id, Options,
                                iolist_to_binary(["wfnet_", integer_to_list(erlang:unique_integer())])),
    gen_server:start_link(?MODULE, [SpecId, Spec, Options], []).

%%--------------------------------------------------------------------
%% @doc Stop the workflow engine.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(engine()) -> ok.
stop(Engine) ->
    gen_server:stop(Engine).

%%--------------------------------------------------------------------
%% @doc Start a new workflow case.
%%
%% Returns the case ID for tracking execution.
%%
%% === Example ===
%% ```erlang
%% {ok, CaseId} = wfnet_engine:start_case(Engine, #{input => data}).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec start_case(engine(), map()) -> {ok, case_id()} | {error, term()}.
start_case(Engine, InitialData) when is_map(InitialData) ->
    gen_server:call(Engine, {start_case, InitialData}).

%%--------------------------------------------------------------------
%% @doc Get the current state of a workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_state(engine(), case_id()) ->
    {ok, #case_state{}} | {error, not_found}.
get_case_state(Engine, CaseId) ->
    gen_server:call(Engine, {get_case_state, CaseId}).

%%--------------------------------------------------------------------
%% @doc Get the current marking of a workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_marking(engine(), case_id()) ->
    {ok, marking()} | {error, not_found}.
get_case_marking(Engine, CaseId) ->
    gen_server:call(Engine, {get_case_marking, CaseId}).

%%--------------------------------------------------------------------
%% @doc List all workflow cases.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_cases(engine()) -> [case_id()].
list_cases(Engine) ->
    gen_server:call(Engine, list_cases).

%%--------------------------------------------------------------------
%% @doc List workflow cases filtered by status.
%%
%% === Example ===
%% ```erlang
%% Running = wfnet_engine:list_cases(Engine, running).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec list_cases(engine(), case_status() | [case_status()]) -> [case_id()].
list_cases(Engine, Status) ->
    gen_server:call(Engine, {list_cases, Status}).

%%--------------------------------------------------------------------
%% @doc Cancel a running workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_case(engine(), case_id()) -> ok | {error, not_found | already_completed}.
cancel_case(Engine, CaseId) ->
    gen_server:call(Engine, {cancel_case, CaseId}).

%%--------------------------------------------------------------------
%% @doc Execute a single step (transition firing) on any enabled case.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_step(engine()) -> {ok, step_result()} | {error, no_enabled_cases}.
execute_step(Engine) ->
    gen_server:call(Engine, execute_step).

%%--------------------------------------------------------------------
%% @doc Execute a single step on a specific case.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_step(engine(), case_id()) -> {ok, step_result()} | {error, term()}.
execute_step(Engine, CaseId) ->
    gen_server:call(Engine, {execute_step, CaseId}).

%%--------------------------------------------------------------------
%% @doc Execute multiple steps on any enabled cases.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_steps(engine(), non_neg_integer()) ->
    {ok, [step_result()]} | {error, no_enabled_cases}.
execute_steps(Engine, Count) ->
    gen_server:call(Engine, {execute_steps, Count}, infinity).

%%--------------------------------------------------------------------
%% @doc Execute multiple steps on a specific case.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_steps(engine(), case_id(), non_neg_integer()) ->
    {ok, [step_result()]} | {error, term()}.
execute_steps(Engine, CaseId, Count) ->
    gen_server:call(Engine, {execute_steps, CaseId, Count}, infinity).

%%--------------------------------------------------------------------
%% @doc Run a workflow case to completion.
%%
%% Executes transitions until the case reaches the terminal state.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_to_completion(engine()) -> {ok, non_neg_integer()} | {error, term()}.
run_to_completion(Engine) ->
    gen_server:call(Engine, run_to_completion, infinity).

%%--------------------------------------------------------------------
%% @doc Run a specific case to completion.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_to_completion(engine(), case_id()) ->
    {ok, non_neg_integer()} | {error, term()}.
run_to_completion(Engine, CaseId) ->
    gen_server:call(Engine, {run_to_completion, CaseId}, infinity).

%%--------------------------------------------------------------------
%% @doc Set the execution mode of the engine.
%%
%% Modes:
%% - `auto` - Automatically execute enabled transitions
%% - `step` - Require explicit step calls
%% - `paused` - No execution (useful for inspection)
%%
%% @end
%%--------------------------------------------------------------------
-spec set_mode(engine(), auto | step | paused) -> ok.
set_mode(Engine, Mode) ->
    gen_server:call(Engine, {set_mode, Mode}).

%%--------------------------------------------------------------------
%% @doc Subscribe to workflow events.
%%
%% === Example ===
%% ```erlang
%% wfnet_engine:subscribe_events(Engine, self(), #{case_id => CaseId}).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_events(engine(), pid()) -> ok.
subscribe_events(Engine, Subscriber) ->
    subscribe_events(Engine, Subscriber, #{}).

%%--------------------------------------------------------------------
%% @doc Subscribe to filtered workflow events.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_events(engine(), pid(), wfnet_events:event_filter()) -> ok.
subscribe_events(Engine, Subscriber, Filter) ->
    gen_server:call(Engine, {subscribe_events, Subscriber, Filter}).

%%--------------------------------------------------------------------
%% @doc Unsubscribe from workflow events.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_events(engine(), pid()) -> ok.
unsubscribe_events(Engine, Subscriber) ->
    gen_server:call(Engine, {unsubscribe_events, Subscriber}).

%%--------------------------------------------------------------------
%% @doc Suspend a workflow case.
%%
%% Pauses execution of the case. Can be resumed later.
%%
%% @end
%%--------------------------------------------------------------------
-spec suspend_case(engine(), case_id()) -> ok | {error, term()}.
suspend_case(Engine, CaseId) ->
    gen_server:call(Engine, {suspend_case, CaseId}).

%%--------------------------------------------------------------------
%% @doc Resume a suspended workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec resume_case(engine(), case_id()) -> ok | {error, term()}.
resume_case(Engine, CaseId) ->
    gen_server:call(Engine, {resume_case, CaseId}).

%%--------------------------------------------------------------------
%% @doc Create a checkpoint for a case.
%%
%% Returns a checkpoint reference that can be used for restoration.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint(engine(), case_id()) -> {ok, binary()} | {error, term()}.
checkpoint(Engine, CaseId) ->
    gen_server:call(Engine, {checkpoint, CaseId}).

%%--------------------------------------------------------------------
%% @doc Restore a case from a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
-spec restore(engine(), binary()) -> {ok, case_id()} | {error, term()}.
restore(Engine, CheckpointRef) ->
    gen_server:call(Engine, {restore, CheckpointRef}).

%%--------------------------------------------------------------------
%% @doc Get engine metrics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_metrics(engine()) -> {ok, metrics()}.
get_metrics(Engine) ->
    gen_server:call(Engine, get_metrics).

%%--------------------------------------------------------------------
%% @doc Get enabled transitions for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_enabled_transitions(engine(), case_id()) ->
    {ok, [atom()]} | {error, not_found}.
get_enabled_transitions(Engine, CaseId) ->
    gen_server:call(Engine, {get_enabled_transitions, CaseId}).

%%--------------------------------------------------------------------
%% @doc Get execution receipts for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_receipts(engine(), case_id()) ->
    {ok, [term()]} | {error, not_found}.
get_receipts(Engine, CaseId) ->
    gen_server:call(Engine, {get_receipts, CaseId}).

%%--------------------------------------------------------------------
%% @doc Get the current status of the engine.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_status(engine()) -> {ok, map()}.
get_status(Engine) ->
    gen_server:call(Engine, get_status).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init([SpecId, Spec, Options]) ->
    %% Initialize event system
    wfnet_events:init_table(),

    %% Initialize state
    EventBufferSize = proplists:get_value(event_buffer_size, Options, 1000),
    ExecutionMode = proplists:get_value(execution_mode, Options, step),

    State = #engine_state{
        spec_id = SpecId,
        spec = Spec,
        event_subscribers = [],
        event_buffer = [],
        max_buffer = EventBufferSize,
        execution_mode = ExecutionMode,
        metrics = #{
            total_cases => 0,
            active_cases => 0,
            completed_cases => 0,
            steps_executed => 0,
            events_emitted => 0
        }
    },

    ?LOG_INFO("Workflow engine started: spec=~p", [SpecId]),
    {ok, State}.

%% @private
handle_call({start_case, InitialData}, _From, State) ->
    #engine_state{
        spec = Spec,
        cases = Cases,
        next_case_seq = Seq,
        metrics = Metrics
    } = State,

    CaseId = list_to_binary([
        "case_",
        integer_to_binary(Seq)
    ]),

    %% Get initial marking from spec
    InitialMarking = get_initial_marking(Spec, InitialData),

    CaseState = #case_state{
        case_id = CaseId,
        status = created,
        marking = InitialMarking,
        usr_info = InitialData,
        receipts = [],
        created_at = erlang:system_time(millisecond),
        updated_at = erlang:system_time(millisecond),
        context = #{}
    },

    %% Emit case created event
    emit_event(case_created, CaseId, #{
        spec_id => maps:get(spec_id, State, undefined),
        initial_data => InitialData
    }, State),

    %% Update state
    NewCases = Cases#{CaseId => CaseState},
    NewMetrics = Metrics#{
        total_cases => maps:get(total_cases, Metrics, 0) + 1,
        active_cases => maps:get(active_cases, Metrics, 0) + 1
    },

    NewState = State#engine_state{
        cases = NewCases,
        next_case_seq = Seq + 1,
        metrics = NewMetrics
    },

    {reply, {ok, CaseId}, NewState};

handle_call({get_case_state, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        CaseState -> {reply, {ok, CaseState}, State}
    end;

handle_call({get_case_marking, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #case_state{marking = Marking} -> {reply, {ok, Marking}, State}
    end;

handle_call(list_cases, _From, #engine_state{cases = Cases} = State) ->
    {reply, maps:keys(Cases), State};

handle_call({list_cases, Status}, _From, #engine_state{cases = Cases} = State) when is_atom(Status) ->
    Filtered = [Id || {Id, #case_state{status = S}} <- maps:to_list(Cases), S =:= Status],
    {reply, Filtered, State};
handle_call({list_cases, Statuses}, _From, #engine_state{cases = Cases} = State) when is_list(Statuses) ->
    Filtered = [Id || {Id, #case_state{status = S}} <- maps:to_list(Cases),
                     lists:member(S, Statuses)],
    {reply, Filtered, State};

handle_call({cancel_case, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_state{status = completed} ->
            {reply, {error, already_completed}, State};
        #case_state{status = cancelled} ->
            {reply, {error, already_cancelled}, State};
        CaseState ->
            NewCaseState = CaseState#case_state{status = cancelled},
            emit_event(case_cancelled, CaseId, #{}, State),
            NewCases = Cases#{CaseId => NewCaseState},
            {reply, ok, State#engine_state{cases = NewCases}}
    end;

handle_call(execute_step, _From, #engine_state{cases = Cases} = State) ->
    %% Find first enabled case
    EnabledCases = find_enabled_cases(Cases),
    case EnabledCases of
        [] ->
            {reply, {error, no_enabled_cases}, State};
        [{CaseId, _CaseState} | _] ->
            handle_execute_step(CaseId, State)
    end;

handle_call({execute_step, CaseId}, _From, State) ->
    handle_execute_step(CaseId, State);

handle_call({execute_steps, Count}, _From, State) ->
    execute_steps_multi(Count, [], State);

handle_call({execute_steps, CaseId, Count}, _From, State) ->
    execute_steps_case(CaseId, Count, [], State);

handle_call(run_to_completion, _From, #engine_state{cases = Cases} = State) ->
    RunningCases = [Id || {Id, #case_state{status = running}} <- maps:to_list(Cases)],
    case RunningCases of
        [] -> {reply, {error, no_running_cases}, State};
        _ -> run_cases_to_completion(RunningCases, 0, State)
    end;

handle_call({run_to_completion, CaseId}, _From, State) ->
    run_cases_to_completion([CaseId], 0, State);

handle_call({set_mode, Mode}, _From, State) ->
    {reply, ok, State#engine_state{execution_mode = Mode}};

handle_call({subscribe_events, Subscriber, Filter}, _From, #engine_state{event_subscribers = Subs} = State) ->
    monitor(process, Subscriber),
    NewSubs = [{Subscriber, Filter} | Subs],
    {reply, ok, State#engine_state{event_subscribers = NewSubs}};

handle_call({unsubscribe_events, Subscriber}, _From, #engine_state{event_subscribers = Subs} = State) ->
    NewSubs = lists:keydelete(Subscriber, 1, Subs),
    {reply, ok, State#engine_state{event_subscribers = NewSubs}};

handle_call({suspend_case, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #case_state{status = running} = CaseState ->
            NewCaseState = CaseState#case_state{status = suspended},
            emit_event(case_suspended, CaseId, #{}, State),
            {reply, ok, State#engine_state{cases = Cases#{CaseId => NewCaseState}}};
        #case_state{status = suspended} ->
            {reply, {error, already_suspended}, State};
        _ ->
            {reply, {error, invalid_status}, State}
    end;

handle_call({resume_case, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #case_state{status = suspended} = CaseState ->
            NewCaseState = CaseState#case_state{status = running},
            emit_event(case_resumed, CaseId, #{}, State),
            {reply, ok, State#engine_state{cases = Cases#{CaseId => NewCaseState}}};
        #case_state{status = running} ->
            {reply, {error, not_suspended}, State};
        _ ->
            {reply, {error, invalid_status}, State}
    end;

handle_call({checkpoint, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        CaseState ->
            CheckpointRef = list_to_binary([
                "cp_", CaseId, "_",
                integer_to_binary(erlang:system_time(millisecond))
            ]),
            %% In production, persist to storage
            {reply, {ok, CheckpointRef}, State}
    end;

handle_call({restore, CheckpointRef}, _From, State) ->
    %% In production, load from storage
    {reply, {error, not_implemented}, State};

handle_call(get_metrics, _From, #engine_state{metrics = Metrics} = State) ->
    {reply, {ok, Metrics}, State};

handle_call({get_enabled_transitions, CaseId}, _From, #engine_state{cases = Cases, spec = Spec} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #case_state{marking = Marking} ->
            Enabled = get_enabled_transitions_for_marking(Marking, Spec),
            {reply, {ok, Enabled}, State}
    end;

handle_call({get_receipts, CaseId}, _From, #engine_state{cases = Cases} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #case_state{receipts = Receipts} ->
            {reply, {ok, lists:reverse(Receipts)}, State}
    end;

handle_call(get_status, _From, State) ->
    #engine_state{
        spec_id = SpecId,
        cases = Cases,
        execution_mode = Mode,
        metrics = Metrics
    } = State,
    Status = #{
        spec_id => SpecId,
        case_count => maps:size(Cases),
        execution_mode => Mode,
        metrics => Metrics
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Info}, #engine_state{event_subscribers = Subs} = State) ->
    %% Remove dead subscriber
    NewSubs = lists:keydelete(Pid, 1, Subs),
    {noreply, State#engine_state{event_subscribers = NewSubs}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
handle_continue(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Execute a single step for a specific case.
%%--------------------------------------------------------------------
handle_execute_step(CaseId, #engine_state{cases = Cases, spec = Spec} = State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_state{status = suspended} ->
            {reply, {error, case_suspended}, State};
        #case_state{status = Status} when Status =/= running, Status =/= created ->
            {reply, {error, {invalid_status, Status}}, State};
        CaseState ->
            #case_state{marking = MarkingBefore, usr_info = UsrInfo} = CaseState,
            %% Find enabled transitions
            Enabled = get_enabled_transitions_for_marking(MarkingBefore, Spec),
            case Enabled of
                [] ->
                    %% No enabled transitions - check if terminal state
                    case is_terminal_state(MarkingBefore, Spec) of
                        true ->
                            NewCaseState = CaseState#case_state{
                                status = completed,
                                updated_at = erlang:system_time(millisecond)
                            },
                            emit_event(case_completed, CaseId, #{
                                marking => MarkingBefore
                            }, State),
                            NewCases = Cases#{CaseId => NewCaseState},
                            {reply, {ok, #{
                                case_id => CaseId,
                                marking_before => MarkingBefore,
                                marking_after => MarkingBefore,
                                status => completed
                            }}, State#engine_state{cases = NewCases}};
                        false ->
                            {reply, {error, no_enabled_transitions}, State}
                    end;
                [Transition | _] ->
                    %% Fire the transition
                    Preset = maps:get(preset, Spec, #{}),
                    PresetPlaces = maps:get(Transition, Preset, []),

                    %% Create mode (simplified - take one token from each place)
                    Mode = lists:foldl(fun(P, Acc) ->
                        Tokens = maps:get(P, MarkingBefore, []),
                        case Tokens of
                            [] -> Acc;
                            [T | _] -> Acc#{P => [T]}
                        end
                    end, #{}, PresetPlaces),

                    %% Apply the move
                    ProduceMap = get_produce_map(Spec, Transition, Mode, UsrInfo),
                    case wfnet_marking:apply_move(MarkingBefore, #{mode => Mode, produce => ProduceMap}) of
                        {ok, MarkingAfter} ->
                            %% Create receipt
                            Receipt = #{
                                transition => Transition,
                                mode => Mode,
                                produce => ProduceMap,
                                timestamp => erlang:system_time(millisecond)
                            },

                            NewCaseState = CaseState#case_state{
                                marking = MarkingAfter,
                                receipts = [Receipt | maps:get(receipts, CaseState, [])],
                                updated_at = erlang:system_time(millisecond)
                            },

                            %% Emit event
                            emit_event(transition_fired, CaseId, #{
                                transition => Transition,
                                marking_before => MarkingBefore,
                                marking_after => MarkingAfter
                            }, State),

                            %% Update case status to running
                            FinalCaseState = case NewCaseState#case_state.status of
                                created -> NewCaseState#case_state{status = running};
                                _ -> NewCaseState
                            end,

                            NewCases = Cases#{CaseId => FinalCaseState},
                            {reply, {ok, #{
                                case_id => CaseId,
                                transition_fired => Transition,
                                marking_before => MarkingBefore,
                                marking_after => MarkingAfter
                            }}, State#engine_state{cases = NewCases}};
                        {error, Reason} ->
                            emit_event(transition_failed, CaseId, #{
                                transition => Transition,
                                reason => Reason
                            }, State),
                            {reply, {error, Reason}, State}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute multiple steps across any cases.
%%--------------------------------------------------------------------
execute_steps_multi(0, Acc, State) ->
    {reply, {ok, lists:reverse(Acc)}, State};
execute_steps_multi(Count, Acc, #engine_state{cases = Cases} = State) ->
    EnabledCases = find_enabled_cases(Cases),
    case EnabledCases of
        [] ->
            {reply, {ok, lists:reverse(Acc)}, State};
        [{CaseId, _} | _] ->
            case handle_execute_step(CaseId, State) of
                {reply, {ok, Result}, NewState} ->
                    execute_steps_multi(Count - 1, [Result | Acc], NewState);
                {reply, {error, _}, _} ->
                    {reply, {ok, lists:reverse(Acc)}, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute multiple steps for a specific case.
%%--------------------------------------------------------------------
execute_steps_case(_CaseId, 0, Acc, State) ->
    {reply, {ok, lists:reverse(Acc)}, State};
execute_steps_case(CaseId, Count, Acc, State) ->
    case handle_execute_step(CaseId, State) of
        {reply, {ok, Result}, NewState} ->
            execute_steps_case(CaseId, Count - 1, [Result | Acc], NewState);
        {reply, {error, no_enabled_transitions}, _} ->
            {reply, {ok, lists:reverse(Acc)}, State};
        {reply, {error, Reason}, _} ->
            {reply, {error, Reason}, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Run cases to completion.
%%--------------------------------------------------------------------
run_cases_to_completion([], StepCount, State) ->
    {reply, {ok, StepCount}, State};
run_cases_to_completion(CaseIds, StepCount, State) ->
    %% Execute one step per case
    case execute_steps_on_cases(CaseIds, State) of
        {ok, 0, _} ->
            %% No steps executed, check if all complete
            {reply, {ok, StepCount}, State};
        {ok, Steps, NewState} ->
            run_cases_to_completion(CaseIds, StepCount + Steps, NewState);
        {error, _} ->
            {reply, {ok, StepCount}, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute one step on each case that has enabled transitions.
%%--------------------------------------------------------------------
execute_steps_on_cases(CaseIds, #engine_state{cases = Cases} = State) ->
    execute_steps_on_cases(CaseIds, Cases, 0, State).

execute_steps_on_cases([], _Cases, Count, _State) ->
    {ok, Count, undefined};
execute_steps_on_cases([CaseId | Rest], Cases, Count, State) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            execute_steps_on_cases(Rest, Cases, Count, State);
        #case_state{status = completed} ->
            execute_steps_on_cases(Rest, Cases, Count, State);
        #case_state{status = running} ->
            case handle_execute_step(CaseId, State) of
                {reply, {ok, _Result}, NewState} ->
                    execute_steps_on_cases(Rest, Cases, Count + 1, NewState);
                {reply, {error, no_enabled_transitions}, NewState} ->
                    execute_steps_on_cases(Rest, Cases, Count, NewState);
                {reply, {error, _}, _} ->
                    {error, no_enabled_transitions}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Find cases with enabled transitions.
%%--------------------------------------------------------------------
find_enabled_cases(Cases) ->
    maps:fold(fun(CaseId, #case_state{marking = Marking, status = Status}, Acc) ->
        case Status of
            running ->
                %% Check if any transition is enabled
                case has_any_enabled_transition(Marking) of
                    true -> [{CaseId, Marking} | Acc];
                    false -> Acc
                end;
            created ->
                %% Check if any transition is enabled
                case has_any_enabled_transition(Marking) of
                    true -> [{CaseId, Marking} | Acc];
                    false -> Acc
                end;
            _ ->
                Acc
        end
    end, [], Cases).

%%--------------------------------------------------------------------
%% @private
%% @doc Get initial marking from spec.
%%--------------------------------------------------------------------
get_initial_marking(Spec, InitialData) ->
    StartPlace = maps:get(start_place, Spec),
    #{StartPlace => [maps:get(initial_token, InitialData, start)]}.

%%--------------------------------------------------------------------
%% @private
%% @doc Get enabled transitions for a marking.
%%--------------------------------------------------------------------
get_enabled_transitions_for_marking(Marking, Spec) ->
    Transitions = maps:get(transitions, Spec, []),
    Preset = maps:get(preset, Spec, #{}),
    [T || T <- Transitions, is_transition_enabled(T, Marking, Preset)].

%%--------------------------------------------------------------------
%% @private
%% @doc Check if a transition is enabled.
%%--------------------------------------------------------------------
is_transition_enabled(Transition, Marking, Preset) ->
    PresetPlaces = maps:get(Transition, Preset, []),
    lists:all(fun(P) ->
        Tokens = maps:get(P, Marking, []),
        Tokens =/= []
    end, PresetPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Check if marking has any enabled transition.
%%--------------------------------------------------------------------
has_any_enabled_transition(Marking) ->
    %% Simplified check - if any place has tokens, something might be enabled
    lists:any(fun({_P, Tokens}) -> Tokens =/= [] end, maps:to_list(Marking)).

%%--------------------------------------------------------------------
%% @private
%% @doc Check if marking is a terminal state.
%%--------------------------------------------------------------------
is_terminal_state(Marking, Spec) ->
    EndPlace = maps:get(end_place, Spec),
    case Marking of
        #{EndPlace := [_]} when map_size(Marking) =:= 1 -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Get produce map for a transition.
%%--------------------------------------------------------------------
get_produce_map(Spec, Transition, Mode, UsrInfo) ->
    Postset = maps:get(postset, Spec, #{}),
    maps:get(Transition, Postset, []).

%%--------------------------------------------------------------------
%% @private
%% @doc Emit a workflow event.
%%--------------------------------------------------------------------
emit_event(Type, CaseId, Data, #engine_state{event_subscribers = Subs}) ->
    Event = wfnet_events:emit_event(Type, CaseId, Data),
    %% Notify subscribers
    lists:foreach(fun({Sub, Filter}) ->
        case wfnet_events:event_filter_match(Event, Filter) of
            true -> Sub ! {wfnet_event, Event};
            false -> ok
        end
    end, Subs),
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Simple engine lifecycle test
engine_lifecycle_test() ->
    Spec = #{
        places => [start, end],
        transitions => [t1],
        start_place => start,
        end_place => end,
        preset => #{t1 => [start]},
        postset => #{t1 => [end]}
    },
    {ok, Engine} = start_link(Spec),
    ?assertEqual(ok, stop(Engine)).

%% Start case test
start_case_test() ->
    Spec = #{
        places => [start, end],
        transitions => [t1],
        start_place => start,
        end_place => end,
        preset => #{t1 => [start]},
        postset => #{t1 => [end]}
    },
    {ok, Engine} = start_link(Spec),
    {ok, CaseId} = start_case(Engine, #{}),
    ?assert(is_binary(CaseId)),
    stop(Engine).

%% Execute step test
execute_step_test() ->
    Spec = #{
        places => [start, end],
        transitions => [t1],
        start_place => start,
        end_place => end,
        preset => #{t1 => [start]},
        postset => #{t1 => [end]}
    },
    {ok, Engine} = start_link(Spec),
    {ok, CaseId} = start_case(Engine, #{}),
    {ok, Result} = execute_step(Engine, CaseId),
    ?assertEqual(CaseId, maps:get(case_id, Result)),
    ?assertEqual(t1, maps:get(transition_fired, Result)),
    stop(Engine).

%% Metrics test
get_metrics_test() ->
    Spec = #{
        places => [start, end],
        transitions => [t1],
        start_place => start,
        end_place => end,
        preset => #{t1 => [start]},
        postset => #{t1 => [end]}
    },
    {ok, Engine} = start_link(Spec),
    {ok, _} = start_case(Engine, #{}),
    {ok, Metrics} = get_metrics(Engine),
    ?assertEqual(1, maps:get(total_cases, Metrics)),
    stop(Engine).

-endif.
