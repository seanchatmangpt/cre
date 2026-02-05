%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Workflow Execution Engine
%%
%% This module implements the core workflow execution engine for YAWL
%% (Yet Another Workflow Language) in the CRE runtime environment.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Workflow case lifecycle management (start, cancel, suspend, resume)</li>
%%   <li>Work item management (allocate, start, complete, fail)</li>
%%   <li>Petri net-based execution using gen_pnet</li>
%%   <li>Support for YAWL net elements (InputCondition, OutputCondition, Task)</li>
%%   <li>AND/OR/XOR split and join semantics</li>
%%   <li>Integration with CRE master, client, and worker modules</li>
%% </ul>
%%
%% <h3>Architecture</h3>
%%
%% The engine is implemented as a gen_server that manages workflow cases.
%% Each case is represented as a Petri net execution with places and
%% transitions mapped from the YAWL specification.
%%
%% <h4>YAWL Net Element Mapping:</h4>
%% <ul>
%%   <li><b>InputCondition:</b> Start place with initial token</li>
%%   <li><b>OutputCondition:</b> End place marking completion</li>
%%   <li><b>Task (AND split):</b> Transition producing multiple tokens</li>
%%   <li><b>Task (OR split):</b> Transition with conditional token production</li>
%%   <li><b>Task (XOR split):</b> Transition producing single token based on condition</li>
%%   <li><b>Task (AND join):</b> Transition requiring all input tokens</li>
%%   <li><b>Task (OR join):</b> Transition requiring threshold tokens</li>
%%   <li><b>Task (XOR join):</b> Transition firing on first available token</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_engine).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% gen_server callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%% Core YAWL Engine API
-export([start_link/0, start_link/1,
         start_workflow/2, start_workflow/3,
         cancel_case/1,
         suspend_case/1,
         resume_case/1,
         get_case_state/1,
         get_available_workitems/1,
         start_workitem/2,
         complete_workitem/3,
         fail_workitem/3,
         register_observer/2,
         unregister_observer/2,
         get_engine_status/1,
         delete_completed_case/1,
         list_cases/1,
         recover_active_cases/0,
         enable_persistence/0,
         disable_persistence/0,
         is_persistence_enabled/0]).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().
-type workitem_id() :: binary().
-type task_id() :: binary().
-type workflow_id() :: binary().
-type case_status() :: initialized | running | suspended | completed | cancelled | failed.
-type workitem_status() :: enabled | started | completed | failed | cancelled.
-type split_type() :: and_split | or_split | xor_split.
-type join_type() :: and_join | or_join | xor_join.

-record(workitem, {
          id :: workitem_id(),
          case_id :: case_id(),
          task_id :: task_id(),
          status :: workitem_status(),
          data :: map(),
          enabled_at :: integer() | undefined,
          started_at :: integer() | undefined,
          completed_at :: integer() | undefined,
          assigned_to :: pid() | undefined
         }).

-record(workflow_case, {
          case_id :: case_id(),
          workflow_id :: workflow_id(),
          spec :: term(),
          status :: case_status(),
          workitems = #{} :: #{workitem_id() => #workitem{}},
          data = #{} :: map(),
          created_at :: integer(),
          started_at :: integer() | undefined,
          completed_at :: integer() | undefined,
          net_state :: term() | undefined,
          observers = [] :: [pid()]
         }).

-record(engine_state, {
          cases = #{} :: #{case_id() => #workflow_case{}},
          observers = [] :: [pid()],
          cre_master :: atom() | pid() | undefined,
          next_case_id = 1 :: non_neg_integer(),
          persistence_enabled = false :: boolean()
         }).

-export_type([case_id/0, workitem_id/0, case_status/0, workitem_status/0,
              split_type/0, join_type/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the YAWL engine with default configuration.
%%
%% Returns `{ok, Pid}' on success.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Starts a named YAWL engine instance.
%%
%% Returns `{ok, Pid}' on success.
%% @end
%%--------------------------------------------------------------------
-spec start_link(EngineName :: atom()) -> {ok, pid()} | {error, term()}.

start_link(EngineName) ->
    gen_server:start_link(EngineName, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Starts a new workflow case from a specification.
%%
%% The specification can be:
%% - A YAWL XML specification file path
%% - A parsed specification term
%% - A cre_yawl workflow record
%%
%% Returns `{ok, CaseId}' on success.
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(Engine :: atom() | pid(),
                    Spec :: term()) ->
          {ok, case_id()} | {error, term()}.

start_workflow(Engine, Spec) ->
    start_workflow(Engine, Spec, #{}).

%%--------------------------------------------------------------------
%% @doc Starts a new workflow case with initial data.
%%
%% The Options map can contain:
%% - `cre_master': Name/pid of CRE master for task distribution
%% - `observers': List of pids to observe case execution
%% - `case_id': Specific case ID (auto-generated if not provided)
%%
%% Returns `{ok, CaseId}' on success.
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(Engine :: atom() | pid(),
                    Spec :: term(),
                    Options :: map()) ->
          {ok, case_id()} | {error, term()}.

start_workflow(Engine, Spec, Options) ->
    gen_server:call(Engine, {start_workflow, Spec, Options}).

%%--------------------------------------------------------------------
%% @doc Cancels a running workflow case.
%%
%% All active work items are cancelled and resources are released.
%%
%% Returns `ok' on success.
%% @end
%%--------------------------------------------------------------------
-spec cancel_case(Engine :: atom() | pid()) ->
          ok | {error, case_not_found | term()}.

cancel_case(Engine) ->
    gen_server:call(Engine, cancel_case).

%%--------------------------------------------------------------------
%% @doc Suspends a running workflow case.
%%
%% The case state is preserved and can be resumed later.
%% Active work items are allowed to complete.
%%
%% Returns `ok' on success.
%% @end
%%--------------------------------------------------------------------
-spec suspend_case(Engine :: atom() | pid()) ->
          ok | {error, case_not_found | term()}.

suspend_case(Engine) ->
    gen_server:call(Engine, suspend_case).

%%--------------------------------------------------------------------
%% @doc Resumes a suspended workflow case.
%%
%% Returns `ok' on success.
%% @end
%%--------------------------------------------------------------------
-spec resume_case(Engine :: atom() | pid()) ->
          ok | {error, case_not_found | term()}.

resume_case(Engine) ->
    gen_server:call(Engine, resume_case).

%%--------------------------------------------------------------------
%% @doc Gets the current state of a workflow case.
%%
%% Returns a map containing:
%% - `case_id': The case identifier
%% - `status': Current case status
%% - `workitems': Map of workitem states
%% - `data': Case variable data
%% - `timestamps': Creation, start, completion times
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_state(Engine :: atom() | pid()) ->
          {ok, map()} | {error, case_not_found}.

get_case_state(Engine) ->
    gen_server:call(Engine, get_case_state).

%%--------------------------------------------------------------------
%% @doc Gets available (enabled) work items for a case.
%%
%% Returns a list of workitem records ready to be executed.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_available_workitems(Engine :: atom() | pid()) ->
          {ok, [#workitem{}]} | {error, case_not_found}.

get_available_workitems(Engine) ->
    gen_server:call(Engine, get_available_workitems).

%%--------------------------------------------------------------------
%% @doc Starts execution of a work item.
%%
%% The work item must be in `enabled' status.
%% Returns `ok' on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workitem(Engine :: atom() | pid(),
                    WorkItemId :: workitem_id()) ->
          ok | {error, term()}.

start_workitem(Engine, WorkItemId) ->
    gen_server:call(Engine, {start_workitem, WorkItemId}).

%%--------------------------------------------------------------------
%% @doc Completes a work item with results.
%%
%% The Results map contains output data from task execution.
%% Returns `ok' on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec complete_workitem(Engine :: atom() | pid(),
                       WorkItemId :: workitem_id(),
                       Results :: map()) ->
          ok | {error, term()}.

complete_workitem(Engine, WorkItemId, Results) ->
    gen_server:call(Engine, {complete_workitem, WorkItemId, Results}).

%%--------------------------------------------------------------------
%% @doc Marks a work item as failed with error information.
%%
%% The ErrorInfo contains details about the failure.
%% Returns `ok' on success.
%%
%% @end
%%--------------------------------------------------------------------
-spec fail_workitem(Engine :: atom() | pid(),
                   WorkItemId :: workitem_id(),
                   ErrorInfo :: map()) ->
          ok | {error, term()}.

fail_workitem(Engine, WorkItemId, ErrorInfo) ->
    gen_server:call(Engine, {fail_workitem, WorkItemId, ErrorInfo}).

%%--------------------------------------------------------------------
%% @doc Registers an observer for engine events.
%%
%% Observers receive messages about case state changes.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_observer(Engine :: atom() | pid(), ObserverPid :: pid()) ->
          ok.

register_observer(Engine, ObserverPid) ->
    gen_server:cast(Engine, {register_observer, ObserverPid}).

%%--------------------------------------------------------------------
%% @doc Unregisters an observer.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_observer(Engine :: atom() | pid(), ObserverPid :: pid()) ->
          ok.

unregister_observer(Engine, ObserverPid) ->
    gen_server:cast(Engine, {unregister_observer, ObserverPid}).

%%--------------------------------------------------------------------
%% @doc Gets the current engine status.
%%
%% Returns a map with engine statistics and state.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_engine_status(Engine :: atom() | pid()) ->
          {ok, map()}.

get_engine_status(Engine) ->
    gen_server:call(Engine, get_engine_status).

%%--------------------------------------------------------------------
%% @doc Lists all cases managed by the engine.
%%
%% Returns a list of case identifiers with their status.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_cases(Engine :: atom() | pid()) ->
          {ok, [{case_id(), case_status()}]}.

list_cases(Engine) ->
    gen_server:call(Engine, list_cases).

%%--------------------------------------------------------------------
%% @doc Recovers active cases from persistence on engine startup.
%%
%% This should be called after starting the engine to restore any
%% active cases that were persisted before shutdown. Returns the count
%% of recovered cases.
%%
%% @end
%%--------------------------------------------------------------------
-spec recover_active_cases() -> {ok, non_neg_integer()} | {error, term()}.

recover_active_cases() ->
    gen_server:call(?MODULE, recover_active_cases).

%%--------------------------------------------------------------------
%% @doc Enables persistence for workflow cases and work items.
%%
%% When enabled, case state changes are automatically persisted
%% to Mnesia for recovery after restart.
%%
%% @end
%%--------------------------------------------------------------------
-spec enable_persistence() -> ok.

enable_persistence() ->
    gen_server:call(?MODULE, enable_persistence).

%%--------------------------------------------------------------------
%% @doc Disables persistence.
%%
%% Existing persisted data remains but new changes are not saved.
%%
%% @end
%%--------------------------------------------------------------------
-spec disable_persistence() -> ok.

disable_persistence() ->
    gen_server:call(?MODULE, disable_persistence).

%%--------------------------------------------------------------------
%% @doc Checks if persistence is enabled.
%%
%% Returns true if persistence is active, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_persistence_enabled() -> boolean().

is_persistence_enabled() ->
    gen_server:call(?MODULE, is_persistence_enabled).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%% @private
init(_Arg) ->
    process_flag(trap_exit, true),

    %% Check if persistence should be enabled via application environment
    PersistenceEnabled = application:get_env(yawl, persistence_enabled, false),

    %% Initialize persistence schema if enabled
    case PersistenceEnabled of
        true ->
            case yawl_persistence:init_schema() of
                ok ->
                    logger:info("Persistence schema initialized",
                                 [{module, ?MODULE}, {persistence, enabled},
                                  {action, schema_initialized}]),
                    {ok, #engine_state{persistence_enabled = true}};
                {error, Reason} ->
                    logger:error("Persistence schema initialization failed: error=~p",
                                  [Reason],
                                  [{module, ?MODULE}, {persistence, failed},
                                   {action, schema_initialized}]),
                    {ok, #engine_state{persistence_enabled = false}}
            end;
        false ->
            {ok, #engine_state{persistence_enabled = PersistenceEnabled}}
    end.

%% @private
handle_call({start_workflow, Spec, Options}, _From, State) ->
    #engine_state{cases = Cases, next_case_id = NextId, cre_master = CreMaster,
                  persistence_enabled = PersistEnabled} = State,

    %% Determine CRE master from options or state
    Master = maps:get(cre_master, Options, CreMaster),

    %% Parse specification if needed
    ParsedSpec = parse_specification(Spec),

    %% Generate case ID
    CaseId = maps:get(case_id, Options, generate_case_id(NextId)),

    %% Initialize workflow case
    Now = erlang:system_time(millisecond),
    Observers = maps:get(observers, Options, []),

    Case = #workflow_case{
      case_id = CaseId,
      workflow_id = extract_workflow_id(ParsedSpec),
      spec = ParsedSpec,
      status = initialized,
      created_at = Now,
      observers = Observers
    },

    %% Initialize Petri net for the workflow
    case initialize_net(ParsedSpec, Case, Master) of
        {ok, CaseWithNet} ->
            %% Store case and update state
            Cases1 = Cases#{CaseId => CaseWithNet},
            State1 = State#engine_state{
                        cases = Cases1,
                        next_case_id = NextId + 1
                       },

            %% Persist initial case state if enabled
            case PersistEnabled of
                true ->
                    safe_persist_case(CaseWithNet);
                false ->
                    ok
            end,

            %% Notify observers
            notify_observers(Observers, {case_started, CaseId}),
            logger:info("Workflow started: id=~p workflow=~p",
                        [CaseId, CaseWithNet#workflow_case.workflow_id],
                        [{info, workflow_started}, {module, ?MODULE}]),

            %% Start workflow execution
            case start_execution(CaseWithNet) of
                {ok, RunningCase} ->
                    %% Persist running state
                    case PersistEnabled of
                        true ->
                            safe_persist_case(RunningCase);
                        false ->
                            ok
                    end,
                    {reply, {ok, CaseId}, State1#engine_state{cases = Cases1#{CaseId => RunningCase}}};
                {error, Reason} ->
                    {reply, {error, Reason}, State1}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(cancel_case, _From, #engine_state{cases = Cases, persistence_enabled = PersistEnabled} = State) ->
    case maps:keys(Cases) of
        [CaseId | _] ->
            Case = maps:get(CaseId, Cases),
            case cancel_workflow_case(Case) of
                {ok, CancelledCase} ->
                    notify_observers(Case#workflow_case.observers, {case_cancelled, CaseId}),
                    Cases1 = Cases#{CaseId => CancelledCase},
                    %% Persist cancelled state
                    case PersistEnabled of
                        true ->
                            safe_persist_case(CancelledCase);
                        false ->
                            ok
                    end,
                    {reply, ok, State#engine_state{cases = Cases1}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, no_cases}, State}
    end;

handle_call(suspend_case, _From, #engine_state{cases = Cases, persistence_enabled = PersistEnabled} = State) ->
    case maps:keys(Cases) of
        [CaseId | _] ->
            Case = maps:get(CaseId, Cases),
            case suspend_workflow_case(Case) of
                {ok, SuspendedCase} ->
                    notify_observers(Case#workflow_case.observers, {case_suspended, CaseId}),
                    Cases1 = Cases#{CaseId => SuspendedCase},
                    %% Persist suspended state
                    case PersistEnabled of
                        true ->
                            safe_persist_case(SuspendedCase);
                        false ->
                            ok
                    end,
                    {reply, ok, State#engine_state{cases = Cases1}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, no_cases}, State}
    end;

handle_call(resume_case, _From, #engine_state{cases = Cases, persistence_enabled = PersistEnabled} = State) ->
    case maps:keys(Cases) of
        [CaseId | _] ->
            Case = maps:get(CaseId, Cases),
            case resume_workflow_case(Case) of
                {ok, ResumedCase} ->
                    notify_observers(Case#workflow_case.observers, {case_resumed, CaseId}),
                    Cases1 = Cases#{CaseId => ResumedCase},
                    %% Persist resumed state
                    case PersistEnabled of
                        true ->
                            safe_persist_case(ResumedCase);
                        false ->
                            ok
                    end,
                    {reply, ok, State#engine_state{cases = Cases1}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, no_cases}, State}
    end;

handle_call(get_case_state, _From, #engine_state{cases = Cases}) ->
    case maps:keys(Cases) of
        [CaseId | _] ->
            Case = maps:get(CaseId, Cases),
            StateMap = #{
              case_id => Case#workflow_case.case_id,
              workflow_id => Case#workflow_case.workflow_id,
              status => Case#workflow_case.status,
              workitems => maps:map(fun(_K, W) -> workitem_to_map(W) end,
                                    Case#workflow_case.workitems),
              data => Case#workflow_case.data,
              timestamps => #{
                created_at => Case#workflow_case.created_at,
                started_at => Case#workflow_case.started_at,
                completed_at => Case#workflow_case.completed_at
              }
             },
            {reply, {ok, StateMap}, #engine_state{cases = Cases}};
        [] ->
            {reply, {error, no_cases}, #engine_state{cases = Cases}}
    end;

handle_call(get_available_workitems, _From, #engine_state{cases = Cases}) ->
    case maps:keys(Cases) of
        [CaseId | _] ->
            Case = maps:get(CaseId, Cases),
            Available = [W || W <- maps:values(Case#workflow_case.workitems),
                            W#workitem.status =:= enabled],
            {reply, {ok, Available}, #engine_state{cases = Cases}};
        [] ->
            {reply, {error, no_cases}, #engine_state{cases = Cases}}
    end;

handle_call({start_workitem, WorkItemId}, _From, #engine_state{cases = Cases, persistence_enabled = PersistEnabled} = State) ->
    case find_workitem(Cases, WorkItemId) of
        {ok, Case, Workitem} ->
            case do_start_workitem(Workitem) of
                {ok, UpdatedWorkitem} ->
                    Workitems0 = Case#workflow_case.workitems,
                    Workitems1 = maps:put(WorkItemId, UpdatedWorkitem, Workitems0),
                    Case1 = Case#workflow_case{workitems = Workitems1},
                    Cases1 = Cases#{Case#workflow_case.case_id => Case1},
                    notify_observers(Case#workflow_case.observers,
                                    {workitem_started, WorkItemId}),
                    %% Persist workitem state if enabled
                    case PersistEnabled of
                        true ->
                            safe_persist_workitem(UpdatedWorkitem);
                        false ->
                            ok
                    end,
                    {reply, ok, State#engine_state{cases = Cases1}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({complete_workitem, WorkItemId, Results}, _From, #engine_state{cases = Cases, persistence_enabled = PersistEnabled} = State) ->
    case find_workitem(Cases, WorkItemId) of
        {ok, Case, Workitem} ->
            case do_complete_workitem(Workitem, Results) of
                {ok, UpdatedWorkitem} ->
                    Workitems0 = Case#workflow_case.workitems,
                    Workitems1 = maps:put(WorkItemId, UpdatedWorkitem, Workitems0),
                    Case1 = Case#workflow_case{workitems = Workitems1},
                    Cases1 = Cases#{Case#workflow_case.case_id => Case1},
                    notify_observers(Case#workflow_case.observers,
                                    {workitem_completed, WorkItemId, Results}),
                    %% Persist workitem completion if enabled
                    case PersistEnabled of
                        true ->
                            safe_persist_workitem(UpdatedWorkitem);
                        false ->
                            ok
                    end,
                    %% Advance the workflow
                    case advance_workflow(Case1) of
                        {ok, AdvancedCase} ->
                            Cases2 = Cases1#{Case#workflow_case.case_id => AdvancedCase},
                            %% Persist advanced case state
                            case PersistEnabled of
                                true ->
                                    safe_persist_case(AdvancedCase);
                                false ->
                                    ok
                            end,
                            {reply, ok, State#engine_state{cases = Cases2}};
                        {complete, CompletedCase} ->
                            Cases2 = Cases1#{Case#workflow_case.case_id => CompletedCase},
                            notify_observers(CompletedCase#workflow_case.observers,
                                            {case_completed, CompletedCase#workflow_case.case_id}),
                            %% Persist completed case and optionally delete
                            case PersistEnabled of
                                true ->
                                    safe_persist_case(CompletedCase),
                                    %% Optionally delete completed cases immediately
                                    %% or keep for audit trail
                                    ok;
                                false ->
                                    ok
                            end,
                            {reply, ok, State#engine_state{cases = Cases2}}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({fail_workitem, WorkItemId, ErrorInfo}, _From, #engine_state{cases = Cases, persistence_enabled = PersistEnabled} = State) ->
    case find_workitem(Cases, WorkItemId) of
        {ok, Case, Workitem} ->
            case do_fail_workitem(Workitem, ErrorInfo) of
                {ok, UpdatedWorkitem} ->
                    Workitems0 = Case#workflow_case.workitems,
                    Workitems1 = maps:put(WorkItemId, UpdatedWorkitem, Workitems0),
                    Case1 = Case#workflow_case{workitems = Workitems1},
                    Cases1 = Cases#{Case#workflow_case.case_id => Case1},
                    notify_observers(Case#workflow_case.observers,
                                    {workitem_failed, WorkItemId, ErrorInfo}),
                    %% Persist failed workitem if enabled
                    case PersistEnabled of
                        true ->
                            safe_persist_workitem(UpdatedWorkitem);
                        false ->
                            ok
                    end,
                    {reply, ok, State#engine_state{cases = Cases1}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_engine_status, _From, #engine_state{cases = Cases, observers = Observers} = State) ->
    CaseCount = maps:size(Cases),
    StatusCounts = count_case_status(Cases),

    StatusMap = #{
      total_cases => CaseCount,
      active_cases => maps:get(running, StatusCounts, 0),
      suspended_cases => maps:get(suspended, StatusCounts, 0),
      completed_cases => maps:get(completed, StatusCounts, 0),
      cancelled_cases => maps:get(cancelled, StatusCounts, 0),
      failed_cases => maps:get(failed, StatusCounts, 0),
      observer_count => length(Observers)
     },
    {reply, {ok, StatusMap}, State};

handle_call(list_cases, _From, #engine_state{cases = Cases} = State) ->
    CaseList = [{CaseId, Case#workflow_case.status} ||
                   CaseId <- maps:keys(Cases),
                   Case <- [maps:get(CaseId, Cases)]],
    {reply, {ok, CaseList}, State};

handle_call(recover_active_cases, _From, #engine_state{cases = Cases} = State) ->
    case yawl_persistence:list_active_cases() of
        {ok, ActiveCases} ->
            {RecoveredCount, Cases1} = lists:foldl(fun(CaseMap, {Count, CasesAcc}) ->
                CaseId = maps:get(case_id, CaseMap),
                %% Check if case already exists in memory
                case maps:is_key(CaseId, CasesAcc) of
                    false ->
                        %% Restore case from persisted data
                        case restore_case_from_map(CaseMap) of
                            {ok, RestoredCase} ->
                                logger:info("Case recovered: id=~p", [CaseId],
                                             [{module, ?MODULE},
                                              {action, case_recovered}]),
                                {Count + 1, CasesAcc#{CaseId => RestoredCase}};
                            {error, Reason} ->
                                logger:error("Case recovery failed: id=~p error=~p",
                                              [CaseId, Reason],
                                              [{module, ?MODULE},
                                               {action, case_recovery_failed}]),
                                {Count, CasesAcc}
                        end;
                    true ->
                        %% Case already in memory, skip
                        {Count, CasesAcc}
                end
            end, {0, Cases}, ActiveCases),
            State1 = State#engine_state{cases = Cases1},
            {reply, {ok, RecoveredCount}, State1};
        {error, Reason} ->
            logger:error("Recover active cases failed: reason=~p",
                        [Reason],
                        [{module, ?MODULE}, {action, recover_active_cases}]),
            {reply, {error, Reason}, State}
    end;

handle_call(enable_persistence, _From, State) ->
    State1 = State#engine_state{persistence_enabled = true},
    logger:info("Persistence enabled", [{module, ?MODULE}, {action, persistence_enabled}]),
    {reply, ok, State1};

handle_call(disable_persistence, _From, State) ->
    State1 = State#engine_state{persistence_enabled = false},
    logger:info("Persistence disabled", [{module, ?MODULE}, {action, persistence_disabled}]),
    {reply, ok, State1};

handle_call(is_persistence_enabled, _From, #engine_state{persistence_enabled = Enabled}) ->
    {reply, Enabled, #engine_state{persistence_enabled = Enabled}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
handle_cast({register_observer, ObserverPid}, #engine_state{observers = Observers} = State) ->
    case lists:member(ObserverPid, Observers) of
        true ->
            {noreply, State};
        false ->
            erlang:monitor(process, ObserverPid),
            {noreply, State#engine_state{observers = [ObserverPid | Observers]}}
    end;

handle_cast({unregister_observer, ObserverPid}, #engine_state{observers = Observers} = State) ->
    Observers1 = lists:delete(ObserverPid, Observers),
    {noreply, State#engine_state{observers = Observers1}};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, #engine_state{observers = Observers} = State) ->
    Observers1 = lists:delete(Pid, Observers),
    {noreply, State#engine_state{observers = Observers1}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Generates a unique case ID.
-spec generate_case_id(non_neg_integer()) -> case_id().

generate_case_id(N) ->
    Timestamp = erlang:system_time(millisecond),
    Hash = binary:encode_hex(crypto:hash(md5, term_to_binary({self(), Timestamp, N}))),
    <<"case_", Hash/binary>>.

%% @private
%% @doc Parses a workflow specification.
-spec parse_specification(term()) -> term().

parse_specification(Spec) when is_list(Spec) ->
    case filelib:is_file(Spec) of
        true ->
            %% Try to parse as YAWL XML file
            case yawl_schema:parse_specification(Spec) of
                {ok, Parsed} -> Parsed;
                {error, _} ->
                    %% Return as-is if parsing fails
                    {error, invalid_specification}
            end;
        false ->
            {error, invalid_specification}
    end;
parse_specification(Spec) when is_map(Spec) ->
    Spec;
parse_specification(_Spec) ->
    {error, invalid_specification}.

%% @private
%% @doc Extracts workflow ID from specification.
-spec extract_workflow_id(term()) -> workflow_id().

extract_workflow_id(#{id := Id}) when is_binary(Id) ->
    Id;
extract_workflow_id(_Spec) ->
    Timestamp = erlang:system_time(millisecond),
    Hash = binary:encode_hex(crypto:hash(md5, <<Timestamp:64>>)),
    <<"workflow_", Hash/binary>>.

%% @private
%% @doc Initializes the Petri net for a workflow case.
-spec initialize_net(term(), #workflow_case{}, atom() | pid() | undefined) ->
          {ok, #workflow_case{}} | {error, term()}.

initialize_net(Spec, Case, _CreMaster) ->
    try
        %% Create initial marking for InputCondition
        NetState = #{
          places => #{input => [start], output => []},
          transitions => #{},
          markings => #{input => [start]}
         },

        %% Create work items for initial tasks
        Workitems = initialize_workitems(Spec, Case#workflow_case.case_id),

        {ok, Case#workflow_case{
                net_state = NetState,
                workitems = Workitems
               }}
    catch
        Kind:Reason:Stack ->
            {error, {Kind, Reason, Stack}}
    end.

%% @private
%% @doc Initializes work items from workflow specification.
-spec initialize_workitems(term(), case_id()) -> #{workitem_id() => #workitem{}}.

initialize_workitems(Spec, CaseId) when is_map(Spec) ->
    Tasks = maps:get(tasks, Spec, #{}),
    Now = erlang:system_time(millisecond),
    maps:map(fun(TaskId, _Task) ->
        WorkitemId = generate_workitem_id(CaseId, TaskId),
        #workitem{
          id = WorkitemId,
          case_id = CaseId,
          task_id = TaskId,
          status = enabled,
          data = #{},
          enabled_at = Now
        }
    end, Tasks);
initialize_workitems(_Spec, _CaseId) ->
    #{}.

%% @private
%% @doc Generates a unique work item ID.
-spec generate_workitem_id(case_id(), task_id()) -> workitem_id().

generate_workitem_id(CaseId, TaskId) ->
    <<CaseId/binary, "_wi_", (binary:encode_hex(crypto:hash(md5, TaskId)))/binary>>.

%% @private
%% @doc Starts execution of a workflow case.
-spec start_execution(#workflow_case{}) ->
          {ok, #workflow_case{}} | {error, term()}.

start_execution(Case) ->
    Now = erlang:system_time(millisecond),
    Case1 = Case#workflow_case{
                status = running,
                started_at = Now
               },
    {ok, Case1}.

%% @private
%% @doc Cancels a workflow case.
-spec cancel_workflow_case(#workflow_case{}) ->
          {ok, #workflow_case{}} | {error, term()}.

cancel_workflow_case(Case) ->
    Now = erlang:system_time(millisecond),
    %% Cancel all active work items
    Workitems1 = maps:map(fun(_K, W) ->
        case W#workitem.status of
            enabled -> W#workitem{status = cancelled};
            started -> W#workitem{status = cancelled, completed_at = Now};
            _ -> W
        end
    end, Case#workflow_case.workitems),

    Case1 = Case#workflow_case{
                status = cancelled,
                completed_at = Now,
                workitems = Workitems1
               },
    {ok, Case1}.

%% @private
%% @doc Suspends a workflow case.
-spec suspend_workflow_case(#workflow_case{}) ->
          {ok, #workflow_case{}} | {error, term()}.

suspend_workflow_case(#workflow_case{status = running} = Case) ->
    Case1 = Case#workflow_case{status = suspended},
    {ok, Case1};
suspend_workflow_case(#workflow_case{status = Status}) ->
    {error, {invalid_status, Status}}.

%% @private
%% @doc Resumes a suspended workflow case.
-spec resume_workflow_case(#workflow_case{}) ->
          {ok, #workflow_case{}} | {error, term()}.

resume_workflow_case(#workflow_case{status = suspended} = Case) ->
    Case1 = Case#workflow_case{status = running},
    {ok, Case1};
resume_workflow_case(#workflow_case{status = Status}) ->
    {error, {invalid_status, Status}}.

%% @private
%% @doc Finds a work item by ID across all cases.
-spec find_workitem(#{case_id() => #workflow_case{}}, workitem_id()) ->
          {ok, #workflow_case{}, #workitem{}} | {error, workitem_not_found | case_not_found}.

find_workitem(Cases, WorkitemId) ->
    case maps:keys(Cases) of
        [] ->
            {error, case_not_found};
        [CaseId | _] ->
            Case = maps:get(CaseId, Cases),
            %% Look for workitem by ID in the values of the workitems map
            %% The workitems map is keyed by task_id, so we need to search values
            Found = lists:search(
                fun({_TaskId, W}) -> W#workitem.id =:= WorkitemId end,
                maps:to_list(Case#workflow_case.workitems)
            ),
            case Found of
                {value, {_TaskId, Workitem}} ->
                    {ok, Case, Workitem};
                false ->
                    {error, workitem_not_found}
            end
    end.

%% @private
%% @doc Starts a work item.
-spec do_start_workitem(#workitem{}) ->
          {ok, #workitem{}} | {error, term()}.

do_start_workitem(#workitem{status = enabled} = Workitem) ->
    Now = erlang:system_time(millisecond),
    {ok, Workitem#workitem{
                status = started,
                started_at = Now
               }};
do_start_workitem(#workitem{status = Status}) ->
    {error, {invalid_status, Status}}.

%% @private
%% @doc Completes a work item with results.
-spec do_complete_workitem(#workitem{}, map()) ->
          {ok, #workitem{}} | {error, term()}.

do_complete_workitem(#workitem{status = started} = Workitem, Results) ->
    Now = erlang:system_time(millisecond),
    {ok, Workitem#workitem{
                status = completed,
                completed_at = Now,
                data = maps:merge(Workitem#workitem.data, Results)
               }};
do_complete_workitem(#workitem{status = Status}, _Results) ->
    {error, {invalid_status, Status}}.

%% @private
%% @doc Fails a work item with error information.
-spec do_fail_workitem(#workitem{}, map()) ->
          {ok, #workitem{}} | {error, term()}.

do_fail_workitem(#workitem{status = started} = Workitem, ErrorInfo) ->
    Now = erlang:system_time(millisecond),
    {ok, Workitem#workitem{
                status = failed,
                completed_at = Now,
                data = maps:put(error, ErrorInfo, Workitem#workitem.data)
               }};
do_fail_workitem(#workitem{status = Status}, _ErrorInfo) ->
    {error, {invalid_status, Status}}.

%% @private
%% @doc Advances workflow execution after work item completion.
-spec advance_workflow(#workflow_case{}) ->
          {ok, #workflow_case{}} | {complete, #workflow_case{}}.

advance_workflow(Case) ->
    %% Check if all work items are completed
    Workitems = maps:values(Case#workflow_case.workitems),
    AllComplete = lists:all(fun(W) -> W#workitem.status =:= completed end, Workitems),

    %% Enable new work items based on workflow structure
    Workitems1 = enable_next_workitems(Case#workflow_case.spec, Case#workflow_case.workitems),

    case AllComplete of
        true ->
            Now = erlang:system_time(millisecond),
            {complete, Case#workflow_case{
                         status = completed,
                         completed_at = Now,
                         workitems = Workitems1
                        }};
        false ->
            {ok, Case#workflow_case{workitems = Workitems1}}
    end.

%% @private
%% @doc Enables work items based on workflow transitions.
-spec enable_next_workitems(term(), #{workitem_id() => #workitem{}}) ->
          #{workitem_id() => #workitem{}}.

enable_next_workitems(Spec, Workitems) when is_map(Spec) ->
    Conns = maps:get(flows, Spec, []),
    Now = erlang:system_time(millisecond),

    %% Find all work items that can be enabled
    EnableFun = fun(_WorkitemId, #workitem{status = completed} = W, Acc) ->
            %% Find outgoing connections
            Outgoing = [Conn || #{source := From} = Conn <- Conns,
                               From =:= W#workitem.task_id],

            %% Enable downstream tasks
            lists:foldl(fun(#{target := ToId}, AccIn) ->
                %% Find work item for this task
                case lists:keyfind(ToId, 3, maps:values(AccIn)) of
                    false ->
                        AccIn;
                    #workitem{id = Id, status = enabled} = DownstreamW ->
                        maps:put(Id, DownstreamW#workitem{enabled_at = Now}, AccIn);
                    #workitem{id = Id} = DownstreamW ->
                        %% Check join type for enabling
                        DownstreamW1 = DownstreamW#workitem{status = enabled, enabled_at = Now},
                        maps:put(Id, DownstreamW1, AccIn)
                end
            end, Acc, Outgoing);
       (_WorkitemId, _Workitem, Acc) ->
            Acc
    end,
    maps:fold(EnableFun, Workitems, Workitems);
enable_next_workitems(_Spec, Workitems) ->
    Workitems.

%% @private
%% @doc Counts cases by status.
-spec count_case_status(#{case_id() => #workflow_case{}}) ->
          #{case_status() => non_neg_integer()}.

count_case_status(Cases) ->
    lists:foldl(fun({_CaseId, #workflow_case{status = Status}}, Acc) ->
        Acc#{Status => maps:get(Status, Acc, 0) + 1}
    end, #{}, maps:to_list(Cases)).

%% @private
%% @doc Converts a work item record to a map.
-spec workitem_to_map(#workitem{}) -> map().

workitem_to_map(#workitem{} = W) ->
    #{
      id => W#workitem.id,
      case_id => W#workitem.case_id,
      task_id => W#workitem.task_id,
      status => W#workitem.status,
      data => W#workitem.data,
      enabled_at => W#workitem.enabled_at,
      started_at => W#workitem.started_at,
      completed_at => W#workitem.completed_at,
      assigned_to => W#workitem.assigned_to
     }.

%% @private
%% @doc Notifies observers of events.
-spec notify_observers([pid()], term()) -> ok.

notify_observers([], _Event) ->
    ok;
notify_observers(Observers, Event) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! {yawl_event, Event}
    end, Observers),
    ok.

%%====================================================================
%% Persistence Helper Functions
%%====================================================================

%% @private
%% @doc Safely persists a case without affecting workflow if persistence fails.
-spec safe_persist_case(#workflow_case{}) -> ok.

safe_persist_case(Case) ->
    try
        case yawl_persistence:save_case(Case) of
            {ok, _CaseId} ->
                ok;
            {error, Reason} ->
                %% Log but don't fail the workflow
                logger:error("Persist case failed: id=~p error=~p",
                                  [Case#workflow_case.case_id, Reason],
                                  [{module, ?MODULE}, {action, persist_case_failed}]),
                ok
        end
    catch
        Kind:ErrorReason:Stack ->
            logger:error("Persist case crashed: id=~p kind=~p reason=~p",
                          [Case#workflow_case.case_id, Kind, ErrorReason],
                          [{module, ?MODULE}, {action, persist_case_crashed},
                           {stacktrace, Stack}]),
            ok
    end.

%% @private
%% @doc Safely persists a workitem without affecting workflow if persistence fails.
-spec safe_persist_workitem(#workitem{}) -> ok.

safe_persist_workitem(Workitem) ->
    try
        case yawl_persistence:save_workitem(Workitem) of
            {ok, _WorkitemId} ->
                ok;
            {error, Reason} ->
                %% Log but don't fail the workflow
                logger:error("Persist workitem failed: id=~p error=~p",
                                  [Workitem#workitem.id, Reason],
                                  [{module, ?MODULE}, {action, persist_workitem_failed}]),
                ok
        end
    catch
        Kind:ErrorReason:Stack ->
            logger:error("Persist workitem crashed: id=~p kind=~p reason=~p",
                          [Workitem#workitem.id, Kind, ErrorReason],
                          [{module, ?MODULE}, {action, persist_workitem_crashed},
                           {stacktrace, Stack}]),
            ok
    end.

%% @private
%% @doc Restores a workflow case from a persisted map.
-spec restore_case_from_map(map()) -> {ok, #workflow_case{}} | {error, term()}.

restore_case_from_map(CaseMap) ->
    try
        CaseId = maps:get(case_id, CaseMap),
        %% Load workitems for this case
        {ok, WorkitemMaps} = yawl_persistence:load_workitems(CaseId),

        %% Convert workitem maps back to records
        Workitems = lists:foldl(fun(WIMap, Acc) ->
            WorkitemId = maps:get(workitem_id, WIMap),
            Workitem = #workitem{
                id = WorkitemId,
                case_id = maps:get(case_id, WIMap),
                task_id = maps:get(task_id, WIMap),
                status = maps:get(status, WIMap),
                data = maps:get(data, WIMap, #{}),
                enabled_at = maps:get(enabled_at, WIMap, undefined),
                started_at = maps:get(started_at, WIMap, undefined),
                completed_at = maps:get(completed_at, WIMap, undefined),
                assigned_to = undefined
            },
            Acc#{WorkitemId => Workitem}
        end, #{}, WorkitemMaps),

        %% Reconstruct the workflow case
        RestoredCase = #workflow_case{
            case_id = CaseId,
            workflow_id = maps:get(workflow_id, CaseMap),
            spec = maps:get(spec, CaseMap),
            status = maps:get(status, CaseMap),
            workitems = Workitems,
            data = maps:get(data, CaseMap, #{}),
            created_at = maps:get(created_at, CaseMap),
            started_at = maps:get(started_at, CaseMap, undefined),
            completed_at = maps:get(completed_at, CaseMap, undefined),
            observers = [],
            net_state = undefined  %% Net state not persisted, will be re-initialized
        },

        {ok, RestoredCase}
    catch
        Kind:Reason:Stack ->
            logger:error("Case restore failed: kind=~p reason=~p",
                          [Kind, Reason],
                          [{module, ?MODULE}, {action, restore_case_failed},
                           {reason, Reason}, {stacktrace, Stack}]),
            {error, {Kind, Reason}}
    end.

%% @private
%% @doc Deletes a completed case from persistence.
%% Exported for use by maintenance operations.
%% @end
%%--------------------------------------------------------------------
-spec delete_completed_case(case_id()) -> ok | {error, term()}.

delete_completed_case(CaseId) ->
    try
        case yawl_persistence:delete_case(CaseId) of
            ok ->
                logger:info("Completed case deleted: id=~p", [CaseId],
                             [{module, ?MODULE}, {action, deleted_completed_case}]),
                ok;
            {error, Reason} ->
                logger:error("Delete case failed: id=~p error=~p",
                                  [CaseId, Reason],
                                  [{module, ?MODULE}, {action, delete_case_failed}]),
                {error, Reason}
        end
    catch
        Kind:ErrorReason:Stack ->
            logger:error("Delete case crashed: id=~p kind=~p reason=~p",
                          [CaseId, Kind, ErrorReason],
                          [{module, ?MODULE}, {action, delete_case_crashed},
                           {reason, ErrorReason}, {stacktrace, Stack}]),
            {error, {Kind, ErrorReason}}
    end.
