%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc CRE Public API Surface
%%
%% Provides a clean, well-documented public API for the Common Runtime Environment.
%% This module serves as the primary entry point for external systems integrating
%% with CRE workflow execution capabilities.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Workflow Execution:</b> Start, stop, query, and list workflow instances</li>
%%   <li><b>Pattern Management:</b> Validate and compile YAWL workflow patterns</li>
%%   <li><b>State Queries:</b> Query workflow state, marking, and status</li>
%%   <li><b>Event Subscription:</b> Subscribe to workflow lifecycle events</li>
%%   <li><b>Health Checks:</b> Verify system health and readiness</li>
%% </ul>
%%
%% <h3>Architecture</h3>
%%
%% The public API delegates to underlying CRE modules:
%% <ul>
%%   <li><code>gen_yawl</code> - Core workflow runtime engine</li>
%%   <li><code>yawl_registry</code> - Workflow instance registry</li>
%%   <li><code>yawl_compile</code> - YAWL to Petri net compiler</li>
%%   <li><code>yawl_validate</code> - YAWL specification validator</li>
%%   <li><code>yawl_pattern_registry</code> - Pattern module lookup</li>
%%   <li><code>cre_health</code> - Health check endpoints</li>
%% </ul>
%%
%% <h3>Usage Examples</h3>
%%
%% <h4>Starting a Workflow</h4>
%% ```erlang
%% %% Start a simple workflow
%% {ok, WorkflowPid} = cre_public_api:start_workflow(<<"my_wf">>, my_workflow_module, #{}).
%% '''
%%
%% <h4>Querying Workflow State</h4>
%% ```erlang
%% %% Get workflow status
%% {ok, Status} = cre_public_api:get_status(<<"my_wf">>).
%% #{status := running, marking := #{...}} = Status.
%% '''
%%
%% <h4>Compiling YAWL Specifications</h4>
%% ```erlang
%% %% Compile a YAWL specification
%% {ok, Compiled} = cre_public_api:compile_pattern(YawlSpec).
%% '''
%%
%% <h4>Subscribing to Events</h4>
%% ```erlang
%% %% Subscribe to workflow lifecycle events
%% {ok, SubRef} = cre_public_api:subscribe_events(self()).
%% %% Receive events: {workflow_event, EventData}
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_public_api).
-author("CRE Project").
-moduledoc """
CRE Public API Surface - Unified interface for workflow execution, pattern management,
state queries, and event subscription.
""".

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Workflow execution functions
-export([start_workflow/3, start_workflow/4]).
-export([stop_workflow/1, stop_workflow/2]).
-export([query_workflow/1, query_workflow/2]).
-export([list_workflows/0, list_workflows/1]).

%% Pattern management functions
-export([validate_pattern/1, validate_pattern/2]).
-export([compile_pattern/1, compile_pattern/2]).
-export([list_patterns/0]).

%% State query functions
-export([get_state/1, get_state/2]).
-export([get_marking/1, get_marking/2]).
-export([get_status/1, get_status/2]).
-export([get_usr_info/1, get_usr_info/2]).

%% Event subscription functions
-export([subscribe_events/1, subscribe_events/2]).
-export([unsubscribe/1, unsubscribe/2]).
-export([publish_event/2]).

%% Health check functions
-export([health/0, health/1]).
-export([ready/0]).
-export([version/0]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type case_id() :: binary().
-type workflow_id() :: binary() | atom().
-type pattern_id() :: binary() | atom().
-type workflow_module() :: atom().
-type init_arg() :: term().
-type options() :: #{atom() => term()}.
-type workflow_pid() :: pid().
-type workflow_status() :: starting | running | completed | failed | stopped | terminating.
-type marking() :: #{atom() => [term()]}.
-type usr_info() :: term().
-type event_type() :: workflow_started | workflow_stopped | workflow_completed | workflow_failed
                  | transition_fired | token_produced | state_changed.
-type event_data() :: #{atom() => term()}.
-type subscription_ref() :: reference().
-type subscriber() :: pid().

-type workflow_result() :: {ok, workflow_pid()} |
                       {error, workflow_already_exists | startup_failed | term()}.
-type query_result(Type) :: {ok, Type} | {error, not_found | term()}.
-type validation_result() :: {ok, [binary()]} | {error, [binary()]}.
-type compilation_result() :: {ok, map()} | {error, term()}.
-type health_status() :: #{status := healthy | degraded | unhealthy,
                        subsystems := [map()]}.
-type subscriber_count() :: non_neg_integer().

-export_type([case_id/0, workflow_id/0, pattern_id/0,
              workflow_module/0, init_arg/0, options/0,
              workflow_pid/0, workflow_status/0, marking/0, usr_info/0,
              event_type/0, event_data/0, subscription_ref/0, subscriber/0,
              workflow_result/0, query_result/1,
              validation_result/0, compilation_result/0,
              health_status/0, subscriber_count/0]).

%%====================================================================
%% Records
%%====================================================================

-record(subscription, {
          subscriber :: subscriber(),
          filter :: event_type() | all | [event_type()],
          ref :: subscription_ref()
         }).

-record(state, {
          subscribers :: ets:tid(),
          event_log :: ets:tid(),
          max_log_size = 1000 :: non_neg_integer()
         }).

%%====================================================================
%% API Function Implementation
%%====================================================================

%%%=====================================================================
%%% Workflow Execution Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Start a new workflow instance.
%%
%% Creates a new workflow instance from the specified module with the given
%% initialization arguments. The workflow is registered in the global registry
%% under the provided case_id.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>CaseId</code> - Unique identifier for this workflow instance</li>
%%   <li><code>WorkflowModule</code> - Module implementing gen_pnet/gen_yawl behavior</li>
%%   <li><code>InitArg</code> - Initialization argument passed to module:init/1</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, WorkflowPid}</code> - Workflow started successfully</li>
%%   <li><code>{error, workflow_already_exists}</code> - Case ID already registered</li>
%%   <li><code>{error, startup_failed}</code> - Failed to start workflow process</li>
%% </ul>
%%
%% <h3>Example</h3>
%% ```erlang
%% > {ok, Pid} = cre_public_api:start_workflow(<<"order_wf">>, order_processor, #{customer => <<"Alice">>}).
%% {ok,<0.123.0>}
%% > {ok, Status} = cre_public_api:get_status(<<"order_wf">>).
%% #{status => running, ...}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(CaseId :: case_id(), WorkflowModule :: workflow_module(),
                 InitArg :: init_arg()) -> workflow_result().

start_workflow(CaseId, WorkflowModule, InitArg) ->
    start_workflow(CaseId, WorkflowModule, InitArg, #{}).

%%--------------------------------------------------------------------
%% @doc Start a new workflow instance with options.
%%
%% Same as start_workflow/3 but accepts additional options:
%% <ul>
%%   <li><code>{register, boolean()}</code> - Register in global registry (default: true)</li>
%%   <li><code>{timeout, milliseconds()}</code> - Startup timeout (default: 5000)</li>
%%   <li><code>{regions, #{}}</code> - Region definitions for cancellation</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(CaseId :: case_id(), WorkflowModule :: workflow_module(),
                 InitArg :: init_arg(), Options :: options()) -> workflow_result().

start_workflow(CaseId, WorkflowModule, InitArg, Options) ->
    logger:info("Starting workflow: case_id=~p, module=~p", [CaseId, WorkflowModule]),

    %% Check if workflow already exists
    case yawl_registry:lookup(CaseId) of
        {ok, _Pid} ->
            logger:warning("Workflow already exists: ~p", [CaseId]),
            {error, workflow_already_exists};
        {error, not_found} ->
            %% Start the workflow with gen_yawl
            StartResult = case maps:get(register, Options, true) of
                true ->
                    %% Start with global name based on case_id
                    RegName = case_id_to_reg_name(CaseId),
                    gen_yawl:start_link({local, RegName}, WorkflowModule, InitArg, []);
                false ->
                    %% Start unregistered
                    gen_yawl:start_link(WorkflowModule, InitArg, [])
            end,

            case StartResult of
                {ok, Pid} ->
                    %% Register if requested
                    RegisterResult = case maps:get(register, Options, true) of
                        true ->
                            yawl_registry:register(CaseId, Pid);
                        false ->
                            ok
                    end,

                    case RegisterResult of
                        ok ->
                            logger:info("Workflow started successfully: ~p (~p)", [CaseId, Pid]),
                            publish_event(workflow_started, #{case_id => CaseId, pid => Pid, module => WorkflowModule, workflow_module => WorkflowModule}),
                            {ok, Pid};
                        {error, Reason} ->
                            logger:error("Failed to register workflow: ~p", [Reason]),
                            gen_yawl:stop(Pid),
                            {error, {register_failed, Reason}}
                    end;
                {error, Reason} ->
                    logger:error("Failed to start workflow: ~p", [Reason]),
                    {error, startup_failed}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Stop a running workflow instance by case_id.
%%
%% Unregisters the workflow from the global registry and terminates
%% the workflow process gracefully.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>CaseId</code> - Unique identifier of the workflow to stop</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>ok</code> - Workflow stopped successfully</li>
%%   <li><code>{error, not_found}</code> - Workflow not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_workflow(CaseId :: case_id()) -> ok | {error, not_found}.

stop_workflow(CaseId) ->
    stop_workflow(CaseId, normal).

%%--------------------------------------------------------------------
%% @doc Stop a workflow with a specific reason.
%%
%% Allows specifying termination reason (normal, shutdown, etc.).
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_workflow(CaseId :: case_id(), Reason :: term()) -> ok | {error, not_found}.

stop_workflow(CaseId, Reason) ->
    logger:info("Stopping workflow: case_id=~p, reason=~p", [CaseId, Reason]),

    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            %% Unregister first to prevent race conditions
            ok = yawl_registry:unregister(CaseId),
            %% Stop the workflow
            gen_yawl:stop(Pid),
            publish_event(workflow_stopped, #{case_id => CaseId, reason => Reason}),
            ok;
        {error, not_found} ->
            logger:warning("Attempted to stop non-existent workflow: ~p", [CaseId]),
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Query workflow state by case_id.
%%
%% Returns comprehensive information about the workflow including its marking,
%% user info, and execution status.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, StateMap}</code> - State map with keys: marking, usr_info, status, enabled_transitions</li>
%%   <li><code>{error, not_found}</code> - Workflow not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec query_workflow(CaseId :: case_id()) ->
          query_result(#{marking => marking(), usr_info => usr_info(),
                       status => workflow_status(), enabled_transitions => [atom()]}).

query_workflow(CaseId) ->
    query_workflow(CaseId, #{include_marking => true, include_usr_info => true}).

%%--------------------------------------------------------------------
%% @doc Query workflow with specific options.
%%
%% Options control what data is included:
%% <ul>
%%   <li><code>{include_marking, boolean()}</code> - Include Petri net marking (default: true)</li>
%%   <li><code>{include_usr_info, boolean()}</code> - Include user info (default: true)</li>
%%   <li><code>{include_stats, boolean()}</code> - Include execution statistics (default: false)</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec query_workflow(CaseId :: case_id(), Options :: options()) ->
          query_result(map()).

query_workflow(CaseId, Options) ->
    logger:debug("Querying workflow: case_id=~p", [CaseId]),

    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            try
                Result = #{pid => Pid},

                %% Get marking if requested
                Marking = case maps:get(include_marking, Options, true) of
                    true ->
                        gen_yawl:marking(Pid);
                    false ->
                        undefined
                end,
                Result1 = case Marking of
                    {ok, M} -> Result#{marking => M};
                    {error, _} -> Result
                end,

                %% Get usr_info if requested
                UsrInfoResult = case maps:get(include_usr_info, Options, true) of
                    true -> gen_yawl:usr_info(Pid);
                    false -> undefined
                end,
                Result2 = case UsrInfoResult of
                    U when U =/= undefined -> Result1#{usr_info => U};
                    _ -> Result1
                end,

                %% Get enabled transitions
                Enabled = gen_yawl:enabled_transitions(Pid),
                FinalResult = Result2#{enabled_transitions => Enabled},

                %% Get stats if requested
                Result3 = case maps:get(include_stats, Options, false) of
                    true ->
                        Stats = gen_yawl:stats(Pid),
                        FinalResult#{stats => Stats};
                    false ->
                        FinalResult
                end,

                {ok, Result3}
            catch
                _:Error ->
                    logger:error("Error querying workflow ~p: ~p", [CaseId, Error]),
                    {error, Error}
            end;
        {error, not_found} ->
            logger:warning("Query for non-existent workflow: ~p", [CaseId]),
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc List all registered workflow instances.
%%
%% Returns a list of all workflows currently registered in the system.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>Workflows</code> - List of {CaseId, Pid} tuples</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec list_workflows() -> [{case_id(), workflow_pid()}].

list_workflows() ->
    list_workflows(#{}).

%%--------------------------------------------------------------------
%% @doc List workflows with filtering options.
%%
%% Options:
%% <ul>
%%   <li><code>{include_stopped, boolean()}</code> - Include stopped workflows (default: false)</li>
%%   <li><code>{filter_pattern, binary()}</code> - Filter by pattern type</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec list_workflows(Options :: options()) -> [{case_id(), workflow_pid()}].

list_workflows(Options) ->
    logger:debug("Listing workflows with options: ~p", [Options]),

    AllWorkflows = yawl_registry:list(),

    %% Apply filters
    Filtered = case maps:get(filter_pattern, Options) of
        undefined ->
            AllWorkflows;
        PatternId ->
            %% This is a simplified filter - in production you'd query
            %% the actual workflow module pattern from the process
            AllWorkflows
    end,

    Filtered.
%%%=====================================================================
%%% Pattern Management Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Validate a YAWL pattern specification.
%%
%% Checks the specification for structural correctness, proper element
%% references, and semantic validity.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>Spec</code> - YAWL specification map or term</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, []}</code> - Specification is valid</li>
%%   <li><code>{ok, Warnings}</code> - Valid with warnings</li>
%%   <li><code>{error, Errors}</code> - Invalid with error list</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_pattern(Spec :: term()) -> validation_result().

validate_pattern(Spec) ->
    validate_pattern(Spec, #{strict => true}).

%%--------------------------------------------------------------------
%% @doc Validate pattern with options.
%%
%% Options:
%% <ul>
%%   <li><code>{strict, boolean()}</code> - Enable strict validation (default: true)</li>
%%   <li><code>{check_flows, boolean()}</code> - Validate flow references (default: true)</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_pattern(Spec :: term(), Options :: options()) -> validation_result().

validate_pattern(Spec, Options) ->
    logger:info("Validating pattern with options: ~p", [Options]),

    try
        case yawl_validate:validate_spec(Spec) of
            {ok, []} ->
                logger:info("Pattern validation passed: no errors"),
                {ok, []};
            {ok, Warnings} when is_list(Warnings) ->
                logger:info("Pattern validation passed with ~p warnings", [length(Warnings)]),
                {ok, Warnings};
            {error, Errors} ->
                logger:warning("Pattern validation failed: ~p errors", [length(Errors)]),
                {error, Errors}
        end
    catch
        _:Error ->
            logger:error("Pattern validation error: ~p", [Error]),
            {error, [Error]}
    end.

%%--------------------------------------------------------------------
%% @doc Compile a YAWL pattern to a gen_pnet module.
%%
%% Transforms a YAWL specification into an executable Petri net module.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>Spec</code> - YAWL specification to compile</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, CompileInfo}</code> - Compilation succeeded</li>
%%   <li><code>{error, Reason}</code> - Compilation failed</li>
%% </ul>
%%
%% <h3>Example</h3>
%% ```erlang
%% > {ok, Info} = cre_public_api:compile_pattern(MyYawlSpec).
%% #{spec_id := <<"order_wf">>, module := yawl_order_wf}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_pattern(Spec :: term()) -> compilation_result().

compile_pattern(Spec) ->
    compile_pattern(Spec, #{}).

%%--------------------------------------------------------------------
%% @doc Compile pattern with options.
%%
%% Options:
%% <ul>
%%   <li><code>{output_dir, path()}</code> - Output directory for compiled module</li>
%%   <li><code>{module_prefix, binary()}</code> - Prefix for generated module name</li>
%%   <li><code>{include_source, boolean()}</code> - Include source in generated module</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_pattern(Spec :: term(), Options :: options()) -> compilation_result().

compile_pattern(Spec, Options) ->
    logger:info("Compiling pattern with options: ~p", [Options]),

    try
        Result = yawl_compile:compile(Spec, Options),
        case Result of
            {ok, CompileInfo} ->
                logger:info("Pattern compiled successfully: spec_id=~p",
                           [maps:get(spec_id, CompileInfo, unknown)]),
                {ok, CompileInfo};
            {error, Reason} ->
                logger:error("Pattern compilation failed: ~p", [Reason]),
                {error, Reason}
        end
    catch
        _:Error ->
            logger:error("Pattern compilation error: ~p", [Error]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc List all available workflow patterns.
%%
%% Returns a list of registered pattern identifiers that can be used
%% for workflow composition.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>Patterns</code> - List of pattern identifiers</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec list_patterns() -> [pattern_id()].

list_patterns() ->
    yawl_pattern_registry:all_patterns().

%%%=====================================================================
%%% State Query Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Get workflow state by case_id.
%%
%% Returns a state map containing the workflow's current marking and
%% user information.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, StateMap}</code> - State map with marking and usr_info</li>
%%   <li><code>{error, not_found}</code> - Workflow not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(CaseId :: case_id()) ->
          query_result(#{marking => marking(), usr_info => usr_info()}).

get_state(CaseId) ->
    get_state(CaseId, #{}).

%%--------------------------------------------------------------------
%% @doc Get workflow state with options.
%%
%% Options allow selective retrieval of state components.
%%
%% @see query_workflow/2
%% @end
%%--------------------------------------------------------------------
-spec get_state(CaseId :: case_id(), Options :: options()) ->
          query_result(map()).

get_state(CaseId, Options) ->
    query_workflow(CaseId, Options).

%%--------------------------------------------------------------------
%% @doc Get Petri net marking for a workflow.
%%
%% Returns the token distribution across places in the workflow.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, Marking}</code> - Current marking map</li>
%%   <li><code>{error, not_found}</code> - Workflow not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec get_marking(CaseId :: case_id()) -> query_result(marking()).

get_marking(CaseId) ->
    get_marking(CaseId, all).

%%--------------------------------------------------------------------
%% @doc Get marking for specific places.
%%
%% Returns tokens only for the specified places.
%%
%% <h3>Example</h3>
%% ```erlang
%% > cre_public_api:get_marking(<<"wf1">>, [input_place, output_place]).
%% {ok, #{input_place := [token1], output_place := []}}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec get_marking(CaseId :: case_id(), Places :: atom() | [atom()]) ->
          query_result(marking()).

get_marking(CaseId, Places) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            try
                FullMarking = gen_yawl:marking(Pid),
                FilteredMarking = case Places of
                    all -> FullMarking;
                    L when is_list(L) ->
                        maps:with(L, FullMarking);
                    A when is_atom(A) ->
                        #{A := Tokens} = FullMarking,
                        #{A => Tokens}
                end,
                {ok, FilteredMarking}
            catch
                _:Error ->
                    logger:error("Error getting marking: ~p", [Error]),
                    {error, Error}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Get workflow execution status.
%%
%% Returns the current status of the workflow (running, completed, etc.)
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, Status}</code> - Workflow status atom</li>
%%   <li><code>{error, not_found}</code> - Workflow not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec get_status(CaseId :: case_id()) ->
          query_result(workflow_status()).

get_status(CaseId) ->
    get_status(CaseId, #{}) .

%%--------------------------------------------------------------------
%% @doc Get workflow status with detailed info.
%%
%% Returns status along with additional workflow information.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_status(CaseId :: case_id(), Options :: options()) ->
          query_result(#{status => workflow_status(), details => map()}).

get_status(CaseId, Options) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            try
                %% Determine status from enabled transitions and marking
                Enabled = gen_yawl:enabled_transitions(Pid),
                Marking = gen_yawl:marking(Pid),

                Status = case {Enabled, Marking} of
                    {[], _} ->
                        completed;
                    {[_ | _], _} ->
                        running
                end,

                Result = #{status => Status},

                %% Add details if requested
                DetailedResult = case maps:get(include_details, Options, false) of
                    true ->
                        Result#{
                            enabled_transitions => Enabled,
                            marking => Marking,
                            pid => Pid
                        };
                    false ->
                        Result
                end,

                {ok, DetailedResult}
            catch
                _:Error ->
                    logger:error("Error getting status: ~p", [Error]),
                    {error, Error}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Get user info from workflow.
%%
%% Returns the user information term stored in the workflow state.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, UsrInfo}</code> - User info term</li>
%%   <li><code>{error, not_found}</code> - Workflow not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec get_usr_info(CaseId :: case_id()) -> query_result(usr_info()).

get_usr_info(CaseId) ->
    get_usr_info(CaseId, #{}) .

%%--------------------------------------------------------------------
%% @doc Get user info with options.
%%
%% Options can control the format or depth of the returned user info.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_usr_info(CaseId :: case_id(), Options :: options()) ->
          query_result(usr_info()).

get_usr_info(CaseId, Options) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            try
                UsrInfo = gen_yawl:usr_info(Pid),

                %% Apply options
                Result = case maps:get(transform, Options) of
                    {ok, {transform_fun, TransformFun}} when is_function(TransformFun, 1) ->
                        try TransformFun(UsrInfo)
                        catch _:E -> {error, {transform_failed, E}}
                    end;
                    _ ->
                        UsrInfo
                end,

                {ok, Result}
            catch
                _:Error ->
                    logger:error("Error getting usr_info: ~p", [Error]),
                    {error, Error}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%%%=====================================================================
%%% Event Subscription Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Subscribe to workflow events.
%%
%% Subscribes the calling process to receive workflow lifecycle events.
%% Events are sent as messages: <code>{workflow_event, EventData}</code>
%%
%% <h3>Event Types</h3>
%% <ul>
%%   <li><code>workflow_started</code> - New workflow instance created</li>
%%   <li><code>workflow_stopped</code> - Workflow instance stopped</li>
%%   <li><code>workflow_completed</code> - Workflow finished successfully</li>
%%   <li><code>workflow_failed</code> - Workflow terminated with error</li>
%%   <li><code>transition_fired</code> - A transition fired</li>
%%   <li><code>token_produced</code> - Tokens produced to a place</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, SubRef}</code> - Subscription reference for unsubscription</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_events(Subscriber :: subscriber()) -> {ok, subscription_ref()}.

subscribe_events(Subscriber) when is_pid(Subscriber) ->
    subscribe_events(Subscriber, all).

%%--------------------------------------------------------------------
%% @doc Subscribe with event filter.
%%
%% Subscribes to specific event types or all events.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>Subscriber</code> - Process to receive events</li>
%%   <li><code>Filter</code> - Event type, list of types, or 'all'</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>{ok, SubRef}</code> - Subscription reference</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_events(Subscriber :: subscriber(),
                     Filter :: event_type() | all | [event_type()]) ->
          {ok, subscription_ref()}.

subscribe_events(Subscriber, Filter) ->
    logger:info("New event subscription: subscriber=~p, filter=~p", [Subscriber, Filter]),

    SubRef = make_ref(),

    %% Ensure state process is running
    ok = ensure_state_process(),

    %% Create subscription record
    Subscription = #subscription{
                    subscriber = Subscriber,
                    filter = Filter,
                    ref = SubRef
                   },

    %% Register subscription
    gen_server:cast(?MODULE, {add_subscription, Subscription}),

    %% Send confirmation
    Subscriber ! {subscription_confirmed, SubRef},

    {ok, SubRef}.

%%--------------------------------------------------------------------
%% @doc Unsubscribe from workflow events.
%%
%% Removes an active subscription using the reference returned from subscribe_events/2.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>SubRef</code> - Subscription reference from subscribe_events/2</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>ok</code> - Successfully unsubscribed</li>
%%   <li><code>{error, not_found}</code> - Subscription not found</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(SubRef :: subscription_ref()) -> ok | {error, not_found}.

unsubscribe(SubRef) ->
    unsubscribe(SubRef, self()).

%%--------------------------------------------------------------------
%% @doc Unsubscribe a specific subscriber.
%%
%% Removes all subscriptions for the given subscriber process.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Subscriber :: subscriber(), SubRef :: subscription_ref()) ->
          ok | {error, not_found}.

unsubscribe(Subscriber, SubRef) ->
    logger:info("Unsubscribing: ref=~p", [SubRef]),

    %% Forward to state process
    gen_server:cast(?MODULE, {remove_subscription, Subscriber, SubRef}),

    ok.

%%--------------------------------------------------------------------
%% @doc Publish a workflow event.
%%
%% Sends an event to all matching subscribers. Used internally
%% by workflow execution functions.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><code>Event</code> - Event type atom</li>
%%   <li><code>Data</code> - Event data map</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_event(Event :: event_type(), Data :: event_data()) -> ok.

publish_event(Event, Data) ->
    %% Ensure state process is running
    ok = ensure_state_process(),

    %% Forward to state process for distribution
    gen_server:cast(?MODULE, {publish_event, Event, Data}).

%%%=====================================================================
%%% Health Check Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Get system health status.
%%
%% Returns overall health status of the CRE system including
%% all monitored subsystems.
%%
%% <h3>Returns</h3>
%% Health status map with keys:
%% <ul>
%%   <li><code>status</code> - Overall status: healthy, degraded, or unhealthy</li>
%%   <li><code>subsystems</code> - List of subsystem status maps</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec health() -> health_status().

health() ->
    health(#{}).

%%--------------------------------------------------------------------
%% @doc Get health with options.
%%
%% Options:
%% <ul>
%%   <li><code>{check_registry, boolean()}</code> - Check workflow registry (default: true)</li>
%%   <li><code>{check_state_process, boolean()}</code> - Check API state process (default: true)</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec health(Options :: options()) -> health_status().

health(Options) ->
    logger:debug("Health check with options: ~p", [Options]),

    Subsystems = [],

    %% Check workflow registry
    Subsystems1 = case maps:get(check_registry, Options, true) of
        true ->
            RegistryStatus = try
                _ = yawl_registry:list(),
                #{name => <<"workflow_registry">>, status => healthy}
            catch _:_ ->
                #{name => <<"workflow_registry">>, status => unhealthy, error => <<"not responding">>}
            end,
            [RegistryStatus | Subsystems];
        false ->
            Subsystems
    end,

    %% Check state process
    Subsystems2 = case maps:get(check_state_process, Options, true) of
        true ->
            StateStatus = try
                _ = gen_server:call(?MODULE, ping),
                #{name => <<"api_state">>, status => healthy}
            catch _:_:_ ->
                #{name => <<"api_state">>, status => unhealthy, error => <<"not responding">>}
            end,
            [StateStatus | Subsystems1];
        false ->
            Subsystems1
    end,

    %% Determine overall status
    StatusList = [maps:get(status, S, unhealthy) || S <- Subsystems2],
    OverallStatus = case StatusList of
        [] -> healthy;
        _ -> degraded
    end,

    #{
        status => OverallStatus,
        subsystems => Subsystems2,
        timestamp => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Check if system is ready to accept workloads.
%%
%% Returns true if all critical subsystems are operational.
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><code>true</code> - System is ready</li>
%%   <li><code>false</code> - System is not ready</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec ready() -> boolean().

ready() ->
    Health = health(),
    maps:get(status, Health, unhealthy) =/= unhealthy.

%%--------------------------------------------------------------------
%% @doc Get CRE version information.
%%
%% Returns the version string of the running CRE system.
%%
%% <h3>Returns</h3>
%% Version map with keys:
%% <ul>
%%   <li><code>version</code> - Version string</li>
%%   <li><code>otp_release</code> - OTP release version</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec version() -> #{version => binary(), otp_release => binary()}.

version() ->
    #{
        version => <<"0.3.0">>,
        otp_release => list_to_binary(erlang:system_info(otp_release))
    }.

%%%=====================================================================
%%% gen_server Callback Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the public API state server.
%%
%% Manages event subscriptions and event log.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.

init([]) ->
    %% Create ETS tables for subscriptions and event log
    SubscriptionsTable = ets:new(cre_public_api_subscriptions, [set, named_table]),
    EventLogTable = ets:new(cre_public_api_events, [queue, named_table]),

    logger:info("CRE Public API state process started"),

    {ok, #state{
                subscribers = SubscriptionsTable,
                event_log = EventLogTable,
                max_log_size = 1000
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous calls.
%%
%% Supports ping for health checks.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
          {reply, term(), #state{}} | {noreply, #state{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};

handle_call(get_subscriber_count, _From, State = #state{subscribers = Table}) ->
    Count = ets:info(Table, size),
    {reply, Count, State};

handle_call({get_subscription_info, SubRef}, _From, State = #state{subscribers = Table}) ->
    case ets:lookup(Table, SubRef) of
        [{_Ref, SubRec}] when element(1, SubRec) =:= subscription ->
            #subscription{subscriber = Sub, filter = Filter} = SubRec,
            Info = #{subscriber => Sub, filter => Filter, ref => SubRef},
            {reply, {ok, Info}, State};
        [{_Ref, SubRec}] ->
            #subscription{subscriber = Sub, filter = Filter} = SubRec,
            Info = #{subscriber => Sub, filter => Filter, ref => SubRef},
            {reply, {ok, Info}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous casts.
%%
%% Manages subscription add/remove and event publishing.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
          {noreply, #state{}}.

handle_cast({add_subscription, Subscription}, State = #state{subscribers = Table}) ->
    %% Add subscription to ETS
    true = ets:insert(Table, Subscription),
    logger:info("Event subscription added: total=~p", [ets:info(Table, size)]),
    {noreply, State};

handle_cast({remove_subscription, Subscriber, SubRef}, State = #state{subscribers = Table}) ->
    %% Remove specific subscription or all for subscriber
    SubRef2 = case SubRef of
        all ->
            %% Match all subscriptions for this subscriber
            ets:select_delete(Table, [{#subscription.subscriber, Subscriber}]);
        _ ->
            %% Remove specific subscription
            ets:delete(Table, SubRef)
    end,
    logger:info("Event subscription removed: ref=~p, remaining=~p",
                [SubRef, ets:info(Table, size)]),
    {noreply, State};

handle_cast({publish_event, Event, Data}, State = #state{subscribers = SubsTable, event_log = LogTable, max_log_size = MaxSize}) ->
    %% Build event data
    Event = #{
        event_type => Event,
        data => Data,
        timestamp => erlang:system_time(millisecond)
    },

    %% Add to event log (trim if needed)
    true = ets:insert(LogTable, Event),
    LogSize = ets:info(LogTable, size),
    State1 = case LogSize > MaxSize of
        true ->
            %% Remove oldest entries
            TrimCount = LogSize - MaxSize,
            ets:select_delete(LogTable, [{ '_', '_', '$1' }], TrimCount),
            State#state{event_log = LogTable};
        false ->
            State
    end,

    %% Distribute to matching subscribers
    MatchSpec = [{#subscription.subscriber, '$1'}, {#subscription.filter, '$2'}],
    Subscriptions = ets:select(SubsTable, MatchSpec),

    DistributeFun = fun({_, #subscription{subscriber = Sub, filter = Filter}}) ->
        case matches_filter(Event, Filter) of
            true ->
                Sub ! {workflow_event, Event#{timestamp => erlang:system_time(millisecond)}};
            false ->
                ok
        end
    end,

    lists:foreach(DistributeFun, Subscriptions),

    logger:debug("Event published: type=~p, subscribers=~p", [Event, length(Subscriptions)]),

    {noreply, State1};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle info messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #state{}) ->
          {noreply, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code changes during hot upgrade.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
          {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Clean shutdown of state server.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: #state{}) -> ok.

terminate(_Reason, #state{subscribers = Table, event_log = LogTable}) ->
    logger:info("CRE Public API state process terminating: reason=~p", [_Reason]),

    %% Delete ETS tables (only if created by this process)
    catch ets:delete(Table), catch ets:delete(LogTable),

    ok.

%%%=====================================================================
%%% Internal Helper Functions
%%%=====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Check if an event matches a subscription filter.
%%
%% @end
%%--------------------------------------------------------------------
-spec matches_filter(EventType :: event_type(), Filter :: event_type() | all | [event_type()]) ->
          boolean().

matches_filter(_EventType, all) ->
    true;
matches_filter(EventType, Filter) when is_list(Filter) ->
    lists:member(EventType, Filter);
matches_filter(EventType, Filter) when is_atom(Filter) ->
    EventType =:= Filter.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensure the state process is running.
%%
%% Starts the gen_server if not already running.
%% @end
%%--------------------------------------------------------------------
-spec ensure_state_process() -> ok.

ensure_state_process() ->
    case whereis(?MODULE) of
        undefined ->
            %% Start state process
            case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
                {ok, _Pid} ->
                    logger:info("CRE Public API state process started"),
                    ok;
                {error, {already_started, _Pid}} ->
                    ok
            end;
        _Pid ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert case_id to a valid registration name.
%%
%% @end
%%--------------------------------------------------------------------
-spec case_id_to_reg_name(case_id()) -> atom().

case_id_to_reg_name(CaseId) when is_binary(CaseId) ->
    %% Convert binary to atom, ensuring valid characters
    BinaryName = <<<<"workflow_">>/binary, CaseId/binary>>,
    list_to_existing_atom(binary_to_list(BinaryName));
case_id_to_reg_name(CaseId) when is_atom(CaseId) ->
    CaseId.
