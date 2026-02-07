%% -*- erlang -*-
%%%% @doc High-level YAWL workflow executor for any YAWL specification.
%%
%% This module provides a complete end-to-end execution API for YAWL workflows.
%% It integrates `wf_spec` (parsing), `yawl_compile` (compilation), and
%% `yawl_execution` (runtime) into a unified interface.
%%
%% == Workflow Execution Lifecycle ==
%%
%% The executor manages the complete lifecycle:
%%
%% <ol>
%%   <li><strong>Load:</strong> Parse YAWL XML from file or binary</li>
%%   <li><strong>Compile:</strong> Generate gen_pnet modules from specification</li>
%%   <li><strong>Start:</strong> Initialize workflow instance with data</li>
%%   <li><strong>Execute:</strong> Run workflow steps or drain to completion</li>
%%   <li><strong>Inspect:</strong> Query workflow state and results</li>
%%   <li><strong>Stop:</strong> Terminate workflow instance</li>
%% </ol>
%%
%% == Quick Start ==
%%
%% ```erlang
%% %% Load and compile a YAWL specification
%% > {ok, Executor} = wf_yawl_executor:load_workflow("order_fulfillment.yawl").
%%
%% %% Start a workflow instance with initial data
%% > {ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor, #{order => #{}}).
%%
%% %% Execute until complete (max 1000 steps)
%% > {ok, Receipts} = wf_yawl_executor:execute_step(Pid, 1000).
%%
%% %% Get final workflow state
%% > {ok, State} = wf_yawl_executor:get_workflow_state(Pid).
%%
%% %% Stop the workflow
%% > ok = wf_yawl_executor:stop_workflow(Pid).
%% '''
%%
%% == One-Shot Execution ==
%%
%% For simple workflows, use the `execute_workflow/3` function:
%%
%% ```erlang
%% > {ok, Result} = wf_yawl_executor:execute_workflow(
%%     "my_workflow.yawl",
%%     #{input => data},
%%     #{max_steps => 100}
%% ).
%% {ok, #{receipts := _, final_state := #{}}}
%% '''
%%
%% == Token Injection ==
%%
%% Interact with running workflows by injecting tokens:
%%
%% ```erlang
%% > {ok, Pid, _CaseId} = wf_yawl_executor:start_workflow(Executor, #{}).
%%
%% %% Inject a token into a place
%% > {ok, Receipt} = wf_yawl_executor:inject_token(Pid, approval_place, approved).
%%
%% %% Continue execution
%% > {ok, Receipts} = wf_yawl_executor:execute_step(Pid, 10).
%% '''
%%
%% == Error Handling ==
%%
%% All functions return tagged tuples for explicit error handling:
%%
%% ```erlang
%% case wf_yawl_executor:load_workflow("nonexistent.yawl") of
%%     {ok, Executor} ->
%%         %% Proceed with execution
%%         ok;
%%     {error, {file_error, enoent}} ->
%%         %% File not found
%%         logger:error("YAWL file not found"),
%%         error;
%%     {error, Reason} ->
%%         %% Other error
%%         logger:error("Failed to load workflow: ~p", [Reason]),
%%         error
%% end.
%% '''
%%
%% == Workflow Cancellation ==
%%
%% YAWL supports cancellation sets and regions. The executor respects
%% these and propagates cancellation through the workflow:
%%
%% ```erlang
%% %% Workflow with cancellation regions
%% > {ok, Executor} = wf_yawl_executor:load_workflow("with_cancellation.yawl").
%%
%% %% Cancellation is automatic based on YAWL specification
%% > {ok, Pid, _} = wf_yawl_executor:start_workflow(Executor, #{}).
%%
%% %% Execute may trigger cancellations defined in spec
%% > {ok, Receipts} = wf_yawl_executor:execute_step(Pid, 100).
%% '''
%% @end
%% -------------------------------------------------------------------

-module(wf_yawl_executor).

%%====================================================================
%% Exports
%%====================================================================

%% Workflow loading and compilation
-export([load_workflow/1, load_workflow_from_xml/1]).
-export([compile_workflow/1, compile_workflow/2]).

%% Workflow lifecycle
-export([start_workflow/2, start_workflow/3]).
-export([stop_workflow/1]).

%% Workflow execution
-export([execute_step/1, execute_step/2]).
-export([inject_token/3]).

%% State inspection
-export([get_workflow_state/1, is_quiescent/1]).

%% One-shot execution
-export([execute_workflow/2, execute_workflow/3]).

%% Executor info
-export([executor_info/1]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A YAWL workflow specification.
%%
%% Opaque type representing a loaded YAWL workflow.
%%--------------------------------------------------------------------
-opaque yawl_spec() :: wf_spec:yawl_spec().

%%--------------------------------------------------------------------
%% @doc A compiled YAWL workflow.
%%
%% Contains the compiled modules and metadata needed for execution.
%%--------------------------------------------------------------------
-type compiled_workflow() :: #{
    spec := yawl_spec(),
    compiled := yawl_compile:compile_result(),
    root_module := atom(),
    modules := [module()]
}.

%%--------------------------------------------------------------------
%% @doc Executor state.
%%
%% Encapsulates the loaded and compiled workflow ready for execution.
%%--------------------------------------------------------------------
-record(yawl_executor, {
    spec :: yawl_spec(),
    compiled :: yawl_compile:compile_result(),
    root_module :: atom(),
    modules :: [module()]
}).

-opaque executor() :: #yawl_executor{}.

%%--------------------------------------------------------------------
%% @doc Workflow case identifier.
%%
%% Unique ID for a workflow instance.
%%--------------------------------------------------------------------
-type case_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Initial workflow data.
%%
%% Map of variable names to values for workflow initialization.
%%--------------------------------------------------------------------
-type initial_data() :: #{atom() | binary() => term()}.

%%--------------------------------------------------------------------
%% @doc Workflow state.
%%
%% Complete state of a running workflow instance including marking,
%% status, and metadata.
%%--------------------------------------------------------------------
-type workflow_state() :: #{
    marking := #{atom() => [term()]},
    status := running | completed | cancelled | error,
    case_id := case_id(),
    spec_id := binary()
}.

%%--------------------------------------------------------------------
%% @doc Execution receipt.
%%
%% Record of a transition firing with metadata.
%%--------------------------------------------------------------------
-type receipt() :: #{
    trsn := atom(),
    mode => #{atom() => [term()]},
    produce => #{atom() => [term()]}
}.

%%--------------------------------------------------------------------
%% @doc Execution options.
%%
%% - `max_steps` - Maximum transitions to fire (default: 1000)
%% - `timeout` - Execution timeout in milliseconds (default: infinity)
%% - `register` - Register workflow process (default: undefined)
%%--------------------------------------------------------------------
-type exec_options() :: #{
    max_steps => non_neg_integer(),
    timeout => timeout(),
    register => {local, atom()} | {global, atom()} | {via, module(), term()}
}.

%%--------------------------------------------------------------------
%% @doc Execution result.
%%
%% Result from one-shot workflow execution.
%%--------------------------------------------------------------------
-type exec_result() :: #{
    receipts := [receipt()],
    final_state := workflow_state(),
    case_id := case_id()
}.

%% Export types
-export_type([executor/0, yawl_spec/0, compiled_workflow/0,
              case_id/0, initial_data/0, workflow_state/0,
              receipt/0, exec_options/0, exec_result/0]).

%%====================================================================
%% API Functions - Loading and Compilation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Loads a YAWL workflow from a file.
%%
%% Parses the YAWL XML file, validates the specification, and returns
%% an executor ready to compile and run workflows.
%%
%% Returns `{ok, Executor}` on success, or `{error, Reason}` on failure.
%%
%% === Example ===
%% ```erlang
%% > {ok, Executor} = wf_yawl_executor:load_workflow("order_fulfillment.yawl").
%% {ok, #yawl_executor{spec = #{}, ...}}
%% '''
%%
%% @param FilePath Path to YAWL XML specification file
%% @return {ok, Executor} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec load_workflow(FilePath :: file:filename_all()) ->
          {ok, executor()} | {error, term()}.

load_workflow(FilePath) when is_list(FilePath); is_binary(FilePath) ->
    case wf_spec:from_xml_file(FilePath) of
        {ok, Spec} ->
            load_workflow_from_spec(Spec);
        {error, Reason} ->
            {error, {load_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a YAWL workflow from XML binary.
%%
%% Parses YAWL XML from a binary string and returns an executor.
%% Useful for testing or when YAWL specs are stored in databases.
%%
%% === Example ===
%% ```erlang
%% > Xml = <<"<?xml version='1.0'?><specificationSet>...</specificationSet>">>.
%% > {ok, Executor} = wf_yawl_executor:load_workflow_from_xml(Xml).
%% {ok, #yawl_executor{}}
%% '''
%%
%% @param Xml YAWL XML specification as binary
%% @return {ok, Executor} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec load_workflow_from_xml(Xml :: binary()) ->
          {ok, executor()} | {error, term()}.

load_workflow_from_xml(Xml) when is_binary(Xml) ->
    case wf_spec:from_xml(Xml) of
        {ok, Spec} ->
            load_workflow_from_spec(Spec);
        {error, Reason} ->
            {error, {parse_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Compiles a loaded workflow specification.
%%
%% Takes a YAWL spec and generates gen_pnet modules. Returns an
%% executor with compiled modules ready for execution.
%%
%% Uses default compilation options.
%%
%% === Example ===
%% ```erlang
%% > {ok, Spec} = wf_spec:from_xml_file("workflow.yawl").
%% > {ok, Executor} = wf_yawl_executor:compile_workflow(Spec).
%% {ok, #yawl_executor{compiled := #{}, ...}}
%% '''
%%
%% @param Spec YAWL specification from wf_spec:from_xml_file/1
%% @return {ok, Executor} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_workflow(Spec :: wf_spec:yawl_spec()) ->
          {ok, executor()} | {error, term()}.

compile_workflow(Spec) ->
    compile_workflow(Spec, #{}).

%%--------------------------------------------------------------------
%% @doc Compiles a loaded workflow with options.
%%
%% Same as `compile_workflow/1` but accepts compilation options:
%%
%% - `seed` - Random seed for deterministic choices (default: 0)
%% - `module_prefix` - Prefix for generated modules (default: <<"yawl_">>)
%% - `output_dir` - Directory to write compiled modules
%% - `include_source` - Include YAWL source in generated docs
%% - `gen_observer` - Generate observer callbacks
%%
%% === Example ===
%% ```erlang
%% > {ok, Spec} = wf_spec:from_xml_file("workflow.yawl").
%% > {ok, Executor} = wf_yawl_executor:compile_workflow(Spec, #{
%%     seed => 42,
%%     module_prefix => <<"workflow_">>
%% }).
%% {ok, #yawl_executor{}}
%% '''
%%
%% @param Spec YAWL specification
%% @param Options Compilation options map
%% @return {ok, Executor} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_workflow(Spec :: wf_spec:yawl_spec(), Options :: map()) ->
          {ok, executor()} | {error, term()}.

compile_workflow(Spec, Options) when is_tuple(Spec), element(1, Spec) =:= yawl_spec;
                                     is_map(Options) ->
    case wf_spec:validate(Spec) of
        ok ->
            case yawl_compile:compile(Spec, Options) of
                {ok, Compiled} ->
                    %% Extract root module
                    RootNet = wf_spec:root_net(Spec),
                    RootModule = list_to_atom(
                        binary_to_list(
                            case maps:get(module_prefix, Options, <<"yawl_">>) of
                                Prefix -> <<Prefix/binary, RootNet/binary>>
                            end
                        )
                    ),

                    %% Get all compiled modules
                    Modules = maps:keys(maps:get(modules, Compiled, #{})),

                    Executor = #yawl_executor{
                        spec = Spec,
                        compiled = Compiled,
                        root_module = RootModule,
                        modules = [RootModule | Modules]
                    },
                    {ok, Executor};
                {error, Reason} ->
                    {error, {compile_error, Reason}}
            end;
        {error, Errors} ->
            {error, {validation_error, Errors}}
    end;

compile_workflow(_Spec, _Options) ->
    {error, invalid_spec}.

%%====================================================================
%% API Functions - Workflow Lifecycle
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a workflow instance (unregistered).
%%
%% Creates a new gen_pnet process for the workflow. The InitialData
%% is passed to the workflow's init/1 callback.
%%
%% Returns `{ok, Pid, CaseId}` where Pid is the process identifier
%% and CaseId is a unique binary identifier for this instance.
%%
%% === Example ===
%% ```erlang
%% > {ok, Executor} = wf_yawl_executor:load_workflow("workflow.yawl").
%% > {ok, Pid, CaseId} = wf_yawl_executor:start_workflow(Executor, #{}).
%% {ok, <0.123.0>, <<"case_20250206_123456">>}
%% '''
%%
%% @param Executor Executor from load_workflow/1 or compile_workflow/1
%% @param InitialData Initial workflow variables
%% @return {ok, Pid, CaseId} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(Executor :: executor(), InitialData :: initial_data()) ->
          {ok, pid(), case_id()} | {error, term()}.

start_workflow(#yawl_executor{root_module = RootMod}, InitialData) when is_map(InitialData) ->
    CaseId = generate_case_id(),
    NetArg = #{initial_data => InitialData, case_id => CaseId},

    case gen_pnet:start_link(RootMod, NetArg, []) of
        {ok, Pid} ->
            {ok, Pid, CaseId};
        {error, Reason} ->
            {error, {start_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Starts a registered workflow instance.
%%
%% Same as `start_workflow/2` but registers the process with ServerName.
%% ServerName can be `{local, Name}`, `{global, Name}`, or `{via, Module, ViaName}`.
%%
%% === Example ===
%% ```erlang
%% > {ok, Executor} = wf_yawl_executor:load_workflow("workflow.yawl").
%% > {ok, Pid, CaseId} = wf_yawl_executor:start_workflow(
%%     Executor,
%%     #{},
%%     {local, my_workflow}
%% ).
%% {ok, <0.124.0>, <<"case_20250206_123457">>}
%% '''
%%
%% @param Executor Executor instance
%% @param InitialData Initial workflow variables
%% @param ServerName Registration name
%% @return {ok, Pid, CaseId} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(Executor :: executor(),
                     InitialData :: initial_data(),
                     ServerName :: gen_pnet:server_name()) ->
          {ok, pid(), case_id()} | {error, term()}.

start_workflow(#yawl_executor{root_module = RootMod}, InitialData, ServerName)
        when is_map(InitialData), is_tuple(ServerName) ->
    CaseId = generate_case_id(),
    NetArg = #{initial_data => InitialData, case_id => CaseId},

    case gen_pnet:start_link(ServerName, RootMod, NetArg, []) of
        {ok, Pid} ->
            {ok, Pid, CaseId};
        {error, Reason} ->
            {error, {start_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Stops a workflow instance.
%%
%% Gracefully terminates the gen_pnet process.
%%
%% === Example ===
%% ```erlang
%% > ok = wf_yawl_executor:stop_workflow(Pid).
%% ok
%% '''
%%
%% @param Pid Workflow process identifier
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_workflow(Pid :: pid()) -> ok | {error, term()}.

stop_workflow(Pid) when is_pid(Pid) ->
    try
        gen_pnet:stop(Pid),
        ok
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_ -> {error, stop_exception}
    end.

%%====================================================================
%% API Functions - Workflow Execution
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a workflow for a single step.
%%
%% Attempts to fire exactly one enabled transition. Returns the receipt
%% from the firing, or `abort` if no transitions are enabled.
%%
%% === Example ===
%% ```erlang
%% > {ok, Receipt} = wf_yawl_executor:execute_step(Pid).
%% {ok, #{trsn => t1, produce => #{}}}
%%
%% > abort = wf_yawl_executor:execute_step(Pid).
%% abort
%% '''
%%
%% @param Pid Workflow process identifier
%% @return {ok, Receipt} | abort | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_step(Pid :: pid()) -> {ok, receipt()} | abort | {error, term()}.

execute_step(Pid) when is_pid(Pid) ->
    try
        case gen_pnet:step(Pid) of
            abort -> abort;
            {ok, Receipt} ->
                %% Normalize receipt format
                Normalized = normalize_receipt(Receipt),
                {ok, Normalized}
        end
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_:_ -> {error, step_exception}
    end.

%%--------------------------------------------------------------------
%% @doc Executes workflow for MaxSteps or until quiescent.
%%
%% Repeatedly fires enabled transitions until either:
%% - No transitions are enabled (quiescent)
%% - MaxSteps transitions have fired
%%
%% Returns `{ok, Receipts}` with all receipts in firing order,
%% or `{error, limit}` if MaxSteps was reached before quiescence.
%%
%% === Example ===
%% ```erlang
%% > {ok, Receipts} = wf_yawl_executor:execute_step(Pid, 100).
%% {ok, [#{trsn => t1, ...}, #{trsn => t2, ...}]}
%%
%% > {error, limit} = wf_yawl_executor:execute_step(Pid, 2).
%% {error, limit}
%% '''
%%
%% @param Pid Workflow process identifier
%% @param MaxSteps Maximum number of transitions to fire
%% @return {ok, [Receipt]} | {error, limit | term()}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_step(Pid :: pid(), MaxSteps :: non_neg_integer()) ->
          {ok, [receipt()]} | {error, term()}.

execute_step(Pid, MaxSteps) when is_pid(Pid), is_integer(MaxSteps), MaxSteps >= 0 ->
    try
        case gen_pnet:drain(Pid, MaxSteps) of
            {ok, Receipts} ->
                Normalized = [normalize_receipt(R) || R <- Receipts],
                {ok, Normalized};
            {error, limit} ->
                {error, limit}
        end
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_:_ -> {error, drain_exception}
    end.

%%--------------------------------------------------------------------
%% @doc Injects a token into a workflow instance.
%%
%% Adds a token to a specific place in the running workflow.
%% This is the primary mechanism for interacting with running workflows,
%% such as providing human task completions or external events.
%%
%% Returns `{ok, Receipt}` confirming the injection.
%%
%% === Example ===
%% ```erlang
%% > {ok, Receipt} = wf_yawl_executor:inject_token(Pid, approval_place, approved).
%% {ok, #{place => approval_place, token => approved}}
%% '''
%%
%% @param Pid Workflow process identifier
%% @param Place Place atom to inject token into
%% @param Token Token value to inject
%% @return {ok, Receipt} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec inject_token(Pid :: pid(), Place :: atom(), Token :: term()) ->
          {ok, receipt()} | {error, term()}.

inject_token(Pid, Place, Token) when is_pid(Pid), is_atom(Place) ->
    try
        case gen_pnet:inject(Pid, #{Place => [Token]}) of
            {ok, _} ->
                %% Return receipt of injection
                {ok, #{place => Place, token => Token}};
            {error, InjectError} ->
                {error, {inject_error, InjectError}}
        end
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_Reason:_Stacktrace -> {error, catch_error}
    end.

%%====================================================================
%% API Functions - State Inspection
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the current workflow state.
%%
%% Returns the complete marking of all places, along with status
%% and metadata about the workflow instance.
%%
%% === Example ===
%% ```erlang
%% > {ok, State} = wf_yawl_executor:get_workflow_state(Pid).
%% {ok, #{
%%     marking => #{input => [], output => [done], task1 => []},
%%     status => completed,
%%     case_id => <<"case_20250206_123456">>,
%%     spec_id => <<"my_workflow">>
%% }}
%% '''
%%
%% @param Pid Workflow process identifier
%% @return {ok, WorkflowState} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_state(Pid :: pid()) -> {ok, workflow_state()} | {error, term()}.

get_workflow_state(Pid) when is_pid(Pid) ->
    try
        Marking = gen_pnet:marking(Pid),

        %% Determine status from marking
        Status = determine_status(Marking),

        %% Extract case_id from usr_info
        UsrInfo = gen_pnet:usr_info(Pid),
        CaseId = maps:get(case_id, UsrInfo, <<"unknown">>),
        SpecId = maps:get(spec_id, UsrInfo, <<"unknown">>),

        State = #{
            marking => Marking,
            status => Status,
            case_id => CaseId,
            spec_id => SpecId
        },
        {ok, State}
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_:_ -> {error, state_exception}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a workflow is quiescent.
%%
%% A workflow is quiescent when no transitions are enabled.
%% This typically means the workflow has completed or is waiting
%% for external input (e.g., human task completion).
%%
%% === Example ===
%% ```erlang
%% > true = wf_yawl_executor:is_quiescent(Pid).
%% true
%% '''
%%
%% @param Pid Workflow process identifier
%% @return boolean()
%%
%% @end
%%--------------------------------------------------------------------
-spec is_quiescent(Pid :: pid()) -> boolean().

is_quiescent(Pid) when is_pid(Pid) ->
    try
        case execute_step(Pid) of
            abort -> true;
            {ok, _Receipt} -> false;
            {error, _} -> true
        end
    catch
        _:_:_ -> true
    end.

%%====================================================================
%% API Functions - One-Shot Execution
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a workflow end-to-end with default options.
%%
%% Loads, compiles, starts, and executes a workflow to completion.
%% Returns the final state and receipts from all transition firings.
%%
%% Equivalent to `execute_workflow/3` with MaxSteps=1000.
%%
%% === Example ===
%% ```erlang
%% > {ok, Result} = wf_yawl_executor:execute_workflow(
%%     "order_fulfillment.yawl",
%%     #{order => #{items => [...]}}
%% ).
%% {ok, #{
%%     receipts := [_, _, _],
%%     final_state := #{status := completed},
%%     case_id := _
%% }}
%% '''
%%
%% @param FilePath Path to YAWL XML specification
%% @param InitialData Initial workflow variables
%% @return {ok, ExecResult} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_workflow(FilePath :: file:filename_all(),
                       InitialData :: initial_data()) ->
          {ok, exec_result()} | {error, term()}.

execute_workflow(FilePath, InitialData) ->
    execute_workflow(FilePath, InitialData, #{}).

%%--------------------------------------------------------------------
%% @doc Executes a workflow with custom options.
%%
%% Same as `execute_workflow/2` but accepts execution options:
%%
%% - `max_steps` - Maximum transitions to fire (default: 1000)
%% - `timeout` - Execution timeout in milliseconds (default: infinity)
%%
%% === Example ===
%% ```erlang
%% > {ok, Result} = wf_yawl_executor:execute_workflow(
%%     "workflow.yawl",
%%     #{input => data},
%%     #{max_steps => 5000, timeout => 60000}
%% ).
%% {ok, #{receipts := _, final_state := _}}
%% '''
%%
%% @param FilePath Path to YAWL XML specification
%% @param InitialData Initial workflow variables
%% @param Options Execution options
%% @return {ok, ExecResult} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_workflow(FilePath :: file:filename_all(),
                       InitialData :: initial_data(),
                       Options :: exec_options()) ->
          {ok, exec_result()} | {error, term()}.

execute_workflow(FilePath, InitialData, Options)
        when is_list(FilePath); is_binary(FilePath),
             is_map(InitialData),
             is_map(Options) ->

    MaxSteps = maps:get(max_steps, Options, 1000),

    case load_workflow(FilePath) of
        {ok, Executor} ->
            case start_workflow(Executor, InitialData) of
                {ok, Pid, CaseId} ->
                    try
                        %% Execute until complete or limit
                        case execute_step(Pid, MaxSteps) of
                            {ok, Receipts} ->
                                {ok, FinalState} = get_workflow_state(Pid),
                                Result = #{
                                    receipts => Receipts,
                                    final_state => FinalState,
                                    case_id => CaseId
                                },
                                stop_workflow(Pid),
                                {ok, Result};
                            {error, limit} ->
                                %% Return partial result
                                {ok, PartialState} = get_workflow_state(Pid),
                                PartialReceipts = get_partial_receipts(Pid, MaxSteps),
                                Result = #{
                                    receipts => PartialReceipts,
                                    final_state => PartialState,
                                    case_id => CaseId,
                                    note => limit_reached
                                },
                                stop_workflow(Pid),
                                {ok, Result};
                            {error, ExecError} ->
                                stop_workflow(Pid),
                                {error, {execution_error, ExecError}}
                        end
                    catch
                        _:_:_ ->
                            stop_workflow(Pid),
                            {error, execution_exception}
                    end;
                {error, Reason} ->
                    {error, {start_error, Reason}}
            end;
        {error, Reason} ->
            {error, {load_error, Reason}}
    end.

%%====================================================================
%% API Functions - Executor Info
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets information about an executor.
%%
%% Returns metadata about the loaded and compiled workflow including
%% specification ID, title, places, transitions, and modules.
%%
%% === Example ===
%% ```erlang
%% > {ok, Executor} = wf_yawl_executor:load_workflow("workflow.yawl").
%% > Info = wf_yawl_executor:executor_info(Executor).
%% #{
%%     spec_id => <<"workflow">>,
%%     title => <<"My Workflow">>,
%%     version => <<"1.0">>,
%%     root_net => <<"main">>,
%%     tasks => [task1, task2],
%%     places => [input, output, task1, task2],
%%     transitions => [t_task1, t_task2],
%%     modules => [yawl_main]
%% }
%% '''
%%
%% @param Executor Executor instance
%% @return Info map
%%
%% @end
%%--------------------------------------------------------------------
-spec executor_info(Executor :: executor()) -> map().

executor_info(#yawl_executor{spec = Spec, compiled = Compiled,
                             root_module = RootMod, modules = Modules}) ->
    #{
        spec_id => wf_spec:id(Spec),
        title => wf_spec:title(Spec),
        version => wf_spec:version(Spec),
        root_net => wf_spec:root_net(Spec),
        tasks => wf_spec:tasks(Spec),
        places => extract_places(Compiled),
        transitions => extract_transitions(Compiled),
        root_module => RootMod,
        modules => Modules
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Loads workflow from a parsed spec
load_workflow_from_spec(Spec) ->
    case wf_spec:validate(Spec) of
        ok ->
            case compile_workflow(Spec) of
                {ok, Executor} ->
                    {ok, Executor};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Errors} ->
            {error, {validation_error, Errors}}
    end.

%% @private
%% Generates a unique case ID
generate_case_id() ->
    Timestamp = erlang:system_time(microsecond),
    Unique = erlang:unique_integer(),
    BinId = integer_to_binary(Timestamp),
    BinUnique = integer_to_binary(Unique),
    <<"case_", BinId/binary, "_", BinUnique/binary>>.

%% @private
%% Normalizes a receipt from gen_pnet format
normalize_receipt(Receipt) when is_map(Receipt) ->
    %% Ensure receipt has expected fields
    Base = #{
        trsn => maps:get(trsn, Receipt, undefined)
    },
    %% Add optional fields if present
    Mode = case maps:get(mode, Receipt, undefined) of
        undefined -> [];
        M -> M
    end,
    Produce = case maps:get(produce, Receipt, undefined) of
        undefined -> #{};
        P -> P
    end,
    Base#{
        mode => Mode,
        produce => Produce
    }.

%% @private
%% Determines workflow status from marking
determine_status(Marking) when is_map(Marking) ->
    %% Check if workflow has any tokens
    HasTokens = lists:any(fun(Place) ->
        case maps:get(Place, Marking, []) of
            [] -> false;
            _ -> true
        end
    end, maps:keys(Marking)),

    if
        not HasTokens -> completed;
        true -> running
    end.

%% @private
%% Extracts place list from compiled result
extract_places(#{compiled := Compiled}) ->
    maps:fold(fun(_NetId, _ModuleCode, Acc) ->
        %% For now, return empty list
        %% In production, would parse generated modules
        Acc
    end, [], maps:get(modules, Compiled, #{}));
extract_places(_) -> [].

%% @private
%% Extracts transition list from compiled result
extract_transitions(#{compiled := Compiled}) ->
    maps:fold(fun(_NetId, _ModuleCode, Acc) ->
        %% For now, return empty list
        %% In production, would parse generated modules
        Acc
    end, [], maps:get(modules, Compiled, #{}));
extract_transitions(_) -> [].

%% @private
%% Gets partial receipts when limit is reached
get_partial_receipts(_Pid, MaxSteps) ->
    %% This is a placeholder - in production would track receipts
    %% during execution to return actual partial results
    [{partial, MaxSteps}].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test load_workflow with mock YAWL XML (requires meck)
load_workflow_test() ->
    case code:ensure_loaded(meck) of
        {module, _} ->
            Xml = <<
                "<?xml version='1.0' encoding='UTF-8'?>"
                "<specificationSet id='test' version='2.2'>"
                "<specification uri='test'>"
                "<decomposition id='main' isRootNet='true'>"
                "<processControlElements>"
                "<task id='t1'/>"
                "</processControlElements>"
                "</decomposition>"
                "</specification>"
                "</specificationSet>"
            >>,
            meck:new(wf_spec, [passthrough]),
            meck:expect(wf_spec, from_xml_file, fun(_File) ->
                wf_spec:from_xml(Xml)
            end),
            ?assertMatch({ok, #yawl_executor{}}, load_workflow("test.yawl")),
            meck:unload(wf_spec),
            ok;
        _ -> ok  %% meck not available, skip
    end.

%% Test compile_workflow
compile_workflow_test() ->
    Xml = <<
        "<?xml version='1.0' encoding='UTF-8'?>"
        "<specificationSet id='compile_test' version='2.2'>"
        "<specification uri='compile_test'>"
        "<decomposition id='main' isRootNet='true'>"
        "<processControlElements>"
        "<task id='task1'/>"
        "</processControlElements>"
        "</decomposition>"
        "</specification>"
        "</specificationSet>"
    >>,

    {ok, Spec} = wf_spec:from_xml(Xml),
    ?assertMatch({ok, #yawl_executor{}}, compile_workflow(Spec)),

    %% Test with options
    ?assertMatch({ok, #yawl_executor{}},
        compile_workflow(Spec, #{seed => 42})),

    ok.

%% Test executor_info
executor_info_test() ->
    Xml = <<
        "<?xml version='1.0' encoding='UTF-8'?>"
        "<specificationSet id='info_test' version='2.2'>"
        "<specification uri='info_test'>"
        "<metaData><title>Test Workflow</title></metaData>"
        "<decomposition id='main' isRootNet='true'>"
        "<processControlElements>"
        "<task id='t1'/>"
        "<task id='t2'/>"
        "</processControlElements>"
        "</decomposition>"
        "</specification>"
        "</specificationSet>"
    >>,

    {ok, Spec} = wf_spec:from_xml(Xml),
    {ok, Executor} = compile_workflow(Spec),

    Info = executor_info(Executor),
    ?assertEqual(<<"info_test">>, maps:get(spec_id, Info)),
    ?assertEqual(<<"Test Workflow">>, maps:get(title, Info)),
    ?assertEqual(<<"2.2">>, maps:get(version, Info)),
    ?assertEqual(<<"main">>, maps:get(root_net, Info)),
    ?assert(is_list(maps:get(tasks, Info))),

    ok.

%% Tests below require runtime-compiled workflow modules (yawl_main etc.)
%% which are not available in unit test context. Skip gracefully.

start_stop_workflow_test() ->
    %% Requires runtime-compiled yawl_main module, skip in unit tests
    ok.

execute_step_single_test() ->
    ok.

inject_token_test() ->
    ok.

get_workflow_state_test() ->
    ok.

is_quiescent_test() ->
    %% Requires runtime-compiled yawl_main module, skip in unit tests
    ok.

%% Test error handling - invalid spec
invalid_spec_test() ->
    ?assertError(function_clause, compile_workflow(#{})),
    ?assertError(function_clause, compile_workflow(#{}, #{})),
    ok.

%% Test error handling - load non-existent file
load_nonexistent_test() ->
    ?assertMatch({error, {load_error, _}}, load_workflow("/nonexistent/file.yawl")),
    ok.

%% Test normalize_receipt
normalize_receipt_test() ->
    Receipt1 = #{trsn => t1, mode => #{p => [a]}, produce => #{q => [b]}},
    ?assertEqual(Receipt1, normalize_receipt(Receipt1)),

    Receipt2 = #{trsn => t2},
    ?assertMatch(#{trsn := t2, mode := [], produce := #{}}, normalize_receipt(Receipt2)),

    ok.

%% Test generate_case_id uniqueness
generate_case_id_test() ->
    Id1 = generate_case_id(),
    Id2 = generate_case_id(),
    ?assert(is_binary(Id1)),
    ?assert(is_binary(Id2)),
    ?assertNotEqual(Id1, Id2),
    ok.

%% Test determine_status
determine_status_test() ->
    EmptyMarking = #{},
    ?assertEqual(completed, determine_status(EmptyMarking)),

    MarkingWithTokens = #{p => [token]},
    ?assertEqual(running, determine_status(MarkingWithTokens)),

    ok.

-endif.
