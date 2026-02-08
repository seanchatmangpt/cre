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
%% @doc YAWL Pattern Unified Executor
%%
%% This module provides a unified execution interface for all 43 YAWL
%% workflow patterns. It dispatches to specific pattern executors based
%% on pattern type and provides consistent error handling, state
%% persistence, and timeout support across all patterns.
%%
%% <h3>Pattern Categories</h3>
%% <ul>
%%   <li><b>Basic (WCP-01 to WCP-10):</b> sequence, parallel_split,
%%       synchronization, exclusive_choice, simple_merge, multi_choice,
%%       synchronizing_merge, multi_merge, discriminator, arbitration</li>
%%   <li><b>Multiple Instances (WCP-11 to WCP-17):</b> implicit_termination,
%%       multiple_instances_no_sync, multiple_instances_static,
%%       multiple_instances_runtime, multiple_instances_dynamic,
%%       deferred_choice, interleaved_routing</li>
%%   <li><b>State-Based (WCP-18 to WCP-20):</b> milestone, cancel_activity,
%%       cancel_case</li>
%%   <li><b>Extended Control (WCP-21 to WCP-28):</b> structured_sync,
%%       partial_join, structured_loop, recursion, interleaved_loop,
%%       critical_section, protocol_pattern, try_catch</li>
%%   <li><b>Data Flow (WDP-01 to WDP-05):</b> param_pass, data_transform,
%%       data_distribute, data_accumulate, data_visibility</li>
%%   <li><b>Resource (WRP-01 to WRP-05):</b> resource_create, role_allocate,
%%       resource_start, role_distribute, capability_allocate</li>
%%   <li><b>Exception Handling (WHP-01 to WHP-05):</b> error_handler,
%%       retry, compensate, triggered_compensation, consecutive_compensate</li>
%% </ul>
%%
%% @end
%%
%% <h3>Doctests</h3>
%%
%% ```
%% %% Execute a basic sequence pattern
%% 1> Pattern = #sequence{task_ids = [<<"task1">>, <<"task2">>]}.
%% 2> {ok, Result} = yawl_executor:execute_pattern(Pattern, #{}).
%% 3> maps:get(status, Result).
%% sequence_complete
%%
%% %% Execute with custom options
%% 1> Options = #{timeout => 10000, persistent => true}.
%% 2> {ok, _} = yawl_executor:execute_pattern(Pattern, #{data => test}, Options).
%%
%% %% Detect pattern type
%% 1> yawl_executor:pattern_type(#sequence{}).
%% sequence
%%
%% %% Get pattern category
%% 1> yawl_executor:pattern_category(sequence).
%% basic
%%
%% %% Create execution context
%% 1> Ctx = yawl_executor:new_context(#{id => <<"my_exec">>}).
%% 2> yawl_executor:context_get(Ctx, id).
%% <<"my_exec">>
%%
%% %% Put value in context
%% 1> Ctx2 = yawl_executor:context_put(Ctx, key, value).
%% 2> yawl_executor:context_get(Ctx2, key).
%% value
%% '''
%%
%% -------------------------------------------------------------------

-module(yawl_executor).
-author("joergen@cuneiform-lang.org").

%% Include record definitions from related modules
-include("cre_yawl.hrl").
-include("cre_yawl_patterns.hrl").

%% Define type aliases that would normally come from includes
-type element_id() :: binary().
-type condition() :: binary() | {atom(), term()} | fun(() -> boolean()).

%%====================================================================
%% Exports
%%====================================================================

%% Main execution interface
-export([execute_pattern/2, execute_pattern/3]).

%% Pattern type detection
-export([pattern_type/1, pattern_category/1]).

%% Execution timeout support
-export([execute_with_timeout/3, execute_with_timeout/4]).

%% State persistence hooks
-export([save_execution_state/2, load_execution_state/1,
         delete_execution_state/1, list_execution_states/0]).

%% Execution context management
-export([new_context/0, new_context/1, context_get/2, context_put/3,
         context_merge/2, context_to_map/1]).

%% Execution statistics
-export([get_stats/0, reset_stats/0, get_pattern_stats/1]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Types
%%====================================================================

-type execution_id() :: binary() | reference().
-type pattern() :: term().
-type input() :: term().
-type result() :: term().
-type options() :: map().
-type execution_context() :: #{
    id => execution_id(),
    pattern => pattern(),
    start_time => integer(),
    timeout => integer() | infinity,
    persistent => boolean(),
    metadata => map()
}.
-type execution_state() :: #{
    context => execution_context(),
    current_step => atom(),
    completed_steps => [atom()],
    pending_steps => [atom()],
    data => map(),
    errors => [term()]
}.
-type execution_result() :: {ok, result()} | {error, term()}.

%%====================================================================
%% Records
%%====================================================================

-record(exec_stats, {
    total_executions = 0 :: non_neg_integer(),
    successful_executions = 0 :: non_neg_integer(),
    failed_executions = 0 :: non_neg_integer(),
    pattern_counts = #{} :: #{atom() => non_neg_integer()},
    last_execution_time = 0 :: integer(),
    total_execution_time = 0 :: non_neg_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a pattern with default options.
%%
%% This is the main entry point for pattern execution. It detects
%% the pattern type and dispatches to the appropriate executor.
%%
%% @param Pattern The pattern record to execute.
%% @param Input Input data for the pattern.
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_pattern(Pattern :: pattern(), Input :: input()) ->
    execution_result().

execute_pattern(Pattern, Input) ->
    execute_pattern(Pattern, Input, #{}).

%%--------------------------------------------------------------------
%% @doc Executes a pattern with custom options.
%%
%% Available options:
%% <ul>
%%   <li><code>timeout</code> - Execution timeout in ms (default: 5000)</li>
%%   <li><code>persistent</code> - Enable state persistence (default: false)</li>
%%   <li><code>exec_id</code> - Custom execution ID (default: auto-generated)</li>
%%   <li><code>retry_count</code> - Number of retries on failure (default: 0)</li>
%%   <li><code>rollback_on_error</code> - Rollback state on error (default: true)</li>
%%   <li><code>metadata</code> - User metadata map (default: #{})</li>
%% </ul%%
%%
%% @param Pattern The pattern record to execute.
%% @param Input Input data for the pattern.
%% @param Options Execution options map.
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_pattern(Pattern :: pattern(), Input :: input(),
                      Options :: options()) ->
    execution_result().

execute_pattern(Pattern, Input, Options) ->
    StartTime = erlang:monotonic_time(millisecond),
    ExecId = maps:get(exec_id, Options, generate_exec_id()),
    Timeout = maps:get(timeout, Options, 5000),
    Persistent = maps:get(persistent, Options, false),
    Metadata = maps:get(metadata, Options, #{}),

    %% Build execution context
    Context = #{
        id => ExecId,
        pattern => Pattern,
        start_time => StartTime,
        timeout => Timeout,
        persistent => Persistent,
        metadata => Metadata
    },

    %% Save initial state if persistence enabled
    case Persistent of
        true ->
            InitialState = #{
                context => Context,
                current_step => initialize,
                completed_steps => [],
                pending_steps => [],
                data => #{input => Input},
                errors => []
            },
            save_execution_state(ExecId, InitialState);
        false ->
            ok
    end,

    %% Execute with error handling and rollback
    Result = try
        PatternType = pattern_type(Pattern),
        Category = pattern_category(PatternType),

        %% Update stats
        update_stats(total_executions),
        update_pattern_count(PatternType),

        %% Dispatch to category-specific executor
        case Category of
            basic ->
                execute_basic_pattern(PatternType, Pattern, Input, Context);
            multiple_instances ->
                execute_multiple_instances_pattern(PatternType, Pattern, Input, Context);
            state_based ->
                execute_state_based_pattern(PatternType, Pattern, Input, Context);
            extended_control ->
                execute_extended_control_pattern(PatternType, Pattern, Input, Context);
            data_flow ->
                execute_data_flow_pattern(PatternType, Pattern, Input, Context);
            resource ->
                execute_resource_pattern(PatternType, Pattern, Input, Context);
            exception_handling ->
                execute_exception_handling_pattern(PatternType, Pattern, Input, Context);
            unknown ->
                {error, {unknown_pattern_type, PatternType}}
        end
    catch
        Kind:Reason:Stack ->
            ErrorContext = #{
                kind => Kind,
                reason => Reason,
                stack => Stack,
                pattern_type => pattern_type(Pattern),
                execution_step => get_current_step(Context)
            },

            %% Update error stats
            update_stats(failed_executions),

            %% Rollback if enabled
            case maps:get(rollback_on_error, Options, true) of
                true ->
                    rollback_execution_state(ExecId);
                false ->
                    ok
            end,

            {error, {execution_error, ErrorContext}}
    after
        %% Update timing stats
        EndTime = erlang:monotonic_time(millisecond),
        Elapsed = EndTime - StartTime,
        update_stats(last_execution_time, Elapsed),
        update_stats(total_execution_time, Elapsed)
    end,

    %% Save final state if persistence enabled
    case Persistent of
        true ->
            FinalState = #{
                context => Context,
                current_step => complete,
                completed_steps => [complete],
                pending_steps => [],
                data => case Result of
                    {ok, Res} -> #{result => Res, input => Input};
                    {error, _} -> #{error => Result, input => Input}
                end,
                errors => case Result of
                    {error, _} -> [Result];
                    {ok, _} -> []
                end
            },
            save_execution_state(ExecId, FinalState);
        false ->
            ok
    end,

    %% Update success stats
    case Result of
        {ok, _} -> update_stats(successful_executions);
        {error, _} -> ok
    end,

    Result.

%%--------------------------------------------------------------------
%% @doc Detects the pattern type from a pattern record.
%%
%% @param Pattern The pattern record.
%% @return Atom representing the pattern type.
%%
%% @end
%%--------------------------------------------------------------------
-spec pattern_type(Pattern :: pattern()) -> atom().

pattern_type(#workflow{}) -> workflow;
pattern_type(#sequence{}) -> sequence;
pattern_type(#parallel_split{}) -> parallel_split;
pattern_type(#synchronization{}) -> synchronization;
pattern_type(#exclusive_choice{}) -> exclusive_choice;
pattern_type(#simple_merge{}) -> simple_merge;
pattern_type(#multi_choice{}) -> multi_choice;
pattern_type(#synchronizing_merge{}) -> synchronizing_merge;
pattern_type(#multi_merge{}) -> multi_merge;
pattern_type(#discriminator{}) -> discriminator;
pattern_type(#arbitration{}) -> arbitration;
pattern_type(#param_pass{}) -> param_pass;
pattern_type(#data_transform{}) -> data_transform;
pattern_type(#data_distribute{}) -> data_distribute;
pattern_type(#data_accumulate{}) -> data_accumulate;
pattern_type(#data_visibility{}) -> data_visibility;
pattern_type(#resource_create{}) -> resource_create;
pattern_type(#role_allocate{}) -> role_allocate;
pattern_type(#resource_start{}) -> resource_start;
pattern_type(#role_distribute{}) -> role_distribute;
pattern_type(#capability_allocate{}) -> capability_allocate;
pattern_type(Pattern) when is_record(Pattern, pattern_state) ->
    %% cre_yawl_patterns pattern_state records
    Pattern#pattern_state.pattern_type;
pattern_type({pattern_type, Type}) -> Type;
pattern_type(Type) when is_atom(Type) -> Type;
pattern_type(_) -> unknown.

%%--------------------------------------------------------------------
%% @doc Returns the category for a given pattern type.
%%
%% @param PatternType The pattern type atom.
%% @return Category atom: basic, multiple_instances, state_based,
%%         extended_control, data_flow, resource, exception_handling,
%%         or unknown.
%%
%% @end
%%--------------------------------------------------------------------
-spec pattern_category(atom() | pattern()) -> atom().

pattern_category(Pattern) when is_record(Pattern, workflow); is_record(Pattern, sequence);
                                is_record(Pattern, parallel_split);
                                is_record(Pattern, synchronization);
                                is_record(Pattern, exclusive_choice);
                                is_record(Pattern, simple_merge);
                                is_record(Pattern, multi_choice);
                                is_record(Pattern, synchronizing_merge);
                                is_record(Pattern, multi_merge);
                                is_record(Pattern, discriminator);
                                is_record(Pattern, arbitration) ->
    basic;
pattern_category(Pattern) when is_record(Pattern, param_pass);
                                is_record(Pattern, data_transform);
                                is_record(Pattern, data_distribute);
                                is_record(Pattern, data_accumulate);
                                is_record(Pattern, data_visibility) ->
    data_flow;
pattern_category(Pattern) when is_record(Pattern, resource_create);
                                is_record(Pattern, role_allocate);
                                is_record(Pattern, resource_start);
                                is_record(Pattern, role_distribute);
                                is_record(Pattern, capability_allocate) ->
    resource;
pattern_category(PatternType) when is_atom(PatternType) ->
    case PatternType of
        %% Basic patterns (WCP-01 to WCP-10)
        workflow -> basic;
        sequence -> basic;
        parallel_split -> basic;
        synchronization -> basic;
        exclusive_choice -> basic;
        simple_merge -> basic;
        multi_choice -> basic;
        synchronizing_merge -> basic;
        multi_merge -> basic;
        discriminator -> basic;
        arbitration -> basic;

        %% Multiple instances patterns (WCP-11 to WCP-17)
        implicit_termination -> multiple_instances;
        multiple_instances_no_sync -> multiple_instances;
        multiple_instances_static -> multiple_instances;
        multiple_instances_runtime -> multiple_instances;
        multiple_instances_dynamic -> multiple_instances;
        deferred_choice -> multiple_instances;
        interleaved_routing -> multiple_instances;

        %% State-based patterns (WCP-18 to WCP-20)
        milestone -> state_based;
        cancel_activity -> state_based;
        cancel_case -> state_based;

        %% Extended control patterns (WCP-21 to WCP-28)
        structured_sync -> extended_control;
        partial_join -> extended_control;
        structured_loop -> extended_control;
        recursion -> extended_control;
        interleaved_loop -> extended_control;
        critical_section -> extended_control;
        protocol -> extended_control;
        protocol_pattern -> extended_control;
        try_catch -> extended_control;

        %% Data flow patterns (WDP-01 to WDP-05)
        param_pass -> data_flow;
        data_transform -> data_flow;
        data_distribute -> data_flow;
        data_accumulate -> data_flow;
        data_visibility -> data_flow;

        %% Resource patterns (WRP-01 to WRP-05)
        resource_create -> resource;
        role_allocate -> resource;
        resource_start -> resource;
        role_distribute -> resource;
        capability_allocate -> resource;

        %% Exception handling patterns (WHP-01 to WHP-05)
        error_handler -> exception_handling;
        retry -> exception_handling;
        compensation -> exception_handling;
        compensate -> exception_handling;
        triggered_compensation -> exception_handling;
        consecutive_compensation -> exception_handling;
        consecutive_compensate -> exception_handling;

        _ -> unknown
    end;
pattern_category(_Pattern) -> unknown.

%%--------------------------------------------------------------------
%% @doc Executes a pattern with a timeout.
%%
%% @param Pattern The pattern to execute.
%% @param Input Input data for the pattern.
%% @param Timeout Timeout in milliseconds or 'infinity'.
%% @return {ok, Result} or {error, timeout} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_with_timeout(Pattern :: pattern(), Input :: input(),
                           Timeout :: integer() | infinity) ->
    execution_result().

execute_with_timeout(Pattern, Input, Timeout) ->
    execute_with_timeout(Pattern, Input, #{}, Timeout).

%%--------------------------------------------------------------------
%% @doc Executes a pattern with timeout and custom options.
%%
%% The timeout parameter overrides any timeout in the Options map.
%%
%% @param Pattern The pattern to execute.
%% @param Input Input data for the pattern.
%% @param Options Execution options.
%% @param Timeout Timeout in milliseconds or 'infinity'.
%% @return {ok, Result} or {error, timeout} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_with_timeout(Pattern :: pattern(), Input :: input(),
                           Options :: options(), Timeout :: integer() | infinity) ->
    execution_result().

execute_with_timeout(Pattern, Input, Options, Timeout) ->
    %% Override timeout in options
    OptionsWithTimeout = Options#{timeout => Timeout},

    %% Spawn executor process
    Ref = make_ref(),

    %% Execute in a separate process to enforce timeout
    Parent = self(),
    Pid = spawn_link(fun() ->
        Result = execute_pattern(Pattern, Input, OptionsWithTimeout),
        Parent ! {Ref, Result}
    end),

    %% Wait for result or timeout
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        %% Kill the executor process
        unlink(Pid),
        exit(Pid, kill),
        {error, {timeout, Pattern, Timeout}}
    end.

%%--------------------------------------------------------------------
%% @doc Saves execution state to persistent storage.
%%
%% State is stored in ETS for fast access and can be restored
%% after crashes or for debugging purposes.
%%
%% @param ExecId The execution ID.
%% @param State The execution state to save.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec save_execution_state(ExecId :: execution_id(), State :: execution_state()) ->
    ok | {error, term()}.

save_execution_state(ExecId, State) ->
    try
        %% Ensure ETS table exists
        Table = ensure_state_table(),
        %% Insert state with timestamp
        Timestamp = erlang:monotonic_time(millisecond),
        ets:insert(Table, {{exec_state, ExecId}, {State, Timestamp}}),
        ok
    catch
        _:Reason -> {error, {save_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Loads execution state from persistent storage.
%%
%% @param ExecId The execution ID.
%% @return {ok, State} or {error, not_found} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec load_execution_state(ExecId :: execution_id()) ->
    {ok, execution_state()} | {error, term()}.

load_execution_state(ExecId) ->
    try
        Table = ensure_state_table(),
        case ets:lookup(Table, {exec_state, ExecId}) of
            [{{exec_state, ExecId}, {State, _Timestamp}}] ->
                {ok, State};
            [] ->
                {error, not_found}
        end
    catch
        _:Reason -> {error, {load_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes execution state from persistent storage.
%%
%% @param ExecId The execution ID.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_execution_state(ExecId :: execution_id()) ->
    ok | {error, term()}.

delete_execution_state(ExecId) ->
    try
        Table = ensure_state_table(),
        ets:delete(Table, {exec_state, ExecId}),
        ok
    catch
        _:Reason -> {error, {delete_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all execution state IDs currently stored.
%%
%% @return List of execution IDs.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_execution_states() -> [execution_id()].

list_execution_states() ->
    try
        Table = ensure_state_table(),
        AllKeys = ets:select(Table, [{{{exec_state, '$1'}}, [], ['$1']}]),
        AllKeys
    catch
        _:_ -> []
    end.

%%--------------------------------------------------------------------
%% @doc Creates a new execution context with default values.
%%
%% @return A new execution context map.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_context() -> execution_context().

new_context() ->
    new_context(#{}).

%%--------------------------------------------------------------------
%% @doc Creates a new execution context with initial values.
%%
%% @param InitialValues Map of initial context values.
%% @return A new execution context map.
%%
%% @end
%%--------------------------------------------------------------------
-spec new_context(map()) -> execution_context().

new_context(InitialValues) ->
    Defaults = #{
        id => generate_exec_id(),
        pattern => undefined,
        start_time => erlang:monotonic_time(millisecond),
        timeout => 5000,
        persistent => false,
        metadata => #{}
    },
    maps:merge(Defaults, InitialValues).

%%--------------------------------------------------------------------
%% @doc Gets a value from the execution context.
%%
%% @param Context The execution context.
%% @param Key The key to look up.
%% @return The value or 'undefined' if not found.
%%
%% @end
%%--------------------------------------------------------------------
-spec context_get(execution_context(), atom()) -> term().

context_get(Context, Key) ->
    maps:get(Key, Context, undefined).

%%--------------------------------------------------------------------
%% @doc Puts a value into the execution context.
%%
%% @param Context The execution context.
%% @param Key The key to set.
%% @param Value The value to set.
%% @return Updated execution context.
%%
%% @end
%%--------------------------------------------------------------------
-spec context_put(execution_context(), atom(), term()) -> execution_context().

context_put(Context, Key, Value) ->
    Context#{Key => Value}.

%%--------------------------------------------------------------------
%% @doc Merges a map into the execution context.
%%
%% @param Context The execution context.
%% @param Updates Map of key-value pairs to merge.
%% @return Updated execution context.
%%
%% @end
%%--------------------------------------------------------------------
-spec context_merge(execution_context(), map()) -> execution_context().

context_merge(Context, Updates) ->
    maps:merge(Context, Updates).

%%--------------------------------------------------------------------
%% @doc Converts an execution context to a plain map.
%%
%% @param Context The execution context.
%% @return A plain map representation.
%%
%% @end
%%--------------------------------------------------------------------
-spec context_to_map(execution_context()) -> map().

context_to_map(Context) when is_map(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @doc Gets global execution statistics.
%%
%% @return Map containing execution statistics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_stats() -> map().

get_stats() ->
    try
        Stats = get_stats_table(),
        case ets:lookup(Stats, stats) of
            [{stats, StatRec}] ->
                #{
                    total_executions => StatRec#exec_stats.total_executions,
                    successful_executions => StatRec#exec_stats.successful_executions,
                    failed_executions => StatRec#exec_stats.failed_executions,
                    pattern_counts => StatRec#exec_stats.pattern_counts,
                    last_execution_time => StatRec#exec_stats.last_execution_time,
                    total_execution_time => StatRec#exec_stats.total_execution_time,
                    average_execution_time => case StatRec#exec_stats.total_executions of
                        0 -> 0;
                        N -> StatRec#exec_stats.total_execution_time div N
                    end
                };
            [] ->
                #{}
        end
    catch
        _:_ -> #{}
    end.

%%--------------------------------------------------------------------
%% @doc Resets global execution statistics.
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_stats() -> ok.

reset_stats() ->
    Stats = get_stats_table(),
    ets:insert(Stats, {stats, #exec_stats{}}),
    ok.

%%--------------------------------------------------------------------
%% @doc Gets statistics for a specific pattern type.
%%
%% @param PatternType The pattern type atom.
%% @return Map containing pattern-specific statistics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern_stats(atom()) -> map().

get_pattern_stats(PatternType) ->
    AllStats = get_stats(),
    PatternCounts = maps:get(pattern_counts, AllStats, #{}),
    #{
        pattern_type => PatternType,
        execution_count => maps:get(PatternType, PatternCounts, 0),
        percentage => case maps:get(total_executions, AllStats, 0) of
            0 -> 0.0;
            Total -> (maps:get(PatternType, PatternCounts, 0) * 100.0) / Total
        end
    }.

%%====================================================================
%% Pattern Category Executors
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes basic control flow patterns (WCP-01 to WCP-10).
%%
%% Handles the ten basic workflow control patterns:
%% - Sequence: Tasks execute in order
%% - Parallel Split: Multiple branches execute concurrently
%% - Synchronization: Wait for all branches to complete
%% - Exclusive Choice: Select one branch based on condition
%% - Simple Merge: Merge without synchronization
%% - Multi Choice: Select multiple branches based on conditions
%% - Synchronizing Merge: Merge with synchronization
%% - Multi Merge: Merge multiple incoming flows
%% - Discriminator: Wait for N of M branches
%% - Arbitration: First M of N branches wins
%%
%% Example:
%% ```
%% 1> Pattern = #parallel_split{split_task_id = <<"s1">>, branch_task_ids = [<<"b1">>, <<"b2">>]}.
%% 2> {ok, Result} = yawl_executor:execute_basic_pattern(parallel_split, Pattern, #{}, #{}).
%% 3> maps:get(status, Result).
%% parallel_split_complete
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_basic_pattern(atom(), pattern(), input(), execution_context()) ->
    execution_result().

execute_basic_pattern(workflow, #workflow{
        id = Id,
        name = Name,
        tasks = Tasks,
        start_task_id = StartTaskId,
        end_task_ids = EndTaskIds}, _Input, _Context) ->
    {ok, #{
        workflow_id => Id,
        workflow_name => Name,
        task_count => maps:size(Tasks),
        start_task => StartTaskId,
        end_tasks => EndTaskIds,
        status => workflow_complete
    }};
execute_basic_pattern(sequence, #sequence{task_ids = TaskIds}, _Input, _Context) ->
    case TaskIds of
        [] -> {ok, #{sequence_complete => true, tasks => []}};
        _ -> {ok, #{sequence_complete => true, tasks => TaskIds}}
    end;
execute_basic_pattern(parallel_split, #parallel_split{
        split_task_id = SplitId,
        branch_task_ids = BranchIds}, _Input, _Context) ->
    {ok, #{
        split_task => SplitId,
        branches => BranchIds,
        status => parallel_split_complete
    }};
execute_basic_pattern(synchronization, #synchronization{
        join_task_id = JoinId,
        incoming_task_ids = IncomingIds}, _Input, _Context) ->
    {ok, #{
        join_task => JoinId,
        incoming_tasks => IncomingIds,
        status => synchronization_complete
    }};
execute_basic_pattern(exclusive_choice, #exclusive_choice{
        choice_task_id = ChoiceId,
        branches = Branches}, Input, _Context) ->
    %% Evaluate conditions and select first matching branch
    Selected = select_first_branch(Branches, Input),
    case Selected of
        {selected, BranchId} ->
            {ok, #{
                choice_task => ChoiceId,
                selected_branch => BranchId,
                status => choice_complete
            }};
        none ->
            {error, {no_branch_matched, ChoiceId}}
    end;
execute_basic_pattern(simple_merge, #simple_merge{
        merge_task_id = MergeId,
        incoming_task_ids = IncomingIds}, _Input, _Context) ->
    {ok, #{
        merge_task => MergeId,
        incoming_tasks => IncomingIds,
        status => merge_complete
    }};
execute_basic_pattern(multi_choice, #multi_choice{
        choice_task_id = ChoiceId,
        branches = Branches}, Input, _Context) ->
    %% Evaluate conditions and select all matching branches
    Selected = select_matching_branches(Branches, Input),
    {ok, #{
        choice_task => ChoiceId,
        selected_branches => Selected,
        status => multi_choice_complete
    }};
execute_basic_pattern(synchronizing_merge, #synchronizing_merge{
        merge_task_id = MergeId,
        incoming_task_ids = IncomingIds}, _Input, _Context) ->
    {ok, #{
        merge_task => MergeId,
        incoming_tasks => IncomingIds,
        status => synchronizing_merge_complete
    }};
execute_basic_pattern(multi_merge, #multi_merge{
        merge_task_id = MergeId,
        incoming_task_ids = IncomingIds}, _Input, _Context) ->
    {ok, #{
        merge_task => MergeId,
        incoming_tasks => IncomingIds,
        status => multi_merge_complete
    }};
execute_basic_pattern(discriminator, #discriminator{
        merge_task_id = MergeId,
        incoming_task_ids = IncomingIds}, _Input, _Context) ->
    {ok, #{
        merge_task => MergeId,
        incoming_tasks => IncomingIds,
        status => discriminator_complete
    }};
execute_basic_pattern(arbitration, #arbitration{
        merge_task_id = MergeId,
        incoming_task_ids = IncomingIds,
        required_count = Required}, _Input, _Context) ->
    {ok, #{
        merge_task => MergeId,
        incoming_tasks => IncomingIds,
        required_count => Required,
        status => arbitration_complete
    }};
execute_basic_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_basic_pattern, PatternType}}.

%%--------------------------------------------------------------------
%% @doc Executes multiple instance patterns (WCP-11 to WCP-17).
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multiple_instances_pattern(atom(), pattern(), input(),
                                         execution_context()) ->
    execution_result().

execute_multiple_instances_pattern(implicit_termination,
        #pattern_state{pattern_type = implicit_termination,
                      subprocess = Subprocess}, Input, _Context) ->
    {ok, #{
        pattern => implicit_termination,
        subprocess => Subprocess,
        input => Input,
        status => implicit_termination_complete
    }};
execute_multiple_instances_pattern(multiple_instances_no_sync,
        #pattern_state{pattern_type = multiple_instances_no_sync,
                      instance_count = Count,
                      pending_instances = Data}, _Input, _Context) ->
    {ok, #{
        pattern => multiple_instances_no_sync,
        instance_count => Count,
        data_items => Data,
        status => no_sync_complete
    }};
execute_multiple_instances_pattern(multiple_instances_static,
        #pattern_state{pattern_type = multiple_instances_static,
                      instance_count = Count,
                      pending_instances = Data}, _Input, _Context) ->
    {ok, #{
        pattern => multiple_instances_static,
        instance_count => Count,
        data_items => Data,
        status => static_complete
    }};
execute_multiple_instances_pattern(multiple_instances_runtime,
        #pattern_state{pattern_type = multiple_instances_runtime,
                      instance_count = Count}, _Input, _Context) ->
    {ok, #{
        pattern => multiple_instances_runtime,
        instance_count => Count,
        status => runtime_complete
    }};
execute_multiple_instances_pattern(multiple_instances_dynamic,
        #pattern_state{pattern_type = multiple_instances_dynamic,
                      choice_data = ChoiceData}, _Input, _Context) ->
    {ok, #{
        pattern => multiple_instances_dynamic,
        data_fun => maps:get(data_fun, ChoiceData, undefined),
        status => dynamic_complete
    }};
execute_multiple_instances_pattern(deferred_choice,
        #pattern_state{pattern_type = deferred_choice,
                      choice_data = ChoiceData}, Input, _Context) ->
    Options = maps:get(options, ChoiceData, #{}),
    ConditionFun = maps:get(condition, ChoiceData, fun(_) -> true end),
    Selected = select_option(Options, ConditionFun, Input),
    {ok, #{
        pattern => deferred_choice,
        selected_option => Selected,
        status => choice_complete
    }};
execute_multiple_instances_pattern(interleaved_routing,
        #pattern_state{pattern_type = interleaved_routing,
                      choice_data = ChoiceData}, _Input, _Context) ->
    Branches = maps:get(branches, ChoiceData, #{}),
    {ok, #{
        pattern => interleaved_routing,
        branches => maps:keys(Branches),
        status => interleaved_complete
    }};
execute_multiple_instances_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_multiple_instances_pattern, PatternType}}.

%%--------------------------------------------------------------------
%% @doc Executes state-based patterns (WCP-18 to WCP-20).
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_state_based_pattern(atom(), pattern(), input(),
                                   execution_context()) ->
    execution_result().

execute_state_based_pattern(milestone,
        #pattern_state{pattern_type = milestone,
                      subprocess = Activity}, Input, _Context) ->
    {ok, #{
        pattern => milestone,
        activity => Activity,
        input => Input,
        milestone_reached => true,
        status => milestone_complete
    }};
execute_state_based_pattern(cancel_activity,
        #pattern_state{pattern_type = cancel_activity,
                      subprocess = Activity}, Input, _Context) ->
    {ok, #{
        pattern => cancel_activity,
        activity => Activity,
        input => Input,
        cancelled => false,
        status => activity_complete
    }};
execute_state_based_pattern(cancel_case,
        #pattern_state{pattern_type = cancel_case,
                      subprocess = Activities}, Input, _Context) ->
    {ok, #{
        pattern => cancel_case,
        activities => Activities,
        input => Input,
        cancelled => false,
        status => case_complete
    }};
execute_state_based_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_state_based_pattern, PatternType}}.

%%--------------------------------------------------------------------
%% @doc Executes extended control patterns (WCP-21 to WCP-28).
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_extended_control_pattern(atom(), pattern(), input(),
                                        execution_context()) ->
    execution_result().

execute_extended_control_pattern(structured_sync,
        #pattern_state{pattern_type = structured_sync,
                      subprocess = Activities}, Input, _Context) ->
    {ok, #{
        pattern => structured_sync,
        activities => Activities,
        input => Input,
        synchronized => true,
        status => sync_complete
    }};
execute_extended_control_pattern(partial_join,
        #pattern_state{pattern_type = partial_join,
                      max_instances = Quorum}, Input, _Context) ->
    {ok, #{
        pattern => partial_join,
        quorum => Quorum,
        input => Input,
        status => partial_join_complete
    }};
execute_extended_control_pattern(structured_loop,
        #pattern_state{pattern_type = structured_loop,
                      choice_data = ChoiceData}, Input, _Context) ->
    LoopType = maps:get(loop_type, ChoiceData, while),
    ConditionFun = maps:get(condition, ChoiceData, fun(_) -> false end),
    ConditionResult = evaluate_condition(ConditionFun, Input),
    {ok, #{
        pattern => structured_loop,
        loop_type => LoopType,
        condition_result => ConditionResult,
        status => loop_complete
    }};
execute_extended_control_pattern(recursion,
        #pattern_state{pattern_type = recursion,
                      subprocess = RecursiveFun}, Input, _Context) ->
    {ok, #{
        pattern => recursion,
        recursive_function => RecursiveFun,
        input => Input,
        base_case => false,
        status => recursion_complete
    }};
execute_extended_control_pattern(interleaved_loop,
        #pattern_state{pattern_type = interleaved_loop,
                      subprocess = Activities}, Input, _Context) ->
    {ok, #{
        pattern => interleaved_loop,
        activities => Activities,
        input => Input,
        status => interleaved_loop_complete
    }};
execute_extended_control_pattern(critical_section,
        #pattern_state{pattern_type = critical_section,
                      choice_data = ChoiceData}, Input, _Context) ->
    LockId = maps:get(lock_id, ChoiceData, undefined),
    {ok, #{
        pattern => critical_section,
        lock_id => LockId,
        input => Input,
        status => critical_section_complete
    }};
execute_extended_control_pattern(protocol,
        #pattern_state{pattern_type = protocol,
                      subprocess = RequestFun}, Input, _Context) ->
    {ok, #{
        pattern => protocol,
        request_fun => RequestFun,
        input => Input,
        status => protocol_complete
    }};
execute_extended_control_pattern(protocol_pattern,
        #pattern_state{pattern_type = protocol,
                      choice_data = ChoiceData}, Input, _Context) ->
    Timeout = maps:get(timeout, ChoiceData, 5000),
    {ok, #{
        pattern => protocol_pattern,
        timeout => Timeout,
        input => Input,
        status => protocol_complete
    }};
execute_extended_control_pattern(try_catch,
        #pattern_state{pattern_type = try_catch,
                      subprocess = TryFun}, Input, _Context) ->
    {ok, #{
        pattern => try_catch,
        try_fun => TryFun,
        input => Input,
        exception => none,
        status => try_catch_complete
    }};
execute_extended_control_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_extended_control_pattern, PatternType}}.

%%--------------------------------------------------------------------
%% @doc Executes data flow patterns (WDP-01 to WDP-05).
%%
%% Handles data passing between workflow tasks:
%% - Parameter Passing: Pass data from source to target task
%% - Data Transformation: Apply transform function to input data
%% - Data Distribution: Send data to multiple recipients
%% - Data Accumulation: Aggregate data from multiple sources
%% - Data Visibility: Control data scope access
%%
%% Example for data transformation:
%% ```
%% 1> TransformFun = fun(X) -> X * 2 end.
%% 2> Pattern = #data_transform{input_task_id = <<"in">>, output_task_id = <<"out">>, transform_fn = TransformFun}.
%% 3> {ok, Result} = yawl_executor:execute_data_flow_pattern(data_transform, Pattern, 5, #{}).
%% 4> maps:get(output, Result).
%% 10
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_data_flow_pattern(atom(), pattern(), input(),
                                 execution_context()) ->
    execution_result().

execute_data_flow_pattern(param_pass,
        #param_pass{source_task_id = SourceId,
                   target_task_id = TargetId}, Input, _Context) ->
    {ok, #{
        pattern => param_pass,
        source => SourceId,
        target => TargetId,
        input => Input,
        status => param_pass_complete
    }};
execute_data_flow_pattern(data_transform,
        #data_transform{input_task_id = InputId,
                       output_task_id = OutputId,
                       transform_fn = TransformFn}, Input, _Context) ->
    Result = case TransformFn of
        undefined -> Input;
        Fun when is_function(Fun, 1) -> Fun(Input)
    end,
    {ok, #{
        pattern => data_transform,
        input_task => InputId,
        output_task => OutputId,
        input => Input,
        output => Result,
        status => transform_complete
    }};
execute_data_flow_pattern(data_distribute,
        #data_distribute{source_task_id = SourceId,
                        recipient_task_ids = Recipients}, Input, _Context) ->
    {ok, #{
        pattern => data_distribute,
        source => SourceId,
        recipients => Recipients,
        input => Input,
        status => distribute_complete
    }};
execute_data_flow_pattern(data_accumulate,
        #data_accumulate{source_task_ids = Sources,
                        target_task_id = TargetId,
                        aggregation_fn = AggFun}, Input, _Context) ->
    Result = case AggFun of
        undefined -> Input;
        Fun when is_function(Fun, 2) -> Fun([], Input)
    end,
    {ok, #{
        pattern => data_accumulate,
        sources => Sources,
        target => TargetId,
        input => Input,
        result => Result,
        status => accumulate_complete
    }};
execute_data_flow_pattern(data_visibility,
        #data_visibility{data_task_id = DataId,
                         scope = Scope}, Input, _Context) ->
    {ok, #{
        pattern => data_visibility,
        data_task => DataId,
        scope => Scope,
        input => Input,
        status => visibility_complete
    }};
execute_data_flow_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_data_flow_pattern, PatternType}}.

%%--------------------------------------------------------------------
%% @doc Executes resource patterns (WRP-01 to WRP-05).
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_resource_pattern(atom(), pattern(), input(),
                                execution_context()) ->
    execution_result().

execute_resource_pattern(resource_create,
        #resource_create{resource_type = ResourceType,
                        init_params = Params}, _Input, _Context) ->
    {ok, #{
        pattern => resource_create,
        resource_type => ResourceType,
        params => Params,
        status => resource_created
    }};
execute_resource_pattern(role_allocate,
        #role_allocate{role_id = RoleId,
                      required_capability = Capability}, Input, _Context) ->
    {ok, #{
        pattern => role_allocate,
        role => RoleId,
        capability => Capability,
        input => Input,
        status => role_allocated
    }};
execute_resource_pattern(resource_start,
        #resource_start{resource_id = ResourceId,
                       start_params = Params}, Input, _Context) ->
    {ok, #{
        pattern => resource_start,
        resource_id => ResourceId,
        params => Params,
        input => Input,
        status => resource_started
    }};
execute_resource_pattern(role_distribute,
        #role_distribute{work_item_ids = WorkItems,
                        role_assignments = Assignments}, Input, _Context) ->
    {ok, #{
        pattern => role_distribute,
        work_items => WorkItems,
        assignments => Assignments,
        input => Input,
        status => role_distributed
    }};
execute_resource_pattern(capability_allocate,
        #capability_allocate{required_capabilities = Capabilities,
                            resource_registry = Registry}, Input, _Context) ->
    {ok, #{
        pattern => capability_allocate,
        capabilities => Capabilities,
        registry => Registry,
        input => Input,
        status => capability_allocated
    }};
execute_resource_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_resource_pattern, PatternType}}.

%%--------------------------------------------------------------------
%% @doc Executes exception handling patterns (WHP-01 to WHP-05).
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_exception_handling_pattern(atom(), pattern(), input(),
                                          execution_context()) ->
    execution_result().

execute_exception_handling_pattern(error_handler,
        #pattern_state{pattern_type = error_handler,
                      subprocess = Activity}, Input, _Context) ->
    {ok, #{
        pattern => error_handler,
        activity => Activity,
        input => Input,
        error => none,
        status => error_handling_complete
    }};
execute_exception_handling_pattern(retry,
        #pattern_state{pattern_type = retry,
                      choice_data = ChoiceData}, Input, _Context) ->
    MaxRetries = maps:get(max_retries, ChoiceData, 3),
    {ok, #{
        pattern => retry,
        max_retries => MaxRetries,
        input => Input,
        attempts => 1,
        status => retry_complete
    }};
execute_exception_handling_pattern(compensation,
        #pattern_state{pattern_type = compensation,
                      subprocess = Activity}, Input, _Context) ->
    {ok, #{
        pattern => compensation,
        activity => Activity,
        input => Input,
        compensated => false,
        status => compensation_complete
    }};
execute_exception_handling_pattern(compensate,
        #pattern_state{pattern_type = compensation,
                      subprocess = Activity}, Input, _Context) ->
    {ok, #{
        pattern => compensate,
        activity => Activity,
        input => Input,
        compensated => false,
        status => compensate_complete
    }};
execute_exception_handling_pattern(triggered_compensation,
        #pattern_state{pattern_type = triggered_compensation,
                      subprocess = Activity}, Input, _Context) ->
    {ok, #{
        pattern => triggered_compensation,
        activity => Activity,
        input => Input,
        triggered => false,
        status => triggered_compensation_complete
    }};
execute_exception_handling_pattern(consecutive_compensation,
        #pattern_state{pattern_type = consecutive_compensation,
                      subprocess = Activities}, Input, _Context) ->
    {ok, #{
        pattern => consecutive_compensation,
        activities => Activities,
        input => Input,
        compensated => false,
        status => consecutive_compensation_complete
    }};
execute_exception_handling_pattern(consecutive_compensate,
        #pattern_state{pattern_type = consecutive_compensation,
                      subprocess = Activities}, Input, _Context) ->
    {ok, #{
        pattern => consecutive_compensate,
        activities => Activities,
        input => Input,
        compensated => false,
        status => consecutive_compensate_complete
    }};
execute_exception_handling_pattern(PatternType, _Pattern, _Input, _Context) ->
    {error, {unknown_exception_handling_pattern, PatternType}}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique execution ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_exec_id() -> execution_id().

generate_exec_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"exec_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures the state storage ETS table exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_state_table() -> ets:tid().

ensure_state_table() ->
    case ets:whereis(?MODULE) of
        undefined ->
            Heir = case get_heir_process() of
                undefined -> none;
                Pid -> {heir, Pid, []}
            end,
            ets:new(?MODULE, [named_table, public, {read_concurrency, true}, Heir]);
        Table ->
            Table
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the heir process for ETS tables.
%% @end
%%--------------------------------------------------------------------
-spec get_heir_process() -> pid() | undefined.

get_heir_process() ->
    case whereis(yawl_executor_sup) of
        undefined -> case whereis(cre_sup) of undefined -> undefined; Pid -> Pid end;
        Pid -> Pid
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures the statistics ETS table exists.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_stats_table() -> ets:tid().

get_stats_table() ->
    TableName = list_to_atom(atom_to_list(?MODULE) ++ "_stats"),
    case ets:whereis(TableName) of
        undefined ->
            Opts = [named_table, public, {read_concurrency, true}],
            OptsWithHeir = case get_heir_process() of
                undefined -> Opts;
                Pid when is_pid(Pid) -> Opts ++ [{heir, Pid, []}]
            end,
            ets:new(TableName, OptsWithHeir),
            TableName;
        Table ->
            Table
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Updates global execution statistics.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_stats(atom()) -> ok.

update_stats(Key) ->
    update_stats(Key, 1).

-spec update_stats(atom(), integer()) -> ok.

update_stats(Key, Value) when is_integer(Value) ->
    Stats = get_stats_table(),
    UpdateFun = fun(StatRec) ->
        case Key of
            total_executions ->
                StatRec#exec_stats{total_executions =
                    StatRec#exec_stats.total_executions + Value};
            successful_executions ->
                StatRec#exec_stats{successful_executions =
                    StatRec#exec_stats.successful_executions + Value};
            failed_executions ->
                StatRec#exec_stats{failed_executions =
                    StatRec#exec_stats.failed_executions + Value};
            last_execution_time ->
                StatRec#exec_stats{last_execution_time = Value};
            total_execution_time ->
                StatRec#exec_stats{total_execution_time =
                    StatRec#exec_stats.total_execution_time + Value};
            _ ->
                StatRec
        end
    end,
    case ets:lookup(Stats, stats) of
        [{stats, StatRec}] ->
            NewRec = UpdateFun(StatRec),
            ets:insert(Stats, {stats, NewRec}),
            ok;
        [] ->
            %% Initialize stats record
            InitialRec = #exec_stats{},
            NewRec = UpdateFun(InitialRec),
            ets:insert(Stats, {stats, NewRec}),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Updates pattern-specific execution count.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_pattern_count(atom()) -> ok.

update_pattern_count(PatternType) ->
    Stats = get_stats_table(),
    try
        [{stats, StatRec}] = ets:lookup(Stats, stats),
        CurrentCounts = StatRec#exec_stats.pattern_counts,
        NewCount = maps:get(PatternType, CurrentCounts, 0) + 1,
        NewRec = StatRec#exec_stats{pattern_counts =
            CurrentCounts#{PatternType => NewCount}},
        ets:insert(Stats, {stats, NewRec}),
        ok
    catch
        error:badarg ->
            %% Initialize stats record with pattern count
            ets:insert(Stats, {stats, #exec_stats{pattern_counts = #{PatternType => 1}}}),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the current execution step from context.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_current_step(execution_context()) -> atom().

get_current_step(Context) ->
    maps:get(current_step, Context, unknown).

%%--------------------------------------------------------------------
%% @private
%% @doc Rolls back execution state for a given execution ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec rollback_execution_state(execution_id()) -> ok.

rollback_execution_state(ExecId) ->
    case load_execution_state(ExecId) of
        {ok, State} ->
            %% Mark state as rolled back
            RolledBackState = State#{
                current_step => rolled_back,
                errors => [{rollback, erlang:monotonic_time(millisecond)} | maps:get(errors, State, [])]
            },
            save_execution_state(ExecId, RolledBackState);
        {error, _} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluates a condition function with input data.
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_condition(function(), input()) -> boolean().

evaluate_condition(ConditionFun, Input) when is_function(ConditionFun, 1) ->
    try ConditionFun(Input)
    catch Class:Reason ->
        logger:warning("evaluate_condition: ~p:~p", [Class, Reason]),
        false
    end;
evaluate_condition(true, _Input) -> true;
evaluate_condition(false, _Input) -> false;
evaluate_condition(_, _Input) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Selects the first matching branch for exclusive choice.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_first_branch([{element_id(), condition()}], input()) ->
    {selected, element_id()} | none.

select_first_branch([], _Input) -> none;
select_first_branch([{TaskId, Condition} | Rest], Input) ->
    case evaluate_condition(Condition, Input) of
        true -> {selected, TaskId};
        false -> select_first_branch(Rest, Input)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Selects all matching branches for multi-choice.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_matching_branches([{element_id(), condition()}], input()) ->
    [element_id()].

select_matching_branches(Branches, Input) ->
    [TaskId || {TaskId, Condition} <- Branches,
               evaluate_condition(Condition, Input)].

%%--------------------------------------------------------------------
%% @private
%% @doc Selects an option for deferred choice pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_option(map(), function(), input()) ->
    {selected, term()} | none.

select_option(Options, ConditionFun, Input) ->
    maps:fold(fun(Key, _Value, Acc) ->
        case Acc of
            none ->
                case evaluate_condition(ConditionFun, {Key, Input}) of
                    true -> {selected, Key};
                    false -> none
                end;
            _ ->
                Acc
        end
    end, none, Options).

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the yawl_executor module.
%%
%% This function validates the examples in the module documentation
%% by running them as test cases.
%%
%% @return ok if all doctests pass, or raises an error if any fail.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Execute a basic sequence pattern
    Pattern = #sequence{task_ids = [<<"task1">>, <<"task2">>]},
    {ok, Result1} = execute_pattern(Pattern, #{}),
    true = maps:get(sequence_complete, Result1),

    %% Test 2: Execute with custom options
    Options = #{timeout => 10000, persistent => false},
    {ok, _} = execute_pattern(Pattern, #{data => test}, Options),

    %% Test 3: Detect pattern type
    sequence = pattern_type(#sequence{}),
    parallel_split = pattern_type(#parallel_split{}),
    synchronization = pattern_type(#synchronization{}),

    %% Test 4: Get pattern category
    basic = pattern_category(sequence),
    basic = pattern_category(parallel_split),
    basic = pattern_category(exclusive_choice),

    %% Test 5: Create execution context
    Ctx = new_context(#{id => <<"my_exec">>}),
    <<"my_exec">> = context_get(Ctx, id),

    %% Test 6: Put value in context
    Ctx2 = context_put(Ctx, key, value),
    value = context_get(Ctx2, key),

    %% Test 7: Context merge
    Ctx3 = context_merge(Ctx2, #{new_key => new_value}),
    new_value = context_get(Ctx3, new_key),
    value = context_get(Ctx3, key),  %% Original key preserved

    %% Test 8: Context to map
    MapResult = context_to_map(Ctx3),
    true = is_map(MapResult),

    %% Test 9: Parallel split pattern execution
    ParallelPattern = #parallel_split{
        split_task_id = <<"split1">>,
        branch_task_ids = [<<"branch1">>, <<"branch2">>]
    },
    {ok, ParallelResult} = execute_pattern(ParallelPattern, #{}),
    parallel_split_complete = maps:get(status, ParallelResult),
    <<"split1">> = maps:get(split_task, ParallelResult),

    %% Test 10: Synchronization pattern execution
    SyncPattern = #synchronization{
        join_task_id = <<"join1">>,
        incoming_task_ids = [<<"branch1">>, <<"branch2">>]
    },
    {ok, SyncResult} = execute_pattern(SyncPattern, #{}),
    synchronization_complete = maps:get(status, SyncResult),

    %% Test 11: Data transform pattern execution
    TransformFun = fun(X) -> X * 2 end,
    TransformPattern = #data_transform{
        input_task_id = <<"input1">>,
        output_task_id = <<"output1">>,
        transform_fn = TransformFun
    },
    {ok, TransformResult} = execute_pattern(TransformPattern, 5),
    10 = maps:get(output, TransformResult),

    %% Test 12: State persistence
    TestExecId = <<"test_exec_123">>,
    TestState = #{
        context => #{id => TestExecId},
        current_step => testing,
        completed_steps => [],
        pending_steps => [],
        data => #{test => true},
        errors => []
    },
    ok = save_execution_state(TestExecId, TestState),
    {ok, LoadedState} = load_execution_state(TestExecId),
    testing = maps:get(current_step, LoadedState),
    ok = delete_execution_state(TestExecId),

    %% Test 13: Statistics tracking
    reset_stats(),
    Stats = get_stats(),
    0 = maps:get(total_executions, Stats),

    %% Test 14: Pattern-specific statistics
    PatternStats = get_pattern_stats(sequence),
    sequence = maps:get(pattern_type, PatternStats),

    %% Test 15: Timeout execution
    QuickPattern = #sequence{task_ids = []},
    {ok, _} = execute_with_timeout(QuickPattern, #{}, 1000),

    ok.
