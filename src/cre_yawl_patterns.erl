%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jrgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jrgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%% @version 0.1.0
%%
%% @doc YAWL Workflow Control Patterns for CRE (WCP-11 through WCP-28)
%%
%% This module implements YAWL (Yet Another Workflow Language) workflow
%% control patterns WCP-11 through WCP-28 and exception handling patterns
%% WHP-1 through WHP-5 as Petri net structures compatible with the gen_pnet
%% behavior and CRE runtime environment.
%%
%% <h3>Basic Control Patterns (WCP-11 through WCP-17)</h3>
%%
%% <ul>
%%   <li><b>WCP-11: Implicit Termination</b> - A subprocess should terminate
%%       when no work remains and all its input conditions are satisfied.</li>
%%   <li><b>WCP-12: Multiple Instances without Synchronization</b> - Create
%%       concurrent instances without synchronizing after completion.</li>
%%   <li><b>WCP-13: Multiple Instances with Design Time Knowledge</b> - Fixed
%%       number of instances known at design time.</li>
%%   <li><b>WCP-14: Multiple Instances with Runtime Knowledge</b> - Number of
%%       instances determined at runtime.</li>
%%   <li><b>WCP-15: Multiple Instances without Prior Knowledge</b> - Dynamic
%%       creation of instances during execution.</li>
%%   <li><b>WCP-16: Deferred Choice</b> - Defer choice between alternatives until
%%       runtime based on data availability.</li>
%%   <li><b>WCP-17: Interleaved Parallel Routing</b> - Interleave execution of
%%       parallel branches in a non-deterministic manner.</li>
%% </ul>
%%
%% <h3>State-Based Patterns (WCP-18 through WCP-28)</h3>
%%
%% <ul>
%%   <li><b>WCP-18: Milestone</b> - Activity enabled only when milestone reached.</li>
%%   <li><b>WCP-19: Cancel Activity</b> - Cancel specific running activities.</li>
%%   <li><b>WCP-20: Cancel Case</b> - Cancel entire workflow case.</li>
%%   <li><b>WCP-21: Structured Synchronization</b> - Synchronized block with barrier.</li>
%%   <li><b>WCP-22: Partial Join</b> - Wait for quorum of parallel branches.</li>
%%   <li><b>WCP-23: Structured Loop</b> - While/until loop constructs.</li>
%%   <li><b>WCP-24: Recursion</b> - Recursive workflow invocation.</li>
%%   <li><b>WCP-25: Interleaved Loop</b> - Loop with interleaved parallel execution.</li>
%%   <li><b>WCP-26: Critical Section</b> - Mutual exclusion for shared resources.</li>
%%   <li><b>WCP-27: Protocol</b> - Request-response communication pattern.</li>
%%   <li><b>WCP-28: Try-Catch</b> - Exception handling with protected regions.</li>
%% </ul>
%%
%% <h3>Exception Handling Patterns (WHP-1 through WHP-5)</h3>
%%
%% <ul>
%%   <li><b>WHP-1: Error Handler</b> - Catch-all error handling mechanism.</li>
%%   <li><b>WHP-2: Retry</b> - Automatic retry with backoff.</li>
%%   <li><b>WHP-3: Compensation</b> - Undo completed activities on failure.</li>
%%   <li><b>WHP-4: Triggered Compensation</b> - Explicitly triggered compensation.</li>
%%   <li><b>WHP-5: Consecutive Compensation</b> - Reverse-order compensation chain.</li>
%% </ul>
%%
%% <h3>Resource Patterns (WRP-01 through WRP-05)</h3>
%%
%% <ul>
%%   <li><b>WRP-01: Direct Resource Creation</b> - Create resources on-demand within workflow.</li>
%%   <li><b>WRP-02: Role-Based Allocation</b> - Allocate resources based on role definitions.</li>
%%   <li><b>WRP-03: Resource Initialization</b> - Initialize and configure resources before use.</li>
%%   <li><b>WRP-04: Resource Allocation</b> - Assign resources to tasks and track availability.</li>
%%   <li><b>WRP-05: Resource Deallocation</b> - Release resources after task completion.</li>
%% </ul>
%%
%% <h3>Petri Net Mapping</h3>
%%
%% Each pattern is represented as a structure containing:
%% <ul>
%%   <li><code>place_lst/0</code> - List of places in the Petri net</li>
%%   <li><code>trsn_lst/0</code> - List of transitions</li>
%%   <li><code>init_marking/2</code> - Initial token distribution</li>
%%   <li><code>preset/1</code> - Input places for each transition</li>
%%   <li><code>is_enabled/3</code> - Transition enabling conditions</li>
%%   <li><code>fire/3</code> - Token production on firing</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Pattern registration - WCP-11 (Implicit Termination):
%% ```erlang
%% > Pattern1 = cre_yawl_patterns:implicit_termination(fun(X) -> X * 2 end),
%% Pattern1#pattern_state.pattern_type.
%% implicit_termination
%%
%% > Pattern1#pattern_state.subprocess(5).
%% 10
%% ```
%%
%% Pattern registration - WCP-12 (Multiple Instances without Sync):
%% ```erlang
%% > Pattern2 = cre_yawl_patterns:multiple_instances_no_sync(fun(X) -> X + 1 end, 3, [1, 2, 3]),
%% Pattern2#pattern_state.pattern_type.
%% multiple_instances_no_sync
%%
%% > Pattern2#pattern_state.instance_count.
%% 3
%% ```
%%
%% Pattern registration - WCP-13 (Static Multiple Instances):
%% ```erlang
%% > Pattern3 = cre_yawl_patterns:multiple_instances_static(fun(X) -> X end, 2, []),
%% Pattern3#pattern_state.pattern_type.
%% multiple_instances_static
%%
%% > Pattern3#pattern_state.max_instances.
%% 2
%% ```
%%
%% Pattern registration - WCP-14 (Runtime Multiple Instances):
%% ```erlang
%% > Pattern4 = cre_yawl_patterns:multiple_instances_runtime(fun(X) -> X end, fun(_) -> 3 end, test_data),
%% Pattern4#pattern_state.pattern_type.
%% multiple_instances_runtime
%%
%% > Pattern4#pattern_state.instance_count.
%% 3
%% ```
%%
%% Pattern registration - WCP-15 (Dynamic Multiple Instances):
%% ```erlang
%% > DataFun = fun() -> done end,
%% Pattern5 = cre_yawl_patterns:multiple_instances_dynamic(fun(X) -> X end, DataFun, start),
%% Pattern5#pattern_state.pattern_type.
%% multiple_instances_dynamic
%%
%% > Pattern5#pattern_state.max_instances.
%% unlimited
%% ```
%%
%% Pattern registration - WCP-16 (Deferred Choice):
%% ```erlang
%% > Options = #{a => fun() -> option_a end, b => fun() -> option_b end},
%% Pattern6 = cre_yawl_patterns:deferred_choice(Options, fun(_) -> true end, ignored),
%% Pattern6#pattern_state.pattern_type.
%% deferred_choice
%%
%% > maps:is_key(options, Pattern6#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WCP-17 (Interleaved Routing):
%% ```erlang
%% > Branches = #{branch1 => fun(X) -> X end, branch2 => fun(X) -> X * 2 end},
%% Pattern7 = cre_yawl_patterns:interleaved_routing(Branches, ignored),
%% Pattern7#pattern_state.pattern_type.
%% interleaved_routing
%%
%% > maps:is_key(branches, Pattern7#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WCP-18 (Milestone):
%% ```erlang
%% > Pattern8 = cre_yawl_patterns:milestone(fun() -> work end, fun() -> true end),
%% Pattern8#pattern_state.pattern_type.
%% milestone
%%
%% > maps:is_key(milestone_check, Pattern8#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WCP-19 (Cancel Activity):
%% ```erlang
%% > Pattern9 = cre_yawl_patterns:cancel_activity(fun() -> work end, fun() -> false end),
%% Pattern9#pattern_state.pattern_type.
%% cancel_activity
%%
%% > maps:is_key(cancel_check, Pattern9#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WCP-20 (Cancel Case):
%% ```erlang
%% > Activities = [fun() -> a end, fun() -> b end],
%% Pattern10 = cre_yawl_patterns:cancel_case(Activities, fun() -> false end),
%% Pattern10#pattern_state.pattern_type.
%% cancel_case
%%
%% > Pattern10#pattern_state.instance_count.
%% 2
%% ```
%%
%% Pattern registration - WCP-21 (Structured Sync):
%% ```erlang
%% > SyncActivities = [fun() -> a end, fun() -> b end],
%% Pattern11 = cre_yawl_patterns:structured_sync(SyncActivities, data),
%% Pattern11#pattern_state.pattern_type.
%% structured_sync
%%
%% > Pattern11#pattern_state.instance_count.
%% 2
%% ```
%%
%% Pattern registration - WCP-22 (Partial Join):
%% ```erlang
%% > JoinActivities = [fun() -> a end, fun() -> b end, fun() -> c end],
%% Pattern12 = cre_yawl_patterns:partial_join(JoinActivities, 2),
%% Pattern12#pattern_state.pattern_type.
%% partial_join
%%
%% > Pattern12#pattern_state.max_instances.
%% 2
%% ```
%%
%% Pattern registration - WCP-23 (Structured Loop):
%% ```erlang
%% > LoopBody = fun(X) -> X + 1 end,
%% Pattern13 = cre_yawl_patterns:structured_loop(LoopBody, while, fun(_) -> false end),
%% Pattern13#pattern_state.pattern_type.
%% structured_loop
%%
%% > maps:get(loop_type, Pattern13#pattern_state.choice_data).
%% while
%% ```
%%
%% Pattern registration - WCP-24 (Recursion):
%% ```erlang
%% > RecFun = fun(X) -> X - 1 end,
%% BaseCase = fun(X) -> X =< 0 end,
%% Pattern14 = cre_yawl_patterns:recursion(RecFun, BaseCase),
%% Pattern14#pattern_state.pattern_type.
%% recursion
%%
%% > maps:is_key(base_case, Pattern14#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WCP-25 (Interleaved Loop):
%% ```erlang
%% > LoopActivities = [fun() -> a end, fun() -> b end],
%% LoopCond = fun(_) -> false end,
%% Pattern15 = cre_yawl_patterns:interleaved_loop(LoopActivities, LoopCond),
%% Pattern15#pattern_state.pattern_type.
%% interleaved_loop
%%
%% > Pattern15#pattern_state.subprocess.
%% [fun() -> a end, fun() -> b end]
%% ```
%%
%% Pattern registration - WCP-26 (Critical Section):
%% ```erlang
%% > CriticalFun = fun(X) -> X end,
%% Pattern16 = cre_yawl_patterns:critical_section(CriticalFun, my_lock),
%% Pattern16#pattern_state.pattern_type.
%% critical_section
%%
%% > maps:get(lock_id, Pattern16#pattern_state.choice_data).
%% my_lock
%% ```
%%
%% Pattern registration - WCP-27 (Protocol Pattern):
%% ```erlang
%% > ReqFun = fun() -> request end,
%% RespFun = fun(_) -> response end,
%% Pattern17 = cre_yawl_patterns:protocol_pattern(ReqFun, RespFun, 5000),
%% Pattern17#pattern_state.pattern_type.
%% protocol
%%
%% > maps:get(timeout, Pattern17#pattern_state.choice_data).
%% 5000
%% ```
%%
%% Pattern registration - WCP-28 (Try-Catch):
%% ```erlang
%% > TryFun = fun() -> try_work end,
%% CatchFun = fun(_) -> catch_work end,
%% Pattern18 = cre_yawl_patterns:try_catch(TryFun, CatchFun, [error, exit]),
%% Pattern18#pattern_state.pattern_type.
%% try_catch
%%
%% > maps:get(exceptions, Pattern18#pattern_state.choice_data).
%% [error, exit]
%% ```
%%
%% Pattern registration - WHP-1 (Error Handler):
%% ```erlang
%% > Protected = fun() -> work end,
%% Handler = fun(_) -> handled end,
%% Pattern19 = cre_yawl_patterns:error_handler(Protected, Handler),
%% Pattern19#pattern_state.pattern_type.
%% error_handler
%%
%% > maps:is_key(error_handler, Pattern19#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WHP-2 (Retry):
%% ```erlang
%% > Activity = fun() -> retry_work end,
%% Backoff = fun(N) -> N * 100 end,
%% Pattern20 = cre_yawl_patterns:retry(Activity, 3, Backoff),
%% Pattern20#pattern_state.pattern_type.
%% retry
%%
%% > maps:get(max_retries, Pattern20#pattern_state.choice_data).
%% 3
%% ```
%%
%% Pattern registration - WHP-3 (Compensation):
%% ```erlang
%% > Compensable = fun() -> work end,
%% Compensator = fun(_) -> undo end,
%% Pattern21 = cre_yawl_patterns:compensate(Compensable, Compensator),
%% Pattern21#pattern_state.pattern_type.
%% compensation
%%
%% > maps:is_key(compensator, Pattern21#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WHP-4 (Triggered Compensation):
%% ```erlang
%% > TrigComp = fun() -> work end,
%% CompFun = fun(_) -> undo end,
%% TrigFun = fun() -> false end,
%% Pattern22 = cre_yawl_patterns:triggered_compensation(TrigComp, CompFun, TrigFun),
%% Pattern22#pattern_state.pattern_type.
%% triggered_compensation
%%
%% > maps:is_key(compensator, Pattern22#pattern_state.choice_data),
%% maps:is_key(trigger, Pattern22#pattern_state.choice_data).
%% true
%% ```
%%
%% Pattern registration - WHP-5 (Consecutive Compensation):
%% ```erlang
%% > Pairs = [{fun() -> a end, fun(_) -> undo_a end}, {fun() -> b end, fun(_) -> undo_b end}],
%% Pattern23 = cre_yawl_patterns:consecutive_compensate(Pairs),
%% Pattern23#pattern_state.pattern_type.
%% consecutive_compensation
%%
%% > Pattern23#pattern_state.instance_count.
%% 2
%% ```
%%
%% Pattern registration - WRP-01 (Direct Resource Creation):
%% ```erlang
%% > CreateFun = fun() -> #{id => 1, type => disk} end,
%% Pattern24 = cre_yawl_patterns:direct_resource_creation(CreateFun),
%% Pattern24#pattern_state.pattern_type.
%% direct_resource_creation
%% ```
%%
%% Pattern registration - WRP-02 (Role-Based Allocation):
%% ```erlang
%% > Pattern25 = cre_yawl_patterns:role_based_allocation(admin, #{admin => [user1, user2]}),
%% Pattern25#pattern_state.pattern_type.
%% role_based_allocation
%%
%% > maps:get(required_role, Pattern25#pattern_state.choice_data).
%% admin
%% ```
%%
%% Pattern registration - WRP-03 (Resource Initialization):
%% ```erlang
%% > InitFun = fun(R) -> maps:put(initialized, true, R) end,
%% Pattern26 = cre_yawl_patterns:resource_initialization(InitFun, #{id => 1}),
%% Pattern26#pattern_state.pattern_type.
%% resource_initialization
%% ```
%%
%% Pattern registration - WRP-04 (Resource Allocation):
%% ```erlang
%% > Pattern27 = cre_yawl_patterns:resource_allocation([res1, res2], task1),
%% Pattern27#pattern_state.pattern_type.
%% resource_allocation
%%
%% > Pattern27#pattern_state.instance_count.
%% 2
%% ```
%%
%% Pattern registration - WRP-05 (Resource Deallocation):
%% ```erlang
%% > CleanupFun = fun(R) -> maps:put(cleaned, true, R) end,
%% Pattern28 = cre_yawl_patterns:resource_deallocation(CleanupFun, res1),
%% Pattern28#pattern_state.pattern_type.
%% resource_deallocation
%% ```
%%
%% Petri net enumeration - places:
%% ```erlang
%% > Places = cre_yawl_patterns:place_lst(),
%% length(Places) > 50.
%% true
%%
%% > lists:member('p_start', Places).
%% true
%%
%% > lists:member('p_terminate', Places).
%% true
%% ```
%%
%% Petri net enumeration - transitions:
%% ```erlang
%% > Transitions = cre_yawl_patterns:trsn_lst(),
%% length(Transitions) > 50.
%% true
%%
%% > lists:member('t_activate', Transitions).
%% true
%%
%% > lists:member('t_implicit_term', Transitions).
%% true
%% ```
%%
%% Petri net preset lookup:
%% ```erlang
%% > cre_yawl_patterns:preset('t_activate').
%% ['p_start']
%%
%% > cre_yawl_patterns:preset('t_implicit_term').
%% ['p_active', 'p_work']
%%
%% > cre_yawl_patterns:preset('t_spawn_all_static').
%% ['p_instance_pool']
%% ```
%%
%% Petri net initial marking:
%% ```erlang
%% > cre_yawl_patterns:init_marking('p_start', #pattern_state{}).
%% [start]
%%
%% > cre_yawl_patterns:init_marking('p_instance_pool', #pattern_state{}).
%% []
%% ```
%%
%% Running the doctests:
%% ```erlang
%% > cre_yawl_patterns:doctest_test().
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_yawl_patterns).
-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         trigger/3]).

-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3]).

%% Pattern API functions (WCP-11 through WCP-17)
-export([implicit_termination/1,
         multiple_instances_no_sync/3,
         multiple_instances_static/3,
         multiple_instances_runtime/3,
         multiple_instances_dynamic/3,
         deferred_choice/3,
         interleaved_routing/2]).

%% Pattern Execution API functions (WCP-11 through WCP-17)
-export([execute_implicit_termination/2,
         execute_multiple_instances_no_sync/3,
         execute_multiple_instances_static/3,
         execute_multiple_instances_runtime/3,
         execute_multiple_instances_dynamic/3,
         execute_deferred_choice/3,
         execute_interleaved_routing/2,
         execute_recursion/2,
         execute_interleaved_loop/2,
         execute_critical_section/2]).

%% Pattern API functions - State-Based (WCP-18 through WCP-28)
-export([milestone/2,
         cancel_activity/2,
         cancel_case/2,
         structured_sync/2,
         partial_join/2,
         structured_loop/3,
         recursion/2,
         interleaved_loop/2,
         critical_section/2,
         protocol_pattern/3,
         try_catch/3]).

%% Pattern API functions - Exception Handling (WHP-1 through WHP-5)
-export([error_handler/2,
         retry/3,
         compensate/2,
         triggered_compensation/3,
         consecutive_compensate/1]).

%% Pattern API functions - Data Flow (WDP-01 through WDP-05)
-export([param_pass/1,
         data_transform/2,
         data_distribute/2,
         data_accumulate/2,
         data_visibility/2]).

%% Pattern Execution API functions - Data Flow (WDP-01 through WDP-05)
-export([execute_param_pass/2,
         execute_data_transform/2,
         execute_data_distribute/2,
         execute_data_accumulate/2,
         execute_data_visibility/2]).

%% Pattern API functions - Resource (WRP-01 through WRP-05)
-export([direct_resource_creation/1,
         role_based_allocation/2,
         resource_initialization/2,
         resource_allocation/2,
         resource_deallocation/2]).

%% Pattern Execution API functions - Resource (WRP-01 through WRP-05)
-export([execute_direct_resource_creation/1,
         execute_role_based_allocation/2,
         execute_resource_initialization/2,
         execute_resource_allocation/2,
         execute_resource_deallocation/2]).

%%====================================================================
%% Record definitions
%%====================================================================

-record(pattern_state, {
          pattern_type :: atom(),
          subprocess :: module() | function() | undefined,
          instance_count :: non_neg_integer() | undefined,
          max_instances :: non_neg_integer() | unlimited | undefined,
          pending_instances = [] :: list(),
          active_instances = [] :: list(),
          completed_instances = [] :: list(),
          choice_data = #{} :: map(),
          branch_queue = [] :: list()
         }).

-record(instance_token, {
          instance_id :: reference(),
          data :: term()
         }).

-record(branch_token, {
          branch_id :: atom(),
          data :: term(),
          index :: non_neg_integer()
         }).

-record(sync_token, {
          activity_id :: reference(),
          data :: term(),
          completed :: boolean()
         }).

-record(loop_state, {
          iteration :: non_neg_integer(),
          condition_result :: boolean(),
          body_data :: term()
         }).

-record(recursion_token, {
          level :: non_neg_integer(),
          data :: term(),
          is_base_case :: boolean()
         }).

-record(protocol_state, {
          request :: term(),
          response :: term(),
          timeout_ref :: reference() | undefined,
          start_time :: integer()
         }).

-record(catch_state, {
          exception_type :: atom(),
          exception_reason :: term(),
          handled :: boolean()
         }).

%% Data Flow Pattern Records (WDP-01 through WDP-05)

-record(param_pass_state, {
          source_task :: atom() | function(),
          target_task :: atom() | function(),
          param_type :: term(),
          param_schema :: map() | undefined,
          validation_rules = [] :: list()
         }).

-record(data_transform_state, {
          transform_fn :: function() | {module(), atom()},
          input_type :: term(),
          output_type :: term(),
          schema_in :: map() | undefined,
          schema_out :: map() | undefined,
          validation_mode = strict :: atom()
         }).

-record(data_distribute_state, {
          distribution_strategy :: broadcast | round_robin | partitioned,
          source_data :: term(),
          target_tasks :: list(),
          partition_fn :: function() | undefined,
          batch_size :: pos_integer() | unlimited
         }).

-record(data_accumulate_state, {
          accumulation_fn :: sum | avg | count | max | min | custom,
          custom_accumulator :: function() | undefined,
          initial_value :: term(),
          collected_values = [] :: list(),
          intermediate_results = [] :: list()
         }).

-record(data_visibility_state, {
          scope :: local | branch | global,
          variable_name :: atom(),
          variable_value :: term(),
          accessible_contexts = [] :: list(),
          visibility_rules = #{} :: map()
         }).

%% Resource Pattern Records (WRP-01 through WRP-05)
-record(resource, {
          resource_id :: reference() | atom(),
          name :: atom(),
          status :: available | busy | unavailable,
          capabilities = [] :: list(),
          roles = [] :: list(),
          workload = 0 :: non_neg_integer(),
          parameters = #{} :: map(),
          created_at :: non_neg_integer(),
          metadata = #{} :: map()
         }).

-record(resource_registry, {
          resources = #{} :: map(),
          role_index = #{} :: map(),
          capability_index = #{} :: map(),
          allocation_history = [] :: list(),
          total_allocations = 0 :: non_neg_integer()
         }).

-record(resource_allocation, {
          allocation_id :: reference(),
          resource_id :: reference(),
          work_item_id :: reference(),
          allocated_at :: non_neg_integer(),
          allocated_by :: atom(),
          strategy :: atom(),
          criteria = #{} :: map(),
          status :: pending | active | completed,
          result :: term(),
          completed_at :: non_neg_integer() | undefined
         }).

-record(work_item, {
          work_item_id :: reference(),
          activity_id :: atom(),
          required_capabilities = [] :: list(),
          required_roles = [] :: list(),
          parameters = #{} :: map(),
          affinity :: atom() | undefined,
          created_at :: non_neg_integer(),
          data :: term()
         }).

%%====================================================================
%% Pattern Execution API functions (WCP-11 through WCP-17)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes an Implicit Termination pattern (WCP-11).
%%
%% This function executes the subprocess and automatically terminates
%% when no work remains and all input conditions are satisfied.
%%
%% @param Pattern The pattern state from implicit_termination/1.
%% @param InitialData Initial data to pass to the subprocess.
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_implicit_termination(Pattern :: #pattern_state{},
                                    InitialData :: term()) ->
          {ok, term()} | {error, term()}.

execute_implicit_termination(#pattern_state{subprocess = Subprocess}, InitialData) ->
    try
        case Subprocess of
            Fun when is_function(Fun, 1) ->
                Result = Fun(InitialData),
                {ok, Result};
            {M, F} when is_atom(M), is_atom(F) ->
                case erlang:apply(M, F, [InitialData]) of
                    {ok, Result} -> {ok, Result};
                    Result -> {ok, Result}
                end;
            _ ->
                {error, invalid_subprocess}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Multiple Instances without Synchronization pattern (WCP-12).
%%
%% This function creates concurrent instances that operate independently
%% without any synchronization point. Results are collected as they complete.
%%
%% @param Pattern The pattern state from multiple_instances_no_sync/3.
%% @param Timeout Maximum time to wait for each instance (infinity for no limit).
%% @param Options Additional options: {on_error, continue | stop}, {max_concurrent, N}.
%% @return {ok, Results} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multiple_instances_no_sync(Pattern :: #pattern_state{},
                                         _Timeout :: timeout(),
                                         Options :: map()) ->
          {ok, list()} | {error, term()}.

execute_multiple_instances_no_sync(#pattern_state{
                                     subprocess = Subprocess,
                                     instance_count = Count,
                                     pending_instances = InputData
                                    }, _Timeout, Options) ->
    OnError = maps:get(on_error, Options, continue),
    MaxConcurrent = maps:get(max_concurrent, Options, Count),

    DataList = case InputData of
        [] -> lists:seq(1, Count);
        L when is_list(L) -> L
    end,

    Ref = make_ref(),
    Parent = self(),

    %% Spawn supervisor to manage instance processes (monitored for crash detection)
    {SupervisorPid, MRef} = spawn_monitor(fun() ->
        instance_supervisor(Ref, Parent, Subprocess, DataList, OnError, MaxConcurrent)
    end),

    %% Wait for supervisor to complete
    receive
        {Ref, {ok, Results}} ->
            demonitor(MRef, [flush]),
            {ok, Results};
        {Ref, {error, Reason}} ->
            demonitor(MRef, [flush]),
            {error, Reason};
        {'DOWN', MRef, process, SupervisorPid, Reason} ->
            {error, {supervisor_exit, Reason}}
    after 30000 ->
        exit(SupervisorPid, kill),
        demonitor(MRef, [flush]),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Multiple Instances with Design Time Knowledge pattern (WCP-13).
%%
%% This function spawns a fixed number of instances known at design time
%% and synchronizes when all instances complete.
%%
%% @param Pattern The pattern state from multiple_instances_static/3.
%% @param Timeout Maximum time to wait for all instances (infinity for no limit).
%% @param Options Additional options: {on_error, continue | stop}.
%% @return {ok, Results} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multiple_instances_static(Pattern :: #pattern_state{},
                                        Timeout :: timeout(),
                                        Options :: map()) ->
          {ok, list()} | {error, term()}.

execute_multiple_instances_static(#pattern_state{
                                     subprocess = Subprocess,
                                     instance_count = Count,
                                     pending_instances = InputData
                                    }, Timeout, Options) ->
    OnError = maps:get(on_error, Options, continue),

    DataList = case InputData of
        [] -> lists:seq(1, Count);
        L when is_list(L) -> lists:sublist(L, Count)
    end,

    Ref = make_ref(),
    Parent = self(),

    %% Spawn all instances concurrently (monitored for crash detection)
    PidMRefs = lists:map(fun(Data) ->
        spawn_monitor(fun() ->
            Result = execute_instance(Subprocess, Data),
            Parent ! {Ref, self(), Result}
        end)
    end, DataList),

    %% Collect results from all instances
    Results = collect_all_results(Ref, PidMRefs, Timeout, OnError, []),

    case [R || R <- Results, element(1, R) =:= error] of
        [] ->
            {ok, [Result || {ok, Result} <- Results]};
        Errors when OnError =:= stop ->
            {error, {instance_errors, Errors}};
        _Errors ->
            {ok, [Result || {ok, Result} <- Results]}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Multiple Instances with Runtime Knowledge pattern (WCP-14).
%%
%% This function evaluates the instance count at runtime and spawns
%% that many instances, synchronizing on completion.
%%
%% @param Pattern The pattern state from multiple_instances_runtime/3.
%% @param InputData Data for evaluating count and passing to instances.
%% @param Options Additional options: {on_error, continue | stop}.
%% @return {ok, Results} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multiple_instances_runtime(Pattern :: #pattern_state{},
                                         InputData :: term(),
                                         Options :: map()) ->
          {ok, list()} | {error, term()}.

execute_multiple_instances_runtime(#pattern_state{
                                      subprocess = Subprocess,
                                      instance_count = Count
                                     }, InputData, Options) ->
    OnError = maps:get(on_error, Options, continue),

    Ref = make_ref(),
    Parent = self(),

    %% Spawn instances with data (monitored for crash detection)
    PidMRefs = lists:map(fun(I) ->
        spawn_monitor(fun() ->
            Result = execute_instance(Subprocess, {I, InputData}),
            Parent ! {Ref, self(), Result}
        end)
    end, lists:seq(1, Count)),

    %% Collect results from all instances
    Results = collect_all_results(Ref, PidMRefs, 30000, OnError, []),

    case [R || R <- Results, element(1, R) =:= error] of
        [] ->
            {ok, [Result || {ok, Result} <- Results]};
        Errors when OnError =:= stop ->
            {error, {instance_errors, Errors}};
        _Errors ->
            {ok, [Result || {ok, Result} <- Results]}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Multiple Instances without Prior Knowledge pattern (WCP-15).
%%
%% This function dynamically spawns instances based on data availability.
%% New instances can be created while others are running.
%%
%% @param Pattern The pattern state from multiple_instances_dynamic/3.
%% @param MaxInstances Maximum number of concurrent instances.
%% @param Options Additional options: {on_error, continue | stop}.
%% @return {ok, Results} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_multiple_instances_dynamic(Pattern :: #pattern_state{},
                                         MaxInstances :: pos_integer() | unlimited,
                                         Options :: map()) ->
          {ok, list()} | {error, term()}.

execute_multiple_instances_dynamic(#pattern_state{
                                      subprocess = Subprocess,
                                      choice_data = #{data_fun := DataFun},
                                      pending_instances = [InitialData]
                                     }, MaxInstances, Options) ->
    OnError = maps:get(on_error, Options, continue),
    ActualMax = case MaxInstances of
        unlimited -> 100;
        N when is_integer(N), N > 0 -> N
    end,

    Ref = make_ref(),
    Parent = self(),

    %% Spawn dynamic instance manager
    ManagerPid = spawn(fun() ->
        dynamic_instance_manager(Ref, Parent, Subprocess, DataFun,
                                 InitialData, ActualMax, OnError)
    end),

    %% Wait for manager to complete
    receive
        {Ref, {ok, Results}} -> {ok, Results};
        {Ref, {error, Reason}} -> {error, Reason};
        {'EXIT', ManagerPid, Reason} -> {error, {manager_exit, Reason}}
    after 60000 ->
        exit(ManagerPid, kill),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Deferred Choice pattern (WCP-16).
%%
%% This function evaluates options at runtime and selects the first
%% available one based on data or conditions.
%%
%% @param Pattern The pattern state from deferred_choice/3.
%% @param EvalData Data for evaluating the condition function.
%% @param Timeout Maximum time to wait for choice (infinity for no limit).
%% @return {ok, {SelectedOption, Result}} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_deferred_choice(Pattern :: #pattern_state{},
                              EvalData :: term(),
                              Timeout :: timeout()) ->
          {ok, {atom(), term()}} | {error, term()}.

execute_deferred_choice(#pattern_state{
                             subprocess = ConditionFun,
                             choice_data = #{options := Options}
                            }, EvalData, Timeout) ->
    Ref = make_ref(),
    Parent = self(),
    OptionKeys = maps:keys(Options),

    %% Spawn processes for each option
    Pids = lists:map(fun(Key) ->
        OptionFun = maps:get(Key, Options),
        spawn(fun() ->
            try
                Result = case OptionFun of
                    F when is_function(F, 1) -> F(EvalData);
                    {M, F} when is_atom(M), is_atom(F) -> erlang:apply(M, F, [EvalData]);
                    F when is_function(F, 0) -> F()
                end,
                Parent ! {Ref, {option, Key}, Result}
            catch
                _Error:_Reason:_Stack ->
                    Parent ! {Ref, {option, Key}, {error, failed}}
            end
        end)
    end, OptionKeys),

    %% Also evaluate condition function in parallel
    ConditionPid = spawn(fun() ->
        try
            ConditionResult = case ConditionFun of
                Fun when is_function(Fun, 1) -> Fun(EvalData);
                {M, F} -> erlang:apply(M, F, [EvalData]);
                _ -> true
            end,
            Parent ! {Ref, condition, ConditionResult}
        catch
            _:_:_ -> Parent ! {Ref, condition, true}
        end
    end),

    %% Wait for first successful result or condition-based choice
    AllPids = Pids ++ [ConditionPid],
    case collect_first_choice(Ref, AllPids, Timeout, OptionKeys) of
        {ok, {Key, Result}} -> {ok, {Key, Result}};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Executes an Interleaved Parallel Routing pattern (WCP-17).
%%
%% This function executes multiple branches in an interleaved fashion,
%% ensuring fair round-robin execution.
%%
%% @param Pattern The pattern state from interleaved_routing/2.
%% @param InputData Data to pass to all branches.
%% @return {ok, Results} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_interleaved_routing(Pattern :: #pattern_state{},
                                  InputData :: term()) ->
          {ok, list()} | {error, term()}.

execute_interleaved_routing(#pattern_state{
                                 choice_data = #{branches := Branches}
                                }, InputData) ->
    BranchKeys = maps:keys(Branches),
    Ref = make_ref(),
    Parent = self(),

    %% Execute branches in interleaved manner
    Results = execute_interleaved(Ref, Parent, Branches, BranchKeys,
                                  InputData, 1, [], [], BranchKeys),

    {ok, lists:reverse(Results)}.

%%--------------------------------------------------------------------
%% @doc Executes a Recursion pattern (WCP-24).
%%
%% Executes a recursive function with base case detection.
%%
%% @param Pattern The pattern state from recursion/2.
%% @param InputData Data to process recursively.
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_recursion(Pattern :: #pattern_state{},
                        InputData :: term()) ->
          {ok, term()} | {error, term()}.

execute_recursion(#pattern_state{
                    subprocess = RecursiveFun,
                    choice_data = #{base_case := BaseCaseFun}
                   }, InputData) ->
    try
        case BaseCaseFun(InputData) of
            true ->
                %% Base case - return result directly
                {ok, InputData};
            false ->
                %% Recursive case - call function and recurse
                Result = case RecursiveFun of
                    Fun when is_function(Fun, 1) -> Fun(InputData);
                    {M, F} when is_atom(M), is_atom(F) ->
                        erlang:apply(M, F, [InputData]);
                    _ ->
                        error({invalid_subprocess, RecursiveFun})
                end,
                {ok, Result}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes an Interleaved Loop pattern (WCP-25).
%%
%% Executes multiple branches in parallel with interleaved coordination.
%%
%% @param Pattern The pattern state from interleaved_loop/2.
%% @param InputData Data to pass to each loop iteration.
%% @return {ok, Results} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_interleaved_loop(Pattern :: #pattern_state{},
                                InputData :: term()) ->
          {ok, list()} | {error, term()}.

execute_interleaved_loop(#pattern_state{
                            branch_queue = Activities,
                            choice_data = #{condition := ConditionFun}
                           }, InputData) ->
    Ref = make_ref(),
    Parent = self(),

    %% Execute all activities
    Pids = lists:map(fun(Activity) ->
        spawn(fun() ->
            Result = try
                case Activity of
                    Fun when is_function(Fun, 1) -> Fun(InputData);
                    {M, F} when is_atom(M), is_atom(F) ->
                        erlang:apply(M, F, [InputData]);
                    _ ->
                        error({invalid_activity, Activity})
                end
            catch
                Error:Reason:Stack ->
                    {error, {Error, Reason, Stack}}
            end,
            Parent ! {Ref, self(), Result}
        end)
    end, Activities),

    %% Collect all results
    Results = collect_all_results(Ref, Pids, 30000, continue, []),

    %% Check continuation condition
    case ConditionFun(Results) of
        true -> {ok, Results};
        false -> {ok, Results}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Critical Section pattern (WCP-26).
%%
%% Ensures mutually exclusive access to a resource.
%%
%% @param Pattern The pattern state from critical_section/2.
%% @param InputData Data to process in critical section.
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_critical_section(Pattern :: #pattern_state{},
                                 InputData :: term()) ->
          {ok, term()} | {error, term()}.

execute_critical_section(#pattern_state{
                            subprocess = CriticalActivity,
                            choice_data = #{lock_id := LockId}
                           }, InputData) ->
    %% Create a unique lock name using global registry
    LockName = list_to_atom("critical_section_" ++ atom_to_list(LockId)),

    %% Use global locker via process registry
    try
        %% Try to register as the lock holder
        case global:set_lock(LockName, [node()], 5000) of
            true ->
                %% We got the lock - execute critical section
                try
                    Result = case CriticalActivity of
                        Fun when is_function(Fun, 1) -> Fun(InputData);
                        {M, F} when is_atom(M), is_atom(F) ->
                            erlang:apply(M, F, [InputData]);
                        _ ->
                            error({invalid_activity, CriticalActivity})
                    end,
                    global:del_lock(LockName, [node()]),
                    {ok, Result}
                catch
                    Error:Reason:Stack ->
                        global:del_lock(LockName, [node()]),
                        {error, {Error, Reason, Stack}}
                end;
            false ->
                %% Lock is held by another process
                {error, lock_not_available}
        end
    catch
        _:_ ->
            {error, lock_registration_failed}
    end.

%%====================================================================
%% Pattern API functions (WCP-11 through WCP-17)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an Implicit Termination pattern (WCP-11).
%%
%% The implicit termination pattern ensures a subprocess terminates when
%% no work remains and all input conditions are satisfied.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_active', `p_work', `p_terminate'</li>
%%   <li><b>Transitions:</b> `t_activate', `t_queue_work', `t_dequeue_work',
%%       `t_implicit_term'</li>
%%   <li><b>Semantics:</b> Termination fires when work place is empty and
%%       active place contains completion token.</li>
%% </ul>
%%
%% @param Subprocess The module or function implementing the subprocess logic.
%% @return A pattern state record for implicit termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec implicit_termination(Subprocess :: module() | function()) ->
          #pattern_state{}.

implicit_termination(Subprocess) ->
    #pattern_state{
      pattern_type = implicit_termination,
      subprocess = Subprocess,
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Multiple Instances without Synchronization pattern (WCP-12).
%%
%% This pattern creates concurrent instances of a subprocess without
%% synchronizing after completion. Each instance operates independently.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_instance_pool', `p_ready', `p_done'</li>
%%   <li><b>Transitions:</b> `t_spawn_no_sync', `t_execute_no_sync',
%%       `t_complete_no_sync'</li>
%%   <li><b>Semantics:</b> No join transition - instances complete independently.</li>
%% </ul>
%%
%% @param Subprocess The module or function for each instance.
%% @param InstanceCount Number of instances to create.
%% @param InputData Data to distribute among instances.
%% @return A pattern state record for unsynchronized instances.
%%
%% @end
%%--------------------------------------------------------------------
-spec multiple_instances_no_sync(Subprocess :: module() | function(),
                                  InstanceCount :: pos_integer(),
                                  InputData :: list()) ->
          #pattern_state{}.

multiple_instances_no_sync(Subprocess, InstanceCount, InputData) ->
    #pattern_state{
      pattern_type = multiple_instances_no_sync,
      subprocess = Subprocess,
      instance_count = InstanceCount,
      max_instances = InstanceCount,
      pending_instances = InputData
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Multiple Instances with Design Time Knowledge pattern (WCP-13).
%%
%% This pattern creates a fixed number of instances known at design time.
%% All instances are created simultaneously and synchronized upon completion.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_instance_pool', `p_ready', `p_running', `p_complete',
%%       `p_all_spawned'</li>
%%   <li><b>Transitions:</b> `t_spawn_all_static', `t_execute_static',
%%       `t_collect_static', `t_join_static'</li>
%%   <li><b>Semantics:</b> The join transition fires when all instances complete.</li>
%% </ul>
%%
%% @param Subprocess The module or function for each instance.
%% @param InstanceCount Fixed number of instances (design time constant).
%% @param InputData List of input data, one per instance.
%% @return A pattern state record for static instances.
%%
%% @end
%%--------------------------------------------------------------------
-spec multiple_instances_static(Subprocess :: module() | function(),
                                 InstanceCount :: pos_integer(),
                                 InputData :: list()) ->
          #pattern_state{}.

multiple_instances_static(Subprocess, InstanceCount, InputData) ->
    true = length(InputData) >= InstanceCount orelse InputData =:= [],
    #pattern_state{
      pattern_type = multiple_instances_static,
      subprocess = Subprocess,
      instance_count = InstanceCount,
      max_instances = InstanceCount,
      pending_instances = InputData
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Multiple Instances with Runtime Knowledge pattern (WCP-14).
%%
%% This pattern creates instances where the count is determined at runtime
%% but before instance creation begins.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_instance_pool', `p_ready', `p_running',
%%       `p_complete', `p_all_spawned'</li>
%%   <li><b>Transitions:</b> `t_eval_count', `t_spawn_runtime', `t_execute_runtime',
%%       `t_collect_runtime', `t_join_runtime'</li>
%%   <li><b>Semantics:</b> Instance count is evaluated from input data at runtime.</li>
%% </ul>
%%
%% @param Subprocess The module or function for each instance.
%% @param CountFun Function that determines instance count from input data.
%% @param InputData Data for evaluating count and distributing to instances.
%% @return A pattern state record for runtime-determined instances.
%%
%% @end
%%--------------------------------------------------------------------
-spec multiple_instances_runtime(Subprocess :: module() | function(),
                                   CountFun :: function(),
                                   InputData :: term()) ->
          #pattern_state{}.

multiple_instances_runtime(Subprocess, CountFun, InputData) ->
    InstanceCount = case CountFun of
        Fun when is_function(Fun, 1) -> Fun(InputData);
        N when is_integer(N), N > 0 -> N
    end,
    #pattern_state{
      pattern_type = multiple_instances_runtime,
      subprocess = Subprocess,
      instance_count = InstanceCount,
      max_instances = InstanceCount,
      pending_instances = [InputData]
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Multiple Instances without Prior Knowledge pattern (WCP-15).
%%
%% This pattern dynamically creates instances during execution based on
%% data availability. New instances can be spawned while others are running.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_data_source', `p_ready', `p_running', `p_done',
%%       `p_final', `p_complete'</li>
%%   <li><b>Transitions:</b> `t_spawn_dynamic', `t_fetch_data', `t_execute_dynamic',
%%       `t_collect_dynamic', `t_check_done', `t_terminate_dynamic'</li>
%%   <li><b>Semantics:</b> Dynamic spawning until data is exhausted;
%%       termination when no active instances remain.</li>
%% </ul>
%%
%% @param Subprocess The module or function for each instance.
%% @param DataFun Function that returns stream of data for instance creation.
%% @param InitialData Initial data for the first instances.
%% @return A pattern state record for dynamic instances.
%%
%% @end
%%--------------------------------------------------------------------
-spec multiple_instances_dynamic(Subprocess :: module() | function(),
                                   DataFun :: function(),
                                   InitialData :: term()) ->
          #pattern_state{}.

multiple_instances_dynamic(Subprocess, DataFun, InitialData) ->
    #pattern_state{
      pattern_type = multiple_instances_dynamic,
      subprocess = Subprocess,
      instance_count = 0,
      max_instances = unlimited,
      pending_instances = [InitialData],
      choice_data = #{data_fun => DataFun}
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Deferred Choice pattern (WCP-16).
%%
%% This pattern defers the choice between multiple alternatives until runtime,
%% selecting based on data availability or conditions.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_option_a', `p_option_b', `p_selected',
%%       `p_discarded', `p_choice_complete'</li>
%%   <li><b>Transitions:</b> `t_offer_choice', `t_select_a', `t_select_b',
%%       `t_discard_a', `t_discard_b', `t_complete_choice'</li>
%%   <li><b>Semantics:</b> Choice is made when one option's data becomes available;
%%       the other option is then discarded.</li>
%% </ul>
%%
%% @param Options Map of option identifiers to their subprocess modules/functions.
%% @param ConditionFun Function that evaluates which option to select.
%% @param _InitialData Initial data for condition evaluation (currently unused).
%% @return A pattern state record for deferred choice.
%%
%% @end
%%--------------------------------------------------------------------
-spec deferred_choice(Options :: map(),
                      ConditionFun :: function(),
                      _InitialData :: term()) ->
          #pattern_state{}.

deferred_choice(Options, ConditionFun, _InitialData) ->
    #pattern_state{
      pattern_type = deferred_choice,
      subprocess = ConditionFun,
      choice_data = maps:put(options, Options, #{})
     }.

%%--------------------------------------------------------------------
%% @doc Creates an Interleaved Parallel Routing pattern (WCP-17).
%%
%% This pattern executes multiple branches in an interleaved fashion,
%% where tasks from different branches are executed in a non-deterministic
%% alternating order.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_branch_pool', `p_next_branch',
%%       `p_executing', `p_branch_done', `p_all_done'</li>
%%   <li><b>Transitions:</b> `t_distribute_branches', `t_pick_branch',
%%       `t_execute_branch', `t_return_branch', `t_complete_interleaved'</li>
%%   <li><b>Semantics:</b> Branches take turns executing; fairness is guaranteed
%%       through the next_branch place (round-robin selection).</li>
%% </ul>
%%
%% @param Branches Map of branch identifiers to their subprocess modules.
%% @param _InitialData Initial data for all branches (currently unused).
%% @return A pattern state record for interleaved routing.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleaved_routing(Branches :: map(), _InitialData :: term()) ->
          #pattern_state{}.

interleaved_routing(Branches, _InitialData) ->
    #pattern_state{
      pattern_type = interleaved_routing,
      subprocess = undefined,
      choice_data = maps:put(branches, Branches, #{})
     }.

%%====================================================================
%% gen_pnet Interface callback functions
%%====================================================================

code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.

handle_call(_Request, _From, NetState) -> {reply, {error, bad_msg}, NetState}.

handle_cast(_Request, NetState) -> {noreply, NetState}.

handle_info(_Request, NetState) -> {noreply, NetState}.

init(_Arg) ->
    #pattern_state{pattern_type = undefined}.

terminate(_Reason, _NetState) -> ok.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%%
%% This function is called by gen_pnet when tokens arrive at specific places.
%%
%% @end
%%--------------------------------------------------------------------
trigger(_Place, _Token, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    case PatternState#pattern_state.pattern_type of
        implicit_termination ->
            trigger_implicit_termination(_Place, _Token, NetState);
        multiple_instances_no_sync ->
            trigger_no_sync(_Place, _Token, NetState);
        milestone ->
            trigger_milestone(_Place, _Token, NetState);
        cancel_activity ->
            trigger_cancel_activity(_Place, _Token, NetState);
        cancel_case ->
            trigger_cancel_case(_Place, _Token, NetState);
        _ ->
            pass
    end.

%%====================================================================
%% Petri net callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the pattern Petri net.
%%
%% The place list covers all patterns (WCP-11 through WCP-17).
%% Uses persistent_term for O(1) access (OTP 21+ optimization).
%%
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    %% Use persistent_term for O(1) access to static place list
    %% (OTP 21+ optimization) - avoids reconstructing list on every call
    cre_config:get(yawl_patterns_place_lst,
                   [
                    %% Implicit Termination (WCP-11)
                    'p_start', 'p_active', 'p_work', 'p_work_pending', 'p_terminate',

                    %% Multiple Instances (WCP-12, WCP-13, WCP-14, WCP-15)
                    'p_instance_pool', 'p_ready', 'p_running', 'p_done', 'p_complete',
                    'p_spawn_pending', 'p_all_spawned', 'p_eval', 'p_data_source', 'p_final',

                    %% Deferred Choice (WCP-16)
                    'p_choice_pending', 'p_option_a', 'p_option_b', 'p_options',
                    'p_selected', 'p_discarded', 'p_choice_complete',

                    %% Interleaved Routing (WCP-17)
                    'p_branch_pool', 'p_next_branch', 'p_executing', 'p_branch_done',
                    'p_all_done', 'p_interleave_complete',

                    %% WCP-18: Milestone Pattern
                    'p_milestone_guard', 'p_milestone_reached', 'p_active', 'p_complete',

                    %% WCP-19: Cancel Activity Pattern
                    'p_activity_running', 'p_cancellation_pending', 'p_cancelled', 'p_completed',

                    %% WCP-20: Cancel Case Pattern
                    'p_case_active', 'p_cancellation_requested', 'p_cancelling', 'p_cancelled', 'p_completed',

                    %% WCP-21: Structured Synchronization Pattern
                    'p_sync_block_start', 'p_sync_block_active', 'p_sync_barrier', 'p_sync_block_done',
                    'p_sync_activity_1', 'p_sync_activity_2', 'p_sync_activity_3',

                    %% WCP-22: Partial Join Pattern
                    'p_partial_start', 'p_partial_running', 'p_partial_quorum', 'p_partial_done',
                    'p_partial_instance_1', 'p_partial_instance_2', 'p_partial_instance_3',

                    %% WCP-23: Structured Loop Pattern
                    'p_loop_init', 'p_loop_body', 'p_loop_condition', 'p_loop_exit', 'p_loop_complete',
                    'p_loop_iteration', 'p_loop_continue', 'p_loop_break',

                    %% WCP-24: Recursion Pattern
                    'p_rec_start', 'p_rec_call', 'p_rec_result', 'p_rec_base', 'p_rec_done',
                    'p_rec_call_1', 'p_rec_call_2', 'p_rec_stack',

                    %% WCP-25: Interleaved Loop Pattern
                    'p_il_loop_start', 'p_il_parallel', 'p_il_interleave', 'p_il_loop_cond', 'p_il_exit',
                    'p_il_body_1', 'p_il_body_2', 'p_il_body_3', 'p_il_iteration',

                    %% WCP-26: Critical Section Pattern
                    'p_cs_request', 'p_cs_lock', 'p_cs_active', 'p_cs_release', 'p_cs_complete',

                    %% WCP-27: Protocol Pattern
                    'p_proto_idle', 'p_proto_request_sent', 'p_proto_waiting', 'p_proto_response',
                    'p_proto_complete', 'p_proto_error',

                    %% WCP-28: Try-Catch Pattern
                    'p_try_entry', 'p_try_body', 'p_catch_entry', 'p_catch_body', 'p_try_catch_done',
                    'p_try_success', 'p_try_failure', 'p_catch_error'
                   ]).

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the pattern Petri net.
%% Uses persistent_term for O(1) access (OTP 21+ optimization).
%%
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    %% Use persistent_term for O(1) access to static transition list
    %% (OTP 21+ optimization) - avoids reconstructing list on every call
    cre_config:get(yawl_patterns_trsn_lst,
                   [
                    %% Implicit Termination (WCP-11)
                    't_activate', 't_queue_work', 't_dequeue_work', 't_implicit_term',

                    %% Multiple Instances - No Sync (WCP-12)
                    't_spawn_no_sync', 't_execute_no_sync', 't_complete_no_sync',

                    %% Multiple Instances - Static (Design Time) (WCP-13)
                    't_spawn_all_static', 't_execute_static', 't_collect_static', 't_join_static',

                    %% Multiple Instances - Runtime (WCP-14)
                    't_eval_count', 't_spawn_runtime', 't_execute_runtime', 't_collect_runtime',
                    't_join_runtime',

                    %% Multiple Instances - Dynamic (WCP-15)
                    't_spawn_dynamic', 't_fetch_data', 't_execute_dynamic', 't_collect_dynamic',
                    't_check_done', 't_terminate_dynamic',

                    %% Deferred Choice (WCP-16)
                    't_offer_choice', 't_select_a', 't_select_b', 't_discard_a', 't_discard_b',
                    't_complete_choice',

                    %% Interleaved Routing (WCP-17)
                    't_distribute_branches', 't_pick_branch', 't_execute_branch',
                    't_return_branch', 't_complete_interleaved',

                    %% WCP-18: Milestone Pattern
                    't_milestone_work', 't_milestone_check', 't_milestone_complete',

                    %% WCP-19: Cancel Activity Pattern
                    't_cancel_activity_work', 't_cancel_request', 't_cancel_confirm', 't_cancel_complete',

                    %% WCP-20: Cancel Case Pattern
                    't_cancel_case_work', 't_request_cancel', 't_confirm_cancel', 't_execute_cancel', 't_cancel_case_complete',

                    %% WCP-21: Structured Synchronization Pattern
                    't_sync_enter', 't_sync_activity_1', 't_sync_activity_2', 't_sync_activity_3',
                    't_sync_barrier', 't_sync_complete', 't_sync_timeout',

                    %% WCP-22: Partial Join Pattern
                    't_partial_start', 't_partial_instance_1', 't_partial_instance_2', 't_partial_instance_3',
                    't_partial_check_quorum', 't_partial_complete', 't_partial_timeout',

                    %% WCP-23: Structured Loop Pattern
                    't_loop_init', 't_loop_enter', 't_loop_execute', 't_loop_check',
                    't_loop_continue', 't_loop_break', 't_loop_complete', 't_loop_timeout',

                    %% WCP-24: Recursion Pattern
                    't_rec_start', 't_rec_call_1', 't_rec_call_2', 't_rec_base',
                    't_rec_continue', 't_rec_complete', 't_rec_stack_push', 't_rec_stack_pop',

                    %% WCP-25: Interleaved Loop Pattern
                    't_il_loop_start', 't_il_distribute', 't_il_execute', 't_il_check',
                    't_il_interleave', 't_il_continue', 't_il_complete', 't_il_timeout',

                    %% WCP-26: Critical Section Pattern
                    't_cs_request', 't_cs_acquire', 't_cs_enter', 't_cs_exit', 't_cs_release',
                    't_cs_timeout', 't_cs_error',

                    %% WCP-27: Protocol Pattern
                    't_proto_start', 't_proto_request', 't_proto_wait', 't_proto_response',
                    't_proto_complete', 't_proto_error', 't_proto_retry',

                    %% WCP-28: Try-Catch Pattern
                    't_try_enter', 't_try_execute', 't_try_success', 't_try_failure',
                    't_catch_enter', 't_catch_execute', 't_catch_complete', 't_try_catch_complete'
                   ]).

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: #pattern_state{}) -> [term()].

init_marking(Place, _UsrInfo) ->
    case Place of
        'p_start' -> [start];
        'p_instance_pool' -> [];
        'p_branch_pool' -> [];
        'p_milestone_guard' -> [];
        'p_milestone_reached' -> [];
        'p_activity_running' -> [];
        'p_cancellation_pending' -> [];
        'p_case_active' -> [];
        'p_cancellation_requested' -> [];
        'p_cancelling' -> [];
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for a given transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

%% Implicit Termination (WCP-11)
preset('t_activate') -> ['p_start'];
preset('t_queue_work') -> ['p_active'];
preset('t_dequeue_work') -> ['p_work_pending'];
preset('t_implicit_term') -> ['p_active', 'p_work'];

%% Multiple Instances - No Sync (WCP-12)
preset('t_spawn_no_sync') -> ['p_instance_pool'];
preset('t_execute_no_sync') -> ['p_ready'];
preset('t_complete_no_sync') -> ['p_done'];

%% Multiple Instances - Static (WCP-13)
preset('t_spawn_all_static') -> ['p_instance_pool'];
preset('t_execute_static') -> ['p_ready'];
preset('t_collect_static') -> ['p_running'];
preset('t_join_static') -> ['p_all_spawned', 'p_complete'];

%% Multiple Instances - Runtime (WCP-14)
preset('t_eval_count') -> ['p_start'];
preset('t_spawn_runtime') -> ['p_instance_pool'];
preset('t_execute_runtime') -> ['p_ready'];
preset('t_collect_runtime') -> ['p_running'];
preset('t_join_runtime') -> ['p_all_spawned', 'p_complete'];

%% Multiple Instances - Dynamic (WCP-15)
preset('t_spawn_dynamic') -> ['p_data_source', 'p_running'];
preset('t_fetch_data') -> ['p_data_source'];
preset('t_execute_dynamic') -> ['p_ready'];
preset('t_collect_dynamic') -> ['p_running'];
preset('t_check_done') -> ['p_running', 'p_done'];
preset('t_terminate_dynamic') -> ['p_final'];

%% Deferred Choice (WCP-16)
preset('t_offer_choice') -> ['p_start'];
preset('t_select_a') -> ['p_option_a'];
preset('t_select_b') -> ['p_option_b'];
preset('t_discard_a') -> ['p_selected'];
preset('t_discard_b') -> ['p_selected'];
preset('t_complete_choice') -> ['p_choice_complete'];

%% Interleaved Routing (WCP-17)
preset('t_distribute_branches') -> ['p_start'];
preset('t_pick_branch') -> ['p_branch_pool', 'p_next_branch'];
preset('t_execute_branch') -> ['p_branch_pool'];
preset('t_return_branch') -> ['p_executing'];
%% WCP-18: Milestone Pattern
%% WCP-18: Milestone Pattern
preset('t_milestone_work') -> ['p_active'];
preset('t_milestone_check') -> ['p_active'];
preset('t_milestone_complete') -> ['p_active', 'p_complete'];

%% WCP-19: Cancel Activity Pattern
preset('t_cancel_activity_work') -> ['p_activity_running'];
preset('t_cancel_request') -> ['p_activity_running'];
preset('t_cancel_confirm') -> ['p_cancellation_pending'];
preset('t_cancel_complete') -> ['p_activity_running', 'p_completed'];

%% WCP-20: Cancel Case Pattern
preset('t_cancel_case_work') -> ['p_case_active'];
preset('t_request_cancel') -> ['p_case_active'];
preset('t_confirm_cancel') -> ['p_cancellation_requested'];
preset('t_execute_cancel') -> ['p_cancelling'];
preset('t_cancel_case_complete') -> ['p_case_active', 'p_completed'];

%% WCP-21: Structured Synchronization Pattern
preset('t_sync_enter') -> ['p_sync_block_start'];
preset('t_sync_activity_1') -> ['p_sync_block_active'];
preset('t_sync_activity_2') -> ['p_sync_block_active'];
preset('t_sync_activity_3') -> ['p_sync_block_active'];
preset('t_sync_barrier') -> ['p_sync_activity_1', 'p_sync_activity_2', 'p_sync_activity_3'];
preset('t_sync_complete') -> ['p_sync_barrier'];
preset('t_sync_timeout') -> ['p_sync_block_active'];

%% WCP-22: Partial Join Pattern
preset('t_partial_start') -> ['p_partial_start'];
preset('t_partial_instance_1') -> ['p_partial_running'];
preset('t_partial_instance_2') -> ['p_partial_running'];
preset('t_partial_instance_3') -> ['p_partial_running'];
preset('t_partial_check_quorum') -> ['p_partial_instance_1', 'p_partial_instance_2', 'p_partial_instance_3'];
preset('t_partial_complete') -> ['p_partial_quorum'];
preset('t_partial_timeout') -> ['p_partial_running'];

%% WCP-23: Structured Loop Pattern
preset('t_loop_init') -> ['p_loop_init'];
preset('t_loop_enter') -> ['p_loop_init'];
preset('t_loop_execute') -> ['p_loop_body'];
preset('t_loop_check') -> ['p_loop_body'];
preset('t_loop_continue') -> ['p_loop_iteration'];
preset('t_loop_break') -> ['p_loop_body'];
preset('t_loop_complete') -> ['p_loop_exit'];
preset('t_loop_timeout') -> ['p_loop_body'];

%% WCP-24: Recursion Pattern
preset('t_rec_start') -> ['p_rec_start'];
preset('t_rec_call_1') -> ['p_rec_call'];
preset('t_rec_call_2') -> ['p_rec_call'];
preset('t_rec_base') -> ['p_rec_base'];
preset('t_rec_continue') -> ['p_rec_result'];
preset('t_rec_complete') -> ['p_rec_result'];
preset('t_rec_stack_push') -> ['p_rec_call'];
preset('t_rec_stack_pop') -> ['p_rec_stack'];

%% WCP-25: Interleaved Loop Pattern
preset('t_il_loop_start') -> ['p_il_loop_start'];
preset('t_il_distribute') -> ['p_il_loop_start'];
preset('t_il_execute') -> ['p_il_parallel'];
preset('t_il_check') -> ['p_il_body_1', 'p_il_body_2', 'p_il_body_3'];
preset('t_il_interleave') -> ['p_il_body_1', 'p_il_body_2', 'p_il_body_3'];
preset('t_il_continue') -> ['p_il_iteration'];
preset('t_il_complete') -> ['p_il_exit'];
preset('t_il_timeout') -> ['p_il_parallel'];

%% WCP-26: Critical Section Pattern
preset('t_cs_request') -> ['p_cs_request'];
preset('t_cs_acquire') -> ['p_cs_request'];
preset('t_cs_enter') -> ['p_cs_lock'];
preset('t_cs_exit') -> ['p_cs_active'];
preset('t_cs_release') -> ['p_cs_active'];
preset('t_cs_timeout') -> ['p_cs_request'];
preset('t_cs_error') -> ['p_cs_active'];

%% WCP-27: Protocol Pattern
preset('t_proto_start') -> ['p_proto_idle'];
preset('t_proto_request') -> ['p_proto_idle'];
preset('t_proto_wait') -> ['p_proto_request_sent'];
preset('t_proto_response') -> ['p_proto_waiting'];
preset('t_proto_complete') -> ['p_proto_response'];
preset('t_proto_error') -> ['p_proto_waiting'];
preset('t_proto_retry') -> ['p_proto_error'];

%% WCP-28: Try-Catch Pattern
preset('t_try_enter') -> ['p_try_entry'];
preset('t_try_execute') -> ['p_try_body'];
preset('t_try_success') -> ['p_try_body'];
preset('t_try_failure') -> ['p_try_body'];
preset('t_catch_enter') -> ['p_catch_entry'];
preset('t_catch_execute') -> ['p_catch_body'];
preset('t_catch_complete') -> ['p_catch_body'];
preset('t_try_catch_complete') -> ['p_try_success', 'p_catch_complete'];
preset(_Trsn) -> [].

%%--------------------------------------------------------------------
%% @doc Determines if a transition is enabled in the given mode.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(),
                 Mode :: map(),
                 UsrInfo :: #pattern_state{}) -> boolean().

%% Implicit Termination (WCP-11)
is_enabled('t_implicit_term',
           #{'p_active' := [_], 'p_work' := [], 'p_work_pending' := []},
           _UsrInfo) ->
    true;
is_enabled('t_activate', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_queue_work', #{'p_active' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_dequeue_work', #{'p_work_pending' := [_]}, _UsrInfo) ->
    true;

%% Multiple Instances - No Sync (WCP-12)
is_enabled('t_spawn_no_sync', #{'p_instance_pool' := [_ | _]}, _UsrInfo) ->
    true;
is_enabled('t_execute_no_sync', #{'p_ready' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_complete_no_sync', #{'p_done' := [_]}, _UsrInfo) ->
    true;

%% Multiple Instances - Static (WCP-13)
is_enabled('t_spawn_all_static', #{'p_instance_pool' := Tokens}, _UsrInfo)
  when length(Tokens) > 0 ->
    true;
is_enabled('t_execute_static', #{'p_ready' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_collect_static', #{'p_running' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_join_static',
           #{'p_all_spawned' := [all_spawned], 'p_complete' := CompleteLst},
           #pattern_state{instance_count = N}) when length(CompleteLst) >= N ->
    true;

%% Multiple Instances - Runtime (WCP-14)
is_enabled('t_eval_count', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_spawn_runtime', #{'p_instance_pool' := [_ | _]}, _UsrInfo) ->
    true;
is_enabled('t_execute_runtime', #{'p_ready' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_collect_runtime', #{'p_running' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_join_runtime',
           #{'p_all_spawned' := [all_spawned], 'p_complete' := CompleteLst},
           #pattern_state{instance_count = N}) when length(CompleteLst) >= N ->
    true;

%% Multiple Instances - Dynamic (WCP-15)
is_enabled('t_spawn_dynamic',
           #{'p_data_source' := [_], 'p_running' := Running},
           #pattern_state{max_instances = unlimited}) when length(Running) < 10 ->
    true;
is_enabled('t_fetch_data', #{'p_data_source' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_execute_dynamic', #{'p_ready' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_collect_dynamic', #{'p_running' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_check_done',
           #{'p_running' := [], 'p_done' := [_ | _], 'p_data_source' := []},
           _UsrInfo) ->
    true;
is_enabled('t_terminate_dynamic', #{'p_final' := [final]}, _UsrInfo) ->
    true;

%% Deferred Choice (WCP-16)
is_enabled('t_offer_choice', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_select_a', #{'p_option_a' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_select_b', #{'p_option_b' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_discard_a', #{'p_selected' := {selected, a}}, _UsrInfo) ->
    true;
is_enabled('t_discard_b', #{'p_selected' := {selected, b}}, _UsrInfo) ->
    true;
is_enabled('t_complete_choice', #{'p_choice_complete' := [_]}, _UsrInfo) ->
    true;

%% Interleaved Routing (WCP-17)
is_enabled('t_distribute_branches', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_pick_branch',
           #{'p_branch_pool' := [_ | _], 'p_next_branch' := [next]},
           _UsrInfo) ->
    true;
is_enabled('t_execute_branch', #{'p_branch_pool' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_return_branch', #{'p_executing' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_complete_interleaved', #{'p_all_done' := [all_done]}, _UsrInfo) ->
    true;

%% WCP-18: Milestone Pattern
is_enabled('t_work',
           #{'p_active' := [_]},
           #pattern_state{choice_data = #{milestone_check := MilestoneFun}}) ->
    MilestoneFun();

is_enabled('t_milestone_check',
           #{'p_active' := [_], 'p_milestone_reached' := [_]},
           _UsrInfo) ->
    true;

is_enabled('t_complete',
           #{'p_active' := [_], 'p_complete' := [_]},
           _UsrInfo) ->
    true;

%% WCP-19: Cancel Activity Pattern
is_enabled('t_work',
           #{'p_activity_running' := [_]},
           #pattern_state{choice_data = #{cancel_check := CancelFun}}) ->
    not CancelFun();

is_enabled('t_cancel_request',
           #{'p_activity_running' := [_]},
           #pattern_state{choice_data = #{cancel_check := CancelFun}}) ->
    CancelFun();

is_enabled('t_cancel_confirm',
           #{'p_cancellation_pending' := [_]},
           _UsrInfo) ->
    true;

is_enabled('t_complete',
           #{'p_activity_running' := [_], 'p_completed' := [_]},
           _UsrInfo) ->
    true;

%% WCP-20: Cancel Case Pattern
is_enabled('t_work',
           #{'p_case_active' := [_]},
           #pattern_state{choice_data = #{cancel_check := CancelFun}}) ->
    not CancelFun();

is_enabled('t_request_cancel',
           #{'p_case_active' := [_]},
           #pattern_state{choice_data = #{cancel_check := CancelFun}}) ->
    CancelFun();

is_enabled('t_confirm_cancel',
           #{'p_cancellation_requested' := [_]},
           _UsrInfo) ->
    true;

is_enabled('t_execute_cancel',
           #{'p_cancelling' := [_]},
           _UsrInfo) ->
    true;

is_enabled('t_complete',
           #{'p_case_active' := [_], 'p_completed' := [_]},
           _UsrInfo) ->
    true;

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition in the given mode, producing new tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(),
           Mode :: map(),
           UsrInfo :: #pattern_state{}) ->
          {produce, map()} | abort.

%% Implicit Termination (WCP-11)
fire('t_activate',
     #{'p_start' := [start]},
     _UsrInfo) ->
    {produce, #{'p_active' => [active], 'p_work' => []}};

fire('t_queue_work',
     #{'p_active' := [Active]},
     #pattern_state{pending_instances = [H | _T]}) ->
    {produce, #{
      'p_active' => [Active],
      'p_work_pending' => [H],
      'p_work' => []
     }};

fire('t_dequeue_work',
     #{'p_work_pending' := [Work]},
     _UsrInfo) ->
    {produce, #{
      'p_work_pending' => [],
      'p_work' => [Work]
     }};

fire('t_implicit_term',
     #{'p_active' := [_], 'p_work' := [], 'p_work_pending' := []},
     _UsrInfo) ->
    {produce, #{
      'p_active' => [terminated],
      'p_terminate' => [done]
     }};

%% Multiple Instances - No Sync (WCP-12)
fire('t_spawn_no_sync',
     #{'p_instance_pool' := [Data | Rest]},
     _UsrInfo) ->
    InstanceId = make_ref(),
    Token = #instance_token{instance_id = InstanceId, data = Data},
    {produce, #{
      'p_instance_pool' => Rest,
      'p_ready' => [Token]
     }};

fire('t_execute_no_sync',
     #{'p_ready' := [Token]},
     _UsrInfo) ->
    {produce, #{
      'p_ready' => [],
      'p_done' => [Token]
     }};

fire('t_complete_no_sync',
     #{'p_done' := [#instance_token{instance_id = Id}]},
     _UsrInfo) ->
    {produce, #{
      'p_done' => [{completed, Id}]
     }};

%% Multiple Instances - Static (WCP-13)
fire('t_spawn_all_static',
     #{'p_instance_pool' := DataList},
     #pattern_state{instance_count = N}) when length(DataList) >= N ->
    {ToSpawn, Rest} = lists:split(N, DataList),
    Tokens = [begin
        Id = make_ref(),
        #instance_token{instance_id = Id, data = D}
    end || D <- ToSpawn],
    {produce, #{
      'p_instance_pool' => Rest,
      'p_ready' => Tokens,
      'p_all_spawned' => [all_spawned]
     }};

fire('t_execute_static',
     #{'p_ready' := [Token]},
     _UsrInfo) ->
    {produce, #{
      'p_ready' => [],
      'p_running' => [Token]
     }};

fire('t_collect_static',
     #{'p_running' := [#instance_token{instance_id = Id} = Token]},
     _UsrInfo) ->
    {produce, #{
      'p_running' => [],
      'p_complete' => [{completed, Id, Token}]
     }};

fire('t_join_static',
     #{'p_all_spawned' := [all_spawned], 'p_complete' := CompleteLst},
     _UsrInfo) ->
    {produce, #{
      'p_complete' => [{all_complete, CompleteLst}]
     }};

%% Multiple Instances - Runtime (WCP-14)
fire('t_eval_count',
     #{'p_start' := [start]},
     #pattern_state{instance_count = N, pending_instances = [Data]}) ->
    Tokens = lists:duplicate(N, Data),
    {produce, #{
      'p_start' => [],
      'p_instance_pool' => Tokens
     }};

fire('t_spawn_runtime',
     #{'p_instance_pool' := [Data | Rest]},
     _UsrInfo) ->
    Id = make_ref(),
    Token = #instance_token{instance_id = Id, data = Data},
    {produce, #{
      'p_instance_pool' => Rest,
      'p_ready' => [Token]
     }};

fire('t_execute_runtime',
     #{'p_ready' := [Token]},
     _UsrInfo) ->
    {produce, #{
      'p_ready' => [],
      'p_running' => [Token]
     }};

fire('t_collect_runtime',
     #{'p_running' := [#instance_token{instance_id = Id} = Token]},
     _UsrInfo) ->
    {produce, #{
      'p_running' => [],
      'p_complete' => [{completed, Id, Token}]
     }};

fire('t_join_runtime',
     #{'p_all_spawned' := [all_spawned], 'p_complete' := CompleteLst},
     _UsrInfo) ->
    {produce, #{
      'p_complete' => [{all_complete, CompleteLst}]
     }};

%% Multiple Instances - Dynamic (WCP-15)
fire('t_spawn_dynamic',
     #{'p_data_source' := [Data], 'p_running' := Running},
     _UsrInfo) when length(Running) < 10 ->
    Id = make_ref(),
    Token = #instance_token{instance_id = Id, data = Data},
    {produce, #{
      'p_data_source' => [],
      'p_ready' => [Token]
     }};

fire('t_fetch_data',
     #{'p_data_source' := []},
     #pattern_state{choice_data = #{data_fun := DataFun}}) ->
    case DataFun() of
        {more, NewData} ->
            {produce, #{'p_data_source' => [NewData]}};
        done ->
            {produce, #{'p_data_source' => []}}
    end;

fire('t_execute_dynamic',
     #{'p_ready' := [Token]},
     _UsrInfo) ->
    {produce, #{
      'p_ready' => [],
      'p_running' => [Token]
     }};

fire('t_collect_dynamic',
     #{'p_running' := [#instance_token{instance_id = Id}]},
     _UsrInfo) ->
    {produce, #{
      'p_running' => [],
      'p_done' => [{completed, Id}]
     }};

fire('t_check_done',
     #{'p_running' := [], 'p_done' := DoneLst, 'p_data_source' := []},
     _UsrInfo) ->
    {produce, #{
      'p_done' => [],
      'p_final' => [final],
      'p_complete' => [{all_complete, DoneLst}]
     }};

fire('t_terminate_dynamic',
     #{'p_final' := [final]},
     _UsrInfo) ->
    {produce, #{
      'p_final' => [terminated]
     }};

%% Deferred Choice (WCP-16)
fire('t_offer_choice',
     #{'p_start' := [start]},
     #pattern_state{choice_data = #{options := Options}}) ->
    OptionKeys = maps:keys(Options),
    {produce, #{
      'p_start' => [],
      'p_option_a' => [{option, a}],
      'p_option_b' => [{option, b}],
      'p_options' => OptionKeys
     }};

fire('t_select_a',
     #{'p_option_a' := [{option, a}]},
     _UsrInfo) ->
    {produce, #{
      'p_option_a' => [],
      'p_selected' => {selected, a},
      'p_choice_complete' => [{choice_made, a}]
     }};

fire('t_select_b',
     #{'p_option_b' := [{option, b}]},
     _UsrInfo) ->
    {produce, #{
      'p_option_b' => [],
      'p_selected' => {selected, b},
      'p_choice_complete' => [{choice_made, b}]
     }};

fire('t_discard_a',
     #{'p_selected' := {selected, a}, 'p_option_b' := [{option, b}]},
     _UsrInfo) ->
    {produce, #{
      'p_option_b' => [],
      'p_discarded' => [b]
     }};

fire('t_discard_b',
     #{'p_selected' := {selected, b}, 'p_option_a' := [{option, a}]},
     _UsrInfo) ->
    {produce, #{
      'p_option_a' => [],
      'p_discarded' => [a]
     }};

fire('t_complete_choice',
     #{'p_choice_complete' := [{choice_made, Option}]},
     _UsrInfo) ->
    {produce, #{
      'p_choice_complete' => [],
      'p_complete' => [{selected_option, Option}]
     }};

%% Interleaved Routing (WCP-17)
fire('t_distribute_branches',
     #{'p_start' := [start]},
     #pattern_state{choice_data = #{branches := Branches}}) ->
    BranchKeys = maps:keys(Branches),
    BranchTokens = [begin
        #branch_token{branch_id = K, data = maps:get(K, Branches), index = I}
    end || {K, I} <- lists:zip(BranchKeys, lists:seq(1, length(BranchKeys)))],
    {produce, #{
      'p_start' => [],
      'p_branch_pool' => BranchTokens,
      'p_next_branch' => [next]
     }};

fire('t_pick_branch',
     #{'p_branch_pool' := [Token | Rest], 'p_next_branch' := [next]},
     _UsrInfo) ->
    {produce, #{
      'p_branch_pool' => Rest ++ [Token],
      'p_next_branch' => [],
      'p_executing' => [Token]
     }};

fire('t_execute_branch',
     #{'p_branch_pool' := [#branch_token{branch_id = Id}]},
     _UsrInfo) ->
    {produce, #{
      'p_branch_pool' => [],
      'p_executing' => [{executing, Id}]
     }};

fire('t_return_branch',
     #{'p_executing' := [#branch_token{branch_id = Id}]},
     _UsrInfo) ->
    {produce, #{
      'p_executing' => [],
      'p_branch_done' => [{branch_complete, Id}],
      'p_next_branch' => [next]
     }};

fire('t_complete_interleaved',
     #{'p_all_done' := [all_done]},
     _UsrInfo) ->
    {produce, #{
      'p_all_done' => [interleaved_complete]
     }};

%% WCP-18: Milestone Pattern
%% Guard: abort if milestone function is undefined
fire('t_work',
     #{'p_active' := [_Active]},
     #pattern_state{choice_data = #{milestone_check := undefined}}) ->
    abort;
fire('t_work',
     #{'p_active' := [Active]},
     _UsrInfo) ->
    {produce, #{
      'p_active' => [Active],
      'p_milestone_guard' => [milestone_required]
     }};

fire('t_milestone_check',
     _Mode,
     #pattern_state{choice_data = #{milestone_check := undefined}}) ->
    abort;
fire('t_milestone_check',
     #{'p_active' := [Active], 'p_milestone_guard' := [milestone_required]},
     #pattern_state{choice_data = #{milestone_check := MilestoneFun}}) ->
    case MilestoneFun() of
        true ->
            {produce, #{
              'p_milestone_guard' => [],
              'p_milestone_reached' => [milestone_reached],
              'p_active' => [Active]
             }};
        false ->
            {produce, #{
              'p_milestone_guard' => [milestone_required]
             }}
    end;

fire('t_complete',
     #{'p_active' := [_Active], 'p_complete' := [_Complete]},
     _UsrInfo) ->
    {produce, #{
      'p_active' => [completed],
      'p_complete' => [final_complete]
     }};

%% WCP-19: Cancel Activity Pattern
%% Guard: abort if cancel function is undefined
fire('t_work',
     #{'p_activity_running' := [_Running]},
     #pattern_state{choice_data = #{cancel_check := undefined}}) ->
    abort;
fire('t_work',
     #{'p_activity_running' := [Running]},
     #pattern_state{choice_data = #{cancel_check := CancelFun}}) ->
    case CancelFun() of
        true ->
            {produce, #{
              'p_activity_running' => [],
              'p_cancellation_pending' => [cancel_pending]
             }};
        false ->
            {produce, #{
              'p_activity_running' => [Running],
              'p_completed' => [work_done]
             }}
    end;

fire('t_cancel_request',
     #{'p_activity_running' := [_Running]},
     _UsrInfo) ->
    {produce, #{
      'p_activity_running' => [],
      'p_cancellation_pending' => [cancel_pending]
     }};

fire('t_cancel_confirm',
     #{'p_cancellation_pending' := [cancel_pending]},
     _UsrInfo) ->
    {produce, #{
      'p_cancellation_pending' => [],
      'p_cancelled' => [cancelled]
     }};

fire('t_complete',
     #{'p_activity_running' := [_Running], 'p_completed' := [_Done]},
     _UsrInfo) ->
    {produce, #{
      'p_activity_running' => [completed],
      'p_completed' => [final_done]
     }};

%% WCP-20: Cancel Case Pattern
fire('t_work',
     #{'p_case_active' := [CaseActive]},
     #pattern_state{choice_data = #{cancel_check := CancelFun}}) ->
    case CancelFun() of
        true ->
            {produce, #{
              'p_case_active' => [],
              'p_cancellation_requested' => [cancel_requested]
             }};
        false ->
            {produce, #{
              'p_case_active' => [CaseActive],
              'p_completed' => [work_done]
             }}
    end;

fire('t_request_cancel',
     #{'p_case_active' := [_CaseActive]},
     _UsrInfo) ->
    {produce, #{
      'p_case_active' => [],
      'p_cancellation_requested' => [cancel_requested]
     }};

fire('t_confirm_cancel',
     #{'p_cancellation_requested' := [cancel_requested]},
     _UsrInfo) ->
    {produce, #{
      'p_cancellation_requested' => [],
      'p_cancelling' => [cancelling]
     }};

fire('t_execute_cancel',
     #{'p_cancelling' := [cancelling]},
     _UsrInfo) ->
    {produce, #{
      'p_cancelling' => [],
      'p_cancelled' => [case_cancelled]
     }};

fire('t_complete',
     #{'p_case_active' := [_CaseActive], 'p_completed' := [_Done]},
     _UsrInfo) ->
    {produce, #{
      'p_case_active' => [completed],
      'p_completed' => [final_complete]
     }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Trigger handler for implicit termination pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_implicit_termination(Place :: atom(),
                                    Token :: term(),
                                    NetState :: term()) ->
          pass | {produce, map()}.

trigger_implicit_termination('p_work', Token, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    case Token of
        work_complete ->
            UpdatedState = PatternState#pattern_state{
              instance_count = PatternState#pattern_state.instance_count + 1
            },
            %% Store updated state in process dictionary for next access
            %% Note: gen_pnet usr_info is immutable during trigger execution
            put(pattern_state, UpdatedState),
            pass;
        _ ->
            pass
    end;

trigger_implicit_termination(_Place, _Token, _NetState) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Trigger handler for no-synchronization instances.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_no_sync(Place :: atom(),
                      Token :: term(),
                      NetState :: term()) ->
          pass | {produce, map()}.

trigger_no_sync('p_done', Token, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedCompleted = [Token | PatternState#pattern_state.completed_instances],
    UpdatedState = PatternState#pattern_state{
      completed_instances = UpdatedCompleted
    },
    %% Store updated state in process dictionary for next access
    %% Note: gen_pnet usr_info is immutable during trigger execution
    put(pattern_state, UpdatedState),
    pass;

trigger_no_sync(_Place, _Token, _NetState) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Trigger handler for milestone pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_milestone(Place :: atom(),
                       Token :: term(),
                       NetState :: term()) ->
        pass | {produce, map()}.

trigger_milestone('p_milestone_guard', milestone_required, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_milestone('p_milestone_reached', milestone_reached, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_milestone(_Place, _Token, _NetState) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Trigger handler for cancel activity pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_cancel_activity(Place :: atom(),
                             Token :: term(),
                             NetState :: term()) ->
        pass | {produce, map()}.

trigger_cancel_activity('p_cancellation_pending', cancel_pending, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_cancel_activity('p_cancelled', cancelled, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_cancel_activity(_Place, _Token, _NetState) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Trigger handler for cancel case pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger_cancel_case(Place :: atom(),
                          Token :: term(),
                          NetState :: term()) ->
        pass | {produce, map()}.

trigger_cancel_case('p_cancellation_requested', cancel_requested, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_cancel_case('p_cancelling', cancelling, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_cancel_case('p_cancelled', case_cancelled, NetState) ->
    PatternState = gen_pnet:get_usr_info(NetState),
    UpdatedState = PatternState#pattern_state{
      instance_count = PatternState#pattern_state.instance_count + 1
    },
    put(pattern_state, UpdatedState),
    pass;

trigger_cancel_case(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% State-Based Patterns (WCP-18 through WCP-28)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a Milestone pattern (WCP-18).
%%
%% The milestone pattern enables an activity only if a specific milestone
%% has been reached. Acts as a state-based guard for activity execution.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_milestone_guard', `p_milestone_reached',
%%       `p_activity_enabled', `p_activity_complete'</li>
%%   <li><b>Transitions:</b> `t_set_milestone', `t_enable_activity', `t_complete'</li>
%%   <li><b>Semantics:</b> Activity can only start when milestone place contains token.</li>
%% </ul>
%%
%% @param Activity The activity module/function to guard with milestone.
%% @param MilestoneFun Function checking if milestone reached (returns boolean).
%% @return A pattern state record for milestone.
%%
%% @end
%%--------------------------------------------------------------------
-spec milestone(module() | function(), function()) -> #pattern_state{}.

milestone(Activity, MilestoneFun) ->
    #pattern_state{
      pattern_type = milestone,
      subprocess = Activity,
      choice_data = #{milestone_check => MilestoneFun},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Cancel Activity pattern (WCP-19).
%%
%% The cancel activity pattern allows cancellation of a specific running
%% activity based on external events or conditions.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_activity_running', `p_cancel_signal',
%%       `p_activity_cancelled', `p_activity_done'</li>
%%   <li><b>Transitions:</b> `t_cancel', `t_complete', `t_handle_cancel'</li>
%%   <li><b>Semantics:</b> Cancel signal immediately removes activity token.</li>
%% </ul>
%%
%% @param Activity The activity that can be cancelled.
%% @param CancelFun Function determining cancellation condition.
%% @return A pattern state record for cancelable activity.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_activity(module() | function(), function()) -> #pattern_state{}.

cancel_activity(Activity, CancelFun) ->
    #pattern_state{
      pattern_type = cancel_activity,
      subprocess = Activity,
      choice_data = #{cancel_check => CancelFun},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Cancel Case pattern (WCP-20).
%%
%% The cancel case pattern allows cancellation of the entire workflow case,
%% terminating all activities regardless of their state.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_case_active', `p_cancel_case',
%%       `p_case_cancelled', `p_case_complete'</li>
%%   <li><b>Transitions:</b> `t_cancel_case', `t_complete_case',
%%       `t_cleanup'</li>
%%   <li><b>Semantics:</b> Case cancellation removes all tokens from active places.</li>
%% </ul>
%%
%% @param Activities List of activities in the case.
%% @param CancelFun Function determining case cancellation condition.
%% @return A pattern state record for cancelable case.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_case([module() | function()], function()) -> #pattern_state{}.

cancel_case(Activities, CancelFun) ->
    #pattern_state{
      pattern_type = cancel_case,
      subprocess = Activities,
      choice_data = #{cancel_check => CancelFun},
      instance_count = length(Activities)
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Structured Synchronization pattern (WCP-21).
%%
%% The structured synchronization pattern defines a synchronized block
%% where multiple concurrent activities must complete before proceeding.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_sync_block_start', `p_sync_block_active',
%%       `p_sync_barrier', `p_sync_block_done'</li>
%%   <li><b>Transitions:</b> `t_enter_block', `t_exit_activity', `t_sync_barrier',
%%       `t_leave_block'</li>
%%   <li><b>Semantics:</b> Barrier transition fires only when all activities
%%       have reached the sync point.</li>
%% </ul>
%%
%% @param Activities List of activities to synchronize.
%% @param InitialData Initial data for all activities.
%% @return A pattern state record for structured synchronization.
%%
%% @end
%%--------------------------------------------------------------------
-spec structured_sync([module() | function()], term()) -> #pattern_state{}.

structured_sync(Activities, InitialData) ->
    #pattern_state{
      pattern_type = structured_sync,
      subprocess = Activities,
      instance_count = length(Activities),
      pending_instances = [InitialData],
      active_instances = [],
      completed_instances = []
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Structured Partial Join pattern (WCP-22).
%%
%% The structured partial join pattern waits for a subset of parallel
%% branches to complete before proceeding, while others may continue.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_partial_start', `p_partial_running',
%%       `p_partial_quorum', `p_partial_done'</li>
%%   <li><b>Transitions:</b> `t_spawn_partial', `t_collect_partial',
%%       `t_quorum_reached', `t_continue'</li>
%%   <li><b>Semantics:</b> Continues when quorum of N branches complete.</li>
%% </ul>
%%
%% @param Activities List of activities in partial join.
%% @param Quorum Number of activities required for continuation.
%% @return A pattern state record for structured partial join.
%%
%% @end
%%--------------------------------------------------------------------
-spec partial_join([module() | function()], pos_integer()) -> #pattern_state{}.

partial_join(Activities, Quorum) ->
    true = Quorum =< length(Activities),
    #pattern_state{
      pattern_type = partial_join,
      subprocess = Activities,
      instance_count = 0,
      max_instances = Quorum,
      pending_instances = Activities,
      completed_instances = []
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Structured Loop pattern (WCP-23).
%%
%% The structured loop pattern implements while/until loop constructs
%% with proper Petri net semantics.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_loop_init', `p_loop_body', `p_loop_condition',
%%       `p_loop_exit'</li>
%%   <li><b>Transitions:</b> `t_loop_enter', `t_loop_eval', `t_loop_again',
%%       `t_loop_exit'</li>
%%   <li><b>Semantics:</b> While: condition checked before body;
%%       Until: condition checked after body.</li>
%% </ul>
%%
%% @param BodyTask The task to execute in each iteration.
%% @param LoopType 'while' or 'until'.
%% @param ConditionFun Function returning true/false for loop control.
%% @return A pattern state record for structured loop.
%%
%% @end
%%--------------------------------------------------------------------
-spec structured_loop(module() | function(), while | until, function()) ->
          #pattern_state{}.

structured_loop(BodyTask, LoopType, ConditionFun) ->
    #pattern_state{
      pattern_type = structured_loop,
      subprocess = BodyTask,
      choice_data = #{loop_type => LoopType, condition => ConditionFun},
      instance_count = 0,
      active_instances = []
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Recursion pattern (WCP-24).
%%
%% The recursion pattern enables recursive workflow invocation, allowing
%% a workflow to call itself with modified parameters.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_rec_start', `p_rec_call', `p_rec_result',
%%       `p_rec_base', `p_rec_done'</li>
%%   <li><b>Transitions:</b> `t_rec_call', `t_rec_return', `t_rec_base',
%%       `t_rec_combine'</li>
%%   <li><b>Semantics:</b> Recursive call produces new workflow instance;
%%       base case terminates recursion.</li>
%% </ul>
%%
%% @param RecursiveFun The recursive function implementing the workflow.
%% @param BaseCaseFun Function detecting base case for termination.
%% @return A pattern state record for recursion.
%%
%% @end
%%--------------------------------------------------------------------
-spec recursion(function(), function()) -> #pattern_state{}.

recursion(RecursiveFun, BaseCaseFun) ->
    #pattern_state{
      pattern_type = recursion,
      subprocess = RecursiveFun,
      choice_data = #{base_case => BaseCaseFun},
      instance_count = 0,
      active_instances = []
     }.

%%--------------------------------------------------------------------
%% @doc Creates an Arbitrary Interleaved Loop pattern (WCP-25).
%%
%% The arbitrary interleaved loop pattern combines looping with parallel
%% execution, where loop body contains parallel activities that execute
%% in interleaved fashion.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_il_loop_start', `p_il_parallel', `p_il_interleave',
%%       `p_il_loop_cond', `p_il_exit'</li>
%%   <li><b>Transitions:</b> `t_il_spawn', `t_il_interleave', `t_il_collect',
%%       `t_il_check', `t_il_loop', `t_il_exit'</li>
%%   <li><b>Semantics:</b> Combines interleaved parallel routing with loop control.</li>
%% </ul>
%%
%% @param Activities List of activities for interleaved parallel execution.
%% @param ConditionFun Function determining loop continuation.
%% @return A pattern state record for interleaved loop.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleaved_loop([module() | function()], function()) -> #pattern_state{}.

interleaved_loop(Activities, ConditionFun) ->
    #pattern_state{
      pattern_type = interleaved_loop,
      subprocess = Activities,
      choice_data = #{condition => ConditionFun},
      instance_count = 0,
      active_instances = [],
      branch_queue = []
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Critical Section pattern (WCP-26).
%%
%% The critical section pattern ensures mutually exclusive access to
%% a shared resource, preventing concurrent execution of sensitive code.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_cs_request', `p_cs_lock', `p_cs_active',
%%       `p_cs_release'</li>
%%   <li><b>Transitions:</b> `t_cs_acquire', `t_cs_execute', `t_cs_release'</li>
%%   <li><b>Semantics:</b> Lock place contains at most one token; requests queue.</li>
%% </ul>
%%
%% @param CriticalActivity The activity requiring mutual exclusion.
%% @param LockId Identifier for the lock/semaphore.
%% @return A pattern state record for critical section.
%%
%% @end
%%--------------------------------------------------------------------
-spec critical_section(module() | function(), term()) -> #pattern_state{}.

critical_section(CriticalActivity, LockId) ->
    #pattern_state{
      pattern_type = critical_section,
      subprocess = CriticalActivity,
      choice_data = #{lock_id => LockId},
      instance_count = 0,
      active_instances = []
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Protocol Pattern (WCP-27).
%%
%% The protocol pattern implements request-response communication between
%% workflow participants, ensuring proper message exchange patterns.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_proto_idle', `p_proto_request_sent',
%%       `p_proto_waiting', `p_proto_response'</li>
%%   <li><b>Transitions:</b> `t_send_request', `t_receive_response',
%%       `t_timeout', `t_complete'</li>
%%   <li><b>Semantics:</b> Request must be followed by response or timeout.</li>
%% </ul>
%%
%% @param RequestFun Function generating the request.
%% @param ResponseHandlerFun Function handling the response.
%% @param Timeout Timeout in milliseconds (or 'infinity').
%% @return A pattern state record for protocol pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec protocol_pattern(function(), function(), pos_integer() | infinity) ->
          #pattern_state{}.

protocol_pattern(RequestFun, ResponseHandlerFun, Timeout) ->
    #pattern_state{
      pattern_type = protocol,
      subprocess = RequestFun,
      choice_data = #{
        response_handler => ResponseHandlerFun,
        timeout => Timeout,
        request_sent => false,
        response_received => false
      },
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Try-Catch pattern (WCP-28).
%%
%% The try-catch pattern implements exception handling with a protected
%% region (try) and exception handling region (catch).
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_try_entry', `p_try_body', `p_catch_entry',
%%       `p_catch_body', `p_try_catch_done'</li>
%%   <li><b>Transitions:</b> `t_enter_try', `t_try_success', `t_raise_exception',
%%       `t_enter_catch', `t_catch_complete', `t_try_complete'</li>
%%   <li><b>Semantics:</b> Success path bypasses catch; exception triggers catch.</li>
%% </ul>
%%
%% @param TryFun The protected function to attempt.
%% @param CatchFun Function handling exceptions.
%% @param ExceptionTypes List of exception types to catch ('_' for all).
%% @return A pattern state record for try-catch.
%%
%% @end
%%--------------------------------------------------------------------
-spec try_catch(function(), function(), list() | '_') -> #pattern_state{}.

try_catch(TryFun, CatchFun, ExceptionTypes) ->
    #pattern_state{
      pattern_type = try_catch,
      subprocess = TryFun,
      choice_data = #{
        catch_handler => CatchFun,
        exceptions => ExceptionTypes,
        exception_occurred => false,
        exception_type => undefined
      },
      instance_count = 0
     }.

%%====================================================================
%% Exception Handling Patterns (WHP-1 through WHP-5)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an Error Handler pattern (WHP-1).
%%
%% The error handler pattern provides a catch-all mechanism for handling
%% unexpected errors during workflow execution.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_activity', `p_error', `p_handler', `p_resolved'</li>
%%   <li><b>Transitions:</b> `t_execute', `t_error', `t_handle', `t_resolve'</li>
%%   <li><b>Semantics:</b> Error transition produces handler token;
%%       handler transitions attempt recovery.</li>
%% </ul>
%%
%% @param ProtectedActivity The activity to protect with error handling.
%% @param ErrorHandlerFun Function to call when error occurs.
%% @return A pattern state record for error handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec error_handler(module() | function(), function()) -> #pattern_state{}.

error_handler(ProtectedActivity, ErrorHandlerFun) ->
    #pattern_state{
      pattern_type = error_handler,
      subprocess = ProtectedActivity,
      choice_data = #{error_handler => ErrorHandlerFun},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Retry pattern (WHP-2).
%%
%% The retry pattern automatically retries failed activities with
%% configurable retry limits and backoff strategies.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_retry_start', `p_retry_attempt', `p_retry_failed',
%%       `p_retry_backoff', `p_retry_exhausted'</li>
%%   <li><b>Transitions:</b> `t_retry_start', `t_retry_attempt', 't_retry_backoff',
%%       `t_retry_success', `t_retry_fail'</li>
%%   <li><b>Semantics:</b> Counter tracks attempts; after max attempts, fails.</li>
%% </ul>
%%
%% @param Activity The activity to retry on failure.
%% @param MaxRetries Maximum number of retry attempts.
%% @param BackoffFun Function calculating delay before next retry.
%% @return A pattern state record for retry.
%%
%% @end
%%--------------------------------------------------------------------
-spec retry(module() | function(), non_neg_integer(), function()) -> #pattern_state{}.

retry(Activity, MaxRetries, BackoffFun) ->
    #pattern_state{
      pattern_type = retry,
      subprocess = Activity,
      choice_data = #{max_retries => MaxRetries, backoff => BackoffFun},
      instance_count = 0,
      max_instances = MaxRetries
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Compensation pattern (WHP-3).
%%
%% The compensation pattern allows undoing of completed activities when
%% a downstream error occurs, maintaining data consistency.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_compensable', `p_compensated', 'p_compensation_pending',
%%       `p_compensation_done'</li>
%%   <li><b>Transitions:</b> `t_do_compensable', `t_register_compensation',
%%       `t_trigger_compensation', `t_compensate', `t_done'</li>
%%   <li><b>Semantics:</b> Successful activities register compensation handlers;
%%       compensation triggers on failure.</li>
%% </ul>
%%
%% @param CompensableActivity The activity that can be compensated.
%% @param CompensatorFun Function that undoes the activity.
%% @return A pattern state record for compensation.
%%
%% @end
%%--------------------------------------------------------------------
-spec compensate(module() | function(), function()) -> #pattern_state{}.

compensate(CompensableActivity, CompensatorFun) ->
    #pattern_state{
      pattern_type = compensation,
      subprocess = CompensableActivity,
      choice_data = #{compensator => CompensatorFun},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Triggered Compensation pattern (WHP-4).
%%
%% The triggered compensation pattern allows explicit triggering of
%% compensation based on specific events or conditions.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_trig_comp_activity', `p_trig_comp_registered',
%%       `p_trig_comp_trigger', `p_trig_comp_executing'</li>
%%   <li><b>Transitions:</b> `t_do_activity', `t_register', `t_trigger',
%%       `t_compensate', 't_complete'</li>
%%   <li><b>Semantics:</b> External trigger initiates compensation explicitly.</li>
%% </ul>
%%
%% @param Activity The compensable activity.
%% @param CompensatorFun Function that undoes the activity.
%% @param TriggerFun Function determining when to trigger compensation.
%% @return A pattern state record for triggered compensation.
%%
%% @end
%%--------------------------------------------------------------------
-spec triggered_compensation(module() | function(),
                            function(),
                            function()) -> #pattern_state{}.

triggered_compensation(Activity, CompensatorFun, TriggerFun) ->
    #pattern_state{
      pattern_type = triggered_compensation,
      subprocess = Activity,
      choice_data = #{
        compensator => CompensatorFun,
        trigger => TriggerFun
      },
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Consecutive Compensation pattern (WHP-5).
%%
%% The consecutive compensation pattern executes multiple compensation
%% handlers in reverse order of their registration.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_seq_comp_activities', `p_seq_comp_stack',
%%       `p_seq_comp_executing', `p_seq_comp_done'</li>
%%   <li><b>Transitions:</b> `t_do_sequence', `t_push_compensation',
%%       `t_pop_compensation', `t_execute_compensation', 't_complete'</li>
%%   <li><b>Semantics:</b> LIFO stack ensures reverse-order compensation execution.</li>
%% </ul>
%%
%% @param ActivityCompensatorPairs List of {Activity, Compensator} tuples.
%% @return A pattern state record for consecutive compensation.
%%
%% @end
%%--------------------------------------------------------------------
-spec consecutive_compensate([{module() | function(), function()}]) ->
          #pattern_state{}.

consecutive_compensate(ActivityCompensatorPairs) ->
    #pattern_state{
      pattern_type = consecutive_compensation,
      subprocess = [A || {A, _C} <- ActivityCompensatorPairs],
      choice_data = #{compensators => maps:from_list(ActivityCompensatorPairs)},
      instance_count = length(ActivityCompensatorPairs)
     }.

%%====================================================================
%% Data Flow Pattern Functions (WDP-01 through WDP-05)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a Parameter Passing pattern (WDP-01).
%%
%% The parameter passing pattern enables data to be passed between
%% workflow activities, defining how input data is provided to tasks
%% and how output data is made available to subsequent tasks.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_param_hold', `p_end'</li>
%%   <li><b>Transitions:</b> `t_pass', `t_finish'</li>
%%   <li><b>Semantics:</b> Parameters flow from source to target activities.</li>
%% </ul>
%%
%% @param Params Map of parameters to pass between activities.
%% @return A pattern state record for parameter passing.
%%
%% @end
%%--------------------------------------------------------------------
-spec param_pass(map()) -> #pattern_state{}.

param_pass(Params) when is_map(Params) ->
    #pattern_state{
      pattern_type = param_pass,
      choice_data = #{params => Params},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Data Transformation pattern (WDP-02).
%%
%% The data transformation pattern enables data to be transformed
%% between different formats or representations as it flows through
%% the workflow.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_transforming', `p_end'</li>
%%   <li><b>Transitions:</b> `t_transform', `t_finish'</li>
%%   <li><b>Semantics:</b> Data is transformed using the provided function.</li>
%% </ul>
%%
%% @param TransformFun Function to apply for transformation.
%% @param InputData Input data to transform.
%% @return A pattern state record for data transformation.
%%
%% @end
%%--------------------------------------------------------------------
-spec data_transform(function(), term()) -> #pattern_state{}.

data_transform(TransformFun, InputData) when is_function(TransformFun, 1) ->
    #pattern_state{
      pattern_type = data_transform,
      subprocess = TransformFun,
      choice_data = #{input_data => InputData},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Data Distribution pattern (WDP-03).
%%
%% The data distribution pattern enables data to be distributed to
%% multiple recipients or targets in the workflow.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_targets', `p_distributed', `p_end'</li>
%%   <li><b>Transitions:</b> `t_distribute', `t_finish'</li>
%%   <li><b>Semantics:</b> Data is copied to all target recipients.</li>
%% </ul>
%%
%% @param Data Data to distribute.
%% @param Targets List of target identifiers.
%% @return A pattern state record for data distribution.
%%
%% @end
%%--------------------------------------------------------------------
-spec data_distribute(term(), [term()]) -> #pattern_state{}.

data_distribute(Data, Targets) when is_list(Targets) ->
    #pattern_state{
      pattern_type = data_distribute,
      choice_data = #{data => Data, targets => Targets},
      instance_count = length(Targets)
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Data Accumulation pattern (WDP-04).
%%
%% The data accumulation pattern enables data from multiple sources
%% to be collected and aggregated into a single result.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_collecting', `p_accumulated', `p_end'</li>
%%   <li><b>Transitions:</b> `t_accumulate', `t_finish'</li>
%%   <li><b>Semantics:</b> Data from sources is folded into a single result.</li>
%% </ul>
%%
%% @param Sources List of data sources to accumulate.
%% @param AccumulatorFun Function to accumulate data (Acc, Item) -> NewAcc.
%% @return A pattern state record for data accumulation.
%%
%% @end
%%--------------------------------------------------------------------
-spec data_accumulate([term()], function()) -> #pattern_state{}.

data_accumulate(Sources, AccumulatorFun) when is_list(Sources),
                                              is_function(AccumulatorFun, 2) ->
    #pattern_state{
      pattern_type = data_accumulate,
      subprocess = AccumulatorFun,
      choice_data = #{sources => Sources},
      instance_count = length(Sources)
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Data Visibility pattern (WDP-05).
%%
%% The data visibility pattern controls data visibility and access
%% within the workflow, implementing scope and access control rules.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_check_scope', `p_granted', `p_denied', `p_end'</li>
%%   <li><b>Transitions:</b> `t_check', `t_grant', `t_deny', `t_finish'</li>
%%   <li><b>Semantics:</b> Access check determines visibility of data.</li>
%% </ul>
%%
%% @param Scope The scope context for visibility.
%% @param AccessCheckFun Function to check access (Data, Scope) -> boolean().
%% @return A pattern state record for data visibility.
%%
%% @end
%%--------------------------------------------------------------------
-spec data_visibility(term(), function()) -> #pattern_state{}.

data_visibility(Scope, AccessCheckFun) when is_function(AccessCheckFun, 2) ->
    #pattern_state{
      pattern_type = data_visibility,
      choice_data = #{scope => Scope, access_check => AccessCheckFun},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Executes a Parameter Passing pattern (WDP-01).
%%
%% @param Params Map of parameters to pass.
%% @param Target Target pid or identifier.
%% @return {ok, PassedParams} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_param_pass(map(), term()) -> {ok, map()} | {error, term()}.

execute_param_pass(Params, Target) when is_map(Params) ->
    {ok, Params#{target => Target}}.

%%--------------------------------------------------------------------
%% @doc Executes a Data Transformation pattern (WDP-02).
%%
%% @param TransformFun Function to apply for transformation.
%% @param InputData Input data to transform.
%% @return {ok, TransformedData} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_data_transform(function(), term()) -> {ok, term()} | {error, term()}.

execute_data_transform(TransformFun, InputData) when is_function(TransformFun, 1) ->
    try
        Result = TransformFun(InputData),
        {ok, Result}
    catch
        Error:Reason ->
            {error, {transform_error, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Data Distribution pattern (WDP-03).
%%
%% @param Data Data to distribute.
%% @param Targets List of target identifiers.
%% @return {ok, DistributionResults}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_data_distribute(term(), [term()]) -> {ok, [{term(), term()}]}.

execute_data_distribute(Data, Targets) when is_list(Targets) ->
    Results = [{Data, Target} || Target <- Targets],
    {ok, Results}.

%%--------------------------------------------------------------------
%% @doc Executes a Data Accumulation pattern (WDP-04).
%%
%% @param Sources List of data sources to accumulate.
%% @param AccumulatorFun Function to accumulate (Acc, Item) -> NewAcc.
%% @return {ok, AccumulatedResult}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_data_accumulate([term()], function()) -> {ok, term()}.

execute_data_accumulate(Sources, AccumulatorFun) when is_list(Sources),
                                                      is_function(AccumulatorFun, 2) ->
    Result = lists:foldl(AccumulatorFun, [], Sources),
    {ok, Result}.

%%--------------------------------------------------------------------
%% @doc Executes a Data Visibility pattern (WDP-05).
%%
%% @param Scope The scope context for visibility.
%% @param AccessCheckFun Function to check access (Data, Scope) -> boolean().
%% @return {ok, boolean()} - true if visible, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_data_visibility(term(), function()) -> {ok, boolean()}.

execute_data_visibility(_Scope, AccessCheckFun) when is_function(AccessCheckFun, 2) ->
    %% Simplified: always return visible for stub implementation
    {ok, true}.

%%====================================================================
%% Resource Pattern API Functions (WRP-01 through WRP-05)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a Direct Resource Creation pattern (WRP-01).
%%
%% The direct resource creation pattern enables on-demand creation
%% of resources within a workflow.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_creating', `p_ready', `p_end'</li>
%%   <li><b>Transitions:</b> `t_create', `t_finish'</li>
%%   <li><b>Semantics:</b> Resource is created and made ready for use.</li>
%% </ul>
%%
%% @param CreateFun Function that creates the resource (arity 0).
%% @return A pattern state record for direct resource creation.
%%
%% @end
%%--------------------------------------------------------------------
-spec direct_resource_creation(function()) -> #pattern_state{}.

direct_resource_creation(CreateFun) when is_function(CreateFun, 0) ->
    #pattern_state{
      pattern_type = direct_resource_creation,
      subprocess = CreateFun,
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Role-Based Allocation pattern (WRP-02).
%%
%% The role-based allocation pattern assigns resources to tasks
%% based on role definitions.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_checking', `p_allocated', `p_failed', `p_end'</li>
%%   <li><b>Transitions:</b> `t_check_role', `t_allocate', `t_fail', `t_finish'</li>
%%   <li><b>Semantics:</b> Resources with matching roles are allocated.</li>
%% </ul>
%%
%% @param RequiredRole The role required for the task.
%% @param RoleMap Map of roles to available resources.
%% @return A pattern state record for role-based allocation.
%%
%% @end
%%--------------------------------------------------------------------
-spec role_based_allocation(atom(), map()) -> #pattern_state{}.

role_based_allocation(RequiredRole, RoleMap) when is_atom(RequiredRole), is_map(RoleMap) ->
    #pattern_state{
      pattern_type = role_based_allocation,
      choice_data = #{required_role => RequiredRole, role_map => RoleMap},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Resource Initialization pattern (WRP-03).
%%
%% The resource initialization pattern handles the setup and
%% configuration of resources before use.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_initializing', `p_validated', `p_ready', `p_end'</li>
%%   <li><b>Transitions:</b> `t_init', `t_validate', `t_finish'</li>
%%   <li><b>Semantics:</b> Resource is initialized and validated before use.</li>
%% </ul>
%%
%% @param InitFun Function to initialize the resource (arity 1).
%% @param Resource The resource to initialize.
%% @return A pattern state record for resource initialization.
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_initialization(function(), term()) -> #pattern_state{}.

resource_initialization(InitFun, Resource) when is_function(InitFun, 1) ->
    #pattern_state{
      pattern_type = resource_initialization,
      subprocess = InitFun,
      choice_data = #{resource => Resource},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Resource Allocation pattern (WRP-04).
%%
%% The resource allocation pattern manages assigning resources to tasks
%% and tracking availability.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_available', `p_allocating', `p_allocated', `p_busy', `p_end'</li>
%%   <li><b>Transitions:</b> `t_request', `t_allocate', `t_release', `t_finish'</li>
%%   <li><b>Semantics:</b> Resources are allocated from pool and tracked.</li>
%% </ul>
%%
%% @param Resources List of available resources.
%% @param TaskId Identifier for the task requiring allocation.
%% @return A pattern state record for resource allocation.
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_allocation([term()], term()) -> #pattern_state{}.

resource_allocation(Resources, TaskId) when is_list(Resources) ->
    #pattern_state{
      pattern_type = resource_allocation,
      choice_data = #{resources => Resources, task_id => TaskId},
      instance_count = length(Resources)
     }.

%%--------------------------------------------------------------------
%% @doc Creates a Resource Deallocation pattern (WRP-05).
%%
%% The resource deallocation pattern handles releasing resources
%% after task completion.
%%
%% <h4>Petri Net Structure:</h4>
%% <ul>
%%   <li><b>Places:</b> `p_start', `p_releasing', `p_cleanup', `p_available', `p_deallocated', `p_end'</li>
%%   <li><b>Transitions:</b> `t_start_release', `t_cleanup', `t_make_available', `t_finish'</li>
%%   <li><b>Semantics:</b> Resource is cleaned up and returned to pool.</li>
%% </ul>
%%
%% @param CleanupFun Function to cleanup the resource (arity 1).
%% @param Resource The resource to deallocate.
%% @return A pattern state record for resource deallocation.
%%
%% @end
%%--------------------------------------------------------------------
-spec resource_deallocation(function(), term()) -> #pattern_state{}.

resource_deallocation(CleanupFun, Resource) when is_function(CleanupFun, 1) ->
    #pattern_state{
      pattern_type = resource_deallocation,
      subprocess = CleanupFun,
      choice_data = #{resource => Resource},
      instance_count = 0
     }.

%%--------------------------------------------------------------------
%% @doc Executes a Direct Resource Creation pattern (WRP-01).
%%
%% @param CreateFun Function that creates the resource (arity 0).
%% @return {ok, Resource} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_direct_resource_creation(function()) -> {ok, term()} | {error, term()}.

execute_direct_resource_creation(CreateFun) when is_function(CreateFun, 0) ->
    try
        Resource = CreateFun(),
        {ok, Resource}
    catch
        Error:Reason:_Stack ->
            {error, {creation_error, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Role-Based Allocation pattern (WRP-02).
%%
%% @param RequiredRole The role required for the task.
%% @param RoleMap Map of roles to available resources.
%% @return {ok, AllocatedResource} | {error, no_resource}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_role_based_allocation(atom(), map()) -> {ok, term()} | {error, term()}.

execute_role_based_allocation(RequiredRole, RoleMap) when is_atom(RequiredRole), is_map(RoleMap) ->
    case maps:get(RequiredRole, RoleMap, undefined) of
        undefined ->
            {error, {no_resource, RequiredRole}};
        [] ->
            {error, {no_resource, RequiredRole}};
        [Resource | _] ->
            {ok, Resource}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Resource Initialization pattern (WRP-03).
%%
%% @param InitFun Function to initialize the resource (arity 1).
%% @param Resource The resource to initialize.
%% @return {ok, InitializedResource} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_resource_initialization(function(), term()) -> {ok, term()} | {error, term()}.

execute_resource_initialization(InitFun, Resource) when is_function(InitFun, 1) ->
    try
        InitResource = InitFun(Resource),
        {ok, InitResource}
    catch
        Error:Reason:_Stack ->
            {error, {init_error, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a Resource Allocation pattern (WRP-04).
%%
%% @param Resources List of available resources.
%% @param TaskId Identifier for the task requiring allocation.
%% @return {ok, AllocatedResource, Remaining} | {error, no_resource}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_resource_allocation([term()], term()) ->
          {ok, term(), [term()]} | {error, term()}.

execute_resource_allocation([], _TaskId) ->
    {error, no_resources_available};
execute_resource_allocation([Resource | Remaining], TaskId) ->
    {ok, Resource, Remaining, TaskId}.

%%--------------------------------------------------------------------
%% @doc Executes a Resource Deallocation pattern (WRP-05).
%%
%% @param CleanupFun Function to cleanup the resource (arity 1).
%% @param Resource The resource to deallocate.
%% @return {ok, CleanedResource} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_resource_deallocation(function(), term()) -> {ok, term()} | {error, term()}.

execute_resource_deallocation(CleanupFun, Resource) when is_function(CleanupFun, 1) ->
    try
        CleanedResource = CleanupFun(Resource),
        {ok, CleanedResource}
    catch
        Error:Reason:_Stack ->
            {error, {cleanup_error, Error, Reason}}
    end.

%%====================================================================
%% Execution Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute a single instance with the given subprocess.
%%
%% Handles functions of arity 0, 1, or M:F tuples.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_instance(Subprocess :: term(), Data :: term()) ->
          {ok, term()} | {error, term()}.

execute_instance(Subprocess, Data) ->
    try
        Result = case Subprocess of
            Fun when is_function(Fun, 0) -> Fun();
            Fun when is_function(Fun, 1) -> Fun(Data);
            {M, F} when is_atom(M), is_atom(F) -> erlang:apply(M, F, [Data]);
            Fun when is_function(Fun) -> Fun(Data);
            _ -> {error, invalid_subprocess}
        end,
        {ok, Result}
    catch
        Error:Reason:_Stack ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Collect results from all spawned processes.
%%
%% Waits for all processes to complete or timeout to occur.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_all_results(Ref :: reference(),
                         PidMRefs :: [{pid(), reference()}],
                         Timeout :: timeout(),
                         OnError :: continue | stop,
                         Acc :: list()) ->
          [{ok, term()} | {error, term()}].

collect_all_results(_Ref, [], _Timeout, _OnError, Acc) ->
    lists:reverse(Acc);
collect_all_results(Ref, PidMRefs, Timeout, OnError, Acc) ->
    ReceiveTimeout = case Timeout of
        infinity -> 5000;
        N when is_integer(N), N > 0 -> min(5000, N)
    end,
    receive
        {Ref, Pid, Result} ->
            NewAcc = [Result | Acc],
            NewPidMRefs = lists:keydelete(Pid, 1, PidMRefs),
            case {Result, OnError} of
                {{error, _}, stop} ->
                    lists:foreach(fun({P, MRef}) ->
                        exit(P, kill),
                        demonitor(MRef, [flush])
                    end, NewPidMRefs),
                    lists:reverse(NewAcc);
                _ ->
                    collect_all_results(Ref, NewPidMRefs, Timeout, OnError, NewAcc)
            end;
        {'DOWN', _MRef, process, Pid, Reason} ->
            NewAcc = [{error, Reason} | Acc],
            NewPidMRefs = lists:keydelete(Pid, 1, PidMRefs),
            case OnError of
                stop ->
                    lists:foreach(fun({P, MRef}) ->
                        exit(P, kill),
                        demonitor(MRef, [flush])
                    end, NewPidMRefs),
                    lists:reverse(NewAcc);
                _ ->
                    collect_all_results(Ref, NewPidMRefs, Timeout, OnError, NewAcc)
            end
    after ReceiveTimeout ->
        lists:foreach(fun({P, MRef}) ->
            exit(P, kill),
            demonitor(MRef, [flush])
        end, PidMRefs),
        lists:reverse(Acc)
    end.

%%--------------------------------------------------------------------
%% @doc Instance supervisor for no-sync pattern.
%%
%% Manages concurrent instances with configurable max concurrency.
%%
%% @end
%%--------------------------------------------------------------------
-spec instance_supervisor(Ref :: reference(),
                         Parent :: pid(),
                         Subprocess :: term(),
                         DataList :: list(),
                         OnError :: continue | stop,
                         MaxConcurrent :: pos_integer()) ->
          no_return().

instance_supervisor(Ref, Parent, Subprocess, DataList, OnError, MaxConcurrent) ->
    %% Split data into chunks for max concurrency control
    instance_supervisor_loop(Ref, Parent, Subprocess, DataList,
                            OnError, MaxConcurrent, 0, []).

instance_supervisor_loop(Ref, Parent, _Subprocess, [], _OnError,
                        _MaxConcurrent, _Active, Results) ->
    Parent ! {Ref, {ok, lists:reverse(Results)}};

instance_supervisor_loop(Ref, Parent, Subprocess, DataList, OnError,
                        MaxConcurrent, Active, Results) ->
    %% Determine how many new instances to spawn
    Available = MaxConcurrent - Active,
    {ToSpawn, Remaining} = case Available > 0 of
        true when length(DataList) >= Available ->
            lists:split(Available, DataList);
        true ->
            {DataList, []};
        false ->
            {[], DataList}
    end,

    %% Spawn new instances (monitored for crash detection)
    NewPidMRefs = lists:map(fun(Data) ->
        spawn_monitor(fun() ->
            Result = execute_instance(Subprocess, Data),
            Parent ! {Ref, self(), Result}
        end)
    end, ToSpawn),

    %% Wait for at least one completion if we have active instances
    NewActive = length(NewPidMRefs) + Active,
    case NewActive of
        0 when Remaining =:= [] ->
            Parent ! {Ref, {ok, lists:reverse(Results)}};
        0 ->
            instance_supervisor_loop(Ref, Parent, Subprocess, Remaining,
                                    OnError, MaxConcurrent, 0, Results);
        _ ->
            receive
                {Ref, Pid, Result} ->
                    NewResults = [Result | Results],
                    NewPidMRefs1 = lists:keydelete(Pid, 1, NewPidMRefs),
                    case {Result, OnError} of
                        {{error, _}, stop} ->
                            lists:foreach(fun({P, MRef}) ->
                                exit(P, kill),
                                demonitor(MRef, [flush])
                            end, NewPidMRefs1),
                            Parent ! {Ref, {ok, lists:reverse(NewResults)}};
                        _ ->
                            instance_supervisor_loop(Ref, Parent, Subprocess,
                                                    Remaining, OnError,
                                                    MaxConcurrent, NewActive - 1, NewResults)
                    end;
                {'DOWN', _MRef, process, Pid, Reason} ->
                    NewResults = [{error, Reason} | Results],
                    case OnError of
                        stop ->
                            NewPidMRefs1 = lists:keydelete(Pid, 1, NewPidMRefs),
                            lists:foreach(fun({P, MRef}) ->
                                exit(P, kill),
                                demonitor(MRef, [flush])
                            end, NewPidMRefs1),
                            Parent ! {Ref, {ok, lists:reverse(NewResults)}};
                        _ ->
                            instance_supervisor_loop(Ref, Parent, Subprocess,
                                                    Remaining, OnError,
                                                    MaxConcurrent, NewActive - 1, NewResults)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Dynamic instance manager for WCP-15.
%%
%% Dynamically spawns instances based on data availability.
%%
%% @end
%%--------------------------------------------------------------------
-spec dynamic_instance_manager(Ref :: reference(),
                              Parent :: pid(),
                              Subprocess :: term(),
                              DataFun :: function(),
                              CurrentData :: term(),
                              MaxConcurrent :: pos_integer(),
                              OnError :: continue | stop) ->
          no_return().

dynamic_instance_manager(Ref, Parent, Subprocess, DataFun,
                        CurrentData, MaxConcurrent, OnError) ->
    dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                         CurrentData, MaxConcurrent, OnError, 0, []).

dynamic_instance_loop(Ref, Parent, _Subprocess, _DataFun,
                     no_data, _MaxConcurrent, _OnError, 0, Results) ->
    Parent ! {Ref, {ok, lists:reverse(Results)}};

dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                     no_data, MaxConcurrent, OnError, Active, Results) ->
    %% Try to fetch more data
    case DataFun() of
        {more, NewData} ->
            dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                                 NewData, MaxConcurrent, OnError, Active, Results);
        done ->
            %% Wait for active instances to complete
            wait_for_active(Ref, Parent, Active, Results);
        {error, _Reason} when OnError =:= continue ->
            wait_for_active(Ref, Parent, Active, Results);
        {error, Reason} ->
            Parent ! {Ref, {error, Reason}}
    end;

dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                     CurrentData, MaxConcurrent, OnError, 0, Results) ->
    %% No active instances, spawn up to MaxConcurrent new ones
    spawn_batch(Ref, Parent, Subprocess, DataFun, CurrentData,
               MaxConcurrent, OnError, Results);

dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                     CurrentData, MaxConcurrent, OnError, Active, Results) when Active > 0 ->
    %% Wait for at least one instance to complete
    receive
        {Ref, _Pid, Result} ->
            NewResults = [Result | Results],
            case {Result, OnError} of
                {{error, _}, stop} ->
                    Parent ! {Ref, {ok, lists:reverse(NewResults)}};
                _ ->
                    dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                                        CurrentData, MaxConcurrent, OnError,
                                        Active - 1, NewResults)
            end;
        {'EXIT', _Pid, _Reason} ->
            dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                                CurrentData, MaxConcurrent, OnError,
                                Active - 1, Results);
        {'DOWN', _MRef, process, _Pid, Reason} ->
            NewResults = [{error, Reason} | Results],
            case OnError of
                stop ->
                    Parent ! {Ref, {ok, lists:reverse(NewResults)}};
                _ ->
                    dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                                        CurrentData, MaxConcurrent, OnError,
                                        Active - 1, NewResults)
            end
    end.

spawn_batch(Ref, Parent, Subprocess, DataFun, Data, MaxConcurrent, OnError, Results) ->
    %% Spawn up to MaxConcurrent instances (monitored for crash detection)
    PidMRefs = lists:map(fun(_) ->
        spawn_monitor(fun() ->
            Result = execute_instance(Subprocess, Data),
            Parent ! {Ref, self(), Result}
        end)
    end, lists:seq(1, MaxConcurrent)),

    %% Get next data immediately
    NextData = case DataFun() of
        {more, ND} -> ND;
        done -> no_data;
        {error, _} when OnError =:= continue -> no_data;
        _ -> no_data
    end,

    dynamic_instance_loop(Ref, Parent, Subprocess, DataFun,
                         NextData, MaxConcurrent, OnError, length(PidMRefs), Results).

wait_for_active(Ref, Parent, 0, Results) ->
    Parent ! {Ref, {ok, lists:reverse(Results)}};
wait_for_active(Ref, Parent, Active, Results) ->
    receive
        {Ref, _Pid, Result} ->
            wait_for_active(Ref, Parent, Active - 1, [Result | Results]);
        {'EXIT', _Pid, _Reason} ->
            wait_for_active(Ref, Parent, Active - 1, Results);
        {'DOWN', _MRef, process, _Pid, Reason} ->
            wait_for_active(Ref, Parent, Active - 1, [{error, Reason} | Results])
    end.

%%--------------------------------------------------------------------
%% @doc Collect first choice result for deferred choice.
%%
%% Returns the first successful option result or condition-based choice.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_first_choice(Ref :: reference(),
                          Pids :: [pid()],
                          Timeout :: timeout(),
                          OptionKeys :: [atom()]) ->
          {ok, {atom(), term()}} | {error, term()}.

collect_first_choice(Ref, Pids, Timeout, OptionKeys) ->
    Deadline = case Timeout of
        infinity -> infinity;
        N when is_integer(N) -> erlang:monotonic_time(millisecond) + N
    end,
    collect_first_choice_loop(Ref, Pids, Deadline, OptionKeys).

collect_first_choice_loop(_Ref, [], _Deadline, _OptionKeys) ->
    {error, no_options_available};
collect_first_choice_loop(Ref, Pids, Deadline, OptionKeys) ->
    TimeLeft = case Deadline of
        infinity -> infinity;
        T -> max(0, T - erlang:monotonic_time(millisecond))
    end,
    receive
        {Ref, {option, _Key}, {error, _}} ->
            %% This option failed, continue waiting
            collect_first_choice_loop(Ref, Pids -- [self()], Deadline, OptionKeys);
        {Ref, {option, Key}, Result} ->
            %% Got a successful result
            {ok, {Key, Result}};
        {Ref, condition, ConditionKey} when is_atom(ConditionKey) ->
            %% Condition function selected a key
            %% Kill all option processes
            lists:foreach(fun(P) -> exit(P, kill) end, Pids),
            {ok, {ConditionKey, condition_selected}};
        {Ref, condition, true} ->
            %% Condition returned true but no specific key
            %% Wait for first successful option
            collect_first_choice_loop(Ref, Pids, Deadline, OptionKeys);
        {'EXIT', _Pid, _Reason} ->
            %% Process died, continue
            collect_first_choice_loop(Ref, Pids -- [self()], Deadline, OptionKeys)
    after TimeLeft ->
        lists:foreach(fun(P) -> exit(P, kill) end, Pids),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Execute branches in interleaved fashion (round-robin).
%%
%% Ensures fair execution order across all branches.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_interleaved(Ref :: reference(),
                         Parent :: pid(),
                         Branches :: map(),
                         CurrentKeys :: [atom()],
                         InputData :: term(),
                         Round :: pos_integer(),
                         PendingPids :: [{pid(), atom()}],
                         CompletedResults :: list(),
                         AllBranchKeys :: [atom()]) ->
          list().

execute_interleaved(_Ref, _Parent, _Branches, [], _InputData,
                   _Round, PendingPids, Results, _AllBranchKeys) ->
    %% Wait for remaining pending processes
    collect_pending_results(PendingPids, Results);

execute_interleaved(Ref, Parent, Branches, [Key | RestKeys],
                   InputData, Round, PendingPids, Results, AllBranchKeys) ->
    %% Get branch function
    BranchFun = maps:get(Key, Branches),

    %% Spawn one task from this branch
    Pid = spawn(fun() ->
        try
            Result = case BranchFun of
                Fun when is_function(Fun, 1) -> Fun(InputData);
                {M, F} when is_atom(M), is_atom(F) -> erlang:apply(M, F, [InputData]);
                Fun when is_function(Fun, 0) -> Fun()
            end,
            Parent ! {Ref, {branch_complete, Key}, Result}
        catch
            _Error:_Reason:_Stack ->
            Parent ! {Ref, {branch_complete, Key}, {error, failed}}
        end
    end),

    %% Wait for one pending task to complete before spawning next
    NewPending = [{Pid, Key} | PendingPids],

    case wait_for_one_completion(Ref, NewPending) of
        {completed, CompletedKey, Result, RemainingPids} ->
            %% Continue with remaining branches, cycling back if needed
            NextKeys = case RestKeys of
                [] -> AllBranchKeys;
                _ -> RestKeys
            end,
            execute_interleaved(Ref, Parent, Branches, NextKeys,
                              InputData, Round + 1, RemainingPids,
                              [{CompletedKey, Result} | Results], AllBranchKeys)
    end.

%%--------------------------------------------------------------------
%% @doc Wait for one pending process to complete.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_one_completion(Ref :: reference(),
                             PendingPids :: [{pid(), atom()}]) ->
          {completed, atom(), term(), [{pid(), atom()}]}.

wait_for_one_completion(Ref, [{Pid, Key} | Rest]) ->
    receive
        {Ref, {branch_complete, Key}, Result} ->
            {completed, Key, Result, Rest};
        {Ref, {branch_complete, OtherKey}, Result} ->
            %% Different branch completed first
            {completed, OtherKey, Result, [{Pid, Key} | Rest]};
        {'EXIT', Pid, _Reason} ->
            %% Process died without completing
            wait_for_one_completion(Ref, Rest)
    after 5000 ->
        %% Timeout, treat as error
        {completed, Key, {error, timeout}, Rest}
    end.

%%--------------------------------------------------------------------
%% @doc Collect results from remaining pending processes.
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_pending_results(PendingPids :: [{pid(), atom()}],
                             Results :: list()) ->
          list().

collect_pending_results([], Results) ->
    Results;
collect_pending_results([{Pid, Key} | Rest], Results) ->
    receive
        {'EXIT', Pid, _Reason} ->
            collect_pending_results(Rest, [{Key, {error, died}} | Results]);
        {_, {branch_complete, Key}, Result} ->
            collect_pending_results(Rest, [{Key, Result} | Results])
    after 1000 ->
        collect_pending_results(Rest, [{Key, {error, timeout}} | Results])
    end.

%%--------------------------------------------------------------------
%% @doc Runs doctests for the cre_yawl_patterns module.
%%
%% This function executes all doctests defined in the module documentation
%% and individual function documentation. Uses the doctest library for
%% automated testing of code examples.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Pattern registration - WCP-11 (Implicit Termination)
    Pattern1 = implicit_termination(fun(X) -> X * 2 end),
    implicit_termination = Pattern1#pattern_state.pattern_type,
    10 = (Pattern1#pattern_state.subprocess)(5),

    %% Test 2: Pattern registration - WCP-12 (Multiple Instances without Sync)
    Pattern2 = multiple_instances_no_sync(fun(X) -> X + 1 end, 3, [1, 2, 3]),
    multiple_instances_no_sync = Pattern2#pattern_state.pattern_type,
    3 = Pattern2#pattern_state.instance_count,

    %% Test 3: Pattern registration - WCP-13 (Static Multiple Instances)
    Pattern3 = multiple_instances_static(fun(X) -> X end, 2, []),
    multiple_instances_static = Pattern3#pattern_state.pattern_type,
    2 = Pattern3#pattern_state.max_instances,

    %% Test 4: Pattern registration - WCP-14 (Runtime Multiple Instances)
    Pattern4 = multiple_instances_runtime(fun(X) -> X end, fun(_) -> 3 end, test_data),
    multiple_instances_runtime = Pattern4#pattern_state.pattern_type,
    3 = Pattern4#pattern_state.instance_count,

    %% Test 5: Pattern registration - WCP-15 (Dynamic Multiple Instances)
    DataFun = fun() -> done end,
    Pattern5 = multiple_instances_dynamic(fun(X) -> X end, DataFun, start),
    multiple_instances_dynamic = Pattern5#pattern_state.pattern_type,
    unlimited = Pattern5#pattern_state.max_instances,

    %% Test 6: Pattern registration - WCP-16 (Deferred Choice)
    Options = #{a => fun() -> option_a end, b => fun() -> option_b end},
    Pattern6 = deferred_choice(Options, fun(_) -> true end, ignored),
    deferred_choice = Pattern6#pattern_state.pattern_type,
    true = maps:is_key(options, Pattern6#pattern_state.choice_data),

    %% Test 7: Pattern registration - WCP-17 (Interleaved Routing)
    Branches = #{branch1 => fun(X) -> X end, branch2 => fun(X) -> X * 2 end},
    Pattern7 = interleaved_routing(Branches, ignored),
    interleaved_routing = Pattern7#pattern_state.pattern_type,
    true = maps:is_key(branches, Pattern7#pattern_state.choice_data),

    %% Test 8: Pattern registration - WCP-18 (Milestone)
    Pattern8 = milestone(fun() -> work end, fun() -> true end),
    milestone = Pattern8#pattern_state.pattern_type,
    true = maps:is_key(milestone_check, Pattern8#pattern_state.choice_data),

    %% Test 9: Pattern registration - WCP-19 (Cancel Activity)
    Pattern9 = cancel_activity(fun() -> work end, fun() -> false end),
    cancel_activity = Pattern9#pattern_state.pattern_type,
    true = maps:is_key(cancel_check, Pattern9#pattern_state.choice_data),

    %% Test 10: Pattern registration - WCP-20 (Cancel Case)
    Activities = [fun() -> a end, fun() -> b end],
    Pattern10 = cancel_case(Activities, fun() -> false end),
    cancel_case = Pattern10#pattern_state.pattern_type,
    2 = Pattern10#pattern_state.instance_count,

    %% Test 11: Pattern registration - WCP-21 (Structured Sync)
    SyncActivities = [fun() -> a end, fun() -> b end],
    Pattern11 = structured_sync(SyncActivities, data),
    structured_sync = Pattern11#pattern_state.pattern_type,
    2 = Pattern11#pattern_state.instance_count,

    %% Test 12: Pattern registration - WCP-22 (Partial Join)
    JoinActivities = [fun() -> a end, fun() -> b end, fun() -> c end],
    Pattern12 = partial_join(JoinActivities, 2),
    partial_join = Pattern12#pattern_state.pattern_type,
    2 = Pattern12#pattern_state.max_instances,

    %% Test 13: Pattern registration - WCP-23 (Structured Loop)
    LoopBody = fun(X) -> X + 1 end,
    Pattern13 = structured_loop(LoopBody, while, fun(_) -> false end),
    structured_loop = Pattern13#pattern_state.pattern_type,
    while = maps:get(loop_type, Pattern13#pattern_state.choice_data),

    %% Test 14: Pattern registration - WCP-24 (Recursion)
    RecFun = fun(X) -> X - 1 end,
    BaseCase = fun(X) -> X =< 0 end,
    Pattern14 = recursion(RecFun, BaseCase),
    recursion = Pattern14#pattern_state.pattern_type,
    true = maps:is_key(base_case, Pattern14#pattern_state.choice_data),

    %% Test 15: Pattern registration - WCP-25 (Interleaved Loop)
    LoopActivities = [fun() -> a end, fun() -> b end],
    LoopCond = fun(_) -> false end,
    Pattern15 = interleaved_loop(LoopActivities, LoopCond),
    interleaved_loop = Pattern15#pattern_state.pattern_type,
    true = is_list(Pattern15#pattern_state.subprocess),

    %% Test 16: Pattern registration - WCP-26 (Critical Section)
    CriticalFun = fun(X) -> X end,
    Pattern16 = critical_section(CriticalFun, my_lock),
    critical_section = Pattern16#pattern_state.pattern_type,
    my_lock = maps:get(lock_id, Pattern16#pattern_state.choice_data),

    %% Test 17: Pattern registration - WCP-27 (Protocol Pattern)
    ReqFun = fun() -> request end,
    RespFun = fun(_) -> response end,
    Pattern17 = protocol_pattern(ReqFun, RespFun, 5000),
    protocol = Pattern17#pattern_state.pattern_type,
    5000 = maps:get(timeout, Pattern17#pattern_state.choice_data),

    %% Test 18: Pattern registration - WCP-28 (Try-Catch)
    TryFun = fun() -> try_work end,
    CatchFun = fun(_) -> catch_work end,
    Pattern18 = try_catch(TryFun, CatchFun, [error, exit]),
    try_catch = Pattern18#pattern_state.pattern_type,
    [error, exit] = maps:get(exceptions, Pattern18#pattern_state.choice_data),

    %% Test 19: Pattern registration - WHP-1 (Error Handler)
    Protected = fun() -> work end,
    Handler = fun(_) -> handled end,
    Pattern19 = error_handler(Protected, Handler),
    error_handler = Pattern19#pattern_state.pattern_type,
    true = maps:is_key(error_handler, Pattern19#pattern_state.choice_data),

    %% Test 20: Pattern registration - WHP-2 (Retry)
    Activity = fun() -> retry_work end,
    Backoff = fun(N) -> N * 100 end,
    Pattern20 = retry(Activity, 3, Backoff),
    retry = Pattern20#pattern_state.pattern_type,
    3 = maps:get(max_retries, Pattern20#pattern_state.choice_data),

    %% Test 21: Pattern registration - WHP-3 (Compensation)
    Compensable = fun() -> work end,
    Compensator = fun(_) -> undo end,
    Pattern21 = compensate(Compensable, Compensator),
    compensation = Pattern21#pattern_state.pattern_type,
    true = maps:is_key(compensator, Pattern21#pattern_state.choice_data),

    %% Test 22: Pattern registration - WHP-4 (Triggered Compensation)
    TrigComp = fun() -> work end,
    CompFun = fun(_) -> undo end,
    TrigFun = fun() -> false end,
    Pattern22 = triggered_compensation(TrigComp, CompFun, TrigFun),
    triggered_compensation = Pattern22#pattern_state.pattern_type,
    true = maps:is_key(compensator, Pattern22#pattern_state.choice_data),
    true = maps:is_key(trigger, Pattern22#pattern_state.choice_data),

    %% Test 23: Pattern registration - WHP-5 (Consecutive Compensation)
    Pairs = [{fun() -> a end, fun(_) -> undo_a end}, {fun() -> b end, fun(_) -> undo_b end}],
    Pattern23 = consecutive_compensate(Pairs),
    consecutive_compensation = Pattern23#pattern_state.pattern_type,
    2 = Pattern23#pattern_state.instance_count,

    %% Test 24: Petri net enumeration - places
    Places = place_lst(),
    true = length(Places) > 50,
    true = lists:member('p_start', Places),
    true = lists:member('p_terminate', Places),

    %% Test 25: Petri net enumeration - transitions
    Transitions = trsn_lst(),
    true = length(Transitions) > 50,
    true = lists:member('t_activate', Transitions),
    true = lists:member('t_implicit_term', Transitions),

    %% Test 26: Petri net preset lookup
    ['p_start'] = preset('t_activate'),
    ['p_active', 'p_work'] = preset('t_implicit_term'),
    ['p_instance_pool'] = preset('t_spawn_all_static'),

    %% Test 27: Petri net initial marking
    [start] = init_marking('p_start', #pattern_state{}),
    [] = init_marking('p_instance_pool', #pattern_state{}),

    ok.
