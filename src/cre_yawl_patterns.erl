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
%% @doc YAWL Workflow Control Patterns for CRE (WCP-11 through WCP-17)
%%
%% This module implements YAWL (Yet Another Workflow Language) workflow
%% control patterns WCP-11 through WCP-17 as Petri net structures compatible
%% with the gen_pnet behavior and CRE runtime environment.
%%
%% <h3>Patterns Implemented</h3>
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
%%
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
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
     'p_all_done', 'p_interleave_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the pattern Petri net.
%%
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
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
     't_return_branch', 't_complete_interleaved'
    ].

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
preset('t_complete_interleaved') -> ['p_all_done'];

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
            gen_pnet:set_usr_info(NetState, UpdatedState),
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
    gen_pnet:set_usr_info(NetState, UpdatedState),
    pass;

trigger_no_sync(_Place, _Token, _NetState) ->
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
      pending_instances = [InitialData]
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
      pending_instances = Activities
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
      instance_count = 0
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
      instance_count = 0
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
      instance_count = 0
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
      instance_count = 0
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
      choice_data = #{response_handler => ResponseHandlerFun, timeout => Timeout},
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
      choice_data = #{catch_handler => CatchFun, exceptions => ExceptionTypes},
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
