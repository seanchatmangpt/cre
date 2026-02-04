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
%% @doc YAWL (Yet Another Workflow Language) pattern support for CRE.
%%
%% This module implements YAWL workflow patterns as Petri net structures
%% compatible with the CRE runtime environment.
%%
%% <h3>Implemented Patterns</h3>
%%
%% <h4>Basic Control Flow (WCP-1 to WCP-10):</h4>
%% <ul>
%%   <li>sequence/0, parallel_split/0, synchronization/0</li>
%%   <li>exclusive_choice/0, simple_merge/0, multi_choice/0</li>
%%   <li>synchronizing_merge/0, multi_merge/0, discriminator/0, arbitration/0</li>
%% </ul>
%%
%% <h4>Data Flow Patterns (WDP-1 to WDP-5):</h4>
%% <ul>
%%   <li><b>param_pass/2</b> (WDP-1) - Pass parameters between tasks</li>
%%   <li><b>data_transform/2</b> (WDP-2) - Transform data between tasks</li>
%%   <li><b>data_distribute/1</b> (WDP-3) - Distribute data to multiple tasks</li>
%%   <li><b>data_accumulate/1</b> (WDP-4) - Accumulate data from multiple tasks</li>
%%   <li><b>data_visibility/2</b> (WDP-5) - Control data visibility scope</li>
%% </ul>
%%
%% <h4>Resource Patterns (WRP-1 to WRP-5):</h4>
%% <ul>
%%   <li><b>resource_create/1</b> (WRP-1) - Create new workflow resource</li>
%%   <li><b>role_allocate/2</b> (WRP-2) - Allocate based on role</li>
%%   <li><b>resource_start/1</b> (WRP-3) - Start resource execution</li>
%%   <li><b>role_distribute/2</b> (WRP-4) - Distribute based on roles</li>
%%   <li><b>capability_allocate/2</b> (WRP-5) - Allocate by capability</li>
%% </ul>
%%
%% <h3>Petri Net Mapping</h3>
%%
%% Each pattern maps to a Petri net structure with:
%% <ul>
%%   <li>Places: State locations holding tokens</li>
%%   <li>Transitions: State transformations consuming/producing tokens</li>
%%   <li>Arcs: Connections between places and transitions</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_yawl).

%%====================================================================
%% Exports
%%====================================================================

-export([new_workflow/0, new_workflow/1]).
-export([add_task/3, add_condition/3, set_split_type/3, set_join_type/3, connect/3]).
-export([validate/1, get_errors/1, get_workflow_id/1, get_workflow_name/1, get_tasks/1, get_connections/1, get_conditions/1]).
-export([sequence/0, parallel_split/0, synchronization/0, exclusive_choice/0,
         simple_merge/0, multi_choice/0, synchronizing_merge/0, multi_merge/0,
         discriminator/0, arbitration/0]).
-export([param_pass/2, data_transform/2, data_distribute/1, data_accumulate/1, data_visibility/2]).
-export([resource_create/1, role_allocate/2, resource_start/1, role_distribute/2, capability_allocate/2]).

%%====================================================================
%% Types
%%====================================================================

-type join_type() :: and_join | or_join | xor_join.
-type split_type() :: and_split | or_split | xor_split.
-type condition() :: binary() | {atom(), term()} | fun(() -> boolean()).
-type element_id() :: binary().
-type task_type() :: atomic | composite | subworkflow | multi_instance.

%%====================================================================
%% Records
%%====================================================================

-record(task, {
          id :: element_id(),
          name :: binary(),
          type :: task_type(),
          split_type :: split_type() | undefined,
          join_type :: join_type() | undefined,
          metadata = #{} :: #{atom() => term()}
         }).

-record(yawl_condition, {
          id :: element_id(),
          expression :: condition(),
          description :: binary() | undefined
         }).

-record(connection, {
          from_id :: element_id(),
          to_id :: element_id(),
          condition_id :: element_id() | undefined
         }).

-record(workflow, {
          id :: element_id(),
          name :: binary(),
          tasks = #{} :: #{element_id() => #task{}},
          conditions = #{} :: #{element_id() => #yawl_condition{}},
          connections = [] :: [#connection{}],
          start_task_id :: element_id() | undefined,
          end_task_ids = [] :: [element_id()]
         }).

-record(sequence, {task_ids :: [element_id()]}).
-record(parallel_split, {split_task_id :: element_id(), branch_task_ids :: [element_id()]}).
-record(synchronization, {join_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(exclusive_choice, {choice_task_id :: element_id(), branches :: [{element_id(), condition()}]}).
-record(simple_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(multi_choice, {choice_task_id :: element_id(), branches :: [{element_id(), condition()}]}).
-record(synchronizing_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(multi_merge, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(discriminator, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()]}).
-record(arbitration, {merge_task_id :: element_id(), incoming_task_ids :: [element_id()], required_count :: pos_integer()}).

%% Data Flow Pattern Records (WDP-1 to WDP-5)
-record(param_pass, {source_task_id :: element_id(), target_task_id :: element_id(), param_name :: atom(), transform_fn :: function() | undefined}).
-record(data_transform, {input_task_id :: element_id(), output_task_id :: element_id(), transform_fn :: function(), output_schema :: term() | undefined}).
-record(data_distribute, {source_task_id :: element_id(), recipient_task_ids :: [element_id()], distribution_type :: broadcast | round_robin | partitioned}).
-record(data_accumulate, {source_task_ids :: [element_id()], target_task_id :: element_id(), aggregation_fn :: function(), initial_value :: term()}).
-record(data_visibility, {data_task_id :: element_id(), scope :: local | branch | global, access_list :: [element_id()] | undefined}).

%% Resource Pattern Records (WRP-1 to WRP-5)
-record(resource_create, {resource_id :: element_id(), resource_type :: atom(), init_params :: map()}).
-record(role_allocate, {role_id :: atom(), required_capability :: term(), allocation_strategy :: first_fit | best_fit | random}).
-record(resource_start, {resource_id :: element_id(), start_params :: map()}).
-record(role_distribute, {work_item_ids :: [element_id()], role_assignments :: map(), distribution_policy :: round_robin | least_loaded | affinity_based}).
-record(capability_allocate, {required_capabilities :: map(), resource_registry :: [term()], matching_strategy :: exact_match | minimum_met | best_effort}).

-type pattern() :: #workflow{} | #sequence{} | #parallel_split{} |
                   #synchronization{} | #exclusive_choice{} |
                   #simple_merge{} | #multi_choice{} |
                   #synchronizing_merge{} | #multi_merge{} |
                   #discriminator{} | #arbitration{} |
                   #param_pass{} | #data_transform{} | #data_distribute{} |
                   #data_accumulate{} | #data_visibility{} |
                   #resource_create{} | #role_allocate{} | #resource_start{} |
                   #role_distribute{} | #capability_allocate{}.

-export_type([pattern/0]).

%%====================================================================
%% API
%%====================================================================

-spec new_workflow() -> #workflow{}.
new_workflow() ->
    WorkflowId = generate_id(<<"workflow">>),
    new_workflow(WorkflowId).

-spec new_workflow(Id :: element_id()) -> #workflow{}.
new_workflow(Id) when is_binary(Id) ->
    #workflow{id = Id, name = <<"Untitled Workflow">>}.

-spec add_task(Workflow :: #workflow{}, TaskId :: element_id(),
               Task :: #task{} | [{atom(), term()}]) -> #workflow{}.
add_task(#workflow{tasks = Tasks} = Workflow, TaskId, TaskRec)
  when element(1, TaskRec) =:= task ->
    Workflow#workflow{tasks = Tasks#{TaskId => TaskRec}};
add_task(Workflow, TaskId, PropList) when is_list(PropList) ->
    Task = make_task(TaskId, PropList),
    add_task(Workflow, TaskId, Task).

-spec add_condition(Workflow :: #workflow{}, ConditionId :: element_id(),
                    Condition :: #yawl_condition{} | condition()) -> #workflow{}.
add_condition(#workflow{conditions = Conds} = Workflow, ConditionId, CondRec)
  when element(1, CondRec) =:= yawl_condition ->
    Workflow#workflow{conditions = Conds#{ConditionId => CondRec}};
add_condition(Workflow, ConditionId, Expression) ->
    CondRec = #yawl_condition{id = ConditionId, expression = Expression},
    add_condition(Workflow, ConditionId, CondRec).

-spec set_split_type(Workflow :: #workflow{}, TaskId :: element_id(),
                     SplitType :: split_type()) -> #workflow{}.
set_split_type(#workflow{tasks = Tasks} = Workflow, TaskId, SplitType)
  when is_binary(TaskId), (SplitType =:= and_split orelse
                           SplitType =:= or_split orelse
                           SplitType =:= xor_split) ->
    case maps:get(TaskId, Tasks, undefined) of
        undefined -> Workflow;
        Task -> Workflow#workflow{tasks = Tasks#{TaskId => Task#task{split_type = SplitType}}}
    end.

-spec set_join_type(Workflow :: #workflow{}, TaskId :: element_id(),
                    JoinType :: join_type()) -> #workflow{}.
set_join_type(#workflow{tasks = Tasks} = Workflow, TaskId, JoinType)
  when is_binary(TaskId), (JoinType =:= and_join orelse
                           JoinType =:= or_join orelse
                           JoinType =:= xor_join) ->
    case maps:get(TaskId, Tasks, undefined) of
        undefined -> Workflow;
        Task -> Workflow#workflow{tasks = Tasks#{TaskId => Task#task{join_type = JoinType}}}
    end.

-spec connect(Workflow :: #workflow{}, FromId :: element_id(),
              ToId :: element_id()) -> #workflow{}.
connect(#workflow{connections = Conns} = Workflow, FromId, ToId)
  when is_binary(FromId), is_binary(ToId) ->
    Conn = #connection{from_id = FromId, to_id = ToId},
    Workflow#workflow{connections = [Conn | Conns]}.

-spec validate(Workflow :: #workflow{}) -> ok | {error, [binary()]}.
validate(#workflow{} = Workflow) ->
    case validate_workflow(Workflow) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

-spec get_errors(Workflow :: #workflow{}) -> [binary()].
get_errors(#workflow{} = Workflow) ->
    validate_workflow(Workflow).

%%--------------------------------------------------------------------
%% @doc Gets the workflow ID from a workflow record.
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_id(#workflow{}) -> {ok, element_id()}.
get_workflow_id(#workflow{id = Id}) -> {ok, Id}.

%%--------------------------------------------------------------------
%% @doc Gets the workflow name from a workflow record.
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_name(#workflow{}) -> {ok, binary()}.
get_workflow_name(#workflow{name = Name}) -> {ok, Name}.

%%--------------------------------------------------------------------
%% @doc Gets the tasks map from a workflow record.
%% @end
%%--------------------------------------------------------------------
-spec get_tasks(#workflow{}) -> {ok, #{element_id() => #task{}}}.
get_tasks(#workflow{tasks = Tasks}) -> {ok, Tasks}.

%%--------------------------------------------------------------------
%% @doc Gets the connections list from a workflow record.
%% @end
%%--------------------------------------------------------------------
-spec get_connections(#workflow{}) -> {ok, [#connection{}]}.
get_connections(#workflow{connections = Conns}) -> {ok, Conns}.

%%--------------------------------------------------------------------
%% @doc Gets the conditions map from a workflow record.
%% @end
%%--------------------------------------------------------------------
-spec get_conditions(#workflow{}) -> {ok, #{element_id() => #yawl_condition{}}}.
get_conditions(#workflow{conditions = Conds}) -> {ok, Conds}.

%% Pattern constructors
-spec sequence() -> #sequence{}.
sequence() -> #sequence{task_ids = []}.

-spec parallel_split() -> #parallel_split{}.
parallel_split() -> #parallel_split{split_task_id = <<>>, branch_task_ids = []}.

-spec synchronization() -> #synchronization{}.
synchronization() -> #synchronization{join_task_id = <<>>, incoming_task_ids = []}.

-spec exclusive_choice() -> #exclusive_choice{}.
exclusive_choice() -> #exclusive_choice{choice_task_id = <<>>, branches = []}.

-spec simple_merge() -> #simple_merge{}.
simple_merge() -> #simple_merge{merge_task_id = <<>>, incoming_task_ids = []}.

-spec multi_choice() -> #multi_choice{}.
multi_choice() -> #multi_choice{choice_task_id = <<>>, branches = []}.

-spec synchronizing_merge() -> #synchronizing_merge{}.
synchronizing_merge() -> #synchronizing_merge{merge_task_id = <<>>, incoming_task_ids = []}.

-spec multi_merge() -> #multi_merge{}.
multi_merge() -> #multi_merge{merge_task_id = <<>>, incoming_task_ids = []}.

-spec discriminator() -> #discriminator{}.
discriminator() -> #discriminator{merge_task_id = <<>>, incoming_task_ids = []}.

-spec arbitration() -> #arbitration{}.
arbitration() -> #arbitration{merge_task_id = <<>>, incoming_task_ids = [], required_count = 1}.

%% Data Flow Pattern constructors (WDP-1 to WDP-5)
-spec param_pass(element_id(), element_id()) -> #param_pass{}.
param_pass(SourceId, TargetId) ->
    #param_pass{source_task_id = SourceId, target_task_id = TargetId, param_name = undefined, transform_fn = undefined}.

-spec data_transform(element_id(), element_id()) -> #data_transform{}.
data_transform(InputId, OutputId) ->
    TransformFn = fun(X) -> X end,
    #data_transform{input_task_id = InputId, output_task_id = OutputId, transform_fn = TransformFn, output_schema = undefined}.

-spec data_distribute([element_id()]) -> #data_distribute{}.
data_distribute(RecipientIds) ->
    #data_distribute{source_task_id = <<>>, recipient_task_ids = RecipientIds, distribution_type = broadcast}.

-spec data_accumulate([element_id()]) -> #data_accumulate{}.
data_accumulate(SourceIds) ->
    AggFn = fun(Acc, X) -> [X | Acc] end,
    #data_accumulate{source_task_ids = SourceIds, target_task_id = <<>>, aggregation_fn = AggFn, initial_value = []}.

-spec data_visibility(element_id(), local | branch | global) -> #data_visibility{}.
data_visibility(DataId, Scope) ->
    #data_visibility{data_task_id = DataId, scope = Scope, access_list = undefined}.

%% Resource Pattern constructors (WRP-1 to WRP-5)
-spec resource_create(atom()) -> #resource_create{}.
resource_create(ResourceType) ->
    #resource_create{resource_id = <<>>, resource_type = ResourceType, init_params = #{}}.

-spec role_allocate(atom(), term()) -> #role_allocate{}.
role_allocate(RoleId, Capability) ->
    #role_allocate{role_id = RoleId, required_capability = Capability, allocation_strategy = first_fit}.

-spec resource_start(element_id()) -> #resource_start{}.
resource_start(ResourceId) ->
    #resource_start{resource_id = ResourceId, start_params = #{}}.

-spec role_distribute([element_id()], map()) -> #role_distribute{}.
role_distribute(WorkItemIds, RoleAssignments) ->
    #role_distribute{work_item_ids = WorkItemIds, role_assignments = RoleAssignments, distribution_policy = round_robin}.

-spec capability_allocate(map(), [term()]) -> #capability_allocate{}.
capability_allocate(Capabilities, Registry) ->
    #capability_allocate{required_capabilities = Capabilities, resource_registry = Registry, matching_strategy = exact_match}.

%%====================================================================
%% Internal
%%====================================================================

-spec generate_id(Prefix :: binary()) -> element_id().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

-spec make_task(TaskId :: element_id(), PropList :: [{atom(), term()}]) -> #task{}.
make_task(TaskId, PropList) ->
    Name = proplists:get_value(name, PropList, <<"Unnamed Task">>),
    Type = proplists:get_value(type, PropList, atomic),
    SplitType = proplists:get_value(split_type, PropList, undefined),
    JoinType = proplists:get_value(join_type, PropList, undefined),
    Metadata = proplists:get_value(metadata, PropList, #{}),
    #task{id = TaskId, name = Name, type = Type, split_type = SplitType,
          join_type = JoinType, metadata = Metadata}.

-spec validate_workflow(#workflow{}) -> [binary()].
validate_workflow(#workflow{tasks = Tasks, connections = Conns,
                            start_task_id = StartTaskId, end_task_ids = EndTaskIds}) ->
    lists:flatten([
        validate_tasks_exist(Tasks, Conns),
        validate_start_task(StartTaskId, Tasks),
        validate_end_tasks(EndTaskIds, Tasks),
        validate_connections(Conns),
        validate_no_cycles(Conns),
        validate_split_join_consistency(Tasks, Conns)
    ]).

-spec validate_tasks_exist(#{element_id() => #task{}}, [#connection{}]) -> [binary()].
validate_tasks_exist(Tasks, Conns) ->
    TaskIds = maps:keys(Tasks),
    F = fun(#connection{from_id = From, to_id = To}, Acc) ->
            Acc1 = case lists:member(From, TaskIds) of
                       false -> [iolist_to_binary([<<"Task '">>, From, <<"' does not exist">>]) | Acc];
                       true -> Acc
                   end,
            case lists:member(To, TaskIds) of
                false -> [iolist_to_binary([<<"Task '">>, To, <<"' does not exist">>]) | Acc1];
                true -> Acc1
            end
        end,
    lists:foldl(F, [], Conns).

-spec validate_start_task(element_id() | undefined, #{element_id() => #task{}}) -> [binary()].
validate_start_task(undefined, _Tasks) -> [];
validate_start_task(StartTaskId, Tasks) ->
    case maps:is_key(StartTaskId, Tasks) of
        false -> [iolist_to_binary([<<"Start task '">>, StartTaskId, <<"' does not exist">>])];
        true -> []
    end.

-spec validate_end_tasks([element_id()], #{element_id() => #task{}}) -> [binary()].
validate_end_tasks([], _Tasks) -> [];
validate_end_tasks(EndTaskIds, Tasks) ->
    F = fun(TaskId, Acc) ->
            case maps:is_key(TaskId, Tasks) of
                false -> [iolist_to_binary([<<"End task '">>, TaskId, <<"' does not exist">>]) | Acc];
                true -> Acc
            end
        end,
    lists:foldl(F, [], EndTaskIds).

-spec validate_connections([#connection{}]) -> [binary()].
validate_connections(Conns) ->
    F = fun(#connection{from_id = From, to_id = To}, Acc) ->
            Acc1 = case From =:= To of
                       true -> [iolist_to_binary([<<"Self-loop on '">>, From, <<"'">>]) | Acc];
                       false -> Acc
                   end,
            case lists:any(fun(C) -> C#connection.from_id =:= From andalso
                                      C#connection.to_id =:= To end,
                           Conns -- [#connection{from_id = From, to_id = To}]) of
                true -> [iolist_to_binary([<<"Duplicate connection '">>, From, <<"' -> '">>, To, <<"'">>]) | Acc1];
                false -> Acc1
            end
        end,
    lists:foldl(F, [], Conns).

-spec validate_no_cycles([#connection{}]) -> [binary()].
validate_no_cycles([]) -> [];
validate_no_cycles(Conns) ->
    F = fun(#connection{from_id = From, to_id = To}, Acc) ->
            Acc#{From => [To | maps:get(From, Acc, [])]}
        end,
    Graph = lists:foldl(F, #{}, Conns),
    Nodes = lists:usort([N || #connection{from_id = N} <- Conns] ++
                         [T || #connection{to_id = T} <- Conns]),
    case detect_cycle(Graph, Nodes, [], #{}) of
        {cycle, Node} -> [iolist_to_binary([<<"Cycle involving '">>, Node, <<"'">>])];
        no_cycle -> []
    end.

-spec detect_cycle(#{element_id() => [element_id()]}, [element_id()],
                   [element_id()], #{element_id() => visiting | visited}) ->
          {cycle, element_id()} | no_cycle.
detect_cycle(_Graph, [], _Path, _Visited) -> no_cycle;
detect_cycle(Graph, [Node | Rest], Path, Visited) ->
    case maps:get(Node, Visited, undefined) of
        visiting -> {cycle, Node};
        visited -> detect_cycle(Graph, Rest, Path, Visited);
        undefined ->
            Visited1 = Visited#{Node => visiting},
            Neighbors = maps:get(Node, Graph, []),
            case detect_cycle(Graph, Neighbors, [Node | Path], Visited1) of
                {cycle, _} = Cycle -> Cycle;
                no_cycle ->
                    Visited2 = Visited1#{Node => visited},
                    detect_cycle(Graph, Rest, Path, Visited2)
            end
    end.

-spec validate_split_join_consistency(#{element_id() => #task{}},
                                      [#connection{}]) -> [binary()].
validate_split_join_consistency(Tasks, Conns) ->
    F = fun(TaskId, Task, Acc) ->
            Acc ++ validate_split_type(TaskId, Task, Conns)
                ++ validate_join_type(TaskId, Task, Conns)
        end,
    maps:fold(F, [], Tasks).

-spec validate_split_type(element_id(), #task{}, [#connection{}]) -> [binary()].
validate_split_type(_TaskId, #task{split_type = undefined}, _Conns) -> [];
validate_split_type(TaskId, #task{split_type = SplitType}, Conns) ->
    OutgoingCount = length([C || #connection{from_id = F} = C <- Conns, F =:= TaskId]),
    NBin = integer_to_binary(OutgoingCount),
    case {SplitType, OutgoingCount} of
        {xor_split, N} when N < 2 ->
            [iolist_to_binary([<<"XOR split on '">>, TaskId,
                              <<"' requires at least 2 outgoing, found ">>, NBin])];
        {and_split, N} when N < 2 ->
            [iolist_to_binary([<<"AND split on '">>, TaskId,
                              <<"' requires at least 2 outgoing, found ">>, NBin])];
        {or_split, N} when N < 2 ->
            [iolist_to_binary([<<"OR split on '">>, TaskId,
                              <<"' requires at least 2 outgoing, found ">>, NBin])];
        _ -> []
    end.

-spec validate_join_type(element_id(), #task{}, [#connection{}]) -> [binary()].
validate_join_type(_TaskId, #task{join_type = undefined}, _Conns) -> [];
validate_join_type(TaskId, #task{join_type = JoinType}, Conns) ->
    IncomingCount = length([C || #connection{to_id = T} = C <- Conns, T =:= TaskId]),
    NBin = integer_to_binary(IncomingCount),
    case {JoinType, IncomingCount} of
        {xor_join, N} when N < 2 ->
            [iolist_to_binary([<<"XOR join on '">>, TaskId,
                              <<"' requires at least 2 incoming, found ">>, NBin])];
        {and_join, N} when N < 2 ->
            [iolist_to_binary([<<"AND join on '">>, TaskId,
                              <<"' requires at least 2 incoming, found ">>, NBin])];
        {or_join, N} when N < 2 ->
            [iolist_to_binary([<<"OR join on '">>, TaskId,
                              <<"' requires at least 2 incoming, found ">>, NBin])];
        _ -> []
    end.
