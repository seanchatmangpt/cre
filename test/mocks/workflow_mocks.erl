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
%% @doc YAWL Workflow Mock Factories
%%
%% This module provides mock factories for YAWL workflow specifications
%% and related structures for testing workflow execution and validation.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Mock YAWL workflow specifications</li>
%%   <li>Mock task definitions</li>
%%   <li>Mock condition expressions</li>
%%   <li>Mock flow definitions</li>
%%   <li>Mock decomposition info</li>
%%   <li>Meck-compatible for easy mocking</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Create a simple workflow:
%% ```erlang
%% > WF = workflow_mocks:simple_workflow().
%% #{id => <<"wf1">>, tasks => #{}, ...}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(workflow_mocks).

%%====================================================================
%% Exports
%%====================================================================

%% Workflow generators
-export([simple_workflow/0, simple_workflow/1]).
-export([complex_workflow/0, complex_workflow/1]).
-export([empty_workflow/0]).
-export([approval_workflow/0]).
-export([parallel_workflow/0]).
-export([loop_workflow/0]).

%% Task generators
-export([mock_task/0, mock_task/1]).
-export([atomic_task/1]).
-export([composite_task/1]).
-export([multi_instance_task/2]).
-export([task_list/1]).

%% Condition generators
-export([mock_condition/0, mock_condition/1]).
-export([input_condition/1]).
-export([output_condition/1]).
-export([condition_list/1]).

%% Flow generators
-export([mock_flow/0, mock_flow/1]).
-export([conditional_flow/3]).
-export([flow_list/2]).

%% Decomposition generators
-export([mock_decomposition/0, mock_decomposition/1]).
-export([root_decomposition/1]).

%% YAWL spec generators (wf_spec compatible)
-export([yawl_spec/0, yawl_spec/1]).
-export([spec_from_tasks/1]).

%% Utility functions
-export([is_valid_workflow/1]).
-export([task_ids/1]).
-export([get_task/2]).
-export([add_task/2]).

%%====================================================================
%% Types
%%====================================================================

-type task_id() :: binary().
-type task_type() :: atomic | composite | multi_instance.
-type split_type() :: 'and' | 'or' | 'xor' | undefined.
-type join_type() :: 'and' | 'or' | 'xor' | undefined.
-type condition_type() :: input | output.

-type mock_task() :: #{
    id := task_id(),
    name := binary(),
    type := task_type(),
    split_type => split_type(),
    join_type => join_type(),
    decomposition => binary() | undefined,
    min_instances => non_neg_integer() | undefined,
    max_instances => non_neg_integer() | unlimited | undefined,
    continuation_threshold => non_neg_integer() | undefined
}.

-type mock_condition() :: #{
    id := binary(),
    type := condition_type(),
    expression := binary() | undefined
}.

-type mock_flow() :: #{
    id := binary(),
    from := task_id(),
    to := task_id(),
    predicate := binary() | undefined
}.

-type mock_decomposition() :: #{
    id := binary(),
    is_root := boolean(),
    tasks := [task_id()]
}.

-type mock_workflow() :: #{
    id := binary(),
    name := binary(),
    version => binary() | undefined,
    root_net := binary(),
    tasks => #{task_id() => mock_task()},
    conditions => #{binary() => mock_condition()},
    flows => [mock_flow()],
    decompositions => #{binary() => mock_decomposition()}
}.

-export_type([mock_task/0, mock_condition/0, mock_flow/0,
             mock_decomposition/0, mock_workflow/0]).

%%====================================================================
%% Workflow Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a simple workflow with default settings.
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_workflow() -> mock_workflow().

simple_workflow() ->
    simple_workflow([]).

%%--------------------------------------------------------------------
%% @doc Generates a simple workflow with options.
%%
%% Options:
%% - `{id, binary()}` - Workflow ID
%% - `{name, binary()}` - Workflow name
%% - `{task_count, pos_integer()}` - Number of tasks
%%
%% @end
%%--------------------------------------------------------------------
-spec simple_workflow([{atom(), term()}]) -> mock_workflow().

simple_workflow(Options) ->
    Id = proplists:get_value(id, Options, <<"simple_wf">>),
    Name = proplists:get_value(name, Options, <<"Simple Workflow">>),
    TaskCount = proplists:get_value(task_count, Options, 2),

    Tasks = lists:map(fun(I) ->
        TaskId = list_to_binary("task_" ++ integer_to_list(I)),
        atomic_task(TaskId)
    end, lists:seq(1, TaskCount)),

    TaskIds = [maps:get(id, T) || T <- Tasks],
    TaskMap = maps:from_list([{maps:get(id, T), T} || T <- Tasks]),

    Flows = flow_list(TaskIds, sequential),

    Conditions = #{
        <<"input">> => input_condition(<<"input">>),
        <<"output">> => output_condition(<<"output">>)
    },

    #{
        id => Id,
        name => Name,
        version => <<"1.0">>,
        root_net => <<"main">>,
        tasks => TaskMap,
        conditions => Conditions,
        flows => Flows,
        decompositions => #{<<"main">> => root_decomposition(TaskIds)}
    }.

%%--------------------------------------------------------------------
%% @doc Generates a complex workflow with various patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec complex_workflow() -> mock_workflow().

complex_workflow() ->
    complex_workflow([]).

%%--------------------------------------------------------------------
%% @doc Generates a complex workflow with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec complex_workflow([{atom(), term()}]) -> mock_workflow().

complex_workflow(Options) ->
    Id = proplists:get_value(id, Options, <<"complex_wf">>),
    Name = proplists:get_value(name, Options, <<"Complex Workflow">>),

    %% Create tasks with different split/join types
    Tasks = [
        atomic_task(<<"start">>),
        (atomic_task(<<"branch1">>))#{split_type => 'xor', join_type => 'xor'},
        (atomic_task(<<"branch2">>))#{split_type => 'xor', join_type => 'xor'},
        (atomic_task(<<"parallel1">>))#{split_type => 'and', join_type => 'and'},
        (atomic_task(<<"parallel2">>))#{split_type => 'and', join_type => 'and'},
        atomic_task(<<"end">>)
    ],

    TaskMap = maps:from_list([{maps:get(id, T), T} || T <- Tasks]),

    Flows = [
        mock_flow(#{from => <<"start">>, to => <<"branch1">>}),
        mock_flow(#{from => <<"start">>, to => <<"branch2">>}),
        mock_flow(#{from => <<"branch1">>, to => <<"parallel1">>}),
        mock_flow(#{from => <<"branch2">>, to => <<"parallel2">>}),
        mock_flow(#{from => <<"parallel1">>, to => <<"end">>}),
        mock_flow(#{from => <<"parallel2">>, to => <<"end">>})
    ],

    Conditions = #{
        <<"input">> => input_condition(<<"input">>),
        <<"output">> => output_condition(<<"output">>)
    },

    #{
        id => Id,
        name => Name,
        version => <<"2.0">>,
        root_net => <<"main">>,
        tasks => TaskMap,
        conditions => Conditions,
        flows => Flows,
        decompositions => #{<<"main">> => root_decomposition(task_ids(TaskMap))}
    }.

%%--------------------------------------------------------------------
%% @doc Generates an empty workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec empty_workflow() -> mock_workflow().

empty_workflow() ->
    #{
        id => <<"empty_wf">>,
        name => <<"Empty Workflow">>,
        root_net => <<"main">>,
        tasks => #{},
        conditions => #{},
        flows => [],
        decompositions => #{<<"main">> => root_decomposition([])}
    }.

%%--------------------------------------------------------------------
%% @doc Generates an approval workflow pattern.
%%
%% Typical approval process with review and decision.
%%
%% @end
%%--------------------------------------------------------------------
-spec approval_workflow() -> mock_workflow().

approval_workflow() ->
    Tasks = [
        atomic_task(<<"submit">>),
        atomic_task(<<"review">>),
        (atomic_task(<<"decision">>))#{split_type => 'xor'},
        atomic_task(<<"approve">>),
        atomic_task(<<"reject">>),
        atomic_task(<<"notify">>)
    ],

    TaskMap = maps:from_list([{maps:get(id, T), T} || T <- Tasks]),

    Flows = [
        mock_flow(#{from => <<"submit">>, to => <<"review">>}),
        mock_flow(#{from => <<"review">>, to => <<"decision">>}),
        conditional_flow(<<"decision">>, <<"approve">>, <<"approved">>),
        conditional_flow(<<"decision">>, <<"reject">>, <<"rejected">>),
        mock_flow(#{from => <<"approve">>, to => <<"notify">>}),
        mock_flow(#{from => <<"reject">>, to => <<"notify">>})
    ],

    Conditions = #{
        <<"input">> => input_condition(<<"input">>),
        <<"output">> => output_condition(<<"output">>)
    },

    #{
        id => <<"approval_wf">>,
        name => <<"Approval Workflow">>,
        root_net => <<"main">>,
        tasks => TaskMap,
        conditions => Conditions,
        flows => Flows,
        decompositions => #{<<"main">> => root_decomposition(task_ids(TaskMap))}
    }.

%%--------------------------------------------------------------------
%% @doc Generates a parallel workflow pattern.
%%
%% Multiple parallel branches that execute concurrently.
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel_workflow() -> mock_workflow().

parallel_workflow() ->
    Tasks = [
        atomic_task(<<"start">>),
        atomic_task(<<"parallel1">>),
        atomic_task(<<"parallel2">>),
        atomic_task(<<"parallel3">>),
        atomic_task(<<"join">>),
        atomic_task(<<"end">>)
    ],

    TaskMap = maps:from_list([{maps:get(id, T), T} || T <- Tasks]),

    Flows = [
        mock_flow(#{from => <<"start">>, to => <<"parallel1">>}),
        mock_flow(#{from => <<"start">>, to => <<"parallel2">>}),
        mock_flow(#{from => <<"start">>, to => <<"parallel3">>}),
        mock_flow(#{from => <<"parallel1">>, to => <<"join">>}),
        mock_flow(#{from => <<"parallel2">>, to => <<"join">>}),
        mock_flow(#{from => <<"parallel3">>, to => <<"join">>}),
        mock_flow(#{from => <<"join">>, to => <<"end">>})
    ],

    Conditions = #{
        <<"input">> => input_condition(<<"input">>),
        <<"output">> => output_condition(<<"output">>)
    },

    #{
        id => <<"parallel_wf">>,
        name => <<"Parallel Workflow">>,
        root_net => <<"main">>,
        tasks => TaskMap,
        conditions => Conditions,
        flows => Flows,
        decompositions => #{<<"main">> => root_decomposition(task_ids(TaskMap))}
    }.

%%--------------------------------------------------------------------
%% @doc Generates a loop workflow pattern.
%%
%% Workflow with a repeating activity.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop_workflow() -> mock_workflow().

loop_workflow() ->
    Tasks = [
        atomic_task(<<"start">>),
        atomic_task(<<"process">>),
        atomic_task(<<"check">>),
        atomic_task(<<"end">>)
    ],

    TaskMap = maps:from_list([{maps:get(id, T), T} || T <- Tasks]),

    Flows = [
        mock_flow(#{from => <<"start">>, to => <<"process">>}),
        mock_flow(#{from => <<"process">>, to => <<"check">>}),
        conditional_flow(<<"check">>, <<"end">>, <<"complete">>),
        conditional_flow(<<"check">>, <<"process">>, <<"repeat">>)
    ],

    Conditions = #{
        <<"input">> => input_condition(<<"input">>),
        <<"output">> => output_condition(<<"output">>)
    },

    #{
        id => <<"loop_wf">>,
        name => <<"Loop Workflow">>,
        root_net => <<"main">>,
        tasks => TaskMap,
        conditions => Conditions,
        flows => Flows,
        decompositions => #{<<"main">> => root_decomposition(task_ids(TaskMap))}
    }.

%%====================================================================
%% Task Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock task with defaults.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_task() -> mock_task().

mock_task() ->
    mock_task([]).

%%--------------------------------------------------------------------
%% @doc Generates a mock task with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_task([{atom(), term()}]) -> mock_task().

mock_task(Options) ->
    Id = proplists:get_value(id, Options, <<"task_1">>),
    Name = proplists:get_value(name, Options, <<"Task 1">>),
    Type = proplists:get_value(type, Options, atomic),
    SplitType = proplists:get_value(split_type, Options, undefined),
    JoinType = proplists:get_value(join_type, Options, undefined),

    #{
        id => Id,
        name => Name,
        type => Type,
        split_type => SplitType,
        join_type => JoinType,
        decomposition => undefined,
        min_instances => undefined,
        max_instances => undefined,
        continuation_threshold => undefined
    }.

%%--------------------------------------------------------------------
%% @doc Generates an atomic task.
%%
%% @end
%%--------------------------------------------------------------------
-spec atomic_task(task_id()) -> mock_task().

atomic_task(Id) ->
    #{
        id => Id,
        name => <<"Atomic Task: ", Id/binary>>,
        type => atomic,
        split_type => undefined,
        join_type => undefined
    }.

%%--------------------------------------------------------------------
%% @doc Generates a composite task (sub-workflow).
%%
%% @end
%%--------------------------------------------------------------------
-spec composite_task(task_id()) -> mock_task().

composite_task(Id) ->
    DecompId = <<Id/binary, "_decomp">>,
    #{
        id => Id,
        name => <<"Composite Task: ", Id/binary>>,
        type => composite,
        split_type => undefined,
        join_type => undefined,
        decomposition => DecompId
    }.

%%--------------------------------------------------------------------
%% @doc Generates a multi-instance task.
%%
%% @end
%%--------------------------------------------------------------------
-spec multi_instance_task(task_id(), {non_neg_integer(), non_neg_integer() | unlimited}) ->
          mock_task().

multi_instance_task(Id, {Min, Max}) ->
    #{
        id => Id,
        name => <<"Multi-Instance Task: ", Id/binary>>,
        type => multi_instance,
        split_type => undefined,
        join_type => undefined,
        min_instances => Min,
        max_instances => Max,
        continuation_threshold => Min
    }.

%%--------------------------------------------------------------------
%% @doc Generates a list of tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec task_list([task_id()]) -> [mock_task()].

task_list(Ids) ->
    [atomic_task(Id) || Id <- Ids].

%%====================================================================
%% Condition Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock condition.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_condition() -> mock_condition().

mock_condition() ->
    mock_condition([]).

%%--------------------------------------------------------------------
%% @doc Generates a mock condition with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_condition([{atom(), term()}]) -> mock_condition().

mock_condition(Options) ->
    Id = proplists:get_value(id, Options, <<"cond_1">>),
    Type = proplists:get_value(type, Options, input),
    Expression = proplists:get_value(expression, Options, undefined),

    #{
        id => Id,
        type => Type,
        expression => Expression
    }.

%%--------------------------------------------------------------------
%% @doc Generates an input condition.
%%
%% @end
%%--------------------------------------------------------------------
-spec input_condition(binary()) -> mock_condition().

input_condition(Id) ->
    #{
        id => Id,
        type => input,
        expression => undefined
    }.

%%--------------------------------------------------------------------
%% @doc Generates an output condition.
%%
%% @end
%%--------------------------------------------------------------------
-spec output_condition(binary()) -> mock_condition().

output_condition(Id) ->
    #{
        id => Id,
        type => output,
        expression => undefined
    }.

%%--------------------------------------------------------------------
%% @doc Generates a list of conditions.
%%
%% @end
%%--------------------------------------------------------------------
-spec condition_list([binary()]) -> #{binary() => mock_condition()}.

condition_list(Ids) ->
    maps:from_list([{Id, input_condition(Id)} || Id <- Ids]).

%%====================================================================
%% Flow Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock flow.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_flow() -> mock_flow().

mock_flow() ->
    mock_flow([]).

%%--------------------------------------------------------------------
%% @doc Generates a mock flow with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_flow([{atom(), term()}] | map()) -> mock_flow().

mock_flow(Options) when is_map(Options) ->
    From = maps:get(from, Options, <<"task_1">>),
    To = maps:get(to, Options, <<"task_2">>),
    Predicate = maps:get(predicate, Options, undefined),
    #{
        id => flow_id(From, To),
        from => From,
        to => To,
        predicate => Predicate
    };
mock_flow(Options) ->
    From = proplists:get_value(from, Options, <<"task_1">>),
    To = proplists:get_value(to, Options, <<"task_2">>),
    Predicate = proplists:get_value(predicate, Options, undefined),
    #{
        id => flow_id(From, To),
        from => From,
        to => To,
        predicate => Predicate
    }.

%%--------------------------------------------------------------------
%% @doc Generates a conditional flow with predicate.
%%
%% @end
%%--------------------------------------------------------------------
-spec conditional_flow(task_id(), task_id(), binary()) -> mock_flow().

conditional_flow(From, To, Predicate) ->
    mock_flow([
        {from, From},
        {to, To},
        {predicate, <<"condition: ", Predicate/binary>>}
    ]).

%%--------------------------------------------------------------------
%% @doc Generates a list of flows.
%%
%% Type can be `sequential` or `parallel'.
%%
%% @end
%%--------------------------------------------------------------------
-spec flow_list([task_id()], sequential | parallel) -> [mock_flow()].

flow_list([], _Type) ->
    [];
flow_list([_Single], _Type) ->
    [];
flow_list(TaskIds, sequential) ->
    lists:map(fun({From, To}) ->
        mock_flow(#{from => From, to => To})
    end, pairs(TaskIds));
flow_list(TaskIds, parallel) ->
    [First | _] = TaskIds,
    lists:map(fun(To) ->
        mock_flow(#{from => First, to => To})
    end, TaskIds).

%%====================================================================
%% Decomposition Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a mock decomposition.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_decomposition() -> mock_decomposition().

mock_decomposition() ->
    mock_decomposition([]).

%%--------------------------------------------------------------------
%% @doc Generates a mock decomposition with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_decomposition([{atom(), term()}]) -> mock_decomposition().

mock_decomposition(Options) ->
    Id = proplists:get_value(id, Options, <<"decomp_1">>),
    IsRoot = proplists:get_value(is_root, Options, false),
    Tasks = proplists:get_value(tasks, Options, []),

    #{
        id => Id,
        is_root => IsRoot,
        tasks => Tasks
    }.

%%--------------------------------------------------------------------
%% @doc Generates a root decomposition.
%%
%% @end
%%--------------------------------------------------------------------
-spec root_decomposition([task_id()]) -> mock_decomposition().

root_decomposition(Tasks) ->
    #{
        id => <<"main">>,
        is_root => true,
        tasks => Tasks
    }.

%%====================================================================
%% YAWL Spec Generators (wf_spec compatible)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a YAWL specification record.
%%
%% Returns a map compatible with wf_spec module.
%%
%% @end
%%--------------------------------------------------------------------
-spec yawl_spec() -> map().

yawl_spec() ->
    yawl_spec([]).

%%--------------------------------------------------------------------
%% @doc Generates a YAWL specification with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec yawl_spec([{atom(), term()}]) -> map().

yawl_spec(Options) ->
    Workflow = simple_workflow(Options),
    #{
        id => maps:get(id, Workflow),
        title => maps:get(name, Workflow),
        version => maps:get(version, Workflow, <<"1.0">>),
        root_net => maps:get(root_net, Workflow),
        tasks => maps:get(tasks, Workflow),
        places => [],
        transitions => [],
        decompositions => maps:get(decompositions, Workflow),
        flows => maps:get(flows, Workflow),
        conditions => maps:get(conditions, Workflow)
    }.

%%--------------------------------------------------------------------
%% @doc Creates a spec from a list of tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec spec_from_tasks([mock_task()]) -> map().

spec_from_tasks(Tasks) ->
    TaskIds = [maps:get(id, T) || T <- Tasks],
    TaskMap = maps:from_list([{maps:get(id, T), T} || T <- Tasks]),
    Flows = flow_list(TaskIds, sequential),

    #{
        id => <<"spec_from_tasks">>,
        title => <<"Generated from tasks">>,
        root_net => <<"main">>,
        tasks => TaskMap,
        places => [],
        transitions => [],
        decompositions => #{<<"main">> => root_decomposition(TaskIds)},
        flows => Flows,
        conditions => #{}
    }.

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a workflow structure.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_workflow(mock_workflow()) -> boolean().

is_valid_workflow(#{id := Id, name := Name, root_net := RootNet}) ->
    is_binary(Id) andalso is_binary(Name) andalso is_binary(RootNet);
is_valid_workflow(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Extracts task IDs from a task map.
%%
%% @end
%%--------------------------------------------------------------------
-spec task_ids(#{binary() => mock_task()}) -> [task_id()].

task_ids(TaskMap) ->
    maps:keys(TaskMap).

%%--------------------------------------------------------------------
%% @doc Gets a task by ID from the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task(mock_workflow(), task_id()) -> mock_task() | undefined.

get_task(#{tasks := Tasks}, TaskId) ->
    maps:get(TaskId, Tasks, undefined).

%%--------------------------------------------------------------------
%% @doc Adds a task to the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_task(mock_workflow(), mock_task()) -> mock_workflow().

add_task(Workflow = #{tasks := Tasks}, Task = #{id := Id}) ->
    Workflow#{tasks => Tasks#{Id => Task}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec flow_id(task_id(), task_id()) -> binary().

flow_id(From, To) ->
    <<From/binary, "_", To/binary>>.

%% @private
-spec pairs([T]) -> [{T, T}].

pairs([]) ->
    [];
pairs([_]) ->
    [];
pairs([A, B | Rest]) ->
    [{A, B} | pairs([B | Rest])].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test simple_workflow/0
simple_workflow_test() ->
    WF = simple_workflow(),
    ?assert(is_valid_workflow(WF)),
    ?assertEqual(<<"simple_wf">>, maps:get(id, WF)).

%% Test simple_workflow/1 with options
simple_workflow_options_test() ->
    WF = simple_workflow([{id, <<"custom">>}, {task_count, 5}]),
    ?assertEqual(<<"custom">>, maps:get(id, WF)),
    ?assertEqual(5, map_size(maps:get(tasks, WF))).

%% Test complex_workflow/0
complex_workflow_test() ->
    WF = complex_workflow(),
    ?assert(is_valid_workflow(WF)),
    ?assert(map_size(maps:get(tasks, WF)) >= 6).

%% Test empty_workflow/0
empty_workflow_test() ->
    WF = empty_workflow(),
    ?assertEqual(#{}, maps:get(tasks, WF)),
    ?assertEqual([], maps:get(flows, WF)).

%% Test approval_workflow/0
approval_workflow_test() ->
    WF = approval_workflow(),
    ?assert(is_valid_workflow(WF)),
    ?assertEqual(<<"approval_wf">>, maps:get(id, WF)).

%% Test parallel_workflow/0
parallel_workflow_test() ->
    WF = parallel_workflow(),
    ?assert(is_valid_workflow(WF)),
    ?assert(length(maps:get(flows, WF)) >= 6).

%% Test loop_workflow/0
loop_workflow_test() ->
    WF = loop_workflow(),
    ?assert(is_valid_workflow(WF)),
    ?assert(length(maps:get(flows, WF)) >= 3).

%% Test mock_task/0
mock_task_test() ->
    Task = mock_task(),
    ?assertEqual(<<"task_1">>, maps:get(id, Task)),
    ?assertEqual(atomic, maps:get(type, Task)).

%% Test atomic_task/1
atomic_task_test() ->
    Task = atomic_task(<<"my_task">>),
    ?assertEqual(<<"my_task">>, maps:get(id, Task)),
    ?assertEqual(atomic, maps:get(type, Task)).

%% Test composite_task/1
composite_task_test() ->
    Task = composite_task(<<"comp">>),
    ?assertEqual(composite, maps:get(type, Task)),
    ?assertEqual(<<"comp_decomp">>, maps:get(decomposition, Task)).

%% Test multi_instance_task/2
multi_instance_task_test() ->
    Task = multi_instance_task(<<"mi">>, {2, 5}),
    ?assertEqual(multi_instance, maps:get(type, Task)),
    ?assertEqual(2, maps:get(min_instances, Task)),
    ?assertEqual(5, maps:get(max_instances, Task)).

%% Test mock_condition/0
mock_condition_test() ->
    Cond = mock_condition(),
    ?assertEqual(input, maps:get(type, Cond)).

%% Test input_condition/1
input_condition_test() ->
    Cond = input_condition(<<"in">>),
    ?assertEqual(input, maps:get(type, Cond)),
    ?assertEqual(<<"in">>, maps:get(id, Cond)).

%% Test output_condition/1
output_condition_test() ->
    Cond = output_condition(<<"out">>),
    ?assertEqual(output, maps:get(type, Cond)),
    ?assertEqual(<<"out">>, maps:get(id, Cond)).

%% Test mock_flow/0
mock_flow_test() ->
    Flow = mock_flow(),
    ?assertEqual(<<"task_1">>, maps:get(from, Flow)),
    ?assertEqual(<<"task_2">>, maps:get(to, Flow)).

%% Test conditional_flow/3
conditional_flow_test() ->
    Flow = conditional_flow(<<"t1">>, <<"t2">>, <<"approved">>),
    ?assertEqual(<<"t1">>, maps:get(from, Flow)),
    ?assertEqual(<<"t2">>, maps:get(to, Flow)),
    ?assertNotEqual(undefined, maps:get(predicate, Flow)).

%% Test mock_decomposition/0
mock_decomposition_test() ->
    Decomp = mock_decomposition(),
    ?assertEqual(false, maps:get(is_root, Decomp)).

%% Test root_decomposition/1
root_decomposition_test() ->
    Decomp = root_decomposition([<<"t1">>, <<"t2">>]),
    ?assertEqual(true, maps:get(is_root, Decomp)),
    ?assertEqual([<<"t1">>, <<"t2">>], maps:get(tasks, Decomp)).

%% Test yawl_spec/0
yawl_spec_test() ->
    Spec = yawl_spec(),
    ?assert(is_binary(maps:get(id, Spec))),
    ?assert(is_binary(maps:get(title, Spec))).

%% Test spec_from_tasks/1
spec_from_tasks_test() ->
    Tasks = [atomic_task(<<"t1">>), atomic_task(<<"t2">>)],
    Spec = spec_from_tasks(Tasks),
    ?assertEqual(2, map_size(maps:get(tasks, Spec))),
    ?assertEqual(1, length(maps:get(flows, Spec))).

%% Test is_valid_workflow/1
is_valid_workflow_test() ->
    ?assert(is_valid_workflow(simple_workflow())),
    ?assertNot(is_valid_workflow(#{})),
    ?assertNot(is_valid_workflow(#{id => 1, name => <<"x">>, root_net => <<"y">>})).

%% Test task_ids/1
task_ids_test() ->
    Tasks = #{<<"t1">> => atomic_task(<<"t1">>),
              <<"t2">> => atomic_task(<<"t2">>)},
    Ids = task_ids(Tasks),
    ?assert(lists:sort([<<"t1">>, <<"t2">>]) =:= lists:sort(Ids)).

%% Test get_task/2
get_task_test() ->
    WF = simple_workflow(),
    Task = get_task(WF, <<"task_1">>),
    ?assertNotEqual(undefined, Task).

%% Test add_task/2
add_task_test() ->
    WF = empty_workflow(),
    NewTask = atomic_task(<<"new_task">>),
    WF2 = add_task(WF, NewTask),
    ?assertNotEqual(undefined, get_task(WF2, <<"new_task">>)).

-endif.
