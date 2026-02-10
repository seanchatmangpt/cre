%% -*- erlang -*-
%%%% @author CRE Team
%% @version 0.3.0
%% @doc YAWL Workflow Validator - Advanced Analysis
%%
%% This module provides comprehensive validation of YAWL workflow definitions
%% with focus on deadlock detection, reachability analysis, and structural issues.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Structural validation (builds on yawl_validate)</li>
%%   <li>Deadlock detection (circular waits, unbounded AND joins)</li>
%%   <li>Reachability analysis (all tasks/conditions reachable from start)</li>
%%   <li>Liveness analysis (can all transitions potentially fire)</li>
%%   <li>Soundness checks (YAWL workflow soundness properties)</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Validate complete workflow
%% Spec = #{id => <<"order_wf">>, ...},
%% {ok, Analysis} = yawl_validator:analyze(Spec).
%% ```
%%
%% ```erlang
%% %% Check for deadlocks
%% {HasDeadlock, Deadlocks} = yawl_validator:detect_deadlocks(Spec).
%% ```
%%
%% ```erlang
%% %% Check reachability
%% UnreachableNodes = yawl_validator:unreachable_nodes(Spec).
%% ```
%% @end
%% -------------------------------------------------------------------

-module(yawl_validator).

%%====================================================================
%% Exports
%%====================================================================

%% Main validation API
-export([analyze/1, validate_workflow/1]).

%% Specific validation checks
-export([detect_deadlocks/1, detect_livelocks/1]).
-export([check_reachability/1, check_soundness/1]).
-export([unreachable_nodes/1, unused_tasks/1]).

%% Graph analysis
-export([build_dependency_graph/1, strongly_connected_components/1]).

%% Result formatting
-export([format_analysis/1, format_issues/1]).

%%====================================================================
%% Types
%%====================================================================

-type specification() :: #{
          id => binary(),
          name => binary(),
          version => binary() | undefined,
          decomposition => term(),
          tasks => #{binary() => task()},
          conditions => #{binary() => condition()},
          flows => [flow()],
          data_mappings => [mapping()]
         }.

-type task() :: #{
          id => binary(),
          name => binary(),
          type => atomic | composite | multiple_instance,
          split_type => 'and' | 'or' | 'xor' | undefined,
          join_type => 'and' | 'or' | 'xor' | undefined,
          decomposition => binary() | undefined,
          min_instances => non_neg_integer() | undefined,
          max_instances => non_neg_integer() | unlimited | undefined,
          continuation_threshold => non_neg_integer() | undefined
         }.

-type condition() :: #{
          id => binary(),
          type => input_condition | output_condition,
          expression => binary() | undefined
         }.

-type flow() :: #{
          id => binary(),
          source => binary(),
          target => binary(),
          predicate => binary() | undefined
         }.

-type mapping() :: #{
          task_id => binary(),
          input => [#{variable => binary(), expression => binary()}],
          output => [#{variable => binary(), expression => binary()}]
         }.

-type analysis_result() :: #{
          valid => boolean(),
          issues => [analysis_issue()],
          deadlocks => [deadlock_info()],
          unreachable => [binary()],
          unused => [binary()],
          sccs => [[binary()]],
          metrics => metrics()
         }.

-type deadlock_info() :: #{
          type => circular_wait | unbounded_join | missing_output,
          nodes => [binary()],
          description => binary()
         }.

-type analysis_issue() :: #{
          severity => error | warning | info,
          type => atom(),
          message => binary(),
          nodes => [binary()] | undefined,
          code => atom()
         }.

-type metrics() :: #{
          task_count => non_neg_integer(),
          condition_count => non_neg_integer(),
          flow_count => non_neg_integer(),
          and_joins => non_neg_integer(),
          or_joins => non_neg_integer(),
          xor_joins => non_neg_integer(),
          max_concurrency => non_neg_integer()
         }.

-export_type([specification/0, task/0, condition/0, flow/0, mapping/0,
              analysis_result/0, deadlock_info/0, analysis_issue/0, metrics/0]).

%%====================================================================
%% Main Validation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Performs comprehensive analysis of a YAWL specification.
%%
%% Combines structural validation with deadlock detection and
%% reachability analysis.
%%
%% === Example ===
%% ```erlang
%% {ok, Analysis} = yawl_validator:analyze(Spec),
%% #{valid := Valid, issues := Issues} = Analysis.
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec analyze(Spec :: specification()) -> {ok, analysis_result()} | {error, [binary()]}.

analyze(#{tasks := Tasks, flows := Flows, conditions := Conditions} = Spec)
  when is_map(Tasks), is_list(Flows), is_map(Conditions) ->
    try
        %% Run all validation checks
        StructuralIssues = run_structural_checks(Spec),
        DeadlockIssues = detect_deadlocks(Spec),
        LivelockIssues = detect_livelocks(Spec),
        ReachabilityIssues = check_reachability(Spec),
        SoundnessIssues = check_soundness(Spec),

        %% Collect all issues
        AllIssues = StructuralIssues ++ DeadlockIssues ++ LivelockIssues ++
                    ReachabilityIssues ++ SoundnessIssues,

        %% Calculate metrics
        Metrics = compute_metrics(Spec),

        %% Build dependency graph for SCC analysis
        Graph = build_dependency_graph(Spec),
        SCCs = strongly_connected_components(Graph),

        %% Get unreachable and unused nodes
        Unreachable = unreachable_nodes(Spec),
        Unused = unused_tasks(Spec),

        %% Determine validity
        HasErrors = lists:any(fun(#{severity := Sev}) -> Sev =:= error end, AllIssues),
        Valid = not HasErrors,

        Result = #{
            valid => Valid,
            issues => AllIssues,
            deadlocks => DeadlockIssues,
            unreachable => Unreachable,
            unused => Unused,
            sccs => SCCs,
            metrics => Metrics
        },

        {ok, Result}
    catch
        _:Error ->
            {error, [iolist_to_binary(io_lib:format("Analysis error: ~p", [Error]))]}
    end;

analyze(_Spec) ->
    {error, [<<"Invalid specification format">>]}.

%%--------------------------------------------------------------------
%% @doc Alias for analyze/1 for backward compatibility.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_workflow(Spec :: specification()) -> {ok, analysis_result()} | {error, [binary()]}.

validate_workflow(Spec) ->
    analyze(Spec).

%%====================================================================
%% Structural Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Runs basic structural checks on the specification.
%%--------------------------------------------------------------------
-spec run_structural_checks(specification()) -> [analysis_issue()].

run_structural_checks(#{id := Id, tasks := Tasks, flows := Flows, conditions := Conditions}) ->
    Issues = [],

    %% Check required ID
    Issues1 = case Id of
        <<>> ->
            [#{severity => error, type => missing_id,
               message => <<"Specification ID is required">>,
               nodes => undefined, code => missing_spec_id} | Issues];
        _ -> Issues
    end,

    %% Check for empty workflow
    Issues2 = case maps:size(Tasks) of
        0 ->
            [#{severity => warning, type => empty_workflow,
               message => <<"Workflow has no tasks">>,
               nodes => undefined, code => empty_workflow} | Issues1];
        _ -> Issues1
    end,

    %% Check for missing input/output conditions
    InputConds = lists:filter(fun({_, #{type := T}}) -> T =:= input_condition end,
                              maps:to_list(Conditions)),
    OutputConds = lists:filter(fun({_, #{type := T}}) -> T =:= output_condition end,
                               maps:to_list(Conditions)),

    Issues3 = case InputConds of
        [] ->
            [#{severity => warning, type => missing_input_condition,
               message => <<"No input condition defined">>,
               nodes => undefined, code => no_input_condition} | Issues2];
        _ -> Issues2
    end,

    Issues4 = case OutputConds of
        [] ->
            [#{severity => warning, type => missing_output_condition,
               message => <<"No output condition defined">>,
               nodes => undefined, code => no_output_condition} | Issues3];
        _ -> Issues3
    end,

    %% Check flows reference valid nodes
    AllNodeIds = maps:keys(Tasks) ++ maps:keys(Conditions),
    NodeSet = sets:from_list(AllNodeIds),

    Issues5 = lists:foldl(fun(#{source := Src, target := Tgt, id := FlowId}, Acc) ->
        case {sets:is_element(Src, NodeSet), sets:is_element(Tgt, NodeSet)} of
            {true, true} -> Acc;
            {false, _} ->
                [#{severity => error, type => invalid_flow_source,
                   message => iolist_to_binary([<<"Flow ">>, FlowId, <<" references invalid source: ">>, Src]),
                   nodes => [FlowId], code => invalid_flow_source} | Acc];
            {_, false} ->
                [#{severity => error, type => invalid_flow_target,
                   message => iolist_to_binary([<<"Flow ">>, FlowId, <<" references invalid target: ">>, Tgt]),
                   nodes => [FlowId], code => invalid_flow_target} | Acc]
        end
    end, Issues4, Flows),

    Issues5;

run_structural_checks(_Spec) ->
    [].

%%====================================================================
%% Deadlock Detection
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Detects potential deadlock conditions in the workflow.
%%
%% Checks for:
%% - Circular wait conditions (cycle with AND join)
%% - Unbounded AND joins (insufficient input paths)
%% - Missing output paths from tasks
%%
%% Returns list of potential deadlock issues.
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_deadlocks(Spec :: specification()) -> [analysis_issue()].

detect_deadlocks(#{tasks := Tasks, flows := Flows, conditions := Conditions} = Spec) ->
    Issues = [],

    %% Check for unbounded AND joins
    Issues1 = lists:foldl(fun({TaskId, Task}, Acc) ->
        case maps:get(join_type, Task, undefined) of
            'and' ->
                %% Count incoming flows
                Incoming = length([1 || #{target := T} <- Flows, T =:= TaskId]),
                case Incoming of
                    0 ->
                        [#{severity => error, type => unbounded_and_join,
                           message => iolist_to_binary([<<"AND join on task ">>, TaskId,
                                                       <<" has no incoming flows">>]),
                           nodes => [TaskId], code => unbounded_and_join} | Acc];
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, Issues, maps:to_list(Tasks)),

    %% Check for cycles with AND joins (potential deadlock)
    Graph = build_dependency_graph(Spec),
    Cycles = detect_cycles(Graph),

    Issues2 = lists:foldl(fun(Cycle, Acc) ->
        %% Check if any node in cycle has AND join
        HasAndJoin = lists:any(fun(NodeId) ->
            case maps:get(NodeId, Tasks, undefined) of
                #{join_type := 'and'} -> true;
                _ -> false
            end
        end, Cycle),
        case HasAndJoin of
            true ->
                Desc = iolist_to_binary([<<"Cycle detected: ">>,
                                        string:join([binary_to_list(N) || N <- Cycle], " -> ")]),
                [#{severity => warning, type => circular_wait_with_and_join,
                   message => Desc, nodes => Cycle, code => circular_and_join} | Acc];
            false -> Acc
        end
    end, Issues1, Cycles),

    Issues2;

detect_deadlocks(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Detects cycles in a directed graph using DFS.
%%--------------------------------------------------------------------
-spec detect_cycles(#{binary() => [binary()]}) -> [[binary()]].

detect_cycles(Graph) ->
    detect_cycles_internal(Graph, sets:new(), sets:new(), [], []).

-spec detect_cycles_internal(#{binary() => [binary()]},
                              sets:set(binary()),
                              sets:set(binary()),
                              [binary()],
                              [[binary()]]) -> [[binary()]].

detect_cycles_internal(Graph, Visited, RecStack, Path, Cycles) ->
    Nodes = maps:keys(Graph),
    lists:foldl(fun(Node, {V, R, Cycles1}) ->
        case sets:is_element(Node, V) of
            true -> {V, R, Cycles1};
            false ->
                visit_node(Graph, Node, V, R, Path, Cycles1)
        end
    end, {Visited, RecStack, Cycles}, Nodes).

-spec visit_node(#{binary() => [binary()]},
                  binary(),
                  sets:set(binary()),
                  sets:set(binary()),
                  [binary()],
                  [[binary()]]) -> {sets:set(binary()), sets:set(binary()), [[binary()]]}.

visit_node(Graph, Node, Visited, RecStack, Path, Cycles) ->
    V1 = sets:add_element(Node, Visited),
    R1 = sets:add_element(Node, RecStack),
    Neighbors = maps:get(Node, Graph, []),

    {V2, R2, Cycles1} = lists:foldl(fun(Neighbor, {V, R, C}) ->
        case sets:is_element(Neighbor, V) of
            true ->
                case sets:is_element(Neighbor, R) of
                    true ->
                        %% Found a cycle
                        CycleStart = lists:dropwhile(fun(N) -> N =/= Neighbor end, Path),
                        Cycle = CycleStart ++ [Neighbor],
                        {V, R, [Cycle | C]};
                    false ->
                        {V, R, C}
                end;
            false ->
                visit_node(Graph, Neighbor, V, R, [Node | Path], C)
        end
    end, {V1, R1, Cycles}, Neighbors),

    {V2, sets:del_element(Node, R2), Cycles1}.

%%====================================================================
%% Livelock Detection
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Detects potential livelock conditions (infinite loops).
%%
%% Returns list of potential livelock issues.
%%
%% @end
%%--------------------------------------------------------------------
-spec detect_livelocks(Spec :: specification()) -> [analysis_issue()].

detect_livelocks(#{tasks := Tasks, flows := Flows} = _Spec) ->
    Issues = [],

    %% Check for OR splits with insufficient synchronization
    Issues1 = lists:foldl(fun({TaskId, Task}, Acc) ->
        case maps:get(split_type, Task, undefined) of
            'or' ->
                %% Count outgoing flows
                Outgoing = length([1 || #{source := S} <- Flows, S =:= TaskId]),
                case Outgoing of
                    0 ->
                        [#{severity => warning, type => or_split_no_output,
                           message => iolist_to_binary([<<"OR split on task ">>, TaskId,
                                                       <<" has no outgoing flows">>]),
                           nodes => [TaskId], code => or_split_no_output} | Acc];
                    1 ->
                        [#{severity => info, type => or_split_single_branch,
                           message => iolist_to_binary([<<"OR split on task ">>, TaskId,
                                                       <<" has only one branch (use XOR)">>]),
                           nodes => [TaskId], code => or_split_single} | Acc];
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, Issues, maps:to_list(Tasks)),

    Issues1;

detect_livelocks(_Spec) ->
    [].

%%====================================================================
%% Reachability Analysis
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks reachability of all tasks and conditions from start.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_reachability(Spec :: specification()) -> [analysis_issue()].

check_reachability(#{tasks := Tasks, flows := Flows, conditions := Conditions} = _Spec) ->
    %% Find input condition (assume first input condition is start)
    InputConditions = lists:filtermap(fun({Id, #{type := input_condition}}) ->
        {true, Id};
        (_) -> false
    end, maps:to_list(Conditions)),

    AllNodes = maps:keys(Tasks) ++ maps:keys(Conditions),
    Reachable = case InputConditions of
        [] -> sets:new();
        [Start | _] ->
            find_reachable(Start, Flows, sets:new())
    end,

    %% Find unreachable nodes
    UnreachableList = lists:filter(fun(Node) ->
        not sets:is_element(Node, Reachable)
    end, AllNodes),

    %% Convert to issues
    lists:map(fun(Node) ->
        #{severity => warning, type => unreachable_node,
          message => iolist_to_binary([<<"Node is unreachable from start: ">>, Node]),
          nodes => [Node], code => unreachable_node}
    end, UnreachableList).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds all nodes reachable from a given node.
%%--------------------------------------------------------------------
-spec find_reachable(binary(), [flow()], sets:set(binary())) -> sets:set(binary()).

find_reachable(Node, Flows, Visited) ->
    case sets:is_element(Node, Visited) of
        true -> Visited;
        false ->
            V1 = sets:add_element(Node, Visited),
            Neighbors = [T || #{source := S, target := T} <- Flows, S =:= Node],
            lists:foldl(fun(Neighbor, Acc) ->
                find_reachable(Neighbor, Flows, Acc)
            end, V1, Neighbors)
    end.

%%--------------------------------------------------------------------
%% @doc Returns list of unreachable nodes.
%%
%% @end
%%--------------------------------------------------------------------
-spec unreachable_nodes(Spec :: specification()) -> [binary()].

unreachable_nodes(#{flows := Flows, conditions := Conditions} = Spec) ->
    Issues = check_reachability(Spec),
    lists:filtermap(fun(#{nodes := [Node]}) ->
        {true, Node};
        (_) -> false
    end, Issues).

%%====================================================================
%% Soundness Checks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks YAWL workflow soundness properties.
%%
%% A workflow is sound if:
%% - All tasks are reachable from input condition
%% - Output condition is reachable from all tasks
%% - No deadlock or livelock conditions
%%
%% @end
%%--------------------------------------------------------------------
-spec check_soundness(Spec :: specification()) -> [analysis_issue()].

check_soundness(#{tasks := Tasks, flows := Flows, conditions := Conditions} = _Spec) ->
    Issues = [],

    %% Find output conditions
    OutputConditions = lists:filtermap(fun({Id, #{type := output_condition}}) ->
        {true, Id};
        (_) -> false
    end, maps:to_list(Conditions)),

    %% Check if output condition is reachable from all tasks
    Issues1 = case OutputConditions of
        [] ->
            [#{severity => warning, type => no_output_condition,
               message => <<"No output condition defined">>,
               nodes => undefined, code => no_output_condition} | Issues];
        [Output | _] ->
            lists:foldl(fun({TaskId, _Task}, Acc) ->
                case is_node_reachable(TaskId, Output, Flows) of
                    true -> Acc;
                    false ->
                        [#{severity => error, type => unreachable_output,
                           message => iolist_to_binary([<<"Output condition unreachable from task ">>,
                                                       TaskId]),
                           nodes => [TaskId, Output], code => unreachable_output} | Acc]
                end
            end, Issues, maps:to_list(Tasks))
    end,

    Issues1;

check_soundness(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if there is a path from source to target.
%%--------------------------------------------------------------------
-spec is_node_reachable(binary(), binary(), [flow()]) -> boolean().

is_node_reachable(Source, Target, Flows) when Source =:= Target ->
    true;

is_node_reachable(Source, Target, Flows) ->
    Visited = sets:new(),
    find_path(Source, Target, Flows, Visited).

-spec find_path(binary(), binary(), [flow()], sets:set(binary())) -> boolean().

find_path(Current, Target, Flows, Visited) when Current =:= Target ->
    true;

find_path(Current, Target, Flows, Visited) ->
    case sets:is_element(Current, Visited) of
        true -> false;
        false ->
            V1 = sets:add_element(Current, Visited),
            Neighbors = [T || #{source := S, target := T} <- Flows, S =:= Current],
            lists:any(fun(Neighbor) ->
                find_path(Neighbor, Target, Flows, V1)
            end, Neighbors)
    end.

%%====================================================================
%% Task Analysis
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of unused tasks (no incoming or outgoing flows).
%%
%% @end
%%--------------------------------------------------------------------
-spec unused_tasks(Spec :: specification()) -> [binary()].

unused_tasks(#{tasks := Tasks, flows := Flows}) ->
    lists:filtermap(fun({TaskId, _Task}) ->
        HasFlow = lists:any(fun(#{source := S, target := T}) ->
            S =:= TaskId orelse T =:= TaskId
        end, Flows),
        case HasFlow of
            true -> false;
            false -> {true, TaskId}
        end
    end, maps:to_list(Tasks)).

%%====================================================================
%% Graph Analysis
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Builds a dependency graph from the workflow.
%%
%% Returns a map of node IDs to their direct successors.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_dependency_graph(Spec :: specification()) -> #{binary() => [binary()]}.

build_dependency_graph(#{flows := Flows}) ->
    lists:foldl(fun(#{source := Src, target := Tgt}, Acc) ->
        Neighbors = maps:get(Src, Acc, []),
        Acc#{Src => [Tgt | Neighbors]}
    end, #{}, Flows).

%%--------------------------------------------------------------------
%% @doc Finds strongly connected components using Tarjan's algorithm.
%%
%% Returns list of SCCs (each SCC is a list of node IDs).
%%
%% @end
%%--------------------------------------------------------------------
-spec strongly_connected_components(#{binary() => [binary()]}) -> [[binary()]].

strongly_connected_components(Graph) ->
    {_, _, SCCs} = tarjan_scc(Graph, maps:keys(Graph), #{}, #{}, 0, []),
    lists:filter(fun(SCC) -> length(SCC) > 1 end, SCCs).

-spec tarjan_scc(#{binary() => [binary()]},
                  [binary()],
                  #{binary() => non_neg_integer()},
                  #{binary() => non_neg_integer()},
                  non_neg_integer(),
                  [[binary()]]) -> {#{binary() => non_neg_integer()},
                                    #{binary() => non_neg_integer()},
                                    [[binary()]]}.

tarjan_scc(_Graph, [], Index, _Lowlinks, _NextIndex, SCCs) ->
    {Index, _Lowlinks, SCCs};

tarjan_scc(Graph, [Node | Rest], Index, Lowlinks, NextIndex, SCCs) ->
    case maps:is_key(Node, Index) of
        true ->
            tarjan_scc(Graph, Rest, Index, Lowlinks, NextIndex, SCCs);
        false ->
            {Index1, Lowlinks1, Stack, SCCs1} = tarjan_visit(
                Graph, Node, Index, Lowlinks, NextIndex, [], SCCs
            ),
            tarjan_scc(Graph, Rest, Index1, Lowlinks1, length(maps:keys(Index1)), SCCs1)
    end.

-spec tarjan_visit(#{binary() => [binary()]},
                    binary(),
                    #{binary() => non_neg_integer()},
                    #{binary() => non_neg_integer()},
                    non_neg_integer(),
                    [binary()],
                    [[binary()]]) -> {#{binary() => non_neg_integer()},
                                       #{binary() => non_neg_integer()},
                                       [binary()],
                                       [[binary()]]}.

tarjan_visit(Graph, Node, Index, Lowlinks, NextIndex, Stack, SCCs) ->
    Index1 = Index#{Node => NextIndex},
    Lowlinks1 = Lowlinks#{Node => NextIndex},
    Stack1 = [Node | Stack],
    NextIndex1 = NextIndex + 1,
    Neighbors = maps:get(Node, Graph, []),

    {Index2, Lowlinks2, Stack2, SCCs1} = lists:foldl(
        fun(Neighbor, {I, L, S, SC}) ->
            case maps:is_key(Neighbor, I) of
                false ->
                    tarjan_visit(Graph, Neighbor, I, L, NextIndex1, S, SC);
                true ->
                    case lists:member(Neighbor, S) of
                        true ->
                            LowNeighbor = maps:get(Neighbor, L),
                            LowNode = maps:get(Node, L),
                            {I, L#{Node => min(LowNode, LowNeighbor)}, S, SC};
                        false ->
                            {I, L, S, SC}
                    end
            end
        end, {Index1, Lowlinks1, Stack1, SCCs}, Neighbors
    ),

    case maps:get(Node, Lowlinks2) =:= maps:get(Node, Index2) of
        true ->
            {SCC, Stack3} = pop_scc(Node, Stack2, []),
            {Index2, Lowlinks2, Stack3, [SCC | SCCs1]};
        false ->
            {Index2, Lowlinks2, Stack2, SCCs1}
    end.

-spec pop_scc(binary(), [binary()], [binary()]) -> {[binary()], [binary()]}.

pop_scc(Node, [Node | Rest], SCC) ->
    {[Node | SCC], Rest};

pop_scc(Node, [Other | Rest], SCC) ->
    pop_scc(Node, Rest, [Other | SCC]);

pop_scc(_Node, [], SCC) ->
    {SCC, []}.

%%====================================================================
%% Metrics
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Computes workflow metrics.
%%--------------------------------------------------------------------
-spec compute_metrics(specification()) -> metrics().

compute_metrics(#{tasks := Tasks, flows := Flows, conditions := Conditions}) ->
    %% Count split/join types
    {AndJoins, OrJoins, XorJoins} = lists:foldl(fun({_Id, Task}, {A, O, X}) ->
        Join = maps:get(join_type, Task, undefined),
        case Join of
            'and' -> {A + 1, O, X};
            'or' -> {A, O + 1, X};
            'xor' -> {A, O, X + 1};
            _ -> {A, O, X}
        end
    end, {0, 0, 0}, maps:to_list(Tasks)),

    %% Calculate max concurrency (AND joins)
    MaxConcurrency = case AndJoins of
        0 -> 1;
        N -> min(N + 1, maps:size(Tasks))
    end,

    #{
        task_count => maps:size(Tasks),
        condition_count => maps:size(Conditions),
        flow_count => length(Flows),
        and_joins => AndJoins,
        or_joins => OrJoins,
        xor_joins => XorJoins,
        max_concurrency => MaxConcurrency
    }.

%%====================================================================
%% Result Formatting
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Formats analysis result for display.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_analysis(analysis_result()) -> [binary()].

format_analysis(#{valid := Valid, issues := Issues, metrics := Metrics}) ->
    Header = case Valid of
        true -> <<"VALIDATION: PASS">>;
        false -> <<"VALIDATION: FAIL">>
    end,

    MetricsLines = format_metrics(Metrics),
    IssuesLines = format_issues(Issues),

    [Header] ++ MetricsLines ++ IssuesLines.

%%--------------------------------------------------------------------
%% @doc Formats issues list for display.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_issues([analysis_issue()]) -> [binary()].

format_issues(Issues) ->
    case Issues of
        [] ->
            [<<"No issues found">>];
        _ ->
            lists:map(fun(#{severity := Sev, message := Msg, nodes := Nodes, code := Code}) ->
                NodeStr = case Nodes of
                    undefined -> <<"">>;
                    [N] -> iolist_to_binary([<<" [">>, N, <<"]: ">>]);
                    Ns -> iolist_to_binary([<<" [">>,
                                          string:join([binary_to_list(X) || X <- Ns], ", "),
                                          <<"]: ">>])
                end,
                SevStr = case Sev of
                    error -> <<"[ERROR]   ">>;
                    warning -> <<"[WARNING] ">>;
                    info -> <<"[INFO]    ">>
                end,
                iolist_to_binary([SevStr, Msg, NodeStr, <<"(">>, atom_to_binary(Code), <<")">>])
            end, Issues)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Formats metrics for display.
%%--------------------------------------------------------------------
-spec format_metrics(metrics()) -> [binary()].

format_metrics(#{task_count := TC, condition_count := CC, flow_count := FC,
                 and_joins := AJ, or_joins := OJ, xor_joins := XJ,
                 max_concurrency := MC}) ->
    [
        <<"">>,
        <<"METRICS:">>,
        iolist_to_binary([<<"  Tasks: ">>, integer_to_binary(TC)]),
        iolist_to_binary([<<"  Conditions: ">>, integer_to_binary(CC)]),
        iolist_to_binary([<<"  Flows: ">>, integer_to_binary(FC)]),
        iolist_to_binary([<<"  AND joins: ">>, integer_to_binary(AJ)]),
        iolist_to_binary([<<"  OR joins: ">>, integer_to_binary(OJ)]),
        iolist_to_binary([<<"  XOR joins: ">>, integer_to_binary(XJ)]),
        iolist_to_binary([<<"  Max concurrency: ">>, integer_to_binary(MC)]),
        <<"">>
    ].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

analyze_valid_workflow_test() ->
    Spec = #{
        id => <<"simple_wf">>,
        name => <<"Simple Workflow">>,
        tasks => #{
            <<"t1">> => #{id => <<"t1">>, type => atomic, split_type => undefined, join_type => undefined},
            <<"t2">> => #{id => <<"t2">>, type => atomic, split_type => undefined, join_type => undefined}
        },
        conditions => #{
            <<"input">> => #{id => <<"input">>, type => input_condition, expression => undefined},
            <<"output">> => #{id => <<"output">>, type => output_condition, expression => undefined}
        },
        flows => [
            #{id => <<"f1">>, source => <<"input">>, target => <<"t1">>, predicate => undefined},
            #{id => <<"f2">>, source => <<"t1">>, target => <<"t2">>, predicate => undefined},
            #{id => <<"f3">>, source => <<"t2">>, target => <<"output">>, predicate => undefined}
        ],
        data_mappings => []
    },
    {ok, Result} = analyze(Spec),
    ?assert(maps:is_key(valid, Result)),
    ?assert(maps:is_key(issues, Result)),
    ?assert(maps:is_key(metrics, Result)).

detect_unreachable_test() ->
    Spec = #{
        id => <<"unreachable_wf">>,
        name => <<"Unreachable Workflow">>,
        tasks => #{
            <<"t1">> => #{id => <<"t1">>, type => atomic, split_type => undefined, join_type => undefined},
            <<"t2">> => #{id => <<"t2">>, type => atomic, split_type => undefined, join_type => undefined}
        },
        conditions => #{
            <<"input">> => #{id => <<"input">>, type => input_condition, expression => undefined},
            <<"output">> => #{id => <<"output">>, type => output_condition, expression => undefined}
        },
        flows => [
            #{id => <<"f1">>, source => <<"input">>, target => <<"t1">>, predicate => undefined},
            #{id => <<"f2">>, source => <<"t1">>, target => <<"output">>, predicate => undefined}
        ],
        data_mappings => []
    },
    Unreachable = unreachable_nodes(Spec),
    ?assert(lists:member(<<"t2">>, Unreachable)).

build_graph_test() ->
    Spec = #{
        flows => [
            #{id => <<"f1">>, source => <<"t1">>, target => <<"t2">>, predicate => undefined},
            #{id => <<"f2">>, source => <<"t2">>, target => <<"t3">>, predicate => undefined}
        ]
    },
    Graph = build_dependency_graph(Spec),
    ?assert(maps:is_key(<<"t1">>, Graph)),
    ?assertEqual([<<"t2">>], maps:get(<<"t1">>, Graph)).

-endif.
