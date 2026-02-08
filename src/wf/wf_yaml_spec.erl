%% -*- erlang -*-
%% @doc YAML 0.2 specification parser for YAWL workflows.
%%
%% Parses YAML format YAWL specifications (yawl_yaml_version: "0.2")
%% and converts them to the internal yawl_spec format compatible with
%% yawl_compile and the rest of the CRE system.
%%
%% == YAML Format ==
%%
%% ```yaml
%% yawl_yaml_version: "0.2"
%% specificationSet:
%%   yawl_schema_version: "2.1"
%%   uri: "workflow_id"
%%   metaData:
%%     title: "Workflow Title"
%%     version: "1.0"
%%   rootNet: "NetId"
%%   roles: [...]
%%   nets:
%%     - id: "NetId"
%%       type: "NetFacts"
%%       variables: [...]
%%       nodes: [...]
%%       flows: [...]
%%       subnets: [...]
%%       regions: [...]
%%   pattern_instances: [...]
%%   pattern_registry: {...}
%%   pattern_usage_index: {...}
%% ```
%%
%% == Examples ==
%%
%% ```erlang
%% > {ok, Spec} = wf_yaml_spec:from_yaml_file("workflow.yaml").
%% > wf_yaml_spec:id(Spec).
%% <<"workflow_id">>
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_yaml_spec).

%%====================================================================
%% Includes
%%====================================================================

%% Import record definitions - define locally since wf_spec doesn't export them
-record(flow_info, {
    from :: atom(),
    to :: atom(),
    predicate :: binary() | undefined
}).

-record(condition_info, {
    id :: binary(),
    type :: input | output,
    expression :: binary() | undefined
}).

-record(decomposition_info, {
    id :: binary(),
    is_root :: boolean(),
    tasks :: [atom()],
    input_condition :: binary(),
    output_condition :: binary()
}).

-record(task_info, {
    docs :: binary() | undefined,
    type :: atom(),
    split_type :: 'and' | 'or' | 'xor' | undefined,
    join_type :: 'and' | 'or' | 'xor' | undefined,
    decomposes_to :: binary() | undefined,
    cancellation_set :: [atom()],
    params :: #{binary() => term()},
    mi_params :: #{min_instances := non_neg_integer(),
                   max_instances := non_neg_integer() | unlimited,
                   continuation_threshold := non_neg_integer()} | undefined
}).

-type task_info() :: #task_info{}.
-type flow_info() :: #flow_info{}.
-type condition_info() :: #condition_info{}.
-type decomposition_info() :: #decomposition_info{}.
-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.

%%====================================================================
%% Exports
%%====================================================================

%% Parsing
-export([from_yaml/1, from_yaml_file/1]).

%% Accessors (compatible with wf_spec API)
-export([id/1, title/1, version/1, root_net/1]).
-export([tasks/1, task_doc/2, task_type/2]).
-export([places/1, transitions/1]).
-export([uri/1, meta/1, schema_types/1, nets/1]).
-export([tasks/2, task_name/3, join_split/3]).
-export([flows/3, variables/2, timer/3, mi/3]).
-export([split_type/2, join_type/2]).
-export([decomposition_nets/1, decomposition_net/2]).
-export([cancellation_set/2, cancellation_regions/1]).
-export([flows/1, flow_predicate/3]).
-export([task_params/2, task_param/3]).
-export([conditions/1, condition_expr/2]).
-export([all_decompositions/1, decomposition_tasks/2]).

%% YAML-specific accessors
-export([yaml_version/1, roles/1, pattern_instances/1, pattern_registry/1, pattern_usage_index/1]).
-export([net_variables/2, net_regions/2, net_subnets/2]).
-export([net_input_condition/2, net_output_condition/2, net_first_flow_target/2]).

%% Validation
-export([validate/1]).

%% Doctests
-ifdef(TEST).
-export([doctest_test/0]).
-endif.

%%====================================================================
%% Record Definitions (compatible with wf_spec)
%%====================================================================

%%====================================================================
%% Types
%%====================================================================

-type spec_id() :: binary().
-type task_id() :: atom().
-type place() :: atom().
-type transition() :: atom().

%% Extended yawl_spec record with YAML-specific fields
-record(yawl_yaml_spec, {
    id :: spec_id(),
    title :: binary(),
    version :: binary() | undefined,
    root_net :: binary(),
    tasks :: #{task_id() => task_info()},
    places :: [place()],
    transitions :: [transition()],
    decompositions :: #{binary() => decomposition_info()},
    flows :: [flow_info()],
    conditions :: #{binary() => condition_info()},
    %% YAML-specific fields
    yaml_version :: binary(),
    roles :: [binary()],
    pattern_instances :: [pattern_instance()],
    pattern_registry :: #{binary() => pattern_registry_entry()},
    pattern_usage_index :: #{binary() => [binary()]},
    net_variables :: #{binary() => [variable_def()]},
    net_regions :: #{binary() => [region_def()]},
    net_subnets :: #{binary() => [subnet_def()]}
}).

-type pattern_instance() :: #{
    id => binary(),
    pattern => binary(),
    net => binary(),
    label => binary() | undefined,
    %% Pattern-specific parameters (varies by pattern)
    split_task => binary() | undefined,
    merge_task => binary() | undefined,
    join_task => binary() | undefined,
    branches => [binary()] | undefined,
    waits_for => [binary()] | undefined,
    from => binary() | undefined,
    to => binary() | undefined,
    froms => [binary()] | undefined,
    at => binary() | undefined,
    choices => [binary()] | undefined,
    entry => binary() | undefined,
    body => [binary()] | undefined,
    exit_condition => binary() | undefined,
    m => non_neg_integer() | undefined,
    n => non_neg_integer() | undefined,
    threshold => non_neg_integer() | undefined,
    threshold_expr => binary() | undefined,
    instances => non_neg_integer() | undefined,
    instances_expr => binary() | undefined,
    total_instances => non_neg_integer() | undefined,
    stop_condition => binary() | undefined,
    complete_condition => binary() | undefined,
    cancel_event => binary() | undefined,
    events => [binary()] | undefined,
    enabled_only_in => binary() | undefined,
    consumed_in => binary() | undefined,
    race => [binary()] | undefined,
    cancel_rest => boolean() | undefined,
    winner_to => binary() | undefined,
    blocks_until => [binary()] | undefined,
    scope => binary() | undefined,
    join => binary() | undefined,
    mutex => binary() | undefined,
    protected => binary() | undefined,
    term => binary() | undefined,
    cancels_all => boolean() | undefined,
    call => binary() | undefined
}.

-type pattern_registry_entry() :: #{
    macro => binary()
}.

-type variable_def() :: #{
    name => binary(),
    type => binary(),
    initial => term()
}.

-type region_def() :: #{
    id => binary(),
    cancel_region => boolean(),
    description => binary() | undefined
}.

-type subnet_def() :: #{
    id => binary(),
    entry => binary(),
    exit => binary()
}.

-opaque yawl_yaml_spec() :: #yawl_yaml_spec{}.

-export_type([yawl_yaml_spec/0, pattern_instance/0, variable_def/0, region_def/0, subnet_def/0]).

%%====================================================================
%% Parsing Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Parses YAML from a binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_yaml(Yaml :: binary() | string()) ->
          {ok, yawl_yaml_spec()} | {error, term()}.

from_yaml(Yaml) when is_binary(Yaml) ->
    TempFile = filename:join(["/tmp", "yaml_" ++ integer_to_list(erlang:phash2(Yaml)) ++ ".yaml"]),
    try
        %% Ensure yamerl application is started
        case application:ensure_all_started(yamerl) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            Error -> throw({yamerl_start_error, Error})
        end,
        %% Parse YAML using yamerl_constr
        %% Write to temp file and parse
        ok = file:write_file(TempFile, Yaml),
        Docs = yamerl_constr:file(TempFile),
        file:delete(TempFile),
        case Docs of
            [] ->
                {error, empty_yaml};
            [Doc | _] ->
                %% yamerl_constr returns Erlang terms directly
                parse_specification_set(Doc);
            ParseError ->
                {error, {parse_error, ParseError}}
        end
    catch
        _:Reason ->
            file:delete(TempFile),
            {error, {yaml_parse_error, Reason}}
    end;
from_yaml(Yaml) when is_list(Yaml) ->
    %% Assume it's a file path
    from_yaml_file(Yaml).

%%--------------------------------------------------------------------
%% @doc Parses YAML from a file.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_yaml_file(FilePath :: file:filename_all()) ->
          {ok, yawl_yaml_spec()} | {error, term()}.

from_yaml_file(FilePath) when is_list(FilePath) ->
    try
        %% Ensure yamerl application is started
        case application:ensure_all_started(yamerl) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            Error -> throw({yamerl_start_error, Error})
        end,
        %% Parse YAML file using yamerl_constr
        Docs = yamerl_constr:file(FilePath),
        case Docs of
            [] ->
                {error, empty_yaml};
            [Doc | _] ->
                %% yamerl_constr returns Erlang terms directly
                parse_specification_set(Doc);
            ParseError ->
                {error, {parse_error, ParseError}}
        end
    catch
        _:Reason ->
            {error, {yaml_parse_error, Reason}}
    end.

%%====================================================================
%% Internal Parsing Functions
%%====================================================================

%% @private
parse_specification_set(Doc) ->
    %% Normalize yamerl output to maps
    DocMap = normalize_yaml_data(Doc),
    %% Check yawl_yaml_version
    YamlVersion = get_value(DocMap, [<<"yawl_yaml_version">>], <<"0.2">>),
    case YamlVersion of
        <<"0.2">> -> ok;
        _ -> throw({unsupported_version, YamlVersion})
    end,

    %% Get specificationSet
    SpecSet = get_value(DocMap, [<<"specificationSet">>], #{}),

    %% Extract metadata
    Uri = get_value(SpecSet, [<<"uri">>], <<"unnamed">>),
    MetaData = get_value(SpecSet, [<<"metaData">>], #{}),
    Title = get_value(MetaData, [<<"title">>], <<"Untitled">>),
    Version = get_value(MetaData, [<<"version">>], undefined),
    RootNet = get_value(SpecSet, [<<"rootNet">>], <<"main">>),
    Roles = get_value(SpecSet, [<<"roles">>], []),

    %% Parse nets
    NetsList = get_value(SpecSet, [<<"nets">>], []),
    {Decomps0, NetVars, NetRegions, NetSubnets} = parse_nets(NetsList),
    %% Mark root net
    Decomps = maps:map(fun(NetId, Decomp) ->
        IsRootNet = (NetId =:= RootNet),
        Decomp#decomposition_info{is_root = IsRootNet}
    end, Decomps0),

    %% Extract tasks from nets
    Tasks = extract_tasks_from_nets(NetsList),

    %% Parse flows
    Flows = extract_flows_from_nets(NetsList),

    %% Parse conditions
    Conditions = extract_conditions_from_nets(NetsList),

    %% Parse pattern_instances
    PatternInstances = get_value(SpecSet, [<<"pattern_instances">>], []),

    %% Parse pattern_registry
    PatternRegistry = get_value(SpecSet, [<<"pattern_registry">>], #{}),

    %% Parse pattern_usage_index
    PatternUsageIndex = get_value(SpecSet, [<<"pattern_usage_index">>], #{}),

    Spec = #yawl_yaml_spec{
        id = Uri,
        title = Title,
        version = Version,
        root_net = RootNet,
        tasks = Tasks,
        places = [],
        transitions = [],
        decompositions = Decomps,
        flows = Flows,
        conditions = Conditions,
        yaml_version = YamlVersion,
        roles = Roles,
        pattern_instances = PatternInstances,
        pattern_registry = PatternRegistry,
        pattern_usage_index = PatternUsageIndex,
        net_variables = NetVars,
        net_regions = NetRegions,
        net_subnets = NetSubnets
    },

    {ok, Spec}.

%% @private
parse_nets(NetsList) when is_list(NetsList) ->
    lists:foldl(fun(Net, {DecompsAcc, VarsAcc, RegionsAcc, SubnetsAcc}) ->
        NetId = get_value(Net, [<<"id">>], <<"net">>),
        Nodes = get_value(Net, [<<"nodes">>], []),
        Tasks = extract_task_ids(Nodes),
        {InputCond, OutputCond} = extract_input_output_conditions(Nodes),
        %% Determine if root from rootNet comparison (set in parse_specification_set)
        IsRoot = false, %% Will be set correctly in parse_specification_set

        Decomp = #decomposition_info{
            id = NetId,
            is_root = IsRoot,
            tasks = Tasks,
            input_condition = InputCond,
            output_condition = OutputCond
        },

        Vars = get_value(Net, [<<"variables">>], []),
        Regions = get_value(Net, [<<"regions">>], []),
        Subnets = get_value(Net, [<<"subnets">>], []),

        {
            DecompsAcc#{NetId => Decomp},
            VarsAcc#{NetId => Vars},
            RegionsAcc#{NetId => Regions},
            SubnetsAcc#{NetId => Subnets}
        }
    end, {#{}, #{}, #{}, #{}}, NetsList).

%% @private
extract_task_ids(Nodes) when is_list(Nodes) ->
    lists:filtermap(fun(Node) ->
        Kind = get_value(Node, [<<"kind">>], undefined),
        case Kind of
            <<"task">> ->
                TaskId = get_value(Node, [<<"id">>], undefined),
                {true, binary_to_atom(TaskId, utf8)};
            _ ->
                false
        end
    end, Nodes).

%% @private
%% Extract input and output condition IDs from net nodes.
%% Prefers inputCondition/outputCondition kinds; falls back to first/last condition.
extract_input_output_conditions(Nodes) when is_list(Nodes) ->
    {Input, Output} = lists:foldl(
        fun(Node, {InAcc, OutAcc}) ->
            Kind = get_value(Node, [<<"kind">>], undefined),
            Id = get_value(Node, [<<"id">>], undefined),
            case Kind of
                <<"inputCondition">> when Id =/= undefined ->
                    {Id, OutAcc};
                <<"outputCondition">> when Id =/= undefined ->
                    {InAcc, Id};
                <<"condition">> when Id =/= undefined ->
                    %% Use as fallback for entry/exit if not already set
                    NewIn = case InAcc of undefined -> Id; _ -> InAcc end,
                    NewOut = Id, %% Last condition wins for output
                    {NewIn, NewOut};
                _ ->
                    {InAcc, OutAcc}
            end
        end,
        {undefined, undefined},
        Nodes
    ),
    {
        case Input of undefined -> <<"Start">>; _ -> Input end,
        case Output of undefined -> <<"End">>; _ -> Output end
    }.

%% @private
extract_tasks_from_nets(NetsList) ->
    lists:foldl(fun(Net, Acc) ->
        Nodes = get_value(Net, [<<"nodes">>], []),
        lists:foldl(fun(Node, TaskAcc) ->
            Kind = get_value(Node, [<<"kind">>], undefined),
            case Kind of
                <<"task">> ->
                    TaskId = get_value(Node, [<<"id">>], undefined),
                    TaskIdAtom = binary_to_atom(TaskId, utf8),
                    TaskName = get_value(Node, [<<"name">>], TaskId),
                    TaskType = get_value(Node, [<<"taskType">>], <<"human">>),
                    TaskTypeAtom = case TaskType of
                        <<"human">> -> human;
                        <<"automated">> -> automated;
                        <<"service">> -> service;
                        _ -> human
                    end,
                    TaskInfo = #task_info{
                        docs = TaskName,
                        type = TaskTypeAtom,
                        split_type = undefined,
                        join_type = undefined,
                        decomposes_to = undefined,
                        cancellation_set = [],
                        params = #{},
                        mi_params = undefined
                    },
                    TaskAcc#{TaskIdAtom => TaskInfo};
                _ ->
                    TaskAcc
            end
        end, Acc, Nodes)
    end, #{}, NetsList).

%% @private
extract_flows_from_nets(NetsList) ->
    lists:foldl(fun(Net, Acc) ->
        Flows = get_value(Net, [<<"flows">>], []),
        lists:foldl(fun(Flow, FlowAcc) ->
            From = get_value(Flow, [<<"from">>], undefined),
            To = get_value(Flow, [<<"to">>], undefined),
            case From =/= undefined andalso To =/= undefined of
                true ->
                    Pred = get_value(Flow, [<<"predicate">>], undefined),
                    FlowInfo = #flow_info{
                        from = binary_to_atom(From, utf8),
                        to = binary_to_atom(To, utf8),
                        predicate = Pred
                    },
                    [FlowInfo | FlowAcc];
                false ->
                    FlowAcc
            end
        end, Acc, Flows)
    end, [], NetsList).

%% @private
extract_conditions_from_nets(NetsList) ->
    lists:foldl(fun(Net, Acc) ->
        Nodes = get_value(Net, [<<"nodes">>], []),
        lists:foldl(fun(Node, CondAcc) ->
            Kind = get_value(Node, [<<"kind">>], undefined),
            case Kind of
                <<"inputCondition">> ->
                    CondId = get_value(Node, [<<"id">>], undefined),
                    CondInfo = #condition_info{
                        id = CondId,
                        type = input,
                        expression = undefined
                    },
                    CondAcc#{CondId => CondInfo};
                <<"outputCondition">> ->
                    CondId = get_value(Node, [<<"id">>], undefined),
                    CondInfo = #condition_info{
                        id = CondId,
                        type = output,
                        expression = undefined
                    },
                    CondAcc#{CondId => CondInfo};
                <<"condition">> ->
                    CondId = get_value(Node, [<<"id">>], undefined),
                    CondInfo = #condition_info{
                        id = CondId,
                        type = input, %% Default
                        expression = undefined
                    },
                    CondAcc#{CondId => CondInfo};
                _ ->
                    CondAcc
            end
        end, Acc, Nodes)
    end, #{}, NetsList).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
%% Convert yamerl output (proplists) to maps
normalize_yaml_data({yamerl_map, Pairs}) ->
    maps:from_list([{normalize_key(K), normalize_yaml_data(V)} || {K, V} <- Pairs]);
normalize_yaml_data({yamerl_seq, Items}) ->
    [normalize_yaml_data(Item) || Item <- Items];
normalize_yaml_data(Value) when is_map(Value) ->
    maps:map(fun(_K, V) -> normalize_yaml_data(V) end, Value);
normalize_yaml_data(Value) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            maps:from_list([{normalize_key(K), normalize_yaml_data(V)} || {K, V} <- Value]);
        false ->
            case io_lib:printable_list(Value) of
                true -> list_to_binary(Value);
                false -> [normalize_yaml_data(Item) || Item <- Value]
            end
    end;
normalize_yaml_data(Value) ->
    Value.

%% @private
%% Detect proplist format (yamerl 0.10+ returns maps as [{K,V}|_])
is_proplist([]) -> true;
is_proplist([{K, _} | Rest]) when is_list(K); is_atom(K) -> is_proplist(Rest);
is_proplist(_) -> false.

%% @private
normalize_key(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
normalize_key(Binary) when is_binary(Binary) -> Binary;
normalize_key(String) when is_list(String) -> list_to_binary(String);
normalize_key(Other) -> Other.

%% @private
%% Get nested value from map/list structure
get_value(Map, [Key], Default) when is_map(Map) ->
    KeyBin = case is_atom(Key) of true -> atom_to_binary(Key, utf8); false -> Key end,
    maps:get(KeyBin, Map, Default);
get_value(Map, [Key | Rest], Default) when is_map(Map) ->
    KeyBin = case is_atom(Key) of true -> atom_to_binary(Key, utf8); false -> Key end,
    case maps:get(KeyBin, Map, undefined) of
        undefined -> Default;
        SubMap -> get_value(SubMap, Rest, Default)
    end;
get_value(_, _, Default) ->
    Default.

%%====================================================================
%% Accessor Functions (compatible with wf_spec API)
%%====================================================================

-spec id(Spec :: yawl_yaml_spec()) -> spec_id().
id(#yawl_yaml_spec{id = Id}) -> Id.

-spec title(Spec :: yawl_yaml_spec()) -> binary().
title(#yawl_yaml_spec{title = Title}) -> Title.

-spec version(Spec :: yawl_yaml_spec()) -> binary() | undefined.
version(#yawl_yaml_spec{version = Version}) -> Version.

-spec root_net(Spec :: yawl_yaml_spec()) -> binary().
root_net(#yawl_yaml_spec{root_net = RootNet}) -> RootNet.

-spec tasks(Spec :: yawl_yaml_spec()) -> [task_id()].
tasks(#yawl_yaml_spec{tasks = Tasks}) -> maps:keys(Tasks).

-spec task_doc(Spec :: yawl_yaml_spec(), TaskId :: task_id()) -> binary().
task_doc(#yawl_yaml_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{docs = Docs} -> Docs;
        _ -> <<"">>
    end.

-spec task_type(Spec :: yawl_yaml_spec(), TaskId :: task_id()) -> atom().
task_type(#yawl_yaml_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{type = Type} -> Type;
        _ -> human
    end.

-spec places(Spec :: yawl_yaml_spec()) -> [place()].
places(#yawl_yaml_spec{places = Places}) -> Places.

-spec transitions(Spec :: yawl_yaml_spec()) -> [transition()].
transitions(#yawl_yaml_spec{transitions = Transitions}) -> Transitions.

-spec uri(Spec :: yawl_yaml_spec()) -> binary().
uri(Spec) -> id(Spec).

-spec meta(Spec :: yawl_yaml_spec()) -> map().
meta(#yawl_yaml_spec{id = Id, title = Title, version = Version}) ->
    #{
        title => Title,
        version => Version,
        identifier => Id,
        persistent => false
    }.

-spec schema_types(Spec :: yawl_yaml_spec()) -> [atom()].
schema_types(_Spec) -> [].

-spec nets(Spec :: yawl_yaml_spec()) -> [binary()].
nets(#yawl_yaml_spec{decompositions = Decomps}) -> maps:keys(Decomps).

-spec tasks(Spec :: yawl_yaml_spec(), NetId :: binary()) -> [task_id()].
tasks(#yawl_yaml_spec{decompositions = Decomps}, NetId) ->
    case maps:get(NetId, Decomps, undefined) of
        #decomposition_info{tasks = Tasks} -> Tasks;
        _ -> []
    end.

-spec task_name(Spec :: yawl_yaml_spec(), NetId :: binary(), TaskId :: task_id()) -> binary().
task_name(Spec, _NetId, TaskId) -> task_doc(Spec, TaskId).

-spec join_split(Spec :: yawl_yaml_spec(), NetId :: binary(), TaskId :: task_id()) -> {atom(), atom()}.
join_split(#yawl_yaml_spec{tasks = Tasks}, _NetId, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{split_type = Split, join_type = Join} ->
            {Split, Join};
        _ ->
            {undefined, undefined}
    end.

-spec flows(Spec :: yawl_yaml_spec(), NetId :: binary(), TaskId :: task_id()) -> [flow_info()].
flows(#yawl_yaml_spec{flows = Flows}, _NetId, TaskId) ->
    lists:filter(fun(#flow_info{from = From}) -> From =:= TaskId end, Flows).

-spec variables(Spec :: yawl_yaml_spec(), NetId :: binary()) -> [variable_def()].
variables(#yawl_yaml_spec{net_variables = NetVars}, NetId) ->
    maps:get(NetId, NetVars, []).

-spec timer(Spec :: yawl_yaml_spec(), NetId :: binary(), TaskId :: task_id()) -> term() | undefined.
timer(_Spec, _NetId, _TaskId) -> undefined.

-spec mi(Spec :: yawl_yaml_spec(), NetId :: binary(), TaskId :: task_id()) -> mi_params() | undefined.
mi(#yawl_yaml_spec{tasks = Tasks}, _NetId, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{mi_params = MIParams} -> MIParams;
        _ -> undefined
    end.

-spec split_type(Spec :: yawl_yaml_spec(), TaskId :: task_id()) -> atom() | undefined.
split_type(#yawl_yaml_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{split_type = Split} -> Split;
        _ -> undefined
    end.

-spec join_type(Spec :: yawl_yaml_spec(), TaskId :: task_id()) -> atom() | undefined.
join_type(#yawl_yaml_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{join_type = Join} -> Join;
        _ -> undefined
    end.

-spec decomposition_nets(Spec :: yawl_yaml_spec()) -> [binary()].
decomposition_nets(Spec) -> nets(Spec).

-spec decomposition_net(Spec :: yawl_yaml_spec(), NetId :: binary()) -> decomposition_info() | undefined.
decomposition_net(#yawl_yaml_spec{decompositions = Decomps}, NetId) ->
    maps:get(NetId, Decomps, undefined).

-spec net_input_condition(Spec :: yawl_yaml_spec(), NetId :: binary()) -> binary().
net_input_condition(#yawl_yaml_spec{decompositions = Decomps}, NetId) ->
    case maps:get(NetId, Decomps, undefined) of
        #decomposition_info{input_condition = C} -> C;
        _ -> <<"Start">>
    end.

-spec net_output_condition(Spec :: yawl_yaml_spec(), NetId :: binary()) -> binary().
net_output_condition(#yawl_yaml_spec{decompositions = Decomps}, NetId) ->
    case maps:get(NetId, Decomps, undefined) of
        #decomposition_info{output_condition = C} -> C;
        _ -> <<"End">>
    end.

%% @doc Returns the first flow target from the net's input condition, or undefined.
%% Used to determine which pattern instance owns the net entry point (split_task matches).
-spec net_first_flow_target(Spec :: yawl_yaml_spec(), NetId :: binary()) -> atom() | undefined.
net_first_flow_target(Spec, NetId) ->
    InputCond = net_input_condition(Spec, NetId),
    InputAtom = input_cond_to_atom(InputCond),
    Flows = flows(Spec),
    case [To || #flow_info{from = F, to = To} <- Flows, F =:= InputAtom] of
        [FirstTo | _] -> FirstTo;
        [] -> undefined
    end.

input_cond_to_atom(undefined) -> undefined;
input_cond_to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
input_cond_to_atom(A) when is_atom(A) -> A;
input_cond_to_atom(_) -> undefined.

-spec cancellation_set(Spec :: yawl_yaml_spec(), TaskId :: task_id()) -> [task_id()].
cancellation_set(#yawl_yaml_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{cancellation_set = Set} -> Set;
        _ -> []
    end.

-spec cancellation_regions(Spec :: yawl_yaml_spec()) -> [binary()].
cancellation_regions(#yawl_yaml_spec{net_regions = NetRegions}) ->
    lists:usort(lists:flatten([maps:keys(Regions) || Regions <- maps:values(NetRegions)])).

-spec flows(Spec :: yawl_yaml_spec()) -> [flow_info()].
flows(#yawl_yaml_spec{flows = Flows}) -> Flows.

-spec flow_predicate(Spec :: yawl_yaml_spec(), NetId :: binary(), FlowId :: term()) -> binary() | undefined.
flow_predicate(#yawl_yaml_spec{flows = Flows}, _NetId, FlowId) when is_tuple(FlowId), tuple_size(FlowId) =:= 2 ->
    {From, To} = FlowId,
    FromA = to_atom(From),
    ToA = to_atom(To),
    case FromA =/= undefined andalso ToA =/= undefined of
        false -> undefined;
        true ->
            case lists:search(fun(#flow_info{from = F, to = T}) -> F =:= FromA andalso T =:= ToA end, Flows) of
                {value, #flow_info{predicate = P}} -> P;
                false -> undefined
            end
    end;
flow_predicate(_, _, _) ->
    undefined.

-spec task_params(Spec :: yawl_yaml_spec(), TaskId :: task_id()) -> map().
task_params(#yawl_yaml_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #task_info{params = Params} -> Params;
        _ -> #{}
    end.

-spec task_param(Spec :: yawl_yaml_spec(), TaskId :: task_id(), ParamName :: binary()) -> term() | undefined.
task_param(Spec, TaskId, ParamName) ->
    Params = task_params(Spec, TaskId),
    maps:get(ParamName, Params, undefined).

-spec conditions(Spec :: yawl_yaml_spec()) -> #{binary() => condition_info()}.
conditions(#yawl_yaml_spec{conditions = Conditions}) -> Conditions.

-spec condition_expr(Spec :: yawl_yaml_spec(), CondId :: binary()) -> binary() | undefined.
condition_expr(#yawl_yaml_spec{conditions = Conditions}, CondId) ->
    case maps:get(CondId, Conditions, undefined) of
        #condition_info{expression = Expr} -> Expr;
        _ -> undefined
    end.

-spec all_decompositions(Spec :: yawl_yaml_spec()) -> [{binary(), boolean(), [task_id()]}].
all_decompositions(#yawl_yaml_spec{decompositions = Decomps, root_net = RootNet}) ->
    maps:fold(fun(NetId, #decomposition_info{is_root = _IsRoot, tasks = Tasks}, Acc) ->
        IsRootNet = (NetId =:= RootNet),
        [{NetId, IsRootNet, Tasks} | Acc]
    end, [], Decomps).

-spec decomposition_tasks(Spec :: yawl_yaml_spec(), NetId :: binary()) -> [task_id()].
decomposition_tasks(Spec, NetId) -> tasks(Spec, NetId).

%%====================================================================
%% YAML-Specific Accessors
%%====================================================================

-spec yaml_version(Spec :: yawl_yaml_spec()) -> binary().
yaml_version(#yawl_yaml_spec{yaml_version = Version}) -> Version.

-spec roles(Spec :: yawl_yaml_spec()) -> [binary()].
roles(#yawl_yaml_spec{roles = Roles}) -> Roles.

-spec pattern_instances(Spec :: yawl_yaml_spec()) -> [pattern_instance()].
pattern_instances(#yawl_yaml_spec{pattern_instances = Instances}) -> Instances.

-spec pattern_registry(Spec :: yawl_yaml_spec()) -> #{binary() => pattern_registry_entry()}.
pattern_registry(#yawl_yaml_spec{pattern_registry = Registry}) -> Registry.

-spec pattern_usage_index(Spec :: yawl_yaml_spec()) -> #{binary() => [binary()]}.
pattern_usage_index(#yawl_yaml_spec{pattern_usage_index = Index}) -> Index.

-spec net_variables(Spec :: yawl_yaml_spec(), NetId :: binary()) -> [variable_def()].
net_variables(#yawl_yaml_spec{net_variables = NetVars}, NetId) ->
    maps:get(NetId, NetVars, []).

-spec net_regions(Spec :: yawl_yaml_spec(), NetId :: binary()) -> [region_def()].
net_regions(#yawl_yaml_spec{net_regions = NetRegions}, NetId) ->
    maps:get(NetId, NetRegions, []).

-spec net_subnets(Spec :: yawl_yaml_spec(), NetId :: binary()) -> [subnet_def()].
net_subnets(#yawl_yaml_spec{net_subnets = NetSubnets}, NetId) ->
    maps:get(NetId, NetSubnets, []).

%%====================================================================
%% Validation
%%====================================================================

-spec validate(Spec :: yawl_yaml_spec()) -> ok | {error, [term()]}.
validate(#yawl_yaml_spec{} = Spec) ->
    Errors = validate_root_net(Spec) ++
        validate_nets_nonempty(Spec) ++
        validate_pattern_registry(Spec),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

validate_root_net(#yawl_yaml_spec{root_net = RootNet, decompositions = Decomps}) ->
    case maps:is_key(RootNet, Decomps) of
        true -> [];
        false -> [{root_net_missing, RootNet}]
    end.

validate_nets_nonempty(#yawl_yaml_spec{decompositions = Decomps}) ->
    case maps:size(Decomps) of
        0 -> [{no_nets_defined}];
        _ -> []
    end.

validate_pattern_registry(#yawl_yaml_spec{pattern_instances = Instances, pattern_registry = Registry}) ->
    PatternNames = [to_binary(maps:get(pattern, I, maps:get(<<"pattern">>, I, <<>>))) || I <- Instances],
    Missing = [P || P <- lists:usort(PatternNames),
                    P =/= <<>>,
                    not maps:is_key(P, Registry)],
    [{pattern_not_in_registry, P} || P <- Missing].

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> unicode:characters_to_binary(L);
to_binary(_) -> <<>>.

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(_) -> undefined.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test parsing a simple YAML spec
parse_simple_yaml_test() ->
    Yaml = <<"yawl_yaml_version: \"0.2\"\nspecificationSet:\n  uri: \"test\"\n  rootNet: \"main\"\n  nets:\n    - id: main\n      nodes: []\n      flows: []">>,
    case from_yaml(Yaml) of
        {ok, Spec} ->
            ?assertEqual(<<"test">>, id(Spec)),
            ?assertEqual(<<"main">>, root_net(Spec)),
            ok;
        Error ->
            ct:fail("Failed to parse YAML: ~p", [Error])
    end.

-endif.
