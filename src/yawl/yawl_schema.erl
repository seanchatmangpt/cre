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
%% @doc YAWL Specification Parsing and Validation
%%
%% This module provides XML schema parsing and validation for YAWL
%% (Yet Another Workflow Language) 2.0 specifications.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Parse YAWL 2.0 XML specifications using xmerl</li>
%%   <li>Validate specification structure and semantics</li>
%%   <li>Extract workflow elements: tasks, conditions, flows, data mappings</li>
%%   <li>Convert to internal CRE workflow format</li>
%%   <li>Support for decomposition, net elements, and routing</li>
%% </ul>
%%
%% <h3>YAWL 2.0 Specification Structure</h3>
%%
%% The YAWL XML schema defines:
%% <ul>
%%   <li><b>specification:</b> Root element containing metadata and decomposition</li>
%%   <li><b>decomposition:</b> Net structure with tasks and conditions</li>
%%   <li><b>task:</b> Work unit with split/join types and decorators</li>
%%   <li><b>condition:</b> Flow control points (input/output)</li>
%%   <li><b>flow:</b> Connections between elements</li>
%%   <li><b>mapping:</b> Data input/output mappings</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Parse a YAWL specification from XML string
%% Xml = <<"<specification id=\"order_wf\"><name>Order Processing</name>...</specification>">>,
%% {ok, Spec} = yawl_schema:parse_specification(Xml).
%% ```
%%
%% ```erlang
%% %% Validate a parsed specification
%% {ok, Spec} = yawl_schema:parse_specification(File),
%% ok = yawl_schema:validate_specification(Spec).
%% ```
%%
%% ```erlang
%% %% Extract tasks from specification
%% Tasks = yawl_schema:get_tasks(Spec),
%% #{<<"Task1">> := #{type := atomic, name := <<"Process Order">>}} = Tasks.
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_schema).

%%====================================================================
%% Exports
%%====================================================================

%% YAWL Specification API
-export([parse_specification/1,
         validate_specification/1,
         get_root_decomposition/1,
         get_tasks/1,
         get_conditions/1,
         get_flows/1,
         get_data_mappings/1,
         to_internal_format/1,
         doctest_test/0]).

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
          type => atomic | composite | multi_instance,
          split_type => and_split | or_split | xor_split | undefined,
          join_type => and_join | or_join | xor_join | undefined,
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

-type validation_error() :: #{
          type => syntax | semantic | structure,
          message => binary(),
          location => binary() | undefined
         }.

-type parse_result() :: {ok, specification()} | {error, validation_error()}.

-export_type([specification/0, task/0, condition/0, flow/0, mapping/0, validation_error/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Parses a YAWL specification from a file path or XML string.
%%
%% Supports YAWL 2.0 XML format. Returns a specification map with
%% all extracted elements.
%%
%% Example:
%% ```erlang
%% 1> Xml = <<"<specification id=\"wf1\"><name>Test</name></specification>">>,
%% 1> {ok, Spec} = yawl_schema:parse_specification(Xml).
%% {ok,#{id => <<"wf1">>,name => <<"Test">>,...}}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_specification(FileOrXml :: file:filename_all() | binary()) ->
          parse_result().

parse_specification(FileOrXml) when is_list(FileOrXml) ->
    case filelib:is_file(FileOrXml) of
        true ->
            case file:read_file(FileOrXml) of
                {ok, XmlContent} ->
                    parse_xml(XmlContent);
                {error, Reason} ->
                    {error, #{type => syntax,
                              message => iolist_to_binary(["File read error: ",
                                                          file:format_error(Reason)]),
                              location => list_to_binary(FileOrXml)}}
            end;
        false ->
            %% Treat as XML string
            parse_xml(list_to_binary(FileOrXml))
    end;

parse_specification(XmlContent) when is_binary(XmlContent) ->
    parse_xml(XmlContent).

%%--------------------------------------------------------------------
%% @doc Validates a YAWL specification.
%%
%% Checks for:
%% - Structural integrity (all references valid)
%% - Split/join consistency
%% - No cycles in flow
%% - Required attributes present
%%
%% Returns `ok' or `{error, [validation_error()]}'.
%%
%% Example:
%% ```erlang
%% 1> Spec = #{id => <<"wf1">>, tasks => #{}, conditions => #{}, flows => []},
%% 1> ok = yawl_schema:validate_specification(Spec).
%% ok
%% ```
%% @end
%%--------------------------------------------------------------------
-spec validate_specification(Spec :: specification()) ->
          ok | {error, [validation_error()]}.

validate_specification(Spec) ->
    Errors = lists:flatten([
        validate_structure(Spec),
        validate_references(Spec),
        validate_split_join(Spec),
        validate_cycles(Spec)
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the root decomposition of a specification.
%%
%% The root decomposition contains the main net structure.
%%
%% Example:
%% ```erlang
%% 1> Spec = #{decomposition => #{id => <<"root">>, type => root}},
%% 1> {ok, Decomp} = yawl_schema:get_root_decomposition(Spec).
%% {ok,#{id => <<"root">>,type => root}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_root_decomposition(Spec :: specification()) ->
          {ok, term()} | {error, not_found}.

get_root_decomposition(#{decomposition := Decomposition}) when Decomposition =/= undefined ->
    {ok, Decomposition};
get_root_decomposition(_Spec) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Gets all tasks from a specification.
%%
%% Returns a map of task ID to task record.
%%
%% Example:
%% ```erlang
%% 1> Spec = #{tasks => #{<<"t1">> => #{id => <<"t1">>, name => <<"Task1">>, type => atomic}}},
%% 1> Tasks = yawl_schema:get_tasks(Spec).
%% #{<<"t1">> => #{id => <<"t1">>,name => <<"Task1">>,type => atomic}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_tasks(Spec :: specification()) -> #{binary() => task()}.

get_tasks(#{tasks := Tasks}) ->
    Tasks;
get_tasks(_Spec) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Gets all conditions from a specification.
%%
%% Returns a map of condition ID to condition record.
%%
%% Example:
%% ```erlang
%% 1> Spec = #{conditions => #{<<"c1">> => #{id => <<"c1">>, type => input_condition}}},
%% 1> Conds = yawl_schema:get_conditions(Spec).
%% #{<<"c1">> => #{id => <<"c1">>,type => input_condition}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_conditions(Spec :: specification()) -> #{binary() => condition()}.

get_conditions(#{conditions := Conditions}) ->
    Conditions;
get_conditions(_Spec) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Gets all flows from a specification.
%%
%% Returns a list of flow records representing connections.
%%
%% Example:
%% ```erlang
%% 1> Spec = #{flows => [#{id => <<"f1">>, source => <<"t1">>, target => <<"t2">>}]},
%% 1> Flows = yawl_schema:get_flows(Spec).
%% [#{id => <<"f1">>,source => <<"t1">>,target => <<"t2">>,predicate => undefined}]
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_flows(Spec :: specification()) -> [flow()].

get_flows(#{flows := Flows}) ->
    Flows;
get_flows(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @doc Gets all data mappings from a specification.
%%
%% Returns a list of data mapping records.
%%
%% Example:
%% ```erlang
%% 1> Spec = #{data_mappings => [#{task_id => <<"t1">>, input => [], output => []}]},
%% 1> Maps = yawl_schema:get_data_mappings(Spec).
%% [#{input => [],output => [],task_id => <<"t1">>}]
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_data_mappings(Spec :: specification()) -> [mapping()].

get_data_mappings(#{data_mappings := Mappings}) ->
    Mappings;
get_data_mappings(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @doc Converts a parsed specification to internal CRE format.
%%
%% Returns a cre_yawl workflow record compatible with CRE execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_internal_format(Spec :: specification()) ->
          {ok, term()} | {error, term()}.

to_internal_format(#{id := Id} = Spec) ->
    try
        %% Create workflow record
        Workflow = cre_yawl:new_workflow(Id),

        %% Add tasks
        Tasks = maps:to_list(maps:get(tasks, Spec, #{})),
        Workflow1 = lists:foldl(fun({TaskId, Task}, Acc) ->
            cre_yawl:add_task(Acc, list_to_binary(TaskId), task_to_proplist(Task))
        end, Workflow, Tasks),

        %% Add conditions
        Conditions = maps:to_list(maps:get(conditions, Spec, #{})),
        Workflow2 = lists:foldl(fun({CondId, Condition}, Acc) ->
            cre_yawl:add_condition(Acc, list_to_binary(CondId),
                                  maps:get(expression, Condition, <<>>))
        end, Workflow1, Conditions),

        %% Add connections (flows)
        Flows = maps:get(flows, Spec, []),
        Workflow3 = lists:foldl(fun(Flow, Acc) ->
            cre_yawl:connect(Acc,
                            maps:get(source, Flow),
                            maps:get(target, Flow))
        end, Workflow2, Flows),

        %% Set split/join types
        Workflow4 = lists:foldl(fun({TaskId, Task}, Acc) ->
            Acc1 = case maps:get(split_type, Task) of
                undefined -> Acc;
                SplitType -> cre_yawl:set_split_type(Acc, TaskId, SplitType)
            end,
            case maps:get(join_type, Task) of
                undefined -> Acc1;
                JoinType -> cre_yawl:set_join_type(Acc1, TaskId, JoinType)
            end
        end, Workflow3, Tasks),

        {ok, Workflow4}
    catch
        Kind:Reason ->
            {error, {Kind, Reason}}
    end.

%%====================================================================
%% Internal Functions - Parsing
%%====================================================================

%% @private
%% @doc Parses XML content using xmerl.
-spec parse_xml(binary()) -> parse_result().

parse_xml(XmlContent) ->
    try
        %% Parse XML with xmerl
        {XmlRoot, _Rest} = xmerl_scan:string(binary_to_list(XmlContent),
                                               [{namespace_conformant, true}]),

        %% Extract specification
        case extract_specification(XmlRoot) of
            {ok, Spec} -> {ok, Spec};
            {error, ExtractReason} ->
                {error, #{type => syntax,
                          message => ExtractReason,
                          location => <<>>}}
        end
    catch
        _:ParseReason ->
            {error, #{type => syntax,
                      message => iolist_to_binary(["XML parse error: ",
                                                  io_lib:format("~p", [ParseReason])]),
                      location => <<>>}}
    end.

%% @private
%% @doc Extracts specification from XML element.
-spec extract_specification(term()) -> {ok, specification()} | {error, binary()}.

extract_specification({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}) ->
    %% Extract metadata
    {SpecId, SpecName, SpecVersion} = extract_spec_metadata({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}),

    %% Extract root decomposition
    Decomposition = extract_decomposition({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}),

    %% Extract tasks
    Tasks = extract_tasks({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}),

    %% Extract conditions
    Conditions = extract_conditions({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}),

    %% Extract flows
    Flows = extract_flows({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}),

    %% Extract data mappings
    DataMappings = extract_data_mappings({xmlElement, specification, _Attrs, _Namespace, _Content, _Context, _Line}),

    {ok, #{
        id => SpecId,
        name => SpecName,
        version => SpecVersion,
        decomposition => Decomposition,
        tasks => Tasks,
        conditions => Conditions,
        flows => Flows,
        data_mappings => DataMappings
     }};

extract_specification(_) ->
    {error, <<"Root element must be 'specification'">>}.

%% @private
%% @doc Extracts specification metadata.
-spec extract_spec_metadata(term()) -> {binary(), binary(), binary() | undefined}.

extract_spec_metadata(XmlElement) ->
    %% Get attributes
    Id = get_xml_attribute(XmlElement, <<"id">>, <<"unnamed_spec">>),
    Name = get_xml_attribute(XmlElement, <<"name">>, <<"Unnamed Specification">>),
    Version = get_xml_attribute(XmlElement, <<"version">>, undefined),
    {list_to_binary(Id), list_to_binary(Name),
     case Version of undefined -> undefined; V -> list_to_binary(V) end}.

%% @private
%% @doc Gets an XML attribute value.
-spec get_xml_attribute(term(), binary(), term()) -> binary() | term().

get_xml_attribute({xmlElement, _Name, Attributes, _NS, _Content, _Context, _Line},
                 AttrName, Default) ->
    case lists:keyfind(AttrName, 1, Attributes) of
        {_, _, Value} when is_list(Value) -> list_to_binary(Value);
        {_, _, Value} -> Value;
        false -> Default
    end;
get_xml_attribute(_, _, Default) ->
    Default.

%% @private
%% @doc Extracts decomposition from XML.
-spec extract_decomposition(term()) -> term() | undefined.

extract_decomposition({xmlElement, _Name, _Attrs, _NS, Content, _Context, _Line}) ->
    case find_element(Content, <<"decomposition">>) of
        {ok, DecompElement} ->
            %% Extract decomposition ID
            DecompId = get_xml_attribute(DecompElement, <<"id">>, <<"root_decomp">>),
            #{
                id => list_to_binary(DecompId),
                type => root,
                nets => extract_nets(Content)
            };
        {error, _} ->
            undefined
    end;
extract_decomposition(_) ->
    undefined.

%% @private
%% @doc Extracts nets from content.
-spec extract_nets([term()]) -> [term()].

extract_nets(Content) ->
    lists:filtermap(fun(Elem) ->
        case Elem of
            {xmlElement, <<"net">>, _Attrs, _NS, _NetContent, _Context, _Line} ->
                {true, extract_net_id(Elem)};
            _ ->
                false
        end
    end, Content).

%% @private
%% @doc Extracts net ID.
-spec extract_net_id(term()) -> binary().

extract_net_id(XmlElement) ->
    list_to_binary(get_xml_attribute(XmlElement, <<"id">>, <<"net">>)).

%% @private
%% @doc Finds an element by tag name in content list.
-spec find_element([term()], binary()) -> {ok, term()} | {error, not_found}.

find_element(Content, TagName) ->
    case lists:search(fun(Elem) ->
        case Elem of
            {xmlElement, TagName, _, _, _, _, _} -> true;
            _ -> false
        end
    end, Content) of
        {value, Found} -> {ok, Found};
        false -> {error, not_found}
    end.

%% @private
%% @doc Extracts all tasks from XML.
-spec extract_tasks(term()) -> #{binary() => task()}.

extract_tasks({xmlElement, _Name, _Attrs, _NS, Content, _Context, _Line}) ->
    lists:foldl(fun(Elem, Acc) ->
        case Elem of
            {xmlElement, <<"task">>, TaskAttrs, _, TaskContent, _, _} ->
                Task = parse_task({xmlElement, <<"task">>, TaskAttrs, _NS, TaskContent, _Context, _Line}),
                TaskId = maps:get(id, Task),
                Acc#{TaskId => Task};
            _ ->
                Acc
        end
    end, #{}, Content);
extract_tasks(_) ->
    #{}.

%% @private
%% @doc Parses a task element.
-spec parse_task(term()) -> task().

parse_task({xmlElement, <<"task">>, Attrs, _NS, Content, _Context, _Line}) ->
    Id = list_to_binary(get_xml_attribute({xmlElement, <<"task">>, Attrs, _NS, Content, _Context, _Line}, <<"id">>, <<>>)),
    Name = list_to_binary(get_xml_attribute({xmlElement, <<"task">>, Attrs, _NS, Content, _Context, _Line}, <<"name">>, <<"Unnamed Task">>)),

    %% Extract task type
    Type = extract_task_type(Content),

    %% Extract split type
    SplitType = extract_split_type(Content),

    %% Extract join type
    JoinType = extract_join_type(Content),

    %% Extract decomposition reference
    DecompRef = extract_decomposition_ref(Content),

    %% Extract multi-instance parameters
    {MinInst, MaxInst, Threshold} = extract_multi_instance_params(Content),

    #{
        id => Id,
        name => Name,
        type => Type,
        split_type => SplitType,
        join_type => JoinType,
        decomposition => DecompRef,
        min_instances => MinInst,
        max_instances => MaxInst,
        continuation_threshold => Threshold
    }.

%% @private
%% @doc Extracts task type from content.
-spec extract_task_type([term()]) -> atomic | composite | multi_instance.

extract_task_type(Content) ->
    case find_element(Content, <<"decomposition">>) of
        {ok, _} -> composite;
        {error, _} ->
            case find_element(Content, <<"instanceInfo">>) of
                {ok, _} -> multi_instance;
                {error, _} -> atomic
            end
    end.

%% @private
%% @doc Extracts split type from flow content.
-spec extract_split_type([term()]) -> and_split | or_split | xor_split | undefined.

extract_split_type(Content) ->
    case find_element(Content, <<"flowsInto">>) of
        {ok, {xmlElement, _, FlowsAttrs, _, _, _, _}} ->
            case get_xml_attribute({xmlElement, <<"flowsInto">>, FlowsAttrs, [], [], [], 1},
                                  <<"splitType">>, undefined) of
                <<"AND">> -> and_split;
                <<"OR">> -> or_split;
                <<"XOR">> -> xor_split;
                _ -> undefined
            end;
        {error, _} ->
            undefined
    end.

%% @private
%% @doc Extracts join type from task attributes or content.
-spec extract_join_type([term()]) -> and_join | or_join | xor_join | undefined.

extract_join_type(Content) ->
    case find_element(Content, <<"joining">>) of
        {ok, {xmlElement, _, JoinAttrs, _, _, _, _}} ->
            case get_xml_attribute({xmlElement, <<"joining">>, JoinAttrs, [], [], [], 1},
                                  <<"joinType">>, undefined) of
                <<"AND">> -> and_join;
                <<"OR">> -> or_join;
                <<"XOR">> -> xor_join;
                _ -> undefined
            end;
        {error, _} ->
            undefined
    end.

%% @private
%% @doc Extracts decomposition reference from task content.
-spec extract_decomposition_ref([term()]) -> binary() | undefined.

extract_decomposition_ref(Content) ->
    case find_element(Content, <<"decomposition">>) of
        {ok, {xmlElement, _, Attrs, _, _, _, _}} ->
            case get_xml_attribute({xmlElement, <<"decomposition">>, Attrs, [], [], [], 1},
                                  <<"id">>, undefined) of
                undefined -> undefined;
                Ref -> list_to_binary(Ref)
            end;
        {error, _} ->
            undefined
    end.

%% @private
%% @doc Extracts multi-instance parameters.
-spec extract_multi_instance_params([term()]) ->
          {non_neg_integer() | undefined, non_neg_integer() | unlimited | undefined,
           non_neg_integer() | undefined}.

extract_multi_instance_params(Content) ->
    case find_element(Content, <<"instanceInfo">>) of
        {ok, {xmlElement, _, _, _, InfoContent, _, _}} ->
            Min = extract_instance_param(InfoContent, <<"min">>),
            Max = extract_instance_param(InfoContent, <<"max">>),
            Threshold = extract_instance_param(InfoContent, <<"continuationThreshold">>),
            {Min, Max, Threshold};
        {error, _} ->
            {undefined, undefined, undefined}
    end.

%% @private
%% @doc Extracts instance parameter value.
-spec extract_instance_param([term()], binary()) ->
          non_neg_integer() | unlimited | undefined.

extract_instance_param(Content, ParamName) ->
    case find_element(Content, ParamName) of
        {ok, {xmlElement, _, _, _, [Value], _, _}} when is_list(Value) ->
            case string:to_integer(Value) of
                {Int, _} when Int >= 0 -> Int;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

%% @private
%% @doc Extracts all conditions from XML.
-spec extract_conditions(term()) -> #{binary() => condition()}.

extract_conditions({xmlElement, _Name, _Attrs, _NS, Content, _Context, _Line}) ->
    lists:foldl(fun(Elem, Acc) ->
        case Elem of
            {xmlElement, <<"condition">>, Attrs, _, CondContent, _, _} ->
                Id = list_to_binary(get_xml_attribute({xmlElement, <<"condition">>, Attrs, _NS, CondContent, _Context, _Line},
                                                    <<"id">>, <<>>)),
                Type = extract_condition_type(Elem),
                Expression = extract_condition_expression(CondContent),
                Acc#{Id => #{
                    id => Id,
                    type => Type,
                    expression => Expression
                }};
            _ ->
                Acc
        end
    end, #{}, Content);
extract_conditions(_) ->
    #{}.

%% @private
%% @doc Determines condition type.
-spec extract_condition_type(term()) -> input_condition | output_condition.

extract_condition_type({xmlElement, <<"condition">>, Attrs, _NS, _Content, _Context, _Line}) ->
    case get_xml_attribute({xmlElement, <<"condition">>, Attrs, _NS, _Content, _Context, _Line},
                          <<"type">>, <<"input">>) of
        <<"input">> -> input_condition;
        <<"output">> -> output_condition;
        _ -> input_condition
    end;
extract_condition_type(_) ->
    input_condition.

%% @private
%% @doc Extracts condition expression.
-spec extract_condition_expression([term()]) -> binary() | undefined.

extract_condition_expression([]) ->
    undefined;
extract_condition_expression([{xmlCharacters, Expr} | _]) when is_list(Expr) ->
    list_to_binary(string:trim(Expr));
extract_condition_expression([_ | Rest]) ->
    extract_condition_expression(Rest).

%% @private
%% @doc Extracts all flows from XML.
-spec extract_flows(term()) -> [flow()].

extract_flows({xmlElement, _Name, _Attrs, _NS, Content, _Context, _Line}) ->
    lists:foldl(fun(Elem, Acc) ->
        case Elem of
            {xmlElement, <<"flow">>, Attrs, _, FlowContent, _, _} ->
                Flow = parse_flow({xmlElement, <<"flow">>, Attrs, _NS, FlowContent, _Context, _Line}),
                [Flow | Acc];
            _ ->
                Acc
        end
    end, [], Content);
extract_flows(_) ->
    [].

%% @private
%% @doc Parses a flow element.
-spec parse_flow(term()) -> flow().

parse_flow({xmlElement, <<"flow">>, Attrs, _NS, _Content, _Context, _Line}) ->
    Id = list_to_binary(get_xml_attribute({xmlElement, <<"flow">>, Attrs, _NS, _Content, _Context, _Line},
                                         <<"id">>, crypto:hash(md5, term_to_binary(erlang:timestamp())))),
    Source = list_to_binary(get_xml_attribute({xmlElement, <<"flow">>, Attrs, _NS, _Content, _Context, _Line},
                                            <<"source">>, <<>>)),
    Target = list_to_binary(get_xml_attribute({xmlElement, <<"flow">>, Attrs, _NS, _Content, _Context, _Line},
                                            <<"target">>, <<>>)),
    Predicate = extract_flow_predicate(Attrs),
    #{
        id => Id,
        source => Source,
        target => Target,
        predicate => Predicate
    }.

%% @private
%% @doc Extracts flow predicate from attributes.
-spec extract_flow_predicate([term()]) -> binary() | undefined.

extract_flow_predicate(Attrs) ->
    case lists:keyfind(<<"predicate">>, 1, Attrs) of
        {_, _, Value} when is_list(Value) -> list_to_binary(Value);
        _ -> undefined
    end.

%% @private
%% @doc Extracts data mappings from XML.
-spec extract_data_mappings(term()) -> [mapping()].

extract_data_mappings({xmlElement, _Name, _Attrs, _NS, Content, _Context, _Line}) ->
    lists:foldl(fun(Elem, Acc) ->
        case Elem of
            {xmlElement, <<"mapping">>, Attrs, _, MapContent, _, _} ->
                Mapping = parse_mapping({xmlElement, <<"mapping">>, Attrs, _NS, MapContent, _Context, _Line}),
                [Mapping | Acc];
            _ ->
                Acc
        end
    end, [], Content);
extract_data_mappings(_) ->
    [].

%% @private
%% @doc Parses a mapping element.
-spec parse_mapping(term()) -> mapping().

parse_mapping({xmlElement, <<"mapping">>, Attrs, _NS, Content, _Context, _Line}) ->
    TaskId = list_to_binary(get_xml_attribute({xmlElement, <<"mapping">>, Attrs, _NS, Content, _Context, _Line},
                                            <<"task">>, <<>>)),
    Input = extract_mapping_content(Content, <<"input">>),
    Output = extract_mapping_content(Content, <<"output">>),
    #{
        task_id => TaskId,
        input => Input,
        output => Output
    }.

%% @private
%% @doc Extracts input/output mapping content.
-spec extract_mapping_content([term()], binary()) ->
          [#{variable => binary(), expression => binary()}].

extract_mapping_content(Content, Type) ->
    case find_element(Content, Type) of
        {ok, {xmlElement, _, _, _, MapContent, _, _}} ->
            lists:foldl(fun(Elem, Acc) ->
                case Elem of
                    {xmlElement, <<"param">>, _Attrs, _, _, _, _} ->
                        Var = list_to_binary(get_xml_attribute(Elem, <<"variable">>, <<>>)),
                        Expr = list_to_binary(get_xml_attribute(Elem, <<"expression">>, <<>>)),
                        [#{variable => Var, expression => Expr} | Acc];
                    _ ->
                        Acc
                end
            end, [], MapContent);
        {error, _} ->
            []
    end.

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%% @private
%% @doc Validates specification structure.
-spec validate_structure(specification()) -> [validation_error()].

validate_structure(#{id := Id, tasks := Tasks, conditions := Conditions}) ->
    Errors = [],
    %% Check if specification has required fields
    Errors1 = case Id of
        <<>> -> [#{type => structure,
                   message => <<"Specification ID is required">>,
                   location => <<"specification">>} | Errors];
        _ -> Errors
    end,
    %% Check if tasks have valid IDs
    Errors2 = maps:fold(fun(TaskId, _Task, Acc) ->
        case TaskId of
            <<>> -> [#{type => structure,
                       message => <<"Task ID is required">>,
                       location => <<"task">>} | Acc];
            _ -> Acc
        end
    end, Errors1, Tasks),
    %% Check if conditions have valid IDs
    maps:fold(fun(CondId, _Cond, Acc) ->
        case CondId of
            <<>> -> [#{type => structure,
                       message => <<"Condition ID is required">>,
                       location => <<"condition">>} | Acc];
            _ -> Acc
        end
    end, Errors2, Conditions).

%% @private
%% @doc Validates element references.
-spec validate_references(specification()) -> [validation_error()].

validate_references(#{flows := Flows, tasks := Tasks, conditions := Conditions}) ->
    AllIds = maps:keys(Tasks) ++ maps:keys(Conditions),
    lists:foldl(fun(#{source := Source, target := Target} = _Flow, Acc) ->
        Acc1 = case lists:member(Source, AllIds) of
            false -> [#{type => semantic,
                       message => iolist_to_binary([<<"Source not found: ">>, Source]),
                       location => Source} | Acc];
            true -> Acc
        end,
        case lists:member(Target, AllIds) of
            false -> [#{type => semantic,
                       message => iolist_to_binary([<<"Target not found: ">>, Target]),
                       location => Target} | Acc1];
            true -> Acc1
        end
    end, [], Flows).

%% @private
%% @doc Validates split/join consistency.
-spec validate_split_join(specification()) -> [validation_error()].

validate_split_join(#{tasks := Tasks, flows := Flows}) ->
    maps:fold(fun(TaskId, #{split_type := SplitType}, Acc) ->
        OutgoingCount = length([F || #{source := S} = F <- Flows, S =:= TaskId]),
        case {SplitType, OutgoingCount} of
            {xor_split, N} when N > 1 ->
                [#{type => semantic,
                   message => iolist_to_binary([<<"XOR split on '">>, TaskId,
                                              <<"' has multiple outgoing flows">>]),
                   location => TaskId} | Acc];
            {and_split, N} when N < 2 ->
                [#{type => semantic,
                   message => iolist_to_binary([<<"AND split on '">>, TaskId,
                                              <<"' requires at least 2 outgoing flows">>]),
                   location => TaskId} | Acc];
            _ ->
                Acc
        end
    end, [], Tasks).

%% @private
%% @doc Validates for cycles in flow graph.
-spec validate_cycles(specification()) -> [validation_error()].

validate_cycles(#{flows := Flows}) ->
    %% Build adjacency list
    Graph = lists:foldl(fun(#{source := Source, target := Target}, Acc) ->
        Acc#{Source => [Target | maps:get(Source, Acc, [])]}
    end, #{}, Flows),

    %% Get all nodes
    Nodes = lists:usort([N || #{source := N} <- Flows] ++
                        [N || #{target := N} <- Flows]),

    %% Check for cycles
    case detect_cycle(Graph, Nodes, #{}, []) of
        {cycle, Node} ->
            [#{type => semantic,
               message => iolist_to_binary([<<"Cycle detected involving '">>, Node, <<"'">>]),
               location => Node}];
        no_cycle ->
            []
    end.

%% @private
%% @doc Detects cycles in a directed graph using DFS.
-spec detect_cycle(#{binary() => [binary()]}, [binary()],
                   #{binary() => visiting | visited}, [binary()]) ->
          {cycle, binary()} | no_cycle.

detect_cycle(_Graph, [], _Visited, _Path) ->
    no_cycle;
detect_cycle(Graph, [Node | Rest], Visited, Path) ->
    case maps:get(Node, Visited, undefined) of
        visiting ->
            {cycle, Node};
        visited ->
            detect_cycle(Graph, Rest, Visited, Path);
        undefined ->
            Visited1 = Visited#{Node => visiting},
            Neighbors = maps:get(Node, Graph, []),
            case detect_cycle(Graph, Neighbors, Visited1, [Node | Path]) of
                {cycle, _} = Cycle ->
                    Cycle;
                no_cycle ->
                    Visited2 = Visited1#{Node => visited},
                    detect_cycle(Graph, Rest, Visited2, Path)
            end
    end.

%%====================================================================
%% Internal Functions - Conversion
%%====================================================================

%% @private
%% @doc Converts task map to proplist for cre_yawl.
-spec task_to_proplist(task()) -> [{atom(), term()}].

task_to_proplist(#{id := Id, name := Name, type := Type} = Task) ->
    Base = [
        {id, Id},
        {name, Name},
        {type, Type}
    ],
    Split = case maps:get(split_type, Task) of
        undefined -> [];
        SplitType -> [{split_type, SplitType}]
    end,
    Join = case maps:get(join_type, Task) of
        undefined -> [];
        JoinType -> [{join_type, JoinType}]
    end,
    Base ++ Split ++ Join.

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Run doctests for the yawl_schema module.
%%
%% This function validates:
%% <ul>
%%   <li>Getting tasks from specification map</li>
%%   <li>Getting conditions from specification map</li>
%%   <li>Getting flows from specification map</li>
%%   <li>Getting data mappings from specification map</li>
%%   <li>Getting root decomposition from specification</li>
%%   <li>Validating a minimal valid specification</li>
%%   <li>Validating specification with empty ID fails</li>
%%   <li>Validating specification with invalid flow references fails</li>
%%   <li>Converting task map to proplist</li>
%% </ul>
%%
%% Running the doctests:
%%
%% ```erlang
%% 1> yawl_schema:doctest_test().
%% ok
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Get tasks from specification
    Spec1 = #{
        id => <<"test_wf">>,
        name => <<"Test Workflow">>,
        tasks => #{<<"task1">> => #{
            id => <<"task1">>,
            name => <<"Task 1">>,
            type => atomic,
            split_type => undefined,
            join_type => undefined,
            decomposition => undefined,
            min_instances => undefined,
            max_instances => undefined,
            continuation_threshold => undefined
        }},
        conditions => #{},
        flows => [],
        data_mappings => []
    },
    Tasks = get_tasks(Spec1),
    1 = map_size(Tasks),
    #{<<"task1">> := #{type := atomic}} = Tasks,

    %% Test 2: Get conditions from specification
    Spec2 = Spec1#{conditions => #{<<"cond1">> => #{
        id => <<"cond1">>,
        type => input_condition,
        expression => <<"true">>
    }}},
    Conds = get_conditions(Spec2),
    1 = map_size(Conds),
    #{<<"cond1">> := #{type := input_condition}} = Conds,

    %% Test 3: Get flows from specification
    Spec3 = Spec1#{flows => [#{id => <<"flow1">>, source => <<"task1">>, target => <<"task2">>, predicate => undefined}]},
    Flows = get_flows(Spec3),
    1 = length(Flows),
    #{source := <<"task1">>} = lists:nth(1, Flows),

    %% Test 4: Get data mappings from specification
    Spec4 = Spec1#{data_mappings => [#{task_id => <<"task1">>, input => [], output => []}]},
    Maps = get_data_mappings(Spec4),
    1 = length(Maps),
    #{task_id := <<"task1">>} = lists:nth(1, Maps),

    %% Test 5: Get root decomposition
    Spec5 = Spec1#{decomposition => #{id => <<"root">>, type => root}},
    {ok, #{id := <<"root">>}} = get_root_decomposition(Spec5),

    %% Test 6: Get root decomposition when missing
    {error, not_found} = get_root_decomposition(Spec1),

    %% Test 7: Validate valid specification (minimal)
    ok = validate_specification(Spec1),

    %% Test 8: Validate specification with empty ID should fail
    BadSpec = Spec1#{id => <<>>},
    {error, Errors} = validate_specification(BadSpec),
    true = length(Errors) > 0,

    %% Test 9: Validate flow references
    SpecWithBadFlow = Spec1#{flows => [#{id => <<"f1">>, source => <<"nonexistent">>, target => <<"task1">>, predicate => undefined}]},
    {error, RefErrors} = validate_specification(SpecWithBadFlow),
    true = length(RefErrors) > 0,

    %% Test 10: Task to proplist conversion
    Task = #{
        id => <<"t1">>,
        name => <<"Task 1">>,
        type => atomic,
        split_type => xor_split,
        join_type => and_join
    },
    Plist = task_to_proplist(Task),
    true = is_list(Plist),
    true = lists:keymember(id, 1, Plist),
    true = lists:keymember(name, 1, Plist),
    true = lists:keymember(type, 1, Plist),
    true = lists:keymember(split_type, 1, Plist),
    true = lists:keymember(join_type, 1, Plist),

    %% Test 11: Task to proplist with undefined split/join
    Task2 = Task#{split_type => undefined, join_type => undefined},
    Plist2 = task_to_proplist(Task2),
    false = lists:keymember(split_type, 1, Plist2),
    false = lists:keymember(join_type, 1, Plist2),

    %% Test 12: Empty specification edge cases
    EmptySpec = #{id => <<"empty">>, name => <<"Empty">>, tasks => #{}, conditions => #{}, flows => [], data_mappings => []},
    ok = validate_specification(EmptySpec),
    #{} = get_tasks(EmptySpec),
    #{} = get_conditions(EmptySpec),
    [] = get_flows(EmptySpec),
    [] = get_data_mappings(EmptySpec),

    ok.
