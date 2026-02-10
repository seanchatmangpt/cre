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
%% @doc YAWL Workflow Export/Import Module
%%
%% This module provides comprehensive export and import functionality for YAWL
%% workflows in both JSON and XML (YAWL 2.0 standard) formats.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Export workflows to JSON format (compact, human-readable)</li>
%%   <li>Import workflows from JSON format</li>
%%   <li>Export workflows to YAWL 2.0 XML format</li>
%%   <li>Import workflows from YAWL 2.0 XML format</li>
%%   <li>Format conversion utilities</li>
%%   <li>Workflow validation on import</li>
%% </ul>
%%
%% <h3>Workflow Structure</h3>
%%
%% A workflow is represented as a map with the following keys:
%% <ul>
%%   <li><b>id:</b> Binary workflow identifier</li>
%%   <li><b>name:</b> Human-readable workflow name</li>
%%   <li><b>version:</b> Optional version string</li>
%%   <li><b>tasks:</b> List of task definitions</li>
%%   <li><b>conditions:</b> List of condition/place definitions</li>
%%   <li><b>flows:</b> List of flow connections</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Export workflow to JSON:
%% ```erlang
%% > Workflow = #{
%%     id => <<"order_workflow">>,
%%     name => <<"Order Processing">>,
%%     tasks => [
%%       #{id => <<"task1">>, name => <<"Receive Order">>, type => atomic}
%%     ],
%%     conditions => [],
%%     flows => []
%%   },
%% > {ok, JsonBin} = yawl_export:to_json(Workflow).
%% ```
%%
%% Import workflow from JSON:
%% ```erlang
%% > {ok, Workflow} = yawl_export:from_json(JsonBin).
%% ```
%%
%% Export workflow to YAWL XML:
%% ```erlang
%% > {ok, XmlBin} = yawl_export:to_yawl_xml(Workflow).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_export).

%%====================================================================
%% Exports
%%====================================================================

%% JSON export/import
-export([to_json/1, to_json/2, from_json/1]).

%% YAWL XML export/import
-export([to_yawl_xml/1, to_yawl_xml/2, from_yawl_xml/1]).

%% Format conversion
-export([json_to_yawl_xml/1, yawl_xml_to_json/1]).

%% Workflow utilities
-export([normalize_workflow/1, validate_workflow/1]).

%% File operations
-export([export_to_file/3, import_from_file/2]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Types
%%====================================================================

-type workflow() :: #{
    id => binary(),
    name => binary(),
    version => binary() | undefined,
    tasks => [task()],
    conditions => [condition()],
    flows => [flow()]
}.

-type task() :: #{
    id => binary(),
    name => binary() | undefined,
    type => atomic | composite | multiple_instance,
    split_type => and | or | xor | undefined,
    join_type => and | or | xor | undefined,
    code => binary() | undefined,
    documentation => binary() | undefined,
    params => map()
}.

-type condition() :: #{
    id => binary(),
    name => binary() | undefined,
    expression => binary() | undefined
}.

-type flow() :: #{
    id => binary(),
    source => binary(),
    target => binary(),
    predicate => binary() | undefined
}.

-type json_opts() :: #{
    pretty => boolean(),
    indent => integer()
}.

-type xml_opts() :: #{
    pretty => boolean(),
    validate => boolean()
}.

-type export_result() :: {ok, binary()} | {error, term()}.
-type import_result() :: {ok, workflow()} | {error, term()}.

-export_type([workflow/0, task/0, condition/0, flow/0]).

%%====================================================================
%% JSON Export/Import
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Exports a workflow to JSON format.
%%
%% Converts an internal workflow map to a JSON-encoded binary.
%% Uses default options (pretty-printed).
%%
%% @param Workflow The workflow map to export.
%% @return {ok, JsonBinary} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec to_json(workflow()) -> export_result().
to_json(Workflow) ->
    to_json(Workflow, #{pretty => true, indent => 2}).

%%--------------------------------------------------------------------
%% @doc Exports a workflow to JSON format with options.
%%
%% @param Workflow The workflow map to export.
%% @param Options JSON formatting options (pretty, indent).
%% @return {ok, JsonBinary} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec to_json(workflow(), json_opts()) -> export_result().
to_json(Workflow, Options) when is_map(Workflow), is_map(Options) ->
    try
        NormWf = normalize_workflow(Workflow),
        JsonMap = workflow_to_json_map(NormWf),
        Pretty = maps:get(pretty, Options, true),
        case Pretty of
            true ->
                Indent = maps:get(indent, Options, 2),
                JsonBin = jsone:encode(JsonMap, [{pretty, true}, {indent, Indent}]);
            false ->
                JsonBin = jsone:encode(JsonMap)
        end,
        {ok, JsonBin}
    catch
        error:Reason:Stack ->
            {error, {encode_error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Imports a workflow from JSON format.
%%
%% Parses JSON-encoded binary and converts to internal workflow map.
%% Validates the resulting workflow structure.
%%
%% @param JsonBinary The JSON-encoded workflow.
%% @return {ok, Workflow} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec from_json(binary()) -> import_result().
from_json(JsonBinary) when is_binary(JsonBinary) ->
    try
        JsonMap = jsone:decode(JsonBinary),
        Workflow = json_map_to_workflow(JsonMap),
        case validate_workflow(Workflow) of
            ok -> {ok, Workflow};
            {error, Reason} -> {error, {validation_error, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {decode_error, Reason, Stack}}
    end.

%%====================================================================
%% YAWL XML Export/Import
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Exports a workflow to YAWL 2.0 XML format.
%%
%% Generates a YAWL 2.0 specification-compliant XML document.
%% Uses default options (pretty-printed, validated).
%%
%% @param Workflow The workflow map to export.
%% @return {ok, XmlBinary} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec to_yawl_xml(workflow()) -> export_result().
to_yawl_xml(Workflow) ->
    to_yawl_xml(Workflow, #{pretty => true, validate => true}).

%%--------------------------------------------------------------------
%% @doc Exports a workflow to YAWL 2.0 XML format with options.
%%
%% @param Workflow The workflow map to export.
%% @param Options XML formatting options (pretty, validate).
%% @return {ok, XmlBinary} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec to_yawl_xml(workflow(), xml_opts()) -> export_result().
to_yawl_xml(Workflow, Options) when is_map(Workflow), is_map(Options) ->
    try
        case maps:get(validate, Options, true) of
            true ->
                case validate_workflow(Workflow) of
                    ok -> ok;
                    {error, Reason} -> throw({validation_error, Reason})
                end;
            false -> ok
        end,
        NormWf = normalize_workflow(Workflow),
        XmlElement = workflow_to_yawl_xml_element(NormWf),
        RawXmlBin = iolist_to_binary(yawl_marshal:build_xml(XmlElement)),
        Pretty = maps:get(pretty, Options, true),
        case Pretty of
            true ->
                case yawl_marshal:pretty_print_xml(RawXmlBin) of
                    {ok, XmlBin} -> {ok, XmlBin};
                    {error, Reason} -> throw({pretty_print_error, Reason})
                end;
            false ->
                case yawl_marshal:format_xml(RawXmlBin) of
                    {ok, XmlBin} -> {ok, XmlBin};
                    {error, Reason} -> throw({format_error, Reason})
                end
        end
    catch
        error:Reason:Stack ->
            {error, {xml_encode_error, Reason, Stack}};
        throw:Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Imports a workflow from YAWL 2.0 XML format.
%%
%% Parses YAWL specification XML and converts to internal workflow map.
%%
%% @param XmlBinary The YAWL XML document.
%% @return {ok, Workflow} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec from_yawl_xml(binary()) -> import_result().
from_yawl_xml(XmlBinary) when is_binary(XmlBinary) ->
    try
        XmlElement = yawl_marshal:parse_xml(XmlBinary),
        Workflow = yawl_xml_element_to_workflow(XmlElement),
        case validate_workflow(Workflow) of
            ok -> {ok, Workflow};
            {error, Reason} -> {error, {validation_error, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {xml_decode_error, Reason, Stack}}
    end.

%%====================================================================
%% Format Conversion
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts JSON format to YAWL XML format.
%%
%% Parses JSON, validates, and generates YAWL XML.
%%
%% @param JsonBinary The JSON-encoded workflow.
%% @return {ok, XmlBinary} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec json_to_yawl_xml(binary()) -> export_result().
json_to_yawl_xml(JsonBinary) ->
    case from_json(JsonBinary) of
        {ok, Workflow} -> to_yawl_xml(Workflow);
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Converts YAWL XML format to JSON format.
%%
%% Parses YAWL XML, validates, and generates JSON.
%%
%% @param XmlBinary The YAWL XML document.
%% @return {ok, JsonBinary} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec yawl_xml_to_json(binary()) -> export_result().
yawl_xml_to_json(XmlBinary) ->
    case from_yawl_xml(XmlBinary) of
        {ok, Workflow} -> to_json(Workflow);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Workflow Utilities
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Normalizes a workflow structure.
%%
%% Ensures all required fields are present with proper defaults.
%%
%% @param Workflow The workflow map to normalize.
%% @return Normalized workflow map.
%% @end
%%--------------------------------------------------------------------
-spec normalize_workflow(workflow()) -> workflow().
normalize_workflow(Workflow) when is_map(Workflow) ->
    Defaults = #{
        id => <<"unknown">>,
        name => <<"Unnamed Workflow">>,
        version => undefined,
        tasks => [],
        conditions => [],
        flows => []
    },
    maps:merge(Defaults, Workflow).

%%--------------------------------------------------------------------
%% @doc Validates a workflow structure.
%%
%% Checks for required fields, valid references, and consistency.
%%
%% @param Workflow The workflow map to validate.
%% @return ok or {error, Reasons}.
%% @end
%%--------------------------------------------------------------------
-spec validate_workflow(workflow()) -> ok | {error, [binary()]}.
validate_workflow(Workflow) when is_map(Workflow) ->
    Errors = [],
    Errors1 = check_required_fields(Workflow, Errors),
    Errors2 = check_element_ids(Workflow, Errors1),
    Errors3 = check_flow_references(Workflow, Errors2),
    case Errors3 of
        [] -> ok;
        _ -> {error, Errors3}
    end;
validate_workflow(_) ->
    {error, [<<"Invalid workflow structure: not a map">>]}.

%%--------------------------------------------------------------------
%% @doc Exports a workflow to a file.
%%
%% Writes the workflow to a file in the specified format (json or yawl_xml).
%%
%% @param Workflow The workflow map to export.
%% @param Filename The output filename.
%% @param Format The format (json or yawl_xml).
%% @return ok or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec export_to_file(workflow(), string() | binary(), json | yawl_xml) ->
          ok | {error, term()}.
export_to_file(Workflow, Filename, Format) when is_map(Workflow) ->
    try
        BinFilename = case is_binary(Filename) of
            true -> Filename;
            false -> list_to_binary(Filename)
        end,
        FilenamePath = binary_to_list(BinFilename),
        Result = case Format of
            json -> to_json(Workflow, #{pretty => true});
            yawl_xml -> to_yawl_xml(Workflow, #{pretty => true});
            _ -> {error, {invalid_format, Format}}
        end,
        case Result of
            {ok, Data} ->
                file:write_file(FilenamePath, Data);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Reason:Stack ->
            {error, {file_error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Imports a workflow from a file.
%%
%% Reads and parses a workflow file in the specified format.
%%
%% @param Filename The input filename.
%% @param Format The format (json or yawl_xml).
%% @return {ok, Workflow} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec import_from_file(string() | binary(), json | yawl_xml) ->
          import_result().
import_from_file(Filename, Format) ->
    try
        BinFilename = case is_binary(Filename) of
            true -> Filename;
            false -> list_to_binary(Filename)
        end,
        FilenamePath = binary_to_list(BinFilename),
        case file:read_file(FilenamePath) of
            {ok, Data} ->
                case Format of
                    json -> from_json(Data);
                    yawl_xml -> from_yawl_xml(Data);
                    _ -> {error, {invalid_format, Format}}
                end;
            {error, Reason} ->
                {error, {file_read_error, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {file_error, Reason, Stack}}
    end.

%%====================================================================
%% Internal Functions - JSON Conversion
%%====================================================================

%%--------------------------------------------------------------------
%% @private Converts workflow map to JSON-compatible map.
%% @end
%%--------------------------------------------------------------------
-spec workflow_to_json_map(workflow()) -> map().
workflow_to_json_map(Workflow) ->
    #{
        <<"id">> => maps:get(id, Workflow, <<"unknown">>),
        <<"name">> => maps:get(name, Workflow, <<"Unnamed">>),
        <<"version">> => maps:get(version, Workflow, null),
        <<"tasks">> => [task_to_json_map(T) || T <- maps:get(tasks, Workflow, [])],
        <<"conditions">> => [condition_to_json_map(C) || C <- maps:get(conditions, Workflow, [])],
        <<"flows">> => [flow_to_json_map(F) || F <- maps:get(flows, Workflow, [])]
    }.

%%--------------------------------------------------------------------
%% @private Converts task map to JSON-compatible map.
%% @end
%%--------------------------------------------------------------------
-spec task_to_json_map(task()) -> map().
task_to_json_map(Task) when is_map(Task) ->
    #{
        <<"id">> => maps:get(id, Task, <<"unknown">>),
        <<"name">> => maps:get(name, Task, null),
        <<"type">> => atom_to_binary(maps:get(type, Task, atomic), utf8),
        <<"splitType">> => case maps:get(split_type, Task, undefined) of
            undefined -> null;
            T -> atom_to_binary(T, utf8)
        end,
        <<"joinType">> => case maps:get(join_type, Task, undefined) of
            undefined -> null;
            T -> atom_to_binary(T, utf8)
        end,
        <<"code">> => maps:get(code, Task, null),
        <<"documentation">> => maps:get(documentation, Task, null),
        <<"params">> => maps:get(params, Task, #{})
    }.

%%--------------------------------------------------------------------
%% @private Converts condition map to JSON-compatible map.
%% @end
%%--------------------------------------------------------------------
-spec condition_to_json_map(condition()) -> map().
condition_to_json_map(Condition) when is_map(Condition) ->
    #{
        <<"id">> => maps:get(id, Condition, <<"unknown">>),
        <<"name">> => maps:get(name, Condition, null),
        <<"expression">> => maps:get(expression, Condition, null)
    }.

%%--------------------------------------------------------------------
%% @private Converts flow map to JSON-compatible map.
%% @end
%%--------------------------------------------------------------------
-spec flow_to_json_map(flow()) -> map().
flow_to_json_map(Flow) when is_map(Flow) ->
    #{
        <<"id">> => maps:get(id, Flow, <<"unknown">>),
        <<"source">> => maps:get(source, Flow, <<"unknown">>),
        <<"target">> => maps:get(target, Flow, <<"unknown">>),
        <<"predicate">> => maps:get(predicate, Flow, null)
    }.

%%--------------------------------------------------------------------
%% @private Converts JSON map to workflow structure.
%% @end
%%--------------------------------------------------------------------
-spec json_map_to_workflow(map()) -> workflow().
json_map_to_workflow(JsonMap) when is_map(JsonMap) ->
    #{
        id => maps:get(<<"id">>, JsonMap, <<"unknown">>),
        name => maps:get(<<"name">>, JsonMap, <<"Unnamed">>),
        version => maps:get(<<"version">>, JsonMap, undefined),
        tasks => [json_map_to_task(T) || T <- maps:get(<<"tasks">>, JsonMap, [])],
        conditions => [json_map_to_condition(C) || C <- maps:get(<<"conditions">>, JsonMap, [])],
        flows => [json_map_to_flow(F) || F <- maps:get(<<"flows">>, JsonMap, [])]
    }.

%%--------------------------------------------------------------------
%% @private Converts JSON task map to internal task structure.
%% @end
%%--------------------------------------------------------------------
-spec json_map_to_task(map()) -> task().
json_map_to_task(TaskMap) when is_map(TaskMap) ->
    #{
        id => maps:get(<<"id">>, TaskMap, <<"unknown">>),
        name => maps:get(<<"name">>, TaskMap, undefined),
        type => binary_to_atom(maps:get(<<"type">>, TaskMap, <<"atomic">>), utf8),
        split_type => case maps:get(<<"splitType">>, TaskMap, null) of
            null -> undefined;
            T when is_binary(T) -> binary_to_atom(T, utf8)
        end,
        join_type => case maps:get(<<"joinType">>, TaskMap, null) of
            null -> undefined;
            T when is_binary(T) -> binary_to_atom(T, utf8)
        end,
        code => maps:get(<<"code">>, TaskMap, undefined),
        documentation => maps:get(<<"documentation">>, TaskMap, undefined),
        params => maps:get(<<"params">>, TaskMap, #{})
    }.

%%--------------------------------------------------------------------
%% @private Converts JSON condition map to internal condition structure.
%% @end
%%--------------------------------------------------------------------
-spec json_map_to_condition(map()) -> condition().
json_map_to_condition(CondMap) when is_map(CondMap) ->
    #{
        id => maps:get(<<"id">>, CondMap, <<"unknown">>),
        name => maps:get(<<"name">>, CondMap, undefined),
        expression => maps:get(<<"expression">>, CondMap, undefined)
    }.

%%--------------------------------------------------------------------
%% @private Converts JSON flow map to internal flow structure.
%% @end
%%--------------------------------------------------------------------
-spec json_map_to_flow(map()) -> flow().
json_map_to_flow(FlowMap) when is_map(FlowMap) ->
    #{
        id => maps:get(<<"id">>, FlowMap, <<"unknown">>),
        source => maps:get(<<"source">>, FlowMap, <<"unknown">>),
        target => maps:get(<<"target">>, FlowMap, <<"unknown">>),
        predicate => maps:get(<<"predicate">>, FlowMap, undefined)
    }.

%%====================================================================
%% Internal Functions - XML Conversion
%%====================================================================

%%--------------------------------------------------------------------
%% @private Converts workflow to YAWL 2.0 XML element.
%% @end
%%--------------------------------------------------------------------
-spec workflow_to_yawl_xml_element(workflow()) -> yawl_marshal:xml_element().
workflow_to_yawl_xml_element(Workflow) ->
    WfId = maps:get(id, Workflow, <<"unknown">>),
    WfName = maps:get(name, Workflow, <<"Unnamed">>),
    Tasks = maps:get(tasks, Workflow, []),
    Conditions = maps:get(conditions, Workflow, []),
    Flows = maps:get(flows, Workflow, []),

    SpecAttrs = [{<<"id">>, WfId}],
    SpecContent = [
        {<<"name">>, [], [WfName]},
        {<<"decomposition">>, [{<<"id">>, <<"root">>}],
            [[task_to_yawl_xml_element(T) || T <- Tasks],
             [condition_to_yawl_xml_element(C) || C <- Conditions],
             [flow_to_yawl_xml_element(F) || F <- Flows]]}
    ],
    {<<"specification">>, SpecAttrs, SpecContent}.

%%--------------------------------------------------------------------
%% @private Converts task to YAWL XML element.
%% @end
%%--------------------------------------------------------------------
-spec task_to_yawl_xml_element(task()) -> yawl_marshal:xml_element().
task_to_yawl_xml_element(Task) ->
    TaskId = maps:get(id, Task, <<"unknown">>),
    TaskName = maps:get(name, Task, TaskId),
    TaskType = maps:get(type, Task, atomic),
    SplitType = maps:get(split_type, Task, undefined),
    JoinType = maps:get(join_type, Task, undefined),

    Attrs = [
        {<<"id">>, TaskId},
        {<<"name">>, TaskName}
    ],
    Attrs1 = case SplitType of
        undefined -> Attrs;
        T -> Attrs ++ [{<<"splitType">>, atom_to_binary(T, utf8)}]
    end,
    Attrs2 = case JoinType of
        undefined -> Attrs1;
        T -> Attrs1 ++ [{<<"joinType">>, atom_to_binary(T, utf8)}]
    end,
    Attrs3 = Attrs2 ++ [{<<"type">>, atom_to_binary(TaskType, utf8)}],

    Content = [],
    {<<"task">>, Attrs3, Content}.

%%--------------------------------------------------------------------
%% @private Converts condition to YAWL XML element.
%% @end
%%--------------------------------------------------------------------
-spec condition_to_yawl_xml_element(condition()) -> yawl_marshal:xml_element().
condition_to_yawl_xml_element(Condition) ->
    CondId = maps:get(id, Condition, <<"unknown">>),
    CondName = maps:get(name, Condition, CondId),

    Attrs = [{<<"id">>, CondId}, {<<"name">>, CondName}],
    {<<"condition">>, Attrs, []}.

%%--------------------------------------------------------------------
%% @private Converts flow to YAWL XML element.
%% @end
%%--------------------------------------------------------------------
-spec flow_to_yawl_xml_element(flow()) -> yawl_marshal:xml_element().
flow_to_yawl_xml_element(Flow) ->
    FlowId = maps:get(id, Flow, <<"unknown">>),
    Source = maps:get(source, Flow, <<"unknown">>),
    Target = maps:get(target, Flow, <<"unknown">>),
    Predicate = maps:get(predicate, Flow, undefined),

    Attrs = [
        {<<"id">>, FlowId},
        {<<"source">>, Source},
        {<<"target">>, Target}
    ],
    Attrs1 = case Predicate of
        undefined -> Attrs;
        P -> Attrs ++ [{<<"predicate">>, P}]
    end,
    {<<"flow">>, Attrs1, []}.

%%--------------------------------------------------------------------
%% @private Converts YAWL XML element to workflow structure.
%% @end
%%--------------------------------------------------------------------
-spec yawl_xml_element_to_workflow(yawl_marshal:xml_element()) -> workflow().
yawl_xml_element_to_workflow({<<"specification">>, Attrs, Content}) ->
    WfId = proplists:get_value(<<"id">>, Attrs, <<"unknown">>),
    {WfName, Tasks, Conditions, Flows} = extract_spec_content(Content, [], [], []),

    #{
        id => WfId,
        name => WfName,
        version => undefined,
        tasks => Tasks,
        conditions => Conditions,
        flows => Flows
    };
yawl_xml_element_to_workflow(_) ->
    #{id => <<"unknown">>, name => <<"Unknown">>, tasks => [], conditions => [], flows => []}.

%%--------------------------------------------------------------------
%% @private Extracts content from specification element.
%% @end
%%--------------------------------------------------------------------
-spec extract_spec_content([term()], [task()], [condition()], [flow()]) ->
          {binary(), [task()], [condition()], [flow()]}.
extract_spec_content([], Name, Tasks, Conditions) ->
    {Name, Tasks, Conditions, []};
extract_spec_content([{<<"name">>, [], Content} | Rest], _, Tasks, Conditions) ->
    Name = case Content of
        [NameBin] when is_binary(NameBin) -> NameBin;
        _ -> <<"Unnamed">>
    end,
    extract_spec_content(Rest, Name, Tasks, Conditions);
extract_spec_content([{<<"decomposition">>, _Attrs, DecompContent} | Rest], Name, Tasks, Conditions) ->
    {Tasks1, Conditions1, Flows1} = extract_decomposition_content(DecompContent, [], [], []),
    extract_spec_content(Rest, Name, Tasks1, Conditions1);
extract_spec_content([_ | Rest], Name, Tasks, Conditions) ->
    extract_spec_content(Rest, Name, Tasks, Conditions).

%%--------------------------------------------------------------------
%% @private Extracts content from decomposition element.
%% @end
%%--------------------------------------------------------------------
-spec extract_decomposition_content([term()], [task()], [condition()], [flow()]) ->
          {[task()], [condition()], [flow()]}.
extract_decomposition_content([], Tasks, Conditions, Flows) ->
    {Tasks, Conditions, Flows};
extract_decomposition_content([{<<"task">>, Attrs, _Content} | Rest], Tasks, Conditions, Flows) ->
    Task = xml_attrs_to_task(Attrs),
    extract_decomposition_content(Rest, [Task | Tasks], Conditions, Flows);
extract_decomposition_content([{<<"condition">>, Attrs, _Content} | Rest], Tasks, Conditions, Flows) ->
    Condition = xml_attrs_to_condition(Attrs),
    extract_decomposition_content(Rest, Tasks, [Condition | Conditions], Flows);
extract_decomposition_content([{<<"flow">>, Attrs, _Content} | Rest], Tasks, Conditions, Flows) ->
    Flow = xml_attrs_to_flow(Attrs),
    extract_decomposition_content(Rest, Tasks, Conditions, [Flow | Flows]);
extract_decomposition_content([_ | Rest], Tasks, Conditions, Flows) ->
    extract_decomposition_content(Rest, Tasks, Conditions, Flows).

%%--------------------------------------------------------------------
%% @private Converts XML attributes to task map.
%% @end
%%--------------------------------------------------------------------
-spec xml_attrs_to_task([{binary(), binary()}]) -> task().
xml_attrs_to_task(Attrs) ->
    #{
        id => proplists:get_value(<<"id">>, Attrs, <<"unknown">>),
        name => proplists:get_value(<<"name">>, Attrs, undefined),
        type => case proplists:get_value(<<"type">>, Attrs, <<"atomic">>) of
            T when is_binary(T) -> binary_to_atom(T, utf8);
            T -> T
        end,
        split_type => case proplists:get_value(<<"splitType">>, Attrs, undefined) of
            undefined -> undefined;
            T when is_binary(T) -> binary_to_atom(T, utf8);
            T -> T
        end,
        join_type => case proplists:get_value(<<"joinType">>, Attrs, undefined) of
            undefined -> undefined;
            T when is_binary(T) -> binary_to_atom(T, utf8);
            T -> T
        end,
        code => undefined,
        documentation => undefined,
        params => #{}
    }.

%%--------------------------------------------------------------------
%% @private Converts XML attributes to condition map.
%% @end
%%--------------------------------------------------------------------
-spec xml_attrs_to_condition([{binary(), binary()}]) -> condition().
xml_attrs_to_condition(Attrs) ->
    #{
        id => proplists:get_value(<<"id">>, Attrs, <<"unknown">>),
        name => proplists:get_value(<<"name">>, Attrs, undefined),
        expression => proplists:get_value(<<"expression">>, Attrs, undefined)
    }.

%%--------------------------------------------------------------------
%% @private Converts XML attributes to flow map.
%% @end
%%--------------------------------------------------------------------
-spec xml_attrs_to_flow([{binary(), binary()}]) -> flow().
xml_attrs_to_flow(Attrs) ->
    #{
        id => proplists:get_value(<<"id">>, Attrs, <<"unknown">>),
        source => proplists:get_value(<<"source">>, Attrs, <<"unknown">>),
        target => proplists:get_value(<<"target">>, Attrs, <<"unknown">>),
        predicate => proplists:get_value(<<"predicate">>, Attrs, undefined)
    }.

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @private Checks required fields in workflow.
%% @end
%%--------------------------------------------------------------------
-spec check_required_fields(workflow(), [binary()]) -> [binary()].
check_required_fields(Workflow, Errors) ->
    case maps:get(id, Workflow, undefined) of
        undefined -> [<<"Missing workflow id">> | Errors];
        _ -> Errors
    end.

%%--------------------------------------------------------------------
%% @private Checks that all element IDs are unique.
%% @end
%%--------------------------------------------------------------------
-spec check_element_ids(workflow(), [binary()]) -> [binary()].
check_element_ids(Workflow, Errors) ->
    Tasks = maps:get(tasks, Workflow, []),
    Conditions = maps:get(conditions, Workflow, []),
    AllIds = [maps:get(id, T, undefined) || T <- Tasks] ++
             [maps:get(id, C, undefined) || C <- Conditions],
    case length(AllIds) =:= length(lists:usort(AllIds)) of
        true -> Errors;
        false -> [<<"Duplicate element IDs found">> | Errors]
    end.

%%--------------------------------------------------------------------
%% @private Checks that all flow references exist.
%% @end
%%--------------------------------------------------------------------
-spec check_flow_references(workflow(), [binary()]) -> [binary()].
check_flow_references(Workflow, Errors) ->
    Tasks = maps:get(tasks, Workflow, []),
    Conditions = maps:get(conditions, Workflow, []),
    Flows = maps:get(flows, Workflow, []),
    ValidIds = [maps:get(id, T, undefined) || T <- Tasks] ++
               [maps:get(id, C, undefined) || C <- Conditions],

    lists:foldl(fun(Flow, Acc) ->
        Source = maps:get(source, Flow, undefined),
        Target = maps:get(target, Flow, undefined),
        Acc1 = case lists:member(Source, ValidIds) of
            true -> Acc;
            false -> [<<"Flow source not found: ", Source/binary>> | Acc]
        end,
        case lists:member(Target, ValidIds) of
            true -> Acc1;
            false -> [<<"Flow target not found: ", Target/binary>> | Acc1]
        end
    end, Errors, Flows).

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Run doctests for yawl_export module.
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.
doctest_test() ->
    %% Test 1: Create simple workflow
    Workflow = #{
        id => <<"test_wf">>,
        name => <<"Test Workflow">>,
        tasks => [
            #{
                id => <<"task1">>,
                name => <<"Task 1">>,
                type => atomic,
                split_type => undefined,
                join_type => undefined,
                code => undefined,
                documentation => undefined,
                params => #{}
            }
        ],
        conditions => [],
        flows => []
    },

    %% Test 2: Export to JSON
    {ok, JsonBin} = to_json(Workflow),
    true = is_binary(JsonBin),
    true = byte_size(JsonBin) > 0,

    %% Test 3: Import from JSON
    {ok, Workflow2} = from_json(JsonBin),
    <<"test_wf">> = maps:get(id, Workflow2),

    %% Test 4: Export to YAWL XML
    {ok, XmlBin} = to_yawl_xml(Workflow),
    true = is_binary(XmlBin),
    true = byte_size(XmlBin) > 0,

    %% Test 5: Import from YAWL XML
    {ok, Workflow3} = from_yawl_xml(XmlBin),
    <<"test_wf">> = maps:get(id, Workflow3),

    %% Test 6: JSON to XML conversion
    {ok, XmlBin2} = json_to_yawl_xml(JsonBin),
    true = is_binary(XmlBin2),

    %% Test 7: XML to JSON conversion
    {ok, JsonBin2} = yawl_xml_to_json(XmlBin),
    true = is_binary(JsonBin2),

    %% Test 8: Validate workflow
    ok = validate_workflow(Workflow),

    %% Test 9: Normalize workflow
    ShortWf = #{id => <<"wf1">>},
    NormWf = normalize_workflow(ShortWf),
    <<"Unnamed Workflow">> = maps:get(name, NormWf),

    %% Test 10: Validate invalid workflow (missing id)
    InvalidWf = #{name => <<"No ID">>},
    {error, _} = validate_workflow(InvalidWf),

    ok.
