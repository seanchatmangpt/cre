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

-module(wf_spec).
-moduledoc """
YAWL XML specification parser, validator, and compiler.

This module provides functionality to import YAWL 2.2 XML specifications,
validate their structure, and compile them to gen_pnet compatible Petri nets.

## Examples

Parse YAWL XML and extract metadata:

```erlang
> Xml = <<
.. "<?xml version='1.0' encoding='UTF-8'?>\n"
.. "<specificationSet id='demo' version='2.2'>\n"
.. "  <specification uri='demo'>\n"
.. "    <metaData><title>Demo</title></metaData>\n"
.. "    <decomposition id='main' isRootNet='true'>\n"
.. "      <processControlElements>\n"
.. "        <inputCondition id='i'/>\n"
.. "        <outputCondition id='o'/>\n"
.. "        <task id='t1' type='human'>\n"
.. "          <name>Approve</name>\n"
.. "          <documentation>Approve an order</documentation>\n"
.. "          <join code='xor'/>\n"
.. "          <split code='xor'/>\n"
.. "        </task>\n"
.. "        <flowsInto><from id='i'/><to id='t1'/></flowsInto>\n"
.. "        <flowsInto><from id='t1'/><to id='o'/></flowsInto>\n"
.. "      </processControlElements>\n"
.. "    </decomposition>\n"
.. "  </specification>\n"
.. "</specificationSet>\n"
.. >>.
_
> {ok, Spec} = wf_spec:from_xml(Xml).
_
> wf_spec:id(Spec).
demo
> wf_spec:root_net(Spec).
main
> wf_spec:tasks(Spec).
[t1]
> wf_spec:task_doc(Spec, t1).
<<"Approve an order">>

> wf_spec:validate(Spec).
ok

> {ok, Compiled} = wf_spec:compile(Spec).
_
> length(wf_spec:places(Compiled)) > 0.
true
> length(wf_spec:transitions(Compiled)) > 0.
true
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Parsing
-export([from_xml/1, from_xml_file/1]).

%% Accessors
-export([id/1, title/1, version/1, root_net/1]).
-export([tasks/1, task_doc/2, task_type/2]).
-export([places/1, transitions/1]).

%% Validation and compilation
-export([validate/1, compile/1]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Types
%%====================================================================

-type spec_id() :: binary().
-type task_id() :: atom().
-type place() :: atom().
-type transition() :: atom().

-record(yawl_spec, {
    id :: spec_id(),
    title :: binary(),
    version :: binary() | undefined,
    root_net :: binary(),
    tasks :: #{task_id() => #{docs => binary() | undefined, type => atom()}},
    places :: [place()],
    transitions :: [transition()]
}).

-record(compiled_spec, {
    original :: #yawl_spec{},
    places :: [place()],
    transitions :: [transition()],
    preset :: #{transition() => [place()]},
    postset :: #{transition() => [place()]}
}).

-opaque yawl_spec() :: #yawl_spec{}.
-opaque compiled_spec() :: #compiled_spec{}.

-export_type([yawl_spec/0, compiled_spec/0]).

%%====================================================================
%% xmerl record field indices (to avoid include issues on OTP 28)
%%====================================================================
%% xmlElement record fields (OTP 28):
%% 1: xmlElement, 2: name, 3: expanded_name, 4: nsinfo, 5: namespace,
%% 6: parents, 7: pos, 8: attributes, 9: content, 10: language,
%% 11: xmlbase, 12: elementdef
%% xmlAttribute record fields (OTP 28):
%% 1: xmlAttribute, 2: name, 3: expanded_name, 4: nsinfo, 5: namespace,
%% 6: parents, 7: pos, 8: language, 9: value, 10: normalized
%% xmlText record fields (OTP 28):
%% 1: xmlText, 2: parents, 3: pos, 4: language, 5: value

-define(XML_ELEMENT_NAME, 2).
-define(XML_ELEMENT_ATTRIBUTES, 8).
-define(XML_ELEMENT_CONTENT, 9).
-define(XML_ATTR_NAME, 2).
-define(XML_ATTR_VALUE, 9).
-define(XML_TEXT_VALUE, 5).

%%====================================================================
%% Parsing Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Parses YAWL XML from a binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_xml(Xml :: binary()) -> {ok, yawl_spec()} | {error, term()}.

from_xml(Xml) when is_binary(Xml) ->
    try
        {XmlElement, _Rest} = xmerl_scan:string(binary_to_list(Xml)),
        Content = element(?XML_ELEMENT_CONTENT, XmlElement),
        extract_spec_from_root(Content)
    catch
        _:Reason ->
            {error, {parse_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Parses YAWL XML from a file.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_xml_file(FilePath :: file:filename_all()) ->
          {ok, yawl_spec()} | {error, term()}.

from_xml_file(FilePath) when is_list(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Xml} -> from_xml(Xml);
        {error, Reason} -> {error, {file_error, Reason}}
    end.

%%====================================================================
%% Accessor Functions
%%====================================================================

-spec id(Spec :: yawl_spec()) -> spec_id().
id(#yawl_spec{id = Id}) -> Id.

-spec title(Spec :: yawl_spec()) -> binary().
title(#yawl_spec{title = Title}) -> Title.

-spec version(Spec :: yawl_spec()) -> binary() | undefined.
version(#yawl_spec{version = Version}) -> Version.

-spec root_net(Spec :: yawl_spec()) -> binary().
root_net(#yawl_spec{root_net = RootNet}) -> RootNet.

-spec tasks(Spec :: yawl_spec()) -> [task_id()].
tasks(#yawl_spec{tasks = Tasks}) ->
    maps:keys(Tasks).

-spec task_doc(Spec :: yawl_spec(), TaskId :: task_id()) -> binary().
task_doc(#yawl_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #{docs := Doc} when Doc =/= undefined -> Doc;
        _ -> <<>>
    end.

-spec task_type(Spec :: yawl_spec(), TaskId :: task_id()) -> atom().
task_type(#yawl_spec{tasks = Tasks}, TaskId) ->
    case maps:get(TaskId, Tasks, undefined) of
        #{type := Type} -> Type;
        _ -> atomic
    end.

-spec places(Compiled :: compiled_spec()) -> [place()].
places(#compiled_spec{places = Places}) -> Places.

-spec transitions(Compiled :: compiled_spec()) -> [transition()].
transitions(#compiled_spec{transitions = Transitions}) -> Transitions.

%%====================================================================
%% Validation Functions
%%====================================================================

-spec validate(Spec :: yawl_spec()) -> ok | {error, [binary()]}.
validate(#yawl_spec{id = Id, root_net = RootNet, tasks = Tasks}) ->
    Errors = lists:flatten([
        validate_id(Id),
        validate_root_net(RootNet),
        validate_tasks(Tasks)
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%%====================================================================
%% Compilation Functions
%%====================================================================

-spec compile(Spec :: yawl_spec()) -> {ok, compiled_spec()} | {error, term()}.
compile(#yawl_spec{} = Spec) ->
    case validate(Spec) of
        ok ->
            {Places, Transitions, Preset, Postset} = build_net(Spec),
            Compiled = #compiled_spec{
                original = Spec,
                places = Places,
                transitions = Transitions,
                preset = Preset,
                postset = Postset
            },
            {ok, Compiled};
        {error, Errors} ->
            {error, {validation_failed, Errors}}
    end.

%%====================================================================
%% Internal Functions - XML Parsing
%%====================================================================

%% @private
extract_spec_from_root([]) ->
    {error, no_root_element};
extract_spec_from_root([XmlElement | Rest]) when element(1, XmlElement) =:= xmlElement ->
    case element(?XML_ELEMENT_NAME, XmlElement) of
        'specificationSet' ->
            extract_from_specification_set(XmlElement);
        specification ->
            extract_from_specification(XmlElement);
        _ ->
            extract_spec_from_root(Rest)
    end;
extract_spec_from_root([_NonElement | Rest]) ->
    %% Skip text nodes and other non-element nodes (whitespace, comments, etc.)
    extract_spec_from_root(Rest);
extract_spec_from_root(_) ->
    {error, invalid_root_element}.

%% @private
extract_from_specification_set(SpecSet) ->
    Attrs = element(?XML_ELEMENT_ATTRIBUTES, SpecSet),
    Content = element(?XML_ELEMENT_CONTENT, SpecSet),
    SpecSetId = attr_value(Attrs, <<"id">>, <<"unnamed">>),
    Version = attr_value(Attrs, <<"version">>, undefined),
    case find_element(Content, specification) of
        {ok, SpecEl} ->
            Spec = extract_from_specification(SpecEl),
            Spec#yawl_spec{id = SpecSetId, version = Version};
        _ ->
            #yawl_spec{id = SpecSetId, version = Version, title = <<"Untitled">>,
                       root_net = <<"main">>, tasks = #{}, places = [], transitions = []}
    end.

%% @private
extract_from_specification(Spec) ->
    Attrs = element(?XML_ELEMENT_ATTRIBUTES, Spec),
    Content = element(?XML_ELEMENT_CONTENT, Spec),
    Id = attr_value(Attrs, <<"uri">>, <<"unnamed">>),
    Title = extract_text_from_content(Content, title, <<"Untitled">>),
    Version = attr_value(Attrs, <<"version">>, undefined),
    RootNet = extract_root_net(Content),
    Tasks = extract_all_tasks(Content),
    #yawl_spec{
        id = Id,
        title = Title,
        version = Version,
        root_net = RootNet,
        tasks = Tasks,
        places = [],
        transitions = []
    }.

%% @private
extract_root_net(Content) ->
    case find_element(Content, decomposition) of
        {ok, DecompEl} ->
            Attrs = element(?XML_ELEMENT_ATTRIBUTES, DecompEl),
            case attr_value(Attrs, <<"id">>, undefined) of
                undefined -> <<"main">>;
                Id -> Id
            end;
        _ ->
            <<"main">>
    end.

%% @private
extract_all_tasks(Content) ->
    case find_element(Content, 'processControlElements') of
        {ok, PceEl} ->
            PceContent = element(?XML_ELEMENT_CONTENT, PceEl),
            extract_tasks_from_elements(PceContent);
        _ ->
            %% Try direct search in Content
            extract_tasks_from_elements(Content)
    end.

%% @private
extract_tasks_from_elements(Elements) ->
    lists:foldl(fun
        (XmlElement, Acc) when element(1, XmlElement) =:= xmlElement,
                             element(?XML_ELEMENT_NAME, XmlElement) =:= 'task' ->
            Attrs = element(?XML_ELEMENT_ATTRIBUTES, XmlElement),
            TaskContent = element(?XML_ELEMENT_CONTENT, XmlElement),
            TaskId = extract_task_id(Attrs),
            Doc = extract_text_from_content(TaskContent, documentation, <<>>),
            Type = extract_task_type(Attrs),
            Acc#{TaskId => #{docs => Doc, type => Type}};
        (_, Acc) ->
            Acc
    end, #{}, Elements).

%% @private
extract_task_id(Attrs) ->
    case attr_value(Attrs, <<"id">>, undefined) of
        undefined ->
            TaskIdInt = erlang:unique_integer([positive]),
            list_to_atom("task_" ++ integer_to_list(TaskIdInt));
        Id ->
            binary_to_atom(Id, utf8)
    end.

%% @private
extract_task_type(Attrs) ->
    case attr_value(Attrs, <<"type">>, undefined) of
        undefined -> atomic;
        <<"human">> -> human;
        <<"atomic">> -> atomic;
        Type -> binary_to_atom(Type, utf8)
    end.

%% @private
find_element([], _Tag) ->
    {error, not_found};
find_element([XmlElement | _Rest], Tag) when element(1, XmlElement) =:= xmlElement,
                                             element(?XML_ELEMENT_NAME, XmlElement) =:= Tag ->
    {ok, XmlElement};
find_element([_ | Rest], Tag) ->
    find_element(Rest, Tag).

%% @private
attr_value([], _Name, Default) ->
    Default;
attr_value([Attr | Rest], Name, Default) when element(1, Attr) =:= xmlAttribute ->
    case element(?XML_ATTR_NAME, Attr) of
        Name ->
            case element(?XML_ATTR_VALUE, Attr) of
                Value when is_list(Value) -> list_to_binary(Value);
                Value when is_binary(Value) -> Value;
                _ -> Default
            end;
        _ ->
            attr_value(Rest, Name, Default)
    end;
attr_value([_ | Rest], Name, Default) ->
    attr_value(Rest, Name, Default).

%% @private
extract_text_from_content([], _Tag, Default) ->
    Default;
extract_text_from_content([XmlElement | _], Tag, Default) when element(1, XmlElement) =:= xmlElement,
                                                               element(?XML_ELEMENT_NAME, XmlElement) =:= Tag ->
    Content = element(?XML_ELEMENT_CONTENT, XmlElement),
    extract_text(Content);
extract_text_from_content([_ | Rest], Tag, Default) ->
    extract_text_from_content(Rest, Tag, Default).

%% @private
extract_text([]) ->
    <<>>;
extract_text([TextEl | _]) when element(1, TextEl) =:= xmlText ->
    element(?XML_TEXT_VALUE, TextEl);
extract_text([_ | Rest]) ->
    extract_text(Rest).

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%% @private
validate_id(<<>>) ->
    [<<"Specification ID cannot be empty">>];
validate_id(_Id) ->
    [].

%% @private
validate_root_net(<<>>) ->
    [<<"Root net cannot be empty">>];
validate_root_net(_RootNet) ->
    [].

%% @private
validate_tasks(Tasks) when map_size(Tasks) =:= 0 ->
    [<<"Specification must contain at least one task">>];
validate_tasks(_Tasks) ->
    [].

%%====================================================================
%% Internal Functions - Compilation
%%====================================================================

%% @private
build_net(#yawl_spec{root_net = RootNet, tasks = Tasks}) ->
    InputPlace = binary_to_atom(<<RootNet/binary, "_input">>, utf8),
    OutputPlace = binary_to_atom(<<RootNet/binary, "_output">>, utf8),

    TaskPlaces = lists:map(fun(TaskId) ->
        binary_to_atom(<<(atom_to_binary(TaskId, utf8))/binary, "_place">>, utf8)
    end, maps:keys(Tasks)),

    AllPlaces = [InputPlace, OutputPlace | TaskPlaces],

    TaskTransitions = lists:map(fun(TaskId) ->
        binary_to_atom(<<(atom_to_binary(TaskId, utf8))/binary, "_transition">>, utf8)
    end, maps:keys(Tasks)),

    Preset = build_preset(InputPlace, OutputPlace, Tasks),
    Postset = build_postset(InputPlace, OutputPlace, Tasks),

    {AllPlaces, TaskTransitions, Preset, Postset}.

%% @private
build_preset(InputPlace, OutputPlace, Tasks) ->
    TaskIds = maps:keys(Tasks),
    lists:foldl(fun(TaskId, Acc) ->
        Transition = binary_to_atom(<<(atom_to_binary(TaskId, utf8))/binary, "_transition">>, utf8),
        TaskPlace = binary_to_atom(<<(atom_to_binary(TaskId, utf8))/binary, "_place">>, utf8),
        Acc#{Transition => [InputPlace, TaskPlace]}
    end, #{OutputPlace => []}, TaskIds).

%% @private
build_postset(InputPlace, OutputPlace, Tasks) ->
    TaskIds = maps:keys(Tasks),
    lists:foldl(fun(TaskId, Acc) ->
        Transition = binary_to_atom(<<(atom_to_binary(TaskId, utf8))/binary, "_transition">>, utf8),
        TaskPlace = binary_to_atom(<<(atom_to_binary(TaskId, utf8))/binary, "_place">>, utf8),
        Acc#{Transition => [TaskPlace, OutputPlace]}
    end, #{InputPlace => []}, TaskIds).

%%====================================================================
%% Doctests
%%====================================================================

-spec doctest_test() -> ok.

doctest_test() ->
    Xml = <<
        "<?xml version='1.0' encoding='UTF-8'?>\n"
        "<specificationSet id='demo' version='2.2'>\n"
        "  <specification uri='demo'>\n"
        "    <metaData><title>Demo</title></metaData>\n"
        "    <decomposition id='main' isRootNet='true'>\n"
        "      <processControlElements>\n"
        "        <inputCondition id='i'/>\n"
        "        <outputCondition id='o'/>\n"
        "        <task id='t1' type='human'>\n"
        "          <name>Approve</name>\n"
        "          <documentation>Approve an order</documentation>\n"
        "          <join code='xor'/>\n"
        "          <split code='xor'/>\n"
        "        </task>\n"
        "        <flowsInto><from id='i'/><to id='t1'/></flowsInto>\n"
        "        <flowsInto><from id='t1'/><to id='o'/></flowsInto>\n"
        "      </processControlElements>\n"
        "    </decomposition>\n"
        "  </specification>\n"
        "</specificationSet>\n"
    >>,

    {ok, Spec} = from_xml(Xml),
    demo = id(Spec),
    main = root_net(Spec),
    [t1] = tasks(Spec),
    <<"Approve an order">> = task_doc(Spec, t1),

    ok = validate(Spec),

    {ok, Compiled} = compile(Spec),
    true = length(places(Compiled)) > 0,
    true = length(transitions(Compiled)) > 0,

    MinimalXml = <<
        "<?xml version='1.0'?>\n"
        "<specificationSet id='test'>\n"
        "  <specification uri='test'>\n"
        "    <decomposition id='root' isRootNet='true'>\n"
        "      <processControlElements>\n"
        "        <task id='t1'/>\n"
        "      </processControlElements>\n"
        "    </decomposition>\n"
        "  </specification>\n"
        "</specificationSet>\n"
    >>,
    {ok, MinSpec} = from_xml(MinimalXml),
    test = id(MinSpec),
    root = root_net(MinSpec),
    [t1] = tasks(MinSpec),

    <<"2.2">> = version(Spec),

    human = task_type(Spec, t1),
    atomic = task_type(MinSpec, t1),

    ok.
