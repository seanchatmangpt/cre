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
%% @doc YAWL Utility Functions
%%
%% This module provides common utility functions for YAWL workflow
%% processing in the CRE runtime environment.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>ID generation for cases, work items, and elements</li>
%%   <li>Timestamp generation and formatting</li>
%%   <li>XML parsing and formatting using xmerl</li>
%%   <li>XPath validation and evaluation</li>
%%   <li>Data structure utilities (deep merge, proplist to map conversion)</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% ID generation:
%% ```erlang
%% > Id = yawl_util:generate_id(),
%% is_binary(Id).
%% true
%%
%% > PrefixedId = yawl_util:generate_id(<<"test">>),
%% byte_size(PrefixedId) > 4.
%% true
%%
%% > CaseId = yawl_util:generate_case_id(),
%% binary:match(CaseId, <<"case_">>) =/= nomatch.
%% true
%%
%% > WorkitemId = yawl_util:generate_workitem_id(<<"case_123">>),
%% binary:match(WorkitemId, <<"case_123_wi_">>) =/= nomatch.
%% true
%% ```
%%
%% Type conversions:
%% ```erlang
%% > yawl_util:to_binary(<<"already">>).
%% <<"already">>
%%
%% > yawl_util:to_binary(atom).
%% <<"atom">>
%%
%% > yawl_util:to_list(<<"binary">>).
%% "binary"
%%
%% > yawl_util:to_atom(<<"binary_atom">>).
%% 'binary_atom'
%% ```
%%
%% Map utilities:
%% ```erlang
%% > yawl_util:sanitize_map(#{a => 1, b => undefined}).
%% #{a => 1}
%%
%% > yawl_util:proplist_to_map([{a, 1}, {b, 2}]).
%% #{<<"a">> => 1, <<"b">> => 2}
%% ```
%%
%% XML escaping:
%% ```erlang
%% > yawl_util:escape_xml(<<"<tag>">>).
%% <<"&lt;tag&gt;">>
%%
%% > yawl_util:unescape_xml(<<"&lt;tag&gt;">>).
%% <<"<tag>">>
%% ```
%%
%% Running the doctests:
%% ```erlang
%% > yawl_util:doctest_test().
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_util).
-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% ID Generation
-export([generate_id/0, generate_id/1,
         generate_case_id/0,
         generate_workitem_id/1,
         generate_task_id/0]).

%% Timestamp Functions
-export([timestamp/0,
         timestamp_to_binary/0,
         timestamp_to_binary/1,
         format_timestamp/1,
         parse_timestamp/1]).

%% XML Functions
-export([format_xml/1,
         parse_xml/1,
         xml_to_binary/1,
         escape_xml/1,
         unescape_xml/1]).

%% XPath Functions
-export([validate_xpath/1,
         evaluate_xpath/2,
         extract_xpath_value/2]).

%% Data Structure Utilities
-export([deep_merge/2,
         proplist_to_map/1,
         map_to_proplist/1,
         keys_to_atoms/1,
         keys_to_binaries/1,
         sanitize_map/1]).

%% Misc Utilities
-export([to_binary/1,
         to_list/1,
         to_atom/1,
         ensure_binary/1,
         uuid_v4/0,
         hash/1]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Types
%%====================================================================

-type xpath() :: binary().
-type xml_element() :: term().
-type timestamp() :: integer().

%%====================================================================
%% ID Generation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a unique identifier.
%%
%% Uses a combination of timestamp, process ID, and a random component
%% to ensure uniqueness across distributed systems.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_id() -> binary().

generate_id() ->
    generate_id(<<"id">>).

%%--------------------------------------------------------------------
%% @doc Generates a unique identifier with a prefix.
%%
%% The prefix is prepended to the generated ID for easier identification.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_id(Prefix :: binary()) -> binary().

generate_id(Prefix) when is_binary(Prefix) ->
    Timestamp = erlang:unique_integer([positive]),
    PidBin = term_to_binary(self()),
    Hash = binary:encode_hex(crypto:hash(md5, <<PidBin/binary, Timestamp:64>>)),
    <<Prefix/binary, "_", Hash/binary>>.

%%--------------------------------------------------------------------
%% @doc Generates a unique case ID.
%%
%% Case IDs include a timestamp component for ordering and a
%% unique hash for identification.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().

generate_case_id() ->
    Timestamp = erlang:system_time(millisecond),
    Unique = erlang:unique_integer([positive]),
    Hash = binary:encode_hex(crypto:hash(md5, <<Timestamp:64, Unique:64>>)),
    <<"case_", Hash/binary>>.

%%--------------------------------------------------------------------
%% @doc Generates a unique work item ID for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_workitem_id(CaseId :: binary()) -> binary().

generate_workitem_id(CaseId) ->
    Unique = erlang:unique_integer([positive]),
    Hash = binary:encode_hex(crypto:hash(md5, <<CaseId/binary, Unique:64>>)),
    <<CaseId/binary, "_wi_", Hash/binary>>.

%%--------------------------------------------------------------------
%% @doc Generates a unique task ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_task_id() -> binary().

generate_task_id() ->
    generate_id(<<"task">>).

%%====================================================================
%% Timestamp Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the current system time in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> timestamp().

timestamp() ->
    erlang:system_time(millisecond).

%%--------------------------------------------------------------------
%% @doc Returns the current timestamp as a binary string.
%%
%% Format: ISO 8601 (YYYY-MM-DDTHH:MM:SS.mmmZ)
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_binary() -> binary().

timestamp_to_binary() ->
    timestamp_to_binary(timestamp()).

%%--------------------------------------------------------------------
%% @doc Converts a millisecond timestamp to ISO 8601 binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_binary(timestamp()) -> binary().

timestamp_to_binary(Millis) when is_integer(Millis) ->
    Seconds = Millis div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Seconds, second),

    Micros = Millis rem 1000 * 1000,

    Format = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B~3.10.0BZ",
                           [Year, Month, Day, Hour, Minute, Second,
                            Micros div 1000, Micros rem 1000]),

    list_to_binary(Format).

%%--------------------------------------------------------------------
%% @doc Formats a timestamp for display.
%%
%% Returns a human-readable string.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_timestamp(timestamp()) -> binary().

format_timestamp(Millis) ->
    timestamp_to_binary(Millis).

%%--------------------------------------------------------------------
%% @doc Parses a timestamp string back to milliseconds.
%%
%% Supports ISO 8601 format.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_timestamp(binary() | string()) ->
          {ok, timestamp()} | {error, invalid_format}.

parse_timestamp(TsString) when is_binary(TsString) ->
    parse_timestamp(binary_to_list(TsString));
parse_timestamp(TsString) when is_list(TsString) ->
    try
        %% Parse ISO 8601 format: YYYY-MM-DDTHH:MM:SS.mmmZ
        case re:run(TsString,
                    "^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})\\.([0-9]{3})Z$",
                    [{capture, all, list}]) of
            {match, [_, Y, M, D, H, Min, S, Ms]} ->
                Year = list_to_integer(Y),
                Month = list_to_integer(M),
                Day = list_to_integer(D),
                Hour = list_to_integer(H),
                Minute = list_to_integer(Min),
                Second = list_to_integer(S),
                Millisecond = list_to_integer(Ms),

                DateTime = {{Year, Month, Day}, {Hour, Minute, Second}},
                Seconds = calendar:datetime_to_gregorian_seconds(DateTime) -
                          calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),

                {ok, Seconds * 1000 + Millisecond};
            _ ->
                {error, invalid_format}
        end
    catch
        _:_ ->
            {error, invalid_format}
    end.

%%====================================================================
%% XML Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Formats an XML element to a readable binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_xml(xml_element()) -> binary().

format_xml(XmlElement) ->
    try
        XmlString = xmerl:export_simple([XmlElement], xmerl_xml),
        list_to_binary(XmlString)
    catch
        _:_ ->
            <<"<!-- XML formatting error -->">>
    end.

%%--------------------------------------------------------------------
%% @doc Parses an XML binary/string into an xmerl element.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_xml(binary() | string()) ->
          {ok, xml_element()} | {error, term()}.

parse_xml(XmlContent) when is_binary(XmlContent) ->
    parse_xml(binary_to_list(XmlContent));
parse_xml(XmlString) when is_list(XmlString) ->
    try
        {Element, _Rest} = xmerl_scan:string(XmlString, [
            {namespace_conformant, true},
            {comments, false}
        ]),
        {ok, Element}
    catch
        Kind:Reason:Stack ->
            {error, {Kind, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Converts an xmerl element to binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec xml_to_binary(xml_element()) -> binary().

xml_to_binary(XmlElement) ->
    format_xml(XmlElement).

%%--------------------------------------------------------------------
%% @doc Escapes special XML characters in a string.
%%
%% Replaces: < -> &lt;, > -> &gt;, & -> &amp;, " -> &quot;, ' -> &apos;
%%
%% Example:
%% ```erlang
%% > yawl_util:escape_xml(<<"<tag>">>).
%% <<"&lt;tag&gt;">>
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec escape_xml(binary() | string()) -> binary().

escape_xml(Text) when is_binary(Text) ->
    escape_xml(binary_to_list(Text));
escape_xml(Text) when is_list(Text) ->
    Bin = list_to_binary(Text),
    %% Order matters: & must be escaped first
    AmpEscaped = binary:replace(Bin, <<"&">>, <<"&amp;">>, [global]),
    LtEscaped = binary:replace(AmpEscaped, <<"<">>, <<"&lt;">>, [global]),
    GtEscaped = binary:replace(LtEscaped, <<">">>, <<"&gt;">>, [global]),
    QuotEscaped = binary:replace(GtEscaped, <<"\"">>, <<"&quot;">>, [global]),
    AposEscaped = binary:replace(QuotEscaped, <<"'">>, <<"&apos;">>, [global]),
    AposEscaped.

%%--------------------------------------------------------------------
%% @doc Unescapes XML entities in a string.
%%
%% Example:
%% ```erlang
%% > yawl_util:unescape_xml(<<"&lt;tag&gt;">>).
%% <<"<tag>">>
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec unescape_xml(binary() | string()) -> binary().

unescape_xml(Text) when is_binary(Text) ->
    unescape_xml(binary_to_list(Text));
unescape_xml(Text) when is_list(Text) ->
    Bin = list_to_binary(Text),
    %% Order matters: &amp; must be unescaped last
    LtUnescaped = binary:replace(Bin, <<"&lt;">>, <<"<">>, [global]),
    GtUnescaped = binary:replace(LtUnescaped, <<"&gt;">>, <<">">>, [global]),
    QuotUnescaped = binary:replace(GtUnescaped, <<"&quot;">>, <<"\"">>, [global]),
    AposUnescaped = binary:replace(QuotUnescaped, <<"&apos;">>, <<"'">>, [global]),
    AmpUnescaped = binary:replace(AposUnescaped, <<"&amp;">>, <<"&">>, [global]),
    AmpUnescaped.

%%====================================================================
%% XPath Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates an XPath expression string.
%%
%% Checks basic syntax validity. Does not execute the expression.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_xpath(xpath()) -> ok | {error, invalid_xpath}.

validate_xpath(XPath) when is_binary(XPath) ->
    XPathStr = binary_to_list(XPath),
    try
        %% Basic syntax validation using regex
        case re:run(XPathStr, "^(/|//|[a-zA-Z_])") of
            {match, _} -> ok;
            nomatch -> {error, invalid_xpath}
        end
    catch
        _:_ ->
            {error, invalid_xpath}
    end;
validate_xpath(_) ->
    {error, invalid_xpath}.

%%--------------------------------------------------------------------
%% @doc Evaluates an XPath expression against an XML document.
%%
%% Returns a list of matching nodes/values.
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_xpath(XmlDoc :: xml_element(), XPath :: xpath()) ->
          {ok, [term()]} | {error, term()}.

evaluate_xpath(XmlDoc, XPath) when is_binary(XPath) ->
    case validate_xpath(XPath) of
        ok ->
            try
                %% Use xmerl XPath support
                XPathStr = binary_to_list(XPath),
                Result = xmerl_xpath:string(XmlDoc, XPathStr),
                {ok, Result}
            catch
                Kind:Reason ->
                    {error, {Kind, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Extracts a single string value from an XPath evaluation.
%%
%% Returns the first match as a binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_xpath_value(xml_element(), xpath()) ->
          {ok, binary()} | {error, not_found | term()}.

extract_xpath_value(XmlDoc, XPath) ->
    case evaluate_xpath(XmlDoc, XPath) of
        {ok, [First | _]} ->
            case First of
                #xmlText{value = Value} ->
                    {ok, list_to_binary(Value)};
                #xmlElement{content = Content} ->
                    Text = extract_text_content(Content),
                    {ok, Text};
                _ ->
                    {ok, list_to_binary(io_lib:format("~p", [First]))}
            end;
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Extracts text content from XML element content list.
-spec extract_text_content([term()]) -> binary().

extract_text_content(Content) ->
    Text = lists:foldl(fun
        (#xmlText{value = V}, Acc) ->
            [V | Acc];
        (_, Acc) ->
            Acc
    end, [], Content),
    list_to_binary(lists:reverse(Text)).

%%====================================================================
%% Data Structure Utilities
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Deep merges two maps.
%%
%% Nested maps are merged recursively. Lists are replaced, not merged.
%%
%% @end
%%--------------------------------------------------------------------
-spec deep_merge(map(), map()) -> map().

deep_merge(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    maps:fold(fun(K, V2, Acc) ->
        case maps:get(K, Acc, undefined) of
            undefined ->
                Acc#{K => V2};
            V1 when is_map(V1), is_map(V2) ->
                Acc#{K => deep_merge(V1, V2)};
            _ ->
                Acc#{K => V2}
        end
    end, Map1, Map2).

%%--------------------------------------------------------------------
%% @doc Converts a proplist to a map.
%%
%% Supports nested proplists.
%%
%% @end
%%--------------------------------------------------------------------
-spec proplist_to_map([{atom() | binary(), term()}]) -> map().

proplist_to_map(Proplist) when is_list(Proplist) ->
    lists:foldl(fun
        ({Key, Value}, Acc) when is_list(Value), length(Value) > 0, is_tuple(hd(Value)) ->
            %% Check if it's a nested proplist
            case is_proplist(Value) of
                true -> Acc#{ensure_binary(Key) => proplist_to_map(Value)};
                false -> Acc#{ensure_binary(Key) => Value}
            end;
        ({Key, Value}, Acc) ->
            Acc#{ensure_binary(Key) => Value};
        (_, Acc) ->
            Acc
    end, #{}, Proplist).

%% @private
%% @doc Checks if a list looks like a proplist.
-spec is_proplist(list()) -> boolean().

is_proplist([]) ->
    true;
is_proplist([{K, _V} | Rest]) when is_atom(K); is_binary(K) ->
    is_proplist(Rest);
is_proplist(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Converts a map to a proplist.
%%
%% @end
%%--------------------------------------------------------------------
-spec map_to_proplist(map()) -> [{binary(), term()}].

map_to_proplist(Map) when is_map(Map) ->
    maps:to_list(Map).

%%--------------------------------------------------------------------
%% @doc Converts all map keys to atoms.
%%
%% Note: This can fail if keys are not valid atom names.
%%
%% @end
%%--------------------------------------------------------------------
-spec keys_to_atoms(map()) -> map().

keys_to_atoms(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = case K of
            Bin when is_binary(Bin) -> binary_to_existing_atom(Bin, utf8);
            Atom when is_atom(Atom) -> Atom;
            List when is_list(List) -> list_to_existing_atom(List)
        end,
        Acc#{Key => V}
    end, #{}, Map).

%%--------------------------------------------------------------------
%% @doc Converts all map keys to binaries.
%%
%% @end
%%--------------------------------------------------------------------
-spec keys_to_binaries(map()) -> map().

keys_to_binaries(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = ensure_binary(K),
        Acc#{Key => V}
    end, #{}, Map).

%%--------------------------------------------------------------------
%% @doc Removes undefined values from a map.
%%
%% @end
%%--------------------------------------------------------------------
-spec sanitize_map(map()) -> map().

sanitize_map(Map) when is_map(Map) ->
    maps:filter(fun(_K, V) ->
        V =/= undefined andalso V =/= null
    end, Map).

%%====================================================================
%% Misc Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts a value to binary.
%%
%% Supports atom, list, integer, float, and binary input.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_binary(atom() | list() | integer() | float() | binary()) -> binary().

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    try
        list_to_binary(Value)
    catch
        error:_ ->
            list_to_binary(io_lib:format("~p", [Value]))
    end;
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
to_binary(Value) when is_float(Value) ->
    list_to_binary(float_to_list(Value));
to_binary(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

%%--------------------------------------------------------------------
%% @doc Converts a value to a list (string).
%%
%% @end
%%--------------------------------------------------------------------
-spec to_list(atom() | binary() | list() | integer() | float()) -> list().

to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_list(Value) when is_float(Value) ->
    float_to_list(Value);
to_list(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).

%%--------------------------------------------------------------------
%% @doc Converts a value to an atom.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_atom(binary() | list() | atom()) -> atom().

to_atom(Value) when is_atom(Value) ->
    Value;
to_atom(Value) when is_binary(Value) ->
    binary_to_atom(Value, utf8);
to_atom(Value) when is_list(Value) ->
    list_to_atom(Value).

%%--------------------------------------------------------------------
%% @doc Ensures a value is a binary, converting if necessary.
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_binary(binary() | atom() | list() | integer() | float()) -> binary().

ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) ->
    to_binary(Value).

%%--------------------------------------------------------------------
%% @doc Generates a UUID v4.
%%
%% Returns a 36-character UUID string without dashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec uuid_v4() -> binary().

uuid_v4() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    % Set version bits (4) and variant bits (2)
    C1 = (C band 16#0FFF) bor 16#4000,
    D1 = (D band 16#3FFF) bor 16#8000,
    iolist_to_binary([
        io_lib:format("~8.16.0b~4.16.0b~4.16.0b~4.16.0b~12.16.0b",
                     [A, B, C1, D1, E])
    ]).

%%--------------------------------------------------------------------
%% @doc Computes a hash of the input value.
%%
%% Uses MD5 algorithm by default.
%%
%% @end
%%--------------------------------------------------------------------
-spec hash(term()) -> binary().

hash(Value) ->
    Data = term_to_binary(Value),
    crypto:hash(md5, Data).

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Run doctests for yawl_util module.
%%
%% Validates utility functions including ID generation, type conversion,
%% map operations, and XML escaping.
%%
%% Example:
%% ```erlang
%% > yawl_util:doctest_test().
%% ok
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: generate_id/0 returns binary with prefix
    Id = generate_id(),
    true = is_binary(Id),
    true = byte_size(Id) > 3,

    %% Test 2: generate_id/1 with custom prefix
    CustomId = generate_id(<<"custom">>),
    true = is_binary(CustomId),
    <<"custom_", _/binary>> = CustomId,

    %% Test 3: generate_case_id returns proper format
    CaseId = generate_case_id(),
    true = is_binary(CaseId),
    <<"case_", _/binary>> = CaseId,

    %% Test 4: generate_workitem_id includes case_id
    BaseCaseId = <<"case_abc123">>,
    WorkitemId = generate_workitem_id(BaseCaseId),
    true = is_binary(WorkitemId),
    true = binary:match(WorkitemId, BaseCaseId) =/= nomatch,

    %% Test 5: generate_task_id uses "task" prefix
    TaskId = generate_task_id(),
    true = is_binary(TaskId),
    <<"task_", _/binary>> = TaskId,

    %% Test 6: timestamp returns positive integer
    Ts = timestamp(),
    true = is_integer(Ts),
    true = Ts > 0,

    %% Test 7: timestamp_to_binary returns ISO 8601 format
    TsBin = timestamp_to_binary(),
    true = is_binary(TsBin),
    true = byte_size(TsBin) > 0,

    %% Test 8: parse_timestamp handles valid format
    {ok, ParsedTs} = parse_timestamp(<<"2024-01-01T12:00:00.000Z">>),
    true = is_integer(ParsedTs),

    %% Test 9: parse_timestamp rejects invalid format
    {error, invalid_format} = parse_timestamp(<<"invalid">>),

    %% Test 10: to_binary handles various types
    true = to_binary(<<"binary">>) =:= <<"binary">>,
    true = to_binary(atom) =:= <<"atom">>,
    true = is_binary(to_binary(123)),
    true = is_binary(to_binary(1.5)),

    %% Test 11: to_list handles various types
    "binary" = to_list(<<"binary">>),
    "atom" = to_list(atom),
    "123" = to_list(123),

    %% Test 12: to_atom handles various types
    atom = to_atom(<<"atom">>),
    atom = to_atom(atom),
    'test_atom' = to_atom("test_atom"),

    %% Test 13: ensure_binary returns binary
    true = is_binary(ensure_binary(<<"already">>)),
    true = is_binary(ensure_binary(atom)),
    true = is_binary(ensure_binary(123)),

    %% Test 14: deep_merge combines maps
    Map1 = #{a => 1, b => #{x => 10}},
    Map2 = #{c => 3, b => #{y => 20}},
    Merged = deep_merge(Map1, Map2),
    true = maps:get(a, Merged) =:= 1,
    true = maps:get(c, Merged) =:= 3,
    true = maps:get(x, maps:get(b, Merged)) =:= 10,
    true = maps:get(y, maps:get(b, Merged)) =:= 20,

    %% Test 15: proplist_to_map converts proplist
    Plist = [{a, 1}, {b, 2}, {c, 3}],
    Pmap = proplist_to_map(Plist),
    true = is_map(Pmap),
    3 = maps:size(Pmap),
    true = lists:all(fun(K) -> lists:member(K, [<<"a">>, <<"b">>, <<"c">>]) end, maps:keys(Pmap)),

    %% Test 16: map_to_proplist converts map
    TestMap = #{<<"x">> => 1, <<"y">> => 2},
    PlistResult = map_to_proplist(TestMap),
    true = is_list(PlistResult),
    true = length(PlistResult) =:= 2,

    %% Test 17: keys_to_binaries converts all keys
    MixedKeysMap = #{a => 1, <<"b">> => 2, "c" => 3},
    BinKeysMap = keys_to_binaries(MixedKeysMap),
    true = lists:all(fun(K) -> is_binary(K) end, maps:keys(BinKeysMap)),

    %% Test 18: sanitize_map removes undefined/null values
    DirtyMap = #{a => 1, b => undefined, c => null, d => 2},
    CleanMap = sanitize_map(DirtyMap),
    true = maps:get(a, CleanMap) =:= 1,
    true = maps:get(d, CleanMap) =:= 2,
    false = maps:is_key(b, CleanMap),
    false = maps:is_key(c, CleanMap),

    %% Test 19: escape_xml handles special characters
    Escaped = escape_xml(<<"<tag>&'\"</tag>">>),
    true = binary:match(Escaped, <<"&lt;">>) =/= nomatch,
    true = binary:match(Escaped, <<"&gt;">>) =/= nomatch,
    true = binary:match(Escaped, <<"&amp;">>) =/= nomatch,

    %% Test 20: unescape_xml reverses escape_xml
    Unescaped = unescape_xml(<<"&lt;tag&gt;">>),
    <<"<tag>">> = Unescaped,

    %% Test 21: validate_xpath accepts valid XPath
    ok = validate_xpath(<<"root/child">>),
    ok = validate_xpath(<<"//element">>),
    ok = validate_xpath(<<"/absolute/path">>),

    %% Test 22: validate_xpath rejects invalid input
    {error, invalid_xpath} = validate_xpath(<<"123invalid">>),

    %% Test 23: uuid_v4 generates 32-char hex string
    Uuid = uuid_v4(),
    true = is_binary(Uuid),
    32 = byte_size(Uuid),

    %% Test 24: hash returns binary
    HashResult = hash(<<"test">>),
    true = is_binary(HashResult),
    16 = byte_size(HashResult),  % MD5 is 16 bytes

    ok.
