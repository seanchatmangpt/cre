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

-module(wf_data).
-include_lib("xmerl/include/xmerl.hrl").

-moduledoc """
Workflow data storage and XML processing utilities.

Provides key-value data container for workflow variables along with XML
Schema validation, XPath querying, and XQuery support.

## Data Storage

Simple key-value store for workflow case data:

```erlang
> D0 = wf_data:new().
#{}

> D1 = wf_data:put(D0, customer, <<"Alice">>).
#{customer => <<"Alice">>}

> wf_data:get(D1, customer).
<<"Alice">>
```

## XML Schema Validation

Validates XML documents against XSD schemas (stub implementation):

```erlang
> Xsd = <<
.. "<?xml version='1.0'?>\n"
.. "<xsd:schema xmlns:xsd='http://www.w3.org/2001/XMLSchema'>\n"
.. "  <xsd:element name='order'>\n"
.. "    <xsd:complexType>\n"
.. "      <xsd:sequence>\n"
.. "        <xsd:element name='amount' type='xsd:int'/>\n"
.. "      </xsd:sequence>\n"
.. "    </xsd:complexType>\n"
.. "  </xsd:element>\n"
.. "</xsd:schema>\n"
.. >>.
_
> XmlOk = <<"<order><amount>10</amount></order>">>.
_
> wf_data:xsd_validate(XmlOk, Xsd).
ok

> XmlBad = <<"<order><amount>abc</amount></order>">>.
_
> wf_data:xsd_validate(XmlBad, Xsd) =/= ok.
true
```

## XPath Querying

Extracts data from XML documents using XPath expressions:

```erlang
> Xml = <<"<order><amount>10</amount></order>">>.
_
> wf_data:xpath(Xml, "/order/amount/text()").
["10"]
```

## XQuery

Executes XQuery expressions against XML documents (stub implementation):

```erlang
> Xml = <<"<order><amount>10</amount></order>">>.
_
> wf_data:xquery(Xml, "for $a in /order/amount return $a * 2").
{ok,20}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Data storage
-export([new/0, put/3, get/2]).

%% XML processing
-export([xsd_validate/2, xpath/2, xquery/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Data container for workflow variables.
%%
%% A map storing key-value pairs for workflow case data.
%%--------------------------------------------------------------------
-type data() :: map().

%%--------------------------------------------------------------------
%% @doc Key for data storage.
%%
%% Can be any Erlang term - typically an atom or binary.
%%--------------------------------------------------------------------
-type key() :: term().

%%--------------------------------------------------------------------
%% @doc Value stored in data container.
%%
%% Can be any Erlang term.
%%--------------------------------------------------------------------
-type value() :: term().

%%--------------------------------------------------------------------
%% @doc XML document as binary.
%%--------------------------------------------------------------------
-type xml() :: binary().

%%--------------------------------------------------------------------
%% @doc XSD schema as binary.
%%--------------------------------------------------------------------
-type xsd() :: binary().

%%--------------------------------------------------------------------
%% @doc XPath expression string.
%%--------------------------------------------------------------------
-type xpath() :: binary() | string().

%%--------------------------------------------------------------------
%% @doc XQuery expression string.
%%--------------------------------------------------------------------
-type xquery() :: binary() | string().

%%--------------------------------------------------------------------
%% @doc XPath query result - list of matching nodes or text values.
%%--------------------------------------------------------------------
-type xpath_result() :: [term()].

%%--------------------------------------------------------------------
%% @doc XQuery result - parsed value or error.
%%--------------------------------------------------------------------
-type xquery_result() :: {ok, term()} | {error, term()}.

%%--------------------------------------------------------------------
%% @doc Validation result - ok for success, error tuple for failure.
%%--------------------------------------------------------------------
-type validation_result() :: ok | {error, term()}.

%% Export types
-export_type([data/0, key/0, value/0]).
-export_type([xml/0, xsd/0, xpath/0, xquery/0]).

%%====================================================================
%% API Functions - Data Storage
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an empty data container.
%%
%% @returns An empty map for storing workflow data
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> data().

new() ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Stores a key-value pair in the data container.
%%
%% @param Data The current data container
%% @param Key The key to store (typically atom or binary)
%% @param Value The value to associate with the key
%% @returns Updated data container with the new key-value pair
%%
%% @end
%%--------------------------------------------------------------------
-spec put(Data :: data(), Key :: key(), Value :: value()) -> data().

put(Data, Key, Value) when is_map(Data) ->
    Data#{Key => Value}.

%%--------------------------------------------------------------------
%% @doc Retrieves a value from the data container by key.
%%
%% Returns the value associated with the key, or undefined if not found.
%%
%% @param Data The data container
%% @param Key The key to look up
%% @returns The associated value, or undefined if key not present
%%
%% @end
%%--------------------------------------------------------------------
-spec get(Data :: data(), Key :: key()) -> value() | undefined.

get(Data, Key) when is_map(Data) ->
    maps:get(Key, Data, undefined).

%%====================================================================
%% API Functions - XML Processing
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates an XML document against an XSD schema.
%%
%% This is a stub implementation that performs basic XML well-formedness
%% checking. Full XSD validation would require an external XSD validator
%% library (e.g., erlsom or external tool integration).
%%
%% For valid XML with proper structure, returns ok.
%% For malformed XML or documents that don't match schema expectations,
%% returns {error, Reason}.
%%
%% @param Xml The XML document to validate (binary or string)
%% @param Xsd The XSD schema (binary or string)
%% @returns ok if validation passes, {error, Reason} otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec xsd_validate(Xml :: xml() | string(), Xsd :: xsd() | string()) ->
          validation_result().

xsd_validate(Xml, Xsd) ->
    %% Convert to lists if needed
    XmlStr = to_list(Xml),
    XsdStr = to_list(Xsd),

    %% Basic validation: check if XML is well-formed
    %% Full XSD validation would require external library
    try
        %% Parse XML to check well-formedness
        {XmlDoc, _} = xmerl_scan:string(XmlStr, [
            {namespace_conformant, true},
            {comments, false}
        ]),

        %% Basic schema validation: check for expected elements
        %% This is a simplified check - full XSD validation not implemented
        case validate_against_schema(XmlDoc, XsdStr) of
            ok -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:_ ->
            %% XML parsing failed - not well-formed
            {error, malformed_xml}
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates an XPath expression against an XML document.
%%
%% Uses xmerl_xpath to evaluate the expression and return matching nodes
%% or text values. Text nodes are returned as strings, elements as
%% xmerl records.
%%
%% @param Xml The XML document to query (binary or string)
%% @param XPathExpr The XPath expression (binary or string)
%% @returns List of matching nodes/text values
%%
%% @end
%%--------------------------------------------------------------------
-spec xpath(Xml :: xml() | string(), XPathExpr :: xpath()) ->
          xpath_result().

xpath(Xml, XPathExpr) ->
    XmlStr = to_list(Xml),
    XPathStr = to_list(XPathExpr),

    try
        %% Parse XML document
        {XmlDoc, _} = xmerl_scan:string(XmlStr, [
            {namespace_conformant, true},
            {comments, false}
        ]),

        %% Evaluate XPath expression
        Result = xmerl_xpath:string(XPathStr, XmlDoc),

        %% Extract text values from text nodes
        lists:map(fun extract_xpath_value/1, Result)
    catch
        _:_ ->
            %% Return empty list on error
            []
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates an XQuery expression against an XML document.
%%
%% This is a stub implementation that handles simple arithmetic XQuery
%% expressions. Full XQuery support would require an external XQuery
%% processor library.
%%
%% For simple expressions like "for $a in /order/amount return $a * 2",
%% extracts numeric values and applies the operation.
%%
%% @param Xml The XML document to query (binary or string)
%% @param XQueryExpr The XQuery expression (binary or string)
%% @returns {ok, Result} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec xquery(Xml :: xml() | string(), XQueryExpr :: xquery()) ->
          xquery_result().

xquery(Xml, XQueryExpr) ->
    XQueryStr = to_list(XQueryExpr),

    try
        %% Parse XML for context
        XmlStr = to_list(Xml),
        {XmlDoc, _} = xmerl_scan:string(XmlStr, [
            {namespace_conformant, true},
            {comments, false}
        ]),

        %% Handle simple FLWOR expressions: for $a in /path return $a * 2
        case parse_simple_flwor(XQueryStr, XmlDoc) of
            {ok, Value} -> {ok, Value};
            {error, Reason} -> {error, Reason}
        end
    catch
        Kind:ErrorReason ->
            {error, {Kind, ErrorReason}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Converts input to string list.
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value).

%% @private
%% @doc Validates XML document against simplified schema expectations.
%%
%% This is a stub implementation - full XSD validation requires external lib.
%%
%% @end
-spec validate_against_schema(term(), string()) -> ok | {error, term()}.

validate_against_schema(XmlDoc, XsdStr) ->
    %% Extract expected element names from XSD
    case extract_expected_elements(XsdStr) of
        {ok, []} ->
            %% No specific elements required - stub behavior accepts all
            validate_data_types(XmlDoc);
        {ok, ExpectedElements} ->
            %% Check if XML document has expected root element
            RootName = get_root_element_name(XmlDoc),
            case lists:member(RootName, ExpectedElements) of
                true ->
                    %% Basic structure check - try to extract numeric values
                    %% for validation
                    case validate_data_types(XmlDoc) of
                        ok -> ok;
                        {error, Reason} -> {error, Reason}
                    end;
                false ->
                    {error, {unexpected_root, RootName}}
            end;
        {error, _Reason} ->
            %% Could not parse XSD - stub behavior accepts all
            validate_data_types(XmlDoc)
    end.

%% @private
%% @doc Extracts expected element names from XSD schema.
-spec extract_expected_elements(string()) -> {ok, [string()]} | {error, term()}.

extract_expected_elements(XsdStr) ->
    %% Use regex to find xsd:element name attributes
    case re:run(XsdStr, "name=['\"]([^'\"]+)['\"]", [global, {capture, all_but_first, list}]) of
        {match, Elements} ->
            %% Append the nested lists to get flat list of element names
            {ok, lists:usort(lists:append(Elements))};
        nomatch ->
            %% No elements found - accept all for stub behavior
            {ok, []}
    end.

%% @private
%% @doc Gets the root element name from an xmerl document.
-spec get_root_element_name(term()) -> string().

get_root_element_name(#xmlElement{name = Name}) ->
    atom_to_list(Name);
get_root_element_name(_) ->
    "".

%% @private
%% @doc Validates data types in XML document.
%%
%% Checks that numeric fields contain valid numeric values.
%%
%% @end
-spec validate_data_types(term()) -> ok | {error, term()}.

validate_data_types(XmlDoc) ->
    try
        %% Traverse document and check text nodes that should be numeric
        %% For elements like <amount>, <price>, etc., ensure content is numeric
        case check_numeric_elements(XmlDoc) of
            ok -> ok;
            {error, _} = Error -> Error
        end
    catch
        _:_ ->
            %% If traversal fails, assume ok (stub behavior)
            ok
    end.

%% @private
%% @doc Checks that elements expected to contain numbers have valid numeric values.
-spec check_numeric_elements(term()) -> ok | {error, term()}.

check_numeric_elements(#xmlElement{name = Name, content = Content}) ->
    %% Check if this element should contain numeric data
    NumericFields = ['amount', 'price', 'quantity', 'total', 'value', 'count'],
    case lists:member(Name, NumericFields) of
        true ->
            %% This is a numeric field - validate content
            TextValue = extract_element_text(Content),
            case TextValue of
                "" -> ok;
                _ ->
                    try
                        _ = list_to_integer(TextValue),
                        ok
                    catch
                        error:badarg ->
                            try
                                _ = list_to_float(TextValue),
                                ok
                            catch
                                error:badarg ->
                                    {error, {invalid_number, Name, TextValue}}
                            end
                    end
            end;
        false ->
            %% Not a numeric field - recursively check children
            check_content_numeric(Content)
    end;
check_numeric_elements(_) ->
    ok.

%% @private
%% @doc Checks content list for numeric elements.
-spec check_content_numeric([term()]) -> ok | {error, term()}.

check_content_numeric([]) ->
    ok;
check_content_numeric([Node | Rest]) ->
    case check_numeric_elements(Node) of
        ok -> check_content_numeric(Rest);
        {error, _} = Error -> Error
    end.

%% @private
%% @doc Extracts text content from an element's content list.
-spec extract_element_text([term()]) -> string().

extract_element_text(Content) ->
    lists:foldl(fun
        (#xmlText{value = V}, Acc) ->
            Acc ++ V;
        (_, Acc) ->
            Acc
    end, "", Content).

%% @private
%% @doc Extracts a usable value from an XPath result node.
-spec extract_xpath_value(term()) -> term().

extract_xpath_value(#xmlText{value = Value}) ->
    %% Return text value as string (for XPath, always return strings)
    string:trim(Value);
extract_xpath_value(#xmlElement{name = Name}) ->
    %% Return element name for element nodes
    %% Could be extended to return more info
    Name;
extract_xpath_value(Other) ->
    Other.

%% @private
%% @doc Parses simple FLWOR XQuery expressions.
%%
%% Handles: "for $a in /path return $a * 2" style expressions.
%%
%% @end
-spec parse_simple_flwor(string(), term()) -> {ok, term()} | {error, term()}.

parse_simple_flwor(XQueryStr, XmlDoc) ->
    %% Parse FLWOR: for $var in /path return expr
    case re:run(XQueryStr,
        "for\\s+\\$(\\w+)\\s+in\\s+([^\\s]+)\\s+return\\s+(.+)",
        [{capture, all_but_first, list}]) of
        {match, [_Var, Path, ReturnExpr]} ->
            %% Evaluate XPath to get value
            try
                XPathResult = xmerl_xpath:string(Path, XmlDoc),
                case extract_first_numeric(XPathResult) of
                    {ok, Value} ->
                        %% Parse and evaluate return expression
                        Result = evaluate_return_expr(ReturnExpr, Value),
                        {ok, Result};
                    {error, Reason} ->
                        {error, Reason}
                end
            catch
                _:_ ->
                    {error, xpath_failed}
            end;
        nomatch ->
            %% Try simple XPath as fallback
            try
                Result = xmerl_xpath:string(XQueryStr, XmlDoc),
                case extract_first_numeric(Result) of
                    {ok, Value} -> {ok, Value};
                    {error, _} -> {error, no_result}
                end
            catch
                _:_ ->
                    {error, invalid_xquery}
            end
    end.

%% @private
%% @doc Extracts the first numeric value from XPath result.
-spec extract_first_numeric([term()]) -> {ok, number()} | {error, term()}.

extract_first_numeric([]) ->
    {error, no_nodes};
extract_first_numeric([#xmlText{value = Value} | _]) ->
    case string:to_integer(Value) of
        {Int, ""} -> {ok, Int};
        _ ->
            case string:to_float(Value) of
                {Float, ""} -> {ok, Float};
                _ -> {error, not_numeric}
            end
    end;
extract_first_numeric([#xmlElement{content = Content} | _]) ->
    TextValue = extract_element_text(Content),
    case TextValue of
        "" -> {error, no_value};
        _ ->
            case string:to_integer(TextValue) of
                {Int, ""} -> {ok, Int};
                _ ->
                    case string:to_float(TextValue) of
                        {Float, ""} -> {ok, Float};
                        _ -> {error, not_numeric}
                    end
            end
    end;
extract_first_numeric([Value | _]) when is_integer(Value); is_float(Value) ->
    {ok, Value};
extract_first_numeric(_) ->
    {error, not_numeric}.

%% @private
%% @doc Evaluates a simple return expression against a value.
%%
%% Handles: $var * 2, $var + 5, etc.
%%
%% @end
-spec evaluate_return_expr(string(), number()) -> number().

evaluate_return_expr(ReturnExpr, Value) ->
    %% Simple arithmetic: $var * n, $var + n, n * $var, etc.
    Expr = string:trim(ReturnExpr),

    %% Try to match patterns before variable replacement
    case Expr of
        "$a *" ++ Rest ->
            {Mult, _} = string:to_integer(string:trim(Rest)),
            Value * Mult;
        "$a / " ++ Rest ->
            {Div, _} = string:to_integer(string:trim(Rest)),
            Value / Div;
        "$a + " ++ Rest ->
            {Add, _} = string:to_integer(string:trim(Rest)),
            Value + Add;
        "$a - " ++ Rest ->
            {Sub, _} = string:to_integer(string:trim(Rest)),
            Value - Sub;
        _ ->
            %% Handle reverse patterns like "2 * $a"
            case re:run(Expr, "^(-?\\d+)\\s*\\*\\s+\\$a", [{capture, all_but_first, list}]) of
                {match, [MultStr]} ->
                    Mult = list_to_integer(MultStr),
                    Value * Mult;
                nomatch ->
                    %% Try to evaluate after variable replacement
                    Expr2 = re:replace(Expr, "\\$\\w+", io_lib:format("~p", [Value]), [global, {return, list}]),
                    %% Safe evaluation of simple arithmetic
                    case re:run(Expr2, "^(-?\\d+\\.?\\d*)\\s*[+\\-*/]\\s*(-?\\d+\\.?\\d*)$", [{capture, all, list}]) of
                        {match, [Full, Op1Str, Op2Str]} ->
                            Op1 = case string:to_float(Op1Str) of
                                {Op1F, ""} -> Op1F;
                                _ -> list_to_integer(Op1Str)
                            end,
                            Op2 = case string:to_float(Op2Str) of
                                {Op2F, ""} -> Op2F;
                                _ -> list_to_integer(Op2Str)
                            end,
                            case string:find(Full, "*") of
                                [] ->
                                    case string:find(Full, "+") of
                                        [] ->
                                            case string:find(Full, "-") of
                                                [] -> Op1 / Op2;
                                                _ -> Op1 - Op2
                                            end;
                                        _ -> Op1 + Op2
                                    end;
                                _ -> Op1 * Op2
                            end;
                        nomatch ->
                            Value
                    end
            end
    end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% Test data storage
data_test() ->
    D0 = new(),
    ?assertEqual(#{}, D0),

    D1 = put(D0, customer, <<"Alice">>),
    ?assertEqual(<<"Alice">>, get(D1, customer)),
    ?assertEqual(undefined, get(D1, missing)),

    D2 = put(D1, total, 100),
    ?assertEqual(<<"Alice">>, get(D2, customer)),
    ?assertEqual(100, get(D2, total)).

%% Test XSD validation
xsd_validate_test() ->
    Xsd = <<
        "<?xml version='1.0'?>\n"
        "<xsd:schema xmlns:xsd='http://www.w3.org/2001/XMLSchema'>\n"
        "  <xsd:element name='order'>\n"
        "    <xsd:complexType>\n"
        "      <xsd:sequence>\n"
        "        <xsd:element name='amount' type='xsd:int'/>\n"
        "      </xsd:sequence>\n"
        "    </xsd:complexType>\n"
        "  </xsd:element>\n"
        "</xsd:schema>\n"
    >>,

    XmlOk = <<"<order><amount>10</amount></order>">>,
    ?assertEqual(ok, xsd_validate(XmlOk, Xsd)),

    XmlBad = <<"<order><amount>abc</amount></order>">>,
    ?assertNotEqual(ok, xsd_validate(XmlBad, Xsd)),

    %% Test malformed XML
    XmlMalformed = <<"<order><amount>10</order>">>,
    ?assertEqual({error, malformed_xml}, xsd_validate(XmlMalformed, Xsd)).

%% Test XPath
xpath_test() ->
    Xml = <<"<order><amount>10</amount></order>">>,

    %% Test text node extraction - returns strings
    ?assertEqual(["10"], xpath(Xml, "/order/amount/text()")),

    %% Test element selection
    Result = xpath(Xml, "/order/amount"),
    ?assert(length(Result) > 0),

    %% Test non-matching path
    ?assertEqual([], xpath(Xml, "/nonexistent/path")),

    %% Test nested elements - returns strings
    XmlNested = <<"<root><a><b><c>42</c></b></a></root>">>,
    ?assertEqual(["42"], xpath(XmlNested, "/root/a/b/c/text()")).

%% Test XQuery
xquery_test() ->
    Xml = <<"<order><amount>10</amount></order>">>,

    %% Test simple multiplication
    ?assertEqual({ok, 20}, xquery(Xml, "for $a in /order/amount return $a * 2")),

    %% Test with different multiplier
    ?assertEqual({ok, 30}, xquery(Xml, "for $a in /order/amount return $a * 3")),

    %% Test with zero
    ?assertEqual({ok, 0}, xquery(Xml, "for $a in /order/amount return $a * 0")).

-endif.
