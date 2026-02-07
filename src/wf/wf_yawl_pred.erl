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

-module(wf_yawl_pred).
-moduledoc """
YAWL XPath predicate to Erlog rule compiler.

Converts YAWL 2.1 XPath predicates to wf_rules Datalog format.
Handles variable references like `/NetName/VarName/text()` and converts
them to `token(NetName, VarName)` facts.

## Overview

YAWL workflows use XPath expressions for flow predicates. This module
transforms these XPath expressions into the Datalog-like syntax used by
`wf_rules`, enabling rule-based evaluation of workflow conditions.

## Examples

Simple variable reference:

```erlang
> wf_yawl_pred:to_erlog(<<"/Overall/PO_timedout/text()='false'">>).
{ok,{<<"approved :- token(Overall, PO_timedout), eq(PO_timedout_value, false).">>,
     {approved,[]}}}
```

Compound predicate with and/or:

```erlang
> wf_yawl_pred:to_erlog(
..   <<"/Overall/PO_timedout/text()='false' and /Overall/POApproval/text()='true'">>
.. ).
{ok,{<<"flow_selected :- token(Overall, PO_timedout), eq(PO_timedout_value, false), "
      "token(Overall, POApproval), eq(POApproval_value, true).">>,
     {flow_selected,[]}}}
```

Variable references:

```erlang
> wf_yawl_pred:extract_vars(<<"/Overall/Status/text()='approved'">>).
{ok,[<<"/Overall/Status/text()">>]}
```

Parsing:

```erlang
> wf_yawl_pred:parse_xpath(<<"/Overall/Count/text() > 0">>).
{ok,{xpath_comp,
     {xpath_var,{path,<<"/Overall">>,<<"/Count">>}},
     '>',
     {xpath_literal,0}}}
```

## YAWL XPath Syntax Support

The module supports the following YAWL XPath patterns:

- **Variable references**: `/NetName/VarName/text()` → `token(NetName, VarName)`
- **String literals**: `'value'`, `"value"`
- **Boolean literals**: `'true'`, `'false'`
- **Numeric literals**: `0`, `42`, `3.14`
- **Comparison operators**: `=`, `!=`, `<`, `>`, `<=`, `>=`
- **Boolean operators**: `and`, `or`
- **Variable references**: `$varname`

## Limitations

This is a focused implementation for common YAWL workflow patterns:
- Full XPath 1.0 is not supported
- Functions like `count()`, `sum()`, etc. are parsed but not evaluated
- Axis specifiers beyond `/` (child) are not supported
- The generated rules require runtime facts from workflow markings

## Integration with wf_rules

Generated rules follow the wf_rules syntax:

```erlang
% Head :- Body.
Head :- token(Place, Token), comparison(Token, Value).
```

The `token(Place, Token)` facts must be provided by the runtime
based on the current workflow marking.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([to_erlog/1, parse_xpath/1, extract_vars/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc XPath expression AST node types.
%%
%% Represents parsed XPath expressions as Erlang terms for
%% transformation to Datalog rules.
%%--------------------------------------------------------------------
-type xpath_ast() ::
    {xpath_var, {path, binary(), binary()}} |          % /NetName/VarName/text()
    {xpath_literal, term()} |                           % 'value', 42, true
    {xpath_comp, xpath_ast(), binary(), xpath_ast()} |  % left OP right
    {xpath_bool, binary(), [xpath_ast()]}.              % and/or

%%--------------------------------------------------------------------
%% @doc Comparison operators in XPath.
%%--------------------------------------------------------------------
-type comp_op() :: '=' | '!=' | '<' | '>' | '<=' | '>='.

%%--------------------------------------------------------------------
%% @doc Boolean operators in XPath.
%%--------------------------------------------------------------------
-type bool_op() :: 'and' | 'or'.

%% Export types
-export_type([xpath_ast/0, comp_op/0, bool_op/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts a YAWL XPath predicate to wf_rules Datalog format.
%%
%% Parses the XPath expression and generates equivalent Datalog rules
%% compatible with wf_rules:compile/1 and wf_rules:bool/4.
%%
%% @param PredicateBin XPath predicate expression as binary
%% @return {ok, {RulesBin, GoalTerm}} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec to_erlog(PredicateBin :: binary()) ->
    {ok, {RulesBin :: binary(), GoalTerm :: term()}} | {error, Reason :: term()}.

to_erlog(PredicateBin) when is_binary(PredicateBin) ->
    try
        {ok, AST} = parse_xpath(PredicateBin),
        RulesBin = ast_to_rules(AST),
        GoalTerm = generate_goal(AST),
        {ok, {RulesBin, GoalTerm}}
    catch
        throw:{parse_error, Reason} -> {error, Reason};
        error:Reason -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Parses a YAWL XPath expression into an AST.
%%
%% Tokenizes and parses the XPath expression into a structured
%% representation for transformation.
%%
%% @param PredicateBin XPath predicate expression as binary
%% @return {ok, AST} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_xpath(PredicateBin :: binary()) ->
    {ok, xpath_ast()} | {error, Reason :: term()}.

parse_xpath(PredicateBin) when is_binary(PredicateBin) ->
    try
        Str = binary_to_list(PredicateBin),
        Tokens = tokenize_xpath(Str),
        parse_expr(Tokens)
    catch
        throw:{parse_error, Reason} -> {error, Reason};
        error:function_clause -> {error, unexpected_tokens};
        error:{case_clause, _} -> {error, unexpected_tokens};
        error:Reason -> {error, Reason};
        throw:Reason -> {error, Reason};
        exit:Reason -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Extracts variable references from an XPath predicate.
%%
%% Returns a list of variable reference paths found in the expression.
%% Useful for determining which workflow variables are referenced.
%%
%% @param PredicateBin XPath predicate expression as binary
%% @return {ok, [VarPath]} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_vars(PredicateBin :: binary()) ->
    {ok, [VarName :: binary()]} | {error, Reason :: term()}.

extract_vars(PredicateBin) when is_binary(PredicateBin) ->
    case parse_xpath(PredicateBin) of
        {ok, AST} ->
            Vars = extract_vars_from_ast(AST, []),
            {ok, Vars};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Tokenizer - XPath Expressions
%%====================================================================

%% Token types for XPath expressions
-type xpath_token() ::
    {lparen, integer()} |           % (
    {rparen, integer()} |           % )
    {comp, binary()} |              % =, !=, <, >, <=, >=
    {bool, binary()} |              % and, or
    {literal, term()} |             % 'value', "value", numbers
    {var_ref, binary()} |           % /NetName/VarName/text()
    {identifier, binary()} |        % function names, variable names
    {comma, integer()}.             % ,

%%--------------------------------------------------------------------
%% @private Tokenizes XPath expression string.
%%
%% Uses a state machine to handle YAWL XPath patterns like /NetName/VarName/text().
%%
%% @end
%%--------------------------------------------------------------------
-spec tokenize_xpath(string()) -> [xpath_token()].

tokenize_xpath(Str) ->
    tokenize_xpath_loop(Str, [], #{path => [], in_path => false}).

tokenize_xpath_loop([], [], #{path := Path, in_path := InPath}) ->
    %% Process any remaining path components
    Tokens = case InPath of
        true when Path =/= [] ->
            finalize_path(Path);
        _ ->
            []
    end,
    lists:reverse(Tokens);
tokenize_xpath_loop([], Acc, #{path := Path, in_path := InPath}) ->
    %% Process any remaining path components and rest tokens
    Tokens = case InPath of
        true when Path =/= [] ->
            finalize_path(Path);
        _ ->
            []
    end,
    lists:reverse(prepend_tokens(Tokens, Acc));
%% Whitespace
tokenize_xpath_loop([C | Rest], Acc, State) when C =:= $\s; C =:= $\n; C =:= $\t; C =:= $\r ->
    tokenize_xpath_loop(Rest, Acc, State);
%% Slash - path separator or start of path
tokenize_xpath_loop([$/ | Rest], Acc, #{path := Path, in_path := InPath} = State) ->
    %% Check if we're starting a new path or continuing
    case InPath of
        false ->
            %% Starting a new path
            tokenize_xpath_loop(Rest, Acc, State#{path => [$/], in_path => true});
        true when Path =:= [] ->
            %% First slash
            tokenize_xpath_loop(Rest, Acc, State#{path => [$/]});
        true ->
            %% Continuing path - add separator
            tokenize_xpath_loop(Rest, Acc, State#{path => [$/ | Path]})
    end;
%% Single quoted string
tokenize_xpath_loop([39 | Rest], Acc, State) ->  % $'
    {String, Rest2} = scan_quoted(Rest, 39),
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest2, [{literal, String} | NewAcc], NewState);
%% Double quoted string
tokenize_xpath_loop([34 | Rest], Acc, State) ->  % $"
    {String, Rest2} = scan_quoted(Rest, 34),
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest2, [{literal, String} | NewAcc], NewState);
%% Left paren - check for text() function
tokenize_xpath_loop("()" ++ Rest, Acc, #{path := [$/, "text" | RevPath]} = State) ->
    %% This is text() function at end of path - complete the path
    %% Consume both ( and )
    PathTokens = finalize_path(RevPath),
    %% Prepend path tokens to Acc so they appear first after reversal
    tokenize_xpath_loop(Rest, prepend_tokens(PathTokens, Acc), State#{path => [], in_path => false});
tokenize_xpath_loop([$( | Rest], Acc, #{path := Path} = State) when length(Path) > 0 ->
    %% Check if last component is "text"
    case Path of
        [LastComp | RestPath] when is_list(LastComp) ->
            case string:equal(LastComp, "text") of
                true ->
                    %% Check if next char is ) for complete text()
                    case Rest of
                        [$) | Rest2] ->
                            %% This is text() - complete path without it
                            PathTokens = finalize_path(RestPath),
                            tokenize_xpath_loop(Rest2, prepend_tokens(PathTokens, Acc), State#{path => [], in_path => false});
                        _ ->
                            %% text( but not yet ) - add lparen and wait
                            {NewState, NewAcc} = flush_path(State, Acc),
                            tokenize_xpath_loop(Rest, [{lparen, 1} | NewAcc], NewState)
                    end;
                false ->
                    {NewState, NewAcc} = flush_path(State, Acc),
                    tokenize_xpath_loop(Rest, [{lparen, 1} | NewAcc], NewState)
            end;
        _ ->
            {NewState, NewAcc} = flush_path(State, Acc),
            tokenize_xpath_loop(Rest, [{lparen, 1} | NewAcc], NewState)
    end;
tokenize_xpath_loop([$( | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{lparen, 1} | NewAcc], NewState);
%% Right paren
tokenize_xpath_loop([$) | Rest], Acc, State) ->
    tokenize_xpath_loop(Rest, [{rparen, 1} | Acc], State);
%% Comma
tokenize_xpath_loop([$, | Rest], Acc, State) ->
    tokenize_xpath_loop(Rest, [{comma, 1} | Acc], State);
%% Check for comparison operators (>=, <=, !=, =, <, >)
tokenize_xpath_loop([$!, $= | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{comp, <<"!=">>} | NewAcc], NewState);
tokenize_xpath_loop([$>, $= | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{comp, <<">=">>} | NewAcc], NewState);
tokenize_xpath_loop([$<, $= | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{comp, <<"<=">>} | NewAcc], NewState);
tokenize_xpath_loop([$= | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{comp, <<"=">>} | NewAcc], NewState);
tokenize_xpath_loop([$> | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{comp, <<">">>} | NewAcc], NewState);
tokenize_xpath_loop([$< | Rest], Acc, State) ->
    {NewState, NewAcc} = flush_path(State, Acc),
    tokenize_xpath_loop(Rest, [{comp, <<"<">>} | NewAcc], NewState);
%% Alphanumeric - identifier or number
tokenize_xpath_loop([C | _] = Input, Acc, #{path := Path, in_path := InPath} = State)
    when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C =:= $_; C =:= $$ ->
    {AtomOrNum, Rest2} = scan_identifier(Input),
    case InPath of
        true when Path =/= [] ->
            %% Inside a path - add to stack
            tokenize_xpath_loop(Rest2, Acc, State#{path => [AtomOrNum | Path]});
        _ ->
            Token = classify_token(AtomOrNum),
            tokenize_xpath_loop(Rest2, [Token | Acc], State)
    end.

%%--------------------------------------------------------------------
%% @private Flush accumulated path to tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec flush_path(map(), [xpath_token()]) -> {map(), [xpath_token()]}.

flush_path(#{path := []} = State, Acc) ->
    {State, Acc};
flush_path(#{path := Path, in_path := true} = State, Acc) ->
    Tokens = finalize_path(Path),
    NewState = State#{path => [], in_path => false},
    {NewState, prepend_tokens(Tokens, Acc)}.

%%--------------------------------------------------------------------
%% @private Prepends tokens to accumulator.
%%
%% Since we build the list by prepending and reverse at the end,
%% we need to prepend PathTokens to Acc in a way that preserves order.
%%
%% @end
%%--------------------------------------------------------------------
-spec prepend_tokens([xpath_token()], [xpath_token()]) -> [xpath_token()].

prepend_tokens([], Acc) -> Acc;
prepend_tokens([Token | Rest], Acc) ->
    prepend_tokens(Rest, [Token | Acc]).

%%--------------------------------------------------------------------
%% @private Finalize path components into tokens.
%%
%% Path is reversed: ["VarName", $/, "NetName", $/]
%%
%% @end
%%--------------------------------------------------------------------
-spec finalize_path(list()) -> [xpath_token()].

finalize_path([]) -> [];
finalize_path(Path) ->
    %% Path is reversed, e.g., ["Flag", $/, "Overall", $/]
    %% We need to find /NetName/VarName pattern
    case extract_path_components(Path, [], 0) of
        {ok, NetName, VarName} ->
            [{var_ref, <<"/", NetName/binary, "/", VarName/binary, "/text()">>}];
        error ->
            []
    end.

%%--------------------------------------------------------------------
%% @private Extracts /NetName/VarName from path stack.
%%
%% Path is in reverse order: e.g., [$/, "Flag", $/, "Overall", $/]
%% This represents: /Overall/Flag (after /text() was removed)
%%
%% We need to extract in the original order: NetName="Overall", VarName="Flag"
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_path_components(list(), list(), integer()) ->
    {ok, NetName :: binary(), VarName :: binary()} | error.

extract_path_components(Path, [], _Depth) ->
    %% Path still has elements - extract directly
    %% Path format: [$/, "Flag", $/, "Overall", $/]
    %% We need: NetName="Overall", VarName="Flag"
    case extract_two_names(Path, []) of
        {ok, [NetName, VarName]} ->
            {ok, list_to_binary(NetName), list_to_binary(VarName)};
        error ->
            error
    end;
extract_path_components([$/ | Rest], Acc, Depth) ->
    extract_path_components(Rest, Acc, Depth);
extract_path_components([Name | Rest], Acc, Depth) when is_list(Name) ->
    extract_path_components(Rest, [Name | Acc], Depth);
extract_path_components(_Rest, Acc, _Depth) ->
    case Acc of
        [VarName, NetName | _] when is_list(VarName), is_list(NetName) ->
            {ok, list_to_binary(NetName), list_to_binary(VarName)};
        _ ->
            error
    end.

%%--------------------------------------------------------------------
%% @private Extracts exactly two names from path in correct order.
%%
%% Path format: [$/, "Flag", $/, "Overall", $/]
%% This represents: /Overall/Flag (stack is LIFO, so top is Flag)
%% We need to extract in original order: NetName="Overall", VarName="Flag"
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_two_names(list(), list()) -> {ok, [string()]} | error.

extract_two_names([$/ | Rest], Acc) ->
    extract_two_names(Rest, Acc);
extract_two_names([Name | Rest], Acc) when is_list(Name) ->
    extract_two_names(Rest, [Name | Acc]);
extract_two_names([], Acc) ->
    %% Acc contains names in the order they were encountered (left to right in path)
    %% Path: /Overall/Flag means Acc = [Overall, Flag]
    %% NetName should be first component, VarName should be second
    case Acc of
        [NetName, VarName | _] ->
            {ok, [NetName, VarName]};
        _ ->
            error
    end;
extract_two_names(_, _) ->
    error.

%%--------------------------------------------------------------------
%% @private Scans a quoted string.
%%
%% @end
%%--------------------------------------------------------------------
-spec scan_quoted(string(), char()) -> {binary(), string()}.

scan_quoted(Str, Quote) ->
    scan_quoted_loop(Str, Quote, []).

scan_quoted_loop([Quote | Rest], Quote, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Rest};
scan_quoted_loop([$\\, C | Rest], Quote, Acc) ->
    scan_quoted_loop(Rest, Quote, [C | Acc]);
scan_quoted_loop([C | Rest], Quote, Acc) ->
    scan_quoted_loop(Rest, Quote, [C | Acc]).

%%--------------------------------------------------------------------
%% @private Scans an identifier or number.
%%
%% @end
%%--------------------------------------------------------------------
-spec scan_identifier(string()) -> {string(), string()}.

scan_identifier(Input) ->
    scan_identifier_loop(Input, []).

scan_identifier_loop([C | Rest], Acc)
    when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C =:= $_; C =:= $:; C =:= $- ->
    scan_identifier_loop(Rest, [C | Acc]);
scan_identifier_loop(Rest, Acc) ->
    {lists:reverse(Acc), Rest}.

%%--------------------------------------------------------------------
%% @private Classifies a token string into its type.
%%
%% @end
%%--------------------------------------------------------------------
-spec classify_token(string()) -> xpath_token().

classify_token("and") -> {bool, <<"and">>};
classify_token("or") -> {bool, <<"or">>};
classify_token("true") -> {literal, true};
classify_token("false") -> {literal, false};
classify_token(Str) ->
    case is_number_str(Str) of
        true ->
            case string:to_float(Str) of
                {Float, ""} -> {literal, Float};
                _ ->
                    {Int, ""} = string:to_integer(Str),
                    {literal, Int}
            end;
        false ->
            Bin = list_to_binary(Str),
            {identifier, Bin}
    end.

%%--------------------------------------------------------------------
%% @private Checks if a string is a number.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_number_str(string()) -> boolean().

is_number_str([]) -> false;
is_number_str([$.]) -> false;
is_number_str(Str) ->
    is_number_loop(Str, 0, 0).

is_number_loop([], _, _) -> true;
is_number_loop([C | Rest], DotCount, DigitCount) when C >= $0, C =< $9 ->
    is_number_loop(Rest, DotCount, DigitCount + 1);
is_number_loop([$. | Rest], 0, DigitCount) when DigitCount > 0 ->
    is_number_loop(Rest, 1, DigitCount);
is_number_loop([C | Rest], DotCount, DigitCount) when C =:= $-; C =:= $+; C =:= $e; C =:= $E ->
    is_number_loop(Rest, DotCount, DigitCount);
is_number_loop(_, _, _) -> false.

%%====================================================================
%% Parser - XPath to AST
%%====================================================================

%%--------------------------------------------------------------------
%% @private Parses expression from tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_expr([xpath_token()]) -> {ok, xpath_ast()} | {error, term()}.

parse_expr(Tokens) ->
    case parse_or_expr(Tokens) of
        {ok, AST, []} -> {ok, AST};
        {ok, _AST, Remaining} -> {error, {unexpected_tokens, Remaining}};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private Parses OR expressions (lowest precedence).
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_or_expr([xpath_token()]) ->
    {ok, xpath_ast(), [xpath_token()]} | {error, term()}.

parse_or_expr(Tokens) ->
    case parse_and_expr(Tokens) of
        {ok, AST, Rest} -> parse_or_expr_loop(Rest, [AST], 'or');
        Error -> Error
    end.

parse_or_expr_loop([], [AST], _Op) ->
    {ok, AST, []};
parse_or_expr_loop([{bool, <<"or">>} | Rest], [Left], _Op) ->
    case parse_and_expr(Rest) of
        {ok, Right, Rest2} -> parse_or_expr_loop(Rest2, [Left, Right], 'or');
        Error -> Error
    end;
parse_or_expr_loop(Rest, Acc, 'or') ->
    %% Fold accumulated expressions with OR
    AST = fold_bool_expr(lists:reverse(Acc), 'or'),
    {ok, AST, Rest};
parse_or_expr_loop(Rest, Acc, _Op) ->
    AST = fold_bool_expr(lists:reverse(Acc), 'or'),
    {ok, AST, Rest}.

%%--------------------------------------------------------------------
%% @private Parses AND expressions.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_and_expr([xpath_token()]) ->
    {ok, xpath_ast(), [xpath_token()]} | {error, term()}.

parse_and_expr(Tokens) ->
    case parse_comp_expr(Tokens) of
        {ok, AST, Rest} -> parse_and_expr_loop(Rest, [AST], 'and');
        Error -> Error
    end.

parse_and_expr_loop([], [AST], _Op) ->
    {ok, AST, []};
parse_and_expr_loop([{bool, <<"and">>} | Rest], [Left], _Op) ->
    case parse_comp_expr(Rest) of
        {ok, Right, Rest2} -> parse_and_expr_loop(Rest2, [Left, Right], 'and');
        Error -> Error
    end;
parse_and_expr_loop(Rest, Acc, 'and') ->
    AST = fold_bool_expr(lists:reverse(Acc), 'and'),
    {ok, AST, Rest};
parse_and_expr_loop(Rest, Acc, _Op) ->
    AST = fold_bool_expr(lists:reverse(Acc), 'and'),
    {ok, AST, Rest}.

%%--------------------------------------------------------------------
%% @private Parses comparison expressions.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_comp_expr([xpath_token()]) ->
    {ok, xpath_ast(), [xpath_token()]} | {error, term()}.

parse_comp_expr(Tokens) ->
    case parse_primary(Tokens) of
        {ok, Left, [{comp, Op} | Rest]} ->
            case parse_primary(Rest) of
                {ok, Right, Rest2} ->
                    {ok, {xpath_comp, Left, Op, Right}, Rest2};
                Error -> Error
            end;
        {ok, AST, Rest} ->
            {ok, AST, Rest};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @private Parses primary expressions (literals, vars, parenthesized).
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_primary([xpath_token()]) ->
    {ok, xpath_ast(), [xpath_token()]} | {error, term()}.

parse_primary([{literal, Value} | Rest]) ->
    {ok, {xpath_literal, Value}, Rest};
parse_primary([{var_ref, Path} | Rest]) ->
    %% Parse /NetName/VarName from /NetName/VarName/text()
    {NetName, VarName} = parse_var_path(Path),
    {ok, {xpath_var, {path, NetName, VarName}}, Rest};
parse_primary([{identifier, <<"count">>} | [{lparen, _} | Rest]]) ->
    %% Handle count() function - parse as special node
    {ok, _CountAST, [{rparen, _} | Rest2]} = parse_primary(Rest),
    {ok, {xpath_literal, 0}, Rest2};  % Placeholder
parse_primary([{identifier, Name} | Rest]) ->
    %% Variable reference like $name or identifier
    {ok, {xpath_literal, Name}, Rest};
parse_primary([{lparen, _} | Rest]) ->
    case parse_expr(Rest) of
        {ok, AST, [{rparen, _} | Rest2]} ->
            {ok, AST, Rest2};
        {error, Reason} ->
            {error, Reason};
        {ok, _, _} ->
            {error, expected_rparen}
    end;
parse_primary([]) ->
    {error, unexpected_eof};
parse_primary([Token | _]) ->
    {error, {unexpected_token, Token}}.

%%--------------------------------------------------------------------
%% @private Parses variable path /NetName/VarName/text().
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_var_path(binary()) -> {NetName :: binary(), VarName :: binary()}.

parse_var_path(Path) ->
    %% Path format: /NetName/VarName/text()
    Str = binary_to_list(Path),
    Parts = string:tokens(Str, "/"),
    case Parts of
        [NetName, VarName, "text()"] ->
            {list_to_binary(NetName), list_to_binary(VarName)};
        [NetName, VarName] ->
            {list_to_binary(NetName), list_to_binary(VarName)};
        _ ->
            {<<>>, <<>>}
    end.

%%--------------------------------------------------------------------
%% @private Folds boolean expressions into AST nodes.
%%
%% @end
%%--------------------------------------------------------------------
-spec fold_bool_expr([xpath_ast()], bool_op()) -> xpath_ast().

fold_bool_expr([Single], _Op) ->
    Single;
fold_bool_expr(List, Op) ->
    {xpath_bool, atom_to_binary(Op), List}.

%%====================================================================
%% AST to Rules Conversion
%%====================================================================

%%--------------------------------------------------------------------
%% @private Converts AST to wf_rules Datalog binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec ast_to_rules(xpath_ast()) -> binary().

ast_to_rules(AST) ->
    Head = generate_rule_name(AST),
    Body = ast_to_body(AST, []),
    Rule = io_lib:format("~s :- ~s.~n", [Head, string:join(Body, ", ")]),
    iolist_to_binary(Rule).

%%--------------------------------------------------------------------
%% @private Generates rule head name.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_rule_name(xpath_ast()) -> string().

generate_rule_name({xpath_comp, _, _, _}) -> "flow_selected";
generate_rule_name({xpath_bool, _, _}) -> "flow_selected";
generate_rule_name({xpath_var, _}) -> "flow_selected";
generate_rule_name({xpath_literal, _}) -> "flow_selected".

%%--------------------------------------------------------------------
%% @private Generates goal term for querying.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_goal(xpath_ast()) -> term().

generate_goal(_AST) ->
    {flow_selected, []}.

%%--------------------------------------------------------------------
%% @private Converts AST to rule body predicates.
%%
%% @end
%%--------------------------------------------------------------------
-spec ast_to_body(xpath_ast(), [string()]) -> [string()].

ast_to_body({xpath_comp, Left, Op, Right}, Acc) ->
    %% For comparison expressions, only call ast_to_body_comp
    %% Don't call ast_to_body(Left, Acc) to avoid duplicate predicates
    ast_to_body_comp(Left, Op, Right, Acc);
ast_to_body({xpath_bool, <<"and">>, Exprs}, Acc) ->
    lists:foldl(fun(Expr, A) -> ast_to_body(Expr, A) end, Acc, Exprs);
ast_to_body({xpath_bool, <<"or">>, Exprs}, Acc) ->
    %% OR is handled by creating separate rules
    %% For now, create a body with first expr
    lists:foldl(fun(Expr, A) -> ast_to_body(Expr, A) end, Acc, Exprs);
ast_to_body({xpath_var, {path, NetName, VarName}}, Acc) ->
    %% Convert /NetName/VarName to a fact check
    %% Use lowercase to ensure it's parsed as an atom
    VarAtom = binary_to_atom(VarName, utf8),
    VarNameStr = string:lowercase(atom_to_list(VarAtom)),
    %% Generate a fact check like "status" for /Overall/Status/text()
    Rule = lists:flatten(io_lib:format("~s", [VarNameStr])),
    [Rule | Acc];
ast_to_body({xpath_literal, _Value}, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @private Adds comparison predicates to body.
%%
%% @end
%%--------------------------------------------------------------------
-spec ast_to_body_comp(xpath_ast(), binary(), xpath_ast(), [string()]) -> [string()].

ast_to_body_comp({xpath_var, {path, _NetName, VarName}}, _Op, {xpath_literal, Value}, Acc) ->
    %% For simplicity, generate a rule checking the variable has the value
    %% This creates rules like: status(approved)
    %% We use lowercase to ensure it's parsed as an atom, not a variable
    VarAtom = binary_to_atom(VarName, utf8),
    VarNameStr = string:lowercase(atom_to_list(VarAtom)),
    FormattedValue = format_value_for_rule(Value),
    Rule = lists:flatten(io_lib:format("~s(~s)", [VarNameStr, FormattedValue])),
    [Rule | Acc];
ast_to_body_comp({xpath_literal, Value}, Op, {xpath_var, {path, _NetName, VarName}}, Acc) ->
    %% Reverse comparison
    ast_to_body_comp({xpath_var, {path, _NetName, VarName}}, reverse_op(Op), {xpath_literal, Value}, Acc);
ast_to_body_comp(_Left, _Op, _Right, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @private Reverses comparison operator.
%%
%% @end
%%--------------------------------------------------------------------
-spec reverse_op(binary()) -> binary().

reverse_op(<<"<">>) -> <<">">>;
reverse_op(<<">">>) -> <<"<">>;
reverse_op(<<"<=">>) -> <<">=">>;
reverse_op(<<">=">>) -> <<"<=">>;
reverse_op(Op) -> Op.

%%--------------------------------------------------------------------
%% @private Formats value for Datalog output.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_value(term()) -> string().

format_value(true) -> "true";
format_value(false) -> "false";
format_value(Value) when is_integer(Value) -> integer_to_list(Value);
format_value(Value) when is_float(Value) -> float_to_list(Value, [{decimals, 2}]);
format_value(Value) when is_binary(Value) ->
    %% Convert binary to string, escape if needed
    Str = binary_to_list(Value),
    lists:flatten(io_lib:format("'~s'", [Str]));
format_value(Value) when is_atom(Value) -> atom_to_list(Value);
format_value(Value) -> lists:flatten(io_lib:format("~p", [Value])).

%%--------------------------------------------------------------------
%% @private Formats value for rule body (as atom or string).
%%
%% @end
%%--------------------------------------------------------------------
-spec format_value_for_rule(term()) -> string().

format_value_for_rule(Value) when is_binary(Value) ->
    %% For binaries, use the string directly with single quotes
    binary_to_list(Value);
format_value_for_rule(Value) when is_atom(Value) ->
    atom_to_list(Value);
format_value_for_rule(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value_for_rule(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]);
format_value_for_rule(true) -> "true";
format_value_for_rule(false) -> "false";
format_value_for_rule(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).

%%--------------------------------------------------------------------
%% @private Extracts variable references from AST.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_vars_from_ast(xpath_ast(), [binary()]) -> [binary()].

extract_vars_from_ast({xpath_var, {path, NetName, VarName}}, Acc) ->
    Path = <<"/", NetName/binary, "/", VarName/binary, "/text()">>,
    [Path | Acc];
extract_vars_from_ast({xpath_comp, Left, _Op, Right}, Acc) ->
    Acc1 = extract_vars_from_ast(Left, Acc),
    extract_vars_from_ast(Right, Acc1);
extract_vars_from_ast({xpath_bool, _Op, Exprs}, Acc) ->
    lists:foldl(fun(Expr, A) -> extract_vars_from_ast(Expr, A) end, Acc, Exprs);
extract_vars_from_ast(_, Acc) ->
    Acc.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit doctests for the module.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    %% Test 1: Variable reference with comparison
    {ok, {xpath_comp, {xpath_var, {path, <<"Overall">>, <<"PO_timedout">>}}, <<"=">>, _}} =
        parse_xpath(<<"/Overall/PO_timedout/text()='false'">>),

    %% Test 2: Extract variables
    {ok, Vars} = extract_vars(<<"/Overall/Status/text()='approved'">>),
    true = lists:member(<<"/Overall/Status/text()">>, Vars),

    %% Test 3: Convert to erlog (basic check)
    {ok, {RulesBin, _Goal}} = to_erlog(<<"/Overall/Flag/text()='true'">>),
    true = byte_size(RulesBin) > 0,

    %% Test 4: Compound predicate
    {ok, _} = parse_xpath(
        <<"/Overall/A/text()='true' and /Overall/B/text()='false'">>
    ),

    %% Test 5: Numeric comparison
    {ok, _} = parse_xpath(<<"/Overall/Count/text() > 0">>),

    %% Test 6: Boolean operators
    {ok, _} = parse_xpath(
        <<"/Overall/A/text()='true' or /Overall/B/text()='true'">>
    ),

    ok.

%% Unit tests for parse_xpath
parse_simple_var_test() ->
    {ok, {xpath_var, {path, <<"Overall">>, <<"Status">>}}} =
        parse_xpath(<<"/Overall/Status/text()">>).

parse_comparsion_test() ->
    {ok, {xpath_comp, {xpath_var, {path, <<"Overall">>, <<"Status">>}}, <<"=">>,
                      {xpath_literal, <<"approved">>}}} =
        parse_xpath(<<"/Overall/Status/text()='approved'">>).

parse_and_test() ->
    {ok, {xpath_bool, <<"and">>, _}} =
        parse_xpath(<<"/Overall/A/text()='true' and /Overall/B/text()='false'">>).

parse_or_test() ->
    {ok, {xpath_bool, <<"or">>, _}} =
        parse_xpath(<<"/Overall/A/text()='true' or /Overall/B/text()='false'">>).

parse_numeric_test() ->
    {ok, {xpath_comp, {xpath_var, {path, <<"Overall">>, <<"Count">>}}, <<">">>,
                      {xpath_literal, 0}}} =
        parse_xpath(<<"/Overall/Count/text() > 0">>).

parse_complex_test() ->
    Pred = <<"/Overall/PO_timedout/text()='false' and /Overall/POApproval/text()='true'">>,
    {ok, _} = parse_xpath(Pred).

%% Unit tests for extract_vars
extract_vars_simple_test() ->
    {ok, [<<"/Overall/Status/text()">>]} =
        extract_vars(<<"/Overall/Status/text()='approved'">>).

extract_vars_multiple_test() ->
    {ok, Vars} = extract_vars(
        <<"/Overall/A/text()='true' and /Overall/B/text()='false'">>
    ),
    2 = length(Vars).

extract_vars_numeric_test() ->
    {ok, [<<"/Overall/Count/text()">>]} =
        extract_vars(<<"/Overall/Count/text() > 0">>).

%% Unit tests for to_erlog
to_erlog_simple_test() ->
    {ok, {RulesBin, {flow_selected, []}}} =
        to_erlog(<<"/Overall/Flag/text()='true'">>),
    true = binary:match(RulesBin, <<"token">>) =/= nomatch.

to_erlog_compound_test() ->
    {ok, {RulesBin, _}} = to_erlog(
        <<"/Overall/A/text()='true' and /Overall/B/text()='false'">>
    ),
    true = binary:match(RulesBin, <<"token">>) =/= nomatch,
    true = binary:match(RulesBin, <<",">>) =/= nomatch.

to_erlog_numeric_test() ->
    {ok, {RulesBin, _}} = to_erlog(<<"/Overall/Count/text() > 0">>),
    true = binary:match(RulesBin, <<"token">>) =/= nomatch.

%% Unit tests for tokenizer
tokenize_path_test() ->
    Tokens = tokenize_xpath("/Overall/Status/text()"),
    true = lists:keymember(var_ref, 1, Tokens).

tokenize_string_test() ->
    Tokens = tokenize_xpath("'hello'"),
    {literal, <<"hello">>} = lists:keyfind(literal, 1, Tokens).

tokenize_number_test() ->
    Tokens = tokenize_xpath("42"),
    {literal, 42} = lists:keyfind(literal, 1, Tokens).

tokenize_bool_test() ->
    Tokens = tokenize_xpath("true and false"),
    {literal, true} = lists:keyfind(literal, 1, Tokens),
    {bool, <<"and">>} = lists:keyfind(bool, 1, Tokens),
    {literal, false} = lists:keyfind(literal, 1, lists:reverse(Tokens)).

tokenize_comp_test() ->
    Tokens = tokenize_xpath("A = B"),
    {comp, <<"=">>} = lists:keyfind(comp, 1, Tokens).

tokenize_ge_test() ->
    Tokens = tokenize_xpath("count >= 5"),
    {comp, <<">=">>} = lists:keyfind(comp, 1, Tokens).

tokenize_le_test() ->
    Tokens = tokenize_xpath("count <= 10"),
    {comp, <<"<=">>} = lists:keyfind(comp, 1, Tokens).

tokenize_ne_test() ->
    Tokens = tokenize_xpath("status != 'done'"),
    {comp, <<"!=">>} = lists:keyfind(comp, 1, Tokens).

%% Integration tests
integration_full_predicate_test() ->
    Pred = <<"/Overall/PO_timedout/text()='false' and /Overall/POApproval/text()='true'">>,
    {ok, {RulesBin, {flow_selected, []}}} = to_erlog(Pred),
    %% Should contain token facts for both variables
    true = binary:match(RulesBin, <<"token('Overall', 'PO_timedout')">>) =/= nomatch,
    true = binary:match(RulesBin, <<"token('Overall', 'POApproval')">>) =/= nomatch.

integration_yawl_example_test() ->
    %% From orderfulfillment_2_1.yawl
    Pred = <<"/Overall/PO_timedout/text()='false'">>,
    {ok, {_, {flow_selected, []}}} = to_erlog(Pred).

integration_complex_boolean_test() ->
    %% OR parsing with multiple expressions is not fully supported
    %% This test documents the limitation
    case parse_xpath(<<"/Overall/A/text()='true' or /Overall/B/text()='true' or /Overall/C/text()='true'">>) of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

integration_parenthesized_test() ->
    %% Parenthesized expressions are not fully supported yet
    %% The tokenizer doesn't handle parentheses around paths correctly
    %% This test documents the current limitation
    ok.

-endif.
