-module(yawl_dsl).
-behaviour(gen_pnet).

-export([
    parse/1,
    compile/1,
    compile/2,
    to_pnet/1,
    validate/1,
    macro_expand/1,
    macro_expand/2
]).

-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2,
    trigger/3
]).

-export([
    tokenize/1,
    parse_workflow/1,
    ast_to_pnet/1,
    validate_ast/1
]).

-include("../../include/gen_pnet.hrl").

-type token() :: {atom(), binary()} | {atom(), binary(), integer()}.
-type ast_node() ::
    {workflow, binary(), [ast_node()]} |
    {sequence, [ast_node()]} |
    {parallel, [ast_node()]} |
    {choice, [ast_node()]} |
    {loop, ast_node(), ast_node()} |
    {task, binary()} |
    {task, binary(), map()} |
    {condition, binary(), ast_node()} |
    {macro, binary(), [ast_node()]}.

-type pnet_spec() :: #{
    places := [atom()],
    transitions := [atom()],
    preset := #{atom() => [atom()]},
    postset := #{atom() => [atom()]},
    init_marking := #{atom() => [term()]},
    guards := #{atom() => fun()},
    actions := #{atom() => fun()}
}.

-record(dsl_state, {
    spec :: pnet_spec(),
    variables = #{} :: map(),
    context = #{} :: map()
}).

-export_type([token/0, ast_node/0, pnet_spec/0]).

-spec parse(binary() | string()) -> {ok, ast_node()} | {error, term()}.
parse(Input) when is_list(Input) ->
    parse(list_to_binary(Input));
parse(Input) when is_binary(Input) ->
    case tokenize(Input) of
        {ok, Tokens} ->
            case parse_workflow(Tokens) of
                {ok, AST, _Rest} ->
                    case validate_ast(AST) of
                        ok -> {ok, AST};
                        {error, Reason} -> {error, {validation_error, Reason}}
                    end;
                {error, Reason} -> {error, {parse_error, Reason}}
            end;
        {error, Reason} -> {error, {tokenize_error, Reason}}
    end.

-spec compile(ast_node()) -> {ok, pnet_spec()} | {error, term()}.
compile(AST) ->
    compile(AST, #{}).

-spec compile(ast_node(), map()) -> {ok, pnet_spec()} | {error, term()}.
compile(AST, Options) ->
    try
        case macro_expand(AST, Options) of
            {ok, ExpandedAST} ->
                case ast_to_pnet(ExpandedAST) of
                    {ok, PNet} ->
                        case validate_pnet(PNet) of
                            ok -> {ok, PNet};
                            {error, ValidationError} -> {error, {validation_error, ValidationError}}
                        end;
                    {error, CompileError} -> {error, {compilation_error, CompileError}}
                end;
            {error, MacroError} -> {error, {macro_expansion_error, MacroError}}
        end
    catch
        error:Error:Stack ->
            {error, {compilation_exception, Error, Stack}}
    end.

-spec to_pnet(binary() | ast_node()) -> {ok, pnet_spec()} | {error, term()}.
to_pnet(Input) when is_binary(Input); is_list(Input) ->
    case parse(Input) of
        {ok, AST} -> compile(AST);
        {error, Reason} -> {error, Reason}
    end;
to_pnet(AST) ->
    compile(AST).

-spec validate(ast_node() | pnet_spec()) -> ok | {error, term()}.
validate(AST = {workflow, _, _}) ->
    validate_ast(AST);
validate(PNet) when is_map(PNet) ->
    validate_pnet(PNet).

-spec macro_expand(ast_node()) -> {ok, ast_node()} | {error, term()}.
macro_expand(AST) ->
    macro_expand(AST, #{}).

-spec macro_expand(ast_node(), map()) -> {ok, ast_node()} | {error, term()}.
macro_expand(AST, Options) ->
    try
        Macros = maps:get(macros, Options, default_macros()),
        {ok, expand_macros(AST, Macros)}
    catch
        error:Reason:Stack ->
            {error, {expansion_error, Reason, Stack}}
    end.

-spec tokenize(binary()) -> {ok, [token()]} | {error, term()}.
tokenize(Input) ->
    try
        Tokens = lex(Input, 1, []),
        {ok, lists:reverse(Tokens)}
    catch
        error:{lexer_error, Line, Message} ->
            {error, {Line, Message}};
        error:Reason:Stack ->
            {error, {lexer_exception, Reason, Stack}}
    end.

lex(<<>>, _Line, Acc) ->
    Acc;
lex(<<$\n, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line + 1, Acc);
lex(<<C, Rest/binary>>, Line, Acc) when C =:= $\s; C =:= $\t; C =:= $\r ->
    lex(Rest, Line, Acc);
lex(<<$/, $/, Rest/binary>>, Line, Acc) ->
    lex(skip_line_comment(Rest), Line, Acc);
lex(<<$/, $*, Rest/binary>>, Line, Acc) ->
    {RestAfterComment, NewLine} = skip_block_comment(Rest, Line),
    lex(RestAfterComment, NewLine, Acc);
lex(<<${, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'{', <<"{">>, Line} | Acc]);
lex(<<$}, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'}', <<"}">>, Line} | Acc]);
lex(<<$(, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'(', <<"(">>, Line} | Acc]);
lex(<<$), Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{')', <<")">>, Line} | Acc]);
lex(<<$[, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'[', <<"[">>, Line} | Acc]);
lex(<<$], Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{']', <<"]">>, Line} | Acc]);
lex(<<$;, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{';', <<";">>, Line} | Acc]);
lex(<<$,, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{',', <<",">>, Line} | Acc]);
lex(<<$:, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{':',<<":">>, Line} | Acc]);
lex(<<$=, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'=', <<"=">>, Line} | Acc]);
lex(<<$-, $>, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'->', <<"->">>, Line} | Acc]);
lex(<<$|, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'|', <<"|">>, Line} | Acc]);
lex(<<$&, Rest/binary>>, Line, Acc) ->
    lex(Rest, Line, [{'&', <<"&">>, Line} | Acc]);
lex(<<$", Rest/binary>>, Line, Acc) ->
    {String, Rest2} = lex_string(Rest, <<>>),
    lex(Rest2, Line, [{string, String, Line} | Acc]);
lex(<<C, Rest/binary>>, Line, Acc) when (C >= $a andalso C =< $z) orelse
                                         (C >= $A andalso C =< $Z) orelse
                                         C =:= $_ ->
    {Ident, Rest2} = lex_identifier(<<C, Rest/binary>>, <<>>),
    Token = case Ident of
        <<"workflow">> -> {workflow, Ident, Line};
        <<"task">> -> {task, Ident, Line};
        <<"parallel">> -> {parallel, Ident, Line};
        <<"sequence">> -> {sequence, Ident, Line};
        <<"choice">> -> {choice, Ident, Line};
        <<"loop">> -> {loop, Ident, Line};
        <<"while">> -> {while, Ident, Line};
        <<"if">> -> {'if', Ident, Line};
        <<"macro">> -> {macro, Ident, Line};
        <<"end">> -> {'end', Ident, Line};
        <<"start">> -> {start, Ident, Line};
        _ -> {identifier, Ident, Line}
    end,
    lex(Rest2, Line, [Token | Acc]);
lex(<<C, Rest/binary>>, Line, Acc) when C >= $0 andalso C =< $9 ->
    {Number, Rest2} = lex_number(<<C, Rest/binary>>, <<>>),
    lex(Rest2, Line, [{number, Number, Line} | Acc]);
lex(<<C, _/binary>>, Line, _Acc) ->
    error({lexer_error, Line, <<"Unexpected character: ", C>>}).

skip_line_comment(<<$\n, Rest/binary>>) -> Rest;
skip_line_comment(<<_, Rest/binary>>) -> skip_line_comment(Rest);
skip_line_comment(<<>>) -> <<>>.

skip_block_comment(<<$*, $/, Rest/binary>>, Line) ->
    {Rest, Line};
skip_block_comment(<<$\n, Rest/binary>>, Line) ->
    skip_block_comment(Rest, Line + 1);
skip_block_comment(<<_, Rest/binary>>, Line) ->
    skip_block_comment(Rest, Line);
skip_block_comment(<<>>, Line) ->
    error({lexer_error, Line, <<"Unterminated block comment">>}).

lex_string(<<$", Rest/binary>>, Acc) ->
    {Acc, Rest};
lex_string(<<$\\, $", Rest/binary>>, Acc) ->
    lex_string(Rest, <<Acc/binary, $">>);
lex_string(<<$\\, $n, Rest/binary>>, Acc) ->
    lex_string(Rest, <<Acc/binary, $\n>>);
lex_string(<<$\\, $t, Rest/binary>>, Acc) ->
    lex_string(Rest, <<Acc/binary, $\t>>);
lex_string(<<$\\, $\\, Rest/binary>>, Acc) ->
    lex_string(Rest, <<Acc/binary, $\\>>);
lex_string(<<C, Rest/binary>>, Acc) ->
    lex_string(Rest, <<Acc/binary, C>>);
lex_string(<<>>, _Acc) ->
    error({lexer_error, 0, <<"Unterminated string">>}).

lex_identifier(<<C, Rest/binary>>, Acc) when (C >= $a andalso C =< $z) orelse
                                               (C >= $A andalso C =< $Z) orelse
                                               (C >= $0 andalso C =< $9) orelse
                                               C =:= $_ ->
    lex_identifier(Rest, <<Acc/binary, C>>);
lex_identifier(Rest, Acc) ->
    {Acc, Rest}.

lex_number(<<C, Rest/binary>>, Acc) when C >= $0 andalso C =< $9 ->
    lex_number(Rest, <<Acc/binary, C>>);
lex_number(Rest, Acc) ->
    {Acc, Rest}.

-spec parse_workflow([token()]) -> {ok, ast_node(), [token()]} | {error, term()}.
parse_workflow([{workflow, _, _}, {identifier, Name, _}, {'{', _, _} | Rest]) ->
    case parse_statements(Rest, []) of
        {ok, Statements, [{'}', _, _} | Rest2]} ->
            {ok, {workflow, Name, Statements}, Rest2};
        {ok, _Statements, _Rest2} ->
            {error, <<"Missing closing brace for workflow">>};
        {error, Reason} ->
            {error, Reason}
    end;
parse_workflow([{workflow, _, _} | _]) ->
    {error, <<"Invalid workflow declaration">>};
parse_workflow(Tokens) ->
    case parse_statement(Tokens) of
        {ok, Statement, Rest} ->
            {ok, {workflow, <<"anonymous">>, [Statement]}, Rest};
        {error, Reason} ->
            {error, Reason}
    end.

parse_statements([], Acc) ->
    {ok, lists:reverse(Acc), []};
parse_statements([{'}', _, _} | _] = Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_statements(Tokens, Acc) ->
    case parse_statement(Tokens) of
        {ok, Statement, Rest} ->
            parse_statements(Rest, [Statement | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

parse_statement([{sequence, _, _} | Rest]) ->
    parse_sequence(Rest);
parse_statement([{parallel, _, _} | Rest]) ->
    parse_parallel(Rest);
parse_statement([{choice, _, _} | Rest]) ->
    parse_choice(Rest);
parse_statement([{loop, _, _} | Rest]) ->
    parse_loop(Rest);
parse_statement([{macro, _, _} | Rest]) ->
    parse_macro_call(Rest);
parse_statement(Tokens) ->
    parse_flow(Tokens).

parse_sequence([{'{', _, _} | Rest]) ->
    case parse_flow_list(Rest, []) of
        {ok, Flows, [{'}', _, _}, {';', _, _} | Rest2]} ->
            {ok, {sequence, Flows}, Rest2};
        {ok, Flows, [{'}', _, _} | Rest2]} ->
            {ok, {sequence, Flows}, Rest2};
        {ok, _Flows, _Rest2} ->
            {error, <<"Missing closing brace or semicolon for sequence">>};
        {error, Reason} ->
            {error, Reason}
    end;
parse_sequence(_) ->
    {error, <<"Invalid sequence syntax">>}.

parse_parallel([{'{', _, _} | Rest]) ->
    case parse_flow_list(Rest, []) of
        {ok, Branches, [{'}', _, _}, {';', _, _} | Rest2]} ->
            {ok, {parallel, Branches}, Rest2};
        {ok, Branches, [{'}', _, _} | Rest2]} ->
            {ok, {parallel, Branches}, Rest2};
        {ok, _Branches, _Rest2} ->
            {error, <<"Missing closing brace for parallel">>};
        {error, Reason} ->
            {error, Reason}
    end;
parse_parallel(_) ->
    {error, <<"Invalid parallel syntax">>}.

parse_choice([{'{', _, _} | Rest]) ->
    case parse_choice_branches(Rest, []) of
        {ok, Branches, [{'}', _, _}, {';', _, _} | Rest2]} ->
            {ok, {choice, Branches}, Rest2};
        {ok, Branches, [{'}', _, _} | Rest2]} ->
            {ok, {choice, Branches}, Rest2};
        {ok, _Branches, _Rest2} ->
            {error, <<"Missing closing brace for choice">>};
        {error, Reason} ->
            {error, Reason}
    end;
parse_choice(_) ->
    {error, <<"Invalid choice syntax">>}.

parse_loop(Tokens) ->
    case parse_task(Tokens) of
        {ok, Body, Rest} ->
            case Rest of
                [{';', _, _} | Rest2] ->
                    {ok, {loop, {task, <<"condition">>}, Body}, Rest2};
                _ ->
                    {ok, {loop, {task, <<"condition">>}, Body}, Rest}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_macro_call([{identifier, Name, _}, {'(', _, _} | Rest]) ->
    case parse_arg_list(Rest, []) of
        {ok, Args, [{')', _, _}, {';', _, _} | Rest2]} ->
            {ok, {macro, Name, Args}, Rest2};
        {ok, Args, [{')', _, _} | Rest2]} ->
            {ok, {macro, Name, Args}, Rest2};
        {ok, _Args, _Rest2} ->
            {error, <<"Missing closing parenthesis for macro call">>};
        {error, Reason} ->
            {error, Reason}
    end;
parse_macro_call(_) ->
    {error, <<"Invalid macro call syntax">>}.

parse_flow(Tokens) ->
    parse_flow_chain(Tokens, []).

parse_flow_chain(Tokens, Acc) ->
    case parse_task(Tokens) of
        {ok, Task, [{'->', _, _} | Rest]} ->
            parse_flow_chain(Rest, [Task | Acc]);
        {ok, Task, [{';', _, _} | Rest]} ->
            Tasks = lists:reverse([Task | Acc]),
            {ok, build_flow_sequence(Tasks), Rest};
        {ok, Task, Rest} ->
            Tasks = lists:reverse([Task | Acc]),
            {ok, build_flow_sequence(Tasks), Rest};
        {error, Reason} ->
            {error, Reason}
    end.

build_flow_sequence([Single]) ->
    Single;
build_flow_sequence(Tasks) ->
    {sequence, Tasks}.

parse_task([{identifier, Name, _} | Rest]) ->
    case Rest of
        [{'{', _, _} | Rest2] ->
            case parse_task_options(Rest2, #{}) of
                {ok, Options, [{'}', _, _} | Rest3]} ->
                    {ok, {task, Name, Options}, Rest3};
                {ok, _Options, _Rest3} ->
                    {error, <<"Missing closing brace for task options">>};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {ok, {task, Name}, Rest}
    end;
parse_task([{start, Name, _} | Rest]) ->
    {ok, {task, Name}, Rest};
parse_task([{'end', Name, _} | Rest]) ->
    {ok, {task, Name}, Rest};
parse_task([{string, Name, _} | Rest]) ->
    {ok, {task, Name}, Rest};
parse_task(_) ->
    {error, <<"Expected task identifier">>}.

parse_task_options([{'}', _, _} | _] = Rest, Acc) ->
    {ok, Acc, Rest};
parse_task_options([{identifier, Key, _}, {'=', _, _} | Rest], Acc) ->
    case parse_task_value(Rest) of
        {ok, Value, [{',', _, _} | Rest2]} ->
            parse_task_options(Rest2, maps:put(Key, Value, Acc));
        {ok, Value, Rest2} ->
            parse_task_options(Rest2, maps:put(Key, Value, Acc));
        {error, Reason} ->
            {error, Reason}
    end;
parse_task_options(_, _) ->
    {error, <<"Invalid task option syntax">>}.

parse_task_value([{string, Value, _} | Rest]) ->
    {ok, Value, Rest};
parse_task_value([{number, Value, _} | Rest]) ->
    {ok, binary_to_integer(Value), Rest};
parse_task_value([{identifier, Value, _} | Rest]) ->
    {ok, Value, Rest};
parse_task_value(_) ->
    {error, <<"Invalid task option value">>}.

parse_flow_list([{'}', _, _} | _] = Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_flow_list(Tokens, Acc) ->
    case parse_flow(Tokens) of
        {ok, Flow, [{',', _, _} | Rest]} ->
            parse_flow_list(Rest, [Flow | Acc]);
        {ok, Flow, Rest} ->
            {ok, lists:reverse([Flow | Acc]), Rest};
        {error, Reason} ->
            {error, Reason}
    end.

parse_choice_branches([{'}', _, _} | _] = Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_choice_branches(Tokens, Acc) ->
    case parse_condition_branch(Tokens) of
        {ok, Branch, [{',', _, _} | Rest]} ->
            parse_choice_branches(Rest, [Branch | Acc]);
        {ok, Branch, Rest} ->
            {ok, lists:reverse([Branch | Acc]), Rest};
        {error, Reason} ->
            {error, Reason}
    end.

parse_condition_branch([{identifier, Condition, _}, {'->', _, _} | Rest]) ->
    case parse_flow(Rest) of
        {ok, Flow, Rest2} ->
            {ok, {condition, Condition, Flow}, Rest2};
        {error, Reason} ->
            {error, Reason}
    end;
parse_condition_branch(_) ->
    {error, <<"Invalid condition branch syntax">>}.

parse_arg_list([{')', _, _} | _] = Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_arg_list([{identifier, Arg, _} | Rest], Acc) ->
    case Rest of
        [{',', _, _} | Rest2] ->
            parse_arg_list(Rest2, [Arg | Acc]);
        _ ->
            {ok, lists:reverse([Arg | Acc]), Rest}
    end;
parse_arg_list([{string, Arg, _} | Rest], Acc) ->
    case Rest of
        [{',', _, _} | Rest2] ->
            parse_arg_list(Rest2, [Arg | Acc]);
        _ ->
            {ok, lists:reverse([Arg | Acc]), Rest}
    end;
parse_arg_list(_, _) ->
    {error, <<"Invalid macro argument list">>}.

-spec validate_ast(ast_node()) -> ok | {error, term()}.
validate_ast({workflow, _Name, Statements}) ->
    validate_statements(Statements);
validate_ast({sequence, Tasks}) ->
    validate_tasks(Tasks);
validate_ast({parallel, Branches}) ->
    validate_branches(Branches);
validate_ast({choice, Branches}) ->
    validate_choice_branches(Branches);
validate_ast({loop, _Condition, Body}) ->
    validate_ast(Body);
validate_ast({task, _Name}) ->
    ok;
validate_ast({task, _Name, _Options}) ->
    ok;
validate_ast({condition, _Cond, Body}) ->
    validate_ast(Body);
validate_ast({macro, _Name, _Args}) ->
    ok;
validate_ast(_) ->
    {error, <<"Unknown AST node type">>}.

validate_statements([]) ->
    ok;
validate_statements([Statement | Rest]) ->
    case validate_ast(Statement) of
        ok -> validate_statements(Rest);
        {error, Reason} -> {error, Reason}
    end.

validate_tasks([]) ->
    ok;
validate_tasks([Task | Rest]) ->
    case validate_ast(Task) of
        ok -> validate_tasks(Rest);
        {error, Reason} -> {error, Reason}
    end.

validate_branches(Branches) when length(Branches) >= 2 ->
    validate_tasks(Branches);
validate_branches(_) ->
    {error, <<"Parallel must have at least 2 branches">>}.

validate_choice_branches([]) ->
    {error, <<"Choice must have at least one branch">>};
validate_choice_branches(Branches) ->
    validate_tasks(Branches).

expand_macros({workflow, Name, Statements}, Macros) ->
    {workflow, Name, [expand_macros(S, Macros) || S <- Statements]};
expand_macros({sequence, Tasks}, Macros) ->
    {sequence, [expand_macros(T, Macros) || T <- Tasks]};
expand_macros({parallel, Branches}, Macros) ->
    {parallel, [expand_macros(B, Macros) || B <- Branches]};
expand_macros({choice, Branches}, Macros) ->
    {choice, [expand_macros(B, Macros) || B <- Branches]};
expand_macros({loop, Condition, Body}, Macros) ->
    {loop, expand_macros(Condition, Macros), expand_macros(Body, Macros)};
expand_macros({condition, Cond, Body}, Macros) ->
    {condition, Cond, expand_macros(Body, Macros)};
expand_macros({macro, Name, Args}, Macros) ->
    case maps:get(Name, Macros, undefined) of
        undefined -> {macro, Name, Args};
        MacroFun when is_function(MacroFun) -> MacroFun(Args);
        MacroTemplate -> substitute_macro(MacroTemplate, Args)
    end;
expand_macros(Node, _Macros) ->
    Node.

substitute_macro(Template, Args) ->
    Template.

default_macros() ->
    #{
        <<"retry">> => fun([Task]) ->
            {loop, {task, <<"retry_condition">>}, {task, Task}}
        end,
        <<"fork_join">> => fun(Branches) ->
            {parallel, [{task, B} || B <- Branches]}
        end,
        <<"if_then_else">> => fun([Cond, Then, Else]) ->
            {choice, [
                {condition, Cond, {task, Then}},
                {condition, <<"else">>, {task, Else}}
            ]}
        end,
        <<"map">> => fun([Task, Collection]) ->
            {parallel, [{sequence, [{task, Task}, {task, Item}]} || Item <- [Collection]]}
        end
    }.

-spec ast_to_pnet(ast_node()) -> {ok, pnet_spec()} | {error, term()}.
ast_to_pnet({workflow, _Name, Statements}) ->
    try
        {Places, Transitions, Preset, Postset, InitMarking} =
            statements_to_pnet(Statements, 0, [], [], #{}, #{}, #{}),

        AllPlaces = lists:usort([p_start, p_end | Places]),
        AllTransitions = lists:usort(Transitions),

        Guards = #{},
        Actions = #{},

        PNet = #{
            places => AllPlaces,
            transitions => AllTransitions,
            preset => Preset,
            postset => Postset,
            init_marking => InitMarking#{p_start => [init]},
            guards => Guards,
            actions => Actions
        },
        {ok, PNet}
    catch
        error:Reason:Stack ->
            {error, {ast_to_pnet_error, Reason, Stack}}
    end;
ast_to_pnet(Node) ->
    ast_to_pnet({workflow, <<"anonymous">>, [Node]}).

statements_to_pnet([], _Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {Places, Transitions, Preset, Postset, InitMarking};
statements_to_pnet([Statement | Rest], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {NewPlaces, NewTransitions, NewPreset, NewPostset, NewInitMarking, NewCounter} =
        statement_to_pnet(Statement, Counter),

    AllPlaces = lists:usort(Places ++ NewPlaces),
    AllTransitions = lists:usort(Transitions ++ NewTransitions),
    MergedPreset = maps:merge(Preset, NewPreset),
    MergedPostset = maps:merge(Postset, NewPostset),
    MergedInitMarking = maps:merge(InitMarking, NewInitMarking),

    statements_to_pnet(Rest, NewCounter, AllPlaces, AllTransitions,
                      MergedPreset, MergedPostset, MergedInitMarking).

statement_to_pnet({sequence, Tasks}, Counter) ->
    sequence_to_pnet(Tasks, Counter);
statement_to_pnet({parallel, Branches}, Counter) ->
    parallel_to_pnet(Branches, Counter);
statement_to_pnet({choice, Branches}, Counter) ->
    choice_to_pnet(Branches, Counter);
statement_to_pnet({loop, Condition, Body}, Counter) ->
    loop_to_pnet(Condition, Body, Counter);
statement_to_pnet({task, Name}, Counter) ->
    task_to_pnet(Name, Counter);
statement_to_pnet({task, Name, _Options}, Counter) ->
    task_to_pnet(Name, Counter);
statement_to_pnet(_Node, Counter) ->
    {[], [], #{}, #{}, #{}, Counter}.

task_to_pnet(Name, Counter) ->
    PlaceName = task_place(Name),
    TransitionName = task_transition(Name),

    Places = [PlaceName],
    Transitions = [TransitionName],
    Preset = #{TransitionName => [PlaceName]},
    Postset = #{TransitionName => [PlaceName]},
    InitMarking = #{PlaceName => []},

    {Places, Transitions, Preset, Postset, InitMarking, Counter + 1}.

sequence_to_pnet(Tasks, Counter) ->
    sequence_to_pnet(Tasks, Counter, [], [], #{}, #{}, #{}).

sequence_to_pnet([], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {Places, Transitions, Preset, Postset, InitMarking, Counter};
sequence_to_pnet([Task | Rest], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {NewPlaces, NewTrans, NewPreset, NewPostset, NewInit, NewCounter} =
        statement_to_pnet(Task, Counter),

    AllPlaces = lists:usort(Places ++ NewPlaces),
    AllTransitions = lists:usort(Transitions ++ NewTrans),
    MergedPreset = maps:merge(Preset, NewPreset),
    MergedPostset = maps:merge(Postset, NewPostset),
    MergedInit = maps:merge(InitMarking, NewInit),

    sequence_to_pnet(Rest, NewCounter, AllPlaces, AllTransitions,
                    MergedPreset, MergedPostset, MergedInit).

parallel_to_pnet(Branches, Counter) ->
    SplitPlace = list_to_atom("p_split_" ++ integer_to_list(Counter)),
    JoinPlace = list_to_atom("p_join_" ++ integer_to_list(Counter)),
    SplitTrans = list_to_atom("t_split_" ++ integer_to_list(Counter)),
    JoinTrans = list_to_atom("t_join_" ++ integer_to_list(Counter)),

    {BranchPlaces, BranchTransitions, BranchPreset, BranchPostset, BranchInit, NewCounter} =
        branches_to_pnet(Branches, Counter + 1, [], [], #{}, #{}, #{}),

    Places = [SplitPlace, JoinPlace | BranchPlaces],
    Transitions = [SplitTrans, JoinTrans | BranchTransitions],

    Preset = maps:merge(BranchPreset, #{
        SplitTrans => [SplitPlace],
        JoinTrans => BranchPlaces
    }),

    Postset = maps:merge(BranchPostset, #{
        SplitTrans => BranchPlaces,
        JoinTrans => [JoinPlace]
    }),

    InitMarking = maps:merge(BranchInit, #{
        SplitPlace => [],
        JoinPlace => []
    }),

    {Places, Transitions, Preset, Postset, InitMarking, NewCounter}.

branches_to_pnet([], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {Places, Transitions, Preset, Postset, InitMarking, Counter};
branches_to_pnet([Branch | Rest], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {NewPlaces, NewTrans, NewPreset, NewPostset, NewInit, NewCounter} =
        statement_to_pnet(Branch, Counter),

    AllPlaces = lists:usort(Places ++ NewPlaces),
    AllTransitions = lists:usort(Transitions ++ NewTrans),
    MergedPreset = maps:merge(Preset, NewPreset),
    MergedPostset = maps:merge(Postset, NewPostset),
    MergedInit = maps:merge(InitMarking, NewInit),

    branches_to_pnet(Rest, NewCounter, AllPlaces, AllTransitions,
                    MergedPreset, MergedPostset, MergedInit).

choice_to_pnet(Branches, Counter) ->
    ChoicePlace = list_to_atom("p_choice_" ++ integer_to_list(Counter)),
    MergePlace = list_to_atom("p_merge_" ++ integer_to_list(Counter)),

    {BranchPlaces, BranchTransitions, BranchPreset, BranchPostset, BranchInit, NewCounter} =
        choice_branches_to_pnet(Branches, Counter + 1, [], [], #{}, #{}, #{}),

    Places = [ChoicePlace, MergePlace | BranchPlaces],
    Transitions = BranchTransitions,

    Preset = maps:merge(BranchPreset, #{
    }),

    Postset = maps:merge(BranchPostset, #{
    }),

    InitMarking = maps:merge(BranchInit, #{
        ChoicePlace => [],
        MergePlace => []
    }),

    {Places, Transitions, Preset, Postset, InitMarking, NewCounter}.

choice_branches_to_pnet([], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {Places, Transitions, Preset, Postset, InitMarking, Counter};
choice_branches_to_pnet([{condition, _Cond, Branch} | Rest], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {NewPlaces, NewTrans, NewPreset, NewPostset, NewInit, NewCounter} =
        statement_to_pnet(Branch, Counter),

    AllPlaces = lists:usort(Places ++ NewPlaces),
    AllTransitions = lists:usort(Transitions ++ NewTrans),
    MergedPreset = maps:merge(Preset, NewPreset),
    MergedPostset = maps:merge(Postset, NewPostset),
    MergedInit = maps:merge(InitMarking, NewInit),

    choice_branches_to_pnet(Rest, NewCounter, AllPlaces, AllTransitions,
                           MergedPreset, MergedPostset, MergedInit);
choice_branches_to_pnet([Branch | Rest], Counter, Places, Transitions, Preset, Postset, InitMarking) ->
    {NewPlaces, NewTrans, NewPreset, NewPostset, NewInit, NewCounter} =
        statement_to_pnet(Branch, Counter),

    AllPlaces = lists:usort(Places ++ NewPlaces),
    AllTransitions = lists:usort(Transitions ++ NewTrans),
    MergedPreset = maps:merge(Preset, NewPreset),
    MergedPostset = maps:merge(Postset, NewPostset),
    MergedInit = maps:merge(InitMarking, NewInit),

    choice_branches_to_pnet(Rest, NewCounter, AllPlaces, AllTransitions,
                           MergedPreset, MergedPostset, MergedInit).

loop_to_pnet(_Condition, Body, Counter) ->
    LoopPlace = list_to_atom("p_loop_" ++ integer_to_list(Counter)),
    LoopTrans = list_to_atom("t_loop_" ++ integer_to_list(Counter)),

    {BodyPlaces, BodyTransitions, BodyPreset, BodyPostset, BodyInit, NewCounter} =
        statement_to_pnet(Body, Counter + 1),

    Places = [LoopPlace | BodyPlaces],
    Transitions = [LoopTrans | BodyTransitions],

    Preset = maps:merge(BodyPreset, #{
        LoopTrans => [LoopPlace | BodyPlaces]
    }),

    Postset = maps:merge(BodyPostset, #{
        LoopTrans => [LoopPlace]
    }),

    InitMarking = maps:merge(BodyInit, #{
        LoopPlace => []
    }),

    {Places, Transitions, Preset, Postset, InitMarking, NewCounter}.

task_place(Name) when is_binary(Name) ->
    binary_to_atom(<<"p_", Name/binary>>, utf8);
task_place(Name) when is_atom(Name) ->
    task_place(atom_to_binary(Name, utf8)).

task_transition(Name) when is_binary(Name) ->
    binary_to_atom(<<"t_", Name/binary>>, utf8);
task_transition(Name) when is_atom(Name) ->
    task_transition(atom_to_binary(Name, utf8)).

-spec validate_pnet(pnet_spec()) -> ok | {error, term()}.
validate_pnet(#{places := Places, transitions := Transitions,
                preset := Preset, postset := _Postset}) ->
    case validate_places(Places) of
        ok ->
            case validate_transitions(Transitions) of
                ok ->
                    case validate_preset(Preset, Places, Transitions) of
                        ok -> ok;
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end;
validate_pnet(_) ->
    {error, <<"Invalid PNet specification">>}.

validate_places(Places) when is_list(Places), length(Places) > 0 ->
    case lists:all(fun is_atom/1, Places) of
        true -> ok;
        false -> {error, <<"All places must be atoms">>}
    end;
validate_places(_) ->
    {error, <<"Places must be a non-empty list">>}.

validate_transitions(Transitions) when is_list(Transitions), length(Transitions) > 0 ->
    case lists:all(fun is_atom/1, Transitions) of
        true -> ok;
        false -> {error, <<"All transitions must be atoms">>}
    end;
validate_transitions(_) ->
    {error, <<"Transitions must be a non-empty list">>}.

validate_preset(Preset, Places, Transitions) when is_map(Preset) ->
    PresetKeys = maps:keys(Preset),
    case lists:all(fun(T) -> lists:member(T, Transitions) end, PresetKeys) of
        true ->
            PresetValues = maps:values(Preset),
            case lists:all(fun(Ps) ->
                lists:all(fun(P) -> lists:member(P, Places) end, Ps)
            end, PresetValues) of
                true -> ok;
                false -> {error, <<"Preset values must reference valid places">>}
            end;
        false ->
            {error, <<"Preset keys must reference valid transitions">>}
    end;
validate_preset(_, _, _) ->
    {error, <<"Preset must be a map">>}.

place_lst() ->
    [p_start, p_end].

trsn_lst() ->
    [t_init, t_finish].

init_marking(p_start, _UsrInfo) ->
    [init];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_init) -> [p_start];
preset(t_finish) -> [p_start];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

fire(t_init, _Mode, _UsrInfo) ->
    {produce, #{p_end => [done]}};
fire(t_finish, _Mode, _UsrInfo) ->
    {produce, #{p_end => [complete]}};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

init(NetArg) ->
    case NetArg of
        Spec when is_map(Spec) ->
            #dsl_state{spec = Spec};
        _ ->
            #dsl_state{spec = #{places => [], transitions => []}}
    end.

handle_call(get_spec, _From, State = #dsl_state{spec = Spec}) ->
    {reply, {ok, Spec}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

trigger(_Place, _Token, _State) ->
    pass.
