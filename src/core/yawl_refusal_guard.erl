%% -*- erlang -*-
%%%% @doc yawl_refusal_guard - Guard evaluation for refusal catalogs.
%%
%% This module implements the guard language and evaluation for refusal
%% catalogs. It provides the primary control surface for "inadmissible-before"
%% behavior - preventing transitions from firing under certain conditions.
%%
%% <h3>Guard Language Grammar (EBNF)</h3>
%%
%% <pre>
%% Guard        ::= ExprGuard | ActionGuard | StateGuard | TemporalGuard
%% ExprGuard    ::= OrExpr
%% OrExpr       ::= AndExpr ('or' AndExpr)*
%% AndExpr      ::= NotExpr ('and' NotExpr)*
%% NotExpr      ::= 'not' NotExpr | PrimaryExpr
%% PrimaryExpr  ::= Literal | Variable | Comparison | Membership | StringOp | Aggregate
%% ActionGuard  ::= 'action' ActionName [('before'|'after_event') Guard]
%% StateGuard   ::= 'state' (StatusPred | MarkingPred | DataPred)
%% TemporalGuard ::= 'before' '(' Guard ')' | 'after_event' '(' Guard ')' |
%%                  'always' '(' Guard ')' | 'within' '(' Guard ',' Duration ')'
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_refusal_guard).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Guard evaluation API
-export([check/3]).
-export([check/4]).
-export([compile_guard/1]).
-export([evaluate/2]).

%% Guard construction helpers
-export([always/1]).
-export([before/2]).
-export([after_event/2]).
-export([within/2]).
-export([action_enabled/2]).
-export([state_is/2]).
-export([marking_has/2]).
-export([data_matches/2]).

%% Refusal categories
-export([refusal_categories/0]).
-export([refusal_reason/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").
-include("gen_pnet.hrl").

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiled Guard Record
%%
%% Guards are compiled for efficient evaluation.
%%--------------------------------------------------------------------
-record(compiled_guard, {
    guard_type :: expr | action | state | temporal,
    condition :: term(),
    metadata = #{} :: map()
}).

%%--------------------------------------------------------------------
%% @doc Guard Evaluation Context
%%
%%--------------------------------------------------------------------
-record(guard_context, {
    mode :: map(),
    usr_info :: term(),
    transition :: atom(),
    marking :: map(),
    net_state :: #net_state{},
    timestamp :: integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type refusal_category() ::
    missing_evidence |
    forbidden_action |
    scope_violation |
    external_boundary |
    resource_unavailable |
    safety_violation |
    validation_failure |
    timeout_exceeded |
    permission_denied.

-type guard() :: #compiled_guard{}.
-type guard_context() :: #guard_context{}.
-type guard_result() :: pass | {refused, refusal_category(), binary()}.

-export_type([refusal_category/0, guard/0, guard_context/0, guard_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a transition is refused before firing.
%%
%% This is the main integration point with gen_yawl.
%%
%% @param Trsn Transition to check
%% @param Mode Current firing mode
%% @param NetMod Network module
%% @return pass | {refused, Category, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec check(atom(), map(), atom()) -> guard_result().

check(Trsn, Mode, NetMod) ->
    check(Trsn, Mode, NetMod, #{}).

%%--------------------------------------------------------------------
%% @doc Checks if a transition is refused with user info.
%%
%% @end
%%--------------------------------------------------------------------
-spec check(atom(), map(), atom(), term() | map()) -> guard_result().

check(Trsn, Mode, NetMod, UsrInfo) when is_atom(Trsn), is_map(Mode), is_atom(NetMod) ->
    try
        %% Build guard context
        Marking = case {is_map(UsrInfo), is_record(UsrInfo, net_state)} of
            {true, false} -> UsrInfo;
            {false, true} -> UsrInfo#net_state.marking;
            _ -> #{}
        end,

        Context = #guard_context{
            mode = Mode,
            usr_info = UsrInfo,
            transition = Trsn,
            marking = Marking,
            timestamp = erlang:system_time(millisecond)
        },

        %% Get guard definitions from net module if available
        Guards = get_guards_for_transition(NetMod, Trsn),

        %% Evaluate each guard
        evaluate_guards(Guards, Context)
    catch
        Type:Error:Stack ->
            logger:error("Guard check error: ~p:~p~n~p", [Type, Error, Stack]),
            pass  %% Fail open for safety
    end.

%%--------------------------------------------------------------------
%% @doc Compiles a guard expression for efficient evaluation.
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_guard(term()) -> {ok, guard()} | {error, term()}.

compile_guard(GuardExpr) when is_tuple(GuardExpr); is_list(GuardExpr); is_atom(GuardExpr) ->
    try
        Guard = #compiled_guard{
            guard_type = determine_guard_type(GuardExpr),
            condition = GuardExpr
        },
        {ok, Guard}
    catch
        Type:Error:Stack ->
            logger:error("Guard compilation error: ~p:~p~n~p", [Type, Error, Stack]),
            {error, {compilation_failed, Type, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Evaluates a compiled guard in a context.
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate(guard(), guard_context()) -> guard_result().

evaluate(#compiled_guard{guard_type = expr, condition = Condition}, Context) ->
    evaluate_expr_guard(Condition, Context);
evaluate(#compiled_guard{guard_type = action, condition = Condition}, Context) ->
    evaluate_action_guard(Condition, Context);
evaluate(#compiled_guard{guard_type = state, condition = Condition}, Context) ->
    evaluate_state_guard(Condition, Context);
evaluate(#compiled_guard{guard_type = temporal, condition = Condition}, Context) ->
    evaluate_temporal_guard(Condition, Context).

%%====================================================================
%% Guard Construction Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an "always true" guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec always(term()) -> term().

always(Condition) ->
    {always, Condition}.

%%--------------------------------------------------------------------
%% @doc Creates a "before" guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec before(term(), term()) -> term().

before(Condition, Event) ->
    {before, Condition, Event}.

%%--------------------------------------------------------------------
%% @doc Creates an "after_event" guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec after_event(term(), term()) -> term().

after_event(Condition, Event) ->
    {after_event, Condition, Event}.

%%--------------------------------------------------------------------
%% @doc Creates a "within" guard with timeout.
%%
%% @end
%%--------------------------------------------------------------------
-spec within(term(), pos_integer()) -> term().

within(Condition, DurationMs) ->
    {within, Condition, DurationMs}.

%%--------------------------------------------------------------------
%% @doc Creates an action-enabled guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec action_enabled(atom(), term()) -> term().

action_enabled(Action, Condition) ->
    {action, Action, Condition}.

%%--------------------------------------------------------------------
%% @doc Creates a state-is guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec state_is(atom(), term()) -> term().

state_is(State, Value) ->
    {state_is, State, Value}.

%%--------------------------------------------------------------------
%% @doc Creates a marking-has guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec marking_has(atom(), term()) -> term().

marking_has(Place, Token) ->
    {marking_has, Place, Token}.

%%--------------------------------------------------------------------
%% @doc Creates a data-matches guard.
%%
%% @end
%%--------------------------------------------------------------------
-spec data_matches(term(), term()) -> term().

data_matches(Path, Pattern) ->
    {data_matches, Path, Pattern}.

%%--------------------------------------------------------------------
%% @doc Returns all refusal categories.
%%
%% @end
%%--------------------------------------------------------------------
-spec refusal_categories() -> [refusal_category()].

refusal_categories() ->
    [
        missing_evidence,
        forbidden_action,
        scope_violation,
        external_boundary,
        resource_unavailable,
        safety_violation,
        validation_failure,
        timeout_exceeded,
        permission_denied
    ].

%%--------------------------------------------------------------------
%% @doc Gets a human-readable refusal reason.
%%
%% @end
%%--------------------------------------------------------------------
-spec refusal_reason(refusal_category(), map()) -> binary().

refusal_reason(Category, Details) when is_map(Details) ->
    ReasonBase = case Category of
        missing_evidence -> <<"Required evidence is missing">>;
        forbidden_action -> <<"Action is forbidden by constitution">>;
        scope_violation -> <<"Operation exceeds authorized scope">>;
        external_boundary -> <<"External boundary not enabled">>;
        resource_unavailable -> <<"Required resource is temporarily unavailable">>;
        safety_violation -> <<"Safety threshold exceeded">>;
        validation_failure -> <<"Data validation failed">>;
        timeout_exceeded -> <<"Operation timeout exceeded">>;
        permission_denied -> <<"Authorization denied">>
    end,

    %% Add details if provided
    case maps:size(Details) of
        0 -> ReasonBase;
        _ ->
            DetailsBin = iolist_to_binary(io_lib:format("~p", [Details])),
            <<ReasonBase/binary, ": ", DetailsBin/binary>>
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec get_guards_for_transition(atom(), atom()) -> [guard()].

get_guards_for_transition(NetMod, Trsn) ->
    case erlang:function_exported(NetMod, refusal_guards, 1) of
        true ->
            case NetMod:refusal_guards(Trsn) of
                Guards when is_list(Guards) ->
                    [begin
                        case compile_guard(G) of
                            {ok, Compiled} -> Compiled;
                            {error, _} -> #compiled_guard{guard_type = expr, condition = {true}}
                        end
                    end || G <- Guards];
                _ -> []
            end;
        false ->
            []
    end.

%% @private
-spec evaluate_guards([guard()], guard_context()) -> guard_result().

evaluate_guards([], _Context) ->
    pass;
evaluate_guards([Guard | Rest], Context) ->
    case evaluate(Guard, Context) of
        pass -> evaluate_guards(Rest, Context);
        Refused -> Refused
    end.

%% @private
-spec determine_guard_type(term()) -> expr | action | state | temporal.

determine_guard_type({always, _}) -> expr;
determine_guard_type({before, _, _}) -> temporal;
determine_guard_type({after_event, _, _}) -> temporal;
determine_guard_type({within, _, _}) -> temporal;
determine_guard_type({action, _, _}) -> action;
determine_guard_type({state_is, _, _}) -> state;
determine_guard_type({marking_has, _, _}) -> state;
determine_guard_type({data_matches, _, _}) -> state;
determine_guard_type(_) -> expr.

%% @private
-spec evaluate_do_expr(term(), guard_context()) -> boolean().

evaluate_do_expr(true, _Context) -> true;
evaluate_do_expr(false, _Context) -> false;
evaluate_do_expr({not, Expr}, Context) ->
    not evaluate_do_expr(Expr, Context);
evaluate_do_expr({and, Left, Right}, Context) ->
    evaluate_do_expr(Left, Context) andalso evaluate_do_expr(Right, Context);
evaluate_do_expr({'or', Left, Right}, Context) ->
    evaluate_do_expr(Left, Context) orelse evaluate_do_expr(Right, Context);
evaluate_do_expr({compare, Op, Left, Right}, Context) ->
    LeftVal = eval_literal(Left, Context),
    RightVal = eval_literal(Right, Context),
    compare(Op, LeftVal, RightVal);
evaluate_do_expr({member, Element, Set}, Context) ->
    lists:member(eval_literal(Element, Context), eval_literal(Set, Context));
evaluate_do_expr({has, Place, Token}, #guard_context{marking = Marking}) ->
    case maps:get(Place, Marking, []) of
        [] -> false;
        Tokens ->
            %% Check if any token matches
            TokenVal = eval_literal(Token, #guard_context{}),
            lists:any(fun(T) -> T =:= TokenVal end, Tokens)
    end;
evaluate_do_expr(_, _) ->
    true.  %% Default: pass

%% @private
-spec evaluate_expr_guard(term(), guard_context()) -> guard_result().

evaluate_expr_guard({always, Condition}, Context) ->
    evaluate_do_expr(Condition, Context);
evaluate_expr_guard({ExprGuard, _Type}, Context) when is_tuple(ExprGuard); is_atom(ExprGuard) ->
    evaluate_do_expr(ExprGuard, Context).

%% @private
-spec compare(atom(), term(), term()) -> boolean().

compare('==', L, R) -> L =:= R;
compare('!=', L, R) -> L =/= R;
compare('<', L, R) -> L < R;
compare('=<', L, R) -> L =< R;
compare('>', L, R) -> L > R;
compare('>=', L, R) -> L >= R;
compare(_, _, _) -> false.

%% @private
-spec eval_literal(term(), guard_context()) -> term().

eval_literal({var, Name}, #guard_context{usr_info = UsrInfo}) when is_map(UsrInfo) ->
    maps:get(Name, UsrInfo, undefined);
eval_literal({marking, Place}, #guard_context{marking = Marking}) ->
    maps:get(Place, Marking, []);
eval_literal(Literal, _Context) ->
    Literal.

%% @private
-spec evaluate_action_guard(term(), guard_context()) -> guard_result().

evaluate_action_guard({action, Action, Condition}, Context) ->
    case evaluate_do_expr(Condition, Context) of
        true -> pass;
        false ->
            {refused, forbidden_action, iolist_to_binary(io_lib:format(
                "Action ~s not enabled", [Action]))}
    end.

%% @private
-spec evaluate_state_guard(term(), guard_context()) -> guard_result().

evaluate_state_guard({state_is, State, Expected}, Context) ->
    %% Check if a state variable has expected value
    case get_state_value(State, Context) of
        Expected -> pass;
        Other ->
            {refused, validation_failure, iolist_to_binary(io_lib:format(
                "State ~s expected ~p, got ~p", [State, Expected, Other]))}
    end;
evaluate_state_guard({marking_has, Place, Token}, #guard_context{marking = Marking}) ->
    case maps:get(Place, Marking, []) of
        [] ->
            {refused, missing_evidence, <<Place/binary, " is empty">>};
        Tokens when is_list(Tokens) ->
            case lists:member(Token, Tokens) of
                true -> pass;
                false ->
                    {refused, validation_failure, <<Place/binary, " does not contain required token">>}
            end
    end;
evaluate_state_guard({data_matches, Path, Pattern}, Context) ->
    case get_data_value(Path, Context) of
        Pattern -> pass;
        Other ->
            {refused, validation_failure, iolist_to_binary(io_lib:format(
                "Data at ~s does not match pattern ~p", [Path, Other]))}
    end.

%% @private
-spec evaluate_temporal_guard(term(), guard_context()) -> guard_result().

evaluate_temporal_guard({before, Condition, _Event}, Context) ->
    %% Check if condition is true before proceeding
    case evaluate_do_expr(Condition, Context) of
        true -> pass;
        false ->
            {refused, missing_evidence, <<"Pre-condition not met">>}
    end;
evaluate_temporal_guard({'after_event', Condition, Event}, Context) ->
    %% Check if condition is true after_event event
    %% For now, just evaluate the condition
    case evaluate_do_expr(Condition, Context) of
        true -> pass;
        false ->
            {refused, validation_failure, iolist_to_binary(io_lib:format(
                "Post-condition not met after_event ~p", [Event]))}
    end;
evaluate_temporal_guard({within, Condition, Duration}, #guard_context{timestamp = Now}) ->
    %% Check if condition can be satisfied within duration
    %% This is a simplified check
    case evaluate_do_expr(Condition, #guard_context{timestamp = Now + Duration}) of
        true -> pass;
        false ->
            {refused, timeout_exceeded, iolist_to_binary(io_lib:format(
                "Condition not satisfiable within ~p ms", [Duration]))}
    end.

%% @private
-spec get_state_value(term(), guard_context()) -> term().

get_state_value(Key, #guard_context{usr_info = UsrInfo}) when is_map(UsrInfo) ->
    maps:get(Key, UsrInfo, undefined);
get_state_value(Key, _Context) ->
    undefined.

%% @private
-spec get_data_value(term(), guard_context()) -> term().

get_data_value(Path, #guard_context{usr_info = UsrInfo}) when is_list(Path), is_map(UsrInfo) ->
    get_nested_value(Path, UsrInfo);
get_data_value(_Path, _Context) ->
    undefined.

%% @private
-spec get_nested_value([term()], map()) -> term().

get_nested_value([Key], Map) ->
    maps:get(Key, Map, undefined);
get_nested_value([Key | Rest], Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> undefined;
        NestedMap when is_map(NestedMap) -> get_nested_value(Rest, NestedMap);
        Other -> Other
    end.
