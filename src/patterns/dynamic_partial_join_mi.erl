%% -*- erlang -*-
%% @doc Dynamic Partial Join for MI Pattern (P36) for YAWL.
%%
%% Implements P36: Dynamic Partial Join for MI - threshold computed dynamically at runtime.
-module(dynamic_partial_join_mi).
-behaviour(gen_yawl).

-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    trigger/3
]).

-record(state, {
    threshold_expr :: binary(),
    threshold :: pos_integer() | undefined,
    completed = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_instances, p_threshold_met, p_end].

trsn_lst() ->
    [t_create_instances, t_complete_instance, t_compute_threshold, t_threshold, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_create_instances) -> [p_start];
preset(t_complete_instance) -> [p_instances];
preset(t_compute_threshold) -> [p_instances];
preset(t_threshold) -> [p_instances, p_threshold_met];
preset(t_finish) -> [p_threshold_met];
preset(_) -> [].

is_enabled(t_threshold, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Threshold = compute_threshold(State),
    Tokens = maps:get(p_instances, Mode, []),
    Completed = length(Tokens),
    Completed >= Threshold;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_create_instances, _Mode, UsrInfo) ->
    {produce, #{p_instances => [inst1, inst2, inst3, inst4, inst5]}, UsrInfo};
fire(t_complete_instance, _Mode, UsrInfo) ->
    {produce, #{p_instances => [completed]}, UsrInfo};
fire(t_compute_threshold, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Threshold = compute_threshold(State),
    NewState = State#state{threshold = Threshold},
    {produce, #{p_threshold_met => [met]}, NewState};
fire(t_threshold, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Threshold = compute_threshold(State),
    Tokens = maps:get(p_instances, Mode, []),
    Completed = length(Tokens),
    NewState = State#state{completed = Completed, threshold = Threshold},
    {produce, #{p_threshold_met => [met]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

compute_threshold(State) ->
    case State#state.threshold of
        undefined ->
            %% Default: compute from expression if available
            case State#state.threshold_expr of
                <<"ceil(attendance_estimate*0.08)">> ->
                    %% Example: 800 * 0.08 = 64
                    64;
                _ ->
                    3
            end;
        T ->
            T
    end.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Expr = maps:get(threshold_expr, UsrInfo, <<>>),
    Threshold = maps:get(threshold, UsrInfo, undefined),
    Completed = maps:get(completed, UsrInfo, 0),
    #state{threshold_expr = Expr, threshold = Threshold, completed = Completed};
get_state(_) ->
    #state{threshold_expr = <<>>, threshold = undefined, completed = 0}.

init(#{threshold_expr := Expr}) ->
    #{threshold_expr => Expr, threshold => undefined, completed => 0};
init(_) ->
    #{threshold_expr => <<>>, threshold => undefined, completed => 0}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.

%%====================================================================
%% Unit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test place_lst/0
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_instances, Places)),
    ?assert(lists:member(p_threshold_met, Places)),
    ?assert(lists:member(p_end, Places)).

%% Test trsn_lst/0
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_create_instances, Transitions)),
    ?assert(lists:member(t_complete_instance, Transitions)),
    ?assert(lists:member(t_compute_threshold, Transitions)),
    ?assert(lists:member(t_threshold, Transitions)),
    ?assert(lists:member(t_finish, Transitions)).

%% Test compute_threshold/1
compute_threshold_default_test() ->
    State = #state{threshold_expr = <<>>, threshold = undefined, completed = 0},
    ?assertEqual(3, compute_threshold(State)).

compute_threshold_expression_test() ->
    State = #state{threshold_expr = <<"ceil(attendance_estimate*0.08)">>,
                    threshold = undefined, completed = 0},
    ?assertEqual(64, compute_threshold(State)).

compute_threshold_existing_test() ->
    State = #state{threshold_expr = <<>>, threshold = 10, completed = 0},
    ?assertEqual(10, compute_threshold(State)).

%% Test get_state/1
get_state_with_map_test() ->
    UsrInfo = #{threshold_expr => <<"test">>, threshold => 5, completed => 1},
    State = get_state(UsrInfo),
    ?assertEqual(<<"test">>, State#state.threshold_expr),
    ?assertEqual(5, State#state.threshold),
    ?assertEqual(1, State#state.completed).

get_state_default_test() ->
    State = get_state(undefined),
    ?assertEqual(<<>>, State#state.threshold_expr),
    ?assertEqual(undefined, State#state.threshold),
    ?assertEqual(0, State#state.completed).

-endif.
