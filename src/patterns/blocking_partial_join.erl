%% -*- erlang -*-
%% @doc Blocking Partial Join Pattern (P31) for YAWL.
%%
%% Implements P31: Blocking Partial Join - partial output after N, final output after all M.
-module(blocking_partial_join).
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
    m :: pos_integer(),
    n :: pos_integer(),
    partial_out :: atom(),
    final_out :: atom(),
    completed = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_partial_out, p_final_out, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_partial, t_final, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_partial) -> [p_branch1, p_branch2, p_branch3];
preset(t_final) -> [p_branch1, p_branch2, p_branch3];
preset(t_finish) -> [p_final_out];
preset(_) -> [].

is_enabled(t_partial, Mode, UsrInfo) ->
    Completed = count_completions(Mode),
    State = get_state(UsrInfo),
    Completed >= State#state.n andalso Completed < State#state.m;
is_enabled(t_final, Mode, UsrInfo) ->
    Completed = count_completions(Mode),
    State = get_state(UsrInfo),
    Completed >= State#state.m;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [token], p_branch2 => [token], p_branch3 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_partial, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Completed = count_completions(Mode),
    NewState = State#state{completed = Completed},
    {produce, #{p_partial_out => [partial]}, NewState};
fire(t_final, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Completed = count_completions(Mode),
    NewState = State#state{completed = Completed},
    {produce, #{p_final_out => [final]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

count_completions(Mode) ->
    lists:sum([1 || K <- [p_branch1, p_branch2, p_branch3], maps:is_key(K, Mode)]).

get_state(UsrInfo) when is_map(UsrInfo) ->
    M = maps:get(m, UsrInfo, 3),
    N = maps:get(n, UsrInfo, 2),
    Partial = maps:get(partial_out, UsrInfo, p_partial_out),
    Final = maps:get(final_out, UsrInfo, p_final_out),
    #state{m = M, n = N, partial_out = Partial, final_out = Final};
get_state(_) ->
    #state{m = 3, n = 2, partial_out = p_partial_out, final_out = p_final_out}.

init(#{m := M, n := N, partial_out := Partial, final_out := Final}) ->
    #{m => M, n => N, partial_out => Partial, final_out => Final, completed => 0};
init(_) ->
    #{m => 3, n => 2, partial_out => p_partial_out, final_out => p_final_out, completed => 0}.

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
    ?assert(lists:member(p_branch1, Places)),
    ?assert(lists:member(p_branch2, Places)),
    ?assert(lists:member(p_branch3, Places)),
    ?assert(lists:member(p_partial_out, Places)),
    ?assert(lists:member(p_final_out, Places)),
    ?assert(lists:member(p_end, Places)).

%% Test trsn_lst/0
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_split, Transitions)),
    ?assert(lists:member(t_complete1, Transitions)),
    ?assert(lists:member(t_complete2, Transitions)),
    ?assert(lists:member(t_complete3, Transitions)),
    ?assert(lists:member(t_partial, Transitions)),
    ?assert(lists:member(t_final, Transitions)),
    ?assert(lists:member(t_finish, Transitions)).

%% Test count_completions/1
count_completions_test() ->
    Mode = #{p_branch1 => [token], p_branch2 => [], p_branch3 => [token]},
    ?assertEqual(2, count_completions(Mode)).

count_completions_empty_test() ->
    Mode = #{},
    ?assertEqual(0, count_completions(Mode)).

%% Test init/1
init_custom_test() ->
    State = init(#{m => 5, n => 2,
                  partial_out => out1, final_out => out2}),
    ?assertEqual(5, maps:get(m, State)),
    ?assertEqual(2, maps:get(n, State)),
    ?assertEqual(out1, maps:get(partial_out, State)),
    ?assertEqual(out2, maps:get(final_out, State)).

init_default_test() ->
    State = init(#{}),
    ?assertEqual(3, maps:get(m, State)),
    ?assertEqual(2, maps:get(n, State)),
    ?assertEqual(p_partial_out, maps:get(partial_out, State)),
    ?assertEqual(p_final_out, maps:get(final_out, State)).

-endif.
