%% -*- erlang -*-
%% @doc Cancelling Partial Join Pattern (P32) for YAWL.
%%
%% Implements P32: Cancelling Partial Join - cancel remaining after N of M complete.
-module(cancelling_partial_join).
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
    cancel_remaining = true :: boolean(),
    completed = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_threshold_met, p_cancelled, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_threshold, t_cancel, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_threshold) -> [p_branch1, p_branch2, p_branch3];
preset(t_cancel) -> [p_threshold_met];
preset(t_finish) -> [p_cancelled];
preset(_) -> [].

is_enabled(t_threshold, Mode, UsrInfo) ->
    Completed = count_completions(Mode),
    State = get_state(UsrInfo),
    Completed >= State#state.n;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [token], p_branch2 => [token], p_branch3 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_threshold, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Completed = count_completions(Mode),
    NewState = State#state{completed = Completed},
    {produce, #{p_threshold_met => [met]}, NewState};
fire(t_cancel, _Mode, UsrInfo) ->
    {produce, #{p_cancelled => [cancelled]}, UsrInfo};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

count_completions(Mode) ->
    lists:sum([1 || K <- [p_branch1, p_branch2, p_branch3], maps:is_key(K, Mode)]).

get_state(UsrInfo) when is_map(UsrInfo) ->
    M = maps:get(m, UsrInfo, 3),
    N = maps:get(n, UsrInfo, 2),
    Cancel = maps:get(cancel_remaining, UsrInfo, true),
    #state{m = M, n = N, cancel_remaining = Cancel};
get_state(_) ->
    #state{m = 3, n = 2, cancel_remaining = true}.

init(#{m := M, n := N, cancel_remaining := Cancel}) ->
    #{m => M, n => N, cancel_remaining => Cancel, completed => 0};
init(#{m := M, n := N}) ->
    #{m => M, n => N, cancel_remaining => true, completed => 0};
init(_) ->
    #{m => 3, n => 2, cancel_remaining => true, completed => 0}.

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
    ?assert(lists:member(p_threshold_met, Places)),
    ?assert(lists:member(p_cancelled, Places)),
    ?assert(lists:member(p_end, Places)).

%% Test trsn_lst/0
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_split, Transitions)),
    ?assert(lists:member(t_complete1, Transitions)),
    ?assert(lists:member(t_complete2, Transitions)),
    ?assert(lists:member(t_complete3, Transitions)),
    ?assert(lists:member(t_threshold, Transitions)),
    ?assert(lists:member(t_cancel, Transitions)),
    ?assert(lists:member(t_finish, Transitions)).

%% Test init/1
init_with_cancel_test() ->
    State = init(#{m => 5, n => 3, cancel_remaining => true}),
    ?assertEqual(5, maps:get(m, State)),
    ?assertEqual(3, maps:get(n, State)),
    ?assertEqual(true, maps:get(cancel_remaining, State)).

init_default_cancel_test() ->
    State = init(#{m => 4, n => 2}),
    ?assertEqual(true, maps:get(cancel_remaining, State)).

-endif.
