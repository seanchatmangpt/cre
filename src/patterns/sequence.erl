%% -*- erlang -*-
%% @doc Sequence Pattern (P1) for YAWL.
%%
%% Implements P1: Sequence - sequential execution of tasks.
-module(sequence).
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
    tasks = [] :: [atom()],
    current = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_task1, p_task2, p_end].

trsn_lst() ->
    [t_start, t_complete1, t_complete2, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_complete1) -> [p_task1];
preset(t_complete2) -> [p_task2];
preset(t_finish) -> [p_task2];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_task1 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_task2 => [token]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_task2 => [done]}, UsrInfo};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{from := From, to := To}) ->
    #{tasks => [From, To], current => 0};
init(_) ->
    #{tasks => [], current => 0}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
