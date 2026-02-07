%% -*- erlang -*-
%% @doc Cancelling Discriminator Pattern (P29) for YAWL.
%%
%% Implements P29: Cancelling Discriminator - first completion wins, cancel others.
-module(cancelling_discriminator).
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
    race :: [atom()],
    cancel_rest = true :: boolean(),
    winner :: atom() | undefined
}).

place_lst() ->
    [p_start, p_race1, p_race2, p_race3, p_winner, p_cancelled, p_end].

trsn_lst() ->
    [t_start_race, t_win1, t_win2, t_win3, t_cancel_others, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start_race) -> [p_start];
preset(t_win1) -> [p_race1];
preset(t_win2) -> [p_race2];
preset(t_win3) -> [p_race3];
preset(t_cancel_others) -> [p_winner];
preset(t_finish) -> [p_cancelled];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start_race, _Mode, UsrInfo) ->
    {produce, #{p_race1 => [token], p_race2 => [token], p_race3 => [token]}, UsrInfo};
fire(t_win1, _Mode, UsrInfo) ->
    {produce, #{p_winner => [winner1]}, UsrInfo};
fire(t_win2, _Mode, UsrInfo) ->
    {produce, #{p_winner => [winner2]}, UsrInfo};
fire(t_win3, _Mode, UsrInfo) ->
    {produce, #{p_winner => [winner3]}, UsrInfo};
fire(t_cancel_others, _Mode, UsrInfo) ->
    {produce, #{p_cancelled => [cancelled]}, UsrInfo};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{race := Race, cancel_rest := Cancel}) ->
    #state{race = Race, cancel_rest = Cancel};
init(#{race := Race}) ->
    #state{race = Race, cancel_rest = true};
init(_) ->
    #state{race = [], cancel_rest = true}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
