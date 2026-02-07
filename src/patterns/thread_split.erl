%% -*- erlang -*-
%% @doc Thread Split Pattern (P42) for YAWL.
%%
%% Implements P42: Thread Split - split into multiple independent thread execution paths.
-module(thread_split).
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
    branches = [] :: [atom()],
    split = false :: boolean()
}).

place_lst() ->
    [p_start, p_thread1, p_thread2, p_thread3, p_thread4, p_end].

trsn_lst() ->
    [t_split, t_finish1, t_finish2, t_finish3, t_finish4].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_finish1) -> [p_thread1];
preset(t_finish2) -> [p_thread2];
preset(t_finish3) -> [p_thread3];
preset(t_finish4) -> [p_thread4];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Produce = lists:foldl(fun(Branch, Acc) ->
        maps:put(Branch, [token], Acc)
    end, #{}, State#state.branches),
    NewState = State#state{split = true},
    {produce, Produce, NewState};
fire(t_finish1, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(t_finish2, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(t_finish3, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(t_finish4, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Branches = maps:get(branches, UsrInfo, [p_thread1, p_thread2, p_thread3, p_thread4]),
    Split = maps:get(split, UsrInfo, false),
    #state{branches = Branches, split = Split};
get_state(_) ->
    #state{branches = [p_thread1, p_thread2, p_thread3, p_thread4], split = false}.

init(#{branches := Branches}) ->
    #{branches => Branches, split => false};
init(_) ->
    #{branches => [p_thread1, p_thread2, p_thread3, p_thread4], split => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
