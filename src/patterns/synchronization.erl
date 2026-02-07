%% -*- erlang -*-
%% @doc Synchronization Pattern (P3) for YAWL.
%%
%% Implements P3: Synchronization - AND-join waiting for all branches.
-module(synchronization).
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
    waits_for = [] :: [atom()],
    joined = false :: boolean()
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_joined, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_join, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_join) -> [p_branch1, p_branch2, p_branch3];
preset(t_finish) -> [p_joined];
preset(_) -> [].

is_enabled(t_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    lists:all(fun(Branch) -> maps:is_key(Branch, Mode) end, State#state.waits_for);
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Produce = lists:foldl(fun(Branch, Acc) ->
        maps:put(Branch, [token], Acc)
    end, #{}, State#state.waits_for),
    {produce, Produce, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_join, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{joined = true},
    {produce, #{p_joined => [joined]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    WaitsFor = maps:get(waits_for, UsrInfo, [p_branch1, p_branch2, p_branch3]),
    Joined = maps:get(joined, UsrInfo, false),
    #state{waits_for = WaitsFor, joined = Joined};
get_state(_) ->
    #state{waits_for = [p_branch1, p_branch2, p_branch3], joined = false}.

init(#{waits_for := WaitsFor}) ->
    #{waits_for => WaitsFor, joined => false};
init(_) ->
    #{waits_for => [p_branch1, p_branch2, p_branch3], joined => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
