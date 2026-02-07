%% -*- erlang -*-
%% @doc Local Sync Merge Pattern (P37) for YAWL.
%%
%% Implements P37: Local Sync Merge - join only local branches within scope.
-module(local_sync_merge).
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
    scope :: atom(),
    local_branches = [] :: [atom()],
    joined = false :: boolean()
}).

place_lst() ->
    [p_start, p_local1, p_local2, p_local3, p_join_ready, p_joined, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_join, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_local1];
preset(t_complete2) -> [p_local2];
preset(t_complete3) -> [p_local3];
preset(t_join) -> [p_local1, p_local2, p_local3];
preset(t_finish) -> [p_joined];
preset(_) -> [].

is_enabled(t_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    %% All local branches must be complete
    AllComplete = lists:all(fun(Branch) -> maps:is_key(Branch, Mode) end, State#state.local_branches),
    AllComplete;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Produce = lists:foldl(fun(Branch, Acc) ->
        maps:put(Branch, [token], Acc)
    end, #{}, State#state.local_branches),
    {produce, Produce, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_local1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_local2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_local3 => [done]}, UsrInfo};
fire(t_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{joined = true},
    {produce, #{p_joined => [joined]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Scope = maps:get(scope, UsrInfo, local),
    Branches = maps:get(local_branches, UsrInfo, [p_local1, p_local2, p_local3]),
    Joined = maps:get(joined, UsrInfo, false),
    #state{scope = Scope, local_branches = Branches, joined = Joined};
get_state(_) ->
    #state{scope = local, local_branches = [p_local1, p_local2, p_local3], joined = false}.

init(#{scope := Scope, local_branches := Branches}) ->
    #{scope => Scope, local_branches => Branches, joined => false};
init(#{scope := Scope}) ->
    #{scope => Scope, local_branches => [p_local1, p_local2, p_local3], joined => false};
init(_) ->
    #{scope => local, local_branches => [p_local1, p_local2, p_local3], joined => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
