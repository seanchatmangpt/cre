%% -*- erlang -*-
%% @doc Structured Synchronizing Merge Pattern (P7) for YAWL.
%%
%% Implements P7: Structured Synchronizing Merge - join only activated branches.
-module(structured_sync_merge).
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
    froms = [] :: [atom()],
    join :: atom(),
    activated = [] :: [atom()]
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_branch4, p_join_ready, p_joined, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_complete4, t_join, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_complete4) -> [p_branch4];
preset(t_join) -> [p_branch1, p_branch2, p_branch3, p_branch4];
preset(t_finish) -> [p_joined];
preset(_) -> [].

is_enabled(t_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    %% Join only activated branches
    Activated = get_activated_branches(Mode, State),
    AllActivatedComplete = lists:all(fun(Branch) -> maps:is_key(Branch, Mode) end, Activated),
    AllActivatedComplete andalso length(Activated) > 0;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Produce = lists:foldl(fun(Branch, Acc) ->
        maps:put(Branch, [token], Acc)
    end, #{}, State#state.froms),
    {produce, Produce, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_complete4, _Mode, UsrInfo) ->
    {produce, #{p_branch4 => [done]}, UsrInfo};
fire(t_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Activated = get_activated_branches(Mode, State),
    NewState = State#state{activated = Activated},
    {produce, #{p_joined => [joined]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_activated_branches(Mode, State) ->
    %% Determine which branches were actually activated
    [B || B <- State#state.froms, maps:is_key(B, Mode)].

get_state(UsrInfo) when is_map(UsrInfo) ->
    Froms = maps:get(froms, UsrInfo, [p_branch1, p_branch2, p_branch3, p_branch4]),
    Join = maps:get(join, UsrInfo, p_join_ready),
    Activated = maps:get(activated, UsrInfo, []),
    #state{froms = Froms, join = Join, activated = Activated};
get_state(_) ->
    #state{froms = [p_branch1, p_branch2, p_branch3, p_branch4], join = p_join_ready, activated = []}.

init(#{froms := Froms, join := Join}) ->
    #{froms => Froms, join => Join, activated => []};
init(_) ->
    #{froms => [p_branch1, p_branch2, p_branch3, p_branch4], join => p_join_ready, activated => []}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
