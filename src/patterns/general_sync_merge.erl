%% -*- erlang -*-
%% @doc General Sync Merge Pattern (P38) for YAWL.
%%
%% Implements P38: General Sync Merge - merge across unstructured active branches.
-module(general_sync_merge).
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
    active_branches = [] :: [atom()],
    joined = false :: boolean()
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_join_ready, p_joined, p_end].

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
    %% Join all active branches (determined dynamically)
    Active = get_active_branches(Mode, State),
    AllActiveComplete = lists:all(fun(Branch) -> maps:is_key(Branch, Mode) end, Active),
    AllActiveComplete andalso length(Active) > 0;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Produce = lists:foldl(fun(Branch, Acc) ->
        maps:put(Branch, [token], Acc)
    end, #{}, State#state.active_branches),
    {produce, Produce, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Active = get_active_branches(Mode, State),
    NewState = State#state{joined = true, active_branches = Active},
    {produce, #{p_joined => [joined]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_active_branches(Mode, State) ->
    %% Determine which branches are actually active based on marking
    AllBranches = [p_branch1, p_branch2, p_branch3],
    [B || B <- AllBranches, maps:is_key(B, Mode) orelse lists:member(B, State#state.active_branches)].

get_state(UsrInfo) when is_map(UsrInfo) ->
    Active = maps:get(active_branches, UsrInfo, [p_branch1, p_branch2, p_branch3]),
    Joined = maps:get(joined, UsrInfo, false),
    #state{active_branches = Active, joined = Joined};
get_state(_) ->
    #state{active_branches = [p_branch1, p_branch2, p_branch3], joined = false}.

init(#{active_branches := Active}) ->
    #{active_branches => Active, joined => false};
init(_) ->
    #{active_branches => [p_branch1, p_branch2, p_branch3], joined => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
