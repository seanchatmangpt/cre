%% -*- erlang -*-
%% @doc Blocking Discriminator Pattern (P28) for YAWL.
%%
%% Implements P28: Blocking Discriminator - first completion triggers, blocks others until cleared.
-module(blocking_discriminator).
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
    trigger :: atom(),
    blocks_until :: [atom()],
    triggered = false :: boolean(),
    blocked = [] :: [atom()]
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_triggered, p_blocked, p_cleared, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_trigger, t_clear, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_trigger) -> [p_branch1, p_branch2, p_branch3];
preset(t_clear) -> [p_triggered];
preset(t_finish) -> [p_cleared];
preset(_) -> [].

is_enabled(t_trigger, Mode, _UsrInfo) ->
    %% First completion triggers
    (maps:is_key(p_branch1, Mode) orelse
     maps:is_key(p_branch2, Mode) orelse
     maps:is_key(p_branch3, Mode));
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [token], p_branch2 => [token], p_branch3 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_trigger, _Mode, UsrInfo) ->
    {produce, #{p_triggered => [triggered], p_blocked => [blocked]}, UsrInfo};
fire(t_clear, _Mode, UsrInfo) ->
    {produce, #{p_cleared => [cleared]}, UsrInfo};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{trigger := T, blocks_until := Blocks}) ->
    #state{trigger = T, blocks_until = Blocks};
init(_) ->
    #state{trigger = undefined, blocks_until = []}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
