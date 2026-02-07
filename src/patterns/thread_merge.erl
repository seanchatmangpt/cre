%% -*- erlang -*-
%% @doc Thread Merge Pattern (P41) for YAWL.
%%
%% Implements P41: Thread Merge - merge multiple thread execution paths.
-module(thread_merge).
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
    threads = [] :: [atom()],
    merged = false :: boolean()
}).

place_lst() ->
    [p_start, p_thread1, p_thread2, p_thread3, p_thread4, p_merged, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_complete4, t_merge, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_thread1];
preset(t_complete2) -> [p_thread2];
preset(t_complete3) -> [p_thread3];
preset(t_complete4) -> [p_thread4];
preset(t_merge) -> [p_thread1, p_thread2, p_thread3, p_thread4];
preset(t_finish) -> [p_merged];
preset(_) -> [].

is_enabled(t_merge, Mode, _UsrInfo) ->
    %% All threads must complete
    maps:is_key(p_thread1, Mode) andalso
    maps:is_key(p_thread2, Mode) andalso
    maps:is_key(p_thread3, Mode) andalso
    maps:is_key(p_thread4, Mode);
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    {produce, #{
        p_thread1 => [token],
        p_thread2 => [token],
        p_thread3 => [token],
        p_thread4 => [token]
    }, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_thread1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_thread2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_thread3 => [done]}, UsrInfo};
fire(t_complete4, _Mode, UsrInfo) ->
    {produce, #{p_thread4 => [done]}, UsrInfo};
fire(t_merge, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{merged = true},
    {produce, #{p_merged => [merged]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Threads = maps:get(threads, UsrInfo, [p_thread1, p_thread2, p_thread3, p_thread4]),
    Merged = maps:get(merged, UsrInfo, false),
    #state{threads = Threads, merged = Merged};
get_state(_) ->
    #state{threads = [p_thread1, p_thread2, p_thread3, p_thread4], merged = false}.

init(#{threads := Threads}) ->
    #{threads => Threads, merged => false};
init(_) ->
    #{threads => [p_thread1, p_thread2, p_thread3, p_thread4], merged => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
