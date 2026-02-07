%% -*- erlang -*-
%% @doc Persistent Trigger Pattern (P24) for YAWL.
%%
%% Implements P24: Persistent Trigger - event persists until consumed.
-module(persistent_trigger).
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
    consumed_in :: atom(),
    event_persistent = false :: boolean()
}).

place_lst() ->
    [p_start, p_event_pool, p_consume_ready, p_consumed, p_end].

trsn_lst() ->
    [t_event_arrives, t_consume, t_complete].

init_marking(_Place, _UsrInfo) -> [].

preset(t_event_arrives) -> [p_start];
preset(t_consume) -> [p_event_pool, p_consume_ready];
preset(t_complete) -> [p_consumed];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_event_arrives, _Mode, UsrInfo) ->
    {produce, #{p_event_pool => [event]}, UsrInfo};
fire(t_consume, _Mode, UsrInfo) ->
    {produce, #{p_consumed => [consumed]}, UsrInfo};
fire(t_complete, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{consumed_in := Task}) ->
    #state{consumed_in = Task};
init(_) ->
    #state{consumed_in = undefined}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
