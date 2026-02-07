%% -*- erlang -*-
%% @doc Transient Trigger Pattern (P23) for YAWL.
%%
%% Implements P23: Transient Trigger - event only matters while specific task is enabled.
-module(transient_trigger).
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
    enabled_only_in :: atom(),
    event_received = false :: boolean()
}).

place_lst() ->
    [p_start, p_enabled, p_event, p_triggered, p_end].

trsn_lst() ->
    [t_enable, t_event, t_trigger, t_complete].

init_marking(_Place, _UsrInfo) -> [].

preset(t_enable) -> [p_start];
preset(t_event) -> [p_event];
preset(t_trigger) -> [p_enabled, p_event];
preset(t_complete) -> [p_triggered];
preset(_) -> [].

is_enabled(t_trigger, Mode, UsrInfo) ->
    maps:is_key(p_enabled, Mode) andalso maps:is_key(p_event, Mode);
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_enable, _Mode, UsrInfo) ->
    {produce, #{p_enabled => [enabled]}, UsrInfo};
fire(t_event, _Mode, UsrInfo) ->
    {produce, #{p_event => [event]}, UsrInfo};
fire(t_trigger, _Mode, UsrInfo) ->
    {produce, #{p_triggered => [triggered]}, UsrInfo};
fire(t_complete, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{enabled_only_in := Task}) ->
    #state{enabled_only_in = Task};
init(_) ->
    #state{enabled_only_in = undefined}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
