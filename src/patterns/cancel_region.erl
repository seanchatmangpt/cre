%% -*- erlang -*-
%% @doc Cancel Region Pattern (P25) for YAWL.
%%
%% Implements P25: Cancel Region - cancel activities within a region.
-module(cancel_region).
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
    region :: atom(),
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).

place_lst() ->
    [p_start, p_region_active, p_cancel_event, p_region_cancelled, p_end].

trsn_lst() ->
    [t_start, t_cancel_region, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_cancel_region) -> [p_region_active, p_cancel_event];
preset(t_finish) -> [p_region_cancelled];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_region_active => [active]}, UsrInfo};
fire(t_cancel_region, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{cancelled = true},
    {produce, #{p_region_cancelled => [cancelled]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Region = maps:get(region, UsrInfo, undefined),
    CancelEvent = maps:get(cancel_event, UsrInfo, undefined),
    Cancelled = maps:get(cancelled, UsrInfo, false),
    #state{region = Region, cancel_event = CancelEvent, cancelled = Cancelled};
get_state(_) ->
    #state{region = undefined, cancel_event = undefined, cancelled = false}.

init(#{region := Region, cancel_event := Event}) ->
    #{region => Region, cancel_event => Event, cancelled => false};
init(_) ->
    #{region => undefined, cancel_event => undefined, cancelled => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
