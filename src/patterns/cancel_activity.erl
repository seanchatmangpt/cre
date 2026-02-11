%% -*- erlang -*-
%% @doc Cancel Activity Pattern (P19) for YAWL.
%%
%% Implements P19: Cancel Activity - cancel a single activity.
-module(cancel_activity).
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
    target :: atom(),
    cancel_event :: atom(),
    cancelled = false :: boolean()
}).

place_lst() ->
    [p_start, p_active, p_cancel_event, p_cancelled, p_end].

trsn_lst() ->
    [t_start, t_cancel, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_cancel) -> [p_active, p_cancel_event];
preset(t_finish) -> [p_cancelled];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_active => [active]}, UsrInfo};
fire(t_cancel, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{cancelled = true},
    {produce, #{p_cancelled => [cancelled]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Target = maps:get(target, UsrInfo, undefined),
    CancelEvent = maps:get(cancel_event, UsrInfo, undefined),
    Cancelled = maps:get(cancelled, UsrInfo, false),
    #state{target = Target, cancel_event = CancelEvent, cancelled = Cancelled};
get_state(_) ->
    #state{target = undefined, cancel_event = undefined, cancelled = false}.

init(#{target := Target, cancel_event := Event}) ->
    #{target => Target, cancel_event => Event, cancelled => false};
init(_) ->
    #{target => undefined, cancel_event => undefined, cancelled => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
