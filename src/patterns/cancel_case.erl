%% -*- erlang -*-
%% @doc Cancel Case Pattern (P20) for YAWL.
%%
%% Implements P20: Cancel Case - cancel entire workflow case.
-module(cancel_case).
-behaviour(gen_yawl).

-include_lib("kernel/include/logger.hrl").

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
    log_xes_event(<<"CancelCase">>, <<"start">>, UsrInfo),
    {produce, #{p_active => [active]}, UsrInfo};
fire(t_cancel, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{cancelled = true},
    log_xes_event(<<"CancelCase">>, <<"cancel">>, UsrInfo#{cancelled => true}),
    {produce, #{p_cancelled => [cancelled]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    log_xes_event(<<"CancelCase">>, <<"complete">>, UsrInfo),
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    CancelEvent = maps:get(cancel_event, UsrInfo, undefined),
    Cancelled = maps:get(cancelled, UsrInfo, false),
    #state{cancel_event = CancelEvent, cancelled = Cancelled};
get_state(_) ->
    #state{cancel_event = undefined, cancelled = false}.

init(#{cancel_event := Event}) ->
    #{cancel_event => Event, cancelled => false};
init(_) ->
    #{cancel_event => undefined, cancelled => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
log_xes_event(PatternType, Transition, UsrInfo) ->
    case whereis(yawl_xes) of
        undefined ->
            %% XES logger not available, skip logging
            ok;
        _Pid ->
            CancelEvent = maps_get_safe(cancel_event, UsrInfo, undefined),
            EventName = <<(atom_to_binary(PatternType))/binary, "_",
                        (format_event(CancelEvent))/binary>>,
            try
                yawl_xes:log_event(
                    default_log_id(),
                    EventName,
                    atom_to_binary(Transition),
                    #{
                        <<"pattern">> => PatternType,
                        <<"cancel_event">> => format_event(CancelEvent),
                        <<"cancelled">> => maps_get_safe(cancelled, UsrInfo, false)
                    },
                    maps_get_safe(case_id, UsrInfo, undefined)
                )
            catch
                _:_ ->
                    %% Silent fail if XES logging fails
                    ok
            end
    end.

%% @private
default_log_id() ->
    <<"yawl_default_log">>.

%% @private
format_event(undefined) -> <<"undefined">>;
format_event(Event) when is_atom(Event) -> atom_to_binary(Event);
format_event(Event) when is_binary(Event) -> Event;
format_event(Event) -> list_to_binary(io_lib:format("~p", [Event])).

%% @private
maps_get_safe(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
