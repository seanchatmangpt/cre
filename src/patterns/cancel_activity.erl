%% -*- erlang -*-
%% @doc Cancel Activity Pattern (P19) for YAWL.
%%
%% Implements P19: Cancel Activity - cancel a single activity.
-module(cancel_activity).
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
    log_xes_event(<<"CancelActivity">>, <<"start">>, UsrInfo),
    {produce, #{p_active => [active]}, UsrInfo};
fire(t_cancel, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{cancelled = true},
    log_xes_event(<<"CancelActivity">>, <<"cancel">>, UsrInfo#{cancelled => true}),
    {produce, #{p_cancelled => [cancelled]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    log_xes_event(<<"CancelActivity">>, <<"complete">>, UsrInfo),
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
            Target = maps_get_safe(target, UsrInfo, undefined),
            EventName = <<(atom_to_binary(PatternType))/binary, "_",
                        (format_target(Target))/binary>>,
            try
                yawl_xes:log_event(
                    default_log_id(),
                    EventName,
                    atom_to_binary(Transition),
                    #{
                        <<"pattern">> => PatternType,
                        <<"target">> => format_target(Target),
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
format_target(undefined) -> <<"undefined">>;
format_target(Target) when is_atom(Target) -> atom_to_binary(Target);
format_target(Target) when is_binary(Target) -> Target;
format_target(Target) -> list_to_binary(io_lib:format("~p", [Target])).

%% @private
maps_get_safe(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
