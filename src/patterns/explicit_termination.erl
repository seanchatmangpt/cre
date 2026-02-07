%% -*- erlang -*-
%% @doc Explicit Termination Pattern (P43) for YAWL.
%%
%% Implements P43: Explicit Termination - hard-stop that cancels all activities.
-module(explicit_termination).
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
    terminator :: atom(),
    cancels_all = true :: boolean(),
    terminated = false :: boolean()
}).

place_lst() ->
    [p_start, p_active, p_terminate_event, p_terminated, p_cancelled, p_end].

trsn_lst() ->
    [t_start, t_terminate, t_cancel_all, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_terminate) -> [p_active, p_terminate_event];
preset(t_cancel_all) -> [p_terminated];
preset(t_finish) -> [p_cancelled];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_active => [active]}, UsrInfo};
fire(t_terminate, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{terminated = true},
    {produce, #{p_terminated => [terminated]}, NewState};
fire(t_cancel_all, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{cancels_all = true},
    {produce, #{p_cancelled => [cancelled]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Terminator = maps:get(terminator, UsrInfo, undefined),
    CancelsAll = maps:get(cancels_all, UsrInfo, true),
    Terminated = maps:get(terminated, UsrInfo, false),
    #state{terminator = Terminator, cancels_all = CancelsAll, terminated = Terminated};
get_state(_) ->
    #state{terminator = undefined, cancels_all = true, terminated = false}.

init(#{terminator := Terminator, cancels_all := CancelsAll}) ->
    #{terminator => Terminator, cancels_all => CancelsAll, terminated => false};
init(#{terminator := Terminator}) ->
    #{terminator => Terminator, cancels_all => true, terminated => false};
init(_) ->
    #{terminator => undefined, cancels_all => true, terminated => false}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
