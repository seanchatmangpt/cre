%% -*- erlang -*-
%% @doc Cancel MI Activity Pattern (P26) for YAWL.
%%
%% Implements P26: Cancel MI Activity - cancel all instances of a multiple instance activity.
-module(cancel_mi_activity).
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
    mi_task :: atom(),
    cancel_event :: atom(),
    instances = [] :: [term()]
}).

place_lst() ->
    [p_start, p_instances, p_cancel_event, p_cancelled, p_end].

trsn_lst() ->
    [t_create_instances, t_cancel, t_complete].

init_marking(_Place, _UsrInfo) -> [].

preset(t_create_instances) -> [p_start];
preset(t_cancel) -> [p_instances, p_cancel_event];
preset(t_complete) -> [p_cancelled];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_create_instances, _Mode, UsrInfo) ->
    {produce, #{p_instances => [inst1, inst2, inst3]}, UsrInfo};
fire(t_cancel, _Mode, UsrInfo) ->
    {produce, #{p_cancelled => [cancelled]}, UsrInfo};
fire(t_complete, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{mi_task := Task, cancel_event := Event}) ->
    #state{mi_task = Task, cancel_event = Event};
init(_) ->
    #state{mi_task = undefined, cancel_event = undefined}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
