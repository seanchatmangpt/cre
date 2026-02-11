%% -*- erlang -*-
%% @doc Cancel MI Activity Pattern (P26) for YAWL.
%%
%% Implements P26: Cancel MI Activity - cancel all instances of a multiple instance activity.
-module(cancel_mi_activity).
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
    State = get_state(UsrInfo),
    InstanceCount = maps_get_safe(instance_count, UsrInfo, 3),
    log_xes_event(<<"CancelMIActivity">>, <<"create_instances">>,
                 UsrInfo#{instance_count => InstanceCount}),
    {produce, #{p_instances => [inst1, inst2, inst3]}, UsrInfo};
fire(t_cancel, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    InstanceList = State#state.instances,
    log_xes_event(<<"CancelMIActivity">>, <<"cancel">>,
                 UsrInfo#{instances_cancelled => length(InstanceList)}),
    {produce, #{p_cancelled => [cancelled]}, UsrInfo};
fire(t_complete, _Mode, UsrInfo) ->
    log_xes_event(<<"CancelMIActivity">>, <<"complete">>, UsrInfo),
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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
get_state(UsrInfo) when is_map(UsrInfo) ->
    Task = maps:get(mi_task, UsrInfo, undefined),
    Event = maps:get(cancel_event, UsrInfo, undefined),
    Instances = maps_get_safe(instances, UsrInfo, []),
    #state{mi_task = Task, cancel_event = Event, instances = Instances};
get_state(_) ->
    #state{mi_task = undefined, cancel_event = undefined, instances = []}.

%% @private
log_xes_event(PatternType, Transition, UsrInfo) ->
    case whereis(yawl_xes) of
        undefined ->
            %% XES logger not available, skip logging
            ok;
        _Pid ->
            Task = maps_get_safe(mi_task, UsrInfo, undefined),
            InstanceCount = maps_get_safe(instance_count, UsrInfo, 0),
            InstancesCancelled = maps_get_safe(instances_cancelled, UsrInfo, 0),
            EventName = <<(atom_to_binary(PatternType))/binary, "_",
                        (atom_to_binary(Transition))/binary>>,
            try
                yawl_xes:log_event(
                    default_log_id(),
                    EventName,
                    atom_to_binary(Transition),
                    #{
                        <<"pattern">> => PatternType,
                        <<"mi_task">> => format_task(Task),
                        <<"instance_count">> => InstanceCount,
                        <<"instances_cancelled">> => InstancesCancelled
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
format_task(undefined) -> <<"undefined">>;
format_task(Task) when is_atom(Task) -> atom_to_binary(Task);
format_task(Task) when is_binary(Task) -> Task;
format_task(Task) -> list_to_binary(io_lib:format("~p", [Task])).

%% @private
maps_get_safe(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
