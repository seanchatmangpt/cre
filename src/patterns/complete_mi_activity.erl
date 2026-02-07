%% -*- erlang -*-
%% @doc Complete MI Activity Pattern (P27) for YAWL.
%%
%% Implements P27: Complete MI Activity - complete all instances early when condition met.
-module(complete_mi_activity).
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
    complete_condition :: binary(),
    instances = [] :: [term()],
    completed = false :: boolean()
}).

place_lst() ->
    [p_start, p_instances, p_condition_met, p_completed, p_end].

trsn_lst() ->
    [t_create_instances, t_check_condition, t_complete, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_create_instances) -> [p_start];
preset(t_check_condition) -> [p_instances];
preset(t_complete) -> [p_instances, p_condition_met];
preset(t_finish) -> [p_completed];
preset(_) -> [].

is_enabled(t_complete, Mode, UsrInfo) ->
    maps:is_key(p_instances, Mode) andalso maps:is_key(p_condition_met, Mode);
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_create_instances, _Mode, UsrInfo) ->
    {produce, #{p_instances => [inst1, inst2, inst3]}, UsrInfo};
fire(t_check_condition, _Mode, UsrInfo) ->
    %% Condition met - produce condition token
    {produce, #{p_condition_met => [met]}, UsrInfo};
fire(t_complete, _Mode, UsrInfo) ->
    {produce, #{p_completed => [completed]}, UsrInfo};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(#{mi_task := Task, complete_condition := Cond}) ->
    #state{mi_task = Task, complete_condition = Cond};
init(_) ->
    #state{mi_task = undefined, complete_condition = <<>>}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
