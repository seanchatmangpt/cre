%% -*- erlang -*-
%% @doc Static Partial Join for MI Pattern (P34) for YAWL.
%%
%% Implements P34: Static Partial Join for MI - proceed after N of M instances complete (fixed pool).
-module(static_partial_join_mi).
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
    total_instances :: pos_integer(),
    threshold :: pos_integer(),
    completed = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_instances, p_threshold_met, p_end].

trsn_lst() ->
    [t_create_instances, t_complete_instance, t_threshold, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_create_instances) -> [p_start];
preset(t_complete_instance) -> [p_instances];
preset(t_threshold) -> [p_instances];
preset(t_finish) -> [p_threshold_met];
preset(_) -> [].

is_enabled(t_threshold, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Tokens = maps:get(p_instances, Mode, []),
    Completed = length(Tokens),
    Completed >= State#state.threshold;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_create_instances, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Instances = lists:seq(1, State#state.total_instances),
    {produce, #{p_instances => Instances}, UsrInfo};
fire(t_complete_instance, _Mode, UsrInfo) ->
    {produce, #{p_instances => [completed]}, UsrInfo};
fire(t_threshold, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Tokens = maps:get(p_instances, Mode, []),
    Completed = length(Tokens),
    NewState = State#state{completed = Completed},
    {produce, #{p_threshold_met => [met]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Total = maps:get(total_instances, UsrInfo, 5),
    Threshold = maps:get(threshold, UsrInfo, 3),
    Completed = maps:get(completed, UsrInfo, 0),
    #state{total_instances = Total, threshold = Threshold, completed = Completed};
get_state(_) ->
    #state{total_instances = 5, threshold = 3, completed = 0}.

init(#{total_instances := Total, threshold := Threshold}) ->
    #{total_instances => Total, threshold => Threshold, completed => 0};
init(_) ->
    #{total_instances => 5, threshold => 3, completed => 0}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
