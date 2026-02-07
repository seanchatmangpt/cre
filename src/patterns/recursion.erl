%% -*- erlang -*-
%% @doc Recursion Pattern (P22) for YAWL.
%%
%% Implements P22: Recursion - call same workflow recursively.
-module(recursion).
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
    call :: atom(),
    depth = 0 :: non_neg_integer(),
    max_depth = 10 :: pos_integer()
}).

place_lst() ->
    [p_start, p_call_ready, p_recursing, p_returned, p_end].

trsn_lst() ->
    [t_start, t_call, t_return, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_call) -> [p_call_ready];
preset(t_return) -> [p_recursing];
preset(t_finish) -> [p_returned];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_call_ready => [ready]}, UsrInfo};
fire(t_call, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewDepth = State#state.depth + 1,
    NewState = State#state{depth = NewDepth},
    {produce, #{p_recursing => [recursing]}, NewState};
fire(t_return, _Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    NewState = State#state{depth = max(0, State#state.depth - 1)},
    {produce, #{p_returned => [returned]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

get_state(UsrInfo) when is_map(UsrInfo) ->
    Call = maps:get(call, UsrInfo, undefined),
    Depth = maps:get(depth, UsrInfo, 0),
    MaxDepth = maps:get(max_depth, UsrInfo, 10),
    #state{call = Call, depth = Depth, max_depth = MaxDepth};
get_state(_) ->
    #state{call = undefined, depth = 0, max_depth = 10}.

init(#{call := Call}) ->
    #{call => Call, depth => 0, max_depth => 10};
init(_) ->
    #{call => undefined, depth => 0, max_depth => 10}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
