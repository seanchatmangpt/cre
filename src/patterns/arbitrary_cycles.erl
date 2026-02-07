%% -*- erlang -*-
%% @doc Arbitrary Cycles Pattern (P10) for YAWL.
%%
%% Implements P10: Arbitrary Cycles - allows cycles with arbitrary entry/exit points.
%% This pattern enables sending tokens back to any previous node in the workflow.
-module(arbitrary_cycles).
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
    nodes :: [atom()],
    cycles :: #{atom() => [atom()]}
}).

place_lst() ->
    [p_start, p_cycle_pool, p_end].

trsn_lst() ->
    [t_start, t_cycle, t_exit].

init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_cycle) -> [p_cycle_pool];
preset(t_exit) -> [p_cycle_pool];
preset(_) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_cycle_pool => [cycle_token]}, UsrInfo};
fire(t_cycle, _Mode, UsrInfo) ->
    %% Can cycle back to any node
    {produce, #{p_cycle_pool => [cycle_token]}, UsrInfo};
fire(t_exit, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

init(_NetArg) -> #{}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
