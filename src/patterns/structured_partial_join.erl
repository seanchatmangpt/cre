%% -*- erlang -*-
%% @doc Structured Partial Join Pattern (P30) for YAWL.
%%
%% Implements P30: Structured Partial Join (N-out-of-M) - proceed after N of M branches complete.
-module(structured_partial_join).
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
    m :: pos_integer(),
    n :: pos_integer(),
    completed = 0 :: non_neg_integer()
}).

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_partial_ready, p_end].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_partial_join, t_finish].

init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_partial_join) -> [p_branch1, p_branch2, p_branch3];
preset(t_finish) -> [p_partial_ready];
preset(_) -> [].

is_enabled(t_partial_join, Mode, UsrInfo) ->
    %% Need at least N completions
    Completed = count_completions(Mode),
    State = get_state(UsrInfo),
    Completed >= State#state.n;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_split, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [token], p_branch2 => [token], p_branch3 => [token]}, UsrInfo};
fire(t_complete1, _Mode, UsrInfo) ->
    {produce, #{p_branch1 => [done]}, UsrInfo};
fire(t_complete2, _Mode, UsrInfo) ->
    {produce, #{p_branch2 => [done]}, UsrInfo};
fire(t_complete3, _Mode, UsrInfo) ->
    {produce, #{p_branch3 => [done]}, UsrInfo};
fire(t_partial_join, Mode, UsrInfo) ->
    State = get_state(UsrInfo),
    Completed = count_completions(Mode),
    NewState = State#state{completed = Completed},
    {produce, #{p_partial_ready => [ready]}, NewState};
fire(t_finish, _Mode, UsrInfo) ->
    {produce, #{p_end => [done]}, UsrInfo};
fire(_Trsn, _Mode, UsrInfo) ->
    abort.

count_completions(Mode) ->
    lists:sum([1 || K <- [p_branch1, p_branch2, p_branch3], maps:is_key(K, Mode)]).

get_state(UsrInfo) when is_map(UsrInfo) ->
    M = maps:get(m, UsrInfo, 3),
    N = maps:get(n, UsrInfo, 2),
    #state{m = M, n = N};
get_state(_) ->
    #state{m = 3, n = 2}.

init(#{m := M, n := N}) ->
    #{m => M, n => N, completed => 0};
init(_) ->
    #{m => 3, n => 2, completed => 0}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _NetState) -> pass.
