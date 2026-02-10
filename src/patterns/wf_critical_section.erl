%% -*- erlang -*-
%% @doc Critical Section Pattern (WCP-26) - Workflow variant.
%%
%% Implements WCP-26: Critical Section - mutual exclusion for shared resources.
%% Only one process can execute within the critical section at a time.
-module(wf_critical_section).
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
    lock_id :: term(),
    lock_held = false :: boolean(),
    data :: term()
}).

-spec place_lst() -> [atom()].
place_lst() ->
    [
        p_start,
        p_request,
        p_acquired,
        p_critical,
        p_release,
        p_end
    ].

-spec trsn_lst() -> [atom()].
trsn_lst() ->
    [
        t_request,
        t_acquire,
        t_enter,
        t_exit,
        t_release,
        t_complete
    ].

-spec init_marking(atom(), term()) -> [term()].
init_marking(p_start, _UsrInfo) -> [start];
init_marking(_, _UsrInfo) -> [].

-spec preset(atom()) -> [atom()].
preset(t_request) -> [p_start];
preset(t_acquire) -> [p_request];
preset(t_enter) -> [p_acquired];
preset(t_exit) -> [p_critical];
preset(t_release) -> [p_release];
preset(t_complete) -> [p_release];
preset(_) -> [].

-spec is_enabled(atom(), map(), term()) -> boolean().
is_enabled(t_request, _Mode, _UsrInfo) -> true;
is_enabled(t_acquire, #{p_request := [_]}, #state{lock_held = false}) -> true;
is_enabled(t_enter, #{p_acquired := [acquired]}, _UsrInfo) -> true;
is_enabled(t_exit, #{p_critical := [_]}, _UsrInfo) -> true;
is_enabled(t_release, #{p_release := [released]}, _UsrInfo) -> true;
is_enabled(t_complete, #{p_release := [_]}, _UsrInfo) -> true;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

-spec fire(atom(), map(), term()) -> {produce, map()} | {produce, map(), term()} | abort.
fire(t_request, #{p_start := [start]}, State) ->
    {produce, #{p_start => [], p_request => [request]}, State};

fire(t_acquire, #{p_request := [request]}, State) ->
    {produce, #{p_request => [], p_acquired => [acquired]}, State#state{lock_held = true}};

fire(t_enter, #{p_acquired := [acquired]}, State) ->
    {produce, #{p_acquired => [], p_critical => [active]}, State};

fire(t_exit, #{p_critical := [active]}, State) ->
    {produce, #{p_critical => [], p_release => [released]}, State};

fire(t_release, #{p_release := [released]}, State) ->
    {produce, #{p_release => []}, State#state{lock_held = false}};

fire(t_complete, #{p_release := [_]}, State) ->
    {produce, #{p_release => [], p_end => [done]}, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

-spec init(term()) -> {ok, term()}.
init(UsrInfo) when is_map(UsrInfo) ->
    LockId = maps:get(lock_id, UsrInfo, make_ref()),
    {ok, #state{lock_id = LockId}};
init(UsrInfo) ->
    {ok, #state{lock_id = make_ref(), data = UsrInfo}}.

-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec handle_call(term(), {pid(), term()}, term()) -> {reply, term(), term()}.
handle_call(_Request, _From, State) -> {reply, ok, State}.

-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(_Request, State) -> {noreply, State}.

-spec handle_info(term(), term()) -> {noreply, term()}.
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) -> ok.

-spec trigger(atom(), term(), term()) -> pass.
trigger(_Place, _Token, _NetState) -> pass.
