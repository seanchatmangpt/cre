%%%-------------------------------------------------------------------
%%% @doc
%%% Test fixture gen_pnet module for receipt generation testing.
%%%
%%% This module implements a simple Petri net for testing receipt generation:
%%% - Places: tasks (input), done (output)
%%% - Transition: t1 fires when any enabled task token is present
%%% - When t1 fires, it consumes the enabled task and produces a done task
%%%
%%% == Test Flow ==
%%% 1. Start net with wf_test_net_receipt
%%% 2. Use wf_task:enabled/3 to create task token produce map
%%% 3. Inject tokens via gen_pnet:inject/2
%%% 4. Fire transition via gen_pnet:drain/2
%%% 5. Verify receipt structure
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_test_net_receipt).
-moduledoc """
Test fixture net for receipt generation testing.

## Petri Net Structure

Places:
- tasks: Input place for task tokens
- done: Output place for done task tokens

Transitions:
- t1: Fires when an enabled task token is present, consumes it and produces a done task

## Example

```erlang
> {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []).
> {produce, PM0} = wf_task:enabled(t1, #{payload => ok}, tasks).
> {ok, _} = gen_pnet:inject(Pg, PM0).
> {ok, [R]} = gen_pnet:drain(Pg, 10).
> pnet_types:is_mode(R).
true
```
""".
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    init/1,
    trigger/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type usr_info() :: #{seed => integer()}.
-type net_state() :: usr_info().
-type marking() :: #{atom() => [term()]}.
-type mode() :: #{atom() => [term()]}.

%%%===================================================================
%%% gen_pnet Callbacks - Structure
%%%===================================================================

%% @doc Returns the list of places for the receipt test net.
-spec place_lst() -> [atom(), ...].

place_lst() ->
    [tasks, done].

%% @doc Returns the list of transitions for the receipt test net.
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t1].

%% @doc Returns the initial marking for a given place.
-spec init_marking(atom(), usr_info()) -> [].

init_marking(_Place, _UsrInfo) ->
    [].

%% @doc Returns the preset (input places) for each transition.
-spec preset(atom()) -> [atom(), ...];
            (_) -> [].

preset(t1) ->
    [tasks];
preset(_Trsn) ->
    [].

%% @doc Checks if a transition is enabled with the given mode.
%% t1 is enabled when tasks contains an enabled task token.
-spec is_enabled(atom(), mode(), usr_info()) -> boolean();
                (_, _, _) -> false.

is_enabled(t1, #{tasks := TaskTokens}, _UsrInfo) ->
    lists:any(fun is_enabled_task/1, TaskTokens);
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%% @private
-spec is_enabled_task(term()) -> boolean().
is_enabled_task({task, _TaskId, enabled, _Payload}) ->
    true;
is_enabled_task(_Other) ->
    false.

%% @doc Fires a transition, consuming and producing tokens.
%% t1 consumes enabled task tokens and produces done task tokens.
-spec fire(atom(), mode(), usr_info()) -> {produce, marking()};
          (_, _, _) -> abort.

fire(t1, #{tasks := TaskTokens}, _UsrInfo) ->
    %% Find and consume the first enabled task token
    case find_enabled_task(TaskTokens) of
        {ok, {task, TaskId, enabled, Payload}} ->
            ProduceMap = #{
                tasks => [],
                done => [{task, TaskId, 'done', Payload}]
            },
            {produce, ProduceMap};
        {error, not_found} ->
            abort
    end;
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%% @private
-spec find_enabled_task([term()]) -> {ok, term()} | {error, not_found}.
find_enabled_task([]) ->
    {error, not_found};
find_enabled_task([Token = {task, _TaskId, enabled, _Payload} | _Rest]) ->
    {ok, Token};
find_enabled_task([_Other | Rest]) ->
    find_enabled_task(Rest).

%%%===================================================================
%%% gen_pnet Callbacks - Interface
%%%===================================================================

%% @doc Initializes the gen_pnet instance.
-spec init(term()) -> usr_info().

init(NetArg) when is_map(NetArg) ->
    NetArg;
init(_NetArg) ->
    #{}.

%% @doc Handles synchronous calls.
-spec handle_call(term(), {pid(), term()}, net_state()) ->
          {reply, term()} | {reply, term(), marking()}.

handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%% @doc Handles asynchronous casts.
-spec handle_cast(term(), net_state()) -> {noreply, net_state()}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%% @doc Handles info messages.
-spec handle_info(term(), net_state()) -> {noreply, net_state()}.

handle_info(_Info, NetState) ->
    {noreply, NetState}.

%% @doc Handles code changes.
-spec code_change(term(), net_state(), term()) -> {ok, net_state()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%% @doc Cleanup on termination.
-spec terminate(term(), net_state()) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%% @doc Trigger callback - all tokens pass.
-spec trigger(atom(), term(), net_state()) -> pass.

trigger(_Place, _Token, _NetState) ->
    pass.

%%%===================================================================
%%% Doctests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test place_lst
place_lst_test() ->
    ?assertEqual([tasks, done], place_lst()).

%% Test trsn_lst
trsn_lst_test() ->
    ?assertEqual([t1], trsn_lst()).

%% Test preset
preset_t1_test() ->
    ?assertEqual([tasks], preset(t1)).

%% Test init_marking
init_marking_test() ->
    ?assertEqual([], init_marking(tasks, #{})),
    ?assertEqual([], init_marking(done, #{})).

%% Test is_enabled with enabled task
is_enabled_enabled_task_test() ->
    Mode = #{tasks => [{task, t1, enabled, #{}}]},
    ?assertEqual(true, is_enabled(t1, Mode, #{})).

%% Test is_enabled with done task (should be false)
is_enabled_done_task_test() ->
    Mode = #{tasks => [{task, t1, 'done', #{}}]},
    ?assertEqual(false, is_enabled(t1, Mode, #{})).

%% Test fire with enabled task
fire_enabled_task_test() ->
    Mode = #{tasks => [{task, t1, enabled, #{payload => data}}]},
    ?assertEqual(
        {produce, #{tasks => [], done => [{task, t1, 'done', #{payload => data}}]}},
        fire(t1, Mode, #{})
    ).

%% Test fire aborts with no enabled task
fire_no_enabled_task_test() ->
    Mode = #{tasks => [{task, t1, 'done', #{}}]},
    ?assertEqual(abort, fire(t1, Mode, #{})).

-endif.
