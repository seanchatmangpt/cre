%%%-------------------------------------------------------------------
%%% @doc
%%% Test-only gen_pnet module for trigger drop mechanism testing.
%%%
%%% This module tests the trigger functionality that allows tokens to be
%%% filtered during injection based on a trigger value.
%%%
%%% == Trigger Contract ==
%%% The trigger/3 callback is invoked during token injection to determine
%%% whether a token should be added to the net marking:
%%%
%%% - trigger(trash, Place, Token) -> drop
%%%     Tokens marked with "trash" trigger are rejected and not added
%%%
%%% - trigger(keep, Place, Token) -> pass
%%%     Tokens marked with "keep" trigger are accepted and added
%%%
%%% == Doctests ==
%%% ```
%%% 1> wf_test_net_trigger_drop:trigger(trash, p1, {data, 1}).
%%% drop
%%% 2> wf_test_net_trigger_drop:trigger(keep, p1, {data, 1}).
%%% pass
%%% 3> wf_test_net_trigger_drop:place_lst().
%%% [trash,keep]
%%% 4> wf_test_net_trigger_drop:trsn_lst().
%%% []
%%% 5> wf_test_net_trigger_drop:init(#{}).
%%% #{}
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_test_net_trigger_drop).
-behaviour(gen_pnet).
-include_lib("gen_pnet.hrl").

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
%%% gen_pnet Callbacks
%%%===================================================================

%% @doc Returns the list of places in this test net.
-spec place_lst() -> [atom()].

place_lst() ->
    [trash, keep].

%% @doc Returns the list of transitions (empty for this test net).
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [].

%% @doc Returns initial marking (empty for this test net).
-spec init_marking(atom(), _) -> [_].

init_marking(_Place, _UsrInfo) ->
    [].

%% @doc Returns preset for transitions (empty - no transitions).
-spec preset(atom()) -> [atom()].

preset(_Trsn) ->
    [].

%% @doc No transitions, so never enabled.
-spec is_enabled(atom(), #{atom() => [_]}, _) -> boolean().

is_enabled(_Trsn, _Marking, _UsrInfo) ->
    false.

%% @doc Never fires (no transitions).
-spec fire(atom(), #{atom() => [_]}, _) ->
          abort | {produce, #{atom() => [_]}}.

fire(_Trsn, _Marking, _UsrInfo) ->
    abort.

%% @doc Initialize the net state (empty map).
-spec init(_) -> _.

init(_NetArg) ->
    #{}.

%% @doc Trigger callback for filtering tokens during injection.
%%
%% This is the core functionality being tested - tokens can be filtered
%% based on their place name before being added to the net marking.
%%
%% == Place Names ==
%% - `trash`: Tokens destined for 'trash' place are dropped
%% - `keep`: Tokens destined for 'keep' place are passed through
%% - Any other place: Treated as "keep" (default pass-through)
%%
-spec trigger(atom(), _, #net_state{}) -> pass | drop.

trigger(trash, _Token, _NetState) ->
    drop;
trigger(keep, _Token, _NetState) ->
    pass;
trigger(_OtherPlace, _Token, _NetState) ->
    %% Default behavior: pass through to other places
    pass.

%% @doc Handle synchronous calls (basic implementation).
-spec handle_call(_, {pid(), _}, #net_state{}) ->
          {reply, _, #net_state{}}.

handle_call(Request, _From, NetState) ->
    {reply, {unknown_request, Request}, NetState}.

%% @doc Handle asynchronous casts.
-spec handle_cast(_, #net_state{}) ->
          noreply |
          {noreply, #{atom() => [_]}} |
          {stop, _}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%% @doc Handle unexpected messages.
-spec handle_info(_, #net_state{}) ->
          noreply |
          {noreply, #{atom() => [_]}} |
          {stop, _}.

handle_info(_Info, NetState) ->
    {noreply, NetState}.

%% @doc Hot code reload.
-spec code_change(_, #net_state{}, _) ->
          {ok, #net_state{}} | {error, _}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%% @doc Cleanup on termination.
-spec terminate(_, #net_state{}) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Run doctests for this module.
%%
%% This function can be used to verify the trigger drop behavior
%% through automated testing.
%%
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test trigger drop behavior
    %% Note: trigger/3 takes (Place, Token, NetState)
    %% But our test uses Place as the trigger value
    drop = wf_test_net_trigger_drop:trigger(trash, {data, 1}, #{}),
    pass = wf_test_net_trigger_drop:trigger(keep, {data, 1}, #{}),
    pass = wf_test_net_trigger_drop:trigger(unknown, {data, 1}, #{}),

    %% Test structure callbacks
    [trash, keep] = wf_test_net_trigger_drop:place_lst(),
    [] = wf_test_net_trigger_drop:trsn_lst(),
    [] = wf_test_net_trigger_drop:preset(t1),
    false = wf_test_net_trigger_drop:is_enabled(t1, #{}, #{}),
    abort = wf_test_net_trigger_drop:fire(t1, #{}, #{}),
    #{} = wf_test_net_trigger_drop:init(#{}),

    ok.
