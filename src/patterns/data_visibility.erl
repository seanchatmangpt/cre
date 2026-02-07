%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------

-module(data_visibility).
-moduledoc """
Data Visibility Pattern (WDP-05) for YAWL.

This module implements the Data Visibility pattern as a gen_yawl behaviour.

## Pattern Description

The Data Visibility pattern (WDP-05) controls data visibility and access
within the workflow. It defines which parts of the workflow can access
specific data items, implementing scope and access control rules.

## Petri Net Structure

Places:
- `p_start` - Start of visibility check
- `p_check_scope` - Checking access scope
- `p_granted` - Access granted
- `p_denied` - Access denied
- `p_end` - End of the pattern

Transitions:
- `t_check` - Check visibility/access permissions
- `t_grant` - Grant access
- `t_deny` - Deny access
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> data_visibility:place_lst().
[p_start,p_check_scope,p_granted,p_denied,p_end]
```

Get the list of transitions:

```erlang
> data_visibility:trsn_lst().
[t_check,t_grant,t_deny,t_finish]
```

Get the preset (input places) for a transition:

```erlang
> data_visibility:preset(t_check).
[p_start]
```

```erlang
> data_visibility:preset(t_grant).
[p_check_scope]
```

```erlang
> data_visibility:preset(t_deny).
[p_check_scope]
```

```erlang
> data_visibility:preset(t_finish).
[p_granted]
```

```erlang
> data_visibility:preset(unknown).
[]
```
""".
-behaviour(gen_yawl).

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3
]).

-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%% API exports
-export([
    new/3,
    start/3,
    get_state/1,
    check_visibility/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(data_visibility_state, {
    data :: term(),
    scope :: term(),
    access_check_fun :: function(),
    access_granted = false :: boolean(),
    start_time :: integer()
}).

-type data_visibility_state() :: #data_visibility_state{}.
-export_type([data_visibility_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Data Visibility pattern state.
%% @end
%%--------------------------------------------------------------------
-spec new(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
          data_visibility_state().

new(Data, Scope, AccessCheckFun) when is_function(AccessCheckFun, 2) ->
    #data_visibility_state{
        data = Data,
        scope = Scope,
        access_check_fun = AccessCheckFun,
        access_granted = false,
        start_time = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Data Visibility workflow as a gen_pnet process.
%%
%% @param Data The data whose visibility is being checked.
%% @param Scope The scope context for visibility.
%% @param AccessCheckFun Function to check access (Data, Scope) -> boolean().
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
          {ok, pid()} | {error, term()}.

start(Data, Scope, AccessCheckFun) when is_function(AccessCheckFun, 2) ->
    VisibilityState = new(Data, Scope, AccessCheckFun),
    gen_yawl:start_link(?MODULE, VisibilityState, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Data Visibility workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, data_visibility_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Checks if data is visible in the given scope.
%%
%% @param Data The data to check.
%% @param Scope The scope context.
%% @param AccessCheckFun Function to check access (Data, Scope) -> boolean().
%% @return {ok, boolean()} - true if visible, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec check_visibility(Data :: term(), Scope :: term(), AccessCheckFun :: function()) ->
          {ok, boolean()}.

check_visibility(Data, Scope, AccessCheckFun) when is_function(AccessCheckFun, 2) ->
    try
        Result = AccessCheckFun(Data, Scope),
        {ok, Result}
    catch
        Error:Reason ->
            {error, {access_check_error, Error, Reason}}
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Data Visibility Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_check_scope',
        'p_granted',
        'p_denied',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Data Visibility Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_check',
        't_grant',
        't_deny',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: data_visibility_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_check') -> ['p_start'];
preset('t_grant') -> ['p_check_scope'];
preset('t_deny') -> ['p_check_scope'];
preset('t_finish') -> ['p_granted'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: data_visibility_state()) ->
          boolean().

is_enabled('t_check', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_grant', #{'p_check_scope' := [{granted, _}]}, _UsrInfo) ->
    true;
is_enabled('t_deny', #{'p_check_scope' := [{denied, _}]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_granted' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: data_visibility_state()) ->
          {produce, map()} | abort.

fire('t_check', #{'p_start' := [start]}, #data_visibility_state{
    data = Data,
    scope = Scope,
    access_check_fun = AccessFun
}) ->
    try
        case AccessFun(Data, Scope) of
            true ->
                {produce, #{
                    'p_start' => [],
                    'p_check_scope' => [{granted, Data}]
                }};
            false ->
                {produce, #{
                    'p_start' => [],
                    'p_check_scope' => [{denied, Data}]
                }}
        end
    catch
        _:_ ->
            abort
    end;

fire('t_grant', #{'p_check_scope' := [{granted, Data}]}, _State) ->
    {produce, #{
        'p_check_scope' => [],
        'p_granted' => [Data],
        'p_denied' => []
    }};

fire('t_deny', #{'p_check_scope' := [{denied, Data}]}, _State) ->
    {produce, #{
        'p_check_scope' => [],
        'p_denied' => [Data],
        'p_granted' => []
    }};

fire('t_finish', #{'p_granted' := [Data]}, _State) ->
    {produce, #{
        'p_granted' => [],
        'p_end' => [complete, {visible, Data}]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: data_visibility_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: data_visibility_state()) ->
          {ok, data_visibility_state()}.

init(DataVisibilityState) ->
    {ok, DataVisibilityState}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) ->
          ok.

terminate(_Reason, _NetState) ->
    ok.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test basic place_lst callback
place_lst_test() ->
    Expected = [p_start, p_check_scope, p_granted, p_denied, p_end],
    ?assertEqual(Expected, place_lst()).

%% Test basic trsn_lst callback
trsn_lst_test() ->
    Expected = [t_check, t_grant, t_deny, t_finish],
    ?assertEqual(Expected, trsn_lst()).

%% Test preset for various transitions
preset_t_check_test() ->
    ?assertEqual([p_start], preset(t_check)).

preset_t_grant_test() ->
    ?assertEqual([p_check_scope], preset(t_grant)).

preset_t_deny_test() ->
    ?assertEqual([p_check_scope], preset(t_deny)).

preset_t_finish_test() ->
    ?assertEqual([p_granted], preset(t_finish)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test new/3 constructor
new_test() ->
    Fun = fun(_, _) -> true end,
    State = new(data, public, Fun),
    ?assertEqual(data, State#data_visibility_state.data),
    ?assertEqual(public, State#data_visibility_state.scope),
    ?assertEqual(Fun, State#data_visibility_state.access_check_fun),
    ?assertEqual(false, State#data_visibility_state.access_granted).

%% Test check_visibility/3
check_visibility_granted_test() ->
    Fun = fun(_, _) -> true end,
    ?assertEqual({ok, true}, check_visibility(data, public, Fun)).

check_visibility_denied_test() ->
    Fun = fun(_, _) -> false end,
    ?assertEqual({ok, false}, check_visibility(data, private, Fun)).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(data, public, fun(_, _) -> true end),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(data, public, fun(_, _) -> true end),
    ?assertEqual([], init_marking(p_check_scope, State)),
    ?assertEqual([], init_marking(p_end, State)).

-endif.
