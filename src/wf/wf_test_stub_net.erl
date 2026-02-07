%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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

-module(wf_test_stub_net).
-moduledoc """
Test fixture net module for gen_pnet testing.

This module implements the gen_pnet behavior to provide a minimal
Petri net for testing. It includes custom handle_call implementations
for testing the gen_pnet API.

## Net Structure

Places:
- `p1' - The only place in the net

Transitions:
- None (empty list)

## Custom handle_call Messages

- `ping' - Returns `pong'
- `{put, Place, Token}' - Produces Token at Place, returns `ok'

## Example

```erlang
> {ok, Pid} = gen_pnet:start_link(wf_test_stub_net, undefined, []), gen_pnet:marking(Pid).
#{p1 => [init]}
```

```erlang
> gen_pnet:call(Pid, ping).
pong
```

```erlang
> gen_pnet:call(Pid, {put, p1, foo}).
ok
```

```erlang
> gen_pnet:ls(Pid, p1).
{ok,[foo,init]}
```

```erlang
> gen_pnet:ls(Pid, missing).
{ok,[]}
```

```erlang
> Pred = fun([T]) -> case length(T) > 0 of true -> ok; false -> {error, empty} end end.
> gen_pnet:state_property(Pid, Pred, [p1]).
ok
```

```erlang
> gen_pnet:stop(Pid).
ok
```
""".

-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3,
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc User info state for the test stub net.
%%--------------------------------------------------------------------
-type usr_info() :: term().

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net.
%%--------------------------------------------------------------------
-type trsn() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A mode maps places to lists of tokens.
%%--------------------------------------------------------------------
-type mode() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%% Export types
-export_type([]).

%%====================================================================
%% gen_pnet Callbacks - Structure
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the test stub net.
%%
%% Only one place: `p1'.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [place()].

place_lst() ->
    [p1].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the test stub net.
%%
%% No transitions - this is a static net for testing API calls.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [trsn()].

trsn_lst() ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%%
%% Place `p1' gets `[init]' token initially, others get `[]'.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: place(), UsrInfo :: usr_info()) -> [token()].

init_marking(p1, _UsrInfo) ->
    [init];
init_marking(_Place, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%%
%% No transitions, so always returns empty list.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: trsn()) -> [place()].

preset(_Trsn) ->
    [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled with the given mode.
%%
%% No transitions, so always returns false.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          boolean().

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% No transitions, so always aborts.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          {produce, marking()} | abort.

fire(_Trsn, _Mode, _UsrInfo) ->
    {produce, #{}}.

%%====================================================================
%% gen_pnet Callbacks - Interface
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet instance.
%%
%% Returns the user info unchanged.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: usr_info()) -> usr_info().

init(UsrInfo) ->
    UsrInfo.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% Custom messages:
%% - `ping' -> Returns `pong'
%% - `{put, Place, Token}' -> Produces Token at Place, returns `ok'
%% - `_ -> `{error, bad_msg}'
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), term()},
                  NetState :: term()) ->
          {reply, term()} |
          {reply, term(), marking()} |
          noreply |
          {noreply, marking()} |
          {stop, term(), term()}.

handle_call(ping, _From, _NetState) ->
    {reply, pong};

handle_call({put, Place, Token}, _From, _NetState) ->
    {reply, ok, #{Place => [Token]}};

handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% Always returns noreply (no state change).
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply |
          {noreply, marking()} |
          {stop, term()}.

handle_cast(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%%
%% Always returns noreply (no state change).
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          noreply |
          {noreply, marking()} |
          {stop, term()}.

handle_info(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%%
%% Returns ok with the net state unchanged.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%%
%% Always returns ok.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%%
%% Always passes (lets all tokens through).
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: place(), Token :: token(), UsrInfo :: usr_info()) ->
          pass | {consume, [token()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Note: moduledoc doctests are disabled because they require
%% maintaining state across code blocks which doctest doesn't support.
%% The doctest examples in moduledoc are for documentation only.
%% @private
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([p1], place_lst()).

%% Test trsn_lst callback (empty)
trsn_lst_test() ->
    ?assertEqual([], trsn_lst()).

%% Test init_marking for p1
init_marking_p1_test() ->
    ?assertEqual([init], init_marking(p1, undefined)).

%% Test init_marking for unknown place
init_marking_unknown_test() ->
    ?assertEqual([], init_marking(unknown, undefined)).

%% Test preset (always empty)
preset_test() ->
    ?assertEqual([], preset(any_transition)).

%% Test is_enabled (always true)
is_enabled_test() ->
    ?assertEqual(true, is_enabled(any_t, #{p1 => []}, undefined)).

%% Test fire (always produces empty map)
fire_test() ->
    ?assertEqual({produce, #{}}, fire(any_t, #{p1 => []}, undefined)).

%% Test init returns user info
init_test() ->
    UsrInfo = some_state,
    ?assertEqual(UsrInfo, init(UsrInfo)).

%% Test handle_call ping
handle_call_ping_test() ->
    ?assertEqual({reply, pong}, handle_call(ping, self(), state)).

%% Test handle_call put
handle_call_put_test() ->
    ?assertMatch({reply, ok, #{p1 := [a]}}, handle_call({put, p1, a}, self(), state)).

%% Test handle_call unknown
handle_call_unknown_test() ->
    ?assertEqual({reply, {error, bad_msg}}, handle_call(unknown, self(), state)).

%% Test handle_cast
handle_cast_test() ->
    ?assertEqual(noreply, handle_cast(request, state)).

%% Test handle_info
handle_info_test() ->
    ?assertEqual(noreply, handle_info(info, state)).

%% Test code_change
code_change_test() ->
    ?assertEqual({ok, state}, code_change(vsn, state, extra)).

%% Test terminate
terminate_test() ->
    ?assertEqual(ok, terminate(normal, state)).

%% Test trigger
trigger_test() ->
    ?assertEqual(pass, trigger(p1, token, undefined)).

-endif.
