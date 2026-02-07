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

-module(wf_test_net_basic).
-moduledoc """
Basic test fixture net module for pnet_mode testing.

This module implements a minimal gen_pnet-like interface for testing
mode enumeration functions. It provides a simple Petri net with
one place and one transition for unit testing.

## Petri Net Structure

Places:
- `p` - The only place in the net

Transitions:
- `t1` - A transition that consumes from place `p`

## Example

```erlang
> wf_test_net_basic:place_lst().
[p]

> wf_test_net_basic:trsn_lst().
[t1]

> wf_test_net_basic:preset(t1).
[p]

> wf_test_net_basic:init_marking(p, undefined).
[]
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet-like structure callbacks for testing
-export([
    init/1,
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    trigger/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc User info state for the basic test net.
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
%% gen_pnet-like Structure Callbacks (for testing)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the basic test net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [place()].

place_lst() ->
    [p].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the basic test net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [trsn()].

trsn_lst() ->
    [t1].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: place(), UsrInfo :: usr_info()) -> [token()].

init_marking(_Place, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: trsn()) -> [place()].

preset(t1) ->
    [p];
preset(_) ->
    [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled with the given mode.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          boolean().

is_enabled(t1, #{p := [_Token]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          {produce, marking()} | abort.

fire(t1, #{p := [_Token]}, _UsrInfo) ->
    {produce, #{p => []}};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback when tokens are produced.
%% Returns 'pass' to keep the token or 'drop' to remove it.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: place(), Token :: token(), NetState :: term()) -> pass | drop.

trigger(_Place, _Token, _NetState) ->
    pass.  % Pass all tokens through

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the net instance.
%% @end
%%--------------------------------------------------------------------
-spec init(NetArg :: term()) -> {ok, usr_info()}.

init(_NetArg) ->
    {ok, undefined}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: usr_info()) ->
          {reply, term(), usr_info()}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: usr_info()) -> {noreply, usr_info()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles non-OTP messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: usr_info()) -> {noreply, usr_info()}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: usr_info()) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Handles hot code reload.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: usr_info(), Extra :: term()) ->
          {ok, usr_info()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Runs all doctests for the module.
%% @private
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% Test basic place_lst callback
place_lst_test() ->
    ?assertEqual([p], place_lst()).

%% Test basic trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t1], trsn_lst()).

%% Test preset for t1
preset_t1_test() ->
    ?assertEqual([p], preset(t1)).

%% Test preset for unknown transition
preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test init_marking
init_marking_test() ->
    ?assertEqual([], init_marking(p, undefined)).

%% Test is_enabled with valid mode
is_enabled_true_test() ->
    ?assertEqual(true, is_enabled(t1, #{p => [token]}, undefined)).

%% Test is_enabled with invalid mode
is_enabled_false_test() ->
    ?assertEqual(false, is_enabled(t1, #{p => []}, undefined)),
    ?assertEqual(false, is_enabled(unknown, #{p => [token]}, undefined)).

%% Test fire with valid mode
fire_valid_test() ->
    ?assertEqual({produce, #{p => []}}, fire(t1, #{p => [token]}, undefined)).

%% Test fire with invalid mode
fire_invalid_test() ->
    ?assertEqual(abort, fire(t1, #{p => []}, undefined)),
    ?assertEqual(abort, fire(unknown, #{p => [token]}, undefined)).

-endif.
