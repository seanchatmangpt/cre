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

-module(wf_test_net_choice).
-moduledoc """
Test-only gen_pnet module for deterministic choice behavior.

This module implements a simple Petri net for testing deterministic
choice between transitions. The key property is that the same seed
always results in the same transition firing first.

## Doctests

```erlang
> wf_test_net_choice:place_lst().
[in,out]
```

```erlang
> wf_test_net_choice:trsn_lst().
[t_choose_a,t_choose_b]
```

```erlang
> wf_test_net_choice:preset(t_choose_a).
[in]
> wf_test_net_choice:preset(t_choose_b).
[in]
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet structure callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%% gen_pnet interface callbacks
-export([
    init/1,
    trigger/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_pnet.hrl").

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc User info containing RNG state for deterministic choice.
%%--------------------------------------------------------------------
-type usr_info() :: #{
    seed := integer(),
    rng_state := rand:state()
}.

%%--------------------------------------------------------------------
%% @doc Place in the choice test net.
%%--------------------------------------------------------------------
-type place() :: in | out.

%%--------------------------------------------------------------------
%% @doc Transition in the choice test net.
%%--------------------------------------------------------------------
-type trsn() :: t_choose_a | t_choose_b.

%%--------------------------------------------------------------------
%% @doc Mode mapping places to tokens consumed.
%%--------------------------------------------------------------------
-type mode() :: #{place() => [term()]}.

%%--------------------------------------------------------------------
%% @doc Marking mapping places to token lists.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [term()]}.

-export_type([usr_info/0, place/0, trsn/0, mode/0, marking/0]).

%%====================================================================
%% gen_pnet Structure Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of places in the choice test net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [place()].

place_lst() ->
    [in, out].

%%--------------------------------------------------------------------
%% @doc Returns list of transitions offering choice.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [trsn()].

trsn_lst() ->
    [t_choose_a, t_choose_b].

%%--------------------------------------------------------------------
%% @doc Returns initial marking for a place.
%%
%% All places start empty. Tokens are injected externally.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: place(), UsrInfo :: usr_info()) -> [term()].

init_marking(_Place, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns preset (input places) for each transition.
%%
%% Both transitions consume from the same input place `in`.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: trsn()) -> [place()].

preset(t_choose_a) -> [in];
preset(t_choose_b) -> [in];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if transition is enabled with given mode.
%%
%% A transition is enabled when `in` has at least one token.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          boolean().

is_enabled(_Trsn, #{in := [_Token | _]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires transition, producing output tokens.
%%
%% Deterministic choice: same seed => same transition fires first.
%% Each transition produces a distinctive token indicating the choice.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          {produce, marking()} | abort.

fire(t_choose_a, #{in := [_Token]}, _UsrInfo) ->
    {produce, #{in => [], out => [chosen_a]}};
fire(t_choose_b, #{in := [_Token]}, _UsrInfo) ->
    {produce, #{in => [], out => [chosen_b]}};
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%====================================================================
%% gen_pnet Interface Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the net instance.
%%
%% Takes a map with `seed` key and creates initial RNG state.
%%
%% ```erlang
%% > UsrInfo = wf_test_net_choice:init(#{seed => 42}).
%% > is_map(UsrInfo).
%% true
%% > maps:is_key(seed, UsrInfo).
%% true
%% > maps:is_key(rng_state, UsrInfo).
%% true
%% ```
%% @end
%%--------------------------------------------------------------------
-spec init(NetArg :: #{seed := integer()}) -> usr_info().

init(#{seed := Seed}) ->
    RngState = pnet_choice:seed(Seed),
    #{seed => Seed, rng_state => RngState}.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token filtering.
%%
%% All tokens pass through by default.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: place(), Token :: term(), NetState :: #net_state{}) ->
          pass | drop.

trigger(_Place, _Token, _NetState) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% Basic implementation that returns `noreply`.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), term()},
                  NetState :: #net_state{}) ->
          {reply, term()} | noreply.

handle_call(_Request, _From, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% Basic implementation that returns `noreply`.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: #net_state{}) ->
          noreply.

handle_cast(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles unformatted messages.
%%
%% Basic implementation that returns `noreply`.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), NetState :: #net_state{}) ->
          noreply.

handle_info(_Info, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles hot code reload.
%%
%% Basic implementation that returns ok.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: #net_state{}, Extra :: term()) ->
          {ok, #net_state{}}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%%
%% Basic implementation that returns ok.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: #net_state{}) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs all doctests for the module.
%% @private
%%--------------------------------------------------------------------
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% Test place_lst
place_lst_test() ->
    ?assertEqual([in, out], place_lst()).

%% Test trsn_lst
trsn_lst_test() ->
    ?assertEqual([t_choose_a, t_choose_b], trsn_lst()).

%% Test preset
preset_t_choose_a_test() ->
    ?assertEqual([in], preset(t_choose_a)).

preset_t_choose_b_test() ->
    ?assertEqual([in], preset(t_choose_b)).

%% Test init_marking returns empty
init_marking_in_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual([], init_marking(in, UsrInfo)).

init_marking_out_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual([], init_marking(out, UsrInfo)).

%% Test init stores seed and rng_state
init_structure_test() ->
    UsrInfo = init(#{seed => 42}),
    ?assert(maps:is_key(seed, UsrInfo)),
    ?assert(maps:is_key(rng_state, UsrInfo)),
    ?assertEqual(42, maps:get(seed, UsrInfo)),
    %% rand:state() is a tuple, not a map
    RngState = maps:get(rng_state, UsrInfo),
    ?assert(is_tuple(RngState)).

%% Test is_enabled with token in input
is_enabled_with_token_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual(true, is_enabled(t_choose_a, #{in => [go]}, UsrInfo)),
    ?assertEqual(true, is_enabled(t_choose_b, #{in => [go]}, UsrInfo)).

%% Test is_enabled without token
is_enabled_without_token_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual(false, is_enabled(t_choose_a, #{in => []}, UsrInfo)),
    ?assertEqual(false, is_enabled(t_choose_b, #{in => []}, UsrInfo)).

%% Test fire produces correct tokens
fire_t_choose_a_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual({produce, #{out => [chosen_a]}},
                 fire(t_choose_a, #{in => []}, UsrInfo)).

fire_t_choose_b_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual({produce, #{out => [chosen_b]}},
                 fire(t_choose_b, #{in => []}, UsrInfo)).

%% Test fire with invalid mode returns abort
fire_invalid_mode_test() ->
    UsrInfo = init(#{seed => 1}),
    ?assertEqual(abort, fire(t_choose_a, #{in => [go]}, UsrInfo)).

%% Test trigger always passes
trigger_pass_test() ->
    NetState = #net_state{},
    ?assertEqual(pass, trigger(in, token, NetState)),
    ?assertEqual(pass, trigger(out, token, NetState)).

%% Test handle_call returns noreply
handle_call_test() ->
    NetState = #net_state{},
    ?assertEqual(noreply, handle_call(req, self(), NetState)).

%% Test handle_cast returns noreply
handle_cast_test() ->
    NetState = #net_state{},
    ?assertEqual(noreply, handle_cast(req, NetState)).

%% Test handle_info returns noreply
handle_info_test() ->
    NetState = #net_state{},
    ?assertEqual(noreply, handle_info(info, NetState)).

%% Test code_change
code_change_test() ->
    NetState = #net_state{},
    ?assertMatch({ok, #net_state{}}, code_change(v1, NetState, extra)).

%% Test terminate
terminate_test() ->
    NetState = #net_state{},
    ?assertEqual(ok, terminate(normal, NetState)).

%% Test determinism: same seed produces same RNG state
determinism_test() ->
    UsrInfo1 = init(#{seed => 12345}),
    UsrInfo2 = init(#{seed => 12345}),
    RngState1 = maps:get(rng_state, UsrInfo1),
    RngState2 = maps:get(rng_state, UsrInfo2),
    ?assertEqual(RngState1, RngState2).

%% Test different seeds produce different RNG states
different_seeds_test() ->
    UsrInfo1 = init(#{seed => 111}),
    UsrInfo2 = init(#{seed => 222}),
    RngState1 = maps:get(rng_state, UsrInfo1),
    RngState2 = maps:get(rng_state, UsrInfo2),
    ?assertNotEqual(RngState1, RngState2).

-endif.
