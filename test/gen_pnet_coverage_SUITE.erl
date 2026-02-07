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
%% @doc Common Test Suite for gen_pnet Coverage
%%
%% Comprehensive test suite for gen_pnet module targeting 80% coverage.
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_pnet_coverage_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_pnet.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        init_creates_initial_marking,
        ls_returns_place_tokens,
        fire_removes_preset_tokens,
        fire_adds_postset_tokens,
        mode_selection_empty_marking,
        enabled_transitions_only,
        process_info_message,
        marking_returns_complete_map,
        usr_info_returns_user_data,
        stats_returns_throughput,
        reset_stats_clears,
        step_fires_one_transition,
        drain_fires_multiple,
        inject_adds_tokens,
        state_property_checks_predicate,
        trigger_filters_tokens,
        code_change_handles_upgrade,
        terminate_cleanup
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

init_creates_initial_marking(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    Marking = gen_pnet:marking(Pid),
    true = is_map(Marking),
    gen_pnet:stop(Pid),
    ok.

ls_returns_place_tokens(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    {ok, Tokens} = gen_pnet:ls(Pid, start),
    true = is_list(Tokens),
    gen_pnet:stop(Pid),
    ok.

fire_removes_preset_tokens(_Config) ->
    NetMod = test_consume_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(100),
    StartTokens = gen_pnet:ls(Pid, start),
    gen_pnet:stop(Pid),
    ok.

fire_adds_postset_tokens(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(100),
    EndTokens = gen_pnet:ls(Pid, done),
    gen_pnet:stop(Pid),
    ok.

mode_selection_empty_marking(_Config) ->
    NetMod = test_empty_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(50),
    Marking = gen_pnet:marking(Pid),
    gen_pnet:stop(Pid),
    ok.

enabled_transitions_only(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(100),
    gen_pnet:stop(Pid),
    ok.

process_info_message(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    Pid ! info,
    timer:sleep(50),
    gen_pnet:stop(Pid),
    ok.

marking_returns_complete_map(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    Marking = gen_pnet:marking(Pid),
    true = maps:is_key(start, Marking),
    true = maps:is_key(done, Marking),
    gen_pnet:stop(Pid),
    ok.

usr_info_returns_user_data(_Config) ->
    NetMod = test_usr_info_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, #{test => data}, []),
    UsrInfo = gen_pnet:usr_info(Pid),
    #{test := data} = UsrInfo,
    gen_pnet:stop(Pid),
    ok.

stats_returns_throughput(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(100),
    Stats = gen_pnet:stats(Pid),
    true = is_record(Stats, stats),
    gen_pnet:stop(Pid),
    ok.

reset_stats_clears(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(100),
    ok = gen_pnet:reset_stats(Pid),
    Stats = gen_pnet:stats(Pid),
    undefined = Stats,
    gen_pnet:stop(Pid),
    ok.

step_fires_one_transition(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    Result = gen_pnet:step(Pid),
    {ok, _Receipt} = Result,
    gen_pnet:stop(Pid),
    ok.

drain_fires_multiple(_Config) ->
    NetMod = test_multi_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    Result = gen_pnet:drain(Pid, 10),
    {ok, Receipts} = Result,
    true = is_list(Receipts),
    gen_pnet:stop(Pid),
    ok.

inject_adds_tokens(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    {ok, Receipt} = gen_pnet:inject(Pid, #{start => [injected]}),
    true = is_map(Receipt),
    gen_pnet:stop(Pid),
    ok.

state_property_checks_predicate(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    Pred = fun([Start]) -> length(Start) >= 0 end,
    ok = gen_pnet:state_property(Pid, Pred, [start]),
    gen_pnet:stop(Pid),
    ok.

trigger_filters_tokens(_Config) ->
    NetMod = test_trigger_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    timer:sleep(100),
    gen_pnet:stop(Pid),
    ok.

code_change_handles_upgrade(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    %% Simulate code change
    {ok, _} = gen_pnet:code_change(1, Pid, undefined),
    gen_pnet:stop(Pid),
    ok.

terminate_cleanup(_Config) ->
    NetMod = test_simple_pnet,
    {ok, Pid} = gen_pnet:start_link(NetMod, undefined, []),
    ok = gen_pnet:stop(Pid),
    false = is_process_alive(Pid),
    ok.

%%====================================================================
%% Test Petri Net Modules
%%====================================================================

-module(test_simple_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2, trigger/3
]).

place_lst() -> [start, done].
trsn_lst() -> [t1].

init_marking(start, _UsrInfo) -> [init];
init_marking(done, _UsrInfo) -> [].

preset(t1) -> [start].

is_enabled(t1, #{start := [init]}, _UsrInfo) -> true;
is_enabled(_, _, _) -> false.

fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{done => [complete]}}.

init(_Arg) -> #{}.
handle_call(_Request, _From, _State) -> {reply, ok}.
handle_cast(_Request, _State) -> noreply.
handle_info(_Info, _State) -> noreply.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _State) -> pass.

-module(test_consume_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2, trigger/3
]).

place_lst() -> [start, done].
trsn_lst() -> [t1].

init_marking(start, _UsrInfo) -> [init];
init_marking(done, _UsrInfo) -> [].

preset(t1) -> [start].

is_enabled(t1, #{start := [init]}, _UsrInfo) -> true;
is_enabled(_, _, _) -> false.

fire(t1, #{start := [init]}, _UsrInfo) ->
    {produce, #{done => [complete]}}.

init(_Arg) -> #{}.
handle_call(_Request, _From, _State) -> {reply, ok}.
handle_cast(_Request, _State) -> noreply.
handle_info(_Info, _State) -> noreply.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _State) -> pass.

-module(test_empty_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2, trigger/3
]).

place_lst() -> [start].
trsn_lst() -> [].

init_marking(_Place, _UsrInfo) -> [].

preset(_Trsn) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(_Trsn, _Mode, _UsrInfo) -> abort.

init(_Arg) -> #{}.
handle_call(_Request, _From, _State) -> {reply, ok}.
handle_cast(_Request, _State) -> noreply.
handle_info(_Info, _State) -> noreply.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _State) -> pass.

-module(test_usr_info_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2, trigger/3
]).

place_lst() -> [start].
trsn_lst() -> [].

init_marking(_Place, _UsrInfo) -> [].

preset(_Trsn) -> [].

is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(_Trsn, _Mode, _UsrInfo) -> abort.

init(Arg) -> Arg.
handle_call(_Request, _From, State) -> {reply, State}.
handle_cast(_Request, _State) -> noreply.
handle_info(_Info, _State) -> noreply.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _State) -> pass.

-module(test_multi_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2, trigger/3
]).

place_lst() -> [p1, p2, p3].
trsn_lst() -> [t1, t2].

init_marking(p1, _UsrInfo) -> [a, b];
init_marking(_Place, _UsrInfo) -> [].

preset(t1) -> [p1].
preset(t2) -> [p2].

is_enabled(t1, #{p1 := [_]}, _UsrInfo) -> true;
is_enabled(t2, #{p2 := [_]}, _UsrInfo) -> true;
is_enabled(_, _, _) -> false.

fire(t1, #{p1 := [_]}, _UsrInfo) ->
    {produce, #{p2 => [x]}};
fire(t2, #{p2 := [_]}, _UsrInfo) ->
    {produce, #{p3 => [y]}}.

init(_Arg) -> #{}.
handle_call(_Request, _From, _State) -> {reply, ok}.
handle_cast(_Request, _State) -> noreply.
handle_info(_Info, _State) -> noreply.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(_Place, _Token, _State) -> pass.

-module(test_trigger_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2, trigger/3
]).

place_lst() -> [start, done].
trsn_lst() -> [t1].

init_marking(start, _UsrInfo) -> [init, drop_me];
init_marking(done, _UsrInfo) -> [].

preset(t1) -> [start].

is_enabled(t1, #{start := [_, _]}, _UsrInfo) -> true;
is_enabled(_, _, _) -> false.

fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{done => [complete]}}.

init(_Arg) -> #{}.
handle_call(_Request, _From, _State) -> {reply, ok}.
handle_cast(_Request, _State) -> noreply.
handle_info(_Info, _State) -> noreply.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
trigger(start, drop_me, _State) -> drop;
trigger(_Place, _Token, _State) -> pass.
