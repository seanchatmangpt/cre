%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
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
%% @doc Simple Test Net for Fault Injection Testing
%%
%% A minimal gen_yawl net module for testing fault injection.
%%
%% @end
%% -------------------------------------------------------------------

-module(test_simple_net).
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([place_lst/0]).
-export([trsn_lst/0]).
-export([init_marking/2]).
-export([preset/1]).
-export([is_enabled/3]).
-export([fire/3]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).
-export([trigger/3]).

%%====================================================================
%% Structure Callbacks
%%====================================================================

place_lst() -> [p_start, p_task, p_end].

trsn_lst() -> [t_start, t_task, t_end].

init_marking(p_start, _UsrInfo) -> [token];
init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_task) -> [p_task];
preset(t_end) -> [p_task].

is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

fire(t_start, _Mode, UsrInfo) ->
    {produce, #{p_task => [started]}, UsrInfo};
fire(t_task, _Mode, UsrInfo) ->
    {produce, #{p_task => [completed], p_end => [done]}, UsrInfo};
fire(t_end, _Mode, UsrInfo) ->
    {produce, #{}, UsrInfo}.

%%====================================================================
%% Interface Callbacks
%%====================================================================

init(_NetArg) -> #{}.

handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

handle_cast(_Msg, NetState) ->
    {noreply, NetState}.

handle_info(_Info, NetState) ->
    {noreply, NetState}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _NetState) ->
    pass.
