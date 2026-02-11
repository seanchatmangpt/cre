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
%% @doc Simple test workflow module
%%
%% A minimal gen_yawl workflow for testing HTTP handler functionality.
%%
%% @end
%% -------------------------------------------------------------------

-module(test_workflow).
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

place_lst() -> [p1, p2].

trsn_lst() -> [t1].

init_marking(p1, _) -> [token];
init_marking(_, _) -> [].

preset(t1) -> [p1].

is_enabled(t1, _Mode, _UsrInfo) -> true.

fire(t1, _Mode, UsrInfo) -> {produce, #{p2 => [token]}, UsrInfo}.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Args) -> {ok, Args}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast(_Req, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
