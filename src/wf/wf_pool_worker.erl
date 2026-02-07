%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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

-module(wf_pool_worker).
-moduledoc """
Placeholder worker module for poolboy integration.

This module implements the poolboy_worker behavior and serves as a
minimal placeholder for doctest purposes. Real applications should
replace this with actual worker implementations that perform useful work.

For wf_pool doctests, the worker is just a placeholder - the actual
work is done by the transaction function passed to wf_pool:transaction/2.
""".

-behaviour(poolboy_worker).
-behaviour(gen_server).

%% poolboy_worker API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

%% Worker state (placeholder - no meaningful state needed)
-record(state, {}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the worker process.
%%
%% Required by poolboy_worker behavior. Args are ignored for this
%% placeholder worker.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: term()) -> {ok, pid()} | {error, term()}.

start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the worker.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.

init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles synchronous call messages.
%%
%% Returns ok for all requests (placeholder behavior).
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
          {reply, ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles asynchronous cast messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: #state{}) -> {noreply, #state{}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles all non call/cast messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #state{}) -> {noreply, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Cleanup on termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), #state{}) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code changes.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
          {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
