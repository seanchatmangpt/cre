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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc Mnesia Clustering Supervisor Module
%%
%% This module implements the top-level supervisor for Mnesia clustering
%% components. It manages the cluster manager and associated helper
%% processes using the one_for_one restart strategy.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Clustering Supervisor:</b> Root supervisor for cluster components</li>
%%   <li><b>Child Specs:</b> Manages cluster and health monitoring</li>
%%   <li><b>One-For-One Strategy:</b> Each child is restarted independently</li>
%%   <li><b>Graceful Shutdown:</b> Proper cleanup on shutdown</li>
%% </ul>
%%
%% <h3>Child Specifications</h3>
%%
%% The supervisor manages two child processes:
%%
%% <ul>
%%   <li><b>cluster:</b> Cluster join/leave orchestration gen_server</li>
%%   <li><b>cluster_utils:</b> Health monitoring and partition detection</li>
%% </ul>
%%
%% <h3>Supervisor Flags</h3>
%%
%% <ul>
%%   <li><b>strategy:</b> one_for_one - only the terminated child is restarted</li>
%%   <li><b>intensity:</b> 5 - maximum restarts in period</li>
%%   <li><b>period:</b> 60 - time window for intensity calculation (seconds)</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Start the clustering supervisor
%% {ok, Pid} = mnesia_cluster_sup:start_link(),
%%
%% %% Stop the supervisor
%% ok = supervisor:terminate_child(mnesia_cluster_sup, cluster).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(mnesia_cluster_sup).
-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0, start_link/1]).
-export([init/1]).

%%====================================================================
%% Type definitions
%%====================================================================

-type sup_flags() :: #{
    strategy => one_for_one,
    intensity => non_neg_integer(),
    period => pos_integer()
}.
-type child_spec() :: #{
    id => atom(),
    start => {atom(), atom(), [term()]},
    restart => permanent | temporary | transient,
    shutdown => timeout() | infinity,
    type => worker | supervisor,
    modules => [atom() | {atom(), [term()]}]
}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the Mnesia clustering supervisor with default options.
%%
%%      The supervisor is registered locally as `mnesia_cluster_sup'
%%      and uses the one_for_one restart strategy.
%%
%%      Returns `{ok, Pid}' when the supervisor starts successfully.
%%      Returns `{error, {already_started, Pid}}' if already running.
%%      Returns `{error, Reason}' if startup fails.
%%
%% @returns `{ok, Pid}' | `{error, already_started} | `{error, Reason}'
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Starts the Mnesia clustering supervisor with options.
%%
%%      Options are passed through to child processes for configuration.
%%      Common options include discovery_method and max_retries.
%%
%% @param Options List of supervisor options
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Options).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

%% @doc Supervisor initialization callback.
%%
%%      Defines the supervisor flags and child specifications for the
%%      Mnesia clustering supervision tree.
%%
%%      <h4>Supervisor Flags</h4>
%%      <ul>
%%        <li><b>strategy:</b> one_for_one - only terminated child is restarted</li>
%%        <li><b>intensity:</b> 5 - maximum restarts in period</li>
%%        <li><b>period:</b> 60 - time window for intensity calculation</li>
%%      </ul>
%%
%%      <h4>Child Specifications</h4>
%%      <ul>
%%        <li><b>cluster:</b> permanent restart, 5000ms shutdown timeout</li>
%%        <li><b>cluster_utils:</b> permanent restart, 5000ms shutdown timeout</li>
%%      </ul>
%%
%% @param Options List of configuration options for child processes
%% @returns `{ok, {SupFlags, [ChildSpec, ...]}}'
%%
-spec init([proplists:property()]) ->
    {ok, {sup_flags(), [child_spec()]}}.
init(Options) ->

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    %% Extract options for cluster manager
    ClusterOptions = #{
        discovery_method => proplists:get_value(discovery_method, Options, dns),
        max_retries => proplists:get_value(max_retries, Options, 5),
        dns_name => proplists:get_value(dns_name, Options, "localhost")
    },

    ClusterSpec = #{
        id => cluster,
        start => {cluster, start_link, [maps:to_list(ClusterOptions)]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cluster]
    },

    ClusterUtilsSpec = #{
        id => cluster_utils,
        start => {cluster_utils, start_link, [Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cluster_utils]
    },

    logger:info("Mnesia cluster supervisor starting with options: ~p",
                [Options],
                [{info, "sup_init"}, {application, cre}]),

    {ok, {SupFlags, [ClusterSpec, ClusterUtilsSpec]}}.
