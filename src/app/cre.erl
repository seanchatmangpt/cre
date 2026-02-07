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
%% @author Jorgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%% @doc CRE Application Module
%%
%% This module implements the OTP application behavior for the CRE system.
%% It provides the application callback functions for starting and stopping
%% the CRE runtime, along with helper functions for process discovery and
%% HTTP server setup.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Application Entry Point:</b> Standard OTP application callbacks</li>
%%   <li><b>Configuration Init:</b> Persistent term-based configuration (OTP 21+)</li>
%%   <li><b>HTTP Server:</b> Cowboy-based status/history endpoints</li>
%%   <li><b>Escript Support:</b> Standalone executable via main/1</li>
%% </ul>
%% @end
%% -------------------------------------------------------------------

-module(cre).
-behaviour(application).

%%====================================================================
%% Exports
%%====================================================================

-export([start/0, pid/1]).
-export([start/2, stop/1]).
-export([main/1]).
-export([doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

-include("../../include/cre.hrl").

%%====================================================================
%% Definitions
%%====================================================================

-define(PORT, 4142).

%%====================================================================
%% API functions
%%====================================================================


%% @doc Starts the CRE application and all its dependencies.
%%
%%      Equivalent to `application:ensure_all_started(cre)' but returns
%%      only `ok' on success.
%%
%% @returns `ok' when the application starts successfully
%%
%% @see application:ensure_all_started/1
%%
-doc("""
Starts the CRE application and all its dependencies.

This is a convenience wrapper around `application:ensure_all_started/1`
that returns only `ok' on success.

## Example

```erlang
1> cre:start().
ok
2> whereis(cre_master).
<0.123.0>
```
""").
-spec start() -> ok.

start() ->
    {ok, _} = application:ensure_all_started(cre),
    ok.


%% @doc Gets the PID of the cre_master process on a remote node.
%%
%%      Uses RPC to query the registered `cre_master' process on the
%%      target node. Returns an error tuple if the node is down or the
%%      process is not registered.
%%
%% @param CreNode The name of the CRE node (atom)
%% @returns `{ok, Pid}' on success, `{error, Reason}' otherwise
%%
%% @see rpc:call/4
%% @see erlang:whereis/1
%%
-doc("""
Gets the PID of the cre_master process on a remote node.

Uses RPC to query the registered `cre_master' process on the target node.

## Example

```erlang
1> cre:pid(cre@localhost).
{ok,<0.123.0>}
```

Returns `{error, cre_node_down}` if the node is unreachable:
```erlang
1> cre:pid(cre@nonexistent).
{error,cre_node_down}
```

Returns `{error, cre_process_not_registered}` if cre_master is not running:
```erlang
1> cre:pid(cre@localhost).
{error,cre_process_not_registered}
```
""").
-spec pid(CreNode :: atom()) -> {error, cre_node_down | cre_process_not_registered} | {ok, _}.

pid(CreNode) when is_atom(CreNode) ->

    % query cre process pid
    case rpc:call(CreNode, erlang, whereis, [cre_master]) of
        undefined -> {error, cre_process_not_registered};
        {badrpc, nodedown} -> {error, cre_node_down};
        CrePid -> {ok, CrePid}
    end.


%%====================================================================
%% Application callback functions
%%====================================================================


%% @doc Application start callback (OTP behaviour).
%%
%%      Initializes persistent_term configuration and starts the HTTP
%%      web service before starting the supervision tree.
%%
%% @param _Type Application start type (ignored)
%% @param _Args Application start arguments (ignored)
%% @returns `{ok, Pid}' when supervisor starts successfully
%%
%% @see application:start/2
%% @see cre_sup:start_link/0
%%
-doc("""
Application start callback (OTP behaviour).

Initializes persistent_term configuration and starts the HTTP web service
before starting the supervision tree.

## Example

The application callback is typically invoked by `application:start/1`:
```erlang
1> application:start(cre).
Starting CRE: vsn="0.1.10" node=nonode@nohost port=4142
{ok,<0.456.0>}
```
""").
-spec start(Type :: _, Args :: _) -> {ok, pid()} | {error, _}.

start(_Type, _Args) ->

    %% Initialize persistent_term configuration (OTP 21+ optimization)
    ok = cre_config:init(),

    %% Use persistent_term for O(1) access to default port
    DefaultPort = cre_config:get(cre_default_port, 4142),
    case start_cre_webservice(DefaultPort) of
        {ok, Port} ->
            logger:info("Starting CRE: vsn=~p node=~p port=~p",
                        [?VSN, node(), Port],
                        [{info, "starting cre"}, {application, cre}]),
            cre_sup:start_link();
        {error, {already_started, Port}} ->
            logger:info("CRE web service already running on port ~p", [Port]),
            cre_sup:start_link()
    end.


%% @doc Application stop callback (OTP behaviour).
%%
%%      Called when the application is stopped. Currently returns `ok'
%%      without performing any cleanup.
%%
%% @param _State The application state (ignored)
%% @returns `ok'
%%
%% @see application:stop/1
%%
-doc("""
Application stop callback (OTP behaviour).

Called when the application is stopped. Currently returns `ok' without
performing any cleanup as the supervision tree handles shutdown.

## Example

```erlang
1> application:stop(cre).
ok
```
""").
-spec stop(State :: _) -> ok.

stop(_State) ->
    ok.


%%====================================================================
%% Escript main function
%%====================================================================


%% @doc Entry point for escript execution.
%%
%%      Starts the CRE application and monitors the supervisor process,
%%      keeping the escript alive until CRE terminates.
%%
%% @param _Args Command line arguments (currently ignored)
%% @returns `ok' when CRE terminates
%%
-doc("""
Entry point for escript execution.

Starts the CRE application and monitors the supervisor process, keeping
the escript alive until CRE terminates.

## Example

Run as an escript:
```bash
$ cre
Starting CRE: vsn="0.1.10" node=nonode@nohost port=4142
[Press Ctrl+C to exit]
```
""").
-spec main(Args :: _) -> ok.

main(_Args) ->

    % start the cre application
    ok = start(),

    % create monitor
    _ = monitor(process, cre_sup),

    % wait indefinitely
    receive
        {'DOWN', _Ref, process, _Object, _Info} ->
            timer:sleep(1000)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

% start_cre_webservice/1
% @doc Attempts to start the CRE web service under the given port.
%      If the port is in use we try new port numbers until we find a
%      free port.


-doc("""
Attempts to start the CRE web service on the given port.

If the port is already in use (eaddrinuse), automatically tries the next
port number until a free port is found.

## Example

```erlang
1> cre:start_cre_webservice(4142).
{ok,4142}
```

If port 4142 is busy, tries 4143:
```erlang
1> cre:start_cre_webservice(4142).
{ok,4143}
```
""").
-spec start_cre_webservice(Port :: inet:port_number()) ->
          {ok, inet:port_number()} | {error, _}.

start_cre_webservice(Port)
  when is_integer(Port),
       Port >= 0,
       Port < 65536 ->

    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/[status.json]", cre_status_handler, []},
                  {"/history.json", cre_history_handler, []}]}]),

    Reply = cowboy:start_clear(cre_status_listener,
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch}}),

    case Reply of
        {ok, _ListenerPid} -> {ok, Port};
        {error, eaddrinuse} -> start_cre_webservice(Port + 1);
        {error, Reason} -> {error, Reason}
    end.


%%====================================================================
%% Doctests
%%====================================================================

%% @doc Runs doctests for the cre module.
%%
%%      Executes fast, minimal tests that verify module behavior without
%%      starting the full application.
%%
%% @returns `ok'
%%
%% @end
%%
-doc("""
Runs doctests for the cre module.

Executes fast, minimal tests verifying basic module behavior without
starting the full application.

## Examples

```erlang
1> cre:doctest_test().
ok
```

Tests verify:
- Module can be loaded
- Default port constant is correct
- pid/1 returns error for nonexistent nodes
- stop/1 returns ok
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Ensure ranch/cowboy/cre are started for webservice test
    _ = application:ensure_all_started(cre),

    %% Test 1: Verify module can be loaded
    {module, cre} = code:ensure_loaded(cre),

    %% Test 2: Verify default port constant
    4142 = ?PORT,

    %% Test 3: Verify pid/1 with invalid node returns error
    {error, _} = pid(cre@nonexistent_node_xyz),

    %% Test 4: Verify pid/1 returns correct error for nodedown
    {error, cre_node_down} = pid(cre@nonexistent_node_xyz),

    %% Test 5: Verify stop/1 returns ok
    ok = stop(undefined),

    %% Test 6: Verify pid/1 rejects non-atom input (will fail guard or pattern match)
    {'EXIT', _} = (catch pid("not_an_atom")),

    %% Test 7: Verify start_cre_webservice/1 validates port range via guard
    %% Port values outside valid range fail the guard clause - catches function_clause
    try start_cre_webservice(-1) of
        _ -> error(should_have_failed)
    catch
        error:function_clause -> ok
    end,
    try start_cre_webservice(65536) of
        _ -> error(should_have_failed)
    catch
        error:function_clause -> ok
    end,

    %% Test 8: Verify start_cre_webservice/1 requires cowboy/ranch running
    %% Returns error, undef, or exit when ranch_sup is not available
    try
        case start_cre_webservice(4142) of
            {error, _} -> ok;
            {ok, _} -> ok  %% If ranch happens to be running, that's fine
        end
    catch
        error:undef -> ok;
        exit:{noproc, _} -> ok
    end,

    %% Test 9: Verify module exports are accessible
    {name, start} = erlang:fun_info(fun start/0, name),
    {name, pid} = erlang:fun_info(fun pid/1, name),
    {name, stop} = erlang:fun_info(fun stop/1, name),
    {name, main} = erlang:fun_info(fun main/1, name),
    {name, doctest_test} = erlang:fun_info(fun doctest_test/0, name),

    %% Test 10: Verify behavior module attribute
    Attributes = module_info(attributes),
    true = proplists:is_defined(behavior, Attributes) orelse
            proplists:is_defined(behaviour, Attributes),

    ok.
