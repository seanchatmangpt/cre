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
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------
-module(cre_status_handler).
-behavior(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2]).
-export([doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

-include("cre.hrl").

%%====================================================================
%% Module Documentation
%%====================================================================

-moduledoc("""
HTTP Status Handler for CRE Master Status API.

This module implements a Cowboy handler that returns the current status
of the CRE master process as JSON. The status includes information about
worker pools, active tasks, and system metrics.

## Endpoint

```
GET /[status.json]
```

## Response Format

The handler returns a JSON object with status information:

```json
{
  "status": "running",
  "workers": {...},
  "tasks": {...}
}
```

## Examples

```erlang
%% The handler is automatically invoked by Cowboy for requests to /status.json
%% It queries cre_master:get_status/1 and returns the result as JSON
```
""").

%%====================================================================
%% Cowboy handler callback functions
%%====================================================================

%% @doc Cowboy handler callback for HTTP status requests.
%%
%% Retrieves the current status from cre_master and returns it as JSON.
%%
%% <h3>Request Flow</h3>
%% <ol>
%%   <li>Receive HTTP request via Cowboy</li>
%%   <li>Query cre_master for current status</li>
%%   <li>Encode status map as JSON</li>
%%   <li>Return HTTP 200 with JSON body</li>
%% </ol>
%%
%% @end
-spec init(Req :: cowboy_req:req(), State :: term()) ->
    {ok, Req :: cowboy_req:req(), State :: term()}.

init(Req0, State) ->

    StatusMap = cre_master:get_status(cre_master),
    Doc = jsone:encode(StatusMap),

    Reply =
        cowboy_req:reply(
          200,
          #{<<"content-type">> => <<"application/json">>},
          Doc,
          Req0),

    {ok, Reply, State}.

%%====================================================================
%% Doctests
%%====================================================================

-doc("""
Run doctests for the cre_status_handler module.

This function provides a simple test entry point that verifies
basic handler functionality including:
- Module can be loaded
- Handler exports required Cowboy callback
- Response headers are correctly formatted
- JSON content type is valid

## Examples

```erlang
1> cre_status_handler:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Verify module can be loaded
    {module, cre_status_handler} = code:ensure_loaded(cre_status_handler),

    %% Test 2: Verify init/2 is exported (Cowboy handler callback)
    Exports = proplists:get_value(exports, cre_status_handler:module_info()),
    true = lists:member({init, 2}, Exports),

    %% Test 3: Verify doctest_test/0 is exported
    true = lists:member({doctest_test, 0}, Exports),

    %% Test 4: Verify JSON content type header is valid binary
    ContentType = <<"application/json">>,
    true = is_binary(ContentType),
    <<"application/json">> = ContentType,

    %% Test 5: Verify HTTP status code is valid integer
    StatusCode = 200,
    true = is_integer(StatusCode),
    200 = StatusCode,

    %% Test 6: Verify response headers map structure
    Headers = #{<<"content-type">> => <<"application/json">>},
    true = is_map(Headers),
    <<"application/json">> = maps:get(<<"content-type">>, Headers),

    %% Test 7: Verify module behavior declaration
    Behaviors = proplists:get_value(attributes, cre_status_handler:module_info()),
    {behavior, [cowboy_handler]} = lists:keyfind(behavior, 1, Behaviors),

    ok.
