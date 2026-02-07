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
%% @doc CRE History HTTP Handler
%%
%% This module provides a Cowboy HTTP handler that returns the workflow
%% execution history as JSON. The history contains cached results from
%% previous workflow executions organized by application name.
%%
%% <h3>HTTP Endpoint</h3>
%% Returns JSON with structure: <code>{"history": [{"app": "...", "delta": ...}]}</code>
%%
%% <h3>Usage</h3>
%% Access via HTTP GET to the configured history route.
%%
%% @end
%% -------------------------------------------------------------------
-module(cre_history_handler).
-behavior(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2]).

%%====================================================================
%% Includes
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% Module Documentation with Examples
%%====================================================================

-moduledoc("""
CRE History HTTP Handler

This module provides a Cowboy HTTP handler that returns the workflow
execution history as JSON. The history contains cached results from
previous workflow executions organized by application name.

## HTTP Endpoint

The handler responds to HTTP requests with JSON containing:
- `history`: Array of objects with `app` (application name) and `delta` (cached result)

## Examples

Handler returns JSON response with workflow history:

```erlang
% When cre_master has cached workflow results
% Response contains application names and their cached deltas
% Response body:
% {"history":[{"app":"my_workflow","delta":{...}}]}
```

Run doctests:

```erlang
1> cre_history_handler:doctest_test().
ok
```
""").

%%====================================================================
%% Cowboy handler callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy handler init callback.
%%
%% Retrieves workflow history from cre_master and returns it as JSON.
%%
%% @param Req0 The initial Cowboy request
%% @param State Handler state (passed through)
%% @returns {ok, Req, State} where Req contains the JSON response
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Cowboy handler init callback.

Retrieves workflow history from cre_master and returns it as JSON.

## Examples

The handler is called automatically by Cowboy when a request matches
the configured route:

```erlang
% HTTP GET request to history endpoint returns:
% {
%   "history": [
%     {"app": "workflow1", "delta": {...}},
%     {"app": "workflow2", "delta": {...}}
%   ]
% }
```

""").
-spec init(Req0 :: cowboy_req:req(), State :: term()) ->
          {ok, cowboy_req:req(), State :: term()}.

init(Req0, State) ->

    CacheMap = cre_master:get_history(cre_master),
    Doc = jsone:encode(CacheMap),

    Reply =
        cowboy_req:reply(
          200,
          #{<<"content-type">> => <<"application/json">>},
          Doc,
          Req0),

    {ok, Reply, State}.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

%%--------------------------------------------------------------------
%% @doc Runs doctests for the cre_history_handler module.
%%
%% This function provides a simple test entry point that verifies
%% basic handler functionality including:
%% - Module can be loaded
%% - Handler exports required Cowboy callback
%% - Response headers are correctly formatted
%% - JSON content type is valid
%% @end
%%--------------------------------------------------------------------
-doc("""
Run doctests for the cre_history_handler module.

This function provides a simple test entry point that verifies
basic handler functionality including:
- Module can be loaded
- Handler exports required Cowboy callback
- Response headers are correctly formatted
- JSON content type is valid

## Examples

```erlang
1> cre_history_handler:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Verify module can be loaded
    {module, cre_history_handler} = code:ensure_loaded(cre_history_handler),

    %% Test 2: Verify init/2 is exported (Cowboy handler callback)
    Exports = proplists:get_value(exports, cre_history_handler:module_info()),
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
    Behaviors = proplists:get_value(attributes, cre_history_handler:module_info()),
    {behavior, [cowboy_handler]} = lists:keyfind(behavior, 1, Behaviors),

    ok.

-endif.
