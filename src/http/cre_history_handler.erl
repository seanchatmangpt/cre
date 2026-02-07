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

-ifdef(TEST).
-export([doctest_test/0]).
-endif.

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
%% @doc Runs doctests for the module.
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Verify module is loaded
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    %% Verify init/2 is exported
    true = erlang:function_exported(?MODULE, init, 2),
    ok.

-endif.
