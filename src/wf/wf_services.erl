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

-module(wf_services).
-moduledoc """
Extensible services registry for workflow external services.

Services are pure functions registered by name and called with input
and a timestamp. This allows workflows to interact with external systems
in a testable, trackable way.

Service functions have the signature:
  fun((Input, Now) -> {ok, Result} | {error, Reason})

```erlang
> S0 = wf_services:new().
_

> Echo = fun(Input, _Now) -> {ok, Input} end.
_

> S1 = wf_services:register(S0, echo, Echo).
_

> wf_services:call(S1, echo, #{x => 1}, 0).
{ok,#{x => 1}}

> Add = fun(#{a := A, b := B}, _Now) -> {ok, A + B} end.
_

> S2 = wf_services:register(S1, add, Add).
_

> wf_services:call(S2, add, #{a => 2, b => 3}, 0).
{ok,5}

> wf_services:call(S2, unknown, #{}, 0).
{error,unknown_service}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Services registry API
-export([new/0, register/3, call/4]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Service function signature.
%%
%% A service is a pure function that takes input and a timestamp,
%% returning either {ok, Result} or {error, Reason}.
%%--------------------------------------------------------------------
-type service_fun() :: fun((Input :: term(), Now :: integer()) ->
    {ok, Result :: term()} | {error, Reason :: term()}).

%%--------------------------------------------------------------------
%% @doc Name of a registered service.
%%
%% Services are identified by atom names for fast map lookup.
%%--------------------------------------------------------------------
-type service_name() :: atom().

%%--------------------------------------------------------------------
%% @doc Services registry mapping names to functions.
%%
%% Pure data structure - no processes involved.
%%--------------------------------------------------------------------
-type registry() :: #{service_name() => service_fun()}.

%%--------------------------------------------------------------------
%% @doc Input data passed to a service function.
%%
%% Can be any Erlang term - typically a map for structured data.
%%--------------------------------------------------------------------
-type input() :: term().

%%--------------------------------------------------------------------
%% @doc Timestamp in milliseconds.
%%
%% Passed to service functions for time-sensitive operations.
%%--------------------------------------------------------------------
-type timestamp() :: integer().

%% Export types
-export_type([registry/0, service_fun/0, service_name/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new empty services registry.
%%
%% @returns An empty registry map
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> registry().

new() ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Registers a service function in the registry.
%%
%% If a service with the same name already exists, it is replaced
%% with the new function. This allows for service mocking and
%% reconfiguration during testing.
%%
%% @param Registry The current services registry
%% @param Name The atom name to identify this service
%% @param Fun The service function with signature (Input, Now) -> Result
%% @returns Updated registry with the service registered
%%
%% @end
%%--------------------------------------------------------------------
-spec register(Registry :: registry(), Name :: service_name(),
              Fun :: service_fun()) -> registry().

register(Registry, Name, Fun) when is_map(Registry), is_atom(Name), is_function(Fun, 2) ->
    Registry#{Name => Fun}.

%%--------------------------------------------------------------------
%% @doc Calls a registered service with input and timestamp.
%%
%% Looks up the service by name and invokes it with the provided input
%% and current time. Returns {ok, Result} on success or {error, unknown_service}
%% if the service is not registered.
%%
%% Service functions may also return {error, Reason} for domain-specific
%% errors (e.g., validation failures, network issues).
%%
%% @param Registry The services registry
%% @param Name The name of the service to call
%% @param Input Input data to pass to the service
%% @param Now Current timestamp in milliseconds
%% @returns {ok, Result} or {error, unknown_service | Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec call(Registry :: registry(), Name :: service_name(),
           Input :: input(), Now :: timestamp()) ->
    {ok, term()} | {error, unknown_service | term()}.

call(Registry, Name, Input, Now) when is_map(Registry), is_atom(Name) ->
    case maps:get(Name, Registry, undefined) of
        undefined ->
            {error, unknown_service};
        Fun when is_function(Fun, 2) ->
            Fun(Input, Now)
    end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).
-endif.
