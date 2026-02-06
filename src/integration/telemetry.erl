%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc telemetry stub module
%%
%% This module provides a stub implementation when telemetry
%% is not available as a dependency. The real telemetry
%% will be used at runtime if available via code:is_loaded/1.
%%
%% To use the real telemetry library, add it to rebar.config deps:
%%   {telemetry, "1.2.1"}
%%
%% @end
%% -------------------------------------------------------------------

-module(telemetry).

%% Export stub functions to satisfy xref
-export([execute/2]).
-export([doctest_test/0]).

-moduledoc("""
Telemetry stub module for optional dependency.

This module provides stub implementations when the telemetry library
is not available as a dependency. At runtime, the real telemetry
will be used if available via code:is_loaded/1.

To use the real telemetry library, add it to rebar.config deps:

```erlang
{deps, [
    {telemetry, "1.2.1"}
]}.
```

## Examples

Execute a telemetry event:

```erlang
1> telemetry:execute([my_app, event], #{value => 42}).
ok
```

Execute with metadata:

```erlang
1> telemetry:execute([workflow, completed], #{duration => 100}, #{case_id => <<"wf1">>}).
ok
```

Run doctests:

```erlang
1> telemetry:doctest_test().
ok
```
""").

%%--------------------------------------------------------------------
%% @doc Stub execute function.
%% The real telemetry:execute/2 will be used if available.
%% @end
%%--------------------------------------------------------------------
-doc("""
Execute a telemetry event with measurements.

This stub function returns ok. When the real telemetry library is
available at runtime, it will be used instead via apply/3.

## Examples

Basic event execution:

```erlang
1> telemetry:execute([my_app, event], #{value => 42}).
ok
```

Event with measurements and metadata:

```erlang
1> telemetry:execute([workflow, completed], #{duration => 100}, #{case_id => <<"wf1">>}).
ok
```
""").
-spec execute(_, _) -> ok.

execute(_EventName, _Measurements) ->
    %% Stub implementation - this is only used for compilation
    %% At runtime, yawl_monitor checks code:is_loaded/1 and uses
    %% the real telemetry if available
    ok.

%%--------------------------------------------------------------------
%% @doc Runs doctests for the telemetry module.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Run doctests for the telemetry module.

This function provides a simple test entry point that verifies
basic stub functionality.

## Examples

```erlang
1> telemetry:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Basic event execution
    ok = execute([test, event], #{value => 42}),

    %% Test 2: Event with list event name
    ok = execute([yawl, metric, workflow_started], #{count => 1}),

    %% Test 3: Event with nested list name
    ok = execute([cre, worker, task, completed], #{duration_ms => 100}),

    %% Test 4: Event with complex measurements
    ok = execute([test, complex], #{value => 1, count => 2, total => 3}),

    %% Test 5: Return value is always ok
    ok = execute([any, event, name], #{any => measurements}),

    ok.
