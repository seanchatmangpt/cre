%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @copyright 2015-2025
%%
%% @doc WF Substrate Supervisor
%%
%% This module implements the top-level supervisor for the WF substrate.
%% It manages the case runner supervisor and optional effect supervisor
%% using the one_for_one restart strategy.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Substrate Supervisor:</b> Root supervisor for WF execution</li>
%%   <li><b>Child Specs:</b> Manages wf_case_sup and optionally wf_effect_sup</li>
%%   <li><b>One-For-One Strategy:</b> Each child is restarted independently</li>
%%   <li><b>Standard Intensity:</b> 10 restarts per 60 seconds</li>
%% </ul>
%%
%% <h3>Child Specifications</h3>
%%
%% The supervisor manages:
%%
%% <ul>
%%   <li><b>wf_case_sup:</b> simple_one_for_one supervisor for case runners</li>
%%   <li><b>wf_effect_sup:</b> (optional) supervisor for async effect execution</li>
%% </ul>
%%
%% <h3>Supervisor Flags</h3>
%%
%% <ul>
%%   <li><b>Strategy:</b> one_for_one - only the terminated child is restarted</li>
%%   <li><b>Intensity:</b> 10 - max 10 restarts per period</li>
%%   <li><b>Period:</b> 60 - time window for intensity calculation (seconds)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_substrate_sup).
-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0, start_link/1]).
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the WF substrate supervisor with default options.
%%
%%      The supervisor is registered locally as `wf_substrate_sup' and uses
%%      the one_for_one restart strategy.
%%
%%      Returns `{ok, Pid}' when the supervisor starts successfully.
%%      Returns `{error, {already_started, Pid}}' if already running.
%%      Returns `{error, Reason}' if startup fails.
%%
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
%% @see supervisor:start_link/3
%%
-doc("""
Starts the WF substrate supervisor with default options.

The supervisor is registered locally as `wf_substrate_sup` and uses the
one_for_one restart strategy.

## Example

```erlang
1> wf_substrate_sup:start_link().
{ok,<0.123.0>}
```

If already started:
```erlang
1> wf_substrate_sup:start_link().
{error,{already_started,<0.123.0>}}
```
""").
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link([]).

%% @doc Starts the WF substrate supervisor with options.
%%
%%      Options:
%%      - `{enable_effects, boolean()}' - whether to start wf_effect_sup (default: false)
%%
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
-doc("""
Starts the WF substrate supervisor with options.

## Options

- `{enable_effects, boolean()}` - whether to start wf_effect_sup (default: false)

## Example

```erlang
1> wf_substrate_sup:start_link([{enable_effects, true}]).
{ok,<0.123.0>}
```
""").
-spec start_link([{enable_effects, boolean()}]) -> {ok, pid()} | {error, term()}.

start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

%% @doc Supervisor initialization callback.
%%
%%      Defines the supervisor flags and child specifications for the
%%      WF substrate supervision tree.
%%
%%      <h4>Supervisor Flags</h4>
%%      <ul>
%%        <li><b>strategy:</b> one_for_one - only terminated child is restarted</li>
%%        <li><b>intensity:</b> 10 - max 10 restarts per period</li>
%%        <li><b>period:</b> 60 - seconds for intensity calculation window</li>
%%      </ul>
%%
%%      <h4>Child Specifications</h4>
%%      <ul>
%%        <li><b>wf_case_sup:</b> permanent restart, infinity shutdown (supervisor)</li>
%%        <li><b>wf_effect_sup:</b> permanent restart, infinity shutdown (optional)</li>
%%      </ul>
%%
%% @param Opts Options list with `{enable_effects, boolean()}'
%% @returns `{ok, {SupFlags, [ChildSpec, ...]}}'
%%
%% @see supervisor:init/1
%%
-doc("""
Supervisor initialization callback.

Defines supervisor flags and child specifications for the WF substrate tree.

## Supervisor Flags

- **strategy**: `one_for_one` - only the terminated child is restarted
- **intensity**: `10` - max 10 restarts per 60 seconds
- **period**: `60` - time window in seconds for intensity calculation

## Child Specifications

Returns a list of child specifications:
- `wf_case_sup` - permanent restart, infinity shutdown (supervisor)
- `wf_effect_sup` - permanent restart, infinity shutdown (optional, supervisor)

## Example

```erlang
1> {ok, {#{strategy := one_for_one}, Children}} = wf_substrate_sup:init([]),
1> length(Children) >= 1.
true
```
""").
-spec init([{enable_effects, boolean()}]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(Opts) ->
    EnableEffects = proplists:get_value(enable_effects, Opts, false),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    CaseSup = #{
        id => wf_case_sup,
        start => {wf_case_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [wf_case_sup]
    },

    EffectSup = #{
        id => wf_effect_sup,
        start => {wf_effect_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [wf_effect_sup]
    },

    Children = case EnableEffects of
        true -> [CaseSup, EffectSup];
        false -> [CaseSup]
    end,

    {ok, {SupFlags, Children}}.
