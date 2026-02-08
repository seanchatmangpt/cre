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
%% @doc CRE Supervisor Module
%%
%% This module implements the top-level supervisor for the CRE application.
%% It manages the core worker processes including cre_master, yawl_timeout,
%% yawl_xes, and yawl_approval using the one_for_one restart strategy.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Top-Level Supervisor:</b> Root supervisor for all CRE processes</li>
%%   <li><b>Child Specs:</b> Manages cre_master, yawl_timeout, yawl_xes, yawl_approval</li>
%%   <li><b>One-For-One Strategy:</b> Each child is restarted independently</li>
%%   <li><b>Zero Intensity:</b> No automatic restarts (manual recovery only)</li>
%% </ul>
%%
%% <h3>Child Specifications</h3>
%%
%% The supervisor manages four child processes:
%%
%% <ul>
%%   <li><b>cre_master:</b> Central coordinator for worker pools and task scheduling</li>
%%   <li><b>yawl_timeout:</b> Timeout and cancellation infrastructure for workflows</li>
%%   <li><b>yawl_xes:</b> XES logging for process mining compliance</li>
%%   <li><b>yawl_approval:</b> Human-in-the-loop approval workflow support</li>
%% </ul>
%%
%% <h3>Supervisor Flags</h3>
%%
%% <ul>
%%   <li><b>Strategy:</b> one_for_one - only the terminated child is restarted</li>
%%   <li><b>Intensity:</b> 0 - no restarts allowed (manual intervention required)</li>
%%   <li><b>Period:</b> 5 - time window for intensity calculation (seconds)</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Module can be loaded:
%% ```erlang
%% 1> {module, cre_sup} = code:ensure_loaded(cre_sup).
%% {module, cre_sup}
%% ```
%%
%% Module exports start_link/0:
%% ```erlang
%% 1> {module, cre_sup} = code:ensure_loaded(cre_sup),
%% 1> {exports, _} = lists:keyfind(exports, 1, module:module_info(cre_sup)),
%% 1> true = lists:keymember(start_link, 1, cre_sup:module_info(exports)).
%% true
%% ```
%%
%% Module exports init/1:
%% ```erlang
%% 1> true = lists:keymember(init, 1, cre_sup:module_info(exports)).
%% true
%% ```
%%
%% Supervisor flags are correctly configured:
%% ```erlang
%% 1> {ok, {#{strategy := one_for_one, intensity := 0, period := 5}, _Children}}
%% 1>     = cre_sup:init([]).
%% {ok,{#{intensity => 0,period => 5,strategy => one_for_one}, [...]}}
%% ```
%%
%% Child specs contain required fields:
%% ```erlang
%% 1> {ok, {_Flags, Children}} = cre_sup:init([]),
%% 1> true = is_list(Children),
%% 1> 7 = length(Children).
%% 7
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_sup).
-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0]).
-export([init/1]).
-export([doctest_test/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the CRE supervisor.
%%
%%      The supervisor is registered locally as `cre_sup' and uses the
%%      one_for_one restart strategy with zero intensity (no auto-restart).
%%
%%      Returns `{ok, Pid}' when the supervisor starts successfully.
%%      Returns `{error, {already_started, Pid}}' if already running.
%%      Returns `{error, Reason}' if startup fails.
%%
%% @returns `{ok, Pid}' | `{error, already_started} | `{error, Reason}'
%%
%% @see supervisor:start_link/3
%%
-doc("""
Starts the CRE supervisor.

The supervisor is registered locally as `cre_sup` and uses the one_for_one
restart strategy with zero intensity (manual restart only).

## Example

```erlang
1> cre_sup:start_link().
{ok,<0.123.0>}
```

If already started:
```erlang
1> cre_sup:start_link().
{error,{already_started,<0.123.0>}}
```
""").
-spec start_link() -> {ok, pid()} | {error, _}.

start_link() ->
    supervisor:start_link({local, cre_sup}, ?MODULE, []).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

%% @doc Supervisor initialization callback.
%%
%%      Defines the supervisor flags and child specifications for the
%%      CRE application supervision tree.
%%
%%      <h4>Supervisor Flags</h4>
%%      <ul>
%%        <li><b>strategy:</b> one_for_one - only terminated child is restarted</li>
%%        <li><b>intensity:</b> 0 - no restarts allowed (manual recovery)</li>
%%        <li><b>period:</b> 5 - seconds for intensity calculation window</li>
%%      </ul>
%%
%%      <h4>Child Specifications</h4>
%%      <ul>
%%        <li><b>cre_master:</b> temporary restart, 5s shutdown timeout</li>
%%        <li><b>yawl_timeout:</b> permanent restart, 5s shutdown timeout</li>
%%        <li><b>yawl_xes:</b> permanent restart, 5s shutdown timeout</li>
%%        <li><b>yawl_approval:</b> permanent restart, 5s shutdown timeout</li>
%%      </ul>
%%
%% @param _Args Unused (empty list)
%% @returns `{ok, {SupFlags, [ChildSpec, ...]}}'
%%
%% @see supervisor:init/1
%%
-doc("""
Supervisor initialization callback.

Defines supervisor flags and child specifications for the CRE supervision tree.

## Supervisor Flags

- **strategy**: `one_for_one` - only the terminated child is restarted
- **intensity**: `0` - no automatic restarts (manual recovery required)
- **period**: `5` - time window in seconds for intensity calculation

## Child Specifications

Returns a list of 4 child specifications:
- `cre_master` - temporary restart, 5000ms shutdown
- `yawl_timeout` - permanent restart, 5000ms shutdown
- `yawl_xes` - permanent restart, 5000ms shutdown
- `yawl_approval` - permanent restart, 5000ms shutdown

## Example

```erlang
1> {ok, {#{strategy := one_for_one}, Children}} = cre_sup:init([]),
1> 4 = length(Children).
4
```
""").
-spec init(_) -> {ok, {#{
                         strategy => one_for_one,
                         intensity => non_neg_integer(),
                         period => pos_integer()
                        },
                       [#{id := _, start := {atom(), atom(), [_]}, restart => _, shutdown => _, type => worker, modules => [atom()]}]}}.

init(_Args) ->

    SupFlags = #{
                 strategy => one_for_one,
                 intensity => 0,
                 period => 5
                },

    ChildSpec = #{
                  id => cre_master,
                  start => {cre_master, start_link, [cre_master]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [cre_master]
                 },

    TimeoutSpec = #{
                    id => yawl_timeout,
                    start => {yawl_timeout, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [yawl_timeout]
                   },

    XesSpec = #{
               id => yawl_xes,
               start => {yawl_xes, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [yawl_xes]
              },

    ApprovalSpec = #{
                    id => yawl_approval,
                    start => {yawl_approval, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [yawl_approval]
                   },

    WorkflowSupSpec = #{
                       id => yawl_workflow_supervisor,
                       start => {yawl_workflow_supervisor, start_link, []},
                       restart => permanent,
                       shutdown => infinity,
                       type => supervisor,
                       modules => [yawl_workflow_supervisor]
                      },

    WorklistSpec = #{
                    id => yawl_worklist,
                    start => {yawl_worklist, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [yawl_worklist]
                   },

    RegistrySpec = #{
                    id => yawl_registry,
                    start => {yawl_registry, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [yawl_registry]
                   },

    {ok, {SupFlags, [ChildSpec, TimeoutSpec, XesSpec, ApprovalSpec, WorkflowSupSpec, WorklistSpec, RegistrySpec]}}.

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Runs doctests for the cre_sup module.
%%
%%      Executes fast, minimal tests that verify supervisor behavior
%%      without starting the full application.
%%
%% @returns `ok'
%%
%% @end
%%
-doc("""
Runs doctests for the cre_sup module.

Executes fast, minimal tests verifying basic supervisor behavior without
starting the full application.

## Example

```erlang
1> cre_sup:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Verify module can be loaded
    {module, cre_sup} = code:ensure_loaded(cre_sup),

    %% Test 2: Verify start_link/0 is exported
    Exports = proplists:get_value(exports, module_info()),
    true = lists:keymember(start_link, 1, Exports),

    %% Test 3: Verify init/1 is exported
    true = lists:keymember(init, 1, Exports),

    %% Test 4: Verify init returns proper supervisor flags
    {ok, {SupFlags, _Children}} = init([]),
    one_for_one = maps:get(strategy, SupFlags),
    0 = maps:get(intensity, SupFlags),
    5 = maps:get(period, SupFlags),

    %% Test 5: Verify child specs count
    {ok, {_, Children}} = init([]),
    true = is_list(Children),
    7 = length(Children),

    %% Test 6: Verify child specs have required fields
    [
        begin
            true = is_map(Child),
            true = maps:is_key(id, Child),
            true = maps:is_key(start, Child),
            true = maps:is_key(restart, Child),
            true = maps:is_key(shutdown, Child),
            true = maps:is_key(type, Child),
            true = maps:is_key(modules, Child)
        end
        || Child <- Children
    ],

    %% Test 7: Verify specific child IDs exist
    {ok, {_, ChildrenList}} = init([]),
    ChildIds = [maps:get(id, C) || C <- ChildrenList],
    true = lists:member(cre_master, ChildIds),
    true = lists:member(yawl_timeout, ChildIds),
    true = lists:member(yawl_xes, ChildIds),
    true = lists:member(yawl_approval, ChildIds),
    true = lists:member(yawl_workflow_supervisor, ChildIds),
    true = lists:member(yawl_worklist, ChildIds),
    true = lists:member(yawl_registry, ChildIds),

    %% Test 8: Verify cre_master spec details
    {ok, {_, Children2}} = init([]),
    CreMasterSpec = lists:keyfind(cre_master, 1, [{maps:get(id, C), C} || C <- Children2]),
    {cre_master, CreMasterMap} = CreMasterSpec,
    {cre_master, start_link, [cre_master]} = maps:get(start, CreMasterMap),
    temporary = maps:get(restart, CreMasterMap),
    5000 = maps:get(shutdown, CreMasterMap),
    worker = maps:get(type, CreMasterMap),
    [cre_master] = maps:get(modules, CreMasterMap),

    %% Test 9: Verify permanent restart strategy for support services
    {ok, {_, Children3}} = init([]),
    TimeoutSpec2 = proplists:get_value(yawl_timeout,
        [{maps:get(id, C), C} || C <- Children3]),
    permanent = maps:get(restart, TimeoutSpec2),
    worker = maps:get(type, TimeoutSpec2),

    %% Test 10: Verify all children are workers (not supervisors)
    {ok, {_, Children4}} = init([]),
    true = lists:all(fun(C) -> worker =:= maps:get(type, C) end, Children4),

    ok.
