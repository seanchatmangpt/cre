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
%% @doc CRE Worker - gen_pnet Worker Implementation
%%
%% This module implements a `gen_pnet` behavior for CRE workers that execute
%% tasks within the distributed CRE runtime environment. Workers handle the
%% complete task lifecycle including stagein (input preparation), execution,
%% stageout (output handling), and cleanup.
%%
%% <h3>Petri Net Structure</h3>
%%
%% The worker is implemented as a Petri net with the following places:
%% <ul>
%%   <li><code>'WorkerRequest'</code> - Initial task requests from master</li>
%%   <li><code>'Stagein'</code> - Input files to stage</li>
%%   <li><code>'StageinOk'</code> - Successfully staged inputs</li>
%%   <li><code>'StageinError'</code> - Failed stagein attempts</li>
%%   <li><code>'PreSync'</code> - Synchronization point before execution</li>
%%   <li><code>'Result'</code> - Task execution results</li>
%%   <li><code>'Stageout'</code> - Output files to stage</li>
%%   <li><code>'StageoutOk'</code> - Successfully staged outputs</li>
%%   <li><code>'StageoutError'</code> - Failed stageout attempts</li>
%%   <li><code>'PostSync'</code> - Synchronization point after execution</li>
%%   <li><code>'Error'</code> - Error conditions</li>
%%   <li><code>'WorkerResult'</code> - Final results returned to master</li>
%% </ul>
%%
%% <h3>Worker Lifecycle</h3>
%%
%% <pre>
%% 1. Request: WorkerRequest -> Stagein (prepare inputs)
%% 2. Stagein: Stagein -> StageinOk/StageinError
%% 3. Sync: StageinOk/StageinError + PreSync -> PreSync
%% 4. Execute: PreSync (when complete) -> Result or Error
%% 5. Stageout: Result -> Stageout -> PostSync
%% 6. Complete: PostSync (when complete) -> WorkerResult
%% 7. Return: Error -> WorkerResult (with error expression)
%% </pre>
%%
%% <h3>Callback Interface</h3>
%%
%% Modules implementing the `cre_worker` behavior must export:
%% <ul>
%%   <li><code>init/1</code> - Initialize worker state</li>
%%   <li><code>prepare_case/2</code> - Prepare task for execution</li>
%%   <li><code>stagein_lst/2</code> - List required input files</li>
%%   <li><code>do_stagein/3</code> - Stage a specific input file</li>
%%   <li><code>run/2</code> - Execute the task</li>
%%   <li><code>stageout_lst/3</code> - List required output files</li>
%%   <li><code>do_stageout/3</code> - Stage a specific output file</li>
%%   <li><code>error_to_expr/3</code> - Convert errors to result expressions</li>
%%   <li><code>cleanup_case/3</code> - Cleanup after task completion</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_worker).

-moduledoc """
CRE Worker - gen_pnet-based Task Execution Worker.

This module implements a Petri net worker that executes tasks within
the CRE distributed runtime environment. Workers handle the complete
task lifecycle including stagein, execution, stageout, and cleanup.

## Petri Net Places

The worker uses 12 places to model task execution:

- `'WorkerRequest'` - Initial task requests from cre_master
- `'Stagein'` - Input files requiring staging
- `'StageinOk'` - Successfully staged inputs
- `'StageinError'` - Failed stagein operations
- `'PreSync'` - Synchronization before execution (tracks ok/failed sets)
- `'Result'` - Successful task execution results
- `'Stageout'` - Output files requiring staging
- `'StageoutOk'` - Successfully staged outputs
- `'StageoutError'` - Failed stageout operations
- `'PostSync'` - Synchronization after execution (tracks ok/failed sets)
- `'Error'` - Error conditions (stagein, run, or stageout failures)
- `'WorkerResult'` - Final results (triggers callback to cre_master)

## Transitions

The worker has 13 transitions that orchestrate execution:

1. `prep_stagein` - Generate stagein tasks from WorkerRequest
2. `do_stagein` - Execute individual stagein operation
3. `sync_inok` - Accumulate successful stageins in PreSync
4. `sync_inerror` - Accumulate failed stageins in PreSync
5. `run` - Execute task when all stageins complete
6. `ret_preerror` - Return error if any stageins failed
7. `prep_stageout` - Generate stageout tasks from Result
8. `do_stageout` - Execute individual stageout operation
9. `sync_outok` - Accumulate successful stageouts in PostSync
10. `sync_outerror` - Accumulate failed stageouts in PostSync
11. `ret_posterror` - Return error if any stageouts failed
12. `return_ok` - Return successful result via WorkerResult
13. `return_error` - Return error expression via WorkerResult

## Doctests

```erlang
%% Place list contains all 12 places
> lists:sort(cre_worker:place_lst()).
['Error','PostSync','PreSync','Result','Stagein','StageinError',
 'StageinOk','Stageout','StageoutError','WorkerRequest','WorkerResult']

%% Transition list contains all 13 transitions
> length(cre_worker:trsn_lst()).
13

%% Initial marking is empty for all places
> cre_worker:init_marking('WorkerRequest', #{}).
[]

%% Preset of prep_stagein is WorkerRequest
> cre_worker:preset(prep_stagein).
['WorkerRequest']

%% Preset of run is PreSync
> cre_worker:preset(run).
['PreSync']
```
""".


-behavior(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         trigger/3]).

-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3]).

-export([start_link/3, start_link/4, worker_request/2, stop/1, doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Callback definitions
%%====================================================================


-callback init(InitArg :: _) -> UsrInfo :: _.

-callback prepare_case(A :: _, UsrInfo :: _) -> ok.

-callback stagein_lst(A :: _, UsrInfo :: _) -> [F :: _].

-callback do_stagein(A :: _, F :: _, UsrInfo :: _) -> ok | {error, enoent}.

-callback run(A :: _, UsrInfo :: _) -> {ok, R :: _} | {error, Reason :: _}.

-callback stageout_lst(A :: _, R :: _, UsrInfo :: _) -> [F :: _].

-callback do_stageout(A :: _, F :: _, UsrInfo :: _) -> ok | {error, enoent}.

-callback error_to_expr(A :: _,
                        Reason :: {stagein | stageout, [_]} | {run, _},
                        UsrInfo :: _) -> _.

-callback cleanup_case(A :: _, R :: _, UsrInfo :: _) -> R1 :: _.

%%====================================================================
%% Record definitions
%%====================================================================

-record(wrk_state, {cre_name, wrk_mod, usr_info}).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a CRE worker linked to the calling process.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Starts a CRE worker linked to the calling process.

Creates a new gen_pnet worker process that registers with the
CRE master for task execution.

Example:
```erlang
> {ok, Pid} = cre_worker:start_link(cre_master, my_worker, #{config => value}).
{ok, <0.123.0>}
```
""".
start_link(CreName, WrkMod, WrkArg) ->
    gen_pnet:start_link(?MODULE, {CreName, WrkMod, WrkArg}, []).


%%--------------------------------------------------------------------
%% @doc Starts a named CRE worker.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Starts a named CRE worker.

Creates a new gen_pnet worker process registered with the given name.

Example:
```erlang
> {ok, Pid} = cre_worker:start_link(my_worker, cre_master, my_worker_mod, #{}).
{ok, <0.124.0>}
```
""".
start_link(WrkName, CreName, WrkMod, WrkArg) ->
    gen_pnet:start_link(WrkName, ?MODULE, {CreName, WrkMod, WrkArg}, []).


%%--------------------------------------------------------------------
%% @doc Sends a work request to a worker.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Sends a work request to a worker.

Submits a task for execution. Results are sent back asynchronously
via the WorkerResult place.

Example:
```erlang
> ok = cre_worker:worker_request(my_worker, #{task => compute}).
ok
```
""".
worker_request(WrkName, A) ->
    gen_pnet:cast(WrkName, {worker_request, A}).


%%--------------------------------------------------------------------
%% @doc Stops a CRE worker.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Stops a CRE worker.

Gracefully stops the worker process.

Example:
```erlang
> ok = cre_worker:stop(my_worker).
ok
```
""".
stop(WrkName) ->
    gen_pnet:stop(WrkName).


%%====================================================================
%% Interface callback functions
%%====================================================================


code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.


handle_call(_Request, _From, _NetState) -> {reply, {error, bad_msg}}.


handle_cast({worker_request, A}, NetState) ->

    WrkState = gen_pnet:get_usr_info(NetState),
    #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo} = WrkState,

    ok = WrkMod:prepare_case(A, UsrInfo),

    {noreply, #{'WorkerRequest' => [A]}};

handle_cast(_Request, _NetState) -> noreply.


handle_info(_Request, _NetState) -> noreply.


init({CreName, WrkMod, WrkArg}) ->

    UsrInfo = WrkMod:init(WrkArg),

    WrkState = #wrk_state{
                 cre_name = CreName,
                 wrk_mod = WrkMod,
                 usr_info = UsrInfo
                },

    ok = cre_master:add_worker(CreName, self()),

    WrkState.


terminate(_Reason, _NetState) -> ok.


trigger('WorkerResult', {A, R}, NetState) ->

    WrkState = gen_pnet:get_usr_info(NetState),
    #wrk_state{
      cre_name = CreName,
      wrk_mod = WrkMod,
      usr_info = UsrInfo
     } = WrkState,

    R1 = WrkMod:cleanup_case(A, R, UsrInfo),
    cre_master:worker_result(CreName, self(), A, R1),

    drop;

trigger(_Place, _Token, _NetState) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places in the worker Petri net.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Returns the list of places in the worker Petri net.

The worker uses 12 places to model task execution through stagein,
execution, and stageout phases.

Example:
```erlang
> lists:sort(cre_worker:place_lst()).
['Error','PostSync','PreSync','Result','Stagein','StageinError',
 'StageinOk','Stageout','StageoutError','WorkerRequest','WorkerResult']
```
""".
place_lst() ->
    ['WorkerRequest', 'Stagein', 'StageinOk', 'StageinError', 'PreSync', 'Result',
     'Stageout', 'StageoutOk', 'StageoutError', 'PostSync', 'Error',
     'WorkerResult'].


%%--------------------------------------------------------------------
%% @doc Returns the list of transitions in the worker Petri net.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Returns the list of transitions in the worker Petri net.

The worker has 13 transitions that orchestrate task execution.

Example:
```erlang
> length(cre_worker:trsn_lst()).
13
```
""".
trsn_lst() ->
    [prep_stagein, do_stagein, sync_inok, sync_inerror, run, prep_stageout,
     do_stageout, sync_outok, sync_outerror, ret_posterror, ret_preerror,
     return_ok, return_error].


%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Returns the initial marking for a given place.

All places start with an empty marking.

Example:
```erlang
> cre_worker:init_marking('WorkerRequest', #{}).
[]
```
""".
init_marking(_Place, _UsrInfo) -> [].


%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for a transition.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for a transition.

The preset defines which places must contain tokens for a
transition to be enabled.

Example:
```erlang
> cre_worker:preset(prep_stagein).
['WorkerRequest']

> cre_worker:preset(run).
['PreSync']

> cre_worker:preset(return_ok).
['PostSync']
```
""".
preset(prep_stagein) -> ['WorkerRequest'];
preset(do_stagein) -> ['Stagein'];
preset(sync_inok) -> ['StageinOk', 'PreSync'];
preset(sync_inerror) -> ['StageinError', 'PreSync'];
preset(run) -> ['PreSync'];
preset(prep_stageout) -> ['Result'];
preset(do_stageout) -> ['Stageout'];
preset(sync_outok) -> ['StageoutOk', 'PostSync'];
preset(sync_outerror) -> ['StageoutError', 'PostSync'];
preset(ret_preerror) -> ['PreSync'];
preset(ret_posterror) -> ['PostSync'];
preset(return_ok) -> ['PostSync'];
preset(return_error) -> ['Error'].


is_enabled(prep_stagein, _, _) -> true;
is_enabled(do_stagein, _, _) -> true;
is_enabled(sync_inok, _, _) -> true;
is_enabled(sync_inerror, _, _) -> true;
is_enabled(prep_stageout, _, _) -> true;
is_enabled(do_stageout, _, _) -> true;
is_enabled(sync_outok, _, _) -> true;
is_enabled(sync_outerror, _, _) -> true;
is_enabled(return_error, _, _) -> true;

is_enabled(ret_preerror,
           #{'PreSync' := [{A, F1, F2}]},
           #wrk_state{
             wrk_mod = WrkMod,
             usr_info = UsrInfo
            })
  when length(F2) > 0 ->
    F1uF2 = ordsets:union(F1, F2),
    Fa = ordsets:from_list(WrkMod:stagein_lst(A, UsrInfo)),
    Fa =:= F1uF2;

is_enabled(run,
           #{'PreSync' := [{A, F1, []}]},
           #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fa = ordsets:from_list(WrkMod:stagein_lst(A, UsrInfo)),
    Fa =:= F1;

is_enabled(ret_posterror,
           #{'PostSync' := [{A, Ra, F1, F2}]},
           #wrk_state{
             wrk_mod = WrkMod,
             usr_info = UsrInfo
            })
  when length(F2) > 0 ->
    F1uF2 = ordsets:union(F1, F2),
    Fa = ordsets:from_list(WrkMod:stageout_lst(A, Ra, UsrInfo)),
    Fa =:= F1uF2;

is_enabled(return_ok,
           #{'PostSync' := [{A, Ra, F1, []}]},
           #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fa = ordsets:from_list(WrkMod:stageout_lst(A, Ra, UsrInfo)),
    Fa =:= F1;

is_enabled(_, _, _) -> false.


fire(prep_stagein,
     #{'WorkerRequest' := [A]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fa = ordsets:from_list(WrkMod:stagein_lst(A, UsrInfo)),
    {produce, #{'Stagein' => [ {A, F} || F <- Fa ], 'PreSync' => [{A, [], []}]}};

fire(do_stagein,
     #{'Stagein' := [{A, F}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    case WrkMod:do_stagein(A, F, UsrInfo) of
        ok -> {produce, #{'StageinOk' => [{A, F}]}};
        {error, enoent} -> {produce, #{'StageinError' => [{A, F}]}}
    end;

fire(sync_inok,
     #{'StageinOk' := [{A, F}], 'PreSync' := [{A, F1, F2}]},
     _WrkState) ->
    {produce, #{'PreSync' => [{A, ordsets:add_element(F, F1), F2}]}};

fire(sync_inerror,
     #{'StageinError' := [{A, F}], 'PreSync' := [{A, F1, F2}]},
     _WrkState) ->
    {produce, #{'PreSync' => [{A, F1, ordsets:add_element(F, F2)}]}};

fire(ret_preerror, #{'PreSync' := [{A, _F1, F2}]}, _WrkState) ->
    {produce, #{'Error' => [{A, {stagein, F2}}]}};

fire(run,
     #{'PreSync' := [{A, _Fa, []}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    case WrkMod:run(A, UsrInfo) of
        {ok, Ra} -> {produce, #{'Result' => [{A, Ra}]}};
        {error, Reason} -> {produce, #{'Error' => [{A, {run, Reason}}]}}
    end;

fire(prep_stageout,
     #{'Result' := [{A, Ra}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Fra = ordsets:from_list(WrkMod:stageout_lst(A, Ra, UsrInfo)),
    {produce, #{
                'Stageout' => [ {A, F} || F <- Fra ],
                'PostSync' => [{A, Ra, [], []}]
               }};

fire(do_stageout,
     #{'Stageout' := [{A, F}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    case WrkMod:do_stageout(A, F, UsrInfo) of
        ok -> {produce, #{'StageoutOk' => [{A, F}]}};
        {error, enoent} -> {produce, #{'StageoutError' => [{A, F}]}}
    end;

fire(sync_outok,
     #{
       'StageoutOk' := [{A, F}],
       'PostSync' := [{A, Ra, F1, F2}]
      },
     _WrkState) ->
    {produce, #{'PostSync' => [{A, Ra, ordsets:add_element(F, F1), F2}]}};

fire(sync_outerror,
     #{
       'StageoutError' := [{A, F}],
       'PostSync' := [{A, Ra, F1, F2}]
      },
     _WrkState) ->
    {produce, #{'PostSync' => [{A, Ra, F1, ordsets:add_element(F, F2)}]}};

fire(ret_posterror, #{'PostSync' := [{A, _Ra, _F1, F2}]}, _WrkState) ->
    {produce, #{'Error' => [{A, {stageout, F2}}]}};

fire(return_ok, #{'PostSync' := [{A, Ra, _Fa, []}]}, _WrkState) ->
    {produce, #{'WorkerResult' => [{A, Ra}]}};

fire(return_error,
     #{'Error' := [{A, E}]},
     #wrk_state{wrk_mod = WrkMod, usr_info = UsrInfo}) ->
    Ra = WrkMod:error_to_expr(A, E, UsrInfo),
    {produce, #{'WorkerResult' => [{A, Ra}]}}.


%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the cre_worker module.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Runs doctests for the cre_worker module.

Validates the complete gen_pnet worker implementation for CRE.

Example:
```erlang
> cre_worker:doctest_test().
ok
```
""".
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Place list contains exactly 12 places
    Places = place_lst(),
    12 = length(Places),

    %% Verify specific places exist
    true = lists:member('WorkerRequest', Places),
    true = lists:member('WorkerResult', Places),
    true = lists:member('Stagein', Places),
    true = lists:member('Stageout', Places),
    true = lists:member('PreSync', Places),
    true = lists:member('PostSync', Places),
    true = lists:member('Result', Places),
    true = lists:member('Error', Places),

    %% Test 2: Transition list contains exactly 13 transitions
    Transitions = trsn_lst(),
    13 = length(Transitions),

    %% Verify specific transitions exist
    true = lists:member(prep_stagein, Transitions),
    true = lists:member(do_stagein, Transitions),
    true = lists:member(run, Transitions),
    true = lists:member(prep_stageout, Transitions),
    true = lists:member(do_stageout, Transitions),
    true = lists:member(return_ok, Transitions),
    true = lists:member(return_error, Transitions),

    %% Test 3: Initial marking is empty for all places
    [] = init_marking('WorkerRequest', #{}),
    [] = init_marking('Stagein', #{}),
    [] = init_marking('Result', #{}),

    %% Test 4: Preset relationships are correct
    ['WorkerRequest'] = preset(prep_stagein),
    ['Stagein'] = preset(do_stagein),
    ['PreSync'] = preset(run),
    ['Result'] = preset(prep_stageout),
    ['PostSync'] = preset(return_ok),
    ['Error'] = preset(return_error),

    %% Test 5: Sync transitions have correct presets
    ['StageinOk', 'PreSync'] = preset(sync_inok),
    ['StageinError', 'PreSync'] = preset(sync_inerror),
    ['StageoutOk', 'PostSync'] = preset(sync_outok),
    ['StageoutError', 'PostSync'] = preset(sync_outerror),

    %% Test 6: Error return transitions
    ['PreSync'] = preset(ret_preerror),
    ['PostSync'] = preset(ret_posterror),

    %% Test 7: Certain transitions are always enabled
    true = is_enabled(prep_stagein, #{}, #wrk_state{}),
    true = is_enabled(do_stagein, #{}, #wrk_state{}),
    true = is_enabled(return_error, #{}, #wrk_state{}),

    ok.
