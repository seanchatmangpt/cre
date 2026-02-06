%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%% @doc CRE Master Process with Petri Net Worker Pool Management
%%
%% This module is the central coordinator for worker pools and task
%% distribution in CRE. It uses Petri net marking algebra for tracking
%% worker availability and task lifecycle tokens.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Marking-Based Pool:</b> Uses pnet_marking for worker state</li>
%%   <li><b>Deterministic Choice:</b> Uses pnet_choice for reproducible selection</li>
%%   <li><b>Task Lifecycle:</b> Uses wf_task constructors for task events</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Start a named CRE master instance
%% {ok, MasterPid} = cre_master:start_link(my_cre),
%%
%% %% Get status of the CRE instance
%% Status = cre_master:get_status(my_cre),
%% #{cre_info := #{load := Load, n_wrk := WorkerCount}} = Status.
%% ```
%%
%% ```erlang
%% %% Start anonymous CRE master
%% {ok, Pid} = cre_master:start_link(),
%%
%% %% Stop the instance
%% cre_master:stop(Pid).
%% ```
%%
%% @end
-moduledoc("""
CRE Master Process with Petri Net Worker Pool Management.

This module is the central coordinator for worker pools and task
distribution in CRE. It uses Petri net marking algebra for tracking
worker availability and task lifecycle tokens.

## Key Features

- **Marking-Based Pool**: Uses `pnet_marking` for worker state tracking
- **Deterministic Choice**: Uses `pnet_choice` for reproducible worker selection
- **Task Lifecycle**: Uses `wf_task` constructors for task event tokens

## Examples

Start a named CRE master instance and check status:

```erlang
1> {ok, Pid} = cre_master:start_link(my_cre).
{ok,<0.123.0>}
2> Status = cre_master:get_status(my_cre).
#{...}
3> Status#{cre_info => #{n_wrk => 0}}.
#{cre_info => #{load => 0.0, n_wrk => 0}, ...}
```

Start an anonymous CRE master:

```erlang
1> {ok, Pid} = cre_master:start_link().
{ok,<0.456.0>}
2> cre_master:stop(Pid).
ok
```
""").
%% -------------------------------------------------------------------

-module(cre_master).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/0, start_link/1,
         add_worker/2,
         worker_result/4,
         cre_request/4,
         stop/1,
         get_status/1,
         get_history/1,
         doctest_test/0]).

%% gen_server callback functions
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Record definitions
%%====================================================================

%% Place atoms for Petri net marking
-define(P_IDLE_WORKERS, 'p_idle_workers').
-define(P_BUSY_WORKERS, 'p_busy_workers').
-define(P_PENDING_TASKS, 'p_pending_tasks').

-record(cre_state, {
          subscr_map = #{},
          worker_marking,
          rng_state,
          busy_map = #{},
          queue = [],
          cache = #{}}).

%%====================================================================
%% API functions
%%====================================================================


%% @doc Starts an anonymous CRE instance.
%%
%%      Returns `{ok, Pid}' on success where `Pid' is the process id of the
%%      newly created process.
%%
%% @see start_link/1
%%
-doc("""
Starts an anonymous CRE instance.

Returns `{ok, Pid}' on success where `Pid' is the process id of the
newly created process. The master is started with an empty worker pool.

## Example

```erlang
1> {ok, Pid} = cre_master:start_link().
{ok,<0.123.0>}
2> Status = cre_master:get_status(Pid).
#{cre_info := #{load := 0.0, n_wrk := 0}, ...}.
```
""").
start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% @doc Starts a named CRE instance.
%%
%%      Returns `{ok, Pid}' on success where `Pid' is the process id of the
%%      newly created process.
%%
%% @see start_link/0
%%
-doc("""
Starts a named CRE instance.

Returns `{ok, Pid}' on success where `Pid' is the process id of the
newly created process. The instance is registered locally under `CreName`.

## Example

```erlang
1> {ok, Pid} = cre_master:start_link(my_cre).
{ok,<0.456.0>}
2> Status = cre_master:get_status(my_cre).
#{...}.
```
""").
start_link(CreName) ->
    gen_server:start_link(CreName, ?MODULE, [], []).


%% @doc Registers a worker process with a given CRE instance.
%%
%% Takes the name of a CRE instance `CreName' and the name of a worker
%% instance `WorkerName' and adds the worker to the worker pool of the CRE.
%% The presence of workers is a precondition for the CRE to send out demand
%% or perform work. A CRE without workers, thus, can accept clients but can
%% never make progress.
%%
%% The worker is added to the idle worker pool using Petri net marking algebra.
%% The master links to the worker process to detect failures.
%%
%% ```erlang
%% 1> {ok, Master} = cre_master:start_link(my_cre).
%% {ok,<0.123.0>}
%% 2> {ok, Worker} = cre_worker:start_link(my_worker).
%% {ok,<0.456.0>}
%% 3> cre_master:add_worker(my_cre, my_worker).
%% ok
%% 4> Status = cre_master:get_status(my_cre).
%% #{cre_info := #{n_wrk := 1}} = Status.
%% ```
%%
-doc("""
Registers a worker process with a given CRE instance.

The worker is added to the idle worker pool using Petri net marking algebra.
Workers are required for task execution - a CRE without workers cannot make progress.

## Example

```erlang
1> {ok, Master} = cre_master:start_link(my_cre).
{ok,<0.123.0>}
2> {ok, Worker} = cre_worker:start_link(my_worker).
{ok,<0.456.0>}
3> cre_master:add_worker(my_cre, my_worker).
ok
4> Status = cre_master:get_status(my_cre).
#{cre_info := #{n_wrk := 1}} = Status.
```
""").
add_worker(CreName, WorkerName) ->
    gen_server:cast(CreName, {add_worker, WorkerName}).


%% @doc Sends the result of a previously computed application to the CRE.
%%
%%      When a worker has computed the result of an application that has
%%      previously been requested from it the worker sends the result back to
%%      the CRE using this function.
%%
%% The worker is transitioned from busy to idle in the Petri net marking.
%% Subscribers to the application are notified with the result.
%%
%% ```erlang
%% 1> App = #{app_id => <<"test_app">>, lambda => #{lambda_name => <<"test_lambda">>}}.
%% 2> Delta = #{result => ok, stdout => <<"done">>}.
%% 3> cre_master:worker_result(my_cre, my_worker, App, Delta).
%% ok
%% ```
%%
-doc("""
Sends the result of a previously computed application to the CRE.

The worker is transitioned from busy to idle in the Petri net marking.
Subscribers to the application are notified with the result.

## Example

```erlang
1> App = #{app_id => <<"test_app">>,
            lambda => #{lambda_name => <<"test_lambda">>}}.
2> Delta = #{result => ok, stdout => <<"done">>}.
3> cre_master:worker_result(my_cre, my_worker, App, Delta).
ok
```
""").
worker_result(CreName, WorkerName, A, Delta) ->
    gen_server:cast(CreName, {worker_result, WorkerName, A, Delta}).


%% @doc Requests the computation of an application from a given CRE intance.
%%
%%      When a client with the name `ClientName' that has received demand has
%%      generated an application `A' belonging to a program with the program
%%      identifier `I' it uses this function to send the application to the CRE
%%      instance with the name `CreName'.
%%
%% The request is queued if no workers are available, or dispatched immediately
%% if idle workers exist. Cached results are returned immediately.
%%
%% ```erlang
%% 1> App = #{app_id => <<"my_app">>, lambda => #{lambda_name => <<"my_lambda">>}}.
%% 2> cre_master:cre_request(my_cre, my_client, prog_id, App).
%% ok
%% ```
%%
-doc("""
Requests the computation of an application from a given CRE instance.

The request is queued if no workers are available, or dispatched immediately
if idle workers exist. Cached results are returned immediately without queuing.

## Example

```erlang
1> App = #{app_id => <<"my_app">>,
            lambda => #{lambda_name => <<"my_lambda">>}}.
2> cre_master:cre_request(my_cre, my_client, prog_id, App).
ok
```
""").
cre_request(CreName, ClientName, I, A) ->
    gen_server:cast(CreName, {cre_request, ClientName, I, A}).


%% @doc Stops the CRE instance.
%%
-doc("""
Stops the CRE instance.

Terminates the gen_server and unlinks all workers.
""").
stop(CreName) ->
    gen_server:stop(CreName).


%% @doc Gets the current status of the CRE instance.
%%
%% Returns a map containing CRE info, node info, and app info.
%%
%% ```erlang
%% 1> Status = cre_master:get_status(my_cre).
%% #{cre_info := #{load := Load, n_wrk := N},
%%   node_info := NodeInfo,
%%   app_info := #{queued := Queued, active := Active, complete := Complete}}.
%% '''
%%
-doc("""
Gets the current status of the CRE instance.

Returns a map containing:
- `cre_info`: Overall load and worker count
- `node_info`: Per-node worker distribution and load
- `app_info`: Queued, active, and completed applications

## Example

```erlang
1> Status = cre_master:get_status(my_cre).
#{cre_info := #{load := 0.0, n_wrk := 2},
  node_info := [#{node := <<\"nonode@nohost\">>, n_wrk := 2, load := 0.0}],
  app_info := #{queued := [], active := [], complete := []}}.
```
""").
-spec get_status(CreName :: _) ->
          #{
            cre_info => #{
                          load => float(),
                          n_wrk => non_neg_integer()
                         },
            node_info => [#{
                            node => binary(),
                            load => float(),
                            n_wrk => pos_integer()
                           }],
            app_info => #{
                          queued => [#{
                                       app_id => binary(),
                                       lambda_name => binary()
                                      }],
                          active => [#{
                                       app_id => binary(),
                                       lambda_name => binary(),
                                       node => binary()
                                      }],
                          complete => [#{
                                         app_id => binary(),
                                         lambda_name => binary(),
                                         node => binary(),
                                         status => binary()
                                        }]
                         }
           }.

get_status(CreName) ->
    {ok, Info} = gen_server:call(CreName, get_status),
    Info.


%% @doc Gets the execution history of completed applications.
%%
%% Returns a map containing the history of all completed applications.
%%
%% ```erlang
%% 1> History = cre_master:get_history(my_cre).
%% #{history := [#{app := App1, delta := Delta1}, ...]}.
%% '''
%%
-doc("""
Gets the execution history of completed applications.

Returns a map with a `history` key containing a list of completed applications
and their results.

## Example

```erlang
1> History = cre_master:get_history(my_cre).
#{history := [#{app := #{app_id := <<\"app1\">>},
                delta := #{result := ok}}]}.
```
""").
-spec get_history(CreName :: _) ->
          #{history => [#{app => _, delta => _}]}.

get_history(CreName) ->
    {ok, HistoryMap} = gen_server:call(CreName, get_history),
    HistoryMap.


%%====================================================================
%% gen_server callback functions
%%====================================================================


code_change(_OldVsn, CreState, _Extra) -> {ok, CreState}.


terminate(_Reason, _CreState) -> ok.


init(_Arg) ->
    process_flag(trap_exit, true),

    %% Initialize persistent terms for configuration if not already done
    %% This ensures tests that call cre_master:start_link() directly
    %% still have access to configuration values
    case catch persistent_term:get(cre_client_poll_interval) of
        {'EXIT', _} -> cre_config:init();
        _ -> ok
    end,

    %% Initialize worker pool marking with Petri net places
    WorkerMarking = pnet_marking:new([?P_IDLE_WORKERS, ?P_BUSY_WORKERS, ?P_PENDING_TASKS]),

    %% Initialize deterministic RNG for worker selection
    RngState = pnet_choice:seed(erlang:timestamp()),

    {ok, #cre_state{
        worker_marking = WorkerMarking,
        rng_state = RngState
    }}.


handle_cast({add_worker, P}, CreState) ->

    #cre_state{worker_marking = Marking, rng_state = RngState} = CreState,

    % extract pid because link/1 cannot deal with registered names
    Pid =
        if
            is_pid(P) -> P;
            true -> whereis(P)
        end,

    %% Get current counts for logging
    {ok, IdleWorkers} = pnet_marking:get(Marking, ?P_IDLE_WORKERS),
    {ok, BusyWorkers} = pnet_marking:get(Marking, ?P_BUSY_WORKERS),
    NIdle = length(IdleWorkers),
    NBusy = length(BusyWorkers),

    logger:info("New worker: pid=~p node=~p count=~p",
                [Pid, node(Pid), NIdle + NBusy + 1],
                [{info, "new worker"}, {application, cre}]),

    true = link(Pid),

    %% Add worker token to idle place using marking algebra
    Marking1 = pnet_marking:add(Marking, #{?P_IDLE_WORKERS => [Pid]}),

    CreState1 = CreState#cre_state{worker_marking = Marking1},

    CreState2 = attempt_progress(CreState1),

    {noreply, CreState2};

handle_cast({worker_result, P, A, Delta}, CreState) ->

    Pid =
        if
            is_pid(P) -> P;
            true -> whereis(P)
        end,

    #cre_state{
      subscr_map = SubscrMap,
      worker_marking = Marking,
      busy_map = BusyMap,
      cache = Cache
     } = CreState,

    F =
        fun({Q, I}) ->
                cre_client:cre_reply(Q, I, A, Delta)
        end,

    case maps:get(A, BusyMap, undefined) of

        Pid ->
            %% Notify subscribers
            lists:foreach(F, maps:get(A, SubscrMap)),

            %% Move worker from busy to idle using marking algebra
            %% Take from busy place
            {ok, Marking1} = pnet_marking:take(Marking, #{?P_BUSY_WORKERS => [Pid]}),

            %% Add to idle place
            Marking2 = pnet_marking:add(Marking1, #{?P_IDLE_WORKERS => [Pid]}),

            %% Emit task completion token using wf_task constructor
            {produce, DoneMap} = wf_task:done(A, Delta, ?P_IDLE_WORKERS),

            CreState1 = CreState#cre_state{
                          subscr_map = maps:remove(A, SubscrMap),
                          worker_marking = Marking2,
                          busy_map = maps:remove(A, BusyMap),
                          cache = Cache#{A => Delta}
                         },
            CreState2 = attempt_progress(CreState1),
            {noreply, CreState2};

        _ ->
            {noreply, CreState}

    end;

handle_cast({cre_request, Q, I, A}, CreState) ->

    #cre_state{
      subscr_map = SubscrMap,
      worker_marking = Marking,
      busy_map = BusyMap,
      queue = Queue,
      cache = Cache
     } = CreState,

    %% Check if result is cached
    case maps:is_key(A, Cache) of

        true ->
            cre_client:cre_reply(Q, I, A, maps:get(A, Cache)),
            {noreply, CreState};

        false ->
            SubscrMap1 = SubscrMap#{A => [{Q, I} | maps:get(A, SubscrMap, [])]},

            %% Check if already in pending or busy using marking
            {ok, PendingTasks} = pnet_marking:get(Marking, ?P_PENDING_TASKS),
            IsPending = lists:member(A, PendingTasks),
            IsBusy = maps:is_key(A, BusyMap),

            case IsPending orelse IsBusy of

                true ->
                    {noreply, CreState#cre_state{subscr_map = SubscrMap1}};

                false ->
                    %% Add to pending place using marking algebra
                    Marking1 = pnet_marking:add(Marking, #{?P_PENDING_TASKS => [A]}),

                    %% Emit task enabled token using wf_task constructor
                    {produce, EnabledMap} = wf_task:enabled(A, I, ?P_PENDING_TASKS),

                    Queue1 = [A | Queue],
                    CreState1 = CreState#cre_state{
                                  subscr_map = SubscrMap1,
                                  worker_marking = Marking1,
                                  queue = Queue1
                                 },
                    CreState2 = attempt_progress(CreState1),
                    {noreply, CreState2}

            end

    end;

handle_cast(_Request, CreState) -> {noreply, CreState}.


handle_info({'EXIT', P, _Reason}, CreState) ->

    #cre_state{
      worker_marking = Marking,
      busy_map = BusyMap,
      queue = Queue,
      rng_state = RngState
     } = CreState,

    Pid =
        if
            is_pid(P) -> P;
            true -> whereis(P)
        end,

    %% Check if worker is in idle place
    {ok, IdleWorkers} = pnet_marking:get(Marking, ?P_IDLE_WORKERS),

    case lists:member(Pid, IdleWorkers) of

        % an idle worker died
        true ->

            logger:info("Idle worker down: pid=~p node=~p",
                          [Pid, node(Pid)],
                          [{info, "idle worker down"},
                           {application, cre},
                           {cre_master_pid, self()},
                           {worker_pid, Pid},
                           {nworker, length(IdleWorkers) - 1}]),

            %% Remove from idle place using marking algebra
            {ok, Marking1} = pnet_marking:take(Marking, #{?P_IDLE_WORKERS => [Pid]}),

            CreState1 = CreState#cre_state{worker_marking = Marking1},

            {noreply, CreState1};

        false ->
            case lists:keyfind(Pid, 2, maps:to_list(BusyMap)) of

                % a busy worker died
                {A, Pid} ->

                    logger:info("Busy worker down: pid=~p node=~p",
                                  [Pid, node(Pid)],
                                  [{info, "busy worker down"},
                                   {application, cre},
                                   {cre_master_pid, self()},
                                   {worker_pid, Pid}]),

                    %% Remove from busy place and add app to pending using marking algebra
                    {ok, Marking1} = pnet_marking:take(Marking, #{?P_BUSY_WORKERS => [Pid]}),
                    Marking2 = pnet_marking:add(Marking1, #{?P_PENDING_TASKS => [A]}),

                    %% Emit task failed token using wf_task constructor
                    {produce, FailedMap} = wf_task:failed(A, {worker_down, Pid}, ?P_PENDING_TASKS),

                    CreState1 = CreState#cre_state{
                                  queue = [A | Queue],
                                  worker_marking = Marking2,
                                  busy_map = maps:remove(A, BusyMap)
                                 },
                    CreState2 = attempt_progress(CreState1),
                    {noreply, CreState2};

                % some other linked process died
                false ->

                    logger:info("Exit signal received: from=~p", [Pid],
                                  [{info, "exit signal received"},
                                   {application, cre},
                                   {cre_master_pid, self()},
                                   {from_pid, Pid}]),

                    {stop, exit, CreState}
            end

    end;

handle_info(_Info, CreState) -> {noreply, CreState}.


handle_call(get_status, _From, CreState) ->

    #cre_state{
      worker_marking = Marking,
      busy_map = BusyMap,
      cache = Cache,
      queue = Queue
     } = CreState,

    %% Get worker counts from marking
    {ok, IdleWorkers} = pnet_marking:get(Marking, ?P_IDLE_WORKERS),
    {ok, BusyWorkers} = pnet_marking:get(Marking, ?P_BUSY_WORKERS),

    NIdle = length(IdleWorkers),
    NBusy = length(BusyWorkers),
    N = NBusy + NIdle,

    Ratio =
        case N of
            0 -> 0.0;
            _ -> NBusy / N
        end,

    CreInfoMap = #{load => Ratio, n_wrk => N},

    PidLst = IdleWorkers ++ maps:values(BusyMap),

    F =
        fun(Pid, Acc) ->
                Node = node(Pid),
                L = maps:get(Node, Acc, []),
                Acc#{Node => [Pid | L]}
        end,

    NodeWrkMap = lists:foldl(F, #{}, PidLst),

    IsBusy =
        fun(Pid) ->
                not lists:member(Pid, IdleWorkers)
        end,

    G =
        fun(Node, PLst) ->
                NWrk = length(PLst),
                NodeNBusy = length(lists:filter(IsBusy, PLst)),
                NodeLoad = NodeNBusy / NWrk,
                #{node => Node, n_wrk => NWrk, load => NodeLoad}
        end,

    NodeInfoLst = maps:values(maps:map(G, NodeWrkMap)),

    BinaryToHexString =
        fun(X) when is_binary(X) ->
                list_to_binary(
                  lists:flatten(
                    [ io_lib:format("~2.16.0b", [B]) || <<B>> <= X ]))
        end,

    FormatQueuedTask =
        fun

           % if apps have the form of Cuneiform applications, we can format them
           (#{app_id := AppId, lambda := #{lambda_name := LambdaName}})
              when is_binary(AppId),
                   is_binary(LambdaName) ->
                #{
                  app_id => AppId,
                  lambda_name => LambdaName
                 };

           % generic apps are represented with the last seven digits of their sha224
           (App) ->
                Hash = crypto:hash(sha224, io_lib:format("~w", [App])),
                B = BinaryToHexString(Hash),
                #{
                  app_id => B,
                  lambda_name => <<"na">>
                 }

        end,

    FormatActiveTask =
        fun(App, Pid) ->
                M = FormatQueuedTask(App),
                M#{node => atom_to_binary(node(Pid), utf8)}
        end,

    FormatCompleteTask =
        fun(App) ->
                M = FormatQueuedTask(App),
                #{App := R} = Cache,
                case R of

                    #{result := #{status := Status, node := Node}}
                      when is_binary(Status), is_binary(Node) ->
                        M#{node => Node, status => Status};

                    _ ->
                        M#{node => <<"na">>, status => <<"na">>}

                end
        end,

    QueuedLst = [ FormatQueuedTask(App) || App <- Queue ],
    ActiveLst = [ FormatActiveTask(App, Pid) || {App, Pid} <- maps:to_list(BusyMap) ],
    CompleteLst = [ FormatCompleteTask(App) || App <- maps:keys(Cache) ],

    AppInfoMap = #{
                   queued => QueuedLst,
                   active => ActiveLst,
                   complete => CompleteLst
                  },

    StatusMap = #{
                  cre_info => CreInfoMap,
                  node_info => NodeInfoLst,
                  app_info => AppInfoMap
                 },

    {reply, {ok, StatusMap}, CreState};

handle_call(get_history, _From, CreState) ->

    #cre_state{cache = Cache} = CreState,
    HistoryMap = #{history => [ #{app => A, delta => R} || {A, R} <- maps:to_list(Cache) ]},
    {reply, {ok, HistoryMap}, CreState};

handle_call(_Request, _From, CreState) ->
    {reply, {error, bad_msg}, CreState}.


%%====================================================================
%% Internal functions
%%====================================================================


%% @doc Attempts to make progress by matching pending tasks with idle workers.
%%
%% Uses Petri net marking operations to transition workers from idle to busy
%% and tasks from pending to active. Worker selection uses deterministic
%% choice for reproducible behavior.
%%
-doc("""
Attempts to make progress by matching pending tasks with idle workers.

This is the core internal function that coordinates task dispatch:
- Takes a worker from the idle pool (pnet_marking:take)
- Adds the worker to the busy pool (pnet_marking:add)
- Removes the task from pending queue
- Dispatches the task to the worker (cre_worker:worker_request)
- Emits a task running token (wf_task:running)

Worker selection is deterministic using pnet_choice:pick/2 to ensure
reproducible behavior across runs with the same RNG seed.
""").
-spec attempt_progress(CreState :: #cre_state{}) -> #cre_state{}.

attempt_progress(CreState) ->

    #cre_state{
      worker_marking = Marking,
      rng_state = RngState,
      busy_map = BusyMap,
      queue = Queue
     } = CreState,

    case Queue of

        [] ->
            CreState;

        [A | Queue1] ->
            {ok, IdleWorkers} = pnet_marking:get(Marking, ?P_IDLE_WORKERS),

            case IdleWorkers of

                [] ->
                    CreState;

                [_ | _] ->

                    %% Use pnet_choice:pick/2 for deterministic worker selection
                    {Pid, RngState1} = pnet_choice:pick(IdleWorkers, RngState),

                    %% Marking-based transition: take from idle, add to busy
                    {ok, Marking1} = pnet_marking:take(Marking, #{?P_IDLE_WORKERS => [Pid]}),
                    Marking2 = pnet_marking:add(Marking1, #{?P_BUSY_WORKERS => [Pid]}),

                    %% Take from pending tasks
                    {ok, Marking3} = pnet_marking:take(Marking2, #{?P_PENDING_TASKS => [A]}),

                    %% Emit task running token using wf_task constructor
                    {produce, RunningMap} = wf_task:running(A, Pid, ?P_BUSY_WORKERS),

                    %% Dispatch work to worker
                    cre_worker:worker_request(Pid, A),

                    CreState#cre_state{
                      worker_marking = Marking3,
                      rng_state = RngState1,
                      busy_map = BusyMap#{A => Pid},
                      queue = Queue1
                     }

            end

    end.


%%====================================================================
%% Doctests
%%====================================================================

%% @doc Run doctests for the cre_master module.
%%
%% This function provides a simple test entry point that verifies
%% basic master/worker pool management functionality.
%%
%% ```erlang
%% 1> cre_master:doctest_test().
%% ok
%% '''
%%
-doc("""
Run doctests for the cre_master module.

This function provides a simple test entry point that verifies
basic master/worker pool management functionality including:
- Starting and stopping anonymous and named CRE instances
- Getting status and checking initial state (empty worker pool)
- Getting history (initially empty)

## Example

```erlang
1> cre_master:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Start anonymous CRE master
    {ok, _Pid} = start_link(),

    %% Test 2: Start named CRE master
    CreName = doctest_cre_master,
    case whereis(CreName) of
        undefined -> ok;
        _ -> unregister(CreName)
    end,
    {ok, _NamedPid} = start_link(CreName),

    %% Test 3: Get status of empty CRE instance
    Status = get_status(CreName),
    #{cre_info := #{load := Load, n_wrk := NWrk},
      node_info := NodeInfo,
      app_info := #{queued := Queued, active := Active, complete := Complete}} = Status,
    0.0 = Load,
    0 = NWrk,
    [] = NodeInfo,
    [] = Queued,
    [] = Active,
    [] = Complete,

    %% Test 4: Get history (should be empty)
    History = get_history(CreName),
    #{history := []} = History,

    %% Test 5: Stop named CRE instance
    ok = stop(CreName),

    %% Clean up any remaining processes
    case whereis(CreName) of
        undefined -> ok;
        _ -> unregister(CreName)
    end,

    ok.
