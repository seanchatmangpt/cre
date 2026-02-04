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
%% @doc YAWL Load Balancer for CRE Workers.
%%
%% This module implements load balancing strategies for distributing
%% YAWL workflow tasks across CRE worker processes.
%%
%% <h3>Supported Strategies</h3>
%%
%% <ul>
%%   <li><b>round_robin:</b> Distributes tasks evenly across workers</li>
%%   <li><b>random:</b> Randomly selects a worker</li>
%%   <li><b>least_loaded:</b> Selects worker with fewest active tasks</li>
%%   <li><b>weighted:</b> Considers worker capacity weights</li>
%%   <li><b>affinity:</b> Routes to worker based on data/task affinity</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_balancer).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Balancer API
-export([start_link/0, start_link/1, stop/0]).
-export([register_worker/2, unregister_worker/1]).
-export([select_worker/1, select_worker/2]).
-export([get_worker_load/1, get_all_workers/0]).
-export([set_strategy/1, get_strategy/0]).
-export([update_worker_capacity/2, update_worker_weight/2]).
-export([get_load_distribution/0]).
-export([reset_worker_load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Records
%%====================================================================

-record(worker, {
    pid :: pid(),
    node :: node(),
    capacity = 10 :: pos_integer(),
    weight = 1 :: pos_integer(),
    active_tasks = 0 :: non_neg_integer(),
    total_tasks = 0 :: non_neg_integer(),
    affinity_tags = [] :: [binary()],
    last_heartbeat :: erlang:timestamp()
}).

-record(balancer_state, {
    workers = #{} :: #{pid() => #worker{}},
    strategy = round_robin :: balance_strategy(),
    round_robin_index = 0 :: non_neg_integer(),
    affinity_map = #{} :: #{binary() => pid()}
}).

%%====================================================================
%% Types
%%====================================================================

-type balance_strategy() ::
    round_robin |
    random |
    least_loaded |
    weighted |
    affinity.

-type worker_info() :: #worker{}.
-type load_distribution() :: #{node() => non_neg_integer()}.

-export_type([balance_strategy/0, worker_info/0, load_distribution/0]).

%%====================================================================
%% Balancer API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the load balancer with default name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link(yawl_balancer).

%%--------------------------------------------------------------------
%% @doc Starts the load balancer with a registered name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the load balancer.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    gen_server:stop(yawl_balancer).

%%--------------------------------------------------------------------
%% @doc Registers a worker with the load balancer.
%%
%% @param WorkerPid The process ID of the worker.
%% @param Options Worker options map (capacity, weight, affinity_tags).
%% @return ok.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_worker(pid(), map()) -> ok.

register_worker(WorkerPid, Options) when is_pid(WorkerPid), is_map(Options) ->
    gen_server:cast(yawl_balancer, {register_worker, WorkerPid, Options}).

%%--------------------------------------------------------------------
%% @doc Unregisters a worker from the load balancer.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_worker(pid()) -> ok.

unregister_worker(WorkerPid) when is_pid(WorkerPid) ->
    gen_server:cast(yawl_balancer, {unregister_worker, WorkerPid}).

%%--------------------------------------------------------------------
%% @doc Selects a worker using the current strategy.
%%
%% @param TaskData Data about the task to distribute.
%% @return {ok, WorkerPid} or {error, no_workers}.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_worker(term()) -> {ok, pid()} | {error, no_workers}.

select_worker(TaskData) ->
    select_worker(TaskData, #{}).

%%--------------------------------------------------------------------
%% @doc Selects a worker using the current strategy with options.
%%
%% @param TaskData Data about the task to distribute.
%% @param Options Selection options map.
%% @return {ok, WorkerPid} or {error, no_workers}.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_worker(term(), map()) -> {ok, pid()} | {error, no_workers}.

select_worker(TaskData, Options) when is_map(Options) ->
    gen_server:call(yawl_balancer, {select_worker, TaskData, Options}).

%%--------------------------------------------------------------------
%% @doc Gets the current load of a specific worker.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_worker_load(pid()) -> {ok, non_neg_integer()} | {error, not_found}.

get_worker_load(WorkerPid) when is_pid(WorkerPid) ->
    gen_server:call(yawl_balancer, {get_worker_load, WorkerPid}).

%%--------------------------------------------------------------------
%% @doc Gets information about all registered workers.
%%
%% @return List of worker info maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_all_workers() -> [map()].

get_all_workers() ->
    gen_server:call(yawl_balancer, get_all_workers).

%%--------------------------------------------------------------------
%% @doc Sets the load balancing strategy.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_strategy(balance_strategy()) -> ok.

set_strategy(Strategy) when Strategy =:= round_robin;
                            Strategy =:= random;
                            Strategy =:= least_loaded;
                            Strategy =:= weighted;
                            Strategy =:= affinity ->
    gen_server:cast(yawl_balancer, {set_strategy, Strategy}).

%%--------------------------------------------------------------------
%% @doc Gets the current load balancing strategy.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_strategy() -> balance_strategy().

get_strategy() ->
    gen_server:call(yawl_balancer, get_strategy).

%%--------------------------------------------------------------------
%% @doc Updates a worker's capacity.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_worker_capacity(pid(), pos_integer()) -> ok | {error, not_found}.

update_worker_capacity(WorkerPid, Capacity) when is_pid(WorkerPid), is_integer(Capacity), Capacity > 0 ->
    gen_server:cast(yawl_balancer, {update_worker_capacity, WorkerPid, Capacity}).

%%--------------------------------------------------------------------
%% @doc Updates a worker's weight for weighted balancing.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_worker_weight(pid(), pos_integer()) -> ok | {error, not_found}.

update_worker_weight(WorkerPid, Weight) when is_pid(WorkerPid), is_integer(Weight), Weight > 0 ->
    gen_server:cast(yawl_balancer, {update_worker_weight, WorkerPid, Weight}).

%%--------------------------------------------------------------------
%% @doc Gets the current load distribution across nodes.
%%
%% @return Map of node to active task count.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_load_distribution() -> load_distribution().

get_load_distribution() ->
    gen_server:call(yawl_balancer, get_load_distribution).

%%--------------------------------------------------------------------
%% @doc Resets a worker's active task count.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_worker_load(pid()) -> ok | {error, not_found}.

reset_worker_load(WorkerPid) when is_pid(WorkerPid) ->
    gen_server:cast(yawl_balancer, {reset_worker_load, WorkerPid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the balancer state.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #balancer_state{}}.

init([]) ->
    error_logger:info_report([{yawl_balancer, starting}, {pid, self()}]),
    %% Monitor CRE master for worker updates
    case whereis(cre_master) of
        undefined -> ok;
        _ -> ok  %% Could link here
    end,
    {ok, #balancer_state{}}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, #balancer_state{}) ->
          {reply, term(), #balancer_state{}}.

handle_call({select_worker, TaskData, Options}, _From, State) ->
    case maps:size(State#balancer_state.workers) of
        0 ->
            {reply, {error, no_workers}, State};
        _ ->
            Strategy = maps:get(strategy, Options, State#balancer_state.strategy),
            {SelectedPid, NewState} = select_by_strategy(Strategy, TaskData, State),
            {reply, {ok, SelectedPid}, NewState}
    end;

handle_call({get_worker_load, WorkerPid}, _From, State) ->
    case maps:get(WorkerPid, State#balancer_state.workers, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Worker -> {reply, {ok, Worker#worker.active_tasks}, State}
    end;

handle_call(get_all_workers, _From, State) ->
    Workers = maps:fold(
        fun(_Pid, Worker, Acc) ->
            [#{
                pid => Worker#worker.pid,
                node => Worker#worker.node,
                capacity => Worker#worker.capacity,
                weight => Worker#worker.weight,
                active_tasks => Worker#worker.active_tasks,
                total_tasks => Worker#worker.total_tasks,
                affinity_tags => Worker#worker.affinity_tags,
                utilization => Worker#worker.active_tasks / Worker#worker.capacity
            } | Acc]
        end,
        [],
        State#balancer_state.workers
    ),
    {reply, lists:reverse(Workers), State};

handle_call(get_strategy, _From, State) ->
    {reply, State#balancer_state.strategy, State};

handle_call(get_load_distribution, _From, State) ->
    Distribution = maps:fold(
        fun(_Pid, #worker{node = Node, active_tasks = Active}, Acc) ->
            maps:update_with(Node, fun(V) -> V + Active end, Active, Acc)
        end,
        #{},
        State#balancer_state.workers
    ),
    {reply, Distribution, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), #balancer_state{}) -> {noreply, #balancer_state{}}.

handle_cast({register_worker, WorkerPid, Options}, State) ->
    Capacity = maps:get(capacity, Options, 10),
    Weight = maps:get(weight, Options, 1),
    AffinityTags = maps:get(affinity_tags, Options, []),

    Worker = #worker{
        pid = WorkerPid,
        node = node(WorkerPid),
        capacity = Capacity,
        weight = Weight,
        active_tasks = 0,
        total_tasks = 0,
        affinity_tags = AffinityTags,
        last_heartbeat = erlang:timestamp()
    },

    %% Monitor the worker process
    erlang:monitor(process, WorkerPid),

    NewWorkers = maps:put(WorkerPid, Worker, State#balancer_state.workers),

    %% Update affinity map
    NewAffinityMap = lists:foldl(
        fun(Tag, Acc) ->
            maps:put(Tag, WorkerPid, Acc)
        end,
        State#balancer_state.affinity_map,
        AffinityTags
    ),

    error_logger:info_report([
        {yawl_balancer, worker_registered},
        {worker_pid, WorkerPid},
        {node, node(WorkerPid)},
        {capacity, Capacity},
        {weight, Weight}
    ]),

    {noreply, State#balancer_state{workers = NewWorkers, affinity_map = NewAffinityMap}};

handle_cast({unregister_worker, WorkerPid}, State) ->
    NewWorkers = maps:remove(WorkerPid, State#balancer_state.workers),

    %% Remove from affinity map
    NewAffinityMap = maps:filter(
        fun(_Tag, Pid) -> Pid =/= WorkerPid end,
        State#balancer_state.affinity_map
    ),

    error_logger:info_report([
        {yawl_balancer, worker_unregistered},
        {worker_pid, WorkerPid}
    ]),

    {noreply, State#balancer_state{workers = NewWorkers, affinity_map = NewAffinityMap}};

handle_cast({set_strategy, Strategy}, State) ->
    error_logger:info_report([
        {yawl_balancer, strategy_changed},
        {from, State#balancer_state.strategy},
        {to, Strategy}
    ]),
    {noreply, State#balancer_state{strategy = Strategy}};

handle_cast({update_worker_capacity, WorkerPid, Capacity}, State) ->
    NewWorkers = maps:update_with(
        WorkerPid,
        fun(Worker) -> Worker#worker{capacity = Capacity} end,
        State#balancer_state.workers
    ),
    {noreply, State#balancer_state{workers = NewWorkers}};

handle_cast({update_worker_weight, WorkerPid, Weight}, State) ->
    NewWorkers = maps:update_with(
        WorkerPid,
        fun(Worker) -> Worker#worker{weight = Weight} end,
        State#balancer_state.workers
    ),
    {noreply, State#balancer_state{workers = NewWorkers}};

handle_cast({reset_worker_load, WorkerPid}, State) ->
    NewWorkers = maps:update_with(
        WorkerPid,
        fun(Worker) -> Worker#worker{active_tasks = 0} end,
        State#balancer_state.workers
    ),
    {noreply, State#balancer_state{workers = NewWorkers}};

handle_cast(task_complete, State) ->
    %% Decrement active tasks for the worker that just completed
    %% This would need tracking of which worker completed the task
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles non-GEN_SERVER messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), #balancer_state{}) -> {noreply, #balancer_state{}}.

handle_info({'DOWN', _Ref, process, WorkerPid, _Reason}, State) ->
    error_logger:info_report([
        {yawl_balancer, worker_down},
        {worker_pid, WorkerPid}
    ]),
    NewWorkers = maps:remove(WorkerPid, State#balancer_state.workers),
    NewAffinityMap = maps:filter(
        fun(_Tag, Pid) -> Pid =/= WorkerPid end,
        State#balancer_state.affinity_map
    ),
    {noreply, State#balancer_state{workers = NewWorkers, affinity_map = NewAffinityMap}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles code change.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), #balancer_state{}, term()) -> {ok, #balancer_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Handles termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), #balancer_state{}) -> ok.

terminate(_Reason, _State) ->
    error_logger:info_report([{yawl_balancer, stopping}, {pid, self()}]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Selects a worker based on the configured strategy.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_by_strategy(balance_strategy(), term(), #balancer_state{}) ->
          {pid(), #balancer_state{}}.

select_by_strategy(round_robin, _TaskData, State) ->
    WorkerList = maps:keys(State#balancer_state.workers),
    Index = State#balancer_state.round_robin_index rem length(WorkerList),
    SelectedPid = lists:nth(Index + 1, WorkerList),
    NewState = State#balancer_state{round_robin_index = Index + 1},
    {SelectedPid, increment_task_count(SelectedPid, NewState)};

select_by_strategy(random, _TaskData, State) ->
    WorkerList = maps:keys(State#balancer_state.workers),
    SelectedPid = lists:nth(rand:uniform(length(WorkerList)), WorkerList),
    {SelectedPid, increment_task_count(SelectedPid, State)};

select_by_strategy(least_loaded, _TaskData, State) ->
    Workers = maps:to_list(State#balancer_state.workers),
    {SelectedPid, _Worker} = lists:min(
        [{Pid, W} || {Pid, W} <- Workers,
         W#worker.active_tasks < W#worker.capacity]
    ),
    {SelectedPid, increment_task_count(SelectedPid, State)};

select_by_strategy(weighted, _TaskData, State) ->
    Workers = maps:to_list(State#balancer_state.workers),
    %% Weighted selection: weight / (active_tasks + 1)
    {SelectedPid, _Worker} = lists:max(
        [{Pid, W#worker.weight / (W#worker.active_tasks + 1)} || {Pid, W} <- Workers]
    ),
    {SelectedPid, increment_task_count(SelectedPid, State)};

select_by_strategy(affinity, TaskData, State) ->
    %% Try to find a worker with matching affinity tags
    AffinityTags = extract_affinity_tags(TaskData),
    case find_worker_by_affinity(AffinityTags, State) of
        {ok, WorkerPid} ->
            {WorkerPid, increment_task_count(WorkerPid, State)};
        no_match ->
            %% Fall back to least loaded
            select_by_strategy(least_loaded, TaskData, State)
    end.

%%--------------------------------------------------------------------
%% @doc Increments the task count for a worker.
%%
%% @end
%%--------------------------------------------------------------------
-spec increment_task_count(pid(), #balancer_state{}) -> #balancer_state{}.

increment_task_count(WorkerPid, State) ->
    NewWorkers = maps:update_with(
        WorkerPid,
        fun(Worker) ->
            Worker#worker{
                active_tasks = Worker#worker.active_tasks + 1,
                total_tasks = Worker#worker.total_tasks + 1,
                last_heartbeat = erlang:timestamp()
            }
        end,
        State#balancer_state.workers
    ),
    State#balancer_state{workers = NewWorkers}.

%%--------------------------------------------------------------------
%% @doc Extracts affinity tags from task data.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_affinity_tags(term()) -> [binary()].

extract_affinity_tags(TaskData) when is_map(TaskData) ->
    maps:get(affinity_tags, TaskData, []);
extract_affinity_tags(_TaskData) ->
    [].

%%--------------------------------------------------------------------
%% @doc Finds a worker by affinity tags.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_worker_by_affinity([binary()], #balancer_state{}) ->
          {ok, pid()} | no_match.

find_worker_by_affinity([], _State) ->
    no_match;
find_worker_by_affinity([Tag | Rest], State) ->
    case maps:get(Tag, State#balancer_state.affinity_map, undefined) of
        undefined -> find_worker_by_affinity(Rest, State);
        WorkerPid -> {ok, WorkerPid}
    end.
