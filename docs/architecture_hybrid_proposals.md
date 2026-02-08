# Architecture Hybrid Proposals

**Generated:** 2026-02-07
**Scope:** Novel architectures combining YAWL and CRE approaches

---

## Executive Summary

This document proposes concrete hybrid architectures that combine the best of YAWL's mature Java workflow engine with CRE's elegant Erlang/OTP implementation. Each proposal includes architectural diagrams, implementation considerations, and feasibility assessment.

---

## Proposal 1: Active Petri Net Architecture

### Concept

Transform passive Petri net tokens into autonomous actors that can:
- Communicate with each other
- Make routing decisions
- Migrate across nodes
- Maintain local state

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Active Petri Net Layer                    │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  Token Actor ─────┐                                          │
│  ├─ ID            │                                          │
│  ├─ Payload       │   ┌──────────┐                          │
│  ├─ Location      ├───│ Migration │                          │
│  ├─ History       │   │ Protocol  │                          │
│  └─ Behaviors     │   └──────────┘                          │
│        └──────────┤                                          │
│                   ▼                                          │
│  ┌────────────────────────────────────┐                     │
│  │     Place (Distributed ETS)        │                     │
│  │  ┌────┐ ┌────┐ ┌────┐ ┌────┐      │                     │
│  │  │ T1 │ │ T2 │ │ T3 │ │ ...│      │                     │
│  │  └────┘ └────┘ └────┘ └────┘      │                     │
│  └────────────────────────────────────┘                     │
│                   ▲                                          │
│  Transition ──────┘                                          │
│  ├─ Preset                                                   │
│  ├─ Postset                                                  │
│  └─ Guard                                                    │
│                                                               │
└─────────────────────────────────────────────────────────────┘
                      ▲
                      │
┌─────────────────────┴─────────────────────────────────────┐
│                  gen_pnet Layer                            │
│  - Pattern modules                                         │
│  - Receipt verification                                    │
│  - Cycle detection                                         │
└───────────────────────────────────────────────────────────┘
```

### Implementation

```erlang
-module(active_token).
-behaviour(gen_server).

-export([start_link/3, move/2, query/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    id :: binary(),
    payload :: term(),
    location :: {atom(), binary()},  % {Place, CaseID}
    history :: [binary()],
    peers :: [pid()]
}).

start_link(ID, Payload, InitialPlace) ->
    gen_server:start_link(?MODULE, [ID, Payload, InitialPlace], []).

init([ID, Payload, {Place, CaseID}]) ->
    % Register with place
    place_registry:register_token(Place, CaseID, self()),
    {ok, #state{
        id = ID,
        payload = Payload,
        location = {Place, CaseID},
        history = [Place],
        peers = []
    }}.

% Move to new place
handle_cast({move, NewPlace, NewCaseID}, State) ->
    OldPlace = element(1, State#state.location),
    place_registry:unregister_token(OldPlace, self()),
    place_registry:register_token(NewPlace, NewCaseID, self()),
    {noreply, State#state{
        location = {NewPlace, NewCaseID},
        history = [NewPlace | State#state.history]
    }};

% Query token state
handle_call(query, _From, State) ->
    {reply, #{
        id => State#state.id,
        payload => State#state.payload,
        location => State#state.location,
        history => lists:reverse(State#state.history)
    }, State};

% Peer communication
handle_info({peer_message, From, Message}, State) ->
    % Handle inter-token communication
    NewState = handle_peer_message(Message, From, State),
    {noreply, NewState}.
```

### Enhanced Transition with Active Tokens

```erlang
-module(active_transition).
-behaviour(gen_yawl).

% Transition consults active tokens
fire(t_process, #{p_input := Tokens}, State) ->
    % Query all active tokens
    TokenStates = [gen_server:call(get_token_pid(T), query) || T <- Tokens],

    % Tokens vote on routing decision
    Decision = vote_on_routing(TokenStates),

    % Tokens self-organize based on decision
    lists:foreach(fun(TokenState) ->
        gen_server:cast(
            maps:get(pid, TokenState),
            {move, Decision#route.target_place, Decision#route.case_id}
        )
    end, TokenStates),

    {produce, #{p_output => Tokens}, State}.
```

### Benefits

1. **Self-Organizing:** Tokens make local routing decisions
2. **Location-Aware:** Tokens know their position and history
3. **Resilient:** Token actors can fail independently
4. **Scalable:** Distributed token placement

### Feasibility

| Aspect | Assessment |
|--------|------------|
| Complexity | High (requires actor coordination) |
| Maturity | Medium (builds on existing OTP) |
| Value | Breakthrough (enables new capabilities) |
| Effort | 6-12 months |

---

## Proposal 2: Layered Persistence Architecture

### Concept

Combine YAWL's Hibernate persistence with CRE's Mnesia for optimal performance and durability.

### Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
│                    (Workflows, Patterns)                     │
└────────────────────────┬─────────────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│                   Hybrid Persistence Layer                    │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────┐      ┌─────────────────┐               │
│  │   Hot Cache     │      │  Warm Cache     │               │
│  │   (ETS, RAM)    │◄────►│  (ETS, RAM)     │               │
│  │                 │      │                 │               │
│  │ - Active cases  │      │ - Recent cases  │               │
│  │ - Current state │      │ - Checkpoints   │               │
│  │ - Work items    │      │ - Audit trail   │               │
│  └────────┬────────┘      └────────┬────────┘               │
│           │                        │                         │
│           │ async write            │ async write            │
│           ▼                        ▼                         │
│  ┌─────────────────┐      ┌─────────────────┐               │
│  │   Cold Store    │      │   Archive       │               │
│  │   (Mnesia)      │      │   (Disk)        │               │
│  │                 │      │                 │               │
│  │ - All cases     │      │ - Completed     │               │
│  │ - Specifications│      │ - Historical    │               │
│  │ - Timers        │      │ - Backups       │               │
│  └─────────────────┘      └─────────────────┘               │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

### Implementation

```erlang
-module(hybrid_persistence).
-behaviour(gen_server).

-record(state, {
    hot_cache :: ets:tid(),
    warm_cache :: ets:tid(),
    cold_store :: atom(),  % Mnesia table
    archive :: atom(),      % Disk table
    write_back :: reference()
}).

% Read from appropriate cache
read(CaseID, AccessPattern) ->
    gen_server:call(?MODULE, {read, CaseID, AccessPattern}).

handle_call({read, CaseID, hot}, _From, State) ->
    case ets:lookup(State#state.hot_cache, CaseID) of
        [{CaseID, Data}] -> {reply, {ok, Data}, State};
        [] ->
            % Check warm cache
            case ets:lookup(State#state.warm_cache, CaseID) of
                [{CaseID, Data}] ->
                    % Promote to hot
                    ets:insert(State#state.hot_cache, {CaseID, Data}),
                    {reply, {ok, Data}, State};
                [] ->
                    % Check cold store
                    case mnesia:dirty_read(State#state.cold_store, CaseID) of
                        [{CaseID, Data}] ->
                            % Promote through caches
                            promote_to_hot(CaseID, Data, State),
                            {reply, {ok, Data}, State};
                        [] ->
                            {reply, {error, not_found}, State}
                    end
            end
    end;

% Write with appropriate persistence
handle_cast({write, CaseID, Data, Persistence}, State) ->
    case Persistence of
        volatile ->
            % Hot cache only
            ets:insert(State#state.hot_cache, {CaseID, Data});
        durable ->
            % Write through all layers
            ets:insert(State#state.hot_cache, {CaseID, Data}),
            ets:insert(State#state.warm_cache, {CaseID, Data}),
            async_write_mnesia(State#state.cold_store, CaseID, Data);
        permanent ->
            % Full persistence including archive
            ets:insert(State#state.hot_cache, {CaseID, Data}),
            ets:insert(State#state.warm_cache, {CaseID, Data}),
            mnesia:dirty_write(State#state.cold_store, {CaseID, Data}),
            async_write_archive(State#state.archive, CaseID, Data)
    end,
    {noreply, State}.

% Background write-back process
write_back_loop() ->
    receive
        flush ->
            % Write dirty entries to Mnesia
            flush_dirty_entries(),
            write_back_loop();
        checkpoint ->
            % Create checkpoint
            create_checkpoint(),
            write_back_loop()
    end.
```

### Timer Persistence

```erlang
-module(persistent_timer).
-behaviour(gen_server).

% Timer that survives restarts
start_timer(CaseID, Duration, Callback) ->
    gen_server:call(?MODULE, {start_timer, CaseID, Duration, Callback}).

handle_call({start_timer, CaseID, Duration, Callback}, _From, State) ->
    TimerID = make_ref(),
    TargetTime = erlang:monotonic_time(millisecond) + Duration,

    % Persist timer
    TimerRecord = #persistent_timer{
        id = TimerID,
        case_id = CaseID,
        target_time = TargetTime,
        callback = Callback,
        state = active
    },
    mnesia:dirty_write(persistent_timers, TimerRecord),

    % Arm timer
    erlang:send_after(Duration, self(), {timer_fire, TimerID}),

    {reply, {ok, TimerID}, State}.

% Restore timers on startup
restore_timers() ->
    Timers = mnesia:dirty_match_object(persistent_timers,
        #persistent_timer{_ = '_'}),
    lists:foreach(fun(Timer) ->
        case Timer#persistent_timer.state of
            active ->
                Remaining = max(0, Timer#persistent_timer.target_time -
                    erlang:monotonic_time(millisecond)),
                erlang:send_after(Remaining, self(),
                    {timer_fire, Timer#persistent_timer.id});
            expired ->
                % Already expired, fire callback
                fire_callback(Timer)
        end
    end, Timers).
```

### Benefits

1. **Performance:** Hot cache for active cases
2. **Durability:** Mnesia for crash recovery
3. **Flexibility:** Choose persistence level per case
4. **Recovery:** Automatic timer restoration

### Feasibility

| Aspect | Assessment |
|--------|------------|
| Complexity | Medium (builds on existing Mnesia) |
| Maturity | High (proven patterns) |
| Value | High (production-ready persistence) |
| Effort | 3-6 months |

---

## Proposal 3: Distributed Workflow Execution

### Concept

Enable workflows to execute across multiple Erlang nodes with automatic distribution and load balancing.

### Architecture

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   Node A     │     │   Node B     │     │   Node C     │
│              │     │              │     │              │
│ ┌──────────┐ │     │ ┌──────────┐ │     │ ┌──────────┐ │
│ │Workflow  │ │     │ │Workflow  │ │     │ │Workflow  │ │
│ │Manager   │ │     │ │Manager   │ │     │ │Manager   │ │
│ └────┬─────┘ │     │ └────┬─────┘ │     │ └────┬─────┘ │
│      │      │     │      │      │     │      │      │
│ ┌────┴─────┐ │     │ ┌────┴─────┐ │     │ ┌────┴─────┐ │
│ │Local    │ │     │ │Local    │ │     │ │Local    │ │
│ │Executor │ │     │ │Executor │ │     │ │Executor │ │
│ └──────────┘ │     │ └──────────┘ │     │ └──────────┘ │
└──────┬───────┘     └──────┬───────┘     └──────┬───────┘
       │                     │                     │
       └─────────────────────┼─────────────────────┘
                             │
                    ┌────────┴────────┐
                    │  Distribution   │
                    │  Coordinator    │
                    │                 │
                    │ - Load Balance  │
                    │ - Failover      │
                    │ - State Sync    │
                    └─────────────────┘
```

### Implementation

```erlang
-module(distributed_workflow).
-behaviour(gen_server).

-record(state, {
    workflow_spec :: term(),
    partitions :: [{atom(), binary(), pid()}],  % {Node, PartitionID, Pid}
    coordinator :: pid() | undefined
}).

% Start distributed workflow
start_distributed(Spec, Options) ->
    {ok, Coordinator} = distribution_coordinator:start_link(),
    gen_server:start_link(?MODULE, [Spec, Coordinator], []).

init([Spec, Coordinator]) ->
    % Analyze workflow for parallelizable regions
    Partitions = analyze_partitions(Spec),

    % Distribute partitions across nodes
    DistributedPartitions = distribute_partitions(Partitions),

    % Start workflow on each node
    StartedPartition = lists:map(fun({Node, PartitionDef}) ->
        {ok, Pid} = rpc:call(Node, gen_yawl, start_link,
            [PartitionDef#partition.module, PartitionDef#partition.args]),
        {Node, PartitionDef#partition.id, Pid}
    end, DistributedPartitions),

    {ok, #state{
        workflow_spec = Spec,
        partitions = StartedPartition,
        coordinator = Coordinator
    }}.

% Distribute partitions based on node load
distribute_partitions(Partitions) ->
    AvailableNodes = node_discovery:available_nodes(),

    % Get current load on each node
    NodeLoads = [{Node, get_node_load(Node)} || Node <- AvailableNodes],

    % Assign partitions to least loaded nodes
    lists:foldl(fun(Partition, Acc) ->
        {LeastLoadedNode, _Load} = lists:min(fun({_, A}, {_, B}) -> A < B end, NodeLoads),
        [{LeastLoadedNode, Partition} | Acc]
    end, [], Partitions).

% Handle inter-partition messages
handle_info({partition_message, FromPartition, ToPartition, Message}, State) ->
    % Find target partition
    case lists:keyfind(ToPartition, 2, State#state.partitions) of
        {_Node, _PartitionID, Pid} ->
            gen_server:cast(Pid, {remote_message, FromPartition, Message});
        false ->
            logger:error("Partition not found: ~p", [ToPartition])
    end,
    {noreply, State};

% Handle partition failure
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    case lists:keyfind(Pid, 3, State#state.partitions) of
        {Node, PartitionID, _} ->
            logger:warning("Partition ~p on ~p failed: ~p", [PartitionID, Node, Reason]),
            % Restart partition on another node
            NewPartition = restart_partition(PartitionID, State),
            NewPartitions = lists:keyreplace(PartitionID, 2, State#state.partitions, NewPartition),
            {noreply, State#state{partitions = NewPartitions}};
        false ->
            {noreply, State}
    end.
```

### Distribution Coordinator

```erlang
-module(distribution_coordinator).
-behaviour(gen_server).

% Coordinate distribution across cluster
init([]) ->
    % Monitor cluster membership
    net_kernel:monitor_nodes(true),
    {ok, #state{
        nodes = node_list(),
        load_map = #{}
    }}.

% Track node load
handle_call({report_load, Node, Load}, _From, State) ->
    NewLoadMap = maps:put(Node, Load, State#state.load_map),
    {reply, ok, State#state{load_map = NewLoadMap}};

% Select best node for new partition
handle_call({assign_partition, PartitionSize}, _From, State) ->
    % Score nodes based on load and partition size
    Scores = [{Node, score_node(Node, PartitionSize, State#state.load_map)}
              || Node <- State#state.nodes],
    {BestNode, _Score} = lists:max(fun({_, A}, {_, B}) -> A > B end, Scores),
    {reply, {ok, BestNode}, State}.

score_node(Node, PartitionSize, LoadMap) ->
    BaseLoad = maps:get(Node, LoadMap, 0.5),
    % Prefer nodes with lower load
    1.0 - BaseLoad.
```

### Benefits

1. **Scalability:** Horizontal scaling across nodes
2. **Resilience:** Automatic failover
3. **Load Balancing:** Intelligent distribution
4. **Transparency:** Location-independent execution

### Feasibility

| Aspect | Assessment |
|--------|------------|
| Complexity | High (requires distribution protocols) |
| Maturity | Medium (builds on Erlang distribution) |
| Value | High (enables massive scale) |
| Effort | 9-12 months |

---

## Proposal 4: Worklet Integration Layer

### Concept

Port YAWL's worklet framework to Erlang/OTP for runtime workflow modification.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Workflow Execution                       │
│                         (gen_yawl)                          │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                   Event Interceptor                         │
│  - Capture workitem events                                   │
│  - Detect exceptions                                         │
│  - Trigger worklet selection                                 │
└────────────────────────────┬────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                  Worklet Selector                           │
│  - Rule evaluation (RDR)                                     │
│  - Worklet matching                                          │
│  - Priority resolution                                       │
└────────────────────────────┬────────────────────────────────┘
                             │
                 ┌───────────┴───────────┐
                 ▼                       ▼
┌────────────────────┐      ┌────────────────────┐
│   Worklet Cache    │      │  Worklet Loader    │
│  - Compiled specs  │      │  - Dynamic load    │
│  - Pre-compiled    │      │  - Version mgmt    │
└────────────────────┘      └────────────────────┘
                 │                       │
                 └───────────┬───────────┘
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                  Worklet Executor                           │
│  - Launch temporary workflow                                 │
│  - Monitor execution                                         │
│  - Collect results                                           │
└─────────────────────────────────────────────────────────────┘
```

### Implementation

```erlang
-module(worklet_service).
-behaviour(gen_server).

-record(state, {
    rules :: [rule()],
    worklets :: #{binary() => worklet_spec()},
    cache :: ets:tid()
}).

% Rule structure (simplified RDR)
-record(rule, {
    id :: binary(),
    conditions :: [condition()],
    worklet_targets :: [binary()],
    priority :: integer()
}).

% Handle workitem event
handle_cast({workitem_event, Event}, State) ->
    case evaluate_rules(Event, State#state.rules) of
        {match, Rule, Bindings} ->
            % Launch worklet
            WorkletSpecs = select_worklets(
                Rule#rule.worklet_targets,
                State#state.worklets,
                Bindings
            ),
            lists:foreach(fun(Spec) ->
                launch_worklet(Spec, Event)
            end, WorkletSpecs);
        nomatch ->
            ok
    end,
    {noreply, State};

% Evaluate rules against event
evaluate_rules(Event, Rules) ->
    SortedRules = lists:sort(fun(R1, R2) ->
        R1#rule.priority >= R2#rule.priority
    end, Rules),

    case find_match(Event, SortedRules) of
        {ok, Rule, Bindings} -> {match, Rule, Bindings};
        nomatch -> nomatch
    end.

find_match(_Event, []) -> nomatch;
find_match(Event, [Rule | Rest]) ->
    case match_conditions(Event, Rule#rule.conditions) of
        {ok, Bindings} -> {ok, Rule, Bindings};
        nomatch -> find_match(Event, Rest)
    end.

match_conditions(Event, Conditions) ->
    lists:foldl(fun(Condition, Acc) ->
        case Acc of
            nomatch -> nomatch;
            {ok, Bindings} ->
                case evaluate_condition(Event, Condition, Bindings) of
                    {ok, NewBindings} -> {ok, NewBindings};
                    nomatch -> nomatch
                end
        end
    end, {ok, #{}}, Conditions).

% Launch worklet as temporary workflow
launch_worklet(WorkletSpec, OriginalEvent) ->
    % Create worklet instance
    {ok, WorkletPid} = gen_yawl:start_link(
        WorkletSpec#worklet_spec.module,
        #{
            original_event => OriginalEvent,
            worklet_id => make_ref()
        }
    ),

    % Monitor worklet
    erlang:monitor(process, WorkletPid),

    % Handle worklet completion
    spawn(fun() ->
        receive
            {'DOWN', _MRef, process, WorkletPid, normal} ->
                logger:info("Worklet completed: ~p", [WorkletSpec#worklet_spec.id]);
            {'DOWN', _MRef, process, WorkletPid, Reason} ->
                logger:error("Worklet failed: ~p, reason: ~p",
                    [WorkletSpec#worklet_spec.id, Reason]),
                handle_worklet_failure(WorkletSpec, OriginalEvent, Reason)
        end
    end).
```

### Exception Types

```erlang
% Exception conditions
-record(condition, {
    type :: exception_type(),
    predicate :: function()
}).

-type exception_type() ::
    {timeout, pos_integer()} |
    {resource_unavailable, atom()} |
    {data_constraint_violation, binary()} |
    {external_trigger, binary()} |
    {manual, binary()}.
```

### Benefits

1. **Runtime Adaptability:** Modify workflows without stopping
2. **Exception Handling:** Sophisticated error recovery
3. **Business Rules:** Declarative exception handling
4. **Hot Deployment:** Add worklets at runtime

### Feasibility

| Aspect | Assessment |
|--------|------------|
| Complexity | High (rule engine + workflow integration) |
| Maturity | Low (new to CRE) |
| Value | High (enterprise capability) |
| Effort | 6-9 months |

---

## Comparison Matrix

| Proposal | Complexity | Value | Effort | Priority |
|----------|------------|-------|--------|----------|
| Active Petri Nets | High | Breakthrough | 6-12 mo | P1 |
| Layered Persistence | Medium | High | 3-6 mo | P1 |
| Distributed Execution | High | High | 9-12 mo | P2 |
| Worklet Integration | High | High | 6-9 mo | P1 |

---

## Implementation Roadmap

### Phase 1: Foundation (0-3 months)
1. Layered persistence architecture
2. Basic distribution primitives
3. Enhanced token model

### Phase 2: Core Innovation (3-9 months)
1. Active token actors
2. Distribution coordinator
3. Worklet rule engine

### Phase 3: Advanced Features (9-18 months)
1. Token migration
2. Advanced worklets
3. Full distribution

---

**Document Version:** 1.0
**Related:** innovation_opportunities_report.md, pattern_enhancement_recommendations.md
