# Advanced Workflow Patterns Tutorial

**Master complex synchronization and multiple instance patterns**

---

## What You'll Learn

In this tutorial, you will:

- Understand advanced synchronization patterns (WCP-07 to WCP-10)
- Learn multiple instance patterns (WCP-13 to WCP-17)
- Implement state-based patterns (WCP-18 to WCP-20)
- Practice with real-world scenarios
- Understand performance implications

**Time required**: 90 minutes
**Prerequisites**: Completed [Basic Patterns Tutorial](basic_patterns_tutorial.md)

---

## Overview of Advanced Patterns

| Category | Patterns | Complexity |
|----------|----------|------------|
| **Advanced Sync** | WCP-07 to WCP-10 | Medium |
| **Multiple Instances** | WCP-13 to WCP-17 | High |
| **State-Based** | WCP-18 to WCP-20 | Medium-High |

---

## Part 1: Advanced Synchronization Patterns

### Pattern 7: Synchronizing Merge (WCP-07)

**Definition**: Merges multiple paths while maintaining synchronization - all paths must contribute, but processing can occur as each completes.

**Use Case**: Data analysis pipeline where results arrive at different times but all are needed.

#### Visual Representation

```
[Path A] ----\
             --> [Partial Result Buffer] --> [Final Merge]
[Path B] ----/
```

#### Key Differences from Basic Patterns

| Pattern | Behavior |
|---------|----------|
| **Synchronization (WCP-03)** | Wait for ALL, then proceed |
| **Synchronizing Merge (WCP-07)** | Collect partials, proceed when ALL collected |

#### Implementation

Create `sync_merge_pattern.erl`:

```erlang
%% @doc WCP-07: Synchronizing Merge Pattern
-module(sync_merge_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

-record(sync_state, {
    required_count :: integer(),
    collected_count :: integer(),
    partial_results :: map()
}).

%% API
run() -> run(3, fun partial_processor/2).
run(Count, ProcessorFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        required_count => Count,
        processor_function => ProcessorFun
    })),
    monitor_process(Pid).

monitor_process(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            receive
                {sync_merge_complete, Results} -> {ok, Results}
            after 1000 -> timeout
            end;
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after 30000 -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_partial_1, p_partial_2, p_partial_3,
     p_partial_buffer, p_sync_complete].

trsn_lst() -> [t_spawn, t_collect_partial, t_merge].

init_marking(p_start, _) -> [?TOKEN(initial, #{})];
init_marking(_, _) -> [].

preset(t_spawn) -> [p_start];
preset(t_collect_partial) -> [p_partial_1, p_partial_2, p_partial_3];
preset(t_merge) -> [p_partial_buffer];
preset(_) -> [].

is_enabled(t_spawn, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_collect_partial, Mode, _) ->
    lists:any(fun(P) -> maps:get(P, Mode, []) =/= [] end,
               [p_partial_1, p_partial_2, p_partial_3]);
is_enabled(t_merge, #{p_partial_buffer := Tokens}, UsrInfo) ->
    length(Tokens) >= maps:get(required_count, UsrInfo, 3);
is_enabled(_, _, _) -> false.

fire(t_spawn, _, UsrInfo) ->
    Count = maps:get(required_count, UsrInfo, 3),
    ProcessorFun = maps:get(processor_function, UsrInfo),

    %% Spawn partial processors
    Places = [p_partial_1, p_partial_2, p_partial_3],
    Tokens = lists:map(fun(N) ->
        spawn_partial_processor(N, ProcessorFun),
        ?TOKEN(partial_start, #{id => N})
    end, lists:seq(1, Count)),

    %% Distribute to places
    ProduceMap = lists:foldl(fun({Token, Place}, Acc) ->
        maps:put(Place, [Token], Acc)
    end, #{p_start => []}, lists:zip(Tokens, Places)),
    {produce, ProduceMap};

fire(t_collect_partial, Mode, UsrInfo) ->
    %% Find which partial has a token
    case lists:search(fun(P) ->
        case maps:get(P, Mode, []) of
            [T|_] when T =/= [] -> true;
            _ -> false
        end
    end, [p_partial_1, p_partial_2, p_partial_3]) of
        {value, Place} ->
            [Token|_] = maps:get(Place, Mode, []),
            PartialId = maps:get(id, Token#colored_token.payload, 0),

            %% Simulate partial processing
            Result = simulate_partial_processing(PartialId),
            io:format("Partial ~p collected: ~p~n", [PartialId, Result]),

            NewToken = ?TOKEN(partial_done, #{
                id => PartialId,
                result => Result
            }),

            {produce, #{
                Place => [],
                p_partial_buffer => [NewToken]
            }};
        false ->
            abort
    end;

fire(t_merge, #{p_partial_buffer := Tokens}, _) ->
    %% Aggregate all partial results
    Results = lists:foldl(fun(#colored_token{payload = P}, Acc) ->
        maps:put(maps:get(id, P), maps:get(result, P), Acc)
    end, #{}, Tokens),

    io:format("Merged ~p partial results: ~p~n", [length(Tokens), Results]),

    self() ! {sync_merge_complete, Results},

    {produce, #{
        p_partial_buffer => [],
        p_sync_complete => [?TOKEN(merge_complete, Results)]
    }};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

%% Internal functions
spawn_partial_processor(Id, Fun) ->
    spawn(fun() ->
        Result = Fun(partial, Id),
        io:format("Partial processor ~p done: ~p~n", [Id, Result])
    end).

simulate_partial_processing(Id) ->
    %% Simulate variable processing time
    timer:sleep(rand:uniform(2000)),
    {partial_complete, Id, rand:uniform(100)}.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

partial_processor(partial, Id) ->
    timer:sleep(rand:uniform(2000)),
    {processed, Id, Id * 10}.
```

#### Testing Synchronizing Merge

```erlang
c(sync_merge_pattern).
sync_merge_pattern:run(3, fun(_, N) ->
    timer:sleep(rand:uniform(3000)),
    N * 100
end).
```

---

### Pattern 9: Discriminator (WCP-09)

**Definition**: Triggers on first branch completion, ignoring subsequent completions.

**Use Case**: Racing multiple services and using the first response.

#### Visual Representation

```
[Service A] --\
              --> [First Result] --> [Continue]
[Service B] --/    [Discard Others]
```

#### Implementation

Create `discriminator_pattern.erl`:

```erlang
%% @doc WCP-09: Discriminator Pattern
-module(discriminator_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

%% API
run() -> run(3, fun service_call/2).
run(ServiceCount, ServiceFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        service_count => ServiceCount,
        service_function => ServiceFun
    })),
    wait_for_first(Pid, 10000).

wait_for_first(Pid, Timeout) ->
    receive
        {discriminator_result, Winner} -> {ok, Winner};
        {discriminator_timeout} -> {error, timeout}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_services, p_first_result, p_discard, p_complete].

trsn_lst() -> [t_spawn, t_service_done, t_capture_first, t_discard_rest].

init_marking(p_start, _) -> [?TOKEN(initial, #{})];
init_marking(_, _) -> [].

preset(t_spawn) -> [p_start];
preset(t_service_done) -> [p_services];
preset(t_capture_first) -> [p_first_result];
preset(t_discard_rest) -> [p_discard];
preset(_) -> [].

is_enabled(t_spawn, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_service_done, #{p_services := Ts}, _) when length(Ts) > 0 -> true;
is_enabled(t_capture_first, #{p_first_result := [T|_]}) when T =/= [] -> true;
is_enabled(t_discard_rest, #{p_discard := Ts}) when length(Ts) > 0 -> true;
is_enabled(_, _, _) -> false.

fire(t_spawn, _, UsrInfo) ->
    Count = maps:get(service_count, UsrInfo, 3),
    ServiceFun = maps:get(service_function, UsrInfo),

    %% Start all services
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Result = ServiceFun(service, N),
            self() ! {service_done, N, Result}
        end)
    end, lists:seq(1, Count)),

    Tokens = [?TOKEN(service_pending, #{id => N}) || N <- lists:seq(1, Count)],
    {produce, #{p_start => [], p_services => Tokens}};

fire(t_service_done, #{p_services := [Token|Rest]}, _) ->
    %% Wait for first service to respond (simulated)
    receive
        {service_done, Id, Result} ->
            io:format("Service ~p responded first with: ~p~n", [Id, Result]),
            WinnerToken = ?TOKEN(first_response, #{
                service_id => Id,
                result => Result
            }),
            {produce, #{
                p_services => Rest,
                p_first_result => [WinnerToken]
            }}
    after 5000 ->
        io:format("Service timeout~n"),
        abort
    end;

fire(t_capture_first, #{p_first_result := [Token]}, _) ->
    io:format("Captured first result from service ~p~n",
              [maps:get(service_id, Token#colored_token.payload)]),

    self() ! {discriminator_result, Token#colored_token.payload},

    %% Move remaining services to discard
    {produce, #{
        p_first_result => [],
        p_discard => [Token],
        p_complete => [?TOKEN(done, Token#colored_token.payload)]
    }};

fire(t_discard_rest, #{p_discard := [_]}, _) ->
    io:format("Discarding remaining service results~n"),
    {produce, #{p_discard => []}};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

service_call(service, N) ->
    Delay = rand:uniform(5000),
    timer:sleep(Delay),
    {service_result, N, Delay}.
```

---

## Part 2: Multiple Instance Patterns

### Pattern 13: Multiple Instances with Static Count (WCP-13)

**Definition**: Create a fixed number of parallel instances, all must complete before continuation.

**Use Case**: Batch processing with known item count.

#### Visual Representation

```
[Start] --> [Spawn N Instances] --> [Instance 1]
                                    [Instance 2] --> [Join All] --> [Continue]
                                    [Instance N]
```

#### Implementation

Create `multi_instance_static.erl`:

```erlang
%% @doc WCP-13: Multiple Instances (Static) Pattern
-module(multi_instance_static).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/3]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

-record(instance_state, {
    total :: integer(),
    completed :: integer(),
    results :: [term()]
}).

%% API
run() -> run(5, fun process_item/2, lists:seq(1, 5)).
run(InstanceCount, ProcessFun, InputData) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        instance_count => InstanceCount,
        process_function => ProcessFun,
        input_data => InputData,
        instance_state => #instance_state{
            total => InstanceCount,
            completed => 0,
            results => []
        }
    })),
    wait_for_all_instances(Pid, 30000).

wait_for_all_instances(Pid, Timeout) ->
    receive
        {all_instances_complete, Results} -> {ok, Results};
        {instances_failed, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_pool, p_running, p_completed, p_done].

trsn_lst() -> [t_spawn, t_complete_one, t_join_all].

init_marking(p_start, UsrInfo) ->
    Data = maps:get(input_data, UsrInfo, []),
    [?TOKEN(initial, Data)];
init_marking(_, _) -> [].

preset(t_spawn) -> [p_start];
preset(t_complete_one) -> [p_running];
preset(t_join_all) -> [p_completed];
preset(_) -> [].

is_enabled(t_spawn, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_complete_one, #{p_running := Ts}, _) when length(Ts) > 0 -> true;
is_enabled(t_join_all, #{p_completed := Ts}, UsrInfo) ->
    State = maps:get(instance_state, UsrInfo),
    length(Ts) >= State#instance_state.total;
is_enabled(_, _, _) -> false.

fire(t_spawn, #{p_start := [Token]}, UsrInfo) ->
    Count = maps:get(instance_count, UsrInfo, 3),
    ProcessFun = maps:get(process_function, UsrInfo),
    Data = Token#colored_token.payload,

    %% Create instance tokens
    InstanceTokens = lists:map(fun(N) ->
        Item = case Data of
            List when is_list(List), length(List) >= N -> lists:nth(N, List);
            _ -> N
        end,

        spawn_instance(N, ProcessFun, Item),

        ?TOKEN(instance_start, #{
            instance_id => N,
            input => Item
        })
    end, lists:seq(1, Count)),

    io:format("Spawned ~p instances~n", [Count]),

    {produce, #{
        p_start => [],
        p_running => InstanceTokens
    }};

fire(t_complete_one, #{p_running := [Token|Rest]}, UsrInfo) ->
    InstanceId = maps:get(instance_id, Token#colored_token.payload, 0),

    %% Wait for instance to complete
    receive
        {instance_done, InstanceId, Result} ->
            io:format("Instance ~p completed: ~p~n", [InstanceId, Result]),

            State = maps:get(instance_state, UsrInfo),
            NewState = State#instance_state{
                completed = State#instance_state.completed + 1,
                results = [Result | State#instance_state.results]
            },

            CompleteToken = ?TOKEN(instance_done, #{
                instance_id => InstanceId,
                result => Result,
                state => NewState
            }),

            {produce, #{
                p_running => Rest,
                p_completed => [CompleteToken]
            }}
    after 5000 ->
        io:format("Instance ~p timeout~n", [InstanceId]),
        abort
    end;

fire(t_join_all, #{p_completed := Tokens}, UsrInfo) ->
    %% Aggregate all results
    Results = [maps:get(result, T#colored_token.payload) || T <- Tokens],

    io:format("All ~p instances joined. Results: ~p~n", [length(Results), Results]),

    self() ! {all_instances_complete, Results},

    {produce, #{
        p_completed => [],
        p_done => [?TOKEN(all_done, #{results => Results})]
    }};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

%% Internal
spawn_instance(Id, Fun, Item) ->
    spawn(fun() ->
        try
            Result = Fun(instance, Item),
            self() ! {instance_done, Id, Result}
        catch
            _:Error ->
                self() ! {instance_done, Id, {error, Error}}
        end
    end).

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

process_item(instance, N) ->
    timer:sleep(rand:uniform(2000)),
    N * 2.
```

---

### Pattern 15: Multiple Instances - Dynamic (WCP-15)

**Definition**: Create instances dynamically as data becomes available, without prior knowledge of count.

**Use Case**: Processing streaming data or message queues.

#### Key Differences

| Pattern | Instance Count | When Known |
|---------|----------------|------------|
| **WCP-13 (Static)** | Fixed | Design time |
| **WCP-14 (Runtime)** | Calculated | Before creation |
| **WCP-15 (Dynamic)** | Unknown | At creation time |

#### Implementation

Create `multi_instance_dynamic.erl`:

```erlang
%% @doc WCP-15: Multiple Instances (Dynamic) Pattern
-module(multi_instance_dynamic).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

-record(dynamic_state, {
    active_count :: integer(),
    completed_count :: integer(),
    results :: [term()],
    source_active :: boolean()
}).

%% API
run() -> run(fun data_stream/0, fun stream_processor/2).
run(StreamFun, ProcessorFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        stream_function => StreamFun,
        processor_function => ProcessorFun,
        dynamic_state => #dynamic_state{
            active_count => 0,
            completed_count => 0,
            results => [],
            source_active => true
        }
    })),
    wait_for_completion(Pid, 60000).

wait_for_completion(Pid, Timeout) ->
    receive
        {dynamic_complete, Results} -> {ok, Results};
        {dynamic_error, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_source, p_pool, p_completed, p_sync, p_done].

trsn_lst() -> [t_start_source, t_fetch_data, t_spawn_instance, t_complete_instance, t_check_done].

init_marking(p_start, _) -> [?TOKEN(initial, #{})];
init_marking(_, _) -> [].

preset(t_start_source) -> [p_start];
preset(t_fetch_data) -> [p_source];
preset(t_spawn_instance) -> [p_pool];
preset(t_complete_instance) -> [p_completed];
preset(t_check_done) -> [p_sync];
preset(_) -> [].

is_enabled(t_start_source, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_fetch_data, #{p_source := [T]}, _) when T =/= [] -> true;
is_enabled(t_spawn_instance, #{p_pool := [T|_]}) when T =/= [] -> true;
is_enabled(t_complete_instance, #{p_completed := Ts}) when length(Ts) > 0 -> true;
is_enabled(t_check_done, #{p_sync := [T]}, UsrInfo) ->
    State = maps:get(dynamic_state, UsrInfo),
    case State#dynamic_state.source_active of
        false -> State#dynamic_state.active_count =:= 0;
        true -> false
    end;
is_enabled(_, _, _) -> false.

fire(t_start_source, _, _) ->
    io:format("Starting data source~n"),
    {produce, #{p_start => [], p_source => [?TOKEN(source_active, #{})]}};

fire(t_fetch_data, #{p_source := [Token]}, UsrInfo) ->
    StreamFun = maps:get(stream_function, UsrInfo),

    case StreamFun() of
        {more, Data} ->
            io:format("Fetched data: ~p~n", [Data]),
            {produce, #{p_pool => [?TOKEN(data_item, Data)]}};
        done ->
            io:format("Data source exhausted~n"),
            State = maps:get(dynamic_state, UsrInfo),
            NewState = State#dynamic_state{source_active = false},
            {produce, #{p_source => [], p_sync => [?TOKEN(source_done, #{state => NewState})]}};
        {error, Reason} ->
            io:format("Stream error: ~p~n", [Reason]),
            self() ! {dynamic_error, Reason},
            abort
    end;

fire(t_spawn_instance, #{p_pool := [Token|Rest]}, UsrInfo) ->
    ProcessorFun = maps:get(processor_function, UsrInfo),
    Data = Token#colored_token.payload,

    InstanceId = generate_id(),

    spawn(fun() ->
        Result = ProcessorFun(instance, Data),
        self() ! {instance_complete, InstanceId, Result}
    end),

    State = maps:get(dynamic_state, UsrInfo),
    NewState = State#dynamic_state{
        active_count = State#dynamic_state.active_count + 1
    },

    io:format("Spawned instance ~s (active: ~p)~n",
              [InstanceId, NewState#dynamic_state.active_count]),

    {produce, #{
        p_pool => Rest,
        p_sync => [?TOKEN(spawned, #{state => NewState})]
    }};

fire(t_complete_instance, #{p_completed := [Token]}, UsrInfo) ->
    InstanceId = Token#colored_token.payload,

    receive
        {instance_complete, InstanceId, Result} ->
            State = maps:get(dynamic_state, UsrInfo),
            NewState = State#dynamic_state{
                active_count = State#dynamic_state.active_count - 1,
                completed_count = State#dynamic_state.completed_count + 1,
                results = [Result | State#dynamic_state.results]
            },

            io:format("Instance ~s complete (active: ~p, done: ~p)~n",
                      [InstanceId, NewState#dynamic_state.active_count,
                       NewState#dynamic_state.completed_count]),

            {produce, #{
                p_completed => [],
                p_sync => [?TOKEN(instance_done, #{state => NewState, result => Result})]
            }}
    after 5000 ->
        io:format("Instance ~s timeout~n", [InstanceId]),
        abort
    end;

fire(t_check_done, #{p_sync := Tokens}, UsrInfo) ->
    %% Aggregate states from all tokens
    FinalState = lists:foldl(fun(T, AccState) ->
        case T#colored_token.type of
            source_done ->
                T#colored_token.payload.state;
            instance_done ->
                S = T#colored_token.payload.state,
                AccState#dynamic_state{
                    active_count = S#dynamic_state.active_count,
                    completed_count = S#dynamic_state.completed_count,
                    results = lists:merge(
                        S#dynamic_state.results,
                        AccState#dynamic_state.results
                    )
                };
            _ ->
                AccState
        end
    end, #dynamic_state{}, Tokens),

    case {FinalState#dynamic_state.source_active, FinalState#dynamic_state.active_count} of
        {false, 0} ->
            io:format("Dynamic instances complete! Processed ~p items~n",
                      [FinalState#dynamic_state.completed_count]),

            self() ! {dynamic_complete, lists:reverse(FinalState#dynamic_state.results)},

            {produce, #{
                p_sync => [],
                p_done => [?TOKEN(all_done, #{results => FinalState#dynamic_state.results})]
            }};
        _ ->
            %% Not done yet
            {produce, #{p_sync => Tokens}}
    end;

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

%% Example stream function
data_stream() ->
    %% Simulate streaming data
    case rand:uniform(10) of
        1 -> done;
        N -> {more, N}
    end.

stream_processor(instance, Data) ->
    timer:sleep(500),
    Data * 10.
```

---

## Part 3: State-Based Patterns

### Pattern 18: Milestone (WCP-18)

**Definition**: Activity only executes when a specific milestone condition is reached.

**Use Case**: Tasks that require certain prerequisites.

#### Visual Representation

```
[Milestone Check] --> [Milestone Reached] --> [Guarded Activity]
                                                      |
                                                 [Complete]
```

#### Implementation

Create `milestone_pattern.erl`:

```erlang
%% @doc WCP-18: Milestone Pattern
-module(milestone_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

-record(milestone_data, {
    current_phase :: integer(),
    required_phase :: integer(),
    activity_data :: term()
}).

%% API
run() -> run(3, fun milestone_activity/2).
run(RequiredPhase, ActivityFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        required_phase => RequiredPhase,
        activity_function => ActivityFun,
        milestone_data => #milestone_data{
            current_phase => 0,
            required_phase => RequiredPhase,
            activity_data => undefined
        }
    })),
    wait_for_activity(Pid, 10000).

wait_for_activity(Pid, Timeout) ->
    receive
        {milestone_reached, Phase} ->
            io:format("Milestone phase ~p reached~n", [Phase]),
            wait_for_activity(Pid, Timeout);
        {activity_complete, Result} -> {ok, Result};
        {activity_blocked, Phase} -> {blocked, Phase}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_check, p_milestone, p_activity, p_complete].

trsn_lst() -> [t_advance, t_check_milestone, t_execute, t_finish].

init_marking(p_start, _) -> [?TOKEN(initial, #{phase => 0})];
init_marking(_, _) -> [].

preset(t_advance) -> [p_start];
preset(t_check_milestone) -> [p_check];
preset(t_execute) -> [p_milestone];
preset(t_finish) -> [p_activity];
preset(_) -> [].

is_enabled(t_advance, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_check_milestone, #{p_check := [T]}, _) when T =/= [] -> true;
is_enabled(t_execute, #{p_milestone := [T]}, _) when T =/= [] -> true;
is_enabled(t_finish, #{p_activity := [T]}, _) when T =/= [] -> true;
is_enabled(_, _, _) -> false.

fire(t_advance, #{p_start := [Token]}, UsrInfo) ->
    %% Advance to next phase
    MilestoneData = maps:get(milestone_data, UsrInfo),
    CurrentPhase = MilestoneData#milestone_data.current_phase,
    NextPhase = CurrentPhase + 1,

    io:format("Advancing to phase ~p~n", [NextPhase]),

    NewToken = ?TOKEN(check, #{
        current_phase => NextPhase,
        required_phase => MilestoneData#milestone_data.required_phase
    }),

    {produce, #{
        p_start => [],
        p_check => [NewToken]
    }};

fire(t_check_milestone, #{p_check := [Token]}, UsrInfo) ->
    CurrentPhase = maps:get(current_phase, Token#colored_token.payload, 0),
    RequiredPhase = maps:get(required_phase, Token#colored_token.payload, 1),

    case CurrentPhase >= RequiredPhase of
        true ->
            io:format("Milestone reached! Phase ~p >= Required ~p~n",
                      [CurrentPhase, RequiredPhase]),
            self() ! {milestone_reached, CurrentPhase},

            NewToken = ?TOKEN(milestone_ready, #{
                phase => CurrentPhase,
                activity_data => Token#colored_token.payload
            }),

            {produce, #{
                p_check => [],
                p_milestone => [NewToken]
            }};
        false ->
            io:format("Milestone not reached. Phase ~p < Required ~p~n",
                      [CurrentPhase, RequiredPhase]),
            self() ! {activity_blocked, CurrentPhase},

            %% Loop back to advance
            {produce, #{
                p_check => [],
                p_start => [?TOKEN(initial, #{phase => CurrentPhase})]
            }}
    end;

fire(t_execute, #{p_milestone := [Token]}, UsrInfo) ->
    ActivityFun = maps:get(activity_function, UsrInfo),
    Phase = maps:get(phase, Token#colored_token.payload, 0),

    io:format("Executing activity at phase ~p~n", [Phase]),

    Result = ActivityFun(activity, Phase),

    NewToken = ?TOKEN(activity_result, #{
        phase => Phase,
        result => Result
    }),

    {produce, #{
        p_milestone => [],
        p_activity => [NewToken]
    }};

fire(t_finish, #{p_activity := [Token]}, _) ->
    Result = maps:get(result, Token#colored_token.payload),

    io:format("Activity complete: ~p~n", [Result]),

    self() ! {activity_complete, Result},

    {produce, #{
        p_activity => [],
        p_complete => [?TOKEN(done, #{})]
    }};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

milestone_activity(activity, Phase) ->
    timer:sleep(500),
    {activity_complete, Phase, Phase * 100}.
```

---

### Pattern 19: Cancel Activity (WCP-19)

**Definition**: Allow cancellation of a single running activity.

#### Visual Representation

```
[Activity Running] --[Complete]--> [Activity Done]
       |
     [Cancel]
       v
[Activity Cancelled]
```

#### Implementation

Create `cancel_activity_pattern.erl`:

```erlang
%% @doc WCP-19: Cancel Activity Pattern
-module(cancel_activity_pattern).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/0, run/2, cancel/1]).

-define(TOKEN(Type, Data), #colored_token{
    id = generate_id(),
    type = Type,
    payload = Data,
    created_at = erlang:system_time(millisecond)
}).

%% API
run() -> run(fun long_running_task/1, 5000).
run(ActivityFun, Timeout) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        activity_function => ActivityFun,
        timeout => Timeout,
        cancel_requested => false
    })),
    {Pid, fun() -> cancel(Pid) end}.

cancel(Pid) ->
    Pid ! {cancel, self()},
    receive
        {cancel_complete} -> ok;
        {cancel_failed, Reason} -> {error, Reason}
    after 5000 -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_running, p_cancel_requested, p_done, p_cancelled].

trsn_lst() -> [t_start, t_complete, t_request_cancel, t_execute_cancel].

init_marking(p_start, _) -> [?TOKEN(initial, #{})];
init_marking(_, _) -> [].

preset(t_start) -> [p_start];
preset(t_complete) -> [p_running];
preset(t_request_cancel) -> [p_running];
preset(t_execute_cancel) -> [p_cancel_requested];
preset(_) -> [].

is_enabled(t_start, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_complete, #{p_running := [T]}, _) when T =/= [] -> true;
is_enabled(t_request_cancel, #{p_running := [T]}, _) when T =/= [] -> true;
is_enabled(t_execute_cancel, #{p_cancel_requested := [T]}, _) when T =/= [] -> true;
is_enabled(_, _, _) -> false.

fire(t_start, _, UsrInfo) ->
    ActivityFun = maps:get(activity_function, UsrInfo),
    Timeout = maps:get(timeout, UsrInfo, 5000),

    %% Start the activity in a monitored process
    ActivityPid = spawn(fun() ->
        Result = ActivityFun(activity),
        self() ! {activity_done, Result}
    end),

    io:format("Started activity ~p (timeout: ~p)~n", [ActivityPid, Timeout]),

    Token = ?TOKEN(running, #{
        activity_pid => ActivityPid,
        start_time => erlang:monotonic_time(millisecond)
    }),

    %% Set up timeout
    erlang:send_after(Timeout, self(), {activity_timeout, ActivityPid}),

    {produce, #{p_start => [], p_running => [Token]}};

fire(t_complete, #{p_running := [Token]}, _) ->
    io:format("Activity completed normally~n"),

    self() ! {activity_complete, normal},

    {produce, #{
        p_running => [],
        p_done => [?TOKEN(complete, #{status => completed})]
    }};

fire(t_request_cancel, #{p_running := [Token]}, _) ->
    io:format("Cancel requested~n"),

    ActivityPid = maps:get(activity_pid, Token#colored_token.payload),

    %% Attempt to cancel the activity
    case is_process_alive(ActivityPid) of
        true ->
            exit(ActivityPid, kill),
            NewToken = ?TOKEN(cancel_pending, #{
                activity_pid => ActivityPid,
                reason => user_requested
            }),
            {produce, #{
                p_running => [],
                p_cancel_requested => [NewToken]
            }};
        false ->
            %% Already done
            {produce, #{
                p_running => [],
                p_done => [?TOKEN(complete, #{status => already_complete})]
            }}
    end;

fire(t_execute_cancel, #{p_cancel_requested := [Token]}, _) ->
    io:format("Executing cancellation~n"),

    self() ! {cancel_complete},

    {produce, #{
        p_cancel_requested => [],
        p_cancelled => [?TOKEN(cancelled, #{
            reason => maps:get(reason, Token#colored_token.payload)
        })]
    }};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(Request, State) ->
    io:format("Cast: ~p~n", [Request]),
    {noreply, State}.
handle_info({cancel, From}, State) ->
    %% Trigger cancellation by injecting a cancel token
    io:format("Processing cancel request from ~p~n", [From]),
    %% In a real implementation, we would trigger the cancel transition
    From ! {cancel_complete},
    {noreply, State};
handle_info({activity_done, Result}, State) ->
    io:format("Activity done: ~p~n", [Result]),
    self() ! {activity_complete, Result},
    {noreply, State};
handle_info({activity_timeout, Pid}, State) ->
    io:format("Activity ~p timed out~n", [Pid]),
    exit(Pid, kill),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
    {noreply, State}.
terminate(_, _) -> ok.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.

long_running_task(activity) ->
    io:format("Starting long running task...~n"),
    timer:sleep(10000),
    {ok, task_complete}.
```

---

## Summary: Advanced Pattern Selection

| Pattern | Use When | Performance Impact |
|---------|----------|-------------------|
| **Synchronizing Merge (WCP-07)** | Collect partials, need all | Medium - partial buffering |
| **Discriminator (WCP-09)** | First response wins | Low - discards extras |
| **Multi-Instance Static (WCP-13)** | Fixed parallelism | Medium - coordination overhead |
| **Multi-Instance Dynamic (WCP-15)** | Streaming data | High - dynamic spawning |
| **Milestone (WCP-18)** | Prerequisite tasks | Low - condition checking |
| **Cancel Activity (WCP-19)** | Cancellable operations | Low - signal handling |

---

## Real-World Exercise: Data Pipeline

Create a workflow that processes sensor data with:

1. **Dynamic Instance Creation**: Spawn processors as data arrives
2. **Synchronizing Merge**: Collect partial results
3. **Milestone Guard**: Only finalize after threshold reached

**Requirements**:
- Process 10+ sensor readings
- Each reading takes variable time (1-3 seconds)
- Collect all results before finalizing
- Minimum 5 readings required to finalize

---

**Next Steps**: Continue to [Colored Tokens Tutorial](colored_tokens_tutorial.md) to learn about data-carrying tokens.
