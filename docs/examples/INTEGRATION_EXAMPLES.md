# CRE Integration Examples

This document provides end-to-end integration examples for the Common Runtime Environment (CRE). Each example includes a complete workflow specification, Erlang code to run it, expected output, and extension guidance.

## Table of Contents

1. [Hello World Workflow](#1-hello-world-workflow) - Simple sequence
2. [HTTP Webhook Integration](#2-http-webhook-integration) - External service calls
3. [Database Persistence](#3-database-persistence) - Mnesia integration
4. [Active Token Workflow](#4-active-token-workflow) - Autonomous agents
5. [RL Agent Intervention](#5-rl-agent-intervention) - Learning system
6. [Predictive Monitoring](#6-predictive-monitoring) - Anomaly detection

---

## 1. Hello World Workflow

### Overview

A simple sequential workflow that demonstrates the basic pattern execution flow. This example shows two tasks executing in sequence.

### Workflow Specification

```erlang
%% File: examples/hello_world.erl
-module(hello_world).
-export([run/0]).

%% Simple sequence: task1 -> task2
run() ->
    %% Define the workflow specification
    Spec = #{
        places => [p_start, p_task1, p_task2, p_end],
        transitions => #{
            t_start => #{
                preset => [p_start],
                produce => #{p_task1 => [go1]}
            },
            t_task1 => #{
                preset => [p_task1],
                is_task => true,
                task_place => p_task1_work,
                produce => #{p_task2 => [go2]}
            },
            t_task2 => #{
                preset => [p_task2],
                is_task => true,
                task_place => p_task2_work,
                produce => #{p_end => [done]}
            }
        },
        init_marking => #{p_start => [start]},
        end_place => p_end,
        start_token => start
    },

    %% Start the workflow engine
    {ok, Engine} = wf_engine:start_link(#{
        spec => Spec,
        org => #{participants => [alice]},
        seed => 42,
        now => 0
    }),

    %% Start a case
    {ok, CaseId} = wf_engine:start_case(Engine, #{data => #{}}, 0),
    io:format("Started case: ~s~n", [CaseId]),

    %% Get offered work items
    Offered = wf_engine:offered_workitems(Engine, CaseId),
    io:format("Offered work items: ~p~n", [Offered]),

    %% Complete first task
    case Offered of
        [{task1, WiId}] ->
            wf_engine:allocate(Engine, WiId, alice, 1),
            wf_engine:start_work(Engine, WiId, alice, 2),
            wf_engine:complete(Engine, WiId, alice, #{result => hello}, 3),
            io:format("Completed task1~n");
        _ ->
            io:format("No task1 found~n")
    end,

    %% Get next work item
    Offered2 = wf_engine:offered_workitems(Engine, CaseId),
    case Offered2 of
        [{task2, WiId2}] ->
            wf_engine:allocate(Engine, WiId2, alice, 4),
            wf_engine:start_work(Engine, WiId2, alice, 5),
            wf_engine:complete(Engine, WiId2, alice, #{result => world}, 6),
            io:format("Completed task2~n");
        _ ->
            io:format("No task2 found~n")
    end,

    %% Check final state
    State = wf_engine:case_state(Engine, CaseId),
    io:format("Final state: ~p~n", [State]),

    %% Drain receipts
    Receipts = wf_engine:drain_receipts(Engine, CaseId),
    io:format("Receipts: ~p~n", [Receipts]),

    %% Cleanup
    gen_server:stop(Engine),
    ok.
```

### Expected Output

```
Started case: <<case_abc123...>>
Offered work items: [{task1,<<wi_def456...>>}]
Completed task1
Completed task2
Final state: completed
Receipts: [{receipt,#{...}}, {receipt,#{...}}, {receipt,#{...}}]
```

### How to Run

```bash
# Compile
erlc -I include -o ebin examples/hello_world.erl

# Run
erl -pa ebin -noshell -s hello_world run -s init stop
```

### How to Extend

- **Add more tasks**: Extend the `places` list and add transitions with `is_task => true`
- **Add conditions**: Add condition places and transitions with predicates
- **Add branching**: Use `yawl_pattern_reference:exclusive_choice` for conditional flows

---

## 2. HTTP Webhook Integration

### Overview

This example demonstrates integrating external HTTP services into a workflow using the YAWL Web Service Integration Framework (WSIF).

### Workflow Specification

```erlang
%% File: examples/webhook_workflow.erl
-module(webhook_workflow).
-export([run/0]).

run() ->
    %% Start inets for HTTP support
    application:ensure_all_started(inets),

    %% Start WSIF
    {ok, _WsifPid} = yawl_wsif:start_wsif(),

    %% Register an external service
    {ok, _ServiceId} = yawl_wsif:register_service(<<"jsonplaceholder">>, [
        {endpoint, <<"https://jsonplaceholder.typicode.com">>},
        {port, <<"HTTP">>}
    ]),

    %% Define workflow with external service call
    Spec = #{
        places => [p_start, p_fetch, p_process, p_end],
        transitions => #{
            t_start => #{
                preset => [p_start],
                produce => #{p_fetch => [fetch]}
            },
            t_fetch => #{
                preset => [p_fetch],
                is_service => true,
                service => http_get,
                timeout => 5000,
                produce => #{p_process => [process_data]}
            },
            t_process => #{
                preset => [p_process],
                is_task => true,
                task_place => p_process_work,
                produce => #{p_end => [done]}
            }
        },
        init_marking => #{p_start => [start]},
        end_place => p_end,
        start_token => start
    },

    %% Start engine
    {ok, Engine} = wf_engine:start_link(#{
        spec => Spec,
        org => #{participants => [alice]},
        seed => 42,
        now => 0
    }),

    %% Start case with URL to fetch
    {ok, CaseId} = wf_engine:start_case(Engine, #{
        data => #{url => "/posts/1"}
    }, 0),

    %% Check for service requests
    timer:sleep(100),
    Events = wf_engine:drain_events(Engine),
    io:format("Service events: ~p~n", [Events]),

    case Events of
        [#service_request{req_id = ReqId, service = http_get, data = Data}] ->
            io:format("Service request: ~p~n", [Data]),

            %% Make the actual HTTP call
            Url = maps:get(url, Data, "/posts/1"),
            FullUrl = <<"https://jsonplaceholder.typicode.com", Url/binary>>,

            case httpc:request(get, {binary_to_list(FullUrl), []}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    Response = jsx:decode(list_to_binary(Body)),
                    io:format("Got response: ~p~n", [Response]),

                    %% Reply to the service request
                    wf_engine:service_reply(Engine, ReqId, CaseId, Response, 100);
                {error, Reason} ->
                    io:format("HTTP error: ~p~n", [Reason]),
                    wf_engine:service_reply(Engine, ReqId, CaseId, #{error => Reason}, 100)
            end;
        _ ->
            io:format("No service requests yet~n")
    end,

    %% Process the work item
    Offered = wf_engine:offered_workitems(Engine, CaseId),
    case Offered of
        [{t_process, WiId}] ->
            wf_engine:allocate(Engine, WiId, alice, 200),
            wf_engine:start_work(Engine, WiId, alice, 300),
            wf_engine:complete(Engine, WiId, alice, #{processed => true}, 400);
        _ ->
            io:format("No process task found~n")
    end,

    %% Check final state
    State = wf_engine:case_state(Engine, CaseId),
    io:format("Final state: ~p~n", [State]),

    %% Cleanup
    gen_server:stop(Engine),
    yawl_wsif:stop_wsif(),
    ok.
```

### Expected Output

```
Service events: [{service_request,#{req_id => <<...>>, service => http_get}}]
Got response: #{<<"userId">> => 1, <<"id">> => 1, <<"title">> => ...}
Final state: completed
```

### How to Run

```bash
# Compile
erlc -I include -o ebin examples/webhook_workflow.erl

# Run
erl -pa ebin -noshell -s webhook_workflow run -s init stop
```

### How to Extend

- **Add retry logic**: Implement circuit breaker pattern using `yawl_breaker`
- **Multiple services**: Register multiple services and use `exclusive_choice` for routing
- **SOAP integration**: Use `yawl_wsif:invoke_soap` for SOAP services
- **WSDL parsing**: Use `yawl_schema:parse_specification` to generate client stubs

---

## 3. Database Persistence

### Overview

This example demonstrates using Mnesia for persistent workflow storage, including case recovery after system restart.

### Workflow Specification

```erlang
%% File: examples/persistence_workflow.erl
-module(persistence_workflow).
-export([run/0, recover/0]).

run() ->
    %% Initialize Mnesia database
    ok = wf_persistence:init_db(),

    %% Define workflow spec
    Spec = #{
        places => [p_start, p_step1, p_step2, p_step3, p_end],
        transitions => #{
            t_start => #{
                preset => [p_start],
                produce => #{p_step1 => [go1]}
            },
            t_step1 => #{
                preset => [p_step1],
                is_task => true,
                task_place => p_step1_work,
                produce => #{p_step2 => [go2]}
            },
            t_step2 => #{
                preset => [p_step2],
                is_task => true,
                task_place => p_step2_work,
                produce => #{p_step3 => [go3]}
            },
            t_step3 => #{
                preset => [p_step3],
                is_task => true,
                task_place => p_step3_work,
                produce => #{p_end => [done]}
            }
        },
        init_marking => #{p_start => [start]},
        end_place => p_end,
        start_token => start
    },

    %% Start engine
    {ok, Engine} = wf_engine:start_link(#{
        spec => Spec,
        org => #{participants => [alice]},
        seed => 42,
        now => 0
    }),

    %% Start case
    {ok, CaseId} = wf_engine:start_case(Engine, #{
        data => #{customer => <<"cust_123">>, amount => 1000}
    }, 0),

    io:format("Started case: ~s~n", [CaseId]),

    %% Save case to database
    Case = get_case_state(Engine, CaseId),
    ok = wf_persistence:save_case(Case),
    io:format("Saved case to database~n"),

    %% Complete step 1
    {ok, WiId1} = allocate_and_complete_task(Engine, CaseId, step1, alice),
    ok = wf_persistence:save_workitem(WI1),
    io:format("Completed step 1~n"),

    %% Save updated case
    Case1 = get_case_state(Engine, CaseId),
    ok = wf_persistence:save_case(Case1),

    %% Log an event
    ok = wf_persistence:save_event(CaseId, {step_completed, step1, 100}),

    %% Complete step 2
    {ok, WiId2} = allocate_and_complete_task(Engine, CaseId, step2, alice),
    ok = wf_persistence:save_workitem(WI2),
    io:format("Completed step 2~n"),

    %% Create a checkpoint
    {ok, CheckpointId} = wf_persistence:create_checkpoint(#{
        cases => [CaseId],
        timestamp => erlang:system_time(millisecond)
    }),
    io:format("Created checkpoint: ~s~n", [CheckpointId]),

    %% List active cases
    {ok, ActiveCases} = wf_persistence:list_active_cases(),
    io:format("Active cases: ~p~n", [ActiveCases]),

    %% Load events
    {ok, Events} = wf_persistence:load_events(CaseId),
    io:format("Events: ~p~n", [Events]),

    %% Complete step 3
    {ok, _WiId3} = allocate_and_complete_task(Engine, CaseId, step3, alice),
    io:format("Completed step 3~n"),

    %% Check final state
    State = wf_engine:case_state(Engine, CaseId),
    io:format("Final state: ~p~n", [State]),

    %% Cleanup
    gen_server:stop(Engine),
    ok.

recover() ->
    %% Initialize database
    ok = wf_persistence:init_db(),

    %% List active cases for recovery
    {ok, ActiveCases} = wf_persistence:list_active_cases(),
    io:format("Found ~p active cases to recover~n", [length(ActiveCases)]),

    %% Restore from checkpoint
    case wf_persistence:restore_from_checkpoint() of
        {ok, CheckpointState} ->
            io:format("Restored from checkpoint: ~p~n", [CheckpointState]);
        {error, not_found} ->
            io:format("No checkpoint found~n")
    end,

    %% Process each active case
    lists:foreach(fun(CaseMap) ->
        CaseId = maps:get(case_id, CaseMap),
        io:format("Recovering case: ~s~n", [CaseId]),

        %% Load case from database
        {ok, Case} = wf_persistence:load_case(CaseId),
        io:format("Loaded case: ~p~n", [Case#wf_case.status]),

        %% Load events
        {ok, Events} = wf_persistence:load_events(CaseId),
        io:format("Case has ~p events~n", [length(Events)])
    end, ActiveCases),

    ok.

%% Helper functions

get_case_state(Engine, CaseId) ->
    %% Get the internal case state from engine
    %% This is a simplified version - actual implementation
    %% would need to access the engine's internal state
    {ok, Cases} = gen_server:call(Engine, get_cases),
    maps:get(CaseId, Cases).

allocate_and_complete_task(Engine, CaseId, TaskName, User) ->
    Offered = wf_engine:offered_workitems(Engine, CaseId),
    case lists:keyfind(TaskName, 1, Offered) of
        {TaskName, WiId} ->
            wf_engine:allocate(Engine, WiId, User, 0),
            wf_engine:start_work(Engine, WiId, User, 0),
            wf_engine:complete(Engine, WiId, User, #{completed => true}, 0),
            {ok, WiId};
        false ->
            {error, not_found}
    end.
```

### Expected Output

```
Started case: <<case_abc123...>>
Saved case to database
Completed step 1
Completed step 2
Created checkpoint: <<ckpt_xyz789...>>
Active cases: [#{case_id => <<...>>, status => running}]
Events: [{step_completed, step1, 100}]
Completed step 3
Final state: completed
```

### How to Run

```bash
# Compile
erlc -I include -o ebin examples/persistence_workflow.erl

# Run
erl -pa ebin -noshell -s persistence_workflow run -s init stop

# Recovery
erl -pa ebin -noshell -s persistence_workflow recover -s init stop
```

### How to Extend

- **Distributed Mnesia**: Set up Mnesia across multiple nodes for HA
- **Event sourcing**: Use `wf_audit_log` for complete event history
- **Snapshots**: Periodic snapshots for faster recovery
- **Replication**: Multi-node replication for disaster recovery

---

## 4. Active Token Workflow

### Overview

Active tokens are autonomous agents that can navigate through the workflow independently, making decisions based on their environment and internal state.

### Workflow Specification

```erlang
%% File: examples/active_token_workflow.erl
-module(active_token_workflow).
-export([run/0]).

%% Active tokens carry their own execution context and can
%% make routing decisions dynamically based on workflow state

run() ->
    %% Define a workflow with decision points
    Spec = #{
        places => [p_start, p_decision, p_process_a, p_process_b, p_end],
        transitions => #{
            t_start => #{
                preset => [p_start],
                produce => #{p_decision => [decide]}
            },
            %% Decision point - routing based on token data
            t_decision => #{
                preset => [p_decision],
                produce => #{p_process_a => [go_a],  %% Both paths enabled
                           p_process_b => [go_b]}
            },
            t_process_a => #{
                preset => [p_process_a],
                is_task => true,
                task_place => p_process_a_work,
                produce => #{p_end => [done_a]}
            },
            t_process_b => #{
                preset => [p_process_b],
                is_task => true,
                task_place => p_process_b_work,
                produce => #{p_end => [done_b]}
            }
        },
        init_marking => #{p_start => [start]},
        end_place => p_end,
        start_token => start
    },

    %% Start engine
    {ok, Engine} = wf_engine:start_link(#{
        spec => Spec,
        org => #{participants => [alice, bob]},
        seed => 42,
        now => 0
    }),

    %% Start case with active token data
    {ok, CaseId} = wf_engine:start_case(Engine, #{
        data => #{
            token_type => active,
            priority => high,
            route => a  %% Initial route preference
        }
    }, 0),

    io:format("Started active token case: ~s~n", [CaseId]),

    %% Check enabled transitions after decision
    timer:sleep(50),
    Enabled = wf_engine:enabled(Engine, CaseId),
    io:format("Enabled transitions: ~p~n", [Enabled]),

    %% The active token should have created both work items
    Offered = wf_engine:offered_workitems(Engine, CaseId),
    io:format("Offered work items: ~p~n", [Offered]),

    %% Simulate token choosing path A based on data
    case lists:keyfind(process_a, 1, Offered) of
        {process_a, WiIdA} ->
            wf_engine:allocate(Engine, WiIdA, alice, 1),
            wf_engine:start_work(Engine, WiIdA, alice, 2),
            wf_engine:complete(Engine, WiIdA, alice, #{
                chosen_path => a,
                reason => priority_high
            }, 3),
            io:format("Token chose path A~n");
        false ->
            io:format("Path A not available~n")
    end,

    %% Check final state
    State = wf_engine:case_state(Engine, CaseId),
    io:format("Final state: ~p~n", [State]),

    %% Get case log
    Log = wf_engine:case_log(Engine, CaseId),
    io:format("Case log: ~p~n", [Log]),

    gen_server:stop(Engine),
    ok.
```

### Expected Output

```
Started active token case: <<case_abc123...>>
Enabled transitions: [t_process_a, t_process_b]
Offered work items: [{process_a, <<...>>}, {process_b, <<...>>}]
Token chose path A
Final state: completed
```

### How to Run

```bash
# Compile
erlc -I include -o ebin examples/active_token_workflow.erl

# Run
erl -pa ebin -noshell -s active_token_workflow run -s init stop
```

### How to Extend

- **Token coordination**: Use `wf_ipc` for inter-token communication
- **Token gossip**: Implement information sharing between tokens
- **Swarm behavior**: Multiple tokens cooperating on a task
- **Token lifecycle**: Spawn, merge, and terminate tokens dynamically

---

## 5. RL Agent Intervention

### Overview

Reinforcement Learning agents can observe workflow execution and recommend interventions to optimize performance. This example shows an RL agent learning to route work items.

### Workflow Specification

```erlang
%% File: examples/rl_agent_workflow.erl
-module(rl_agent_workflow).
-export([run/0, train_agent/1]).

run() ->
    %% Start the predictive mining system
    {ok, _PredPid} = predictive_mining:start_link(),

    %% Start RL agent
    AgentId = <<"workflow_optimizer">>,
    {ok, RlPid} = rl_agent:start_link(AgentId, #{
        pattern_id => <<"sequence">>,
        workflow_id => <<"order_processing">>,
        state_space => #{queue_length => [short, medium, long]},
        action_space => #{actions => [reroute, prioritize, parallelize, no_action]},
        learning_rate => 0.1,
        exploration_rate => 0.5
    }),

    io:format("Started RL agent: ~p~n", [RlPid]),

    %% Define workflow with decision points
    Spec = #{
        places => [p_start, p_route, p_fast, p_slow, p_end],
        transitions => #{
            t_start => #{
                preset => [p_start],
                produce => #{p_route => [route]}
            },
            t_route => #{
                preset => [p_route],
                produce => #{p_fast => [fast], p_slow => [slow]}
            },
            t_fast => #{
                preset => [p_fast],
                is_task => true,
                task_place => p_fast_work,
                produce => #{p_end => [done_fast]}
            },
            t_slow => #{
                preset => [p_slow],
                is_task => true,
                task_place => p_slow_work,
                produce => #{p_end => [done_slow]}
            }
        },
        init_marking => #{p_start => [start]},
        end_place => p_end,
        start_token => start
    },

    %% Start engine
    {ok, Engine} = wf_engine:start_link(#{
        spec => Spec,
        org => #{participants => [alice]},
        seed => 42,
        now => 0
    }),

    %% Simulate multiple workflow executions with agent guidance
    Results = lists:map(fun(N) ->
        io:format("~n--- Execution ~p ---~n", [N]),

        %% Start case
        {ok, CaseId} = wf_engine:start_case(Engine, #{
            data => #{queue_length => rand:uniform(3)}
        }, 0),

        %% Get agent recommendation for current state
        StateFeatures = #{
            queue_length => case rand:uniform(3) of
                1 -> short;
                2 -> medium;
                3 -> long
            end,
            pending_cases => rand:uniform(10)
        },

        {ok, Action} = rl_agent:recommend_action(AgentId, StateFeatures),
        io:format("Agent recommendation: ~p~n", [Action]),

        %% Apply action if applicable
        case Action#rl_action.action_type of
            prioritize ->
                io:format("Agent prioritized this case~n");
            reroute ->
                io:format("Agent suggested rerouting~n");
            no_action ->
                ok
        end,

        %% Complete the workflow
        Offered = wf_engine:offered_workitems(Engine, CaseId),
        {_, WiId} = hd(Offered),
        wf_engine:allocate(Engine, WiId, alice, N*10),
        wf_engine:start_work(Engine, WiId, alice, N*10+1),
        wf_engine:complete(Engine, WiId, alice, #{}, N*10+2),

        %% Record reward based on completion time
        Reward = case Action#rl_action.action_type of
            prioritize when StateFeatures#{queue_length := long} -> 1.0;
            no_action when StateFeatures#{queue_length := short} -> 0.5;
            _ -> -0.1
        end,
        rl_agent:record_reward(AgentId, Reward),

        %% Observe next state
        rl_agent:observe_next_state(AgentId, #{
            queue_length => short,
            pending_cases => 0
        }),

        %% Get agent statistics
        {ok, Stats} = rl_agent:get_statistics(AgentId),
        #{intervention_count := IC, total_reward := TR} = Stats,
        io:format("Agent stats: interventions=~p, total_reward=~p~n", [IC, TR]),

        N
    end, lists:seq(1, 10)),

    %% Get final agent statistics
    {ok, FinalStats} = rl_agent:get_statistics(AgentId),
    io:format("~nFinal agent statistics: ~p~n", [FinalStats]),

    %% Get learned policy
    {ok, Policy} = rl_agent:get_policy(AgentId),
    io:format("Learned policy: ~p~n", [Policy]),

    %% Cleanup
    rl_agent:stop(AgentId),
    predictive_mining:stop(),
    gen_server:stop(Engine),
    ok.

%% Train the agent with multiple episodes
train_agent(Episodes) when Episodes > 0 ->
    io:format("Training agent for ~p episodes...~n", [Episodes]),

    AgentId = <<"trainer_agent">>,
    {ok, _Pid} = rl_agent:start_link(AgentId, #{
        state_space => #{},
        action_space => #{},
        learning_rate => 0.1,
        exploration_rate => 1.0
    }),

    lists:foreach(fun(Episode) ->
        %% Simulate state
        StateFeatures = #{value => rand:uniform()},

        %% Get recommendation
        {ok, Action} = rl_agent:recommend_action(AgentId, StateFeatures),

        %% Simulated reward
        Reward = rand:uniform() - 0.3,
        rl_agent:record_reward(AgentId, Reward),

        %% Next state
        rl_agent:observe_next_state(AgentId, #{value => rand:uniform()}),

        case Episode rem 100 of
            0 -> io:format("Episode ~p completed~n", [Episode]);
            _ -> ok
        end
    end, lists:seq(1, Episodes)),

    %% Get final stats
    {ok, Stats} = rl_agent:get_statistics(AgentId),
    io:format("Training complete. Final stats: ~p~n", [Stats]),

    rl_agent:stop(AgentId),
    ok.
```

### Expected Output

```
Started RL agent: <0.123.0>

--- Execution 1 ---
Agent recommendation: {rl_action,prioritize,...}
Agent prioritized this case
Agent stats: interventions=1, total_reward=0.5

...

Final agent statistics: #{intervention_count => 3, total_reward => 1.2, ...}
Learned policy: #{exploration_rate => 0.45, learning_rate => 0.1, ...}
```

### How to Run

```bash
# Compile
erlc -I include -o ebin examples/rl_agent_workflow.erl

# Run
erl -pa ebin -noshell -s rl_agent_workflow run -s init stop

# Train agent
erl -pa ebin -noshell -s rl_agent_workflow train 1000 -s init stop
```

### How to Extend

- **Deep Q-Networks**: Replace tabular Q-learning with neural networks
- **Multi-agent RL**: Multiple agents cooperating/competing
- **Transfer learning**: Pre-trained agents for new workflows
- **Curriculum learning**: Progressive difficulty training

---

## 6. Predictive Monitoring

### Overview

This example demonstrates using predictive models to detect anomalies in workflow execution and predict potential failures.

### Workflow Specification

```erlang
%% File: examples/predictive_monitoring.erl
-module(predictive_monitoring).
-export([run/0, monitor_case/2]).

run() ->
    %% Start predictive mining system
    {ok, _PredPid} = predictive_mining:start_link(),

    %% Load prediction models (simulated)
    ok = predictive_mining:load_model(<<"next_activity_predictor">>),
    ok = predictive_mining:load_model(<<"outcome_predictor">>),

    io:format("Loaded prediction models~n"),

    %% Define a workflow to monitor
    Spec = #{
        places => [p_start, p_validate, p_process, p_approve, p_reject, p_end],
        transitions => #{
            t_start => #{
                preset => [p_start],
                produce => #{p_validate => [go]}
            },
            t_validate => #{
                preset => [p_validate],
                is_task => true,
                task_place => p_validate_work,
                produce => #{p_process => [valid]}
            },
            t_process => #{
                preset => [p_process],
                is_task => true,
                task_place => p_process_work,
                produce => #{p_approve => [pending_approval]}
            },
            t_approve => #{
                preset => [p_approve],
                is_task => true,
                task_place => p_approve_work,
                produce => #{p_end => [approved]}
            },
            t_reject => #{
                preset => [p_reject],
                is_task => true,
                task_place => p_reject_work,
                produce => #{p_end => [rejected]}
            }
        },
        init_marking => #{p_start => [start]},
        end_place => p_end,
        start_token => start
    },

    %% Start engine
    {ok, Engine} = wf_engine:start_link(#{
        spec => Spec,
        org => #{participants => [alice, bob]},
        seed => 42,
        now => 0
    }),

    %% Start a case to monitor
    {ok, CaseId} = wf_engine:start_case(Engine, #{
        data => #{customer => <<"cust_456">>, amount => 5000}
    }, 0),

    io:format("Started case for monitoring: ~s~n", [CaseId]),

    %% Monitor the case
    monitor_case(Engine, CaseId),

    %% Demonstrate prediction on trace
    Trace = [t_start, t_validate, t_process],
    {ok, NextPredictions} = predictive_mining:predict_next_activity(CaseId, Trace),
    io:format("Next activity predictions: ~p~n", [NextPredictions]),

    %% Predict remaining time
    {ok, RemainingTime} = predictive_mining:predict_remaining_time(CaseId, Trace),
    io:format("Predicted remaining time: ~pms~n", [RemainingTime]),

    %% Predict outcome
    {ok, Outcome, Confidence} = predictive_mining:predict_outcome(CaseId, Trace),
    io:format("Predicted outcome: ~p (confidence: ~p)~n", [Outcome, Confidence]),

    %% Start telemetry for monitoring
    {ok, _TelPid} = yawl_telemetry:start_telemetry(),

    %% Create a monitoring span
    {ok, SpanId} = yawl_telemetry:start_span(sequence, CaseId, #{
        case_id => CaseId,
        amount => 5000
    }),
    io:format("Started telemetry span: ~p~n", [SpanId]),

    %% Add custom attributes
    ok = yawl_telemetry:span_attribute(SpanId, monitoring_level, detailed),

    %% Log an event
    ok = yawl_telemetry:span_event(SpanId, prediction_made),

    %% End span
    ok = yawl_telemetry:end_span(SpanId, {ok, completed}, ok),

    %% Get metrics summary
    Summary = yawl_telemetry:get_metrics_summary(),
    io:format("Metrics summary: ~p~n", [Summary]),

    %% Check system health
    {ok, Health} = yawl_telemetry:system_health(),
    io:format("System health: ~p~n", [Health]),

    %% Cleanup
    yawl_telemetry:stop_telemetry(),
    predictive_mining:stop(),
    gen_server:stop(Engine),
    ok.

%% Monitor a case for anomalies
monitor_case(Engine, CaseId) ->
    monitor_case(Engine, CaseId, 0).

monitor_case(Engine, CaseId, Iteration) when Iteration < 5 ->
    %% Get current state
    State = wf_engine:case_state(Engine, CaseId),

    case State of
        completed ->
            io:format("Case completed successfully~n"),
            ok;
        _ ->
            %% Check for anomalies
            case detect_anomaly(Engine, CaseId) of
                {anomaly, Type, Severity} ->
                    io:format("ANOMALY DETECTED: ~p (severity: ~p)~n", [Type, Severity]),

                    %% Take corrective action based on severity
                    case Severity of
                        high ->
                            io:format("Taking immediate corrective action~n"),
                            wf_engine:cancel_case(Engine, CaseId, 0);
                        medium ->
                            io:format("Logging alert for review~n");
                        low ->
                            io:format("Anomaly noted, continuing~n")
                    end;
                ok ->
                    io:format("Iteration ~p: No anomalies~n", [Iteration])
            end,

            timer:sleep(100),
            monitor_case(Engine, CaseId, Iteration + 1)
    end;

monitor_case(_Engine, _CaseId, _Iteration) ->
    io:format("Monitoring timeout~n"),
    ok.

%% Detect anomalies in workflow execution
detect_anomaly(Engine, CaseId) ->
    %% Get case data
    Log = wf_engine:case_log(Engine, CaseId),
    Receipts = wf_engine:drain_receipts(Engine, CaseId),

    %% Check various anomaly indicators
    Anomalies = [
        check_duration_anomaly(Log),
        check_receipt_anomaly(Receipts),
        check_state_anomaly(Engine, CaseId)
    ],

    %% Filter out 'ok' results
    case [A || A <- Anomalies, A =/= ok] of
        [] -> ok;
        [FirstAnomaly | _] -> FirstAnomaly
    end.

check_duration_anomaly(Log) ->
    %% Check if steps are taking too long
    case length(Log) > 10 of
        true -> {anomaly, long_execution, medium};
        false -> ok
    end.

check_receipt_anomaly(Receipts) ->
    %% Check receipt patterns
    case Receipts of
        [] -> ok;
        _ ->
            %% Look for unexpected receipt patterns
            ok
    end.

check_state_anomaly(Engine, CaseId) ->
    State = wf_engine:case_state(Engine, CaseId),
    case State of
        {error, not_found} -> {anomaly, case_not_found, high};
        suspended -> {anomaly, case_suspended, low};
        _ -> ok
    end.
```

### Expected Output

```
Loaded prediction models
Started case for monitoring: <<case_abc123...>>
Iteration 0: No anomalies
Next activity predictions: [{t_approve,0.6},{t_reject,0.4}]
Predicted remaining time: 120000
Predicted outcome: success (confidence: 0.75)
Started telemetry span: <0.234.0>
Metrics summary: #{total_executions => 1, ...}
System health: #{uptime_ms => 100, ...}
```

### How to Run

```bash
# Compile
erlc -I include -o ebin examples/predictive_monitoring.erl

# Run
erl -pa ebin -noshell -s predictive_monitoring run -s init stop
```

### How to Extend

- **Custom anomaly detectors**: Implement domain-specific detection logic
- **Prometheus integration**: Export metrics for Grafana dashboards
- **Alert routing**: Integrate with notification systems (email, Slack)
- **Model retraining**: Periodic model updates based on new data

---

## Common Patterns

### Error Handling

```erlang
case wf_engine:start_case(Engine, Options, 0) of
    {ok, CaseId} ->
        process_case(Engine, CaseId);
    {error, Reason} ->
        logger:error("Failed to start case: ~p", [Reason]),
        handle_error(Reason)
end
```

### Telemetry Integration

```erlang
%% Start span
{ok, SpanId} = yawl_telemetry:start_span(PatternType, PatternId),

%% Add attributes
yawl_telemetry:span_attribute(SpanId, key, value),

%% End span
yawl_telemetry:end_span(SpanId, Result, Status).
```

### Work Item Processing

```erlang
%% Allocate
{ok, _} = wf_engine:allocate(Engine, WiId, User, Now),

%% Start work
{ok, _} = wf_engine:start_work(Engine, WiId, User, Now),

%% Complete with data
{ok, _} = wf_engine:complete(Engine, WiId, User, Data, Now).
```

### Service Integration

```erlang
%% Drain service requests
Events = wf_engine:drain_events(Engine),

%% Process each request
lists:foreach(fun(#service_request{req_id = ReqId, ...} = Req) ->
    Result = handle_service_request(Req),
    wf_engine:service_reply(Engine, ReqId, CaseId, Result, Now)
end, Events).
```

---

## Testing Your Integrations

```erlang
%% EUnit test example
-include_lib("eunit/include/eunit.hrl").

integration_test() ->
    %% Setup
    {ok, Engine} = wf_engine:start_link(#{spec => test_spec()}),

    %% Execute
    {ok, CaseId} = wf_engine:start_case(Engine, #{}, 0),

    %% Assert
    ?assertMatch(completed, wf_engine:case_state(Engine, CaseId)),

    %% Cleanup
    gen_server:stop(Engine).
```

---

## Further Reading

- `src/wf/wf_engine.erl` - Core workflow engine
- `src/wf/wf_persistence.erl` - Database persistence
- `src/patterns/rl_agent.erl` - Reinforcement learning agent
- `src/mining/predictive_mining.erl` - Predictive analytics
- `src/yawl/yawl_telemetry.erl` - Observability and monitoring
- `src/yawl/yawl_wsif.erl` - Web service integration

---

**Copyright (c) 2024 CRE Team. All rights reserved.**
