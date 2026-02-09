# CRE Architecture Diagrams

This document provides visual architecture diagrams for the CRE (Common Runtime Environment) system using Mermaid syntax. CRE is an Erlang/OTP YAWL workflow engine with Petri Net patterns.

## Table of Contents

1. [System Overview](#system-overview)
2. [gen_pnet Behavior](#gen_pnet-behavior)
3. [gen_yawl Wrapper](#gen_yawl-wrapper)
4. [Workflow Execution](#workflow-execution)
5. [Active Token System](#active-token-system)
6. [RL Agent Integration](#rl-agent-integration)
7. [Data Flow - Mining Pipeline](#data-flow-mining-pipeline)

---

## System Overview

```mermaid
graph TB
    subgraph "Client Layer"
        API["REST API / CLI"]
        Master["cre_master"]
    end

    subgraph "Worker Layer"
        Worker["gen_yawl Worker"]
        Worker2["gen_yawl Worker N"]
    end

    subgraph "Core Layer - gen_pnet"
        PNet["gen_pnet Behavior"]
        Marking["pnet_marking<br/>(Token Management)"]
        Mode["pnet_mode<br/>(Mode Enumeration)"]
        Choice["pnet_choice<br/>(Conflict Resolution)"]
        Receipt["pnet_receipt<br/>(Audit Records)"]
    end

    subgraph "Petri Net Layer"
        Types["pnet_types<br/>(Data Structures)"]
        Algebra["Petri Net Algebra"]
    end

    subgraph "Workflow Layer"
        YAWL["YAWL Compiler"]
        YAWLExec["YAWL Executor"]
        WFEngine["wf_engine<br/>(Workflow Utilities)"]
        WFState["yawl_state<br/>(Case State)"]
    end

    subgraph "Patterns Layer - 43 Control Flow Patterns"
        Basic["Basic Patterns"]
        Advanced["Advanced Patterns"]
        RL["RL Agent Patterns"]
        Strategy["Strategy Patterns"]
    end

    subgraph "Mining Layer"
        Discovery["process_discovery<br/>(Alpha, Heuristic)"]
        Predictive["predictive_mining<br/>(RNN Models)"]
        Training["pred_training<br/>(Model Training)"]
    end

    subgraph "Data Layer"
        ETS["ETS Tables"]
        Storage["Persistent Storage"]
    end

    API --> Master
    Master --> Worker
    Master --> Worker2

    Worker --> PNet
    Worker2 --> PNet

    PNet --> Marking
    PNet --> Mode
    PNet --> Choice
    PNet --> Receipt

    Marking --> Types
    Mode --> Types
    Choice --> Algebra

    Worker --> YAWL
    YAWL --> YAWLExec
    YAWLExec --> WFEngine
    WFEngine --> WFState

    YAWLExec --> Patterns
    Patterns --> Basic
    Patterns --> Advanced
    Patterns --> RL
    Patterns --> Strategy

    RL --> Mining
    Mining --> Discovery
    Mining --> Predictive
    Mining --> Training

    Worker --> ETS
    WFState --> Storage

    style API fill:#e1f5fe
    style PNet fill:#fff3e0
    style YAWL fill:#f3e5f5
    style Patterns fill:#e8f5e9
    style Mining fill:#fce4ec
```

---

## gen_pnet Behavior

```mermaid
sequenceDiagram
    participant Client as Client Code
    participant PNet as gen_pnet
    participant NetMod as Net Module
    participant Marking as pnet_marking
    participant Mode as pnet_mode
    participant Receipt as pnet_receipt

    Note over Client,NetMod: gen_pnet Behavior Lifecycle

    Client->>PNet: start_link(NetMod, UsrInfo)
    activate PNet
    PNet->>NetMod: init(UsrInfo)
    NetMod-->>PNet: {ok, InitialMarking}
    PNet->>Marking: hash(InitialMarking)
    PNet-->>Client: {ok, Pid}
    deactivate PNet

    Note over Client,NetMod: Transition Firing

    Client->>PNet: fire(Transition, Options)
    activate PNet

    PNet->>Marking: get(CurrentMarking)
    PNet->>NetMod: preset(Transition)
    NetMod-->>PNet: [Place1, Place2, ...]

    PNet->>Mode: enum_modes(Preset, CurrentMarking)
    Mode-->>PNet: [Mode1, Mode2, ...]

    alt Multiple Modes (Conflict)
        PNet->>NetMod: decision(Transition, Modes, UsrInfo)
        NetMod-->>PNet: SelectedMode
    else Single Mode
        PNet->>PNet: Mode = hd(Modes)
    end

    PNet->>NetMod: fire(Transition, Mode, UsrInfo)
    NetMod-->>PNet: ProduceMap

    PNet->>Marking: consume(Preset, Mode, CurrentMarking)
    PNet->>Marking: produce(Postset, ProduceMap, ConsumedMarking)
    Marking-->>PNet: NewMarking

    PNet->>Receipt: make(BeforeHash, AfterHash, Move)
    PNet->>PNet: append_receipt(Receipt)

    PNet->>NetMod: post(Transition, Mode, ProduceMap, UsrInfo)

    PNet-->>Client: {ok, Receipt}
    deactivate PNet

    Note over Client,NetMod: State Query

    Client->>PNet: marking()
    PNet-->>Client: CurrentMarking

    Client->>PNet: receipt(ReceiptId)
    PNet-->>Client: ReceiptRecord
```

---

## gen_yawl Wrapper

```mermaid
graph TB
    subgraph "gen_yawl Extension Layer"
        direction TB
        YAWL["gen_yawl"]

        subgraph "YAWL Callbacks"
            Init["init/1"]
            Compile["compile/1<br/>(YAWL -> Petri Net)"]
            Validate["validate/1"]
            Execute["execute/2"]
        end

        subgraph "YAWL Components"
            Compiler["yawl_compile<br/>(Workflow Compilation)"]
            Validator["yawl_validate<br/>(Soundness Checking)"]
            Executor["yawl_executor<br/>(Execution Engine)"]
            Parser["yawl_parser<br/>(XML/YAWL Parsing)"]
        end

        subgraph "Workflow Utilities"
            WFTimer["wf_persistent_timer<br/>(Durable Timers)"]
            WFTask["wf_task<br/>(Task Management)"]
            WFScope["wf_scope<br/>(Scope Handling)"]
        end
    end

    subgraph "gen_pnet Base Layer"
        direction TB
        PNet["gen_pnet"]
        PNetCallbacks["pnet_net callbacks"]
    end

    YAWL -.->|implements| PNetCallbacks
    YAWL --> Compiler
    YAWL --> Validator
    YAWL --> Executor
    YAWL --> Parser

    Compiler -->|generates| PNetCallbacks
    Validator -->|checks| PNetCallbacks

    Executor --> WFTimer
    Executor --> WFTask
    Executor --> WFScope

    PNetCallbacks --> PNet

    style YAWL fill:#f3e5f5
    style PNet fill:#fff3e0
    style Compiler fill:#e1f5fe
    style Validator fill:#e8f5e9
    style Executor fill:#fff9c4
```

---

## Workflow Execution

```mermaid
sequenceDiagram
    participant Client as Client
    participant Master as cre_master
    participant Worker as gen_yawl_worker
    participant YAWL as YAWL Executor
    participant State as yawl_state
    participant PNet as gen_pnet
    participant Timer as wf_persistent_timer

    Note over Client,Timer: Workflow Case Execution

    Client->>Master: start_case(WorkflowDef, CaseData)
    activate Master
    Master->>Worker: start_worker(CaseId, WorkflowDef)
    activate Worker

    Worker->>YAWL: compile(WorkflowDef)
    YAWL-->>Worker: {ok, PetriNet}

    Worker->>State: new(CaseId, CaseData)
    Worker->>State: mark_created()
    Worker->>PNet: start_link(PetriNet, InitialMarking)

    Worker->>YAWL: execute(CaseId)
    activate YAWL

    YAWL->>PNet: enabled_transitions()
    PNet-->>YAWL: [T1, T2, ...]

    loop For each enabled transition
        YAWL->>PNet: fire(Transition, Mode)

        alt Task transition
            YAWL->>State: add_workitem(WorkItem)
            YAWL->>Timer: start_timer(Timeout)
            YAWL-->>Client: {wait, WorkItem}
        else Automatic transition
            YAWL->>PNet: fire(NextTransition)
        end
    end

    Note over Client,Timer: Work Item Completion

    Client->>Master: complete_workitem(CaseId, WorkItemId, Result)
    Master->>Worker: complete(WorkItemId, Result)

    Worker->>State: remove_workitem(WorkItemId)
    Worker->>State: update_data(Result)
    Worker->>YAWL: resume()
    YAWL->>PNet: fire(NextTransition)

    alt Case Complete
        YAWL->>State: mark_completed()
        YAWL-->>Worker: {complete, FinalState}
        Worker-->>Master: case_complete
        Master-->>Client: {ok, CaseResult}
    else More Work
        YAWL-->>Client: {continue, NewWorkItems}
    end

    deactivate YAWL
    deactivate Worker
    deactivate Master
```

---

## Active Token System

```mermaid
stateDiagram-v2
    [*] --> Unmarked

    Unmarked --> Marked: Token Arrives
    Marked --> Unmarked: Token Consumed

    state Marked {
        [*] --> Active
        Active --> Enabled: Transition Enabled
        Enabled --> Firing: Mode Selected
        Firing --> Active: Fire Complete

        state Enabled {
            [*] --> SingleMode
            [*] --> MultiMode: Conflict

            SingleMode --> [*]
            MultiMode --> Decision
            Decision --> [*]
        }
    }

    note right of Marked
        Token Lifecycle:
        - Active: Token in place
        - Enabled: Can enable transition
        - Firing: Being consumed
    end note

    Marked --> [*]: Place Empty

    Marked --> Archived: Receipt Created
    Archived --> [*]
```

```mermaid
graph LR
    subgraph "Token Flow"
        direction TB
        T1["Token 1"]
        T2["Token 2"]
        T3["Token 3"]

        P1["Place A"]
        P2["Place B"]
        P3["Place C"]
        P4["Place D"]

        TR1["Transition X"]
        TR2["Transition Y"]
    end

    subgraph "Active Token Management"
        AT["active_token Coordinator"]
        ATQ["Token Queue"]
        ATLock["Token Lock"]
    end

    T1 --> P1
    T2 --> P1
    T3 --> P2

    P1 --> TR1
    P2 --> TR1
    P1 --> TR2

    TR1 --> P3
    TR2 --> P4

    TR1 --> AT
    TR2 --> AT

    AT --> ATQ
    AT --> ATLock

    style AT fill:#ffecb3
```

---

## RL Agent Integration

```mermaid
graph TB
    subgraph "RL Agent Ecosystem"
        direction TB

        subgraph "Learning Core"
            RLAgent["rl_agent gen_server"]
            RNNModel["pred_rnn<br/>(RNN Model)"]
            Training["pred_training<br/>(Model Training)"]
        end

        subgraph "Strategy Selection"
            Thompson["strategy_thompson_sampling"]
            EpsilonGreedy["strategy_epsilon_greedy"]
            UCB["strategy_ucb"]
        end

        subgraph "Action Selection"
            ActionSpace["Action Space"]
            RewardCalc["Reward Calculation"]
            StateRep["State Representation"]
        end

        subgraph "Experience Management"
            ReplayBuffer["Replay Buffer"]
            Experience["Experience Storage"]
        end
    end

    subgraph "Workflow Integration"
        PNet["gen_pnet"]
        YAWL["YAWL Executor"]
        DecisionPoint["Decision Point"]
    end

    subgraph "Mining Pipeline"
        Mining["predictive_mining"]
        Discovery["process_discovery"]
        Analysis["Pattern Analysis"]
    end

    DecisionPoint -->|State Query| StateRep
    DecisionPoint -->|Request Action| RLAgent

    RLAgent -->|Select Strategy| Thompson
    RLAgent -->|Select Strategy| EpsilonGreedy
    RLAgent -->|Select Strategy| UCB

    Thompson -->|Beta Distribution| RLAgent
    EpsilonGreedy -->|Exploration| RLAgent
    UCB -->|Optimism| RLAgent

    RLAgent -->|Get Prediction| RNNModel
    RLAgent -->|Select| ActionSpace
    RLAgent -->|Return| DecisionPoint

    DecisionPoint -->|Execute| YAWL
    YAWL -->|Reward| RewardCalc
    RewardCalc -->|Update| RLAgent

    RLAgent -->|Store| Experience
    Experience -->|Training Data| Training
    Training -->|Update Model| RNNModel

    Mining -->|Discovered Patterns| RLAgent
    Discovery -->|Process Model| Mining
    Analysis -->|Insights| RLAgent

    RLAgent -->|Influence| PNet

    style RLAgent fill:#ce93d8
    style RNNModel fill:#9fa8da
    style Thompson fill:#80cbc4
```

```mermaid
sequenceDiagram
    participant Net as Petri Net
    participant RL as rl_agent
    participant Strategy as Strategy Module
    participant RNN as RNN Model
    participant Training as pred_training

    Note over Net,Training: RL Agent Learning Loop

    Net->>RL: select_transition(EnabledTransitions, State)
    activate RL

    RL->>RL: get_context(State)
    RL->>Strategy: select_action(Context)

    alt Thompson Sampling
        Strategy->>Strategy: sample_beta(Alpha, Beta)
        Strategy-->>RL: SelectedTransition
    else Epsilon-Greedy
        Strategy->>Strategy: explore_or_exploit()
        Strategy-->>RL: SelectedTransition
    else UCB
        Strategy->>Strategy: calculate_ucb(Counts, Rewards)
        Strategy-->>RL: SelectedTransition
    end

    RL->>RNN: predict_value(State, Action)
    RNN-->>RL: PredictedValue

    RL-->>Net: {ok, SelectedTransition}
    deactivate RL

    Net->>Net: fire(SelectedTransition)
    Net-->>RL: observe_reward(State, Action, Reward, NextState)
    activate RL

    RL->>RL: update_q_value(State, Action, Reward)
    RL->>Training: store_experience(Experience)
    Training->>Training: add_to_replay_buffer(Experience)

    alt Training Triggered
        Training->>RNN: train_batch(Batch)
        RNN-->>Training: UpdatedModel
        Training-->>RL: model_updated
    end

    RL-->>RL: update_strategy_parameters()
    deactivate RL
```

---

## Data Flow - Mining Pipeline

```mermaid
graph TB
    subgraph "Event Log Input"
        XES["XES Files"]
        CSV["CSV Logs"]
        DB["Database"]
        Stream["Event Stream"]
    end

    subgraph "Log Processing"
        Parse["Log Parser"]
        Filter["Log Filter"]
        TraceExtract["Trace Extraction"]
    end

    subgraph "Process Discovery"
        Alpha["Alpha Algorithm"]
        Heuristic["Heuristic Miner"]
        Inductive["Inductive Miner"]
        Fuzzy["Fuzzy Miner"]
    end

    subgraph "Pattern Analysis"
        LoopDetect["Loop Detection"]
        NFCDetect["Non-Free-Choice Detection"]
        ParallelDetect["Parallelism Detection"]
    end

    subgraph "Model Generation"
        PetriNet["Petri Net Model"]
        YawlNet["YAWL Model"]
        BPMN["BPMN Model"]
    end

    subgraph "Predictive Mining"
        MarkovModel["Markov Chain Model"]
        RNNTrain["RNN Training"]
        Ensemble["Ensemble Model"]
    end

    subgraph "Prediction Output"
        NextActivity["Next Activity Prediction"]
        RemainingTime["Remaining Time Prediction"]
        Outcome["Outcome Prediction"]
    end

    subgraph "Feedback Loop"
        Validate["Model Validation"]
        Optimize["Hyperparameter Optimization"]
    end

    XES --> Parse
    CSV --> Parse
    DB --> Parse
    Stream --> Parse

    Parse --> Filter
    Filter --> TraceExtract

    TraceExtract --> Alpha
    TraceExtract --> Heuristic
    TraceExtract --> Inductive
    TraceExtract --> Fuzzy

    Alpha --> LoopDetect
    Heuristic --> LoopDetect
    Inductive --> LoopDetect

    LoopDetect --> NFCDetect
    NFCDetect --> ParallelDetect

    ParallelDetect --> PetriNet
    ParallelDetect --> YawlNet
    ParallelDetect --> BPMN

    TraceExtract --> MarkovModel
    TraceExtract --> RNNTrain

    RNNTrain --> Ensemble
    MarkovModel --> Ensemble

    Ensemble --> NextActivity
    Ensemble --> RemainingTime
    Ensemble --> Outcome

    PetriNet --> Validate
    YawlNet --> Validate
    NextActivity --> Validate

    Validate --> Optimize
    Optimize --> RNNTrain
    Optimize --> Heuristic

    style Alpha fill:#e1f5fe
    style Heuristic fill:#fff3e0
    style RNNTrain fill:#f3e5f5
    style Ensemble fill:#e8f5e9
```

```mermaid
sequenceDiagram
    participant Log as Event Log
    participant Disc as process_discovery
    participant Pred as predictive_mining
    participant Train as pred_training
    participant RNN as RNN Model
    participant Workflow as YAWL Workflow

    Note over Log,Workflow: Mining Pipeline Flow

    Log->>Disc: discover(EventLog)
    activate Disc

    Disc->>Disc: events_to_traces(Log)
    Disc->>Disc: calculate_dependencies(Traces)
    Disc->>Disc: detect_loops(Traces)
    Disc->>Disc: classify_loops(Traces)

    Disc->>Disc: heuristic_miner(Log)
    Disc-->>Log: {ok, PetriNet}
    deactivate Disc

    Log->>Pred: predict_next_activity(CaseId, Trace)
    activate Pred

    Pred->>Pred: extract_activities(Trace)
    Pred->>RNN: predict(Trace, State)
    RNN-->>Pred: ActivityProbs

    Pred-->>Log: {ok, [{Activity, Prob}, ...]}
    deactivate Pred

    Log->>Train: train_model(TrainingData)
    activate Train

    Train->>RNN: fit(TrainingData, Options)
    Train->>Train: validate(Model, TestData)
    Train-->>RNN: UpdatedModel
    Train-->>Log: {ok, ModelMetrics}
    deactivate Train

    Log->>Workflow: apply_insights(DiscoveredPatterns)
    activate Workflow
    Workflow->>Workflow: optimize_execution(Patterns)
    Workflow-->>Log: {ok, OptimizedWorkflow}
    deactivate Workflow
```

---

## Module Directory Structure

```mermaid
graph TB
    subgraph "src/"
        subgraph "core/"
            PNetMod["gen_pnet.erl"]
            PNetServer["gen_pnet_server.erl"]
            PNetBuilder["gen_pnet_builder.erl"]
        end

        subgraph "pnet/"
            Marking["pnet_marking.erl"]
            Types["pnet_types.erl"]
            Mode["pnet_mode.erl"]
            Choice["pnet_choice.erl"]
            Receipt["pnet_receipt.erl"]
        end

        subgraph "yawl/"
            Compile["yawl_compile.erl"]
            Validate["yawl_validate.erl"]
            Parser["yawl_parser.erl"]
            Executor["yawl_executor.erl"]
        end

        subgraph "wf/"
            Engine["wf_engine.erl"]
            Timer["wf_persistent_timer.erl"]
            Task["wf_task.erl"]
            Scope["wf_scope.erl"]
            State["yawl_state.erl"]
        end

        subgraph "patterns/"
            RLAgent["rl_agent.erl"]
            StrategyTS["strategy_thompson_sampling.erl"]
            StrategyEG["strategy_epsilon_greedy.erl"]
            StrategyUCB["strategy_ucb.erl"]
            BasePattern["pattern_base.erl"]
        end

        subgraph "mining/"
            Discovery["process_discovery.erl"]
            Predictive["predictive_mining.erl"]
            Training["pred_training.erl"]
            RNN["pred_rnn.erl"]
        end
    end

    PNetMod -.-> PNetServer
    PNetMod -.-> PNetBuilder

    PNetServer --> Marking
    PNetServer --> Types
    PNetServer --> Mode
    PNetServer --> Choice
    PNetServer --> Receipt

    Compile --> Parser
    Compile --> Validate
    Compile --> Executor

    Executor --> Engine
    Executor --> Timer
    Executor --> Task
    Executor --> Scope
    Executor --> State

    RLAgent --> StrategyTS
    RLAgent --> StrategyEG
    RLAgent --> StrategyUCB
    RLAgent --> BasePattern

    Predictive --> Discovery
    Predictive --> Training
    Training --> RNN

    style core fill:#e3f2fd
    style pnet fill:#fff3e0
    style yawl fill:#f3e5f5
    style wf fill:#e8f5e9
    style patterns fill:#fce4ec
    style mining fill:#fff9c4
```

---

## Petri Net State Transitions

```mermaid
stateDiagram-v2
    [*] --> Initial: Net Created

    Initial --> Running: start_link/2

    state Running {
        [*] --> Idle
        Idle --> Enabled: Tokens Available
        Enabled --> Firing: Mode Selected
        Firing --> Idle: Fire Complete
        Enabled --> Conflict: Multiple Modes
        Conflict --> Firing: Decision Made
    }

    Running --> Suspended: suspend/1
    Suspended --> Running: resume/1

    Running --> Final: Final Marking
    Final --> [*]: terminate/2

    Running --> [*]: crash/abort
```

---

## Error Handling and Recovery

```mermaid
graph TB
    subgraph "Error Detection"
        Validation["Input Validation"]
        Soundness["Soundness Check"]
        Runtime["Runtime Errors"]
    end

    subgraph "Error Handling"
        Retry["Retry Mechanism"]
        Fallback["Fallback Strategy"]
        Recovery["State Recovery"]
    end

    subgraph "Logging & Monitoring"
        Receipt["Receipt Audit Trail"]
        Metrics["Performance Metrics"]
        Alerts["Alerting"]
    end

    Validation -->|Invalid Input| Retry
    Soundness -->|Unsound| Recovery
    Runtime -->|Exception| Fallback

    Retry -->|Exponential Backoff| Validation
    Fallback -->|Default Path| Recovery

    Recovery -->|Restore State| Soundness
    Recovery -->|Resume| Runtime

    Runtime -->|Log Event| Receipt
    Recovery -->|Update Metrics| Metrics
    Metrics -->|Threshold Exceeded| Alerts

    style Validation fill:#ffcdd2
    style Soundness fill:#fff9c4
    style Recovery fill:#c8e6c9
```

---

## Key Architecture Principles

1. **OTP Compliance**: All components follow OTP gen_server, gen_statem behaviors
2. **Pure Functional Core**: State management is pure functional (yawl_state, pnet_marking)
3. **Immutable Receipts**: All state transitions create immutable audit records
4. **Pluggable Strategies**: RL agents support multiple strategy selection algorithms
5. **Separation of Concerns**: Clear boundaries between core, workflow, patterns, and mining layers
6. **Extensibility**: 43 workflow patterns can be composed for complex workflows
7. **Soundness Verification**: YAWL models are validated for soundness before execution
