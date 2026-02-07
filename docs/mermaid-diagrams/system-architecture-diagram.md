```mermaid
graph TB
    %% External Systems
    subgraph "External Systems"
        API[REST API Clients]
        WS[WebSocket Clients]
        CLI[Command Line Interface]
        UI[Web Dashboard]
    end

    %% Application Layer
    subgraph "Application Layer"
        HTTP[HTTP Server]
        IPC[IPC Handler]
        WSIF[WebSocket Interface]
        WS[Worker Service]
    end

    %% Core Engine Layer
    subgraph "Core Engine Layer"
        MASTER[CRE Master<br/>gen_server]
        ENGINE[YAWL Engine<br/>gen_server]
        WORKER[CRE Worker Pool]
        APPROVAL[Approval System<br/>gen_server]
    end

    %% Execution Layer
    subgraph "Execution Layer"
        GEN_PNET[gen_pnet<br/>OTP Process]
        WF_ENGINE[Workflow Engine]
        PATTERN[YAWL Patterns]
        PETRI[Petri Net Execution]
    end

    %% Utility Layer
    subgraph "Utility Layer"
        PNET_TYPES[pnet_types.erl]
        PNET_MARKING[pnet_marking.erl]
        PNET_MODE[pnet_mode.erl]
        PNET_CHOICE[pnet_choice.erl]
        WF_TASK[wf_task.erl]
        PNET_RECEIPT[pnet_receipt.erl]
    end

    %% Storage Layer
    subgraph "Storage Layer"
        CACHE[Result Cache]
        HISTORY[Execution History]
        STATE[Workflow State]
        LOGS[Application Logs]
    end

    %% Monitoring Layer
    subgraph "Monitoring Layer"
        TELEMETRY[OpenTelemetry]
        METRICS[Performance Metrics]
        OBSERVER[Process Observer]
        MONITOR[System Monitor]
    end

    %% External Connections
    API --> HTTP
    WS --> WSIF
    CLI --> IPC
    UI --> HTTP

    %% Application to Core
    HTTP --> MASTER
    IPC --> MASTER
    WSIF --> MASTER
    WS --> WORKER

    %% Core to Execution
    MASTER --> ENGINE
    ENGINE --> GEN_PNET
    WORKER --> WF_ENGINE
    APPROVAL --> ENGINE

    %% Execution to Utilities
    GEN_PNET --> PNET_TYPES
    GEN_PNET --> PNET_MARKING
    GEN_PNET --> PNET_MODE
    WF_ENGINE --> PNET_CHOICE
    WF_ENGINE --> WF_TASK
    WF_ENGINE --> PNET_RECEIPT

    %% Utilities to Storage
    PNET_MARKING --> STATE
    WF_ENGINE --> HISTORY
    MASTER --> CACHE
    MONITOR --> LOGS

    %% Monitoring Connections
    TELEMETRY --> ENGINE
    TELEMETRY --> MASTER
    TELEMETRY --> WORKER
    METRICS --> MONITOR
    OBSERVER --> MASTER
    OBSERVER --> ENGINE
    OBSERVER --> WORKER

    %% Data Flow Arrows
    MASTER -- Requests --> ENGINE
    ENGINE -- Tasks --> WORKER
    WORKER -- Results --> MASTER
    ENGINE -- Events --> APPROVAL
    APPROVAL -- Decisions --> ENGINE
    ENGINE -- State --> STATE
    MASTER -- History --> HISTORY
    TELEMETRY -- Metrics --> OBSERVER

    %% Title and Legend
    title CRE System Architecture - Joe Armstrong Design
    style MASTER fill:#ff9,stroke:#333,stroke-width:3px
    style ENGINE fill:#ff9,stroke:#333,stroke-width:3px
    style GEN_PNET fill:#9cf,stroke:#333,stroke-width:3px
    style MASTER font-size:16px
    style ENGINE font-size:16px
    style GEN_PNET font-size:16px

    %% Architecture Notes
    note 1,1:::fa  Joe Armstrong Design: Only one OTP process (gen_pnet) maintains state
    note 2,2:::fa  All other components are pure utilities with message contracts
    note 3,3:::fa  Deterministic worker selection using pnet_choice
    note 4,4:::fa  Human-in-the-loop workflows with approval system
    note 5,5:::fa  OpenTelemetry integration for observability
```