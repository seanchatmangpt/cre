```mermaid
graph TD
    %% YAWL Pattern Categories
    subgraph "Basic Flow Control"
        A1[Sequence] --> A2[Parallel Split]
        A2 --> A3[Synchronization]
        A3 --> A4[Exclusive Choice]
        A4 --> A5[Merge]
        A5 --> A6[Simple Merge]
    end

    subgraph "Advanced Patterns"
        B1[Iteration] --> B2[Multi-Instance]
        B2 --> B3[Discriminator]
        B3 --> B4[Deferred Choice]
        B4 --> B5[Or Join]
        B5 --> B6[N-out-of-M]
    end

    subgraph "Human Interaction"
        C1[Human Task] --> C2[Approval]
        C2 --> C3[Milestone]
        C3 --> C4[Multiple Merge]
    end

    subgraph "Advanced Control Flow"
        D1[Parallel Split] --> D2[Interleaved Routing]
        D2 --> D3[Exclusive Choice]
        D3 --> D4[Synchronization]
        D4 --> D5[Merge]
    end

    subgraph "Exception Handling"
        E1[Exception] --> E2[Compensation]
        E2 --> E3[Retry]
        E3 --> E4[Timeout]
    end

    %% Pattern Implementation Details
    subgraph "CRE Implementation"
        F1[gen_pnet behavior] --> F2[pnet_marking]
        F2 --> F3[wf_task constructors]
        F3 --> F4[Petri net transitions]
        F4 --> F5[Token management]
    end

    %% Pattern Relationships
    A1 -.-> F1
    B1 -.-> F1
    C1 -.-> F1
    D1 -.-> F1
    E1 -.-> F1

    %% Pattern Properties
    subgraph "Pattern Properties"
        G1[Deterministic] --> F1
        G2[Transactional] --> F1
        G3[Stateful] --> F1
        G4[Observable] --> F1
        G5[Approvable] --> F1
    end

    %% Example Pattern: Exclusive Choice
    subgraph "Exclusive Choice Example"
        H1[Input] --> H2{Condition}
        H2 -->|Option A| H3[Task A]
        H2 -->|Option B| H4[Task B]
        H3 --> H5[Output]
        H4 --> H5
    end

    %% Legend
    subgraph "Legend"
        L1[Lines] --> Implementation Relationships
        L2[Dashed Lines] --> Dependencies
        L3[Rectangles] --> Pattern Categories
        L4[Shapes] --> Pattern Types
    end

    %% Title and Description
    title YAWL Workflow Patterns in CRE
    style A1 fill:#f9f,stroke:#333,stroke-width:2px
    style B1 fill:#f9f,stroke:#333,stroke-width:2px
    style C1 fill:#f9f,stroke:#333,stroke-width:2px
    style D1 fill:#f9f,stroke:#333,stroke-width:2px
    style E1 fill:#f9f,stroke:#333,stroke-width:2px
    style F1 fill:#9cf,stroke:#333,stroke-width:2px
    style G1 fill:#9cf,stroke:#333,stroke-width:2px
    style H1 fill:#9cf,stroke:#333,stroke-width:2px
    style L1 fill:#fff,stroke:#333,stroke-width:1px
```