# CRE Workflow Engine - Mermaid Diagrams

This directory contains comprehensive Mermaid diagrams that visualize the CRE workflow engine architecture, workflows, and user interactions.

## Diagram Overview

### 1. State Diagram (`workflow-state-diagram.md`)
- **Purpose**: Visualizes workflow lifecycle states and transitions
- **Key Features**:
  - System states: Initialisation, Running, Idle, Suspended, Shutdown
  - Task execution states: Dispatching, Executing, Completing, Failed
  - Human approval states: Awaiting_Approval, Approved, Denied, Timeout
  - Exception handling states: Exception_Handling, Compensating
- **Traces**: CRE workflow execution patterns from `cre_master.erl`, `yawl_engine.erl`, `yawl_approval.erl`

### 2. User Journey Diagram (`user-journey-diagram.md`)
- **Purpose**: Shows interaction flows for different user personas
- **Key Personas**:
  - **Developer**: Creates, deploys, and monitors workflows
  - **Business User**: Submits tasks, tracks progress, makes approvals
  - **System Administrator**: Configures, monitors, and scales the system
  - **AI Assistant**: Analyzes patterns, predicts delays, suggests optimizations
- **Interactions**: Shows how different roles interact with the CRE system

### 3. Sequence Diagram (`sequence-diagram.md`)
- **Purpose**: Illustrates key system interactions and message flows
- **Key Components**:
  - API Client: External interaction point
  - CRE Master: Central coordinator
  - CRE Worker: Task execution
  - YAWL Engine: Workflow execution
  - Approval System: Human-in-the-loop approvals
  - Persistent Storage: Data persistence
- **Flows Covered**:
  - Task request and execution
  - Human approval workflow
  - Exception handling
  - Response propagation

### 4. Gantt Chart (`gantt-chart.md`)
- **Purpose**: Development timeline and project milestones
- **Phases**:
  - Phase 1: Core Architecture (Petri nets, master/worker, YAWL engine)
  - Phase 2: Workflow Patterns (basic and advanced patterns, approval system)
  - Phase 3: Integration (HTTP API, client libraries, persistence)
  - Phase 4: Enhanced Features (dashboard, simulation, optimization)
  - Phase 5: Production Ready (documentation, testing, deployment)
  - Phase 6: Future Enhancements (AI assistant, distributed scaling)
- **Timeline**: 2025-2026 development roadmap

### 5. Requirement Diagram (`requirement-diagram.md`)
- **Purpose**: Requirements traceability and system architecture mapping
- **Requirement Categories**:
  - CRE Workflow Engine: Core functionality
  - API Interfaces: Client APIs and protocols
  - Performance Requirements: Scalability and response times
  - Security Requirements: Authentication and compliance
  - Observability: Monitoring and metrics
- **Traceability**: Links requirements to implementation files

### 6. YAWL Patterns Diagram (`yawl-patterns-diagram.md`)
- **Purpose**: Visualizes the 43 YAWL workflow patterns and their relationships
- **Pattern Categories**:
  - Basic Flow Control: Sequence, Parallel Split, Synchronization, Exclusive Choice
  - Advanced Patterns: Iteration, Multi-Instance, Discriminator, Deferred Choice
  - Human Interaction: Human Task, Approval, Milestone, Multiple Merge
  - Advanced Control Flow: Interleaved Routing, Complex splits/joins
  - Exception Handling: Compensation, Retry, Timeout
- **Implementation**: Shows how patterns map to CRE's Petri net implementation

### 7. System Architecture Diagram (`system-architecture-diagram.md`)
- **Purpose**: High-level system architecture following Joe Armstrong design principles
- **Key Design Principles**:
  - Single OTP runner: `gen_pnet` maintains state
  - Pure utilities: All other components are stateless helpers
  - Message contracts: Clean inter-process communication
- **Layers**:
  - External Systems: API clients, web interfaces
  - Application Layer: HTTP server, IPC handlers
  - Core Engine: CRE master, YAWL engine, worker pool
  - Execution Layer: gen_pnet, workflow patterns, Petri net
  - Utility Layer: Pure Erlang utilities
  - Storage Layer: Cache, history, state, logs
  - Monitoring Layer: Telemetry, metrics, observers

## Usage

Each diagram is self-contained and can be used independently:

1. **View in Markdown**: Open any `.md` file in a Markdown viewer that supports Mermaid
2. **Integration**: Embed diagrams in documentation or presentations
3. **Interactive**: Use Mermaid Live Editor for interactive viewing

## File Structure

```
docs/mermaid-diagrams/
├── README.md                      # This overview file
├── workflow-state-diagram.md      # Workflow lifecycle states
├── user-journey-diagram.md       # User interaction flows
├── sequence-diagram.md           # System interaction sequences
├── gantt-chart.md                # Development timeline
├── requirement-diagram.md        # Requirements traceability
├── yawl-patterns-diagram.md      # YAWL workflow patterns
└── system-architecture-diagram.md # System architecture overview
```

## Key Design Principles

Based on the CRE architecture analysis:

1. **Joe Armstrong Design**: Single OTP runner (`gen_pnet`) with pure utilities
2. **YAWL Pattern Support**: 43 workflow patterns with Petri net execution
3. **Human-in-the-Loop**: Approval workflows with multiple approver types
4. **Exception Handling**: Robust error handling with worklets
5. **Performance**: Deterministic worker selection and efficient task distribution
6. **Observability**: OpenTelemetry integration for monitoring

## Implementation Status

- ✅ **State Diagram**: Complete with all workflow states
- ✅ **User Journey**: Complete with all persona interactions
- ✅ **Sequence Diagram**: Complete with message flows
- ✅ **Gantt Chart**: Complete with development roadmap
- ✅ **Requirement Diagram**: Complete with traceability mapping
- ✅ **YAWL Patterns Diagram**: Complete with pattern categories and implementation mapping
- ✅ **System Architecture Diagram**: Complete with Joe Armstrong design principles

All diagrams are production-ready and accurately represent the CRE workflow engine's processes based on the current implementation in the `/src/` directory.