# Execution Flow

## Omega Demo Loop

```mermaid
flowchart TD
    Start([Start]) --> Step[gen_yawl:step]
    Step -->|ok| Pattern[Track pattern execution]
    Pattern --> Step
    Step -->|abort| Find[find_inject_place]
    Find -->|undefined| Subnets[run_subnets_if_needed]
    Find -->|Place, Task| Agent[Agent decision]
    Agent --> Inject[gen_yawl:inject]
    Inject --> Step
    Subnets -->|ok| Drain[step_until_abort]
    Drain --> Step
    Subnets -->|none| Check[check_completed]
    Check -->|true| Done([Done])
    Check -->|false| Blocked([Blocked])
```

## Gen YAWL Step

```mermaid
flowchart TD
    Step[gen_yawl:step] --> Enabled[find_enabled_transitions]
    Enabled -->|[]| Abort[abort]
    Enabled -->|[_|_]| Pick[pick transition]
    Pick --> Fire[fire transition]
    Fire --> Consume[consume preset]
    Consume --> Produce[produce postset]
    Produce --> Ok[{ok, Receipt}]
```

## Subnet Execution

```mermaid
flowchart TD
    Run[run_ready_subnets] --> Withdraw[gen_yawl:withdraw]
    Withdraw --> Start[gen_yawl:start_link subnet]
    Start --> Inject[gen_yawl:inject entry]
    Inject --> Drain[gen_yawl:drain 500ms]
    Drain --> Marking[gen_yawl:marking]
    Marking --> Exit{Exit has tokens?}
    Exit -->|yes| InjectRoot[gen_yawl:inject root p_gonogo_branchN]
    Exit -->|no| Stop[gen_yawl:stop]
    InjectRoot --> Stop
```
