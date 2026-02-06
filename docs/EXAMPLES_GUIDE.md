# Examples Guide

This guide documents the example files that demonstrate the usage of the new CRE architecture and its utility modules. Examples are designed to be practical, working implementations that users can study and adapt for their own use cases.

## Example Overview

The refactoring includes two main example files:

1. **`examples/yawl_pnet_demo.erl`** - Comprehensive workflow demonstration
2. **`examples/yawl_pnet_demo.sh`** - Execution script for the demo

Both examples showcase the new architecture principles and demonstrate how utility modules work together.

---

## 1. `yawl_pnet_demo.erl` - Comprehensive Workflow Demo

### Overview
This is a complete workflow implementation that demonstrates:
- Pattern composition
- Utility module integration
- Task lifecycle management
- Timer-based execution
- Receipt tracking for audit trails

### Key Features Demonstrated

#### Pattern Composition
```erlang
% Combine multiple patterns: parallel split -> choice -> merge
-module(yawl_pnet_demo).
-behaviour(pnet_net).

% Define places and transitions
places() -> [start, task_a, task_b, choice, merge, end].
transitions() -> [t_start, t_parallel, t_choice, t_merge, t_end].

% Transition presets
preset(t_parallel) -> [start];
preset(t_choice) -> [task_a, task_b];
preset(t_merge) -> [choice].
```

#### Utility Module Integration
```erlang
% Using pnet_marking for state management
-spec init_marking(place(), term()) -> [token()].
init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

% Using pnet_mode for mode enumeration
-spec modes(atom(), marking(), term()) -> [mode()].
modes(t_choice, Marking, _UsrInfo) ->
    %% Both tasks completed, can proceed
    case maps:get(task_a, Marking, []) =/= [] andalso
         maps:get(task_b, Marking, []) =/= [] of
        true -> [#{task_a => [], task_b => []}];
        false -> []
    end.

% Using wf_task for external work
fire(t_parallel, #{start := [init]}, _UsrInfo) ->
    %% Enable external tasks
    TaskA = wf_task:enabled(task_a_id, #{type => "process"}, task_a),
    TaskB = wf_task:enabled(task_b_id, #{type => "process"}, task_b),
    {produce, TaskA#{task_b := TaskB}}.
```

#### Time-Based Execution
```erlang
% Using wf_timerq for timeouts
fire(t_choice, Marking, _UsrInfo) ->
    case check_tasks_completed(task_a_id, task_b_id) of
        true ->
            {produce, #{choice => [completed], merge => [ready]}};
        false ->
            %% Schedule timeout (deadline is monotonic milliseconds)
            Deadline = erlang:monotonic_time(millisecond) + 30000,  % 30 seconds
            TimerQ = wf_timerq:new(),
            Event = {produce, #{timeout => [triggered]}},
            ArmedQ = wf_timerq:arm(TimerQ, timeout_id, Deadline, Event),
            {produce, #{choice => [pending], timer => [ArmedQ]}}
    end.
```

#### Receipt Tracking
```erlang
% Using pnet_receipt for audit trails
fire(t_merge, #{choice := [completed]}, _UsrInfo) ->
    BeforeHash = pnet_marking:hash(Marking),
    ConsumeMap = #{choice => []},
    ProduceMap = #{end => [done]},
    Move = #{trsn => t_merge, mode => ConsumeMap, produce => ProduceMap},
    {ok, NewMarking} = pnet_marking:apply(Marking, ConsumeMap, ProduceMap),
    AfterHash = pnet_marking:hash(NewMarking),
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    {produce, maps:merge(ProduceMap, #{receipt => [Receipt]})}.
```

#### Deterministic Choice
```erlang
% Using pnet_choice for nondeterministic selection
-spec execute_choice(marking(), term()) -> atom().
execute_choice(Marking, UsrInfo) ->
    AvailableOptions = [option_a, option_b, option_c],
    RngState = pnet_choice:seed(12345),  % Deterministic seed
    {Selected, _NewRng} = pnet_choice:pick(AvailableOptions, RngState),
    Selected.
```

### Complete Workflow Execution

#### Initialization
```erlang
% Start the workflow
Marking0 = #{start => [init]},
Modes0 = demo:modes(t_start, Marking0, _usr_info),
{SelectedMode, Rng1} = pnet_choice:pick(Modes0, initial_rng),
{produce, Marking1} = demo:fire(t_start, Marking0, SelectedMode, _usr_info).
```

#### Parallel Execution
```erlang
% Enable parallel tasks
{produce, Marking2} = demo:fire(t_parallel, Marking1, _usr_info),
% => #{task_a => [{task, task_a_id, enabled, ...}],
%     task_b => [{task, task_b_id, enabled, ...}]}
```

#### Task Monitoring
```erlang
% Monitor external task completion
check_task_completion() ->
    case gen_pnet:ls(task_a_id) of
        {ok, done} ->
            wf_task:done(task_a_id, #{result => "success"}, task_a);
        {ok, failed} ->
            wf_task:failed(task_a_id, #{error => "timeout"}, task_a)
    end.
```

#### Final Merge
```erlang
% Merge results
{produce, FinalMarking} = demo:fire(t_merge, MarkingN, _usr_info),
% => #{end => [done], receipt => [Receipt]}
```

### Key Design Patterns

#### 1. Pattern Composition
```erlang
% Chain multiple patterns together
workflow_demo() ->
    % 1. Parallel split
    SplitResult = parallel_split:fire(t_split, InitialMarking),

    % 2. Choice based on data
    ChoiceResult = choice_demo:execute(ChoiceTransition, SplitResult),

    % 3. Simple merge
    MergeResult = simple_merge:fire(t_merge, ChoiceResult).
```

#### 2. State Management
```erlang
% Consistent use of pnet_marking
update_state(Marking, Transition, Data) ->
    Current = pnet_marking:get(Marking, transition_place),
    NewTokens = append_tokens(Current, Data),
    pnet_marking:set(Marking, transition_place, NewTokens).
```

#### 3. Error Handling
```erlang
% Comprehensive error handling
handle_transition(Transition, Marking, UsrInfo) ->
    try
        Modes = demo:modes(Transition, Marking, UsrInfo),
        case Modes of
            [] -> {error, no_valid_modes};
            _ -> execute_transition(Transition, Marking, hd(Modes), UsrInfo)
        end
    catch
        Error:Reason -> {error, {Error, Reason}}
    end.
```

### Performance Considerations

#### 1. Mode Enumeration Optimization
```erlang
% Cache static modes for efficiency
-spec modes(atom(), marking(), term()) -> [mode()].
modes(Transition, Marking, _UsrInfo) ->
    case Transition of
        t_static ->
            %% Cache these modes as they never change
            get_cached_modes(Transition);
        t_dynamic ->
            %% Dynamic modes depend on current marking
            enumerate_dynamic_modes(Transition, Marking)
    end.
```

#### 2. State Hashing
```erlang
% Efficient state comparison for optimization
-spec is_same_state(marking(), marking()) -> boolean().
is_same_state(Marking1, Marking2) ->
    pnet_marking:hash(Marking1) =:= pnet_marking:hash(Marking2).
```

---

## 2. `yawl_pnet_demo.sh` - Execution Script

### Overview
This script provides a command-line interface to execute the demo workflow. It demonstrates how to interact with the new architecture from the command line and includes error handling and configuration.

### Script Features

#### 1. Configuration
```bash
#!/bin/bash

# Configuration
REBAR3_CMD="rebar3"
CRE_NODE="cre@localhost"
CRE_COOKIE="secret"
DEMO_MODULE="yawl_pnet_demo"
LOG_FILE="/tmp/cre_demo.log"
```

#### 2. Environment Setup
```bash
# Setup environment
setup_environment() {
    echo "Setting up environment..."

    # Compile project
    ${REBAR3_CMD} compile
    if [ $? -ne 0 ]; then
        echo "Error compiling project"
        exit 1
    fi

    # Ensure log directory exists
    mkdir -p $(dirname ${LOG_FILE})
}
```

#### 3. Node Management
```bash
# Start/stop Erlang node
start_node() {
    echo "Starting Erlang node..."
    ${REBAR3_CMD} shell --name ${CRE_NODE} --setcookie ${CRE_COOKIE} \
        --eval "code:add_pathz(\"ebin\")" \
        > ${LOG_FILE} 2>&1 &

    # Wait for node to start
    sleep 2
    check_node_status
}

stop_node() {
    echo "Stopping Erlang node..."
    ${REBAR3_CMD} shell --name ${CRE_NODE} --setcookie ${CRE_COOKIE} \
        --eval "init:stop()" \
        > ${LOG_FILE} 2>&1
}
```

#### 4. Demo Execution
```bash
# Execute demo workflow
execute_demo() {
    echo "Executing demo workflow..."

    # Run demo in Erlang shell
    ${REBAR3_CMD} shell --name ${CRE_NODE} --setcookie ${CRE_COOKIE} \
        --eval "
            code:add_pathz(\"ebin\"),
            c(${DEMO_MODULE}),
            Result = ${DEMO_MODULE}:run_demo(),
            io:format(\"Demo result: ~p~n\", [Result]),
            init:stop()
        " >> ${LOG_FILE} 2>&1

    # Check result
    if [ $? -eq 0 ]; then
        echo "Demo executed successfully"
        show_tail_log
    else
        echo "Demo execution failed"
        show_error_log
    fi
}
```

#### 5. Logging and Monitoring
```bash
# Show log output
show_tail_log() {
    echo "=== Last 20 lines of log ==="
    tail -20 ${LOG_FILE}
}

# Show error details
show_error_log() {
    echo "=== Error details ==="
    echo "Last 50 lines of log:"
    tail -50 ${LOG_FILE}
    echo "=== End of error details ==="
}
```

#### 6. Main Execution Flow
```bash
# Main script execution
main() {
    case "$1" in
        start)
            setup_environment
            start_node
            ;;
        stop)
            stop_node
            ;;
        run)
            setup_environment
            start_node
            execute_demo
            stop_node
            ;;
        *)
            echo "Usage: $0 {start|stop|run}"
            exit 1
            ;;
    esac
}

# Execute main function
main "$@"
```

### Usage Examples

#### 1. Running the Demo
```bash
# Execute complete demo (compile, run, cleanup)
./yawl_pnet_demo.sh run

# Or step by step
./yawl_pnet_demo.sh start    # Setup and start node
./yawl_pnet_demo.sh run      # Execute demo (node already started)
./yawl_pnet_demo.sh stop     # Stop and cleanup
```

#### 2. Custom Configuration
```bash
# Override environment variables
CRE_NODE="cre@myhost" \
CRE_COOKIE="mycookie" \
LOG_FILE="/custom/path/demo.log" \
./yawl_pnet_demo.sh run
```

#### 3. Debug Mode
```bash
# Run with verbose output
set -x
./yawl_pnet_demo.sh run
set +x
```

### Error Handling

#### 1. Node Startup Issues
```bash
check_node_status() {
    if ! pgrep -f "erlang.*${CRE_NODE}" > /dev/null; then
        echo "Failed to start Erlang node"
        echo "Check log: ${LOG_FILE}"
        exit 1
    fi
}
```

#### 2. Module Compilation Errors
```bash
handle_compile_error() {
    echo "Compilation failed. Checking details..."
    if grep -q "syntax error" ${LOG_FILE}; then
        echo "Syntax error in source code"
    elif grep -q "undefined function" ${LOG_FILE}; then
        echo "Missing function exports"
    else
        echo "Unknown compilation error"
    fi
    exit 1
}
```

---

## Integration Patterns

### 1. With Web Services
```erlang
% Integrate with external services
fire(t_external, Marking, _UsrInfo) ->
    Task = wf_task:enabled(ext_task_id, #{url => "http://service.com"}, external_place),
    {produce, Task}.

% Handle external completion
handle_external_result(Result) ->
    case Result of
        success -> wf_task:done(ext_task_id, Result, done_place);
        error -> wf_task:failed(ext_task_id, Result, error_place)
    end.
```

### 2. With Databases
```erlang
% Database integration
fire(t_database, Marking, _UsrInfo) ->
    Query = build_database_query(Marking),
    {ok, Results} = database:execute(Query),
    Tokens = [result(R) || R <- Results],
    {produce, #{database => Tokens}}.
```

### 3. With Message Queues
```erlang
% Message queue integration
fire(t_queue, Marking, _UsrInfo) ->
    Messages = queue:get_messages(queue_name),
    Tokens = [format_message(M) || M <- Messages],
    {produce, #{queue => Tokens}}.
```

---

## Best Practices from Examples

### 1. Module Organization
- Keep modules under 500 lines
- Clear separation of concerns
- Consistent naming conventions

### 2. Error Handling
- Total functions for all operations
- Comprehensive error cases
- Graceful degradation

### 3. Performance
- Cache static data
- Use efficient data structures
- Minimize state copying

### 4. Testing
- Unit tests for each function
- Integration tests for workflows
- Property-based testing where applicable

### 5. Documentation
- Comprehensive docstrings
- Usage examples
- Clear error messages

---

## Extending the Examples

### 1. Adding New Patterns
```erlang
% Extend the demo with new patterns
add_pattern(Workflow) ->
    % Add exclusive choice
    ExclusiveChoice = exclusive_choice:new(),
    % Add to workflow composition
    compose_patterns(Workflow, ExclusiveChoice).
```

### 2. Custom Utility Usage
```erlang
% Add custom utility functions
custom_util_demo() ->
    % Use pnet_marking for complex state
    State = complex_state_initialization(),
    % Apply custom operations
    UpdatedState = custom_operations(State),
    % Track with receipts
    create_audit_trail(State, UpdatedState).
```

### 3. Integration with External Systems
```erlang
% Extend for specific use cases
integration_demo() ->
    % Add business logic
    BusinessLogic = implement_business_rules(),
    % Add data transformation
    DataTransform = data_processing_pipeline(),
    % Combine with workflow
    integrate_components(BusinessLogic, DataTransform).
```

---

These examples provide a solid foundation for understanding and extending the new CRE architecture. The demo workflow showcases practical implementation of patterns and utility modules, while the execution script demonstrates how to deploy and run workflows in production environments.