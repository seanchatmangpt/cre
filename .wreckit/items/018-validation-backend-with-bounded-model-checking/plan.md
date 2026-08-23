# Validation Backend with Bounded Model Checking Implementation Plan

## Implementation Plan Title
Bounded Model Checking Validation Backend for YAWL Workflows

## Overview
Implement a formal validation backend that compiles YAWL workflows to Petri nets and performs bounded model checking to detect deadlocks, dead transitions, unreachable states, and completion problems before runtime. This provides formal verification of workflow correctness beyond unit tests.

## Current State
CRE has a sophisticated Petri net foundation through the `gen_pnet` library and comprehensive Petri net type system in `/Users/sac/cre/src/pnet/` (pnet_types, pnet_marking, pnet_mode). The project has 36 of 43 YAWL patterns implemented and existing validation infrastructure (yawl_validate, yawl_schema). However, there is **no formal validation backend** that performs bounded model checking to detect workflow defects.

### Key Discoveries:
- **Petri net infrastructure exists**: `/Users/sac/cre/src/pnet/pnet_types.erl:164` defines `marking() :: #{place() => [token()]}` for state representation
- **Marking algebra available**: `/Users/sac/cre/src/pnet/pnet_marking.erl:278-291` provides `apply/2` for atomic transition firing with mode enumeration
- **Mode enumeration ready**: `/Users/sac/cre/src/pnet/pnet_mode.erl:169-177` implements `enum_modes/2` to find all enabled transitions
- **Validation pattern established**: `/Users/sac/cre/src/core/yawl_validate.erl:158-186` shows validation API structure returning `{ok, Warnings}` or `{error, Errors}`
- **Pattern implementations available**: 36+ patterns in `/Users/sac/cre/src/patterns/` follow gen_yawl behaviour with place_lst/0, trsn_lst/0, preset/1, fire/3 callbacks
- **Compilation target exists**: YAWL specifications can be parsed from XML via `/Users/sac/cre/src/yawl/yawl_schema.erl:161-182`

## Desired End State
A validation backend module `yawl_model_checker` that:
1. Accepts YAWL specifications (parsed XML or internal format)
2. Compiles workflows to Petri nets using existing pnet_types infrastructure
3. Performs bounded state space exploration (depth D, token bound K)
4. Detects and reports deadlocks, dead transitions, and completion problems
5. Returns structured validation errors compatible with existing yawl_validate format
6. Completes validation in reasonable time (seconds to minutes for typical workflows)

### Success Criteria:
- Detect at least one known deadlock case (e.g., incorrect critical section pattern)
- Bounded exploration completes in reasonable time (< 30 seconds for typical workflows)
- Reports specific validation issues with location information
- Not used in runtime path (validation-only backend)
- Integration point with existing yawl_validate:validate/1

## What We're NOT Doing
- Runtime execution or monitoring (this is validation-only)
- Unbounded model checking (all exploration is bounded by depth D and token bound K)
- Full support for all 43 YAWL patterns initially (starting with basic control flow patterns)
- Colored Petri net validation (initially focusing on basic Petri nets)
- Real-time or streaming validation
- Visualization of state spaces or execution traces
- Integration with external model checkers (SPIN, NuSMV, etc.)
- Automatic correction or repair of detected issues

## Implementation Approach

The implementation follows a phased approach, starting with foundational compilation and exploration, then adding property checking. Each phase is independently testable and builds on the previous phase. The approach leverages existing Petri net infrastructure and validation patterns to minimize risk and ensure compatibility.

### Design Decisions:
1. **Petri nets over LTS**: Using existing pnet_* infrastructure is more natural than building labeled transition systems
2. **Bounded exploration**: Depth D=10-20, token bound K=5-10 provides practical balance between coverage and performance
3. **Separate module namespace**: `yawl_model_checker` avoids conflicts with runtime code
4. **Opt-in validation**: Initially a separate check, potentially integrated later as optional validation step
5. **Focus on basic patterns**: Start with sequence, parallel split, synchronization, exclusive choice (4 most common patterns)

---

## Phases

### Phase 1: Foundation - Core Petri Net Compilation

#### Overview
Create the workflow to Petri net compiler that translates YAWL specifications into Petri net representations using existing pnet_types infrastructure. Establish the module structure and basic API.

#### Changes Required:

##### 1. Create yawl_model_checker module
**File**: `/Users/sac/cre/src/validate/yawl_model_checker.erl`
**Changes**: New module providing main validation API

```erlang
-module(yawl_model_checker).
-moduledoc """
Bounded model checking validation backend for YAWL workflows.

Compiles YAWL workflows to Petri nets and performs bounded state space
exploration to detect deadlocks, dead transitions, and completion problems.
""".

%% Main validation API
-export([validate/1, validate/2]).

%% Individual property checks
-export([check_deadlock/1, check_dead_transitions/1, check_completion/1]).

%% Types
-type validation_result() :: {ok, [yawl_validate:validation_error()]} |
                             {error, term()}.

-type bounds() :: #{
    depth => pos_integer(),      % Maximum exploration depth (default 15)
    token_bound => pos_integer() % Maximum tokens per place (default 10)
}.

-spec validate(Spec :: yawl_schema:specification()) -> validation_result().
validate(Spec) ->
    validate(Spec, #{depth => 15, token_bound => 10}).

-spec validate(Spec :: yawl_schema:specification(), Options :: bounds()) -> validation_result().
validate(Spec, Options) when is_map(Spec), is_map(Options) ->
    try
        %% Compile to Petri net
        {ok, InitialMarking, Transitions} = yawl_pnet_compiler:compile(Spec),

        %% Explore bounded state space
        {ok, Traces} = yawl_explorer:explore(InitialMarking, Transitions, Options),

        %% Check properties
        Deadlocks = check_deadlock(Traces),
        DeadTransitions = check_dead_transitions(Traces, Transitions),
        Completion = check_completion(Traces),

        %% Combine results
        AllIssues = Deadlocks ++ DeadTransitions ++ Completion,

        {Errors, Warnings} = lists:partition(
            fun(#{severity := Sev}) -> Sev =:= error end,
            AllIssues
        ),

        case Errors of
            [] -> {ok, Warnings};
            _ -> {error, Errors ++ Warnings}
        end
    catch
        _:Error -> {error, Error}
    end.
```

##### 2. Create yawl_pnet_compiler module
**File**: `/Users/sac/cre/src/validate/yawl_pnet_compiler.erl`
**Changes**: New module compiling YAWL workflows to Petri nets

```erlang
-module(yawl_pnet_compiler).
-moduledoc """
Compiles YAWL workflow specifications to Petri net representations.

Uses existing pnet_types infrastructure for marking, mode, and transition
representations. Supports basic control flow patterns: sequence, parallel
split, synchronization, exclusive choice.
""".

-export([compile/1]).

%% Compilation result
-type compiled_pnet() :: {
    pnet_marking:marking(),  % Initial marking
    [transition()]            % List of transitions
}.

-type transition() :: #{
    id => pnet_types:trsn(),
    preset => [pnet_types:place()],
    postset => [pnet_types:place()]
}.

-spec compile(Spec :: yawl_schema:specification()) ->
    {ok, pnet_marking:marking(), [transition()]} | {error, term()}.
compile(Spec) ->
    Tasks = yawl_schema:get_tasks(Spec),
    Conditions = yawl_schema:get_conditions(Spec),
    Flows = yawl_schema:get_flows(Spec),

    %% Build place set
    Places = build_places(Tasks, Conditions),

    %% Build initial marking
    InitialMarking = build_initial_marking(Places, Conditions),

    %% Build transitions
    Transitions = build_transitions(Tasks, Flows),

    {ok, InitialMarking, Transitions}.

%% Internal functions
build_places(Tasks, Conditions) ->
    TaskPlaces = [binary_to_existing_atom(Id, utf8) || Id <- maps:keys(Tasks)],
    ConditionPlaces = [binary_to_existing_atom(Id, utf8) || Id <- maps:keys(Conditions)],
    TaskPlaces ++ ConditionPlaces.

build_initial_marking(Places, Conditions) ->
    InputConditions = [Id || #{id := Id, type := input_condition} <- maps:values(Conditions)],
    InitialTokens = [begin
        PlaceId = binary_to_existing_atom(Id, utf8),
        #{PlaceId => [start]}
    end || Id <- InputConditions],
    lists:foldl(fun maps:merge/2, #{}, InitialTokens).

build_transitions(Tasks, Flows) ->
    %% Create a transition for each task
    maps:fold(fun(TaskId, _Task, Acc) ->
        Transition = #{
            id => binary_to_existing_atom(TaskId, utf8),
            preset => get_preset(TaskId, Flows),
            postset => get_postset(TaskId, Flows)
        },
        [Transition | Acc]
    end, [], Tasks).

get_preset(TaskId, Flows) ->
    [binary_to_existing_atom(F, utf8) || #{source := F, target := T} <- Flows, T =:= TaskId].

get_postset(TaskId, Flows) ->
    [binary_to_existing_atom(F, utf8) || #{source := S, target := F} <- Flows, S =:= TaskId].
```

##### 3. Create yawl_explorer module
**File**: `/Users/sac/cre/src/validate/yawl_explorer.erl`
**Changes**: New module implementing bounded state space exploration

```erlang
-module(yawl_explorer).
-moduledoc """
Bounded state space exploration for Petri net validation.

Performs depth-limited DFS with token bounds to explore reachable states.
Tracks visited states using pnet_marking:hash/1 for cycle detection.
""".

-export([explore/3]).

-type trace() :: [pnet_marking:marking()].
-type bounds() :: #{depth := pos_integer(), token_bound := pos_integer()}.

-spec explore(InitialMarking :: pnet_marking:marking(),
             Transitions :: [yawl_pnet_compiler:transition()],
             Bounds :: bounds()) ->
    {ok, [trace()]} | {error, term()}.
explore(InitialMarking, Transitions, Bounds) ->
    Visited = sets:new(),
    MaxDepth = maps:get(depth, Bounds, 15),
    TokenBound = maps:get(token_bound, Bounds, 10),

    Traces = dfs([InitialMarking], InitialMarking, Transitions, Visited, 0, MaxDepth, TokenBound),
    {ok, Traces}.

%% Depth-limited DFS with token bound checking
dfs(Path, CurrentMarking, Transitions, Visited, Depth, MaxDepth, TokenBound) ->
    %% Check token bound
    case check_token_bound(CurrentMarking, TokenBound) of
        exceed -> [lists:reverse(Path)];
        ok ->
            %% Get enabled transitions
            Enabled = get_enabled_transitions(CurrentMarking, Transitions),

            case Enabled of
                [] ->
                    %% Deadlock or final state
                    [lists:reverse(Path)];
                _ when Depth >= MaxDepth ->
                    %% Depth limit reached
                    [lists:reverse(Path)];
                _ ->
                    %% Explore successors
                    StateHash = pnet_marking:hash(CurrentMarking),
                    case sets:is_element(StateHash, Visited) of
                        true ->
                            %% Already visited
                            [lists:reverse(Path)];
                        false ->
                            Visited1 = sets:add_element(StateHash, Visited),
                            lists:flatmap(fun(Transition) ->
                                fire_and_explore(Transition, CurrentMarking, Transitions,
                                                 Path, Visited1, Depth, MaxDepth, TokenBound)
                            end, Enabled)
                    end
            end
    end.

fire_and_explore(Transition, CurrentMarking, Transitions, Path, Visited, Depth, MaxDepth, TokenBound) ->
    case fire_transition(Transition, CurrentMarking) of
        {ok, NextMarking} ->
            dfs([NextMarking | Path], NextMarking, Transitions, Visited, Depth + 1, MaxDepth, TokenBound);
        {error, _} ->
            []
    end.

get_enabled_transitions(Marking, Transitions) ->
    lists:filter(fun(Transition) ->
        is_enabled(Marking, Transition)
    end, Transitions).

is_enabled(Marking, #{preset := Preset}) ->
    lists:all(fun(Place) ->
        case pnet_marking:get(Marking, Place) of
            {ok, Tokens} when length(Tokens) > 0 -> true;
            _ -> false
        end
    end, Preset).

fire_transition(#{id := Id, preset := Preset, postset := Postset}, Marking) ->
    %% Consume from preset
    ConsumeMap = maps:from_list([{P, [token]} || P <- Preset]),
    case pnet_marking:take(Marking, ConsumeMap) of
        {ok, Marking1} ->
            %% Produce to postset
            ProduceMap = maps:from_list([{P, [token]} || P <- Postset]),
            {ok, pnet_marking:add(Marking1, ProduceMap)};
        {error, Reason} ->
            {error, Reason}
    end.

check_token_bound(Marking, Bound) ->
    Exceeds = lists:any(fun(Place) ->
        {ok, Tokens} = pnet_marking:get(Marking, Place),
        length(Tokens) > Bound
    end, maps:keys(Marking)),
    case Exceeds of
        true -> exceed;
        false -> ok
    end.
```

##### 4. Update rebar.config
**File**: `/Users/sac/cre/rebar.config:4`
**Changes**: Add src/validate to source directories

```
{src_dirs, ["src",
            "src/core",
            "src/pnet",
            "src/wf",
            "src/yawl",
            "src/patterns",
            "src/wfnet",
            "src/wfnet/patterns",
            "src/api",
            "src/integration",
            "src/http",
            "src/app",
            "src/nato",
            "src/mining",
            "src/prediction",
            "src/telemetry",
            "src/bench",
            "src/rust_nifs",
            "src/rust_implementations/paper_algorithms",
            "src/validate",
            "src/db"]}.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles: `rebar3 compile`
- [ ] Type checking passes: No dialyzer warnings for new modules
- [ ] Basic API tests pass: Can create module instances and call exported functions
- [ ] Integration with pnet_types: Marking and transition types work correctly

##### Manual Verification:
- [ ] Can compile simple YAWL specification to Petri net
- [ ] Initial marking correctly identifies input conditions
- [ ] Transitions have correct preset/postset from flows
- [ ] Explorer performs bounded DFS without infinite loops

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Property Checking - Deadlock and Dead Transition Detection

#### Overview
Implement property checking functions that analyze exploration traces to detect deadlocks, dead transitions, and completion problems. Report issues in the existing validation error format.

#### Changes Required:

##### 1. Implement deadlock detection in yawl_model_checker
**File**: `/Users/sac/cre/src/validate/yawl_model_checker.erl`
**Changes**: Add check_deadlock/1 function

```erlang
-spec check_deadlock(Traces :: [yawl_explorer:trace()]) -> [yawl_validate:validation_error()].
check_deadlock(Traces) ->
    lists:filtermap(fun(Trace) ->
        case is_deadlock(Trace) of
            {true, Marking} ->
                Error = #{
                    type => semantic,
                    severity => error,
                    message => <<"Deadlock detected: no enabled transitions but not in final state">>,
                    location => format_marking_location(Marking),
                    code => deadlock_detected
                },
                {true, Error};
            false ->
                false
        end
    end, Traces).

is_deadlock(Trace) ->
    LastMarking = lists:last(Trace),
    %% Check if this is a final state (all tokens at output conditions)
    IsFinal = is_final_marking(LastMarking),
    case IsFinal of
        true -> false;
        false ->
            %% Not final, but no enabled transitions = deadlock
            {true, LastMarking}
    end.

is_final_marking(Marking) ->
    %% Final if all tokens are at output conditions
    maps:fold(fun(Place, Tokens, Acc) ->
        case atom_to_list(Place) of
            "output_" ++ _ when length(Tokens) > 0 -> Acc andalso true;
            _ when length(Tokens) > 0 -> false;
            _ -> Acc
        end
    end, true, Marking).
```

##### 2. Implement dead transition detection
**File**: `/Users/sac/cre/src/validate/yawl_model_checker.erl`
**Changes**: Add check_dead_transitions/2 function

```erlang
-spec check_dead_transitions(Traces :: [yawl_explorer:trace()],
                             AllTransitions :: [yawl_pnet_compiler:transition()]) ->
    [yawl_validate:validation_error()]).

check_dead_transitions(Traces, AllTransitions) ->
    %% Collect all fired transitions from traces
    FiredTransitions = lists:usort(lists:flatmap(fun(Trace) ->
        extract_fired_transitions(Trace)
    end, Traces)),

    %% Find transitions that never fired
    AllTransitionIds = [Id || #{id := Id} <- AllTransitions],
    DeadTransitions = lists:filter(fun(Id) ->
        not lists:member(Id, FiredTransitions)
    end, AllTransitionIds),

    %% Generate errors for dead transitions
    [begin
        #{
            type => semantic,
            severity => warning,
            message => iolist_to_binary([<<"Unreachable transition: '">>,
                                        atom_to_binary(Id), <<"'">>]),
            location => atom_to_binary(Id),
            code => dead_transition
        }
    end || Id <- DeadTransitions].

extract_fired_transitions(Trace) ->
    %% Analyze trace to extract transition IDs
    %% This requires tracking which transition fired between markings
    %% For now, return empty list (will be enhanced in Phase 3)
    [].
```

##### 3. Implement completion check
**File**: `/Users/sac/cre/src/validate/yawl_model_checker.erl`
**Changes**: Add check_completion/1 function

```erlang
-spec check_completion(Traces :: [yawl_explorer:trace()]) -> [yawl_validate:validation_error()].
check_completion(Traces) ->
    %% Check if any trace reaches a final state
    HasCompletion = lists:any(fun(Trace) ->
        LastMarking = lists:last(Trace),
        is_final_marking(LastMarking)
    end, Traces),

    case HasCompletion of
        true -> [];
        false ->
            [#{
                type => semantic,
                severity => error,
                message => <<"Workflow cannot reach completion state">>,
                location => undefined,
                code => no_completion_path
            }]
    end.
```

##### 4. Enhance yawl_explorer to track transitions
**File**: `/Users/sac/cre/src/validate/yawl_explorer.erl`
**Changes**: Modify trace type to include transition information

```erlang
-type trace_step() :: {pnet_types:trsn(), pnet_marking:marking()}.
-type trace() :: [trace_step()].

-spec explore(InitialMarking :: pnet_marking:marking(),
             Transitions :: [yawl_pnet_compiler:transition()],
             Bounds :: bounds()) ->
    {ok, [trace()]} | {error, term()}.
explore(InitialMarking, Transitions, Bounds) ->
    Visited = sets:new(),
    MaxDepth = maps:get(depth, Bounds, 15),
    TokenBound = maps:get(token_bound, Bounds, 10),

    %% Start with no transition (initial state)
    Traces = dfs([], InitialMarking, Transitions, Visited, 0, MaxDepth, TokenBound),
    {ok, Traces}.

dfs(Path, CurrentMarking, Transitions, Visited, Depth, MaxDepth, TokenBound) ->
    %% ... existing checks ...

    lists:flatmap(fun(Transition) ->
        fire_and_explore(Transition, CurrentMarking, Transitions,
                         Path, Visited, Depth, MaxDepth, TokenBound)
    end, Enabled).

fire_and_explore(Transition, CurrentMarking, Transitions, Path, Visited, Depth, MaxDepth, TokenBound) ->
    case fire_transition(Transition, CurrentMarking) of
        {ok, NextMarking} ->
            %% Add transition to path
            Step = {maps:get(id, Transition), NextMarking},
            dfs([Step | Path], NextMarking, Transitions, Visited, Depth + 1, MaxDepth, TokenBound);
        {error, _} ->
            []
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Deadlock detection identifies deadlock states
- [ ] Dead transition detection reports unreachable transitions
- [ ] Completion check detects non-completing workflows
- [ ] All errors follow yawl_validate validation_error() format

##### Manual Verification:
- [ ] Test with known deadlock workflow (e.g., incorrect critical section)
- [ ] Test with workflow containing unreachable task
- [ ] Test with workflow that cannot complete (livelock scenario)
- [ ] Verify error messages are clear and actionable

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Integration and Testing

#### Overview
Create test cases for validation backend, integrate with existing validation infrastructure, and ensure performance requirements are met.

#### Changes Required:

##### 1. Create test suite
**File**: `/Users/sac/cre/src/validate/yawl_model_checker_tests.erl`
**Changes**: New comprehensive test suite

```erlang
-module(yawl_model_checker_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test simple sequence workflow (should pass)
sequence_workflow_test() ->
    Spec = create_sequence_spec(),
    {ok, Warnings} = yawl_model_checker:validate(Spec),
    ?assertEqual([], Warnings).

%% Test deadlock workflow (should detect deadlock)
deadlock_workflow_test() ->
    Spec = create_deadlock_spec(),
    {error, Errors} = yawl_model_checker:validate(Spec),
    ?assert(lists:any(fun(#{code := Code}) -> Code =:= deadlock_detected end, Errors)).

%% Test workflow with unreachable task
dead_transition_test() ->
    Spec = create_dead_transition_spec(),
    {ok, Warnings} = yawl_model_checker:validate(Spec),
    ?assert(lists:any(fun(#{code := Code}) -> Code =:= dead_transition end, Warnings)).

%% Test workflow that cannot complete
no_completion_test() ->
    Spec = create_no_completion_spec(),
    {error, Errors} = yawl_model_checker:validate(Spec),
    ?assert(lists:any(fun(#{code := Code}) -> Code =:= no_completion_path end, Errors)).

%% Helper functions to create test specifications
create_sequence_spec() ->
    #{
        id => <<"sequence_test">>,
        name => <<"Sequence Test">>,
        tasks => #{
            <<"task1">> => #{id => <<"task1">>, name => <<"Task 1">>, type => atomic},
            <<"task2">> => #{id => <<"task2">>, name => <<"Task 2">>, type => atomic}
        },
        conditions => #{
            <<"input">> => #{id => <<"input">>, type => input_condition},
            <<"output">> => #{id => <<"output">>, type => output_condition}
        },
        flows => [
            #{id => <<"f1">>, source => <<"input">>, target => <<"task1">>},
            #{id => <<"f2">>, source => <<"task1">>, target => <<"task2">>},
            #{id => <<"f3">>, source => <<"task2">>, target => <<"output">>}
        ],
        data_mappings => []
    }.

create_deadlock_spec() ->
    %% Create workflow with XOR split / AND join mismatch (potential deadlock)
    #{
        id => <<"deadlock_test">>,
        name => <<"Deadlock Test">>,
        tasks => #{
            <<"split">> => #{
                id => <<"split">>,
                name => <<"Split">>,
                type => atomic,
                split_type => 'xor'
            },
            <<"task1">> => #{id => <<"task1">>, name => <<"Task 1">>, type => atomic},
            <<"task2">> => #{id => <<"task2">>, name => <<"Task 2">>, type => atomic},
            <<"join">> => #{
                id => <<"join">>,
                name => <<"Join">>,
                type => atomic,
                join_type => 'and'
            }
        },
        conditions => #{
            <<"input">> => #{id => <<"input">>, type => input_condition},
            <<"output">> => #{id => <<"output">>, type => output_condition}
        },
        flows => [
            #{id => <<"f1">>, source => <<"input">>, target => <<"split">>},
            #{id => <<"f2">>, source => <<"split">>, target => <<"task1">>},
            #{id => <<"f3">>, source => <<"split">>, target => <<"task2">>},
            #{id => <<"f4">>, source => <<"task1">>, target => <<"join">>},
            #{id => <<"f5">>, source => <<"task2">>, target => <<"join">>},
            #{id => <<"f6">>, source => <<"join">>, target => <<"output">>}
        ],
        data_mappings => []
    }.

create_dead_transition_spec() ->
    %% Create workflow with unreachable task
    #{
        id => <<"dead_transition_test">>,
        name => <<"Dead Transition Test">>,
        tasks => #{
            <<"task1">> => #{id => <<"task1">>, name => <<"Task 1">>, type => atomic},
            <<"task2">> => #{id => <<"task2">>, name => <<"Task 2 (unreachable)">>, type => atomic},
            <<"task3">> => #{id => <<"task3">>, name => <<"Task 3">>, type => atomic}
        },
        conditions => #{
            <<"input">> => #{id => <<"input">>, type => input_condition},
            <<"output">> => #{id => <<"output">>, type => output_condition}
        },
        flows => [
            #{id => <<"f1">>, source => <<"input">>, target => <<"task1">>},
            %% No flow to task2 - it's unreachable
            #{id => <<"f2">>, source => <<"task1">>, target => <<"task3">>},
            #{id => <<"f3">>, source => <<"task3">>, target => <<"output">>}
        ],
        data_mappings => []
    }.

create_no_completion_spec() ->
    %% Create workflow with infinite loop
    #{
        id => <<"no_completion_test">>,
        name => <<"No Completion Test">>,
        tasks => #{
            <<"task1">> => #{id => <<"task1">>, name => <<"Task 1">>, type => atomic},
            <<"task2">> => #{id => <<"task2">>, name => <<"Task 2">>, type => atomic}
        },
        conditions => #{
            <<"input">> => #{id => <<"input">>, type => input_condition},
            <<"output">> => #{id => <<"output">>, type => output_condition}
        },
        flows => [
            #{id => <<"f1">>, source => <<"input">>, target => <<"task1">>},
            #{id => <<"f2">>, source => <<"task1">>, target => <<"task2">>},
            #{id => <<"f3">>, source => <<"task2">>, target => <<"task1">>}  % Loop back
        ],
        data_mappings => []
    }.
```

##### 2. Integrate with existing validation
**File**: `/Users/sac/cre/src/core/yawl_validate.erl:158-186`
**Changes**: Add optional model checking step

```erlang
validate(Spec) when is_map(Spec) ->
    AllErrors = lists:flatten([
        check_required_elements(Spec),
        check_tasks(Spec),
        check_flows(Spec),
        check_decompositions(Spec),
        check_variables(Spec),
        check_consistency(Spec),
        check_model_properties(Spec)  % New: bounded model checking
    ]),

    %% Separate errors and warnings
    {Errors, Warnings} = lists:partition(
        fun(#{severity := Sev}) -> Sev =:= error end,
        AllErrors
    ),

    case Errors of
        [] -> {ok, Warnings};
        _ -> {error, Errors ++ Warnings}
    end;

%% New function
-spec check_model_properties(specification()) -> [validation_error()].

check_model_properties(Spec) ->
    case application:get_env(cre, enable_model_checking, false) of
        true ->
            case yawl_model_checker:validate(Spec) of
                {ok, ModelWarnings} -> ModelWarnings;
                {error, ModelErrors} -> ModelErrors
            end;
        false ->
            []
    end.
```

##### 3. Add performance monitoring
**File**: `/Users/sac/cre/src/validate/yawl_model_checker.erl`
**Changes**: Add timing and statistics

```erlang
validate(Spec, Options) when is_map(Spec), is_map(Options) ->
    StartTime = erlang:monotonic_time(millisecond),
    try
        {ok, InitialMarking, Transitions} = yawl_pnet_compiler:compile(Spec),

        {ok, Traces} = yawl_explorer:explore(InitialMarking, Transitions, Options),

        Deadlocks = check_deadlock(Traces),
        DeadTransitions = check_dead_transitions(Traces, Transitions),
        Completion = check_completion(Traces),

        AllIssues = Deadlocks ++ DeadTransitions ++ Completion,

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Log performance metrics
        io:format("Model checking completed in ~pms~n", [Duration]),
        io:format("Explored ~p states~n", [length(Traces)]),

        {Errors, Warnings} = lists:partition(
            fun(#{severity := Sev}) -> Sev =:= error end,
            AllIssues
        ),

        case Errors of
            [] -> {ok, Warnings};
            _ -> {error, Errors ++ Warnings}
        end
    catch
        _:Error -> {error, Error}
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] All tests pass: `rebar3 eunit`
- [ ] Test coverage > 80% for new modules
- [ ] Performance: Validation completes in < 30 seconds for typical workflows
- [ ] Memory usage: State space fits in memory for workflows up to 50 tasks

##### Manual Verification:
- [ ] Run validation on real YAWL workflow files
- [ ] Verify deadlock detection on known problematic workflow
- [ ] Confirm no false positives on correct workflows
- [ ] Check integration with existing validation workflow
- [ ] Test with increasing depth bounds to verify termination

**Note**: Complete all automated verification, then pause for manual confirmation. This completes the implementation.

---

## Testing Strategy

### Unit Tests:
- Test compilation of various YAWL structures (single task, sequence, parallel, choice)
- Test bounded exploration with different depth/token bounds
- Test deadlock detection on known deadlock patterns
- Test dead transition detection on workflows with isolated tasks
- Test completion check on non-completing workflows

### Integration Tests:
- Test full validation pipeline from YAWL XML to error reports
- Test integration with existing yawl_validate:validate/1
- Test performance on realistic workflow sizes (10-50 tasks)
- Test memory usage on workflows with many parallel branches

### Manual Testing Steps:
1. Create a YAWL XML file with a simple sequence workflow
2. Run validation: `yawl_model_checker:validate(Spec)` and verify no errors
3. Create a YAWL XML file with XOR split / AND join mismatch
4. Run validation and verify deadlock is detected
5. Create a YAWL XML file with isolated (unreachable) task
6. Run validation and verify dead transition warning
7. Measure validation time on workflow with 20 tasks
8. Verify validation completes in < 30 seconds

## Migration Notes
- This is a new feature with no backward compatibility concerns
- Existing validation continues to work unchanged
- Model checking is opt-in via application environment variable
- No data migration required

## References
- Research: `/Users/sac/cre/.wreckit/items/018-validation-backend-with-bounded-model-checking/research.md`
- Petri net types: `/Users/sac/cre/src/pnet/pnet_types.erl:1-558`
- Marking algebra: `/Users/sac/cre/src/pnet/pnet_marking.erl:1-488`
- Mode enumeration: `/Users/sac/cre/src/pnet/pnet_mode.erl:1-353`
- YAWL validation: `/Users/sac/cre/src/core/yawl_validate.erl:1-1197`
- Schema parsing: `/Users/sac/cre/src/yawl/yawl_schema.erl:1-1104`
- Pattern implementations: `/Users/sac/cre/src/patterns/sequence.erl`, `/Users/sac/cre/src/patterns/parallel_split.erl`
- Critical section pattern (deadlock example): `/Users/sac/cre/src/patterns/critical_section.erl:1-739`
