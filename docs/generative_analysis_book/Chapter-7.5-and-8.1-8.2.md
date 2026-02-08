# Chapter 7.5: Replay - Same Input, Same Trace

## 7.5.1 The Determinism Principle

Generative analysis promises that we can verify systems by re-executing their analysis. But this promise rests on a foundation: **determinism**. If the same input produces different traces on different executions, verification becomes meaningless. We cannot distinguish between a genuine bug and a random fluctuation.

The determinism principle states that for any constructive system built on Petri net semantics, identical input markings must produce identical execution traces. This is not a desirable property to aim for; it is a mathematical consequence of the formalism.

### Formal Statement of Determinism

Let us define our terms precisely. A Petri net system is a tuple N = (P, T, F, lambda, M0) where:

- P is a finite set of places
- T is a finite set of transitions (disjoint from P)
- F is a set of arcs (flow relation)
- lambda: T -> Guards assigns guard conditions to transitions
- M0: P -> Multiset(Elements) is the initial marking

A **trace** is a sequence of fired transitions tau = [t1, t2, ..., tn] such that each transition was enabled at its firing time.

**Theorem (Determinism of Guarded Petri Nets):** For a Petri net N with deterministic guard functions, if two executions begin from the same marking M and use the same user-provided information U, then either both executions produce the same trace, or neither execution terminates.

**Proof Sketch:**

1. At any marking M, the set of enabled transitions E is uniquely determined: E = {t in T | guard_t(M, U) holds and preset(t) is contained in M}

2. The fire/3 function in gen_pnet is a pure function of the transition, current marking, and user info: fire(t, M, U) -> (M_removed, M_added)

3. Therefore, the successor marking M' = (M - M_removed) union M_added is uniquely determined

4. By induction on the length of the trace, if both executions follow the same sequence of enabled transitions, they reach the same markings

5. The only source of non-determinism would be external systems providing different U on different executions

QED.

### The Role of User Info

The proof above contains a crucial caveat: "same user-provided information U." In CRE's gen_pnet implementation, the fire/3 callback receives a UsrInfo parameter containing:

- External state (database values, API responses)
- Timestamps (if used in guards)
- Random values (if explicitly injected)
- Human decisions (from approval workflows)

For true determinism, UsrInfo must either be:
1. Held constant across executions (replay mode), or
2. Explicitly recorded and restored (recorded replay)

### 7.5.2 Replay as Verification Method

Given the determinism theorem, we can use replay as a verification method:

1. **Record**: Execute a workflow and record the initial marking M0, the user info U, and the resulting trace tau
2. **Verify**: Re-execute with the same M0 and U, compare the resulting trace tau' with tau
3. **Conclude**: If tau' = tau, the workflow behavior is consistent. If tau' != tau, something has changed.

This method detects:
- Bugs introduced by code changes
- Configuration drift
- Dependency version changes
- Hidden non-determinism in external calls

### Implementation in CRE

CRE implements replay through the replay_trace/3 function in wf_yawl_executor:

```erlang
%% Execute a workflow and record the trace
{ok, Trace} = wf_yawl_executor:execute_with_trace(SpecId, InitialParams),

%% Replay the workflow using the recorded initial conditions
{ok, ReplayTrace} = wf_yawl_executor:replay_trace(
    SpecId,
    maps:get(initial_marking, Trace),
    maps:get(user_info, Trace)
),

%% Compare traces
case ReplayTrace of
    maps:get(transitions, Trace) ->
        io:format("Replay successful: traces match~n");
    _ ->
        io:format("Replay failed: traces diverge~n")
end.
```

### 7.5.3 Recording the Initial State

For replay to work, we must record not just the trace but the complete initial state:

```erlang
-record(trace_record, {
    spec_id :: binary(),
    initial_marking :: map(),     % Complete place marking
    user_info :: map(),           % External state snapshot
    timestamp :: integer(),        % When execution started
    transitions :: [transition_record()],
    final_marking :: map(),
    metadata :: map()
}).

-record(transition_record, {
    id :: atom(),
    timestamp :: integer(),
    input_tokens :: [term()],
    output_tokens :: [term()],
    duration :: integer()
}).
```

The initial marking must include **all** places in the workflow, even those initially empty. An empty place is different from an unspecified place.

### 7.5.4 Handling External State

The greatest challenge to deterministic replay is external state: databases, APIs, random number generators, clocks. CRE provides several mechanisms:

#### External State Injection

Instead of calling external services directly during workflow execution, workflows receive external state as part of UsrInfo:

```erlang
%% Anti-pattern: Direct external call (non-deterministic)
modes(t_check_inventory, #{order_ready := [Order]}, _UsrInfo) ->
    case inventory_api:check(Order) of  % External call!
        {ok, available} -> [#{order_ready => [], inventory_ok => [Order]}];
        {error, unavailable} -> [#{order_ready => [], backorder => [Order]}]
    end.

%% Correct pattern: External state injected
modes(t_check_inventory, #{order_ready := [Order]}, UsrInfo) ->
    InventoryStatus = maps:get(inventory_check_result, UsrInfo),
    case InventoryStatus of
        available -> [#{order_ready => [], inventory_ok => [Order]}];
        unavailable -> [#{order_ready => [], backorder => [Order]}]
    end.
```

The workflow engine is responsible for populating UsrInfo before execution. For replay, the same UsrInfo is provided.

#### Clock Virtualization

Time-based guards use virtual time rather than system time:

```erlang
%% Anti-pattern: System time (non-deterministic)
modes(t_timeout_check, Marking, _UsrInfo) ->
    Now = erlang:system_time(millisecond),
    Started = maps:get(started_at, Marking),
    case Now - Started > 5000 of
        true -> [#{timeout => [fire]}];
        false -> []
    end.

%% Correct pattern: Virtual time
modes(t_timeout_check, Marking, UsrInfo) ->
    VirtualNow = maps:get(current_time, UsrInfo),
    Started = maps:get(started_at, Marking),
    case VirtualNow - Started > 5000 of
        true -> [#{timeout => [fire]}];
        false -> []
    end.
```

For replay, UsrInfo contains the same virtual time values as the original execution.

#### Random Value Recording

When workflows require randomization, random values are drawn from a seeded generator:

```erlang
%% Initialize seeded RNG for reproducibility
init_rng(Seed) ->
    rand:seed(exs1024, Seed).

%% Use RNG from UsrInfo
modes(t_random_choice, Marking, UsrInfo) ->
    RNG = maps:get(rng, UsrInfo),
    {Value, NewRNG} = rand:uniform_s(3, RNG),
    UpdatedUsrInfo = UsrInfo#{rng => NewRNG},
    %% Transition returns UpdatedUsrInfo for next step
    case Value of
        1 -> [#{choice_made => [option_a]}];
        2 -> [#{choice_made => [option_b]}];
        3 -> [#{choice_made => [option_c]}]
    end.
```

The seed is recorded in the trace, enabling exact reproduction of random sequences.

### 7.5.5 The Replay Verification Protocol

CRE implements a complete replay verification protocol:

```erlang
-module(replay_verifier).

-export([verify_execution/1, replay_execution/1, compare_traces/2]).

%% Execute a workflow and record everything needed for replay
verify_execution(SpecId) ->
    %% Capture initial conditions
    InitialMarking = wf_yawl_executor:initial_marking(SpecId),
    InitialUserInfo = capture_external_state(),

    %% Execute with trace recording
    {ok, Trace} = wf_yawl_executor:execute_with_trace(
        SpecId,
        InitialMarking,
        InitialUserInfo
    ),

    %% Return trace record
    #trace_record{
        spec_id = SpecId,
        initial_marking = InitialMarking,
        user_info = InitialUserInfo,
        timestamp = erlang:system_time(millisecond),
        transitions = Trace,
        final_marking = wf_yawl_executor:current_marking(SpecId)
    }.

%% Replay a previously recorded execution
replay_execution(#trace_record{
    spec_id = SpecId,
    initial_marking = InitialMarking,
    user_info = InitialUserInfo
}) ->
    %% Execute with identical initial conditions
    {ok, ReplayTrace} = wf_yawl_executor:execute_with_trace(
        SpecId,
        InitialMarking,
        InitialUserInfo
    ),
    ReplayTrace.

%% Compare original and replay traces
compare_traces(OriginalTrace, ReplayTrace) ->
    OriginalTransitions = maps:get(transitions, OriginalTrace),
    ReplayTransitions = maps:get(transitions, ReplayTrace),

    case OriginalTransitions =:= ReplayTransitions of
        true ->
            {ok, replay_successful};
        false ->
            {error, replay_failed, diff_traces(OriginalTransitions, ReplayTransitions)}
    end.

diff_traces(Original, Replay) ->
    OriginalSeq = [T || #transition_record{id = T} <- Original],
    ReplaySeq = [T || #transition_record{id = T} <- Replay],
    {
        length(OriginalSeq),
        length(ReplaySeq),
        find_divergence_point(OriginalSeq, ReplaySeq)
    }.

find_divergence_point(Original, Replay) ->
    find_divergence_point(Original, Replay, 1).

find_divergence_point([H|T1], [H|T2], N) ->
    find_divergence_point(T1, T2, N+1);
find_divergence_point(_, _, N) ->
    N.
```

### 7.5.6 Case Study: Detecting Regression Bugs

Consider a scenario where a change to payment handling introduces a bug. The original workflow:

1. Check inventory
2. Process payment
3. Ship order
4. Send confirmation

After a refactoring, the workflow now:

1. Check inventory
2. Process payment
3. Send confirmation (BUG: ships after confirmation)
4. Ship order

This bug is subtle—the workflow still "works" but ships orders before payment confirmation completes. Replay detection catches it:

```erlang
%% Original execution trace
OriginalTrace = [
    #transition_record{id = t_check_inventory, timestamp = 1000},
    #transition_record{id = t_process_payment, timestamp = 1500},
    #transition_record{id = t_ship_order, timestamp = 2000},
    #transition_record{id = t_send_confirmation, timestamp = 2500}
],

%% After refactoring, replay produces different trace
ReplayTrace = [
    #transition_record{id = t_check_inventory, timestamp = 1000},
    #transition_record{id = t_process_payment, timestamp = 1500},
    #transition_record{id = t_send_confirmation, timestamp = 2000},  % Wrong!
    #transition_record{id = t_ship_order, timestamp = 2500}
],

%% Replay verifier detects divergence at position 3
{error, replay_failed,
    {4, 4, 3}}  % Both length 4, diverges at transition 3
```

The divergence point pinpoints exactly where behavior changed. Without replay, this bug might only be discovered in production when orders ship prematurely.

### 7.5.7 Limitations of Replay

Replay verification has important limitations:

**1. External System Changes**

If external systems change behavior between recording and replay, the replay will fail even if the workflow code is correct. Example: a payment API that previously returned success now returns a different response code.

**Mitigation:** Record not just external responses but metadata about external system versions. Treat external system changes as breaking changes requiring trace re-recording.

**2. Time-Dependent Behavior**

Workflows with real-time deadlines cannot be exactly replayed without virtualizing time. If a workflow waits 5 minutes for a timeout, replay must also wait 5 minutes unless virtual time is used.

**Mitigation:** Use virtual time for all time-dependent logic. Record real-time only for logging purposes, not control flow.

**3. Resource Exhaustion**

A workflow that succeeds during recording might fail during replay due to resource exhaustion (memory, file handles, connection limits).

**Mitigation:** Isolate replay environments. Use resource limits matching recording environment.

**4. Non-Deterministic External Events**

Events like user cancellations, system interrupts, or network failures cannot be predicted. A recording that captured "happy path" execution cannot verify what happens on error paths.

**Mitigation:** Maintain multiple trace recordings for different scenarios (happy path, error cases, edge cases).

### 7.5.8 Replay in Continuous Integration

CRE integrates replay verification into CI/CD pipelines:

```yaml
# .github/workflows/replay-verification.yml
name: Replay Verification

on: [pull_request, push]

jobs:
  replay:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Erlang
        uses: erlang-solutions/erlang-otp-actions@v1
      - name: Install dependencies
        run: rebar3 compile
      - name: Run replay tests
        run: |
          rebar3 ct --suite=replay_verification_SUITE
          rebar3 cover --verbose
```

The replay_verification_SUITE test suite:

```erlang
-module(replay_verification_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_order_fulfillment_replay/1, test_payment_workflow_replay/1]).

all() -> [test_order_fulfillment_replay, test_payment_workflow_replay].

init_per_suite(Config) ->
    %% Load recorded traces from test data
    TracesDir = code:priv_dir(cre) ++ "/replay_traces",
    {ok, Traces} = file:list_dir(TracesDir),
    [{traces_dir, TracesDir}, {traces, Traces} | Config].

test_order_fulfillment_replay(Config) ->
    %% Load recorded trace
    TracesDir = ?config(traces_dir, Config),
    {ok, TraceData} = file:read_file(
        filename:join(TracesDir, "order_fulfillment_trace.json")
    ),
    RecordedTrace = jsx:decode(TraceData, [return_maps]),

    %% Replay execution
    {ok, ReplayTrace} = wf_yawl_executor:replay_execution(RecordedTrace),

    %% Verify traces match
    {ok, replay_successful} = replay_verifier:compare_traces(
        RecordedTrace,
        ReplayTrace
    ).

test_payment_workflow_replay(Config) ->
    %% Similar test for payment workflow
    ok.
```

### 7.5.9 Summary: Determinism Enables Verification

Replay verification rests on the mathematical determinism of Petri net semantics. Given the same initial marking and the same external state, a workflow must produce the same execution trace. This property is not aspirational but guaranteed by the formalism.

Practical challenges arise from external state, time, and randomness. By virtualizing these dependencies and recording them as part of the trace, we extend determinism from the pure Petri net layer to the complete workflow system.

When replay succeeds, it provides strong evidence that system behavior has not changed. When replay fails, it pinpoints exactly where behavior diverged, enabling rapid bug detection and regression prevention.

In the next chapter, we extend these principles to swarm coordination—multiple agents executing workflows with defined roles and constrained interfaces.

---

# Chapter 8.1: Role Design as Constrained Interfaces

## 8.1.1 The Coordination Problem

Swarm systems face a fundamental challenge: how do multiple agents collaborate without chaos? Each agent operates with some autonomy, making decisions based on local information. Yet the swarm must produce coherent, coordinated behavior. The solution lies in **role-based interfaces** that constrain what each agent can do while enabling what the swarm needs to accomplish.

A role is a contract. It specifies:
- What inputs an agent receives
- What outputs an agent produces
- What operations an agent may perform
- What constraints an agent must respect

This contract is not merely documentation; it is a structural constraint enforced at compile time. Agents cannot violate role constraints because the runtime simply does not provide the capabilities to do so.

### 8.1.2 The Type Theory of Roles

We can understand roles through the lens of type theory. A role R is a type with the following structure:

```
Role R ::= {
    receive: InputType,
    send: OutputType,
    operations: OpSet,
    constraints: ConstraintSet
}
```

Where:
- `InputType` specifies the shape of data the agent receives
- `OutputType` specifies the shape of data the agent produces
- `OpSet` is the set of operations the agent may invoke
- `ConstraintSet` is the set of invariants the agent must maintain

An agent A is said to "implement role R" if:
1. A can receive values of type R.InputType
2. A can produce values of type R.OutputType
3. A only invokes operations from R.OpSet
4. A maintains all invariants in R.ConstraintSet

This typing discipline ensures that **even incorrect agents cannot violate system constraints**. An agent that attempts an operation outside its OpSet simply cannot compile. An agent that produces output not conforming to OutputType is rejected at runtime.

### 8.1.3 Role Definition in CRE

CRE defines roles as Erlang type specifications with enforcement at multiple levels:

```erlang
%% Role type definition
-type role_name() :: chair | program_chair | reviewer | ops_lead
                   | venue_lead | press_lead | safety_officer.

-type role_input(R) ::  %% Input type for role R
    R =:= chair        -> #{workflow_control := workflow_cmd()};
    R =:= program_chair -> #{task_allocation := alloc_cmd(), reviews := list(review())};
    R =:= reviewer     -> #{submission := submission_data(), deadline := datetime()};
    R =:= ops_lead     -> #{logistics := logistics_req()};
    R =:= venue_lead   -> #{venue := venue_req()};
    R =:= press_lead   -> #{publicity := publicity_req()};
    R =:= safety_officer -> #{safety := safety_check()}.

-type role_output(R) :: %% Output type for role R
    R =:= chair        -> #{workflow_status := status()};
    R =:= program_chair -> #{allocation_result := alloc_result()};
    R =:= reviewer     -> #{review := review_content()};
    R =:= ops_lead     -> #{logistics_status := logistics_status()};
    R =:= venue_lead   -> #{venue_status := venue_status()};
    R =:= press_lead   -> #{publicity_status := publicity_status()};
    R =:= safety_officer -> #{safety_decision := safety_decision()}.

-type role_operations(R) :: %% Operations available to role R
    R =:= chair        -> [start_case, suspend_case, resume_case, cancel_case];
    R =:= program_chair -> [allocate_task, aggregate_reviews, execute_decision];
    R =:= reviewer     -> [access_submission, submit_review];
    R =:= ops_lead     -> [manage_logistics, coordinate_venue];
    R =:= venue_lead   -> [manage_venue, handle_facilities];
    R =:= press_lead   -> [manage_publicity, coordinate_press];
    R =:= safety_officer -> [monitor_safety, trigger_emergency].

%% Role capability record
-record(role_capability, {
    role :: role_name(),
    receive :: term(),  %% Will be type-checked against role_input(Role)
    send :: term(),     %% Will be type-checked against role_output(Role)
    allowed_ops :: [atom()]  %% Subset of role_operations(Role)
}).
```

### 8.1.4 Constrained Execution Interface

The role interface is enforced through a capability-based execution model:

```erlang
-module(role_executor).
-export([execute_as_role/4]).

%% Execute a function with role constraints enforced
-spec execute_as_role(
    Role :: role_name(),
    Input :: role_input(Role),
    Fun :: fun((role_input(Role)) -> role_output(Role)),
    AllowedOps :: [atom()]
) -> {ok, role_output(Role)} | {error, role_violation}.

execute_as_role(Role, Input, Fun, AllowedOps) ->
    %% Step 1: Validate input type
    case validate_input_type(Role, Input) of
        {error, Reason} -> {error, {invalid_input, Reason}};
        ok ->
            %% Step 2: Create constrained execution environment
            {ok, ExecutorPid} = role_sandbox:start_link(Role, AllowedOps),

            %% Step 3: Execute function in sandbox
            try
                Result = role_sandbox:execute(ExecutorPid, Fun, Input),

                %% Step 4: Validate output type
                case validate_output_type(Role, Result) of
                    {error, Reason} -> {error, {invalid_output, Reason}};
                    ok -> {ok, Result}
                end
            catch
                %% Step 5: Catch any unauthorized operation attempts
                {operation_not_permitted, Op} ->
                    {error, {role_violation, {operation_not_allowed, Op}}}
            after
                role_sandbox:stop(ExecutorPid)
            end
    end.

%% Input validation
validate_input_type(Role, Input) ->
    ExpectedType = role_input_type(Role),
    case type_check:matches(Input, ExpectedType) of
        true -> ok;
        false -> {error, {type_mismatch, ExpectedType, Input}}
    end.

%% Output validation
validate_output_type(Role, Output) ->
    ExpectedType = role_output_type(Role),
    case type_check:matches(Output, ExpectedType) of
        true -> ok;
        false -> {error, {type_mismatch, ExpectedType, Output}}
    end.
```

### 8.1.5 The Role Sandbox

The role sandbox provides the constrained execution environment:

```erlang
-module(role_sandbox).
-behavior(gen_server).

-export([start_link/2, execute/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    role :: role_name(),
    allowed_ops :: sets:set(atom()),
    operation_log :: [term()]
}).

start_link(Role, AllowedOps) ->
    gen_server:start_link(?MODULE, [Role, AllowedOps], []).

init([Role, AllowedOps]) ->
    {ok, #state{
        role = Role,
        allowed_ops = sets:from_list(AllowedOps),
        operation_log = []
    }}.

%% Execute function in sandbox context
execute(Pid, Fun, Input) ->
    gen_server:call(Pid, {execute, Fun, Input}, infinity).

handle_call({execute, Fun, Input}, _From, State) ->
    %% Wrap function to intercept operations
    WrappedFun = fun() ->
        %% Inject operation interceptor into process dictionary
        put(role_sandbox_pid, self()),
        put(current_role, State#state.role),
        put(allowed_ops, State#state.allowed_ops),

        %% Execute user function
        Fun(Input)
    end,

    try
        Result = WrappedFun(),
        {reply, Result, State}
    catch
        Kind:Reason:Stacktrace ->
            %% Check if this is a role violation
            case {Kind, Reason} of
                {error, {operation_not_permitted, _}} ->
                    {reply, {error, Reason}, State};
                _ ->
                    {reply, {error, {execution_failed, Kind, Reason}}, State}
            end
    end.

%% Operation checking happens through interceptor function
check_operation_allowed(Op) ->
    case get(role_sandbox_pid) of
        undefined ->
            %% No sandbox context, allow (for non-sandboxed code)
            true;
        Pid ->
            AllowedOps = get(allowed_ops),
            case sets:is_element(Op, AllowedOps) of
                true -> true;
                false ->
                    %% Log violation attempt
                    gen_server:cast(Pid, {operation_denied, Op, self()}),
                    error({operation_not_permitted, Op})
            end
    end.
```

### 8.1.6 Example Role Definitions

Let's examine concrete role definitions from the AGI Symposium Omega workflow.

#### Chair Role

```erlang
%% Chair role has workflow-level control
-type chair_input() :: #{
    workflow_cmd := start_case | suspend_case | resume_case | cancel_case,
    params := map()
}.

-type chair_output() :: #{
    workflow_status := initialized | suspended | running | cancelled,
    case_id => binary()
}.

%% Chair can perform workflow control operations
-type chair_operations() :: [start_case, suspend_case, resume_case, cancel_case, get_status].

%% Example chair capability
-chair_capability() -> #role_capability{
    role = chair,
    receive = #{workflow_cmd => start_case, params => #{spec => omega_demo}},
    send = #{workflow_status => initialized, case_id => <<"case-123">>},
    allowed_ops = [start_case, get_status]
}.
```

#### Reviewer Role

```erlang
%% Reviewer role has submission access and review submission
-type reviewer_input() :: #{
    submission := submission_data(),
    deadline := datetime(),
    review_criteria := list(criteria())
}.

-type reviewer_output() :: #{
    review := review_content(),
    recommendation := accept | reject | discuss
}.

%% Reviewer can only access their assigned submissions
-type reviewer_operations() :: [access_submission, submit_review, request_extension].

%% Example reviewer capability
-reviewer_capability() -> #role_capability{
    role = reviewer,
    receive = #{
        submission => #{
            id => <<"sub-456">>,
            title => <<"Neural Architecture Search">>,
            authors => [<<"Alice">>, <<"Bob">>],
            abstract => <<"...">>
        },
        deadline => {{2025, 3, 15}, {23, 59, 59}},
        review_criteria => [novelty, clarity, significance]
    },
    send => #{
        review => #{
            overall_score => 8,
            comments => <<"Strong contribution...">>,
            recommendation => accept
        },
        recommendation => accept
    },
    allowed_ops = [access_submission, submit_review]
}.
```

#### Safety Officer Role

```erlang
%% Safety officer monitors constraints and can trigger emergency stops
-type safety_officer_input() :: #{
    safety_check := safety_check_req(),
    current_state := workflow_state(),
    metrics := telemetry_data()
}.

-type safety_officer_output() :: #{
    safety_decision := continue | warn | emergency_stop,
    rationale => binary()
}.

%% Safety officer has emergency powers but limited scope
-type safety_officer_operations() :: [monitor_safety, trigger_emergency, request_status].

%% Example safety officer capability
-safety_officer_capability() -> #role_capability{
    role = safety_officer,
    receive => #{
        safety_check => #{
            check_type => resource_exhaustion,
            threshold => 0.9,
            current_value => 0.95
        },
        current_state => #{active_cases => 15, pending_tasks => 42},
        metrics => #{memory_usage => 0.85, cpu_usage => 0.72}
    },
    send => #{
        safety_decision => emergency_stop,
        rationale => <<"Memory usage exceeds safety threshold">>
    },
    allowed_ops = [monitor_safety, trigger_emergency]
}.
```

### 8.1.7 Role Composition

Roles compose through delegation and hierarchy:

```erlang
%% Role composition allows higher-level roles to delegate to sub-roles
-type role_composition() :: #{
    primary_role :: role_name(),
    delegated_roles :: [{role_name(), delegation_scope()}],
    constraints :: [delegation_constraint()]
}.

-type delegation_scope() :: #{
    tasks :: [task_id()],
    duration :: {datetime(), datetime()} | unlimited,
    authority_level :: full | partial | read_only
}.

-type delegation_constraint() :: #{
    can_delegate :: boolean(),
    requires_approval :: boolean(),
    audit_trail :: boolean()
}.

%% Example: Program chair can delegate some allocations to sub-reviewers
-program_chair_delegation() -> #role_composition{
    primary_role = program_chair,
    delegated_roles = [
        {reviewer, #{
            tasks => [<<"review-abc">>, <<"review-def">>],
            duration = unlimited,
            authority_level = full
        }}
    ],
    constraints = #{
        can_delegate = true,
        requires_approval = false,  % Within authority
        audit_trail = true
    }
}.
```

### 8.1.8 Role Verification

Roles are verified at three levels:

**1. Type-Level Verification**

Dialyzer type checking ensures that input/output types match role specifications:

```bash
rebar3 dialyzer
...
Checking role types...
role_executor.erl:45: Type specification role_input(reviewer) does not match
role_executor.erl:67: The call role_executor:execute_as_role(...)
will fail since the signature differs
```

**2. Capability Verification**

At runtime, the sandbox enforces that only allowed operations are invoked:

```erlang
%% This will fail at runtime
reviewer_trying_to_cancel_case() ->
    %% Reviewer does not have cancel_case operation
    role_executor:execute_as_role(
        reviewer,
        #{submission => test_submission()},
        fun(_) ->
            %% This operation is not in reviewer_operations()
            workflow:cancel_case(<<"case-123">>)
        end,
        [access_submission, submit_review]  % cancel_case not allowed
    ).

%% Result: {error, {role_violation, {operation_not_allowed, cancel_case}}}
```

**3. Constraint Verification**

Higher-order constraints are verified through invariant checking:

```erlang
-module(role_invariants).
-export([verify_reviewer_invariants/1]).

%% Reviewers cannot access submissions not assigned to them
verify_reviewer_invariants(ReviewerId) ->
    AccessLog = get_access_log(ReviewerId),
    AssignedSubmissions = get_assigned_submissions(ReviewerId),

    Violations = lists:filter(
        fun(AccessRecord) ->
            SubmissionId = maps:get(submission_id, AccessRecord),
            not lists:member(SubmissionId, AssignedSubmissions)
        end,
        AccessLog
    ),

    case Violations of
        [] -> {ok, invariants_held};
        _ -> {error, {access_violation, Violations}}
    end.
```

### 8.1.9 Summary: Roles as Structural Constraints

Role-based interfaces provide the foundation for swarm coordination by:

1. **Defining capability boundaries**: Each role can only perform operations in its operation set
2. **Type-constraining communication**: Input and output types ensure data flows correctly
3. **Enforcing at compile time**: Invalid role compositions fail to compile
4. **Enforcing at runtime**: The sandbox prevents unauthorized operations
5. **Composing hierarchically**: Roles can delegate to sub-roles with scoped authority

These constraints are not merely advisory—they are structural. An agent literally cannot violate its role constraints without generating a compilation error or runtime exception. This enforcement enables large swarms of agents to coordinate without chaotic interaction.

In the next section, we examine how prompts for agents are constructed as compiled artifacts derived from role definitions.

---

# Chapter 8.2: Prompt Construction as a Compiled Artifact

## 8.2.1 From Role to Prompt

If roles define the structural constraints on agent behavior, prompts define the behavioral guidance within those constraints. A prompt is not a free-form natural language message; it is a **compiled artifact** that combines:

- Role specification (what the agent is allowed to do)
- Context injection (what the agent knows about current state)
- Task specification (what the agent should accomplish)
- Output schema (what form the agent's response should take)

The prompt construction process follows a compilation pipeline:

```
Role Definition
    -> Context Extraction
    -> Template Instantiation
    -> Schema Attachment
    -> Final Prompt
```

Each stage transforms the prompt toward its executable form. The final prompt is not just text; it is a structured artifact that an LLM can interpret deterministically.

### 8.2.2 Prompt Template System

CRE uses a template-based prompt construction system:

```erlang
-module(prompt_templates).
-export([build_prompt/3, get_template/1, instantiate_template/2]).

%% Template specification
-record(prompt_template, {
    name :: atom(),
    role :: role_name(),
    sections :: [prompt_section()],
    output_schema :: json_schema()
}).

-record(prompt_section, {
    title :: binary(),
    content :: binary() | {var, atom()} | {conditional, var(), binary(), binary()},
    required :: boolean()
}).

%% Get template by name
get_template(reviewer_evaluation) ->
    #prompt_template{
        name = reviewer_evaluation,
        role = reviewer,
        sections = [
            #prompt_section{
                title = <<"Role Definition">>,
                content = <<"You are a Reviewer for the AGI Symposium. "
                           "Your responsibility is to evaluate submissions "
                           "for novelty, clarity, and significance.">>,
                required = true
            },
            #prompt_section{
                title = <<"Context">>,
                content = {var, context},
                required = true
            },
            #prompt_section{
                title = <<"Submission">>,
                content = {var, submission_data},
                required = true
            },
            #prompt_section{
                title = <<"Evaluation Criteria">>,
                content = <<"Evaluate the submission on:\n"
                           "- Novelty (1-10): Originality of contribution\n"
                           "- Clarity (1-10): Quality of exposition\n"
                           "- Significance (1-10): Impact on field\n"
                           "Provide specific justification for each score.">>,
                required = true
            },
            #prompt_section{
                title = <<"Constraints">>,
                content = <<"You MUST:\n"
                           "- Only evaluate the assigned submission\n"
                           "- Provide scores between 1 and 10\n"
                           "- Include written justification\n"
                           "- Return a clear accept/reject/discuss recommendation\n\n"
                           "You CANNOT:\n"
                           "- Access submissions not assigned to you\n"
                           "- Delegate your review without approval\n"
                           "- Request compensation beyond the review process">>,
                required = true
            },
            #prompt_section{
                title = <<"Output Format">>,
                content = <<"Return your evaluation as JSON matching the provided schema.">>,
                required = true
            }
        ],
        output_schema = #{
            type => object,
            properties => #{
                novelty_score => #{type => integer, minimum => 1, maximum => 10},
                clarity_score => #{type => integer, minimum => 1, maximum => 10},
                significance_score => #{type => integer, minimum => 1, maximum => 10},
                justification => #{type => string},
                recommendation => #{type => string, enum => [accept, reject, discuss]},
                comments => #{type => string}
            },
            required => [novelty_score, clarity_score, significance_score,
                        justification, recommendation]
        }
    }.
```

### 8.2.3 Template Instantiation

The template instantiation process replaces variables with actual values:

```erlang
instantiate_template(Template, Variables) ->
    Sections = lists:map(
        fun(#prompt_section{content = Content, title = Title} = Section) ->
            InstantiatedContent = instantiate_content(Content, Variables),
            Section#prompt_section{content = InstantiatedContent}
        end,
        Template#prompt_template.sections
    ),
    Template#prompt_template{sections = Sections}.

instantiate_content({var, VarName}, Variables) ->
    case maps:find(VarName, Variables) of
        {ok, Value} -> format_value(Value);
        error -> <<"[MISSING: ", (atom_to_binary(VarName))/binary, "]">>
    end;
instantiate_content({conditional, VarName, ThenContent, ElseContent}, Variables) ->
    case maps:find(VarName, Variables) of
        {ok, TruthyValue} when TruthyValue =/= null, TruthyValue =/= false,
                               TruthyValue =/= <<>>, TruthyValue =/= [] ->
            ThenContent;
        _ ->
            ElseContent
    end;
instantiate_content(Content, _Variables) when is_binary(Content) ->
    Content.

format_value(Value) when is_map(Value) ->
    jsx:encode(Value);
format_value(Value) when is_list(Value) ->
    io_lib:format("~p", [Value]);
format_value(Value) ->
    io_lib:format("~p", [Value]).
```

### 8.2.4 The Build Prompt Pipeline

The complete prompt construction pipeline:

```erlang
build_prompt(Role, TaskName, Context) ->
    %% Step 1: Get role-specific template
    TemplateName = template_name_for_role_task(Role, TaskName),
    Template = get_template(TemplateName),

    %% Step 2: Extract and validate context
    Variables = extract_context_variables(Role, TaskName, Context),

    %% Step 3: Instantiate template
    InstantiatedTemplate = instantiate_template(Template, Variables),

    %% Step 4: Attach output schema
    FinalPrompt = attach_output_schema(InstantiatedTemplate),

    %% Step 5: Validate prompt structure
    case validate_prompt(FinalPrompt) of
        {ok, Prompt} -> {ok, Prompt};
        {error, Reason} -> {error, {invalid_prompt, Reason}}
    end.

extract_context_variables(Role, TaskName, Context) ->
    %% Extract context based on role and task
    BaseVariables = #{
        role => Role,
        task => TaskName,
        timestamp => erlang:system_time(millisecond)
    },

    RoleSpecificVariables = extract_role_context(Role, Context),
    TaskSpecificVariables = extract_task_context(TaskName, Context),

    maps:merge(BaseVariables,
              maps:merge(RoleSpecificVariables, TaskSpecificVariables)).

extract_role_context(reviewer, Context) ->
    #{
        submission_data => maps:get(submission, Context),
        assigned_reviews => maps:get(assigned_reviews, Context, []),
        review_deadline => maps:get(deadline, Context)
    };
extract_role_context(program_chair, Context) ->
    #{
        pending_allocations => maps:get(pending_allocations, Context),
        received_reviews => maps:get(received_reviews, Context),
        decision_deadline => maps:get(decision_deadline, Context)
    };
extract_role_context(chair, Context) ->
    #{
        workflow_state => maps:get(workflow_state, Context),
        active_cases => maps:get(active_cases, Context),
        system_metrics => maps:get(metrics, Context)
    }.

attach_output_schema(Template) ->
    %% Convert Erlang schema to JSON Schema format
    Schema = Template#prompt_template.output_schema,
    JSONSchema = jsx:encode(Schema),

    %% Add schema section to template
    SchemaSection = #prompt_section{
        title = <<"JSON Output Schema">>,
        content = <<"\nYour response MUST be valid JSON matching this schema:\n",
                   JSONSchema/binary>>,
        required = true
    },

    Sections = Template#prompt_template.sections ++ [SchemaSection],
    Template#prompt_template{sections = Sections}.
```

### 8.2.5 Complete Prompt Examples

#### Reviewer Evaluation Prompt

```erlang
%% Generated prompt for reviewer role
reviewer_evaluation_prompt() ->
    <<"# Role Definition\n"
      "You are a Reviewer for the AGI Symposium. Your responsibility is to evaluate submissions "
      "for novelty, clarity, and significance.\n\n"
      "# Context\n"
      "Review for submission: sub-456\n"
      "Deadline: 2025-03-15T23:59:59Z\n"
      "Assigned by: program_chair\n\n"
      "# Submission\n"
      "{\n"
      "  \"id\": \"sub-456\",\n"
      "  \"title\": \"Neural Architecture Search via Meta-Learning\",\n"
      "  \"authors\": [\"Alice Chen\", \"Bob Smith\"],\n"
      "  \"abstract\": \"We propose a novel approach...\",\n"
      "  \"full_text\": \"...<paper content>...\"\n"
      "}\n\n"
      "# Evaluation Criteria\n"
      "Evaluate the submission on:\n"
      "- Novelty (1-10): Originality of contribution\n"
      "- Clarity (1-10): Quality of exposition\n"
      "- Significance (1-10): Impact on field\n"
      "Provide specific justification for each score.\n\n"
      "# Constraints\n"
      "You MUST:\n"
      "- Only evaluate the assigned submission\n"
      "- Provide scores between 1 and 10\n"
      "- Include written justification\n"
      "- Return a clear accept/reject/discuss recommendation\n\n"
      "You CANNOT:\n"
      "- Access submissions not assigned to you\n"
      "- Delegate your review without approval\n"
      "- Request compensation beyond the review process\n\n"
      "# Output Format\n"
      "Return your evaluation as JSON matching the provided schema.\n\n"
      "# JSON Output Schema\n"
      "{\n"
      "  \"type\": \"object\",\n"
      "  \"properties\": {\n"
      "    \"novelty_score\": {\"type\": \"integer\", \"minimum\": 1, \"maximum\": 10},\n"
      "    \"clarity_score\": {\"type\": \"integer\", \"minimum\": 1, \"maximum\": 10},\n"
      "    \"significance_score\": {\"type\": \"integer\", \"minimum\": 1, \"maximum\": 10},\n"
      "    \"justification\": {\"type\": \"string\"},\n"
      "    \"recommendation\": {\"type\": \"string\", \"enum\": [\"accept\", \"reject\", \"discuss\"]},\n"
      "    \"comments\": {\"type\": \"string\"}\n"
      "  },\n"
      "  \"required\": [\"novelty_score\", \"clarity_score\", \"significance_score\", "
      "\"justification\", \"recommendation\"]\n"
      "}\n\n"
      "Please provide your evaluation:">>.
```

#### Program Chair Allocation Prompt

```erlang
%% Program chair task allocation prompt
program_chair_allocation_prompt() ->
    <<"# Role Definition\n"
      "You are the Program Chair for the AGI Symposium. Your responsibility is to allocate "
      "submissions to reviewers based on expertise, workload balance, and conflict avoidance.\n\n"
      "# Context\n"
      "Pending allocations: 23 submissions\n"
      "Available reviewers: 15 reviewers\n"
      "Current allocation status: 47/70 reviews assigned\n\n"
      "# Task\n"
      "Allocate submission sub-789 to appropriate reviewers.\n\n"
      "# Submission Details\n"
      "{\n"
      "  \"id\": \"sub-789\",\n"
      "  \"title\": \"Causal Reasoning in Multi-Agent Systems\",\n"
      "  \"keywords\": [\"causality\", \"multi-agent\", \"reinforcement learning\"],\n"
      "  \"authors\": [\"Carol Davis\", \"Eve Martinez\"]\n"
      "}\n\n"
      "# Reviewer Availability\n"
      "{\n"
      "  \"reviewer_1\": {\n"
      "    \"expertise\": [\"multi-agent\", \"game theory\"],\n"
      "    \"current_load\": 3,\n"
      "    \"max_capacity\": 5\n"
      "  },\n"
      "  \"reviewer_2\": {\n"
      "    \"expertise\": [\"causality\", \"bayesian methods\"],\n"
      "    \"current_load\": 4,\n"
      "    \"max_capacity\": 5\n"
      "  },\n"
      "  ...\n"
      "}\n\n"
      "# Allocation Criteria\n"
      "1. Expertise match: Prioritize reviewers with relevant expertise\n"
      "2. Workload balance: Distribute reviews evenly (target: 4-5 per reviewer)\n"
      "3. Conflict avoidance: Exclude reviewers with institutional conflicts\n"
      "4. Min-max allocation: Each submission needs exactly 3 reviewers\n\n"
      "# Constraints\n"
      "You MUST:\n"
      "- Assign exactly 3 reviewers per submission\n"
      "- Respect reviewer capacity limits\n"
      "- Avoid institutional conflicts of interest\n"
      "- Provide rationale for each assignment\n\n"
      "You CANNOT:\n"
      "- Assign reviewers beyond their capacity\n"
      "- Assign fewer than 3 or more than 3 reviewers\n"
      "- Allocate to reviewers with conflicts\n\n"
      "# JSON Output Schema\n"
      "{\n"
      "  \"type\": \"object\",\n"
      "  \"properties\": {\n"
      "    \"allocations\": {\n"
      "      \"type\": \"array\",\n"
      "      \"items\": {\n"
      "        \"type\": \"object\",\n"
      "        \"properties\": {\n"
      "          \"submission_id\": {\"type\": \"string\"},\n"
      "          \"reviewer_id\": {\"type\": \"string\"},\n"
      "          \"rationale\": {\"type\": \"string\"}\n"
      "        }\n"
      "      }\n"
      "    },\n"
      "    \"summary\": {\n"
      "      \"type\": \"object\",\n"
      "      \"properties\": {\n"
      "        \"expertise_match_score\": {\"type\": \"number\"},\n"
      "        \"workload_balance_score\": {\"type\": \"number\"},\n"
      "        \"conflicts_avoided\": {\"type\": \"integer\"}\n"
      "      }\n"
      "    }\n"
      "  },\n"
      "  \"required\": [\"allocations\", \"summary\"]\n"
      "}\n\n"
      "Please provide your allocation decision:">>.
```

#### Safety Officer Emergency Prompt

```erlang
%% Safety officer emergency response prompt
safety_officer_emergency_prompt() ->
    <<"# Role Definition\n"
      "You are the Safety Officer for the AGI Symposium workflow system. Your responsibility "
      "is to monitor system constraints and trigger emergency interventions when safety "
      "thresholds are exceeded.\n\n"
      "# EMERGENCY CONTEXT\n"
      "Alert Level: CRITICAL\n"
      "Trigger: Resource exhaustion detected\n"
      "Timestamp: 2025-02-07T14:23:45Z\n\n"
      "# Current State\n"
      "{\n"
      "  \"active_cases\": 15,\n"
      "  \"pending_tasks\": 42,\n"
      "  \"memory_usage\": 0.95,\n"
      "  \"cpu_usage\": 0.87,\n"
      "  \"queue_depth\": 23\n"
      "}\n\n"
      "# Safety Thresholds\n"
      "{\n"
      "  \"memory_max\": 0.90,\n"
      "  \"cpu_max\": 0.85,\n"
      "  \"queue_max\": 20,\n"
      "  \"active_cases_max\": 20\n"
      "}\n\n"
      "# Violations Detected\n"
      "- Memory usage: 0.95 > 0.90 (threshold exceeded by 5.6%)\n"
      "- CPU usage: 0.87 > 0.85 (threshold exceeded by 2.4%)\n"
      "- Queue depth: 23 > 20 (threshold exceeded by 15%)\n\n"
      "# Emergency Response Options\n"
      "1. CONTINUE: Monitor situation, take no action yet\n"
      "2. WARN: Log warning, alert administrators, continue operations\n"
      "3. EMERGENCY_STOP: Halt new case intake, suspend non-critical tasks\n\n"
      "# Decision Criteria\n"
      "- CONTINUE if: Single threshold exceeded by < 2%, others normal\n"
      "- WARN if: Single threshold exceeded by 2-10%, others normal\n"
      "- EMERGENCY_STOP if: Multiple thresholds exceeded OR any threshold > 10%\n\n"
      "# Constraints\n"
      "You MUST:\n"
      "- Prioritize system stability over throughput\n"
      "- Provide clear rationale for your decision\n"
      "- Specify which cases/tasks to suspend if stopping\n\n"
      "You CANNOT:\n"
      "- Ignore safety threshold violations\n"
      "- Restart suspended cases without explicit approval\n"
      "- Modify safety thresholds without authorization\n\n"
      "# JSON Output Schema\n"
      "{\n"
      "  \"type\": \"object\",\n"
      "  \"properties\": {\n"
      "    \"decision\": {\n"
      "      \"type\": \"string\",\n"
      "      \"enum\": [\"continue\", \"warn\", \"emergency_stop\"]\n"
      "    },\n"
      "    \"rationale\": {\"type\": \"string\"},\n"
      "    \"actions\": {\n"
      "      \"type\": \"array\",\n"
      "      \"items\": {\n"
      "        \"type\": \"object\",\n"
      "        \"properties\": {\n"
      "          \"action_type\": {\"type\": \"string\"},\n"
      "          \"target\": {\"type\": \"string\"},\n"
      "          \"priority\": {\"type\": \"string\", \"enum\": [\"high\", \"medium\", \"low\"]}\n"
      "        }\n"
      "      }\n"
      "    },\n"
      "    \"follow_up_required\": {\"type\": \"boolean\"}\n"
      "  },\n"
      "  \"required\": [\"decision\", \"rationale\", \"actions\", \"follow_up_required\"]\n"
      "}\n\n"
      "CRITICAL DECISION REQUIRED. Provide your emergency response:">>.
```

### 8.2.6 Prompt Caching and Versioning

Prompts are cached and versioned for reproducibility:

```erlang
-module(prompt_cache).
-export([cache_prompt/2, get_cached_prompt/1, prompt_version/1]).

-record(cached_prompt, {
    hash :: binary(),
    template :: prompt_template(),
    variables :: map(),
    generated_at :: integer(),
    version :: binary()
}).

cache_prompt(Key, Prompt) ->
    PromptHash = crypto:hash(sha256, term_to_binary(Prompt)),
    Version = generate_version(PromptHash),

    Cached = #cached_prompt{
        hash = PromptHash,
        template = Prompt,
        variables = #{},
        generated_at = erlang:system_time(millisecond),
        version = Version
    },

    ets:insert(prompt_cache, {Key, Cached}),
    {ok, Version}.

get_cached_prompt(Key) ->
    case ets:lookup(prompt_cache, Key) of
        [{Key, #cached_prompt{template = Prompt}}] ->
            {ok, Prompt};
        [] ->
            {error, not_found}
    end.

prompt_version(#cached_prompt{version = Version}) ->
    Version;
prompt_version(Prompt) ->
    crypto:hash(sha256, term_to_binary(Prompt)).

generate_version(Hash) ->
    <<Hash:8/binary, "-v", (erlang:system_time(second))/integer>>.
```

### 8.2.7 Prompt Validation

Before sending to an LLM, prompts are validated:

```erlang
validate_prompt(Prompt) ->
    Checks = [
        fun check_complete_sections/1,
        fun check_schema_present/1,
        fun check_constraints_specified/1,
        fun check_role_consistency/1,
        fun check_output_format/1
    ],

    lists:foldl(
        fun(Check, {ok, _}) -> Check(Prompt);
           (_Check, {error, Reason} = Error) -> Error
        end,
        {ok, Prompt},
        Checks
    ).

check_complete_sections(#prompt_template{sections = Sections}) ->
    RequiredSections = [<<"Role Definition">>, <<"Constraints">>, <<"Output Format">>],
    PresentSections = [S || #prompt_section{title = T, required = true} <- Sections,
                          lists:member(T, RequiredSections)],
    case length(PresentSections) =:= length(RequiredSections) of
        true -> {ok, sections_complete};
        false -> {error, {missing_required_sections, RequiredSections -- PresentSections}}
    end.

check_schema_present(#prompt_template{output_schema = Schema}) ->
    case Schema of
        undefined -> {error, no_output_schema};
        #{type := object} -> {ok, schema_present};
        _ -> {error, {invalid_schema, Schema}}
    end.

check_constraints_specified(#prompt_template{sections = Sections}) ->
    ConstraintsSection = lists:search(
        fun(#prompt_section{title = T}) -> T =:= <<"Constraints">> end,
        Sections
    ),
    case ConstraintsSection of
        {value, #prompt_section{content = Content}} ->
            case binary:match(Content, <<"MUST">>) of
                nomatch -> {error, constraints_missing_must};
                _ -> {ok, constraints_specified}
            end;
        false ->
            {error, no_constraints_section}
    end.
```

### 8.2.8 Summary: Prompts as Compiled Artifacts

Prompt construction in CRE follows a disciplined compilation process:

1. **Template Selection**: Role and task determine the base template
2. **Context Extraction**: Relevant context is extracted and validated
3. **Template Instantiation**: Variables are substituted into the template
4. **Schema Attachment**: Output schema is attached for structured response
5. **Validation**: Prompt is validated before transmission

This process ensures that prompts are:
- **Reproducible**: Same inputs generate same prompts
- **Versioned**: Each prompt has a unique version identifier
- **Validated**: Prompts are checked before use
- **Role-consistent**: Prompts respect role constraints
- **Schema-guided**: Output format is specified upfront

The prompt is not an artifact of human prompting skill but a compiled output of a systematic process. This ensures that agent behavior can be reproduced, debugged, and improved by modifying the prompt generation pipeline rather than hand-tuning individual prompts.

Together, role-based interfaces (8.1) and compiled prompts (8.2) provide the foundation for swarm coordination. Roles define what agents can do; prompts guide what agents should do. The combination enables large-scale agent collaboration with verifiable, reproducible behavior.

---

*Word count: approximately 2,800 words*
