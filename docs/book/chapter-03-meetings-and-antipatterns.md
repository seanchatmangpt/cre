# Chapter 3: Capturing Information as Constructive Inputs

## 3.4 Turning Meetings into Testable Artifacts

Meetings are the primary source of requirements, decisions, and design intent in most organizations. Yet they are also the single largest source of drift—the gradual divergence between what stakeholders think they've agreed upon and what the system actually implements. The problem is not that meetings happen; the problem is that their outputs evaporate into unstructured notes, fragmented memory, and implicit assumptions.

A constructive system treats meetings not as transient conversations but as **generative events** that must produce testable artifacts. Every decision, every requirement, and every constraint extracted from a meeting must be transformed into something executable: a test, a validation rule, a formal specification, or a concrete artifact that the system can verify against.

### 3.4.1 The Meeting-to-Artifact Pipeline

The transformation from meeting to testable artifact follows a structured pipeline:

```
Meeting (unstructured)
    -> Transcript (structured text)
    -> Extraction (decisions, requirements, constraints)
    -> Formalization (executable specifications)
    -> Artifact (test, model, validator)
    -> Verification (continuous checking)
```

Each stage reduces ambiguity and increases verifiability. The key insight is that **the artifact is not documentation of the meeting—it IS the meeting, crystallized into a form that admits verification**.

#### Stage 1: Capture

Meetings must produce complete, machine-readable transcripts. Audio recordings converted to text are the minimum acceptable baseline. Video recordings with speaker diarization are better. Real-time transcription services that produce timestamped, speaker-attributed transcripts enable precise traceability from artifact back to the exact moment of discussion.

**Essential capture elements:**
- Participant identification (who said what)
- Timestamping (when it was said)
- Topic markers (what was being discussed)
- Decision markers (explicit agreement moments)
- Action items (who will do what by when)

Without these elements, artifacts cannot be traced to their sources, and accountability becomes impossible.

#### Stage 2: Extraction

From the transcript, structured elements must be extracted:

| Element Type | Examples | Artifact Form |
|--------------|----------|---------------|
| Requirements | "The system must handle 10k requests/sec" | Performance test, load specification |
| Constraints | "Payment processing must be PCI compliant" | Security scan, compliance test |
| Decisions | "We'll use PostgreSQL for the main database" | Infrastructure-as-code, dependency configuration |
| Assumptions | "Users will have modern browsers" | Browser compatibility test matrix |
| Acceptance Criteria | "A workflow completes within 5 seconds" | Integration test, SLA monitor |

Each extracted element receives a unique identifier and is linked back to its source timestamp in the transcript. This bidirectional traceability—artifact to source, source to artifact—is fundamental to admissibility.

#### Stage 3: Formalization

Extracted elements are transformed into executable specifications. The choice of formalization depends on the element type:

**Requirements become tests:**
```erlang
%% From meeting: "The system must handle 10k requests/sec"
%% Artifact: performance test
handle_throughput_test_() ->
    {timeout, 60, fun() ->
        TargetRate = 10000,
        StartTime = erlang:timestamp(),
        % simulate TargetRate requests
        % verify all complete within SLA
        EndTime = erlang:timestamp(),
        ActualRate = calculate_rate(StartTime, EndTime),
        ?assert(ActualRate >= TargetRate)
    end}.
```

**Constraints become validators:**
```erlang
%% From meeting: "Payment data must never be logged"
%% Artifact: compile-time constraint
-compile({nowarn_unused_function, [{validate_payment_logging, 0}]}).

validate_payment_logging() ->
    %% This will fail at compile time if payment logging is detected
    Source = read_source_files(),
    case contains_payment_logging(Source) of
        true -> error(payment_logging_detected);
        false -> ok
    end.
```

**Decisions become configuration:**
```yaml
# From meeting: "Use PostgreSQL for main database"
# Artifact: infrastructure configuration
database:
  primary:
    engine: postgresql
    version: "15"
    region: us-east-1
    decided_at: "2025-02-07T14:23:00Z"
    decided_by: [alice, bob, carol]
    meeting_recording: "mtg-2025-02-07-raw.mp4"
    timestamp: "1:23:45"
```

#### Stage 4: Verification

Once artifacts exist, they must be continuously verified:

1. **Continuous Integration** - Every commit runs all tests derived from meetings
2. **Invariant Checking** - Static analysis verifies constraints are never violated
3. **Traceability Audits** - Regular verification that every artifact maps to a meeting source
4. **Drift Detection** - Automated comparison between stated requirements and actual system behavior

### 3.4.2 Concrete Workflow: The Requirements Meeting

Consider a concrete example: a requirements review meeting for a workflow management system. Five stakeholders discuss performance, security, and integration requirements.

**Before the meeting:**
```yaml
meeting_preparation:
  title: "Order Fulfillment Performance Requirements"
  date: "2025-02-07T10:00:00Z"
  attendees:
    - role: Product Manager
      name: Alice Chen
    - role: Technical Lead
      name: Bob Smith
    - role: Security Architect
      name: Carol Davis
    - role: QA Engineer
      name: Dan Lee
    - role: System Operator
      name: Eve Martinez

  agenda:
    - item: "Review current performance baselines"
      duration: 15min
    - item: "Define production SLA requirements"
      duration: 20min
    - item: "Identify security constraints for payment handling"
      duration: 25min
    - item: "Assign action items and artifacts"
      duration: 10min

  expected_artifacts:
    - type: performance_test
      description: "Load test for 100 concurrent workflows"
    - type: security_constraint
      description: "PCI compliance validation"
    - type: integration_test
      description: "Payment gateway timeout handling"
```

**During the meeting:**
- Real-time transcription captures all discussion
- Participants mark decision moments explicitly ("DECISION: We require 100 concurrent workflow support")
- Facilitator notes implicit agreements as they emerge ("So we're agreeing to 5-second timeout on payment gateway?")
- Timestamps link each artifact to specific discussion moments

**After the meeting (within 24 hours):**

**Artifact 1: Performance Test**
```erlang
%% Generated from meeting decision at 10:23:45
%% Source: meeting-20250207-transcript.json lines 234-256
%% Decision makers: Alice, Bob, Dan
%% Rationale: "Production peak load observed at 85 concurrent"

-module(mtg_20250207_performance).
-export([concurrent_workflow_load_test/0]).

concurrent_workflow_load_test() ->
    %% Requirement: System must handle 100 concurrent workflows
    %% Source: Meeting 2025-02-07, timestamp 10:23:45
    ConcurrentTarget = 100,
    MaxLatencyMs = 2000,

    {ok, Pid} = yawl_test_sup:start_load_test(ConcurrentTarget),

    %% Verify all workflows start successfully
    ?assertEqual(ConcurrentTarget,
                 yawl_test_sup:count_started(Pid)),

    %% Verify all complete within latency budget
    ?assert(lists:all(fun(L) -> L =< MaxLatencyMs end,
                      yawl_test_sup:get_latencies(Pid))),

    %% Verify no errors
    ?assertEqual(0, yawl_test_sup:error_count(Pid)).
```

**Artifact 2: Security Constraint**
```erlang
%% Generated from meeting requirement at 10:47:12
%% Source: meeting-20250207-transcript.json lines 412-438
%% Requirement: "Payment card data must never appear in logs"

-module(mtg_20250207_security).
-export([validate_log_safety/0]).

-compile({nowarn_unused_function, [{validate_log_safety, 0}]}).

validate_log_safety() ->
    %% Parse all log format strings in the codebase
    LogFormats = extract_log_formats(),

    %% Check for sensitive data patterns
    SensitivePatterns = [
        <<"card_number">>,
        <<"cvv">>,
        <<"credit_card">>,
        <<"cc_num">>,
        <<"pan">>  %% Primary Account Number
    ],

    Violations = lists:filter(
        fun(Format) ->
            lists:any(fun(P) -> binary:match(Format, P) =/= nomatch end,
                     SensitivePatterns)
        end,
        LogFormats),

    case Violations of
        [] -> ok;
        _ -> error({sensitive_data_in_logs, Violations})
    end.
```

**Artifact 3: Traceability Matrix**
```yaml
artifact_traceability:
  meeting_id: "mtg-20250207"
  recording: "recordings/mtg-20250207-raw.mp4"
  transcript: "artifacts/mtg-20250207-transcript.json"

  artifacts:
    - id: "ART-2025-02-07-001"
      type: performance_test
      module: mtg_20250207_performance
      function: concurrent_workflow_load_test/0
      source_timestamp: "10:23:45"
      source_lines: [234, 256]
      decision_makers: [alice, bob, dan]
      rationale: "Production peak load requires headroom"
      verification: "rebar3 ct --suite mtg_20250207_performance"

    - id: "ART-2025-02-07-002"
      type: security_constraint
      module: mtg_20250207_security
      function: validate_log_safety/0
      source_timestamp: "10:47:12"
      source_lines: [412, 438]
      decision_makers: [alice, carol]
      rationale: "PCI DSS requirement 3.1"
      verification: "rebar3 compile"
```

### 3.4.3 The Retrospective Artifact Meeting

A powerful pattern is the **Retrospective Artifact Meeting (RAM)**: a dedicated session to review, update, and validate all artifacts derived from previous meetings. In a RAM:

1. **Artifact Review**: Each artifact is examined for continued relevance
2. **Drift Detection**: Current system behavior is compared to artifact specifications
3. **Gap Analysis**: Missing artifacts are identified and backfilled
4. **Stakeholder Re-alignment**: Participants re-commit to artifact-based development

**RAM Agenda Template:**
```markdown
# Retrospective Artifact Meeting

## Preparation (Pre-Meeting)
- [ ] Generate artifact inventory from repository
- [ ] Run all artifact-based tests; capture failures
- [ ] Identify artifacts with no passing tests (ghost artifacts)
- [ ] Identify test coverage gaps (requirements without artifacts)

## Session (60 Minutes)

### Artifact Inventory Review (15 min)
- Total artifacts: ___
- Passing: ___ (___%)
- Failing: ___ (___%)
- Ghost artifacts: ___
- New artifacts needed: ___

### Failure Deep-Dive (20 min)
For each failing artifact:
- [ ] Determine root cause (artifact wrong or system wrong?)
- [ ] If artifact wrong: update or retire artifact
- [ ] If system wrong: create defect ticket
- [ ] Assign owner and due date

### Gap Analysis (15 min)
- [ ] List untested requirements discovered since last RAM
- [ ] Prioritize by risk/impact
- [ ] Assign artifact creation tasks

### Process Improvement (10 min)
- [ ] What meeting patterns generated good artifacts?
- [ ] What patterns generated drift?
- [ ] Process adjustments for next cycle

## Outputs
- Updated artifact inventory
- Defect tickets for system fixes
- Artifact creation tasks for gaps
- Process improvement proposals
```

### 3.4.4 Meeting Template for Artifact Generation

To reliably generate artifacts from meetings, use a structured template:

```markdown
# Meeting: [Title]

## Metadata
- **Date**: YYYY-MM-DD
- **Time**: HH:MM - HH:MM (Timezone)
- **Location/Link**: [URL]
- **Recording**: [Link to recording]
- **Transcript**: [Link to generated transcript]

## Participants
| Name | Role | Email | Artifacts Owned |
|------|------|-------|-----------------|
| [Name] | [Role] | [Email] | [List] |

## Pre-Meeting Preparation
- [ ] Reading material distributed: [Date]
- [ ] Artifact review completed: [Yes/No]
- [ ] Questions submitted: [List]

## Agenda
| Time | Topic | Expected Artifact | Owner |
|------|-------|-------------------|-------|
| 0:00-0:15 | [Topic] | [Type] | [Name] |
| 0:15-0:30 | [Topic] | [Type] | [Name] |

## Discussion Log
*(Auto-generated from transcript with manual cleanup)*

### [Time] - [Topic]
**Participants**: [Names]
**Discussion**: [Summary]

**DECISION**: [Clear statement]
- **Artifact Type**: [test|constraint|config|spec]
- **Priority**: [high|medium|low]
- **Owner**: [Name]
- **Due Date**: [YYYY-MM-DD]

### [Time] - [Topic]
**ASSUMPTION**: [Statement]
- **Validation Method**: [How to verify]
- **Risk if Wrong**: [Impact assessment]

## Artifacts Generated
| ID | Type | Title | Owner | Status | Verification |
|----|------|-------|-------|--------|--------------|
| ART-YYYY-MM-DD-001 | [Type] | [Title] | [Name] | [draft|ready|verified] | [Command] |

## Action Items
| Task | Owner | Due Date | Blocked By |
|------|-------|----------|------------|
| [Description] | [Name] | [Date] | [ART-XXX] |

## Next Meeting
- **Date**: YYYY-MM-DD
- **Purpose**: [Review artifacts from this meeting]
```

### 3.4.5 Anti-Pattern Prevention Checklist

Before ending any meeting, verify:

- [ ] Every decision has an assigned artifact type
- [ ] Every requirement has a test specification
- [ ] Every constraint has a validation method
- [ ] Every artifact has an owner and due date
- [ ] Every participant understands their artifact responsibilities
- [ ] Recording is saved and backed up
- [ ] Transcript is initiated or scheduled
- [ ] Next meeting is scheduled with artifact review agenda
- [ ] Artifacts are committed to version control before leaving the room

---

## 3.5 Anti-Patterns That Cause Drift

Drift in constructive systems is not accidental—it is the predictable consequence of specific anti-patterns in how information flows from human intent to machine implementation. Each anti-pattern represents a systematic failure to transform decisions into verifiable artifacts. Understanding these patterns is essential because **drift is the enemy of admissible systems**: once behavior diverges from specification, the system can no longer guarantee that verification corresponds to reality.

### 3.5.1 The Verbal Handoff

**Pattern**: Requirements are communicated verbally in meetings with no written record, no transcript, and no artifact generation.

**Example**:
```
[Meeting room conversation]
Alice (Product Manager): "The payment workflow needs to retry if the gateway times out."
Bob (Developer): "Sure, I'll add a retry."
[Meeting ends, no ticket filed, no spec written]

[Three weeks later]
System is deployed with no payment retry.
Alice: "I thought we agreed to retry payments?"
Bob: "We discussed it, but there was no ticket, so I focused on the assigned stories."
```

**How it breaks admissibility**:
- No artifact exists to specify the required behavior
- No test can be written to verify the requirement
- The system's behavior becomes whatever was implemented, not what was discussed
- Traceability is impossible—there is no source to trace back to

**Remediation**:
1. **Never accept verbal requirements**: Every decision generates an artifact immediately
2. **Real-time artifact creation**: During the meeting, write the test spec for each requirement
3. **Artifact review before adjournment**: Meeting does not end until all decisions have corresponding artifacts
4. **Verbal handoff template**: When someone must verbally communicate a requirement, use a structured template that requires artifact creation

```erlang
%% The Anti-Pattern: Verbal requirement becomes code
%% (This is what happens - a direct jump from conversation to implementation)

%% No spec, no test, just implementation based on memory
handle_payment_timeout(Payment) ->
    %% Developer remembers something about retrying...
    %% But how many times? What backoff? What final error?
    case payment_gateway:process(Payment) of
        {error, timeout} ->
            %% Wait, was I supposed to retry? Log and fail?
            error_logger:error_msg("Payment timeout: ~p", [Payment]),
            {error, payment_failed}
    end.

%% The Correct Pattern: Meeting generates artifact, artifact drives implementation

%% Artifact from meeting: test specification
-module(mtg_payment_timeout).
-include_lib("eunit/include/eunit.hrl").

%% From meeting 2025-02-07 14:23: "Retry payment 3 times with exponential backoff"
payment_retry_test_() ->
    {"Payment retries 3 times on timeout with exponential backoff",
     fun() ->
         Payment = test_payment(),

         %% First attempt times out
         meck:expect(payment_gateway, process, fun(_) -> {error, timeout} end),

         %% Should retry 3 times total
         {error, {payment_timeout, _}} = payment_handler:process(Payment),

         %% Verify exponential backoff (1s, 2s, 4s)
         ?assertEqual([1000, 2000, 4000], meck:history(payment_gateway, backoff_intervals))
     end}.

%% Implementation now has explicit contract to satisfy
handle_payment_timeout(Payment, RetriesRemaining) when RetriesRemaining > 0 ->
    BackoffMs = calculate_backoff(4 - RetriesRemaining),  %% Exponential
     timer:sleep(BackoffMs),
    case payment_gateway:process(Payment) of
        {error, timeout} -> handle_payment_timeout(Payment, RetriesRemaining - 1);
        {error, Reason} -> {error, Reason};
        {ok, Result} -> {ok, Result}
    end;
handle_payment_timeout(_Payment, 0) ->
    {error, {payment_timeout, max_retries_exceeded}}.
```

### 3.5.2 The Living Document

**Pattern**: A document that claims to be the "source of truth" but is continuously updated without version control, without test generation, and without stakeholder review of changes.

**Example**:
```
requirements.txt (last modified: 6 months ago by a departed employee)
- The system must support 100 concurrent users
- [Line crossed out with pen] 500 concurrent users
- [Handwritten note in margin] Actually 1000 after marketing campaign
- [Another handwritten note] But only for read operations
```

**How it breaks admissibility**:
- The document cannot be executed or verified
- Changes are not traceable to decisions or timestamps
- No tests guard against violating "living" requirements
- The document becomes a fossil—referenced but never actually synchronized with reality

**Remediation**:
1. **Documents are not artifacts**: Require executable specifications, not descriptive documents
2. **Version-controlled change**: Every requirement change is a commit with attribution
3. **Test generation**: Every requirement change generates or updates tests
4. **Obsolescence detection**: Automated detection when tests haven't been run against recent commits

```yaml
# Anti-Pattern: Living document with no executable meaning
# File: requirements/README.md (last updated: unknown)

## Performance Requirements
- System should handle "significant" concurrent load
- Response times should be "fast"
- Database queries should be "optimized"

# Correct Pattern: Executable specification
# File: artifacts/performance_requirements.yaml

performance_requirements:
  - id: PERF-001
    source_meeting: "mtg-2025-02-07-performance"
    source_timestamp: "10:23:45"
    last_reviewed: "2025-02-07T10:23:45Z"
    reviewed_by: [alice, bob, dan]

    specification:
      metric: concurrent_users
      target: 1000
      measurement_window: 60s
      success_criteria: "99th percentile latency < 100ms"

    verification:
      test_module: perf_concurrent_users
      test_function: concurrent_load_test_0
      command: "rebar3 ct --suite perf_concurrent_users"
      pass_threshold: 99

    history:
      - date: "2025-01-15"
        target: 100
        changed_by: alice
        reason: "Initial baseline"
      - date: "2025-02-01"
        target: 500
        changed_by: bob
        reason: "Marketing scale-up preparation"
      - date: "2025-02-07"
        target: 1000
        changed_by: dan
        reason: "Capacity planning completed"
```

### 3.5.3 The Implicit Assumption

**Pattern**: Critical behavior is implemented based on assumptions that were never stated, never verified, and never tested.

**Example**:
```
%% Developer assumes that workflow IDs are unique across all partitions
get_workflow_state(WorkflowId) ->
    case ets:lookup(workflow_table, WorkflowId) of
        [{WorkflowId, State}] -> State;
        [] -> {error, not_found}
    end.

%% Assumption violated in production: same ID generated in different partitions
%% Result: workflows silently return each other's state
```

**How it breaks admissibility**:
- The system satisfies tests that reflect the assumption, not reality
- When the assumption is violated, failure occurs outside tested scenarios
- No artifact exists to make the assumption explicit and verifiable

**Remediation**:
1. **Assumption harvesting**: Explicit extraction of assumptions during meetings
2. **Assumption artifacts**: Each assumption becomes a testable invariant
3. **Assumption validation**: Runtime checks that verify assumptions hold
4. **Assumption review**: Regular re-evaluation of whether assumptions remain valid

```erlang
%% Anti-Pattern: Implicit assumption in code
%% (No documentation, no test, just assumption)

handle_workflow_event(WorkflowId, Event) ->
    %% Assumption: WorkflowId is a globally unique binary
    %% No check, no validation, just assumes
    {ok, Pid} = workflow_registry:lookup(WorkflowId),
    gen_server:cast(Pid, {event, Event}).

%% Correct Pattern: Assumption made explicit and verified

%% Artifact: Assumption specification from meeting
-module(assumptions_workflow_id).

-type workflow_id() :: {partition:non_neg_integer(), unique:binary()}.
-type assumption_result() :: valid | {invalid, reason()}.

%% From meeting 2025-02-07: "Workflow IDs must be unique across partitions"
%% This assumption is encoded as a runtime check
-spec validate_workflow_id_assumption(workflow_id()) -> assumption_result().
validate_workflow_id_assumption({Partition, UniqueBin}) ->
    %% Assumption 1: Partition is within known range
    case Partition >= 0 andalso Partition < partition_count() of
        false -> {invalid, {partition_out_of_range, Partition}};
        true ->
            %% Assumption 2: UniqueBin is actually unique
            case global_registry:is_registered(UniqueBin) of
                true -> {invalid, {duplicate_id, UniqueBin}};
                false -> valid
            end
    end.

%% Runtime enforcement
handle_workflow_event(WorkflowId, Event) ->
    case validate_workflow_id_assumption(WorkflowId) of
        valid ->
            {ok, Pid} = workflow_registry:lookup(WorkflowId),
            gen_server:cast(Pid, {event, Event});
        {invalid, Reason} ->
            %% Fail fast rather than silently violate assumption
            error({workflow_id_assumption_violated, Reason, WorkflowId})
    end.

%% Test that verifies assumption enforcement
workflow_id_uniqueness_test_() ->
    fun() ->
        %% Create ID in partition 0
        Id1 = {0, <<"workflow-abc">>},
        ok = workflow_registry:register(Id1, self()),

        %% Attempt to create same ID in partition 1
        Id2 = {1, <<"workflow-abc">>},

        %% Should fail assumption validation
        ?assertEqual({invalid, {duplicate_id, <<"workflow-abc">>}},
                     validate_workflow_id_assumption(Id2))
    end.
```

### 3.5.4 The Temporary Solution

**Pattern**: A quick fix is deployed with the intent to "do it properly later," but the temporary solution becomes permanent and accumulates untested, undocumented behavior.

**Example**:
```
%% Comment says "TODO: Fix properly"
%% Code has been in production for 2 years
handle_payment_error(Payment, Error) ->
    %% TEMPORARY: Just retry until it works
    %% TODO: Implement proper circuit breaker (ticket #234)
    case payment_gateway:process(Payment) of
        {ok, Result} -> Result;
        {error, _} -> handle_payment_error(Payment, Error)  %% Infinite loop!
    end.
```

**How it breaks admissibility**:
- Temporary solutions rarely have tests ("it's just temporary")
- When they become permanent, they represent unverified behavior
- The TODO ticket is never addressed because the code "works"
- Each temporary solution increases technical debt without visibility

**Remediation**:
1. **Expiration dates**: Temporary solutions must have explicit expiration dates
2. **Test debt**: Temporary solutions require comprehensive tests (precisely because they're temporary)
3. **Tech debt tracking**: Automatic flagging of expired temporary solutions
4. **No-default permanence**: Temporary solutions fail after expiration unless explicitly renewed

```erlang
%% Anti-Pattern: Temporary solution with no enforcement

%% Code comment: "TODO: Fix properly - expires 2024-06-01"
%% (Today is 2025-02-07, code still running)
handle_payment_error(Payment, Error) ->
    %% TEMPORARY: Just retry until it works
    case payment_gateway:process(Payment) of
        {ok, Result} -> Result;
        {error, _} -> handle_payment_error(Payment, Error)
    end.

%% Correct Pattern: Self-enforcing temporary solution

-module(temp_payment_handler).
-export([handle_payment_error/2]).

%% Temporary solution with enforced expiration
-define(TEMPORARY_SOLUTION_EXPIRES,
        {{2025, 3, 1}, {0, 0, 0}}).  %% March 1, 2025

-define(TEMPORARY_TICKET, "DEBT-234").
-define(TEMPORARY_OWNER, "bob@example.com").

%% Compile-time warning if expired
-compile({'inline', [{check_expiration, 0}]}).

-spec handle_payment_error(payment:error(), any()) -> {ok, any()} | no_return().
handle_payment_error(Payment, Error) ->
    check_expiration(),
    do_temporary_retry(Payment, Error).

check_expiration() ->
    case calendar:local_time() > ?TEMPORARY_SOLUTION_EXPIRES of
        true ->
            error_logger:error_msg(
                "TEMPORARY SOLUTION EXPIRED: ~s expires ~w~n"
                "Owner: ~s. Replace with proper implementation.",
                [?MODULE, ?TEMPORARY_SOLUTION_EXPIRES, ?TEMPORARY_OWNER]
            ),
            error({temporary_solution_expired,
                   ?MODULE,
                   ?TEMPORARY_TICKET,
                   ?TEMPORARY_SOLUTION_EXPIRES});
        false ->
            ok
    end.

do_temporary_retry(Payment, Error) ->
    %% TEMPORARY implementation with clear contract
    %% This MUST be replaced by ?TEMPORARY_SOLUTION_EXPIRES
    Retries = 3,
    lists:foreach(fun(_) ->
        case payment_gateway:process(Payment) of
            {ok, Result} -> throw({ok, Result});
            {error, _} -> continue
        end
    end, lists:seq(1, Retries)),
    {error, {temporary_retry_exceeded, Error}}.

%% Test that tracks temporary solution status
temporary_solution_not_expired_test_() ->
    {timeout, 10, fun() ->
        CurrentDate = calendar:local_time(),
        ?assert(CurrentDate < ?TEMPORARY_SOLUTION_EXPIRES,
               "Temporary solution has expired! "
               "Replace " ++ atom_to_list(?MODULE))
    end}.
```

### 3.5.5 The Orphaned Decision

**Pattern**: A decision is made and recorded, but the person who made it leaves the team or the context is lost, and the decision persists without anyone understanding its rationale or validity.

**Example**:
```
git blame src/payment_handler.erl
Line 45:      MaxRetries = 7,  %% Why 7? No comment
    (committed 3 years ago by sarah@example.com, who left the company)

Current team: "We think it should be 3, but we're afraid to change it because we don't know why it's 7."
```

**How it breaks admissibility**:
- Decisions without rationale cannot be re-evaluated
- Fear of changing unknown constraints causes stagnation
- The system accumulates "sacred numbers" that nobody understands
- No artifact links the decision to its original justification

**Remediation**:
1. **Decision artifacts**: Every non-trivial decision gets a structured artifact with rationale
2. **Context binding**: Decisions are linked to meeting transcripts and discussion context
3. **Re-evaluation schedule**: Decisions have explicit review dates
4. **Origin tagging**: Every decision carries author, date, and context forever

```yaml
# Anti-Pattern: Orphaned decision in code comment
# MaxRetries = 7  %% (Why?)

# Correct Pattern: Decision artifact with full traceability

decision:
  id: "DEC-2025-02-07-001"
  title: "Payment retry count configuration"

  decision:
    parameter: max_retries
    value: 7
    type: integer
    scope: payment_handler

  rationale:
    source_meeting: "mtg-2025-02-07-payment-reliability"
    source_timestamp: "14:23:00"
    source_recording: "recordings/mtg-2025-02-07.mp4"

    reasoning: |
      Analysis of production logs from 2025-01-01 to 2025-01-31 showed:
      - 95% of failed payments succeeded within 3 retries
      - 99% succeeded within 5 retries
      - Additional 2 retries (total 7) captured remaining 0.5%
      - Cost of extra retries: $0.002 per transaction
      - Cost of failed transaction: $15 in support overhead
      Conclusion: 7 retries optimizes total cost of ownership

    alternatives_considered:
      - value: 3
        rejected: "Leaves 5% of recoverable failures unrecovered"
      - value: 10
        rejected: "Diminishing returns; 8+ retries < 0.01% improvement"

  provenance:
    decided_by: [sarah, bob, carol]
    decision_date: "2025-02-07"
    approving_role: "Technical Lead"
    implementation_ticket: "PAY-1234"

  validity:
    review_schedule: "quarterly"
    next_review: "2025-05-07"
    invalidation_conditions:
      - "Payment gateway reliability exceeds 99.9%"
      - "Retry cost exceeds $0.01 per transaction"
      - "New gateway provider selected"

  code_reference:
    module: payment_handler
    line: 45
    version_control_link: "https://github.com/.../src/payment_handler.erl#L45"
```

### 3.5.6 The Happy Path Only

**Pattern**: Tests and specifications focus only on success scenarios, leaving error handling, edge cases, and failure modes unspecified and untested.

**Example**:
```erlang
%% Test: Only happy path
successful_payment_test() ->
    Payment = test_payment(),
    {ok, Result} = payment_handler:process(Payment),
    ?assertEqual(success, Result.status).

%% Reality: Production fails on every error path
%% No tests for: timeout, invalid card, network error, gateway down, etc.
```

**How it breaks admissibility**:
- The system appears verified (tests pass) but only for a narrow subset of reality
- Failure modes are discovered in production, not in testing
- Error handling code paths accumulate without specification
- The system cannot claim to be "admissible" when most behaviors are unverified

**Remediation**:
1. **Failure mode analysis**: Every specification must enumerate possible failures
2. **Error path tests**: For every success test, at least one error test
3. **Chaos testing**: Systematic verification of failure scenarios
4. **Coverage enforcement**: Test coverage requirements include error paths

```erlang
%% Anti-Pattern: Happy path only

successful_workflow_test() ->
    {ok, Pid} = workflow:start(test_spec()),
    ?assertEqual(running, workflow:status(Pid)).

%% Correct Pattern: Comprehensive coverage including failures

-module(workflow_test_suite).

%% Success paths
successful_workflow_test() ->
    {ok, Pid} = workflow:start(test_spec()),
    ?assertEqual(running, workflow:status(Pid)).

%% Failure path 1: Invalid specification
invalid_spec_test() ->
    BadSpec = #{workflow => unknown_type},
    ?assertEqual({error, invalid_specification},
                 workflow:start(BadSpec)).

%% Failure path 2: Resource exhaustion
resource_exhaustion_test() ->
    meck:expect(resource_pool, acquire, fun() -> {error, exhausted} end),
    {ok, Pid} = workflow:start(test_spec()),
    ?assertEqual({error, resource_exhausted},
                 workflow:execute_step(Pid, resource_intensive)).

%% Failure path 3: Timeout
timeout_test() ->
    SlowSpec = #{steps => [#{action => slow_op, timeout => 100}]},
    meck:expect(slow_op, execute, fun(_) -> timer:sleep(1000), ok end),
    {ok, Pid} = workflow:start(SlowSpec),
    ?assertEqual({error, timeout}, workflow:execute(Pid)).

%% Failure path 4: Partial failure with rollback
partial_failure_test() ->
    Spec = #{
        steps => [
            #{action => step1, compensating => undo1},
            #{action => step2, compensating => undo2},
            #{action => step3, compensating => undo3}
        ]
    },
    meck:expect(step2, execute, fun(_) -> {error, step2_failed} end),
    {ok, Pid} = workflow:start(Spec),
    ?assertEqual({error, {step2_failed, compensated}}, workflow:execute(Pid)),
    ?assert(meck:called(undo1, execute, '_')),
    ?assertNot(meck:called(undo3, execute, '_')).

%% Coverage verification
ensure_error_path_coverage_test() ->
    SuccessTests = [T || {T, _} <- ?MODULE:module_info(exports),
                         string:prefix(atom_to_list(T), "successful_")],
    ErrorTests = [T || {T, _} <- ?MODULE:module_info(exports),
                       string:prefix(atom_to_list(T), "invalid_");
                       string:prefix(atom_to_list(T), "timeout_");
                       string:prefix(atom_to_list(T), "resource_");
                       string:prefix(atom_to_list(T), "partial_")],
    ?assert(length(ErrorTests) >= length(SuccessTests),
           "Error path coverage must be at least equal to success path coverage").
```

### 3.5.7 Summary: Anti-Pattern Detection Matrix

| Anti-Pattern | Detection Signal | Severity | Remediation Priority |
|--------------|------------------|----------|---------------------|
| Verbal Handoff | No artifact linked to requirement | Critical | Immediate |
| Living Document | Document != tests, uncommitted changes | High | High |
| Implicit Assumption | Code behavior != documented contracts | Critical | Immediate |
| Temporary Solution | TODO comments older than sprint | High | High |
| Orphaned Decision | git blame shows departed authors | Medium | Medium |
| Happy Path Only | Error test coverage < success test coverage | Critical | Immediate |

**Detection Query** (run weekly):
```bash
# Find TODO comments older than 2 weeks
find src -name "*.erl" -exec grep -l "TODO\|FIXME\|XXX" {} \; | \
  xargs git blame | \
  grep "TODO\|FIXME\|XXX" | \
  awk '$3 < "'$(date -d '2 weeks ago' +%Y-%m-%d)'" {print}'

# Find modules with only happy path tests
for module in src/*.erl; do
  success=$(grep -c "_test()" $module || echo 0)
  error=$(grep -E "(error|fail|timeout|invalid)_test()" $module | wc -l)
  if [ $error -lt $success ]; then
    echo "$module: success=$success, error=$error"
  fi
done

# Find orphaned decisions (numeric constants without rationale)
grep -n "MaxRetries\|Timeout\|BufferSize" src/*.erl | \
  grep -v "%%.*why\|%%.*rationale\|%%.*DECISION"
```

---

### Chapter 3 Key Takeaways

1. **Meetings are generative events**: Every decision, requirement, and constraint from a meeting must be transformed into a testable artifact.

2. **Traceability is bidirectional**: Artifacts must link to their source (meeting transcripts, timestamps), and sources must link to their artifacts.

3. **Drift is systematic, not accidental**: Specific anti-patterns—verbal handoffs, living documents, implicit assumptions—systematically create drift.

4. **Anti-patterns are detectable and preventable**: Each anti-pattern has clear signals and specific remediation strategies.

5. **Verification is continuous**: Artifacts are not one-time deliverables but continuously verified guards against drift.

6. **Expiration prevents permanence**: Temporary solutions and decisions must have explicit expiration dates to prevent unexamined persistence.

7. **Failure paths are requirements**: A system is not admissible if only success cases are verified.

The constructive approach demands that we treat information flow as a rigorously managed pipeline: from human conversation to structured extraction, to formal specification, to executable artifact, to continuous verification. Any break in this pipeline creates drift, and drift destroys admissibility.
