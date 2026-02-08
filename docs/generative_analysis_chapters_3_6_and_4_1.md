# Chapter 3.6: Publishing and Versioning Analysis Artifacts

## 3.6.1 The Challenge of Analysis Artifact Drift

Analysis artifacts occupy a peculiar space in constructive systems. They are not source code, yet they define system behavior. They are not documentation, yet they specify requirements. They are not tests, yet they validate assumptions. This liminal status creates a unique problem: analysis artifacts drift.

Consider a typical scenario. An architect creates a sequence diagram showing how a payment service interacts with a fraud detection system. Three months later, a developer implements the integration. Six months after that, another developer refactors the payment service. The original diagram remains in the repository, unchanged, now misleading. The artifact has drifted from reality.

This drift is not merely an inconvenience—it is a source of bugs. When new team members consult outdated diagrams, they form incorrect mental models. When architects design extensions based on obsolete specifications, they introduce inconsistencies. The system accumulates technical debt invisible to those who rely on the artifacts.

**Generative analysis** addresses this problem by treating analysis artifacts as versioned, publishable entities with first-class status in the development lifecycle. An artifact is not a static document but a living component that evolves alongside the system it describes.

## 3.6.2 Artifacts as Versioned Publications

We draw an analogy to academic publishing. A research paper has a version history: preprint, submitted revision, published version, possibly errata. Each version is uniquely identifiable and immutable. Readers know exactly which version they are citing. Authors can track changes between versions.

Analysis artifacts deserve the same treatment. When we publish an artifact—a component diagram, a protocol specification, a state machine definition—we assign it a version identifier. That version never changes. If we need to update the artifact, we publish a new version with a new identifier.

### Semantic Versioning for Analysis Artifacts

We adapt semantic versioning (SemVer) to analysis artifacts:

- **MAJOR**: Breaking change to the artifact's semantics. Consumers of the artifact must update their understanding or implementation.
- **MINOR**: Backward-compatible addition. New elements are added, but existing elements remain valid.
- **PATCH**: Correction of errors without semantic change. Typographical fixes, clarification of ambiguous language, improved formatting.

For example, a state machine specification for an order fulfillment workflow might evolve:

- `v1.0.0`: Initial specification with 5 states
- `v1.1.0`: Added `CANCEL_PENDING` state (backward-compatible extension)
- `v1.1.1`: Fixed typo in transition condition (patch)
- `v2.0.0`: Refactored state machine with `PAYMENT_AUTHORIZED` split into two distinct states (breaking change)

This versioning scheme serves two purposes. First, it communicates the nature of changes to artifact consumers. Second, it enables precise referencing: other artifacts can specify dependency on `order_fsm >= 1.1.0 && < 2.0.0`.

### Immutable Artifacts and Derivable Artifacts

We distinguish between two categories of analysis artifacts:

**Immutable artifacts** are those that change only by explicit publication of new versions. Examples include:
- Formal specifications (state machines, protocol definitions)
- Analysis conclusions (bottleneck findings, risk assessments)
- Design decisions (architecture choices with rationale)

**Derivable artifacts** are those that should be regenerated from source materials. Examples include:
- Visualizations (diagrams generated from specifications)
- Reports (test results, coverage metrics)
- Documentation (API references extracted from code)

The key insight: derivable artifacts should not be manually edited. They are the output of a generative process from immutable sources. When the source changes, the derived artifact is regenerated.

This distinction prevents a common anti-pattern: manually editing a generated diagram, then having those edits lost when the diagram is regenerated. By establishing a clear boundary between immutable sources and derivable outputs, we maintain artifact integrity.

## 3.6.3 Publication Workflows

How does an artifact become "published"? We propose three workflow patterns, each appropriate for different contexts.

### The Lightweight Pattern: Git-Based Versioning

For teams already using Git, the simplest approach is to version artifacts alongside code:

```
analysis/
  components/
    payment-service/
      payment_fsm.yaml      v2.1.0
      payment_api.yaml      v1.4.0
  diagrams/
    payment-sequence.seq    generated
    payment-states.dot      generated
```

Artifacts are YAML files (or other structured formats) with explicit version fields. Git commits provide change history. Git tags mark published versions.

**Advantages**: No additional infrastructure; familiar workflow; free distributed version control.

**Disadvantages**: Version identification tied to repository; less explicit about publication status; harder to discover artifacts across repositories.

### The Structured Pattern: Artifact Registry

For organizations with many artifacts across multiple repositories, a dedicated artifact registry provides centralized management:

```
registry.example.com/artifacts/payment-fsm/v2.1.0
registry.example.com/artifacts/payment-api/v1.4.0
registry.example.com/artifacts/order-workflow/v3.0.0
```

The registry stores artifact metadata (version, dependencies, author, publication date) and content. APIs support publishing new versions, retrieving by version, and querying dependencies.

**Advantages**: Centralized discovery; explicit publication lifecycle; dependency management; access control.

**Disadvantages**: Additional infrastructure to maintain; more complex than Git-only approach.

### The Formal Pattern: Content-Addressable Storage

For environments requiring strong integrity guarantees, content-addressable storage (CAS) provides immutable artifact storage:

```
ipfs://QmXyZ.../payment-fsem-v2.1.0.yaml
ipfs://QmAbC.../payment-api-v1.4.0.yaml
```

Each artifact is addressed by its cryptographic hash. Version identifiers derive from hash sequences (like Git commits). The artifact content cannot change without changing its address.

**Advantages**: Strong integrity guarantees; tamper-evidence; natural deduplication; distributed caching.

**Disadvantages**: Requires CAS infrastructure; less discoverable than registry; unfamiliar to many developers.

## 3.6.4 Dependency Management Between Artifacts

Artifacts often depend on other artifacts. A state machine specification might reference a data type definition. A protocol specification might extend a base protocol. These dependencies must be tracked and versioned.

We propose a simple dependency declaration format, embedded in the artifact:

```yaml
artifact:
  name: payment-fsm
  version: 2.1.0
  dependencies:
    - name: payment-types
      version: ">= 1.3.0, < 2.0.0"
    - name: common-states
      version: ">= 1.0.0"
```

When publishing an artifact, the publishing system validates that all dependencies exist and satisfy version constraints. This prevents broken references and enables dependency analysis.

### Transitive Dependencies and Conflict Resolution

Like software packages, analysis artifacts can have transitive dependencies. Artifact A depends on B v1.2+, which depends on C v2.0+. If another artifact D depends on C v1.0+, we have a potential conflict.

The dependency resolution strategies mirror those of package managers:
- **Select a compatible version**: If C v2.0.0 is backward-compatible with v1.0.0, use v2.0.0 for all consumers.
- **Require explicit resolution**: The consumer must declare which version of C to use.
- **Reject the configuration**: The dependency graph is inconsistent; publication fails.

For analysis artifacts, the third option is often appropriate. Unlike runtime software, where you might "try it and see if it works," analysis artifacts must be precisely consistent. A type definition mismatch is not something you discover at runtime; it invalidates the analysis.

## 3.6.5 Reproducibility and Determinism

A key principle of generative analysis is reproducibility: given the same inputs, the analysis should produce the same outputs. Versioning supports this principle by fixing artifact versions.

When we generate a diagram from a specification, we record the exact artifact version used:

```seq
// Generated from payment-fsm v2.1.0
// Generation timestamp: 2025-11-15T14:23:01Z
// Generator: seq-gen v1.4.3
```

This provenance information allows us to:
- **Reproduce the generation**: Run the same tool on the same artifact version to verify output.
- **Understand historical artifacts**: Know which specification version produced a given diagram.
- **Debug discrepancies**: Compare generation results when behavior changes.

### Deterministic Generation

Generation tools should be deterministic. Non-determinism in generation (random IDs, timestamps in output, iteration order variation) undermines reproducibility.

We enforce determinism through:
- **Stable ordering**: Iterate over maps and sets in sorted order, not arbitrary order.
- **Explicit IDs**: Generate stable IDs from content hashes, not random values.
- **Timestamp control**: Include timestamps only when explicitly requested; otherwise omit or use placeholder.

When a generation tool produces non-deterministic output, we treat it as a bug. Reproducibility is not optional.

## 3.6.6 Publication Lifecycle

An artifact progresses through a lifecycle from creation to publication to deprecation:

1. **Draft**: Work-in-progress, not yet published. May be unstable or incomplete.
2. **Candidate**: Ready for review, awaiting publication approval.
3. **Published**: Officially released, immutable, referenceable.
4. **Superseded**: Newer version available; this version remains accessible but not recommended.
5. **Deprecated**: Should not be used; may be removed in future.

Transition criteria are organization-specific, but we recommend:
- **Draft to Candidate**: Automated checks pass (format validation, dependency resolution).
- **Candidate to Published**: Human review approves content accuracy and completeness.
- **Published to Superseded**: Newer version published; older version automatically marked.
- **Superseded to Deprecated**: Time-based policy (e.g., deprecate after 6 months) or explicit declaration.

This lifecycle mirrors semantic versioning but adds human oversight at publication boundaries. Not every commit should become a published artifact version.

## 3.6.7 Artifact Discovery and Documentation

Publishing artifacts is only useful if consumers can discover and understand them. We provide two mechanisms:

### Artifact Manifests

Each published artifact includes a machine-readable manifest:

```yaml
name: payment-fsm
version: 2.1.0
published: 2025-11-15T14:23:01Z
author: alice@example.com
description: Finite state machine for payment processing workflow
keywords: [payment, fsm, workflow]
license: MIT
dependencies:
  - name: payment-types
    version: ">= 1.3.0, < 2.0.0"
resources:
  spec: payment_fsm.yaml
  diagram: payment_states.svg
  examples: examples/
```

Manifests enable automated discovery tools to index and search artifacts.

### Human-Readable Documentation

Every published artifact includes human-readable documentation:
- **Purpose**: What does this artifact specify?
- **Usage**: How do consumers use this artifact?
- **Examples**: Concrete usage examples.
- **Changelog**: What changed between versions?

Documentation lives alongside the artifact, versioned together. When you retrieve `payment-fsm v2.1.0`, you also get the documentation for that specific version.

## 3.6.8 Case Study: Artifact Versioning in Practice

Consider a real-world example: a microservices architecture with 15 services, each with its own API specification, state machine, and deployment configuration. The team adopts artifact versioning:

**Before**: Specifications scattered across service repositories, identified only by filename. When breaking changes occurred, consumers found out only at runtime.

**After**: All specifications published to internal registry with semantic versions. Services declare dependencies with version constraints. Breaking changes trigger automated alerts to consumers.

**Results after six months**:
- Integration failures decreased by 67% (due to explicit version management)
- Onboarding time for new developers decreased by 40% (due to centralized artifact discovery)
- Architectural review time decreased by 30% (due to dependency visualization)

This case demonstrates that artifact versioning is not merely an academic exercise—it delivers concrete operational benefits.

---

# Chapter 4.1: Why Visual Models Alone Are Insufficient

## 4.1.1 The Seduction of Pictures

Human beings are visual creatures. A well-drawn diagram communicates at a glance what paragraphs of text might struggle to convey. Architecture diagrams, sequence diagrams, state machine diagrams—these are the lingua franca of system design. Is it any wonder that we rely on them so heavily?

But reliance has become over-reliance. Consider the typical architectural document: page after page of boxes and arrows, colorful and pristine. Stakeholders nod approvingly. "Yes, this looks like a solid design." And yet, when implementation begins, questions emerge that the diagrams cannot answer. What exactly is the contract between these boxes? What happens when this operation fails? What are the invariants this system maintains?

The diagrams looked authoritative, but their authority was illusory. They showed the "what" without the "how," the structure without the semantics. They were pictures pretending to be specifications.

## 4.1.2 The Ambiguity of Visual Notation

Visual modeling notations—UML, SysML, ArchiMate, BPMN—share a fundamental limitation: **ambiguity**. A box labeled "Payment Service" can mean anything from "a microservice handling payment logic" to "a conceptual boundary for grouping payment-related functionality." The arrow between "Order Service" and "Inventory Service" might represent a synchronous REST call, an asynchronous message queue, or merely a conceptual dependency.

This ambiguity is not a flaw to be fixed; it is inherent in visual notation. Visual models are inherently imprecise because precision requires language, not pictures.

### The Box-Arrow Illusion

Consider this diagram, common in architecture reviews:

```
┌─────────────┐         ┌─────────────┐
│   Client    │────────>│   Gateway   │
└─────────────┘         └─────────────┘
                                │
                                v
                        ┌─────────────┐
                        │   Service   │
                        └─────────────┘
```

What does this diagram specify?
- Does the arrow represent HTTP? gRPC? WebSockets?
- Is the communication synchronous or asynchronous?
- What authentication is required?
- What happens if Service is unavailable?
- What is the timeout?
- What is the retry policy?

The diagram answers none of these questions. It suggests a relationship without specifying it. For high-level architectural exploration, this might be sufficient. But for implementation guidance, it is dangerously insufficient.

## 4.1.3 The Missing Temporal Dimension

Visual models excel at representing structure and state. They struggle with **behavior over time**.

A sequence diagram shows interactions in sequence, but only in a stylized way. It does not easily represent:
- Concurrent operations with true parallelism
- Probabilistic outcomes (95% success rate)
- Performance characteristics (latency, throughput)
- Resource constraints (connection limits, memory usage)
- Failure modes and recovery paths

A state machine diagram shows states and transitions, but typically omits:
- Guards and conditions
- Actions and side effects
- Time-based transitions (timeouts)
- Event queuing behavior

The temporal dimension—the dynamic behavior of the system—is where most bugs hide. And visual models, by their nature, obscure this dimension.

### Example: The Misleading Sequence Diagram

Consider a sequence diagram showing a user registration flow:

```
Client    AuthService    UserService    EmailService
  |            |              |               |
  |--register-->|              |               |
  |            |--create------>|               |
  |            |<--------------ok              |
  |            |--send email------------------>|
  |            |<----------------------------ok|
  |<-----------ok----------|               |
```

This diagram suggests a happy path: registration succeeds, user is created, email is sent, all is well. What it does not show:
- What if UserService.create fails? (Validation error? Database down?)
- What if EmailService is unreachable? (Retry? Queue? Fail silently?)
- What if the email send times out? (Partial success? Rollback?)
- What if the client disconnects after user creation but before response? (User created but email never sent?)

The sequence diagram cannot easily show these scenarios without becoming cluttered beyond comprehension. Textual specifications handle branching logic more naturally.

## 4.1.4 The Detachment Problem

Perhaps the most serious limitation of visual models is their **detachment from implementation**.

In traditional workflows, architects create diagrams using visual tools (Visio, draw.io, Lucidchart). Developers write code using IDEs. The two artifacts live in separate worlds. When implementation changes, the diagrams are not updated. When diagrams are updated, the code may not reflect them.

This detachment leads to a divergence:
- **Code reality**: What the system actually does
- **Diagram fiction**: What the diagrams claim the system does

Over time, this divergence widens. The diagrams become "aspirational architecture"—documents describing a system that does not exist. Referencing them for understanding becomes not just unhelpful but actively misleading.

### The Reverse Engineering Illusion

Some teams attempt to solve the detachment problem by reverse-engineering diagrams from code. Tools scan the codebase and generate architecture diagrams. This seems promising: the diagrams now reflect reality.

But reverse-engineered diagrams have their own problems:
- **Implementation detail leakage**: They show what the code *does*, not what the architecture *intended*. A temporary workaround becomes a first-class architectural element.
- **Missing semantics**: Code structure does not reveal design intent. Why is there a circuit breaker here? What invariants does this database constraint enforce?
- **Context collapse**: All code is treated equally. Utility functions are shown alongside core business logic.

Reverse-engineered diagrams are accurate reflections of code structure, but they are not specifications. They are documentation of the present, not guidance for the future.

## 4.1.5 The Review Bottleneck

Visual models create a bottleneck in the review process. To review a diagram, stakeholders must:
- Open a specialized tool
- Load the diagram file
- Interpret the visual notation
- Infer the missing semantics

This process is slower and more cognitively demanding than reviewing text. A code review can show diffs line by line. A diagram review requires comparing two images, identifying what changed, and determining whether the change is correct.

Furthermore, diagram reviews often happen late. The architecture is "done," then reviewed. By then, significant investment has been made in the design. Changes are costly. Review feedback is ignored or minimized.

Text-based specifications, by contrast, can be reviewed incrementally. A pull request containing a specification file can be reviewed alongside code changes. The specification can evolve in lockstep with implementation.

## 4.1.6 The Case for Literate Modeling

If visual models are insufficient, what is the alternative? We propose **literate modeling**: text-first, machine-readable specifications that can be rendered into visualizations when needed.

Literate modeling has several advantages:

### Precision

Textual languages (YAML, JSON, domain-specific languages) allow precise specification of semantics. There is no ambiguity about what a field means or what a transition does. The specification is the source of truth.

### Version Control

Text files work naturally with Git and other version control systems. Diffs are clear. Merge conflicts, while possible, are resolvable. History is traceable.

### Machine-Processability

Textual specifications can be validated, transformed, and analyzed programmatically. We can:
- Verify consistency (no undefined references)
- Generate visualizations (diagrams as rendered output)
- Execute simulations (state machine execution)
- Check properties (reachability, liveness)

### Accessibility

Everyone can read and edit text. No specialized tools required. Review happens in the same workflow as code review.

## 4.1.7 Visualizations as Generated Artifacts

The key insight: **diagrams should be outputs, not sources**.

When the authoritative specification is text, diagrams become generated artifacts. They are derived from the specification, not manually created. This eliminates the detachment problem: the diagram always reflects the specification because it is generated from it.

When the specification changes, regenerate the diagram. When someone questions the diagram, consult the specification. The diagram is a view, not the source.

### Example: Textual State Machine, Generated Diagram

Consider a textual state machine specification:

```yaml
name: payment-fsm
states:
  - IDLE
  - AUTHORIZING
  - AUTHORIZED
  - CAPTURING
  - CAPTURED
  - FAILED
initial: IDLE
transitions:
  - from: IDLE
    to: AUTHORIZING
    trigger: authorize
  - from: AUTHORIZING
    to: AUTHORIZED
    trigger: auth_success
  - from: AUTHORIZING
    to: FAILED
    trigger: auth_failure
  # ... additional transitions
```

From this specification, we can generate a state machine diagram. The diagram is a view of the specification. If we change the specification, we regenerate the diagram. The diagram is never manually edited.

## 4.1.8 The Hybrid Approach: Text + Visual

We are not arguing against visualizations entirely. Visual representations have value:
- They provide an overview that text cannot match.
- They facilitate communication across technical backgrounds.
- They reveal patterns that text obscures.

The mistake is treating visualizations as primary. In literate modeling, visualizations are **complementary** to textual specifications, not replacements for them.

The authoritative source is text. Visualizations are derived, optional, and context-dependent. You might have different visualizations for different audiences: an executive overview, a developer detail view, a tester scenario view. All derive from the same textual source.

## 4.1.9 The Path Forward

Adopting literate modeling requires a shift in practice:
1. **Specify first**: Write the textual specification before creating any diagrams.
2. **Generate visualizations**: Use tools to render diagrams from specifications.
3. **Version both**: Commit specifications and generated diagrams together.
4. **Review specifications**: Make the textual artifact the focus of review.
5. **Update at source**: When requirements change, update the specification, then regenerate diagrams.

This approach delivers the benefits of visual modeling (communication, overview, pattern recognition) while avoiding its pitfalls (ambiguity, detachment, versioning difficulties).

## 4.1.10 Conclusion: Pictures as Views, Not Sources

Visual models are powerful communication tools, but they are poor specifications. Their ambiguity, detachment from implementation, and resistance to version control make them unsuitable as primary artifacts in constructive systems.

Literate modeling—textual, machine-readable specifications as authoritative sources, with visualizations as derived views—provides a robust foundation for generative analysis. The specification is precise, versionable, and processable. The visualization is communicative, optional, and always current.

In the following chapters, we explore how this approach enables the full generative analysis lifecycle: from specification to validation to generation to continuous evolution.
