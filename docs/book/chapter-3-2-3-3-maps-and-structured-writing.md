# Chapter 3.2-3.3: Mind Maps, Concept Maps, Dialog Maps, and Structured Writing

## 3.2 Mind Maps, Concept Maps, and Dialog Maps

### 3.2.1 Introduction to Visual Knowledge Structures

Constructive systems require structured inputs that can be compiled into executable workflows. Three primary visualization techniques serve this purpose: mind maps for hierarchical brainstorming, concept maps for relational knowledge modeling, and dialog maps for conversational structure. Each serves distinct cognitive purposes while sharing the fundamental property of transformability into workflow tokens.

**Visual Description:** Imagine a three-panel diagram. The left panel shows a mind map with a central node radiating outward in a star pattern. The center panel displays a concept map with interconnected nodes showing labeled directional relationships. The right panel presents a dialog map with speech-act nodes arranged in conversation flow. Arrows between panels indicate transformation pathways.

### 3.2.2 Mind Maps: Hierarchical Decomposition

#### Definition and Structure

A mind map represents hierarchical information as a tree structure radiating from a central concept. Nodes branch outward with decreasing specificity, capturing the natural associative structure of human thought. In constructive systems, mind maps translate directly into workflow decomposition patterns.

**Formal Definition:** A mind map M is a tuple (N, E, r, lambda) where:
- N is a finite set of nodes
- E is a set of directed edges forming a tree rooted at r
- r in N is the root (central) concept
- lambda: N -> Sigma assigns labels from alphabet Sigma
- For each n in N \ {r}, there exists exactly one path from r to n

#### Example: Order Fulfillment Mind Map

Consider an order fulfillment workflow captured as a mind map:

```
                    [Order Fulfillment]
                           |
          +----------------+----------------+
          |                |                |
    [Ordering]      [Carrier]          [Payment]
          |                |                |
    +-----+-----+     +----+----+     +----+----+
    |     |     |     |    |    |     |    |    |
 [Verify][Check][Confirm] [FTL][LTL][SP] [Card][Wire][PO]
```

**Workflow Compilation:**

```erlang
%% The mind map structure compiles to place/transition definitions:
%%
%% Root: order_fulfillment
%% Level 1: ordering, carrier_appointment, payment
%% Level 2 (under ordering): verify_inventory, check_credit, confirm_order
%% Level 2 (under carrier): ftl, ltl, sp
%% Level 2 (under payment): card, wire, purchase_order

%% Compiled places:
places() -> [
    order_fulfillment,
    ordering, carrier_appointment, payment,
    verify_inventory, check_credit, confirm_order,
    ftl, ltl, sp,
    card, wire, purchase_order
].

%% Root-to-leaf transitions represent workflow steps:
transitions() -> [
    t_start_ordering, t_start_carrier, t_start_payment,
    t_verify, t_check, t_confirm,
    t_ftl, t_ltl, t_sp,
    t_card, t_wire, t_po
].
```

**Token Transformation Rule:** Each mind map node becomes a place that can hold tokens. Parent-child edges become transitions that move tokens from parent to child. The root node holds the initial token that initiates workflow execution.

### 3.2.3 Concept Maps: Relational Knowledge Modeling

#### Definition and Structure

Concept maps extend mind maps by explicitly modeling relationships between concepts. Rather than strict hierarchy, concept maps capture networks of meaning with labeled edges representing semantic relationships. This maps naturally to Petri net places with conditional transitions.

**Formal Definition:** A concept map C is a tuple (C, R, rho_c, rho_r) where:
- C is a finite set of concept nodes
- R is a finite set of relationship edges
- rho_c: C -> Sigma assigns concept labels
- rho_r: R -> Sigma × Sigma assigns (predicate, direction) to each relationship
- Unlike mind maps, cycles are permitted

#### Example: Payment Method Selection Concept Map

The Order Fulfillment workflow uses a concept map to determine payment methods based on shipment characteristics:

```
                    [Shipment]
                      / | \
                     /  |  \
        (volume>10k) /   |   \ (volume<1k)
                   /     |    \
              [FTL]    [LTL]  [Small Parcel]
                |        |        |
        (prepay) | (requires) | (collect)
                |        |        |
           [Carrier Quote][Account Setup][Customer Billing]
```

**Workflow Compilation:**

```erlang
%% Concept map relationships become guarded transitions

%% FTL: volume >= 10000
modes(t_ftl_decision, #{shipment_ready := [S]}, _UsrInfo) ->
    Volume = maps:get(volume, S, 0),
    case Volume >= 10000 of
        true -> [#{shipment_ready => [], ftl_payment => [S]}];
        false -> []
    end.

%% LTL: 1000 <= volume < 10000
modes(t_ltl_decision, #{shipment_ready := [S]}, _UsrInfo) ->
    Volume = maps:get(volume, S, 0),
    case Volume >= 1000 andalso Volume < 10000 of
        true -> [#{shipment_ready => [], ltl_payment => [S]}];
        false -> []
    end.

%% Small Parcel: volume < 1000
modes(t_sp_decision, #{shipment_ready := [S]}, _UsrInfo) ->
    Volume = maps:get(volume, S, 0),
    case Volume < 10000 andalso Volume < 1000 of
        true -> [#{shipment_ready => [], sp_payment => [S]}];
        false -> []
    end.
```

**Visual Description:** A diagram showing the transformation pipeline from concept map to Petri net. Concept nodes become places (circles). Relationship arrows become transitions (rectangles) with guard conditions written in Erlang syntax. Dashed lines show the correspondence between original relationships and compiled guards.

### 3.2.4 Dialog Maps: Conversational Workflows

#### Definition and Structure

Dialog maps model conversation flows between agents in a workflow. They capture speech acts, turn-taking structure, and conversational moves. In constructive systems, dialog maps compile into human-in-the-loop workflows with approval steps.

**Formal Definition:** A dialog map D is a tuple (A, T, delta, tau) where:
- A is a set of agents (participants)
- T is a set of turn nodes representing speech acts
- delta: T -> A assigns each turn to an agent
- tau: T × T -> {next, response, interrupt} defines turn transitions

#### Example: Carrier Appointment Dialog Map

The carrier appointment subprocess uses a dialog map for human coordination:

```
         [System: Request Carrier]
                   |
                   | (query)
                   v
         [Human: Provide Preferences]
                   |
         +---------+---------+
         |                   |
    (accept)             (reject)
         |                   |
         v                   v
 [System: Select Carrier] [System: Request Alternative]
         |                   |
         | (confirm)         | (retry)
         v                   v
    [Human: Confirm]----->[Human: Provide Alternative]
         |
         | (finalize)
         v
    [System: Finalize Appointment]
```

**Workflow Compilation:**

```erlang
%% Dialog map compiles to human task places with token types

%% Turn 1: System initiates
modes(t_request_carrier, #{start := [_]}, _UsrInfo) ->
    [#{start => [], human_turn => [{task, carrier_request, system}]}].

%% Turn 2: Human responds with preferences
fire(carrier_request, #{human_turn := [{task, carrier_request, system}]}, _UsrInfo) ->
    Token = {await_human_input, #{preferences => []}},
    {produce, #{human_response => [Token]}}.

%% Turn 3: Branch based on response
modes(t_process_preferences, #{human_response := [{await_human_input, Data}]}, _UsrInfo) ->
    case maps:get(action, Data, accept) of
        accept ->
            [#{human_response => [], carrier_selection => [Data]}];
        reject ->
            [#{human_response => [], alternative_request => [Data]}]
    end.

%% Turn 4: Confirmation loop
modes(t_confirm_selection, #{carrier_selection := [Selection]}, _UsrInfo) ->
    [#{carrier_selection => [], human_confirm => [{confirm, Selection}]}].
```

**Token Structure:** Dialog map tokens carry speech-act metadata:

```erlang
-record(speech_act, {
    act :: request | inform | confirm | reject,
    speaker :: atom(),
    content :: map(),
    timestamp :: integer()
}).
```

### 3.2.5 Comparative Analysis

| Aspect | Mind Map | Concept Map | Dialog Map |
|--------|----------|-------------|------------|
| **Structure** | Hierarchical tree | Relational network | Turn-based sequence |
| **Primary Use** | Brainstorming | Knowledge modeling | Human workflows |
| **Cycle Handling** | No cycles | Cycles permitted | Controlled loops |
| **Compilation Target** | Sequential workflow | Conditional workflow | Approval workflow |
| **Token Semantics** | Presence/absence | Data-carrying | Speech-act typed |
| **Typical YAWL Pattern** | Sequence (WCP-01) | Exclusive Choice (WCP-04) | Human Task (custom) |

**Visual Description:** A comparison matrix visualization with three columns. Each cell contains an icon representing the aspect (structure, cycles, etc.) alongside comparative notes. Arrows indicate typical transformation pathways from each map type to specific YAWL workflow patterns.

### 3.2.6 Map-to-Workflow Compilation Pipeline

The transformation from visual maps to executable workflows follows a systematic pipeline:

**Stage 1: Structural Parsing**

1. Extract nodes as places
2. Extract edges as transition candidates
3. Identify root/entry points
4. Detect cycles requiring resolution

**Stage 2: Semantic Annotation**

```erlang
%% Annotate places with metadata
-record(place_meta, {
    id :: atom(),
    type :: start | end | decision | task | merge,
    data_schema :: map() | undefined,
    human_required :: boolean()
}).

%% Annotate transitions with guards
-record(trans_meta, {
    id :: atom(),
    guard :: function() | undefined,
    effect :: function() | undefined,
    timeout :: integer() | infinity
}).
```

**Stage 3: Code Generation**

```erlang
%% Generate pnet_net behavior module
generate_net_module(MapData) ->
    #{
        places => extract_places(MapData),
        transitions => extract_transitions(MapData),
        preset => generate_presets(MapData),
        postset => generate_postsets(MapData),
        modes => generate_mode_functions(MapData),
        fire => generate_fire_functions(MapData)
    }.
```

**Visual Description:** Flow diagram showing three stages. Stage 1 shows a mind map being parsed into node/edge lists. Stage 2 shows annotation adding metadata (icons for human tasks, guards for conditions). Stage 3 shows Erlang code generation with module template.

---

## 3.3 Structured Writing as an Executable Substrate

### 3.3.1 The Informal-to-Formal Transformation

Structured writing represents the highest-fidelity capture of workflow specification. Unlike visual maps, text preserves nuance, constraint, and intent. The challenge lies in transforming natural language into executable Petri net structures. This section describes methods for writing that compiles.

**Core Principle:** Write workflow specifications using a constrained natural language that maps 1:1 to Petri net constructs. This is not a new programming language—it is disciplined prose with clear transformation rules.

### 3.3.2 The Structured Specification Language

#### Grammar Definition

The structured specification language (SSL) has the following grammar:

```
<specification> ::= <declaration_list>

<declaration> ::= <place_decl>
                | <transition_decl>
                | <flow_decl>
                | <data_decl>

<place_decl> ::= "PLACE" <id> <type_annotation>?
<type_annotation> ::= ":" <place_type>
<place_type> ::= "START" | "END" | "DECISION" | "TASK" | "MERGE"

<transition_decl> ::= "TRANSITION" <id> <guard_annotation>?
<guard_annotation> ::= "WHEN" <boolean_expression>

<flow_decl> ::= "FLOW" <from_id> "TO" <to_id> <condition_annotation>?
<condition_annotation> ::= "IF" <boolean_expression>

<data_decl> ::= "DATA" <id> "=" <expression>
```

#### Example: Order Fulfillment Specification

```
PLACE ordering_start: START
PLACE inventory_check: TASK
PLACE credit_check: TASK
PLACE ordering_complete: DECISION

TRANSITION t_begin_ordering
FLOW ordering_start TO inventory_check

TRANSITION t_verify_inventory
FLOW inventory_check TO credit_check IF inventory_available

TRANSITION t_check_credit
FLOW credit_check TO ordering_complete IF credit_approved

PLACE carrier_appointment: TASK
PLACE payment_selection: DECISION
PLACE ftl_payment: TASK
PLACE ltl_payment: TASK
PLACE sp_payment: TASK

TRANSITION t_select_payment_method
FLOW payment_selection TO ftl_payment IF volume >= 10000
FLOW payment_selection TO ltl_payment IF volume >= 1000 AND volume < 10000
FLOW payment_selection TO sp_payment IF volume < 1000
```

### 3.3.3 Compilation Process

#### Lexer and Parser

The SSL specification tokenizes into an abstract syntax tree (AST):

```erlang
%% AST node types
-record(ast_place, {id, type, line}).
-record(ast_transition, {id, guard, line}).
-record(ast_flow, {from, to, condition, line}).
-record(ast_data, {id, value, line}).

%% Token specification
-define(TOKENS, [
    {place, "PLACE"},
    {transition, "TRANSITION"},
    {flow, "FLOW"},
    {to, "TO"},
    {data, "DATA"},
    {colon, ":"},
    {if_kw, "IF"},
    {when_kw, "WHEN"}
]).
```

#### Place Generation

Each PLACE declaration generates a Petri net place:

```erlang
generate_place(#ast_place{id = Id, type = Type}) ->
    PlaceId = list_to_atom(Id),
    InitMarking = case Type of
        start -> [init];
        end -> [];
        _ -> []
    end,
    {PlaceId, InitMarking, Type}.

%% Generates:
%% place_lst() -> [ordering_start, inventory_check, ...].
%% init_marking(ordering_start, _UsrInfo) -> [init].
%% init_marking(inventory_check, _UsrInfo) -> [].
```

#### Transition Generation

TRANSITION declarations generate Petri net transitions:

```erlang
generate_transition(#ast_transition{id = Id, guard = Guard}) ->
    TransId = list_to_atom("t_" ++ Id),
    {TransId, compile_guard(Guard)}.

%% Guard compilation
compile_guard({boolean_expr, Expr}) ->
    {fun Module:Expr/3, Expr}.  % Partial application
```

#### Flow Generation

FLOW declarations connect places via transitions:

```erangelog
%% FLOW A TO B creates preset/postset relationships
generate_flow(#ast_flow{from = From, to = To, condition = Cond}) ->
    FromPlace = list_to_atom(From),
    ToPlace = list_to_atom(To),
    Transition = list_to_atom("t_" ++ From ++ "_to_" ++ To),
    {
        Transition,
        #{preset => [FromPlace], postset => [ToPlace]},
        compile_condition(Cond)
    }.

%% Generates:
%% preset(t_ordering_to_inventory) -> [ordering_start].
%% postset(t_ordering_to_inventory) -> [inventory_check].
```

### 3.3.4 Advanced Structured Writing Patterns

#### Pattern 1: Conditional Branching

Structured specification for exclusive choice:

```
DECISION payment_method
    WHEN order_volume >= 10000: FTL_PAYMENT
    WHEN order_volume >= 1000: LTL_PAYMENT
    OTHERWISE: SP_PAYMENT
END DECISION
```

Compiles to:

```erlang
modes(t_payment_decision, #{payment_method := [Data]}, _UsrInfo) ->
    Volume = maps:get(order_volume, Data, 0),
    if
        Volume >= 10000 ->
            [#{payment_method => [], ftl_payment => [Data]}];
        Volume >= 1000 ->
            [#{payment_method => [], ltl_payment => [Data]}];
        true ->
            [#{payment_method => [], sp_payment => [Data]}]
    end.
```

#### Pattern 2: Parallel Execution

Structured specification for parallel split:

```
PARALLEL check_inventory_check_payment
    BRANCH inventory_verification
        DO verify_stock
        DO verify_location
    END BRANCH
    BRANCH payment_verification
        DO check_credit
        DO validate_payment_method
    END BRANCH
WAIT ALL
END PARALLEL
```

Compiles to:

```erlang
%% Parallel split transition
modes(t_parallel_split, #{check_all := [Data]}, _UsrInfo) ->
    [#{check_all => [],
       inventory_verification => [Data],
       payment_verification => [Data]}].

%% Synchronization merge
modes(t_sync_merge, Marking, _UsrInfo) ->
    HasInventory = maps:is_key(inventory_verification, Marking),
    HasPayment = maps:is_key(payment_verification, Marking),
    case HasInventory andalso HasPayment of
        true ->
            %% Both branches complete
            [#{
                inventory_verification => [],
                payment_verification => [],
                all_complete => [merged]
            }];
        false ->
            []  % Not ready to merge
    end.
```

#### Pattern 3: Human-in-the-Loop Tasks

Structured specification for human tasks:

```
HUMAN TASK carrier_selection
    ASSIGNEE: logistics_manager
    TIMEOUT: 24 hours
    INPUT: shipment_details, carrier_options
    OUTPUT: selected_carrier, negotiated_rate
    ON TIMEOUT: escalate_to_director
END HUMAN TASK
```

Compiles to:

```erlang
%% Human task place generation
init_marking(carrier_selection, _UsrInfo) ->
    [{human_task, #{
        task_id => carrier_selection,
        assignee => logistics_manager,
        timeout => 86400000,  % 24 hours
    }}].

%% Timeout handling
modes(t_timeout_handler, #{carrier_selection := Tokens}, UsrInfo) ->
    Now = maps:get(now, UsrInfo, 0),
    case lists:filter(fun({human_task, T}) ->
        Started = maps:get(started_at, T, 0),
        Timeout = maps:get(timeout, T, infinity),
        Now - Started > Timeout
    end, Tokens) of
        [] -> [];
        Expired ->
            [#{carrier_selection => Expired,
               escalate_to_director => [escalate]}]
    end.
```

### 3.3.5 Token Structure and Data Flow

#### Data-Carrying Tokens

Structured specifications declare data schemas:

```
DATA SCHEMA order_data
    order_id: STRING
    customer_id: STRING
    items: LIST<item>
    total: DECIMAL
    volume: NUMBER
    weight: NUMBER
END SCHEMA

DATA SCHEMA item
    sku: STRING
    quantity: NUMBER
    price: DECIMAL
END SCHEMA
```

Compiles to Erlang records:

```erlang
-record(order_data, {
    order_id :: binary(),
    customer_id :: binary(),
    items :: [#item{}],
    total :: number(),
    volume :: number(),
    weight :: number()
}).

-record(item, {
    sku :: binary(),
    quantity :: pos_integer(),
    price :: number()
}).
```

#### Token Transformation Rules

Tokens transform as they flow through transitions:

```erlang
%% Fire function transforms input tokens to output tokens
fire(t_calculate_shipping, #{payment_ready := [#order_data{} = Order]}, _UsrInfo) ->
    Volume = Order#order_data.volume,
    Weight = Order#order_data.weight,
    PaymentMethod = determine_payment_method(Volume, Weight),
    {produce, #{
        payment_ready => [],
        payment_processing => [Order#order_data{
            payment_method = PaymentMethod
        }]
    }}.
```

### 3.3.6 From Informal Prose to Formal Specification

#### Example Transformation

**Informal Description:**

> "When an order comes in, we first check if we have the items in stock. If we do, we proceed to check the customer's credit. If credit is good, we appoint a carrier. The carrier depends on shipment size—big shipments go FTL, medium go LTL, and small go parcel. Then we take payment and ship."

**Structured Specification:**

```
WORKFLOW OrderFulfillment

    PLACE order_received: START
    PLACE stock_checked: DECISION
    PLACE credit_checked: DECISION
    PLACE carrier_appointed: TASK
    PLACE payment_processed: TASK
    PLACE order_shipped: END

    TRANSITION check_stock
    FLOW order_received TO stock_checked

    TRANSITION verify_credit IF stock_available
    FLOW stock_checked TO credit_checked

    TRANSITION appoint_carrier IF credit_approved
    FLOW credit_checked TO carrier_appointed

    DECISION carrier_selection
        WHEN volume >= 10000: ftl_carrier
        WHEN volume >= 1000: ltl_carrier
        OTHERWISE: parcel_carrier
    END DECISION

    TRANSITION process_payment
    FLOW carrier_appointed TO payment_processed

    TRANSITION ship_order
    FLOW payment_processed TO order_shipped

END WORKFLOW
```

**Visual Description:** Three-panel transformation diagram. Left: Informal prose paragraph. Center: Structured specification with keywords highlighted. Right: Petri net diagram with places (circles), transitions (rectangles), and labeled arcs. Arrows between panels show transformation steps.

### 3.3.7 Validation and Verification

#### Static Analysis

Structured specifications undergo static analysis before compilation:

```erlang
%% Validation checks
validate_spec(AST) ->
    Checks = [
        fun check_place_undeclared/1,
        fun check_transition_undeclared/1,
        fun check_flow_validity/1,
        fun check_no_orphan_places/1,
        fun check_single_start_place/1,
        fun check_reachable_end_places/1
    ],
    lists:foldl(fun(Check, {Ok, Errors}) ->
        case Check(AST) of
            ok -> {Ok, Errors};
            {error, NewErrors} -> {false, Errors ++ NewErrors}
        end
    end, {true, []}, Checks).

%% Reachability analysis
check_reachable_end_places(AST) ->
    StartPlaces = find_places_by_type(AST, start),
    EndPlaces = find_places_by_type(AST, end),
    Reachable = compute_reachable_nodes(AST, StartPlaces),
    Unreachable = EndPlaces -- Reachable,
    case Unreachable of
        [] -> ok;
        _ -> {error, [{unreachable_end, P} || P <- Unreachable]}
    end.
```

#### Dynamic Verification

Compiled workflows are verified through simulation:

```erlang
%% Reachability analysis via state space exploration
verify_reachability(Module) ->
    {ok, Pid} = gen_pnet:start_link(Module, #{}, []),
    States = explore_state_space(Pid, 1000, #{}),
    gen_pnet:stop(Pid),
    #{
        states_visited = maps:size(States),
        deadlocks_found = find_deadlocks(States),
        unbounded_places = find_unbounded(States)
    }.

%% Deadlock detection
find_deadlocks(States) ->
    maps:fold(fun(_StateId, State, Acc) ->
        Marking = maps:get(marking, State),
        Enabled = maps:get(enabled, State),
        case Enabled of
            [] when Marking =/= #{} ->
                [{State, Marking} | Acc];
            _ ->
                Acc
        end
    end, [], States).
```

### 3.3.8 Best Practices for Structured Writing

1. **Declare All Places First**: Explicitly declare all places before using them in flows
2. **Use Descriptive Identifiers**: Place and transition names should be self-documenting
3. **Specify Guard Conditions Clearly**: Boolean expressions should be unambiguous
4. **Document Data Schemas**: Declare all data structures before using them
5. **Validate Early**: Run static analysis before attempting compilation
6. **Test Incrementally**: Verify each subprocess before integrating

**Anti-patterns to Avoid:**

- Implicit places (not declared but referenced)
- Ambiguous guard conditions using "maybe" or "sometimes"
- Data transformations without explicit schema changes
- Orphaned places with no incoming or outgoing flows

**Visual Description:** A two-column comparison table. Left column: "Good Practice" with checkmarks and examples. Right column: "Anti-Pattern" with X marks and examples. Color coding highlights the differences.

### 3.3.9 Summary

Structured writing provides the most precise method for capturing workflow specifications. By using disciplined prose with clear transformation rules, we bridge the gap between human understanding and machine execution. The compilation pipeline from structured text to Petri nets ensures that workflows are:

- **Readable**: Humans can understand the specification
- **Verifiable**: Static analysis catches errors early
- **Executable**: Direct compilation to runtime systems
- **Maintainable**: Changes propagate through the pipeline

The integration of mind maps, concept maps, dialog maps, and structured writing provides a complete toolkit for capturing workflow specifications at varying levels of formality. Each technique serves different cognitive needs while sharing the fundamental property of transformability into executable workflow tokens.

---

## Figure References

**Figure 3.1:** Map Types Comparison - Three-panel diagram showing mind map, concept map, and dialog map structures.

**Figure 3.2:** Order Fulfillment Mind Map - Hierarchical decomposition of the order fulfillment workflow.

**Figure 3.3:** Payment Method Concept Map - Relational network showing payment selection logic with volume thresholds.

**Figure 3.4:** Carrier Appointment Dialog Map - Turn-based conversation flow between system and human agents.

**Figure 3.5:** Map Compilation Pipeline - Three-stage transformation from visual map to executable Erlang code.

**Figure 3.6:** Structured Specification Compilation - Transformation from informal prose to structured spec to Petri net.

**Figure 3.7:** Best Practices Comparison - Two-column table contrasting good practices with anti-patterns.

---

## Key Takeaways

1. **Mind maps** capture hierarchical decomposition and compile to sequential workflows
2. **Concept maps** model relational knowledge and compile to conditional workflows with guarded transitions
3. **Dialog maps** represent conversation flows and compile to human-in-the-loop approval workflows
4. **Structured writing** provides the most precise specification method through disciplined prose
5. All four approaches transform into **Petri net places and transitions** via systematic compilation
6. **Tokens** carry data, speech acts, or control signals as they flow through the workflow
7. **Validation** occurs at multiple stages: structural parsing, static analysis, and runtime verification

---

*Word count: approximately 2,300 words*
