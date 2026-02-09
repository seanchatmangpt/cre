# GA Constitution Validation Rules

This document specifies all validation rules for GA Constitutions.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Structural Validation (REQ)](#2-structural-validation-req)
3. [Sigma Validation (SIG)](#3-sigma-validation-sig)
4. [Eta Validation (ETA)](#4-eta-validation-eta)
5. [Kappa Validation (KAP)](#5-kappa-validation-kap)
6. [Lambda Validation (LAM)](#6-lambda-validation-lam)
7. [Cross-Aspect Validation (CROSS)](#7-cross-aspect-validation-cross)
8. [Safety Level Constraints](#8-safety-level-constraints)
9. [Validation Functions](#9-validation-functions)

---

## 1. Overview

Validation rules are organized into categories:
- **REQ**: Structural requirements (constitution format)
- **SIG**: Sigma (typing profile) validation
- **ETA**: Eta (refusal catalog) validation
- **KAP**: Kappa (quality gates) validation
- **LAM**: Lambda (pattern composition) validation
- **CROSS**: Cross-aspect validation

Each rule has:
- **ID**: Unique identifier (e.g., REQ-001)
- **Name**: Human-readable name
- **Severity**: error | warning | critical
- **Description**: What the rule checks
- **Remediation**: How to fix violations

---

## 2. Structural Validation (REQ)

| ID | Name | Severity | Description | Remediation |
|----|------|----------|-------------|-------------|
| REQ-001 | Valid Constitution ID | error | ID must match `^CON_[A-F0-9]+$` | Use format `CON_<hexadecimal>` |
| REQ-002 | Valid Version | error | Version must match `^\\d+\\.\\d+$` | Use semantic versioning like `1.0` |
| REQ-003 | All Aspects Present | error | Σ, H, Q, Λ must all be present | Add missing aspect with valid structure |
| REQ-004 | Unique Import Aliases | error | Import aliases must be unique | Rename duplicate aliases |
| REQ-005 | Valid Domain | error | Domain must be valid enum value | Use: workflow, agent, hybrid, testing |
| REQ-006 | Valid Safety Level | error | Safety level must be valid enum value | Use: permissive, standard, strict, critical |
| REQ-007 | Valid Verification Mode | error | Verification mode must be valid enum value | Use: none, static, runtime, full |
| REQ-008 | Non-Empty Name | error | Constitution name must not be empty | Provide a descriptive name |
| REQ-009 | Max Name Length | warning | Name should be <= 256 characters | Shorten name |
| REQ-010 | Unique IDs in Import | error | Import IDs must be unique | Remove duplicate imports |

### Validation Functions

```erlang
%% Validate constitution ID format
-spec validate_constitution_id(binary()) -> validation_result().
validate_constitution_id(Id) ->
    case re:run(Id, "^CON_[A-F0-9]+$") of
        {match, _} -> ok;
        nomatch -> {error, ?REF("REQ-001", "Invalid constitution ID format")}
    end.

%% Validate version format
-spec validate_version(binary()) -> validation_result().
validate_version(Version) ->
    case re:run(Version, "^\\d+\\.\\d+$") of
        {match, _} -> ok;
        nomatch -> {error, ?REF("REQ-002", "Invalid version format")}
    end.
```

---

## 3. Sigma Validation (SIG)

| ID | Name | Severity | Description | Remediation |
|----|------|----------|-------------|-------------|
| SIG-001 | Unique Type Names | error | Type names must be unique | Rename duplicate types |
| SIG-002 | Union Has Discriminant | error | Union types must have discriminant field | Add discriminant field |
| SIG-003 | Valid Contract Type | error | Token contract must reference defined type | Create type or fix reference |
| SIG-004 | Valid Cardinality | error | Place cardinality: max >= min | Fix cardinality values |
| SIG-005 | Valid Consume Place | error | Transition consumes must reference valid place | Fix place reference |
| SIG-006 | Valid Produce Place | error | Transition produces must reference valid place | Fix place reference |
| SIG-007 | Valid Narrowing Type | error | Narrowing rules must reference valid types | Fix type reference |
| SIG-008 | Required Fields Present | error | Required fields must be marked required | Mark field as required |
| SIG-009 | Valid Field Type | error | Field types must be known primitives or records | Fix field type |
| SIG-010 | Valid Variant List | warning | Union types should have at least 2 variants | Add more variants |
| SIG-011 | Non-Empty Field Name | error | Field names must not be empty | Provide field name |
| SIG-012 | Valid Constraint Name | error | Constraint names must be valid atoms | Use valid constraint name |

### Validation Functions

```erlang
%% Validate type uniqueness
-spec validate_unique_types([#type_def{}]) -> validation_result().
validate_unique_types(Types) ->
    Names = [T#type_def.name || T <- Types],
    case length(Names) =:= length(lists:usort(Names)) of
        true -> ok;
        false -> {error, ?REF("SIG-001", "Duplicate type names found")}
    end.

%% Validate union discriminant
-spec validate_union_discriminant(#type_def{}) -> validation_result().
validate_union_discriminant(#type_def{base = union, discriminant = undefined}) ->
    {error, ?REF("SIG-002", "Union types must have discriminant")};
validate_union_discriminant(#type_def{base = union}) -> ok;
validate_union_discriminant(_) -> ok.

%% Validate cardinality
-spec validate_cardinality({non_neg_integer(), pos_integer() | unlimited}) -> validation_result().
validate_cardinality({Min, Max}) when Max =:= unlimited; Max >= Min -> ok;
validate_cardinality(_) ->
    {error, ?REF("SIG-004", "Cardinality max must be >= min")}.
```

---

## 4. Eta Validation (ETA)

| ID | Name | Severity | Description | Remediation |
|----|------|----------|-------------|-------------|
| ETA-001 | Unique Refusal IDs | error | Refusal IDs must be unique | Rename duplicate refusals |
| ETA-002 | Valid Pattern Params | error | Refusal pattern params must match guard args | Fix parameter list |
| ETA-003 | Valid Composition References | error | Composition combines must reference refusals | Fix reference or add refusal |
| ETA-004 | Loadable Handler Module | error | Handler modules must be loadable | Ensure module exists |
| ETA-005 | Critical Has Remediation | error | Critical refusals must have remediation strategy | Add remediation |
| ETA-006 | Valid Guard Type | error | Guard type must be valid enum value | Use valid guard type |
| ETA-007 | Valid Remediation Strategy | error | Remediation strategy must be valid enum value | Use valid strategy |
| ETA-008 | Non-Empty Guard Check | warning | Guard should have check or args | Add check or args |
| ETA-009 | Valid Severity | error | Severity must be valid enum value | Use: warning, error, critical |
| ETA-010 | Valid Category | error | Category must be valid enum value | Use valid category |
| ETA-011 | Handler Function Exists | error | Handler function must exist in module | Add function or fix reference |
| ETA-012 | Unique Handler Priorities | warning | Handler priorities should be unique | Adjust priorities |

### Validation Functions

```erlang
%% Validate refusal uniqueness
-spec validate_unique_refusals([#refusal{}]) -> validation_result().
validate_unique_refusals(Refusals) ->
    IDs = [R#refusal.id || R <- Refusals],
    case length(IDs) =:= length(lists:usort(IDs)) of
        true -> ok;
        false -> {error, ?REF("ETA-001", "Duplicate refusal IDs found")}
    end.

%% Validate critical has remediation
-spec validate_critical_remediation(#refusal{}) -> validation_result().
validate_critical_remediation(#refusal{severity = critical, remediation = #remediation{}}) ->
    ok;
validate_critical_remediation(#refusal{severity = critical}) ->
    {error, ?REF("ETA-005", "Critical refusals must have remediation")};
validate_critical_remediation(_) -> ok.

%% Validate handler exists
-spec validate_handler_exists(#refusal_handler{}) -> validation_result().
validate_handler_exists(#refusal_handler{handler_module = Mod, handler_function = Fun}) ->
    case code:is_loaded(Mod) of
        false ->
            try
                {module, Mod} = code:load_file(Mod),
                case erlang:function_exported(Mod, Fun, 0) of
                    true -> ok;
                    false -> {error, ?REF("ETA-011", "Handler function not found")}
                end
            catch
                _:_ -> {error, ?REF("ETA-004", "Handler module not loadable")}
            end;
        true ->
            case erlang:function_exported(Mod, Fun, 0) of
                true -> ok;
                false -> {error, ?REF("ETA-011", "Handler function not found")}
            end
    end.
```

---

## 5. Kappa Validation (KAP)

| ID | Name | Severity | Description | Remediation |
|----|------|----------|-------------|-------------|
| KAP-001 | Unique Invariant IDs | error | Invariant IDs must be unique | Rename duplicate invariants |
| KAP-002 | Valid Retention Policy | error | Receipt retention must have duration or max | Add retention constraint |
| KAP-003 | Valid Replay Interval | error | Replay interval must be > 0 for periodic | Set positive interval |
| KAP-004 | Valid Gate Metric | error | Gate metric must be valid | Use valid metric name |
| KAP-005 | Receipts for Critical | error | Critical safety requires receipts enabled | Enable receipts |
| KAP-006 | Valid Check Point | error | Check point must be valid enum value | Use valid check point |
| KAP-007 | Valid Failure Action | error | Failure action must be valid enum value | Use valid action |
| KAP-008 | Valid Hash Algorithm | error | Hash algorithm must be valid | Use: sha256, sha512, blake2b, none |
| KAP-009 | Valid Comparison Operator | error | Gate operator must be valid | Use: =:=, =/=, <, =<, >, >= |
| KAP-010 | Positive Gate Value | error | Gate threshold value must be positive | Use positive value |
| KAP-011 | Valid Verification Check | error | Verification checks must be valid | Use valid check name |

### Validation Functions

```erlang
%% Validate invariant uniqueness
-spec validate_unique_invariants([#invariant{}]) -> validation_result().
validate_unique_invariants(Invariants) ->
    IDs = [I#invariant.id || I <- Invariants],
    case length(IDs) =:= length(lists:usort(IDs)) of
        true -> ok;
        false -> {error, ?REF("KAP-001", "Duplicate invariant IDs found")}
    end.

%% Validate retention policy
-spec validate_retention_policy(#retention_policy{}) -> validation_result().
validate_retention_policy(#retention_policy{policy = indefinite}) -> ok;
validate_retention_policy(#retention_policy{duration_ms = Duration}) when is_integer(Duration), Duration > 0 -> ok;
validate_retention_policy(#retention_policy{max_receipts = Max}) when is_integer(Max), Max > 0 -> ok;
validate_retention_policy(_) ->
    {error, ?REF("KAP-002", "Retention policy must have duration or max_receipts")}.

%% Validate critical has receipts
-spec validate_critical_receipts(#constitution_profile{}, #receipt_config{}) -> validation_result().
validate_critical_receipts(#constitution_profile{safety_level = critical}, #receipt_config{enabled = false}) ->
    {error, ?REF("KAP-005", "Critical safety level requires receipts enabled")};
validate_critical_receipts(_, _) -> ok.
```

---

## 6. Lambda Validation (LAM)

| ID | Name | Severity | Description | Remediation |
|----|------|----------|-------------|-------------|
| LAM-001 | Unique Instance IDs | error | Pattern instance IDs must be unique | Rename duplicate instances |
| LAM-002 | Valid Pattern Reference | error | Pattern must be in registry | Use valid pattern name |
| LAM-003 | Valid Topology Nodes | error | Topology edges must reference valid nodes | Add node or fix edge |
| LAM-004 | No Acyclic Violation | warning | Non-recursive compositions should be acyclic | Break cycle or mark recursive |
| LAM-005 | Valid Macro Params | error | Macro params must be valid atoms | Use valid atom names |
| LAM-006 | Valid Composition Type | error | Composition type must be valid | Use valid type |
| LAM-007 | Valid Constraint Type | error | Constraint type must be valid | Use valid type |
| LAM-008 | Valid Cardinality Constraint | error | Cardinality constraints must be valid | Fix min/max values |
| LAM-009 | Instance in Topology | warning | All instances should be in topology | Add instance to topology |
| LAM-010 | Valid Net Reference | error | Net must be defined in workflow | Fix net reference |

### Validation Functions

```erlang
%% Validate instance uniqueness
-spec validate_unique_instances([#pattern_instance{}]) -> validation_result().
validate_unique_instances(Instances) ->
    IDs = [I#pattern_instance.id || I <- Instances],
    case length(IDs) =:= length(lists:usort(IDs)) of
        true -> ok;
        false -> {error, ?REF("LAM-001", "Duplicate pattern instance IDs found")}
    end.

%% Validate topology edges
-spec validate_topology_edges(#pattern_topology{}) -> validation_result().
validate_topology_edges(#pattern_topology{nodes = Nodes, edges = Edges}) ->
    NodeSet = sets:from_list(Nodes),
    InvalidEdges = lists:filter(fun(E) ->
        not (sets:is_element(E#topology_edge.from, NodeSet) andalso
             sets:is_element(E#topology_edge.to, NodeSet))
    end, Edges),
    case InvalidEdges of
        [] -> ok;
        _ -> {error, ?REF("LAM-003", "Topology edges reference undefined nodes")}
    end.

%% Validate acyclic for non-recursive
-spec validate_acyclic([#pattern_composition{}]) -> validation_result().
validate_acyclic(Compositions) ->
    NonRecursive = [C || C <- Compositions, C#pattern_composition.type =/= recursive],
    case has_cycle(NonRecursive) of
        false -> ok;
        true -> {warning, ?REF("LAM-004", "Non-recursive composition contains cycle")}
    end.

%% Helper to detect cycles
has_cycle(Compositions) ->
    % Build graph and detect cycle using DFS
    Nodes = lists:usort([C#pattern_composition.id || C <- Compositions] ++
                        [I || C <- Compositions, I <- element(2, C) ||
                                                     C#pattern_composition.outer =/= undefined orelse
                                                     C#pattern_composition.inner =/= undefined]),
    Edges = lists:flatmap(fun(C) ->
        case C#pattern_composition.order of
            undefined when C#pattern_composition.inner =/= undefined ->
                [{C#pattern_composition.outer, C#pattern_composition.inner}];
            undefined -> [];
            Order -> lists:zip(Order, tl(Order ++ [hd(Order)]))
        end
    end, Compositions),
    dfs_cycle_detect(Nodes, Edges, [], []).

dfs_cycle_detect([], _, _, _) -> false;
dfs_cycle_detect([Node|Rest], Edges, Visited, Path) ->
    case lists:member(Node, Path) of
        true -> true;
        false ->
            Neighbors = [To || {From, To} <- Edges, From =:= Node],
            NewPath = [Node|Path],
            NewVisited = [Node|Visited],
            case dfs_cycle_detect(Neighbors -- NewVisited, Edges, NewVisited, NewPath) of
                true -> true;
                false -> dfs_cycle_detect(Rest, Edges, NewVisited, Path)
            end
    end.
```

---

## 7. Cross-Aspect Validation (CROSS)

| ID | Name | Severity | Description | Remediation |
|----|------|----------|-------------|-------------|
| CROSS-001 | Eta Compatible with Sigma | error | Refusals must not contradict type contracts | Adjust refusal or contract |
| CROSS-002 | Kappa Compatible with Lambda | error | Invariants must be compatible with patterns | Adjust invariant or pattern |
| CROSS-003 | Place Types Match | error | Place types must match Lambda instances | Fix place type reference |
| CROSS-004 | Critical Requirements Met | error | Critical profile requires comprehensive checks | Add missing checks |
| CROSS-005 | Receipt Fields Available | warning | Receipt fields should exist in tokens | Add fields to tokens |
| CROSS-006 | Gate Metrics Available | error | Gate metrics must be collectible | Fix metric reference |
| CROSS-007 | Handler Matches Refusal | error | Handler must match refusal signature | Fix handler signature |
| CROSS-008 | Narrowing Preserves Type | error | Narrowing must preserve base type | Fix narrowing rule |

### Validation Functions

```erlang
%% Validate Eta compatible with Sigma
-spec validate_eta_sigma_compatibility(#eta{}, #sigma{}) -> validation_result().
validate_eta_sigma_compatibility(Eta, Sigma) ->
    % Check that refusal guards don't contradict type contracts
    TypeSet = sets:from_list(maps:keys(Sigma#sigma.types)),
    Incompatible = lists:filter(fun(R) ->
        case R#refusal.guard of
            #guard{type = type_check, args = #{type := Type}} ->
                not sets:is_element(Type, TypeSet);
            _ -> false
        end
    end, Eta#eta.refusals),
    case Incompatible of
        [] -> ok;
        _ -> {error, ?REF("CROSS-001", "Refusals reference undefined types")}
    end.

%% Validate Kappa compatible with Lambda
-spec validate_kappa_lambda_compatibility(#kappa{}, #lambda{}) -> validation_result().
validate_kappa_lambda_compatibility(Kappa, Lambda) ->
    % Check that invariants reference valid places from Lambda
    InstancePlaces = lists:flatmap(fun(I) ->
        maps:keys(places_from_pattern_config(I#pattern_instance.pattern, I#pattern_instance.config))
    end, Lambda#lambda.instances),
    PlaceSet = sets:from_list(InstancePlaces),
    InvalidInvariants = lists:filter(fun(I) ->
        case I#invariant.predicate of
            #invariant_predicate{places = Places} when Places =/= undefined ->
                not lists:all(fun(P) -> sets:is_element(P, PlaceSet) end, Places);
            _ -> false
        end
    end, Kappa#kappa.invariants),
    case InvalidInvariants of
        [] -> ok;
        _ -> {error, ?REF("CROSS-002", "Invariants reference undefined places")}
    end.

%% Validate critical requirements
-spec validate_critical_requirements(#constitution_profile{}, #kappa{}, #eta{}) -> validation_result().
validate_critical_requirements(#constitution_profile{safety_level = critical}, Kappa, Eta) ->
    Checks = [
        {Kappa#kappa.receipts#receipt_config.enabled, orelse,
         {error, ?REF("CROSS-004", "Critical requires receipts enabled")}},
        {Kappa#kappa.verification#verification_config.static.enabled, orelse,
         {error, ?REF("CROSS-004", "Critical requires static verification")}},
        {Kappa#kappa.verification#verification_config.runtime.enabled, orelse,
         {error, ?REF("CROSS-004", "Critical requires runtime verification")}},
        {has_critical_refusal(Eta), orelse,
         {error, ?REF("CROSS-004", "Critical requires at least one critical refusal")}}
    ],
    validate_all(Checks);
validate_critical_requirements(_, _, _) -> ok.

has_critical_refusal(#eta{refusals = Refusals}) ->
    lists:any(fun(R) -> R#refusal.severity =:= critical end, Refusals).
```

---

## 8. Safety Level Constraints

### Permissive

- **Required**: REQ-001 through REQ-010 only
- **Optional**: All SIG, ETA, KAP, LAM rules are warnings only
- **Validation**: Basic structural validation only

### Standard

- **Required**: All REQ, SIG, ETA, KAP, LAM rules
- **Verification**: Runtime verification recommended
- **Receipts**: Enabled with SHA-256
- **Gates**: Basic execution gates

### Strict

- **Required**: All standard rules
- **Additional**: All cross-aspect validation (CROSS-001 through CROSS-008)
- **Verification**: Static + runtime verification required
- **Receipts**: SHA-256 or higher
- **Gates**: Comprehensive gate set
- **Handlers**: All refusals must have handlers

### Critical

- **Required**: All strict rules
- **Additional**: Formal proof requirements for soundness
- **Verification**: Full verification (static + runtime + periodic)
- **Receipts**: SHA-512, indefinite retention
- **Gates**: All gates with abort action
- **Handlers**: Critical refusals must have auto-remediation
- **Replay**: Deterministic, real-time replay capability

### Validation Matrix

| Rule | Permissive | Standard | Strict | Critical |
|------|-----------|----------|--------|----------|
| REQ-001 to REQ-010 | Error | Error | Error | Error |
| SIG-001 to SIG-012 | Warning | Error | Error | Error |
| ETA-001 to ETA-012 | Warning | Error | Error | Error |
| KAP-001 to KAP-011 | Warning | Error | Error | Error |
| LAM-001 to LAM-010 | Warning | Error | Error | Error |
| CROSS-001 to CROSS-008 | N/A | Warning | Error | Error |
| Formal Proofs | N/A | N/A | Warning | Error |

---

## 9. Validation Functions

### Main Validation Entry Point

```erlang
%% Main validation function
-spec validate_constitution(#ga_constitution{}) -> validation_result().
validate_constitution(Constitution) ->
    Profile = Constitution#ga_constitution.profile,

    % Run all validation phases
    Results = [
        validate_structural(Constitution),
        validate_sigma(Constitution#ga_constitution.sigma, Profile),
        validate_eta(Constitution#ga_constitution.eta, Profile),
        validate_kappa(Constitution#ga_constitution.kappa, Profile),
        validate_lambda(Constitution#ga_constitution.lambda, Profile),
        validate_cross_aspects(Constitution, Profile)
    ],

    % Collect errors and warnings
    {Errors, Warnings} = lists:foldl(fun
        (ok, {E, W}) -> {E, W};
        ({warning, Msg}, {E, W}) -> {E, [Msg|W]};
        ({error, Msg}, {E, W}) -> {[Msg|E], W}
    end, {[], []}, Results),

    case Errors of
        [] -> case Warnings of
            [] -> ok;
            _ -> {warning, Warnings}
        end;
        _ -> {error, Errors}
    end.

%% Validate structural requirements
validate_structural(#ga_constitution{id = Id, version = Version, name = Name, imports = Imports}) ->
    Checks = [
        validate_constitution_id(Id),
        validate_version(Version),
        case Name of
            undefined -> {error, ?REF("REQ-008", "Name cannot be undefined")};
            <<>> -> {error, ?REF("REQ-008", "Name cannot be empty")};
            _ when byte_size(Name) > 256 -> {warning, ?REF("REQ-009", "Name exceeds 256 characters")};
            _ -> ok
        end,
        validate_unique_import_aliases(Imports)
    ],
    validate_all(Checks).

%% Validate sigma based on profile
validate_sigma(Sigma, #constitution_profile{safety_level = Level}) ->
    Checks = [
        validate_unique_types(maps:values(Sigma#sigma.types)),
        validate_unions_have_discriminant(maps:values(Sigma#sigma.types)),
        validate_contract_types_exist(Sigma#sigma.token_contracts, Sigma#sigma.types),
        validate_cardinalities(Sigma#sigma.place_types)
    ],
    case Level of
        permissive -> {warning, validate_all(Checks)};
        _ -> validate_all(Checks)
    end.

%% Validate eta based on profile
validate_eta(Eta, #constitution_profile{safety_level = Level}) ->
    Checks = [
        validate_unique_refusals(Eta#eta.refusals),
        validate_critical_remediations(Eta#eta.refusals),
        validate_composition_references(Eta#eta.compositions, Eta#eta.refusals)
    ],
    case Level of
        permissive -> {warning, validate_all(Checks)};
        _ -> validate_all(Checks)
    end.

%% Validate kappa based on profile
validate_kappa(Kappa, #constitution_profile{safety_level = Level}) ->
    Checks = [
        validate_unique_invariants(Kappa#kappa.invariants),
        validate_retention_policies([Kappa#kappa.receipts#receipt_config.retention]),
        validate_gate_metrics(Kappa#kappa.gates)
    ],
    CriticalCheck = validate_critical_receipts(
        #constitution_profile{safety_level = Level},
        Kappa#kappa.receipts
    ),
    validate_all(lists:flatten([Checks, [CriticalCheck]])).

%% Validate lambda based on profile
validate_lambda(Lambda, #constitution_profile{safety_level = Level}) ->
    Checks = [
        validate_unique_instances(Lambda#lambda.instances),
        validate_topology_edges(Lambda#lambda.topology),
        validate_acyclic(Lambda#lambda.compositions)
    ],
    case Level of
        permissive -> {warning, validate_all(Checks)};
        _ -> validate_all(Checks)
    end.

%% Validate cross-aspects based on profile
validate_cross_aspects(#ga_constitution{sigma = Sigma, eta = Eta, kappa = Kappa, lambda = Lambda}, Profile) ->
    case Profile#constitution_profile.safety_level of
        permissive -> ok;
        strict -> validate_all([
            validate_eta_sigma_compatibility(Eta, Sigma),
            validate_kappa_lambda_compatibility(Kappa, Lambda),
            validate_place_types_match(Sigma, Lambda)
        ]);
        critical -> validate_all([
            validate_eta_sigma_compatibility(Eta, Sigma),
            validate_kappa_lambda_compatibility(Kappa, Lambda),
            validate_place_types_match(Sigma, Lambda),
            validate_critical_requirements(Profile, Kappa, Eta)
        ]);
        _ -> {warning, validate_all([
            validate_eta_sigma_compatibility(Eta, Sigma),
            validate_kappa_lambda_compatibility(Kappa, Lambda)
        ])}
    end.

%% Helper: validate all checks, stopping at first error
validate_all([]) -> ok;
validate_all([ok|Rest]) -> validate_all(Rest);
validate_all([{warning, _} = W|Rest]) ->
    case validate_all(Rest) of
        {error, E} -> {error, E};
        {warning, Ws} -> {warning, [W|Ws]};
        ok -> {warning, [W]}
    end;
validate_all([{error, _} = E|_]) -> E.

%% Helper: create validation result
?REF(Rule, Msg) -> {error, {Rule, Msg}}.
```

---

## Appendix: Error Codes Reference

| Code | Category | Message Format |
|------|----------|----------------|
| REQ-001 | Structural | `Invalid constitution ID format` |
| SIG-001 | Typing | `Duplicate type names found` |
| ETA-001 | Refusals | `Duplicate refusal IDs found` |
| KAP-001 | Quality | `Duplicate invariant IDs found` |
| LAM-001 | Pattern | `Duplicate pattern instance IDs found` |
| CROSS-001 | Cross-aspect | `Refusals reference undefined types` |

---

**End of Validation Rules**
