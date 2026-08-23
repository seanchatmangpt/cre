# PNet Types Tutorial

## Getting Started with PNet Types

This tutorial will guide you through using the `pnet_types` module in the CRE workflow engine. You'll learn about type definitions, validation functions, and how to integrate them into your workflow definitions.

## Prerequisites

Before starting this tutorial, make sure you have:

1. Understanding of basic Erlang syntax
2. Knowledge of Petri nets fundamentals
3. Access to the CRE workflow engine
4. Basic understanding of Erlang maps and types

## Tutorial Overview

1. **Basic Types Introduction** - Understanding place, trsn, and token types
2. **State Types Deep Dive** - Working with marking, mode, and consumption maps
3. **Colored Petri Nets** - Using variables and bindings in colored nets
4. **Validation Functions** - Learning to use the validation functions
5. **Integration Patterns** - Connecting types with actual workflow execution
6. **Advanced Examples** - Complex workflow scenarios

## 1. Basic Types Introduction

### Understanding Place Types

Places are the nodes in your Petri net where tokens can reside.

```erlang
% Define some places for our workflow
StartPlace = start,
EndPlace = end,
DataPlace = data,
ApprovalPlace = approved.

% Create a simple marking - where tokens are currently located
InitialMarking = #{StartPlace => [init],
                   DataPlace => [],
                   ApprovalPlace => []}.

% Validate the marking
case pnet_types:is_marking(InitialMarking) of
    true ->
        io:format("Valid marking: ~p~n", [InitialMarking]);
    false ->
        io:format("Invalid marking!~n")
end.
```

### Transition Types

Transitions are the active elements that consume tokens and produce new ones.

```erlang
% Define transitions for our workflow
ProcessData = process_data,
RequestApproval = request_approval,
CompleteWorkflow = complete.

% A move represents a transition firing
Move = #{trsn => ProcessData,
         mode => #{StartPlace => [init]},
         produce => #{DataPlace => [processed_data]}}.

% Validate the move structure
if pnet_types:is_move(Move) ->
    io:format("Valid move: ~p~n", [Move]);
    true ->
    ok
end.
```

### Token Types

Tokens can be any Erlang term, giving you flexibility in your data flow.

```erlang
% Different types of tokens
IntegerToken = 1,
StringToken = "data",
MapToken = #{type => document, id => "doc123"},
ListToken = [item1, item2],
ComplexToken = #{user => #{id => 1, name => "Alice"}, data => [1,2,3]}.

% Create a marking with various token types
ComplexMarking = #{input => [ComplexToken],
                   buffer => [MapToken, StringToken],
                   output => []}.

% Validate the complex marking
if pnet_types:is_marking(ComplexMarking) ->
    io:format("Complex marking validated~n");
    true ->
    ok
end.
```

## 2. State Types Deep Dive

### Working with Markings

Markings represent the current state of your workflow. Let's explore some examples:

```erlang
% Example 1: Simple document workflow
DocumentMarking = #{inbox => [new_doc],
                   review => [],
                   approved => [],
                   archive => []}.

% Example 2: Order processing workflow
OrderMarking = #{orders => [order123, order456],
                 processing => [],
                 completed => [],
                 shipped => []}.

% Example 3: Empty workflow (no tokens)
EmptyMarking = #{start => [],
                 process => [],
                 end => []}.

% Validate all markings
ValidMarkings = [DocumentMarking, OrderMarking, EmptyMarking],
lists:foreach(fun(M) ->
    case pnet_types:is_marking(M) of
        true -> io:format("✓ Valid marking: ~p~n", [M]);
        false -> io:format("✗ Invalid marking: ~p~n", [M])
    end
end, ValidMarkings).
```

### Understanding Modes

Modes define what tokens are needed to enable a transition:

```erlang
% Transition: Process document
% Mode: Needs a document in inbox and no review in process
ProcessDocumentMode = #{inbox => [new_doc],
                       review => []}.

% Transition: Approve document
% Mode: Needs a document in review and no approval
ApproveDocumentMode = #{review => [reviewing],
                        approved => []}.

% Transition: Archive document
% Mode: Needs approved document in approved place
ArchiveDocumentMode = #{approved => [approved]}.

% Validate all modes
ValidModes = [ProcessDocumentMode, ApproveDocumentMode, ArchiveDocumentMode],
lists:foreach(fun(M) ->
    case pnet_types:is_mode(M) of
        true -> io:format("✓ Valid mode: ~p~n", [M]);
        false -> io:format("✗ Invalid mode: ~p~n", [M])
    end
end, ValidModes).
```

### Consumption and Production Maps

These maps specify exactly which tokens to consume and produce:

```erlang
% Transition: Process a document
% Consume: new_doc from inbox
% Produce: reviewing in review
ProcessDocument = #{consume => #{inbox => [new_doc]},
                   produce => #{review => [reviewing]}}.

% Transition: Approve a document
% Consume: reviewing from review
% Produce: approved in approved
ApproveDocument = #{consume => #{review => [reviewing]},
                   produce => #{approved => [approved]}}.

% Transition: Archive a document
% Consume: approved from approved
% Produce: nothing
ArchiveDocument = #{consume => #{approved => [approved]},
                   produce => #{}}.

% Validate consumption maps
ConsumptionMaps = [ProcessDocument#consume, ApproveDocument#consume, ArchiveDocument#consume],
lists:foreach(fun(C) ->
    case pnet_types:is_consume_map(C) of
        true -> io:format("✓ Valid consume map: ~p~n", [C]);
        false -> io:format("✗ Invalid consume map: ~p~n", [C])
    end
end, ConsumptionMaps).

% Validate production maps
ProductionMaps = [ProcessDocument#produce, ApproveDocument#produce, ArchiveDocument#produce],
lists:foreach(fun(P) ->
    case pnet_types:is_produce_map(P) of
        true -> io:format("✓ Valid produce map: ~p~n", [P]);
        false -> io:format("✗ Invalid produce map: ~p~n", [P])
    end
end, ProductionMaps).
```

## 3. Colored Petri Nets

### Variable and Binding Types

Colored Petri nets allow you to work with typed tokens using variables:

```erlang
% Define variables for colored tokens
DocumentId = doc_id,
DocumentType = doc_type,
UserId = user_id,
UserName = user_name.

% Create bindings - mapping variables to actual values
DocumentBinding = #{DocumentId => "DOC-123",
                     DocumentType => "invoice"},
UserBinding = #{UserId => 42,
                UserName => "John Doe"}.

% Validate bindings
ValidBindings = [DocumentBinding, UserBinding],
lists:foreach(fun(B) ->
    case pnet_types:is_binding(B) of
        true -> io:format("✓ Valid binding: ~p~n", [B]);
        false -> io:format("✗ Invalid binding: ~p~n", [B])
    end
end, ValidBindings).
```

### Colored Modes (cMode)

Colored modes combine bindings with token modes:

```erlang
% Example 1: Document processing with type checking
DocumentProcessing = {
    #{DocumentType => "invoice"},
    #{input => [DocumentId],
      review => []}
}.

% Example 2: User approval with user context
UserApproval = {
    #{UserId => 42, UserName => "John Doe"},
    #{review => [reviewing],
      approval => []}
}.

% Example 3: Multiple variables
ComplexProcessing = {
    #{DocumentId => "DOC-123", DocumentType => "invoice", UserId => 42},
    #{input => [DocumentId],
      assigned_to => [UserId],
      review => []}
}.

% Validate colored modes
ColoredModes = [DocumentProcessing, UserApproval, ComplexProcessing],
lists:foreach(fun(C) ->
    case pnet_types:is_cmode(C) of
        true -> io:format("✓ Valid colored mode: ~p~n", [C]);
        false -> io:format("✗ Invalid colored mode: ~p~n", [C])
    end
end, ColoredModes).
```

## 4. Validation Functions

### Building a Validation Framework

Let's create a comprehensive validation system for our workflow:

```erlang
% Workflow definition structure
-record(workflow_net, {
    name,
    places,
    transitions,
    markings,
    modes,
    consume_maps,
    produce_maps,
    colored_modes
}).

% Validate a complete workflow definition
validate_workflow(Net) ->
    % Validate places (should be atoms)
    ValidPlaces = lists:all(fun is_atom/1, Net#workflow_net.places),

    % Validate markings
    ValidMarkings = lists:all(fun pnet_types:is_marking/1,
                              maps:values(Net#workflow_net.markings)),

    % Validate modes
    ValidModes = lists:all(fun pnet_types:is_mode/1,
                           maps:values(Net#workflow_net.modes)),

    % Validate consume maps
    ValidConsumes = lists:all(fun pnet_types:is_consume_map/1,
                              maps:values(Net#workflow_net.consume_maps)),

    % Validate produce maps
    ValidProduces = lists:all(fun pnet_types:is_produce_map/1,
                              maps:values(Net#workflow_net.produce_maps)),

    % Validate colored modes
    ValidColoredModes = lists:all(fun pnet_types:is_cmode/1,
                                  maps:values(Net#workflow_net.colored_modes)),

    % Return validation results
    #{valid => ValidPlaces and ValidMarkings and ValidModes and
              ValidConsumes and ValidProduces and ValidColoredModes,
      issues => collect_issues(Net, ValidPlaces, ValidMarkings, ValidModes,
                             ValidConsumes, ValidProduces, ValidColoredModes)}.
```

### Error Collection and Reporting

```erlang
% Helper function to collect validation issues
collect_issues(Net, ValidPlaces, ValidMarkings, ValidModes,
               ValidConsumes, ValidProduces, ValidColoredModes) ->
    Issues = [],

    % Check places
    case ValidPlaces of
        false -> [invalid_places | Issues];
        true -> Issues
    end,

    % Check markings
    case ValidMarkings of
        false -> [invalid_markings | Issues];
        true -> Issues
    end,

    % Check modes
    case ValidModes of
        false -> [invalid_modes | Issues];
        true -> Issues
    end,

    % Check consume maps
    case ValidConsumes of
        false -> [invalid_consume_maps | Issues];
        true -> Issues
    end,

    % Check produce maps
    case ValidProduces of
        false -> [invalid_produce_maps | Issues];
        true -> Issues
    end,

    % Check colored modes
    case ValidColoredModes of
        false -> [invalid_colored_modes | Issues];
        true -> Issues
    end.
```

## 5. Integration Patterns

### Connecting with gen_pnet

Let's integrate our types with the actual workflow engine:

```erlang
% Define a simple workflow using pnet_types
define_simple_workflow() ->
    % Define workflow structure
    #workflow_net{
        name => simple_approval,
        places => [start, review, approved, end],
        transitions => [start_review, approve, complete],
        markings => #{
            initial => #{start => [init], review => [], approved => [], end => []}
        },
        modes => #{
            start_review => #{start => [init]},
            approve => #{review => [reviewing]},
            complete => #{approved => [approved]}
        },
        consume_maps => #{
            start_review => #{start => [init]},
            approve => #{review => [reviewing]},
            complete => #{approved => [approved]}
        },
        produce_maps => #{
            start_review => #{review => [reviewing]},
            approve => #{approved => [approved]},
            complete => #{end => [complete]}
        }
    }.

% Start workflow with validation
start_workflow_with_validation(CaseId) ->
    Workflow = define_simple_workflow(),

    case validate_workflow(Workflow) of
        #{valid := true} ->
            % Get initial marking
            InitialMarking = Workflow#workflow_net.markings.initial,

            % Validate before starting
            case pnet_types:is_marking(InitialMarking) of
                true ->
                    gen_pnet:start_workflow(CaseId, InitialMarking),
                    io:format("Workflow started: ~p~n", [CaseId]);
                false ->
                    io:format("Invalid initial marking!~n")
            end;
        #{valid := false, issues := Issues} ->
            io:format("Workflow validation failed: ~p~n", [Issues])
    end.
```

### Building a Transition Validator

```erlang
% Check if a transition can be fired
can_fire_transition(CaseId, TransitionName) ->
    % Get current marking
    CurrentMarking = gen_pnet:get_marking(CaseId),

    % Get transition mode
    TransitionMode = get_transition_mode(TransitionName),

    % Validate both types
    case pnet_types:is_marking(CurrentMarking) andalso
         pnet_types:is_mode(TransitionMode) of
        false ->
            false;  % Invalid types, cannot fire
        true ->
            % Check if mode is satisfied by marking
            is_mode_satisfied(TransitionMode, CurrentMarking)
    end.

% Execute transition with validation
fire_transition_safe(CaseId, Move) ->
    case pnet_types:is_move(Move) of
        true ->
            % Move is valid, proceed with execution
            gen_pnet:fire_transition(CaseId, Move);
        false ->
            % Invalid move, handle error
            io:format("Invalid move structure: ~p~n", [Move]),
            {error, invalid_move}
    end.
```

## 6. Advanced Examples

### Complex Document Workflow

```erlang
% Define a complex document approval workflow
define_document_workflow() ->
    #workflow_net{
        name => document_approval,
        places => [inbox, triage, review, approval, rejected, published, archive],
        transitions => [triage, review, approve, reject, publish, archive],
        markings => #{
            initial => #{inbox => [doc1, doc2], triage => [], review => [],
                       approval => [], rejected => [], published => [], archive => []}
        },
        modes => #{
            triage => #{inbox => [doc1], review => [], approval => []},
            review => #{triage => [triaged], approval => []},
            approve => #{review => [reviewed], approval => []},
            reject => #{review => [reviewed], rejected => []},
            publish => #{approval => [approved], published => []},
            archive => #{published => [published], archive => []}
        },
        consume_maps => #{
            triage => #{inbox => [doc1]},
            review => #{triage => [triaged]},
            approve => #{review => [reviewed]},
            reject => #{review => [reviewed]},
            publish => #{approval => [approved]},
            archive => #{published => [published]}
        },
        produce_maps => #{
            triage => #{triage => [triaged]},
            review => #{review => [reviewing]},
            approve => #{approval => [approved]},
            reject => #{rejected => [rejected]},
            publish => #{published => [published]},
            archive => #{archive => [archived]}
        }
    }.

% Workflow simulation with validation
simulate_document_workflow() ->
    Workflow = define_document_workflow(),

    % Validate the entire workflow definition
    case validate_workflow(Workflow) of
        #{valid := true} ->
            io:format("✓ Document workflow is valid~n"),

            % Start workflow
            CaseId = "doc_workflow_001",
            gen_pnet:start_workflow(CaseId, Workflow#workflow_net.markings.initial),

            % Simulate some transitions
            TriageMove = #{trsn => triage, mode => Workflow#workflow_net.modes.triage,
                          produce => Workflow#workflow_net.produce_maps.triage},

            case pnet_types:is_move(TriageMove) of
                true ->
                    gen_pnet:fire_transition(CaseId, TriageMove),
                    io:format("✓ Triage transition executed~n");
                false ->
                    io:format("✗ Invalid triage move~n")
            end;
        #{valid := false, issues := Issues} ->
            io:format("✗ Document workflow has issues: ~p~n", [Issues])
    end.
```

### Multi-User Approval System

```erlang
% Colored Petri net for multi-user approval
define_multi_user_workflow() ->
    #workflow_net{
        name => multi_user_approval,
        places => [request, assigned, review, approved, rejected],
        transitions => [assign, review, approve, reject],
        markings => #{
            initial => #{request => [#{doc_id => "DOC-123", doc_type => "invoice"}],
                       assigned => [], review => [], approved => [], rejected => []}
        },
        colored_modes => #{
            assign => {
                #{user_id => 42, user_name => "Alice"},
                #{request => [#{doc_id => "DOC-123"}], assigned => []}
            },
            review => {
                #{user_id => 42, user_name => "Alice"},
                #{assigned => [#{doc_id => "DOC-123", user_id => 42}],
                  review => []}
            },
            approve => {
                #{user_id => 42, user_name => "Alice"},
                #{review => [#{doc_id => "DOC-123", user_id => 42}],
                  approved => []}
            },
            reject => {
                #{user_id => 42, user_name => "Alice"},
                #{review => [#{doc_id => "DOC-123", user_id => 42}],
                  rejected => []}
            }
        },
        consume_maps => #{
            assign => #{request => [#{doc_id => "DOC-123"}]},
            review => #{assigned => [#{doc_id => "DOC-123", user_id => 42}]},
            approve => #{review => [#{doc_id => "DOC-123", user_id => 42}]},
            reject => #{review => [#{doc_id => "DOC-123", user_id => 42}]}
        },
        produce_maps => #{
            assign => #{assigned => [#{doc_id => "DOC-123", user_id => 42}]},
            review => #{review => [#{doc_id => "DOC-123", status => "under_review"}]},
            approve => #{approved => [#{doc_id => "DOC-123", status => "approved"}]},
            reject => #{rejected => [#{doc_id => "DOC-123", status => "rejected"}]}
        }
    }.

% Multi-user workflow validation
validate_multi_user_workflow() ->
    Workflow = define_multi_user_workflow(),

    case validate_workflow(Workflow) of
        #{valid := true} ->
            io:format("✓ Multi-user workflow is valid~n"),

            % Validate colored modes specifically
            ColoredModes = maps:values(Workflow#workflow_net.colored_modes),
            lists:foreach(fun(CM) ->
                case pnet_types:is_cmode(CM) of
                    true ->
                        io:format("✓ Valid colored mode: ~p~n", [CM]);
                    false ->
                        io:format("✗ Invalid colored mode: ~p~n", [CM])
                end
            end, ColoredModes);
        #{valid := false, issues := Issues} ->
            io:format("✗ Multi-user workflow issues: ~p~n", [Issues])
    end.
```

## Best Practices

### 1. Always Validate Before Processing

```erlang
% Good practice
execute_transition(Move) ->
    case pnet_types:is_move(Move) of
        true -> gen_pnet:fire_transition(Move);
        false -> handle_invalid_move(Move)
    end.
```

### 2. Use Types in Guards

```erlang
% Safe to use in guards due to total functions
safe_transition_check(Mode, Marking) when
    pnet_types:is_mode(Mode),
    pnet_types:is_marking(Marking) ->
    check_enablement(Mode, Marking).
```

### 3. Comprehensive Error Handling

```erlang
% Handle all validation cases
start_workflow_safely(WorkflowDef) ->
    case validate_workflow(WorkflowDef) of
        #{valid := true} ->
            proceed_with_workflow(WorkflowDef);
        #{valid := false, issues := Issues} ->
            handle_validation_errors(Issues)
    end.
```

### 4. Maintain Type Consistency

```erlang
% Ensure consistency across workflow definition
ensure_type_consistency(Net) ->
    % All places must be atoms
    Places = Net#workflow_net.places,
    case lists:all(fun is_atom/1, Places) of
        false -> throw(invalid_places);
        true -> ok
    end,

    % All markings must be valid
    Markings = maps:values(Net#workflow_net.markings),
    case lists:all(fun pnet_types:is_marking/1, Markings) of
        false -> throw(invalid_markings);
        true -> ok
    end.
```

## Conclusion

This tutorial has covered the essential aspects of using the `pnet_types` module in the CRE workflow engine. You've learned about:

1. **Basic Types**: place, trsn, token
2. **State Types**: marking, mode, consume_map, produce_map
3. **Colored Types**: var, binding, cmode
4. **Validation Functions**: Total functions for safe type checking
5. **Integration Patterns**: Connecting types with workflow execution
6. **Advanced Examples**: Complex workflow scenarios

Remember that the `pnet_types` module provides total validation functions that never crash, making them safe to use in guards and throughout your workflow system. Always validate your workflow definitions before execution to ensure type safety and correctness.

## Next Steps

1. Experiment with different workflow patterns
2. Create your own validation functions
3. Integrate with the full CRE workflow engine
4. Explore more complex Petri net patterns
5. Read the complete API reference for additional details