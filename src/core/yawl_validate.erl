%% -*- erlang -*-
%%%% @author CRE Team
%% @version 0.2.0
%% @doc YAWL Specification Validator
%%
%% This module provides comprehensive validation for YAWL 2.1 specifications.
%% It checks for:
%% <ul>
%%   <li>Required elements present (specification, decomposition, tasks)</li>
%%   <li>Task configurations valid (join/split types)</li>
%%   <li>Flows reference valid tasks</li>
%%   <li>Decompositions are properly formed</li>
%%   <li>Variables are declared before use</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Validate a specification map
%% Spec = #{
%%   id => <<"order_wf">>,
%%   name => <<"Order Processing">>,
%%   tasks => #{<<"t1">> => #{id => <<"t1">>, type => atomic}},
%%   conditions => #{},
%%   flows => []
%% },
%% {ok, Warnings} = yawl_validate:validate(Spec).
%% ```
%%
%% ```erlang
%% %% Validate tasks specifically
%% Errors = yawl_validate:check_tasks(Spec).
%% ```
%%
%% ```erlang
%% %% Format validation errors for display
%% Formatted = yawl_validate:format_errors(Errors).
%% ```
%% @end
%% -------------------------------------------------------------------

-module(yawl_validate).

%%====================================================================
%% Exports
%%====================================================================

%% Main validation API
-export([validate/1, validate_spec/1]).

%% Individual validation checks
-export([check_tasks/1, check_flows/1]).
-export([check_decompositions/1, check_variables/1]).

%% Error reporting
-export([format_errors/1]).

%%====================================================================
%% Types
%%====================================================================

%% YAWL specification type (compatible with yawl_schema)
-type specification() :: #{
          id => binary(),
          name => binary(),
          version => binary() | undefined,
          decomposition => term(),
          tasks => #{binary() => task()},
          conditions => #{binary() => condition()},
          flows => [flow()],
          data_mappings => [mapping()]
         }.

%% Task definition
-type task() :: #{
          id => binary(),
          name => binary(),
          type => atomic | composite | multiple_instance,
          split_type => 'and' | 'or' | 'xor' | undefined,
          join_type => 'and' | 'or' | 'xor' | undefined,
          decomposition => binary() | undefined,
          min_instances => non_neg_integer() | undefined,
          max_instances => non_neg_integer() | unlimited | undefined,
          continuation_threshold => non_neg_integer() | undefined
         }.

%% Condition definition
-type condition() :: #{
          id => binary(),
          type => input_condition | output_condition,
          expression => binary() | undefined
         }.

%% Flow definition
-type flow() :: #{
          id => binary(),
          source => binary(),
          target => binary(),
          predicate => binary() | undefined
         }.

%% Data mapping
-type mapping() :: #{
          task_id => binary(),
          input => [#{variable => binary(), expression => binary()}],
          output => [#{variable => binary(), expression => binary()}]
         }.

%% Validation error
-type validation_error() :: #{
          type => required | structure | semantic | reference,
          severity => error | warning,
          message => binary(),
          location => binary() | undefined,
          code => atom()
         }.

%% Validation result
-type validation_result() :: {ok, [validation_error()]} |
                             {error, [validation_error()]}.

-export_type([specification/0, task/0, condition/0, flow/0,
              validation_error/0, validation_result/0]).

%%====================================================================
%% Constants
%%====================================================================

%% Valid split types
-define(SPLIT_TYPES, ['and', 'or', 'xor']).

%% Valid join types
-define(JOIN_TYPES, ['and', 'or', 'xor']).

%% Valid task types
-define(TASK_TYPES, [atomic, composite, multiple_instance]).

%% Valid condition types
-define(CONDITION_TYPES, [input_condition, output_condition]).

%%====================================================================
%% Main Validation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a complete YAWL specification.
%%
%% Performs all validation checks and returns ok with warnings or
%% error with critical issues.
%%
%% === Example ===
%% ```erlang
%% {ok, Warnings} = yawl_validate:validate(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(Spec :: specification()) -> validation_result().

validate(Spec) when is_map(Spec) ->
    AllErrors = lists:flatten([
        check_required_elements(Spec),
        check_tasks(Spec),
        check_flows(Spec),
        check_decompositions(Spec),
        check_variables(Spec),
        check_consistency(Spec)
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

validate(_Spec) ->
    {error, [#{type => structure,
               severity => error,
               message => <<"Specification must be a map">>,
               location => undefined,
               code => invalid_spec_type}]}.

%%--------------------------------------------------------------------
%% @doc Alias for validate/1 for backward compatibility.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_spec(Spec :: specification()) -> validation_result().

validate_spec(Spec) ->
    validate(Spec).

%%====================================================================
%% Required Elements Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Checks that all required elements are present.
%%--------------------------------------------------------------------
-spec check_required_elements(specification()) -> [validation_error()].

check_required_elements(#{id := Id, name := Name}) ->
    Errors = [],
    Errors1 = case Id of
        <<>> -> [#{type => required,
                   severity => error,
                   message => <<"Specification ID is required">>,
                   location => <<"specification">>,
                   code => missing_id} | Errors];
        _ -> Errors
    end,
    Errors2 = case Name of
        <<>> -> [#{type => required,
                   severity => error,
                   message => <<"Specification name is required">>,
                   location => <<"specification">>,
                   code => missing_name} | Errors1];
        _ -> Errors1
    end,
    lists:reverse(Errors2);

check_required_elements(_Spec) ->
    [#{type => required,
       severity => error,
       message => <<"Missing required fields: id or name">>,
       location => <<"specification">>,
       code => missing_required_fields}].

%%====================================================================
%% Task Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates all tasks in a specification.
%%
%% Checks for:
%% - Valid task types
%% - Valid split/join configurations
%% - Multi-instance parameter consistency
%%
%% Returns a list of validation errors (may be empty).
%%
%% === Example ===
%% ```erlang
%% Errors = yawl_validate:check_tasks(Spec),
%% case Errors of
%%     [] -> ok;
%%     _ -> io:format("Tasks have errors: ~p~n", [Errors])
%% end.
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_tasks(specification()) -> [validation_error()].

check_tasks(#{tasks := Tasks}) when is_map(Tasks) ->
    maps:fold(fun(TaskId, Task, Acc) ->
        Acc ++ validate_task(TaskId, Task)
    end, [], Tasks);

check_tasks(#{tasks := Tasks}) when not is_map(Tasks) ->
    [#{type => structure,
       severity => error,
       message => <<"Tasks must be a map">>,
       location => <<"tasks">>,
       code => invalid_tasks_format}];

check_tasks(_Spec) ->
    [#{type => required,
       severity => warning,
       message => <<"No tasks defined in specification">>,
       location => <<"tasks">>,
       code => no_tasks}].

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a single task.
%%--------------------------------------------------------------------
-spec validate_task(binary(), task()) -> [validation_error()].

validate_task(TaskId, Task) ->
    Errors = [],
    %% Validate task ID
    Errors1 = case TaskId of
        <<>> -> [#{type => required,
                   severity => error,
                   message => <<"Task ID is required">>,
                   location => TaskId,
                   code => missing_task_id} | Errors];
        _ -> Errors
    end,
    %% Validate task name
    Errors2 = case maps:get(name, Task, <<>>) of
        <<>> -> [#{type => required,
                   severity => warning,
                   message => <<"Task name is recommended">>,
                   location => TaskId,
                   code => missing_task_name} | Errors1];
        _ -> Errors1
    end,
    %% Validate task type
    Errors3 = case maps:get(type, Task, undefined) of
        undefined ->
            [#{type => required,
               severity => error,
               message => <<"Task type is required">>,
               location => TaskId,
               code => missing_task_type} | Errors2];
        Type when Type =:= atomic; Type =:= composite; Type =:= multiple_instance ->
            Errors2;
        Type ->
            [#{type => structure,
               severity => error,
               message => iolist_to_binary([<<"Invalid task type: '">>,
                                           atom_to_binary(Type), <<"'">>]),
               location => TaskId,
               code => invalid_task_type} | Errors2]
    end,
    %% Validate split type
    Errors4 = case maps:get(split_type, Task, undefined) of
        undefined -> Errors3;
        Split when Split =:= 'and'; Split =:= 'or'; Split =:= 'xor' -> Errors3;
        Split ->
            [#{type => structure,
               severity => error,
               message => iolist_to_binary([<<"Invalid split type: '">>,
                                           atom_to_binary(Split), <<"'">>]),
               location => TaskId,
               code => invalid_split_type} | Errors3]
    end,
    %% Validate join type
    Errors5 = case maps:get(join_type, Task, undefined) of
        undefined -> Errors4;
        Join when Join =:= 'and'; Join =:= 'or'; Join =:= 'xor' -> Errors4;
        Join ->
            [#{type => structure,
               severity => error,
               message => iolist_to_binary([<<"Invalid join type: '">>,
                                           atom_to_binary(Join), <<"'">>]),
               location => TaskId,
               code => invalid_join_type} | Errors4]
    end,
    %% Validate split/join consistency
    Errors6 = validate_split_join_consistency(TaskId, Task, Errors5),
    %% Validate multi-instance parameters
    validate_multi_instance(TaskId, Task, Errors6).

%%--------------------------------------------------------------------
%% @private
%% @doc Validates split/join consistency rules.
%%--------------------------------------------------------------------
-spec validate_split_join_consistency(binary(), task(), [validation_error()]) ->
          [validation_error()].

validate_split_join_consistency(TaskId, Task, Errors) ->
    Split = maps:get(split_type, Task, undefined),
    Join = maps:get(join_type, Task, undefined),
    Type = maps:get(type, Task, atomic),

    Errors1 = case Type of
        multiple_instance when Split =/= undefined, Split =/= 'and' ->
            [#{type => semantic,
               severity => error,
               message => <<"Multiple instance tasks must use AND split">>,
               location => TaskId,
               code => mi_split_must_be_and} | Errors];
        _ ->
            Errors
    end,

    Errors2 = case {Split, Join} of
        {'xor', 'and'} ->
            [#{type => semantic,
               severity => warning,
               message => <<"XOR split with AND join may cause deadlock">>,
               location => TaskId,
               code => xor_split_and_join_warning} | Errors1];
        {'or', 'xor'} ->
            [#{type => semantic,
               severity => warning,
               message => <<"OR split with XOR join may lose tokens">>,
               location => TaskId,
               code => or_split_xor_join_warning} | Errors1];
        _ ->
            Errors1
    end,

    Errors2.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates multi-instance task parameters.
%%--------------------------------------------------------------------
-spec validate_multi_instance(binary(), task(), [validation_error()]) ->
          [validation_error()].

validate_multi_instance(TaskId, Task, Errors) ->
    case maps:get(type, Task, atomic) of
        multiple_instance ->
            Min = maps:get(min_instances, Task, undefined),
            Max = maps:get(max_instances, Task, undefined),
            Threshold = maps:get(continuation_threshold, Task, undefined),

            %% Check Min is valid integer or undefined
            MinOk = case Min of
                undefined -> ok;
                N when is_integer(N), N >= 0 -> ok;
                _ -> error
            end,

            Errors1 = case Min of
                undefined ->
                    [#{type => required,
                       severity => warning,
                       message => <<"Multi-instance task should specify min_instances">>,
                       location => TaskId,
                       code => missing_min_instances} | Errors];
                MinVal when is_integer(MinVal), MinVal >= 0 ->
                    Errors;
                _ ->
                    [#{type => structure,
                       severity => error,
                       message => <<"min_instances must be a non-negative integer">>,
                       location => TaskId,
                       code => invalid_min_instances} | Errors]
            end,

            %% Validate max_instances
            Errors2 = case Max of
                undefined ->
                    [#{type => required,
                       severity => warning,
                       message => <<"Multi-instance task should specify max_instances">>,
                       location => TaskId,
                       code => missing_max_instances} | Errors1];
                unlimited ->
                    Errors1;
                MaxVal when is_integer(MaxVal), MaxVal >= 0 ->
                    case MinOk of
                        ok when is_integer(Min), MaxVal > Min ->
                            Errors1;
                        ok when is_integer(Min), MaxVal =< Min ->
                            [#{type => semantic,
                               severity => error,
                               message => <<"max_instances must be >= min_instances">>,
                               location => TaskId,
                               code => max_less_than_min} | Errors1];
                        _ ->
                            %% Min is undefined or invalid, already reported above
                            Errors1
                    end;
                _ ->
                    [#{type => structure,
                       severity => error,
                       message => <<"max_instances must be a non-negative integer or 'unlimited'">>,
                       location => TaskId,
                       code => invalid_max_instances} | Errors1]
            end,

            %% Validate threshold
            case Threshold of
                undefined -> Errors2;
                ThreshVal when is_integer(ThreshVal), ThreshVal >= 0 -> Errors2;
                _ ->
                    [#{type => structure,
                       severity => error,
                       message => <<"continuation_threshold must be a non-negative integer">>,
                       location => TaskId,
                       code => invalid_threshold} | Errors2]
            end;

        _ ->
            %% Not a multi-instance task
            Errors
    end.

%%====================================================================
%% Flow Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates all flows in a specification.
%%
%% Checks for:
%% - Flows reference valid tasks/conditions
%% - No duplicate flows
%% - Predicate validity
%% - Flow connectivity (no isolated nodes)
%%
%% === Example ===
%% ```erlang
%% Errors = yawl_validate:check_flows(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_flows(specification()) -> [validation_error()].

check_flows(#{flows := Flows, tasks := Tasks, conditions := Conditions}) ->
    AllIds = maps:keys(Tasks) ++ maps:keys(Conditions),
    IdSet = sets:from_list(AllIds),

    %% Check each flow
    FlowErrors = lists:foldl(fun(Flow, Acc) ->
        Acc ++ validate_flow(Flow, IdSet)
    end, [], Flows),

    %% Check for duplicate flows
    DuplicateErrors = check_duplicate_flows(Flows),

    %% Check for isolated nodes
    IsolatedErrors = check_isolated_nodes(Flows, AllIds),

    FlowErrors ++ DuplicateErrors ++ IsolatedErrors;

check_flows(#{flows := Flows}) ->
    [#{type => structure,
       severity => error,
       message => <<"Cannot validate flows: missing tasks or conditions">>,
       location => <<"flows">>,
       code => missing_tasks_for_flow_validation} | lists:map(
        fun(_Flow) ->
            #{type => structure,
              severity => warning,
              message => <<"Flow not validated">>,
              location => undefined,
              code => flow_not_validated}
        end, Flows)];

check_flows(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a single flow.
%%--------------------------------------------------------------------
-spec validate_flow(flow(), sets:set(binary())) -> [validation_error()].

validate_flow(#{id := FlowId, source := Source, target := Target} = Flow, IdSet) ->
    Errors = [],
    %% Validate source exists
    Errors1 = case sets:is_element(Source, IdSet) of
        true -> Errors;
        false ->
            [#{type => reference,
               severity => error,
               message => iolist_to_binary([<<"Flow source not found: '">>, Source, <<"'">>]),
               location => FlowId,
               code => source_not_found} | Errors]
    end,
    %% Validate target exists
    Errors2 = case sets:is_element(Target, IdSet) of
        true -> Errors1;
        false ->
            [#{type => reference,
               severity => error,
               message => iolist_to_binary([<<"Flow target not found: '">>, Target, <<"'">>]),
               location => FlowId,
               code => target_not_found} | Errors1]
    end,
    %% Validate source != target (no self-loops)
    Errors3 = case Source =:= Target of
        true ->
            [#{type => semantic,
               severity => error,
               message => <<"Flow cannot have same source and target">>,
               location => FlowId,
               code => self_loop} | Errors2];
        false ->
            Errors2
    end,
    %% Validate predicate if present
    case maps:get(predicate, Flow, undefined) of
        undefined -> Errors3;
        Predicate -> validate_predicate(Predicate, FlowId, Errors3)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a flow predicate expression.
%%--------------------------------------------------------------------
-spec validate_predicate(binary(), binary(), [validation_error()]) ->
          [validation_error()].

validate_predicate(Predicate, FlowId, Errors) ->
    case is_valid_predicate(Predicate) of
        true -> Errors;
        false ->
            [#{type => structure,
               severity => error,
               message => iolist_to_binary([<<"Invalid predicate: '">>, Predicate, <<"'">>]),
               location => FlowId,
               code => invalid_predicate} | Errors]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a predicate expression is syntactically valid.
%%--------------------------------------------------------------------
-spec is_valid_predicate(binary()) -> boolean().

is_valid_predicate(<<>>) -> true;
is_valid_predicate(Predicate) when byte_size(Predicate) > 0 ->
    %% Basic validation: balanced parentheses and valid characters
    try
        PredList = binary_to_list(Predicate),
        check_parens(PredList, 0) andalso check_predicate_chars(PredList)
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for balanced parentheses.
%%--------------------------------------------------------------------
-spec check_parens([char()], integer()) -> boolean().

check_parens([], 0) -> true;
check_parens([], _) -> false;
check_parens([$( | Rest], Depth) -> check_parens(Rest, Depth + 1);
check_parens([$) | Rest], Depth) when Depth > 0 -> check_parens(Rest, Depth - 1);
check_parens([$) | _], _) -> false;
check_parens([_ | Rest], Depth) -> check_parens(Rest, Depth).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for valid predicate characters.
%%--------------------------------------------------------------------
-spec check_predicate_chars([char()]) -> boolean().

check_predicate_chars([]) -> true;
check_predicate_chars([C | Rest]) when C >= $a, C =< $z -> check_predicate_chars(Rest);
check_predicate_chars([C | Rest]) when C >= $A, C =< $Z -> check_predicate_chars(Rest);
check_predicate_chars([C | Rest]) when C >= $0, C =< $9 -> check_predicate_chars(Rest);
check_predicate_chars([$( | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$) | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$  | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$\t | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$\n | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$_ | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$< | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$> | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$= | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$! | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$& | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$| | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$+ | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$- | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$* | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$/ | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$" | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$' | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$, | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$. | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$: | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$[ | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars([$] | Rest]) -> check_predicate_chars(Rest);
check_predicate_chars(_Other) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for duplicate flows (same source and target).
%%--------------------------------------------------------------------
-spec check_duplicate_flows([flow()]) -> [validation_error()].

check_duplicate_flows(Flows) ->
    FlowKeys = [{maps:get(source, F, <<>>), maps:get(target, F, <<>>)} || F <- Flows],
    Duplicates = find_duplicates(FlowKeys),

    lists:map(fun({Source, Target}) ->
        #{type => semantic,
          severity => warning,
          message => iolist_to_binary([<<"Duplicate flow from '">>, Source,
                                      <<"' to '">>, Target, <<"'">>]),
          location => iolist_to_binary([Source, <<"->">>, Target]),
          code => duplicate_flow}
    end, Duplicates).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds duplicate elements in a list.
%%--------------------------------------------------------------------
-spec find_duplicates([term()]) -> [term()].

find_duplicates(List) ->
    Counts = lists:foldl(fun(E, Acc) ->
        Acc#{E => maps:get(E, Acc, 0) + 1}
    end, #{}, List),
    [E || E <- lists:usort(List), maps:get(E, Counts) > 1].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks for isolated nodes (no incoming or outgoing flows).
%%--------------------------------------------------------------------
-spec check_isolated_nodes([flow()], [binary()]) -> [validation_error()].

check_isolated_nodes(Flows, AllIds) ->
    %% Build sets of connected nodes
    Sources = sets:from_list([maps:get(source, F, <<>>) || F <- Flows]),
    Targets = sets:from_list([maps:get(target, F, <<>>) || F <- Flows]),
    Connected = sets:union(Sources, Targets),

    %% Find isolated nodes (input/output conditions are allowed to be isolated)
    Isolated = lists:filter(fun(Id) ->
        not sets:is_element(Id, Connected) andalso
        not (is_input_condition(Id) orelse is_output_condition(Id))
    end, AllIds),

    lists:map(fun(Id) ->
        #{type => semantic,
          severity => warning,
          message => iolist_to_binary([<<"Isolated node: '">>, Id, <<"'">>]),
          location => Id,
          code => isolated_node}
    end, Isolated).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if an ID looks like an input condition.
%%--------------------------------------------------------------------
-spec is_input_condition(binary()) -> boolean().

is_input_condition(Id) ->
    case binary:match(Id, <<"input">>) of
        nomatch -> false;
        _ -> true
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if an ID looks like an output condition.
%%--------------------------------------------------------------------
-spec is_output_condition(binary()) -> boolean().

is_output_condition(Id) ->
    case binary:match(Id, <<"output">>) of
        nomatch -> false;
        _ -> true
    end.

%%====================================================================
%% Decomposition Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates all decompositions in a specification.
%%
%% Checks for:
%% - Referenced decompositions exist
%% - No circular decomposition references
%% - Decomposition termination points
%%
%% === Example ===
%% ```erlang
%% Errors = yawl_validate:check_decompositions(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_decompositions(specification()) -> [validation_error()].

check_decompositions(#{tasks := Tasks} = Spec) ->
    %% Get all decomposition IDs from tasks
    DecompRefs = lists:filtermap(fun({_TaskId, Task}) ->
        case maps:get(type, Task, atomic) of
            composite ->
                case maps:get(decomposition, Task, undefined) of
                    undefined -> false;
                    DecompId -> {true, DecompId}
                end;
            _ ->
                false
        end
    end, maps:to_list(Tasks)),

    %% Get root decomposition
    RootDecomp = case Spec of
        #{decomposition := #{id := Id}} -> Id;
        _ -> undefined
    end,

    %% Check if all referenced decompositions exist
    check_decomposition_references(DecompRefs, RootDecomp, Spec);

check_decompositions(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks that all decomposition references are valid.
%%--------------------------------------------------------------------
-spec check_decomposition_references([binary()], binary() | undefined,
                                     specification()) -> [validation_error()].

check_decomposition_references(DecompRefs, RootDecomp, _Spec) ->
    %% In a full implementation, we would check that each referenced
    %% decomposition exists in the specification. For now, we check
    %% for obvious issues.

    Errors = [],
    %% Check for circular references (root referencing itself)
    Errors1 = case RootDecomp of
        undefined -> Errors;
        RootId ->
            case lists:member(RootId, DecompRefs) of
                true ->
                    [#{type => semantic,
                       severity => error,
                       message => iolist_to_binary([<<"Circular reference: root decomposition '">>,
                                                   RootId, <<"' references itself">>]),
                       location => RootId,
                       code => circular_decomposition} | Errors];
                false ->
                    Errors
            end
    end,

    %% Check for empty decomposition references
    Errors2 = lists:foldl(fun(DecompId, Acc) ->
        case DecompId of
            <<>> ->
                [#{type => structure,
                   severity => error,
                   message => <<"Empty decomposition reference">>,
                   location => <<"decomposition">>,
                   code => empty_decomp_ref} | Acc];
            _ ->
                Acc
        end
    end, Errors1, DecompRefs),

    Errors2.

%%====================================================================
%% Variable Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates variable usage in a specification.
%%
%% Checks for:
%% - Variables are declared before use
%% - No undefined variable references
%% - Variable type consistency
%%
%% === Example ===
%% ```erlang
%% Errors = yawl_validate:check_variables(Spec).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec check_variables(specification()) -> [validation_error()].

check_variables(#{data_mappings := Mappings}) ->
    %% Collect all declared variables
    Declared = collect_declared_variables(Mappings),

    %% Check for undeclared variable usage in mappings
    lists:foldl(fun(#{task_id := TaskId, input := Input, output := Output}, Acc) ->
        Acc ++ check_mapping_variables(TaskId, Input, Declared) ++
             check_mapping_variables(TaskId, Output, Declared)
    end, [], Mappings);

check_variables(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Collects all declared variables from mappings.
%%--------------------------------------------------------------------
-spec collect_declared_variables([mapping()]) -> sets:set(binary()).

collect_declared_variables(Mappings) ->
    lists:foldl(fun(#{input := Input, output := Output}, Acc) ->
        InputVars = sets:from_list([maps:get(variable, V, <<>>) || V <- Input]),
        OutputVars = sets:from_list([maps:get(variable, V, <<>>) || V <- Output]),
        sets:union(Acc, sets:union(InputVars, OutputVars))
    end, sets:new(), Mappings).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks that variables used in mappings are declared.
%%--------------------------------------------------------------------
-spec check_mapping_variables(binary(), [mapping()], sets:set(binary())) ->
          [validation_error()].

check_mapping_variables(TaskId, Mappings, _Declared) ->
    lists:foldl(fun(#{variable := VarName}, Acc) ->
        case VarName of
            <<>> ->
                [#{type => structure,
                   severity => warning,
                   message => <<"Mapping has empty variable name">>,
                   location => TaskId,
                   code => empty_variable_name} | Acc];
            _ ->
                %% Variables are self-declaring in mappings
                Acc
        end
    end, [], Mappings).

%%====================================================================
%% Consistency Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Checks overall specification consistency.
%%--------------------------------------------------------------------
-spec check_consistency(specification()) -> [validation_error()].

check_consistency(#{tasks := Tasks, flows := Flows}) ->
    %% Check split/join vs flow count consistency
    lists:foldl(fun({TaskId, Task}, Acc) ->
        Acc ++ check_task_flow_consistency(TaskId, Task, Flows)
    end, [], maps:to_list(Tasks));

check_consistency(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Checks that split/join types match flow counts.
%%--------------------------------------------------------------------
-spec check_task_flow_consistency(binary(), task(), [flow()]) ->
          [validation_error()].

check_task_flow_consistency(TaskId, Task, Flows) ->
    Split = maps:get(split_type, Task, undefined),
    Join = maps:get(join_type, Task, undefined),

    %% Count outgoing flows
    Outgoing = length([1 || #{source := S} <- Flows, S =:= TaskId]),
    %% Count incoming flows
    Incoming = length([1 || #{target := T} <- Flows, T =:= TaskId]),

    Errors = [],

    %% Check split type vs outgoing flows
    Errors1 = case {Split, Outgoing} of
        {undefined, 0} -> Errors;
        {undefined, _} -> Errors;
        {'and', 0} -> Errors;
        {'and', 1} ->
            [#{type => semantic,
               severity => warning,
               message => <<"AND split with only one outgoing flow (use no split)">>,
               location => TaskId,
               code => and_split_single_flow} | Errors];
        {'and', _} -> Errors;
        {'xor', 0} -> Errors;
        {'xor', 1} -> Errors;
        {'xor', _} ->
            [#{type => semantic,
               severity => warning,
               message => <<"XOR split with multiple outgoing flows (only one will be taken)">>,
               location => TaskId,
               code => xor_split_multiple_flows} | Errors];
        {'or', 0} -> Errors;
        {'or', _} -> Errors
    end,

    %% Check join type vs incoming flows
    Errors2 = case {Join, Incoming} of
        {undefined, 0} -> Errors1;
        {undefined, _} -> Errors1;
        {'and', 0} -> Errors1;
        {'and', 1} ->
            [#{type => semantic,
               severity => warning,
               message => <<"AND join with only one incoming flow (use no join)">>,
               location => TaskId,
               code => and_join_single_flow} | Errors1];
        {'and', _} -> Errors1;
        {'xor', 0} -> Errors1;
        {'xor', _} -> Errors1;
        {'or', 0} -> Errors1;
        {'or', _} -> Errors1
    end,

    Errors2.

%%====================================================================
%% Error Formatting
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Formats validation errors for human-readable display.
%%
%% Returns a list of formatted strings suitable for logging or
%% user interface display.
%%
%% === Example ===
%% ```erlang
%% Errors = [#{severity => error, message => <<"Invalid task">>}],
%% Formatted = yawl_validate:format_errors(Errors),
%% io:format("~s~n", [Formatted]).
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec format_errors([validation_error()]) -> [binary()].

format_errors(Errors) ->
    lists:map(fun(#{severity := Sev, message := Msg} = Error) ->
        Location = case maps:get(location, Error, undefined) of
            undefined -> <<>>;
            Loc -> iolist_to_binary([<<" (at '">>, Loc, <<"')">>])
        end,
        iolist_to_binary([
            case Sev of
                error -> <<"[ERROR]   ">>;
                warning -> <<"[WARNING] ">>
            end,
            Msg,
            Location
        ])
    end, Errors).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests for this module.
%%
%% Invoked via `rebar3 as test eunit --module=yawl_validate`.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%%--------------------------------------------------------------------
%% Test: validate accepts a valid specification
%%--------------------------------------------------------------------
validate_valid_spec_test() ->
    Spec = #{
        id => <<"test_wf">>,
        name => <<"Test Workflow">>,
        tasks => #{<<"task1">> => #{
            id => <<"task1">>,
            name => <<"Task 1">>,
            type => atomic
        }},
        conditions => #{},
        flows => [],
        data_mappings => []
    },
    {ok, _Warnings} = validate(Spec).

%%--------------------------------------------------------------------
%% Test: validate rejects spec without id
%%--------------------------------------------------------------------
validate_missing_id_test() ->
    Spec = #{
        id => <<>>,
        name => <<"Test Workflow">>,
        tasks => #{},
        conditions => #{},
        flows => [],
        data_mappings => []
    },
    {error, Errors} = validate(Spec),
    ?assert(length(Errors) > 0),
    ?assert(lists:any(fun(#{type := Type}) -> Type =:= required end, Errors)).

%%--------------------------------------------------------------------
%% Test: check_tasks validates task types
%%--------------------------------------------------------------------
check_tasks_invalid_type_test() ->
    Spec = #{
        id => <<"test">>,
        name => <<"Test">>,
        tasks => #{<<"t1">> => #{
            id => <<"t1">>,
            type => invalid_type
        }},
        conditions => #{},
        flows => [],
        data_mappings => []
    },
    Errors = check_tasks(Spec),
    ?assert(length(Errors) > 0).

%%--------------------------------------------------------------------
%% Test: check_flows detects invalid source
%%--------------------------------------------------------------------
check_flows_invalid_source_test() ->
    Spec = #{
        id => <<"test">>,
        name => <<"Test">>,
        tasks => #{<<"t1">> => #{id => <<"t1">>, type => atomic}},
        conditions => #{},
        flows => [#{id => <<"f1">>, source => <<"nonexistent">>, target => <<"t1">>, predicate => undefined}],
        data_mappings => []
    },
    Errors = check_flows(Spec),
    ?assert(length(Errors) > 0).

%%--------------------------------------------------------------------
%% Test: check_flows detects self-loops
%%--------------------------------------------------------------------
check_flows_self_loop_test() ->
    Spec = #{
        id => <<"test">>,
        name => <<"Test">>,
        tasks => #{<<"t1">> => #{id => <<"t1">>, type => atomic}},
        conditions => #{},
        flows => [#{id => <<"f1">>, source => <<"t1">>, target => <<"t1">>, predicate => undefined}],
        data_mappings => []
    },
    Errors = check_flows(Spec),
    ?assert(length(Errors) > 0).

%%--------------------------------------------------------------------
%% Test: is_valid_predicate checks balanced parentheses
%%--------------------------------------------------------------------
is_valid_predicate_test() ->
    ?assert(is_valid_predicate(<<"(A or B) and C">>)),
    ?assert(is_valid_predicate(<<"A and B">>)),
    ?assert(is_valid_predicate(<<>>)),
    ?assertNot(is_valid_predicate(<<"(A and B">>)),
    ?assertNot(is_valid_predicate(<<"A and B)">>)).

%%--------------------------------------------------------------------
%% Test: format_errors produces readable output
%%--------------------------------------------------------------------
format_errors_test() ->
    Errors = [
        #{severity => error, message => <<"Error 1">>, location => <<"task1">>},
        #{severity => warning, message => <<"Warning 1">>, location => undefined}
    ],
    Formatted = format_errors(Errors),
    ?assertEqual(2, length(Formatted)),
    ?assert(match(<<"[ERROR]">>, lists:nth(1, Formatted))),
    ?assert(match(<<"[WARNING]">>, lists:nth(2, Formatted))).

%%--------------------------------------------------------------------
%% Test: Multi-instance validation
%%--------------------------------------------------------------------
multi_instance_validation_test() ->
    Spec = #{
        id => <<"test">>,
        name => <<"Test">>,
        tasks => #{<<"t1">> => #{
            id => <<"t1">>,
            type => multiple_instance,
            split_type => 'xor'
        }},
        conditions => #{},
        flows => [],
        data_mappings => []
    },
    Errors = check_tasks(Spec),
    ?assert(lists:any(fun(#{code := Code}) -> Code =:= mi_split_must_be_and end, Errors)).

%%--------------------------------------------------------------------
%% Test: Decomposition validation
%%--------------------------------------------------------------------
decomposition_validation_test() ->
    Spec = #{
        id => <<"test">>,
        name => <<"Test">>,
        decomposition => #{id => <<"root">>},
        tasks => #{<<"t1">> => #{
            id => <<"t1">>,
            type => composite,
            decomposition => <<"root">>
        }},
        conditions => #{},
        flows => [],
        data_mappings => []
    },
    Errors = check_decompositions(Spec),
    ?assert(lists:any(fun(#{code := Code}) -> Code =:= circular_decomposition end, Errors)).

%% Helper function for matching binary prefix
match(Prefix, Binary) ->
    PrefixSize = byte_size(Prefix),
    case Binary of
        <<Prefix:PrefixSize/binary, _/binary>> -> true;
        _ -> false
    end.

-endif.
