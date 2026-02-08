%% -*- erlang -*-
%%%% @doc ga_yaml - YAML constitution parser and serializer.
%%
%% This module provides YAML parsing and serialization for GA constitutions.
%% It uses the yamerl library for YAML parsing and constructs constitution
%% records from YAML documents.
%%
%% <h3>Constitution YAML Schema</h3>
%%
%% <pre>
%% constitution:
%%   id: "workflow_id"
%%   version: "1.0"
%%
%%   # Σ - Typing Profile
%%   sigma:
%%     type_system: "behavioral"
%%     type_bindings:
%%       - term: "Request"
%%         type: "PurchaseOrder"
%%         token_contract:
%%           shape: "singleton"
%%           validity: "eager"
%%
%%   # H - Refusal Catalog
%%   refusals:
%%     - state: "PendingApproval"
%%       refused_transitions: ["t_ship_goods"]
%%       refusal_reason: "Cannot ship before approval"
%%
%%   # Q - Quality Gates
%%   quality_gates:
%%     - name: "audit_trail"
%%       invariant: "forall t. receipt_exists(t)"
%%       replay_enabled: true
%%
%%   # Λ - Deterministic Order
%%   lambda:
%%     compilation_strategy: "topological"
%%     pattern_sequence:
%%       - pattern: "P2_ParallelSplit"
%%         instance_id: "pi_split_approval_paths"
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(ga_yaml).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Parsing and serialization
-export([from_yaml/1]).
-export([to_yaml/1]).
-export([from_yaml_file/1]).
-export([to_yaml_file/2]).

%% Validation
-export([validate_schema/1]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").
-include("ga_constitution.hrl").

%%====================================================================
%% Types
%%====================================================================

-type yaml_node() :: map() | list() | binary() | number() | boolean().
-type parse_result() :: {ok, #constitution{}} | {error, [binary()]}.
-type parse_error() :: {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Parses a YAML binary into a constitution record.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_yaml(binary()) -> parse_result().

from_yaml(YamlBinary) when is_binary(YamlBinary) ->
    try
        %% Parse YAML using yamerl
        case yamerl:decode(YamlBinary) of
            {error, Reason} ->
                {error, [iolist_to_binary(io_lib:format("YAML parse error: ~p", [Reason]))]};
            Data when is_map(Data) ->
                parse_constitution(Data);
            _ ->
                {error, [<<"YAML root must be a map">>]}
        end
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("YAML parsing failed: ~p:~p~n~p", [Type, Error, Stack]),
            {error, [iolist_to_binary(io_lib:format("Parse exception: ~p:~p", [Type, Error]))]}
    end.

%%--------------------------------------------------------------------
%% @doc Serializes a constitution record to YAML format.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_yaml(#constitution{}) -> binary().

to_yaml(#constitution{} = Constitution) ->
    %% Convert constitution to YAML-compatible map
    Map = constitution_to_map(Constitution),
    %% Use yamerl to encode
    iolist_to_binary(yamerl:encode(Map)).

%%--------------------------------------------------------------------
%% @doc Reads and parses a YAML file into a constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_yaml_file(file:filename_all()) -> parse_result().

from_yaml_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            from_yaml(Content);
        {error, Reason} ->
            {error, [iolist_to_binary(io_lib:format("File read error: ~p", [Reason]))]}
    end.

%%--------------------------------------------------------------------
%% @doc Serializes a constitution to a YAML file.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_yaml_file(#constitution{}, file:filename_all()) -> ok | {error, term()}.

to_yaml_file(#constitution{} = Constitution, FilePath) ->
    Yaml = to_yaml(Constitution),
    file:write_file(FilePath, Yaml).

%%--------------------------------------------------------------------
%% @doc Validates the YAML schema structure.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_schema(map()) -> ok | {error, [binary()]}.

validate_schema(YamlMap) when is_map(YamlMap) ->
    Errors = [
        validate_required(YamlMap, <<"constitution">>, [
            <<"id">>, <<"version">>
        ]),
        validate_section(YamlMap, <<"sigma">>, [
            <<"type_system">>
        ]),
        validate_section(YamlMap, <<"lambda">>, [
            <<"compilation_strategy">>, <<"pattern_sequence">>
        ])
    ],
    case lists:flatten(Errors) of
        [] -> ok;
        Errs -> {error, Errs}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec parse_constitution(map()) -> parse_result().

parse_constitution(YamlMap) ->
    %% Validate schema first
    case validate_schema(YamlMap) of
        {error, Errs} ->
            {error, Errs};
        ok ->
            %% Extract constitution map
            ConstMap = maps:get(<<"constitution">>, YamlMap, YamlMap),

            %% Parse basic fields
            Id = maps:get(<<"id">>, ConstMap),
            Version = maps:get(<<"version">>, ConstMap, <<"1.0">>),

            %% Parse Σ (Sigma)
            Sigma = parse_sigma(maps:get(<<"sigma">>, ConstMap, #{})),

            %% Parse H (Refusals)
            Refusals = parse_refusals(maps:get(<<"refusals">>, ConstMap, [])),

            %% Parse Q (Quality Gates)
            QualityGates = parse_quality_gates(maps:get(<<"quality_gates">>, ConstMap, [])),

            %% Parse Λ (Lambda)
            Lambda = parse_lambda(maps:get(<<"lambda">>, ConstMap, #{})),

            %% Build constitution record
            Constitution = #constitution{
                id = Id,
                version = Version,
                sigma = Sigma,
                refusals = Refusals,
                quality_gates = QualityGates,
                lambda = Lambda
            },

            {ok, Constitution}
    end.

%% @private
-spec parse_sigma(map()) -> map().

parse_sigma(SigmaMap) when is_map(SigmaMap) ->
    TypeSystem = maps:get(<<"type_system">>, SigmaMap, behavioral),
    TypeBindingsList = maps:get(<<"type_bindings">>, SigmaMap, []),
    TypeBindings = [parse_type_binding(B) || B <- TypeBindingsList],
    #{
        type_system => TypeSystem,
        type_bindings => TypeBindings
    }.

%% @private
-spec parse_type_binding(map()) -> ga_constitution:type_binding().

parse_type_binding(BindingMap) ->
    Term = maps:get(<<"term">>, BindingMap),
    Type = maps:get(<<"type">>, BindingMap),
    ContractMap = maps:get(<<"token_contract">>, BindingMap, #{}),
    Contract = #{
        shape => maps:get(<<"shape">>, ContractMap, singleton),
        validity => maps:get(<<"validity">>, ContractMap, eager)
    },
    #type_binding{
        term = Term,
        type = Type,
        token_contract = Contract
    }.

%% @private
-spec parse_refusals([map()]) -> [ga_constitution:refusal()].

parse_refusals(RefusalsList) when is_list(RefusalsList) ->
    [parse_refusal(R) || R <- RefusalsList];
parse_refusals(_) ->
    [].

%% @private
-spec parse_refusal(map()) -> ga_constitution:refusal().

parse_refusal(RefusalMap) ->
    State = maps:get(<<"state">>, RefusalMap),
    RefusedTrans = maps:get(<<"refused_transitions">>, RefusalMap, []),
    Reason = maps:get(<<"refusal_reason">>, RefusalMap, <<"">>),
    #refusal{
        state = State,
        refused_transitions = RefusedTrans,
        refusal_reason = Reason
    }.

%% @private
-spec parse_quality_gates([map()]) -> [ga_constitution:quality_gate()].

parse_quality_gates(GatesList) when is_list(GatesList) ->
    [parse_quality_gate(G) || G <- GatesList];
parse_quality_gates(_) ->
    [].

%% @private
-spec parse_quality_gate(map()) -> ga_constitution:quality_gate().

parse_quality_gate(GateMap) ->
    Name = maps:get(<<"name">>, GateMap),
    Invariant = maps:get(<<"invariant">>, GateMap, <<"">>),
    ReplayEnabled = maps:get(<<"replay_enabled">>, GateMap, false),
    ProvenanceEnabled = maps:get(<<"provenance_enabled">>, GateMap, false),
    ReceiptRequired = maps:get(<<"receipt_required">>, GateMap, false),
    #quality_gate{
        name = Name,
        invariant = Invariant,
        replay_enabled = ReplayEnabled,
        provenance_enabled = ProvenanceEnabled,
        receipt_required = ReceiptRequired
    }.

%% @private
-spec parse_lambda(map()) -> ga_constitution:lambda().

parse_lambda(LambdaMap) when is_map(LambdaMap) ->
    Strategy = maps:get(<<"compilation_strategy">>, LambdaMap, sequential),
    PatternSeqList = maps:get(<<"pattern_sequence">>, LambdaMap, []),
    PatternSequence = [parse_pattern_instance(P) || P <- PatternSeqList],
    #lambda{
        compilation_strategy = Strategy,
        pattern_sequence = PatternSequence
    }.

%% @private
-spec parse_pattern_instance(map()) -> ga_constitution:pattern_instance().

parse_pattern_instance(PatternMap) ->
    Pattern = maps:get(<<"pattern">>, PatternMap),
    InstanceId = maps:get(<<"instance_id">>, PatternMap),
    Config = maps:get(<<"config">>, PatternMap, #{}),
    #pattern_instance{
        pattern = Pattern,
        instance_id = InstanceId,
        config = Config
    }.

%% @private
-spec constitution_to_map(#constitution{}) -> map().

constitution_to_map(#constitution{
    id = Id,
    version = Version,
    sigma = SigmaMap,
    refusals = Refusals,
    quality_gates = QualityGates,
    lambda = Lambda
}) ->
    #{
        <<"id">> => Id,
        <<"version">> => Version,
        <<"sigma">> => sigma_to_map(SigmaMap),
        <<"refusals">> => [refusal_to_map(R) || R <- Refusals],
        <<"quality_gates">> => [quality_gate_to_map(Q) || Q <- QualityGates],
        <<"lambda">> => lambda_to_map(Lambda)
    }.

%% @private
-spec sigma_to_map(map()) -> map().

sigma_to_map(#{type_system := Type, type_bindings := Bindings}) ->
    #{
        <<"type_system">> => atom_to_binary(Type),
        <<"type_bindings">> => [type_binding_to_map(B) || B <- Bindings]
    }.

%% @private
-spec type_binding_to_map(ga_constitution:type_binding()) -> map().

type_binding_to_map(#type_binding{term = Term, type = Type, token_contract = Contract}) ->
    #{
        <<"term">> => Term,
        <<"type">> => Type,
        <<"token_contract">> => Contract
    }.

%% @private
-spec refusal_to_map(ga_constitution:refusal()) -> map().

refusal_to_map(#refusal{state = State, refused_transitions = Trans, refusal_reason = Reason}) ->
    #{
        <<"state">> => State,
        <<"refused_transitions">> => Trans,
        <<"refusal_reason">> => Reason
    }.

%% @private
-spec quality_gate_to_map(ga_constitution:quality_gate()) -> map().

quality_gate_to_map(#quality_gate{
    name = Name,
    invariant = Inv,
    replay_enabled = Replay,
    provenance_enabled = Prov,
    receipt_required = Receipt
}) ->
    #{
        <<"name">> => Name,
        <<"invariant">> => Inv,
        <<"replay_enabled">> => Replay,
        <<"provenance_enabled">> => Prov,
        <<"receipt_required">> => Receipt
    }.

%% @private
-spec lambda_to_map(ga_constitution:lambda()) -> map().

lambda_to_map(#lambda{compilation_strategy = Strat, pattern_sequence = Seq}) ->
    #{
        <<"compilation_strategy">> => atom_to_binary(Strat),
        <<"pattern_sequence">> => [pattern_instance_to_map(P) || P <- Seq]
    }.

%% @private
-spec pattern_instance_to_map(ga_constitution:pattern_instance()) -> map().

pattern_instance_to_map(#pattern_instance{pattern = Pat, instance_id = Id, config = Config}) ->
    #{
        <<"pattern">> => Pat,
        <<"instance_id">> => Id,
        <<"config">> => Config
    }.

%% @private
-spec validate_required(map(), binary(), [binary()]) -> [binary()].

validate_required(Map, Section, RequiredFields) ->
    case maps:get(Section, Map, undefined) of
        undefined ->
            [io_lib:format("Missing section: ~p", [Section])];
        SectionMap when is_map(SectionMap) ->
            [io_lib:format("Missing required field ~p in section ~p", [F, Section])
             || F <- RequiredFields, not maps:is_key(F, SectionMap)];
        _ ->
            [io_lib:format("Section ~p must be a map", [Section])]
    end.

%% @private
-spec validate_section(map(), binary(), [binary()]) -> [binary()].

validate_section(Map, Section, RequiredFields) ->
    case maps:get(Section, Map, undefined) of
        undefined ->
            [];  %% Section is optional
        SectionMap when is_map(SectionMap) ->
            [io_lib:format("Missing field ~p in section ~p", [F, Section])
             || F <- RequiredFields, not maps:is_key(F, SectionMap)];
        _ ->
            [io_lib:format("Section ~p must be a map", [Section])]
    end.
