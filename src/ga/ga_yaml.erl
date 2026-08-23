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
        %% Ensure yamerl is started
        case application:ensure_all_started(yamerl) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok
        end,
        %% Parse YAML using yamerl_constr:string
        %% This returns materialized results, not generators
        case yamerl_constr:string(YamlBinary) of
            [Doc | _] ->
                %% Normalize yamerl output (may be proplist or map)
                NormalizedData = normalize_yaml_data(Doc),
                parse_constitution(NormalizedData);
            [] ->
                {error, [<<"Empty YAML document">>]};
            {error, Reason} ->
                {error, [iolist_to_binary(io_lib:format("YAML parse error: ~p", [Reason]))]}
        end
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("YAML parsing failed: ~p:~p~n~p", [Type, Error, Stack]),
            {error, [iolist_to_binary(io_lib:format("Parse exception: ~p:~p", [Type, Error]))]}
    end.

%%--------------------------------------------------------------------
%% @doc Serializes a constitution record to YAML format.
%%
%% Note: This is a simple YAML encoder for basic constitution structures.
%% For complex cases, consider using a proper YAML encoding library.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_yaml(#constitution{}) -> binary().

to_yaml(#constitution{} = Constitution) ->
    try
        %% Convert constitution to YAML-compatible map
        Map = constitution_to_map(Constitution),
        %% Encode to YAML using our simple encoder
        encode_yaml(#{<<"constitution">> => Map})
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("YAML encoding failed: ~p:~p~n~p", [Type, Error, Stack]),
            error({encoding_failed, {Type, Error}})
    end.

%% @private
%% Simple YAML encoder for constitution maps
encode_yaml(Map) when is_map(Map) ->
    iolist_to_binary(encode_yaml(Map, "")).

encode_yaml(Map, Indent) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = binary_to_list(K),
        case V of
            List when is_list(List) ->
                %% List of items
                Acc ++ Indent ++ Key ++ ":\n" ++ encode_list(List, Indent ++ "  ");
            SubMap when is_map(SubMap) ->
                %% Nested map
                Acc ++ Indent ++ Key ++ ":\n" ++ encode_yaml(SubMap, Indent ++ "  ");
            Bin when is_binary(Bin) ->
                %% Binary value - quote if needed
                Val = binary_to_list(Bin),
                Acc ++ Indent ++ Key ++ ": " ++ maybe_quote(Val) ++ "\n";
            Int when is_integer(Int) ->
                Acc ++ Indent ++ Key ++ ": " ++ integer_to_list(Int) ++ "\n";
            Atom when is_atom(Atom) ->
                Acc ++ Indent ++ Key ++ ": " ++ atom_to_list(Atom) ++ "\n";
            _ ->
                Acc ++ Indent ++ Key ++ ": " ++ io_lib:format("~p", [V]) ++ "\n"
        end
    end, "", Map).

encode_list([], _Indent) ->
    "";
encode_list(List, Indent) when is_list(List) ->
    lists:foldl(fun(Item, Acc) ->
        case Item of
            Map when is_map(Map) ->
                %% For list items that are maps, format as:
                %% - key1: value1
                %%   key2: value2
                Acc ++ encode_list_item_map(Map, Indent);
            Bin when is_binary(Bin) ->
                Acc ++ Indent ++ "- " ++ binary_to_list(Bin) ++ "\n";
            _ ->
                Acc ++ Indent ++ "- " ++ io_lib:format("~p", [Item]) ++ "\n"
        end
    end, "", List).

%% @private
%% Encode a map as a list item (first key with "- ", rest with "  ")
encode_list_item_map(Map, Indent) ->
    Keys = maps:keys(Map),
    encode_list_item_map(Keys, Map, Indent, true).

encode_list_item_map([], _Map, _Indent, _IsFirst) ->
    "";
encode_list_item_map([K | Rest], Map, Indent, true) ->
    %% First key goes on same line as "-"
    Key = binary_to_list(K),
    V = maps:get(K, Map),
    Prefix = Indent ++ "- ",
    Value = encode_value(V, Indent ++ "  "),
    Prefix ++ Key ++ ": " ++ Value ++ "\n" ++ encode_list_item_map(Rest, Map, Indent, false);
encode_list_item_map([K | Rest], Map, Indent, false) ->
    %% Rest of keys go on next line with proper indentation
    Key = binary_to_list(K),
    V = maps:get(K, Map),
    Prefix = Indent ++ "  ",
    Value = encode_value(V, Indent ++ "  "),
    Prefix ++ Key ++ ": " ++ Value ++ "\n" ++ encode_list_item_map(Rest, Map, Indent, false).

%% @private
%% Encode a value (not a key)
encode_value(V, _Indent) ->
    case V of
        List when is_list(List) ->
            %% For lists that are values, encode as YAML flow sequence
            encode_flow_list(List);
        SubMap when is_map(SubMap) ->
            "{}";
        Bin when is_binary(Bin) ->
            Str = binary_to_list(Bin),
            maybe_quote(Str);
        Int when is_integer(Int) ->
            integer_to_list(Int);
        Atom when is_atom(Atom) ->
            atom_to_list(Atom);
        true ->
            "true";
        false ->
            "false";
        _ ->
            io_lib:format("~p", [V])
    end.

%% @private
%% Encode a list as a YAML flow sequence: [item1, item2, ...]
encode_flow_list([]) ->
    "[]";
encode_flow_list(List) ->
    Items = [encode_flow_item(I) || I <- List],
    "[" ++ string:join(Items, ", ") ++ "]".

%% @private
%% Encode a single item for flow sequence
encode_flow_item(Item) ->
    case Item of
        Bin when is_binary(Bin) ->
            Str = binary_to_list(Bin),
            maybe_quote(Str);
        Int when is_integer(Int) ->
            integer_to_list(Int);
        Atom when is_atom(Atom) ->
            atom_to_list(Atom);
        _ ->
            io_lib:format("~p", [Item])
    end.

%% @private
maybe_quote(Str) ->
    case needs_quoting(Str) of
        true -> [$", Str, $"];
        false -> Str
    end.

%% @private
needs_quoting([]) ->
    false;
needs_quoting(Str) ->
    %% Quote if contains special chars, is empty, or looks like a boolean/null
    case string:trim(Str) of
        "" -> true;
        "true" -> true;
        "false" -> true;
        "null" -> true;
        "True" -> true;
        "False" -> true;
        "Null" -> true;
        Trimmed -> needs_quoting_chars(Trimmed)
    end.

needs_quoting_chars([]) ->
    false;
needs_quoting_chars([$: | _]) -> true;  % Colon
needs_quoting_chars([$# | _]) -> true;  % Comment
needs_quoting_chars([$\s | _]) -> true; % Space
needs_quoting_chars([$\t | _]) -> true; % Tab
needs_quoting_chars([$\n | _]) -> true; % Newline
needs_quoting_chars([$[ | _]) -> true;  % Bracket
needs_quoting_chars([$] | _]) -> true;  % Bracket
needs_quoting_chars([${ | _]) -> true;  % Brace
needs_quoting_chars([$} | _]) -> true;  % Brace
needs_quoting_chars([$, | _]) -> true;  % Comma
needs_quoting_chars([$! | _]) -> true;  % Exclamation
needs_quoting_chars([$& | _]) -> true;  % Ampersand
needs_quoting_chars([$* | _]) -> true;  % Asterisk
needs_quoting_chars([$% | _]) -> true;  % Percent
needs_quoting_chars([$| | _]) -> true;  % Pipe
needs_quoting_chars([$> | _]) -> true;  % Greater than
needs_quoting_chars([$' | _]) -> true;  % Single quote
needs_quoting_chars([$" | _]) -> true;  % Double quote
needs_quoting_chars([$` | _]) -> true;  % Backtick
needs_quoting_chars([$@ | _]) -> true;  % At sign
needs_quoting_chars([$_ | _]) -> true;  %% Others
needs_quoting_chars([$\r | _]) -> true; % Carriage return
needs_quoting_chars([_ | Rest]) ->
    needs_quoting_chars(Rest).

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
        <<"type_system">> => TypeSystem,
        <<"type_bindings">> => TypeBindings
    }.

%% @private
-spec parse_type_binding(map()) -> ga_constitution:type_binding().

parse_type_binding(BindingMap) ->
    Term = maps:get(<<"term">>, BindingMap),
    Type = maps:get(<<"type">>, BindingMap),
    ContractMap = maps:get(<<"token_contract">>, BindingMap, #{}),
    Shape = case maps:get(<<"shape">>, ContractMap, <<"singleton">>) of
        <<"singleton">> -> singleton;
        <<"multiple">> -> multiple;
        <<"optional">> -> optional
    end,
    Validity = case maps:get(<<"validity">>, ContractMap, <<"eager">>) of
        <<"eager">> -> eager;
        <<"lazy">> -> lazy
    end,
    Lifespan = case maps:get(<<"lifespan">>, ContractMap, <<"temporary">>) of
        <<"temporary">> -> temporary;
        <<"permanent">> -> permanent
    end,
    Contract = #token_contract{
        shape = Shape,
        validity = Validity,
        lifespan = Lifespan
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
-spec sigma_to_map(term()) -> map().

sigma_to_map(SigmaMap) when is_map(SigmaMap) ->
    %% Handle both map and #sigma{} record
    TypeSystem = maps:get(<<"type_system">>, SigmaMap, behavioral),
    TypeBindings = maps:get(<<"type_bindings">>, SigmaMap, []),
    TypeSystemBin = case TypeSystem of
        Bin when is_binary(Bin) -> Bin;
        Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8)
    end,
    #{
        <<"type_system">> => TypeSystemBin,
        <<"type_bindings">> => [type_binding_to_map(B) || B <- TypeBindings]
    };
sigma_to_map(#sigma{type_system = Type, type_bindings = Bindings}) ->
    #{
        <<"type_system">> => atom_to_binary(Type, utf8),
        <<"type_bindings">> => [type_binding_to_map(B) || B <- Bindings]
    }.

%% @private
-spec type_binding_to_map(ga_constitution:type_binding()) -> map().

type_binding_to_map(#type_binding{term = Term, type = Type, token_contract = Contract}) ->
    #{
        <<"term">> => Term,
        <<"type">> => Type,
        <<"token_contract">> => token_contract_to_map(Contract)
    }.

%% @private
-spec token_contract_to_map(ga_constitution:token_contract()) -> map().

token_contract_to_map(#token_contract{shape = Shape, validity = Validity, lifespan = Lifespan}) ->
    #{
        <<"shape">> => atom_to_binary(Shape),
        <<"validity">> => atom_to_binary(Validity),
        <<"lifespan">> => atom_to_binary(Lifespan)
    };
token_contract_to_map(Map) when is_map(Map) ->
    Map.

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
    %% compilation_strategy may be an atom or binary
    StratBin = case Strat of
        Bin when is_binary(Bin) -> Bin;
        Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);
        _ -> <<"topological">>  %% default fallback
    end,
    #{
        <<"compilation_strategy">> => StratBin,
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

%% @private
%% @doc Normalizes yamerl output to convert proplists to maps.
%% Yamerl 0.10+ may return proplists for some structures.
-spec normalize_yaml_data(term()) -> term().
normalize_yaml_data({yamerl_map, Pairs}) when is_list(Pairs) ->
    maps:from_list([{normalize_key(K), normalize_yaml_data(V)} || {K, V} <- Pairs]);
normalize_yaml_data({yamerl_seq, Items}) when is_list(Items) ->
    [normalize_yaml_data(Item) || Item <- Items];
normalize_yaml_data(Value) when is_map(Value) ->
    maps:map(fun(_K, V) -> normalize_yaml_data(V) end, Value);
normalize_yaml_data(Value) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            maps:from_list([{normalize_key(K), normalize_yaml_data(V)} || {K, V} <- Value]);
        false ->
            case io_lib:printable_list(Value) of
                true -> list_to_binary(Value);
                false -> [normalize_yaml_data(Item) || Item <- Value]
            end
    end;
normalize_yaml_data(Value) ->
    Value.

%% @private
%% @doc Normalizes YAML keys to binary format.
-spec normalize_key(term()) -> binary().
normalize_key(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
normalize_key(Binary) when is_binary(Binary) -> Binary;
normalize_key(String) when is_list(String) -> list_to_binary(String);
normalize_key(Other) -> Other.

%% @private
%% @doc Detects proplist format (common in yamerl output).
is_proplist([]) -> true;
is_proplist([{K, _} | Rest]) when is_list(K); is_atom(K) -> is_proplist(Rest);
is_proplist(_) -> false.
