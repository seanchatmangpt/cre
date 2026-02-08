%% -*- erlang -*-
%%%% @doc ga_constitution - Constitution record and validation module.
%%
%% This module defines the constitution schema for the GA (Generative Analysis)
%% compiler front-end. Constitutions define workflow specifications with
%% typing profiles (Σ), refusal catalogs (H), quality gates (Q), and
%% deterministic ordering (Λ).
%%
%% <h3>Constitution Schema</h3>
%%
%% A constitution consists of four main components:
%% <ul>
%%   <li><b>Σ (Sigma) - Typing Profile:</b> Terms → types → token contracts</li>
%%   <li><b>H - Refusal Catalog:</b> Inadmissible-before behavior guards</li>
%%   <li><b>Q - Quality Gates:</b> Receipts, replay, provenance invariants</li>
%%   <li><b>Λ (Lambda) - Deterministic Order:</b> Pattern compilation topology</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(ga_constitution).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Constitution creation and validation
-export([new/1, new/5]).
-export([validate/1]).
-export([is_valid/1]).
-export([add_sigma/2, add_refusal/2, add_quality_gate/2, add_lambda/2]).
-export([get_sigma/1, get_refusals/1, get_quality_gates/1, get_lambda/1]).

%% Type conversions
-export([to_map/1, from_map/1]).
-export([to_yaml/1, from_yaml/1]).

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Constitution record for GA compiler.
%%
%% Contains all components needed to compile a workflow specification
%% into executable gen_yawl modules.
%%--------------------------------------------------------------------
-record(constitution, {
    id :: binary(),                           %% Unique constitution identifier
    version :: binary(),                      %% Version string
    sigma = #{} :: #{}                        %% Σ - Typing profile
}).

-record(sigma, {
    type_system = behavioral :: behavioral | static | dynamic,
    type_bindings = [] :: [type_binding()]
}).

-record(type_binding, {
    term :: binary(),
    type :: binary(),
    token_contract :: #{
        shape := singleton | multiple | optional,
        validity := eager | lazy
    }
}).

-record(refusal, {
    state :: binary(),
    refused_transitions = [] :: [binary()],
    refusal_reason :: binary()
}).

-record(quality_gate, {
    name :: binary(),
    invariant :: binary(),
    replay_enabled = false :: boolean(),
    provenance_enabled = false :: boolean()
}).

-record(lambda, {
    compilation_strategy = topological :: topological | sequential | parallel,
    pattern_sequence = [] :: [pattern_instance()]
}).

-record(pattern_instance, {
    pattern :: binary(),
    instance_id :: binary(),
    config = #{} :: map()
}).

%%--------------------------------------------------------------------
%% @doc Token contract specification.
%%--------------------------------------------------------------------
-record(token_contract, {
    shape :: singleton | multiple | optional,
    validity :: eager | lazy,
    lifespan :: temporary | permanent
}).

%%====================================================================
%% Types
%%====================================================================

-type constitution() :: #constitution{}.
-type sigma() :: #sigma{}.
-type type_binding() :: #type_binding{}.
-type refusal() :: #refusal{}.
-type quality_gate() :: #quality_gate{}.
-type lambda() :: #lambda{}.
-type pattern_instance() :: #pattern_instance{}.
-type token_contract() :: #token_contract{}.

-type refusal_category() ::
    missing_evidence |
    forbidden_action |
    scope_violation |
    external_boundary |
    resource_unavailable |
    safety_violation |
    validation_failure |
    timeout_exceeded |
    permission_denied.

-export_type([
    constitution/0,
    sigma/0,
    type_binding/0,
    refusal/0,
    quality_gate/0,
    lambda/0,
    pattern_instance/0,
    token_contract/0,
    refusal_category/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new constitution from a map.
%%
%% @param Map Map with keys: id, version, sigma, refusals, quality_gates, lambda
%% @return #constitution{} record
%%
%% @end
%%--------------------------------------------------------------------
-spec new(map()) -> constitution().

new(#{<<"id">> := Id, <<"version">> := Version} = Map) ->
    Sigma = parse_sigma(maps:get(<<"sigma">>, Map, #{})),
    Refusals = parse_refusals(maps:get(<<"refusals">>, Map, [])),
    QualityGates = parse_quality_gates(maps:get(<<"quality_gates">>, Map, [])),
    Lambda = parse_lambda(maps:get(<<"lambda">>, Map, #{})),

    #constitution{
        id = Id,
        version = Version,
        sigma = Sigma,
        refusals = Refusals,
        quality_gates = QualityGates,
        lambda = Lambda
    }.

%%--------------------------------------------------------------------
%% @doc Creates a new constitution with individual components.
%%
%% @param Id Constitution identifier
%% @param Version Version string
%% @param Sigma Typing profile
%% @param Refusals Refusal catalog
%% @param Lambda Deterministic ordering
%% @return #constitution{} record
%%
%% @end
%%--------------------------------------------------------------------
-spec new(binary(), binary(), sigma(), [refusal()], lambda()) ->
          constitution().

new(Id, Version, Sigma, Refusals, Lambda) when
      is_binary(Id), is_binary(Version) ->
    #constitution{
        id = Id,
        version = Version,
        sigma = Sigma,
        refusals = Refusals,
        quality_gates = [],
        lambda = Lambda
    }.

%%--------------------------------------------------------------------
%% @doc Validates a constitution.
%%
%% Checks that all required fields are present and well-formed.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(constitution()) -> ok | {error, term()}.

validate(#constitution{id = Id, version = Version})
  when is_binary(Id), Id =/= <<>>, is_binary(Version), Version =/= <<>> ->
    ok;
validate(#constitution{id = Id, version = Version}) ->
    {error, {invalid_constitution, #{
        id => iolist_to_binary(io_lib:format("~p", [Id])),
        version => iolist_to_binary(io_lib:format("~p", [Version]))
    }}};
validate(_Other) ->
    {error, invalid_constitution_format}.

%%--------------------------------------------------------------------
%% @doc Returns true if the constitution is valid.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid(constitution()) -> boolean().

is_valid(Constitution) ->
    case validate(Constitution) of
        ok -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Adds a typing profile (Σ) to the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_sigma(constitution(), sigma()) -> constitution().

add_sigma(Constitution, Sigma) when is_record(Sigma, sigma) ->
    Constitution#constitution{sigma = Sigma}.

%%--------------------------------------------------------------------
%% @doc Adds a refusal to the constitution's catalog.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_refusal(constitution(), refusal()) -> constitution().

add_refusal(Constitution = #constitution{refusals = Refusals}, Refusal)
  when is_record(Refusal, refusal) ->
    Constitution#constitution{refusals = [Refusal | Refusals]}.

%%--------------------------------------------------------------------
%% @doc Adds a quality gate to the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_quality_gate(constitution(), quality_gate()) -> constitution().

add_quality_gate(Constitution = #constitution{quality_gates = Gates}, Gate)
  when is_record(Gate, quality_gate) ->
    Constitution#constitution{quality_gates = [Gate | Gates]}.

%%--------------------------------------------------------------------
%% @doc Sets the deterministic ordering (Λ) for the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_lambda(constitution(), lambda()) -> constitution().

add_lambda(Constitution, Lambda) when is_record(Lambda, lambda) ->
    Constitution#constitution{lambda = Lambda}.

%%--------------------------------------------------------------------
%% @doc Gets the typing profile (Σ) from the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_sigma(constitution()) -> sigma().

get_sigma(#constitution{sigma = Sigma}) ->
    Sigma.

%%--------------------------------------------------------------------
%% @doc Gets the refusal catalog from the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_refusals(constitution()) -> [refusal()].

get_refusals(#constitution{refusals = Refusals}) ->
    Refusals.

%%--------------------------------------------------------------------
%% @doc Gets the quality gates from the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_quality_gates(constitution()) -> [quality_gate()].

get_quality_gates(#constitution{quality_gates = Gates}) ->
    Gates.

%%--------------------------------------------------------------------
%% @doc Gets the deterministic ordering (Λ) from the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_lambda(constitution()) -> lambda().

get_lambda(#constitution{lambda = Lambda}) ->
    Lambda.

%%--------------------------------------------------------------------
%% @doc Converts a constitution to a map.
%%
%% Useful for serialization (JSON, YAML, etc).
%%
%% @end
%%--------------------------------------------------------------------
-spec to_map(constitution()) -> map().

to_map(#constitution{
    id = Id,
    version = Version,
    sigma = Sigma,
    refusals = Refusals,
    quality_gates = Gates,
    lambda = Lambda
}) ->
    #{
        <<"id">> => Id,
        <<"version">> => Version,
        <<"sigma">> => sigma_to_map(Sigma),
        <<"refusals">> => [refusal_to_map(R) || R <- Refusals],
        <<"quality_gates">> => [gate_to_map(G) || G <- Gates],
        <<"lambda">> => lambda_to_map(Lambda)
    }.

%%--------------------------------------------------------------------
%% @doc Creates a constitution from a map.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_map(map()) -> constitution().

from_map(Map) when is_map(Map) ->
    new(Map).

%%--------------------------------------------------------------------
%% @doc Converts a constitution to YAML format.
%%
%% Returns a binary string containing the YAML representation.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_yaml(constitution()) -> binary().

to_yaml(Constitution) ->
    Map = to_map(Constitution),
    %% Simple YAML generation - for production use a proper YAML library
    yaml_encode(Map).

%%--------------------------------------------------------------------
%% @doc Parses a constitution from YAML format.
%%
%% @end
%%--------------------------------------------------------------------
-spec from_yaml(binary()) -> {ok, constitution()} | {error, term()}.

from_yaml(YamlBinary) when is_binary(YamlBinary) ->
    try
        Map = yaml_decode(YamlBinary),
        {ok, from_map(Map)}
    catch
        Type:Error:Stack ->
            {error, {yaml_parse_error, Type, Error, Stack}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec parse_sigma(map()) -> sigma().

parse_sigma(Map) when is_map(Map) ->
    TypeSystem = maps:get(<<"type_system">>, Map, behavioral),
    BindingsList = maps:get(<<"type_bindings">>, Map, []),
    TypeBindings = [parse_type_binding(B) || B <- BindingsList],
    #sigma{
        type_system = normalize_type_system(TypeSystem),
        type_bindings = TypeBindings
    }.

%% @private
-spec parse_type_binding(map()) -> type_binding().

parse_type_binding(Map) when is_map(Map) ->
    Term = maps:get(<<"term">>, Map),
    Type = maps:get(<<"type">>, Map),
    ContractMap = maps:get(<<"token_contract">>, Map, #{}),
    Contract = #token_contract{
        shape = maps:get(<<"shape">>, ContractMap, singleton),
        validity = maps_get(<<"validity">>, ContractMap, eager)
    },
    #type_binding{
        term = Term,
        type = Type,
        token_contract = Contract
    }.

%% @private
-spec normalize_type_system(atom() | binary()) -> behavioral | static | dynamic.

normalize_type_system(behavioral) -> behavioral;
normalize_type_system(<<"behavioral">>) -> behavioral;
normalize_type_system(static) -> static;
normalize_type_system(<<"static">>) -> static;
normalize_type_system(dynamic) -> dynamic;
normalize_type_system(<<"dynamic">>) -> dynamic;
normalize_type_system(_) -> behavioral.

%% @private
-spec parse_refusals([map()]) -> [refusal()].

parse_refusals(List) when is_list(List) ->
    [parse_refusal(Map) || Map <- List].

%% @private
-spec parse_refusal(map()) -> refusal().

parse_refusal(Map) when is_map(Map) ->
    State = maps:get(<<"state">>, Map),
    Refused = maps:get(<<"refused_transitions">>, Map, []),
    Reason = maps:get(<<"refusal_reason">>, Map, <<>>),
    #refusal{
        state = State,
        refused_transitions = Refused,
        refusal_reason = Reason
    }.

%% @private
-spec parse_quality_gates([map()]) -> [quality_gate()].

parse_quality_gates(List) when is_list(List) ->
    [parse_quality_gate(Map) || Map <- List].

%% @private
-spec parse_quality_gate(map()) -> quality_gate().

parse_quality_gate(Map) when is_map(Map) ->
    Name = maps:get(<<"name">>, Map),
    Invariant = maps:get(<<"invariant">>, Map, <<>>),
    Replay = maps:get(<<"replay_enabled">>, Map, false),
    Provenance = maps:get(<<"provenance_enabled">>, Map, false),
    #quality_gate{
        name = Name,
        invariant = Invariant,
        replay_enabled = Replay,
        provenance_enabled = Provenance
    }.

%% @private
-spec parse_lambda(map()) -> lambda().

parse_lambda(Map) when is_map(Map) ->
    Strategy = maps:get(<<"compilation_strategy">>, Map, topological),
    PatternSeq = maps:get(<<"pattern_sequence">>, Map, []),
    #lambda{
        compilation_strategy = normalize_strategy(Strategy),
        pattern_sequence = [parse_pattern_instance(P) || P <- PatternSeq]
    }.

%% @private
-spec normalize_strategy(atom() | binary()) -> topological | sequential | parallel.

normalize_strategy(topological) -> topological;
normalize_strategy(<<"topological">>) -> topological;
normalize_strategy(sequential) -> sequential;
normalize_strategy(<<"sequential">>) -> sequential;
normalize_strategy(parallel) -> parallel;
normalize_strategy(<<"parallel">>) -> parallel;
normalize_strategy(_) -> topological.

%% @private
-spec parse_pattern_instance(map()) -> pattern_instance().

parse_pattern_instance(Map) when is_map(Map) ->
    Pattern = maps:get(<<"pattern">>, Map),
    InstanceId = maps:get(<<"instance_id">>, Map, <<>>),
    Config = maps:get(<<"config">>, Map, #{}),
    #pattern_instance{
        pattern = Pattern,
        instance_id = InstanceId,
        config = Config
    }.

%% @private
-spec sigma_to_map(sigma()) -> map().

sigma_to_map(#sigma{type_system = TypeSystem, type_bindings = Bindings}) ->
    #{
        <<"type_system">> => atom_to_binary(TypeSystem),
        <<"type_bindings">> => [type_binding_to_map(B) || B <- Bindings]
    }.

%% @private
-spec type_binding_to_map(type_binding()) -> map().

type_binding_to_map(#type_binding{term = Term, type = Type, token_contract = Contract}) ->
    #{
        <<"term">> => Term,
        <<"type">> => Type,
        <<"token_contract">> => #{
            <<"shape">> => Contract#token_contract.shape,
            <<"validity">> => Contract#token_contract.validity
        }
    }.

%% @private
-spec refusal_to_map(refusal()) -> map().

refusal_to_map(#refusal{state = State, refused_transitions = Refused, refusal_reason = Reason}) ->
    #{
        <<"state">> => State,
        <<"refused_transitions">> => Refused,
        <<"refusal_reason">> => Reason
    }.

%% @private
-spec gate_to_map(quality_gate()) -> map().

gate_to_map(#quality_gate{name = Name, invariant = Inv, replay_enabled = Replay, provenance_enabled = Prov}) ->
    #{
        <<"name">> => Name,
        <<"invariant">> => Inv,
        <<"replay_enabled">> => Replay,
        <<"provenance_enabled">> => Prov
    }.

%% @private
-spec lambda_to_map(lambda()) -> map().

lambda_to_map(#lambda{compilation_strategy = Strategy, pattern_sequence = Seq}) ->
    #{
        <<"compilation_strategy">> => atom_to_binary(Strategy),
        <<"pattern_sequence">> => [pattern_instance_to_map(P) || P <- Seq]
    }.

%% @private
-spec pattern_instance_to_map(pattern_instance()) -> map().

pattern_instance_to_map(#pattern_instance{pattern = Pattern, instance_id = Id, config = Config}) ->
    #{
        <<"pattern">> => Pattern,
        <<"instance_id">> => Id,
        <<"config">> => Config
    }.

%% @private
%% Simple YAML encoding - for production, use a proper YAML library
-spec yaml_encode(map()) -> binary().

yaml_encode(Map) when is_map(Map) ->
    Lines = [
        <<"constitution:">>,
        encode_map(<<"  ">, Map)
    ],
    iolist_to_binary(lists:flatten(Lines)).

%% @private
encode_map(Prefix, Map) when is_map(Map) ->
    lists:map(
        fun({K, V}) ->
            case V of
                L when is_list(L) ->
                    [
                        <<Prefix/binary, K/binary, ":\n">>,
                        encode_list(<<Prefix/binary, "  ">>, L)
                    ];
                M when is_map(M) ->
                    [
                        <<Prefix/binary, K/binary, ":\n">>,
                        encode_map(<<Prefix/binary, "  ">>, M)
                    ];
                A when is_atom(A) ->
                    <<Prefix/binary, K/binary, ": ", (atom_to_binary(A))/binary, "\n">>;
                B when is_binary(B) ->
                    <<Prefix/binary, K/binary, ": ", B/binary, "\n">>;
                I when is_integer(I) ->
                    <<Prefix/binary, K/binary, ": ", (integer_to_binary(I))/binary, "\n">>;
                _ ->
                    <<Prefix/binary, K/binary, ": null\n">>
            end
        end,
        lists:sort(maps:to_list(Map))
    ).

%% @private
encode_list(_Prefix, []) ->
    <<"[]\n">>;
encode_list(Prefix, List) when is_list(List) ->
    lists:map(
        fun(Item) when is_map(Item) ->
            [
                <<Prefix/binary, "- \n">>,
                encode_map(<<Prefix/binary, "  ">>, Item)
            ];
           (Item) when is_binary(Item) ->
            [<<Prefix/binary, "- ", Item/binary, "\n">>];
           (Item) ->
            [<<Prefix/binary, "- ", (iolist_to_binary(io_lib:format("~p", [Item])))/binary, "\n">>]
        end,
        List
    ).

%% @private
%% Simple YAML decoding - placeholder for production YAML parser
-spec yaml_decode(binary()) -> map().

yaml_decode(_YamlBinary) ->
    %% TODO: Implement proper YAML parsing
    %% For now, return empty map as placeholder
    #{}.

%% @private
-spec maps_get(Key, Map, Default) -> term().

maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
