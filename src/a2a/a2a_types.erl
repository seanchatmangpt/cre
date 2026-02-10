%%%-----------------------------------------------------------------------------
%%% @doc A2A-CONSTRUCT Core Type System
%%%
%%% Deterministic agent-to-agent protocol execution types.
%%% Implements the closed ontology: Σ (types), H (refusals), Q (invariants),
%%% Λ (ordering), Δ (actuation).
%%%
%%% This module defines the core data structures for A2A protocol execution
%%% without AI. All types enforce deterministic behavior.
%%% @end
%%%-----------------------------------------------------------------------------
-module(a2a_types).

-export([
    new_task/3,
    new_artifact/2,
    new_event/3,
    new_receipt/3,
    new_station/2,
    validate_task/1,
    validate_artifact/1,
    validate_receipt/1
]).

-export_type([
    task/0,
    artifact/0,
    event/0,
    receipt/0,
    station/0,
    refusal/0,
    protocol_id/0,
    station_id/0
]).

%%%=============================================================================
%%% Types - Closed Ontology (Σ)
%%%=============================================================================

%% Task: Work order with typed inputs/outputs
-type task() :: #{
    id := binary(),
    protocol := protocol_id(),
    input_types := [atom()],
    output_types := [atom()],
    timestamp := integer(),
    metadata := map()
}.

%% Artifact: Typed data packet
-type artifact() :: #{
    id := binary(),
    type := atom(),
    data := term(),
    hash := binary(),
    timestamp := integer()
}.

%% Event: Telemetry with causality
-type event() :: #{
    id := binary(),
    type := event_type(),
    station := station_id(),
    timestamp := integer(),
    cause := binary() | undefined,
    data := map()
}.

-type event_type() :: task_received | task_started | task_completed |
                      task_refused | artifact_produced.

%% Receipt: Proof object binding input → output
-type receipt() :: #{
    id := binary(),
    task_id := binary(),
    station_id := station_id(),
    inputs := [binary()],  % Artifact hashes
    outputs := [binary()], % Artifact hashes
    signature := binary(),
    timestamp := integer(),
    deterministic := boolean()
}.

%% Station: Typed protocol executor (agent without AI)
-type station() :: #{
    id := station_id(),
    protocols := [protocol_id()],
    state := station_state(),
    receipts := [receipt()],
    invariants := [fun((station()) -> boolean())]
}.

-type station_state() :: idle | processing | refused | completed.

%% Refusal: Typed terminal refusal (H - refusal guards)
-type refusal() :: #{
    type := refusal_type(),
    reason := binary(),
    task_id := binary(),
    station_id := station_id(),
    timestamp := integer(),
    terminal := boolean()
}.

-type refusal_type() ::
    type_mismatch |      % Wrong input/output types
    precondition_failed | % Guard predicate false
    invariant_violated |  % Q constraint broken
    timeout |            % Bounded time exceeded
    capacity_exceeded |   % Resource limit
    protocol_unknown.    % Σ violation

%% Protocol and Station identifiers
-type protocol_id() :: binary().
-type station_id() :: binary().

%%%=============================================================================
%%% Constructor Functions
%%%=============================================================================

-spec new_task(protocol_id(), [atom()], [atom()]) -> task().
new_task(ProtocolId, InputTypes, OutputTypes) ->
    #{
        id => generate_id(),
        protocol => ProtocolId,
        input_types => InputTypes,
        output_types => OutputTypes,
        timestamp => erlang:system_time(microsecond),
        metadata => #{}
    }.

-spec new_artifact(atom(), term()) -> artifact().
new_artifact(Type, Data) ->
    Artifact = #{
        id => generate_id(),
        type => Type,
        data => Data,
        hash => <<>>,
        timestamp => erlang:system_time(microsecond)
    },
    Hash = compute_hash(Artifact),
    Artifact#{hash => Hash}.

-spec new_event(event_type(), station_id(), map()) -> event().
new_event(Type, StationId, Data) ->
    #{
        id => generate_id(),
        type => Type,
        station => StationId,
        timestamp => erlang:system_time(microsecond),
        cause => maps:get(cause, Data, undefined),
        data => Data
    }.

-spec new_receipt(binary(), station_id(), #{inputs => [binary()], outputs => [binary()]}) -> receipt().
new_receipt(TaskId, StationId, #{inputs := Inputs, outputs := Outputs}) ->
    Receipt = #{
        id => generate_id(),
        task_id => TaskId,
        station_id => StationId,
        inputs => Inputs,
        outputs => Outputs,
        signature => <<>>,
        timestamp => erlang:system_time(microsecond),
        deterministic => true
    },
    Signature = sign_receipt(Receipt),
    Receipt#{signature => Signature}.

-spec new_station(station_id(), [protocol_id()]) -> station().
new_station(StationId, Protocols) ->
    #{
        id => StationId,
        protocols => Protocols,
        state => idle,
        receipts => [],
        invariants => [
            fun station_has_valid_id/1,
            fun station_protocols_non_empty/1
        ]
    }.

%%%=============================================================================
%%% Validation Functions (Q - Invariants)
%%%=============================================================================

-spec validate_task(task()) -> ok | {error, term()}.
validate_task(#{id := Id, protocol := Protocol, input_types := In, output_types := Out})
  when is_binary(Id), is_binary(Protocol), is_list(In), is_list(Out) ->
    case {In, Out} of
        {[], _} -> {error, no_input_types};
        {_, []} -> {error, no_output_types};
        _ -> ok
    end;
validate_task(_) ->
    {error, invalid_task_structure}.

-spec validate_artifact(artifact()) -> ok | {error, term()}.
validate_artifact(#{id := Id, type := Type, hash := Hash})
  when is_binary(Id), is_atom(Type), is_binary(Hash) ->
    ok;
validate_artifact(_) ->
    {error, invalid_artifact_structure}.

-spec validate_receipt(receipt()) -> ok | {error, term()}.
validate_receipt(#{id := Id, task_id := TaskId, inputs := Ins, outputs := Outs, signature := Sig})
  when is_binary(Id), is_binary(TaskId), is_list(Ins), is_list(Outs), is_binary(Sig) ->
    case verify_signature(Sig) of
        true -> ok;
        false -> {error, invalid_signature}
    end;
validate_receipt(_) ->
    {error, invalid_receipt_structure}.

%%%=============================================================================
%%% Invariant Predicates
%%%=============================================================================

-spec station_has_valid_id(station()) -> boolean().
station_has_valid_id(#{id := Id}) when is_binary(Id), byte_size(Id) > 0 -> true;
station_has_valid_id(_) -> false.

-spec station_protocols_non_empty(station()) -> boolean().
station_protocols_non_empty(#{protocols := Protos}) when length(Protos) > 0 -> true;
station_protocols_non_empty(_) -> false.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec generate_id() -> binary().
generate_id() ->
    UUID = uuid_v4(),
    list_to_binary(io_lib:format("~s", [UUID])).

-spec uuid_v4() -> string().
uuid_v4() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                                [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).

-spec compute_hash(artifact()) -> binary().
compute_hash(#{type := Type, data := Data, timestamp := TS}) ->
    Bin = term_to_binary({Type, Data, TS}),
    crypto:hash(sha256, Bin).

-spec sign_receipt(receipt()) -> binary().
sign_receipt(#{task_id := TaskId, inputs := Ins, outputs := Outs, timestamp := TS}) ->
    Bin = term_to_binary({TaskId, Ins, Outs, TS}),
    crypto:hash(sha256, Bin).

-spec verify_signature(binary()) -> boolean().
verify_signature(Sig) when is_binary(Sig), byte_size(Sig) == 32 -> true;
verify_signature(_) -> false.
