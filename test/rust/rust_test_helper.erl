%% -*- erlang -*-
%% @doc Rust Test Helper Module
%%
%% Provides helper functions and mock implementations for Rust interface tests.
%% This module simulates Rust NIF functions when actual Rust library is not loaded.
%%
%% @end

-module(rust_test_helper).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Exports
%%====================================================================

-export([
    setup/0,
    cleanup/1,
    mock_rust_interface/0,
    is_rust_available/0
]).

%% Mock implementations for testing
-export([
    mock_marshal_int/1,
    mock_marshal_float/1,
    mock_marshal_bool/1,
    mock_marshal_string/1,
    mock_marshal_atom/1,
    mock_marshal_list/1,
    mock_marshal_tuple/1,
    mock_marshal_map/1,
    mock_marshal_nested/1
]).

%% Algorithm mock implementations
-export([
    mock_alpha/1,
    mock_heuristic/2,
    mock_conformance/2,
    mock_align_trace/2
]).

%% Resource management mocks
-export([
    mock_resource_create/0,
    mock_resource_destroy/1,
    mock_resource_status/1,
    mock_resource_operation/2
]).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

%% @doc Setup Rust test environment
setup() ->
    %% Initialize mock tables
    ets:new(rust_mock_table, [named_table, public, set]),
    %% Track resources
    ets:new(rust_resources, [named_table, public, set]),
    ok.

%% @doc Cleanup Rust test environment
cleanup(_Config) ->
    %% Clean up mock tables
    catch ets:delete(rust_mock_table),
    catch ets:delete(rust_resources),
    ok.

%%====================================================================
%% Interface Detection
%%====================================================================

%% @doc Check if actual Rust interface is available
is_rust_available() ->
    case code:which(rust_nif) of
        non_local -> true;
        _ -> false
    end.

%% @doc Setup mock Rust interface
mock_rust_interface() ->
    %% Register mock modules
    meck:new(rust_marshal, [non_strict]),
    meck:new(rust_algorithm, [non_strict]),
    meck:new(rust_resource, [non_strict]),
    meck:new(rust_serialize, [non_strict]),
    meck:new(rust_benchmark, [non_strict]),

    %% Set up expectations
    setup_marshal_mocks(),
    setup_algorithm_mocks(),
    setup_resource_mocks(),
    setup_serialize_mocks(),
    setup_benchmark_mocks(),

    ok.

%%====================================================================
%% Marshal Mocks
%%====================================================================

setup_marshal_mocks() ->
    meck:expect(rust_marshal, encode_atom, fun mock_marshal_atom/1),
    meck:expect(rust_marshal, encode_binary, fun(X) -> X end),
    meck:expect(rust_marshal, encode_integer, fun(X) -> X end),
    meck:expect(rust_marshal, encode_float, fun mock_marshal_float/1),
    meck:expect(rust_marshal, encode_bool, fun mock_marshal_bool/1),
    meck:expect(rust_marshal, encode_list, fun mock_marshal_list/1),
    meck:expect(rust_marshal, encode_tuple, fun mock_marshal_tuple/1),
    meck:expect(rust_marshal, encode_map, fun mock_marshal_map/1),
    meck:expect(rust_marshal, encode_nested, fun mock_marshal_nested/1),
    meck:expect(rust_marshal, encode_pid, fun(Pid) ->
        #{node => node(Pid), id => pid_to_list(Pid), creation => 1}
    end),
    meck:expect(rust_marshal, encode_ref, fun(_Ref) ->
        #{node => node(), id => "ref", creation => 1}
    end),
    meck:expect(rust_marshal, encode_port, fun(_Port) ->
        #{id => "port", name => "test_port"}
    end),
    meck:expect(rust_marshal, encode_timestamp, fun(X) -> X end),
    meck:expect(rust_marshal, encode_timestamp_us, fun(X) -> X end),
    meck:expect(rust_marshal, encode_timestamp_native, fun(X) -> X end),
    meck:expect(rust_marshal, encode_large_binary, fun(Bin) ->
        {ok, byte_size(Bin)}
    end),
    meck:expect(rust_marshal, stream_binary, fun(Bin, ChunkSize) ->
        Chunks = byte_size(Bin) div ChunkSize,
        {ok, lists:duplicate(Chunks, ChunkSize)}
    end),
    meck:expect(rust_marshal, decode_atom, fun
        (<<"true">>) -> true;
        (<<"false">>) -> false;
        (<<"undefined">>) -> undefined;
        (Bin) -> binary_to_existing_atom(Bin, utf8)
    end),
    meck:expect(rust_marshal, decode_pid, fun(#{id := IdStr}) ->
        list_to_pid(IdStr)
    end),
    meck:expect(rust_marshal, decode_ref, fun(_) -> make_ref() end),
    meck:expect(rust_marshal, decode_timestamp, fun(X) -> X end),

    ok.

mock_marshal_int(Int) -> Int.
mock_marshal_float(Float) ->
    case Float of
        infinity -> positive_infinity;
        neg_infinity -> negative_infinity;
        nan when Float =:= 0.0/0.0 -> not_a_number;
        _ -> Float
    end.
mock_marshal_bool(Bool) -> Bool.
mock_marshal_string(Str) -> Str.
mock_marshal_atom(Atom) -> atom_to_binary(Atom, utf8).
mock_marshal_list(List) -> List.
mock_marshal_tuple(Tuple) -> Tuple.
mock_marshal_map(Map) -> Map.
mock_marshal_nested(Nested) -> Nested.

%%====================================================================
%% Algorithm Mocks
%%====================================================================

setup_algorithm_mocks() ->
    meck:expect(rust_algorithm, discover, fun
        ([]) -> {error, empty_log};
        ([{_, _, _} | _] = Log) ->
            {ok, generate_model(Log)}
    end),
    meck:expect(rust_algorithm, discover, fun(Log, Options) ->
            Timeout = proplists:get_value(timeout, Options, 5000),
            case Timeout < 10 of
                true -> {error, timeout};
                false -> {ok, generate_model(Log)}
            end
    end),
    meck:expect(rust_algorithm, alpha, fun(Log) ->
        {ok, generate_model(Log)}
    end),
    meck:expect(rust_algorithm, heuristic, fun(Log, _Options) ->
        {ok, generate_model(Log)}
    end),
    meck:expect(rust_algorithm, conformance, fun(Log, Model) ->
        {ok, generate_conformance_result(Log, Model)}
    end),
    meck:expect(rust_algorithm, align_trace, fun(Trace, _Model) ->
        {ok, #{
            alignment => [{sync_move, A} || A <- Trace],
            cost => 0,
            trace => Trace,
            fitness => 1.0
        }}
    end),
    meck:expect(rust_algorithm, object_centric, fun(_OCELLog) ->
        {ok, #{
            object_types => [order, payment, item],
            activities => [create, link, ship],
            relations => []
        }}
    end),
    meck:expect(rust_algorithm, object_centric_relations, fun(_OCELLog) ->
        {ok, #{
            relations => [
                #{from => order, to => item, relation_type => contains},
                #{from => payment, to => order, relation_type => pays_for}
            ]
        }}
    end),
    meck:expect(rust_algorithm, llp_model, fun(_Descriptions, _Options) ->
        {ok, #{
            activities => [create, pay, ship],
            relations => [{create, pay}, {pay, ship}],
            confidence => 0.85
        }}
    end),
    meck:expect(rust_algorithm, local_models, fun(_Log, _Options) ->
        {ok, #{
            local_models => [
                #{activities => [a, b], support => 0.8, confidence => 0.9},
                #{activities => [b, c], support => 0.7, confidence => 0.8}
            ]
        }}
    end),

    ok.

mock_alpha(Log) ->
    {ok, generate_model(Log)}.

mock_heuristic(Log, Options) ->
    case proplists:get_value(timeout, Options) of
        N when is_integer(N), N < 10 ->
            {error, timeout};
        _ ->
            {ok, generate_model(Log)}
    end.

mock_conformance(Log, Model) ->
    {ok, generate_conformance_result(Log, Model)}.

mock_align_trace(Trace, Model) ->
    {ok, #{
        alignment => [{sync_move, A} || A <- Trace],
        cost => 0,
        trace => Trace,
        fitness => 1.0
    }}.

%%====================================================================
%% Resource Mocks
%%====================================================================

setup_resource_mocks() ->
    meck:expect(rust_resource, create, fun() ->
        ResourceId = list_to_atom("resource_" ++ integer_to_list(erlang:unique_integer([positive]))),
        ets:insert(rust_resources, {ResourceId, active}),
        {ok, ResourceId}
    end),
    meck:expect(rust_resource, create, fun(Options) ->
        ResourceId = list_to_atom("resource_" ++ integer_to_list(erlang:unique_integer([positive]))),
        Timeout = proplists:get_value(timeout, Options, infinity),
        ets:insert(rust_resources, {ResourceId, active, Timeout}),
        {ok, ResourceId}
    end),
    meck:expect(rust_resource, destroy, fun(ResourceId) ->
        ets:delete(rust_resources, ResourceId),
        ok
    end),
    meck:expect(rust_resource, status, fun(ResourceId) ->
        case ets:lookup(rust_resources, ResourceId) of
            [{_, active}] -> active;
            [{_, active, _}] -> active;
            _ -> not_found
        end
    end),
    meck:expect(rust_resource, execute, fun(ResourceId, _Operation) ->
        case ets:lookup(rust_resources, ResourceId) of
            [{_, active}] -> ok;
            _ -> {error, not_found}
        end
    end),
    meck:expect(rust_resource, get_result, fun(ResourceId) ->
        case ets:lookup(rust_resources, ResourceId) of
            [{_, active}] -> {ok, mock_result};
            _ -> {error, not_found}
        end
    end),
    meck:expect(rust_resource, invalid_operation, fun(_ResourceId) ->
        {error, invalid_operation}
    end),
    meck:expect(rust_resource, long_operation, fun(ResourceId) ->
        case ets:lookup(rust_resources, ResourceId) of
            [{_, active, Timeout}] when Timeout < 100 ->
                timer:sleep(Timeout),
                ok;
            [{_, active}] ->
                timer:sleep(2000),
                ok;
            _ ->
                {error, not_found}
        end
    end),
    meck:expect(rust_resource, create_port, fun() ->
        {ok, port_resource}
    end),
    meck:expect(rust_resource, port_operation, fun(_ResourceId, _Operation) -> ok end),
    meck:expect(rust_resource, create_nif, fun() ->
        {ok, nif_resource}
    end),
    meck:expect(rust_resource, tracked_count, fun() ->
        length(ets:tab2list(rust_resources))
    end),
    meck:expect(rust_resource, list_active, fun() ->
        [Id || {Id, active} <- ets:tab2list(rust_resources)]
    end),
    meck:expect(rust_resource, list_all, fun() ->
        [Id || {Id, _} <- ets:tab2list(rust_resources)]
    end),
    meck:expect(rust_resource, owner, fun(_ResourceId) -> undefined end),
    meck:expect(rust_resource, force_cleanup, fun(ResourceId) ->
        ets:delete(rust_resources, ResourceId),
        ok
    end),
    meck:expect(rust_resource, cleanup_all, fun() ->
        ets:delete_all_objects(rust_resources),
        ok
    end),
    meck:expect(rust_resource, memory_snapshot, fun() ->
        {ok, #{
            timestamp => erlang:system_time(millisecond),
            total_memory => erlang:memory(total),
            active_resources => length(ets:tab2list(rust_resources))
        }}
    end),
    meck:expect(rust_resource, leak_check, fun(Snapshot1, Snapshot2) ->
        Count1 = maps:get(active_resources, Snapshot1, 0),
        Count2 = maps:get(active_resources, Snapshot2, 0),
        {ok, #{
            leaked_resources => Count2 - Count1,
            leaked_bytes => 0
        }}
    end),

    ok.

mock_resource_create() ->
    {ok, mock_resource}.

mock_resource_destroy(_ResourceId) ->
    ok.

mock_resource_status(_ResourceId) ->
    active.

mock_resource_operation(_ResourceId, _Operation) ->
    ok.

%%====================================================================
%% Serialization Mocks
%%====================================================================

setup_serialize_mocks() ->
    meck:expect(rust_serialize, to_json, fun(Log) ->
        Json = io_lib:format("~p", [Log]),
        {ok, iolist_to_binary(Json)}
    end),
    meck:expect(rust_serialize, from_json, fun(JsonBin) ->
        %% Simple mock - just return success
        _ = is_binary(JsonBin),
        {ok, []}
    end),
    meck:expect(rust_serialize, to_xes, fun(_Log) ->
        XES = <<"<?xml version=\"1.0\"?><log></log>">>,
        {ok, XES}
    end),
    meck:expect(rust_serialize, to_binary, fun(Log) ->
        {ok, term_to_binary(Log)}
    end),
    meck:expect(rust_serialize, from_binary, fun(Bin) ->
        {ok, binary_to_term(Bin)}
    end),

    ok.

%%====================================================================
%% Benchmark Mocks
%%====================================================================

setup_benchmark_mocks() ->
    meck:expect(rust_benchmark, discover, fun(Log) ->
        %% Simulate processing time based on log size
        Size = length(Log),
        Time = Size div 10,
        {ok, Time}
    end),
    meck:expect(rust_benchmark, memory_usage, fun(_Log) ->
        {ok, #{
            total => erlang:memory(total),
            process => erlang:memory(processes),
            system => erlang:memory(system)
        }}
    end),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Generate a mock Petri net model from event log
generate_model(Log) ->
    %% Extract activities from log
    Activities = lists:usort([A || {_, A, _} <- Log]),
    Places = [list_to_atom("p_" ++ atom_to_list(A)) || A <- Activities],

    #{
        places => [i_source, o_sink | Places],
        transitions => Activities,
        arcs => generate_arcs(Activities),
        initial_place => i_source,
        final_place => o_sink,
        metadata => #{
            algorithm => alpha,
            cases_processed => length(lists:usort([C || {C, _, _} <- Log])),
            total_events => length(Log),
            noise_level => 0.0
        }
    }.

%% @doc Generate mock arcs from activities
generate_arcs([]) -> [];
generate_arcs([_]) -> [];
generate_arcs([First, Second | Rest]) ->
    [{First, p}, {p, Second} | generate_arcs([Second | Rest])] ++ [{i_source, First}, {Second, o_sink}].

%% @doc Generate mock conformance result
generate_conformance_result(_Log, _Model) ->
    #{
        fitness => 1.0,
        precision => 1.0,
        generalization => 1.0,
        replay_problems => #{
            missing => 0,
            remaining => 0,
            consumed => 3,
            produced => 3
        },
        trace_count => 1,
        event_count => 3,
        model_complexity => #{
            places => 2,
            transitions => 3,
            arcs => 6
        }
    }.
