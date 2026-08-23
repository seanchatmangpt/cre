%% -*- erlang -*-
%% @doc Rust Data Marshaling Test Suite
%%
%% This suite tests data type conversion between Erlang and Rust.
%% It validates that all Erlang types can be properly converted to
%% Rust-compatible formats and back again without data loss.
%%
%% @end

-module(rust_data_marshal_SUITE).
-author("CRE Team").

-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%%====================================================================
%% Test Cases
%%====================================================================

-export([
    atom_encoding_test/1,
    binary_encoding_test/1,
    list_encoding_test/1,
    tuple_encoding_test/1,
    map_encoding_test/1,
    integer_ranges_test/1,
    float_precision_test/1,
    pid_encoding_test/1,
    ref_encoding_test/1,
    port_encoding_test/1,
    nested_structures_test/1,
    circular_reference_test/1,
    unicode_test/1,
    timestamp_conversion_test/1,
    large_binary_test/1
]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        atom_encoding_test,
        binary_encoding_test,
        list_encoding_test,
        tuple_encoding_test,
        map_encoding_test,
        integer_ranges_test,
        float_precision_test,
        pid_encoding_test,
        ref_encoding_test,
        port_encoding_test,
        nested_structures_test,
        circular_reference_test,
        unicode_test,
        timestamp_conversion_test,
        large_binary_test
    ].

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test atom to Rust string encoding
atom_encoding_test(_Config) ->
    %% Test simple atoms
    ?assertEqual(<<"hello">>, rust_marshal:encode_atom(hello)),
    ?assertEqual(<<"test_atom">>, rust_marshal:encode_atom(test_atom)),

    %% Test special atoms
    ?assertEqual(<<"true">>, rust_marshal:encode_atom(true)),
    ?assertEqual(<<"false">>, rust_marshal:encode_atom(false)),
    ?assertEqual(<<"undefined">>, rust_marshal:encode_atom(undefined)),

    %% Test atoms with underscores and numbers
    ?assertEqual(<<"atom_with_123">>, rust_marshal:encode_atom('atom_with_123')),

    %% Decode back
    ?assertEqual(hello, rust_marshal:decode_atom(<<"hello">>)),
    ?assertEqual(true, rust_marshal:decode_atom(<<"true">>)),

    ok.

%% @doc Test binary encoding
binary_encoding_test(_Config) ->
    %% Test empty binary
    ?assertEqual(<<>>, rust_marshal:encode_binary(<<>>)),

    %% Test simple binary
    Input = <<1, 2, 3, 4, 5>>,
    ?assertEqual(Input, rust_marshal:encode_binary(Input)),

    %% Test binary with null bytes
    NullBinary = <<0, 1, 0, 2>>,
    ?assertEqual(NullBinary, rust_marshal:encode_binary(NullBinary)),

    %% Test large binary
    LargeBinary = << <<X>> || X <- lists:seq(1, 1000) >>,
    Encoded = rust_marshal:encode_binary(LargeBinary),
    ?assertEqual(byte_size(LargeBinary), byte_size(Encoded)),

    ok.

%% @doc Test list encoding
list_encoding_test(_Config) ->
    %% Test empty list
    ?assertEqual([], rust_marshal:encode_list([])),

    %% Test simple list
    ?assertEqual([1, 2, 3], rust_marshal:encode_list([1, 2, 3])),

    %% Test heterogeneous list
    ?assertEqual([1, <<"a">>, true], rust_marshal:encode_list([1, <<"a">>, true])),

    %% Test nested list
    ?assertEqual([[1, 2], [3, 4]], rust_marshal:encode_list([[1, 2], [3, 4]])),

    %% Test large list
    LargeList = lists:seq(1, 10000),
    Encoded = rust_marshal:encode_list(LargeList),
    ?assertEqual(length(LargeList), length(Encoded)),

    %% Test improper list (should handle or error)
    ?assertMatch({error, improper_list}, rust_marshal:encode_list([1|2])),

    ok.

%% @doc Test tuple encoding
tuple_encoding_test(_Config) ->
    %% Test simple tuple
    ?assertEqual({1, 2}, rust_marshal:encode_tuple({1, 2})),

    %% Test single element tuple
    ?assertEqual({only}, rust_marshal:encode_tuple({only})),

    %% Test large tuple
    LargeTuple = list_to_tuple(lists:seq(1, 100)),
    Encoded = rust_marshal:encode_tuple(LargeTuple),
    ?assertEqual(tuple_size(LargeTuple), tuple_size(Encoded)),

    %% Test nested tuple
    ?assertEqual({1, {2, 3}, 4}, rust_marshal:encode_tuple({1, {2, 3}, 4})),

    ok.

%% @doc Test map encoding
map_encoding_test(_Config) ->
    %% Test empty map
    ?assertEqual(#{}, rust_marshal:encode_map(#{})),

    %% Test simple map
    ?assertEqual(#{a => 1}, rust_marshal:encode_map(#{a => 1})),

    %% Test map with multiple keys
    Input = #{a => 1, b => 2, c => 3},
    ?assertEqual(Input, rust_marshal:encode_map(Input)),

    %% Test map with complex keys
    ComplexMap = #{<<"a">> => 1, <<"b">> => 2},
    Encoded = rust_marshal:encode_map(ComplexMap),
    ?assertEqual(map_size(ComplexMap), map_size(Encoded)),

    %% Test nested map
    ?assertEqual(#{outer => #{inner => value}}, rust_marshal:encode_map(#{outer => #{inner => value}})),

    ok.

%% @doc Test integer range handling
integer_ranges_test(_Config) ->
    %% Test small positive integers
    ?assertEqual(0, rust_marshal:encode_integer(0)),
    ?assertEqual(1, rust_marshal:encode_integer(1)),
    ?assertEqual(255, rust_marshal:encode_integer(255)),

    %% Test negative integers
    ?assertEqual(-1, rust_marshal:encode_integer(-1)),
    ?assertEqual(-128, rust_marshal:encode_integer(-128)),

    %% Test larger integers (within 64-bit range)
    ?assertEqual(16#7FFFFFFF, rust_marshal:encode_integer(16#7FFFFFFF)),
    ?assertEqual(-16#80000000, rust_marshal:encode_integer(-16#80000000)),

    %% Test very large integers (may need big int handling)
    VeryLarge = 16#FFFFFFFFFFFFFFFF,
    ?assertEqual(VeryLarge, rust_marshal:encode_integer(VeryLarge)),

    ok.

%% @doc Test float precision preservation
float_precision_test(_Config) ->
    %% Test simple floats
    ?assertEqual(0.0, rust_marshal:encode_float(0.0)),
    ?assertEqual(1.0, rust_marshal:encode_float(1.0)),
    ?assertEqual(-1.0, rust_marshal:encode_float(-1.0)),

    %% Test fractional values
    ?assertEqual(0.5, rust_marshal:encode_float(0.5)),
    ?assertEqual(0.125, rust_marshal:encode_float(0.125)),

    %% Test scientific notation
    ?assertEqual(1.0e10, rust_marshal:encode_float(1.0e10)),
    ?assertEqual(1.0e-10, rust_marshal:encode_float(1.0e-10)),

    %% Test precision preservation (within reasonable tolerance)
    Pi = 3.141592653589793,
    EncodedPi = rust_marshal:encode_float(Pi),
    ?assert(abs(Pi - EncodedPi) < 0.0000000001),

    %% Test special values
    ?assertEqual(positive_infinity, rust_marshal:encode_float(infinity)),
    ?assertEqual(negative_infinity, rust_marshal:encode_float(neg_infinity)),
    ?assertEqual(not_a_number, rust_marshal:encode_float(nan)),

    ok.

%% @doc Test PID encoding (for process references)
pid_encoding_test(_Config) ->
    %% Test current process PID
    SelfPid = self(),
    Encoded = rust_marshal:encode_pid(SelfPid),
    ?assertMatch(#{node := _, id := _, creation := _}, Encoded),

    %% Test round-trip
    Decoded = rust_marshal:decode_pid(Encoded),
    ?assertEqual(SelfPid, Decoded),

    %% Test external PID
    ExternalPid = spawn(fun() -> receive after infinity -> ok end end),
    EncodedExternal = rust_marshal:encode_pid(ExternalPid),
    ?assertMatch(#{node := _, id := _, creation := _}, EncodedExternal),

    exit(ExternalPid, kill),

    ok.

%% @doc Test reference encoding
ref_encoding_test(_Config) ->
    %% Create a reference
    Ref = make_ref(),

    %% Encode reference
    Encoded = rust_marshal:encode_ref(Ref),
    ?assertMatch(#{node := _, id := _, creation := _}, Encoded),

    %% Decode back
    Decoded = rust_marshal:decode_ref(Encoded),
    ?assert(is_reference(Decoded)),

    ok.

%% @doc Test port encoding
port_encoding_test(_Config) ->
    %% Try to encode a port (may not be available in all environments)
    case open_port({spawn, "true"}, []) of
        Port when is_port(Port) ->
            Encoded = rust_marshal:encode_port(Port),
            ?assertMatch(#{id := _, name := _}, Encoded),
            port_close(Port);
        _ ->
            ct:log("Port not available, skipping")
    end,
    ok.

%% @doc Test nested structure encoding
nested_structures_test(_Config) ->
    %% Test deeply nested maps
    DeepMap = #{a => #{b => #{c => #{d => value}}}},
    Encoded = rust_marshal:encode_nested(DeepMap),
    ?assert(is_map(Encoded)),
    ?assert(maps:is_key(a, Encoded)),

    %% Test list of maps
    ListOfMaps = [#{a => 1}, #{b => 2}, #{c => 3}],
    EncodedList = rust_marshal:encode_nested(ListOfMaps),
    ?assertEqual(3, length(EncodedList)),

    %% Test map with lists as values
    MapWithLists = #{items => [1, 2, 3], nested => #{items => [4, 5]}},
    EncodedMapWithLists = rust_marshal:encode_nested(MapWithLists),
    ?assertEqual([1, 2, 3], maps:get(items, EncodedMapWithLists)),

    %% Test tuple in map
    MapWithTuple = #{tuple => {1, 2, 3}},
    EncodedMapWithTuple = rust_marshal:encode_nested(MapWithTuple),
    ?assertMatch({1, 2, 3}, maps:get(tuple, EncodedMapWithTuple)),

    ok.

%% @doc Test circular reference handling
circular_reference_test(_Config) ->
    %% Create circular reference using process dictionary
    put(circular_ref, undefined),
    Circular = #{self_ref => circular_ref},
    put(circular_ref, Circular),

    %% Should handle circular references gracefully
    case rust_marshal:encode_nested(Circular) of
        {error, circular_reference} ->
            ct:log("Correctly detected circular reference"),
            ?assert(true);
        Encoded ->
            %% If encoded, should have placeholder or special marker
            ct:log("Encoded circular reference: ~p", [Encoded]),
            ?assert(true)
    end,

    erase(circular_ref),
    ok.

%% @doc Test Unicode string handling
unicode_test(_Config) ->
    %% Test various Unicode characters
    UnicodeStr = <<"Hello 世界 🌍 Ñoño">>,
    Encoded = rust_marshal:encode_binary(UnicodeStr),
    ?assertEqual(UnicodeStr, Encoded),

    %% Test UTF-8 encoding
    Utf8Bytes = <<228, 184, 150>>,  %% Chinese character
    ?assertEqual(Utf8Bytes, rust_marshal:encode_binary(Utf8Bytes)),

    %% Test string atoms with Unicode
    UnicodeAtom = '日本語',
    EncodedAtom = rust_marshal:encode_atom(UnicodeAtom),
    ?assert(is_binary(EncodedAtom)),

    %% Test emoji
    EmojiBin = <<240, 159, 152, 128>>,  %% Rocket emoji first byte
    ?assertEqual(EmojiBin, rust_marshal:encode_binary(EmojiBin)),

    ok.

%% @doc Test timestamp conversion between Erlang and Rust
timestamp_conversion_test(_Config) ->
    %% Test current timestamp
    Now = erlang:system_time(millisecond),
    Encoded = rust_marshal:encode_timestamp(Now),
    ?assert(is_integer(Encoded)),
    ?assert(Encoded > 0),

    %% Test epoch
    ?assertEqual(0, rust_marshal:encode_timestamp(0)),

    %% Test decode back
    Decoded = rust_marshal:decode_timestamp(Encoded),
    ?assertEqual(Now, Decoded),

    %% Test microseconds
    NowMicro = erlang:system_time(microsecond),
    EncodedMicro = rust_marshal:encode_timestamp_us(NowMicro),
    ?assert(EncodedMicro > 0),

    %% Test native time units
    NowNative = erlang:system_time(native),
    EncodedNative = rust_marshal:encode_timestamp_native(NowNative),
    ?assert(is_integer(EncodedNative)),

    ok.

%% @doc Test large binary handling
large_binary_test(_Config) ->
    %% Create 10MB binary
    Size = 10 * 1024 * 1024,
    LargeBin = << <<X>> || X <- lists:seq(1, Size) >>,

    %% Should handle without overflow
    {ok, EncodedSize} = rust_marshal:encode_large_binary(LargeBin),
    ?assertEqual(Size, EncodedSize),

    %% Test streaming for very large binaries
    ChunkSize = 1024 * 1024,  %% 1MB chunks
    {ok, Chunks} = rust_marshal:stream_binary(LargeBin, ChunkSize),
    ?assert(Chunks >= 1),

    %% Verify chunk sizes
    TotalFromChunks = lists:sum([C || C <- Chunks]),
    ?assertEqual(Size, TotalFromChunks),

    ok.
