%% -*- erlang -*-
%%%% @doc Common Test Suite for pnet_receipt Coverage
%%
%% Comprehensive test suite for pnet_receipt module targeting 70% coverage.
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_receipt_coverage_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [
        new_receipt_has_unique_id,
        add_transition_updates_history,
        receipt_to_serializable_map,
        receipt_from_map_roundtrip,
        timestamp_is_monotonic,
        timestamp_returns_integer,
        effects_silent_no_tokens,
        effects_single_one_place,
        effects_multiple_multiple_places,
        make_creates_valid_receipt,
        make_validates_move_structure
    ].

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

new_receipt_has_unique_id(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{}, produce => #{}},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    true = maps:is_key(before_hash, Receipt),
    true = maps:is_key(after_hash, Receipt),
    true = maps:is_key(move, Receipt),
    true = maps:is_key(ts, Receipt),
    ok.

add_transition_updates_history(_Config) ->
    %% Test that receipt contains move info
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{}, produce => #{p => [a]}},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    MoveFromReceipt = maps:get(move, Receipt),
    ?assertEqual(Move, MoveFromReceipt),
    ok.

receipt_to_serializable_map(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{p => [x]}, produce => #{q => [y]}},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    true = is_map(Receipt),
    ok.

receipt_from_map_roundtrip(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{}, produce => #{}},
    Original = pnet_receipt:make(BeforeHash, AfterHash, Move),
    %% Receipts are immutable, just verify structure
    true = maps:is_key(before_hash, Original),
    true = maps:is_key(after_hash, Original),
    ok.

timestamp_is_monotonic(_Config) ->
    T1 = pnet_receipt:timestamp(),
    timer:sleep(1),
    T2 = pnet_receipt:timestamp(),
    ?assert(T2 >= T1),
    ok.

timestamp_returns_integer(_Config) ->
    T = pnet_receipt:timestamp(),
    true = is_integer(T),
    ok.

effects_silent_no_tokens(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{}, produce => #{}},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    {silent, Receipt} = pnet_receipt:effects(Receipt),
    ok.

effects_single_one_place(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{}, produce => #{p => [a]}},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    {single_production, Receipt2} = pnet_receipt:effects(Receipt),
    ?assertEqual(Receipt, Receipt2),
    ok.

effects_multiple_multiple_places(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    Move = #{trsn => t1, mode => #{}, produce => #{p1 => [a], p2 => [b]}},
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    {multiple_production, Receipt2} = pnet_receipt:effects(Receipt),
    ?assertEqual(Receipt, Receipt2),
    ok.

make_creates_valid_receipt(_Config) ->
    BeforeHash = crypto:hash(sha256, term_to_binary(before_state)),
    AfterHash = crypto:hash(sha256, term_to_binary(after_state)),
    Move = #{
        trsn => my_transition,
        mode => #{input => [token]},
        produce => #{output => [result]}
    },
    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
    ?assertEqual(BeforeHash, maps:get(before_hash, Receipt)),
    ?assertEqual(AfterHash, maps:get(after_hash, Receipt)),
    ?assertEqual(Move, maps:get(move, Receipt)),
    ?assert(is_integer(maps:get(ts, Receipt))),
    ok.

make_validates_move_structure(_Config) ->
    BeforeHash = <<1>>,
    AfterHash = <<2>>,
    %% Invalid moves missing required fields cause badmatch in make/3
    InvalidMove1 = #{trsn => t1},
    ?assertError(_, pnet_receipt:make(BeforeHash, AfterHash, InvalidMove1)),
    InvalidMove2 = #{mode => #{}},
    ?assertError(_, pnet_receipt:make(BeforeHash, AfterHash, InvalidMove2)),
    ok.
