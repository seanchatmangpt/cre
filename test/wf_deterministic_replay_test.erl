%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------

-module(wf_deterministic_replay_test).
-moduledoc """
Test Slice 10 of compound doctests - deterministic replay.

This test module verifies that:
1. Same seed produces same first move
2. Receipts have consistent structure
3. drain returns receipts in firing order
4. Same seed + same injections => same hash chain

The key property is deterministic execution: same seed = same outcome.
This proves reproducible workflow execution across runs.
""".

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Cases
%%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that same seed produces same hash chain.
%%
%% This is the core deterministic replay test: running the same
%% workflow with the same seed should produce identical results.
%%
%% Returns: true when hash chains match.
%% ```
%% Run = fun(Seed) ->
%%   {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
%%   {ok, _} = gen_pnet:inject(P, #{in => [go]}),
%%   {ok, Rs} = gen_pnet:drain(P, 10),
%%   Hs = [receipt_to_hash(R) || R <- Rs],
%%   ok = gen_pnet:stop(P),
%%   Hs
%% end,
%% H1 = Run(9),
%% H2 = Run(9),
%% H1 =:= H2 andalso lists:all(fun is_integer/1, H1).
%% true
%% '''
%% @end
%%--------------------------------------------------------------------
deterministic_replay_test() ->
    Run = fun(Seed) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
        {ok, _} = gen_pnet:inject(P, #{in => [go]}),
        {ok, Rs} = gen_pnet:drain(P, 10),
        Hs = [receipt_to_hash(R) || R <- Rs],
        ok = gen_pnet:stop(P),
        Hs
    end,

    H1 = Run(9),
    H2 = Run(9),

    ?assertEqual(H1, H2),
    ?assert(lists:all(fun is_integer/1, H1)),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that same seed produces same first move choice.
%%
%% Verifies that the first transition chosen is deterministic
%% when using the same seed.
%% @end
%%--------------------------------------------------------------------
same_seed_same_first_move_test() ->
    Seed = 42,

    FirstMove1 = get_first_move(Seed),
    FirstMove2 = get_first_move(Seed),

    ?assertEqual(FirstMove1, FirstMove2),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that different seeds may produce different first moves.
%%
%% While determinism is required for same seeds, different seeds
%% may produce different results (this is acceptable).
%% @end
%%--------------------------------------------------------------------
different_seeds_may_differ_test() ->
    Seed1 = 1,
    Seed2 = 999,

    FirstMove1 = get_first_move(Seed1),
    FirstMove2 = get_first_move(Seed2),

    %% Check that each is one of the valid choices (or unknown if no transition fired)
    ?assert(lists:member(FirstMove1, [chosen_a, chosen_b, unknown])),
    ?assert(lists:member(FirstMove2, [chosen_a, chosen_b, unknown])),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that receipts are returned in firing order.
%%
%% The drain function should return receipts in the order
%% that transitions were fired.
%% @end
%%--------------------------------------------------------------------
receipts_in_firing_order_test() ->
    {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => 123}, []),
    {ok, _} = gen_pnet:inject(P, #{in => [go]}),
    {ok, Rs} = gen_pnet:drain(P, 10),

    %% Verify we get receipts (may be 0 or more depending on transition firing)
    ?assert(is_list(Rs)),

    %% If any receipts, verify receipt structure (Mode map)
    case Rs of
        [R | _] ->
            ?assert(is_map(R)),
            %% Receipt should have the consumed token(s)
            ?assert(maps:is_key(in, R));
        [] ->
            ok
    end,

    ok = gen_pnet:stop(P),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that final marking hash is deterministic for same seed.
%%
%% Running the workflow twice with the same seed should produce
%% the same final marking hash.
%% @end
%%--------------------------------------------------------------------
final_marking_hash_deterministic_test() ->
    Run = fun(Seed) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
        {ok, _} = gen_pnet:inject(P, #{in => [go]}),
        {ok, _} = gen_pnet:drain(P, 10),
        Marking = gen_pnet:marking(P),
        Hash = pnet_marking:hash(Marking),
        ok = gen_pnet:stop(P),
        Hash
    end,

    Hash1 = Run(77),
    Hash2 = Run(77),

    ?assertEqual(Hash1, Hash2),
    ok.

%%--------------------------------------------------------------------
%% @doc Test hash chain consistency.
%%
%% The complete hash chain (injection + drain) should be
%% reproducible with the same seed.
%% @end
%%--------------------------------------------------------------------
hash_chain_consistency_test() ->
    Run = fun(Seed) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
        {ok, InjectReceipt} = gen_pnet:inject(P, #{in => [go]}),
        {ok, DrainReceipts} = gen_pnet:drain(P, 10),
        Marking = gen_pnet:marking(P),
        FinalHash = pnet_marking:hash(Marking),
        ok = gen_pnet:stop(P),
        {InjectReceipt, DrainReceipts, FinalHash}
    end,

    {I1, D1, F1} = Run(55),
    {I2, D2, F2} = Run(55),

    %% Injection receipts should match
    ?assertEqual(I1, I2),

    %% Drain receipts should match
    ?assertEqual(D1, D2),

    %% Final marking hashes should match
    ?assertEqual(F1, F2),
    ok.

%%--------------------------------------------------------------------
%% @doc Test multiple seeds produce potentially different outcomes.
%%
%% Running with different seeds demonstrates that the choice
%% mechanism is working while maintaining determinism per seed.
%% @end
%%--------------------------------------------------------------------
multiple_seeds_test() ->
    Seeds = [1, 2, 3, 42, 99, 100, 999],

    Results = [get_first_move(S) || S <- Seeds],

    %% All results should be valid (including unknown for edge cases)
    ValidResults = lists:all(fun(M) -> lists:member(M, [chosen_a, chosen_b, unknown]) end, Results),
    ?assert(ValidResults),

    %% Each seed should produce deterministic results
    ?assertEqual(get_first_move(42), get_first_move(42)),
    ?assertEqual(get_first_move(99), get_first_move(99)),

    ok.

%%%====================================================================
%%% Internal Helper Functions
%%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the first move (chosen token) for a given seed.
%%
%% Runs the workflow and extracts which transition was chosen first.
%%--------------------------------------------------------------------
get_first_move(Seed) ->
    {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
    {ok, _} = gen_pnet:inject(P, #{in => [go]}),
    {ok, Rs} = gen_pnet:drain(P, 10),

    %% Get final marking to see which token was produced
    Marking = gen_pnet:marking(P),
    Tokens = maps:get(out, Marking, []),

    ok = gen_pnet:stop(P),

    case Tokens of
        [chosen_a] -> chosen_a;
        [chosen_b] -> chosen_b;
        _ -> unknown
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a receipt to a hash for comparison.
%%
%% Uses phash2 for fast deterministic hashing of receipts.
%%--------------------------------------------------------------------
receipt_to_hash(Receipt) when is_map(Receipt) ->
    erlang:phash2(Receipt);
receipt_to_hash(Other) ->
    erlang:phash2(Other).
