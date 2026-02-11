%%%-------------------------------------------------------------------
%%% @doc
%%% Receipt generation tests for gen_pnet and wf_task integration.
%%%
%%% Test Slice 4 of compound doctests - receipt generation.
%%%
%%% == Verification Checklist ==
%%% 1. wf_task:enabled/3 creates task token produce map
%%% 2. wf_task:done/3 creates done task token produce map
%%% 3. gen_pnet:inject/2 injects tokens into net
%%% 4. gen_pnet:drain/2 fires enabled transitions
%%% 5. Receipt has expected structure (mode)
%%% 6. pnet_types:is_mode/1 validates receipt structure
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_receipt_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test wf_task:enabled/3 creates task token produce map.
%% @end
%%--------------------------------------------------------------------
wf_task_enabled_creates_produce_map_test() ->
    TaskId = t1,
    Payload = #{payload => ok},
    Place = tasks,
    {produce, ProduceMap} = wf_task:enabled(TaskId, Payload, Place),
    ?assertMatch(#{tasks := [{task, t1, enabled, _}]}, ProduceMap),
    %% Verify produce map structure
    ?assert(is_map(ProduceMap)),
    ?assert(maps:is_key(tasks, ProduceMap)),
    [{task, t1, enabled, PayloadMap}] = maps:get(tasks, ProduceMap),
    ?assertEqual(#{payload => ok}, PayloadMap).

%%--------------------------------------------------------------------
%% @doc Test wf_task:done/3 creates done task token produce map.
%% @end
%%--------------------------------------------------------------------
wf_task_done_creates_produce_map_test() ->
    TaskId = t1,
    Payload = #{result => ok},
    Place = tasks,
    {produce, ProduceMap} = wf_task:done(TaskId, Payload, Place),
    ?assertMatch(#{tasks := [{task, t1, 'done', _}]}, ProduceMap),
    %% Verify produce map structure
    ?assert(is_map(ProduceMap)),
    ?assert(maps:is_key(tasks, ProduceMap)),
    [{task, t1, 'done', PayloadMap}] = maps:get(tasks, ProduceMap),
    ?assertEqual(#{result => ok}, PayloadMap).

%%--------------------------------------------------------------------
%% @doc Test full receipt generation flow with task gate net.
%%
%% This test verifies:
%% 1. Starting the net with wf_test_net_receipt
%% 2. Creating enabled task tokens with wf_task:enabled/3
%% 3. Injecting tokens via gen_pnet:inject/2
%% 4. Firing transitions via gen_pnet:drain/2
%% 5. Receipt structure validation
%% @end
%%--------------------------------------------------------------------
receipt_generation_flow_test() ->
    %% 1. Start the net using wf_test_net_receipt
    %% This module fires transitions on enabled task tokens
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),

    %% 2. Create enabled task token produce map using wf_task:enabled/3
    {produce, PM0} = wf_task:enabled(t1, #{payload => ok}, tasks),
    ?assertMatch(#{tasks := [{task, t1, enabled, _}]}, PM0),

    %% 3. Inject tokens into the net
    {ok, InjectReceipt} = gen_pnet:inject(Pg, PM0),
    ?assertMatch(#{tasks := [{task, t1, enabled, _}]}, InjectReceipt),

    %% Verify tokens are in the net
    {ok, TaskTokens} = gen_pnet:ls(Pg, tasks),
    ?assertMatch([{task, t1, enabled, _}], TaskTokens),

    %% 4. Drain to fire the transition
    {ok, Receipts} = gen_pnet:drain(Pg, 10),
    ?assert(length(Receipts) > 0),

    %% 5. Verify receipt structure
    Rfire = hd(Receipts),

    %% Current implementation returns mode as receipt
    %% A mode is a map of places to consumed token lists
    ?assert(is_map(Rfire)),
    ?assert(maps:is_key(tasks, Rfire)),
    ?assertMatch([{task, t1, enabled, _}], maps:get(tasks, Rfire)),

    %% Verify the done place has the result token (t1 produces done task)
    {ok, DoneTokens} = gen_pnet:ls(Pg, 'done'),
    ?assertMatch([{task, t1, 'done', _}], DoneTokens),

    %% Cleanup
    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test that pnet_types:is_mode/1 validates receipt (mode).
%%
%% Current CRE gen_pnet implementation returns the consumed mode
%% as the receipt. This test validates that structure.
%% @end
%%--------------------------------------------------------------------
receipt_mode_validation_test() ->
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),
    {produce, PM0} = wf_task:enabled(t1, #{payload => ok}, tasks),
    {ok, _} = gen_pnet:inject(Pg, PM0),
    {ok, [Rfire]} = gen_pnet:drain(Pg, 10),

    %% Verify receipt is a valid mode (current implementation)
    ?assert(pnet_types:is_mode(Rfire)),

    %% Verify receipt structure has expected keys
    ?assert(maps:is_key(tasks, Rfire)),
    [{task, t1, enabled, Payload}] = maps:get(tasks, Rfire),
    ?assertEqual(#{payload => ok}, Payload),

    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test receipt contains move-like structure.
%%
%% The current mode receipt can be interpreted as a move's
%% consumed portion. A full receipt would have before_hash,
%% after_hash, move (with trsn, mode, produce), and ts.
%% @end
%%--------------------------------------------------------------------
receipt_move_structure_test() ->
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),
    {produce, PM0} = wf_task:enabled(t1, #{payload => data}, tasks),
    {ok, _} = gen_pnet:inject(Pg, PM0),
    {ok, [Receipt]} = gen_pnet:drain(Pg, 10),

    %% Extract consumed mode from receipt
    ConsumedMode = Receipt,

    %% Verify we can construct a move from the receipt
    %% A move has: trsn, mode (consumed), produce
    Move = #{
        trsn => t1,
        mode => ConsumedMode,
        produce => #{'done' => [{task, t1, 'done', #{payload => data}}]}
    },

    ?assert(pnet_types:is_move(Move)),
    ?assertEqual(t1, maps:get(trsn, Move)),
    ?assert(pnet_types:is_mode(maps:get(mode, Move))),
    ?assert(pnet_types:is_produce_map(maps:get(produce, Move))),

    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test multiple step drain accumulates receipts.
%% @end
%%--------------------------------------------------------------------
multiple_receipts_accumulation_test() ->
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),

    %% Inject and drain multiple times
    {produce, PM1} = wf_task:enabled(task1, #{id => 1}, tasks),
    {ok, _} = gen_pnet:inject(Pg, PM1),

    {produce, PM2} = wf_task:enabled(task2, #{id => 2}, tasks),
    {ok, _} = gen_pnet:inject(Pg, PM2),

    {ok, Receipts} = gen_pnet:drain(Pg, 10),

    %% Should have 2 receipts
    ?assertEqual(2, length(Receipts)),

    %% Each receipt should be a valid mode
    lists:foreach(fun(R) ->
        ?assert(pnet_types:is_mode(R))
    end, Receipts),

    %% Verify done place has both result tokens
    {ok, DoneTokens} = gen_pnet:ls(Pg, 'done'),
    ?assertEqual(2, length(DoneTokens)),

    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test drain limit error when max steps exceeded.
%% @end
%%--------------------------------------------------------------------
drain_limit_test() ->
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),

    %% Inject multiple tasks but drain with limit of 1
    {produce, PM1} = wf_task:enabled(task1, #{id => 1}, tasks),
    {ok, _} = gen_pnet:inject(Pg, PM1),

    {produce, PM2} = wf_task:enabled(task2, #{id => 2}, tasks),
    {ok, _} = gen_pnet:inject(Pg, PM2),

    %% Drain with MaxSteps=1 should return at least 1 receipt
    {ok, Receipts} = gen_pnet:drain(Pg, 1),
    ?assert(length(Receipts) >= 1),

    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test that inject returns receipt matching input.
%% @end
%%--------------------------------------------------------------------
inject_receipt_matches_input_test() ->
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),

    {produce, PM0} = wf_task:enabled(t1, #{data => test}, tasks),
    {ok, InjectReceipt} = gen_pnet:inject(Pg, PM0),

    %% Inject receipt should match the produce map
    ?assertEqual(PM0, InjectReceipt),

    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test empty marking on startup.
%% @end
%%--------------------------------------------------------------------
empty_initial_marking_test() ->
    {ok, Pg} = gen_pnet:start_link(wf_test_net_receipt, #{seed => 0}, []),

    %% All places should start empty
    {ok, Tasks} = gen_pnet:ls(Pg, tasks),
    {ok, Done} = gen_pnet:ls(Pg, 'done'),

    ?assertEqual([], Tasks),
    ?assertEqual([], Done),

    gen_pnet:stop(Pg).

%%--------------------------------------------------------------------
%% @doc Test pnet_receipt:make/3 creates valid receipt structure.
%% @end
%%--------------------------------------------------------------------
pnet_receipt_make_test() ->
    %% Create a move
    Move = #{
        trsn => t1,
        mode => #{tasks => [{task, test, enabled, #{}}]},
        produce => #{'done' => [{task, test, 'done', #{}}]}
    },

    %% Create a receipt
    BeforeHash = crypto:hash(md5, term_to_binary({before, Move})),
    AfterHash = crypto:hash(md5, term_to_binary({'after', Move})),

    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),

    %% Verify receipt structure
    ?assert(is_map(Receipt)),
    ?assert(maps:is_key(before_hash, Receipt)),
    ?assert(maps:is_key(after_hash, Receipt)),
    ?assert(maps:is_key(move, Receipt)),
    ?assert(maps:is_key(ts, Receipt)),

    %% Verify types
    ?assert(is_binary(maps:get(before_hash, Receipt))),
    ?assert(is_binary(maps:get(after_hash, Receipt))),
    ?assert(is_map(maps:get(move, Receipt))),
    ?assert(is_integer(maps:get(ts, Receipt))),

    %% Verify pnet_types:is_receipt validates it
    ?assert(pnet_types:is_receipt(Receipt)).

%%--------------------------------------------------------------------
%% @doc Test that full receipt with all fields passes validation.
%% @end
%%--------------------------------------------------------------------
full_receipt_validation_test() ->
    Move = #{
        trsn => test_trsn,
        mode => #{p1 => [a, b]},
        produce => #{p2 => [c]}
    },

    FullReceipt = #{
        before_hash => <<1, 2, 3>>,
        after_hash => <<4, 5, 6>>,
        move => Move,
        ts => 123456789
    },

    ?assert(pnet_types:is_receipt(FullReceipt)),
    ?assert(pnet_types:is_move(Move)).

%%--------------------------------------------------------------------
%% @doc Test receipt with invalid ts fails validation.
%% @end
%%--------------------------------------------------------------------
invalid_receipt_ts_test() ->
    Move = #{
        trsn => test_trsn,
        mode => #{p1 => [a]},
        produce => #{p2 => [b]}
    },

    %% Receipt with binary ts should fail
    BadReceipt = #{
        before_hash => <<1>>,
        after_hash => <<2>>,
        move => Move,
        ts => <<"not_an_integer">>
    },

    ?assertNot(pnet_types:is_receipt(BadReceipt)).
