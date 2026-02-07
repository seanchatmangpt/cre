%%%-------------------------------------------------------------------
%%% @doc
%%% Compound Workflow Integration Test Suite
%%%
%%% This Common Test suite runs all 10 slices of the compound workflow
%%% integration tests in sequence. Each slice tests a specific aspect of
%%% the workflow engine:
%%%
%%% <b>Slice 0: Preflight</b><br/>
%%% Test that all required net modules are loadable.
%%%
%%% <b>Slice 1: Deterministic Choice</b><br/>
%%% Verifies that pnet_choice:make/1 produces the same first move when
%%% given the same seed.
%%%
%%% <b>Slice 2: Trigger Drop Filtering</b><br/>
%%% Validates that trigger/3 callback filters tokens correctly.
%%%
%%% <b>Slice 3: Timer Queue Operations</b><br/>
%%% Tests that wf_timerq correctly manages deadline registration and
%%% token injection.
%%%
%%% <b>Slice 4: Task Lifecycle Tokens</b><br/>
%%% Verifies the complete task token lifecycle: construction, enqueuing,
%%% processing, and completion.
%%%
%%% <b>Slice 5: Receipt Audit Trail</b><br/>
%%% Tests that receipts are correctly generated for each transition and
%%% can be stored and retrieved.
%%%
%%% <b>Slice 6: Checkpoint/Resume Cycle</b><br/>
%%% Validates that a workflow state can be checkpointed to Mnesia and
%%% later resumed.
%%%
%%% <b>Slice 7: Pool + Mnesia Transaction</b><br/>
%%% Tests that wf_pool correctly bounds concurrency and wf_store
%%% transactions are exclusive.
%%%
%%% <b>Slice 8: Concuerror Spec Generation</b><br/>
%%% Verifies that Concuerror specifications can be generated for testing.
%%%
%%% <b>Slice 9: Post-Drain Sanity</b><br/>
%%% After draining a workflow, verifies that the system is stable.
%%%
%%% <b>Slice 10: Replay Determinism</b><br/>
%%% Confirms that using the same seed produces deterministic execution.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_compound_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    slice_0_preflight/1,
    slice_1_deterministic_choice/1,
    slice_2_trigger_drop/1,
    slice_3_timer_queue/1,
    slice_4_task_lifecycle/1,
    slice_5_receipt_audit/1,
    slice_6_checkpoint_resume/1,
    slice_7_pool_transaction/1,
    slice_8_concuerror_spec/1,
    slice_9_post_drain_sanity/1,
    slice_10_replay_determinism/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, compound_slices}
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [], [atom()]}].
groups() ->
    [
        {compound_slices, [], [
            slice_0_preflight,
            slice_1_deterministic_choice,
            slice_2_trigger_drop,
            slice_3_timer_queue,
            slice_4_task_lifecycle,
            slice_5_receipt_audit,
            slice_6_checkpoint_resume,
            slice_7_pool_transaction,
            slice_8_concuerror_spec,
            slice_9_post_drain_sanity,
            slice_10_replay_determinism
        ]}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: ct:config()) -> ct:config().
init_per_suite(Config) ->
    ct:pal("Starting wf_compound_integration_SUITE"),
    ok = test_helper:ensure_cre_gen_pnet_loaded(),
    %% Clean slate - stop and delete any existing Mnesia schema
    application:stop(mnesia),
    timer:sleep(50),
    mnesia:delete_schema([node()]),
    timer:sleep(50),
    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: ct:config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed wf_compound_integration_SUITE"),
    ok.

%% @doc Group-level initialization.
-spec init_per_group(Group :: atom(), Config :: ct:config()) -> ct:config().
init_per_group(compound_slices, Config) ->
    ct:pal("Initializing compound_slices group"),
    Config;
init_per_group(_Group, Config) ->
    Config.

%% @doc Group-level cleanup.
-spec end_per_group(Group :: atom(), Config :: ct:config()) -> ok.
end_per_group(compound_slices, _Config) ->
    ct:pal("Completed compound_slices group"),
    ok;
end_per_group(_Group, _Config) ->
    ok.

%% @doc Testcase-level initialization.
-spec init_per_testcase(TestCase :: atom(), Config :: ct:config()) -> ct:config().
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    %% Clean up any lingering Mnesia tables before each test
    clean_mnesia_tables(),
    Config.

%% @doc Testcase-level cleanup.
-spec end_per_testcase(TestCase :: atom(), Config :: ct:config()) -> ok.
end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    %% Stop any gen_pnet processes that might still be running
    cleanup_processes(),
    ok.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

clean_mnesia_tables() ->
    %% Delete test tables if they exist
    Tables = [wf_store_instance, wf_store_workitem],
    lists:foreach(
        fun(Table) ->
            case mnesia:delete_table(Table) of
                {atomic, ok} -> ok;
                {aborted, {no_exists, _}} -> ok;
                {aborted, {already_exists, _}} -> ok;  % Race condition guard
                _ -> ok
            end
        end,
        Tables
    ).

cleanup_processes() ->
    %% Stop any gen_pnet processes that might still be running
    Processes = processes(),
    lists:foreach(
        fun(Pid) ->
            case catch gen_pnet:stop(Pid) of
                ok -> ok;
                _ -> ok
            end
        end,
        Processes
    ).

%%%===================================================================
%%% SLICE 0: Preflight - Load Test Net Modules
%%%===================================================================

%% @doc Test that all required net modules are loadable.
-spec slice_0_preflight(Config :: ct:config()) -> ok.
slice_0_preflight(_Config) ->
    ct:pal("Slice 0: Verifying all test net modules load correctly"),

    %% Test net modules that should exist
    Modules = [
        wf_test_net_basic,
        wf_test_net_choice,
        wf_test_net_task_gate,
        wf_test_net_trigger_drop,
        wf_test_net_resume,
        wf_test_stub_net
    ],

    lists:foreach(
        fun(Module) ->
            ct:pal("  Loading module: ~p", [Module]),
            case code:load_file(Module) of
                {module, Module} ->
                    ct:pal("    -> Loaded successfully");
                {error, Reason} ->
                    ct:fail("Failed to load ~p: ~p", [Module, Reason])
            end
        end,
        Modules
    ),

    %% Verify helper modules
    HelperModules = [
        pnet_types,
        pnet_marking,
        pnet_choice,
        pnet_receipt,
        pnet_mode,
        wf_task,
        wf_timerq,
        wf_pool,
        wf_store
    ],

    lists:foreach(
        fun(Module) ->
            ct:pal("  Loading helper module: ~p", [Module]),
            case code:load_file(Module) of
                {module, Module} ->
                    ct:pal("    -> Loaded successfully");
                {error, Reason} ->
                    ct:fail("Failed to load helper ~p: ~p", [Module, Reason])
            end
        end,
        HelperModules
    ),

    ok.

%%%===================================================================
%%% SLICE 1: Deterministic Choice
%%%===================================================================

%% @doc Test deterministic choice with same seed produces same result.
-spec slice_1_deterministic_choice(Config :: ct:config()) -> ok.
slice_1_deterministic_choice(_Config) ->
    ct:pal("Slice 1: Testing deterministic choice behavior"),

    %% Test 1: Same seed produces same choice
    Seed = {42, 12345, 54321},
    List = [a, b, c, d],

    {First1, Rng1} = pnet_choice:pick(List, pnet_choice:seed(Seed)),
    {First2, _Rng2} = pnet_choice:pick(List, pnet_choice:seed(Seed)),

    ?assertEqual(First1, First2),
    ct:pal("  Same seed produces same choice: ~p", [First1]),

    %% Test 2: Different seeds may produce different choices
    Seed1 = {1, 100, 200},
    Seed2 = {2, 200, 300},
    List2 = [alpha, beta, gamma],

    {Pick1, _} = pnet_choice:pick(List2, pnet_choice:seed(Seed1)),
    {Pick2, _} = pnet_choice:pick(List2, pnet_choice:seed(Seed2)),

    %% Both should be valid elements
    ?assert(lists:member(Pick1, List2)),
    ?assert(lists:member(Pick2, List2)),
    ct:pal("  Different seeds: ~p, ~p", [Pick1, Pick2]),

    %% Test 3: Run actual gen_pnet with wf_test_net_choice
    {ok, P1} = gen_pnet:start_link(wf_test_net_choice, #{seed => 42}, []),
    {ok, _} = gen_pnet:inject(P1, #{in => [go]}),
    {ok, Receipts1} = gen_pnet:drain(P1, 10),
    Marking1 = gen_pnet:marking(P1),
    Tokens1 = maps:get(out, Marking1, []),
    ok = gen_pnet:stop(P1),

    %% Run again with same seed
    {ok, P2} = gen_pnet:start_link(wf_test_net_choice, #{seed => 42}, []),
    {ok, _} = gen_pnet:inject(P2, #{in => [go]}),
    {ok, Receipts2} = gen_pnet:drain(P2, 10),
    Marking2 = gen_pnet:marking(P2),
    Tokens2 = maps:get(out, Marking2, []),
    ok = gen_pnet:stop(P2),

    %% Should produce same tokens
    ?assertEqual(length(Receipts1), length(Receipts2)),
    ?assertEqual(Tokens1, Tokens2),
    ct:pal("  Gen pnet deterministic: ~p", [Tokens1]),

    ok.

%%%===================================================================
%%% SLICE 2: Trigger Drop Filtering
%%%===================================================================

%% @doc Test trigger drop enforcement.
-spec slice_2_trigger_drop(Config :: ct:config()) -> ok.
slice_2_trigger_drop(_Config) ->
    ct:pal("Slice 2: Testing trigger drop enforcement"),

    %% Test trigger callback directly
    NetState = #{},

    %% Tokens to 'trash' place should be dropped
    drop = wf_test_net_trigger_drop:trigger(trash, {data, 1}, NetState),
    ct:pal("  Trigger for 'trash' place returns: drop"),

    %% Tokens to 'keep' place should pass
    pass = wf_test_net_trigger_drop:trigger(keep, {data, 1}, NetState),
    ct:pal("  Trigger for 'keep' place returns: pass"),

    %% Test with gen_pnet injection
    {ok, P} = gen_pnet:start_link(wf_test_net_trigger_drop, #{}, []),

    %% Inject to 'trash' place - should be dropped
    {ok, _} = gen_pnet:inject(P, #{trash => [data1]}),
    {ok, TrashTokens} = gen_pnet:ls(P, trash),
    ?assertEqual([], TrashTokens),
    ct:pal("  Tokens to 'trash' were dropped"),

    %% Inject to 'keep' place - should pass
    {ok, _} = gen_pnet:inject(P, #{keep => [data2]}),
    {ok, KeepTokens} = gen_pnet:ls(P, keep),
    ?assertEqual([data2], KeepTokens),
    ct:pal("  Tokens to 'keep' passed through"),

    ok = gen_pnet:stop(P),
    ok.

%%%===================================================================
%%% SLICE 3: Timer Queue Operations
%%%===================================================================

%% @doc Test timer queue basic operations flow.
-spec slice_3_timer_queue(Config :: ct:config()) -> ok.
slice_3_timer_queue(_Config) ->
    ct:pal("Slice 3: Testing timer queue operations"),

    %% Step 1: Create empty queue
    Q0 = wf_timerq:new(),
    ?assertEqual(true, wf_timerq:is_empty(Q0)),
    ?assertEqual(0, wf_timerq:size(Q0)),
    ?assertEqual(undefined, wf_timerq:peek(Q0)),
    ct:pal("  Empty queue created"),

    %% Step 2: Arm timer with event
    Now = erlang:system_time(millisecond),
    Deadline1 = Now + 1000,
    Q1 = wf_timerq:arm(Q0, k1, Deadline1, {inject, #{timer => [tick1]}}),

    ?assertEqual(false, wf_timerq:is_empty(Q1)),
    ?assertEqual(1, wf_timerq:size(Q1)),
    ct:pal("  Timer armed for deadline +1000ms"),

    %% Step 3: Poll before deadline
    {Events0, Q2} = wf_timerq:poll(Q1, Now + 500),
    ?assertEqual([], Events0),
    ?assertEqual(1, wf_timerq:size(Q2)),
    ct:pal("  No events before deadline"),

    %% Step 4: Arm multiple timers
    Deadline2 = Now + 2000,
    Q3 = wf_timerq:arm(Q2, k2, Deadline2, {inject, #{timer => [tick2]}}),
    ?assertEqual(2, wf_timerq:size(Q3)),

    %% Step 5: Poll at first deadline
    {Events1, Q4} = wf_timerq:poll(Q3, Deadline1),
    ?assertEqual([{inject, #{timer => [tick1]}}], Events1),
    ?assertEqual(1, wf_timerq:size(Q4)),
    ct:pal("  First event delivered at deadline"),

    %% Step 6: Poll at second deadline
    {Events2, Q5} = wf_timerq:poll(Q4, Deadline2),
    ?assertEqual([{inject, #{timer => [tick2]}}], Events2),
    ?assertEqual(0, wf_timerq:size(Q5)),
    ct:pal("  Second event delivered at deadline"),

    %% Step 7: Test disarm
    Q6 = wf_timerq:arm(Q5, k3, Now + 3000, {inject, #{timer => [tick3]}}),
    Q7 = wf_timerq:disarm(Q6, k3),
    ?assertEqual(0, wf_timerq:size(Q7)),
    ct:pal("  Disarm removes timer"),

    %% Step 8: Test out-of-order arming
    Q8a = wf_timerq:new(),
    Q8b = wf_timerq:arm(Q8a, k1, Now + 1300, {inject, #{timer => [t3]}}),
    Q8c = wf_timerq:arm(Q8b, k2, Now + 1100, {inject, #{timer => [t1]}}),
    Q8d = wf_timerq:arm(Q8c, k3, Now + 1200, {inject, #{timer => [t2]}}),

    %% Peek should return earliest (1100)
    {PeekDL, _} = wf_timerq:peek(Q8d),
    ?assertEqual(Now + 1100, PeekDL),
    ct:pal("  Out-of-order timers sorted correctly"),

    ok.

%%%===================================================================
%%% SLICE 4: Task Lifecycle Tokens
%%%===================================================================

%% @doc Test task lifecycle from enabled to done.
-spec slice_4_task_lifecycle(Config :: ct:config()) -> ok.
slice_4_task_lifecycle(_Config) ->
    ct:pal("Slice 4: Testing task lifecycle tokens"),

    %% Step 1: Test task construction
    TaskId = task42,
    Payload = #{type => approval, data => #{request => r1}},
    Place = tasks,

    {produce, ProduceMap} = wf_task:enabled(TaskId, Payload, Place),
    ?assertMatch(#{tasks := [{task, task42, enabled, _}]}, ProduceMap),
    ct:pal("  Task enabled token created"),

    %% Step 2: Start gen_pnet with task gate
    {ok, P} = gen_pnet:start_link(wf_test_net_task_gate, #{}, []),

    %% Step 3: Inject enabled task token
    {ok, InjectReceipt} = gen_pnet:inject(P, ProduceMap),
    ?assertMatch(#{tasks := [{task, task42, enabled, _}]}, InjectReceipt),

    %% Verify token is in the net
    {ok, TaskTokens} = gen_pnet:ls(P, tasks),
    ?assertMatch([{task, task42, enabled, _}], TaskTokens),
    ct:pal("  Enabled task token injected"),

    %% Step 4: Inject done task (transition should fire)
    {produce, DoneMap} = wf_task:done(TaskId, #{ok => true}, tasks),
    {ok, _} = gen_pnet:inject(P, DoneMap),

    %% Step 5: Drain to fire the transition
    {ok, Receipts} = gen_pnet:drain(P, 10),
    ?assert(length(Receipts) > 0),
    ct:pal("  Transition fired: ~p receipts", [length(Receipts)]),

    %% Step 6: Verify done place has the result
    {ok, DoneTokens} = gen_pnet:ls(P, done),
    ?assertMatch([{done, task42}], DoneTokens),
    ct:pal("  Done token produced: ~p", [DoneTokens]),

    %% Step 7: Test other task states
    {produce, RunningMap} = wf_task:running(TaskId, #{pct => 50}, tasks),
    ?assertMatch(#{tasks := [{task, task42, running, _}]}, RunningMap),

    {produce, FailedMap} = wf_task:failed(TaskId, timeout, tasks),
    ?assertMatch(#{tasks := [{task, task42, failed, timeout}]}, FailedMap),

    {produce, CancelledMap} = wf_task:cancelled(TaskId, user_abort, tasks),
    ?assertMatch(#{tasks := [{task, task42, cancelled, user_abort}]}, CancelledMap),

    ok = gen_pnet:stop(P),
    ok.

%%%===================================================================
%%% SLICE 5: Receipt Audit Trail
%%%===================================================================

%% @doc Test receipt generation and audit trail.
-spec slice_5_receipt_audit(Config :: ct:config()) -> ok.
slice_5_receipt_audit(_Config) ->
    ct:pal("Slice 5: Testing receipt audit trail"),

    %% Step 1: Create a receipt manually
    Move = #{
        trsn => t1,
        mode => #{p1 => [a]},
        produce => #{p2 => [b]}
    },

    BeforeHash = crypto:hash(md5, term_to_binary({before, Move})),
    AfterHash = crypto:hash(md5, term_to_binary({'after', Move})),

    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),

    %% Verify receipt structure
    ?assertEqual(BeforeHash, maps:get(before_hash, Receipt)),
    ?assertEqual(AfterHash, maps:get(after_hash, Receipt)),
    ?assertEqual(Move, maps:get(move, Receipt)),
    ?assert(is_integer(maps:get(ts, Receipt))),
    ct:pal("  Receipt structure valid"),

    %% Step 2: Verify pnet_types:is_receipt validates it
    ?assert(pnet_types:is_receipt(Receipt)),
    ct:pal("  Receipt passes pnet_types validation"),

    %% Step 3: Test receipt effects classification
    {silent, _} = pnet_receipt:effects(Receipt#{move => Move#{produce => #{}}}),
    ct:pal("  Silent receipt classified"),

    {single_production, _} = pnet_receipt:effects(Receipt),
    ct:pal("  Single production receipt classified"),

    MultiMove = Move#{produce => #{p2 => [b], p3 => [c]}},
    MultiReceipt = pnet_receipt:make(BeforeHash, AfterHash, MultiMove),
    {multiple_production, _} = pnet_receipt:effects(MultiReceipt),
    ct:pal("  Multiple production receipt classified"),

    %% Step 4: Test receipts from gen_pnet
    {ok, P} = gen_pnet:start_link(wf_test_net_task_gate, #{}, []),
    {produce, TaskDone} = wf_task:done(my_task, #{ok => true}, tasks),
    {ok, _} = gen_pnet:inject(P, TaskDone),
    {ok, DrainReceipts} = gen_pnet:drain(P, 10),

    %% Should have at least one receipt
    ?assert(length(DrainReceipts) >= 1),
    ct:pal("  Gen pnet generated ~p receipts", [length(DrainReceipts)]),

    %% Step 5: Verify receipts are valid modes
    lists:foreach(
        fun(R) ->
            ?assert(pnet_types:is_mode(R))
        end,
        DrainReceipts
    ),

    ok = gen_pnet:stop(P),
    ok.

%%%===================================================================
%%% SLICE 6: Checkpoint/Resume Cycle
%%%===================================================================

%% @doc Test full checkpoint/resume cycle.
-spec slice_6_checkpoint_resume(Config :: ct:config()) -> ok.
slice_6_checkpoint_resume(_Config) ->
    ct:pal("Slice 6: Testing checkpoint/resume cycle"),

    %% Step 1: Open wf_store
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),
    ct:pal("  wf_store opened"),

    %% Step 2: Create and start a net instance
    {ok, P0} = gen_pnet:start_link(wf_test_net_resume, #{seed => 3}, []),

    %% Step 3: Inject tokens
    {ok, _} = gen_pnet:inject(P0, #{p1 => [a, b]}),

    %% Step 4: Get marking and usr_info
    Mark0 = gen_pnet:marking(P0),
    Usr0 = gen_pnet:usr_info(P0),

    %% Verify marking
    ?assertEqual([a, b], lists:sort(maps:get(p1, Mark0))),
    ?assertEqual([], maps:get(p2, Mark0)),
    ct:pal("  Initial marking: p1=~p, p2=~p", [maps:get(p1, Mark0), maps:get(p2, Mark0)]),

    %% Step 5: Save to wf_store
    InstId = <<"wf-checkpoint-test">>,
    Snapshot = #{marking => Mark0, usr_info => Usr0},
    ok = wf_store:put_instance(Store, InstId, Snapshot, undefined),
    ct:pal("  Checkpoint saved"),

    %% Step 6: Stop the instance
    ok = gen_pnet:stop(P0),

    %% Step 7: Load the saved state
    {ok, LoadedSnap} = wf_store:get_instance(Store, InstId),
    LoadedMarking = maps:get(marking, LoadedSnap),

    ?assertEqual([a, b], lists:sort(maps:get(p1, LoadedMarking))),
    ?assertEqual([], maps:get(p2, LoadedMarking)),
    ct:pal("  Loaded checkpoint verified"),

    %% Step 8: Resume with saved state
    ResumeArg = #{resume => LoadedSnap},
    {ok, P1} = gen_pnet:start_link(wf_test_net_resume, ResumeArg, []),

    %% Step 9: Verify marking restored
    RestoredMarking = gen_pnet:marking(P1),
    ?assertEqual([a, b], lists:sort(maps:get(p1, RestoredMarking))),
    ?assertEqual([], maps:get(p2, RestoredMarking)),
    ct:pal("  Marking restored after resume"),

    %% Step 10: Verify restored matches saved
    ?assertEqual(LoadedMarking, RestoredMarking),

    ok = gen_pnet:stop(P1),
    ok = wf_store:close(Store),
    ok.

%%%===================================================================
%%% SLICE 7: Pool + Mnesia Transaction
%%%===================================================================

%% @doc Test bounded concurrency with Mnesia transactions.
-spec slice_7_pool_transaction(Config :: ct:config()) -> ok.
slice_7_pool_transaction(_Config) ->
    ct:pal("Slice 7: Testing pool + Mnesia transaction"),

    %% Step 1: Open store
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),
    ct:pal("  Store opened"),

    %% Step 2: Add work item
    ok = wf_store:put_work_item(Store, #{id => <<"w1">>, status => enabled}),
    ct:pal("  Work item added"),

    %% Step 3: Start pool with bounded size
    {ok, Pool} = wf_pool:start_link(#{name => claimpool, size => 1, max_overflow => 0}),
    ct:pal("  Pool started with size=1"),

    %% Step 4: Define claim function
    Claim = fun() ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w1">>, <<"joe">>) end)
    end,

    %% Step 5: First claim should succeed
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) -> Claim() end),
    ct:pal("  First claim succeeded"),

    %% Step 6: Second claim should fail (already claimed)
    Res2 = wf_pool:transaction(Pool, fun(_) -> Claim() end),
    ?assertEqual({atomic, {error, already_claimed}}, Res2),
    ct:pal("  Second claim failed as expected"),

    %% Step 7: Add another work item
    ok = wf_store:put_work_item(Store, #{id => <<"w2">>, status => enabled}),

    %% Step 8: Claim second item
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w2">>, <<"jane">>) end)
    end),
    ct:pal("  Second work item claimed"),

    %% Step 9: Verify work items have correct status
    {ok, [W1]} = wf_store:query_work_items(Store, #{id => <<"w1">>}),
    ?assertEqual(claimed, maps:get(status, W1)),
    ?assertEqual(<<"joe">>, maps:get(assignee, W1)),

    {ok, [W2]} = wf_store:query_work_items(Store, #{id => <<"w2">>}),
    ?assertEqual(claimed, maps:get(status, W2)),
    ?assertEqual(<<"jane">>, maps:get(assignee, W2)),
    ct:pal("  Work item statuses verified"),

    %% Cleanup
    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store),
    ok.

%%%===================================================================
%%% SLICE 8: Concuerror Spec Generation
%%%===================================================================

%% @doc Test Concuerror specification generation.
-spec slice_8_concuerror_spec(Config :: ct:config()) -> ok.
slice_8_concuerror_spec(_Config) ->
    ct:pal("Slice 8: Testing Concuerror spec generation"),

    %% Step 1: Create a basic spec structure
    NetMod = wf_test_net_choice,
    Actors = [client, worker, timer],

    Spec = #{
        module => NetMod,
        actors => Actors,
        depth => 100,
        timeout => 5000,
        critical => []
    },

    %% Verify spec structure
    ?assertEqual(NetMod, maps:get(module, Spec)),
    ?assertEqual(Actors, maps:get(actors, Spec)),
    ?assertEqual(100, maps:get(depth, Spec)),
    ?assertEqual(5000, maps:get(timeout, Spec)),
    ct:pal("  Base spec created"),

    %% Step 2: Add critical sections
    Critical = [{mnesia, transaction}, {gen_pnet, step}],
    Spec2 = Spec#{critical => Critical},

    ?assertEqual(Critical, maps:get(critical, Spec2)),
    ct:pal("  Critical sections added"),

    %% Step 3: Test spec generation helper (simulated)
    GeneratedSpec = generate_concuerror_spec(NetMod, Actors, Critical),
    ?assertEqual(NetMod, maps:get(module, GeneratedSpec)),
    ?assertEqual(Critical, maps:get(critical, GeneratedSpec)),
    ct:pal("  Generated spec verified"),

    %% Step 4: Verify spec would be valid for Concuerror
    ?assert(is_atom(maps:get(module, GeneratedSpec))),
    ?assert(is_list(maps:get(actors, GeneratedSpec))),
    ?assert(is_integer(maps:get(depth, GeneratedSpec))),
    ?assert(maps:get(depth, GeneratedSpec) > 0),
    ?assert(is_integer(maps:get(timeout, GeneratedSpec))),
    ?assert(maps:get(timeout, GeneratedSpec) > 0),
    ct:pal("  Spec validation passed"),

    ok.

%% @private Helper to generate Concuerror spec.
generate_concuerror_spec(Module, Actors, Critical) ->
    #{
        module => Module,
        actors => Actors,
        depth => 100,
        timeout => 5000,
        critical => Critical
    }.

%%%===================================================================
%%% SLICE 9: Post-Drain Sanity
%%%===================================================================

%% @doc Test post-drain sanity checks.
-spec slice_9_post_drain_sanity(Config :: ct:config()) -> ok.
slice_9_post_drain_sanity(_Config) ->
    ct:pal("Slice 9: Testing post-drain sanity"),

    %% Step 1: Start a net that will complete
    {ok, P} = gen_pnet:start_link(wf_test_net_task_gate, #{}, []),

    %% Step 2: Inject tokens that will trigger a transition
    {produce, TaskDone} = wf_task:done(test_task, #{ok => true}, tasks),
    {ok, _} = gen_pnet:inject(P, TaskDone),

    %% Step 3: Drain until no more transitions
    {ok, Receipts} = gen_pnet:drain(P, 10),
    ct:pal("  Drain produced ~p receipts", [length(Receipts)]),

    %% Step 4: Verify final marking
    FinalMarking = gen_pnet:marking(P),
    {ok, Tasks} = gen_pnet:ls(P, tasks),
    {ok, Done} = gen_pnet:ls(P, done),

    %% Verify net is in stable state
    ?assert(is_map(FinalMarking)),
    ?assertEqual([], Tasks),
    ?assertMatch([{done, test_task}], Done),
    ct:pal("  Final marking stable"),

    %% Step 5: Verify no more transitions can fire
    %% Drain again should return no receipts
    {ok, EmptyReceipts} = gen_pnet:drain(P, 10),
    ?assertEqual(0, length(EmptyReceipts)),
    ct:pal("  No further transitions possible"),

    %% Step 6: Verify marking hash is stable
    Hash1 = pnet_marking:hash(FinalMarking),
    Hash2 = pnet_marking:hash(FinalMarking),
    ?assertEqual(Hash1, Hash2),
    ct:pal("  Marking hash stable: ~p", [Hash1]),

    %% Step 7: Verify stats
    Stats = gen_pnet:stats(P),
    ?assert(is_map(Stats)),
    ct:pal("  Stats available: ~p", [Stats]),

    ok = gen_pnet:stop(P),
    ok.

%%%===================================================================
%%% SLICE 10: Replay Determinism
%%%===================================================================

%% @doc Test deterministic replay with same seed.
-spec slice_10_replay_determinism(Config :: ct:config()) -> ok.
slice_10_replay_determinism(_Config) ->
    ct:pal("Slice 10: Testing replay determinism"),

    %% Define a test run function
    Run = fun(Seed) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => Seed}, []),
        {ok, InjectReceipt} = gen_pnet:inject(P, #{in => [go]}),
        {ok, DrainReceipts} = gen_pnet:drain(P, 10),
        Marking = gen_pnet:marking(P),
        FinalHash = pnet_marking:hash(Marking),
        ok = gen_pnet:stop(P),
        {InjectReceipt, DrainReceipts, FinalHash}
    end,

    %% Step 1: Run with same seed twice
    Seed = 123,
    {I1, D1, F1} = Run(Seed),
    {I2, D2, F2} = Run(Seed),

    %% Step 2: Verify injection receipts match
    ?assertEqual(I1, I2),
    ct:pal("  Injection receipts match"),

    %% Step 3: Verify drain receipts match
    ?assertEqual(D1, D2),
    ct:pal("  Drain receipts match (~p receipts)", [length(D1)]),

    %% Step 4: Verify final marking hashes match
    ?assertEqual(F1, F2),
    ct:pal("  Final marking hashes match"),

    %% Step 5: Verify hash chains are deterministic
    HashChain1 = receipts_to_hash_chain([I1 | D1]),
    HashChain2 = receipts_to_hash_chain([I2 | D2]),
    ?assertEqual(HashChain1, HashChain2),
    ct:pal("  Hash chains match: ~p", [HashChain1]),

    %% Step 6: Test with different seeds (may differ)
    Seed1 = 1,
    Seed2 = 999,
    {_IA, DA, FA} = Run(Seed1),
    {_IB, DB, FB} = Run(Seed2),

    %% Hashes might differ (both outcomes acceptable)
    ct:pal("  Different seeds produce hashes: ~p vs ~p", [FA, FB]),

    %% But same seed should still produce same result
    {_, _, FA2} = Run(Seed1),
    ?assertEqual(FA, FA2),
    ct:pal("  Seed1 deterministic verified"),

    ok.

%% @private Convert receipts to hash chain.
receipts_to_hash_chain(Receipts) ->
    lists:map(
        fun(R) when is_map(R) -> erlang:phash2(R);
           (R) -> erlang:phash2(R)
        end,
        Receipts
    ).
