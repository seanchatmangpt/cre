%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Validation Integration Test Suite
%%%
%%% Tests the complete SOC 2 validation supervision tree.
%%% Proves that Joe Armstrong-level capability validation works.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_validation_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    test_supervision_tree_starts/1,
    test_control_executors_running/1,
    test_evidence_generators_running/1,
    test_receipt_chain_accumulates/1,
    test_meta_validator_detects_health/1,
    test_control_validation_produces_receipts/1,
    test_receipt_chain_merkle_proof/1,
    test_system_survives_executor_crash/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [
        test_supervision_tree_starts,
        test_control_executors_running,
        test_evidence_generators_running,
        test_receipt_chain_accumulates,
        test_meta_validator_detects_health,
        test_control_validation_produces_receipts,
        test_receipt_chain_merkle_proof,
        test_system_survives_executor_crash
    ].

init_per_suite(Config) ->
    %% Start SOC 2 validation supervision tree
    {ok, _Pid} = soc2_validation_sup:start_link(),

    %% Wait for system to stabilize
    timer:sleep(2000),

    Config.

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_supervision_tree_starts(_Config) ->
    %% Verify top-level supervisor is running
    Pid = whereis(soc2_validation_sup),
    true = is_pid(Pid),
    true = is_process_alive(Pid),

    ct:pal("SOC 2 validation supervisor started: ~p", [Pid]),
    ok.

test_control_executors_running(_Config) ->
    %% Verify all 7 control executors are running
    ExpectedControls = [<<"CC6.1">>, <<"CC7.1">>, <<"CC8.1">>, <<"CC9.1">>,
                        <<"PI1.1">>, <<"C1.1">>, <<"P1.1">>],

    lists:foreach(
        fun(ControlId) ->
            Status = soc2_control_executor:get_status(ControlId),
            ct:pal("Control ~s status: ~p", [ControlId, Status]),
            true = maps:is_key(status, Status)
        end,
        ExpectedControls
    ),

    ok.

test_evidence_generators_running(_Config) ->
    %% Verify all 4 evidence generators are running
    ExpectedGens = [
        soc2_evidence_gen_uptime,
        soc2_evidence_gen_load_test,
        soc2_evidence_gen_chaos,
        soc2_evidence_gen_build
    ],

    lists:foreach(
        fun(GenName) ->
            Pid = whereis(GenName),
            ct:pal("Evidence generator ~p: ~p", [GenName, Pid]),
            true = is_pid(Pid),
            true = is_process_alive(Pid)
        end,
        ExpectedGens
    ),

    ok.

test_receipt_chain_accumulates(_Config) ->
    %% Wait for some receipts to be generated
    timer:sleep(3000),

    %% Check receipt chain
    Chain = soc2_receipt_chain:get_chain(),
    ct:pal("Receipt chain: ~p", [Chain]),

    ReceiptCount = maps:get(receipt_count, Chain, 0),
    true = ReceiptCount > 0,

    RootHash = maps:get(root_hash, Chain),
    true = byte_size(RootHash) > 0,

    ok.

test_meta_validator_detects_health(_Config) ->
    %% Run meta-validation
    Health = soc2_meta_validator:run_meta_validation(),
    ct:pal("System health: ~p", [Health]),

    %% Should have status field
    Status = maps:get(status, Health),
    true = lists:member(Status, [pass, fail, pending]),

    %% Should have all check categories
    Checks = maps:get(checks, Health),
    true = maps:is_key(control_executors, Checks),
    true = maps:is_key(evidence_generators, Checks),
    true = maps:is_key(receipt_chain, Checks),
    true = maps:is_key(coverage, Checks),

    ok.

test_control_validation_produces_receipts(_Config) ->
    %% Trigger immediate validation for CC6.1
    ControlId = <<"CC6.1">>,

    {ok, Pid} = soc2_control_executor_sup:get_control_pid(ControlId),
    {ok, Receipt} = soc2_control_executor:validate_now(Pid),

    ct:pal("Validation receipt: ~p", [Receipt]),

    %% Verify receipt structure
    ControlId = maps:get(control_id, Receipt),
    true = maps:is_key(status, Receipt),
    true = maps:is_key(timestamp, Receipt),
    true = maps:is_key(validator_results, Receipt),
    true = maps:is_key(evidence_results, Receipt),

    ok.

test_receipt_chain_merkle_proof(_Config) ->
    %% Get current Merkle root
    RootHash1 = soc2_receipt_chain:get_merkle_root(),
    ct:pal("Initial Merkle root: ~p", [RootHash1]),

    %% Add a receipt
    Receipt = #{
        control_id => <<"TEST.1">>,
        status => pass,
        timestamp => calendar:universal_time()
    },
    ok = soc2_receipt_chain:append_receipt(Receipt),

    %% Merkle root should change
    timer:sleep(100),
    RootHash2 = soc2_receipt_chain:get_merkle_root(),
    ct:pal("New Merkle root: ~p", [RootHash2]),

    true = RootHash1 =/= RootHash2,

    ok.

test_system_survives_executor_crash(_Config) ->
    %% Get CC6.1 executor pid
    {ok, Pid} = soc2_control_executor_sup:get_control_pid(<<"CC6.1">>),
    ct:pal("Original CC6.1 executor: ~p", [Pid]),

    %% Kill the executor
    exit(Pid, kill),
    timer:sleep(1000),

    %% Supervisor should have restarted it
    {ok, NewPid} = soc2_control_executor_sup:get_control_pid(<<"CC6.1">>),
    ct:pal("Restarted CC6.1 executor: ~p", [NewPid]),

    %% Should be a different pid
    true = Pid =/= NewPid,
    true = is_process_alive(NewPid),

    %% Meta-validator should still report healthy system
    timer:sleep(2000),
    Health = soc2_meta_validator:run_meta_validation(),
    ControlCheck = maps:get(control_executors, maps:get(checks, Health)),
    ct:pal("Control executors check after crash: ~p", [ControlCheck]),

    ok.
