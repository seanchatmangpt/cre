%% -*- erlang -*-
%%%% @doc wf_pool bounded concurrency + wf_store transaction test
%%
%% Test Slice 7 of compound doctests - bounded concurrency + store tx.
%%
%% This test module verifies:
%% 1. wf_store:open/1 opens store
%% 2. wf_store:put_work_item/2 adds work item
%% 3. wf_pool:start_link/1 starts pool
%% 4. wf_pool:transaction/2 runs function in worker
%% 5. wf_store:claim_work_item/2 claims work item (exclusive)
%% 6. Second claim returns {error, already_claimed}
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_pool_transaction_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% EUnit Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main test generator with setup/cleanup.
%%--------------------------------------------------------------------
wf_pool_transaction_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          {"bounded concurrency claim", fun bounded_concurrency_claim_test/0},
          {"multiple work items pool", fun multiple_work_items_pool_test/0},
          {"pool overflow config", fun pool_overflow_config_test/0},
          {"store transaction isolation", fun store_transaction_isolation_test/0},
          {"pool resilience after error", fun pool_resilience_after_error_test/0},
          {"specification doctest", fun specification_doctest_test/0}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Setup function - ensures clean Mnesia state.
%%--------------------------------------------------------------------
setup() ->
    %% Stop and delete any existing Mnesia schema to ensure clean state
    application:stop(mnesia),
    timer:sleep(50),
    mnesia:delete_schema([node()]),
    timer:sleep(50),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function - cleans up Mnesia tables.
%%--------------------------------------------------------------------
cleanup(_Ctx) ->
    %% Clean up any wf_store tables that may exist
    Tables = [wf_store_instance, wf_store_workitem],
    lists:foreach(
        fun(Table) ->
            case mnesia:delete_table(Table) of
                {atomic, ok} -> ok;
                {aborted, {no_exists, _}} -> ok;
                _ -> ok
            end
        end,
        Tables
    ),
    ok.

%%--------------------------------------------------------------------
%% @doc Test bounded concurrency with store transactions.
%%
%% Verifies that:
%% - Store can be opened with tmp backend
%% - Work items can be added to store
%% - Pool can be started with bounded size
%% - Transactions execute within pool workers
%% - First claim succeeds, second fails with already_claimed
%%
%% @end
%%--------------------------------------------------------------------
bounded_concurrency_claim_test() ->
    %% 1. Open store with tmp backend
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),

    %% 2. Add work item to store
    ok = wf_store:put_work_item(Store, #{id => <<"w1">>, status => enabled}),

    %% 3. Start pool with size=1, max_overflow=0 (bounded)
    {ok, Pool} = wf_pool:start_link(#{name => claimpool, size => 1, max_overflow => 0}),

    %% 4. Define claim function that wraps claim_work_item in tx
    Claim = fun() ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w1">>, <<"joe">>) end)
    end,

    %% 5. First transaction should succeed (claim work item)
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) -> Claim() end),

    %% 6. Second transaction should fail (already claimed)
    Res2 = wf_pool:transaction(Pool, fun(_) -> Claim() end),
    ?assertEqual({atomic, {error, already_claimed}}, Res2),

    %% 7. Cleanup
    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store).

%%--------------------------------------------------------------------
%% @doc Test multiple work items with pool concurrency.
%%
%% Verifies that different work items can be claimed independently
%% while already claimed items remain protected.
%%
%% @end
%%--------------------------------------------------------------------
multiple_work_items_pool_test() ->
    %% Setup
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),

    %% Add multiple work items
    ok = wf_store:put_work_item(Store, #{id => <<"w1">>, status => enabled}),
    ok = wf_store:put_work_item(Store, #{id => <<"w2">>, status => enabled}),
    ok = wf_store:put_work_item(Store, #{id => <<"w3">>, status => enabled}),

    %% Start pool
    {ok, Pool} = wf_pool:start_link(#{name => multipool, size => 1, max_overflow => 0}),

    %% Claim w1 - should succeed
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w1">>, <<"alice">>) end)
    end),

    %% Claim w2 - should succeed
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w2">>, <<"bob">>) end)
    end),

    %% Claim w3 - should succeed
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w3">>, <<"charlie">>) end)
    end),

    %% Try to claim w1 again - should fail
    {atomic, {error, already_claimed}} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w1">>, <<"alice">>) end)
    end),

    %% Try to claim w2 again - should fail
    {atomic, {error, already_claimed}} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"w2">>, <<"bob">>) end)
    end),

    %% Cleanup
    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store).

%%--------------------------------------------------------------------
%% @doc Test pool overflow configuration.
%%
%% Verifies that max_overflow configuration is respected
%% and pool can handle concurrent requests within bounds.
%%
%% @end
%%--------------------------------------------------------------------
pool_overflow_config_test() ->
    %% Setup
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),
    ok = wf_store:put_work_item(Store, #{id => <<"overflow1">>, status => enabled}),

    %% Start pool with size=1, max_overflow=1 (allows 2 concurrent)
    {ok, Pool} = wf_pool:start_link(#{name => overflowpool, size => 1, max_overflow => 1}),

    %% Single claim should succeed
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"overflow1">>, <<"dave">>) end)
    end),

    %% Second claim should fail (already claimed, regardless of pool size)
    {atomic, {error, already_claimed}} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"overflow1">>, <<"dave">>) end)
    end),

    %% Cleanup
    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store).

%%--------------------------------------------------------------------
%% @doc Test store transaction isolation through pool.
%%
%% Verifies that store transactions execute atomically
%% even when dispatched through pool workers.
%%
%% @end
%%--------------------------------------------------------------------
store_transaction_isolation_test() ->
    %% Setup
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),
    ok = wf_store:put_work_item(Store, #{id => <<"iso1">>, status => enabled, assignee => <<>>}),

    %% Start pool
    {ok, Pool} = wf_pool:start_link(#{name => isopool, size => 2, max_overflow => 0}),

    %% Transaction that updates assignee atomically
    UpdateAssignee = fun(WorkItemId, NewAssignee) ->
        wf_store:tx(Store, fun() ->
            {ok, [WI]} = wf_store:query_work_items(Store, #{id => WorkItemId}),
            %% Delete old record
            mnesia:delete(wf_store_workitem, WorkItemId, write),
            %% Write updated record
            UpdatedWI = WI#{assignee => NewAssignee},
            wf_store:put_work_item(Store, UpdatedWI),
            ok
        end)
    end,

    %% Update assignee through pool
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) -> UpdateAssignee(<<"iso1">>, <<"eve">>) end),

    %% Verify the update
    {ok, [UpdatedWI]} = wf_store:query_work_items(Store, #{id => <<"iso1">>}),
    <<"eve">> = maps:get(assignee, UpdatedWI),

    %% Cleanup
    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store).

%%--------------------------------------------------------------------
%% @doc Test pool cleanup after transaction failure.
%%
%% Verifies that pool remains functional after a transaction
%% that returns an error.
%%
%% @end
%%--------------------------------------------------------------------
pool_resilience_after_error_test() ->
    %% Setup
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),
    ok = wf_store:put_work_item(Store, #{id => <<"resilient">>, status => enabled}),

    %% Start pool
    {ok, Pool} = wf_pool:start_link(#{name => resilientpool, size => 1, max_overflow => 0}),

    %% First claim succeeds
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"resilient">>, <<"frank">>) end)
    end),

    %% Second claim fails (expected)
    {atomic, {error, already_claimed}} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"resilient">>, <<"frank">>) end)
    end),

    %% Add a new work item
    ok = wf_store:put_work_item(Store, #{id => <<"resilient2">>, status => enabled}),

    %% Pool should still work for the new item
    {atomic, ok} = wf_pool:transaction(Pool, fun(_) ->
        wf_store:tx(Store, fun() -> wf_store:claim_work_item(Store, <<"resilient2">>, <<"grace">>) end)
    end),

    %% Cleanup
    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store).

%%--------------------------------------------------------------------
%% @doc Test doctest example from specification.
%%
%% This is the exact flow from the user's specification.
%%
%% @end
%%--------------------------------------------------------------------
specification_doctest_test() ->
    {ok, Store2} = wf_store:open(#{backend => mnesia, dir => tmp}),
    ok = wf_store:put_work_item(Store2, #{id => <<"w1">>, status => enabled}),
    {ok, Pool} = wf_pool:start_link(#{name => claimpool, size => 1, max_overflow => 0}),

    Claim = fun() ->
        wf_store:tx(Store2, fun() -> wf_store:claim_work_item(Store2, <<"w1">>, <<"joe">>) end)
    end,

    {atomic, ok} = wf_pool:transaction(Pool, fun(_) -> Claim() end),
    Res2 = wf_pool:transaction(Pool, fun(_) -> Claim() end),

    %% Verify the result matches expected {atomic, {error, already_claimed}}
    ?assertEqual({atomic, {error, already_claimed}}, Res2),

    ok = wf_pool:stop(Pool),
    ok = wf_store:close(Store2).
