%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Petri Net Demo Script
%%
%% This script runs a complete demonstration of the YAWL Petri Net
%% integration example. It shows order processing with approval workflow.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_pnet_demo).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main entry point for the demo.
%%
%% @end
%%--------------------------------------------------------------------
-export([main/1, run/0]).

main(_Args) ->
    run(),
    init:stop().

%%--------------------------------------------------------------------
%% @doc Runs the complete demo.
%%
%% @end
%%--------------------------------------------------------------------
run() ->
    io:format("~n~n"),
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  YAWL Petri Net Integration Example: Order Processing    ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),

    %% Start the workflow
    io:format("→ Starting workflow process...~n"),
    {ok, Pid} = yawl_pnet_example:start_link(),
    io:format("  Workflow PID: ~p~n", [Pid]),
    io:format("~n"),

    %% Give process time to initialize
    timer:sleep(100),

    %% Demo 1: Low-value order (direct processing)
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("Scenario 1: Low-Value Order (Direct Processing)~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("~n"),

    Order1Items = [
        #{<<"sku">> => <<"PROD_BASIC">>, <<"quantity">> => 2},
        #{<<"sku">> => <<"PROD_STANDARD">>, <<"quantity">> => 1}
    ],
    yawl_pnet_example:submit_order(Pid, <<"customer_001">>, Order1Items),
    io:format("  Expected: Direct processing without approval~n"),
    io:format("~n"),

    %% Wait for processing
    wait_for_completion(Pid, 2000),

    %% Demo 2: High-value order (requires approval)
    io:format("~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("Scenario 2: High-Value Order (Requires Approval)~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("~n"),

    Order2Items = [
        #{<<"sku">> => <<"PROD_ENTERPRISE">>, <<"quantity">> => 1}
    ],
    yawl_pnet_example:submit_order(Pid, <<"customer_002">>, Order2Items),
    io:format("  Expected: Routed to approval (total >= $1000)~n"),
    io:format("~n"),

    %% Wait for routing
    timer:sleep(500),

    %% Inject approval decision
    io:format("→ Injecting approval decision...~n"),

    %% Get current marking to find pending orders
    {ok, Marking} = yawl_pnet_example:get_marking(Pid),
    PendingOrders = maps:get('p_pending', Marking, []),

    case PendingOrders of
        [Order | _] ->
            OrderId = Order#yawl_pnet_example.order_id,
            io:format("  Approving order ~s...~n", [OrderId]),
            yawl_pnet_example:inject_approval(Pid, OrderId, approved);
        [] ->
            io:format("  No pending orders found~n")
    end,

    timer:sleep(500),

    %% Demo 3: Another low-value order
    io:format("~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("Scenario 3: Multiple Concurrent Orders~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("~n"),

    Order3Items = [
        #{<<"sku">> => <<"PROD_PREMIUM">>, <<"quantity">> => 1}
    ],
    yawl_pnet_example:submit_order(Pid, <<"customer_003">>, Order3Items),
    io:format("  Expected: Direct processing (~$500)~n"),
    io:format("~n"),

    %% Wait for final processing
    wait_for_completion(Pid, 2000),

    %% Show final state
    io:format("~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("Final State Check~n"),
    io:format("────────────────────────────────────────────────────────────~n"),
    io:format("~n"),

    {ok, FinalMarking} = yawl_pnet_example:get_marking(Pid),
    io:format("Place Token Counts:~n"),
    maps:fold(fun
        (Place, Tokens, _) when length(Tokens) > 0 ->
            io:format("  ~p: ~p token(s)~n", [Place, length(Tokens)]);
        (_, _, _) ->
            ok
    end, ok, FinalMarking),

    %% Show receipts
    {ok, Receipts} = yawl_pnet_example:get_receipts(Pid),
    io:format("~n"),
    io:format("Total receipts (state transitions): ~p~n", [length(Receipts)]),

    %% Stop the workflow
    io:format("~n"),
    io:format("→ Stopping workflow process...~n"),
    yawl_pnet_example:stop(Pid),

    io:format("~n"),
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║                   Demo Complete!                           ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),
    io:format("Key Concepts Demonstrated:~n"),
    io:format("  • pnet_types: Type-safe place, token, and marking definitions~n"),
    io:format("  • pnet_marking: Immutable marking algebra for state management~n"),
    io:format("  • wf_task: Token constructors for external work integration~n"),
    io:format("  • pnet_receipt: Audit trail for state transitions~n"),
    io:format("  • gen_pnet: Complete behaviour implementation~n"),
    io:format("~n").

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Waits for workflow completion by checking for tokens in p_complete.
%%
%% @end
%%--------------------------------------------------------------------
wait_for_completion(Pid, Timeout) ->
    wait_for_completion(Pid, Timeout, 0).

wait_for_completion(_Pid, Timeout, Elapsed) when Elapsed >= Timeout ->
    io:format("  (timeout waiting for completion)~n");
wait_for_completion(Pid, Timeout, Elapsed) ->
    {ok, Marking} = yawl_pnet_example:get_marking(Pid),
    CompleteTokens = maps:get('p_complete', Marking, []),
    RejectedTokens = maps:get('p_rejected', Marking, []),
    ActiveTokens = maps:get('p_validating', Marking, []) ++
                   maps:get('p_validated', Marking, []) ++
                   maps:get('p_pending', Marking, []) ++
                   maps:get('p_processing', Marking, []) ++
                   maps:get('p_approved', Marking, []) ++
                   maps:get('p_paying', Marking, []) ++
                   maps:get('p_shipping', Marking, []),

    case {length(CompleteTokens) + length(RejectedTokens), length(ActiveTokens)} of
        {_, 0} ->
            io:format("  → Order processing complete~n");
        _ ->
            timer:sleep(100),
            wait_for_completion(Pid, Timeout, Elapsed + 100)
    end.
