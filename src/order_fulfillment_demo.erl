%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Order Fulfillment Demo Script
%%
%% This module demonstrates the YAWL Order Fulfillment workflow using all
%% 5 subprocesses implemented with gen_pnet.
%%
%% @end
%%--------------------------------------------------------------------

-module(order_fulfillment_demo).
-export([run_demo/0, run_demo/1, run_simple_test/0]).

%% Include shared types
-include_lib("order_fulfillment_types.hrl").

%%====================================================================
%% Demo Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs the full Order Fulfillment demo.
%% @end
%%--------------------------------------------------------------------
-spec run_demo() -> ok.
run_demo() ->
    %% Start XES logger
    application:ensure_all_started(cre),
    {ok, _XesPid} = yawl_xes:start_link(),

    %% Create sample order
    Order = #order{
        order_id = <<"DEMO-ORDER-001">>,
        customer_id = <<"CUST-DEMO-001">>,
        customer_name = <<"John Doe">>,
        customer_email = <<"john.doe@example.com">>,
        items = [
            #item{
                sku = <<"SKU-001">>,
                name = <<"Widget A">>,
                quantity = 2,
                price = 29.99,
                weight = 1.5,
                category = <<"Electronics">>
            },
            #item{
                sku = <<"SKU-002">>,
                name = <<"Gadget B">>,
                quantity = 1,
                price = 49.99,
                weight = 0.8,
                category = <<"Electronics">>
            }
        ],
        shipping_address = #{
            <<"street">> => <<"123 Main St">>,
            <<"city">> => <<"New York">>,
            <<"state">> => <<"NY">>,
            <<"zip">> => <<"10001">>,
            <<"country">> => <<"USA">>
        },
        billing_address = #{
            <<"street">> => <<"123 Main St">>,
            <<"city">> => <<"New York">>,
            <<"state">> => <<"NY">>,
            <<"zip">> => <<"10001">>,
            <<"country">> => <<"USA">>
        },
        subtotal = 109.97,
        tax = 8.80,
        shipping_cost = 12.99,
        total = 131.76,
        status = pending,
        created_at = erlang:system_time(millisecond)
    },

    io:format("~n=== YAWL Order Fulfillment Demo ===~n"),
    io:format("Order ID: ~s~n", [Order#order.order_id]),
    io:format("Customer: ~s (~s)~n", [Order#order.customer_name, Order#order.customer_email]),
    io:format("Items: ~p~n", [Order#order.items]),
    io:format("Total: $~.2f~n~n", [Order#order.total]),

    %% Run the order fulfillment workflow
    OrderInput = #{
        <<"order_id">> => Order#order.order_id,
        <<"customer_id">> => Order#order.customer_id,
        <<"customer_name">> => Order#order.customer_name,
        <<"customer_email">> => Order#order.customer_email,
        <<"items">> => Order#order.items,
        <<"shipping_address">> => Order#order.shipping_address,
        <<"billing_address">> => Order#order.billing_address,
        <<"subtotal">> => Order#order.subtotal,
        <<"tax">> => Order#order.tax,
        <<"shipping_cost">> => Order#order.shipping_cost,
        <<"total">> => Order#order.total,
        <<"payment_details">> => #{
            <<"method">> => credit_card
        }
    },

    Result = order_fulfillment:run(OrderInput),

    case Result of
        {ok, _State} ->
            io:format("~n=== Order Fulfillment Complete ===~n"),
            io:format("Order ID: ~s~n", [Order#order.order_id]),
            io:format("Status: All subprocesses completed~n~n");
        {error, Reason} ->
            io:format("~n=== Order Fulfillment Failed ===~n"),
            io:format("Reason: ~p~n~n", [Reason])
    end,

    %% Export XES log
    case yawl_xes:list_logs() of
        [] ->
            io:format("No XES logs to export.~n");
        Logs ->
            [{LogId, _Log} | _] = Logs,
            io:format("Exporting XES log: ~s~n", [LogId]),
            case yawl_xes:export_xes(LogId, "xes_logs") of
                {ok, _Content} ->
                    io:format("XES log exported to xes_logs/~s.xes~n~n", [LogId]);
                {error, ExportReason} ->
                    io:format("Failed to export XES log: ~p~n~n", [ExportReason])
            end
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc Runs the Order Fulfillment demo with custom order.
%% @end
%%--------------------------------------------------------------------
-spec run_demo(OrderInput :: map()) -> ok.
run_demo(OrderInput) when is_map(OrderInput) ->
    application:ensure_all_started(cre),
    {ok, _XesPid} = yawl_xes:start_link(),
    Result = order_fulfillment:run(OrderInput),

    case Result of
        {ok, _State} ->
            io:format("~n=== Order Fulfillment Complete ===~n"),
            OrderId = maps_get_default(OrderInput, <<"order_id">>, <<"UNKNOWN">>),
            io:format("Order ID: ~s~n~n", [OrderId]);
        {error, Reason} ->
            io:format("~n=== Order Fulfillment Failed ===~n"),
            io:format("Reason: ~p~n~n", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Runs a simple test of the individual subprocesses.
%% @end
%%--------------------------------------------------------------------
-spec run_simple_test() -> ok.
run_simple_test() ->
    io:format("~n=== YAWL Order Fulfillment Subprocess Tests ===~n~n"),

    application:ensure_all_started(cre),
    {ok, _XesPid} = yawl_xes:start_link(),

    %% Test 1: Ordering
    io:format("Test 1: Ordering Subprocess~n"),
    Order = #order{
        order_id = <<"TEST-ORDER-001">>,
        customer_id = <<"CUST-001">>,
        customer_name = <<"Jane Smith">>,
        customer_email = <<"jane@example.com">>,
        items = [
            #item{sku = <<"SKU-001">>, name = <<"Widget">>, quantity = 1, price = 10.0, weight = 1.0}
        ],
        shipping_address = #{<<"city">> => <<"Boston">>},
        billing_address = #{},
        subtotal = 10.0,
        tax = 0.8,
        shipping_cost = 5.99,
        total = 16.79,
        status = pending,
        created_at = erlang:system_time(millisecond)
    },
    {ok, OrderingPid} = ordering:start(Order),
    timer:sleep(1000),
    io:format("  Ordering: Complete~n~n"),
    gen_pnet:stop(OrderingPid),

    %% Test 2: Payment
    io:format("Test 2: Payment Subprocess~n"),
    PaymentDetails = #{<<"method">> => credit_card},
    {ok, PaymentPid} = payment:start(Order, PaymentDetails),
    timer:sleep(1000),
    io:format("  Payment: Complete~n~n"),
    gen_pnet:stop(PaymentPid),

    %% Test 3: Transit
    io:format("Test 3: Freight In Transit Subprocess~n"),
    Shipment = #shipment{
        shipment_id = <<"TEST-SHIP-001">>,
        order_id = Order#order.order_id,
        carrier = <<"TEST-CARRIER">>,
        tracking_number = <<"TRACK-001">>,
        shipping_method = ltl,
        origin = <<"Warehouse">>,
        destination = <<"Boston">>,
        current_location = undefined,
        status = picked_up,
        estimated_delivery = undefined,
        actual_delivery = undefined,
        weight = 1.0,
        dimensions = undefined
    },
    {ok, TransitPid} = freight_in_transit:start(Shipment, Order),
    timer:sleep(1000),
    io:format("  Transit: Complete~n~n"),
    gen_pnet:stop(TransitPid),

    %% Test 4: Delivery
    io:format("Test 4: Freight Delivered Subprocess~n"),
    {ok, DeliveryPid} = freight_delivered:start(Shipment, Order),
    timer:sleep(1000),
    io:format("  Delivery: Complete~n~n"),
    gen_pnet:stop(DeliveryPid),

    io:format("=== All Subprocess Tests Complete ===~n~n"),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
maps_get_default(Map, Key, Default) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Value -> Value
    end.
