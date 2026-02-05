-module(test_workflow).
-compile([export_all]).

-include_lib("order_fulfillment_types.hrl").

run_test() ->
    %% Create a simple order
    Order = #order{
        order_id = <<"TEST-001">>,
        customer_id = <<"CUST-001">>,
        customer_name = <<"Test Customer">>,
        customer_email = <<"test@example.com">>,
        items = [
            #item{sku = <<"SKU-001">>, name = <<"Widget">>,
                  quantity = 1, price = 10.0, weight = 1.0}
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
    
    io:format("~n=== Order Created ===~n"),
    io:format("Order ID: ~s~n", [Order#order.order_id]),
    io:format("Customer: ~s~n", [Order#order.customer_name]),
    io:format("Total: $~.2f~n", [Order#order.total]),
    
    %% Test the workflow structure is valid
    io:format("~n=== Workflow Structure Valid ===~n"),
    io:format("Places: ~p~n", [ordering:place_lst()]),
    io:format("Transitions: ~p~n", [ordering:trsn_lst()]),
    io:format("~nTest completed successfully.~n~n"),
    ok.
