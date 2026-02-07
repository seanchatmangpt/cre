%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Order Fulfillment Orchestrator - YAWL Order Fulfillment Pattern
%%
%% This module implements the top-level Order Fulfillment orchestrator from
%% the CAISE 2013 paper. The Order Fulfillment workflow is modeled as a
%% Petri net using gen_pnet:
%% - Places (circles in YAWL) = Conditions/states
%% - Transitions (boxes in YAWL) = Tasks (subprocess invocations)
%% - Tokens = Flow of control
%% - Arcs = Flow relations
%%
%% Petri Net Structure:
%% PLACES:                          TRANSITIONS:
%% p_input                          t_start_ordering
%% p_ordering_complete              t_start_carrier
%% p_carrier_complete               t_start_payment
%% p_payment_complete               t_start_transit
%% p_transit_complete               t_start_delivery
%% p_delivery_complete              t_complete
%% p_output
%%
%% The orchestrator coordinates 5 subprocesses:
%% 1. Ordering - Customer order entry and validation
%% 2. Carrier Appointment - Shipping arrangement (already implemented)
%% 3. Payment - Payment processing
%% 4. Freight In Transit - Shipment tracking
%% 5. Freight Delivered - Delivery confirmation
%%
%% Key Patterns:
%% - WCP-01 (Sequence) - All subprocesses run sequentially
%%
%% @reference Supporting Risk-Informed Decisions during Business Process Execution (CAISE 2013)
%% @end
%%--------------------------------------------------------------------

-module(order_fulfillment).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    trigger/3,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
]).

%% API
-export([new/1, start/1, run/1, get_status/1, get_state/1]).
-export([start_ordering/1, start_carrier/1, start_payment/1,
         start_transit/1, start_delivery/1]).

%% Include shared types
-include_lib("order_fulfillment_types.hrl").
-include_lib("gen_pnet/include/gen_pnet.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(fulfillment_state, {
    order :: #order{} | undefined,
    payment :: #payment{} | undefined,
    shipment :: #shipment{} | undefined,
    delivery :: #delivery{} | undefined,
    payment_details :: map() | undefined,
    log_id :: binary() | undefined,
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined,
    %% Subprocess PIDs for monitoring
    ordering_pid :: pid() | undefined,
    carrier_pid :: pid() | undefined,
    payment_pid :: pid() | undefined,
    transit_pid :: pid() | undefined,
    delivery_pid :: pid() | undefined,
    %% Subprocess results
    ordering_result = pending :: pending | ok | {error, term()},
    carrier_result = pending :: pending | ok | {error, term()},
    payment_result = pending :: pending | ok | {error, term()},
    transit_result = pending :: pending | ok | {error, term()},
    delivery_result = pending :: pending | ok | {error, term()}
}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type fulfillment_state() :: #fulfillment_state{}.
-type subprocess_result() :: pending | ok | {error, term()}.
-export_type([fulfillment_state/0, subprocess_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Order Fulfillment state.
%% @end
%%--------------------------------------------------------------------
-spec new(OrderInput :: #order{} | map()) -> #fulfillment_state{}.
new(OrderInput) when is_record(OrderInput, order) ->
    LogId = generate_log_id(),
    #fulfillment_state{
        order = OrderInput,
        log_id = LogId,
        started_at = erlang:system_time(millisecond)
    };
new(OrderInput) when is_map(OrderInput) ->
    Order = #order{
        order_id = maps_get_default(OrderInput, <<"order_id">>, generate_order_id()),
        customer_id = maps_get_default(OrderInput, <<"customer_id">>, <<"CUST-UNKNOWN">>),
        customer_name = maps_get_default(OrderInput, <<"customer_name">>, <<"Unknown Customer">>),
        customer_email = maps_get_default(OrderInput, <<"customer_email">>, <<"unknown@example.com">>),
        items = parse_items(maps_get_default(OrderInput, <<"items">>, [])),
        shipping_address = maps_get_default(OrderInput, <<"shipping_address">>, #{}),
        billing_address = maps_get_default(OrderInput, <<"billing_address">>, #{}),
        subtotal = maps_get_default(OrderInput, <<"subtotal">>, 0.0),
        tax = maps_get_default(OrderInput, <<"tax">>, 0.0),
        shipping_cost = maps_get_default(OrderInput, <<"shipping_cost">>, 0.0),
        total = maps_get_default(OrderInput, <<"total">>, 0.0),
        status = pending,
        created_at = erlang:system_time(millisecond),
        notes = maps_get_default(OrderInput, <<"notes">>, undefined)
    },
    PaymentDetails = maps_get_default(OrderInput, <<"payment_details">>, #{
        <<"method">> => credit_card
    }),
    LogId = generate_log_id(),
    #fulfillment_state{
        order = Order,
        payment_details = PaymentDetails,
        log_id = LogId,
        started_at = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Order Fulfillment workflow.
%% @end
%%--------------------------------------------------------------------
-spec start(OrderInput :: #order{} | map()) -> {ok, pid()} | {error, term()}.
start(OrderInput) ->
    FulfillmentState = new(OrderInput),
    gen_pnet:start_link(?MODULE, FulfillmentState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Order Fulfillment workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(OrderInput :: #order{} | map()) ->
          {ok, #fulfillment_state{}} | {error, term()}.
run(OrderInput) ->
    case start(OrderInput) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 300000) of  %% 5 minute timeout
                {ok, State} ->
                    gen_pnet:stop(Pid),
                    {ok, State};
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current status of the Order Fulfillment workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_status(Pid :: pid()) -> map().
get_status(Pid) ->
    case gen_pnet:call(Pid, get_status) of
        {ok, Status} -> Status;
        {error, _} -> #{status => unknown}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Order Fulfillment workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, #fulfillment_state{}} | {error, term()}.
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Order Fulfillment Petri net.
%% @end
%%--------------------------------------------------------------------
place_lst() ->
    [
        'p_input',
        'p_ordering_complete',
        'p_carrier_complete',
        'p_payment_complete',
        'p_transit_complete',
        'p_delivery_complete',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Order Fulfillment Petri net.
%% @end
%%--------------------------------------------------------------------
trsn_lst() ->
    [
        't_start_ordering',
        't_start_carrier',
        't_start_payment',
        't_start_transit',
        't_start_delivery',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for the Petri net.
%% @end
%%--------------------------------------------------------------------
init_marking('p_input', _UsrInfo) ->
    [start];
init_marking(_, _) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
preset('t_start_ordering') -> ['p_input'];
preset('t_start_carrier') -> ['p_ordering_complete'];
preset('t_start_payment') -> ['p_carrier_complete'];
preset('t_start_transit') -> ['p_payment_complete'];
preset('t_start_delivery') -> ['p_transit_complete'];
preset('t_complete') -> ['p_delivery_complete'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%% @end
%%--------------------------------------------------------------------
is_enabled('t_start_carrier', _Mode, #fulfillment_state{ordering_result = ok}) ->
    true;
is_enabled('t_start_carrier', _Mode, _State) ->
    false;

is_enabled('t_start_payment', _Mode, #fulfillment_state{carrier_result = ok}) ->
    true;
is_enabled('t_start_payment', _Mode, _State) ->
    false;

is_enabled('t_start_transit', _Mode, #fulfillment_state{payment_result = ok}) ->
    true;
is_enabled('t_start_transit', _Mode, _State) ->
    false;

is_enabled('t_start_delivery', _Mode, #fulfillment_state{transit_result = ok}) ->
    true;
is_enabled('t_start_delivery', _Mode, _State) ->
    false;

is_enabled(_Trsn, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: #fulfillment_state{}) ->
          {produce, map()} | abort.

fire('t_start_ordering', #{'p_input' := [start]}, State) ->
    Order = State#fulfillment_state.order,
    log_event(State, <<"OrderFulfillment">>, <<"OrderingStarted">>, #{
        <<"order_id">> => Order#order.order_id
    }),
    {ok, OrderingPid} = start_ordering(State),
    Result = wait_for_subprocess(OrderingPid, ordering),
    log_event(State, <<"OrderFulfillment">>, <<"OrderingComplete">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"result">> => result_to_binary(Result)
    }),
    {produce, #{'p_ordering_complete' => [start]}};

fire('t_start_carrier', #{'p_ordering_complete' := [start]}, State) ->
    Order = State#fulfillment_state.order,
    log_event(State, <<"OrderFulfillment">>, <<"CarrierAppointmentStarted">>, #{
        <<"order_id">> => Order#order.order_id
    }),
    {ok, CarrierPid} = start_carrier(State),
    Result = wait_for_subprocess(CarrierPid, carrier),
    log_event(State, <<"OrderFulfillment">>, <<"CarrierAppointmentComplete">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"result">> => result_to_binary(Result)
    }),
    {produce, #{'p_carrier_complete' => [start]}};

fire('t_start_payment', #{'p_carrier_complete' := [start]}, State) ->
    Order = State#fulfillment_state.order,
    log_event(State, <<"OrderFulfillment">>, <<"PaymentStarted">>, #{
        <<"order_id">> => Order#order.order_id
    }),
    {ok, PaymentPid} = start_payment(State),
    Result = wait_for_subprocess(PaymentPid, payment),
    log_event(State, <<"OrderFulfillment">>, <<"PaymentComplete">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"result">> => result_to_binary(Result)
    }),
    {produce, #{'p_payment_complete' => [start]}};

fire('t_start_transit', #{'p_payment_complete' := [start]}, State) ->
    Order = State#fulfillment_state.order,
    log_event(State, <<"OrderFulfillment">>, <<"TransitStarted">>, #{
        <<"order_id">> => Order#order.order_id
    }),
    {ok, TransitPid} = start_transit(State),
    Result = wait_for_subprocess(TransitPid, freight_in_transit),
    log_event(State, <<"OrderFulfillment">>, <<"TransitComplete">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"result">> => result_to_binary(Result)
    }),
    {produce, #{'p_transit_complete' => [start]}};

fire('t_start_delivery', #{'p_transit_complete' := [start]}, State) ->
    Order = State#fulfillment_state.order,
    Shipment = State#fulfillment_state.shipment,
    ShipmentId = case Shipment of
        undefined -> generate_shipment_id();
        S -> S#shipment.shipment_id
    end,
    log_event(State, <<"OrderFulfillment">>, <<"DeliveryStarted">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"shipment_id">> => ShipmentId
    }),
    {ok, DeliveryPid} = start_delivery(State),
    Result = wait_for_subprocess(DeliveryPid, freight_delivered),
    log_event(State, <<"OrderFulfillment">>, <<"DeliveryComplete">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"result">> => result_to_binary(Result)
    }),
    {produce, #{'p_delivery_complete' => [start]}};

fire('t_complete', #{'p_delivery_complete' := [start]}, State) ->
    Order = State#fulfillment_state.order,
    CompletedAt = erlang:system_time(millisecond),
    log_event(State, <<"OrderFulfillment">>, <<"OrderFulfillmentComplete">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"duration_ms">> => CompletedAt - State#fulfillment_state.started_at
    }),
    {produce, #{'p_output' => [start]}};

fire(_Trsn, _Mode, _State) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for custom processing.
%% @end
%%--------------------------------------------------------------------
trigger(Place, _Token, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    log_event(UsrInfo, <<"OrderFulfillment">>, <<"PlaceEntered">>, #{
        <<"place">> => atom_to_binary(Place, utf8)
    }),
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
init(FulfillmentState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"OrderFulfillment">>}) of
        {ok, LogId} ->
            State1 = FulfillmentState#fulfillment_state{log_id = LogId},
            OrderId = case State1#fulfillment_state.order of
                undefined -> <<"UNKNOWN">>;
                O -> O#order.order_id
            end,
            yawl_xes:log_case_start(LogId, OrderId),
            State1;
        _ ->
            FulfillmentState
    end.

%%--------------------------------------------------------------------
%% @doc Handles call messages.
%% @end
%%--------------------------------------------------------------------
handle_call(get_state, _From, NetState) ->
    {reply, {ok, NetState}, NetState};
handle_call(get_status, _From, NetState) ->
    Status = #{
        <<"order_id">> => case NetState#fulfillment_state.order of
            undefined -> <<"UNKNOWN">>;
            O -> O#order.order_id
        end,
        <<"ordering">> => result_to_binary(NetState#fulfillment_state.ordering_result),
        <<"carrier">> => result_to_binary(NetState#fulfillment_state.carrier_result),
        <<"payment">> => result_to_binary(NetState#fulfillment_state.payment_result),
        <<"transit">> => result_to_binary(NetState#fulfillment_state.transit_result),
        <<"delivery">> => result_to_binary(NetState#fulfillment_state.delivery_result),
        <<"started_at">> => NetState#fulfillment_state.started_at,
        <<"completed_at">> => NetState#fulfillment_state.completed_at
    },
    {reply, {ok, Status}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles cast messages.
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-call/cast messages.
%% @end
%%--------------------------------------------------------------------
handle_info(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Terminates the gen_pnet.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, NetState) ->
    case NetState of
        #net_state{usr_info = #fulfillment_state{log_id = LogId} = UsrInfo}
          when LogId =/= undefined ->
            OrderId = case UsrInfo#fulfillment_state.order of
                undefined -> <<"UNKNOWN">>;
                O -> O#order.order_id
            end,
            yawl_xes:log_case_complete(LogId, OrderId, #{
                <<"ordering">> => result_to_binary(UsrInfo#fulfillment_state.ordering_result),
                <<"carrier">> => result_to_binary(UsrInfo#fulfillment_state.carrier_result),
                <<"payment">> => result_to_binary(UsrInfo#fulfillment_state.payment_result),
                <<"transit">> => result_to_binary(UsrInfo#fulfillment_state.transit_result),
                <<"delivery">> => result_to_binary(UsrInfo#fulfillment_state.delivery_result)
            });
        _ -> ok
    end,
    ok.

%%====================================================================
%% Subprocess Orchestration Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the Ordering subprocess.
%% @end
%%--------------------------------------------------------------------
-spec start_ordering(State :: #fulfillment_state{}) -> {ok, pid()} | {error, term()}.
start_ordering(#fulfillment_state{order = Order}) ->
    ordering:start(Order).

%%--------------------------------------------------------------------
%% @doc Starts the Carrier Appointment subprocess.
%% @end
%%--------------------------------------------------------------------
-spec start_carrier(State :: #fulfillment_state{}) -> {ok, pid()} | {error, term()}.
start_carrier(#fulfillment_state{order = Order}) ->
    %% Build purchase order map for carrier appointment
    PurchaseOrder = #{
        <<"id">> => Order#order.order_id,
        <<"customer_id">> => Order#order.customer_id,
        <<"destination">> => get_destination(Order),
        <<"items">> => convert_items(Order#order.items),
        <<"total_volume">> => calculate_volume(Order#order.items)
    },
    carrier_appointment:start(PurchaseOrder, <<"carrier_log">>).

%%--------------------------------------------------------------------
%% @doc Starts the Payment subprocess.
%% @end
%%--------------------------------------------------------------------
-spec start_payment(State :: #fulfillment_state{}) -> {ok, pid()} | {error, term()}.
start_payment(#fulfillment_state{order = Order, payment_details = PaymentDetails}) ->
    payment:start(Order, PaymentDetails).

%%--------------------------------------------------------------------
%% @doc Starts the Freight In Transit subprocess.
%% @end
%%--------------------------------------------------------------------
-spec start_transit(State :: #fulfillment_state{}) -> {ok, pid()} | {error, term()}.
start_transit(#fulfillment_state{order = Order}) ->
    %% Create shipment record
    Shipment = #shipment{
        shipment_id = generate_shipment_id(),
        order_id = Order#order.order_id,
        carrier = <<"DEFAULT-CARRIER">>,
        tracking_number = generate_tracking_number(),
        shipping_method = determine_shipping_method(Order),
        origin = <<"WAREHOUSE-001">>,
        destination = get_destination(Order),
        current_location = undefined,
        status = picked_up,
        estimated_delivery = undefined,
        actual_delivery = undefined,
        weight = calculate_weight(Order#order.items),
        dimensions = undefined
    },
    freight_in_transit:start(Shipment, Order).

%%--------------------------------------------------------------------
%% @doc Starts the Freight Delivered subprocess.
%% @end
%%--------------------------------------------------------------------
-spec start_delivery(State :: #fulfillment_state{}) -> {ok, pid()} | {error, term()}.
start_delivery(#fulfillment_state{order = Order, shipment = Shipment}) ->
    freight_delivered:start(Shipment, Order).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
log_event(#fulfillment_state{log_id = undefined}, _, _, _) ->
    ok;
log_event(#fulfillment_state{log_id = LogId}, ConceptName, LifecycleTransition, Data) ->
    yawl_xes:log_event(LogId, ConceptName, LifecycleTransition, Data).

%% @private
wait_for_subprocess(Pid, _Module) ->
    %% Wait for subprocess to complete with timeout
    Timeout = 60000,  %% 60 seconds
    Ref = monitor(process, Pid),
    wait_for_subprocess_result(Pid, Ref, Timeout).

wait_for_subprocess_result(_Pid, Ref, Timeout) when Timeout =< 0 ->
    demonitor(Ref, [flush]),
    {error, timeout};
wait_for_subprocess_result(Pid, Ref, Timeout) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            case Reason of
                normal -> ok;
                _ -> {error, Reason}
            end;
        {Pid, complete} ->
            demonitor(Ref, [flush]),
            ok
    after 1000 ->
        wait_for_subprocess_result(Pid, Ref, Timeout - 1000)
    end.

%% @private
result_to_binary(ok) -> <<"ok">>;
result_to_binary(pending) -> <<"pending">>;
result_to_binary({error, Reason}) ->
    list_to_binary(["error:", atom_to_list(Reason)]).

%% @private
generate_log_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"fulfillment_log_", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_order_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"ORDER-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_shipment_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"SHIP-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_tracking_number() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"1Z", (integer_to_binary(Timestamp))/binary>>.

%% @private
maps_get_default(Map, Key, Default) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Value -> Value
    end.

%% @private
parse_items([]) -> [];
parse_items(Items) when is_list(Items) ->
    lists:map(fun parse_item/1, Items);
parse_items(_) -> [].

%% @private
parse_item(Item) when is_map(Item) ->
    #item{
        sku = maps_get_default(Item, <<"sku">>, <<"UNKNOWN-SKU">>),
        name = maps_get_default(Item, <<"name">>, <<"Unknown Item">>),
        quantity = maps_get_default(Item, <<"quantity">>, 1),
        price = maps_get_default(Item, <<"price">>, 0.0),
        weight = maps_get_default(Item, <<"weight">>, 1.0),
        category = undefined
    };
parse_item(Item) when is_record(Item, item) ->
    Item.

%% @private
convert_items([]) -> [];
convert_items(Items) when is_list(Items) ->
    lists:map(fun(#item{sku = SKU, quantity = Q, weight = W}) ->
        #{<<"sku">> => SKU, <<"quantity">> => Q, <<"weight">> => W}
    end, Items).

%% @private
calculate_volume([]) -> 0.0;
calculate_volume(Items) ->
    lists:foldl(fun(#item{quantity = Q, weight = W}, Acc) ->
        Acc + (Q * W * 10)  %% Rough volume estimate
    end, 0.0, Items).

%% @private
calculate_weight([]) -> 0.0;
calculate_weight(Items) ->
    lists:foldl(fun(#item{quantity = Q, weight = W}, Acc) ->
        Acc + (Q * W)
    end, 0.0, Items).

%% @private
determine_shipping_method(#order{items = Items}) ->
    TotalWeight = calculate_weight(Items),
    if
        TotalWeight > 1000 -> ftl;
        TotalWeight > 100 -> ltl;
        true -> single
    end.

%% @private
get_destination(#order{shipping_address = Address}) when is_map(Address), map_size(Address) > 0 ->
    case maps:get(<<"city">>, Address, undefined) of
        undefined -> <<"UNKNOWN-DEST">>;
        City -> City
    end;
get_destination(_) ->
    <<"UNKNOWN-DEST">>.

%% @private
wait_for_completion(Pid, Timeout) ->
    wait_for_completion(Pid, Timeout, 0).

wait_for_completion(_Pid, Timeout, Elapsed) when Elapsed >= Timeout ->
    {error, timeout};
wait_for_completion(Pid, Timeout, Elapsed) ->
    case get_state(Pid) of
        {ok, #fulfillment_state{delivery_result = Result}} when Result =:= ok orelse element(1, Result) =:= error ->
            get_state(Pid);
        {ok, _} ->
            timer:sleep(500),
            wait_for_completion(Pid, Timeout, Elapsed + 500);
        {error, Reason} ->
            {error, Reason}
    end.
