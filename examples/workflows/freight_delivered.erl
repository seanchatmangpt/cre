%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Freight Delivered Subprocess - YAWL Order Fulfillment Pattern
%%
%% This module implements the Freight Delivered subprocess from the CAISE 2013 paper.
%% The Delivery workflow is modeled as a Petri net using gen_pnet:
%% - Places (circles in YAWL) = Conditions/states
%% - Transitions (boxes in YAWL) = Tasks
%% - Tokens = Flow of control
%% - Arcs = Flow relations
%%
%% Petri Net Structure:
%% PLACES:                          TRANSITIONS:
%% p_input                          t_receive_delivery
%% p_delivery_received              t_verify_items (WCP-18: Milestone)
%% p_items_verified                 t_obtain_signature
%% p_signature_obtained             t_send_receipt
%% p_receipt_sent                   t_complete
%% p_output
%%
%% Key Patterns:
%% - WCP-18 (Milestone) - Items must be verified before signature
%%
%% @reference Supporting Risk-Informed Decisions during Business Process Execution (CAISE 2013)
%% @end
%%--------------------------------------------------------------------

-module(freight_delivered).
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
-export([new/2, start/2, run/2, get_state/1]).
-export([receive_delivery/1, verify_items/1, obtain_signature/1,
         send_receipt/1]).

%% Include shared types
-include_lib("order_fulfillment_types.hrl").
-include_lib("gen_pnet/include/gen_pnet.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(delivery_state, {
    delivery :: #delivery{} | undefined,
    shipment :: #shipment{} | undefined,
    order :: #order{} | undefined,
    items_verified = false :: boolean(),
    signature_obtained = false :: boolean(),
    receipt_sent = false :: boolean(),
    log_id :: binary() | undefined
}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type delivery_state() :: #delivery_state{}.
-export_type([delivery_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Freight Delivered Petri net state.
%% @end
%%--------------------------------------------------------------------
-spec new(Shipment :: #shipment{}, Order :: #order{}) -> #delivery_state{}.
new(Shipment, Order) when is_record(Shipment, shipment), is_record(Order, order) ->
    DeliveryId = generate_delivery_id(),
    Delivery = #delivery{
        delivery_id = DeliveryId,
        shipment_id = Shipment#shipment.shipment_id,
        delivered_at = erlang:system_time(millisecond),
        delivered_by = Shipment#shipment.carrier,
        signature = undefined,
        photo_proof = undefined,
        notes = undefined,
        condition = undefined
    },
    LogId = generate_log_id(),
    #delivery_state{
        delivery = Delivery,
        shipment = Shipment,
        order = Order,
        log_id = LogId
    };
new(ShipmentMap, OrderMap) when is_map(ShipmentMap), is_map(OrderMap) ->
    Shipment = #shipment{
        shipment_id = maps_get_default(ShipmentMap, <<"shipment_id">>, generate_shipment_id()),
        order_id = maps_get_default(ShipmentMap, <<"order_id">>, <<"UNKNOWN-ORDER">>),
        carrier = maps_get_default(ShipmentMap, <<"carrier">>, <<"DEFAULT-CARRIER">>),
        tracking_number = maps_get_default(ShipmentMap, <<"tracking_number">>, <<"TRACK-UNKNOWN">>),
        shipping_method = maps_get_default(ShipmentMap, <<"shipping_method">>, ltl),
        origin = maps_get_default(ShipmentMap, <<"origin">>, <<"UNKNOWN">>),
        destination = maps_get_default(ShipmentMap, <<"destination">>, <<"UNKNOWN">>),
        current_location = maps_get_default(ShipmentMap, <<"destination">>, <<"UNKNOWN">>),
        status = delivered,
        estimated_delivery = undefined,
        actual_delivery = erlang:system_time(millisecond),
        weight = maps_get_default(ShipmentMap, <<"weight">>, 0.0),
        dimensions = undefined
    },
    Order = #order{
        order_id = maps_get_default(OrderMap, <<"order_id">>, <<"UNKNOWN-ORDER">>),
        customer_id = maps_get_default(OrderMap, <<"customer_id">>, <<"CUST-UNKNOWN">>),
        customer_name = maps_get_default(OrderMap, <<"customer_name">>, <<"Unknown Customer">>),
        customer_email = maps_get_default(OrderMap, <<"customer_email">>, <<"unknown@example.com">>),
        items = parse_items(maps_get_default(OrderMap, <<"items">>, [])),
        shipping_address = maps_get_default(OrderMap, <<"shipping_address">>, #{}),
        billing_address = #{},
        subtotal = 0.0,
        tax = 0.0,
        shipping_cost = 0.0,
        total = 0.0,
        status = shipped,
        created_at = erlang:system_time(millisecond)
    },
    new(Shipment, Order).

%%--------------------------------------------------------------------
%% @doc Starts the Freight Delivered workflow.
%% @end
%%--------------------------------------------------------------------
-spec start(Shipment :: #shipment{} | map(), Order :: #order{} | map()) ->
          {ok, pid()} | {error, term()}.
start(Shipment, Order) ->
    DeliveryState = new(Shipment, Order),
    gen_pnet:start_link(?MODULE, DeliveryState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Freight Delivered workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Shipment :: #shipment{} | map(), Order :: #order{} | map()) ->
          {ok, #delivery{}} | {error, term()}.
run(Shipment, Order) ->
    case start(Shipment, Order) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, #delivery_state{delivery = Delivery}} ->
                    gen_pnet:stop(Pid),
                    {ok, Delivery};
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Delivery workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, #delivery_state{}} | {error, term()}.
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Delivery Petri net.
%% @end
%%--------------------------------------------------------------------
place_lst() ->
    [
        'p_input',
        'p_delivery_received',
        'p_items_verified',
        'p_signature_obtained',
        'p_receipt_sent',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Delivery Petri net.
%% @end
%%--------------------------------------------------------------------
trsn_lst() ->
    [
        't_receive_delivery',
        't_verify_items',
        't_obtain_signature',
        't_send_receipt',
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
preset('t_receive_delivery') -> ['p_input'];
preset('t_verify_items') -> ['p_delivery_received'];
preset('t_obtain_signature') -> ['p_items_verified'];
preset('t_send_receipt') -> ['p_signature_obtained'];
preset('t_complete') -> ['p_receipt_sent'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%% @end
%%--------------------------------------------------------------------
is_enabled('t_obtain_signature', _Mode, #delivery_state{items_verified = true}) ->
    true;
is_enabled('t_obtain_signature', _Mode, _State) ->
    false;

is_enabled(_Trsn, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: #delivery_state{}) ->
          {produce, map()} | abort.

fire('t_receive_delivery', #{'p_input' := [start]}, State) ->
    Delivery = State#delivery_state.delivery,
    Shipment = State#delivery_state.shipment,
    receive_delivery(State),
    log_event(State, <<"Delivery">>, <<"DeliveryReceived">>, #{
        <<"delivery_id">> => Delivery#delivery.delivery_id,
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"carrier">> => Delivery#delivery.delivered_by
    }),
    {produce, #{'p_delivery_received' => [start]}};

fire('t_verify_items', #{'p_delivery_received' := [start]}, State) ->
    Delivery = State#delivery_state.delivery,
    Order = State#delivery_state.order,
    #{verified := Verified, condition := Condition} = verify_items(State),
    log_event(State, <<"Delivery">>, <<"ItemsVerified">>, #{
        <<"delivery_id">> => Delivery#delivery.delivery_id,
        <<"verified">> => Verified,
        <<"condition">> => Condition,
        <<"item_count">> => length(Order#order.items)
    }),
    {produce, #{'p_items_verified' => [start]}};

fire('t_obtain_signature', #{'p_items_verified' := [start]}, State) ->
    Delivery = State#delivery_state.delivery,
    Order = State#delivery_state.order,
    #{signature := Signature, obtained := _Obtained} = obtain_signature(State),
    log_event(State, <<"Delivery">>, <<"SignatureObtained">>, #{
        <<"delivery_id">> => Delivery#delivery.delivery_id,
        <<"signature">> => Signature,
        <<"customer_name">> => Order#order.customer_name
    }),
    {produce, #{'p_signature_obtained' => [start]}};

fire('t_send_receipt', #{'p_signature_obtained' := [start]}, State) ->
    Delivery = State#delivery_state.delivery,
    Order = State#delivery_state.order,
    send_receipt(State),
    log_event(State, <<"Delivery">>, <<"ReceiptSent">>, #{
        <<"delivery_id">> => Delivery#delivery.delivery_id,
        <<"order_id">> => Order#order.order_id,
        <<"customer_email">> => Order#order.customer_email
    }),
    {produce, #{'p_receipt_sent' => [start]}};

fire('t_complete', #{'p_receipt_sent' := [start]}, State) ->
    Delivery = State#delivery_state.delivery,
    log_event(State, <<"Delivery">>, <<"DeliveryComplete">>, #{
        <<"delivery_id">> => Delivery#delivery.delivery_id,
        <<"order_id">> => State#delivery_state.order#order.order_id
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
    log_event(UsrInfo, <<"Delivery">>, <<"PlaceEntered">>, #{
        <<"place">> => atom_to_binary(Place, utf8)
    }),
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
init(DeliveryState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"Delivery">>}) of
        {ok, LogId} ->
            State1 = DeliveryState#delivery_state{log_id = LogId},
            DeliveryId = case State1#delivery_state.delivery of
                undefined -> <<"UNKNOWN">>;
                D -> D#delivery.delivery_id
            end,
            yawl_xes:log_case_start(LogId, DeliveryId),
            State1;
        _ ->
            DeliveryState
    end.

%%--------------------------------------------------------------------
%% @doc Handles call messages.
%% @end
%%--------------------------------------------------------------------
handle_call(get_state, _From, NetState) ->
    {reply, {ok, NetState}, NetState};
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
        #net_state{usr_info = #delivery_state{log_id = LogId} = UsrInfo}
          when LogId =/= undefined ->
            DeliveryId = case UsrInfo#delivery_state.delivery of
                undefined -> <<"UNKNOWN">>;
                D -> D#delivery.delivery_id
            end,
            yawl_xes:log_case_complete(LogId, DeliveryId, #{
                <<"items_verified">> => UsrInfo#delivery_state.items_verified,
                <<"signature_obtained">> => UsrInfo#delivery_state.signature_obtained,
                <<"receipt_sent">> => UsrInfo#delivery_state.receipt_sent
            });
        _ -> ok
    end,
    ok.

%%====================================================================
%% Task Implementation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Receives and records the delivery.
%% @end
%%--------------------------------------------------------------------
-spec receive_delivery(State :: #delivery_state{}) -> ok.
receive_delivery(#delivery_state{delivery = Delivery, shipment = Shipment}) ->
    %% In production, verify with carrier that delivery was made
    {ok, {
        Delivery#delivery.delivery_id,
        Shipment#shipment.tracking_number
    }}.

%%--------------------------------------------------------------------
%% @doc Verifies items in the delivery (WCP-18: Milestone).
%% @end
%%--------------------------------------------------------------------
-spec verify_items(State :: #delivery_state{}) -> map().
verify_items(#delivery_state{order = Order, delivery = Delivery}) ->
    %% Verify all items are present and undamaged
    Items = Order#order.items,
    VerificationResults = lists:map(fun verify_item/1, Items),
    AllVerified = lists:all(fun(R) -> maps:get(verified, R, false) end, VerificationResults),
    Condition = case AllVerified of
        true -> <<"good">>;
        false -> <<"damaged">>
    end,
    #{
        verified => AllVerified,
        condition => Condition,
        items => VerificationResults
    }.

%%--------------------------------------------------------------------
%% @doc Verifies a single item.
%% @end
%%--------------------------------------------------------------------
verify_item(#item{sku = SKU, name = Name, quantity = Qty}) ->
    %% Simulate item verification
    IsDamaged = rand:uniform(20) =:= 1,  %% 5% chance of damage
    #{
        sku => SKU,
        name => Name,
        quantity => Qty,
        verified => not IsDamaged,
        condition => case IsDamaged of
            true -> <<"damaged">>;
            false -> <<"good">>
        end
    }.

%%--------------------------------------------------------------------
%% @doc Obtains customer signature on delivery.
%% @end
%%--------------------------------------------------------------------
-spec obtain_signature(State :: #delivery_state{}) -> map().
obtain_signature(#delivery_state{delivery = Delivery, order = Order}) ->
    %% Simulate signature capture
    CustomerName = Order#order.customer_name,
    Signature = case rand:uniform(10) of
        1 -> undefined;  %% 10% chance no signature
        _ -> list_to_binary(["Signed by ", CustomerName, " on ",
            date_to_string(erlang:system_time(millisecond))])
    end,
    Obtained = Signature =/= undefined,
    #{
        signature => Signature,
        obtained => Obtained,
        timestamp => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Sends delivery receipt to customer.
%% @end
%%--------------------------------------------------------------------
-spec send_receipt(State :: #delivery_state{}) -> ok.
send_receipt(#delivery_state{order = Order, delivery = Delivery, shipment = Shipment}) ->
    %% In production, send email/SMS receipt
    {ok, {
        Order#order.customer_email,
        Delivery#delivery.delivery_id,
        Shipment#shipment.tracking_number,
        Delivery#delivery.delivered_at
    }}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
log_event(#delivery_state{log_id = undefined}, _, _, _) ->
    ok;
log_event(#delivery_state{log_id = LogId}, ConceptName, LifecycleTransition, Data) ->
    yawl_xes:log_event(LogId, ConceptName, LifecycleTransition, Data).

%% @private
generate_log_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"delivery_log_", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_delivery_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"DEL-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_shipment_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"SHIP-", (integer_to_binary(Timestamp))/binary>>.

%% @private
date_to_string(Millis) ->
    Seconds = Millis div 1000,
    DateTime = calendar:gregorian_seconds_to_datetime(
        Seconds + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
    ),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Minute, Second])).

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
wait_for_completion(Pid, Timeout) ->
    wait_for_completion(Pid, Timeout, 0).

wait_for_completion(Pid, Timeout, Elapsed) when Elapsed >= Timeout ->
    {error, timeout};
wait_for_completion(Pid, Timeout, Elapsed) ->
    case get_state(Pid) of
        {ok, #delivery_state{receipt_sent = true}} ->
            get_state(Pid);
        {ok, _} ->
            timer:sleep(100),
            wait_for_completion(Pid, Timeout, Elapsed + 100);
        {error, Reason} ->
            {error, Reason}
    end.
