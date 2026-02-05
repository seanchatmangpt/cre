%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Order Fulfillment Shared Type Definitions
%%
%% This header file defines all shared records and types used by the
%% YAWL Order Fulfillment workflow modules (ordering, payment,
%% freight_in_transit, freight_delivered, order_fulfillment).
%%
%% @end
%%--------------------------------------------------------------------

%%====================================================================
%% Order Item Record (must be defined first, used by order record)
%%====================================================================
-record(item, {
    sku :: binary(),
    name :: binary(),
    quantity :: non_neg_integer(),
    price :: float(),
    weight :: float(),
    category :: binary() | undefined
}).

%%====================================================================
%% Customer Order Record
%%====================================================================
-record(order, {
    order_id :: binary(),
    customer_id :: binary(),
    customer_name :: binary(),
    customer_email :: binary(),
    items :: list(#item{}),
    shipping_address :: map(),
    billing_address :: map(),
    subtotal :: float(),
    tax :: float(),
    shipping_cost :: float(),
    total :: float(),
    status :: pending | confirmed | processing | shipped | delivered | cancelled,
    created_at :: integer(),
    notes :: binary() | undefined
}).

%%====================================================================
%% Payment Information Record
%%====================================================================
-record(payment, {
    payment_id :: binary(),
    order_id :: binary(),
    method :: credit_card | paypal | bank_transfer,
    amount :: float(),
    currency :: binary(),
    status :: pending | processing | completed | failed | refunded,
    transaction_id :: binary() | undefined,
    created_at :: integer(),
    completed_at :: integer() | undefined,
    failure_reason :: binary() | undefined
}).

%%====================================================================
%% Credit Card Payment Details
%%====================================================================
-record(credit_card, {
    card_number :: binary(),
    cardholder_name :: binary(),
    expiry_month :: 1..12,
    expiry_year :: integer(),
    cvv :: binary(),
    billing_zip :: binary()
}).

%%====================================================================
%% PayPal Payment Details
%%====================================================================
-record(paypal, {
    email :: binary(),
    payer_id :: binary()
}).

%%====================================================================
%% Bank Transfer Details
%%====================================================================
-record(bank_transfer, {
    account_number :: binary(),
    routing_number :: binary(),
    account_holder :: binary(),
    bank_name :: binary()
}).

%%====================================================================
%% Shipment Information Record
%%====================================================================
-record(shipment, {
    shipment_id :: binary(),
    order_id :: binary(),
    carrier :: binary(),
    tracking_number :: binary(),
    shipping_method :: ftl | ltl | single,
    origin :: binary(),
    destination :: binary(),
    current_location :: binary() | undefined,
    status :: preparing | picked_up | in_transit | out_for_delivery | delivered,
    estimated_delivery :: integer() | undefined,
    actual_delivery :: integer() | undefined,
    weight :: float(),
    dimensions :: map() | undefined
}).

%%====================================================================
%% Tracking Event Record
%%====================================================================
-record(tracking_event, {
    event_id :: binary(),
    timestamp :: integer(),
    location :: binary(),
    status :: binary(),
    description :: binary()
}).

%%====================================================================
%% Delivery Confirmation Record
%%====================================================================
-record(delivery, {
    delivery_id :: binary(),
    shipment_id :: binary(),
    delivered_at :: integer(),
    delivered_by :: binary(),
    signature :: binary() | undefined,
    photo_proof :: binary() | undefined,
    notes :: binary() | undefined,
    condition :: binary() | undefined
}).

%%====================================================================
%% Type Exports
%%====================================================================

-type order_status() :: pending | confirmed | processing | shipped | delivered | cancelled.
-type payment_method() :: credit_card | paypal | bank_transfer.
-type payment_status() :: pending | processing | completed | failed | refunded.
-type shipping_method() :: ftl | ltl | single.
-type shipment_status() :: preparing | picked_up | in_transit | out_for_delivery | delivered.

-export_type([
    order_status/0,
    payment_method/0,
    payment_status/0,
    shipping_method/0,
    shipment_status/0
]).
