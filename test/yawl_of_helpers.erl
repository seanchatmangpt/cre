%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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

-module(yawl_of_helpers).
-moduledoc """
Test helper functions for Order Fulfillment YAWL 2.1 workflow.

This module consolidates all Order Fulfillment-specific business logic
that was previously spread across multiple handler modules. These are
TEST HELPERS for the Order Fulfillment workflow example, not production
source code.

## Payment Method Determination

```erlang
> OrderData = #{total_amount => 50000, weight => 1000, distance => 500}.
> Rules = #{ftl_threshold => 10000, ltl_threshold => 5000}.
> yawl_of_helpers:determine_payment_method(OrderData, Rules).
{ok, ftl}
```

## Payment Branch Selection

```erlang
> OrderData = #{volume => 6000, package_count => 25}.
> yawl_of_helpers:determine_branch(OrderData).
{ok, ftl}
```

## Credit Card Processing

```erlang
> PaymentInfo = #{
..   card_number => "4111111111111111",
..   expiry => "12/25",
..   cvv => "123",
..   amount => 10000
.. }.
> OrderData = #{order_id => "ORD-12345"}.
> yawl_of_helpers:process_credit_card(PaymentInfo, OrderData).
{ok, #{status => approved, transaction_id => <<"TXN-">>}}
```

## Inventory Management

```erlang
> OrderData = #{items => [{sku123, 5}, {sku456, 2}]}.
> yawl_of_helpers:check_inventory(OrderData).
true

> yawl_of_helpers:allocate_inventory(OrderData).
{ok, #{allocated => [{sku123, 5}, {sku456, 2}]}}
```

## Shipping Coordination

```erlang
> yawl_of_helpers:select_carrier(ftl, #{distance => 500}).
{ok, fedex_freight}

> yawl_of_helpers:calculate_shipping(#{weight => 1000}, fedex_freight).
{ok, 250.00}
```
""".

%%====================================================================
%% Exports - Order Fulfillment Handlers
%%====================================================================

%% Payment method determination
-export([determine_payment_method/2]).

%% Inventory management
-export([allocate_inventory/1, check_inventory/1]).

%% Shipping coordination
-export([select_carrier/2, calculate_shipping/2]).

%%====================================================================
%% Exports - Payment Handlers
%%====================================================================

%% Credit card processing
-export([process_credit_card/2]).

%% Payment verification
-export([verify_payment/1, handle_payment_failure/2]).

%% Timeout handling
-export([check_payment_timeout/1]).

%%====================================================================
%% Exports - Payment Branch
%%====================================================================

%% Branch determination
-export([determine_branch/1]).

%% Branch predicates
-export([is_ftl/1, is_ltl/1, is_sp/1]).

%% Threshold configuration
-export([get_thresholds/0, set_thresholds/1, reset_thresholds/0]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Order data map.
%%
%% Contains order information relevant to payment method determination.
%% Keys include: total_amount, weight, distance, customer_tier, items.
%%--------------------------------------------------------------------
-type order_data() :: #{
    total_amount => number(),
    weight => number(),
    distance => number(),
    customer_tier => premium | standard | basic,
    items => list(),
    shipping_method => ftl | ltl | sp,
    carrier => atom()
}.

%%--------------------------------------------------------------------
%% @doc Business rules for payment method determination.
%%
%% Configuration for threshold values and business logic.
%%--------------------------------------------------------------------
-type rules() :: #{
    ftl_threshold => number(),
    ltl_threshold => number(),
    sp_threshold => number(),
    premium_discount => number()
}.

%%--------------------------------------------------------------------
%% @doc Payment method type.
%%
%% FTL - Full Truck Load: Large shipments requiring entire truck
%% LTL - Less Than Truckload: Medium shipments sharing truck space
%% SP - Small Parcel: Small shipments via parcel carriers
%%--------------------------------------------------------------------
-type payment_method() :: ftl | ltl | sp.
-type shipping_method() :: payment_method().

%%--------------------------------------------------------------------
%% @doc Carrier type.
%%
%% Available shipping carriers for different shipment types.
%%--------------------------------------------------------------------
-type carrier() :: fedex_freight | ups_freight | estes | xpo |
                   fedex | ups | dhl | usps | ontrac.

%%--------------------------------------------------------------------
%% @doc Allocated inventory result.
%%
%% Maps SKUs to allocated quantities.
%%--------------------------------------------------------------------
-type allocated_inventory() :: #{atom() | binary() => number()}.

%%--------------------------------------------------------------------
%% @doc Payment information map.
%%
%% Contains credit card and payment details.
%%--------------------------------------------------------------------
-type payment_info() :: #{
    card_number => binary() | string(),
    cardholder_name => binary() | string(),
    expiry => binary() | string(),
    cvv => binary() | string(),
    amount => number(),
    currency => binary()
}.

%%--------------------------------------------------------------------
%% @doc Payment result map.
%%
%% Contains the outcome of payment processing.
%%--------------------------------------------------------------------
-type payment_result() :: #{
    status => approved | declined | pending | failed,
    transaction_id => binary(),
    auth_code => binary() | undefined,
    message => binary(),
    timestamp => integer()
}.

%%--------------------------------------------------------------------
%% @doc Payment failure reason.
%%
%% Indicates why a payment failed.
%%--------------------------------------------------------------------
-type failure_reason() :: insufficient_funds | declined | expired |
                         invalid_card | timeout | processing_error |
                         fraud_detected | card_limit_exceeded.

%%--------------------------------------------------------------------
%% @doc Failure action type.
%%
%% Action to take after payment failure.
%%--------------------------------------------------------------------
-type failure_action() :: retry | abort | manual_review | contact_customer.

%%--------------------------------------------------------------------
%% @doc Branch result.
%%
%% Successful determination returns {ok, Method}.
%%--------------------------------------------------------------------
-type branch_result() :: {ok, shipping_method()}.

%%--------------------------------------------------------------------
%% @doc Error reason for branch determination failure.
%%--------------------------------------------------------------------
-type branch_error() :: {error, missing_volume | invalid_volume | invalid_data}.

%%--------------------------------------------------------------------
%% @doc Threshold configuration map.
%%
%% Maps shipping methods to their threshold values in pounds.
%%--------------------------------------------------------------------
-type thresholds() :: #{
    ftl => number(),   %% Minimum weight/volume for FTL
    ltl => number(),   %% Minimum weight/volume for LTL
    sp => number()     %% Maximum weight/volume for SP
}.

%%--------------------------------------------------------------------
%% @doc Handler result types.
%%--------------------------------------------------------------------
-type handler_ok() :: {ok, term()}.
-type handler_error() :: {error, term()}.

%% Export types
-export_type([order_data/0, rules/0, payment_method/0, carrier/0]).
-export_type([allocated_inventory/0, payment_info/0, payment_result/0]).
-export_type([failure_reason/0, failure_action/0, thresholds/0]).

%%====================================================================
%% Constants
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Default threshold values for branch determination.
%%
%% FTL threshold: 5000 lbs - shipments above this go FTL
%% LTL threshold: 500 lbs - shipments above this go LTL (if below FTL)
%% SP threshold: 500 lbs - shipments at or below this go SP
%%--------------------------------------------------------------------
-define(DEFAULT_THRESHOLDS, #{
    ftl => 5000,
    ltl => 500,
    sp => 500
}).

%%--------------------------------------------------------------------
%% @doc ETS table name for storing threshold configuration.
%%--------------------------------------------------------------------
-define(THRESHOLD_TABLE, yawl_payment_branch_thresholds).

%%====================================================================
%% API Functions - Payment Method Determination
%% (From yawl_orderfulfillment_handlers)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Determines payment method based on order data and rules.
%%
%% Evaluates order characteristics against configured thresholds to
%% select the appropriate payment method:
%%
%% - FTL (Full Truck Load): Orders above FTL threshold or very heavy items
%% - LTL (Less Than Truckload): Orders above LTL but below FTL threshold
%% - SP (Small Parcel): Small orders below LTL threshold
%%
%% @param OrderData Map containing order information
%% @param Rules Business rules for thresholds
%% @returns {ok, PaymentMethod} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec determine_payment_method(OrderData :: order_data(), Rules :: rules()) ->
    handler_ok() | handler_error().

determine_payment_method(OrderData, Rules) ->
    try
        TotalAmount = maps:get(total_amount, OrderData, 0),
        Weight = maps:get(weight, OrderData, 0),
        Distance = maps:get(distance, OrderData, 0),
        CustomerTier = maps:get(customer_tier, OrderData, standard),

        %% Get thresholds
        FtlThreshold = maps:get(ftl_threshold, Rules, 10000),
        LtlThreshold = maps:get(ltl_threshold, Rules, 1000),

        %% Apply business rules - check physical characteristics first
        Method = case {Weight, Distance, TotalAmount, CustomerTier} of
            %% Very heavy items - always FTL
            {WeightVal, _, _, _} when WeightVal >= 5000 ->
                ftl;
            %% Heavy items - LTL
            {WeightVal, _, _, _} when WeightVal >= 500 ->
                ltl;
            %% Long distance shipments - FTL
            {_, DistanceVal, _, _} when DistanceVal >= 1000 ->
                ftl;

            %% Premium customers get better rates
            {_, _, Amount, premium} when Amount >= FtlThreshold * 0.8 ->
                ftl;
            {_, _, Amount, premium} when Amount >= LtlThreshold * 0.8 ->
                ltl;
            {_, _, _, premium} ->
                sp;

            %% Standard tier
            {_, _, Amount, standard} when Amount >= FtlThreshold ->
                ftl;
            {_, _, Amount, standard} when Amount >= LtlThreshold ->
                ltl;
            {_, _, _, standard} ->
                sp;

            %% Default
            _ ->
                sp
        end,

        {ok, Method}
    catch
        error:{badkey, Key} ->
            {error, {missing_key, Key}};
        _:_ ->
            {error, invalid_data}
    end.

%%====================================================================
%% API Functions - Inventory Management
%% (From yawl_orderfulfillment_handlers)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Allocates inventory for order items.
%%
%% Checks inventory availability and reserves items for the order.
%% Returns allocation details or error if items are out of stock.
%%
%% @param OrderData Map containing order items
%% @returns {ok, Allocated} or {error, out_of_stock}
%%
%% @end
%%--------------------------------------------------------------------
-spec allocate_inventory(OrderData :: order_data()) ->
    {ok, Allocated :: allocated_inventory()} | handler_error().

allocate_inventory(OrderData) ->
    Items = maps:get(items, OrderData, []),
    allocate_items_loop(Items, #{}).

%% @private
-spec allocate_items_loop(list(), allocated_inventory()) ->
    {ok, allocated_inventory()} | handler_error().

allocate_items_loop([], Allocated) ->
    {ok, #{allocated => Allocated}};
allocate_items_loop([{Sku, Quantity} | Rest], Allocated) ->
    case check_stock_available(Sku, Quantity) of
        true ->
            allocate_items_loop(Rest, Allocated#{Sku => Quantity});
        false ->
            {error, {out_of_stock, Sku}}
    end;
allocate_items_loop([Sku | Rest], Allocated) when is_atom(Sku); is_binary(Sku) ->
    %% Default quantity of 1
    case check_stock_available(Sku, 1) of
        true ->
            allocate_items_loop(Rest, Allocated#{Sku => 1});
        false ->
            {error, {out_of_stock, Sku}}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if inventory is available for order items.
%%
%% Performs availability check without reserving inventory.
%%
%% @param OrderData Map containing order items
%% @returns true if all items available, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec check_inventory(OrderData :: order_data()) -> boolean().

check_inventory(OrderData) ->
    Items = maps:get(items, OrderData, []),
    check_items_available(Items).

%% @private
-spec check_items_available(list()) -> boolean().

check_items_available([]) ->
    true;
check_items_available([{Sku, Quantity} | Rest]) ->
    case check_stock_available(Sku, Quantity) of
        true -> check_items_available(Rest);
        false -> false
    end;
check_items_available([Sku | Rest]) when is_atom(Sku); is_binary(Sku) ->
    case check_stock_available(Sku, 1) of
        true -> check_items_available(Rest);
        false -> false
    end.

%% @private
%% Mock inventory check - in production, this would query a database
-spec check_stock_available(Sku :: term(), Quantity :: number()) -> boolean().

check_stock_available(_Sku, _Quantity) ->
    %% Mock: Always return true for demo purposes
    %% In production, query inventory database
    true.

%%====================================================================
%% API Functions - Shipping Coordination
%% (From yawl_orderfulfillment_handlers)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Selects carrier based on shipping method and order data.
%%
%% Chooses appropriate carrier from available options based on:
%% - Shipping method (FTL/LTL/SP)
%% - Distance (regional vs national)
%% - Service level requirements
%%
%% @param ShippingMethod The shipping method (ftl, ltl, or sp)
%% @param OrderData Order context for carrier selection
%% @returns {ok, Carrier} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec select_carrier(ShippingMethod :: payment_method(), OrderData :: order_data()) ->
    {ok, Carrier :: carrier()} | handler_error().

select_carrier(ftl, OrderData) ->
    %% FTL carriers for freight
    Distance = maps:get(distance, OrderData, 0),
    case Distance of
        Dist when Dist >= 1500 ->
            {ok, xpo};  %% Long haul
        Dist when Dist >= 500 ->
            {ok, estes};  %% Regional to national
        _ ->
            {ok, fedex_freight}  %% Short haul / priority
    end;

select_carrier(ltl, OrderData) ->
    %% LTL carriers for less-than-truckload
    Distance = maps:get(distance, OrderData, 0),
    Urgency = maps:get(urgency, OrderData, normal),

    case {Distance, Urgency} of
        {_, urgent} ->
            {ok, fedex_freight};
        {Dist, _} when Dist >= 1000 ->
            {ok, ups_freight};
        _ ->
            {ok, estes}
    end;

select_carrier(sp, OrderData) ->
    %% Small parcel carriers
    Distance = maps:get(distance, OrderData, 0),
    International = maps:get(international, OrderData, false),

    case International of
        true ->
            {ok, dhl};
        false when Distance >= 1000 ->
            {ok, fedex};
        false when Distance >= 500 ->
            {ok, ups};
        _ ->
            {ok, ontrac}  %% Regional
    end.

%%--------------------------------------------------------------------
%% @doc Calculates shipping cost based on order data and carrier.
%%
%% Estimates shipping cost using weight, distance, and carrier rates.
%% Returns base shipping cost before any discounts.
%%
%% @param OrderData Map containing order weight, distance, etc.
%% @param Carrier Selected carrier for rate calculation
%% @returns {ok, Cost} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_shipping(OrderData :: order_data(), Carrier :: carrier()) ->
    {ok, Cost :: number()} | handler_error().

calculate_shipping(OrderData, Carrier) ->
    try
        Weight = maps:get(weight, OrderData, 0),
        Distance = maps:get(distance, OrderData, 0),

        %% Base rate calculation
        BaseRate = get_base_rate(Carrier),
        WeightFactor = get_weight_factor(Carrier),
        DistanceFactor = get_distance_factor(Carrier),

        Cost = BaseRate + (Weight * WeightFactor) + (Distance * DistanceFactor),

        {ok, Cost}
    catch
        error:{badkey, _} ->
            {error, missing_order_data};
        _:_ ->
            {error, calculation_failed}
    end.

%%====================================================================
%% API Functions - Credit Card Processing
%% (From yawl_payment_handlers)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Processes a credit card payment.
%%
%% Validates payment information and processes the transaction.
%% Returns a payment result with status, transaction ID, and details.
%%
%% @param PaymentInfo Map containing card and payment details
%% @param OrderData Order context for the payment
%% @returns {ok, PaymentResult} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec process_credit_card(PaymentInfo :: payment_info(),
                          OrderData :: map()) ->
    {ok, payment_result()} | {error, term()}.

process_credit_card(PaymentInfo, OrderData) ->
    try
        %% Validate required fields
        CardNumber = maps:get(card_number, PaymentInfo),
        Amount = maps:get(amount, PaymentInfo),

        %% Validate card number format (Luhn check mock)
        case validate_card_number(CardNumber) of
            false ->
                {ok, create_payment_result(declined, invalid_card)};
            true ->
                %% Check amount
                case Amount of
                    Amt when Amt =< 0 ->
                        {error, invalid_amount};
                    _ ->
                        %% Process payment (mock)
                        process_payment_mock(PaymentInfo, OrderData)
                end
        end
    catch
        error:{badkey, Key} ->
            {error, {missing_field, Key}};
        _:_ ->
            {error, payment_processing_error}
    end.

%%--------------------------------------------------------------------
%% @doc Verifies payment was successful.
%%
%% Checks the payment result status and returns:
%% - true for approved payments
%% - false for declined/failed payments
%% - {pending, Reference} for pending payments
%%
%% @param PaymentResult Result from payment processing
%% @returns true, false, or {pending, Reference}
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_payment(PaymentResult :: payment_result()) ->
    boolean() | {pending, binary()}.

verify_payment(#{status := approved}) ->
    true;
verify_payment(#{status := pending, transaction_id := TxnId}) ->
    {pending, TxnId};
verify_payment(#{status := declined}) ->
    false;
verify_payment(#{status := failed}) ->
    false;
verify_payment(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Handles payment failure with appropriate action.
%%
%% Determines the appropriate action based on failure reason:
%% - insufficient_funds: retry
%% - declined: retry
%% - expired: contact_customer
%% - invalid_card: abort
%% - timeout: retry
%% - processing_error: retry
%% - fraud_detected: manual_review
%% - card_limit_exceeded: retry
%%
%% @param Reason The payment failure reason
%% @param OrderData Order context for decision making
%% @returns {ok, Action} indicating how to proceed
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_payment_failure(Reason :: failure_reason(),
                             OrderData :: map()) ->
    {ok, failure_action()}.

handle_payment_failure(insufficient_funds, _OrderData) ->
    {ok, retry};
handle_payment_failure(declined, _OrderData) ->
    {ok, retry};
handle_payment_failure(expired, _OrderData) ->
    {ok, contact_customer};
handle_payment_failure(invalid_card, _OrderData) ->
    {ok, abort};
handle_payment_failure(timeout, _OrderData) ->
    {ok, retry};
handle_payment_failure(processing_error, _OrderData) ->
    {ok, retry};
handle_payment_failure(fraud_detected, _OrderData) ->
    {ok, manual_review};
handle_payment_failure(card_limit_exceeded, _OrderData) ->
    {ok, retry};
handle_payment_failure(_Unknown, _OrderData) ->
    {ok, retry}.

%%--------------------------------------------------------------------
%% @doc Checks if payment has timed out.
%%
%% Returns true if more than 30 seconds have elapsed since start time.
%%
%% @param StartTime Timestamp in milliseconds (erlang:system_time(millisecond))
%% @returns true if timed out, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec check_payment_timeout(StartTime :: integer()) -> boolean().

check_payment_timeout(StartTime) when is_integer(StartTime) ->
    CurrentTime = erlang:system_time(millisecond),
    Timeout = maps:get(payment_timeout_ms, get_payment_config(), 30000),
    (CurrentTime - StartTime) > Timeout;
check_payment_timeout(_) ->
    true.

%%====================================================================
%% API Functions - Payment Branch
%% (From yawl_payment_branch)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Determines the appropriate shipping branch for an order.
%%
%% Evaluates order data against configured thresholds to select
%% between FTL, LTL, or Small Parcel shipping methods.
%%
%% Priority rules:
%% 1. FTL: volume > FTL threshold OR package_count > 20
%% 2. LTL: volume > LTL threshold AND volume <= FTL threshold
%% 3. SP: volume <= SP threshold (default: same as LTL threshold)
%%
%% @param OrderData Map containing volume and package_count
%% @returns {ok, ftl | ltl | sp} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec determine_branch(order_data()) -> branch_result() | branch_error().

determine_branch(OrderData) when is_map(OrderData) ->
    %% Extract volume with fallback to weight key
    Volume = maps:get(volume, OrderData, maps:get(weight, OrderData, undefined)),

    %% Validate and process volume
    if
        Volume =:= undefined ->
            {error, missing_volume};
        not is_number(Volume) ->
            {error, invalid_volume};
        Volume < 0 ->
            {error, invalid_volume};
        true ->
            %% Extract package_count with fallback to item_count key
            PackageCount = maps:get(package_count, OrderData,
                                   maps:get(item_count, OrderData, 0)),

            %% Get thresholds
            Thresholds = get_thresholds(),
            FtlThreshold = maps:get(ftl, Thresholds, ?DEFAULT_THRESHOLDS),
            LtlThreshold = maps:get(ltl, Thresholds, ?DEFAULT_THRESHOLDS),

            %% Apply branching logic
            Method = if
                %% FTL: Very high volume OR many packages
                Volume > FtlThreshold; PackageCount > 20 ->
                    ftl;
                %% LTL: Medium volume
                Volume > LtlThreshold, Volume =< FtlThreshold ->
                    ltl;
                %% SP: Small volume
                Volume =< LtlThreshold ->
                    sp
            end,

            {ok, Method}
    end;

determine_branch(_) ->
    {error, invalid_data}.

%%--------------------------------------------------------------------
%% @doc Checks if order qualifies for FTL shipping.
%%
%% Returns true if volume > FTL threshold OR package_count > 20.
%%
%% @param OrderData Map containing volume and package_count
%% @returns true if FTL, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_ftl(order_data()) -> boolean().

is_ftl(OrderData) ->
    case determine_branch(OrderData) of
        {ok, ftl} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Checks if order qualifies for LTL shipping.
%%
%% Returns true if volume is between LTL and FTL thresholds.
%%
%% @param OrderData Map containing volume and package_count
%% @returns true if LTL, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_ltl(order_data()) -> boolean().

is_ltl(OrderData) ->
    case determine_branch(OrderData) of
        {ok, ltl} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Checks if order qualifies for Small Parcel shipping.
%%
%% Returns true if volume <= LTL threshold.
%%
%% @param OrderData Map containing volume and package_count
%% @returns true if SP, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_sp(order_data()) -> boolean().

is_sp(OrderData) ->
    case determine_branch(OrderData) of
        {ok, sp} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current threshold configuration.
%%
%% Returns the map of threshold values for branch determination.
%% If thresholds have been customized via set_thresholds/1, returns
%% the custom values. Otherwise returns default thresholds.
%%
%% @returns Map of ftl, ltl, and sp thresholds
%%
%% @end
%%--------------------------------------------------------------------
-spec get_thresholds() -> thresholds().

get_thresholds() ->
    try
        case ets:whereis(?THRESHOLD_TABLE) of
            undefined ->
                %% Table doesn't exist, return defaults
                ?DEFAULT_THRESHOLDS;
            _ ->
                [{thresholds, Thresholds}] = ets:lookup(?THRESHOLD_TABLE, thresholds),
                Thresholds
        end
    catch
        _:_ ->
            ?DEFAULT_THRESHOLDS
    end.

%%--------------------------------------------------------------------
%% @doc Sets custom threshold configuration.
%%
%% Updates the threshold values used for branch determination.
%% The changes persist until reset_thresholds/0 is called or
%% the application restarts.
%%
%% Only the provided keys are updated; missing keys retain
%% their current values.
%%
%% @param Thresholds Map with ftl, ltl, and/or sp keys
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec set_thresholds(map()) -> ok.

set_thresholds(Thresholds) when is_map(Thresholds) ->
    %% Ensure ETS table exists
    case ets:whereis(?THRESHOLD_TABLE) of
        undefined ->
            ets:new(?THRESHOLD_TABLE, [named_table, public, {read_concurrency, true}]);
        _ ->
            ok
    end,

    %% Merge with existing thresholds
    Current = get_thresholds(),
    Merged = maps:merge(Current, Thresholds),

    %% Validate thresholds are positive numbers
    maps:foreach(fun
        (_Key, Value) when is_number(Value), Value > 0 -> ok;
        (_Key, _Value) -> error(invalid_threshold)
    end, Merged),

    %% Store merged thresholds
    ets:insert(?THRESHOLD_TABLE, {thresholds, Merged}),
    ok.

%%--------------------------------------------------------------------
%% @doc Resets thresholds to default values.
%%
%% Clears any custom threshold configuration and restores
%% the default values:
%% - ftl: 5000
%% - ltl: 500
%% - sp: 500
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_thresholds() -> ok.

reset_thresholds() ->
    case ets:whereis(?THRESHOLD_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete(?THRESHOLD_TABLE),
            ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Gets base rate for carrier.
-spec get_base_rate(carrier()) -> number().

get_base_rate(fedex_freight) -> 150.00;
get_base_rate(ups_freight) -> 140.00;
get_base_rate(estes) -> 120.00;
get_base_rate(xpo) -> 130.00;
get_base_rate(fedex) -> 15.00;
get_base_rate(ups) -> 14.00;
get_base_rate(dhl) -> 20.00;
get_base_rate(usps) -> 8.00;
get_base_rate(ontrac) -> 10.00;
get_base_rate(_) -> 15.00.

%% @private
%% @doc Gets weight-based rate factor for carrier.
-spec get_weight_factor(carrier()) -> number().

get_weight_factor(fedex_freight) -> 0.05;
get_weight_factor(ups_freight) -> 0.045;
get_weight_factor(estes) -> 0.04;
get_weight_factor(xpo) -> 0.042;
get_weight_factor(fedex) -> 0.10;
get_weight_factor(ups) -> 0.095;
get_weight_factor(dhl) -> 0.15;
get_weight_factor(usps) -> 0.08;
get_weight_factor(ontrac) -> 0.07;
get_weight_factor(_) -> 0.10.

%% @private
%% @doc Gets distance-based rate factor for carrier.
-spec get_distance_factor(carrier()) -> number().

get_distance_factor(fedex_freight) -> 0.20;
get_distance_factor(ups_freight) -> 0.18;
get_distance_factor(estes) -> 0.15;
get_distance_factor(xpo) -> 0.16;
get_distance_factor(fedex) -> 0.03;
get_distance_factor(ups) -> 0.028;
get_distance_factor(dhl) -> 0.05;
get_distance_factor(usps) -> 0.02;
get_distance_factor(ontrac) -> 0.015;
get_distance_factor(_) -> 0.03.

%% @private
%% @doc Mock payment processing with simulated outcomes.
-spec process_payment_mock(payment_info(), map()) ->
    {ok, payment_result()}.

process_payment_mock(PaymentInfo, OrderData) ->
    %% Mock payment processing with deterministic simulation
    Amount = maps:get(amount, PaymentInfo),

    %% Simulate different outcomes based on amount
    Status = case Amount of
        Amt when Amt > 1000000 ->
            declined;  %% Very large amounts declined
        Amt when Amt rem 7 =:= 0 ->
            pending;  %% Every 7th payment goes to pending
        Amt when Amt < 0 ->
            declined;
        _ ->
            approved  %% Most payments approved
    end,

    %% Generate mock transaction ID
    TxnId = generate_transaction_id(OrderData),

    Result = create_payment_result(Status, TxnId),
    {ok, Result}.

%% @private
%% @doc Creates a payment result map.
-spec create_payment_result(Status :: atom(), TxnId :: binary()) -> payment_result().

create_payment_result(Status, TxnId) ->
    #{
        status => Status,
        transaction_id => TxnId,
        auth_code => case Status of
            approved -> generate_auth_code();
            _ -> undefined
        end,
        message => status_message(Status),
        timestamp => erlang:system_time(millisecond)
    }.

%% @private
%% @doc Generates a mock transaction ID.
-spec generate_transaction_id(map()) -> binary().

generate_transaction_id(OrderData) ->
    OrderId = maps:get(order_id, OrderData, "unknown"),
    Timestamp = erlang:system_time(millisecond),
    <<(list_to_binary([OrderId, "-", integer_to_binary(Timestamp)]))/binary,
      (crypto:strong_rand_bytes(4))/binary>>.

%% @private
%% @doc Generates a mock authorization code.
-spec generate_auth_code() -> binary().

generate_auth_code() ->
    Rand = 100000 + rand:uniform(899999),  %% Generate 6-digit number from 100000 to 999999
    list_to_binary(integer_to_list(Rand)).

%% @private
%% @doc Returns status message for payment result.
-spec status_message(atom()) -> binary().

status_message(approved) -> <<"Payment approved">>;
status_message(declined) -> <<"Payment declined">>;
status_message(pending) -> <<"Payment pending review">>;
status_message(failed) -> <<"Payment processing failed">>;
status_message(_) -> <<"Unknown status">>.

%% @private
%% @doc Validates card number using Luhn algorithm (simplified mock).
-spec validate_card_number(binary() | string()) -> boolean().

validate_card_number(CardNum) when is_binary(CardNum) ->
    validate_card_number(binary_to_list(CardNum));
validate_card_number(CardNum) when is_list(CardNum) ->
    %% Remove spaces and dashes
    Cleaned = [C || C <- CardNum, C >= $0, C =< $9],
    Length = length(Cleaned),
    case Length of
        N when N < 13; N > 19 -> false;
        _ ->
            %% Simplified Luhn check - in production use full algorithm
            %% For mock, just check basic pattern
            lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Cleaned)
    end.

%% @private
%% @doc Gets payment configuration.
-spec get_payment_config() -> map().

get_payment_config() ->
    #{
        payment_timeout_ms => 30000,
        max_retry_attempts => 3,
        pending_check_interval_ms => 5000
    }.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests from the moduledoc and function documentation.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%%====================================================================
%% Tests from yawl_orderfulfillment_handlers
%%====================================================================

%% Test payment method determination
determine_payment_method_ftl_test() ->
    OrderData = #{total_amount => 15000, weight => 100, distance => 100},
    Rules = #{ftl_threshold => 10000, ltl_threshold => 1000},
    ?assertEqual({ok, ftl}, determine_payment_method(OrderData, Rules)).

determine_payment_method_ltl_test() ->
    OrderData = #{total_amount => 5000, weight => 100, distance => 100},
    Rules = #{ftl_threshold => 10000, ltl_threshold => 1000},
    ?assertEqual({ok, ltl}, determine_payment_method(OrderData, Rules)).

determine_payment_method_sp_test() ->
    OrderData = #{total_amount => 500, weight => 10, distance => 50},
    Rules = #{ftl_threshold => 10000, ltl_threshold => 1000},
    ?assertEqual({ok, sp}, determine_payment_method(OrderData, Rules)).

determine_payment_method_premium_test() ->
    OrderData = #{total_amount => 8500, weight => 100, distance => 100,
                  customer_tier => premium},
    Rules = #{ftl_threshold => 10000, ltl_threshold => 1000},
    ?assertEqual({ok, ftl}, determine_payment_method(OrderData, Rules)).

determine_payment_method_heavy_test() ->
    OrderData = #{total_amount => 500, weight => 6000, distance => 100},
    Rules = #{ftl_threshold => 10000, ltl_threshold => 1000},
    ?assertEqual({ok, ftl}, determine_payment_method(OrderData, Rules)).

determine_payment_method_long_distance_test() ->
    OrderData = #{total_amount => 5000, weight => 100, distance => 1500},
    Rules = #{ftl_threshold => 10000, ltl_threshold => 1000},
    ?assertEqual({ok, ftl}, determine_payment_method(OrderData, Rules)).

%% Test inventory allocation
allocate_inventory_success_test() ->
    OrderData = #{items => [{sku123, 5}, {sku456, 2}]},
    ?assertMatch({ok, #{allocated := _}}, allocate_inventory(OrderData)).

allocate_inventory_empty_test() ->
    OrderData = #{items => []},
    ?assertEqual({ok, #{allocated => #{}}}, allocate_inventory(OrderData)).

allocate_inventory_single_item_test() ->
    OrderData = #{items => [sku123]},
    ?assertMatch({ok, #{allocated := #{sku123 := 1}}}, allocate_inventory(OrderData)).

%% Test inventory checking
check_inventory_available_test() ->
    OrderData = #{items => [{sku123, 5}, {sku456, 2}]},
    ?assertEqual(true, check_inventory(OrderData)).

check_inventory_empty_test() ->
    OrderData = #{items => []},
    ?assertEqual(true, check_inventory(OrderData)).

%% Test carrier selection
select_carrier_ftl_short_test() ->
    OrderData = #{distance => 200},
    ?assertEqual({ok, fedex_freight}, select_carrier(ftl, OrderData)).

select_carrier_ftl_medium_test() ->
    OrderData = #{distance => 600},
    ?assertEqual({ok, estes}, select_carrier(ftl, OrderData)).

select_carrier_ftl_long_test() ->
    OrderData = #{distance => 2000},
    ?assertEqual({ok, xpo}, select_carrier(ftl, OrderData)).

select_carrier_ltl_urgent_test() ->
    OrderData = #{distance => 200, urgency => urgent},
    ?assertEqual({ok, fedex_freight}, select_carrier(ltl, OrderData)).

select_carrier_sp_domestic_test() ->
    OrderData = #{distance => 200, international => false},
    ?assertEqual({ok, ontrac}, select_carrier(sp, OrderData)).

select_carrier_sp_international_test() ->
    OrderData = #{distance => 200, international => true},
    ?assertEqual({ok, dhl}, select_carrier(sp, OrderData)).

%% Test shipping calculation
calculate_shipping_ftl_test() ->
    OrderData = #{weight => 1000, distance => 500},
    ?assertMatch({ok, Cost}, calculate_shipping(OrderData, fedex_freight)),
    {ok, Cost} = calculate_shipping(OrderData, fedex_freight),
    ?assert(Cost > 150).  %% Should be above base rate

calculate_shipping_sp_test() ->
    OrderData = #{weight => 10, distance => 100},
    ?assertMatch({ok, Cost}, calculate_shipping(OrderData, fedex)),
    {ok, Cost} = calculate_shipping(OrderData, fedex),
    ?assert(Cost > 15).  %% Should be above base rate

calculate_shipping_base_rate_test() ->
    OrderData = #{weight => 0, distance => 0},
    {ok, Cost} = calculate_shipping(OrderData, usps),
    ?assertEqual(8.00, Cost).

%%====================================================================
%% Tests from yawl_payment_handlers
%%====================================================================

%% Test credit card processing
process_credit_card_approved_test() ->
    PaymentInfo = #{
        card_number => "4111111111111111",
        expiry => "12/25",
        cvv => "123",
        amount => 10000
    },
    OrderData = #{order_id => "ORD-12345"},
    ?assertMatch({ok, #{status := approved}}, process_credit_card(PaymentInfo, OrderData)).

process_credit_card_invalid_card_test() ->
    PaymentInfo = #{
        card_number => "invalid",
        expiry => "12/25",
        cvv => "123",
        amount => 10000
    },
    OrderData = #{order_id => "ORD-12345"},
    ?assertMatch({ok, #{status := declined}}, process_credit_card(PaymentInfo, OrderData)).

process_credit_card_missing_field_test() ->
    PaymentInfo = #{
        card_number => "4111111111111111"
        %% Missing amount
    },
    OrderData = #{order_id => "ORD-12345"},
    ?assertEqual({error, {missing_field, amount}}, process_credit_card(PaymentInfo, OrderData)).

process_credit_card_negative_amount_test() ->
    PaymentInfo = #{
        card_number => "4111111111111111",
        expiry => "12/25",
        cvv => "123",
        amount => -100
    },
    OrderData = #{order_id => "ORD-12345"},
    ?assertEqual({error, invalid_amount}, process_credit_card(PaymentInfo, OrderData)).

process_credit_card_large_amount_test() ->
    PaymentInfo = #{
        card_number => "4111111111111111",
        expiry => "12/25",
        cvv => "123",
        amount => 2000000
    },
    OrderData = #{order_id => "ORD-12345"},
    ?assertMatch({ok, #{status := declined}}, process_credit_card(PaymentInfo, OrderData)).

process_credit_card_pending_test() ->
    PaymentInfo = #{
        card_number => "4111111111111111",
        expiry => "12/25",
        cvv => "123",
        amount => 7000  %% Divisible by 7
    },
    OrderData = #{order_id => "ORD-12345"},
    ?assertMatch({ok, #{status := pending}}, process_credit_card(PaymentInfo, OrderData)).

%% Test payment verification
verify_payment_approved_test() ->
    Result = #{status => approved, transaction_id => <<"TXN-123">>},
    ?assertEqual(true, verify_payment(Result)).

verify_payment_pending_test() ->
    Result = #{status => pending, transaction_id => <<"TXN-456">>},
    ?assertEqual({pending, <<"TXN-456">>}, verify_payment(Result)).

verify_payment_declined_test() ->
    Result = #{status => declined, transaction_id => <<"TXN-789">>},
    ?assertEqual(false, verify_payment(Result)).

verify_payment_failed_test() ->
    Result = #{status => failed, transaction_id => <<"TXN-000">>},
    ?assertEqual(false, verify_payment(Result)).

verify_payment_unknown_status_test() ->
    Result = #{status => unknown, transaction_id => <<"TXN-111">>},
    ?assertEqual(false, verify_payment(Result)).

%% Test failure handling
handle_payment_failure_insufficient_funds_test() ->
    ?assertEqual({ok, retry}, handle_payment_failure(insufficient_funds, #{})).

handle_payment_failure_declined_test() ->
    ?assertEqual({ok, retry}, handle_payment_failure(declined, #{})).

handle_payment_failure_expired_test() ->
    ?assertEqual({ok, contact_customer}, handle_payment_failure(expired, #{})).

handle_payment_failure_invalid_card_test() ->
    ?assertEqual({ok, abort}, handle_payment_failure(invalid_card, #{})).

handle_payment_failure_timeout_test() ->
    ?assertEqual({ok, retry}, handle_payment_failure(timeout, #{})).

handle_payment_failure_fraud_detected_test() ->
    ?assertEqual({ok, manual_review}, handle_payment_failure(fraud_detected, #{})).

%% Test timeout handling
check_payment_timeout_not_expired_test() ->
    StartTime = erlang:system_time(millisecond) - 10000,  %% 10 seconds ago
    ?assertEqual(false, check_payment_timeout(StartTime)).

check_payment_timeout_expired_test() ->
    StartTime = erlang:system_time(millisecond) - 35000,  %% 35 seconds ago
    ?assertEqual(true, check_payment_timeout(StartTime)).

check_payment_timeout_boundary_test() ->
    StartTime = erlang:system_time(millisecond) - 30000,  %% Exactly 30 seconds
    ?assertEqual(false, check_payment_timeout(StartTime)).

check_payment_timeout_invalid_input_test() ->
    ?assertEqual(true, check_payment_timeout(invalid)).

%% Test card validation
validate_card_number_valid_test() ->
    ?assertEqual(true, validate_card_number("4111111111111111")).

validate_card_number_with_spaces_test() ->
    ?assertEqual(true, validate_card_number("4111 1111 1111 1111")).

validate_card_number_with_dashes_test() ->
    ?assertEqual(true, validate_card_number("4111-1111-1111-1111")).

validate_card_number_too_short_test() ->
    ?assertEqual(false, validate_card_number("123")).

validate_card_number_too_long_test() ->
    ?assertEqual(false, validate_card_number(lists:duplicate(25, $1))).

validate_card_number_invalid_chars_test() ->
    ?assertEqual(false, validate_card_number("abcd-efgh-ijkl-mnop")).

%%====================================================================
%% Tests from yawl_payment_branch
%%====================================================================

%% Test branch determination - FTL
determine_branch_ftl_by_volume_test() ->
    OrderData = #{volume => 6000, package_count => 5},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_ftl_by_volume_exactly_at_threshold_test() ->
    OrderData = #{volume => 5001, package_count => 5},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_ftl_by_package_count_test() ->
    OrderData = #{volume => 1000, package_count => 25},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_ftl_by_package_count_at_threshold_test() ->
    OrderData = #{volume => 1000, package_count => 21},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_ftl_both_conditions_test() ->
    OrderData = #{volume => 10000, package_count => 50},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

%% Test branch determination - LTL
determine_branch_ltl_mid_range_test() ->
    OrderData = #{volume => 2000, package_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

determine_branch_ltl_just_above_threshold_test() ->
    OrderData = #{volume => 501, package_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

determine_branch_ltl_just_below_ftl_threshold_test() ->
    OrderData = #{volume => 4999, package_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

determine_branch_ltl_at_exactly_ltl_threshold_test() ->
    OrderData = #{volume => 500, package_count => 5},
    %% At exactly 500, should be SP (<= goes to SP)
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

%% Test branch determination - SP
determine_branch_sp_small_volume_test() ->
    OrderData = #{volume => 250, package_count => 2},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

determine_branch_sp_at_threshold_test() ->
    OrderData = #{volume => 500, package_count => 2},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

determine_branch_sp_zero_volume_test() ->
    OrderData = #{volume => 0, package_count => 1},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

determine_branch_sp_below_threshold_test() ->
    OrderData = #{volume => 499, package_count => 2},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

%% Test branch predicates
is_ftl_true_test() ->
    OrderData = #{volume => 6000, package_count => 5},
    ?assertEqual(true, is_ftl(OrderData)).

is_ftl_false_test() ->
    OrderData = #{volume => 100, package_count => 2},
    ?assertEqual(false, is_ftl(OrderData)).

is_ltl_true_test() ->
    OrderData = #{volume => 2000, package_count => 5},
    ?assertEqual(true, is_ltl(OrderData)).

is_ltl_false_test() ->
    OrderData = #{volume => 100, package_count => 2},
    ?assertEqual(false, is_ltl(OrderData)).

is_sp_true_test() ->
    OrderData = #{volume => 100, package_count => 2},
    ?assertEqual(true, is_sp(OrderData)).

is_sp_false_test() ->
    OrderData = #{volume => 6000, package_count => 5},
    ?assertEqual(false, is_sp(OrderData)).

%% Test error handling
determine_branch_missing_volume_test() ->
    OrderData = #{package_count => 5},
    ?assertEqual({error, missing_volume}, determine_branch(OrderData)).

determine_branch_invalid_volume_test() ->
    OrderData = #{volume => invalid, package_count => 5},
    ?assertEqual({error, invalid_volume}, determine_branch(OrderData)).

determine_branch_negative_volume_test() ->
    OrderData = #{volume => -100, package_count => 5},
    ?assertEqual({error, invalid_volume}, determine_branch(OrderData)).

determine_branch_invalid_input_test() ->
    ?assertEqual({error, invalid_data}, determine_branch(not_a_map)).

determine_branch_empty_map_test() ->
    ?assertEqual({error, missing_volume}, determine_branch(#{})).

%% Test aliases - weight and item_count
determine_branch_weight_alias_test() ->
    OrderData = #{weight => 6000, package_count => 5},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_item_count_alias_test() ->
    OrderData = #{volume => 1000, item_count => 25},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_both_aliases_test() ->
    OrderData = #{weight => 2000, item_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

%% Volume key takes precedence over weight key
determine_branch_volume_precedence_test() ->
    OrderData = #{volume => 100, weight => 6000, package_count => 5},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

%% package_count key takes precedence over item_count key
determine_branch_package_count_precedence_test() ->
    OrderData = #{volume => 1000, package_count => 5, item_count => 25},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

%% Test threshold configuration
get_thresholds_defaults_test() ->
    Thresholds = get_thresholds(),
    ?assertEqual(5000, maps:get(ftl, Thresholds)),
    ?assertEqual(500, maps:get(ltl, Thresholds)),
    ?assertEqual(500, maps:get(sp, Thresholds)).

set_thresholds_ftl_test() ->
    ok = set_thresholds(#{ftl => 10000}),
    Thresholds = get_thresholds(),
    ?assertEqual(10000, maps:get(ftl, Thresholds)),
    ?assertEqual(500, maps:get(ltl, Thresholds)),  %% Unchanged
    reset_thresholds().

set_thresholds_all_test() ->
    ok = set_thresholds(#{ftl => 10000, ltl => 1000, sp => 1000}),
    Thresholds = get_thresholds(),
    ?assertEqual(10000, maps:get(ftl, Thresholds)),
    ?assertEqual(1000, maps:get(ltl, Thresholds)),
    ?assertEqual(1000, maps:get(sp, Thresholds)),
    reset_thresholds().

set_thresholds_branch_behavior_test() ->
    %% Set custom thresholds
    ok = set_thresholds(#{ftl => 1000, ltl => 100, sp => 100}),

    %% Now 800 should be LTL (was SP with defaults)
    OrderData = #{volume => 800, package_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)),

    %% And 1200 should be FTL
    OrderData2 = #{volume => 1200, package_count => 5},
    ?assertEqual({ok, ftl}, determine_branch(OrderData2)),

    %% And 50 should still be SP
    OrderData3 = #{volume => 50, package_count => 2},
    ?assertEqual({ok, sp}, determine_branch(OrderData3)),

    reset_thresholds().

reset_thresholds_test() ->
    %% Set custom thresholds
    ok = set_thresholds(#{ftl => 10000}),

    %% Reset
    ok = reset_thresholds(),

    %% Verify defaults restored
    Thresholds = get_thresholds(),
    ?assertEqual(5000, maps:get(ftl, Thresholds)),
    ?assertEqual(500, maps:get(ltl, Thresholds)).

set_thresholds_multiple_calls_test() ->
    ok = set_thresholds(#{ftl => 8000}),
    ok = set_thresholds(#{ltl => 750}),
    ok = set_thresholds(#{sp => 750}),

    Thresholds = get_thresholds(),
    ?assertEqual(8000, maps:get(ftl, Thresholds)),
    ?assertEqual(750, maps:get(ltl, Thresholds)),
    ?assertEqual(750, maps:get(sp, Thresholds)),

    reset_thresholds().

%% Test edge cases
determine_branch_boundary_ftl_ltl_test() ->
    %% At exactly 5000, should be LTL (not > 5000)
    OrderData = #{volume => 5000, package_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

determine_branch_boundary_ltl_sp_test() ->
    %% At exactly 500, should be SP (<= 500)
    OrderData = #{volume => 500, package_count => 5},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

determine_branch_package_count_exactly_20_test() ->
    %% At exactly 20 packages, should NOT be FTL
    OrderData = #{volume => 1000, package_count => 20},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

determine_branch_small_volume_many_packages_test() ->
    %% Low volume but >20 packages -> FTL
    OrderData = #{volume => 100, package_count => 21},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_large_volume_few_packages_test() ->
    %% High volume but few packages -> still FTL by volume
    OrderData = #{volume => 6000, package_count => 2},
    ?assertEqual({ok, ftl}, determine_branch(OrderData)).

determine_branch_float_volume_test() ->
    OrderData = #{volume => 2500.5, package_count => 5},
    ?assertEqual({ok, ltl}, determine_branch(OrderData)).

determine_branch_zero_package_count_test() ->
    OrderData = #{volume => 100, package_count => 0},
    ?assertEqual({ok, sp}, determine_branch(OrderData)).

-endif.
