# Handler Development Guide

**Business Logic Integration with YAWL Workflows**

## Overview

This guide covers the development of business logic handlers that integrate with YAWL workflows. Handlers provide the bridge between workflow transitions and real business operations, enabling workflows to interact with external systems, databases, and services.

## Handler Architecture

### Separation of Concerns

Handler development follows the principle of separation:
- **Workflow Definition**: YAWL specifies the process flow
- **Business Logic**: Handlers implement the actual work
- **External Integration**: Handlers interface with external systems

### Handler Types

1. **Task Handlers**: Handle external task completion
2. **Business Rule Handlers**: Implement decision logic
3. **Event Handlers**: Handle workflow events
4. **Data Handlers**: Manage data transformations

## Handler Development Basics

### Basic Handler Template

```erlang
-module(example_handler).
-export([handle_task/3, handle_event/3, handle_result/3]).

% Handle task execution
handle_task(TaskId, Context, UsrInfo) ->
    case TaskId of
        approval_task ->
            handle_approval(Context, UsrInfo);
        payment_task ->
            handle_payment(Context, UsrInfo);
        _ ->
            {error, unknown_task}
    end.

% Handle workflow events
handle_event(EventType, Data, UsrInfo) ->
    case EventType of
        workflow_start ->
            handle_workflow_start(Data, UsrInfo);
        workflow_complete ->
            handle_workflow_complete(Data, UsrInfo);
        _ ->
            ignore
    end.

% Handle task results
handle_result(TaskId, Result, UsrInfo) ->
    case TaskId of
        approval_task ->
            handle_approval_result(Result, UsrInfo);
        _ ->
            ok
    end.
```

### Handler Integration with YAWL

```erlang
% In YAWL workflow module
fire(Transition, Mode, UsrInfo) ->
    % Extract business context
    Context = extract_context(Mode, UsrInfo),

    % Call handler
    case handler:handle_task(Transition, Context, UsrInfo) of
        {ok, Result} ->
            {produce, update_context(Result, Context)};
        {error, Reason} ->
            handle_error(Reason, Context);
        retry ->
            {consume, Context}
    end.
```

## Order Fulfillment Handlers

### File: `test/yawl_of_helpers.erl`

This module provides test helper functions for the order fulfillment workflow pattern.

#### Handler Structure

```erlang
-module(yawl_of_helpers).

% Handler exports
-export([handle_order_received/3,
         handle_payment_processed/3,
         handle_inventory_checked/3,
         handle_order_shipped/3,
         handle_order_completed/3,
         handle_order_cancelled/3]).

% State management
-export([get_order_state/1, update_order_state/2]).

-include("gen_pnet.hrl").

% Order state record
-record(order_state, {
    order_id :: binary(),
    customer_id :: binary(),
    items :: list(),
    total_amount :: number(),
    payment_status :: pending | processed | failed,
    inventory_status :: pending | checked | insufficient,
    shipping_status :: pending | shipped,
    timestamps :: map()
}).
```

#### Order Received Handler

```erlang
%% @doc Handle order received event
-spec handle_order_received(context(), usr_info()) -> {ok, context()} | {error, term()}.
handle_order_received(Context, UsrInfo) ->
    OrderId = get_order_id(Context),
    CustomerId = get_customer_id(Context),
    Items = get_order_items(Context),
    TotalAmount = calculate_total(Items),

    % Validate order
    case validate_order(OrderId, CustomerId, Items) of
        ok ->
            % Create order state
            OrderState = #order_state{
                order_id = OrderId,
                customer_id = CustomerId,
                items = Items,
                total_amount = TotalAmount,
                payment_status = pending,
                inventory_status = pending,
                shipping_status = pending,
                timestamps = #{created => timestamp()}
            },

            % Save order state
            save_order_state(OrderId, OrderState),

            % Trigger next steps
            NextContext = Context#{
                order_id => OrderId,
                order_state => OrderState,
                next_steps => [payment_processing, inventory_check]
            },

            {ok, NextContext};

        {error, Reason} ->
            {error, {order_validation_failed, Reason}}
    end.

% Order validation
validate_order(OrderId, CustomerId, Items) ->
    % Check customer exists
    case customer_exists(CustomerId) of
        true ->
            % Check items are valid
            validate_items(Items);
        false ->
            {error, customer_not_found}
    end.

% Customer existence check
customer_exists(CustomerId) ->
    % Integration with customer database
    case customer_db:find(CustomerId) of
        {ok, _} -> true;
        {error, not_found} -> false
    end.

% Item validation
validate_items(Items) ->
    lists:all(fun(Item) -> validate_item(Item) end, Items).

validate_item(Item) ->
    case Item of
        #{sku := SKU, quantity := Quantity} when Quantity > 0 ->
            case product_db:find(SKU) of
                {ok, _} -> true;
                {error, not_found} -> false
            end;
        _ -> false
    end.
```

#### Payment Processing Handler

```erlang
%% @doc Handle payment processing
-spec handle_payment_processed(context(), usr_info()) -> {ok, context()} | {error, term()}.
handle_payment_processed(Context, UsrInfo) ->
    OrderId = get_order_id(Context),
    OrderState = get_order_state(OrderId),

    % Process payment
    PaymentResult = process_payment(OrderId, OrderState#order_state.total_amount),

    case PaymentResult of
        {ok, PaymentId} ->
            % Update order state
            UpdatedState = OrderState#order_state{
                payment_status = processed,
                timestamps = OrderState#order_state.timestamps#{
                    payment_processed => timestamp()
                }
            },

            save_order_state(OrderId, UpdatedState),

            % Continue with inventory check
            NextContext = Context#{
                payment_id => PaymentId,
                order_state => UpdatedState,
                inventory_check_required => true
            },

            {ok, NextContext};

        {error, Reason} ->
            % Handle payment failure
            ErrorState = OrderState#order_state{
                payment_status = failed,
                timestamps = OrderState#order_state.timestamps#{
                    payment_failed => timestamp(),
                    failure_reason => Reason
                }
            },

            save_order_state(OrderId, ErrorState),

            {error, {payment_failed, Reason}}
    end.

% Payment processing
process_payment(OrderId, Amount) ->
    % Integration with payment gateway
    PaymentGateway = payment_gateway:get(),
    case PaymentGateway:process(OrderId, Amount) of
        {ok, PaymentId} ->
            {ok, PaymentId};
        {error, Reason} ->
            {error, Reason}
    end.
```

#### Inventory Check Handler

```erlang
%% @doc Handle inventory checking
-spec handle_inventory_checked(context(), usr_info()) -> {ok, context()} | {error, term()}.
handle_inventory_checked(Context, UsrInfo) ->
    OrderId = get_order_id(Context),
    OrderState = get_order_state(OrderId),
    Items = OrderState#order_state.items,

    % Check inventory
    InventoryCheck = check_inventory(Items),

    case InventoryCheck of
        {sufficient, AvailableItems} ->
            % Update inventory
            update_inventory(Items),

            % Update order state
            UpdatedState = OrderState#order_state{
                inventory_status = checked,
                timestamps = OrderState#order_state.timestamps#{
                    inventory_checked => timestamp()
                }
            },

            save_order_state(OrderId, UpdatedState),

            % Ready for shipping
            NextContext = Context#{
                available_items => AvailableItems,
                order_state => UpdatedState,
                shipping_ready => true
            },

            {ok, NextContext};

        {insufficient, OutOfStockItems} ->
            % Handle insufficient inventory
            UpdatedState = OrderState#order_state{
                inventory_status = insufficient,
                timestamps = OrderState#order_state.timestamps#{
                    inventory_failed => timestamp(),
                    out_of_stock => OutOfStockItems
                }
            },

            save_order_state(OrderId, UpdatedState),

            {error, {insufficient_inventory, OutOfStockItems}}
    end.

% Inventory checking
check_inventory(Items) ->
    lists:foldl(fun(Item, {Sufficient, Available}) ->
        case Item of
            #{sku := SKU, quantity := Quantity} ->
                case inventory_db:check_stock(SKU, Quantity) of
                    {ok, AvailableQuantity} when AvailableQuantity >= Quantity ->
                        {Sufficient, [#{sku => SKU, available => AvailableQuantity} | Available]};
                    {ok, AvailableQuantity} when AvailableQuantity < Quantity ->
                        {insufficient, [#{sku => SKU, requested => Quantity, available => AvailableQuantity} | Available]}
                end
        end
    end, {sufficient, []}, Items).
```

#### Shipping Handler

```erlang
%% @doc Handle order shipping
-spec handle_order_shipped(context(), usr_info()) -> {ok, context()}.
handle_order_shipped(Context, UsrInfo) ->
    OrderId = get_order_id(Context),
    OrderState = get_order_state(OrderId),

    % Create shipping label
    ShippingLabel = create_shipping_label(OrderId, OrderState),

    % Update order state
    UpdatedState = OrderState#order_state{
        shipping_status = shipped,
        timestamps = OrderState#order_state.timestamps#{
            shipped => timestamp(),
            shipping_label => ShippingLabel
        }
    },

    save_order_state(OrderId, UpdatedState),

    % Send confirmation email
    send_shipping_confirmation(OrderId, ShippingLabel),

    {ok, Context#{
        order_state => UpdatedState,
        shipping_label => ShippingLabel
    }}.

% Shipping label creation
create_shipping_label(OrderId, OrderState) ->
    ShippingInfo = #{
        order_id => OrderId,
        customer_id => OrderState#order_state.customer_id,
        items => OrderState#order_state.items,
        shipping_address => get_customer_address(OrderState#order_state.customer_id)
    },

    % Integration with shipping service
    ShippingService = shipping_service:get(),
    ShippingService:create_label(ShippingInfo).

% Email notification
send_shipping_confirmation(OrderId, ShippingLabel) ->
    CustomerEmail = get_customer_email(OrderId),

    EmailContent = #{
        to => CustomerEmail,
        subject => "Your Order Has Been Shipped",
        body => build_shipping_email(OrderId, ShippingLabel)
    },

    email_service:send(EmailContent).
```

## Payment Branch Handlers

### File: `test/yawl_of_helpers.erl`

This module provides test helpers for specialized payment handling logic.

#### Handler Structure

```erlang
-module(yawl_of_helpers).

-export([handle_credit_card_payment/3,
         handle_paypal_payment/3,
         handle_crypto_payment/3,
         handle_payment_retry/3]).

-include("gen_pnet.hrl").

% Payment result record
-record(payment_result, {
    payment_id :: binary(),
    status :: success | failure | pending,
    amount :: number(),
    currency :: binary(),
    timestamp :: integer(),
    transaction_id :: binary(),
    gateway_response :: map()
}).
```

#### Credit Card Payment Handler

```erlang
%% @doc Handle credit card payment processing
-spec handle_credit_card_payment(context(), usr_info()) -> {ok, context()} | {error, term()}.
handle_credit_card_payment(Context, UsrInfo) ->
    OrderId = get_order_id(Context),
    Amount = get_order_amount(Context),
    PaymentInfo = get_payment_info(Context),

    % Validate payment info
    case validate_credit_card(PaymentInfo) of
        ok ->
            % Process payment
            case credit_card_processor:process(PaymentInfo, Amount) of
                {ok, TransactionId} ->
                    PaymentResult = #payment_result{
                        payment_id = generate_payment_id(),
                        status = success,
                        amount = Amount,
                        currency = "USD",
                        timestamp = timestamp(),
                        transaction_id = TransactionId,
                        gateway_response = #{processor => "credit_card"}
                    },

                    {ok, Context#{
                        payment_result => PaymentResult,
                        transaction_id => TransactionId
                    }};

                {error, Reason} ->
                    {error, {payment_failed, Reason}}
            end;

        {error, Reason} ->
            {error, {validation_failed, Reason}}
    end.

% Credit card validation
validate_credit_card(PaymentInfo) ->
    RequiredFields = [card_number, expiry_date, cvv, cardholder_name],

    % Check all required fields
    case lists:all(fun(Field) -> maps:is_key(Field, PaymentInfo) end, RequiredFields) of
        true ->
            validate_card_format(PaymentInfo);
        false ->
            {error, missing_required_fields}
    end.

% Card format validation
validate_card_format(PaymentInfo) ->
    CardNumber = maps:get(card_number, PaymentInfo),

    % Luhn algorithm check
    case luhn:valid(CardNumber) of
        true -> ok;
        false -> {error, invalid_card_number}
    end.
```

#### PayPal Payment Handler

```erlang
%% @doc Handle PayPal payment processing
-spec handle_paypal_payment(context(), usr_info()) -> {ok, context()} | {error, term()}.
handle_paypal_payment(Context, UsrInfo) ->
    OrderId = get_order_id(Context),
    Amount = get_order_amount(Context),
    PaymentInfo = get_payment_info(Context),

    % Get PayPal client
    PayPalClient = paypal_client:get(),

    % Create PayPal payment
    case PayPalClient:create_payment(OrderId, Amount, PaymentInfo) of
        {ok, PaymentId} ->
            % Execute payment
            case PayPalClient:execute_payment(PaymentId) of
                {ok, TransactionId} ->
                    PaymentResult = #payment_result{
                        payment_id = PaymentId,
                        status = success,
                        amount = Amount,
                        currency = "USD",
                        timestamp = timestamp(),
                        transaction_id = TransactionId,
                        gateway_response = #{processor => "paypal"}
                    },

                    {ok, Context#{
                        payment_result => PaymentResult,
                        transaction_id => TransactionId
                    }};

                {error, Reason} ->
                    {error, {paypal_payment_failed, Reason}}
            end;

        {error, Reason} ->
            {error, {paypal_setup_failed, Reason}}
    end.
```

## Handler Development Patterns

### 1. State Management Pattern

```erlang
% State management helper functions
get_order_state(OrderId) ->
    case wf_store:load_state(OrderId) of
        {ok, State} -> State;
        {error, not_found} -> create_initial_state(OrderId)
    end.

save_order_state(OrderId, State) ->
    wf_store:save_state(OrderId, State).

update_order_state(OrderId, Updates) ->
    CurrentState = get_order_state(OrderId),
    UpdatedState = maps:merge(CurrentState, Updates),
    save_order_state(OrderId, UpdatedState).
```

### 2. Error Handling Pattern

```erlang
% Comprehensive error handling
handle_task(TaskId, Context, UsrInfo) ->
    try
        TaskResult = do_handle_task(TaskId, Context, UsrInfo),
        {ok, TaskResult}
    catch
        throw:Error -> {error, Error};
        error:Reason -> {error, {unexpected_error, Reason}};
        _:_ -> {error, unknown_error}
    end.

% Retry pattern
handle_with_retry(Task, Context, UsrInfo, MaxRetries) ->
    handle_with_retry(Task, Context, UsrInfo, MaxRetries, 0).

handle_with_retry(Task, Context, UsrInfo, MaxRetries, CurrentRetry) when CurrentRetry < MaxRetries ->
    case handle_task(Task, Context, UsrInfo) of
        {error, retry} ->
            timer:sleep(1000 * CurrentRetry),  # Exponential backoff
            handle_with_retry(Task, Context, UsrInfo, MaxRetries, CurrentRetry + 1);
        Result -> Result
    end;
handle_with_retry(_, Context, _, _, _) ->
    {error, max_retries_exceeded}.
```

### 3. Logging Pattern

```erlang
% Logging helper
log_task_start(TaskId, Context, UsrInfo) ->
    LogEntry = #{
        task_id => TaskId,
        user_id => UsrInfo#usr_info.user_id,
        timestamp => timestamp(),
        context => Context
    },
    wf_audit_log:log(log_entry, LogEntry).

log_task_complete(TaskId, Result, Context) ->
    LogEntry = #{
        task_id => TaskId,
        result => Result,
        timestamp => timestamp(),
        context => Context
    },
    wf_audit_log:log(log_entry, LogEntry).

log_task_error(TaskId, Error, Context) ->
    LogEntry = #{
        task_id => TaskId,
        error => Error,
        timestamp => timestamp(),
        context => Context
    },
    wf_audit_log:log(log_entry, LogEntry).
```

## Testing Handlers

### Unit Testing

```erlang
-module(yawl_of_helpers_tests).
-include_lib("eunit/include/eunit.hrl").

handle_order_received_test() ->
    Context = #{items => [#{sku => "SKU001", quantity => 2}]},
    UsrInfo = #{user_id => "user123"},

    {ok, NewContext} = yawl_of_helpers:handle_order_received(Context, UsrInfo),

    ?assert(maps:is_key(order_id, NewContext)),
    ?assert(maps:is_key(order_state, NewContext)).

handle_payment_processed_test() ->
    Context = #{order_id => "order123", amount => 100.00},
    UsrInfo = #{user_id => "user123"},

    {ok, NewContext} = yawl_of_helpers:handle_payment_processed(Context, UsrInfo),

    ?assert(maps:is_key(payment_id, NewContext)).
```

### Integration Testing

```erlang
integration_test() ->
    % Setup test environment
    setup_test_db(),

    % Test complete workflow
    Context = #{items => [#{sku => "SKU001", quantity => 1}]},
    UsrInfo = #{user_id => "test_user"},

    % Test order received
    {ok, Context1} = yawl_of_helpers:handle_order_received(Context, UsrInfo),

    % Test payment processing
    {ok, Context2} = yawl_of_helpers:handle_payment_processed(Context1, UsrInfo),

    % Test inventory check
    {ok, Context3} = yawl_of_helpers:handle_inventory_checked(Context2, UsrInfo),

    % Verify final state
    ?assert(maps:is_key(shipping_label, Context3)).
```

## Best Practices

### 1. Handler Design
- Keep handlers focused and single-purpose
- Use clear function names and documentation
- Implement comprehensive error handling
- Use records for structured data

### 2. Integration
- Use dependency injection for external services
- Implement proper connection pooling
- Handle network timeouts gracefully
- Implement circuit breakers for external calls

### 3. Performance
- Cache frequently accessed data
- Use async operations where appropriate
- Monitor performance metrics
- Implement proper logging for debugging

### 4. Security
- Validate all input data
- Sanitize output data
- Use proper authentication for external services
- Implement proper error handling that doesn't expose sensitive data

## Deployment and Monitoring

### Deployment
- Deploy handlers as part of the application
- Use proper versioning for handlers
- Implement hot reload capabilities
- Monitor handler performance and errors

### Monitoring
- Track handler execution time
- Monitor error rates and types
- Log handler execution traces
- Set up alerts for critical failures

### Maintenance
- Regularly update dependencies
- Review and refactor handlers
- Update documentation with changes
- Implement proper testing for changes