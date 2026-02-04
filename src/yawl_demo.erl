%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author YAWL Demo Service Implementation
%% @copyright 2025
%%
%% @doc YAWL Demo Service Module for CRE
%%
%% This module provides example service integrations and demonstrations
%% of YAWL workflow patterns within the CRE runtime environment.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Demo Workflows:</b> Pre-built example workflows</li>
%%   <li><b>Task Handlers:</b> Sample task implementations</li>
%%   <li><b>Test Data:</b> Sample data for demonstrations</li>
%%   <li><b>Telemetry:</b> Demo monitoring and reporting</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% <pre>
%% %% Start demo services
%% yawl_demo:start_demo_services().
%%
%% %% Run a demo workflow
%% yawl_demo:run_order_fulfillment_workflow().
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_demo).

%%====================================================================
%% Exports
%%====================================================================

%% Demo lifecycle
-export([start_demo_services/0,
         stop_demo_services/0,
         reset_demo_state/0]).

%% Demo workflows
-export([run_order_fulfillment_workflow/0,
         run_loan_approval_workflow/0,
         run_customer_onboarding_workflow/0,
         run_claim_processing_workflow/0]).

%% Demo tasks
-export([approve_order/1,
         check_inventory/1,
         process_payment/1,
         ship_order/1,
         send_confirmation/1,
         maps_get/2]).

%% Data generation
-export([generate_order_data/0,
         generate_customer_data/0,
         generate_claim_data/0,
         generate_random_case_data/1]).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Types
%%====================================================================

-type order_data() :: #{
    order_id => binary(),
    customer_id => binary(),
    items => list(map()),
    total => number(),
    status => pending | approved | rejected | shipped | completed
}.

-type customer_data() :: #{
    customer_id => binary(),
    name => binary(),
    email => binary(),
    address => map(),
    verified => boolean()
}.

-type claim_data() :: #{
    claim_id => binary(),
    policy_number => binary(),
    claim_type => binary(),
    amount => number(),
    description => binary(),
    status => pending | investigating | approved | denied
}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts all demo services.
%%
%% @return {ok, Started}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_demo_services() -> {ok, atom()}.

start_demo_services() ->
    %% Start the monitor if not already running
    try
        yawl_monitor:start_monitor(),
        error_logger:info_report([{module, ?MODULE}, {action, monitor_started}])
    catch
        _:{already_started, _} ->
            ok
    end,

    %% Start the cost tracker
    try
        yawl_cost:start_link(),
        error_logger:info_report([{module, ?MODULE}, {action, cost_tracker_started}])
    catch
        _:{already_started, _} ->
            ok
    end,

    %% Start the IPC server
    try
        yawl_ipc:start_link(),
        error_logger:info_report([{module, ?MODULE}, {action, ipc_started}])
    catch
        _:{already_started, _} ->
            ok
    end,

    %% Register demo task handlers
    register_demo_handlers(),

    {ok, demo_services_started}.

%%--------------------------------------------------------------------
%% @doc Stops all demo services.
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_demo_services() -> ok.

stop_demo_services() ->
    try
        yawl_monitor:stop_monitor()
    catch
        _:_ -> ok
    end,

    try
        yawl_cost:stop()
    catch
        _:_ -> ok
    end,

    try
        yawl_ipc:stop()
    catch
        _:_ -> ok
    end,

    error_logger:info_report([{module, ?MODULE}, {action, demo_services_stopped}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Resets demo state.
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_demo_state() -> ok.

reset_demo_state() ->
    yawl_monitor:reset_metrics(),
    yawl_cost:reset_costs(),
    ok.

%%--------------------------------------------------------------------
%% @doc Runs the order fulfillment demo workflow.
%%
%% @return {ok, Result}
%%
%% @end
%%--------------------------------------------------------------------
-spec run_order_fulfillment_workflow() -> {ok, map()}.

run_order_fulfillment_workflow() ->
    error_logger:info_report([{module, ?MODULE}, {action, run_order_workflow}]),

    %% Generate order data
    OrderData = generate_order_data(),
    CaseId = maps:get(order_id, OrderData),

    %% Start timing
    StartTime = erlang:system_time(millisecond),

    %% Record case started
    yawl_monitor:record_metric(
        <<"case_started">>,
        StartTime,
        #{<<"case_id">> => CaseId, <<"workflow_type">> => <<"order_fulfillment">>}
    ),

    %% Step 1: Check inventory
    InventoryResult = check_inventory(OrderData),
    yawl_monitor:record_metric(
        <<"inventory_checked">>,
        maps:get(available, InventoryResult, 0),
        #{<<"case_id">> => CaseId}
    ),

    %% Step 2: Approve order
    ApprovalResult = approve_order(OrderData#{inventory => InventoryResult}),
    yawl_monitor:record_metric(
        <<"order_approval">>,
        case maps:get(status, ApprovalResult) of approved -> 1; _ -> 0 end,
        #{<<"case_id">> => CaseId}
    ),

    %% Step 3: Process payment
    PaymentResult = case maps:get(status, ApprovalResult) of
        approved -> process_payment(OrderData);
        _ -> #{status => skipped, reason => order_not_approved}
    end,
    yawl_monitor:record_metric(
        <<"payment_processed">>,
        case maps:get(status, PaymentResult) of completed -> 1; _ -> 0 end,
        #{<<"case_id">> => CaseId}
    ),

    %% Step 4: Ship order
    ShipmentResult = case maps:get(status, PaymentResult) of
        completed -> ship_order(OrderData);
        _ -> #{status => skipped, reason => payment_not_completed}
    end,
    yawl_monitor:record_metric(
        <<"order_shipped">>,
        case maps:get(status, ShipmentResult) of shipped -> 1; _ -> 0 end,
        #{<<"case_id">> => CaseId}
    ),

    %% Step 5: Send confirmation
    ConfirmationResult = send_confirmation(OrderData#{
        approval => ApprovalResult,
        payment => PaymentResult,
        shipment => ShipmentResult
    }),

    %% Calculate cycle time
    EndTime = erlang:system_time(millisecond),
    CycleTime = EndTime - StartTime,

    yawl_monitor:record_metric(
        <<"case_completed">>,
        CycleTime,
        #{<<"case_id">> => CaseId}
    ),

    %% Track costs
    yawl_cost:assign_cost(CaseId, undefined, execution_cost, CycleTime * 0.001),

    {ok, #{
        case_id => CaseId,
        workflow_type => order_fulfillment,
        cycle_time_ms => CycleTime,
        order => OrderData,
        inventory => InventoryResult,
        approval => ApprovalResult,
        payment => PaymentResult,
        shipment => ShipmentResult,
        confirmation => ConfirmationResult
    }}.

%%--------------------------------------------------------------------
%% @doc Runs the loan approval demo workflow.
%%
%% @return {ok, Result}
%%
%% @end
%%--------------------------------------------------------------------
-spec run_loan_approval_workflow() -> {ok, map()}.

run_loan_approval_workflow() ->
    error_logger:info_report([{module, ?MODULE}, {action, run_loan_workflow}]),

    CaseId = generate_case_id(<<"loan">>),
    StartTime = erlang:system_time(millisecond),

    %% Simulate loan application processing
    ApplicationData = #{
        case_id => CaseId,
        applicant_name => <<"John Doe">>,
        loan_amount => 50000,
        credit_score => rand:uniform(850),
        income => 75000,
        debt_to_income => rand:uniform() * 0.5
    },

    %% Credit check
    CreditScore = maps:get(credit_score, ApplicationData),
    Approved = CreditScore >= 650,

    %% Record metrics
    yawl_monitor:record_metric(
        <<"loan_decision">>,
        case Approved of true -> 1; false -> 0 end,
        #{<<"case_id">> => CaseId, <<"credit_score">> => CreditScore}
    ),

    EndTime = erlang:system_time(millisecond),

    {ok, #{
        case_id => CaseId,
        workflow_type => loan_approval,
        cycle_time_ms => EndTime - StartTime,
        application => ApplicationData,
        decision => case Approved of true -> approved; false -> rejected end
    }}.

%%--------------------------------------------------------------------
%% @doc Runs the customer onboarding demo workflow.
%%
%% @return {ok, Result}
%%
%% @end
%%--------------------------------------------------------------------
-spec run_customer_onboarding_workflow() -> {ok, map()}.

run_customer_onboarding_workflow() ->
    error_logger:info_report([{module, ?MODULE}, {action, run_onboarding_workflow}]),

    CaseId = generate_case_id(<<"onboarding">>),
    CustomerData = generate_customer_data(),

    StartTime = erlang:system_time(millisecond),

    %% Steps: verify email, create account, send welcome, schedule training
    EmailVerified = maps:get(verified, CustomerData, true),

    AccountCreated = case EmailVerified of
        true -> #{status => created, account_id => generate_id(<<"account">>)};
        false -> #{status => failed, reason => email_not_verified}
    end,

    WelcomeSent = case maps:get(status, AccountCreated) of
        created -> #{status => sent};
        _ -> #{status => skipped}
    end,

    TrainingScheduled = case maps:get(status, AccountCreated) of
        created -> #{status => scheduled, date => add_days(7)};
        _ -> #{status => skipped}
    end,

    EndTime = erlang:system_time(millisecond),

    {ok, #{
        case_id => CaseId,
        workflow_type => customer_onboarding,
        cycle_time_ms => EndTime - StartTime,
        customer => CustomerData,
        steps => #{
            email_verified => EmailVerified,
            account_created => AccountCreated,
            welcome_sent => WelcomeSent,
            training_scheduled => TrainingScheduled
        }
    }}.

%%--------------------------------------------------------------------
%% @doc Runs the claim processing demo workflow.
%%
%% @return {ok, Result}
%%
%% @end
%%--------------------------------------------------------------------
-spec run_claim_processing_workflow() -> {ok, map()}.

run_claim_processing_workflow() ->
    error_logger:info_report([{module, ?MODULE}, {action, run_claim_workflow}]),

    CaseId = generate_case_id(<<"claim">>),
    ClaimData = generate_claim_data(),

    StartTime = erlang:system_time(millisecond),

    %% Claim processing steps
    _ClaimId = maps:get(claim_id, ClaimData),
    Amount = maps:get(amount, ClaimData),

    %% Auto-approve for small amounts
    AutoApproved = Amount < 1000,

    Decision = case AutoApproved of
        true ->
            yawl_monitor:record_metric(
                <<"claim_auto_approved">>,
                1,
                #{<<"case_id">> => CaseId}
            ),
            approved;
        false ->
            %% Require manual review
            yawl_monitor:record_metric(
                <<"claim_manual_review">>,
                1,
                #{<<"case_id">> => CaseId}
            ),
            %% Simulate review decision
            case rand:uniform() > 0.3 of
                true -> approved;
                false -> denied
            end
    end,

    EndTime = erlang:system_time(millisecond),

    {ok, #{
        case_id => CaseId,
        workflow_type => claim_processing,
        cycle_time_ms => EndTime - StartTime,
        claim => ClaimData,
        decision => Decision
    }}.

%%--------------------------------------------------------------------
%% @doc Demo task: Approves an order.
%%
%% @end
%%--------------------------------------------------------------------
-spec approve_order(order_data()) -> map().

approve_order(OrderData) ->
    Total = maps:get(total, OrderData, 0),
    CreditScore = maps:get(credit_score, OrderData, 700),

    Approved = Total < 10000 andalso CreditScore >= 600,

    #{
        status => case Approved of true -> approved; false -> rejected end,
        order_id => maps:get(order_id, OrderData),
        total => Total,
        approved_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Demo task: Checks inventory availability.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_inventory(order_data()) -> map().

check_inventory(OrderData) ->
    Items = maps:get(items, OrderData, []),
    Available = rand:uniform(100) > 20,

    #{
        available => case Available of true -> 1; false -> 0 end,
        items_checked => length(Items),
        warehouse_location => <<"WHS-01">>,
        checked_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Demo task: Processes payment.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_payment(order_data()) -> map().

process_payment(OrderData) ->
    Total = maps:get(total, OrderData, 0),
    Success = rand:uniform() > 0.1,  %% 90% success rate

    #{
        status => case Success of true -> completed; false -> failed end,
        amount => Total,
        payment_method => <<"credit_card">>,
        transaction_id => generate_id(<<"txn">>),
        processed_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Demo task: Ships an order.
%%
%% @end
%%--------------------------------------------------------------------
-spec ship_order(order_data()) -> map().

ship_order(_OrderData) ->
    Carrier = case rand:uniform(3) of
        1 -> <<"UPS">>;
        2 -> <<"FedEx">>;
        3 -> <<"USPS">>
    end,

    #{
        status => shipped,
        carrier => Carrier,
        tracking_number => generate_id(<<"track">>),
        estimated_delivery => add_days(5),
        shipped_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Demo task: Sends order confirmation.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_confirmation(map()) -> map().

send_confirmation(Data) ->
    OrderData = case maps:get(order, Data, undefined) of
        undefined -> Data;
        O -> O
    end,

    #{
        status => sent,
        sent_to => maps:get(email, OrderData, <<"customer@example.com">>),
        confirmation_number => generate_id(<<"conf">>),
        sent_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Generates demo order data.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_order_data() -> order_data().

generate_order_data() ->
    OrderId = generate_id(<<"order">>),
    CustomerId = generate_id(<<"customer">>),

    Items = [
        #{
            sku => <<"SKU-001">>,
            name => <<"Widget A">>,
            quantity => rand:uniform(5),
            price => 19.99
        },
        #{
            sku => <<"SKU-002">>,
            name => <<"Gadget B">>,
            quantity => rand:uniform(3),
            price => 29.99
        }
    ],

    Total = lists:foldl(
        fun(Item, Acc) ->
            Acc + (maps:get(quantity, Item, 1) * maps:get(price, Item, 0))
        end,
        0,
        Items
    ),

    #{
        order_id => OrderId,
        customer_id => CustomerId,
        email => <<CustomerId/binary, "@example.com">>,
        items => Items,
        total => Total,
        credit_score => 600 + rand:uniform(250),
        status => pending,
        created_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Generates demo customer data.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_customer_data() -> customer_data().

generate_customer_data() ->
    CustomerId = generate_id(<<"customer">>),

    #{
        customer_id => CustomerId,
        name => <<"Demo Customer">>,
        email => <<CustomerId/binary, "@example.com">>,
        phone => <<"+1-555-", (integer_to_binary(rand:uniform(9000) + 1000))/binary, "-",
                   (integer_to_binary(rand:uniform(9000) + 1000))/binary>>,
        address => #{
            street => <<"123 Main St">>,
            city => <<"Anytown">>,
            state => <<"CA">>,
            zip => <<"12345">>,
            country => <<"USA">>
        },
        verified => rand:uniform() > 0.2,
        created_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Generates demo claim data.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_claim_data() -> claim_data().

generate_claim_data() ->
    ClaimId = generate_id(<<"claim">>),
    PolicyNumber = generate_id(<<"policy">>),

    ClaimTypes = [<<"auto">>, <<"home">>, <<"health">>, <<"life">>],
    ClaimType = lists:nth(rand:uniform(length(ClaimTypes)), ClaimTypes),

    Amount = case ClaimType of
        <<"auto">> -> 500 + rand:uniform(5000);
        <<"home">> -> 1000 + rand:uniform(20000);
        <<"health">> -> 100 + rand:uniform(5000);
        <<"life">> -> 50000 + rand:uniform(200000)
    end,

    #{
        claim_id => ClaimId,
        policy_number => PolicyNumber,
        claim_type => ClaimType,
        amount => Amount,
        description => <<"Demo claim for ", ClaimType/binary, " damage">>,
        status => pending,
        filed_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Generates random case data for simulations.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_random_case_data(binary()) -> map().

generate_random_case_data(Prefix) ->
    CaseId = generate_case_id(Prefix),
    #{
        case_id => CaseId,
        created_at => erlang:system_time(millisecond),
        priority => lists:nth(rand:uniform(3), [low, medium, high]),
        estimated_duration_ms => 1000 + rand:uniform(10000),
        assignee => generate_id(<<"user">>),
        metadata => #{
            source => <<"demo">>,
            random_seed => rand:uniform(1000000)
        }
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Registers demo task handlers.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_demo_handlers() -> ok.

register_demo_handlers() ->
    %% Register order processing handlers
    try
        cre_yawl_worker:register_task_handler(
            <<"demo.approve_order">>,
            fun approve_order/1,
            atomic
        ),
        cre_yawl_worker:register_task_handler(
            <<"demo.check_inventory">>,
            fun check_inventory/1,
            atomic
        ),
        cre_yawl_worker:register_task_handler(
            <<"demo.process_payment">>,
            fun process_payment/1,
            atomic
        ),
        cre_yawl_worker:register_task_handler(
            <<"demo.ship_order">>,
            fun ship_order/1,
            atomic
        ),
        cre_yawl_worker:register_task_handler(
            <<"demo.send_confirmation">>,
            fun send_confirmation/1,
            atomic
        ),
        error_logger:info_report([{module, ?MODULE}, {action, handlers_registered}])
    catch
        _:_ ->
            error_logger:warning_report([{module, ?MODULE}, {warning, worker_not_available}])
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_id(binary()) -> binary().

generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a case ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id(binary()) -> binary().

generate_case_id(Prefix) ->
    TS = integer_to_binary(erlang:system_time(millisecond)),
    Unique = crypto:hash(md5, term_to_binary({self(), TS})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_case_", TS/binary, "_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Safely gets a key from a map with default.
%%
%% @end
%%--------------------------------------------------------------------
%% @private
%% @doc Helper to get value from map with default.
%% Exported for backward compatibility.
%% @end
-spec maps_get(atom(), map()) -> term().

maps_get(Key, Map) ->
    maps:get(Key, Map, <<>>).

%%--------------------------------------------------------------------
%% @private
%% @doc Adds days to current time.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_days(non_neg_integer()) -> integer().

add_days(Days) ->
    erlang:system_time(millisecond) + (Days * 86400000).
