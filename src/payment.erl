%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Payment Subprocess - YAWL Order Fulfillment Pattern
%%
%% This module implements the Payment subprocess from the CAISE 2013 paper.
%% The Payment workflow is modeled as a Petri net using gen_pnet:
%% - Places (circles in YAWL) = Conditions/states
%% - Transitions (boxes in YAWL) = Tasks
%% - Tokens = Flow of control
%% - Arcs = Flow relations
%%
%% Petri Net Structure:
%% PLACES:                          TRANSITIONS:
%% p_input                          t_receive_payment
%% p_payment_received               t_validate_payment
%% p_validated                      t_process_credit_card (WCP-04: XOR)
%% p_processing_cc                 t_process_paypal
%% p_processing_pp                  t_process_bank_transfer
%% p_processing_bt                  t_send_confirmation
%% p_confirmed                      t_handle_payment_failure (WHP-01)
%% p_failure_handled               t_retry_payment
%% p_output                         t_complete
%%
%% Key Patterns:
%% - WCP-04 (Exclusive Choice) - Select payment method
%% - WHP-01 (Error Handler) - Handle failed payments
%% - WCP-23 (Structured Loop) - Retry failed payments
%%
%% @reference Supporting Risk-Informed Decisions during Business Process Execution (CAISE 2013)
%% @end
%%--------------------------------------------------------------------

-module(payment).
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
-export([receive_payment/1, validate_payment/1, process_credit_card/1,
         process_paypal/1, process_bank_transfer/1, send_confirmation/1,
         handle_payment_failure/1, retry_payment/1]).

%% Include shared types
-include_lib("order_fulfillment_types.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(payment_state, {
    payment :: #payment{} | undefined,
    order :: #order{} | undefined,
    payment_details :: map() | undefined,
    retry_count = 0 :: non_neg_integer(),
    max_retries = 3 :: non_neg_integer(),
    log_id :: binary() | undefined
}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type payment_state() :: #payment_state{}.
-export_type([payment_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Payment Petri net state.
%% @end
%%--------------------------------------------------------------------
-spec new(Order :: #order{}, PaymentDetails :: map()) -> #payment_state{}.
new(Order, PaymentDetails) when is_record(Order, order), is_map(PaymentDetails) ->
    PaymentId = generate_payment_id(),
    Payment = #payment{
        payment_id = PaymentId,
        order_id = Order#order.order_id,
        method = maps:get(method, PaymentDetails, credit_card),
        amount = Order#order.total,
        currency = <<"USD">>,
        status = pending,
        created_at = erlang:system_time(millisecond)
    },
    LogId = generate_log_id(),
    #payment_state{
        payment = Payment,
        order = Order,
        payment_details = PaymentDetails,
        log_id = LogId
    };
new(Order, PaymentDetails) when is_map(Order), is_map(PaymentDetails) ->
    %% Convert map to order record
    OrderRec = #order{
        order_id = maps_get_default(Order, <<"order_id">>, generate_order_id()),
        customer_id = maps_get_default(Order, <<"customer_id">>, <<"CUST-UNKNOWN">>),
        customer_name = maps_get_default(Order, <<"customer_name">>, <<"Unknown Customer">>),
        customer_email = maps_get_default(Order, <<"customer_email">>, <<"unknown@example.com">>),
        items = [],
        shipping_address = maps_get_default(Order, <<"shipping_address">>, #{}),
        billing_address = maps_get_default(Order, <<"billing_address">>, #{}),
        subtotal = maps_get_default(Order, <<"subtotal">>, 0.0),
        tax = maps_get_default(Order, <<"tax">>, 0.0),
        shipping_cost = maps_get_default(Order, <<"shipping_cost">>, 0.0),
        total = maps_get_default(Order, <<"total">>, 0.0),
        status = confirmed,
        created_at = erlang:system_time(millisecond)
    },
    new(OrderRec, PaymentDetails).

%%--------------------------------------------------------------------
%% @doc Starts the Payment workflow.
%% @end
%%--------------------------------------------------------------------
-spec start(Order :: #order{} | map(), PaymentDetails :: map()) ->
          {ok, pid()} | {error, term()}.
start(Order, PaymentDetails) ->
    PaymentState = new(Order, PaymentDetails),
    gen_pnet:start_link(?MODULE, PaymentState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Payment workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Order :: #order{} | map(), PaymentDetails :: map()) ->
          {ok, #payment{}} | {error, term()}.
run(Order, PaymentDetails) ->
    case start(Order, PaymentDetails) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, State = #payment_state{payment = Payment}} ->
                    gen_pnet:stop(Pid),
                    case Payment#payment.status of
                        completed -> {ok, Payment};
                        failed -> {error, payment_failed}
                    end;
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Payment workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, #payment_state{}} | {error, term()}.
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Payment Petri net.
%% @end
%%--------------------------------------------------------------------
place_lst() ->
    [
        'p_input',
        'p_payment_received',
        'p_validated',
        'p_processing_cc',
        'p_processing_pp',
        'p_processing_bt',
        'p_confirmed',
        'p_failure_handled',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Payment Petri net.
%% @end
%%--------------------------------------------------------------------
trsn_lst() ->
    [
        't_receive_payment',
        't_validate_payment',
        't_process_cc',
        't_process_pp',
        't_process_bt',
        't_send_confirmation',
        't_handle_failure',
        't_retry_payment',
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
preset('t_receive_payment') -> ['p_input'];
preset('t_validate_payment') -> ['p_payment_received'];
preset('t_process_cc') -> ['p_validated'];
preset('t_process_pp') -> ['p_validated'];
preset('t_process_bt') -> ['p_validated'];
preset('t_send_confirmation') -> ['p_processing_cc', 'p_processing_pp', 'p_processing_bt'];
preset('t_handle_failure') -> ['p_processing_cc', 'p_processing_pp', 'p_processing_bt'];
preset('t_retry_payment') -> ['p_failure_handled'];
preset('t_complete') -> ['p_confirmed'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%% @end
%%--------------------------------------------------------------------
is_enabled('t_process_cc', _Mode, #payment_state{payment_details = Details}) ->
    %% WCP-04: Exclusive Choice - Route based on payment method
    case maps:get(method, Details, credit_card) of
        credit_card -> true;
        _ -> false
    end;

is_enabled('t_process_pp', _Mode, #payment_state{payment_details = Details}) ->
    case maps:get(method, Details, credit_card) of
        paypal -> true;
        _ -> false
    end;

is_enabled('t_process_bt', _Mode, #payment_state{payment_details = Details}) ->
    case maps:get(method, Details, credit_card) of
        bank_transfer -> true;
        _ -> false
    end;

is_enabled('t_retry_payment', _Mode, #payment_state{retry_count = Count, max_retries = Max}) ->
    %% WCP-23: Structured Loop - Allow retry if under max attempts
    Count < Max;

is_enabled(_Trsn, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: #payment_state{}) ->
          {produce, map()} | abort.

fire('t_receive_payment', #{'p_input' := [start]}, State) ->
    Payment = State#payment_state.payment,
    log_event(State, <<"Payment">>, <<"PaymentReceived">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"order_id">> => Payment#payment.order_id,
        <<"amount">> => Payment#payment.amount
    }),
    {produce, #{'p_payment_received' => [start]}};

fire('t_validate_payment', #{'p_payment_received' := [start]}, State) ->
    Payment = State#payment_state.payment,
    #{valid := Valid} = validate_payment(State),
    UpdatedPayment = Payment#payment{status = processing},
    State1 = State#payment_state{payment = UpdatedPayment},
    log_event(State, <<"Payment">>, <<"PaymentValidated">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"valid">> => Valid
    }),
    {produce, #{'p_validated' => [start]}, State1};

fire('t_process_cc', #{'p_validated' := [start]}, State) ->
    Payment = State#payment_state.payment,
    #{success := Success, transaction_id := TxId} = process_credit_card(State),
    UpdatedPayment = case Success of
        true ->
            Payment#payment{
                status = completed,
                transaction_id = TxId,
                completed_at = erlang:system_time(millisecond)
            };
        false ->
            Payment#payment{
                status = failed,
                failure_reason = <<"Card declined">>
            }
    end,
    State1 = State#payment_state{payment = UpdatedPayment},
    TargetPlace = case Success of
        true -> 'p_confirmed';
        false -> 'p_failure_handled'
    end,
    log_event(State, <<"Payment">>, <<"CreditCardProcessed">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"success">> => Success,
        <<"transaction_id">> => TxId
    }),
    {produce, #{TargetPlace => [start]}, State1};

fire('t_process_pp', #{'p_validated' := [start]}, State) ->
    Payment = State#payment_state.payment,
    #{success := Success, transaction_id := TxId} = process_paypal(State),
    UpdatedPayment = case Success of
        true ->
            Payment#payment{
                status = completed,
                transaction_id = TxId,
                completed_at = erlang:system_time(millisecond)
            };
        false ->
            Payment#payment{
                status = failed,
                failure_reason = <<"PayPal transaction failed">>
            }
    end,
    State1 = State#payment_state{payment = UpdatedPayment},
    TargetPlace = case Success of
        true -> 'p_confirmed';
        false -> 'p_failure_handled'
    end,
    log_event(State, <<"Payment">>, <<"PayPalProcessed">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"success">> => Success,
        <<"transaction_id">> => TxId
    }),
    {produce, #{TargetPlace => [start]}, State1};

fire('t_process_bt', #{'p_validated' := [start]}, State) ->
    Payment = State#payment_state.payment,
    #{success := Success, transaction_id := TxId} = process_bank_transfer(State),
    UpdatedPayment = case Success of
        true ->
            Payment#payment{
                status = completed,
                transaction_id = TxId,
                completed_at = erlang:system_time(millisecond)
            };
        false ->
            Payment#payment{
                status = failed,
                failure_reason = <<"Bank transfer failed">>
            }
    end,
    State1 = State#payment_state{payment = UpdatedPayment},
    TargetPlace = case Success of
        true -> 'p_confirmed';
        false -> 'p_failure_handled'
    end,
    log_event(State, <<"Payment">>, <<"BankTransferProcessed">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"success">> => Success,
        <<"transaction_id">> => TxId
    }),
    {produce, #{TargetPlace => [start]}, State1};

fire('t_send_confirmation', #{'p_confirmed' := [start]}, State) ->
    Payment = State#payment_state.payment,
    Order = State#payment_state.order,
    send_confirmation(State),
    log_event(State, <<"Payment">>, <<"ConfirmationSent">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"order_id">> => Order#order.order_id,
        <<"customer_email">> => Order#order.customer_email
    }),
    {produce, #{'p_output' => [start]}, State};

fire('t_handle_failure', #{'p_failure_handled' := [start]}, State) ->
    %% WHP-01: Error Handler
    Payment = State#payment_state.payment,
    handle_payment_failure(State),
    log_event(State, <<"Payment">>, <<"FailureHandled">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"reason">> => Payment#payment.failure_reason
    }),
    {produce, #{'p_failure_handled' => [start]}, State};

fire('t_retry_payment', #{'p_failure_handled' := [start]}, State) ->
    %% WCP-23: Structured Loop - Retry payment
    Payment = State#payment_state.payment,
    NewRetryCount = State#payment_state.retry_count + 1,
    State1 = State#payment_state{retry_count = NewRetryCount},
    #{success := Success} = retry_payment(State1),
    case Success of
        true ->
            UpdatedPayment = Payment#payment{
                status = completed,
                transaction_id = generate_transaction_id(),
                completed_at = erlang:system_time(millisecond)
            },
            State2 = State1#payment_state{payment = UpdatedPayment},
            log_event(State, <<"Payment">>, <<"RetrySuccessful">>, #{
                <<"payment_id">> => Payment#payment.payment_id,
                <<"attempt">> => NewRetryCount
            }),
            {produce, #{'p_confirmed' => [start]}, State2};
        false ->
            log_event(State, <<"Payment">>, <<"RetryFailed">>, #{
                <<"payment_id">> => Payment#payment.payment_id,
                <<"attempt">> => NewRetryCount
            }),
            {produce, #{'p_failure_handled' => [start]}, State1}
    end;

fire('t_complete', #{'p_output' := [start]}, State) ->
    Payment = State#payment_state.payment,
    log_event(State, <<"Payment">>, <<"PaymentComplete">>, #{
        <<"payment_id">> => Payment#payment.payment_id,
        <<"status">> => atom_to_binary(Payment#payment.status, utf8)
    }),
    {produce, #{}, State};

fire(_Trsn, _Mode, _State) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for custom processing.
%% @end
%%--------------------------------------------------------------------
trigger(Place, _Token, State) ->
    log_event(State, <<"Payment">>, <<"PlaceEntered">>, #{
        <<"place">> => atom_to_binary(Place, utf8)
    }),
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
init(PaymentState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"Payment">>}) of
        {ok, LogId} ->
            State1 = PaymentState#payment_state{log_id = LogId},
            PaymentId = case State1#payment_state.payment of
                undefined -> <<"UNKNOWN">>;
                P -> P#payment.payment_id
            end,
            yawl_xes:log_case_start(LogId, PaymentId),
            {ok, State1};
        _ ->
            {ok, PaymentState}
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
terminate(_Reason, State) ->
    LogId = State#payment_state.log_id,
    case LogId of
        undefined -> ok;
        _ ->
            PaymentId = case State#payment_state.payment of
                undefined -> <<"UNKNOWN">>;
                P -> P#payment.payment_id
            end,
            Status = case State#payment_state.payment of
                undefined -> <<"unknown">>;
                Pay -> atom_to_binary(Pay#payment.status, utf8)
            end,
            yawl_xes:log_case_complete(LogId, PaymentId, #{
                <<"status">> => Status
            })
    end,
    ok.

%%====================================================================
%% Task Implementation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Receives payment information.
%% @end
%%--------------------------------------------------------------------
-spec receive_payment(State :: #payment_state{}) -> #payment_state{}.
receive_payment(State) ->
    State.

%%--------------------------------------------------------------------
%% @doc Validates payment information.
%% @end
%%--------------------------------------------------------------------
-spec validate_payment(State :: #payment_state{}) -> map().
validate_payment(#payment_state{payment_details = Details}) ->
    %% Validate payment details
    Method = maps:get(method, Details, credit_card),
    Valid = case Method of
        credit_card ->
            Card = maps:get(credit_card, Details, #{}),
            validate_credit_card(Card);
        paypal ->
            PaypalInfo = maps:get(paypal, Details, #{}),
            validate_paypal(PaypalInfo);
        bank_transfer ->
            BankInfo = maps:get(bank_transfer, Details, #{}),
            validate_bank_transfer(BankInfo)
    end,
    #{
        valid => Valid,
        method => Method
    }.

%%--------------------------------------------------------------------
%% @doc Validates credit card information.
%% @end
%%--------------------------------------------------------------------
validate_credit_card(Card) ->
    HasNumber = maps:is_key(<<"card_number">>, Card),
    HasName = maps:is_key(<<"cardholder_name">>, Card),
    HasExpiry = maps:is_key(<<"expiry_month">>, Card) andalso maps:is_key(<<"expiry_year">>, Card),
    HasCVV = maps:is_key(<<"cvv">>, Card),
    HasNumber andalso HasName andalso HasExpiry andalso HasCVV.

%%--------------------------------------------------------------------
%% @doc Validates PayPal information.
%% @end
%%--------------------------------------------------------------------
validate_paypal(PaypalInfo) ->
    HasEmail = maps:is_key(<<"email">>, PaypalInfo),
    HasPayerId = maps:is_key(<<"payer_id">>, PaypalInfo),
    HasEmail andalso HasPayerId.

%%--------------------------------------------------------------------
%% @doc Validates bank transfer information.
%% @end
%%--------------------------------------------------------------------
validate_bank_transfer(BankInfo) ->
    HasAccount = maps:is_key(<<"account_number">>, BankInfo),
    HasRouting = maps:is_key(<<"routing_number">>, BankInfo),
    HasAccount andalso HasRouting.

%%--------------------------------------------------------------------
%% @doc Processes credit card payment.
%% @end
%%--------------------------------------------------------------------
-spec process_credit_card(State :: #payment_state{}) -> map().
process_credit_card(#payment_state{payment = Payment, payment_details = Details}) ->
    %% Simulate payment processing
    Success = rand:uniform(10) > 2,  %% 80% success rate
    TransactionId = case Success of
        true -> generate_transaction_id();
        false -> undefined
    end,
    #{
        success => Success,
        transaction_id => TransactionId,
        amount => Payment#payment.amount
    }.

%%--------------------------------------------------------------------
%% @doc Processes PayPal payment.
%% @end
%%--------------------------------------------------------------------
-spec process_paypal(State :: #payment_state{}) -> map().
process_paypal(#payment_state{payment = Payment}) ->
    %% Simulate PayPal payment processing
    Success = rand:uniform(10) > 1,  %% 90% success rate
    TransactionId = case Success of
        true -> <<"PP-", (generate_transaction_id())/binary>>;
        false -> undefined
    end,
    #{
        success => Success,
        transaction_id => TransactionId,
        amount => Payment#payment.amount
    }.

%%--------------------------------------------------------------------
%% @doc Processes bank transfer payment.
%% @end
%%--------------------------------------------------------------------
-spec process_bank_transfer(State :: #payment_state{}) -> map().
process_bank_transfer(#payment_state{payment = Payment}) ->
    %% Simulate bank transfer processing
    Success = rand:uniform(10) > 3,  %% 70% success rate
    TransactionId = case Success of
        true -> <<"BT-", (generate_transaction_id())/binary>>;
        false -> undefined
    end,
    #{
        success => Success,
        transaction_id => TransactionId,
        amount => Payment#payment.amount
    }.

%%--------------------------------------------------------------------
%% @doc Sends payment confirmation.
%% @end
%%--------------------------------------------------------------------
-spec send_confirmation(State :: #payment_state{}) -> ok.
send_confirmation(#payment_state{order = Order, payment = Payment}) ->
    %% In production, send email confirmation
    {ok, {
        Order#order.customer_email,
        Payment#payment.payment_id,
        Payment#payment.amount
    }}.

%%--------------------------------------------------------------------
%% @doc Handles payment failure.
%% @end
%%--------------------------------------------------------------------
-spec handle_payment_failure(State :: #payment_state{}) -> ok.
handle_payment_failure(#payment_state{payment = Payment}) ->
    %% Log failure, notify customer
    FailureReason = case Payment#payment.failure_reason of
        undefined -> <<"Unknown error">>;
        Reason -> Reason
    end,
    {ok, FailureReason}.

%%--------------------------------------------------------------------
%% @doc Retries payment processing.
%% @end
%%--------------------------------------------------------------------
-spec retry_payment(State :: #payment_state{}) -> map().
retry_payment(State) ->
    %% Retry with backoff
    #{
        success => rand:uniform(10) > 4,  %% 60% success on retry
        retry_count => State#payment_state.retry_count + 1
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
log_event(#payment_state{log_id = undefined}, _, _, _) ->
    ok;
log_event(#payment_state{log_id = LogId}, ConceptName, LifecycleTransition, Data) ->
    yawl_xes:log_event(LogId, ConceptName, LifecycleTransition, Data).

%% @private
generate_log_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"payment_log_", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_payment_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"PAY-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_order_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"ORDER-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_transaction_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"TXN-", (integer_to_binary(Timestamp))/binary>>.

%% @private
maps_get_default(Map, Key, Default) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Value -> Value
    end.

%% @private
wait_for_completion(Pid, Timeout) ->
    wait_for_completion(Pid, Timeout, 0).

wait_for_completion(Pid, Timeout, Elapsed) when Elapsed >= Timeout ->
    {error, timeout};
wait_for_completion(Pid, Timeout, Elapsed) ->
    case get_state(Pid) of
        {ok, #payment_state{payment = #payment{status = completed}}} ->
            get_state(Pid);
        {ok, #payment_state{payment = #payment{status = failed}, retry_count = C, max_retries = M}} when C >= M ->
            get_state(Pid);
        {ok, _} ->
            timer:sleep(100),
            wait_for_completion(Pid, Timeout, Elapsed + 100);
        {error, Reason} ->
            {error, Reason}
    end.
