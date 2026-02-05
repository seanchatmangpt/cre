%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Ordering Subprocess - YAWL Order Fulfillment Pattern
%%
%% This module implements the Ordering subprocess from the CAISE 2013 paper.
%% The Ordering workflow is modeled as a Petri net using gen_pnet:
%% - Places (circles in YAWL) = Conditions/states
%% - Transitions (boxes in YAWL) = Tasks
%% - Tokens = Flow of control
%% - Arcs = Flow relations
%%
%% Petri Net Structure:
%% PLACES:                          TRANSITIONS:
%% p_input                          t_receive_order
%% p_order_received                 t_check_inventory
%% p_inventory_checked              t_reserve_items (WCP-26: Critical Section)
%% p_items_reserved                 t_calculate_total
%% p_total_calculated               t_confirm_order
%% p_order_confirmed                t_complete
%% p_output
%%
%% @reference Supporting Risk-Informed Decisions during Business Process Execution (CAISE 2013)
%% @end
%%--------------------------------------------------------------------

-module(ordering).
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
-export([new/1, start/1, run/1, get_state/1]).
-export([receive_order/1, check_inventory/1, reserve_items/1,
         calculate_total/1, confirm_order/1]).

%% Include shared types
-include_lib("order_fulfillment_types.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(ordering_state, {
    order :: #order{} | undefined,
    inventory_checked = false :: boolean(),
    items_reserved = false :: boolean(),
    total_calculated = false :: boolean(),
    log_id :: binary() | undefined
}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type ordering_state() :: #ordering_state{}.
-export_type([ordering_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Ordering Petri net state.
%% @end
%%--------------------------------------------------------------------
-spec new(Order :: #order{}) -> #ordering_state{}.
new(Order) when is_record(Order, order) ->
    LogId = generate_log_id(),
    #ordering_state{
        order = Order,
        log_id = LogId
    };
new(OrderParams) when is_map(OrderParams) ->
    Order = #order{
        order_id = maps:get(<<"order_id">>, OrderParams, generate_order_id()),
        customer_id = maps:get(<<"customer_id">>, OrderParams, <<"CUST-UNKNOWN">>),
        customer_name = maps_get_default(OrderParams, <<"customer_name">>, <<"Unknown Customer">>),
        customer_email = maps_get_default(OrderParams, <<"customer_email">>, <<"unknown@example.com">>),
        items = parse_items(maps:get(<<"items">>, OrderParams, [])),
        shipping_address = maps_get_default(OrderParams, <<"shipping_address">>, #{}),
        billing_address = maps_get_default(OrderParams, <<"billing_address">>, #{}),
        subtotal = maps_get_default(OrderParams, <<"subtotal">>, 0.0),
        tax = maps_get_default(OrderParams, <<"tax">>, 0.0),
        shipping_cost = maps_get_default(OrderParams, <<"shipping_cost">>, 0.0),
        total = maps_get_default(OrderParams, <<"total">>, 0.0),
        status = pending,
        created_at = erlang:system_time(millisecond),
        notes = maps_get_default(OrderParams, <<"notes">>, undefined)
    },
    LogId = generate_log_id(),
    #ordering_state{
        order = Order,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Ordering workflow.
%% @end
%%--------------------------------------------------------------------
-spec start(Order :: #order{} | map()) -> {ok, pid()} | {error, term()}.
start(Order) ->
    OrderingState = new(Order),
    gen_pnet:start_link(?MODULE, OrderingState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Ordering workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Order :: #order{} | map()) -> {ok, #order{}} | {error, term()}.
run(Order) ->
    case start(Order) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, #ordering_state{order = FinalOrder}} ->
                    gen_pnet:stop(Pid),
                    {ok, FinalOrder};
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Ordering workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, #ordering_state{}} | {error, term()}.
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Ordering Petri net.
%% @end
%%--------------------------------------------------------------------
place_lst() ->
    [
        'p_input',
        'p_order_received',
        'p_inventory_checked',
        'p_items_reserved',
        'p_total_calculated',
        'p_order_confirmed',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Ordering Petri net.
%% @end
%%--------------------------------------------------------------------
trsn_lst() ->
    [
        't_receive_order',
        't_check_inventory',
        't_reserve_items',
        't_calculate_total',
        't_confirm_order',
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
preset('t_receive_order') -> ['p_input'];
preset('t_check_inventory') -> ['p_order_received'];
preset('t_reserve_items') -> ['p_inventory_checked'];
preset('t_calculate_total') -> ['p_items_reserved'];
preset('t_confirm_order') -> ['p_total_calculated'];
preset('t_complete') -> ['p_order_confirmed'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%% @end
%%--------------------------------------------------------------------
is_enabled('t_reserve_items', _Mode, State) ->
    %% WCP-26: Critical Section - Check if inventory is available
    State#ordering_state.inventory_checked;

is_enabled(_Trsn, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: #ordering_state{}) ->
          {produce, map()} | abort.

fire('t_receive_order', #{'p_input' := [start]}, State) ->
    Order = State#ordering_state.order,
    log_event(State, <<"Ordering">>, <<"OrderReceived">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"customer_id">> => Order#order.customer_id
    }),
    {produce, #{'p_order_received' => [start]}};

fire('t_check_inventory', #{'p_order_received' := [start]}, State) ->
    Order = State#ordering_state.order,
    #{available := Available} = check_inventory(State),
    log_event(State, <<"Ordering">>, <<"InventoryChecked">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"items_available">> => Available
    }),
    {produce, #{'p_inventory_checked' => [start]}};

fire('t_reserve_items', #{'p_inventory_checked' := [start]}, State) ->
    Order = State#ordering_state.order,
    #{reserved := Reserved} = reserve_items(State),
    log_event(State, <<"Ordering">>, <<"ItemsReserved">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"items_reserved">> => Reserved
    }),
    {produce, #{'p_items_reserved' => [start]}};

fire('t_calculate_total', #{'p_items_reserved' := [start]}, State) ->
    Order = State#ordering_state.order,
    #{total := Total} = calculate_total(State),
    UpdatedOrder = Order#order{total = Total, status = confirmed},
    State1 = State#ordering_state{order = UpdatedOrder, total_calculated = true},
    log_event(State, <<"Ordering">>, <<"TotalCalculated">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"total">> => Total
    }),
    {produce, #{'p_total_calculated' => [start]}, State1};

fire('t_confirm_order', #{'p_total_calculated' := [start]}, State) ->
    Order = State#ordering_state.order,
    confirm_order(State),
    UpdatedOrder = Order#order{status = confirmed},
    State1 = State#ordering_state{order = UpdatedOrder},
    log_event(State, <<"Ordering">>, <<"OrderConfirmed">>, #{
        <<"order_id">> => Order#order.order_id,
        <<"status">> => <<"confirmed">>
    }),
    {produce, #{'p_order_confirmed' => [start]}, State1};

fire('t_complete', #{'p_order_confirmed' := [start]}, State) ->
    Order = State#ordering_state.order,
    log_event(State, <<"Ordering">>, <<"OrderingComplete">>, #{
        <<"order_id">> => Order#order.order_id
    }),
    {produce, #{'p_output' => [start]}, State};

fire(_Trsn, _Mode, _State) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for custom processing.
%% @end
%%--------------------------------------------------------------------
trigger(Place, _Token, State) ->
    log_event(State, <<"Ordering">>, <<"PlaceEntered">>, #{
        <<"place">> => atom_to_binary(Place, utf8)
    }),
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
init(OrderingState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"Ordering">>}) of
        {ok, LogId} ->
            State1 = OrderingState#ordering_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, State1#ordering_state.order#order.order_id),
            {ok, State1};
        _ ->
            {ok, OrderingState}
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
    LogId = State#ordering_state.log_id,
    case LogId of
        undefined -> ok;
        _ ->
            OrderId = case State#ordering_state.order of
                undefined -> <<"UNKNOWN">>;
                Order -> Order#order.order_id
            end,
            yawl_xes:log_case_complete(LogId, OrderId, #{
                <<"status">> => <<"completed">>
            })
    end,
    ok.

%%====================================================================
%% Task Implementation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Receives and validates the incoming order.
%% @end
%%--------------------------------------------------------------------
-spec receive_order(State :: #ordering_state{}) -> #ordering_state{}.
receive_order(State) ->
    Order = State#ordering_state.order,
    %% Validate order has required fields
    case Order#order.order_id of
        undefined -> error(invalid_order);
        _ -> State
    end.

%%--------------------------------------------------------------------
%% @doc Checks inventory availability for all items in the order.
%% @end
%%--------------------------------------------------------------------
-spec check_inventory(State :: #ordering_state{}) -> map().
check_inventory(#ordering_state{order = Order}) when Order =:= undefined ->
    #{available => false, reason => no_order};
check_inventory(#ordering_state{order = Order}) ->
    Items = Order#order.items,
    CheckResults = lists:map(fun check_item_inventory/1, Items),
    AllAvailable = lists:all(fun(R) -> maps:get(available, R, false) end, CheckResults),
    #{
        available => AllAvailable,
        items => CheckResults
    }.

%%--------------------------------------------------------------------
%% @doc Checks inventory for a single item.
%% @end
%%--------------------------------------------------------------------
check_item_inventory(#item{sku = SKU, quantity = Qty}) ->
    %% Simulate inventory check - in production this would query a database
    AvailableQty = rand:uniform(100),
    #{
        sku => SKU,
        requested => Qty,
        available => AvailableQty >= Qty,
        on_hand => AvailableQty
    }.

%%--------------------------------------------------------------------
%% @doc Reserves items in inventory (WCP-26: Critical Section).
%% @end
%%--------------------------------------------------------------------
-spec reserve_items(State :: #ordering_state{}) -> map().
reserve_items(State = #ordering_state{order = Order}) when Order =:= undefined ->
    #{reserved => false, reason => no_order};
reserve_items(State = #ordering_state{order = Order}) ->
    Items = Order#order.items,
    %% Simulate critical section reservation
    %% In production, use ETS table with atomic operations
    ReserveResults = lists:map(fun reserve_item/1, Items),
    AllReserved = lists:all(fun(R) -> maps:get(reserved, R, false) end, ReserveResults),
    State1 = State#ordering_state{items_reserved = AllReserved},
    %% Update net state via side effect (triggered by fire)
    put(reserve_state, State1),
    #{
        reserved => AllReserved,
        items => ReserveResults
    }.

%%--------------------------------------------------------------------
%% @doc Reserves a single item in inventory.
%% @end
%%--------------------------------------------------------------------
reserve_item(#item{sku = SKU, quantity = Qty}) ->
    %% Simulate reservation
    Reserved = rand:uniform(10) > 1,  %% 90% success rate
    #{
        sku => SKU,
        quantity => Qty,
        reserved => Reserved,
        reservation_id => if Reserved -> generate_reservation_id(SKU); true -> <<>> end
    }.

%%--------------------------------------------------------------------
%% @doc Calculates the total cost including tax and shipping.
%% @end
%%--------------------------------------------------------------------
-spec calculate_total(State :: #ordering_state{}) -> map().
calculate_total(#ordering_state{order = Order}) when Order =:= undefined ->
    #{total => 0.0};
calculate_total(#ordering_state{order = Order}) ->
    Items = Order#order.items,
    Subtotal = lists:foldl(fun(#item{quantity = Q, price = P}, Acc) ->
        Acc + (Q * P)
    end, 0.0, Items),
    TaxRate = 0.08,  %% 8% tax
    Tax = Subtotal * TaxRate,
    ShippingCost = calculate_shipping_cost(Order),
    Total = Subtotal + Tax + ShippingCost,
    #{
        subtotal => Subtotal,
        tax => Tax,
        shipping => ShippingCost,
        total => Total
    }.

%%--------------------------------------------------------------------
%% @doc Calculates shipping cost based on order characteristics.
%% @end
%%--------------------------------------------------------------------
calculate_shipping_cost(#order{items = Items}) ->
    TotalWeight = lists:foldl(fun(#item{quantity = Q, weight = W}, Acc) ->
        Acc + (Q * W)
    end, 0.0, Items),
    case TotalWeight of
        W when W < 1.0 -> 5.99;
        W when W < 10.0 -> 12.99;
        W when W < 50.0 -> 29.99;
        _ -> 49.99
    end.

%%--------------------------------------------------------------------
%% @doc Confirms the order and prepares for next subprocess.
%% @end
%%--------------------------------------------------------------------
-spec confirm_order(State :: #ordering_state{}) -> ok.
confirm_order(#ordering_state{order = Order}) when Order =:= undefined ->
    {error, no_order};
confirm_order(#ordering_state{order = Order}) ->
    %% In production, send confirmation email, update database, etc.
    {ok, Order#order.order_id}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
log_event(#ordering_state{log_id = undefined}, _, _, _) ->
    ok;
log_event(#ordering_state{log_id = LogId}, ConceptName, LifecycleTransition, Data) ->
    yawl_xes:log_event(LogId, ConceptName, LifecycleTransition, Data).

%% @private
generate_log_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"ordering_log_", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_order_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"ORDER-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_reservation_id(SKU) ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<(binary:copy(SKU, 1))/binary, "-RES-", (integer_to_binary(Timestamp))/binary>>.

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
        category = maps_get_default(Item, <<"category">>, undefined)
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
        {ok, #ordering_state{order = #order{status = confirmed}}} ->
            get_state(Pid);
        {ok, _} ->
            timer:sleep(100),
            wait_for_completion(Pid, Timeout, Elapsed + 100);
        {error, Reason} ->
            {error, Reason}
    end.
