%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Petri Net Integration Example
%%
%% This module demonstrates a complete working example of using the new
%% helper modules (pnet_types, pnet_marking, pnet_receipt, wf_task) with
%% the gen_pnet behaviour for implementing a YAWL-style workflow.
%%
%% <h3>Workflow: Order Processing with Approval</h3>
%%
%% The example implements a simple order processing workflow:
%% <ol>
%%   <li>Receive order request</li>
%%   <li>Validate order data</li>
%%   <li>Route to approval (if high value) or direct processing</li>
%%   <li>Execute approval (for high value orders)</li>
%%   <li>Process payment</li>
%%   <li>Ship order</li>
%%   <li>Complete workflow</li>
%% </ol>
%%
%% <h3>Petri Net Structure</h3>
%%
%% <pre>
%%     p_start
%%        |
%%     t_receive
%%        |
%%     p_validating --> t_validate --> p_validated
%%        |                                    |
%%        |                             t_route
%%        |                                    |
%%        |                             +------+------+
%%        |                             |             |
%%        |                        p_pending       p_processing
%%        |                             |             |
%%        |                        t_approve        |
%%        |                             |             |
%%        |                        p_approved       |
%%        |                             \            /
%%        |                              \          /
%%        |                               t_payment
%%        |                                    |
%%        |                             p_paying --> t_ship --> p_shipping
%%        |                                                           |
%%        |                                                        t_complete
%%        |                                                           |
%%        +------------------------------------------------------- p_complete
%% </pre>
%%
%% <h3>Key Features Demonstrated</h3>
%% <ul>
%%   <li><b>pnet_types:</b> Type definitions for places, tokens, markings</li>
%%   <li><b>pnet_marking:</b> Marking algebra for state management</li>
%%   <li><b>pnet_receipt:</b> Receipt creation for audit trail</li>
%%   <li><b>wf_task:</b> Token injection for external work</li>
%%   <li><b>gen_pnet:</b> Complete behaviour implementation</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_pnet_example).
-behavior(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         trigger/3]).

-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3]).

%% Public API
-export([start_link/0,
         start_link/1,
         submit_order/3,
         get_marking/1,
         get_receipts/1,
         inject_approval/3,
         stop/1]).

%%====================================================================
%% Includes
%%====================================================================

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Order record representing a customer order.
%%--------------------------------------------------------------------
-record(order, {
          order_id :: binary(),
          customer_id :: binary(),
          items :: [#{binary() => pos_integer()}],
          total :: number(),
          currency :: binary(),
          timestamp :: integer()
         }).

-type order() :: #order{}.

%%--------------------------------------------------------------------
%% @doc Approval record for high-value orders.
%%--------------------------------------------------------------------
-record(approval, {
          order_id :: binary(),
          approver_id :: binary(),
          decision :: approved | rejected,
          reason :: binary() | undefined,
          timestamp :: integer()
         }).

-type approval() :: #approval{}.

%%--------------------------------------------------------------------
%% @doc Workflow state tracking.
%%--------------------------------------------------------------------
-record(workflow_state, {
          receipts = [] :: [pnet_receipt:receipt()],
          order_count = 0 :: non_neg_integer(),
          approval_count = 0 :: non_neg_integer(),
          last_transition = undefined :: {atom(), integer()}
         }).

-type workflow_state() :: #workflow_state{}.

%% Export types
-export_type([order/0, approval/0, workflow_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the order processing workflow net with default options.
%%
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc Starts the order processing workflow net.
%%
%% @param Options Proplist for configuration (currently unused)
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Options :: proplists:proplist()) -> {ok, pid()} | {error, term()}.

start_link(Options) ->
    gen_pnet:start_link(?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Submits a new order for processing.
%%
%% Creates an order record and injects it into the workflow via
%% the start place.
%%
%% @param NetPid The pid of the workflow net process
%% @param CustomerId Customer identifier
%% @param Items List of item maps with quantity
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec submit_order(NetPid :: pid(),
                  CustomerId :: binary(),
                  Items :: [#{binary() => pos_integer()}]) -> ok.

submit_order(NetPid, CustomerId, Items) ->
    %% Calculate order total
    Total = lists:foldl(
        fun(#{<<"sku">> := SKU, <<"quantity">> := Qty}, Acc) ->
            Price = get_item_price(SKU),
            Acc + (Price * Qty)
        end,
        0,
        Items
    ),

    OrderId = generate_order_id(),
    Order = #order{
        order_id = OrderId,
        customer_id = CustomerId,
        items = Items,
        total = Total,
        currency = <<"USD">>,
        timestamp = erlang:system_time(millisecond)
    },

    io:format("Submitting Order ~s for Customer ~s (Total: $~.2f)~n",
              [OrderId, CustomerId, Total]),

    gen_pnet:cast(NetPid, {submit_order, Order}),
    ok.

%%--------------------------------------------------------------------
%% @doc Gets the current marking of the workflow net.
%%
%% Uses pnet_marking helpers to extract and display state.
%%
%% @param NetPid The pid of the workflow net process
%% @return {ok, Marking} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_marking(NetPid :: pid()) ->
          {ok, pnet_types:marking()} | {error, term()}.

get_marking(NetPid) ->
    gen_pnet:call(NetPid, get_marking).

%%--------------------------------------------------------------------
%% @doc Gets the receipt history of the workflow.
%%
%% Receipts provide an audit trail of all state transitions.
%%
%% @param NetPid The pid of the workflow net process
%% @return {ok, [Receipt]} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_receipts(NetPid :: pid()) ->
          {ok, [pnet_receipt:receipt()]} | {error, term()}.

get_receipts(NetPid) ->
    gen_pnet:call(NetPid, get_receipts).

%%--------------------------------------------------------------------
%% @doc Injects an external approval decision into the workflow.
%%
%% Uses wf_task constructors to create properly shaped tokens.
%%
%% @param NetPid The pid of the workflow net process
%% @param OrderId The order being approved
%% @param Decision approved | rejected
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec inject_approval(NetPid :: pid(),
                     OrderId :: binary(),
                     Decision :: approved | rejected) -> ok.

inject_approval(NetPid, OrderId, Decision) ->
    ApproverId = <<"admin">>,
    Approval = #approval{
        order_id = OrderId,
        approver_id = ApproverId,
        decision = Decision,
        reason = undefined,
        timestamp = erlang:system_time(millisecond)
    },

    io:format("Injecting approval for Order ~s: ~p~n", [OrderId, Decision]),

    %% Use wf_task to create a properly shaped done token
    {produce, ProduceMap} = wf_task:done(OrderId, Approval, 'p_approval_wait'),
    gen_pnet:produce(NetPid, ProduceMap),

    ok.

%%--------------------------------------------------------------------
%% @doc Stops the workflow net.
%%
%% @param NetPid The pid of the workflow net process
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(NetPid :: pid()) -> ok.

stop(NetPid) ->
    gen_pnet:stop(NetPid).

%%====================================================================
%% gen_pnet Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the workflow state.
%%
%% @param _InitArg Initialization argument (unused)
%% @return Initial workflow state
%%
%% @end
%%--------------------------------------------------------------------
-spec init(_InitArg :: term()) -> workflow_state().

init(_InitArg) ->
    #workflow_state{}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% Supports get_marking and get_receipts queries.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), term()},
                  NetState :: term()) ->
          {reply, term(), pnet_types:marking()} |
          {reply, term(), pnet_types:marking(), term()}.

handle_call(get_marking, _From, Marking) ->
    {reply, {ok, Marking}, Marking};

handle_call(get_receipts, _From, Marking) ->
    %% Extract receipts from usr_info
    UsrInfo = gen_pnet:get_usr_info(Marking),
    {reply, {ok, UsrInfo#workflow_state.receipts}, Marking};

handle_call(_Request, _From, Marking) ->
    {reply, {error, bad_msg}, Marking}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% Supports order submission by injecting tokens into p_start.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(),
                  Marking :: pnet_types:marking()) ->
          {noreply, pnet_types:marking()}.

handle_cast({submit_order, Order}, Marking) ->
    %% Use pnet_marking to add the order token to p_start
    case pnet_marking:add(Marking, #{'p_start' => [Order]}) of
        {error, Reason} ->
            io:format("Error adding order token: ~p~n", [Reason]),
            {noreply, Marking};
        NewMarking ->
            {noreply, NewMarking}
    end;

handle_cast(_Request, Marking) ->
    {noreply, Marking}.

%%--------------------------------------------------------------------
%% @doc Handles unexpected info messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(),
                  Marking :: pnet_types:marking()) ->
          {noreply, pnet_types:marking()}.

handle_info(_Info, Marking) ->
    {noreply, Marking}.

%%--------------------------------------------------------------------
%% @doc Handles code changes during hot upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(),
                  NetState :: term(),
                  Extra :: term()) -> {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), _NetState :: term()) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token processing.
%%
%% Called when tokens arrive at output places. We use this to
%% record completion events and update workflow state.
%%
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(),
              Token :: term(),
              NetState :: term()) -> pass | drop.

trigger('p_complete', Order, NetState) ->
    io:format("  → Order ~s COMPLETED~n", [Order#order.order_id]),
    UsrInfo = gen_pnet:get_usr_info(NetState),
    NewUsrInfo = UsrInfo#workflow_state{
        order_count = UsrInfo#workflow_state.order_count + 1
    },
    gen_pnet:set_usr_info(NetState, NewUsrInfo),
    pass;

trigger('p_rejected', _Order, NetState) ->
    %% io:format("  → Order REJECTED~n"),
    pass;

trigger(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% Petri Net Structure Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of all places in the net.
%%
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    ['p_start',
     'p_validating',
     'p_validated',
     'p_pending',
     'p_processing',
     'p_approved',
     'p_paying',
     'p_shipping',
     'p_complete',
     'p_rejected',
     'p_approval_wait'].

%%--------------------------------------------------------------------
%% @doc Returns the list of all transitions in the net.
%%
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_receive,
     t_validate,
     t_route,
     t_approve,
     t_payment,
     t_ship,
     t_complete,
     t_reject].

%%--------------------------------------------------------------------
%% @doc Returns initial marking for a place.
%%
%% All places start empty.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), _UsrInfo :: workflow_state()) -> [term()].

init_marking(_Place, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_receive) -> ['p_start'];
preset(t_validate) -> ['p_validating'];
preset(t_route) -> ['p_validated'];
preset(t_approve) -> ['p_pending', 'p_approval_wait'];
preset(t_payment) -> ['p_approved', 'p_processing'];
preset(t_ship) -> ['p_paying'];
preset(t_complete) -> ['p_shipping'];
preset(t_reject) -> ['p_validated'].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%%
%% Uses pnet_types validation helpers for mode checking.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(),
                Mode :: pnet_types:mode(),
                UsrInfo :: workflow_state()) -> boolean().

%% t_receive always enabled when there's a token at p_start
is_enabled(t_receive, _Mode, _UsrInfo) ->
    true;

%% t_validate always enabled for order tokens
is_enabled(t_validate, _Mode, _UsrInfo) ->
    true;

%% t_route checks order value to determine path
is_enabled(t_route, #{'p_validated' := [_Order]}, _UsrInfo) ->
    true;

%% t_approve needs both pending order and approval token
is_enabled(t_approve, #{'p_pending' := [_Order],
                        'p_approval_wait' := [_Approval]}, _UsrInfo) ->
    true;

%% t_payment needs approved order or direct processing order
is_enabled(t_payment, Mode, _UsrInfo) ->
    case Mode of
        #{'p_approved' := [_]} -> true;
        #{'p_processing' := [_]} -> true;
        _ -> false
    end;

%% All other transitions
is_enabled(t_ship, _Mode, _UsrInfo) -> true;
is_enabled(t_complete, _Mode, _UsrInfo) -> true;
is_enabled(t_reject, _Mode, _UsrInfo) -> true;

is_enabled(_, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% This is where the main workflow logic lives. Uses pnet_marking
%% helpers for state operations and wf_task for token construction.
%%
%% Note: The fire callback signature is fire(Trsn, Mode, UsrInfo).
%% The Marking is handled internally by gen_pnet.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(),
           Mode :: pnet_types:mode(),
           UsrInfo :: workflow_state()) ->
          {produce, pnet_types:produce_map()}.

%%--------------------------------------------------------------------
%% t_receive: Move order from start to validation
%%--------------------------------------------------------------------
fire(t_receive,
     #{'p_start' := [Order]},
     #workflow_state{order_count = Count}) ->
    io:format("  [~p] t_receive: Order ~s received~n", [Count, Order#order.order_id]),
    {produce, #{'p_validating' => [Order]}};

%%--------------------------------------------------------------------
%% t_validate: Validate order data
%%--------------------------------------------------------------------
fire(t_validate,
     #{'p_validating' := [Order] = Tokens},
     UsrInfo) ->
    %% Validate order
    IsValid = validate_order(Order),

    %% Record the transition with pnet_receipt
    %% Note: We don't have access to the full marking in fire/3,
    %% so we create a minimal receipt for tracking purposes
    Move = #{
        trsn => t_validate,
        mode => #{'p_validating' => Tokens},
        produce => #{}
    },
    %% Create receipt with placeholder hashes (actual hashes computed by gen_pnet)
    Receipt = pnet_receipt:make(<<>>, <<>>, Move),

    %% Update usr_info with the receipt
    NewUsrInfo = UsrInfo#workflow_state{
        receipts = [Receipt | UsrInfo#workflow_state.receipts],
        last_transition = {t_validate, erlang:system_time(millisecond)}
    },
    gen_pnet:set_usr_info(UsrInfo, NewUsrInfo),

    case IsValid of
        true ->
            io:format("  → Order ~s validated successfully~n", [Order#order.order_id]),
            {produce, #{'p_validated' => [Order]}};
        false ->
            io:format("  → Order ~s validation failed~n", [Order#order.order_id]),
            {produce, #{'p_rejected' => [Order]}}
    end;

%%--------------------------------------------------------------------
%% t_route: Route high-value orders to approval, others to processing
%%--------------------------------------------------------------------
fire(t_route,
     #{'p_validated' := [Order]},
     _UsrInfo) ->
    IsHighValue = Order#order.total >= 1000,

    if
        IsHighValue ->
            io:format("  → Order ~s ($~.2f) routed for approval~n",
                      [Order#order.order_id, Order#order.total]),
            {produce, #{'p_pending' => [Order]}};
        true ->
            io:format("  → Order ~s ($~.2f) routed to direct processing~n",
                      [Order#order.order_id, Order#order.total]),
            {produce, #{'p_processing' => [Order]}}
    end;

%%--------------------------------------------------------------------
%% t_approve: Process approval decision
%%--------------------------------------------------------------------
fire(t_approve,
     #{'p_pending' := [Order], 'p_approval_wait' := [Approval]},
     UsrInfo) ->
    io:format("  → Processing approval for Order ~s: ~p~n",
              [Order#order.order_id, Approval#approval.decision]),

    NewUsrInfo = UsrInfo#workflow_state{
        approval_count = UsrInfo#workflow_state.approval_count + 1
    },
    gen_pnet:set_usr_info(UsrInfo, NewUsrInfo),

    case Approval#approval.decision of
        approved ->
            {produce, #{'p_approved' => [Order]}};
        rejected ->
            {produce, #{'p_rejected' => [Order]}}
    end;

%%--------------------------------------------------------------------
%% t_payment: Process payment
%%--------------------------------------------------------------------
fire(t_payment,
     #{'p_approved' := [Order]},
     _UsrInfo) ->
    io:format("  → Processing payment for Order ~s: $~.2f~n",
              [Order#order.order_id, Order#order.total]),
    {produce, #{'p_paying' => [Order]}};

fire(t_payment,
     #{'p_processing' := [Order]},
     _UsrInfo) ->
    io:format("  → Processing payment for Order ~s: $~.2f~n",
              [Order#order.order_id, Order#order.total]),
    {produce, #{'p_paying' => [Order]}};

%%--------------------------------------------------------------------
%% t_ship: Ship the order
%%--------------------------------------------------------------------
fire(t_ship,
     #{'p_paying' := [Order]},
     _UsrInfo) ->
    io:format("  → Shipping Order ~s~n", [Order#order.order_id]),
    {produce, #{'p_shipping' => [Order]}};

%%--------------------------------------------------------------------
%% t_complete: Mark order as complete
%%--------------------------------------------------------------------
fire(t_complete,
     #{'p_shipping' := [Order]},
     _UsrInfo) ->
    {produce, #{'p_complete' => [Order]}};

%%--------------------------------------------------------------------
%% t_reject: Reject invalid order
%%--------------------------------------------------------------------
fire(t_reject,
     #{'p_validated' := [Order]},
     _UsrInfo) ->
    io:format("  → Order ~s REJECTED~n", [Order#order.order_id]),
    {produce, #{'p_rejected' => [Order]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique order ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_order_id() -> binary().

generate_order_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"ORD_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the price for an item SKU.
%%
%% Simulates a product catalog lookup.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_item_price(SKU :: binary()) -> number().

get_item_price(<<"PROD_BASIC">>) -> 50.0;
get_item_price(<<"PROD_STANDARD">>) -> 150.0;
get_item_price(<<"PROD_PREMIUM">>) -> 500.0;
get_item_price(<<"PROD_ENTERPRISE">>) -> 2000.0;
get_item_price(_SKU) -> 100.0.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates an order.
%%
%% Checks basic order constraints.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_order(Order :: order()) -> boolean().

validate_order(Order) ->
    %% Basic validation rules
    HasItems = length(Order#order.items) > 0,
    HasPositiveTotal = Order#order.total > 0,
    HasValidCurrency = Order#order.currency =:= <<"USD">> orelse
                       Order#order.currency =:= <<"EUR">> orelse
                       Order#order.currency =:= <<"GBP">>,

    HasItems andalso HasPositiveTotal andalso HasValidCurrency.
