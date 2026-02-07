%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Carrier Appointment Workflow - YAWL Order Fulfillment Pattern
%%
%% This module implements the Carrier Appointment subprocess from the CAISE 2013 paper
%% by Conforti, de Leoni, La Rosa, and van der Aalst.
%%
%% The Carrier Appointment workflow is modeled as a Petri net using gen_pnet:
%% - Places (circles in YAWL) = Conditions/states
%% - Transitions (boxes in YAWL) = Tasks
%% - Tokens = Flow of control
%% - Arcs = Flow relations
%%
%% @reference https://www.researchgate.net/figure/The-carrier-appointment-subprocess-of-an-order-fulfillment-process-shown-in-YAWL_fig1_268526460
%% @end
%%--------------------------------------------------------------------

-module(carrier_appointment).
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
-export([new/2, start/2]).
-export([receive_confirmation_order/1, estimate_trailer_usage/1,
         prepare_route_guide/1, prepare_transportation_quote/1,
         check_ftl_eligibility/1, create_shipment_info/1,
         arrange_pickup_ltl/1, arrange_delivery_ltl/1,
         create_bill_of_lading/1, arrange_pickup_ftl_a/1,
         arrange_pickup_ftl_b/1, arrange_delivery_ftl/1,
         arrange_pickup_single/1, arrange_delivery_single/1,
         create_manifest/1, modify_pickup/1, modify_delivery/1,
         produce_shipping_notice/1, complete_appointment/1]).

%% Record definitions
-record(ca_state, {
    purchase_order = undefined :: undefined | map(),
    trailer_estimate = undefined :: undefined | number(),
    route_guide = undefined :: undefined | map(),
    quote = undefined :: undefined | map(),
    shipment_info = undefined :: undefined | map(),
    appointments = [] :: list(),
    docs = [] :: list(),
    log_id = undefined :: undefined | binary(),
    shipping_type = undefined :: undefined | ftl | ltl | single
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Carrier Appointment Petri net state.
%% @end
%%--------------------------------------------------------------------
-spec new(PurchaseOrder :: map(), LogId :: binary()) -> #ca_state{}.
new(PurchaseOrder, LogId) ->
    #ca_state{
        purchase_order = PurchaseOrder,
        log_id = LogId,
        appointments = [],
        docs = []
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Carrier Appointment workflow.
%% @end
%%--------------------------------------------------------------------
-spec start(PurchaseOrder :: map(), LogId :: binary()) ->
          {ok, pid()} | {error, term()}.
start(PurchaseOrder, LogId) ->
    CAState = new(PurchaseOrder, LogId),
    gen_pnet:start_link(?MODULE, CAState, []).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Carrier Appointment Petri net.
%% @end
%%--------------------------------------------------------------------
place_lst() ->
    [
        %% Input/Output
        'p_input', 'p_output',

        %% Main flow places
        'p_po_received', 'p_trailer_estimated', 'p_route_guide_ready',
        'p_quote_ready', 'p_shipment_info_ready', 'p_docs_ready',
        'p_notice_produced',

        %% Shipping type places
        'p_ftl', 'p_ltl', 'p_single',

        %% Appointment places
        'p_appointment_pickup_ftl', 'p_appointment_pickup_ltl',
        'p_appointment_delivery_ftl', 'p_appointment_delivery_ltl',
        'p_appointment_pickup_single', 'p_appointment_delivery_single',

        %% Modifiable states
        'p_pickup_modifiable', 'p_delivery_modifiable'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Carrier Appointment Petri net.
%% @end
%%--------------------------------------------------------------------
trsn_lst() ->
    [
        't_receive_confirmation',
        't_estimate_trailer',
        't_prepare_route',
        't_prepare_quote',
        't_check_ftl',
        't_create_shipment_info',
        't_arrange_pickup_ltl',
        't_arrange_delivery_ltl',
        't_create_bol',
        't_arrange_pickup_ftl_a',
        't_arrange_pickup_ftl_b',
        't_arrange_delivery_ftl',
        't_arrange_pickup_single',
        't_arrange_delivery_single',
        't_create_manifest',
        't_modify_pickup',
        't_modify_delivery',
        't_produce_notice',
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
preset('t_receive_confirmation') -> ['p_input'];
preset('t_estimate_trailer') -> ['p_po_received'];
preset('t_prepare_route') -> ['p_po_received'];
preset('t_prepare_quote') -> ['p_trailer_estimated', 'p_route_guide_ready'];
preset('t_check_ftl') -> ['p_quote_ready'];
preset('t_create_shipment_info') -> ['p_ftl'];
preset('t_arrange_pickup_ltl') -> ['p_ltl'];
preset('t_arrange_delivery_ltl') -> ['p_ltl'];
preset('t_create_bol') -> ['p_appointment_pickup_ltl', 'p_appointment_delivery_ltl'];
preset('t_arrange_pickup_ftl_a') -> ['p_ftl'];
preset('t_arrange_pickup_ftl_b') -> ['p_ftl'];
preset('t_arrange_delivery_ftl') -> ['p_shipment_info_ready'];
preset('t_arrange_pickup_single') -> ['p_single'];
preset('t_arrange_delivery_single') -> ['p_single'];
preset('t_create_manifest') -> ['p_appointment_pickup_single', 'p_appointment_delivery_single'];
preset('t_modify_pickup') -> ['p_pickup_modifiable'];
preset('t_modify_delivery') -> ['p_delivery_modifiable'];
preset('t_produce_notice') -> ['p_docs_ready'];
preset('t_complete') -> ['p_notice_produced'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%% @end
%%--------------------------------------------------------------------
is_enabled('t_check_ftl', _Mode, #ca_state{quote = Quote}) ->
    Quote =/= undefined andalso maps:get(<<"total_volume">>, Quote, 0) > 0;

is_enabled('t_produce_notice', _Mode, _UsrInfo) ->
    true;

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% Returns {produce, ProduceMap} where ProduceMap specifies tokens to produce.
%% The gen_pnet engine handles consumption based on preset/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: #ca_state{}) ->
          {produce, map()} | abort.

fire('t_receive_confirmation', #{'p_input' := [Token]}, _CAState) ->
    {produce, #{'p_po_received' => [Token]}};

fire('t_estimate_trailer', #{'p_po_received' := [Token]}, _CAState) ->
    {produce, #{'p_trailer_estimated' => [Token]}};

fire('t_prepare_route', #{'p_po_received' := [Token]}, _CAState) ->
    {produce, #{'p_route_guide_ready' => [Token]}};

fire('t_prepare_quote', #{'p_trailer_estimated' := [_], 'p_route_guide_ready' := [Token]}, CAState) ->
    Quote = prepare_transportation_quote(CAState),
    ShippingType = determine_shipping_type(Quote),
    TargetPlace = case ShippingType of
        ftl -> 'p_ftl';
        ltl -> 'p_ltl';
        single -> 'p_single'
    end,
    {produce, #{TargetPlace => [Token]}};

fire('t_check_ftl', #{'p_quote_ready' := [Token]}, CAState) ->
    FTLThreshold = 10000,
    Quote = CAState#ca_state.quote,
    IsFTL = Quote =/= undefined andalso maps:get(<<"total_volume">>, Quote, 0) > FTLThreshold,
    TargetPlace = case IsFTL of
        true -> 'p_ftl';
        false -> 'p_ltl'
    end,
    {produce, #{TargetPlace => [Token]}};

fire('t_create_shipment_info', #{'p_ftl' := [Token]}, _CAState) ->
    {produce, #{'p_shipment_info_ready' => [Token]}};

fire('t_arrange_pickup_ltl', #{'p_ltl' := [Token]}, _CAState) ->
    {produce, #{'p_appointment_pickup_ltl' => [Token]}};

fire('t_arrange_delivery_ltl', #{'p_appointment_pickup_ltl' := [Token]}, _CAState) ->
    {produce, #{'p_appointment_delivery_ltl' => [Token]}};

fire('t_create_bol', #{'p_appointment_pickup_ltl' := [_], 'p_appointment_delivery_ltl' := [Token]}, _CAState) ->
    {produce, #{'p_docs_ready' => [Token]}};

fire('t_arrange_pickup_ftl_a', #{'p_ftl' := [Token]}, _CAState) ->
    {produce, #{'p_appointment_pickup_ftl' => [Token]}};

fire('t_arrange_pickup_ftl_b', #{'p_appointment_pickup_ftl' := [Token]}, _CAState) ->
    {produce, #{'p_pickup_modifiable' => [Token]}};

fire('t_arrange_delivery_ftl', #{'p_shipment_info_ready' := [Token]}, _CAState) ->
    {produce, #{'p_appointment_delivery_ftl' => [Token]}};

fire('t_arrange_pickup_single', #{'p_single' := [Token]}, _CAState) ->
    {produce, #{'p_appointment_pickup_single' => [Token]}};

fire('t_arrange_delivery_single', #{'p_appointment_pickup_single' := [Token]}, _CAState) ->
    {produce, #{'p_appointment_delivery_single' => [Token]}};

fire('t_create_manifest', #{'p_appointment_pickup_single' := [_], 'p_appointment_delivery_single' := [Token]}, _CAState) ->
    {produce, #{'p_docs_ready' => [Token]}};

fire('t_modify_pickup', #{'p_pickup_modifiable' := [Token]}, _CAState) ->
    {produce, #{'p_delivery_modifiable' => [Token]}};

fire('t_modify_delivery', #{'p_delivery_modifiable' := [Token]}, _CAState) ->
    {produce, #{'p_docs_ready' => [Token]}};

fire('t_produce_notice', #{'p_docs_ready' := [Token]}, _CAState) ->
    {produce, #{'p_notice_produced' => [Token]}};

fire('t_complete', #{'p_notice_produced' := [Token]}, _CAState) ->
    {produce, #{'p_output' => [Token]}};

fire(_Trsn, _Mode, _CAState) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for custom processing.
%% @end
%%--------------------------------------------------------------------
trigger('p_quote_ready', _Token, CAState) ->
    #ca_state{log_id = LogId, quote = Quote} = CAState,
    case LogId of
        undefined -> pass;
        _ ->
            QuoteId = maps:get(<<"quote_id">>, Quote, <<"UNKNOWN">>),
            yawl_xes:log_event(LogId, <<"CarrierAppointment">>, <<"QuoteReady">>, #{
                <<"quote_id">> => QuoteId
            }),
            {ok, Quote}
    end;
trigger(_Place, _Token, _CAState) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
init(CAState) ->
    {ok, CAState}.

%%--------------------------------------------------------------------
%% @doc Handles call messages.
%% @end
%%--------------------------------------------------------------------
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
terminate(_Reason, _NetState) ->
    ok.

%%====================================================================
%% Task Implementation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Receives confirmation of the purchase order.
%% @end
%%--------------------------------------------------------------------
-spec receive_confirmation_order(CAState :: #ca_state{}) -> #ca_state{}.
receive_confirmation_order(CAState) ->
    CAState.

%%--------------------------------------------------------------------
%% @doc Estimates trailer usage based on purchase order.
%% @end
%%--------------------------------------------------------------------
-spec estimate_trailer_usage(PO :: map() | term()) -> number().
estimate_trailer_usage(#{total_volume := Volume}) when is_number(Volume) ->
    %% Estimate number of trailers needed (5000 cubic feet per trailer)
    TrailerCapacity = 5000,
    TrailerCount = ceil(Volume / TrailerCapacity),
    max(1, TrailerCount);
estimate_trailer_usage(_) ->
    1.

%%--------------------------------------------------------------------
%% @doc Prepares the route guide for shipping.
%% @end
%%--------------------------------------------------------------------
-spec prepare_route_guide(CAState :: #ca_state{}) -> map().
prepare_route_guide(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    Destination = maps:get(<<"destination">>, PO, <<"UNKNOWN">>),
    #{
        <<"origin">> => <<"WAREHOUSE-001">>,
        <<"destination">> => Destination,
        <<"distance">> => rand:uniform(1000),
        <<"estimated_time">> => rand:uniform(72)
    };
prepare_route_guide(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Prepares transportation quote based on trailer and route.
%% @end
%%--------------------------------------------------------------------
-spec prepare_transportation_quote(CAState :: #ca_state{}) -> map().
prepare_transportation_quote(#ca_state{
                               purchase_order = PO,
                               trailer_estimate = Trailers
                              }) when is_map(PO), map_size(PO) > 0, is_number(Trailers) ->
    QuoteId = list_to_binary(["QUOTE-", integer_to_list(unique_id())]),
    RatePerCubicFoot = 0.15,
    Volume = maps:get(<<"total_volume">>, PO, 0),
    Rate = Volume * RatePerCubicFoot * Trailers,
    ItemCount = length(maps:get(<<"items">>, PO, [])),
    #{
        <<"quote_id">> => QuoteId,
        <<"total_volume">> => Volume,
        <<"package_count">> => ItemCount,
        <<"rate">> => Rate,
        <<"carrier">> => select_carrier(PO)
    };
prepare_transportation_quote(_) ->
    #{
        <<"quote_id">> => <<"QUOTE-DEFAULT">>,
        <<"total_volume">> => 0,
        <<"package_count">> => 1,
        <<"rate">> => 100.0,
        <<"carrier">> => <<"DEFAULT">>
    }.

%%--------------------------------------------------------------------
%% @doc Checks FTL eligibility based on quote.
%% @end
%%--------------------------------------------------------------------
-spec check_ftl_eligibility(CAState :: #ca_state{}) -> boolean().
check_ftl_eligibility(#ca_state{quote = Quote}) when is_map(Quote), map_size(Quote) > 0 ->
    maps:get(<<"total_volume">>, Quote, 0) > 10000;
check_ftl_eligibility(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Creates shipment information.
%% @end
%%--------------------------------------------------------------------
-spec create_shipment_info(CAState :: #ca_state{}) -> map().
create_shipment_info(#ca_state{
                       purchase_order = PO,
                       quote = Quote
                      }) when is_map(PO), is_map(Quote), map_size(PO) > 0, map_size(Quote) > 0 ->
    #{
        <<"shipment_id">> => list_to_binary(["SHIP-", integer_to_list(unique_id())]),
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"destination">> => maps:get(<<"destination">>, PO, <<"UNKNOWN">>),
        <<"carrier">> => maps:get(<<"carrier">>, Quote, <<"UNKNOWN">>),
        <<"rate">> => maps:get(<<"rate">>, Quote, 0)
    };
create_shipment_info(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges LTL pickup appointment.
%% @end
%%--------------------------------------------------------------------
-spec arrange_pickup_ltl(CAState :: #ca_state{}) -> map().
arrange_pickup_ltl(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"pickup">>,
        <<"mode">> => <<"ltl">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"scheduled_date">> => scheduled_date()
    };
arrange_pickup_ltl(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges LTL delivery appointment.
%% @end
%%--------------------------------------------------------------------
-spec arrange_delivery_ltl(CAState :: #ca_state{}) -> map().
arrange_delivery_ltl(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"delivery">>,
        <<"mode">> => <<"ltl">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"estimated_delivery">> => estimated_delivery()
    };
arrange_delivery_ltl(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Creates Bill of Lading.
%% @end
%%--------------------------------------------------------------------
-spec create_bill_of_lading(CAState :: #ca_state{}) -> map().
create_bill_of_lading(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"bol_id">> => list_to_binary(["BOL-", integer_to_list(unique_id())]),
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"carrier">> => <<"LTL-CARRIER">>,
        <<"date">> => erlang:system_time(second)
    };
create_bill_of_lading(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges FTL pickup (Client Liaison A).
%% @end
%%--------------------------------------------------------------------
-spec arrange_pickup_ftl_a(CAState :: #ca_state{}) -> map().
arrange_pickup_ftl_a(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"pickup">>,
        <<"mode">> => <<"ftl">>,
        <<"liaison">> => <<"A">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"scheduled_date">> => scheduled_date()
    };
arrange_pickup_ftl_a(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges FTL pickup (Client Liaison B).
%% @end
%%--------------------------------------------------------------------
-spec arrange_pickup_ftl_b(CAState :: #ca_state{}) -> map().
arrange_pickup_ftl_b(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"pickup">>,
        <<"mode">> => <<"ftl">>,
        <<"liaison">> => <<"B">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"scheduled_date">> => scheduled_date()
    };
arrange_pickup_ftl_b(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges FTL delivery.
%% @end
%%--------------------------------------------------------------------
-spec arrange_delivery_ftl(CAState :: #ca_state{}) -> map().
arrange_delivery_ftl(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"delivery">>,
        <<"mode">> => <<"ftl">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"estimated_delivery">> => estimated_delivery()
    };
arrange_delivery_ftl(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges single package pickup.
%% @end
%%--------------------------------------------------------------------
-spec arrange_pickup_single(CAState :: #ca_state{}) -> map().
arrange_pickup_single(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"pickup">>,
        <<"mode">> => <<"single">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"scheduled_date">> => scheduled_date()
    };
arrange_pickup_single(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Arranges single package delivery.
%% @end
%%--------------------------------------------------------------------
-spec arrange_delivery_single(CAState :: #ca_state{}) -> map().
arrange_delivery_single(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    #{
        <<"type">> => <<"delivery">>,
        <<"mode">> => <<"single">>,
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"estimated_delivery">> => estimated_delivery()
    };
arrange_delivery_single(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Creates shipping manifest.
%% @end
%%--------------------------------------------------------------------
-spec create_manifest(CAState :: #ca_state{}) -> map().
create_manifest(#ca_state{purchase_order = PO}) when is_map(PO), map_size(PO) > 0 ->
    Items = length(maps:get(<<"items">>, PO, [])),
    #{
        <<"manifest_id">> => list_to_binary(["MANIFEST-", integer_to_list(unique_id())]),
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"items">> => Items,
        <<"date">> => erlang:system_time(second)
    };
create_manifest(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Modifies pickup appointment.
%% @end
%%--------------------------------------------------------------------
-spec modify_pickup(CAState :: #ca_state{}) -> map().
modify_pickup(#ca_state{appointments = Appointments}) ->
    #{
        <<"action">> => <<"modify_pickup">>,
        <<"previous_appointments">> => length(Appointments),
        <<"modified_at">> => erlang:system_time(second)
    }.

%%--------------------------------------------------------------------
%% @doc Modifies delivery appointment.
%% @end
%%--------------------------------------------------------------------
-spec modify_delivery(CAState :: #ca_state{}) -> map().
modify_delivery(#ca_state{appointments = Appointments}) ->
    #{
        <<"action">> => <<"modify_delivery">>,
        <<"previous_appointments">> => length(Appointments),
        <<"modified_at">> => erlang:system_time(second)
    }.

%%--------------------------------------------------------------------
%% @doc Produces shipping notice.
%% @end
%%--------------------------------------------------------------------
-spec produce_shipping_notice(CAState :: #ca_state{}) -> map().
produce_shipping_notice(#ca_state{
                          purchase_order = PO,
                          shipment_info = ShipmentInfo
                         }) when is_map(PO), is_map(ShipmentInfo), map_size(PO) > 0, map_size(ShipmentInfo) > 0 ->
    #{
        <<"notice_id">> => list_to_binary(["NOTICE-", integer_to_list(unique_id())]),
        <<"po_id">> => maps:get(<<"id">>, PO, <<"UNKNOWN">>),
        <<"shipment_id">> => maps:get(<<"shipment_id">>, ShipmentInfo, <<"UNKNOWN">>),
        <<"produced_at">> => erlang:system_time(second)
    };
produce_shipping_notice(_) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Completes the carrier appointment.
%% @end
%%--------------------------------------------------------------------
-spec complete_appointment(CAState :: #ca_state{}) -> #ca_state{}.
complete_appointment(CAState) ->
    CAState.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
select_carrier(#{<<"total_weight">> := Weight}) when is_number(Weight), Weight > 1000 ->
    <<"HEAVY_FREIGHT_INC">>;
select_carrier(_) ->
    case rand:uniform(3) of
        1 -> <<"FED_EX">>;
        2 -> <<"UPS">>;
        3 -> <<"DHL">>
    end.

%% @private
determine_shipping_type(#{<<"total_volume">> := Volume, <<"package_count">> := Count}) ->
    if
        Volume > 10000 -> ftl;
        Count > 1 -> ltl;
        true -> single
    end;
determine_shipping_type(_) ->
    ltl.

%% @private
scheduled_date() ->
    DaysAhead = rand:uniform(5),
    Timestamp = erlang:system_time(second) + (DaysAhead * 86400),
    Timestamp.

%% @private
estimated_delivery() ->
    DaysAhead = 3 + rand:uniform(7),
    Timestamp = erlang:system_time(second) + (DaysAhead * 86400),
    Timestamp.

%% @private
unique_id() ->
    erlang:unique_integer([positive, monotonic]).
