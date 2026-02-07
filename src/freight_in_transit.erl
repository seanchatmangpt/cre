%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Freight In Transit Subprocess - YAWL Order Fulfillment Pattern
%%
%% This module implements the Freight In Transit subprocess from the CAISE 2013 paper.
%% The Transit workflow is modeled as a Petri net using gen_pnet:
%% - Places (circles in YAWL) = Conditions/states
%% - Transitions (boxes in YAWL) = Tasks
%% - Tokens = Flow of control
%% - Arcs = Flow relations
%%
%% Petri Net Structure:
%% PLACES:                          TRANSITIONS:
%% p_input                          t_start_tracking
%% p_tracking_started               t_monitor_transit
%% p_in_transit                     t_update_location
%% p_location_updated               t_check_delays
%% p_delay_checked                  t_notify_delay
%% p_delay_notified                 t_complete_transit
%% p_transit_complete               t_complete
%% p_output
%%
%% Key Patterns:
%% - WCP-25 (Interleaved Loop) - Continuous monitoring
%% - WCP-16 (Deferred Choice) - Delay notification decision
%%
%% @reference Supporting Risk-Informed Decisions during Business Process Execution (CAISE 2013)
%% @end
%%--------------------------------------------------------------------

-module(freight_in_transit).
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
-export([new/2, start/2, run/2, get_state/1, add_tracking_event/2]).
-export([start_tracking/1, monitor_transit/1, update_location/1,
         check_delays/1, notify_delay/1, complete_transit/1]).

%% Include shared types
-include_lib("order_fulfillment_types.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(transit_state, {
    shipment :: #shipment{} | undefined,
    order :: #order{} | undefined,
    tracking_events = [] :: list(#tracking_event{}),
    current_location = <<"Origin Facility">> :: binary(),
    transit_started = false :: boolean(),
    delay_detected = false :: boolean(),
    log_id :: binary() | undefined,
    started_at :: integer() | undefined
}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type transit_state() :: #transit_state{}.
-export_type([transit_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Freight In Transit Petri net state.
%% @end
%%--------------------------------------------------------------------
-spec new(Shipment :: #shipment{}, Order :: #order{}) -> #transit_state{}.
new(Shipment, Order) when is_record(Shipment, shipment), is_record(Order, order) ->
    LogId = generate_log_id(),
    #transit_state{
        shipment = Shipment,
        order = Order,
        current_location = Shipment#shipment.origin,
        log_id = LogId,
        started_at = erlang:system_time(millisecond)
    };
new(ShipmentMap, OrderMap) when is_map(ShipmentMap), is_map(OrderMap) ->
    Shipment = #shipment{
        shipment_id = maps_get_default(ShipmentMap, <<"shipment_id">>, generate_shipment_id()),
        order_id = maps_get_default(ShipmentMap, <<"order_id">>, <<"UNKNOWN-ORDER">>),
        carrier = maps_get_default(ShipmentMap, <<"carrier">>, <<"DEFAULT-CARRIER">>),
        tracking_number = maps_get_default(ShipmentMap, <<"tracking_number">>, generate_tracking_number()),
        shipping_method = maps_get_default(ShipmentMap, <<"shipping_method">>, ltl),
        origin = maps_get_default(ShipmentMap, <<"origin">>, <<"WAREHOUSE-001">>),
        destination = maps_get_default(ShipmentMap, <<"destination">>, <<"UNKNOWN">>),
        current_location = undefined,
        status = picked_up,
        estimated_delivery = undefined,
        actual_delivery = undefined,
        weight = maps_get_default(ShipmentMap, <<"weight">>, 0.0),
        dimensions = undefined
    },
    Order = #order{
        order_id = maps_get_default(OrderMap, <<"order_id">>, <<"UNKNOWN-ORDER">>),
        customer_id = maps_get_default(OrderMap, <<"customer_id">>, <<"CUST-UNKNOWN">>),
        customer_name = maps_get_default(OrderMap, <<"customer_name">>, <<"Unknown Customer">>),
        customer_email = maps_get_default(OrderMap, <<"customer_email">>, <<"unknown@example.com">>),
        items = [],
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
%% @doc Starts the Freight In Transit workflow.
%% @end
%%--------------------------------------------------------------------
-spec start(Shipment :: #shipment{} | map(), Order :: #order{} | map()) ->
          {ok, pid()} | {error, term()}.
start(Shipment, Order) ->
    TransitState = new(Shipment, Order),
    gen_pnet:start_link(?MODULE, TransitState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Freight In Transit workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Shipment :: #shipment{} | map(), Order :: #order{} | map()) ->
          {ok, #shipment{}} | {error, term()}.
run(Shipment, Order) ->
    case start(Shipment, Order) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 60000) of
                {ok, #transit_state{shipment = FinalShipment}} ->
                    gen_pnet:stop(Pid),
                    {ok, FinalShipment};
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Transit workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, #transit_state{}} | {error, term()}.
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Adds a tracking event to the transit workflow.
%% @end
%%--------------------------------------------------------------------
-spec add_tracking_event(Pid :: pid(), Event :: map()) -> ok.
add_tracking_event(Pid, Event) ->
    gen_pnet:cast(Pid, {add_tracking_event, Event}).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Transit Petri net.
%% @end
%%--------------------------------------------------------------------
place_lst() ->
    [
        'p_input',
        'p_tracking_started',
        'p_in_transit',
        'p_location_updated',
        'p_delay_checked',
        'p_delay_notified',
        'p_transit_complete',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Transit Petri net.
%% @end
%%--------------------------------------------------------------------
trsn_lst() ->
    [
        't_start_tracking',
        't_monitor_transit',
        't_update_location',
        't_check_delays',
        't_notify_delay',
        't_complete_transit',
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
preset('t_start_tracking') -> ['p_input'];
preset('t_monitor_transit') -> ['p_tracking_started'];
preset('t_update_location') -> ['p_in_transit'];
preset('t_check_delays') -> ['p_location_updated'];
preset('t_notify_delay') -> ['p_delay_checked'];
preset('t_complete_transit') -> ['p_delay_checked', 'p_in_transit'];
preset('t_complete') -> ['p_transit_complete'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%% @end
%%--------------------------------------------------------------------
is_enabled('t_notify_delay', _Mode, #transit_state{delay_detected = true}) ->
    true;
is_enabled('t_notify_delay', _Mode, _State) ->
    false;

is_enabled('t_complete_transit', _Mode, #transit_state{current_location = Loc, shipment = #shipment{destination = Dest}}) ->
    %% Complete transit when location matches destination
    Loc =:= Dest;
is_enabled('t_complete_transit', _Mode, _State) ->
    false;

is_enabled(_Trsn, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: #transit_state{}) ->
          {produce, map()} | abort.

fire('t_start_tracking', #{'p_input' := [start]}, State) ->
    Shipment = State#transit_state.shipment,
    log_event(State, <<"Transit">>, <<"TrackingStarted">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"tracking_number">> => Shipment#shipment.tracking_number,
        <<"carrier">> => Shipment#shipment.carrier
    }),
    {produce, #{'p_tracking_started' => [start]}};

fire('t_monitor_transit', #{'p_tracking_started' := [start]}, State) ->
    Shipment = State#transit_state.shipment,
    monitor_transit(State),
    UpdatedShipment = Shipment#shipment{status = in_transit},
    State1 = State#transit_state{
        shipment = UpdatedShipment,
        transit_started = true
    },
    log_event(State, <<"Transit">>, <<"MonitoringStarted">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id
    }),
    {produce, #{'p_in_transit' => [start]}, State1};

fire('t_update_location', #{'p_in_transit' := [start]}, State) ->
    Shipment = State#transit_state.shipment,
    #{location := NewLocation} = update_location(State),
    TrackingEvent = #tracking_event{
        event_id = generate_event_id(),
        timestamp = erlang:system_time(millisecond),
        location = NewLocation,
        status = <<"in_transit">>,
        description = <<"Shipment in transit">>
    },
    UpdatedShipment = Shipment#shipment{current_location = NewLocation},
    State1 = State#transit_state{
        shipment = UpdatedShipment,
        current_location = NewLocation,
        tracking_events = State#transit_state.tracking_events ++ [TrackingEvent]
    },
    log_event(State, <<"Transit">>, <<"LocationUpdated">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"location">> => NewLocation
    }),
    {produce, #{'p_location_updated' => [start]}, State1};

fire('t_check_delays', #{'p_location_updated' := [start]}, State) ->
    Shipment = State#transit_state.shipment,
    #{delay_detected := DelayDetected} = check_delays(State),
    State1 = State#transit_state{delay_detected = DelayDetected},
    log_event(State, <<"Transit">>, <<"DelayChecked">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"delay_detected">> => DelayDetected
    }),
    {produce, #{'p_delay_checked' => [start]}, State1};

fire('t_notify_delay', #{'p_delay_checked' := [start]}, State) ->
    Shipment = State#transit_state.shipment,
    Order = State#transit_state.order,
    notify_delay(State),
    log_event(State, <<"Transit">>, <<"DelayNotified">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"customer_email">> => Order#order.customer_email
    }),
    {produce, #{'p_delay_notified' => [start]}, State};

fire('t_complete_transit', #{'p_delay_checked' := [start]}, State) ->
    %% Transit complete - arrived at destination
    Shipment = State#transit_state.shipment,
    complete_transit(State),
    UpdatedShipment = Shipment#shipment{
        status = out_for_delivery,
        current_location = Shipment#shipment.destination
    },
    State1 = State#transit_state{shipment = UpdatedShipment},
    log_event(State, <<"Transit">>, <<"TransitComplete">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"destination">> => Shipment#shipment.destination
    }),
    {produce, #{'p_transit_complete' => [start]}, State1};

fire('t_complete', #{'p_transit_complete' := [start]}, State) ->
    Shipment = State#transit_state.shipment,
    log_event(State, <<"Transit">>, <<"InTransitComplete">>, #{
        <<"shipment_id">> => Shipment#shipment.shipment_id,
        <<"tracking_events">> => length(State#transit_state.tracking_events)
    }),
    {produce, #{'p_output' => [start]}, State};

fire(_Trsn, _Mode, _State) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for custom processing.
%% @end
%%--------------------------------------------------------------------
trigger(Place, _Token, State) ->
    log_event(State, <<"Transit">>, <<"PlaceEntered">>, #{
        <<"place">> => atom_to_binary(Place, utf8)
    }),
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
init(TransitState) ->
    %% Start periodic location updates
    erlang:send_after(5000, self(), update_location),
    {ok, TransitState}.

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
handle_cast({add_tracking_event, Event}, NetState) ->
    TrackingEvent = #tracking_event{
        event_id = maps_get_default(Event, <<"event_id">>, generate_event_id()),
        timestamp = maps_get_default(Event, <<"timestamp">>, erlang:system_time(millisecond)),
        location = maps_get_default(Event, <<"location">>, <<"Unknown">>),
        status = maps_get_default(Event, <<"status">>, <<"updated">>),
        description = maps_get_default(Event, <<"description">>, <<"">>)
    },
    {noreply, NetState#transit_state{
        tracking_events = NetState#transit_state.tracking_events ++ [TrackingEvent]
    }};
handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-call/cast messages.
%% @end
%%--------------------------------------------------------------------
handle_info(update_location, NetState) ->
    %% Simulate location updates during transit
    case NetState#transit_state.transit_started of
        true when NetState#transit_state.current_location =/= NetState#transit_state.shipment#shipment.destination ->
            %% Generate next location update
            Locations = [
                <<"Regional Distribution Center">>,
                <<"Local Hub">>,
                <<"Delivery Facility">>
            ],
            CurrentIdx = case lists:keyfind(NetState#transit_state.current_location, 1, lists:enumerate(Locations)) of
                {_, Idx} -> Idx;
                false -> 0
            end,
            _NextLocation = case CurrentIdx + 1 > length(Locations) of
                true -> NetState#transit_state.shipment#shipment.destination;
                false -> lists:nth(CurrentIdx + 1, Locations)
            end,
            %% Trigger the update_location transition
            gen_pnet:enable(NetState, 't_update_location');
        false ->
            ok
    end,
    %% Schedule next update
    erlang:send_after(5000, self(), update_location),
    {noreply, NetState};
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
    LogId = State#transit_state.log_id,
    case LogId of
        undefined -> ok;
        _ ->
            ShipmentId = case State#transit_state.shipment of
                undefined -> <<"UNKNOWN">>;
                S -> S#shipment.shipment_id
            end,
            yawl_xes:log_case_complete(LogId, ShipmentId, #{
                <<"tracking_events">> => length(State#transit_state.tracking_events),
                <<"delay_detected">> => State#transit_state.delay_detected
            })
    end,
    ok.

%%====================================================================
%% Task Implementation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts tracking the shipment.
%% @end
%%--------------------------------------------------------------------
-spec start_tracking(State :: #transit_state{}) -> ok.
start_tracking(#transit_state{shipment = Shipment}) ->
    {ok, Shipment#shipment.shipment_id}.

%%--------------------------------------------------------------------
%% @doc Monitors the shipment during transit.
%% @end
%%--------------------------------------------------------------------
-spec monitor_transit(State :: #transit_state{}) -> ok.
monitor_transit(#transit_state{shipment = Shipment}) ->
    %% In production, integrate with carrier APIs for real-time tracking
    {ok, Shipment#shipment.tracking_number}.

%%--------------------------------------------------------------------
%% @doc Updates the current location of the shipment.
%% @end
%%--------------------------------------------------------------------
-spec update_location(State :: #transit_state{}) -> map().
update_location(#transit_state{shipment = Shipment, current_location = Current}) ->
    %% Simulate location progression
    Locations = [
        Shipment#shipment.origin,
        <<"Regional Distribution Center">>,
        <<"Local Hub">>,
        <<"Delivery Facility">>,
        Shipment#shipment.destination
    ],
    NextLocation = case Current of
        Loc when Loc =:= Shipment#shipment.origin -> lists:nth(2, Locations);
        Loc ->
            case lists:nth(1, lists:dropwhile(fun(L) -> L =/= Loc end, Locations)) of
                Current ->
                    Idx = case lists:keyfind(Current, 1, lists:enumerate(Locations)) of
                        {_, I} -> I;
                        false -> 1
                    end,
                    case Idx + 1 > length(Locations) of
                        true -> Shipment#shipment.destination;
                        false -> lists:nth(Idx + 1, Locations)
                    end;
                _ -> Shipment#shipment.destination
            end
    end,
    #{
        location => NextLocation,
        timestamp => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Checks for delays in shipment.
%% @end
%%--------------------------------------------------------------------
-spec check_delays(State :: #transit_state{}) -> map().
check_delays(#transit_state{shipment = _Shipment, started_at = StartedAt}) ->
    %% Simulate delay detection based on estimated vs actual time
    Elapsed = erlang:system_time(millisecond) - StartedAt,
    EstimatedTime = 3600000,  %% 1 hour in ms
    DelayDetected = Elapsed > EstimatedTime,
    #{
        delay_detected => DelayDetected,
        elapsed => Elapsed,
        estimated => EstimatedTime
    }.

%%--------------------------------------------------------------------
%% @doc Notifies customer about delays.
%% @end
%%--------------------------------------------------------------------
-spec notify_delay(State :: #transit_state{}) -> ok.
notify_delay(#transit_state{order = Order, shipment = Shipment}) ->
    %% In production, send delay notification email
    {ok, {
        Order#order.customer_email,
        Shipment#shipment.shipment_id,
        <<"Your shipment is experiencing delays">>
    }}.

%%--------------------------------------------------------------------
%% @doc Completes the transit phase.
%% @end
%%--------------------------------------------------------------------
-spec complete_transit(State :: #transit_state{}) -> ok.
complete_transit(#transit_state{shipment = Shipment}) ->
    {ok, Shipment#shipment.shipment_id}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
log_event(#transit_state{log_id = undefined}, _, _, _) ->
    ok;
log_event(#transit_state{log_id = LogId}, ConceptName, LifecycleTransition, Data) ->
    yawl_xes:log_event(LogId, ConceptName, LifecycleTransition, Data).

%% @private
generate_log_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"transit_log_", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_shipment_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"SHIP-", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_tracking_number() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"1Z", (integer_to_binary(Timestamp))/binary>>.

%% @private
generate_event_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"EVENT-", (integer_to_binary(Timestamp))/binary>>.

%% @private
maps_get_default(Map, Key, Default) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Value -> Value
    end.

%% @private
wait_for_completion(Pid, Timeout) ->
    wait_for_completion(Pid, Timeout, 0).

wait_for_completion(_Pid, Timeout, Elapsed) when Elapsed >= Timeout ->
    {error, timeout};
wait_for_completion(Pid, Timeout, Elapsed) ->
    case get_state(Pid) of
        {ok, #transit_state{shipment = #shipment{status = out_for_delivery}}} ->
            get_state(Pid);
        {ok, #transit_state{current_location = Loc, shipment = #shipment{destination = Dest}}} when Loc =:= Dest ->
            get_state(Pid);
        {ok, _} ->
            timer:sleep(500),
            wait_for_completion(Pid, Timeout, Elapsed + 500);
        {error, Reason} ->
            {error, Reason}
    end.
