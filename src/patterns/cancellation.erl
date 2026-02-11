%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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
%% @doc Cancellation Region Pattern Utilities for YAWL workflows.
%%
%% This module provides utility functions for defining and managing
%% cancellation regions in Petri net based workflows. Cancellation regions
%% are sets of places and/or transitions that can be cancelled together
%% when a trigger event occurs.
%%
%% == Cancellation Patterns (from Workflow Patterns 2003) ==
%%
%% This module supports the following cancellation patterns:
%%
%% <strong>P19: Cancel Activity</strong> - Cancel a single activity
%%
%% <strong>P20: Cancel Case</strong> - Cancel entire workflow case
%%
%% <strong>P25: Cancel Region</strong> - Cancel activities within a region
%%
%% == Basic Usage ==
%%
%% ```erlang
%% > %% Define a cancellation region
%% > Region = cancellation:define_region(order_fulfillment,
%% ..     [p_payment, p_shipping, p_notification]).
%% #{name => order_fulfillment,
%%   places => [p_payment, p_shipping, p_notification],
%%   transitions => [],
%%   parent => undefined}
%%
%% > %% Check if a place is in the region
%% > cancellation:region_contains(p_payment, Region).
%% true
%%
%% > %% Create a marking
%% > Marking = pnet_marking:new([p_payment, p_shipping, p_notification]),
%% > Marking1 = pnet_marking:set(Marking, p_payment, [processing]).
%%
%% > %% Check for active tokens
%% > cancellation:tokens_in_region(Marking1, Region).
%% true
%%
%% > %% Clear all tokens in the region
%% > Marking2 = cancellation:clear_region_tokens(Marking1, Region).
%% > {ok, Tokens} = pnet_marking:get(Marking2, p_payment).
%% > Tokens.
%% []
%% '''
%%
%% == Cancel Activity in Region ==
%%
%% ```erlang
%% > Region = cancellation:define_region(my_region, [p1, p2, p3]),
%% > Marking = pnet_marking:new([p1, p2, p3, p4]),
%% > Marking1 = pnet_marking:set(Marking, p1, [active]),
%% > Marking2 = pnet_marking:set(Marking1, p_cancel_trigger, [trigger]),
%%
%% > %% Cancel all activities in region when trigger fires
%% > Marking3 = cancellation:cancel_activity_region(
%% ..     Marking2, p_cancel_trigger, Region).
%% > {ok, T1} = pnet_marking:get(Marking3, p1).
%% > T1.
%% []
%% '''
%%
%% == Cancel Entire Workflow Case ==
%%
%% ```erlang
%% > %% Define all places as one region for complete cancellation
%% > AllPlaces = [p_start, p_task1, p_task2, p_end],
%% > Region = cancellation:define_region(entire_workflow, AllPlaces),
%% > Marking = pnet_marking:new(AllPlaces),
%%
%% > %% Cancel everything
%% > MarkingCancelled = cancellation:cancel_case_region(Marking, Region).
%% > cancellation:tokens_in_region(MarkingCancelled, Region).
%% false
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(cancellation).

-moduledoc """
Cancellation region pattern utilities for YAWL workflows.

Provides functions for defining cancellation regions and managing token
cancellation within those regions. Supports nested regions and partial
cancellation for complex workflow exception handling.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Region definition
-export([define_region/2, define_region/3]).

%% Cancellation operations
-export([cancel_activity_region/3, cancel_case_region/2]).

%% Token inspection and manipulation
-export([tokens_in_region/2, clear_region_tokens/2]).

%% Region membership checking
-export([region_contains/2]).

%% Region information
-export([region_places/1, region_transitions/1, region_name/1]).

%% XES logging integration
-export([log_cancel_event/2, log_cancel_event/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net workflow.
%%
%% Places are locations where tokens reside.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net workflow.
%%
%% Transitions consume and produce tokens.
%%--------------------------------------------------------------------
-type transition() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net workflow.
%%
%% Tokens can be any Erlang term.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Uses pnet_marking format: maps places to token lists.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A cancellation region definition.
%%
%% Contains a name, list of places, list of transitions, and optional parent.
%%--------------------------------------------------------------------
-type region() :: #{
    name => atom(),
    places => [place()],
    transitions => [transition()],
    parent => atom() | undefined
}.

%%--------------------------------------------------------------------
%% @doc A region map for multiple regions.
%%
%% Maps region names to their definitions.
%%--------------------------------------------------------------------
-type region_map() :: #{atom() => region()}.

%% Export types
-export_type([region/0, region_map/0, place/0, transition/0,
              token/0, marking/0]).

%%====================================================================
%% API Functions - Region Definition
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Defines a cancellation region with places and transitions.
%%
%% Creates a region definition with a name and combined list of places
%% and transitions. Both are stored in their respective fields for
%% precise cancellation control.
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     order_region,
%% ..     [p_payment, p_shipping],  %% places
%% ..     [t_charge, t_ship]         %% transitions
%% .. ).
%% #{name => order_region,
%%   places => [p_payment, p_shipping],
%%   transitions => [t_charge, t_ship],
%%   parent => undefined}
%% ```
%%
%% @param Name Unique name for the region
%% @param Places List of places in the region
%% @param Transitions List of transitions in the region
%% @return Region definition map
%%
%% @end
%%--------------------------------------------------------------------
-spec define_region(Name :: atom(),
                    Places :: [place()],
                    Transitions :: [transition()]) -> region().

define_region(Name, Places, Transitions)
    when is_atom(Name), is_list(Places), is_list(Transitions) ->
    #{
        name => Name,
        places => Places,
        transitions => Transitions,
        parent => undefined
    }.

%%--------------------------------------------------------------------
%% @doc Defines a cancellation region with places only.
%%
%% Convenience function that creates a region containing only places.
%% Transitions list will be empty.
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     payment_region,
%% ..     [p_validate, p_charge, p_confirm]
%% .. ).
%% #{name => payment_region,
%%   places => [p_validate, p_charge, p_confirm],
%%   transitions => [],
%%   parent => undefined}
%% ```
%%
%% @param Name Unique name for the region
%% @param Places List of places in the region
%% @return Region definition map
%%
%% @end
%%--------------------------------------------------------------------
-spec define_region(Name :: atom(), Places :: [place()]) -> region().

define_region(Name, Places) when is_atom(Name), is_list(Places) ->
    define_region(Name, Places, []).

%%====================================================================
%% API Functions - Cancellation Operations
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cancels all activities in a region when a trigger place has tokens.
%%
%% Checks if the trigger place has tokens, and if so, clears all tokens
%% from places and transitions within the region. The trigger itself
%% is also consumed.
%%
%% This implements the "Cancel Region" pattern (P25) from the workflow
%% patterns catalog.
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     shipping_region,
%% ..     [p_pick, p_pack, p_label]
%% .. ),
%% > Marking = pnet_marking:new([p_cancel, p_pick, p_pack, p_label]),
%% > Marking1 = pnet_marking:set(Marking, p_cancel, [fire]),
%% > Marking2 = pnet_marking:set(Marking1, p_pick, [working]),
%%
%% > %% Trigger fires - cancel region
%% > Marking3 = cancellation:cancel_activity_region(
%% ..     Marking2, p_cancel, Region).
%% > {ok, Tokens} = pnet_marking:get(Marking3, p_pick).
%% > Tokens.
%% []
%% ```
%%
%% @param Marking The current marking
%% @param TriggerPlace Place that triggers cancellation when it has tokens
%% @param Region The region to cancel
%% @return Updated marking with region cancelled if triggered
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_activity_region(Marking :: marking(),
                              TriggerPlace :: place(),
                              Region :: region()) -> marking().

cancel_activity_region(Marking, TriggerPlace, Region) ->
    case pnet_marking:get(Marking, TriggerPlace) of
        {ok, []} ->
            %% No trigger - return unchanged marking
            Marking;
        {ok, _TriggerTokens} ->
            %% Trigger present - cancel the region
            %% First, consume the trigger tokens
            Marking1 = pnet_marking:set(Marking, TriggerPlace, []),
            %% Then clear all places in the region
            clear_region_tokens(Marking1, Region)
    end.

%%--------------------------------------------------------------------
%% @doc Cancels the entire workflow case by clearing all regions.
%%
%% Clears all tokens from all places in the specified region. This
%% effectively terminates the workflow case within that region.
%%
%% This implements the "Cancel Case" pattern (P20) from the workflow
%% patterns catalog.
%%
%% ```erlang
%% > AllPlaces = [p_start, p_auth, p_process, p_save, p_end],
%% > Region = cancellation:define_region(entire_case, AllPlaces),
%% > Marking = pnet_marking:new(AllPlaces),
%% > Marking1 = pnet_marking:set(Marking, p_auth, [checking]),
%% > Marking2 = pnet_marking:set(Marking1, p_process, [running]),
%%
%% > %% Cancel entire case
%% > Marking3 = cancellation:cancel_case_region(Marking2, Region).
%% > cancellation:tokens_in_region(Marking3, Region).
%% false
%% ```
%%
%% @param Marking The current marking
%% @param Region The region defining the entire case to cancel
%% @return Updated marking with all tokens cleared
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_case_region(Marking :: marking(), Region :: region()) -> marking().

cancel_case_region(Marking, Region) ->
    clear_region_tokens(Marking, Region).

%%====================================================================
%% API Functions - Token Inspection and Manipulation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if there are active tokens within a region.
%%
%% Returns true if any place in the region has non-empty tokens.
%% Useful for determining if a region is active before cancellation.
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     active_tasks,
%% ..     [p_task1, p_task2, p_task3]
%% .. ),
%% > Marking = pnet_marking:new([p_task1, p_task2, p_task3]),
%% > Marking1 = pnet_marking:set(Marking, p_task2, [running]),
%%
%% > cancellation:tokens_in_region(Marking1, Region).
%% true
%%
%% > Marking2 = cancellation:clear_region_tokens(Marking1, Region),
%% > cancellation:tokens_in_region(Marking2, Region).
%% false
%% ```
%%
%% @param Marking The current marking
%% @param Region The region to check
%% @return true if any tokens exist in region places, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec tokens_in_region(Marking :: marking(), Region :: region()) -> boolean().

tokens_in_region(Marking, Region) ->
    Places = region_places(Region),
    lists:any(fun(Place) ->
        case pnet_marking:get(Marking, Place) of
            {ok, []} -> false;
            {ok, Tokens} when is_list(Tokens) -> Tokens =/= []
        end
    end, Places).

%%--------------------------------------------------------------------
%% @doc Removes all tokens from places in a region.
%%
%% Clears all tokens from every place defined in the region.
%% The places themselves remain in the marking (as empty lists).
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     cleanup_region,
%% ..     [p_temp1, p_temp2, p_temp3]
%% .. ),
%% > Marking = pnet_marking:new([p_temp1, p_temp2, p_temp3]),
%% > Marking1 = pnet_marking:set(Marking, p_temp1, [a]),
%% > Marking2 = pnet_marking:set(Marking1, p_temp2, [b, c]),
%% > Marking3 = pnet_marking:set(Marking2, p_temp3, [d]),
%%
%% > %% Clear all region tokens
%% > Marking4 = cancellation:clear_region_tokens(Marking3, Region).
%% > {ok, T1} = pnet_marking:get(Marking4, p_temp1).
%% > T1.
%% []
%% '''
%%
%% @param Marking The current marking
%% @param Region The region to clear
%% @return Updated marking with region tokens cleared
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_region_tokens(Marking :: marking(), Region :: region()) -> marking().

clear_region_tokens(Marking, Region) ->
    Places = region_places(Region),
    lists:foldl(fun(Place, AccMarking) ->
        pnet_marking:set(AccMarking, Place, [])
    end, Marking, Places).

%%====================================================================
%% API Functions - Region Membership
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a place or transition is in a region.
%%
%% Returns true if the element (place or transition atom) is explicitly
%% listed in the region's places or transitions.
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     my_region,
%% ..     [p1, p2, p3],
%% ..     [t1, t2]
%% .. ),
%% > cancellation:region_contains(p2, Region).
%% true
%% > cancellation:region_contains(t1, Region).
%% true
%% > cancellation:region_contains(p4, Region).
%% false
%% '''
%%
%% @param Element The place or transition atom to check
%% @param Region The region definition
%% @return true if element is in region, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec region_contains(Element :: place() | transition(), Region :: region()) -> boolean().

region_contains(Element, Region) when is_atom(Element) ->
    Places = region_places(Region),
    Transitions = region_transitions(Region),
    lists:member(Element, Places) orelse lists:member(Element, Transitions).

%%====================================================================
%% API Functions - Region Information
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts the name from a region definition.
%%
%% ```erlang
%% > Region = cancellation:define_region(test_region, [p1, p2]),
%% > cancellation:region_name(Region).
%% test_region
%% '''
%%
%% @param Region The region definition
%% @return The region name
%%
%% @end
%%--------------------------------------------------------------------
-spec region_name(Region :: region()) -> atom().

region_name(#{name := Name}) -> Name.

%%--------------------------------------------------------------------
%% @doc Extracts the list of places from a region definition.
%%
%% ```erlang
%% > Region = cancellation:define_region(test_region, [p1, p2, p3]),
%% > cancellation:region_places(Region).
%% [p1, p2, p3]
%% '''
%%
%% @param Region The region definition
%% @return List of places in the region
%%
%% @end
%%--------------------------------------------------------------------
-spec region_places(Region :: region()) -> [place()].

region_places(#{places := Places}) when is_list(Places) -> Places;
region_places(_) -> [].

%%--------------------------------------------------------------------
%% @doc Extracts the list of transitions from a region definition.
%%
%% ```erlang
%% > Region = cancellation:define_region(
%% ..     test_region,
%% ..     [p1, p2],
%% ..     [t1, t2, t3]
%% .. ),
%% > cancellation:region_transitions(Region).
%% [t1, t2, t3]
%% '''
%%
%% @param Region The region definition
%% @return List of transitions in the region
%%
%% @end
%%--------------------------------------------------------------------
-spec region_transitions(Region :: region()) -> [transition()].

region_transitions(#{transitions := Transitions}) when is_list(Transitions) -> Transitions;
region_transitions(_) -> [].

%%====================================================================
%% XES Logging Integration
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Logs a cancellation event to XES.
%%
%% Logs when a region, activity, or case is cancelled for
%% process mining and audit trail purposes.
%%
%% ```erlang
%% > Region = cancellation:define_region(my_region, [p1, p2]),
%% > cancellation:log_cancel_event(Region, cancel_activity, p1).
%% > cancellation:log_cancel_event(Region, cancel_case, undefined).
%% '''
%%
%% @param Region The region being cancelled
%% @param CancelType Type of cancellation (cancel_activity, cancel_case, cancel_region)
%% @param Target The specific target being cancelled (place, transition, or undefined)
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec log_cancel_event(region(), atom()) -> ok.
log_cancel_event(Region, CancelType) ->
    log_cancel_event(Region, CancelType, undefined).

%%--------------------------------------------------------------------
%% @doc Logs a cancellation event to XES with target.
%%
%% @param Region The region being cancelled
%% @param CancelType Type of cancellation
%% @param Target The specific target being cancelled
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec log_cancel_event(region(), atom(), term()) -> ok.
log_cancel_event(Region, CancelType, Target) ->
    case whereis(yawl_xes) of
        undefined ->
            %% XES logger not available, skip logging
            ok;
        _Pid ->
            RegionName = region_name(Region),
            EventName = <<"Cancellation_", (atom_to_binary(CancelType))/binary>>,
            try
                yawl_xes:log_event(
                    <<"yawl_default_log">>,
                    EventName,
                    <<"abort">>,
                    #{
                        <<"region_name">> => atom_to_binary(RegionName),
                        <<"cancel_type">> => atom_to_binary(CancelType),
                        <<"target">> => format_target(Target),
                        <<"places_affected">> => length(region_places(Region))
                    },
                    undefined
                )
            catch
                _:_ ->
                    %% Silent fail if XES logging fails
                    ok
            end
    end.

%% @private
format_target(undefined) -> <<"undefined">>;
format_target(Target) when is_atom(Target) -> atom_to_binary(Target);
format_target(Target) when is_binary(Target) -> Target;
format_target(Target) -> list_to_binary(io_lib:format("~p", [Target])).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Test define_region/2
%%--------------------------------------------------------------------
define_region_places_test() ->
    Region = define_region(test_region, [p1, p2, p3]),
    ?assertEqual(test_region, region_name(Region)),
    ?assertEqual([p1, p2, p3], region_places(Region)),
    ?assertEqual([], region_transitions(Region)).

%%--------------------------------------------------------------------
%% @doc Test define_region/3
%%--------------------------------------------------------------------
define_region_full_test() ->
    Region = define_region(full_region, [p1, p2], [t1, t2]),
    ?assertEqual(full_region, region_name(Region)),
    ?assertEqual([p1, p2], region_places(Region)),
    ?assertEqual([t1, t2], region_transitions(Region)).

%%--------------------------------------------------------------------
%% @doc Test region_contains for places
%%--------------------------------------------------------------------
region_contains_places_test() ->
    Region = define_region(test_region, [p1, p2, p3]),
    ?assertEqual(true, region_contains(p1, Region)),
    ?assertEqual(true, region_contains(p2, Region)),
    ?assertEqual(true, region_contains(p3, Region)),
    ?assertEqual(false, region_contains(p4, Region)),
    ?assertEqual(false, region_contains(t1, Region)).

%%--------------------------------------------------------------------
%% @doc Test region_contains for transitions
%%--------------------------------------------------------------------
region_contains_transitions_test() ->
    Region = define_region(test_region, [p1, p2], [t1, t2]),
    ?assertEqual(true, region_contains(p1, Region)),
    ?assertEqual(true, region_contains(t1, Region)),
    ?assertEqual(true, region_contains(t2, Region)),
    ?assertEqual(false, region_contains(t3, Region)).

%%--------------------------------------------------------------------
%% @doc Test tokens_in_region with empty marking
%%--------------------------------------------------------------------
tokens_in_region_empty_test() ->
    Region = define_region(test_region, [p1, p2, p3]),
    Marking = pnet_marking:new([p1, p2, p3]),
    ?assertEqual(false, tokens_in_region(Marking, Region)).

%%--------------------------------------------------------------------
%% @doc Test tokens_in_region with tokens
%%--------------------------------------------------------------------
tokens_in_region_present_test() ->
    Region = define_region(test_region, [p1, p2, p3]),
    Marking = pnet_marking:new([p1, p2, p3]),
    Marking1 = pnet_marking:set(Marking, p2, [token]),
    ?assertEqual(true, tokens_in_region(Marking1, Region)).

%%--------------------------------------------------------------------
%% @doc Test tokens_in_region with multiple tokens
%%--------------------------------------------------------------------
tokens_in_region_multiple_test() ->
    Region = define_region(test_region, [p1, p2]),
    Marking = pnet_marking:new([p1, p2]),
    Marking1 = pnet_marking:set(Marking, p1, [a, b]),
    Marking2 = pnet_marking:set(Marking1, p2, [c]),
    ?assertEqual(true, tokens_in_region(Marking2, Region)).

%%--------------------------------------------------------------------
%% @doc Test clear_region_tokens
%%--------------------------------------------------------------------
clear_region_tokens_test() ->
    Region = define_region(test_region, [p1, p2, p3]),
    Marking = pnet_marking:new([p1, p2, p3, p4]),
    Marking1 = pnet_marking:set(Marking, p1, [a]),
    Marking2 = pnet_marking:set(Marking1, p2, [b, c]),
    Marking3 = pnet_marking:set(Marking2, p3, [d]),
    Marking4 = pnet_marking:set(Marking3, p4, [e]),  %% Not in region

    Result = clear_region_tokens(Marking4, Region),

    ?assertEqual({ok, []}, pnet_marking:get(Result, p1)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p2)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p3)),
    ?assertEqual({ok, [e]}, pnet_marking:get(Result, p4)).

%%--------------------------------------------------------------------
%% @doc Test cancel_activity_region with trigger present
%%--------------------------------------------------------------------
cancel_activity_region_triggered_test() ->
    Region = define_region(cancel_region, [p1, p2, p3]),
    Marking = pnet_marking:new([p_trigger, p1, p2, p3]),
    Marking1 = pnet_marking:set(Marking, p_trigger, [fire]),
    Marking2 = pnet_marking:set(Marking1, p1, [working]),
    Marking3 = pnet_marking:set(Marking2, p2, [processing]),

    Result = cancel_activity_region(Marking3, p_trigger, Region),

    ?assertEqual({ok, []}, pnet_marking:get(Result, p_trigger)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p1)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p2)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p3)).

%%--------------------------------------------------------------------
%% @doc Test cancel_activity_region without trigger
%%--------------------------------------------------------------------
cancel_activity_region_no_trigger_test() ->
    Region = define_region(cancel_region, [p1, p2]),
    Marking = pnet_marking:new([p_trigger, p1, p2]),
    Marking1 = pnet_marking:set(Marking, p1, [working]),
    Marking2 = pnet_marking:set(Marking1, p2, [processing]),

    Result = cancel_activity_region(Marking2, p_trigger, Region),

    %% No trigger - marking unchanged
    ?assertEqual({ok, []}, pnet_marking:get(Result, p_trigger)),
    ?assertEqual({ok, [working]}, pnet_marking:get(Result, p1)),
    ?assertEqual({ok, [processing]}, pnet_marking:get(Result, p2)).

%%--------------------------------------------------------------------
%% @doc Test cancel_case_region
%%--------------------------------------------------------------------
cancel_case_region_test() ->
    Region = define_region(entire_case, [p_start, p_auth, p_work, p_end]),
    Marking = pnet_marking:new([p_start, p_auth, p_work, p_end]),
    Marking1 = pnet_marking:set(Marking, p_start, [active]),
    Marking2 = pnet_marking:set(Marking1, p_auth, [checking]),
    Marking3 = pnet_marking:set(Marking2, p_work, [running]),

    Result = cancel_case_region(Marking3, Region),

    ?assertEqual(false, tokens_in_region(Result, Region)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p_start)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p_auth)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p_work)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p_end)).

%%--------------------------------------------------------------------
%% @doc Test clear_region_tokens on already empty region
%%--------------------------------------------------------------------
clear_region_tokens_empty_test() ->
    Region = define_region(test_region, [p1, p2]),
    Marking = pnet_marking:new([p1, p2]),
    Result = clear_region_tokens(Marking, Region),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p1)),
    ?assertEqual({ok, []}, pnet_marking:get(Result, p2)).

%%--------------------------------------------------------------------
%% @doc Test region_name
%%--------------------------------------------------------------------
region_name_test() ->
    Region = define_region(my_region, [p1]),
    ?assertEqual(my_region, region_name(Region)).

%%--------------------------------------------------------------------
%% @doc Test region_places
%%--------------------------------------------------------------------
region_places_test() ->
    Region = define_region(test_region, [p1, p2, p3]),
    ?assertEqual([p1, p2, p3], region_places(Region)).

%%--------------------------------------------------------------------
%% @doc Test region_transitions
%%--------------------------------------------------------------------
region_transitions_test() ->
    Region = define_region(test_region, [p1], [t1, t2]),
    ?assertEqual([t1, t2], region_transitions(Region)).

%%--------------------------------------------------------------------
%% @doc Test empty region places
%%--------------------------------------------------------------------
region_empty_places_test() ->
    Region = define_region(empty_region, []),
    ?assertEqual([], region_places(Region)),
    ?assertEqual(false, region_contains(p1, Region)).

%%--------------------------------------------------------------------
%% @doc Test order fulfillment carrier timeout scenario
%%--------------------------------------------------------------------
order_fulfillment_scenario_test() ->
    %% Define the carrier timeout cancellation region
    CarrierTimeoutRegion = define_region(
        carrier_timeout,
        [pending_appointment, awaiting_confirmation]
    ),

    %% Simulate order fulfillment marking
    Places = [
        order_received,
        payment_processed,
        pending_appointment,
        awaiting_confirmation,
        carrier_confirmed,
        shipping_scheduled
    ],
    Marking = pnet_marking:new(Places),
    Marking1 = pnet_marking:set(Marking, order_received, [#{order_id => "ORD-123"}]),
    Marking2 = pnet_marking:set(Marking1, payment_processed, [paid]),
    Marking3 = pnet_marking:set(Marking2, pending_appointment, [waiting_for_carrier]),

    %% Initially, tokens are in the region
    ?assertEqual(true, tokens_in_region(Marking3, CarrierTimeoutRegion)),

    %% Carrier timeout occurs - cancel the region
    MarkingAfterCancel = clear_region_tokens(Marking3, CarrierTimeoutRegion),

    %% Verify pending appointment is cleared
    ?assertEqual({ok, []}, pnet_marking:get(MarkingAfterCancel, pending_appointment)),
    ?assertEqual({ok, []}, pnet_marking:get(MarkingAfterCancel, awaiting_confirmation)),

    %% Verify other places preserved
    ?assertEqual({ok, [#{order_id => "ORD-123"}]}, pnet_marking:get(MarkingAfterCancel, order_received)),
    ?assertEqual({ok, [paid]}, pnet_marking:get(MarkingAfterCancel, payment_processed)),

    %% No more tokens in region
    ?assertEqual(false, tokens_in_region(MarkingAfterCancel, CarrierTimeoutRegion)).

%%--------------------------------------------------------------------
%% @doc Test region_contains with atoms that are not in region
%%--------------------------------------------------------------------
region_contains_negative_test() ->
    Region = define_region(small_region, [p1], [t1]),
    ?assertEqual(false, region_contains(undefined, Region)),
    ?assertEqual(false, region_contains('not_in_region', Region)),
    ?assertEqual(false, region_contains(p99, Region)).

%%--------------------------------------------------------------------
%% @doc Test multiple regions with overlapping places
%%--------------------------------------------------------------------
overlapping_regions_test() ->
    Region1 = define_region(region1, [p1, p2, p3]),
    Region2 = define_region(region2, [p2, p3, p4]),

    Marking = pnet_marking:new([p1, p2, p3, p4]),
    Marking1 = pnet_marking:set(Marking, p1, [a]),
    Marking2 = pnet_marking:set(Marking1, p2, [b]),
    Marking3 = pnet_marking:set(Marking2, p3, [c]),
    Marking4 = pnet_marking:set(Marking3, p4, [d]),

    %% Both regions have tokens
    ?assertEqual(true, tokens_in_region(Marking4, Region1)),
    ?assertEqual(true, tokens_in_region(Marking4, Region2)),

    %% Clear region1
    Marking5 = clear_region_tokens(Marking4, Region1),

    %% Region1 now empty
    ?assertEqual(false, tokens_in_region(Marking5, Region1)),
    %% Region2 still has tokens in p4
    ?assertEqual(true, tokens_in_region(Marking5, Region2)).

-endif.
