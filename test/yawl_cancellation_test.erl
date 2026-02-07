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

-module(yawl_cancellation_test).
-moduledoc """
Unit tests for yawl_cancellation module.

Tests cancellation region handling for YAWL workflows including:
- Basic cancellation token processing
- Token removal from cancelled places
- Nested cancellation regions
- Partial cancellation scenarios
- Edge cases
""".

%%====================================================================
%% EUnit Test Suite
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Functions - process_cancellation/2
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test basic cancellation token processing
%%--------------------------------------------------------------------
process_cancellation_basic_test() ->
    Marking = #{
        trigger => [{cancel, [p1, p2]}],
        p1 => [a, b],
        p2 => [c],
        p3 => [d]
    },
    Result = yawl_cancellation:process_cancellation(Marking, #{}),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([d], maps:get(p3, Result)).

%%--------------------------------------------------------------------
%% @doc Test multiple cancellation tokens in marking
%%--------------------------------------------------------------------
process_cancellation_multiple_tokens_test() ->
    Marking = #{
        trigger1 => [{cancel, [p1]}],
        trigger2 => [{cancel, [p2]}],
        trigger3 => [{cancel, [p3]}],
        p1 => [a],
        p2 => [b],
        p3 => [c],
        p4 => [d]
    },
    Result = yawl_cancellation:process_cancellation(Marking, #{}),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)),
    ?assertEqual([d], maps:get(p4, Result)).

%%--------------------------------------------------------------------
%% @doc Test no cancellation tokens - marking unchanged
%%--------------------------------------------------------------------
process_cancellation_no_tokens_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    Result = yawl_cancellation:process_cancellation(Marking, #{}),
    ?assertEqual(Marking, Result).

%%--------------------------------------------------------------------
%% @doc Test empty marking handling
%%--------------------------------------------------------------------
process_cancellation_empty_marking_test() ->
    Marking = #{},
    Result = yawl_cancellation:process_cancellation(Marking, #{}),
    ?assertEqual(#{}, Result).

%%--------------------------------------------------------------------
%% @doc Test cancellation with region definitions
%%--------------------------------------------------------------------
process_cancellation_with_regions_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    Regions = #{r1 => #{places => [p1]}},
    Result = yawl_cancellation:process_cancellation(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([b], maps:get(p2, Result)).

%%--------------------------------------------------------------------
%% @doc Test cancellation preserves non-cancelled places
%%--------------------------------------------------------------------
process_cancellation_preserves_non_cancelled_test() ->
    Marking = #{
        trigger => [{cancel, [p1]}],
        p1 => [a, b, c],
        p2 => [d],
        p3 => []
    },
    Result = yawl_cancellation:process_cancellation(Marking, #{}),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([d], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)).

%%====================================================================
%% Test Functions - apply_to_marking/2
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test apply region map to marking
%%--------------------------------------------------------------------
apply_to_marking_region_map_test() ->
    Marking = #{p1 => [a], p2 => [b], p3 => [c]},
    Regions = #{
        r1 => #{places => [p1, p2]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([c], maps:get(p3, Result)).

%%--------------------------------------------------------------------
%% @doc Test apply region list to marking
%%--------------------------------------------------------------------
apply_to_marking_region_list_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    RegionList = [
        #{name => r1, places => [p1]},
        #{name => r2, places => [p2]}
    ],
    Result = yawl_cancellation:apply_to_marking(Marking, RegionList),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)).

%%--------------------------------------------------------------------
%% @doc Test empty regions - no change
%%--------------------------------------------------------------------
apply_to_marking_empty_regions_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    Result = yawl_cancellation:apply_to_marking(Marking, #{}),
    ?assertEqual([a], maps:get(p1, Result)),
    ?assertEqual([b], maps:get(p2, Result)).

%%--------------------------------------------------------------------
%% @doc Test multiple regions all cancelled
%%--------------------------------------------------------------------
apply_to_marking_multiple_regions_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c],
        p4 => [d],
        p5 => [e]
    },
    Regions = #{
        r1 => #{places => [p1, p2]},
        r2 => #{places => [p3, p4]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)),
    ?assertEqual([], maps:get(p4, Result)),
    ?assertEqual([e], maps:get(p5, Result)).

%%--------------------------------------------------------------------
%% @doc Test region with non-existent places
%%--------------------------------------------------------------------
apply_to_marking_nonexistent_places_test() ->
    Marking = #{p1 => [a]},
    Regions = #{r1 => #{places => [p2, p3]}},
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([a], maps:get(p1, Result)),
    %% Non-existent places are added as empty
    ?assertEqual([], maps:get(p2, Result)).

%%====================================================================
%% Test Functions - find_cancelled_places/2
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test find places from cancellation tokens
%%--------------------------------------------------------------------
find_cancelled_places_from_tokens_test() ->
    Marking = #{
        t1 => [{cancel, [p1, p2]}],
        t2 => [{cancel, [p3]}]
    },
    Result = yawl_cancellation:find_cancelled_places(Marking, #{}),
    ?assertEqual(lists:sort([p1, p2, p3]), lists:sort(Result)).

%%--------------------------------------------------------------------
%% @doc Test find places from region definitions
%%--------------------------------------------------------------------
find_cancelled_places_from_regions_test() ->
    Marking = #{p1 => [a]},
    Regions = #{
        r1 => #{places => [p1, p2, p3]}
    },
    Result = yawl_cancellation:find_cancelled_places(Marking, Regions),
    ?assertEqual(lists:sort([p1, p2, p3]), lists:sort(Result)).

%%--------------------------------------------------------------------
%% @doc Test combine tokens and regions
%%--------------------------------------------------------------------
find_cancelled_places_combined_test() ->
    Marking = #{
        trigger => [{cancel, [p1]}]
    },
    Regions = #{
        r1 => #{places => [p2, p3]}
    },
    Result = yawl_cancellation:find_cancelled_places(Marking, Regions),
    ?assertEqual(lists:sort([p1, p2, p3]), lists:sort(Result)).

%%--------------------------------------------------------------------
%% @doc Test no cancellations - empty list
%%--------------------------------------------------------------------
find_cancelled_places_none_test() ->
    Marking = #{p1 => [a], p2 => [b]},
    Result = yawl_cancellation:find_cancelled_places(Marking, #{}),
    ?assertEqual([], Result).

%%--------------------------------------------------------------------
%% @doc Test deduplicate cancelled places
%%--------------------------------------------------------------------
find_cancelled_places_deduplicate_test() ->
    Marking = #{
        t1 => [{cancel, [p1, p2]}],
        t2 => [{cancel, [p2, p3]}]
    },
    Result = yawl_cancellation:find_cancelled_places(Marking, #{}),
    ?assertEqual(lists:sort([p1, p2, p3]), lists:sort(Result)),
    ?assertEqual(length([p1, p2, p3]), length(Result)).

%%====================================================================
%% Test Functions - is_in_cancellation_region/2
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test place is in region
%%--------------------------------------------------------------------
is_in_cancellation_region_true_test() ->
    Region = #{
        name => test,
        places => [p1, p2, p3]
    },
    ?assert(yawl_cancellation:is_in_cancellation_region(p2, Region)).

%%--------------------------------------------------------------------
%% @doc Test place is not in region
%%--------------------------------------------------------------------
is_in_cancellation_region_false_test() ->
    Region = #{
        name => test,
        places => [p1, p2]
    },
    ?assertNot(yawl_cancellation:is_in_cancellation_region(p3, Region)).

%%--------------------------------------------------------------------
%% @doc Test region as map with places list
%%--------------------------------------------------------------------
is_in_cancellation_region_map_with_list_test() ->
    Region = #{places => [p1, p2]},
    ?assert(yawl_cancellation:is_in_cancellation_region(p1, Region)),
    ?assertNot(yawl_cancellation:is_in_cancellation_region(p3, Region)).

%%--------------------------------------------------------------------
%% @doc Test region as simple key map
%%--------------------------------------------------------------------
is_in_cancellation_region_simple_map_test() ->
    Region = #{p1 => true, p2 => true},
    ?assert(yawl_cancellation:is_in_cancellation_region(p1, Region)),
    ?assertNot(yawl_cancellation:is_in_cancellation_region(p3, Region)).

%%--------------------------------------------------------------------
%% @doc Test invalid region format
%%--------------------------------------------------------------------
is_in_cancellation_region_invalid_test() ->
    Region = not_a_map,
    ?assertNot(yawl_cancellation:is_in_cancellation_region(p1, Region)).

%%====================================================================
%% Test Functions - is_cancel_token/1
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test valid cancel token with places
%%--------------------------------------------------------------------
is_cancel_token_valid_test() ->
    ?assert(yawl_cancellation:is_cancel_token({cancel, [p1, p2]})),
    ?assert(yawl_cancellation:is_cancel_token({cancel, []})).

%%--------------------------------------------------------------------
%% @doc Test valid cancel token with empty list
%%--------------------------------------------------------------------
is_cancel_token_empty_test() ->
    ?assert(yawl_cancellation:is_cancel_token({cancel, []})).

%%--------------------------------------------------------------------
%% @doc Test not a cancel tuple
%%--------------------------------------------------------------------
is_cancel_token_not_tuple_test() ->
    ?assertNot(yawl_cancellation:is_cancel_token({other, [p1]})),
    ?assertNot(yawl_cancellation:is_cancel_token(not_a_token)).

%%--------------------------------------------------------------------
%% @doc Test non-atom place names
%%--------------------------------------------------------------------
is_cancel_token_non_atom_places_test() ->
    ?assertNot(yawl_cancellation:is_cancel_token({cancel, [p1, "p2"]})),
    ?assertNot(yawl_cancellation:is_cancel_token({cancel, [p1, 123]})).

%%--------------------------------------------------------------------
%% @doc Test not list second element
%%--------------------------------------------------------------------
is_cancel_token_not_list_second_test() ->
    ?assertNot(yawl_cancellation:is_cancel_token({cancel, p1})).

%%====================================================================
%% Test Functions - extract_targets/1
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test extract targets from valid token
%%--------------------------------------------------------------------
extract_targets_valid_test() ->
    ?assertEqual([p1, p2], yawl_cancellation:extract_targets({cancel, [p1, p2]})),
    ?assertEqual([], yawl_cancellation:extract_targets({cancel, []})).

%%--------------------------------------------------------------------
%% @doc Test extract from invalid token returns empty
%%--------------------------------------------------------------------
extract_targets_invalid_test() ->
    ?assertEqual([], yawl_cancellation:extract_targets(not_a_token)),
    ?assertEqual([], yawl_cancellation:extract_targets({other, [p1]})).

%%--------------------------------------------------------------------
%% @doc Test extract from atom
%%--------------------------------------------------------------------
extract_targets_from_atom_test() ->
    ?assertEqual([], yawl_cancellation:extract_targets(p1)).

%%--------------------------------------------------------------------
%% @doc Test extract from non-tuple
%%--------------------------------------------------------------------
extract_targets_from_non_tuple_test() ->
    ?assertEqual([], yawl_cancellation:extract_targets([p1, p2])).

%%====================================================================
%% Test Functions - define_region/2
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test define basic region
%%--------------------------------------------------------------------
define_region_basic_test() ->
    Region = yawl_cancellation:define_region(test, [p1, p2]),
    ?assertEqual(test, maps:get(name, Region)),
    ?assertEqual([p1, p2], maps:get(places, Region)).

%%--------------------------------------------------------------------
%% @doc Test define region with single place
%%--------------------------------------------------------------------
define_region_single_test() ->
    Region = yawl_cancellation:define_region(test, [p1]),
    ?assertEqual([p1], maps:get(places, Region)).

%%--------------------------------------------------------------------
%% @doc Test define region with no places
%%--------------------------------------------------------------------
define_region_empty_test() ->
    Region = yawl_cancellation:define_region(test, []),
    ?assertEqual([], maps:get(places, Region)).

%%--------------------------------------------------------------------
%% @doc Test region structure is correct
%%--------------------------------------------------------------------
define_region_structure_test() ->
    Region = yawl_cancellation:define_region(test, [p1]),
    ?assertEqual(undefined, maps:get(parent, Region)),
    ?assert(is_map(Region)).

%%====================================================================
%% Test Functions - region_places/1
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test extract places from region
%%--------------------------------------------------------------------
region_places_extract_test() ->
    Region = #{places => [p1, p2, p3]},
    ?assertEqual([p1, p2, p3], yawl_cancellation:region_places(Region)).

%%--------------------------------------------------------------------
%% @doc Test region without places returns empty
%%--------------------------------------------------------------------
region_places_no_places_field_test() ->
    Region = #{name => test},
    ?assertEqual([], yawl_cancellation:region_places(Region)).

%%--------------------------------------------------------------------
%% @doc Test empty places list
%%--------------------------------------------------------------------
region_places_empty_list_test() ->
    Region = #{places => []},
    ?assertEqual([], yawl_cancellation:region_places(Region)).

%%====================================================================
%% Test Functions - Order Fulfillment Scenarios
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test Order Fulfillment carrier timeout scenario
%%--------------------------------------------------------------------
order_fulfillment_carrier_timeout_test() ->
    %% Simulate Order Fulfillment carrier timeout
    Marking = #{
        order_received => [#{order_id => "ORD-123"}],
        payment_processed => [paid],
        pending_appointment => [waiting_for_carrier],
        awaiting_confirmation => [pending],
        carrier_confirmed => [],
        shipping_scheduled => []
    },
    Regions = #{
        carrier_timeout => #{
            name => carrier_timeout,
            places => [pending_appointment, awaiting_confirmation]
        }
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(pending_appointment, Result)),
    ?assertEqual([], maps:get(awaiting_confirmation, Result)),
    ?assertEqual([#{order_id => "ORD-123"}], maps:get(order_received, Result)),
    ?assertEqual([paid], maps:get(payment_processed, Result)).

%%--------------------------------------------------------------------
%% @doc Test full order cancellation
%%--------------------------------------------------------------------
order_fulfillment_order_cancellation_test() ->
    %% Full order cancellation
    Marking = #{
        order_received => [#{order_id => "ORD-456"}],
        payment_processed => [paid],
        pending_appointment => [waiting],
        carrier_confirmed => [confirmed],
        shipping_scheduled => [scheduled],
        order_completed => []
    },
    Regions = #{
        order_cancelled => #{
            name => order_cancelled,
            places => [
                pending_appointment,
                carrier_confirmed,
                shipping_scheduled
            ]
        }
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(pending_appointment, Result)),
    ?assertEqual([], maps:get(carrier_confirmed, Result)),
    ?assertEqual([], maps:get(shipping_scheduled, Result)),
    ?assertEqual([#{order_id => "ORD-456"}], maps:get(order_received, Result)).

%%--------------------------------------------------------------------
%% @doc Test payment timeout cancels reservation
%%--------------------------------------------------------------------
order_fulfillment_payment_timeout_test() ->
    %% Payment timeout cancels reservation
    Marking = #{
        order_received => [#{order_id => "ORD-789"}],
        payment_pending => [awaiting_payment],
        inventory_reserved => [#{items => reserved}],
        payment_processed => []
    },
    Regions = #{
        payment_timeout => #{
            name => payment_timeout,
            places => [payment_pending, inventory_reserved]
        }
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(payment_pending, Result)),
    ?assertEqual([], maps:get(inventory_reserved, Result)),
    ?assertEqual([#{order_id => "ORD-789"}], maps:get(order_received, Result)).

%%--------------------------------------------------------------------
%% @doc Test full Order Fulfillment workflow with cancellations
%%--------------------------------------------------------------------
order_fulfillment_full_workflow_test() ->
    %% Complete Order Fulfillment workflow with multiple cancellation regions
    Marking = #{
        %% Order state
        order_received => [#{order_id => "ORD-999", total => 1000}],
        payment_processed => [paid],
        inventory_allocated => [allocated],

        %% Carrier appointment state
        pending_appointment => [waiting],
        awaiting_confirmation => [pending],
        carrier_confirmed => [],
        shipping_scheduled => [],

        %% Completion state
        order_completed => [],
        order_cancelled => []
    },

    %% Define all cancellation regions
    Regions = #{
        %% Carrier timeout - cancels pending appointment
        carrier_timeout => #{
            name => carrier_timeout,
            places => [pending_appointment, awaiting_confirmation]
        },
        %% Order cancelled - cancels fulfillment
        order_cancelled_region => #{
            name => order_cancelled_region,
            places => [
                pending_appointment,
                awaiting_confirmation,
                carrier_confirmed,
                shipping_scheduled
            ]
        },
        %% Payment timeout - cancels reservation
        payment_timeout => #{
            name => payment_timeout,
            places => [payment_pending, inventory_reserved]
        }
    },

    %% Apply carrier timeout cancellation
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),

    %% Verify carrier timeout places cancelled
    ?assertEqual([], maps:get(pending_appointment, Result)),
    ?assertEqual([], maps:get(awaiting_confirmation, Result)),

    %% Verify order state preserved
    ?assertEqual([#{order_id => "ORD-999", total => 1000}],
                 maps:get(order_received, Result)),
    ?assertEqual([paid], maps:get(payment_processed, Result)).

%%====================================================================
%% Test Functions - Nested Cancellation Regions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test inner region cancels subset
%%--------------------------------------------------------------------
nested_cancellation_inner_subset_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c]
    },
    Regions = #{
        outer => #{places => [p1, p2, p3]},
        inner => #{places => [p2]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)).

%%--------------------------------------------------------------------
%% @doc Test outer region includes inner
%%--------------------------------------------------------------------
nested_cancellation_outer_includes_inner_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c],
        p4 => [d]
    },
    %% Only apply outer region (includes inner)
    Regions = #{
        outer => #{places => [p1, p2, p3]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)),
    ?assertEqual([d], maps:get(p4, Result)).

%%--------------------------------------------------------------------
%% @doc Test sibling regions independent
%%--------------------------------------------------------------------
nested_cancellation_sibling_regions_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c],
        p4 => [d]
    },
    Regions = #{
        region1 => #{places => [p1, p2]},
        region2 => #{places => [p3, p4]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)),
    ?assertEqual([], maps:get(p4, Result)).

%%--------------------------------------------------------------------
%% @doc Test deeply nested regions
%%--------------------------------------------------------------------
nested_cancellation_deeply_nested_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c],
        p4 => [d],
        p5 => [e]
    },
    Regions = #{
        level1 => #{places => [p1, p2, p3, p4]},
        level2 => #{places => [p2, p3]},
        level3 => #{places => [p3]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    %% All places in level1 should be cancelled
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)),
    ?assertEqual([], maps:get(p4, Result)),
    ?assertEqual([e], maps:get(p5, Result)).

%%====================================================================
%% Test Functions - Edge Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cancellation token with non-existent places
%%--------------------------------------------------------------------
edge_case_cancel_nonexistent_places_test() ->
    Marking = #{p1 => [a]},
    Regions = #{r1 => #{places => [p2, p3, p4]}},
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([a], maps:get(p1, Result)),
    %% Non-existent places are added as empty
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)).

%%--------------------------------------------------------------------
%% @doc Test place in multiple regions
%%--------------------------------------------------------------------
edge_case_place_multiple_regions_test() ->
    Marking = #{
        p1 => [a],
        p2 => [b],
        p3 => [c]
    },
    Regions = #{
        r1 => #{places => [p1, p2]},
        r2 => #{places => [p2, p3]}
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([], maps:get(p3, Result)).

%%--------------------------------------------------------------------
%% @doc Test token filter edge cases
%%--------------------------------------------------------------------
edge_case_token_filter_test() ->
    Marking = #{
        trigger => [{cancel, [p1]}],
        p1 => [a, b, c],
        p2 => [d, e]
    },
    Result = yawl_cancellation:process_cancellation(Marking, #{}),
    ?assertEqual([], maps:get(p1, Result)),
    ?assertEqual([d, e], maps:get(p2, Result)).

%%--------------------------------------------------------------------
%% @doc Test large marking performance
%%--------------------------------------------------------------------
edge_case_large_marking_test() ->
    %% Performance test with large marking
    Places = lists:seq(1, 100),
    Marking = lists:foldl(
        fun(N, Acc) ->
            Acc#{list_to_atom("p" ++ integer_to_list(N)) => [N]}
        end,
        #{},
        Places
    ),
    %% Cancel first 50 places
    CancelPlaces = lists:seq(1, 50),
    Regions = #{
        large_region => #{
            name => large_region,
            places => [list_to_atom("p" ++ integer_to_list(N)) || N <- CancelPlaces]
        }
    },
    Result = yawl_cancellation:apply_to_marking(Marking, Regions),
    %% Verify first 50 are empty
    lists:foreach(fun(N) ->
        Place = list_to_atom("p" ++ integer_to_list(N)),
        ?assertEqual([], maps:get(Place, Result))
    end, CancelPlaces),
    %% Verify last 50 are preserved
    lists:foreach(fun(N) ->
        Place = list_to_atom("p" ++ integer_to_list(N)),
        ?assertEqual([N], maps:get(Place, Result))
    end, lists:seq(51, 100)).
