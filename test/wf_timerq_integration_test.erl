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

-module(wf_timerq_integration_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% EUnit Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test Slice 3: Timer queue basic operations flow.
%% Verifies:
%% 1. wf_timerq:new/0 creates empty queue
%% 2. wf_timerq:arm/4 adds event with deadline
%% 3. wf_timerq:poll/2 returns events ready at given time
%% 4. Token structure matches expected marking format
%%--------------------------------------------------------------------
timer_queue_basic_flow_test() ->
    %% Step 1: Create empty queue
    Q0 = wf_timerq:new(),
    ?assertEqual(true, wf_timerq:is_empty(Q0)),
    ?assertEqual(0, wf_timerq:size(Q0)),
    ?assertEqual(undefined, wf_timerq:peek(Q0)),

    %% Step 2: Arm timer with event containing marking injection data
    %% Event format: {inject, PlaceTokens}
    %% PlaceTokens is a map of #{Place => [Tokens]}
    Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, #{timer => [tick1]}}),

    %% Verify queue has one entry
    ?assertEqual(false, wf_timerq:is_empty(Q1)),
    ?assertEqual(1, wf_timerq:size(Q1)),
    ?assertEqual({1100, {inject, #{timer => [tick1]}}}, wf_timerq:peek(Q1)),

    %% Step 3: Poll before deadline - should return empty list
    {E0, Q2} = wf_timerq:poll(Q1, 1000),
    ?assertEqual([], E0),
    ?assertEqual(1, wf_timerq:size(Q2)),

    %% Step 4: Poll at deadline - should return the event
    {E1, Q3} = wf_timerq:poll(Q2, 1100),
    ?assertEqual([{inject, #{timer => [tick1]}}], E1),
    ?assertEqual(0, wf_timerq:size(Q3)),
    ?assertEqual(true, wf_timerq:is_empty(Q3)),

    %% Step 5: Verify token structure matches marking format
    %% The event contains a marking map #{Place => [Tokens]}
    [{inject, PlaceTokens}] = E1,
    ?assertEqual([tick1], maps:get(timer, PlaceTokens)).

%%--------------------------------------------------------------------
%% @doc Test timer queue with multiple timers.
%% Verifies ordering and correct event extraction.
%%--------------------------------------------------------------------
timer_queue_multiple_events_test() ->
    Q0 = wf_timerq:new(),

    %% Arm multiple timers with different deadlines
    Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, #{timer => [tick1]}}),
    Q2 = wf_timerq:arm(Q1, k2, 1200, {inject, #{timer => [tick2]}}),
    Q3 = wf_timerq:arm(Q2, k3, 1300, {inject, #{timer => [tick3]}}),

    ?assertEqual(3, wf_timerq:size(Q3)),

    %% Poll at first deadline
    {E1, Q4} = wf_timerq:poll(Q3, 1100),
    ?assertEqual([{inject, #{timer => [tick1]}}], E1),
    ?assertEqual(2, wf_timerq:size(Q4)),

    %% Poll at second deadline
    {E2, Q5} = wf_timerq:poll(Q4, 1200),
    ?assertEqual([{inject, #{timer => [tick2]}}], E2),
    ?assertEqual(1, wf_timerq:size(Q5)),

    %% Poll at third deadline
    {E3, Q6} = wf_timerq:poll(Q5, 1300),
    ?assertEqual([{inject, #{timer => [tick3]}}], E3),
    ?assertEqual(0, wf_timerq:size(Q6)),
    ?assertEqual(true, wf_timerq:is_empty(Q6)).

%%--------------------------------------------------------------------
%% @doc Test timer queue with out-of-order arming.
%% Verifies deadlines are kept in sorted order.
%%--------------------------------------------------------------------
timer_queue_out_of_order_test() ->
    Q0 = wf_timerq:new(),

    %% Arm timers in reverse order
    Q1 = wf_timerq:arm(Q0, k1, 1300, {inject, #{timer => [tick3]}}),
    Q2 = wf_timerq:arm(Q1, k2, 1100, {inject, #{timer => [tick1]}}),
    Q3 = wf_timerq:arm(Q2, k3, 1200, {inject, #{timer => [tick2]}}),

    ?assertEqual(3, wf_timerq:size(Q3)),

    %% Peek should return earliest deadline (1100)
    ?assertEqual({1100, {inject, #{timer => [tick1]}}}, wf_timerq:peek(Q3)),

    %% Poll at 1100 should get tick1
    {E1, Q4} = wf_timerq:poll(Q3, 1100),
    ?assertEqual([{inject, #{timer => [tick1]}}], E1),

    %% Poll at 1200 should get tick2
    {E2, Q5} = wf_timerq:poll(Q4, 1200),
    ?assertEqual([{inject, #{timer => [tick2]}}], E2),

    %% Poll at 1300 should get tick3
    {E3, Q6} = wf_timerq:poll(Q5, 1300),
    ?assertEqual([{inject, #{timer => [tick3]}}], E3).

%%--------------------------------------------------------------------
%% @doc Test timer queue replacement.
%% Verifies arming same key replaces existing timer.
%%--------------------------------------------------------------------
timer_queue_replace_test() ->
    Q0 = wf_timerq:new(),

    Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, #{timer => [old_tick]}}),
    ?assertEqual(1, wf_timerq:size(Q1)),

    %% Replace timer k1 with new deadline and event
    Q2 = wf_timerq:arm(Q1, k1, 1200, {inject, #{timer => [new_tick]}}),
    ?assertEqual(1, wf_timerq:size(Q2)),

    %% Poll at 1100 should get nothing
    {E0, Q3} = wf_timerq:poll(Q2, 1100),
    ?assertEqual([], E0),

    %% Poll at 1200 should get new event
    {E1, Q4} = wf_timerq:poll(Q3, 1200),
    ?assertEqual([{inject, #{timer => [new_tick]}}], E1).

%%--------------------------------------------------------------------
%% @doc Test timer queue disarm.
%% Verifies removing a timer by key.
%%--------------------------------------------------------------------
timer_queue_disarm_test() ->
    Q0 = wf_timerq:new(),

    Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, #{timer => [tick1]}}),
    Q2 = wf_timerq:arm(Q1, k2, 1200, {inject, #{timer => [tick2]}}),

    ?assertEqual(2, wf_timerq:size(Q2)),

    %% Disarm k2
    Q3 = wf_timerq:disarm(Q2, k2),
    ?assertEqual(1, wf_timerq:size(Q3)),
    ?assertEqual({1100, {inject, #{timer => [tick1]}}}, wf_timerq:peek(Q3)),

    %% Disarm non-existent key should be no-op
    Q4 = wf_timerq:disarm(Q3, k999),
    ?assertEqual(1, wf_timerq:size(Q4)).

%%--------------------------------------------------------------------
%% @doc Test marking integration format.
%% Verifies timer events produce valid marking structures.
%%--------------------------------------------------------------------
timer_marking_format_test() ->
    %% Timer event should produce marking map: #{Place => [Tokens]}
    TimerEvent = {inject, #{timer => [tick1], alarm => [alert1]}},

    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1000, TimerEvent),

    {Events, _Q2} = wf_timerq:poll(Q1, 1000),

    ?assertEqual(1, length(Events)),

    [{inject, Marking}] = Events,

    %% Verify marking is a map
    ?assert(is_map(Marking)),

    %% Verify places exist in marking
    ?assertEqual([tick1], maps:get(timer, Marking)),
    ?assertEqual([alert1], maps:get(alarm, Marking)).

%%--------------------------------------------------------------------
%% @doc Test compound flow from user specification.
%%
%% This is the exact test case from the specification:
%% ```erlang
%% Q0 = wf_timerq:new(),
%% Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, #{timer => [tick1]}}),
%% {E0, Q2} = wf_timerq:poll(Q1, 1000),
%% E0 =:= [] andalso wf_timerq:size(Q2) =:= 1.
%% ```
%%--------------------------------------------------------------------
compound_doctest_spec_test() ->
    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, #{timer => [tick1]}}),
    {E0, Q2} = wf_timerq:poll(Q1, 1000),

    %% Verify the exact specification
    ?assertEqual([], E0),
    ?assertEqual(1, wf_timerq:size(Q2)),

    %% Additional verification
    ?assertEqual(true, E0 =:= [] andalso wf_timerq:size(Q2) =:= 1).

%%--------------------------------------------------------------------
%% @doc Test token injection simulation.
%% Simulates how timer events would be injected into a Petri net marking.
%%--------------------------------------------------------------------
token_injection_simulation_test() ->
    %% Initial marking ('end' is a reserved word, must use single quotes)
    InitialMarking = #{start => [init], timer => [], 'end' => []},

    %% Create and poll timer queue
    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1000, {inject, #{timer => [tick1]}}),
    {Events, _Q2} = wf_timerq:poll(Q1, 1000),

    %% Extract injection data from events
    [{inject, InjectionMap}] = Events,

    %% Simulate injection into marking (union of token lists)
    UpdatedMarking =
        maps:fold(
            fun(Place, Tokens, Acc) ->
                OldTokens = maps:get(Place, Acc, []),
                Acc#{Place => OldTokens ++ Tokens}
            end,
            InitialMarking,
            InjectionMap
        ),

    %% Verify token appeared in marking
    ?assertEqual([tick1], maps:get(timer, UpdatedMarking)),
    ?assertEqual([init], maps:get(start, UpdatedMarking)),
    ?assertEqual([], maps:get('end', UpdatedMarking)).

%%--------------------------------------------------------------------
%% @doc Test multiple token injection into same place.
%% Verifies multiset semantics (tokens accumulate).
%%--------------------------------------------------------------------
multiple_tokens_same_place_test() ->
    Q0 = wf_timerq:new(),

    %% Arm multiple timers that inject to same place
    Q1 = wf_timerq:arm(Q0, k1, 1000, {inject, #{timer => [tick1]}}),
    Q2 = wf_timerq:arm(Q1, k2, 1100, {inject, #{timer => [tick2]}}),
    Q3 = wf_timerq:arm(Q2, k3, 1200, {inject, #{timer => [tick3]}}),

    %% Poll all at once
    {Events, _Q4} = wf_timerq:poll(Q3, 1200),

    ?assertEqual(3, length(Events)),

    %% Simulate accumulation in marking
    FinalMarking =
        lists:foldl(
            fun({inject, InjectionMap}, Acc) ->
                maps:fold(
                    fun(Place, Tokens, AccIn) ->
                        OldTokens = maps:get(Place, AccIn, []),
                        AccIn#{Place => OldTokens ++ Tokens}
                    end,
                    Acc,
                    InjectionMap
                )
            end,
            #{timer => []},
            Events
        ),

    ?assertEqual([tick1, tick2, tick3], maps:get(timer, FinalMarking)).
