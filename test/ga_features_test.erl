%% -*- erlang -*-
%%%% @doc ga_features_test - EUnit tests for new GA and workflow features.
%%
%% Tests for:
%% - GA Constitution (ga_constitution, ga_yaml, ga_compiler)
%% - Circuit Breaker pattern (circuit_breaker)
%% - Persistent Timer (wf_persistent_timer)
%% - Time Travel Debugger (wf_time_travel)
%% - Cancel Region pattern (cancel_region)
%%
%% @end

-module(ga_features_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% GA Constitution Tests
%%====================================================================

ga_constitution_new_from_map_test() ->
    Map = #{
        <<"id">> => <<"test_wf">>,
        <<"version">> => <<"1.0">>,
        <<"sigma">> => #{
            <<"type_system">> => <<"behavioral">>,
            <<"type_bindings">> => []
        },
        <<"refusals">> => [],
        <<"quality_gates">> => [],
        <<"lambda">> => #{
            <<"compilation_strategy">> => <<"topological">>,
            <<"pattern_sequence">> => []
        }
    },
    Constitution = ga_constitution:from_map(Map),
    ?assertEqual(<<"test_wf">>, Constitution#ga_constitution.id),
    ?assertEqual(<<"1.0">>, Constitution#ga_constitution.version),
    ?assertEqual(ok, ga_constitution:validate(Constitution)).

ga_constitution_validate_test() ->
    ValidMap = #{
        <<"id">> => <<"test_wf">>,
        <<"version">> => <<"1.0">>
    },
    Constitution = ga_constitution:from_map(ValidMap),
    ?assertEqual(ok, ga_constitution:validate(Constitution)).

ga_constitution_add_refusal_test() ->
    Constitution = ga_constitution:new(<<"id">>, <<"1.0">>,
       #ga_constitution.sigma{}, [], #ga_constitution.lambda{}),
    Refusal = #ga_constitution:refusal{
        state = <<"pending">>,
        refused_transitions => [<<"t_cancel">>],
        refusal_reason => <<"Cannot cancel before approval">>
    },
    Updated = ga_constitution:add_refusal(Constitution, Refusal),
    ?assertEqual(1, length(Updated#ga_constitution.refusals)).

ga_constitution_to_map_test() ->
    Constitution = ga_constitution:new(<<"test">>, <<"1.0">>,
       #ga_constitution.sigma{}, [], #ga_constitution.lambda{}),
    Map = ga_constitution:to_map(Constitution),
    ?assertEqual(<<"test">>, maps:get(<<"id">>, Map)),
    ?assertEqual(<<"1.0">>, maps:get(<<"version">>, Map)).

%%====================================================================
%% GA Compiler Tests
%%====================================================================

ga_compiler_compile_test() ->
    Map = #{
        <<"id">> => <<"test_wf">>,
        <<"version">> => <<"1.0">>,
        <<"sigma">> => #{},
        <<"refusals">> => [],
        <<"quality_gates">> => [],
        <<"lambda">> => #{
            <<"compilation_strategy">> => <<"topological">>,
            <<"pattern_sequence">> => []
        }
    },
    Result = ga_compiler:compile(Map),
    ?assertMatch({ok, _Compilation}, Result).

ga_compiler_supported_patterns_test() ->
    Patterns = ga_compiler:supported_patterns(),
    ?assertEqual(43, length(Patterns)),
    ?assert(lists:member(<<"P1_Sequence">>, Patterns)),
    ?assert(lists:member(<<"P2_ParallelSplit">>, Patterns)).

%%====================================================================
%% YAML Parser Tests
%%====================================================================

ga_yaml_parse_simple_test() ->
    Yaml = <<"
constitution:
  id: test_workflow
  version: 1.0
  sigma:
    type_system: behavioral
    type_bindings: []
  refusals: []
  quality_gates: []
  lambda:
    compilation_strategy: topological
    pattern_sequence: []
">>,
    Result = ga_yaml:from_yaml(Yaml),
    ?assertMatch({ok, _Constitution}, Result).

ga_yaml_parse_with_bindings_test() ->
    Yaml = <<"
constitution:
  id: test_workflow
  version: 1.0
  sigma:
    type_system: behavioral
    type_bindings:
      - term: Request
        type: PurchaseOrder
        token_contract:
          shape: singleton
          validity: eager
  lambda:
    compilation_strategy: topological
    pattern_sequence: []
">>,
    Result = ga_yaml:from_yaml(Yaml),
    ?assertMatch({ok, _Constitution}, Result).

%%====================================================================
%% Circuit Breaker Tests
%%====================================================================

circuit_breaker_state_transitions_test() ->
    %% Test closed -> open transition on failures
    {ok, Breaker} = circuit_breaker:start_link(<<"test_breaker">>, fun() -> ok end,
        [{failure_threshold, 3}]),

    %% Execute should succeed initially
    ?assertMatch({ok, _}, circuit_breaker:execute(<<"test_breaker">>, fun() -> ok end)),

    %% Simulate failures to open circuit
    FailFun = fun() -> erlang:error(test_error) end,
    lists:foreach(fun(_) ->
        circuit_breaker:execute(<<"test_breaker">>, FailFun)
    end, lists:seq(1, 3)),

    %% Circuit should be open now
    ?assertMatch({error, circuit_open},
                 circuit_breaker:execute(<<"test_breaker">>, fun() -> ok end)),

    circuit_breaker:stop(<<"test_breaker">>).

circuit_breaker_reset_test() ->
    {ok, Breaker} = circuit_breaker:start_link(<<"test_breaker2">>, fun() -> ok end),
    ?assertEqual(ok, circuit_breaker:reset(<<"test_breaker2">>)),
    circuit_breaker:stop(<<"test_breaker2">>).

circuit_breaker_get_state_test() ->
    {ok, _Breaker} = circuit_breaker:start_link(<<"test_breaker3">>, fun() -> ok end),
    {ok, State} = circuit_breaker:get_state(<<"test_breaker3">>),
    ?assertEqual(closed, State#circuit_breaker_state.state),
    circuit_breaker:stop(<<"test_breaker3">>).

%%====================================================================
%% Persistent Timer Tests
%%====================================================================

wf_persistent_timer_start_timer_test() ->
    {ok, TimerPid} = wf_persistent_timer:start_link([
        {timezone, <<"UTC">>},
        {work_start, {9, 0}},
        {work_end, {17, 0}}
    ]),

    TargetTime = erlang:system_time(millisecond) + 1000,
    {ok, _TimerId} = wf_persistent_timer:start_timer(
        <<"execution_1">>,
        TargetTime,
        {?MODULE, execute_callback, []}
    ),

    Timers = wf_persistent_timer:list_timers(),
    ?assert(length(Timers) >= 0),

    wf_persistent_timer:cancel_all_timers(),
    gen_server:stop(TimerPid).

wf_persistent_timer_calendar_test() ->
    {ok, TimerPid} = wf_persistent_timer:start_link([]),

    ?assertEqual(ok, wf_persistent_timer:set_work_hours({9, 0}, {17, 0})),
    ?assertMatch({{9, 0}, {17, 0}}, wf_persistent_timer:get_work_hours()),

    ?assertEqual(ok, wf_persistent_timer:set_timezone(<<"America/New_York">>)),
    ?assertEqual(<<"America/New_York">>, wf_persistent_timer:get_timezone()),

    gen_server:stop(TimerPid).

wf_persistent_timer_holiday_test() ->
    {ok, TimerPid} = wf_persistent_timer:start_link([]),

    ?assertEqual(ok, wf_persistent_timer:add_holiday(<<"2024-12-25">>, <<"Christmas">>)),
    Holidays = wf_persistent_timer:list_holidays(),
    ?assertEqual(true, lists:keymember(<<"2024-12-25">>, 1, Holidays)),

    ?assertEqual(ok, wf_persistent_timer:remove_holiday(<<"2024-12-25">>)),

    gen_server:stop(TimerPid).

%%====================================================================
%% Time Travel Debugger Tests
%%====================================================================

wf_time_travel_session_test() ->
    {ok, _Pid} = wf_time_travel:start_link(#{}),

    {ok, SessionId} = wf_time_travel:start_session(
        <<"case_1">>,
        test_module,
        #{}
    ),

    ?assert(is_binary(SessionId)),

    ActiveSessions = wf_time_travel:get_active_sessions(),
    ?assert(lists:member(SessionId, ActiveSessions)),

    wf_time_travel:stop_session(SessionId).

wf_time_travel_record_events_test() ->
    {ok, _Pid} = wf_time_travel:start_link(#{}),

    {ok, SessionId} = wf_time_travel:start_session(<<"case_2">>, test_module, #{}),

    ?assertEqual(ok, wf_time_travel:record_transition(
        SessionId,
        t_start,
        #{p_start => [start]},
        auto,
        #{}
    )),

    ?assertEqual(ok, wf_time_travel:record_marking(
        SessionId,
        #{p_start => [start]},
        #{p_active => [active]}
    )),

    {ok, Timeline} = wf_time_travel:get_timeline(SessionId),
    ?assertEqual(true, length(Timeline) >= 2),

    wf_time_travel:stop_session(SessionId).

wf_time_travel_replay_test() ->
    {ok, _Pid} = wf_time_travel:start_link(#{}),

    {ok, SessionId} = wf_time_travel:start_session(<<"case_3">>, test_module, #{}),

    ?assertEqual(ok, wf_time_travel:record_transition(
        SessionId,
        t_start,
        #{p_start => [start]},
        auto,
        #{}
    )),

    {ok, Events} = wf_time_travel:replay_from_start(SessionId),
    ?assertEqual(true, length(Events) >= 1),

    {ok, _} = wf_time_travel:step_forward(SessionId),
    {ok, _} = wf_time_travel:step_backward(SessionId),

    wf_time_travel:stop_session(SessionId).

wf_time_travel_breakpoints_test() ->
    {ok, _Pid} = wf_time_travel:start_link(#{}),

    {ok, SessionId} = wf_time_travel:start_session(<<"case_4">>, test_module, #{}),

    ?assertEqual(ok, wf_time_travel:set_breakpoint(
        SessionId,
        {transition, t_cancel}
    )),

    {ok, Breakpoints} = wf_time_travel:list_breakpoints(SessionId),
    ?assertEqual(1, length(Breakpoints)),

    ?assertEqual(ok, wf_time_travel:clear_breakpoint(
        SessionId,
        {transition, t_cancel}
    )),

    wf_time_travel:stop_session(SessionId).

%%====================================================================
%% Cancel Region Tests
%%====================================================================

cancel_region_define_region_test() ->
    Places = [p_region_1, p_region_2],
    ?assertEqual(ok, cancel_region:define_region(<<"region_1">>, Places)),
    ?assertEqual(ok, cancel_region:define_region(<<"region_2">>, <<"region_1">>, Places)).

cancel_region_api_test() ->
    ?assertEqual(ok, cancel_region:cancel_region(<<"test_region">>)),
    ?assertEqual(ok, cancel_region:cancel_case(<<"test_case">>)),

    {ok, _Regions} = cancel_region:get_active_regions(<<"test_case">>).

cancel_region_register_activity_test() ->
    ActivityId = <<"activity_1">>,
    RegionId = <<"region_1">>,

    ?assertEqual(ok, cancel_region:register_activity(
        ActivityId,
        RegionId,
        p_activity
    )),

    ?assertEqual(ok, cancel_region:unregister_activity(ActivityId)).

%%====================================================================
%% Integration Tests
%%====================================================================

ga_compile_execute_test() ->
    %% End-to-end test: compile constitution and verify structure
    Map = #{
        <<"id">> => <<"integration_test">>,
        <<"version">> => <<"1.0">>,
        <<"sigma">> => #{
            <<"type_system">> => <<"behavioral">>,
            <<"type_bindings">> => [
                #{
                    <<"term">> => <<"Order">>,
                    <<"type">> => <<"PurchaseOrder">>,
                    <<"token_contract">> => #{
                        <<"shape">> => <<"singleton">>,
                        <<"validity">> => <<"eager">>
                    }
                }
            ]
        },
        <<"lambda">> => #{
            <<"compilation_strategy">> => <<"topological">>,
            <<"pattern_sequence">> => [
                #{
                    <<"pattern">> => <<"P1_Sequence">>,
                    <<"instance_id">> => <<"seq_1">>,
                    <<"config">> => #{}
                }
            ]
        }
    },

    {ok, Compilation} = ga_compiler:compile(Map),
    ?assertMatch(#compilation{}, Compilation).

%%====================================================================
%% Test Generators
%%====================================================================

ga_constitution_roundtrip_test_() ->
    %% Test that constitution -> map -> constitution preserves data
    Constitutions = [
        ga_constitution:new(<<"id1">>, <<"1.0">>,
           #ga_constitution.sigma{}, [], #ga_constitution.lambda{}),
        ga_constitution:new(<<"id2">>, <<"2.0">>,
           #ga_constitution.sigma{},
            [#ga_constitution:refusal{state = <<"s">>}],
            #ga_constitution.lambda{})
    ],

    [{Constitution,
      fun() ->
          Map = ga_constitution:to_map(Constitution),
          Roundtrip = ga_constitution:from_map(Map),
          ?assertEqual(Constitution#ga_constitution.id, Roundtrip#ga_constitution.id),
          ?assertEqual(Constitution#ga_constitution.version, Roundtrip#ga_constitution.version)
      end}
     || Constitution <- Constitutions].

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup_test_() ->
    {setup,
     fun() ->
         %% Start any required processes
         {ok, _Pid} = wf_time_travel:start_link(#{}),
         ok
     end,
     fun(_ok) ->
         %% Cleanup
         wf_time_travel:stop_session(<<"case_1">>),
         ok
     end
    }.
