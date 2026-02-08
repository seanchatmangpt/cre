%% -*- erlang -*-
%%%% @doc ga_features_integration_SUITE - Common Test suite for GA features.
%%
%% Integration tests for the GA compiler front-end and related features.
%%
%% @end

-module(ga_features_integration_SUITE).
-author("CRE Team").

-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/ga/ga_constitution.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

%% all/0 must return list of test cases and/or groups
-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%%====================================================================
%% Test Cases
%%====================================================================

-export([
    ga_constitution_validation_test/1,
    ga_yaml_roundtrip_test/1,
    ga_compiler_full_pipeline_test/1,
    circuit_breaker_failure_recovery_test/1,
    persistent_timer_calendar_test/1,
    time_travel_debugger_test/1,
    cancel_region_hierarchy_test/1
]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

%% @doc Returns list of all test cases and groups
all() ->
    [
        ga_constitution_validation_test,
        ga_yaml_roundtrip_test,
        ga_compiler_full_pipeline_test,
        circuit_breaker_failure_recovery_test,
        persistent_timer_calendar_test,
        time_travel_debugger_test,
        cancel_region_hierarchy_test
    ].

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    %% Start mnesia for persistent_timer tests
    case mnesia:start() of
        ok ->
            %% Create mnesia schema if needed (for test environment)
            case mnesia:table_info(schema, disc_copies) of
                [] ->
                    %% No disc copies, use ram_copies for testing
                    %% Use the record name from wf_persistent_timer module
                    Attrs = [timer_id, execution_id, target_time, timezone,
                             callback, status, created_at, fired_at, result,
                             retry_count, max_retries, metadata, timer_ref],
                    case mnesia:create_table(wf_persistent_timer, [
                            {ram_copies, [node()]},
                            {record_name, persistent_timer},
                            {attributes, Attrs}
                        ]) of
                        {atomic, ok} -> ok;
                        {aborted, {already_exists, _}} -> ok;
                        {aborted, Reason} ->
                            ct:fail({failed_to_create_mnesia_table, Reason})
                    end;
                _ ->
                    %% Disc copies available, use disc_copies
                    Attrs = [timer_id, execution_id, target_time, timezone,
                             callback, status, created_at, fired_at, result,
                             retry_count, max_retries, metadata, timer_ref],
                    case mnesia:create_table(wf_persistent_timer, [
                            {disc_copies, [node()]},
                            {record_name, persistent_timer},
                            {attributes, Attrs}
                        ]) of
                        {atomic, ok} -> ok;
                        {aborted, {already_exists, _}} -> ok;
                        {aborted, Reason} ->
                            ct:fail({failed_to_create_mnesia_table, Reason})
                    end
            end,
            Config;
        {error, {already_started, _}} ->
            Config;
        {error, Reason} ->
            ct:fail({failed_to_start_mnesia, Reason})
    end,
    %% Start wf_time_travel server for time_travel tests
    case wf_time_travel:start_link(#{}) of
        {ok, _Pid} -> Config;
        {error, {already_started, _}} -> Config
    end.

end_per_suite(_Config) ->
    %% Stop wf_time_travel server
    case whereis(wf_time_travel) of
        Pid when is_pid(Pid) -> gen_server:stop(Pid);
        undefined -> ok
    end,
    %% Stop mnesia (only for test cleanup)
    mnesia:stop(),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Tests constitution validation with various configurations
ga_constitution_validation_test(_Config) ->
    %% Test 1: Valid minimal constitution
    MinimalMap = #{
        <<"id">> => <<"minimal_wf">>,
        <<"version">> => <<"1.0">>
    },
    Constitution1 = ga_constitution:from_map(MinimalMap),
    ?assertEqual(ok, ga_constitution:validate(Constitution1)),

    %% Test 2: Constitution with sigma
    SigmaMap = MinimalMap#{
        <<"sigma">> => #{
            <<"type_system">> => <<"static">>,
            <<"type_bindings">> => [
                #{
                    <<"term">> => <<"Data">>,
                    <<"type">> => <<"binary">>,
                    <<"token_contract">> => #{
                        <<"shape">> => <<"multiple">>,
                        <<"validity">> => <<"lazy">>
                    }
                }
            ]
        }
    },
    Constitution2 = ga_constitution:from_map(SigmaMap),
    ?assertEqual(ok, ga_constitution:validate(Constitution2)),

    %% Test 3: Constitution with refusals
    RefusalMap = MinimalMap#{
        <<"refusals">> => [
            #{
                <<"state">> => <<"pending_approval">>,
                <<"refused_transitions">> => [<<"t_ship">>],
                <<"refusal_reason">> => <<"Must approve before shipping">>
            }
        ]
    },
    Constitution3 = ga_constitution:from_map(RefusalMap),
    ?assertEqual(ok, ga_constitution:validate(Constitution3)),

    %% Test 4: Invalid constitution (missing id)
    InvalidMap = #{<<"version">> => <<"1.0">>},
    ?assertException(error, {invalid_constitution, missing_id}, ga_constitution:from_map(InvalidMap)),

    ok.

%% @doc Tests YAML parsing and serialization roundtrip
ga_yaml_roundtrip_test(_Config) ->
    %% Test 1: Parse and serialize back
    Yaml = <<"
constitution:
  id: purchase_order_workflow
  version: 1.0.0
  sigma:
    type_system: behavioral
    type_bindings:
      - term: PurchaseOrder
        type: Order
        token_contract:
          shape: singleton
          validity: eager
      - term: Invoice
        type: Billing
        token_contract:
          shape: multiple
          validity: lazy
  refusals:
    - state: pending_approval
      refused_transitions:
        - t_ship
        - t_invoice
      refusal_reason: Requires approval before shipping
  quality_gates:
    - name: audit_trail
      invariant: forall t. receipt_exists(t)
      replay_enabled: true
      provenance_enabled: true
  lambda:
    compilation_strategy: topological
    pattern_sequence:
      - pattern: P2_ParallelSplit
        instance_id: split_approval_paths
        config: {}
      - pattern: P3_Synchronization
        instance_id: merge_approval_paths
">>,

    {ok, ParsedConstitution} = ga_yaml:from_yaml(Yaml),

    %% Serialize back to YAML
    SerializedYaml = ga_constitution:to_yaml(ParsedConstitution),
    ?assertMatch(<<_/binary>>, SerializedYaml),  %% Should be binary

    %% Parse the serialized version
    {ok, RoundtripConstitution} = ga_yaml:from_yaml(SerializedYaml),
    ?assertEqual(ParsedConstitution#constitution.id,
                 RoundtripConstitution#constitution.id),

    ok.

%% @doc Tests the full GA compiler pipeline
ga_compiler_full_pipeline_test(_Config) ->
    %% Create a complete constitution with all components
    FullMap = #{
        <<"id">> => <<"full_test_wf">>,
        <<"version">> => <<"2.0">>,
        <<"sigma">> => #{
            <<"type_system">> => <<"behavioral">>,
            <<"type_bindings">> => [
                #{
                    <<"term">> => <<"Request">>,
                    <<"type">> => <<"ServiceRequest">>,
                    <<"token_contract">> => #{
                        <<"shape">> => <<"singleton">>,
                        <<"validity">> => <<"eager">>
                    }
                }
            ]
        },
        <<"refusals">> => [
            #{
                <<"state">> => <<"validating">>,
                <<"refused_transitions">> => [<<"t_process">>],
                <<"refusal_reason">> => <<"Validation required">>
            }
        ],
        <<"quality_gates">> => [
            #{
                <<"name">> => <<"logging">>,
                <<"invariant">> => <<"all_events_logged">>,
                <<"replay_enabled">> => true,
                <<"provenance_enabled">> => true
            }
        ],
        <<"lambda">> => #{
            <<"compilation_strategy">> => <<"parallel">>,
            <<"pattern_sequence">> => [
                #{
                    <<"pattern">> => <<"P2_ParallelSplit">>,
                    <<"instance_id">> => <<"parallel_1">>,
                    <<"config">> => #{branches => 3}
                },
                #{
                    <<"pattern">> => <<"P3_Synchronization">>,
                    <<"instance_id">> => <<"sync_1">>,
                    <<"config">> => #{}
                }
            ]
        }
    },

    %% Compile the constitution
    {ok, Compilation} = ga_compiler:compile(FullMap),
    ?assertMatch({ok, _}, ga_compiler:compile(FullMap)),
    ?assertMatch({compilation, _, _, _, _, _}, Compilation),

    %% Verify compilation result has expected fields (using tuple access)
    is_tuple(Compilation) andalso element(1, Compilation) =:= compilation,

    ok.

%% @doc Tests circuit breaker failure and recovery
circuit_breaker_failure_recovery_test(_Config) ->
    {ok, _Breaker} = circuit_breaker:start_link(
        <<"test_breaker_fail">>,
        fun() -> ok end,
        [
            {failure_threshold, 2},
            {timeout_ms, 100},
            {success_threshold, 1}
        ]
    ),

    %% Execute successful calls
    ?assertMatch({ok, _}, circuit_breaker:execute(<<"test_breaker_fail">>, fun() -> ok end)),
    ?assertMatch({ok, _}, circuit_breaker:execute(<<"test_breaker_fail">>, fun() -> ok end)),

    %% Trigger failures to open circuit
    FailFun = fun() -> erlang:error(simulated_failure) end,
    ?assertMatch({error, _}, circuit_breaker:execute(<<"test_breaker_fail">>, FailFun)),
    ?assertMatch({error, _}, circuit_breaker:execute(<<"test_breaker_fail">>, FailFun)),

    %% Circuit should be open
    ?assertMatch({error, circuit_open},
                 circuit_breaker:execute(<<"test_breaker_fail">>, fun() -> ok end)),

    %% Wait for timeout and try again (should transition to half-open)
    timer:sleep(150),
    ?assertMatch({ok, _}, circuit_breaker:execute(<<"test_breaker_fail">>, fun() -> ok end)),

    %% Circuit should be closed again after success
    ?assertMatch({ok, _}, circuit_breaker:execute(<<"test_breaker_fail">>, fun() -> ok end)),

    circuit_breaker:stop(<<"test_breaker_fail">>),

    ok.

%% @doc Tests persistent timer with work-day calendar
persistent_timer_calendar_test(_Config) ->
    {ok, _TimerPid} = wf_persistent_timer:start_link([
        {timezone, <<"America/New_York">>},
        {work_start, {9, 0}},
        {work_end, {17, 0}},
        {weekend_days, [6, 0]}
    ]),

    %% Test 1: Add and list holidays
    ?assertEqual(ok, wf_persistent_timer:add_holiday(<<"2024-07-04">>, <<"Independence Day">>)),
    ?assertEqual(ok, wf_persistent_timer:add_holiday(<<"2024-12-25">>, <<"Christmas">>)),
    Holidays = wf_persistent_timer:list_holidays(),
    ?assertEqual(2, length(Holidays)),

    %% Test 2: Get work hours
    ?assertMatch({{9, 0}, {17, 0}}, wf_persistent_timer:get_work_hours()),

    %% Test 3: Set different work hours
    ?assertEqual(ok, wf_persistent_timer:set_work_hours({8, 30}, {16, 30})),
    ?assertMatch({{8, 30}, {16, 30}}, wf_persistent_timer:get_work_hours()),

    %% Test 4: Start a timer
    TargetTime = erlang:system_time(millisecond) + 5000,
    {ok, TimerId} = wf_persistent_timer:start_timer(
        <<"exec_calendar_test">>,
        TargetTime
    ),
    ?assert(is_binary(TimerId)),

    %% Test 5: Get timer info
    ?assertMatch({ok, _Timer}, wf_persistent_timer:get_timer(TimerId)),

    %% Test 6: Cancel timer
    ?assertEqual(ok, wf_persistent_timer:cancel_timer(TimerId)),

    wf_persistent_timer:cancel_all_timers(),

    ok.

%% @doc Tests time travel debugger functionality
time_travel_debugger_test(_Config) ->
    {ok, _Pid} = wf_time_travel:start_link(#{}),

    %% Test 1: Create and manage sessions
    {ok, SessionId} = wf_time_travel:start_session(
        <<"debug_case">>,
        test_module,
        #{breakpoints => [{transition, t_cancel}]}
    ),
    ?assert(is_binary(SessionId)),

    %% Test 2: Record various event types
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

    ?assertEqual(ok, wf_time_travel:record_mode_change(
        SessionId,
        test_module,
        auto,
        manual
    )),

    ?assertEqual(ok, wf_time_travel:record_token_event(
        SessionId,
        p_start,
        start,
        consume,
        p_active
    )),

    %% Test 3: Replay timeline
    {ok, Timeline} = wf_time_travel:get_timeline(SessionId),
    ?assertEqual(4, length(Timeline)),

    {ok, Events} = wf_time_travel:replay_from_start(SessionId),
    ?assertEqual(4, length(Events)),

    %% Test 4: Step through events
    {ok, _} = wf_time_travel:step_forward(SessionId),
    {ok, _} = wf_time_travel:step_forward(SessionId),
    {ok, _} = wf_time_travel:step_backward(SessionId),

    %% Test 5: Jump to specific index
    {ok, _} = wf_time_travel:jump_to(SessionId, 0),

    %% Test 6: Breakpoints
    {ok, Breakpoints} = wf_time_travel:list_breakpoints(SessionId),
    ?assertEqual(1, length(Breakpoints)),

    %% Test 7: Get state at timestamp
    Now = erlang:system_time(microsecond),
    {ok, _State} = wf_time_travel:get_state_at(SessionId, Now),

    wf_time_travel:stop_session(SessionId),

    ok.

%% @doc Tests hierarchical cancellation region management
cancel_region_hierarchy_test(_Config) ->
    %% Test 1: Define hierarchical regions
    ?assertEqual(ok, cancel_region:define_region(
        <<"root_region">>,
        undefined,
        [p_root_1, p_root_2]
    )),

    ?assertEqual(ok, cancel_region:define_region(
        <<"child_region_1">>,
        <<"root_region">>,
        [p_child_1, p_child_2]
    )),

    ?assertEqual(ok, cancel_region:define_region(
        <<"child_region_2">>,
        <<"root_region">>,
        [p_child_3]
    )),

    ?assertEqual(ok, cancel_region:define_region(
        <<"grandchild_region">>,
        <<"child_region_1">>,
        [p_grandchild_1]
    )),

    %% Test 2: Register activities in regions
    ?assertEqual(ok, cancel_region:register_activity(
        <<"activity_1">>,
        <<"root_region">>,
        p_root_1
    )),

    ?assertEqual(ok, cancel_region:register_activity(
        <<"activity_2">>,
        <<"child_region_1">>,
        p_child_1
    )),

    ?assertEqual(ok, cancel_region:register_activity(
        <<"activity_3">>,
        <<"grandchild_region">>,
        p_grandchild_1
    )),

    %% Test 3: Cancel region and verify
    ?assertEqual(ok, cancel_region:cancel_region(<<"child_region_1">>)),

    ?assertEqual(ok, cancel_region:unregister_activity(<<"activity_1">>)),
    ?assertEqual(ok, cancel_region:unregister_activity(<<"activity_2">>)),
    ?assertEqual(ok, cancel_region:unregister_activity(<<"activity_3">>)),

    %% Test 4: Cancel entire case
    ?assertEqual(ok, cancel_region:cancel_case(<<"test_hier_case">>)),

    ok.
