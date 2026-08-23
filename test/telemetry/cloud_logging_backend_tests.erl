%% -*- erlang -*-
%% @doc Unit tests for cloud_logging_backend

-module(cloud_logging_backend_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Mock configuration
test_config() ->
    #{log_name => <<"projects/test-project/logs/cre">>,
      resource => #{type => <<"global">>, labels => #{}},
      batch_size => 2,
      batch_interval_ms => 100,
      max_retries => 2,
      retry_delay_ms => 50}.

minimal_config() ->
    #{log_name => <<"projects/test/logs/erlang">>}.

%%====================================================================
%% Config Validation Tests
%%====================================================================

adding_handler_valid_config_test() ->
    Config = #{config => test_config()},
    Result = cloud_logging_backend:adding_handler(Config),
    ?assertMatch({ok, _State}, Result).

adding_handler_missing_log_name_test() ->
    Config = #{config => #{resource => #{type => <<"global">>}}},
    Result = cloud_logging_backend:adding_handler(Config),
    ?assertMatch({error, {missing_config, log_name}}, Result).

adding_handler_invalid_log_name_test() ->
    Config = #{config => #{log_name => invalid_type}},
    Result = cloud_logging_backend:adding_handler(Config),
    ?assertMatch({error, {invalid_config, _}}, Result).

adding_handler_minimal_config_test() ->
    Config = #{config => minimal_config()},
    Result = cloud_logging_backend:adding_handler(Config),
    ?assertMatch({ok, _State}, Result).

changing_config_test() ->
    OldConfig = #{config => test_config()},
    {ok, State} = cloud_logging_backend:adding_handler(OldConfig),

    NewConfig = #{config => test_config#{batch_size => 5}},
    Result = cloud_logging_backend:changing_config(NewConfig, State),
    ?assertMatch({ok, _NewState}, Result).

%%====================================================================
%% Log Formatting Tests
%%====================================================================

log_basic_message_test() ->
    {ok, State} = cloud_logging_backend:adding_handler(#{config => test_config()}),

    LogEvent = #{
        level => info,
        msg => {"Test message", []},
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    ?assertEqual(ok, cloud_logging_backend:log(LogEvent, State)).

log_error_message_test() ->
    {ok, State} = cloud_logging_backend:adding_handler(#{config => test_config()}),

    LogEvent = #{
        level => error,
        msg => {"Error occurred: ~p", [test_error]},
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    ?assertEqual(ok, cloud_logging_backend:log(LogEvent, State)).

log_debug_message_test() ->
    {ok, State} = cloud_logging_backend:adding_handler(#{config => test_config()}),

    LogEvent = #{
        level => debug,
        msg => <<"Debug info">>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    ?assertEqual(ok, cloud_logging_backend:log(LogEvent, State)).

log_with_metadata_test() ->
    {ok, State} = cloud_logging_backend:adding_handler(#{config => test_config()}),

    LogEvent = #{
        level => info,
        msg => <<"Message with metadata">>,
        meta => #{module => test_module,
                  function => test_function,
                  line => 42,
                  custom_field => custom_value},
        time => erlang:system_time(millisecond)
    },

    ?assertEqual(ok, cloud_logging_backend:log(LogEvent, State)).

log_report_message_test() ->
    {ok, State} = cloud_logging_backend:adding_handler(#{config => test_config()}),

    Report = #{error_type => test_error,
               details => <<"Test details">>},
    LogEvent = #{
        level => error,
        msg => {report, Report},
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    ?assertEqual(ok, cloud_logging_backend:log(LogEvent, State)).

%%====================================================================
%% Batch Handling Tests
%%====================================================================

batch_flush_on_size_test() ->
    Config = test_config(),
    {ok, State} = cloud_logging_backend:adding_handler(#{config => Config}),

    LogEvent = #{
        level => info,
        msg => <<"Test">>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    %% Add logs up to batch size
    cloud_logging_backend:log(LogEvent, State),
    cloud_logging_backend:log(LogEvent, State),

    %% Should trigger flush
    ?assertEqual(ok, cloud_logging_backend:flush(State)).

batch_buffering_test() ->
    Config = test_config(),
    {ok, State} = cloud_logging_backend:adding_handler(#{config => Config}),

    LogEvent = #{
        level => info,
        msg => <<"Buffer test">>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    %% Single log should not flush immediately
    cloud_logging_backend:log(LogEvent, State),
    ?assertEqual(ok, cloud_logging_backend:flush(State)).

%%====================================================================
%% Severity Mapping Tests
%%====================================================================

severity_mapping_test_() ->
    Levels = [
        {emergency, <<"EMERGENCY">>},
        {alert, <<"ALERT">>},
        {critical, <<"CRITICAL">>},
        {error, <<"ERROR">>},
        {warning, <<"WARNING">>},
        {notice, <<"NOTICE">>},
        {info, <<"INFO">>},
        {debug, <<"DEBUG">>}
    ],

    [{Level,
      fun() ->
          Severity = cloud_logging_formatter:level_to_severity(Level),
          ?assertEqual(Expected, Severity)
      end}
     || {Level, Expected} <- Levels].

%%====================================================================
%% Formatter Tests
%%====================================================================

formatter_basic_message_test() ->
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => #{type => <<"global">>, labels => #{},
               labels => #{}}},

    LogEvent = #{
        level => info,
        msg => <<"Test message">>,
        meta => #{},
        time => 1704067200000  % Fixed timestamp
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),

    ?assertEqual(<<"projects/test/logs/app">>, maps:get(logName, Entry)),
    ?assertEqual(<<"INFO">>, maps:get(severity, Entry)),
    ?assertMatch(#{message := <<"Test message">>}, maps:get(jsonPayload, Entry)).

formatter_with_resource_test() ->
    Resource = #{type => <<"gce_instance">>,
                 labels => #{instance_id => <<"123">>, zone => <<"us-central1-a">>}},
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => Resource,
               labels => #{}},

    LogEvent = #{
        level => info,
        msg => <<"Test">>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),

    ?assertEqual(<<"gce_instance">>, maps:get(type, maps:get(resource, Entry))).

formatter_with_labels_test() ->
    Config = cloud_logging_formatter:add_label(
        #{log_name => <<"projects/test/logs/app">>,
          resource => #{type => <<"global">>, labels => #{}},
          labels => #{}},
        <<"environment">>, production),

    LogEvent = #{
        level => info,
        msg => <<"Test">>,
        meta => #{module => my_module},
        time => erlang:system_time(millisecond)
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),

    ?assertEqual(<<"production">>, maps:get(<<"environment">>, maps:get(labels, Entry))).

formatter_timestamp_format_test() ->
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => #{type => <<"global">>, labels => #{}},
               labels => #{}},

    %% Use known timestamp: 2024-01-01 00:00:00 UTC in milliseconds
    Timestamp = 1704067200000,
    LogEvent = #{
        level => info,
        msg => <<"Test">>,
        meta => #{},
        time => Timestamp
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),
    FormattedTimestamp = maps:get(timestamp, Entry),

    %% Should contain date and time
    ?assertMatch(true, binary:match(FormattedTimestamp, <<"2024-01-01">>) =/= nomatch).

formatter_report_message_test() ->
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => #{type => <<"global">>, labels => #{}},
               labels => #{}},

    Report = #{error => <<"test_error">>, reason => timeout},
    LogEvent = #{
        level => error,
        msg => {report, Report},
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),
    Payload = maps:get(jsonPayload, Entry),

    ?assertEqual(error, maps:get(level, Payload)),
    ?assert(is_binary(maps:get(message, Payload))).

%%====================================================================
%% Resource Configuration Tests
%%====================================================================

set_log_name_test() ->
    Config = #{},
    NewConfig = cloud_logging_formatter:set_log_name(Config, <<"projects/myapp/logs/custom">>),
    ?assertEqual(<<"projects/myapp/logs/custom">>, maps:get(log_name, NewConfig)).

set_resource_test() ->
    Resource = #{type => <<"k8s_container">>,
                 labels => #{namespace => <<"default">>, pod_name => <<"my-pod">>}},
    Config = #{},
    NewConfig = cloud_logging_formatter:set_resource(Config, Resource),
    ?assertEqual(Resource, maps:get(resource, NewConfig)).

add_label_test() ->
    Config = #{labels => #{existing => <<"value">>}},
    NewConfig = cloud_logging_formatter:add_label(Config, <<"new_label">>, <<"new_value">>),
    Labels = maps:get(labels, NewConfig),
    ?assertEqual(<<"value">>, maps:get(existing, Labels)),
    ?assertEqual(<<"new_value">>, maps:get(<<"new_label">>, Labels)).

add_label_to_empty_config_test() ->
    Config = #{},
    NewConfig = cloud_logging_formatter:add_label(Config, <<"env">>, prod),
    ?assertEqual(<<"prod">>, maps:get(<<"env">>, maps:get(labels, NewConfig))).

%%====================================================================
%% Stats Tests
%%====================================================================

get_stats_test() ->
    {ok, State} = cloud_logging_backend:adding_handler(#{config => test_config()}),
    Stats = cloud_logging_backend:get_stats(State),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(logs_sent, Stats)),
    ?assert(maps:is_key(logs_failed, Stats)),
    ?assert(maps:is_key(batches_sent, Stats)),
    ?assert(maps:is_key(last_error, Stats)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

empty_message_test() ->
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => #{type => <<"global">>, labels => #{}},
               labels => #{}},

    LogEvent = #{
        level => info,
        msg => <<>>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),
    ?assertMatch(#{message := <<>>}, maps:get(jsonPayload, Entry)).

special_characters_in_message_test() ->
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => #{type => <<"global">>, labels => #{}},
               labels => #{}},

    LogEvent = #{
        level => info,
        msg => <<"Message with \"quotes\" and 'apostrophes' and \n newlines">>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),
    ?assert(is_binary(maps:get(message, maps:get(jsonPayload, Entry)))).

unicode_message_test() ->
    Config = #{log_name => <<"projects/test/logs/app">>,
               resource => #{type => <<"global">>, labels => #{}},
               labels => #{}},

    LogEvent = #{
        level => info,
        msg => <<"Unicode test: \x{2603} snowman \x{1F600} grin">>,
        meta => #{},
        time => erlang:system_time(millisecond)
    },

    Entry = cloud_logging_formatter:format(LogEvent, Config),
    ?assert(is_binary(maps:get(message, maps:get(jsonPayload, Entry)))).

%%====================================================================
%% Integration Tests
%%====================================================================

full_log_flow_test_() ->
    {setup,
     fun() ->
         logger:add_handler_ref(cloud_test, cloud_logging_backend,
                               #{config => test_config()})
     end,
     fun(_) ->
         logger:remove_handler(cloud_test)
     end,
     fun(_) ->
         [
          ?_test(begin
                    logger:info("Test info message"),
                    ok
                end),
          ?_test(begin
                    logger:error("Test error message: ~p", [test_error]),
                    ok
                end),
          ?_test(begin
                    logger:debug("Test debug with metadata", #{module => test_module}),
                    ok
                end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

format_label_value_binary_test() ->
    ?assertEqual(<<"test">>, cloud_logging_formatter:add_label(#{}, <<"key">>, <<"test">>)),
    ok.

format_label_value_atom_test() ->
    Config = cloud_logging_formatter:add_label(#{}, <<"key">>, test_atom),
    ?assertEqual(<<"test_atom">>, maps:get(<<"key">>, maps:get(labels, Config))).

format_label_value_integer_test() ->
    Config = cloud_logging_formatter:add_label(#{}, <<"key">>, 42),
    ?assertEqual(<<"42">>, maps:get(<<"key">>, maps:get(labels, Config))).

format_label_value_list_test() ->
    Config = cloud_logging_formatter:add_label(#{}, <<"key">>, "test_string"),
    ?assertEqual(<<"test_string">>, maps:get(<<"key">>, maps:get(labels, Config))).
