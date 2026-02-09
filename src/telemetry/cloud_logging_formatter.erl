%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Google Cloud Logging Log Entry Formatter
%%
%% @doc Cloud Logging Log Entry Formatter
%%
%% Formats log entries according to Google Cloud Logging JSON format.
%% Supports structured logging with labels, severity mapping, and
%% resource metadata.
%%
%% == Cloud Logging Entry Format ==
%%
%% ```json
%% {
%%   "logName": "projects/PROJECT_ID/logs/LOG_ID",
%%   "resource": {
%%     "type": "global",
%%     "labels": {
%%       "key": "value"
%%     }
%%   },
%%   "severity": "INFO",
%%   "timestamp": "2024-01-01T12:00:00.000Z",
%%   "labels": {
%%     "custom_label": "value"
%%   },
%%   "jsonPayload": {
%%     "message": "Log message",
%%     "level": "info",
%%     "metadata": {}
%%   }
%% }
%% ```
%%
%% @end

-module(cloud_logging_formatter).
-author("CRE Team").

%% API
-export([format/2]).
-export([set_log_name/2, set_resource/2]).
-export([add_label/3]).
-export([level_to_severity/1]).

%%====================================================================
%% Types
%%====================================================================

-type formatter_config() :: #{log_name => binary(),
                               resource => resource(),
                               labels => map()}.

-type resource() :: #{type => binary(),
                       labels => map()}.

-type log_entry() :: #{log_name := binary(),
                        resource := resource(),
                        severity := binary(),
                        timestamp := binary(),
                        labels => map(),
                        json_payload := map()}.

-export_type([formatter_config/0, resource/0, log_entry/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Format a log event for Cloud Logging.
-spec format(logger:log_event(), formatter_config()) -> log_entry().
format(LogEvent, Config) ->
    #{level := Level,
      msg := Msg,
      meta := Meta,
      time := Timestamp} = LogEvent,

    LogName = maps:get(log_name, Config, default_log_name()),
    Resource = maps:get(resource, Config, default_resource()),
    BaseLabels = maps:get(labels, Config, #{}),

    Severity = level_to_severity(Level),
    FormattedMsg = format_message(Msg),

    Entry = #{logName => LogName,
              resource => Resource,
              severity => Severity,
              timestamp => format_timestamp(Timestamp),
              jsonPayload => build_payload(FormattedMsg, Level, Meta)},

    %% Merge labels from config and metadata
    MergedLabels = maps:merge(BaseLabels, extract_labels(Meta)),
    case maps:size(MergedLabels) of
        0 -> Entry;
        _ -> Entry#{labels => MergedLabels}
    end.

%% @doc Set the log name in the config.
-spec set_log_name(formatter_config(), binary()) -> formatter_config().
set_log_name(Config, LogName) when is_binary(LogName) ->
    Config#{log_name => LogName}.

%% @doc Set the resource in the config.
-spec set_resource(formatter_config(), resource()) -> formatter_config().
set_resource(Config, Resource) when is_map(Resource) ->
    Config#{resource => Resource}.

%% @doc Add a label to the config.
-spec add_label(formatter_config(), binary(), term()) -> formatter_config().
add_label(Config, Key, Value) when is_binary(Key) ->
    Labels = maps:get(labels, Config, #{}),
    Config#{labels => Labels#{Key => format_label_value(Value)}}.

%% @doc Convert logger level to Cloud Logging severity.
-spec level_to_severity(logger:level()) -> binary().
level_to_severity(emergency) -> <<"EMERGENCY">>;
level_to_severity(alert) -> <<"ALERT">>;
level_to_severity(critical) -> <<"CRITICAL">>;
level_to_severity(error) -> <<"ERROR">>;
level_to_severity(warning) -> <<"WARNING">>;
level_to_severity(notice) -> <<"NOTICE">>;
level_to_severity(info) -> <<"INFO">>;
level_to_severity(debug) -> <<"DEBUG">>;
level_to_severity(_) -> <<"DEFAULT">>.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec default_log_name() -> binary().
default_log_name() ->
    ProjectId = get_project_id(),
    <<"projects/", ProjectId/binary, "/logs/cre">>.

%% @private
-spec default_resource() -> resource().
default_resource() ->
    #{
        type => <<"global">>,
        labels => #{}
    }.

%% @private
-spec get_project_id() -> binary().
get_project_id() ->
    case os:getenv("GOOGLE_CLOUD_PROJECT") of
        false -> <<"default-project">>;
        ProjectId -> list_to_binary(ProjectId)
    end.

%% @private
-spec format_timestamp(integer()) -> binary().
format_timestamp(Millis) ->
    %% Convert milliseconds since epoch to ISO 8601 format
    Seconds = Millis div 1000,
    MillisPart = Millis rem 1000,

    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Seconds, seconds),

    Format = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
                          [Year, Month, Day, Hour, Minute, Second, MillisPart]),
    iolist_to_binary(Format).

%% @private
-spec format_message(logger:msg()) -> binary().
format_message({string, String}) ->
    iolist_to_binary(String);
format_message({report, Report}) ->
    format_report(Report);
format_message(Msg) when is_binary(Msg) ->
    Msg;
format_message(Msg) when is_list(Msg) ->
    iolist_to_binary(Msg);
format_message(Msg) ->
    iolist_to_binary(io_lib:format("~p", [Msg])).

%% @private
-spec format_report(logger:report()) -> binary().
format_report(Report) when is_map(Report) ->
    Formatted = maps:fold(fun(K, V, Acc) ->
        [io_lib:format("~s=~p ", [K, V]) | Acc]
    end, [], Report),
    iolist_to_binary(lists:reverse(Formatted));
format_report(Report) ->
    iolist_to_binary(io_lib:format("~p", [Report])).

%% @private
-spec build_payload(binary(), logger:level(), logger:metadata()) -> map().
build_payload(Message, Level, Meta) ->
    Base = #{message => Message, level => Level},

    %% Add important metadata fields
    Enhanced = case maps:get(domain, Meta, undefined) of
        undefined -> Base;
        Domain -> Base#{domain => Domain}
    end,

    Enhanced1 = case maps:get(pid, Meta, undefined) of
        undefined -> Enhanced;
        Pid -> Enhanced#{pid => pid_to_list(Pid)}
    end,

    %% Add any custom metadata (excluding system fields)
    CustomFields = extract_custom_fields(Meta),
    maps:merge(Enhanced1, CustomFields).

%% @private
-spec extract_custom_fields(logger:metadata()) -> map().
extract_custom_fields(Meta) ->
    SystemKeys = [domain, report_cb, pid, time, gl],
    maps:filter(fun(K, _V) -> not lists:member(K, SystemKeys) end, Meta).

%% @private
-spec extract_labels(logger:metadata()) -> map().
extract_labels(Meta) ->
    %% Extract fields that should be Cloud Logging labels
    LabelKeys = [module, function, line, mfa],
    maps:filter(fun(K, _V) -> lists:member(K, LabelKeys) end, Meta).

%% @private
-spec format_label_value(term()) -> binary().
format_label_value(V) when is_binary(V) -> V;
format_label_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_label_value(V) when is_integer(V) -> integer_to_binary(V);
format_label_value(V) when is_list(V) -> list_to_binary(V);
format_label_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).
