%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author YAWL Reporter Implementation
%% @copyright 2025
%%
%% @doc YAWL Reporting and Data Export Module for CRE
%%
%% This module implements comprehensive reporting capabilities for YAWL
%% workflows, including multiple export formats and scheduled report generation.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Multiple Formats:</b> JSON, XML, CSV, HTML export support</li>
%%   <li><b>Case Data Export:</b> Export complete case histories</li>
%%   <li><b>Period Reports:</b> Time-based reporting with configurable intervals</li>
%%   <li><b>Custom Reports:</b> Create reports with custom filters and aggregations</li>
%%   <li><b>Scheduled Reports:</b> Automated report generation and delivery</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% <pre>
%% %% Generate a JSON report for a case
%% {ok, Report} = yawl_reporter:generate_report(json, case_id, <<"case_123">>).
%%
%% %% Export case data to CSV
%% yawl_reporter:export_case_data(<<"case_123">>, csv).
%%
%% %% Schedule daily reports
%% yawl_reporter:schedule_report(daily, #{format => json}, self()).
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_reporter).

%%====================================================================
%% Macros
%%====================================================================

-define(DAY, 86400000).   %% 24 hours in milliseconds
-define(HOUR, 3600000).   %% 1 hour in milliseconds
-define(MINUTE, 60000).   %% 1 minute in milliseconds

%%====================================================================
%% Exports
%%====================================================================

%% Report generation
-export([generate_report/3,
         export_case_data/2,
         export_case_data/3,
         export_period_data/3,
         create_custom_report/2,
         schedule_report/3,
         cancel_schedule/1,
         get_available_reports/0,
         list_schedules/0]).

%% Format-specific exports
-export([to_json/1, to_xml/1, to_csv/1, to_html/1]).

%%====================================================================
%% Types
%%====================================================================

-type report_format() :: json | xml | csv | html | pdf.
-type report_scope() :: case_id | period | engine | custom.
-type schedule_id() :: binary().

-record(report_config, {
    format :: report_format(),
    scope :: report_scope(),
    filters :: map(),
    include_metadata :: boolean()
}).

-record(schedule, {
    id :: schedule_id(),
    interval :: hourly | daily | weekly | monthly,
    config :: #report_config{},
    recipient :: pid() | binary(),
    next_run :: integer()
}).

-type report_config() :: #report_config{}.
-type schedule() :: #schedule{}.

-export_type([report_config/0, schedule/0]).

%% Import metric record from yawl_monitor for pattern matching
-record(metric, {
    name :: binary(),
    value :: number(),
    timestamp :: integer(),
    labels :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a report in the specified format.
%%
%% @param Format The output format (json, xml, csv, html).
%% @param Scope The report scope (case_id, period, engine, custom).
%% @param Identifier The identifier for the scope (e.g., case ID or date range).
%% @return {ok, ReportBinary} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_report(Format :: report_format(),
                     Scope :: report_scope(),
                     Identifier :: term()) -> {ok, binary()} | {error, term()}.

generate_report(json, case_id, CaseId) ->
    Data = fetch_case_data(CaseId),
    {ok, to_json(Data)};
generate_report(json, period, {Start, End}) ->
    Data = fetch_period_data(Start, End),
    {ok, to_json(Data)};
generate_report(json, engine, _Node) ->
    Data = fetch_engine_data(),
    {ok, to_json(Data)};

generate_report(xml, case_id, CaseId) ->
    Data = fetch_case_data(CaseId),
    {ok, to_xml(Data)};
generate_report(xml, period, {Start, End}) ->
    Data = fetch_period_data(Start, End),
    {ok, to_xml(Data)};
generate_report(xml, engine, _Node) ->
    Data = fetch_engine_data(),
    {ok, to_xml(Data)};

generate_report(csv, case_id, CaseId) ->
    Data = fetch_case_data(CaseId),
    {ok, to_csv(Data)};
generate_report(csv, period, {Start, End}) ->
    Data = fetch_period_data(Start, End),
    {ok, to_csv(Data)};
generate_report(csv, engine, _Node) ->
    Data = fetch_engine_data(),
    {ok, to_csv(Data)};

generate_report(html, case_id, CaseId) ->
    Data = fetch_case_data(CaseId),
    {ok, to_html(Data)};
generate_report(html, period, {Start, End}) ->
    Data = fetch_period_data(Start, End),
    {ok, to_html(Data)};
generate_report(html, engine, _Node) ->
    Data = fetch_engine_data(),
    {ok, to_html(Data)};

generate_report(pdf, _Scope, _Identifier) ->
    {error, pdf_not_supported};

generate_report(_Format, _Scope, _Identifier) ->
    {error, unsupported_combination}.

%%--------------------------------------------------------------------
%% @doc Exports case data to the specified format.
%%
%% @param CaseId The case identifier.
%% @param Format The export format.
%% @return {ok, ReportBinary} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec export_case_data(CaseId :: binary(), Format :: report_format()) ->
          {ok, binary()} | {error, term()}.

export_case_data(CaseId, Format) ->
    export_case_data(CaseId, Format, #{}).

%%--------------------------------------------------------------------
%% @doc Exports case data with options.
%%
%% @param CaseId The case identifier.
%% @param Format The export format.
%% @param Options Export options map.
%% @return {ok, ReportBinary} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec export_case_data(CaseId :: binary(),
                      Format :: report_format(),
                      Options :: map()) ->
          {ok, binary()} | {error, term()}.

export_case_data(CaseId, Format, Options) ->
    IncludeMetadata = maps:get(include_metadata, Options, true),
    IncludeEvents = maps:get(include_events, Options, true),

    Data = #{
        case_id => CaseId,
        exported_at => erlang:system_time(millisecond),
        metadata => case IncludeMetadata of
            true -> fetch_case_metadata(CaseId);
            false -> #{}
        end,
        events => case IncludeEvents of
            true -> fetch_case_events(CaseId);
            false -> []
        end
    },

    case Format of
        json -> {ok, to_json(Data)};
        xml -> {ok, to_xml(Data)};
        csv -> {ok, to_csv(Data)};
        html -> {ok, to_html(Data)};
        pdf -> {error, pdf_not_supported}
    end.

%%--------------------------------------------------------------------
%% @doc Exports data for a specific time period.
%%
%% @param Start Start timestamp (milliseconds since epoch).
%% @param End End timestamp (milliseconds since epoch).
%% @param Format The export format.
%% @return {ok, ReportBinary} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec export_period_data(Start :: integer(),
                        End :: integer(),
                        Format :: report_format()) ->
          {ok, binary()} | {error, term()}.

export_period_data(Start, End, Format) ->
    Data = fetch_period_data(Start, End),

    BaseData = Data#{
        period => #{
            start => Start,
            end_time => End,
            duration_ms => End - Start
        }
    },

    case Format of
        json -> {ok, to_json(BaseData)};
        xml -> {ok, to_xml(BaseData)};
        csv -> {ok, to_csv(BaseData)};
        html -> {ok, to_html(BaseData)};
        pdf -> {error, pdf_not_supported}
    end.

%%--------------------------------------------------------------------
%% @doc Creates a custom report with specified filters.
%%
%% @param Filters Filter criteria for the report.
%% @param Format The export format.
%% @return {ok, ReportBinary} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec create_custom_report(Filters :: map(), Format :: report_format()) ->
          {ok, binary()} | {error, term()}.

create_custom_report(Filters, Format) ->
    Data = fetch_filtered_data(Filters),

    case Format of
        json -> {ok, to_json(Data)};
        xml -> {ok, to_xml(Data)};
        csv -> {ok, to_csv(Data)};
        html -> {ok, to_html(Data)};
        pdf -> {error, pdf_not_supported}
    end.

%%--------------------------------------------------------------------
%% @doc Schedules a report to be generated periodically.
%%
%% @param Interval The reporting interval (hourly, daily, weekly, monthly).
%% @param Config The report configuration.
%% @param Recipient The pid or email to receive the report.
%% @return {ok, ScheduleId}
%%
%% @end
%%--------------------------------------------------------------------
-spec schedule_report(Interval :: hourly | daily | weekly | monthly,
                     Config :: map(),
                     Recipient :: pid() | binary()) ->
          {ok, schedule_id()}.

schedule_report(Interval, Config, Recipient) when is_pid(Recipient); is_binary(Recipient) ->
    ScheduleId = generate_schedule_id(),

    ReportConfig = #report_config{
        format = maps:get(format, Config, json),
        scope = maps:get(scope, Config, engine),
        filters = maps:get(filters, Config, #{}),
        include_metadata = maps:get(include_metadata, Config, true)
    },

    NextRun = calculate_next_run(Interval),

    Schedule = #schedule{
        id = ScheduleId,
        interval = Interval,
        config = ReportConfig,
        recipient = Recipient,
        next_run = NextRun
    },

    %% Register the schedule
    gen_server:cast(yawl_reporter_scheduler, {register_schedule, Schedule}),

    logger:info("Report scheduled: id=~p interval=~p",
                [ScheduleId, Interval],
                [{module, ?MODULE}, {action, schedule_report},
                 {recipient, Recipient}, {next_run, NextRun}]),

    {ok, ScheduleId}.

%%--------------------------------------------------------------------
%% @doc Cancels a scheduled report.
%%
%% @param ScheduleId The schedule identifier.
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_schedule(ScheduleId :: schedule_id()) -> ok.

cancel_schedule(ScheduleId) ->
    gen_server:cast(yawl_reporter_scheduler, {cancel_schedule, ScheduleId}),
    logger:info("Report schedule cancelled: id=~p", [ScheduleId],
                 [{module, ?MODULE}, {action, cancel_schedule}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Lists all available report types.
%%
%% @return List of available report types.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_available_reports() -> [binary()].

get_available_reports() ->
    [
        <<"case_execution_report">>,
        <<"period_summary_report">>,
        <<"engine_performance_report">>,
        <<"resource_utilization_report">>,
        <<"exception_report">>,
        <<"work_item_report">>
    ].

%%--------------------------------------------------------------------
%% @doc Lists all scheduled reports.
%%
%% @return List of scheduled reports.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_schedules() -> [schedule()].

list_schedules() ->
    try
        gen_server:call(yawl_reporter_scheduler, list_schedules)
    catch
        exit:{noproc, _} ->
            %% Scheduler not started
            []
    end.

%%====================================================================
%% Format Conversion Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts data to JSON format.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_json(Data :: term()) -> binary().

to_json(Data) when is_map(Data); is_list(Data) ->
    try
        jsone:encode(Data, [use_utf8, {float_format, [{decimals, 4}]}])
    catch
        _:_ ->
            %% Fallback encoding
            encode_json(Data)
    end;
to_json(Data) ->
    to_json(#{data => Data}).

%%--------------------------------------------------------------------
%% @doc Converts data to XML format.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_xml(Data :: term()) -> binary().

to_xml(Data) ->
    Header = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
    Body = encode_xml(Data, <<"yawl_report">>, 0),
    <<Header/binary, Body/binary>>.

%%--------------------------------------------------------------------
%% @doc Converts data to CSV format.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_csv(Data :: term()) -> binary().

to_csv(Data) when is_map(Data) ->
    case maps:get(events, Data, []) of
        Events when is_list(Events), length(Events) > 0 ->
            %% Event-based CSV
            HeaderRow = encode_csv_header(events),
            DataRows = [encode_csv_event(E) || E <- Events],
            iolist_to_binary([HeaderRow, $\n | lists:join($\n, DataRows)]);
        _ ->
            %% Simple key-value CSV
            Rows = [[K, V] || {K, V} <- maps:to_list(Data)],
            Header = <<"key,value\n">>,
            DataRows = [encode_csv_row(R) || R <- Rows],
            iolist_to_binary([Header, lists:join($\n, DataRows)])
    end;
to_csv(Data) when is_list(Data) ->
    case Data of
        [] -> <<"">>;
        [First | _] when is_map(First) ->
            Keys = lists:sort(maps:keys(First)),
            Header = iolist_to_binary(lists:join($,, [escape_csv(K) || K <- Keys])),
            Rows = [encode_csv_row_from_map(M, Keys) || M <- Data],
            iolist_to_binary([Header, $\n | lists:join($\n, Rows)]);
        _ ->
            encode_csv(Data)
    end.

%%--------------------------------------------------------------------
%% @doc Converts data to HTML format.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_html(Data :: term()) -> binary().

to_html(Data) ->
    Title = case maps:get(case_id, Data, undefined) of
        undefined -> <<"YAWL Workflow Report">>;
        CaseId -> iolist_to_binary([<<"Case Report: ">>, CaseId])
    end,

    Style = <<"
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            h1 { color: #333; }
            table { border-collapse: collapse; width: 100%; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
            th { background-color: #4CAF50; color: white; }
            tr:nth-child(even) { background-color: #f2f2f2; }
            .summary { background-color: #e7f3ff; padding: 15px; margin: 10px 0; }
        </style>
    ">>,

    Content = generate_html_content(Data),

    <<"<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<title>",
        Title/binary, "</title>\n", Style/binary,
        "</head>\n<body>\n<h1>", Title/binary, "</h1>\n",
        Content/binary,
        "\n</body>\n</html>">>.

%%====================================================================
%% Internal Functions - Data Fetching
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches complete data for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_case_data(binary()) -> map().

fetch_case_data(CaseId) ->
    #{
        case_id => CaseId,
        metadata => fetch_case_metadata(CaseId),
        events => fetch_case_events(CaseId),
        tasks => fetch_case_tasks(CaseId),
        variables => fetch_case_variables(CaseId),
        exported_at => erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches case metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_case_metadata(binary()) -> map().

fetch_case_metadata(CaseId) ->
    %% Try to get data from monitor if available
    try
        case yawl_monitor:get_case_metrics(CaseId) of
            [] ->
                #{
                    case_id => CaseId,
                    status => unknown,
                    created_at => unknown,
                    completed_at => unknown
                };
            Metrics ->
                %% Extract metadata from metrics
                StartTime = find_metric_value(<<"case_started">>, Metrics, 0),
                EndTime = find_metric_value(<<"case_completed">>, Metrics, 0),
                Status = case EndTime of
                    0 -> running;
                    _ -> completed
                end,

                #{
                    case_id => CaseId,
                    status => Status,
                    created_at => StartTime,
                    completed_at => EndTime,
                    cycle_time_ms => EndTime - StartTime
                }
        end
    catch
        _:_ ->
            #{
                case_id => CaseId,
                status => unknown,
                created_at => unknown,
                completed_at => unknown
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches case events.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_case_events(binary()) -> [map()].

fetch_case_events(CaseId) ->
    try
        Metrics = yawl_monitor:get_case_metrics(CaseId),
        lists:map(
            fun(#metric{name = Name, value = Value, timestamp = TS, labels = Labels}) ->
                #{
                    timestamp => TS,
                    event_name => Name,
                    event_value => Value,
                    labels => Labels
                }
            end,
            Metrics
        )
    catch
        _:_ -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches case tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_case_tasks(binary()) -> [map()].

fetch_case_tasks(_CaseId) ->
    %% This would integrate with the workflow engine
    %% For now, return empty list
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches case variables.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_case_variables(binary()) -> map().

fetch_case_variables(_CaseId) ->
    %% This would integrate with the workflow engine
    %% For now, return empty map
    #{}.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches data for a time period.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_period_data(integer(), integer()) -> map().

fetch_period_data(Start, End) ->
    try
        Query = #{time_range => {Start, End}},
        Metrics = yawl_monitor:get_metrics(Query, 10000),

        %% Aggregate metrics by name
        Grouped = lists:foldl(
            fun(#metric{name = Name} = M, Acc) ->
                maps:update_with(Name,
                    fun(Existing) -> [M | Existing] end,
                    [M],
                    Acc)
            end,
            #{},
            Metrics
        ),

        #{
            period_start => Start,
            period_end => End,
            total_metrics => length(Metrics),
            metrics_by_name => Grouped
        }
    catch
        _:_ ->
            #{
                period_start => Start,
                period_end => End,
                total_metrics => 0,
                metrics_by_name => #{}
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches engine-level data.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_engine_data() -> map().

fetch_engine_data() ->
    try
        Metrics = yawl_monitor:get_engine_metrics(),
        Summary = yawl_monitor:get_metric_summary(),

        #{
            engine_metrics => Metrics,
            metric_summary => Summary,
            exported_at => erlang:system_time(millisecond)
        }
    catch
        _:_ ->
            #{
                engine_metrics => #{},
                metric_summary => #{},
                exported_at => erlang:system_time(millisecond)
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetches filtered data based on criteria.
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_filtered_data(map()) -> map().

fetch_filtered_data(Filters) ->
    Query = maps:get(query, Filters, #{}),
    Limit = maps:get(limit, Filters, 1000),

    try
        Metrics = yawl_monitor:get_metrics(Query, Limit),
        #{
            filters => Filters,
            result_count => length(Metrics),
            metrics => Metrics
        }
    catch
        _:_ ->
            #{
                filters => Filters,
                result_count => 0,
                metrics => []
            }
    end.

%%====================================================================
%% Internal Functions - Encoding
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Fallback JSON encoding.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_json(term()) -> binary().

encode_json(Data) when is_map(Data) ->
    Pairs = [[encode_json(K), ":", encode_json(V)] || {K, V} <- maps:to_list(Data)],
    ["{", lists:join(<<",">>, Pairs), "}"];
encode_json(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            ["\"", Data, "\""];
        false ->
            Elements = [encode_json(E) || E <- Data],
            ["[", lists:join(<<",">>, Elements), "]"]
    end;
encode_json(Data) when is_binary(Data) ->
    ["\"", escape_json(Data), "\""];
encode_json(Data) when is_atom(Data) ->
    ["\"", atom_to_binary(Data, utf8), "\""];
encode_json(Data) when is_integer(Data) ->
    integer_to_binary(Data);
encode_json(Data) when is_float(Data) ->
    float_to_binary(Data, [{decimals, 4}]);
encode_json(Data) when is_boolean(Data) ->
    case Data of true -> <<"true">>; false -> <<"false">> end;
encode_json(Term) ->
    io_lib:format("\"~w\"", [Term]).

%%--------------------------------------------------------------------
%% @private
%% @doc Escapes JSON strings.
%%
%% @end
%%--------------------------------------------------------------------
-spec escape_json(binary()) -> binary().

escape_json(Bin) ->
    Escaped = [case C of
            $" -> <<"\\\"">>;
            $\\ -> <<"\\\\">>;
            $\n -> <<"\\n">>;
            $\r -> <<"\\r">>;
            $\t -> <<"\\t">>;
            _ -> <<C>>
        end || <<C>> <= Bin],
    iolist_to_binary(Escaped).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes data to XML.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_xml(term(), binary(), integer()) -> binary().

encode_xml(Data, RootTag, Indent) when is_map(Data) ->
    IndentStr = lists:duplicate(Indent, $ ),
    Items = [
        [IndentStr, "<", K, ">", encode_xml(V, <<>>, 0), "</", K, ">\n"]
        || {K, V} <- maps:to_list(Data)
    ],
    iolist_to_binary(["<", RootTag, ">\n", Items, "</", RootTag, ">"]);
encode_xml(Data, _Tag, _Indent) when is_list(Data) ->
    %% Simple list encoding
    encode_json(Data);
encode_xml(Data, _Tag, _Indent) ->
    to_string(Data).

%%--------------------------------------------------------------------
%% @private
%% @doc Escapes and encodes CSV value.
%%
%% @end
%%--------------------------------------------------------------------
-spec escape_csv(term()) -> binary().

escape_csv(V) when is_binary(V); is_atom(V) ->
    Str = to_string(V),
    case binary:match(Str, <<",">>) of
        nomatch ->
            Str;
        _ ->
            <<"\"", Str/binary, "\"">>
    end;
escape_csv(V) when is_integer(V) ->
    integer_to_binary(V);
escape_csv(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 4}]);
escape_csv(V) ->
    to_string(V).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes a row as CSV.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_csv_row([term()]) -> binary().

encode_csv_row(Row) ->
    iolist_to_binary(lists:join($,, [escape_csv(V) || V <- Row])).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes a row from a map with specific keys.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_csv_row_from_map(map(), [binary()]) -> binary().

encode_csv_row_from_map(Map, Keys) ->
    Values = [maps:get(K, Map, <<>>) || K <- Keys],
    encode_csv_row(Values).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes CSV header from event structure.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_csv_header(events) -> binary().

encode_csv_header(events) ->
    <<"timestamp,event_name,event_value">>.

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes an event map as CSV.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_csv_event(map()) -> binary().

encode_csv_event(Event) ->
    Timestamp = maps:get(timestamp, Event, 0),
    EventName = maps:get(event_name, Event, <<>>),
    EventValue = maps:get(event_value, Event, <<>>),
    iolist_to_binary([
        integer_to_binary(Timestamp), $,,
        escape_csv(EventName), $,,
        escape_csv(EventValue)
    ]).

%%--------------------------------------------------------------------
%% @private
%% @doc Encodes generic list as CSV.
%%
%% @end
%%--------------------------------------------------------------------
-spec encode_csv([term()]) -> binary().

encode_csv([]) -> <<"">>;
encode_csv(List) when is_list(hd(List)) ->
    %% List of lists - treat as rows
    Rows = [encode_csv_row(R) || R <- List],
    iolist_to_binary(lists:join($\n, Rows));
encode_csv(List) ->
    encode_csv_row(List).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates HTML content from data.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_html_content(map()) -> binary().

generate_html_content(Data) ->
    Summary = generate_summary_section(Data),
    Tables = generate_data_tables(Data),
    <<Summary/binary, Tables/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates summary section of HTML.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_summary_section(map()) -> binary().

generate_summary_section(Data) ->
    ExportedAt = maps:get(exported_at, Data, 0),
    DateStr = httpd_util:rfc1123_date(ExportedAt div 1000),

    Items = [
        {case_id, maps:get(case_id, Data, <<"N/A">>)},
        {exported_at, DateStr},
        {total_events, length(maps:get(events, Data, []))}
    ],

    ItemHTML = lists:map(
        fun({Key, Value}) ->
            iolist_to_binary([
                "<p><strong>", to_string(Key), ":</strong> ",
                to_string(Value), "</p>"
            ])
        end,
        Items
    ),

    <<"<div class=\"summary\">\n",
        (iolist_to_binary(ItemHTML))/binary,
        "\n</div>\n">>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates data tables section of HTML.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_data_tables(map()) -> binary().

generate_data_tables(Data) ->
    Events = maps:get(events, Data, []),

    EventsTable = case Events of
        [] -> <<"">>;
        _ ->
            Header = ["<tr><th>Timestamp</th><th>Event</th><th>Value</th></tr>"],
            Rows = [
                iolist_to_binary([
                    "<tr><td>",
                    integer_to_binary(maps:get(timestamp, E, 0)),
                    "</td><td>",
                    to_string(maps:get(event_name, E, <<>>)),
                    "</td><td>",
                    to_string(maps:get(event_value, E, <<>>)),
                    "</td></tr>"
                ])
                || E <- lists:sublist(Events, 100) %% Limit to 100 rows
            ],
            iolist_to_binary([
                "\n<h2>Events</h2>\n<table>\n",
                iolist_to_binary(Header), $\n,
                iolist_to_binary(Rows),
                "\n</table>"
            ])
    end,

    EventsTable.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts various types to binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_string(term()) -> binary().

to_string(V) when is_binary(V) -> V;
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) -> float_to_binary(V, [{decimals, 4}]);
to_string(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> list_to_binary(V);
        false -> list_to_binary(io_lib:format("~p", [V]))
    end;
to_string(V) -> list_to_binary(io_lib:format("~p", [V])).

%%--------------------------------------------------------------------
%% @private
%% @doc Finds a specific metric value from a list.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_metric_value(binary(), [yawl_monitor:metric()], term()) -> term().

find_metric_value(_Name, [], Default) ->
    Default;
find_metric_value(Name, [#metric{name = Name, value = Value} | _Rest], _Default) ->
    Value;
find_metric_value(Name, [_Metric | Rest], Default) ->
    find_metric_value(Name, Rest, Default).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates the next run time for a schedule.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_next_run(hourly | daily | weekly | monthly) -> integer().

calculate_next_run(hourly) ->
    erlang:system_time(millisecond) + ?HOUR;
calculate_next_run(daily) ->
    erlang:system_time(millisecond) + ?DAY;
calculate_next_run(weekly) ->
    erlang:system_time(millisecond) + (7 * ?DAY);
calculate_next_run(monthly) ->
    erlang:system_time(millisecond) + (30 * ?DAY).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique schedule ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_schedule_id() -> binary().

generate_schedule_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"schedule_", Hex/binary>>.

%%====================================================================
%% Internal Functions - End of File
%%====================================================================
