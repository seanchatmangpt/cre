%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% YAWL Web Dashboard - OpenTelemetry Viewer
%%
%% Provides HTTP interface to view YAWL workflow execution logs.
%%
%% @doc YAWL Web Dashboard
%% @end

-module(yawl_web_dashboard).
-author("CRE Team").

-include("yawl_otel_logger.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Cowboy REST handler callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2,
    delete_resource/2,
    delete_completed/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the dashboard web server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{port => 8081}).

%% @doc Start the dashboard web server with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    Port = maps:get(port, Options, 8081),

    % Build routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", ?MODULE, []},
            {"/api/events", ?MODULE, []},
            {"/api/events/:trace_id", ?MODULE, []},
            {"/api/traces", ?MODULE, []},
            {"/api/stats", ?MODULE, []},
            {"/api/clear", ?MODULE, []}
        ]}
    ]),

    CowboyOpts = #{
        env => #{dispatch => Dispatch}
    },

    case cowboy:start_clear(yawl_dashboard_http, [{port, Port}], CowboyOpts) of
        {ok, _Pid} ->
            logger:info("YAWL Dashboard started on http://localhost:~p", [Port]),
            gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []);
        {error, {already_started, _Pid}} ->
            logger:info("YAWL Dashboard already running on http://localhost:~p", [Port]),
            {ok, already_started};
        Error ->
            logger:error("Failed to start YAWL Dashboard: ~p", [Error]),
            Error
    end.

%% @doc Stop the dashboard web server.
-spec stop() -> ok.
stop() ->
    cowboy:stop_listener(yawl_dashboard_http),
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    {ok, #{port => maps:get(port, Options, 8081)}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Cowboy handler callbacks
%%====================================================================

init(Req, State) ->
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),
    case {Path, Method} of
        {<<"/">>, <<"GET">>} ->
            Html = build_dashboard_html(),
            Req2 = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/html; charset=utf-8">>
            }, Html, Req),
            {ok, Req2, State};
        _ ->
            {cowboy_rest, Req, State}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

to_json(Req, State) ->
    Path = cowboy_req:path(Req),
    handle_api_request(Path, Req, State).

delete_resource(Req, State) ->
    Path = cowboy_req:path(Req),
    handle_delete_request(Path, Req, State).

delete_completed(Req, State) ->
    {true, Req, State}.

%%====================================================================
%% Request Handlers
%%====================================================================

%% @private
handle_api_request(<<"/api/events">>, Req, State) ->
    QsList = cowboy_req:parse_qs(Req),
    Level = case proplists:get_value(<<"level">>, QsList) of
        undefined -> <<"all">>;
        Val -> Val
    end,
    LimitRaw = case proplists:get_value(<<"limit">>, QsList) of
        undefined -> <<"100">>;
        L -> L
    end,
    LimitInt = binary_to_integer(LimitRaw),

    Events = case Level of
        <<"all">> -> yawl_otel_logger:get_recent_events(LimitInt, all);
        <<"info">> -> yawl_otel_logger:get_recent_events(LimitInt, info);
        <<"error">> -> yawl_otel_logger:get_recent_events(LimitInt, error);
        <<"warning">> -> yawl_otel_logger:get_recent_events(LimitInt, warning);
        <<"debug">> -> yawl_otel_logger:get_recent_events(LimitInt, debug);
        _ -> yawl_otel_logger:get_recent_events(LimitInt, all)
    end,

    Response = jsx:encode(#{
        events => [format_event(E) || E <- Events],
        timestamp => erlang:system_time(millisecond)
    }),

    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req2, State};

handle_api_request(<<"/api/traces">>, Req, State) ->
    Traces = yawl_otel_logger:get_traces(),

    Response = jsx:encode(#{
        traces => [format_trace(T) || T <- Traces],
        count => length(Traces)
    }),

    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req2, State};

handle_api_request(<<"/api/stats">>, Req, State) ->
    Stats = yawl_otel_logger:get_stats(),
    Traces = yawl_otel_logger:get_traces(),
    CompletedCount = length([T || T <- Traces, element(6, T) =:= completed]),

    Response = jsx:encode(maps:merge(Stats, #{
        timestamp => erlang:system_time(millisecond),
        node => atom_to_binary(node()),
        completed_count => CompletedCount
    })),

    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req2, State};

handle_api_request(<<"/api/events/", TraceId/binary>>, Req, State) ->
    Events = yawl_otel_logger:get_events_by_trace(TraceId),

    Response = jsx:encode(#{
        trace_id => TraceId,
        events => [format_event(E) || E <- Events],
        count => length(Events)
    }),

    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req2, State};

handle_api_request(_Path, Req, State) ->
    Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"Not Found">>, Req),
    {ok, Req2, State}.

%% @private
handle_delete_request(<<"/api/clear">>, Req, State) ->
    yawl_otel_logger:clear_events(),
    Response = jsx:encode(#{
        status => <<"ok">>,
        message => <<"Events cleared">>
    }),
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {true, Req2, State};

handle_delete_request(_Path, Req, State) ->
    {false, Req, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
format_event(#otel_event{
    id = Id,
    trace_id = TraceId,
    timestamp = Timestamp,
    event_type = EventType,
    level = Level,
    user_id = UserId,
    case_id = CaseId,
    task_id = TaskId,
    message = Message,
    attributes = Attributes
}) ->
    #{
        id => Id,
        trace_id => TraceId,
        timestamp => Timestamp,
        event_type => to_binary(EventType),
        level => Level,
        user_id => format_maybe(UserId),
        case_id => format_maybe(CaseId),
        task_id => format_maybe(TaskId),
        message => Message,
        attributes => format_attributes(Attributes)
    };
format_event(Event) ->
    #{
        id => <<"unknown">>,
        timestamp => 0,
        event_type => unknown,
        level => info,
        message => list_to_binary(io_lib:format("~p", [Event]))
    }.

%% @private
format_maybe(undefined) -> <<>>;
format_maybe(Binary) when is_binary(Binary) -> Binary;
format_maybe(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
format_maybe(Other) -> list_to_binary(io_lib:format("~p", [Other])).

%% @private
format_trace(#otel_trace{
    trace_id = TraceId,
    case_id = CaseId,
    pattern_id = PatternId,
    start_time = StartTime,
    end_time = EndTime,
    status = Status,
    span_count = SpanCount
}) ->
    #{
        trace_id => TraceId,
        case_id => CaseId,
        pattern_id => PatternId,
        start_time => StartTime,
        end_time => EndTime,
        duration_ms => case EndTime of
            undefined -> 0;
            _ -> EndTime - StartTime
        end,
        status => Status,
        span_count => SpanCount
    };
format_trace(Trace) when is_tuple(Trace) ->
    #{
        trace_id => element(2, Trace),
        case_id => element(3, Trace),
        status => element(6, Trace)
    }.

%% @private
format_attributes(Attrs) when is_map(Attrs) ->
    maps:fold(fun(K, V, Acc) ->
        KeyBin = case K of
            Kb when is_binary(Kb) -> Kb;
            Ka when is_atom(Ka) -> atom_to_binary(Ka, utf8);
            Kl when is_list(Kl) -> list_to_binary(Kl);
            _ -> <<"unknown">>
        end,
        ValueBin = case V of
            Vb when is_binary(Vb) -> Vb;
            Va when is_atom(Va) -> atom_to_binary(Va, utf8);
            Vi when is_integer(Vi) -> integer_to_binary(Vi);
            Vl when is_list(Vl) -> list_to_binary(Vl);
            _ -> <<"">>
        end,
        Acc#{KeyBin => ValueBin}
    end, #{}, Attrs);
format_attributes(_) -> #{}.

%% @private
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(_) -> <<"">>.

%% @private
build_dashboard_html() ->
    <<
        "<!DOCTYPE html>\n"
        "<html lang='en'>\n"
        "<head>\n"
        "    <meta charset='UTF-8'>\n"
        "    <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
        "    <title>YAWL Workflow Dashboard</title>\n"
        "    <style>\n"
        "        * { box-sizing: border-box; }\n"
        "        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 0; background: #0f172a; color: #e2e8f0; }\n"
        "        .header { background: linear-gradient(135deg, #3b82f6 0%, #8b5cf6 100%); padding: 20px 30px; border-bottom: 1px solid #1e293b; }\n"
        "        .header h1 { margin: 0; font-size: 24px; font-weight: 600; color: #fff; }\n"
        "        .header .subtitle { margin-top: 5px; opacity: 0.8; font-size: 14px; }\n"
        "        .container { max-width: 1400px; margin: 0 auto; padding: 20px; }\n"
        "        .stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 15px; margin-bottom: 20px; }\n"
        "        .stat-card { background: #1e293b; border-radius: 12px; padding: 20px; border: 1px solid #334155; box-shadow: 0 4px 6px rgba(0,0,0,0.3); }\n"
        "        .stat-card h3 { margin: 0 0 10px 0; font-size: 11px; opacity: 0.6; text-transform: uppercase; letter-spacing: 0.5px; }\n"
        "        .stat-card .value { font-size: 32px; font-weight: 700; color: #3b82f6; }\n"
        "        .toolbar { display: flex; gap: 10px; margin-bottom: 20px; flex-wrap: wrap; align-items: center; }\n"
        "        .toolbar button, .toolbar select { padding: 10px 16px; border: none; border-radius: 8px; cursor: pointer; font-size: 14px; font-weight: 500; }\n"
        "        .toolbar button { background: #3b82f6; color: #fff; transition: background 0.2s; }\n"
        "        .toolbar button:hover { background: #2563eb; }\n"
        "        .toolbar button.danger { background: #ef4444; }\n"
        "        .toolbar button.danger:hover { background: #dc2626; }\n"
        "        .toolbar button.active { background: #10b981; }\n"
        "        .toolbar select { background: #1e293b; color: #e2e8f0; border: 1px solid #334155; }\n"
        "        .events-container { background: #1e293b; border-radius: 12px; border: 1px solid #334155; overflow: hidden; }\n"
        "        .events-header { padding: 15px 20px; background: #0f172a; display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #334155; }\n"
        "        .events-header h2 { margin: 0; font-size: 16px; font-weight: 600; }\n"
        "        .events-list { max-height: 600px; overflow-y: auto; }\n"
        "        .event { padding: 12px 20px; border-bottom: 1px solid #1e293b; display: grid; grid-template-columns: 70px 140px 1fr; gap: 12px; align-items: start; cursor: pointer; transition: background 0.15s; }\n"
        "        .event:hover { background: #0f172a; }\n"
        "        .event:last-child { border-bottom: none; }\n"
        "        .event.error { border-left: 3px solid #ef4444; }\n"
        "        .event.warning { border-left: 3px solid #f59e0b; }\n"
        "        .event.info { border-left: 3px solid #3b82f6; }\n"
        "        .event.debug { border-left: 3px solid #64748b; }\n"
        "        .event-time { font-family: 'SF Mono', Monaco, monospace; font-size: 11px; opacity: 0.5; }\n"
        "        .event-type { font-size: 10px; text-transform: uppercase; font-weight: 600; padding: 3px 8px; border-radius: 4px; letter-spacing: 0.5px; }\n"
        "        .event-type.workflow_start { background: #3b82f6; color: #fff; }\n"
        "        .event-type.workflow_complete { background: #10b981; color: #fff; }\n"
        "        .event-type.checkpoint_approve { background: #10b981; color: #fff; }\n"
        "        .event-type.checkpoint_deny { background: #ef4444; color: #fff; }\n"
        "        .event-type.checkpoint_create { background: #8b5cf6; color: #fff; }\n"
        "        .event-type.workitem_start { background: #f59e0b; color: #fff; }\n"
        "        .event-type.workitem_complete { background: #10b981; color: #fff; }\n"
        "        .event-message { font-size: 14px; color: #f1f5f9; }\n"
        "        .event-details { grid-column: 3; font-size: 11px; opacity: 0.5; margin-top: 4px; font-family: 'SF Mono', Monaco, monospace; }\n"
        "        .empty-state { padding: 80px 20px; text-align: center; opacity: 0.4; }\n"
        "        .live-indicator { display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: #10b981; margin-right: 8px; animation: pulse 2s infinite; }\n"
        "        @keyframes pulse { 0%, 100% { opacity: 1; transform: scale(1); } 50% { opacity: 0.5; transform: scale(0.95); } }\n"
        "        .traces-panel { margin-top: 20px; background: #1e293b; border-radius: 12px; border: 1px solid #334155; padding: 20px; display: none; }\n"
        "        .traces-panel.visible { display: block; }\n"
        "        .trace-event { padding: 10px; margin: 8px 0; background: #0f172a; border-radius: 6px; border-left: 3px solid #3b82f6; }\n"
        "        .trace-event small { opacity: 0.6; }\n"
        "    </style>\n"
        "</head>\n"
        "<body>\n"
        "    <div class='header'>\n"
        "        <h1><span class='live-indicator'></span>YAWL Workflow Dashboard</h1>\n"
        "        <div class='subtitle'>OpenTelemetry traces for YAWL workflow execution</div>\n"
        "    </div>\n"
        "    <div class='container'>\n"
        "        <div class='stats'>\n"
        "            <div class='stat-card'>\n"
        "                <h3>Total Events</h3>\n"
        "                <div class='value' id='stat-events'>-</div>\n"
        "            </div>\n"
        "            <div class='stat-card'>\n"
        "                <h3>Active Traces</h3>\n"
        "                <div class='value' id='stat-traces'>-</div>\n"
        "            </div>\n"
        "            <div class='stat-card'>\n"
        "                <h3>Workflows Completed</h3>\n"
        "                <div class='value' id='stat-completed'>-</div>\n"
        "            </div>\n"
        "            <div class='stat-card'>\n"
        "                <h3>Node</h3>\n"
        "                <div class='value' id='stat-node' style='font-size: 14px; color: #64748b;'>-</div>\n"
        "            </div>\n"
        "        </div>\n"
        "        <div class='toolbar'>\n"
        "            <select id='level-filter' onchange='loadEvents()'>\n"
        "                <option value='all'>All Levels</option>\n"
        "                <option value='info'>Info</option>\n"
        "                <option value='warning'>Warning</option>\n"
        "                <option value='error'>Error</option>\n"
        "                <option value='debug'>Debug</option>\n"
        "            </select>\n"
        "            <select id='limit-filter' onchange='loadEvents()'>\n"
        "                <option value='50'>Last 50</option>\n"
        "                <option value='100' selected>Last 100</option>\n"
        "                <option value='500'>Last 500</option>\n"
        "            </select>\n"
        "            <button onclick='loadEvents()'>Refresh</button>\n"
        "            <button class='danger' onclick='clearEvents()'>Clear</button>\n"
        "            <button id='auto-btn' class='active' onclick='toggleAuto()'>Auto: ON</button>\n"
        "        </div>\n"
        "        <div class='events-container'>\n"
        "            <div class='events-header'>\n"
        "                <h2>Recent Events</h2>\n"
        "                <span id='last-update'>Loading...</span>\n"
        "            </div>\n"
        "            <div class='events-list' id='events-list'>\n"
        "                <div class='empty-state'>Loading events...</div>\n"
        "            </div>\n"
        "        </div>\n"
        "        <div class='traces-panel' id='traces-panel'>\n"
        "            <h3>Trace: <span id='trace-id'></span></h3>\n"
        "            <div id='trace-events'></div>\n"
        "        </div>\n"
        "    </div>\n"
        "    <script>\n"
        "        let autoRefresh = true;\n"
        "        let refreshInterval;\n"
        "\n"
        "        function loadEvents() {\n"
        "            const level = document.getElementById('level-filter').value;\n"
        "            const limit = document.getElementById('limit-filter').value;\n"
        "            fetch('/api/events?level=' + level + '&limit=' + limit)\n"
        "                .then(r => r.json())\n"
        "                .then(data => {\n"
        "                    renderEvents(data.events);\n"
        "                    document.getElementById('last-update').textContent = new Date().toLocaleTimeString();\n"
        "                })\n"
        "                .catch(e => console.error('Failed to load events:', e));\n"
        "        }\n"
        "\n"
        "        function loadStats() {\n"
        "            fetch('/api/stats')\n"
        "                .then(r => r.json())\n"
        "                .then(data => {\n"
        "                    document.getElementById('stat-events').textContent = data.event_count;\n"
        "                    document.getElementById('stat-traces').textContent = data.trace_count;\n"
        "                    document.getElementById('stat-completed').textContent = data.completed_count;\n"
        "                    document.getElementById('stat-node').textContent = data.node;\n"
        "                });\n"
        "        }\n"
        "\n"
        "        function renderEvents(events) {\n"
        "            const container = document.getElementById('events-list');\n"
        "            if (!events || events.length === 0) {\n"
        "                container.innerHTML = '<div class=\\'empty-state\\'>No events yet. Run a YAWL workflow to see events here.</div>';\n"
        "                return;\n"
        "            }\n"
        "            container.innerHTML = events.map(e => \n"
        "                '<div class=\\'event ' + e.level + '\\' onclick=\\'showTrace(\\'' + e.trace_id + '\\')\\'>' +\n"
        "                '<div class=\\'event-time\\'>' + new Date(e.timestamp).toLocaleTimeString() + '</div>' +\n"
        "                '<div class=\\'event-type ' + e.event_type + '\\'>' + e.event_type.replace(/_/g, ' ') + '</div>' +\n"
        "                '<div>' +\n"
        "                '<div class=\\'event-message\\'>' + e.message + '</div>' +\n"
        "                (e.case_id ? '<div class=\\'event-details\\'>Case: ' + e.case_id + (e.task_id ? ' | Task: ' + e.task_id : '') + (e.user_id ? ' | User: ' + e.user_id : '') + '</div>' : '') +\n"
        "                '</div></div>'\n"
        "            ).join('');\n"
        "        }\n"
        "\n"
        "        function showTrace(traceId) {\n"
        "            fetch('/api/events/' + traceId)\n"
        "                .then(r => r.json())\n"
        "                .then(data => {\n"
        "                    document.getElementById('traces-panel').classList.add('visible');\n"
        "                    document.getElementById('trace-id').textContent = traceId.substring(0, 16) + '...';\n"
        "                    document.getElementById('trace-events').innerHTML = data.events.map(e => \n"
        "                        '<div class=\\'trace-event\\'>' +\n"
        "                        '<small>' + new Date(e.timestamp).toLocaleTimeString() + '</small> ' +\n"
        "                        '<strong>' + e.message + '</strong></div>'\n"
        "                    ).join('');\n"
        "                });\n"
        "        }\n"
        "\n"
        "        function clearEvents() {\n"
        "            if (confirm('Clear all events?')) {\n"
        "                fetch('/api/clear', { method: 'DELETE' }).then(() => { loadEvents(); loadStats(); });\n"
        "            }\n"
        "        }\n"
        "\n"
        "        function toggleAuto() {\n"
        "            autoRefresh = !autoRefresh;\n"
        "            const btn = document.getElementById('auto-btn');\n"
        "            btn.textContent = 'Auto: ' + (autoRefresh ? 'ON' : 'OFF');\n"
        "            btn.classList.toggle('active', autoRefresh);\n"
        "            if (autoRefresh) {\n"
        "                refreshInterval = setInterval(() => { loadEvents(); loadStats(); }, 2000);\n"
        "            } else {\n"
        "                clearInterval(refreshInterval);\n"
        "            }\n"
        "        }\n"
        "\n"
        "        // Initial load\n"
        "        loadEvents();\n"
        "        loadStats();\n"
        "        refreshInterval = setInterval(() => { loadEvents(); loadStats(); }, 2000);\n"
        "    </script>\n"
        "</body>\n"
        "</html>\n"
    >>.
