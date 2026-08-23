%%% @doc Simple integration demo - show manufacturing + execution
-module(demo_run_simple).
-export([run/0]).

run() ->
    io:format("=== LineController Factory - Demo ===~n~n"),

    %% Step 1: Load ontology
    io:format("1. Loading Security Ops ontology...~n"),
    case ggen_rdf:load("ontology/security-ops") of
        {ok, Graph} ->
            io:format("   ✓ Loaded ~w triples~n", [length(Graph)]);
        {error, Reason} ->
            io:format("   ✗ Failed: ~w~n", [Reason]),
            halt(1)
    end,

    %% Step 2: Define a simple line (incident triage)
    io:format("~n2. Defining incident triage line...~n"),
    TriageLine = define_triage_line(),
    io:format("   ✓ Line defined~n"),

    %% Step 3: Create test incident
    io:format("~n3. Creating test incident...~n"),
    TestIncident = #{
        id => <<"incident_001">>,
        alert_name => <<"Malware Detected">>,
        severity => critical,
        source => siem,
        timestamp => calendar:universal_time()
    },
    io:format("   ✓ Incident: ~p~n", [TestIncident]),

    %% Step 4: Run line
    io:format("~n4. Executing triage line...~n"),
    case wf_compile:compile(TriageLine) of
        {ok, Compiled} ->
            io:format("   ✓ Compiled to bytecode~n"),

            %% Create case and run
            InitCtx = #{data => TestIncident},
            State = wf_exec:exec_init(Compiled, InitCtx),

            %% Execute until halt
            case exec_line(State) of
                {ok, FinalState} ->
                    io:format("   ✓ Execution completed~n"),

                    %% Display trace
                    Trace = wf_vm:exec_trace(FinalState),
                    io:format("~n5. Execution trace (~w steps):~n", [length(Trace)]),
                    lists:foreach(fun(Event) ->
                        io:format("   Step ~p: ~p~n", [element(1, Event), element(2, Event)])
                    end, Trace);

                {error, Reason} ->
                    io:format("   ✗ Execution failed: ~w~n", [Reason])
            end;
        {error, Reason} ->
            io:format("   ✗ Compilation failed: ~w~n", [Reason])
    end,

    %% Step 5: Start connectors
    io:format("~n6. Starting connector inventory...~n"),
    incident_connector_siem:start_link(siem),
    incident_connector_edr:start_link(edr),
    incident_connector_ticket:start_link(ticket),
    incident_connector_notify:start_link(notify),
    io:format("   ✓ 4 connectors running~n"),

    %% Step 6: Test connectors
    io:format("~n7. Testing connectors...~n"),
    {ok, _} = incident_connector_siem:ingest(siem, TestIncident),
    io:format("   ✓ SIEM ingest~n"),

    {ok, Evidence} = incident_connector_edr:get_evidence(edr, <<"incident_001">>),
    io:format("   ✓ EDR evidence: ~w items~n", [length(Evidence)]),

    {ok, TicketId} = incident_connector_ticket:create_ticket(ticket, TestIncident),
    io:format("   ✓ Ticket created: ~w~n", [TicketId]),

    {ok, _} = incident_connector_notify:notify(notify, "security@example.com", "Incident critical"),
    io:format("   ✓ Notification sent~n"),

    %% Stop connectors
    incident_connector_siem:stop(siem),
    incident_connector_edr:stop(edr),
    incident_connector_ticket:stop(ticket),
    incident_connector_notify:stop(notify),

    io:format("~n=== Demo Complete ===~n").

%% Define simple triage line: classify -> severity_gate -> ticket -> notify
define_triage_line() ->
    Classify = wf_term:task(classify_incident, fun classify_task/1),
    SeverityGate = wf_term:task(severity_gate, fun severity_task/1),
    CreateTicket = wf_term:task(create_ticket, fun ticket_task/1),
    SendNotification = wf_term:task(send_notification, fun notify_task/1),

    wf_term:seq(
        wf_term:seq(
            wf_term:seq(Classify, SeverityGate),
            CreateTicket
        ),
        SendNotification
    ).

%% Task functions

classify_task(Ctx) ->
    Incident = maps:get(data, Ctx),
    Classification = case maps:get(alert_name, Incident, <<"Unknown">>) of
        <<"Malware", _/binary>> -> malware;
        <<"Phishing", _/binary>> -> phishing;
        _ -> suspicious
    end,

    UpdatedData = maps:put(classification, Classification, Incident),
    {ok, Ctx#{data => UpdatedData}}.

severity_task(Ctx) ->
    Incident = maps:get(data, Ctx),
    Severity = maps:get(severity, Incident, low),

    case Severity of
        critical ->
            {ok, Ctx#{data => maps:put(action, escalate, Incident)}};
        high ->
            {ok, Ctx#{data => maps:put(action, review, Incident)}};
        _ ->
            {ok, Ctx#{data => maps:put(action, log, Incident)}}
    end.

ticket_task(Ctx) ->
    Incident = maps:get(data, Ctx),
    Action = maps:get(action, Incident, log),

    case Action of
        escalate ->
            {effect, {create_ticket, Incident}, Ctx};
        _ ->
            {ok, Ctx}
    end.

notify_task(Ctx) ->
    Incident = maps:get(data, Ctx),
    {ok, Ctx#{notification_sent => true}}.

%% Execution helper

exec_line(State) ->
    case wf_exec:exec_step(State) of
        {continue, NewState} ->
            exec_line(NewState);
        {halt, ok, FinalState} ->
            {ok, FinalState};
        {error, Reason, FinalState} ->
            {error, {Reason, FinalState}};
        {yield, _Spec, NewState} ->
            exec_line(NewState)
    end.

