%% @doc
%% Advanced approval workflow example with LLM integration
%% This demonstrates a complex workflow with multiple approval types
%%
%% Features demonstrated:
%% - Multi-tier approval architecture
%% - LLM-powered approval decisions
%% - Human-in-the-loop with escalation
%% - OpenTelemetry integration
%% @end
-module(approval_workflow).

-export([demo/0, create_fraud_detection_workflow/0, execute_with_approval/2]).

-include_lib("yawl_otel_logger/include/yawl_otel_logger.hrl").

%% Workflow records
-record(workflow, {
    id :: binary(),
    name :: binary(),
    tasks :: list(),
    connections :: list(),
    config :: map()
}).

-record(task, {
    id :: binary(),
    name :: binary(),
    type :: atom(),
    approval_config :: map()
}).

%% Approval records
-record(approval_request, {
    id :: binary(),
    workflow_id :: binary(),
    task_id :: binary(),
    type :: automated | llm | human,
    priority :: low | medium | high | critical,
    metadata :: map(),
    timeout :: integer(),
    created_at :: integer()
}).

%% @doc
%% Run the approval workflow demo
%% @end
demo() ->
    io:format("=== Advanced Approval Workflow Demo ===~n"),

    %% Initialize OpenTelemetry logging
    init_telemetry(),

    %% Create workflow
    Workflow = create_fraud_detection_workflow(),

    %% Execute workflow with sample data
    CaseData = #{
        transaction_id => <<"TXN-2024-001">>,
        amount => 15000,
        customer_id => <<"CUST-456">>,
        location => "high_risk_country",
        payment_method => wire_transfer,
        risk_score => 0.85
    },

    Result = execute_with_approval(Workflow, CaseData),

    %% Display results
    io:format("Workflow Execution Result: ~p~n", [Result]),

    %% Export telemetry data
    export_telemetry_data(),

    Result.

%% @doc
%% Create a fraud detection workflow with multiple approval levels
%% @end
create_fraud_detection_workflow() ->
    #workflow{
        id = <<"fraud_detection_workflow">>,
        name = "Fraud Detection & Approval System",
        tasks = [],
        connections = [],
        config = #{
            enable_auto_approval => true,
            llm_provider => openai,
            llm_model => "gpt-4-turbo",
            escalation_levels => 2,
            max_processing_time => 300000  % 5 minutes
        }
    }.

%% @doc
%% Execute workflow with approval handling
%% @end
execute_with_approval(Workflow, CaseData) ->
    io:format("Executing workflow: ~s~n", [Workflow#workflow.name]),

    %% Start OpenTelemetry trace
    {ok, TraceId} = yawl_otel_logger:start_trace(<<"fraud_detection">>, #{
        case_id => maps:get(transaction_id, CaseData),
        amount => maps:get(amount, CaseData),
        customer_id => maps:get(customer_id, CaseData)
    }),

    try
        %% Process workflow with approval gates
        Results = process_workflow_steps(Workflow#workflow.tasks,
                                        Workflow#workflow.connections,
                                        CaseData,
                                        TraceId,
                                        #{}),

        %% Final result
        case Results of
            #{final := {approved, FinalData}} ->
                yawl_otel_logger:log_workflow_complete(TraceId, completed),
                {ok, approved, FinalData, Results};
            #{final := {denied, Reason}} ->
                yawl_otel_logger:log_workflow_complete(TraceId, denied),
                {error, denied, Reason, Results};
            Error ->
                yawl_otel_logger:log_workflow_complete(TraceId, error),
                {error, failed, Error, Results}
        end
    catch
        Error:Reason ->
            yawl_otel_logger:log_event(error, <<"workflow_exception">>,
                                      #{error => Error, reason => Reason}),
            {error, exception, {Error, Reason}}
    after
        yawl_otel_logger:end_trace(TraceId)
    end.

%% @doc
%% Process workflow steps with approval handling
%% @end
process_workflow_steps([], _Connections, Data, _TraceId, Results) ->
    Results#{final => {approved, Data}};
process_workflow_steps([Task | Rest], Connections, Data, TraceId, Results) ->
    io:format("Processing task: ~s~n", [Task#task.name]),

    %% Log task start
    yawl_otel_logger:log_task_start(TraceId, Task#task.id, Task#task.name),

    %% Execute task logic
    TaskResult = execute_task_with_approval(Task, Data, TraceId),

    %% Log task completion
    yawl_otel_logger:log_task_complete(TraceId, Task#task.id, TaskResult),

    %% Update data based on result
    NewData = case TaskResult of
        {approved, ResultData} ->
            maps:merge(Data, ResultData);
        {denied, Reason} ->
            throw({denied, Reason});
        {error, Error} ->
            throw({error, Error})
    end,

    %% Store result
    NewResults = maps:put(Task#task.id, TaskResult, Results),

    %% Continue to next task
    process_workflow_steps(Rest, Connections, NewData, TraceId, NewResults).

%% @doc
%% Execute a task with approval handling
%% @end
execute_task_with_approval(Task, Data, TraceId) ->
    TaskId = Task#task.id,

    case should_require_approval(Task, Data) of
        false ->
            %% Auto-approve task
            TaskResult = execute_auto_task(Task, Data),
            {approved, TaskResult};
        true ->
            %% Require approval
            ApprovalRequest = create_approval_request(Task, Data, TraceId),
            ApprovalDecision = handle_approval(ApprovalRequest),
            ApprovalDecision
    end.

%% @doc
%% Determine if task requires approval based on data and configuration
%% @end
should_require_approval(Task, Data) ->
    Amount = maps:get(amount, Data, 0),
    RiskScore = maps:get(risk_score, Data, 0.0),

    TaskConfig = Task#task.approval_config,

    %% Apply approval rules
    ApprovalRules = maps:get(rules, TaskConfig, #{}),

    case maps:get(auto_approve_if, ApprovalRules, false) of
        true when RiskScore < 0.3 and Amount < 5000 ->
            false;
        _ ->
            case maps:get(auto_deny_if, ApprovalRules, false) of
                true when RiskScore > 0.9 and Amount > 50000 ->
                    throw({denied, "High risk transaction"});
                _ ->
                    maps:get(required_approval, TaskConfig, true)
            end
    end.

%% @doc
%% Execute automatic task (no approval needed)
%% @end
execute_auto_task(Task, Data) ->
    TaskId = Task#task.id,

    case TaskId of
        <<"initial_assessment">> ->
            #{assessment_result => "passed", risk_level => low};
        <<"basic_validation">> ->
            #{validation_result => "ok", data_clean => true};
        _ ->
            #{task_result => "completed", timestamp => erlang:system_time(millisecond)}
    end.

%% @doc
%% Create approval request
%% @end
create_approval_request(Task, Data, TraceId) ->
    #approval_request{
        id = generate_request_id(),
        workflow_id = TraceId,
        task_id = Task#task.id,
        type = maps:get(approval_type, Task#task.approval_config, human),
        priority = maps:get(priority, Task#task.approval_config, medium),
        metadata = #{
            task_name => Task#task.name,
            case_data => Data,
            risk_score => maps:get(risk_score, Data, 0.0),
            amount => maps:get(amount, Data, 0)
        },
        timeout = maps:get(timeout, Task#task.approval_config, 30000),
        created_at = erlang:system_time(millisecond)
    }.

%% @doc
%% Handle approval request with appropriate strategy
%% @end
handle_approval(Request) ->
    RequestId = Request#approval_request.id,
    Type = Request#approval_request.type,

    yawl_otel_logger:log_approval_request(RequestId, Type, Request#approval_request.metadata),

    case Type of
        automated ->
            handle_automated_approval(Request);
        llm ->
            handle_llm_approval(Request);
        human ->
            handle_human_approval(Request);
        _ ->
            {error, invalid_approval_type}
    end.

%% @doc
%% Handle automated approval
%% @end
handle_automated_approval(Request) ->
    Metadata = Request#approval_request.metadata,
    RiskScore = maps:get(risk_score, Metadata, 0.0),
    Amount = maps:get(amount, Metadata, 0),

    %% Apply business rules
    if
        RiskScore < 0.3 and Amount < 10000 ->
            yawl_otel_logger:log_approval_decision(Request#approval_request.id, auto, "Low risk auto-approved"),
            {approved, #{decision => auto, reason => "Low risk"}};
        RiskScore < 0.6 and Amount < 25000 ->
            yawl_otel_logger:log_approval_decision(Request#approval_request.id, llm, "Medium risk requires LLM review"),
            {approved, #{decision => llm, reason => "Medium risk approved"}};
        true ->
            yawl_otel_logger:log_approval_decision(Request#approval_request.id, human, "High risk requires human review"),
            {denied, "High risk requires manual review"}
    end.

%% @doc
%% Handle LLM-powered approval
%% @end
handle_llm_approval(Request) ->
    try
        %% Simulate LLM approval process
        Prompt = generate_llm_prompt(Request),
        Decision = simulate_llm_response(Prompt),

        yawl_otel_logger:log_approval_decision(Request#approval_request.id, llm, Decision),

        case Decision of
            "approve" ->
                {approved, #{decision => llm, reason => "LLM approval granted"}};
            "reject" ->
                {denied, "LLM approval denied"};
            "escalate" ->
                handle_escalation(Request)
        end
    catch
        Error:Reason ->
            yawl_otel_logger:log_event(error, <<"llm_approval_failed">>,
                                      #{request_id => Request#approval_request.id, error => Error, reason => Reason}),
            {error, {llm_error, Reason}}
    end.

%% @doc
%% Handle human approval (simulated)
%% @end
handle_human_approval(Request) ->
    RequestId = Request#approval_request.id,
    Metadata = Request#approval_request.metadata,

    yawl_otel_logger:log_approval_request(RequestId, human, Metadata),

    %% Simulate human decision (80% approval rate)
    Approved = case crypto:strong_rand_bytes(1) of
        <<0>> -> false;
        <<1>> -> true;
        <<2>> -> true  % 66% approval rate
    end,

    case Approved of
        true ->
            Reason = "Manual approval granted",
            yawl_otel_logger:log_approval_decision(RequestId, human, Reason),
            {approved, #{decision => human, reason => Reason, approver => "manager"}};
        false ->
            Reason = "Manual approval denied",
            yawl_otel_logger:log_approval_decision(RequestId, human, Reason),
            {denied, Reason}
    end.

%% @doc
%% Handle escalation to higher authority
%% @end
handle_escalation(Request) ->
    yawl_otel_logger:log_event(info, <<"approval_escalated">>,
                              #{request_id => Request#approval_request.id}),

    %% Escalate to higher priority
    EscalatedRequest = Request#approval_request{
        priority => critical,
        timeout => Request#approval_request.timeout * 2
    },

    %% Re-process with higher priority
    handle_approval(EscalatedRequest).

%% @doc
%% Generate LLM prompt for approval decision
%% @end
generate_llm_prompt(Request) ->
    Metadata = Request#approval_request.metadata,
    Amount = maps:get(amount, Metadata, 0),
    RiskScore = maps:get(risk_score, Metadata, 0.0),

    io_lib:format("Review this transaction for potential fraud:
Amount: $~p
Risk Score: ~.2f
Location: ~p
Payment Method: ~p

Please approve, reject, or escalate if additional review is needed.",
                 [Amount, RiskScore,
                  maps:get(location, Metadata, "unknown"),
                  maps:get(payment_method, Metadata, "unknown")]).

%% @doc
%% Simulate LLM response
%% @end
simulate_llm_response(Prompt) ->
    %% Simulate LLM thinking time
    timer:sleep(1000),

    %% Simple decision logic based on prompt content
    case Prompt of
        Prompt when Prompt =/= [] ->
            case crypto:uniform() of
                0.0 when RiskScore > 0.8 -> "reject";
                0.1 when RiskScore > 0.7 -> "escalate";
                _ -> "approve"
            end;
        _ ->
            "approve"
    end.

%% @doc
%% Initialize OpenTelemetry logging
%% @end
init_telemetry() ->
    try
        %% Start OpenTelemetry logger with custom configuration
        {ok, _Pid} = yawl_otel_logger:start_link(#{
            max_events => 1000,
            retention_ms => 3600000,  % 1 hour
            enable_workflow_tracing => true
        }),

        %% Log workflow initialization
        yawl_otel_logger:log_event(workflow, <<"workflow_started">>, #{
            module => ?MODULE,
            workflow_type => approval_workflow
        })
    catch
        Error:Reason ->
            io:format("Warning: Could not initialize OpenTelemetry: ~p:~p~n", [Error, Reason])
    end.

%% @doc
%% Export telemetry data for analysis
%% @end
export_telemetry_data() ->
    try
        {ok, Events} = yawl_otel_logger:get_events_by_type(<<"approval_request">>),
        io:format("Exported ~p approval events~n", [length(Events)]),

        {ok, Traces} = yawl_otel_logger:get_traces(),
        io:format("Exported ~p workflow traces~n", [length(Traces)])
    catch
        Error:Reason ->
            io:format("Warning: Could not export telemetry data: ~p:~p~n", [Error, Reason])
    end.

%% @doc
%% Generate unique request ID
%% @end
generate_request_id() ->
    list_to_binary(io_lib:format("REQ-~s-~p",
                                [timestamp_to_string(erlang:system_time(millisecond)),
                                 crypto:strong_rand_bytes(8)])).

%% @doc
%% Convert timestamp to string
%% @end
timestamp_to_string(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(Timestamp),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B_~2.10.0B-~2.10.0B-~2.10.0B",
                  [Year, Month, Day, Hour, Minute, Second]).