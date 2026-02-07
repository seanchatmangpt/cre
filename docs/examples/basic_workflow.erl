%% @doc
%% Basic YAWL workflow example with CRE
%% This demonstrates a simple approval workflow with manual review
%%
%% Example usage:
%% 1. Compile: erlc -I ../include -o ../src ../examples/basic_workflow.erl
%% 2. Run: erl -pa ../src -pa ../test -s basic_workflow demo -s init stop
%% @end
-module(basic_workflow).

-export([demo/0, create_workflow/0, execute_workflow/1]).

%% Records for workflow data
-record(workflow, {
    id :: binary(),
    tasks :: list(),
    connections :: list()
}).

-record(task, {
    id :: binary(),
    name :: binary(),
    type :: atom(),
    module :: atom(),
    function :: atom(),
    approval_required :: boolean(),
    approval_config :: map()
}).

%% @doc
%% Demo function to run a basic workflow
%% @end
demo() ->
    io:format("=== Basic YAWL Workflow Demo ===~n"),

    %% Create the workflow
    Workflow = create_workflow(),

    %% Execute the workflow
    Result = execute_workflow(Workflow),

    %% Display results
    io:format("Workflow Result: ~p~n", [Result]),
    Result.

%% @doc
%% Create a basic workflow with approval checkpoint
%% @end
create_workflow() ->
    Workflow = #workflow{
        id = <<"basic_approval_workflow">>,
        tasks = [],
        connections = []
    },

    %% Task 1: Initial processing (no approval needed)
    Task1 = #task{
        id = <<"initial_check">>,
        name = "Initial Data Processing",
        type = atomic,
        module = basic_workflow,
        function = initial_processing,
        approval_required = false,
        approval_config = #{}
    },

    %% Task 2: Fraud detection (requires approval)
    Task2 = #task{
        id = <<"fraud_check">>,
        name = "Fraud Detection",
        type = atomic,
        module = basic_workflow,
        function = fraud_detection,
        approval_required = true,
        approval_config = #{
            type => human,
            required_approvals => 1,
            timeout => 30000,
            escalation => #{
                timeout => 60000,
                notify => [manager]
            }
        }
    },

    %% Task 3: Final processing (only if approved)
    Task3 = #task{
        id = <<"final_processing">>,
        name = "Final Processing",
        type = atomic,
        module = basic_workflow,
        function = final_processing,
        approval_required = false,
        approval_config = #{}
    },

    %% Connect tasks in sequence
    Connections = [
        {<<"initial_check">>, <<"fraud_check">>},
        {<<"fraud_check">>, <<"final_processing">>}
    ],

    Workflow#workflow{
        tasks = [Task1, Task2, Task3],
        connections = Connections
    }.

%% @doc
%% Execute the workflow
%% @end
execute_workflow(Workflow) ->
    io:format("Executing workflow: ~s~n", [Workflow#workflow.id]),

    %% Execute tasks in order
    Results = execute_tasks(Workflow#workflow.tasks, Workflow#workflow.connections, #{}),

    case Results of
        #{<<"fraud_check">> := {approved, _}} ->
            io:format("Workflow completed successfully!~n"),
            {ok, completed, Results};
        #{<<"fraud_check">> := {denied, Reason}} ->
            io:format("Workflow denied: ~p~n", [Reason]),
            {error, denied, Reason};
        Error ->
            io:format("Workflow failed: ~p~n", [Error]),
            {error, failed, Error}
    end.

%% @doc
%% Execute tasks in workflow order
%% @end
execute_tasks([], _Connections, Results) ->
    Results;
execute_tasks([Task | Rest], Connections, Results) ->
    %% Check if previous tasks completed successfully
    case can_execute_task(Task, Connections, Results) of
        true ->
            Result = execute_task(Task),

            %% Log the result
            io:format("Task ~s result: ~p~n", [Task#task.id, Result]),

            %% Store result
            NewResults = maps:put(Task#task.id, Result, Results),

            %% If task requires approval and was denied, stop workflow
            case Task#task.approval_required andalso element(1, Result) =:= denied of
                true ->
                    NewResults;
                false ->
                    execute_tasks(Rest, Connections, NewResults)
            end;
        false ->
            %% Prerequisite not met
            io:format("Skipping task ~s (prerequisites not met)~n", [Task#task.id]),
            execute_tasks(Rest, Connections, Results)
    end.

%% @doc
%% Check if task can be executed (prerequisites met)
%% @end
can_execute_task(Task, Connections, Results) ->
    %% Find incoming connections (prerequisites)
    Prerequisites = [From || {From, To} <- Connections, To =:= Task#task.id],

    case Prerequisites of
        [] ->
            %% No prerequisites, can execute
            true;
        _ ->
            %% All prerequisites must be completed successfully
            lists:all(fun(Prereq) ->
                case maps:get(Prereq, Results, undefined) of
                    {ok, _} -> true;
                    {approved, _} -> true;
                    _ -> false
                end
            end, Prerequisites)
    end.

%% @doc
%% Execute a single task
%% @end
execute_task(Task) ->
    io:format("Executing task: ~s~n", [Task#task.name]),

    %% Call the task function
    try
        case erlang:function_exported(Task#task.module, Task#task.function, 0) of
            true ->
                case apply(Task#task.module, Task#task.function, []) of
                    {ok, Result} ->
                        case Task#task.approval_required of
                            true ->
                                %% Simulate approval process
                                simulate_approval(Task);
                            false ->
                                {ok, Result}
                        end;
                    Error ->
                        {error, Error}
                end;
            false ->
                {error, {function_not_found, Task#task.module, Task#task.function}}
        end
    catch
        _Error:Reason ->
            {error, {Reason}}
    end.

%% @doc
%% Simulate approval process
%% @end
simulate_approval(Task) ->
    io:format("Waiting for approval on task: ~s~n", [Task#task.name]),

    %% Simulate approval decision (50% chance of approval)
    Approved = case crypto:strong_rand_bytes(1) of
        <<0>> -> false;
        <<1>> -> true
    end,

    case Approved of
        true ->
            io:format("Task ~s approved~n", [Task#task.id]),
            {approved, "Manual approval granted"};
        false ->
            io:format("Task ~s denied~n", [Task#task.id]),
            {denied, "Manual approval denied"}
    end.

%% @doc
%% Task function: Initial data processing
%% @end
initial_processing() ->
    io:format("Performing initial data processing...~n"),
    timer:sleep(1000),  %% Simulate work
    {ok, #{
        data_processed => true,
        timestamp => erlang:system_time(millisecond)
    }}.

%% @doc
%% Task function: Fraud detection
%% @end
fraud_detection() ->
    io:format("Performing fraud detection analysis...~n"),
    timer:sleep(2000),  %% Simulate analysis

    %% Simulate fraud score (0-1)
    FraudScore = crypto:uniform(),

    case FraudScore > 0.7 of
        true ->
            {ok, #{
                fraud_score => FraudScore,
                risk_level => high,
                requires_review => true
            }};
        false ->
            {ok, #{
                fraud_score => FraudScore,
                risk_level => low,
                requires_review => false
            }}
    end.

%% @doc
%% Task function: Final processing
%% @end
final_processing() ->
    io:format("Performing final processing...~n"),
    timer:sleep(500),  %% Simulate work
    {ok, #{
        workflow_completed => true,
        final_result => "Processing completed"
    }}.

%% @doc
%% Configuration for workflow execution
%% @end
get_workflow_config() ->
    #{
        approval_timeout => 30000,
        max_retries => 3,
        log_level => info,
        enable_telemetry => true
    }.