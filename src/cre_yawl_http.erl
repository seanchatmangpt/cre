%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% HTTP API Handler for YAWL Workflow Engine
%%
%% Provides REST API endpoints for:
%% - Workflow management (create, read, update, validate, execute)
%% - Pattern management and instantiation
%% - Task management and completion
%% - Event listeners and WebSocket support
%%
%% @doc YAWL HTTP API Handler
%% @end

-module(cre_yawl_http).
-behaviour(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2]).

%%====================================================================
%% Types
%%====================================================================

-type workflow_id() :: binary().
-type pattern_name() :: binary().
-type task_id() :: binary().
-type http_method() :: binary().
-type http_path() :: binary().

%%====================================================================
%% Cowboy Handler Callbacks
%%====================================================================

%% @doc Initialize HTTP request handler
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    % Route request based on path and method
    case route_request(Method, Path, Req0, State) of
        {ok, Req} ->
            {ok, Req, State};
        {error, Reason} ->
            ErrorResponse = build_error_response(Reason),
            Status = get_http_status(Reason),
            Req = cowboy_req:reply(Status,
                #{<<"content-type">> => <<"application/json">>},
                ErrorResponse, Req0),
            {ok, Req, State}
    end.

%%====================================================================
%% Internal Functions - Request Routing
%%====================================================================

%% @doc Route incoming HTTP requests to appropriate handlers
%% Note: Binary pattern matching with segments must use explicit variables
-spec route_request(http_method(), http_path(), cowboy_req:req(), term()) ->
    {ok, cowboy_req:req()} | {error, term()}.

%% Workflow management endpoints
route_request(<<"POST">>, <<"/yawl/workflows">>, Req, _State) ->
    handle_create_workflow(Req);

route_request(<<"GET">>, <<"/yawl/workflows">>, Req, _State) ->
    handle_list_workflows(Req);

route_request(<<"GET">>, Path, Req, _State) ->
    case Path of
        <<"/yawl/workflows/", WorkflowIdRest/binary>> ->
            case binary:split(WorkflowIdRest, <<"/">>) of
                [WorkflowId, <<>>] -> handle_get_workflow(WorkflowId, Req);
                [WorkflowId, <<"validate">>] -> handle_validate_workflow(WorkflowId, Req);
                [WorkflowId, <<"execute">>] -> handle_execute_workflow(WorkflowId, Req);
                [_, <<"events">>] -> handle_workflow_events(Req);
                _ -> {error, {not_found, <<"Endpoint not found">>}}
            end;
        _ ->
            route_request_get_other(Path, Req, _State)
    end;

route_request(<<"PUT">>, Path, Req, _State) ->
    case Path of
        <<"/yawl/workflows/", WorkflowId/binary>> ->
            handle_update_workflow(WorkflowId, Req);
        _ ->
            {error, {not_found, <<"Endpoint not found">>}}
    end;

route_request(<<"POST">>, Path, Req, _State) ->
    case Path of
        <<"/yawl/workflows/", WorkflowIdRest/binary>> ->
            case binary:split(WorkflowIdRest, <<"/">>) of
                [WorkflowId, <<"validate">>] -> handle_validate_workflow(WorkflowId, Req);
                [WorkflowId, <<"execute">>] -> handle_execute_workflow(WorkflowId, Req);
                _ -> {error, {not_found, <<"Endpoint not found">>}}
            end;
        <<"/yawl/patterns/", PatternName/binary>> ->
            handle_create_pattern(PatternName, Req);
        <<"/yawl/tasks/", TaskIdRest/binary>> ->
            case binary:split(TaskIdRest, <<"/">>) of
                [TaskId, <<"complete">>] -> handle_complete_task(TaskId, Req);
                _ -> {error, {not_found, <<"Endpoint not found">>}}
            end;
        _ ->
            {error, {not_found, <<"Endpoint not found">>}}
    end;

route_request(<<"GET">>, <<"/yawl/patterns">>, Req, _State) ->
    handle_list_patterns(Req);

route_request(<<"GET">>, <<"/yawl/tasks">>, Req, _State) ->
    handle_list_tasks(Req);

route_request(<<"GET">>, Path, Req, _State) ->
    case Path of
        <<"/yawl/tasks/", TaskId/binary>> ->
            handle_get_task(TaskId, Req);
        _ ->
            route_request_get_other(Path, Req, _State)
    end;

route_request(<<"GET">>, <<"/yawl/health">>, Req, _State) ->
    handle_health_check(Req);

route_request(<<"OPTIONS">>, _Path, Req, _State) ->
    handle_options(Req);

route_request(_, _, _Req, _State) ->
    {error, {not_found, <<"Endpoint not found">>}}.

%% Helper for other GET routes
route_request_get_other(Path, Req, _State) ->
    case binary:match(Path, <<"/yawl/workflows/">>) of
        {match, _} ->
            [_, _, WorkflowId | _] = binary:split(Path, <<"/">>, [global]),
            handle_get_workflow(WorkflowId, Req);
        nomatch ->
            {error, {not_found, <<"Endpoint not found">>}}
    end.


%%====================================================================
%% Workflow Management Handlers
%%====================================================================

%% @doc Handle POST /yawl/workflows - Create new workflow
-spec handle_create_workflow(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_create_workflow(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            case parse_json(Body) of
                {ok, WorkflowData} ->
                    case validate_workflow_data(WorkflowData) of
                        {ok, ValidData} ->
                            case create_workflow(ValidData) of
                                {ok, WorkflowId, Workflow} ->
                                    Response = jsone:encode(#{
                                        <<"id">> => WorkflowId,
                                        <<"status">> => <<"created">>,
                                        <<"workflow">> => workflow_to_json(Workflow)
                                    }),
                                    Req2 = cowboy_req:reply(201,
                                        #{<<"content-type">> => <<"application/json">>},
                                        Response, Req),
                                    {ok, Req2};
                                {error, Reason} ->
                                    {error, {creation_failed, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {validation_error, Reason}}
                    end;
                {error, Reason} ->
                    {error, {json_parse_error, Reason}}
            end;
        {error, Reason} ->
            {error, {read_body_error, Reason}}
    end.

%% @doc Handle GET /yawl/workflows - List workflows
-spec handle_list_workflows(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_list_workflows(Req) ->
    case get_all_workflows() of
        {ok, Workflows} ->
            Response = jsone:encode(#{
                <<"workflows">> => [workflow_to_json(W) || W <- Workflows],
                <<"count">> => length(Workflows)
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req2};
        {error, Reason} ->
            {error, {fetch_error, Reason}}
    end.

%% @doc Handle GET /yawl/workflows/:id - Get workflow by ID
-spec handle_get_workflow(workflow_id(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_get_workflow(WorkflowId, Req) ->
    case get_workflow(WorkflowId) of
        {ok, Workflow} ->
            Response = jsone:encode(workflow_to_json(Workflow)),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req2};
        {error, not_found} ->
            {error, {workflow_not_found, WorkflowId}};
        {error, Reason} ->
            {error, {fetch_error, Reason}}
    end.

%% @doc Handle PUT /yawl/workflows/:id - Update workflow
-spec handle_update_workflow(workflow_id(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_update_workflow(WorkflowId, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            case parse_json(Body) of
                {ok, UpdateData} ->
                    case update_workflow(WorkflowId, UpdateData) of
                        {ok, UpdatedWorkflow} ->
                            Response = jsone:encode(#{
                                <<"id">> => WorkflowId,
                                <<"status">> => <<"updated">>,
                                <<"workflow">> => workflow_to_json(UpdatedWorkflow)
                            }),
                            Req2 = cowboy_req:reply(200,
                                #{<<"content-type">> => <<"application/json">>},
                                Response, Req),
                            {ok, Req2};
                        {error, not_found} ->
                            {error, {workflow_not_found, WorkflowId}};
                        {error, Reason} ->
                            {error, {update_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {json_parse_error, Reason}}
            end;
        {error, Reason} ->
            {error, {read_body_error, Reason}}
    end.

%% @doc Handle POST /yawl/workflows/:id/validate - Validate workflow
-spec handle_validate_workflow(workflow_id(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_validate_workflow(WorkflowId, Req) ->
    case get_workflow(WorkflowId) of
        {ok, Workflow} ->
            case validate_workflow_structure(Workflow) of
                {ok, Errors} ->
                    Response = jsone:encode(#{
                        <<"id">> => WorkflowId,
                        <<"valid">> => length(Errors) =:= 0,
                        <<"errors">> => Errors
                    }),
                    StatusCode = case length(Errors) of
                        0 -> 200;
                        _ -> 400
                    end,
                    Req2 = cowboy_req:reply(StatusCode,
                        #{<<"content-type">> => <<"application/json">>},
                        Response, Req),
                    {ok, Req2};
                {error, Reason} ->
                    {error, {validation_error, Reason}}
            end;
        {error, not_found} ->
            {error, {workflow_not_found, WorkflowId}};
        {error, Reason} ->
            {error, {fetch_error, Reason}}
    end.

%% @doc Handle POST /yawl/workflows/:id/execute - Execute workflow
-spec handle_execute_workflow(workflow_id(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_execute_workflow(WorkflowId, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            ExecutionParams = case parse_json(Body) of
                {ok, Data} -> Data;
                {error, _} -> #{}
            end,
            case execute_workflow(WorkflowId, ExecutionParams) of
                {ok, ExecutionId, ExecutionState} ->
                    Response = jsone:encode(#{
                        <<"workflow_id">> => WorkflowId,
                        <<"execution_id">> => ExecutionId,
                        <<"status">> => <<"started">>,
                        <<"state">> => execution_state_to_json(ExecutionState)
                    }),
                    Req2 = cowboy_req:reply(202,
                        #{<<"content-type">> => <<"application/json">>},
                        Response, Req),
                    {ok, Req2};
                {error, not_found} ->
                    {error, {workflow_not_found, WorkflowId}};
                {error, Reason} ->
                    {error, {execution_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_body_error, Reason}}
    end.

%%====================================================================
%% Pattern Management Handlers
%%====================================================================

%% @doc Handle GET /yawl/patterns - List available patterns
-spec handle_list_patterns(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_list_patterns(Req) ->
    case get_available_patterns() of
        {ok, Patterns} ->
            Response = jsone:encode(#{
                <<"patterns">> => [pattern_to_json(P) || P <- Patterns],
                <<"count">> => length(Patterns)
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req2};
        {error, Reason} ->
            {error, {fetch_error, Reason}}
    end.

%% @doc Handle POST /yawl/patterns/:name - Create pattern instance
-spec handle_create_pattern(pattern_name(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_create_pattern(PatternName, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            case parse_json(Body) of
                {ok, PatternData} ->
                    case instantiate_pattern(PatternName, PatternData) of
                        {ok, PatternInstance} ->
                            Response = jsone:encode(#{
                                <<"pattern">> => PatternName,
                                <<"status">> => <<"instantiated">>,
                                <<"instance">> => PatternInstance
                            }),
                            Req2 = cowboy_req:reply(201,
                                #{<<"content-type">> => <<"application/json">>},
                                Response, Req),
                            {ok, Req2};
                        {error, not_found} ->
                            {error, {pattern_not_found, PatternName}};
                        {error, Reason} ->
                            {error, {instantiation_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {json_parse_error, Reason}}
            end;
        {error, Reason} ->
            {error, {read_body_error, Reason}}
    end.

%%====================================================================
%% Task Management Handlers
%%====================================================================

%% @doc Handle GET /yawl/tasks - List tasks
-spec handle_list_tasks(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_list_tasks(Req) ->
    case get_all_tasks() of
        {ok, Tasks} ->
            Response = jsone:encode(#{
                <<"tasks">> => [task_to_json(T) || T <- Tasks],
                <<"count">> => length(Tasks)
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req2};
        {error, Reason} ->
            {error, {fetch_error, Reason}}
    end.

%% @doc Handle GET /yawl/tasks/:id - Get task by ID
-spec handle_get_task(task_id(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_get_task(TaskId, Req) ->
    case get_task(TaskId) of
        {ok, Task} ->
            Response = jsone:encode(task_to_json(Task)),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req2};
        {error, not_found} ->
            {error, {task_not_found, TaskId}};
        {error, Reason} ->
            {error, {fetch_error, Reason}}
    end.

%% @doc Handle POST /yawl/tasks/:id/complete - Complete task
-spec handle_complete_task(task_id(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_complete_task(TaskId, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            TaskData = case parse_json(Body) of
                {ok, Data} -> Data;
                {error, _} -> #{}
            end,
            case complete_task(TaskId, TaskData) of
                {ok, CompletedTask} ->
                    Response = jsone:encode(#{
                        <<"id">> => TaskId,
                        <<"status">> => <<"completed">>,
                        <<"task">> => task_to_json(CompletedTask)
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response, Req),
                    {ok, Req2};
                {error, not_found} ->
                    {error, {task_not_found, TaskId}};
                {error, Reason} ->
                    {error, {completion_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_body_error, Reason}}
    end.

%%====================================================================
%% Event and Monitoring Handlers
%%====================================================================

%% @doc Handle GET /yawl/workflows/:id/events - Get workflow events (SSE or polling)
-spec handle_workflow_events(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_workflow_events(Req) ->
    % Check if client wants Server-Sent Events or polling
    case cowboy_req:header(<<"accept">>, Req) of
        <<"text/event-stream">> ->
            % Enable Server-Sent Events
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/event-stream">>,
                  <<"cache-control">> => <<"no-cache">>,
                  <<"connection">> => <<"keep-alive">>},
                Req),
            {ok, Req2};
        _ ->
            % Return recent events
            Response = jsone:encode(#{
                <<"events">> => [],
                <<"count">> => 0
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req2}
    end.

%% @doc Handle GET /yawl/health - Health check endpoint
-spec handle_health_check(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_health_check(Req) ->
    Response = jsone:encode(#{
        <<"status">> => <<"healthy">>,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"version">> => <<"1.0.0">>
    }),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req),
    {ok, Req2}.

%% @doc Handle OPTIONS request for CORS
-spec handle_options(cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, term()}.
handle_options(Req) ->
    Req2 = cowboy_req:reply(200,
        #{<<"access-control-allow-origin">> => <<"*">>,
          <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
          <<"access-control-allow-headers">> => <<"content-type">>},
        Req),
    {ok, Req2}.

%%====================================================================
%% Utility Functions - JSON Handling
%%====================================================================

%% @doc Parse JSON body
-spec parse_json(binary()) -> {ok, map()} | {error, term()}.
parse_json(Body) ->
    try
        {ok, jsone:decode(Body)}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Build error response
-spec build_error_response(term()) -> binary().
build_error_response({ErrorType, Detail}) ->
    jsone:encode(#{
        <<"error">> => atom_to_binary(ErrorType, utf8),
        <<"message">> => to_binary(Detail),
        <<"timestamp">> => erlang:system_time(millisecond)
    });
build_error_response(Error) ->
    jsone:encode(#{
        <<"error">> => to_binary(Error),
        <<"timestamp">> => erlang:system_time(millisecond)
    }).

%% @doc Convert term to binary
-spec to_binary(term()) -> binary().
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) -> term_to_binary(X).

%% @doc Get HTTP status code from error reason
-spec get_http_status(term()) -> integer().
get_http_status({json_parse_error, _}) -> 400;
get_http_status({validation_error, _}) -> 400;
get_http_status({workflow_not_found, _}) -> 404;
get_http_status({task_not_found, _}) -> 404;
get_http_status({pattern_not_found, _}) -> 404;
get_http_status({not_found, _}) -> 404;
get_http_status({read_body_error, _}) -> 400;
get_http_status({creation_failed, _}) -> 500;
get_http_status({update_failed, _}) -> 500;
get_http_status({execution_failed, _}) -> 500;
get_http_status({instantiation_failed, _}) -> 500;
get_http_status({completion_failed, _}) -> 500;
get_http_status({fetch_error, _}) -> 500;
get_http_status(_) -> 500.

%%====================================================================
%% Utility Functions - JSON Serialization
%%====================================================================

%% @doc Convert workflow to JSON representation
-spec workflow_to_json(term()) -> map().
workflow_to_json(Workflow) ->
    #{
        <<"name">> => maps:get(name, Workflow, <<"unknown">>),
        <<"description">> => maps:get(description, Workflow, <<"">>),
        <<"tasks">> => maps:get(tasks, Workflow, []),
        <<"connections">> => maps:get(connections, Workflow, [])
    }.

%% @doc Convert pattern to JSON representation
-spec pattern_to_json(term()) -> map().
pattern_to_json(Pattern) ->
    #{
        <<"name">> => maps:get(name, Pattern, <<"unknown">>),
        <<"type">> => maps:get(type, Pattern, <<"basic">>),
        <<"description">> => maps:get(description, Pattern, <<"">>)
    }.

%% @doc Convert task to JSON representation
-spec task_to_json(term()) -> map().
task_to_json(Task) ->
    #{
        <<"id">> => maps:get(id, Task, <<"unknown">>),
        <<"name">> => maps:get(name, Task, <<"unknown">>),
        <<"status">> => maps:get(status, Task, <<"pending">>),
        <<"assigned_to">> => maps:get(assigned_to, Task, null),
        <<"created_at">> => maps:get(created_at, Task, 0)
    }.

%% @doc Convert execution state to JSON
-spec execution_state_to_json(term()) -> map().
execution_state_to_json(State) ->
    #{
        <<"status">> => maps:get(status, State, <<"running">>),
        <<"timestamp">> => maps:get(timestamp, State, erlang:system_time(millisecond)),
        <<"progress">> => maps:get(progress, State, 0)
    }.

%%====================================================================
%% Placeholder Functions - Backend Integration
%%====================================================================

%% @doc Validate workflow data structure
-spec validate_workflow_data(map()) -> {ok, map()} | {error, term()}.
validate_workflow_data(Data) when is_map(Data) ->
    case maps:is_key(<<"name">>, Data) of
        true -> {ok, Data};
        false -> {error, <<"name field required">>}
    end;
validate_workflow_data(_) ->
    {error, <<"invalid workflow data">>}.

%% @doc Create a new workflow
-spec create_workflow(map()) -> {ok, workflow_id(), term()} | {error, term()}.
create_workflow(WorkflowData) ->
    WorkflowId = generate_id(),
    Workflow = WorkflowData#{
        <<"id">> => WorkflowId,
        <<"created_at">> => erlang:system_time(millisecond),
        <<"status">> => <<"draft">>
    },
    % TODO: Store in backend
    {ok, WorkflowId, Workflow}.

%% @doc Get workflow by ID
-spec get_workflow(workflow_id()) -> {ok, term()} | {error, term()}.
get_workflow(_WorkflowId) ->
    % TODO: Retrieve from backend
    {error, not_found}.

%% @doc Get all workflows
-spec get_all_workflows() -> {ok, list()} | {error, term()}.
get_all_workflows() ->
    % TODO: Retrieve from backend
    {ok, []}.

%% @doc Update workflow
-spec update_workflow(workflow_id(), map()) -> {ok, term()} | {error, term()}.
update_workflow(_WorkflowId, _UpdateData) ->
    % TODO: Update in backend
    {error, not_found}.

%% @doc Validate workflow structure
-spec validate_workflow_structure(term()) -> {ok, list()} | {error, term()}.
validate_workflow_structure(_Workflow) ->
    % TODO: Implement validation logic
    {ok, []}.

%% @doc Execute workflow
-spec execute_workflow(workflow_id(), map()) -> {ok, binary(), map()} | {error, term()}.
execute_workflow(_WorkflowId, _Params) ->
    % TODO: Execute in backend
    ExecutionId = generate_id(),
    ExecutionState = #{
        <<"status">> => <<"started">>,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"progress">> => 0
    },
    {ok, ExecutionId, ExecutionState}.

%% @doc Get available patterns
-spec get_available_patterns() -> {ok, list()} | {error, term()}.
get_available_patterns() ->
    % TODO: Retrieve from backend
    {ok, []}.

%% @doc Instantiate a pattern
-spec instantiate_pattern(pattern_name(), map()) -> {ok, map()} | {error, term()}.
instantiate_pattern(_PatternName, _Data) ->
    % TODO: Instantiate in backend
    {error, not_found}.

%% @doc Get all tasks
-spec get_all_tasks() -> {ok, list()} | {error, term()}.
get_all_tasks() ->
    % TODO: Retrieve from backend
    {ok, []}.

%% @doc Get task by ID
-spec get_task(task_id()) -> {ok, term()} | {error, term()}.
get_task(_TaskId) ->
    % TODO: Retrieve from backend
    {error, not_found}.

%% @doc Complete a task
-spec complete_task(task_id(), map()) -> {ok, term()} | {error, term()}.
complete_task(_TaskId, _Data) ->
    % TODO: Complete in backend
    {error, not_found}.

%% @doc Generate unique ID
-spec generate_id() -> binary().
generate_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Timestamp = (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs,
    Random = rand:uniform(1000000),
    list_to_binary(io_lib:format("~p~p", [Timestamp, Random])).
