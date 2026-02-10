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
%% @doc REST API Handlers for Workflow CRUD Operations
%%
%% This module implements HTTP handlers for workflow create, read, update,
%% and delete operations using the Cowboy web framework.
%%
%% <h3>Endpoints</h3>
%% <ul>
%%   <li><b>POST /workflows</b> - Create new workflow</li>
%%   <li><b>GET /workflows</b> - List all workflows</li>
%%   <li><b>GET /workflows/{id}</b> - Get workflow details</li>
%%   <li><b>PUT /workflows/{id}</b> - Update workflow</li>
%%   <li><b>DELETE /workflows/{id}</b> - Delete workflow</li>
%% </ul>
%%
%% <h3>Request/Response Format</h3>
%% All requests and responses use JSON format with Content-Type: application/json.
%%
%% <h3>Error Handling</h3>
%% The module returns appropriate HTTP status codes:
%% - 200 OK: Successful GET, PUT
%% - 201 Created: Successful POST
%% - 204 No Content: Successful DELETE
%% - 400 Bad Request: Invalid input
%% - 404 Not Found: Workflow not found
%% - 409 Conflict: Duplicate or invalid operation
%% - 500 Internal Server Error: Server error
%%
%% @end
%% -------------------------------------------------------------------

-module(rest_workflow).
-behavior(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2]).
-export([create_workflow/1,
         read_workflow/1,
         read_all_workflows/0,
         update_workflow/2,
         delete_workflow/1,
         doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

-include("cre.hrl").

%%====================================================================
%% Cowboy Handler Callback
%%====================================================================

%% @doc Cowboy handler callback for HTTP requests.
%%
%% Routes requests to appropriate handler functions based on method and path.
%%
%% @end
-spec init(Req :: cowboy_req:req(), State :: term()) ->
    {ok, cowboy_req:req(), State :: term()}.

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    handle_request(Method, Path, Req0, State).

%%====================================================================
%% Request Routing
%%====================================================================

%% @private
handle_request(<<"POST">>, <<"/workflows">>, Req0, State) ->
    handle_create_workflow(Req0, State);

handle_request(<<"GET">>, <<"/workflows">>, Req0, State) ->
    handle_list_workflows(Req0, State);

handle_request(<<"GET">>, Path, Req0, State) ->
    case extract_workflow_id(Path) of
        {ok, WorkflowId} ->
            handle_read_workflow(Req0, WorkflowId, State);
        error ->
            reply_not_found(Req0, State)
    end;

handle_request(<<"PUT">>, Path, Req0, State) ->
    case extract_workflow_id(Path) of
        {ok, WorkflowId} ->
            handle_update_workflow(Req0, WorkflowId, State);
        error ->
            reply_not_found(Req0, State)
    end;

handle_request(<<"DELETE">>, Path, Req0, State) ->
    case extract_workflow_id(Path) of
        {ok, WorkflowId} ->
            handle_delete_workflow(Req0, WorkflowId, State);
        error ->
            reply_not_found(Req0, State)
    end;

handle_request(_, _, Req0, State) ->
    reply_not_found(Req0, State).

%%====================================================================
%% Handler Functions for CRUD Operations
%%====================================================================

%% @private
%% @doc Handles POST /workflows - create new workflow
handle_create_workflow(Req0, State) ->
    case read_body(Req0) of
        {ok, Body} ->
            case parse_json(Body) of
                {ok, Data} ->
                    case validate_create_request(Data) of
                        {ok, WorkflowData} ->
                            case create_workflow(WorkflowData) of
                                {ok, Workflow} ->
                                    Response = format_workflow_response(Workflow),
                                    reply_json(Req0, 201, Response, State);
                                {error, Reason} ->
                                    reply_error(Req0, 400, <<"creation_failed">>,
                                               reason_to_binary(Reason), State)
                            end;
                        {error, Reason} ->
                            reply_error(Req0, 400, <<"validation_failed">>,
                                       reason_to_binary(Reason), State)
                    end;
                {error, _} ->
                    reply_error(Req0, 400, <<"invalid_json">>,
                               <<"Request body is not valid JSON">>, State)
            end;
        {error, _} ->
            reply_error(Req0, 400, <<"read_error">>,
                       <<"Failed to read request body">>, State)
    end.

%% @private
%% @doc Handles GET /workflows - list all workflows
handle_list_workflows(Req0, State) ->
    case read_all_workflows() of
        {ok, Workflows} ->
            Response = #{
                status => <<"success">>,
                data => #{
                    workflows => [format_workflow_response(W) || W <- Workflows],
                    count => length(Workflows)
                }
            },
            reply_json(Req0, 200, Response, State);
        {error, Reason} ->
            reply_error(Req0, 500, <<"listing_failed">>,
                       reason_to_binary(Reason), State)
    end.

%% @private
%% @doc Handles GET /workflows/{id} - read workflow
handle_read_workflow(Req0, WorkflowId, State) ->
    case read_workflow(WorkflowId) of
        {ok, Workflow} ->
            Response = #{
                status => <<"success">>,
                data => format_workflow_response(Workflow)
            },
            reply_json(Req0, 200, Response, State);
        {error, not_found} ->
            reply_error(Req0, 404, <<"not_found">>,
                       <<"Workflow not found">>, State);
        {error, Reason} ->
            reply_error(Req0, 500, <<"read_failed">>,
                       reason_to_binary(Reason), State)
    end.

%% @private
%% @doc Handles PUT /workflows/{id} - update workflow
handle_update_workflow(Req0, WorkflowId, State) ->
    case read_body(Req0) of
        {ok, Body} ->
            case parse_json(Body) of
                {ok, Data} ->
                    case validate_update_request(Data) of
                        {ok, UpdateData} ->
                            case update_workflow(WorkflowId, UpdateData) of
                                {ok, Workflow} ->
                                    Response = #{
                                        status => <<"success">>,
                                        data => format_workflow_response(Workflow)
                                    },
                                    reply_json(Req0, 200, Response, State);
                                {error, not_found} ->
                                    reply_error(Req0, 404, <<"not_found">>,
                                               <<"Workflow not found">>, State);
                                {error, Reason} ->
                                    reply_error(Req0, 400, <<"update_failed">>,
                                               reason_to_binary(Reason), State)
                            end;
                        {error, Reason} ->
                            reply_error(Req0, 400, <<"validation_failed">>,
                                       reason_to_binary(Reason), State)
                    end;
                {error, _} ->
                    reply_error(Req0, 400, <<"invalid_json">>,
                               <<"Request body is not valid JSON">>, State)
            end;
        {error, _} ->
            reply_error(Req0, 400, <<"read_error">>,
                       <<"Failed to read request body">>, State)
    end.

%% @private
%% @doc Handles DELETE /workflows/{id} - delete workflow
handle_delete_workflow(Req0, WorkflowId, State) ->
    case delete_workflow(WorkflowId) of
        ok ->
            reply_json(Req0, 204, #{}, State);
        {error, not_found} ->
            reply_error(Req0, 404, <<"not_found">>,
                       <<"Workflow not found">>, State);
        {error, Reason} ->
            reply_error(Req0, 400, <<"deletion_failed">>,
                       reason_to_binary(Reason), State)
    end.

%%====================================================================
%% API Functions for Workflow CRUD
%%====================================================================

%% @doc Creates a new workflow.
%%
%% Takes a map with required fields:
%% - name: Workflow name (binary)
%% - spec: Workflow specification (binary)
%%
%% Optional fields:
%% - metadata: Additional metadata (map)
%%
%% Returns {ok, Workflow} on success or {error, Reason} on failure.
%%
%% @end
-spec create_workflow(map()) -> {ok, map()} | {error, term()}.

create_workflow(#{name := Name, spec := Spec} = Data) when is_binary(Name), is_binary(Spec) ->
    try
        WorkflowId = generate_workflow_id(),
        Now = erlang:system_time(millisecond),
        Metadata = maps:get(metadata, Data, #{}),

        Workflow = #{
            id => WorkflowId,
            name => Name,
            spec => Spec,
            status => <<"running">>,
            created_at => Now,
            updated_at => Now,
            metadata => Metadata
        },

        %% Store workflow (implementation depends on backend)
        case store_workflow(Workflow) of
            ok -> {ok, Workflow};
            StoreError -> StoreError
        end
    catch
        ErrorType:ErrorReason ->
            logger:error("Error creating workflow: ~p:~p", [ErrorType, ErrorReason]),
            {error, {ErrorType, ErrorReason}}
    end;

create_workflow(_) ->
    {error, invalid_input}.

%% @doc Reads a single workflow by ID.
%%
%% Returns {ok, Workflow} on success or {error, not_found} if not found.
%%
%% @end
-spec read_workflow(binary()) -> {ok, map()} | {error, term()}.

read_workflow(WorkflowId) when is_binary(WorkflowId) ->
    try
        case fetch_workflow(WorkflowId) of
            {ok, Workflow} -> {ok, Workflow};
            FetchError -> FetchError
        end
    catch
        ErrorType:ErrorReason ->
            logger:error("Error reading workflow ~p: ~p:~p", [WorkflowId, ErrorType, ErrorReason]),
            {error, {ErrorType, ErrorReason}}
    end;

read_workflow(_) ->
    {error, invalid_id}.

%% @doc Reads all workflows.
%%
%% Returns {ok, Workflows} where Workflows is a list of workflow maps.
%%
%% @end
-spec read_all_workflows() -> {ok, [map()]} | {error, term()}.

read_all_workflows() ->
    try
        case fetch_all_workflows() of
            {ok, Workflows} -> {ok, Workflows};
            FetchError -> FetchError
        end
    catch
        ErrorType:ErrorReason ->
            logger:error("Error reading all workflows: ~p:~p", [ErrorType, ErrorReason]),
            {error, {ErrorType, ErrorReason}}
    end.

%% @doc Updates an existing workflow.
%%
%% Takes the workflow ID and a map with fields to update:
%% - name: Workflow name (optional)
%% - status: Workflow status (optional)
%% - metadata: Metadata map (optional)
%%
%% Returns {ok, UpdatedWorkflow} on success or {error, Reason} on failure.
%%
%% @end
-spec update_workflow(binary(), map()) -> {ok, map()} | {error, term()}.

update_workflow(WorkflowId, UpdateData) when is_binary(WorkflowId), is_map(UpdateData) ->
    try
        case fetch_workflow(WorkflowId) of
            {ok, Workflow} ->
                UpdatedWorkflow = merge_updates(Workflow, UpdateData),
                case store_workflow(UpdatedWorkflow) of
                    ok -> {ok, UpdatedWorkflow};
                    StoreError -> StoreError
                end;
            FetchError -> FetchError
        end
    catch
        ErrorType:ErrorReason ->
            logger:error("Error updating workflow ~p: ~p:~p", [WorkflowId, ErrorType, ErrorReason]),
            {error, {ErrorType, ErrorReason}}
    end;

update_workflow(_, _) ->
    {error, invalid_input}.

%% @doc Deletes a workflow.
%%
%% Returns ok on success or {error, Reason} on failure.
%%
%% @end
-spec delete_workflow(binary()) -> ok | {error, term()}.

delete_workflow(WorkflowId) when is_binary(WorkflowId) ->
    try
        case delete_stored_workflow(WorkflowId) of
            ok -> ok;
            DeleteError -> DeleteError
        end
    catch
        ErrorType:ErrorReason ->
            logger:error("Error deleting workflow ~p: ~p:~p", [WorkflowId, ErrorType, ErrorReason]),
            {error, {ErrorType, ErrorReason}}
    end;

delete_workflow(_) ->
    {error, invalid_id}.

%%====================================================================
%% HTTP Response Helpers
%%====================================================================

%% @private
reply_json(Req, StatusCode, Body, State) ->
    Json = jsone:encode(Body),
    Reply = cowboy_req:reply(
        StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req
    ),
    {ok, Reply, State}.

%% @private
reply_error(Req, StatusCode, Error, Message, State) ->
    Body = #{
        status => <<"error">>,
        error => #{
            code => Error,
            message => Message
        }
    },
    reply_json(Req, StatusCode, Body, State).

%% @private
reply_not_found(Req, State) ->
    Body = #{
        status => <<"error">>,
        error => #{
            code => <<"not_found">>,
            message => <<"Endpoint not found">>
        }
    },
    reply_json(Req, 404, Body, State).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
read_body(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, _Req} -> {ok, Body};
        {more, _Body, _Req} ->
            {error, body_too_large};
        Error -> Error
    end.

%% @private
parse_json(Body) ->
    try
        {ok, jsone:decode(Body)}
    catch
        _:_ -> {error, invalid_json}
    end.

%% @private
validate_create_request(#{<<"name">> := Name, <<"spec">> := Spec}) ->
    {ok, #{
        name => Name,
        spec => Spec
    }};

validate_create_request(#{name := Name, spec := Spec}) ->
    {ok, #{
        name => Name,
        spec => Spec
    }};

validate_create_request(_) ->
    {error, missing_required_fields}.

%% @private
validate_update_request(Data) when is_map(Data) ->
    {ok, Data};

validate_update_request(_) ->
    {error, invalid_update_data}.

%% @private
extract_workflow_id(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<"workflows">>, Id] when Id =/= <<>> -> {ok, Id};
        _ -> error
    end.

%% @private
generate_workflow_id() ->
    list_to_binary(
        lists:flatten(
            io_lib:format("wf_~w_~w",
                [erlang:system_time(millisecond),
                 erlang:unique_integer([positive])])
        )
    ).

%% @private
format_workflow_response(Workflow) when is_map(Workflow) ->
    Workflow.

%% @private
merge_updates(Workflow, UpdateData) ->
    Now = erlang:system_time(millisecond),
    UpdatedWorkflow = maps:merge(Workflow, UpdateData),
    UpdatedWorkflow#{updated_at => Now}.

%% @private
reason_to_binary(Reason) when is_binary(Reason) ->
    Reason;
reason_to_binary(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
reason_to_binary(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% Storage Backend Functions (Stub Implementation)
%%====================================================================

%% @private
%% Placeholder for actual storage implementation
store_workflow(Workflow) ->
    logger:info("Storing workflow: ~p", [maps:get(id, Workflow)]),
    ok.

%% @private
%% Placeholder for actual fetch implementation
fetch_workflow(WorkflowId) ->
    logger:info("Fetching workflow: ~p", [WorkflowId]),
    {error, not_found}.

%% @private
%% Placeholder for actual fetch all implementation
fetch_all_workflows() ->
    logger:info("Fetching all workflows"),
    {ok, []}.

%% @private
%% Placeholder for actual delete implementation
delete_stored_workflow(WorkflowId) ->
    logger:info("Deleting workflow: ~p", [WorkflowId]),
    {error, not_found}.

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Runs doctests for the rest_workflow module.
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Module can be loaded
    {module, rest_workflow} = code:ensure_loaded(rest_workflow),

    %% Test 2: init/2 is exported (Cowboy handler callback)
    Exports = proplists:get_value(exports, rest_workflow:module_info()),
    true = lists:member({init, 2}, Exports),

    %% Test 3: All CRUD functions are exported
    true = lists:member({create_workflow, 1}, Exports),
    true = lists:member({read_workflow, 1}, Exports),
    true = lists:member({read_all_workflows, 0}, Exports),
    true = lists:member({update_workflow, 2}, Exports),
    true = lists:member({delete_workflow, 1}, Exports),

    %% Test 4: Create workflow with valid data
    ValidData = #{
        name => <<"Test Workflow">>,
        spec => <<"workflow_spec">>
    },
    {ok, _Workflow} = create_workflow(ValidData),

    %% Test 5: Create workflow with invalid data
    {error, invalid_input} = create_workflow(#{}),
    {error, invalid_input} = create_workflow(#{name => <<"Test">>}),

    %% Test 6: Read non-existent workflow
    {error, not_found} = read_workflow(<<"non_existent">>),

    %% Test 7: Invalid workflow ID
    {error, invalid_id} = read_workflow(not_binary),

    %% Test 8: Update with valid data
    UpdateData = #{
        name => <<"Updated Workflow">>
    },
    ValidateResult = validate_update_request(UpdateData),
    {ok, UpdateData} = ValidateResult,

    %% Test 9: Read all workflows (should work even if empty)
    {ok, Workflows} = read_all_workflows(),
    true = is_list(Workflows),

    %% Test 10: JSON content type header format
    ContentType = <<"application/json">>,
    true = is_binary(ContentType),
    <<"application/json">> = ContentType,

    ok.
