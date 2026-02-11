%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc REST API Handler for Workflow Management
%%
%% Provides comprehensive HTTP endpoints for managing CRE workflow instances.
%%
%% <h3>Endpoints</h3>
%%
%% <ul>
%%   <li><b>POST /workflows</b> - Create a new workflow instance</li>
%%   <li><b>GET /workflows/:id</b> - Get workflow status and details</li>
%%   <li><b>POST /workflows/:id/start</b> - Start a workflow execution</li>
%%   <li><b>POST /workflows/:id/stop</b> - Stop a workflow instance</li>
%%   <li><b>GET /workflows</b> - List all active workflow instances</li>
%% </ul>
%%
%% <h3>Request/Response Format</h3>
%%
%% All requests and responses use JSON format with proper Content-Type headers.
%%
%% <h4>Create Workflow (POST /workflows)</h4>
%% Request body:
%% ```json
%% {
%%   "workflow_module": "my_workflow",
%%   "case_id": "case-001",
%%   "init_args": {...},
%%   "options": []
%% }
%% ```
%%
%% Response (201):
%% ```json
%% {
%%   "status": "created",
%%   "case_id": "case-001",
%%   "pid": "<0.123.0>",
%%   "message": "Workflow created successfully"
%% }
%% ```
%%
%% <h4>Get Workflow Status (GET /workflows/:id)</h4>
%% Response (200):
%% ```json
%% {
%%   "status": "running",
%%   "case_id": "case-001",
%%   "pid": "<0.123.0>",
%%   "marking": {...},
%%   "usr_info": {...}
%% }
%% ```
%%
%% <h4>Start Workflow (POST /workflows/:id/start)</h4>
%% Response (200):
%% ```json
%% {
%%   "status": "started",
%%   "case_id": "case-001",
%%   "message": "Workflow started successfully"
%% }
%% ```
%%
%% <h4>Stop Workflow (POST /workflows/:id/stop)</h4>
%% Response (200):
%% ```json
%% {
%%   "status": "stopped",
%%   "case_id": "case-001",
%%   "message": "Workflow stopped successfully"
%% }
%% ```
%%
%% <h4>List Workflows (GET /workflows)</h4>
%% Response (200):
%% ```json
%% {
%%   "status": "ok",
%%   "workflows": [
%%     {"case_id": "case-001", "pid": "<0.123.0>"},
%%     {"case_id": "case-002", "pid": "<0.124.0>"}
%%   ],
%%   "count": 2
%% }
%% ```
%%
%% <h3>Error Responses</h3>
%%
%% All error responses follow this format:
%% ```json
%% {
%%   "status": "error",
%%   "message": "Error description",
%%   "details": {...}
%% }
%% ```
%%
%% HTTP Status Codes:
%% <ul>
%%   <li>200 - Success</li>
%%   <li>201 - Created</li>
%%   <li>400 - Bad Request (invalid JSON, missing fields)</li>
%%   <li>404 - Not Found (workflow not found)</li>
%%   <li>405 - Method Not Allowed</li>
%%   <li>409 - Conflict (workflow already exists)</li>
%%   <li>500 - Internal Server Error</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_http_handler).
-behaviour(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

%% Cowboy handler callback
-export([init/2]).

%% Public API for testing
-export([handle_request/3]).
-export([start_listener/0, start_listener/1, stop_listener/0]).

%%====================================================================
%% Types
%%====================================================================

-type json_object() :: map().
-type http_method() :: binary().
-type path_segments() :: [binary()].

-export_type([json_object/0, http_method/0, path_segments/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the HTTP listener on default port 8080.
-spec start_listener() -> {ok, inet:port_number()} | {error, term()}.
start_listener() ->
    start_listener(8080).

%% @doc Starts the HTTP listener on specified port.
-spec start_listener(Port :: inet:port_number()) ->
    {ok, inet:port_number()} | {error, term()}.
start_listener(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/workflows", ?MODULE, []},
            {"/workflows/:id", ?MODULE, []},
            {"/workflows/:id/:action", ?MODULE, []}
        ]}
    ]),
    case cowboy:start_clear(cre_workflow_api,
                           [{port, Port}],
                           #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            logger:info("CRE Workflow API started on port ~p", [Port]),
            {ok, Port};
        {error, {already_started, _}} ->
            logger:info("CRE Workflow API already running on port ~p", [Port]),
            {ok, Port};
        Error ->
            logger:error("Failed to start CRE Workflow API: ~p", [Error]),
            Error
    end.

%% @doc Stops the HTTP listener.
-spec stop_listener() -> ok.
stop_listener() ->
    case cowboy:stop_listener(cre_workflow_api) of
        ok ->
            logger:info("CRE Workflow API stopped"),
            ok;
        {error, not_found} ->
            ok
    end.

%%====================================================================
%% Cowboy Handler Callback
%%====================================================================

%% @doc Initialize HTTP request handler.
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    PathSegments = cowboy_req:path_info(Req),

    logger:debug("CRE API Request: ~p ~p", [Method, Path]),

    try
        handle_route(Method, PathSegments, Req, State)
    catch
        error:Error:Stack ->
            logger:error("CRE API Error: ~p~nStack: ~p", [Error, Stack]),
            ErrorResponse = jsone:encode(#{
                <<"status">> => <<"error">>,
                <<"message">> => <<"Internal server error">>,
                <<"details">> => list_to_binary(io_lib:format("~p", [Error]))
            }),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                ErrorResponse, Req),
            {ok, Req2, State}
    end.

%%====================================================================
%% Route Handlers
%%====================================================================

%% @private Handle routing based on method and path segments.
-spec handle_route(http_method(), path_segments() | undefined, cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.

%% POST /workflows - Create workflow
handle_route(<<"POST">>, undefined, Req, State) ->
    handle_create_workflow(Req, State);

%% GET /workflows - List all workflows
handle_route(<<"GET">>, undefined, Req, State) ->
    handle_list_workflows(Req, State);

%% GET /workflows/:id - Get workflow status
handle_route(<<"GET">>, [Id], Req, State) ->
    handle_get_workflow(Id, Req, State);

%% POST /workflows/:id/start - Start workflow
handle_route(<<"POST">>, [Id, <<"start">>], Req, State) ->
    handle_start_workflow(Id, Req, State);

%% POST /workflows/:id/stop - Stop workflow
handle_route(<<"POST">>, [Id, <<"stop">>], Req, State) ->
    handle_stop_workflow(Id, Req, State);

%% Unsupported routes
handle_route(Method, Path, Req, State) ->
    logger:warning("Unsupported route: ~p ~p", [Method, Path]),
    ErrorResponse = jsone:encode(#{
        <<"status">> => <<"error">>,
        <<"message">> => <<"Endpoint not found">>,
        <<"method">> => Method,
        <<"path">> => case Path of
            undefined -> <<"/workflows">>;
            _ -> iolist_to_binary([<<"/workflows/">>, lists:join(<<"/">>, Path)])
        end
    }),
    Req2 = cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        ErrorResponse, Req),
    {ok, Req2, State}.

%%====================================================================
%% Endpoint Handlers
%%====================================================================

%% @private Handle POST /workflows - Create new workflow instance.
-spec handle_create_workflow(cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
handle_create_workflow(Req, State) ->
    case read_json_body(Req) of
        {ok, Body, Req2} ->
            case validate_create_request(Body) of
                {ok, WorkflowModule, CaseId, InitArgs, Options} ->
                    case create_workflow(WorkflowModule, CaseId, InitArgs, Options) of
                        {ok, Pid} ->
                            Response = jsone:encode(#{
                                <<"status">> => <<"created">>,
                                <<"case_id">> => CaseId,
                                <<"pid">> => list_to_binary(pid_to_list(Pid)),
                                <<"message">> => <<"Workflow created successfully">>
                            }),
                            Req3 = cowboy_req:reply(201,
                                #{<<"content-type">> => <<"application/json">>},
                                Response, Req2),
                            {ok, Req3, State};
                        {error, Reason} ->
                            reply_error(409, <<"Failed to create workflow">>, Reason, Req2, State)
                    end;
                {error, Reason} ->
                    reply_error(400, <<"Invalid request">>, Reason, Req2, State)
            end;
        {error, Reason} ->
            reply_error(400, <<"Invalid JSON">>, Reason, Req, State)
    end.

%% @private Handle GET /workflows/:id - Get workflow status.
-spec handle_get_workflow(binary(), cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
handle_get_workflow(CaseId, Req, State) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            case get_workflow_status(Pid) of
                {ok, Status} ->
                    Response = jsone:encode(#{
                        <<"status">> => <<"running">>,
                        <<"case_id">> => CaseId,
                        <<"pid">> => list_to_binary(pid_to_list(Pid)),
                        <<"marking">> => maps:get(marking, Status, #{}),
                        <<"usr_info">> => maps:get(usr_info, Status, #{})
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    reply_error(500, <<"Failed to get workflow status">>, Reason, Req, State)
            end;
        {error, not_found} ->
            reply_error(404, <<"Workflow not found">>, CaseId, Req, State)
    end.

%% @private Handle POST /workflows/:id/start - Start workflow execution.
-spec handle_start_workflow(binary(), cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
handle_start_workflow(CaseId, Req, State) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            case start_workflow_execution(Pid) of
                ok ->
                    Response = jsone:encode(#{
                        <<"status">> => <<"started">>,
                        <<"case_id">> => CaseId,
                        <<"message">> => <<"Workflow started successfully">>
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    reply_error(500, <<"Failed to start workflow">>, Reason, Req, State)
            end;
        {error, not_found} ->
            reply_error(404, <<"Workflow not found">>, CaseId, Req, State)
    end.

%% @private Handle POST /workflows/:id/stop - Stop workflow instance.
-spec handle_stop_workflow(binary(), cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
handle_stop_workflow(CaseId, Req, State) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            case yawl_workflow_supervisor:stop_workflow(Pid) of
                ok ->
                    Response = jsone:encode(#{
                        <<"status">> => <<"stopped">>,
                        <<"case_id">> => CaseId,
                        <<"message">> => <<"Workflow stopped successfully">>
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Response, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    reply_error(500, <<"Failed to stop workflow">>, Reason, Req, State)
            end;
        {error, not_found} ->
            reply_error(404, <<"Workflow not found">>, CaseId, Req, State)
    end.

%% @private Handle GET /workflows - List all workflow instances.
-spec handle_list_workflows(cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
handle_list_workflows(Req, State) ->
    Workflows = yawl_registry:list(),
    WorkflowList = [#{
        <<"case_id">> => CaseId,
        <<"pid">> => list_to_binary(pid_to_list(Pid))
    } || {CaseId, Pid} <- Workflows],

    Response = jsone:encode(#{
        <<"status">> => <<"ok">>,
        <<"workflows">> => WorkflowList,
        <<"count">> => length(WorkflowList)
    }),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req),
    {ok, Req2, State}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Read and parse JSON request body.
-spec read_json_body(cowboy_req:req()) ->
    {ok, json_object(), cowboy_req:req()} | {error, term()}.
read_json_body(Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            try
                Json = jsone:decode(Body),
                {ok, Json, Req2}
            catch
                _:Error ->
                    {error, {json_decode_error, Error}}
            end;
        false ->
            {error, no_body}
    end.

%% @private Validate create workflow request body.
-spec validate_create_request(json_object()) ->
    {ok, atom(), binary(), term(), list()} | {error, term()}.
validate_create_request(Body) when is_map(Body) ->
    try
        WorkflowModuleBin = maps:get(<<"workflow_module">>, Body),
        CaseId = maps:get(<<"case_id">>, Body),
        InitArgs = maps:get(<<"init_args">>, Body, #{}),
        Options = maps:get(<<"options">>, Body, []),

        WorkflowModule = binary_to_existing_atom(WorkflowModuleBin, utf8),

        {ok, WorkflowModule, CaseId, InitArgs, Options}
    catch
        error:badarg ->
            {error, <<"workflow_module does not exist">>};
        error:{badkey, Key} ->
            {error, iolist_to_binary(io_lib:format("Missing required field: ~p", [Key]))};
        _:Error ->
            {error, iolist_to_binary(io_lib:format("Validation error: ~p", [Error]))}
    end;
validate_create_request(_) ->
    {error, <<"Request body must be a JSON object">>}.

%% @private Create a new workflow instance.
-spec create_workflow(atom(), binary(), term(), list()) ->
    {ok, pid()} | {error, term()}.
create_workflow(WorkflowModule, CaseId, InitArgs, Options) ->
    case yawl_registry:lookup(CaseId) of
        {ok, _} ->
            {error, <<"Workflow with this case_id already exists">>};
        {error, not_found} ->
            case yawl_workflow_supervisor:start_workflow(WorkflowModule, InitArgs, Options) of
                {ok, Pid} ->
                    ok = yawl_registry:register(CaseId, Pid),
                    {ok, Pid};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private Get workflow status from gen_yawl process.
-spec get_workflow_status(pid()) -> {ok, map()} | {error, term()}.
get_workflow_status(Pid) ->
    try
        UsrInfo = gen_yawl:get_usr_info(Pid),
        Marking = gen_yawl:get_ls(Pid, all),
        {ok, #{
            usr_info => encode_term(UsrInfo),
            marking => encode_term(Marking)
        }}
    catch
        _:Error ->
            {error, Error}
    end.

%% @private Start workflow execution (trigger initial transitions).
-spec start_workflow_execution(pid()) -> ok | {error, term()}.
start_workflow_execution(_Pid) ->
    %% Workflow starts automatically when created with gen_yawl
    %% This endpoint is for explicit start if needed
    ok.

%% @private Encode Erlang term to JSON-safe format.
-spec encode_term(term()) -> term().
encode_term(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
encode_term(Term) when is_pid(Term) ->
    list_to_binary(pid_to_list(Term));
encode_term(Term) when is_reference(Term) ->
    list_to_binary(ref_to_list(Term));
encode_term(Term) when is_map(Term) ->
    maps:map(fun(_K, V) -> encode_term(V) end, Term);
encode_term(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> list_to_binary(Term);
        false -> [encode_term(T) || T <- Term]
    end;
encode_term(Term) when is_tuple(Term) ->
    [encode_term(T) || T <- tuple_to_list(Term)];
encode_term(Term) ->
    Term.

%% @private Reply with error response.
-spec reply_error(integer(), binary(), term(), cowboy_req:req(), term()) ->
    {ok, cowboy_req:req(), term()}.
reply_error(StatusCode, Message, Details, Req, State) ->
    ErrorResponse = jsone:encode(#{
        <<"status">> => <<"error">>,
        <<"message">> => Message,
        <<"details">> => encode_term(Details)
    }),
    Req2 = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        ErrorResponse, Req),
    {ok, Req2, State}.

%%====================================================================
%% Public API for Testing
%%====================================================================

%% @doc Handle request (for testing without HTTP).
-spec handle_request(http_method(), path_segments(), json_object()) ->
    {ok, json_object()} | {error, term()}.
handle_request(<<"POST">>, [], Body) ->
    case validate_create_request(Body) of
        {ok, WorkflowModule, CaseId, InitArgs, Options} ->
            case create_workflow(WorkflowModule, CaseId, InitArgs, Options) of
                {ok, Pid} ->
                    {ok, #{
                        status => created,
                        case_id => CaseId,
                        pid => list_to_binary(pid_to_list(Pid))
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
handle_request(<<"GET">>, [], _Body) ->
    Workflows = yawl_registry:list(),
    WorkflowList = [#{
        case_id => CaseId,
        pid => list_to_binary(pid_to_list(Pid))
    } || {CaseId, Pid} <- Workflows],
    {ok, #{
        status => ok,
        workflows => WorkflowList,
        count => length(WorkflowList)
    }};
handle_request(<<"GET">>, [CaseId], _Body) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            case get_workflow_status(Pid) of
                {ok, Status} ->
                    {ok, #{
                        status => running,
                        case_id => CaseId,
                        pid => list_to_binary(pid_to_list(Pid)),
                        marking => maps:get(marking, Status, #{}),
                        usr_info => maps:get(usr_info, Status, #{})
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, not_found}
    end;
handle_request(<<"POST">>, [CaseId, <<"start">>], _Body) ->
    case yawl_registry:lookup(CaseId) of
        {ok, _Pid} ->
            {ok, #{
                status => started,
                case_id => CaseId
            }};
        {error, not_found} ->
            {error, not_found}
    end;
handle_request(<<"POST">>, [CaseId, <<"stop">>], _Body) ->
    case yawl_registry:lookup(CaseId) of
        {ok, Pid} ->
            case yawl_workflow_supervisor:stop_workflow(Pid) of
                ok ->
                    {ok, #{
                        status => stopped,
                        case_id => CaseId
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, not_found}
    end;
handle_request(_Method, _Path, _Body) ->
    {error, unsupported_endpoint}.
