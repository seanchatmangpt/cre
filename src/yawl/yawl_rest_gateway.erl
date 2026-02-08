%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
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
%% @doc YAWL REST API Gateway
%%
%% This module implements a comprehensive REST API gateway that provides
%% YAWL-compatible HTTP endpoints matching Interface A, B, E, and X.
%%
%% <h3>Interface A (Management)</h3>
%% <ul>
%%   <li>POST /api/yawl/specifications - Upload workflow specification</li>
%%   <li>GET /api/yawl/specifications - List specifications</li>
%%   <li>GET /api/yawl/specifications/{id} - Get specification details</li>
%%   <li>POST /api/yawl/specifications/{id}/launch - Launch workflow instance</li>
%%   <li>GET /api/yawl/cases - List workflow cases</li>
%%   <li>GET /api/yawl/cases/{id} - Get case details</li>
%%   <li>POST /api/yawl/cases/{id}/cancel - Cancel a case</li>
%%   <li>DELETE /api/yawl/cases/{id} - Delete a case</li>
%% </ul>
%%
%% <h3>Interface B (Worklist)</h3>
%% <ul>
%%   <li>GET /api/yawl/worklist/{participant} - Get work items for participant</li>
%%   <li>GET /api/yawl/workitems/{id} - Get work item details</li>
%%   <li>POST /api/yawl/workitems/{id}/start - Start work item</li>
%%   <li>POST /api/yawl/workitems/{id}/complete - Complete work item</li>
%%   <li>POST /api/yawl/workitems/{id}/fail - Mark work item as failed</li>
%% </ul>
%%
%% <h3>Interface E (Logging)</h3>
%% <ul>
%%   <li>GET /api/yawl/logs - Query event log</li>
%%   <li>GET /api/yawl/logs/{case_id} - Get case log</li>
%%   <li>POST /api/yawl/logs/query - Advanced log query</li>
%% </ul>
%%
%% <h3>Interface X (Exception)</h3>
%% <ul>
%%   <li>POST /api/yawl/exceptions - Raise exception</li>
%%   <li>GET /api/yawl/exceptions/{id} - Get exception details</li>
%%   <li>POST /api/yawl/exceptions/{id}/handle - Handle exception</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_rest_gateway).
-behaviour(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

%% Cowboy handler callbacks
-export([init/2, terminate/3]).

%% API functions
-export([start_listener/0, start_listener/1, stop_listener/0]).
-export([get_routes/0]).

%% Internal exports for testing
-export([doctest_test/0]).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    method :: binary(),
    path :: [binary()],
    headers :: cowboy_req:headers(),
    auth_context :: undefined | map()
}).

%%====================================================================
%% Types
%%====================================================================

-type handler_result() ::
    {ok, cowboy_req:req(), term()} |
    {stop, cowboy_req:req(), term()}.

-export_type([handler_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the REST API listener with default configuration.
%%
%% @returns {ok, Port} | {error, Reason}
%%
-spec start_listener() -> {ok, inet:port_number()} | {error, term()}.

start_listener() ->
    start_listener(8080).

%% @doc Starts the REST API listener on the specified port.
%%
%% @returns {ok, Port} | {error, Reason}
%%
-spec start_listener(Port :: inet:port_number()) ->
    {ok, inet:port_number()} | {error, term()}.

start_listener(Port) ->
    Dispatch = cowboy_router:compile(get_routes()),
    case cowboy:start_clear(yawl_rest_listener,
                           [{port, Port}],
                           #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            logger:info("YAWL REST API gateway started on port ~p", [Port]),
            {ok, Port};
        {error, {already_started, _}} ->
            logger:info("YAWL REST API gateway already running on port ~p", [Port]),
            {ok, Port};
        Error ->
            logger:error("Failed to start YAWL REST API gateway: ~p", [Error]),
            Error
    end.

%% @doc Stops the REST API listener.
%%
%% @returns ok
%%
-spec stop_listener() -> ok.

stop_listener() ->
    case cowboy:stop_listener(yawl_rest_listener) of
        ok ->
            logger:info("YAWL REST API gateway stopped"),
            ok;
        {error, not_found} ->
            ok
    end.

%% @doc Gets the REST API route definitions.
%%
%% @returns Routes for cowboy_router:compile/1
%%
-spec get_routes() -> cowboy_router:routes().

get_routes() ->
    %% Single host binding with all routes (cowboy_router format)
    [{'_', [
        {"/health", ?MODULE, #{handler => health}},
        {"/api/yawl/specifications", ?MODULE, #{handler => specifications}},
        {"/api/yawl/specifications/:id", ?MODULE, #{handler => specification}},
        {"/api/yawl/specifications/:id/launch", ?MODULE, #{handler => launch}},
        {"/api/yawl/cases", ?MODULE, #{handler => cases}},
        {"/api/yawl/cases/:id", ?MODULE, #{handler => case_detail}},
        {"/api/yawl/cases/:id/cancel", ?MODULE, #{handler => case_cancel}},
        {"/api/yawl/cases/:id/suspend", ?MODULE, #{handler => case_suspend}},
        {"/api/yawl/cases/:id/resume", ?MODULE, #{handler => case_resume}},
        {"/api/yawl/worklist/:participant", ?MODULE, #{handler => worklist}},
        {"/api/yawl/workitems/:id", ?MODULE, #{handler => workitem}},
        {"/api/yawl/workitems/:id/start", ?MODULE, #{handler => workitem_start}},
        {"/api/yawl/workitems/:id/complete", ?MODULE, #{handler => workitem_complete}},
        {"/api/yawl/workitems/:id/fail", ?MODULE, #{handler => workitem_fail}},
        {"/api/yawl/logs", ?MODULE, #{handler => logs}},
        {"/api/yawl/logs/:case_id", ?MODULE, #{handler => case_logs}},
        {"/api/yawl/logs/query", ?MODULE, #{handler => logs_query}},
        {"/api/yawl/exceptions", ?MODULE, #{handler => exceptions}},
        {"/api/yawl/exceptions/:id", ?MODULE, #{handler => exception_detail}},
        {"/api/yawl/exceptions/:id/handle", ?MODULE, #{handler => exception_handle}}
    ]}].

%%====================================================================
%% Cowboy Handler Callbacks
%%====================================================================

%% @doc Initialize handler - dispatch to handler based on route binding.
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, Opts) ->
    Handler = maps:get(handler, Opts, health),
    Req = dispatch(Handler, Req0, Opts),
    {ok, Req, Opts}.

%% @doc Terminate callback (optional).
-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Handler Dispatch
%%====================================================================

-spec dispatch(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(Handler, Req0, _Opts) ->
    case Handler of
        health ->
            reply_json(200, #{<<"status">> => <<"ok">>}, Req0);
        specifications -> yawl_interface_a:handle(Req0, Handler);
        specification -> yawl_interface_a:handle(Req0, Handler);
        launch -> yawl_interface_a:handle(Req0, Handler);
        cases -> yawl_interface_a:handle(Req0, Handler);
        case_detail -> yawl_interface_a:handle(Req0, Handler);
        case_cancel -> yawl_interface_a:handle(Req0, Handler);
        case_suspend -> yawl_interface_a:handle(Req0, Handler);
        case_resume -> yawl_interface_a:handle(Req0, Handler);
        worklist -> yawl_interface_b:handle(Req0, Handler);
        workitem -> yawl_interface_b:handle(Req0, Handler);
        workitem_start -> yawl_interface_b:handle(Req0, Handler);
        workitem_complete -> yawl_interface_b:handle(Req0, Handler);
        workitem_fail -> yawl_interface_b:handle(Req0, Handler);
        logs -> yawl_interface_e:handle(Req0, Handler);
        case_logs -> yawl_interface_e:handle(Req0, Handler);
        logs_query -> yawl_interface_e:handle(Req0, Handler);
        exceptions -> yawl_interface_x:handle(Req0, Handler);
        exception_detail -> yawl_interface_x:handle(Req0, Handler);
        exception_handle -> yawl_interface_x:handle(Req0, Handler);
        _ ->
            reply_json(501, #{<<"error">> => <<"Not implemented">>, <<"handler">> => atom_to_binary(Handler)}, Req0)
    end.

reply_json(Status, Body, Req) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).

%%====================================================================
%% Doctests
%%====================================================================

-spec doctest_test() -> ok.
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    Routes = get_routes(),
    true = is_list(Routes),
    1 = length(Routes),
    {'_', PathList} = hd(Routes),
    true = length(PathList) >= 20,
    ok.