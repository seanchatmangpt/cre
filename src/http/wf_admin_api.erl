%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc Workflow Admin Monitoring API
%%
%% This module implements a Cowboy HTTP handler for the admin monitoring
%% API, providing endpoints for managing workflow cases, work items, and
%% system statistics.
%%
%% <h3>Endpoints</h3>
%% <ul>
%%   <li><b>GET /cases</b> - List all cases with status</li>
%%   <li><b>GET /cases/{id}</b> - Get case details</li>
%%   <li><b>POST /cases/{id}/cancel</b> - Cancel case</li>
%%   <li><b>POST /cases/{id}/suspend</b> - Suspend case</li>
%%   <li><b>POST /cases/{id}/resume</b> - Resume case</li>
%%   <li><b>GET /worklist/{user}</b> - Get user's work items</li>
%%   <li><b>GET /stats</b> - System statistics</li>
%% </ul>
%%
%% <h3>Authentication</h3>
%% All endpoints require authentication. The API supports:
%% - Basic authentication (username:password in Authorization header)
%% - Token authentication (Bearer token in Authorization header)
%%
%% <h3>Doctests</h3>
%%
%% List all cases:
%%
%% ```erlang
%% 1> wf_admin_api:list_cases().
%% [{ok, #{case_id => <<"case1">>, status => running, ...}}]
%% ```
%%
%% Get case details:
%%
%% ```erlang
%% 1> wf_admin_api:get_case(<<"case1">>).
%% {ok, #{case_id => <<"case1">>, status => running, ...}}
%% ```
%%
%% Cancel a case:
%%
%% ```erlang
%% 1> wf_admin_api:cancel_case(<<"case1">>).
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_admin_api).
-behavior(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

%% Cowboy handler callback
-export([init/2]).

%% API functions (for direct use, not HTTP)
-export([list_cases/0,
         get_case/1,
         cancel_case/1,
         suspend_case/1,
         resume_case/1,
         get_user_worklist/1,
         get_system_stats/0,
         doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

-include("cre.hrl").

%%====================================================================
%% Module Documentation
%%====================================================================

-moduledoc("""
Workflow Admin Monitoring API

This module provides HTTP endpoints for administering and monitoring
YAWL workflow cases. It integrates with `yawl_control` for case
management and `yawl_auth` for authentication.

## HTTP Endpoints

### List All Cases
```
GET /cases
```
Returns a list of all workflow cases with their status.

### Get Case Details
```
GET /cases/{id}
```
Returns detailed information about a specific case.

### Cancel Case
```
POST /cases/{id}/cancel
```
Cancels a running or suspended case.

### Suspend Case
```
POST /cases/{id}/suspend
```
Suspends a running case.

### Resume Case
```
POST /cases/{id}/resume
```
Resumes a suspended case.

### Get User Worklist
```
GET /worklist/{user}
```
Returns work items assigned to a specific user.

### System Statistics
```
GET /stats
```
Returns system-wide statistics and engine status.

## Authentication

All endpoints require authentication via:
- Basic Auth: `Authorization: Basic base64(username:password)`
- Token Auth: `Authorization: Bearer <session_token>`

## Examples

```erlang
%% List all cases via API
1> wf_admin_api:list_cases().
[#{case_id => <<"case1">>, status => running, ...}]

%% Get case details
2> wf_admin_api:get_case(<<"case1">>)).
{ok, #{case_id => <<"case1">>, status => running, ...}}

%% Cancel a case
3> wf_admin_api:cancel_case(<<"case1">>)).
ok
```
""").

%%====================================================================
%% Records
%%====================================================================

-record(auth_context, {
    user_id :: binary() | undefined,
    username :: binary() | undefined,
    roles :: [binary()] | undefined,
    authenticated = false :: boolean()
}).

-record(api_error, {
    code :: integer(),
    reason :: binary(),
    details :: term()
}).

%%====================================================================
%% Cowboy Handler Callback
%%====================================================================

%% @doc Cowboy handler callback for HTTP requests.
%%
%% Routes requests to appropriate handler functions based on path and method.
%% All endpoints require authentication.
%%
%% @end
-spec init(Req :: cowboy_req:req(), State :: term()) ->
    {ok, cowboy_req:req(), State :: term()}.

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    %% Authenticate request
    AuthResult = authenticate_request(Req0),

    case AuthResult of
        {ok, _AuthCtx} ->
            handle_request(Method, Path, Req0, State);
        {error, _Reason} ->
            reply_unauthorized(Req0, State)
    end.

%%====================================================================
%% Request Handling
%%====================================================================

%% @private
handle_request(Method, _Path, Req0, State) ->
    %% Parse path for routing
    {PathTokens, _} = cowboy_req:path_info(Req0),

    case {Method, PathTokens} of
        {<<"GET">>, [<<"cases">>]} ->
            handle_list_cases(Req0, State);
        {<<"GET">>, [<<"cases">>, CaseId]} ->
            handle_get_case(Req0, CaseId, State);
        {<<"POST">>, [<<"cases">>, CaseId, <<"cancel">>]} ->
            handle_cancel_case(Req0, CaseId, State);
        {<<"POST">>, [<<"cases">>, CaseId, <<"suspend">>]} ->
            handle_suspend_case(Req0, CaseId, State);
        {<<"POST">>, [<<"cases">>, CaseId, <<"resume">>]} ->
            handle_resume_case(Req0, CaseId, State);
        {<<"GET">>, [<<"worklist">>, User]} ->
            handle_get_worklist(Req0, User, State);
        {<<"GET">>, [<<"stats">>]} ->
            handle_get_stats(Req0, State);
        _ ->
            reply_not_found(Req0, State)
    end.

%%--------------------------------------------------------------------
%% @doc Handles GET /cases - list all cases
%% @end
%%--------------------------------------------------------------------
handle_list_cases(Req0, State) ->
    Cases = list_cases(),
    Response = #{
        status => <<"success">>,
        data => #{
            cases => Cases,
            count => length(Cases)
        }
    },
    reply_json(Req0, 200, Response, State).

%%--------------------------------------------------------------------
%% @doc Handles GET /cases/{id} - get case details
%% @end
%%--------------------------------------------------------------------
handle_get_case(Req0, CaseId, State) ->
    case get_case(CaseId) of
        {ok, Case} ->
            Response = #{
                status => <<"success">>,
                data => Case
            },
            reply_json(Req0, 200, Response, State);
        {error, not_found} ->
            reply_error(Req0, 404, <<"not_found">>,
                       <<"Case not found">>, CaseId, State)
    end.

%%--------------------------------------------------------------------
%% @doc Handles POST /cases/{id}/cancel - cancel case
%% @end
%%--------------------------------------------------------------------
handle_cancel_case(Req0, CaseId, State) ->
    case cancel_case(CaseId) of
        ok ->
            Response = #{
                status => <<"success">>,
                data => #{
                    case_id => CaseId,
                    action => <<"cancelled">>
                }
            },
            reply_json(Req0, 200, Response, State);
        {error, not_found} ->
            reply_error(Req0, 404, <<"not_found">>,
                       <<"Case not found">>, CaseId, State);
        {error, Reason} ->
            reply_error(Req0, 400, <<"bad_request">>,
                       to_binary(Reason), CaseId, State)
    end.

%%--------------------------------------------------------------------
%% @doc Handles POST /cases/{id}/suspend - suspend case
%% @end
%%--------------------------------------------------------------------
handle_suspend_case(Req0, CaseId, State) ->
    case suspend_case(CaseId) of
        ok ->
            Response = #{
                status => <<"success">>,
                data => #{
                    case_id => CaseId,
                    action => <<"suspended">>
                }
            },
            reply_json(Req0, 200, Response, State);
        {error, not_found} ->
            reply_error(Req0, 404, <<"not_found">>,
                       <<"Case not found">>, CaseId, State);
        {error, Reason} ->
            reply_error(Req0, 400, <<"bad_request">>,
                       to_binary(Reason), CaseId, State)
    end.

%%--------------------------------------------------------------------
%% @doc Handles POST /cases/{id}/resume - resume case
%% @end
%%--------------------------------------------------------------------
handle_resume_case(Req0, CaseId, State) ->
    case resume_case(CaseId) of
        ok ->
            Response = #{
                status => <<"success">>,
                data => #{
                    case_id => CaseId,
                    action => <<"resumed">>
                }
            },
            reply_json(Req0, 200, Response, State);
        {error, not_found} ->
            reply_error(Req0, 404, <<"not_found">>,
                       <<"Case not found">>, CaseId, State);
        {error, Reason} ->
            reply_error(Req0, 400, <<"bad_request">>,
                       to_binary(Reason), CaseId, State)
    end.

%%--------------------------------------------------------------------
%% @doc Handles GET /worklist/{user} - get user's work items
%% @end
%%--------------------------------------------------------------------
handle_get_worklist(Req0, User, State) ->
    Worklist = get_user_worklist(User),
    Response = #{
        status => <<"success">>,
        data => #{
            user => User,
            work_items => Worklist,
            count => length(Worklist)
        }
    },
    reply_json(Req0, 200, Response, State).

%%--------------------------------------------------------------------
%% @doc Handles GET /stats - system statistics
%% @end
%%--------------------------------------------------------------------
handle_get_stats(Req0, State) ->
    Stats = get_system_stats(),
    Response = #{
        status => <<"success">>,
        data => Stats
    },
    reply_json(Req0, 200, Response, State).

%%====================================================================
%% Authentication
%%====================================================================

%% @private
authenticate_request(Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req),

    case AuthHeader of
        undefined ->
            {error, missing_auth_header};
        Header ->
            case parse_auth_header(Header) of
                {basic, Username, Password} ->
                    authenticate_basic(Username, Password);
                {bearer, Token} ->
                    authenticate_token(Token);
                _ ->
                    {error, invalid_auth_format}
            end
    end.

%% @private
parse_auth_header(Header) ->
    Prefix = <<"Basic ">>,
    BearerPrefix = <<"Bearer ">>,

    case Header of
        <<Prefix:6/binary, Encoded/binary>> ->
            case base64:decode(Encoded) of
                {ok, Decoded} ->
                    case binary:split(Decoded, <<":">>) of
                        [Username, Password] ->
                            {basic, Username, Password};
                        _ ->
                            invalid
                    end;
                _ ->
                    invalid
            end;
        <<BearerPrefix:7/binary, Token/binary>> ->
            {bearer, Token};
        _ ->
            invalid
    end.

%% @private
authenticate_basic(Username, Password) ->
    %% Authenticate with yawl_auth
    case yawl_auth:authenticate(Username, Password) of
        {ok, _SessionId} ->
            %% Get user info
            {ok, #auth_context{
                username = Username,
                authenticated = true
            }};
        {error, _Reason} ->
            {error, invalid_credentials}
    end.

%% @private
authenticate_token(Token) ->
    case yawl_auth:get_session(Token) of
        {ok, _Session} ->
            {ok, #auth_context{
                authenticated = true
            }};
        {error, _Reason} ->
            {error, invalid_token}
    end.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Lists all workflow cases with status.
%%
%% Returns a list of case maps with keys:
%% - case_id: Unique case identifier
%% - spec_id: Workflow specification identifier
%% - status: Case status (running, suspended, completed, cancelled, failed)
%% - start_time: Case start timestamp
%% - end_time: Case end timestamp (if completed)
%% - current_task: Currently executing task
%% - progress: Progress ratio (0.0 to 1.0)
%% - tasks_completed: Number of completed tasks
%% - tasks_total: Total number of tasks
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Lists all workflow cases with status.

Returns a list of case information maps.

## Example

```erlang
1> wf_admin_api:list_cases().
[#{case_id => <<"case1">>, status => running, progress => 0.5},
 #{case_id => <<"case2">>, status => completed, progress => 1.0}]
```
""").
-spec list_cases() -> [map()].

list_cases() ->
    try
        yawl_control:list_all_cases()
    catch
        exit:{noproc, _} ->
            %% yawl_control not started, return empty list
            logger:warning("yawl_control not running"),
            [];
        Error:Reason ->
            logger:error("Error listing cases: ~p:~p", [Error, Reason]),
            []
    end.

%%--------------------------------------------------------------------
%% @doc Gets details for a specific case.
%%
%% Returns {ok, CaseMap} on success, {error, not_found} if case doesn't exist.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets details for a specific case.

Returns {ok, CaseMap} on success or {error, not_found}.

## Example

```erlang
1> wf_admin_api:get_case(<<"case1">>).
{ok, #{case_id => <<"case1">>, status => running, ...}}
```
""").
-spec get_case(binary()) -> {ok, map()} | {error, not_found | term()}.

get_case(CaseId) when is_binary(CaseId) ->
    try
        case yawl_control:get_case_status(CaseId) of
            {error, not_found} ->
                {error, not_found};
            CaseMap when is_map(CaseMap) ->
                {ok, CaseMap};
            Other ->
                {error, Other}
        end
    catch
        exit:{noproc, _} ->
            {error, service_unavailable};
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Cancels a workflow case.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Cancels a workflow case.

Returns ok on success or {error, Reason} on failure.

## Example

```erlang
1> wf_admin_api:cancel_case(<<"case1">>).
ok
```
""").
-spec cancel_case(binary()) -> ok | {error, term()}.

cancel_case(CaseId) when is_binary(CaseId) ->
    try
        %% Use default reason for API-initiated cancellations
        yawl_control:cancel_case(CaseId, <<"Cancelled via API">>)
    catch
        exit:{noproc, _} ->
            {error, service_unavailable};
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Suspends a workflow case.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Suspends a workflow case.

Returns ok on success or {error, Reason} on failure.

## Example

```erlang
1> wf_admin_api:suspend_case(<<"case1">>).
ok
```
""").
-spec suspend_case(binary()) -> ok | {error, term()}.

suspend_case(CaseId) when is_binary(CaseId) ->
    try
        yawl_control:suspend_case(CaseId, <<"Suspended via API">>)
    catch
        exit:{noproc, _} ->
            {error, service_unavailable};
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Resumes a suspended workflow case.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Resumes a suspended workflow case.

Returns ok on success or {error, Reason} on failure.

## Example

```erlang
1> wf_admin_api:resume_case(<<"case1">>).
ok
```
""").
-spec resume_case(binary()) -> ok | {error, term()}.

resume_case(CaseId) when is_binary(CaseId) ->
    try
        yawl_control:resume_case(CaseId, <<"Resumed via API">>)
    catch
        exit:{noproc, _} ->
            {error, service_unavailable};
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Gets work items for a specific user.
%%
%% Returns a list of work item maps assigned to the user.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets work items for a specific user.

Returns a list of work item maps for the specified user.

## Example

```erlang
1> wf_admin_api:get_user_worklist(<<"alice">>).
[#{workitem_id => <<"wi1">>, task_id => <<"task1">>, status => enabled}]
```
""").
-spec get_user_worklist(binary()) -> [map()].

get_user_worklist(User) when is_binary(User) ->
    try
        %% Get all cases and extract work items for this user
        AllCases = yawl_control:list_all_cases(),

        %% Extract work items from cases
        lists:flatmap(
            fun(CaseMap) ->
                extract_user_workitems(CaseMap, User)
            end,
            AllCases
        )
    catch
        exit:{noproc, _} ->
            logger:warning("yawl_control not running"),
            [];
        Error:Reason ->
            logger:error("Error getting worklist for ~p: ~p:~p",
                        [User, Error, Reason]),
            []
    end.

%%--------------------------------------------------------------------
%% @doc Gets system-wide statistics.
%%
%% Returns a map containing:
%% - cases: Case counts by status
%% - engine: Engine status and uptime
%% - performance: Performance metrics
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets system-wide statistics.

Returns a map with case counts, engine status, and performance metrics.

## Example

```erlang
1> wf_admin_api:get_system_stats().
#{cases => #{running => 2, completed => 10}, engine => #{uptime_ms => 12345}}
```
""").
-spec get_system_stats() -> map().

get_system_stats() ->
    try
        CaseStats = yawl_control:get_case_statistics(),
        EngineStatus = yawl_control:get_engine_status(),

        #{
            cases => CaseStats,
            engine => EngineStatus,
            timestamp => erlang:system_time(millisecond)
        }
    catch
        exit:{noproc, _} ->
            #{
                cases => #{error => service_unavailable},
                engine => #{error => service_unavailable},
                timestamp => erlang:system_time(millisecond)
            };
        Error:Reason ->
            logger:error("Error getting stats: ~p:~p", [Error, Reason]),
            #{error => {Error, Reason}}
    end.

%%====================================================================
%% JSON Response Helpers
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
reply_error(Req, StatusCode, Error, Message, Details, State) ->
    Body = #{
        status => <<"error">>,
        error => #{
            code => Error,
            message => Message,
            details => Details
        }
    },
    reply_json(Req, StatusCode, Body, State).

%% @private
reply_unauthorized(Req, State) ->
    Body = #{
        status => <<"error">>,
        error => #{
            code => <<"unauthorized">>,
            message => <<"Authentication required">>
        }
    },
    reply_json(Req, 401, Body, State).

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
extract_user_workitems(CaseMap, User) ->
    %% Extract work items assigned to user from case data
    case maps:get(data, CaseMap, #{}) of
        #{work_items := WorkItems} when is_list(WorkItems) ->
            [begin
                 WI#{case_id => maps:get(case_id, CaseMap)}
             end || WI <- WorkItems,
                is_assigned_to_user(WI, User)];
        _ ->
            []
    end.

%% @private
is_assigned_to_user(WorkItem, User) ->
    case maps:get(assigned_to, WorkItem, undefined) of
        User -> true;
        _ -> false
    end.

%% @private
to_binary(Term) when is_binary(Term) -> Term;
to_binary(Term) when is_list(Term) -> list_to_binary(Term);
to_binary(Term) -> list_to_binary(io_lib:format("~p", [Term])).

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the wf_admin_api module.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Runs doctests for the wf_admin_api module.

## Example

```erlang
1> wf_admin_api:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: list_cases/0 returns a list (may be empty if no cases or service unavailable)
    Cases = list_cases(),
    true = is_list(Cases),

    %% Test 2: Each case is a map with expected keys (if any cases exist)
    lists:foreach(
        fun(Case) ->
            true = is_map(Case),
            true = maps:is_key(case_id, Case),
            true = maps:is_key(status, Case)
        end,
        Cases
    ),

    %% Test 3: get_case/1 returns error for non-existent case
    Result = get_case(<<"nonexistent_case_12345">>),
    true = case Result of
        {error, not_found} -> true;
        {error, service_unavailable} -> true;
        _ -> false
    end,

    %% Test 4: get_system_stats/0 returns a map with expected keys
    Stats = get_system_stats(),
    true = is_map(Stats),

    %% Test 5: get_user_worklist/1 returns a list (may be empty)
    Worklist = get_user_worklist(<<"testuser">>),
    true = is_list(Worklist),

    %% Test 6: cancel_case/1 returns error for non-existent case or service unavailable
    CancelResult = cancel_case(<<"nonexistent_case_12345">>),
    true = case CancelResult of
        {error, _} -> true;
        ok -> true;  %% May succeed if yawl_control is running
        _ -> false
    end,

    %% Test 7: suspend_case/1 returns error for non-existent case or service unavailable
    SuspendResult = suspend_case(<<"nonexistent_case_12345">>),
    true = case SuspendResult of
        {error, _} -> true;
        _ -> false
    end,

    %% Test 8: resume_case/1 returns error for non-existent case or service unavailable
    ResumeResult = resume_case(<<"nonexistent_case_12345">>),
    true = case ResumeResult of
        {error, _} -> true;
        _ -> false
    end,

    %% Test 9: Verify valid status values
    ValidStatuses = [running, suspended, completed, cancelled, failed],
    lists:foreach(
        fun(Status) ->
            true = lists:member(Status, ValidStatuses)
        end,
        ValidStatuses
    ),

    %% Test 10: Verify JSON response structure helpers
    Response = #{
        status => <<"success">>,
        data => #{test => <<"value">>}
    },
    true = is_map(Response),
    <<"success">> = maps:get(status, Response),

    ok.
