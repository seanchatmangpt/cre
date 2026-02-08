%% -*- erlang -*-
%%
%% CRE: YAWL Interface A - Workflow Management
%%
%% HTTP handlers for YAWL Interface A (specifications, cases). Calls
%% gen_yawl processes via yawl_registry and wf_yawl_executor.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_interface_a).

-export([handle/2]).

%% @doc Handles Interface A requests. Method and path from Req.
-spec handle(cowboy_req:req(), atom()) -> cowboy_req:req().
handle(Req0, Handler) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {Code, Body} = route_a(Method, Path, Req0, Handler),
    cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req0).

route_a(<<"GET">>, <<"/api/yawl/specifications">>, _Req, specifications) ->
    {200, #{<<"specifications">> => []}};
route_a(<<"POST">>, <<"/api/yawl/specifications">>, _Req, specifications) ->
    {501, #{<<"error">> => <<"Upload not implemented">>}};
route_a(<<"GET">>, _Path, Req, specification) ->
    Id = binding(Req, id),
    {200, #{<<"id">> => Id, <<"status">> => <<"loaded">>}};
route_a(<<"POST">>, _Path, Req, launch) ->
    _Id = binding(Req, id),
    {501, #{<<"error">> => <<"Launch not implemented">>}};
route_a(<<"GET">>, <<"/api/yawl/cases">>, _Req, cases) ->
    List = case whereis(yawl_registry) of
        undefined -> [];
        _ -> [CaseId || {CaseId, _Pid} <- yawl_registry:list()]
    end,
    {200, #{<<"cases">> => List}};
route_a(<<"GET">>, _Path, Req, case_detail) ->
    CaseId = binding(Req, id),
    case CaseId of
        <<>> -> {404, #{<<"error">> => <<"Not found">>}};
        _ ->
            case whereis(yawl_registry) of
                undefined -> {503, #{<<"error">> => <<"Registry unavailable">>}};
                _ ->
                    case yawl_registry:lookup(CaseId) of
                        {ok, Pid} ->
                            UsrInfo = try gen_yawl:usr_info(Pid) of
                                U -> U
                            catch _:_ -> #{}
                            end,
                            {200, #{<<"case_id">> => CaseId, <<"status">> => <<"running">>, <<"usr_info">> => UsrInfo}};
                        {error, not_found} -> {404, #{<<"error">> => <<"Case not found">>}}
                    end
            end
    end;
route_a(<<"POST">>, _Path, Req, case_cancel) ->
    CaseId = binding(Req, id),
    case CaseId of
        <<>> -> {404, #{<<"error">> => <<"Not found">>}};
        _ ->
            case whereis(yawl_registry) of
                undefined -> {503, #{<<"error">> => <<"Registry unavailable">>}};
                _ ->
                    case yawl_registry:lookup(CaseId) of
                        {ok, Pid} ->
                            _ = yawl_workflow_supervisor:stop_workflow(Pid),
                            {200, #{<<"status">> => <<"cancelled">>}};
                        {error, not_found} -> {404, #{<<"error">> => <<"Case not found">>}}
                    end
            end
    end;
route_a(<<"POST">>, _Path, Req, case_suspend) ->
    {501, #{<<"error">> => <<"Suspend not implemented">>}};
route_a(<<"POST">>, Path, Req, case_resume) ->
    {501, #{<<"error">> => <<"Resume not implemented">>}};
route_a(_Method, _Path, _Req, _Handler) ->
    {501, #{<<"error">> => <<"Not implemented">>}}.

binding(Req, Key) when is_atom(Key) ->
    cowboy_req:binding(Key, Req, <<>>).
