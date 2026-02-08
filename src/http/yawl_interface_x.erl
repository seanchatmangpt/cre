%% -*- erlang -*-
%%
%% CRE: YAWL Interface X - Exception
%%
%% HTTP handlers for YAWL Interface X (exceptions). Integrates with
%% yawl_interface_d for exception handling.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_interface_x).

-export([handle/2]).

%% @doc Handles Interface X requests (exceptions).
-spec handle(cowboy_req:req(), atom()) -> cowboy_req:req().
handle(Req0, Handler) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {Code, Body} = route_x(Method, Path, Req0, Handler),
    cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req0).

route_x(<<"POST">>, <<"/api/yawl/exceptions">>, Req, exceptions) ->
    {501, #{<<"error">> => <<"Raise exception not implemented">>}};
route_x(<<"GET">>, Path, Req, exception_detail) ->
    Id = binding(Path, id, Req),
    {200, #{<<"id">> => Id, <<"status">> => <<"pending">>}};
route_x(<<"POST">>, Path, Req, exception_handle) ->
    Id = binding(Path, id, Req),
    {200, #{<<"id">> => Id, <<"status">> => <<"handled">>}};
route_x(_Method, _Path, _Req, _Handler) ->
    {501, #{<<"error">> => <<"Not implemented">>}}.

binding(_Path, Key, Req) when is_atom(Key) ->
    cowboy_req:binding(Key, Req, <<>>).
