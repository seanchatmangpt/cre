%% -*- erlang -*-
%%
%% CRE: YAWL Interface B - Worklist
%%
%% HTTP handlers for YAWL Interface B (worklist, work items). Integrates
%% with yawl_worklist for observer notifications.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_interface_b).

-export([handle/2]).

%% @doc Handles Interface B requests (worklist, work items).
-spec handle(cowboy_req:req(), atom()) -> cowboy_req:req().
handle(Req0, Handler) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {Code, Body} = route_b(Method, Path, Req0, Handler),
    cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req0).

route_b(<<"GET">>, Path, Req, worklist) ->
    Participant = binding(Path, participant, Req),
    {200, #{<<"participant">> => Participant, <<"work_items">> => []}};
route_b(<<"GET">>, Path, Req, workitem) ->
    Id = binding(Path, id, Req),
    {200, #{<<"id">> => Id, <<"status">> => <<"offered">>}};
route_b(<<"POST">>, Path, Req, workitem_start) ->
    Id = binding(Path, id, Req),
    {200, #{<<"id">> => Id, <<"status">> => <<"started">>}};
route_b(<<"POST">>, Path, Req, workitem_complete) ->
    Id = binding(Path, id, Req),
    {200, #{<<"id">> => Id, <<"status">> => <<"completed">>}};
route_b(<<"POST">>, Path, Req, workitem_fail) ->
    Id = binding(Path, id, Req),
    {200, #{<<"id">> => Id, <<"status">> => <<"failed">>}};
route_b(_Method, _Path, _Req, _Handler) ->
    {501, #{<<"error">> => <<"Not implemented">>}}.

binding(_Path, Key, Req) when is_atom(Key) ->
    cowboy_req:binding(Key, Req, <<>>).
