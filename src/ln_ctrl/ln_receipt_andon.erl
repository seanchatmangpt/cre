%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 Receipt System Contributors
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
%% @module ln_receipt_andon
%% @doc Andon system for status signaling (green/yellow/red).
%%
%% Provides real-time visibility into system health with three states:
%% - Green: nominal execution, all gates passed
%% - Yellow: warnings (SLO near miss, recoverable errors)
%% - Red: critical halt (budget exceeded, hard errors)
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt_andon).

-export([
    new_andon/0,
    set_green/1,
    set_yellow/2,
    set_red/2,
    status/1,
    expose_http/2
]).

-type andon_handle() :: {andon, atom()}.
-type status_color() :: green | yellow | red.
-type status_details() :: maps:map().

%% ====================================================================
%% API
%% ====================================================================

-spec new_andon() -> {ok, andon_handle()}.
%% @doc Create a new andon status signaling system.
new_andon() ->
    AndonID = andon_ets,
    case ets:whereis(AndonID) of
        undefined ->
            ets:new(AndonID, [named_table, {keypos, 1}]),
            init_andon_state(AndonID),
            {ok, {andon, AndonID}};
        _Tid ->
            init_andon_state(AndonID),
            {ok, {andon, AndonID}}
    end.

-spec set_green(andon_handle()) -> ok.
%% @doc Set andon to green: nominal execution, all gates passed.
set_green({andon, Table}) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(Table, {status, green}),
    ets:insert(Table, {details, #{
        color => green,
        timestamp => Timestamp,
        gates_passed => true,
        nominal => true
    }}),
    ok.

-spec set_yellow(andon_handle(), [term()]) -> ok.
%% @doc Set andon to yellow: warnings detected.
set_yellow({andon, Table}, WarningList) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(Table, {status, yellow}),
    ets:insert(Table, {details, #{
        color => yellow,
        timestamp => Timestamp,
        warnings => WarningList,
        recoverable => true
    }}),
    ok.

-spec set_red(andon_handle(), atom() | string()) -> ok.
%% @doc Set andon to red: critical halt condition.
set_red({andon, Table}, HaltReason) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(Table, {status, red}),
    ets:insert(Table, {details, #{
        color => red,
        timestamp => Timestamp,
        halt_reason => HaltReason,
        critical => true
    }}),
    ok.

-spec status(andon_handle()) -> {status_color(), status_details()}.
%% @doc Get current andon status.
status({andon, Table}) ->
    case ets:lookup(Table, status) of
        [{status, Color}] ->
            case ets:lookup(Table, details) of
                [{details, Details}] ->
                    {Color, Details};
                [] ->
                    {Color, #{color => Color, timestamp => erlang:system_time(millisecond)}}
            end;
        [] ->
            {green, #{color => green, timestamp => erlang:system_time(millisecond)}}
    end.

-spec expose_http(andon_handle(), integer()) -> ok | {error, term()}.
%% @doc Expose andon status via HTTP endpoint for dashboard.
expose_http({andon, _Table} = AndonHandle, Port) ->
    % Store reference for HTTP handler
    HttpTableName = andon_http_registry,
    case ets:whereis(HttpTableName) of
        undefined ->
            ets:new(HttpTableName, [named_table, {keypos, 1}]);
        _Tid ->
            ok
    end,
    ets:insert(HttpTableName, {andon_handle, AndonHandle}),
    ets:insert(HttpTableName, {port, Port}),
    ok.

%% ====================================================================
%% Internal Functions
%% ====================================================================

-spec init_andon_state(atom()) -> ok.
init_andon_state(Table) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(Table, {status, green}),
    ets:insert(Table, {details, #{
        color => green,
        timestamp => Timestamp,
        gates_passed => true,
        nominal => true
    }}),
    ok.
