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
%% @doc Example Simple Workflow
%%
%% A minimal workflow implementation for demonstrating the REST API.
%%
%% <h3>Workflow Structure</h3>
%%
%% ```
%%     [start] --t_process--> [processing] --t_complete--> [done]
%% ```
%%
%% <h3>Places</h3>
%% <ul>
%%   <li><b>start</b> - Initial place with one token</li>
%%   <li><b>processing</b> - Work in progress</li>
%%   <li><b>done</b> - Final place</li>
%% </ul>
%%
%% <h3>Transitions</h3>
%% <ul>
%%   <li><b>t_process</b> - Start processing (start → processing)</li>
%%   <li><b>t_complete</b> - Complete workflow (processing → done)</li>
%% </ul>
%%
%% <h3>Usage with REST API</h3>
%%
%% Create workflow:
%% ```bash
%% curl -X POST http://localhost:8080/workflows \
%%   -H "Content-Type: application/json" \
%%   -d '{
%%     "workflow_module": "example_simple_workflow",
%%     "case_id": "example-001",
%%     "init_args": {"user": "alice"},
%%     "options": []
%%   }'
%% ```
%%
%% Get status:
%% ```bash
%% curl http://localhost:8080/workflows/example-001
%% ```
%%
%% Stop workflow:
%% ```bash
%% curl -X POST http://localhost:8080/workflows/example-001/stop
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(example_simple_workflow).
-behaviour(gen_yawl).

%%====================================================================
%% Exports
%%====================================================================

%% gen_yawl structure callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).

%% gen_yawl interface callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%%====================================================================
%% Types
%%====================================================================

-type usr_info() :: #{
    user => binary(),
    start_time => integer(),
    complete_time => integer() | undefined
}.

%%====================================================================
%% gen_yawl Structure Callbacks
%%====================================================================

%% @doc List of places in the workflow.
-spec place_lst() -> [atom()].
place_lst() ->
    [start, processing, done].

%% @doc List of transitions in the workflow.
-spec trsn_lst() -> [atom()].
trsn_lst() ->
    [t_process, t_complete].

%% @doc Initial marking - one token in 'start' place.
-spec init_marking(atom(), usr_info()) -> [term()].
init_marking(start, _UsrInfo) ->
    [token];
init_marking(_, _UsrInfo) ->
    [].

%% @doc Preset (input places) for each transition.
-spec preset(atom()) -> [atom()].
preset(t_process) ->
    [start];
preset(t_complete) ->
    [processing].

%% @doc Check if a transition is enabled.
-spec is_enabled(atom(), #{atom() => [term()]}, usr_info()) -> boolean().
is_enabled(t_process, Mode, _UsrInfo) ->
    %% Enabled if there's a token in 'start'
    case maps:get(start, Mode, []) of
        [_Token] -> true;
        _ -> false
    end;
is_enabled(t_complete, Mode, _UsrInfo) ->
    %% Enabled if there's a token in 'processing'
    case maps:get(processing, Mode, []) of
        [_Token] -> true;
        _ -> false
    end.

%% @doc Fire a transition - consume tokens and produce new ones.
-spec fire(atom(), #{atom() => [term()]}, usr_info()) ->
    abort | {produce, #{atom() => [term()]}} | {produce, #{atom() => [term()]}, usr_info()}.
fire(t_process, _Mode, UsrInfo) ->
    %% Start processing
    logger:info("Workflow starting processing: ~p", [UsrInfo]),
    NewUsrInfo = UsrInfo#{start_time => erlang:system_time(millisecond)},
    {produce, #{processing => [token]}, NewUsrInfo};
fire(t_complete, _Mode, UsrInfo) ->
    %% Complete workflow
    logger:info("Workflow completed: ~p", [UsrInfo]),
    NewUsrInfo = UsrInfo#{complete_time => erlang:system_time(millisecond)},
    {produce, #{done => [token]}, NewUsrInfo}.

%%====================================================================
%% gen_yawl Interface Callbacks
%%====================================================================

%% @doc Initialize workflow user info.
-spec init(term()) -> usr_info().
init(Args) when is_map(Args) ->
    Args#{
        start_time => undefined,
        complete_time => undefined
    };
init(_Args) ->
    #{
        user => <<"anonymous">>,
        start_time => undefined,
        complete_time => undefined
    }.

%% @doc Handle code change.
-spec code_change(term(), term(), term()) -> {ok, term()} | {error, term()}.
code_change(_OldVsn, UsrInfo, _Extra) ->
    {ok, UsrInfo}.

%% @doc Handle synchronous calls.
-spec handle_call(term(), {pid(), term()}, term()) ->
    {reply, term()} | {reply, term(), #{atom() => [term()]}} | noreply.
handle_call(get_info, _From, UsrInfo) ->
    {reply, UsrInfo};
handle_call(_Request, _From, _UsrInfo) ->
    {reply, {error, unknown_request}}.

%% @doc Handle asynchronous casts.
-spec handle_cast(term(), term()) ->
    noreply | {noreply, #{atom() => [term()]}}.
handle_cast(_Request, _UsrInfo) ->
    noreply.

%% @doc Handle other messages.
-spec handle_info(term(), term()) ->
    noreply | {noreply, #{atom() => [term()]}}.
handle_info(_Info, _UsrInfo) ->
    noreply.

%%====================================================================
%% Documentation Tests
%%====================================================================

%% @doc Run doctests for this module.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

place_lst_test() ->
    ?assertEqual([start, processing, done], place_lst()).

trsn_lst_test() ->
    ?assertEqual([t_process, t_complete], trsn_lst()).

init_marking_test() ->
    UsrInfo = init(#{}),
    ?assertEqual([token], init_marking(start, UsrInfo)),
    ?assertEqual([], init_marking(processing, UsrInfo)),
    ?assertEqual([], init_marking(done, UsrInfo)).

preset_test() ->
    ?assertEqual([start], preset(t_process)),
    ?assertEqual([processing], preset(t_complete)).

is_enabled_test() ->
    UsrInfo = init(#{}),
    %% t_process enabled with token in start
    ?assertEqual(true, is_enabled(t_process, #{start => [token]}, UsrInfo)),
    ?assertEqual(false, is_enabled(t_process, #{start => []}, UsrInfo)),
    %% t_complete enabled with token in processing
    ?assertEqual(true, is_enabled(t_complete, #{processing => [token]}, UsrInfo)),
    ?assertEqual(false, is_enabled(t_complete, #{processing => []}, UsrInfo)).

fire_test() ->
    UsrInfo = init(#{}),
    %% Fire t_process
    {produce, Produce1, UsrInfo1} = fire(t_process, #{start => [token]}, UsrInfo),
    ?assertEqual(#{processing => [token]}, Produce1),
    ?assertMatch(#{start_time := _}, UsrInfo1),
    %% Fire t_complete
    {produce, Produce2, UsrInfo2} = fire(t_complete, #{processing => [token]}, UsrInfo1),
    ?assertEqual(#{done => [token]}, Produce2),
    ?assertMatch(#{complete_time := _}, UsrInfo2).

-endif.
