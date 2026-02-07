%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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

-module(wf_test_net_task_gate).
-moduledoc """
Test fixture gen_pnet module for task gate transitions.

This module implements a gen_pnet behavior that gates task completion tokens.
When a task token with status `done' appears in the `tasks' place, the
`t_complete' transition fires, consuming the done task and producing a
`{done, TaskId}' token in the `out' place.

## Petri Net Structure

**Places:**
- `tasks` - Holds task tokens of the form `{task, TaskId, Status, Payload}`
- `out` - Output place receiving `{done, TaskId}` tokens on task completion

**Transitions:**
- `t_complete` - Fires when a `done` task token is present, extracts TaskId

## Contract

The transition `t_complete` is enabled iff the `tasks` place contains a task token
with status `done`:
```erlang
{task, TaskId, done, Payload}
```

When `t_complete` fires, it:
1. Consumes the done task token from `tasks`
2. Produces `{done, TaskId}` in the `out` place

## Examples

Get the list of places:
```erlang
> wf_test_net_task_gate:place_lst().
[tasks, out]
```

Get the list of transitions:
```erlang
> wf_test_net_task_gate:trsn_lst().
[t_complete]
```

Get the preset for t_complete:
```erlang
> wf_test_net_task_gate:preset(t_complete).
[tasks]
```

Check if t_complete is enabled with a done task:
```erlang
> Mode = #{tasks => [{task, task42, done, #{result => ok}}]},
> wf_test_net_task_gate:is_enabled(t_complete, Mode, undefined).
true
```

Check if t_complete is NOT enabled with a non-done task:
```erlang
> Mode = #{tasks => [{task, task42, running, #{pct => 50}}]},
> wf_test_net_task_gate:is_enabled(t_complete, Mode, undefined).
false
```

Fire the transition, extracting TaskId from done task:
```erlang
> Mode = #{tasks => [{task, task42, done, #{ok => true}}]},
> wf_test_net_task_gate:fire(t_complete, Mode, undefined).
{produce,#{out => [{done, task42}], tasks => []}}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3,
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc User info state for the task gate net.
%%--------------------------------------------------------------------
-type usr_info() :: term().

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net.
%%--------------------------------------------------------------------
-type trsn() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%% Task tokens have form {task, TaskId, Status, Payload}.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc Task identifier extracted from task tokens.
%%--------------------------------------------------------------------
-type task_id() :: term().

%%--------------------------------------------------------------------
%% @doc Task status atom (enabled, running, done, failed, cancelled).
%%--------------------------------------------------------------------
-type task_status() :: enabled | running | done | failed | cancelled.

%%--------------------------------------------------------------------
%% @doc A mode maps places to lists of tokens to be consumed.
%%--------------------------------------------------------------------
-type mode() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%% Export types
-export_type([]).

%%====================================================================
%% gen_pnet Callbacks - Structure
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the task gate net.
%%
%% Places are `tasks' (input) and `out' (output).
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [place()].

place_lst() ->
    [tasks, out].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the task gate net.
%%
%% Only one transition: `t_complete'.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [trsn()].

trsn_lst() ->
    [t_complete].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%%
%% All places start empty.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: place(), UsrInfo :: usr_info()) -> [token()].

init_marking(_Place, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%%
%% `t_complete' consumes from `tasks'.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: trsn()) -> [place()].

preset(t_complete) ->
    [tasks];
preset(_Trsn) ->
    [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled with the given mode.
%%
%% `t_complete' is enabled iff `tasks' contains a `{task, _, done, _}' token.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          boolean().

is_enabled(t_complete, #{tasks := TaskTokens}, _UsrInfo) ->
    % Check if any task token has status 'done'
    lists:any(fun is_done_task/1, TaskTokens);
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @internal
%% @doc Returns true if the token is a done task token.
%%--------------------------------------------------------------------
-spec is_done_task(token()) -> boolean().

is_done_task({task, _TaskId, done, _Payload}) ->
    true;
is_done_task(_Other) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% `t_complete' consumes the done task token and produces `{done, TaskId}'
%% in the `out' place.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: trsn(), Mode :: mode(), UsrInfo :: usr_info()) ->
          {produce, marking()} | abort.

fire(t_complete, #{tasks := TaskTokens} = _Mode, _UsrInfo) ->
    case find_done_task(TaskTokens) of
        {ok, {task, TaskId, done, _Payload}} ->
            % Consume the done task, produce {done, TaskId} in out
            ProduceMap = #{
                tasks => [],  % consume all from tasks
                out => [{done, TaskId}]
            },
            {produce, ProduceMap};
        {error, not_found} ->
            abort
    end;
fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @internal
%% @doc Finds a done task token in the token list.
%% Returns {ok, TaskToken} or {error, not_found}.
%%--------------------------------------------------------------------
-spec find_done_task([token()]) -> {ok, token()} | {error, not_found}.

find_done_task([]) ->
    {error, not_found};
find_done_task([Token = {task, _TaskId, done, _Payload} | _Rest]) ->
    {ok, Token};
find_done_task([_Other | Rest]) ->
    find_done_task(Rest).

%%====================================================================
%% gen_pnet Callbacks - Interface
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet instance.
%%
%% Returns empty map as initial user info.
%% @end
%%--------------------------------------------------------------------
-spec init(NetArg :: term()) -> usr_info().

init(_NetArg) ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% Returns `{error, bad_msg}' for unknown requests.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), term()},
                  NetState :: term()) ->
          {reply, term()} |
          {reply, term(), marking()} |
          noreply |
          {noreply, marking()} |
          {stop, term(), term()}.

handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% Always returns noreply.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply |
          {noreply, marking()} |
          {stop, term()}.

handle_cast(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%%
%% Always returns noreply.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          noreply |
          {noreply, marking()} |
          {stop, term()}.

handle_info(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%%
%% Returns ok with net state unchanged.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%%
%% Always returns ok.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%%
%% Always passes (lets all tokens through).
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: place(), Token :: token(), UsrInfo :: usr_info()) ->
          pass | {consume, [token()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Runs all doctests for the module.
%% @private
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([tasks, out], place_lst()).

%% Test trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t_complete], trsn_lst()).

%% Test preset for t_complete
preset_t_complete_test() ->
    ?assertEqual([tasks], preset(t_complete)).

%% Test preset for unknown transition
preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test init_marking returns empty list
init_marking_test() ->
    ?assertEqual([], init_marking(tasks, undefined)),
    ?assertEqual([], init_marking(out, undefined)).

%% Test is_enabled true with done task token
is_enabled_done_task_test() ->
    Mode = #{tasks => [{task, task42, done, #{ok => true}}]},
    ?assertEqual(true, is_enabled(t_complete, Mode, undefined)).

%% Test is_enabled false with running task token
is_enabled_running_task_test() ->
    Mode = #{tasks => [{task, task42, running, #{pct => 50}}]},
    ?assertEqual(false, is_enabled(t_complete, Mode, undefined)).

%% Test is_enabled false with enabled task token
is_enabled_enabled_task_test() ->
    Mode = #{tasks => [{task, task42, enabled, #{}}]},
    ?assertEqual(false, is_enabled(t_complete, Mode, undefined)).

%% Test is_enabled false with failed task token
is_enabled_failed_task_test() ->
    Mode = #{tasks => [{task, task42, failed, timeout}]},
    ?assertEqual(false, is_enabled(t_complete, Mode, undefined)).

%% Test is_enabled false with empty tasks
is_enabled_empty_tasks_test() ->
    Mode = #{tasks => []},
    ?assertEqual(false, is_enabled(t_complete, Mode, undefined)).

%% Test fire with done task token extracts TaskId
fire_done_task_test() ->
    Mode = #{tasks => [{task, task42, done, #{ok => true}}]},
    ?assertEqual(
        {produce, #{out => [{done, task42}], tasks => []}},
        fire(t_complete, Mode, undefined)
    ).

%% Test fire with done task token and atomic TaskId
fire_atomic_task_id_test() ->
    Mode = #{tasks => [{task, my_task, done, #{}}]},
    ?assertEqual(
        {produce, #{out => [{done, my_task}], tasks => []}},
        fire(t_complete, Mode, undefined)
    ).

%% Test fire with done task token and binary TaskId
fire_binary_task_id_test() ->
    Mode = #{tasks => [{task, <<"task_123">>, done, #{}}]},
    ?assertEqual(
        {produce, #{out => [{done, <<"task_123">>}], tasks => []}},
        fire(t_complete, Mode, undefined)
    ).

%% Test fire aborts when no done task in list
fire_no_done_task_test() ->
    Mode = #{tasks => [{task, task42, running, #{}}]},
    ?assertEqual(abort, fire(t_complete, Mode, undefined)).

%% Test fire aborts with empty tasks
fire_empty_tasks_test() ->
    Mode = #{tasks => []},
    ?assertEqual(abort, fire(t_complete, Mode, undefined)).

%% Test fire with multiple tasks, finds the done one
fire_multiple_tasks_test() ->
    Mode = #{
        tasks => [
            {task, task1, running, #{}},
            {task, task2, done, #{ok => true}},
            {task, task3, enabled, #{}}
        ]
    },
    ?assertEqual(
        {produce, #{out => [{done, task2}], tasks => []}},
        fire(t_complete, Mode, undefined)
    ).

%% Test is_done_task helper
is_done_task_true_test() ->
    ?assertEqual(true, is_done_task({task, t1, done, #{}})).

is_done_task_false_test() ->
    ?assertEqual(false, is_done_task({task, t1, running, #{}})),
    ?assertEqual(false, is_done_task({task, t1, enabled, #{}})),
    ?assertEqual(false, is_done_task({task, t1, failed, reason})),
    ?assertEqual(false, is_done_task({task, t1, cancelled, reason})),
    ?assertEqual(false, is_done_task(other_token)).

%% Test find_done_task helper
find_done_task_found_test() ->
    Tokens = [{task, t1, running, #{}}, {task, t2, done, #{ok => true}}],
    ?assertEqual({ok, {task, t2, done, #{ok => true}}}, find_done_task(Tokens)).

find_done_task_not_found_test() ->
    Tokens = [{task, t1, running, #{}}],
    ?assertEqual({error, not_found}, find_done_task(Tokens)).

find_done_task_empty_test() ->
    ?assertEqual({error, not_found}, find_done_task([])).

%% Test init returns empty map
init_test() ->
    ?assertEqual(#{}, init(any_arg)).

%% Test handle_call returns bad_msg
handle_call_test() ->
    ?assertEqual({reply, {error, bad_msg}}, handle_call(req, self(), state)).

%% Test handle_cast
handle_cast_test() ->
    ?assertEqual(noreply, handle_cast(req, state)).

%% Test handle_info
handle_info_test() ->
    ?assertEqual(noreply, handle_info(info, state)).

%% Test code_change
code_change_test() ->
    ?assertEqual({ok, state}, code_change(vsn, state, extra)).

%% Test terminate
terminate_test() ->
    ?assertEqual(ok, terminate(normal, state)).

%% Test trigger passes
trigger_test() ->
    ?assertEqual(pass, trigger(place, token, usr_info)).

-endif.
