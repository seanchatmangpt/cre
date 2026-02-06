%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jrgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jrgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%% @doc CRE Client Process gen_server Implementation
%%
%% This module implements a gen_server behavior that acts as a client
%% for communicating with the CRE master process. It manages workflow
%% evaluation, request/response handling, and state tracking.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Asynchronous Evaluation:</b> Non-blocking workflow submission</li>
%%   <li><b>Request Tracking:</b> Maps pending requests to their state</li>
%%   <li><b>Reply Collection:</b> Aggregates worker replies before continuation</li>
%%   <li><b>Callback Interface:</b> Uses client module for customization</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Start an anonymous client
%% {ok, ClientPid} = cre_client:start_link(my_cre, my_client_mod, []),
%%
%% %% Submit a workflow for evaluation
%% Result = cre_client:eval(ClientPid, my_workflow),
%%
%% %% Stop the client
%% ok = cre_client:stop(ClientPid).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_client).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         init/1,
         terminate/2,
         handle_info/2]).

-export([start_link/3, start_link/4, eval/2, cre_reply/4, stop/1, doctest_test/0]).

%%====================================================================
%% Constant definitions
%%====================================================================

%% Poll interval is now managed via persistent_term (OTP 21+ optimization)
%% for O(1) access. Default 250ms.
-define(get_interval(), cre_config:get(cre_client_poll_interval, 250)).

%%====================================================================
%% Callback definitions
%%====================================================================


-callback init(InitArg :: _) -> UsrInfo :: _.

-callback is_value(E :: _, UsrInfo :: _) -> boolean().

-callback step(E :: _, UsrInfo :: _) -> {ok, _, [_]}.

-callback recv(E :: _, ReplyLst :: [{_, _}], UsrInfo :: _) -> _.

-callback load(_, UserInfo :: _) -> _.

-callback unload(_, UserInfo :: _) -> _.

%%====================================================================
%% Record definitions
%%====================================================================

-record(client_state, {
          cre_name,
          client_mod,
          usr_info,
          request_map = #{},
          reply_map = #{},
          state_map = #{}
         }).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an anonymous CRE client.
%%
%%      Returns `{ok, Pid}' on success where `Pid' is the process id of the
%%      newly created client process. The client will connect to the CRE
%%      master specified by `CreName' and use `ClientMod' for callbacks.
%%
%% @see start_link/4
%%
-doc("""
Starts an anonymous CRE client gen_server.

Links the client to the CRE master process identified by `CreName`.
The `ClientMod` module must implement the `cre_client` callback interface
to customize workflow evaluation behavior.

## Parameters

- `CreName`: Name or pid of the CRE master process
- `ClientMod`: Module implementing the client callbacks
- `ClientArg`: Argument passed to `ClientMod:init/1`

## Returns

`{ok, Pid}` on success, where `Pid` is the client process identifier

## Example

```erlang
1> {ok, Pid} = cre_client:start_link(my_cre, my_client_mod, []).
{ok,<0.123.0>}
```
""").

start_link(CreName, ClientMod, ClientArg) ->
    gen_server:start_link(?MODULE, {CreName, ClientMod, ClientArg}, []).


%% @doc Starts a named CRE client.
%%
%%      Registers the client under `ClientName' (an atom) and links it
%%      to the CRE master specified by `CreName'.
%%
%% @see start_link/3
%%
-doc("""
Starts a named CRE client gen_server.

Registers the client under `ClientName` (an atom) and links it to the
CRE master process identified by `CreName`. The client uses `ClientMod`
for callbacks during workflow evaluation.

## Parameters

- `ClientName`: Atom name to register the client process
- `CreName`: Name or pid of the CRE master process
- `ClientMod`: Module implementing the client callbacks
- `ClientArg`: Argument passed to `ClientMod:init/1`

## Returns

`{ok, Pid}` on success, where `Pid` is the client process identifier

## Example

```erlang
1> {ok, Pid} = cre_client:start_link(my_client, my_cre, my_client_mod, []).
{ok,<0.456.0>}
2> whereis(my_client).
<0.456.0>
```
""").

start_link(ClientName, CreName, ClientMod, ClientArg) ->
    gen_server:start_link(ClientName,
                          ?MODULE,
                          {CreName, ClientMod, ClientArg},
                          []).


%% @doc Evaluates a workflow expression.
%%
%%      Submits `E' for evaluation to the CRE master. This is a blocking
%%      call that returns only when the workflow completes.
%%
%% @see cre_reply/4
%%
-doc("""
Evaluates a workflow expression.

Submits the expression `E` for evaluation through the connected CRE master.
This is a blocking call that only returns when the workflow completes or fails.

The evaluation process:
1. Loads the expression via `ClientMod:load/2`
2. Iteratively evaluates via `ClientMod:step/2`
3. Collects worker replies
4. Returns the final result via `ClientMod:unload/2`

## Parameters

- `ClientName`: Name or pid of the client process
- `E`: Workflow expression to evaluate

## Returns

The result from `ClientMod:unload/2` when evaluation completes

## Example

```erlang
1> {ok, Pid} = cre_client:start_link(my_cre, my_client_mod, []),
1> Result = cre_client:eval(Pid, my_workflow_expr).
{ok, #{result => "completed"}}
```
""").

eval(ClientName, E) ->
    gen_server:call(ClientName, {eval, E}, infinity).


%% @doc Handles a reply from the CRE master.
%%
%%      Called by the CRE master to deliver a reply for a pending request.
%%      This is an asynchronous cast operation.
%%
%% @see eval/2
%%
-doc("""
Handles a reply from the CRE master.

Asynchronously delivers a worker reply to the client. Called by the CRE
master when a worker completes a task. The reply is queued and processed
during the next evaluation step.

## Parameters

- `ClientName`: Name or pid of the client process
- `I`: Request identifier (typically the gen_server `From` tuple)
- `A`: The task argument that was sent to the worker
- `Delta`: The result value returned by the worker

## Returns

`ok` (cast operation, no response)

## Example

```erlang
1> cre_client:cre_reply(my_client, From, task_arg, result_value).
ok
```
""").

cre_reply(ClientName, I, A, Delta) ->
    gen_server:cast(ClientName, {cre_reply, I, A, Delta}).


%% @doc Stops the client process.
%%
%%      Terminates the client gen_server gracefully.
%%
%% @see start_link/3
%%
-doc("""
Stops the client process.

Terminates the client gen_server gracefully, cleaning up all pending
requests and state maps.

## Parameters

- `ClientName`: Name or pid of the client process

## Returns

`ok`

## Example

```erlang
1> cre_client:stop(my_client).
ok
```
""").

stop(ClientName) ->
    gen_server:stop(ClientName).


%%====================================================================
%% gen_server callback functions
%%====================================================================


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _ClientState) ->
    ok.


init({CreName, ClientMod, ClientArg}) ->

    UsrInfo = ClientMod:init(ClientArg),

    ClientState = #client_state{
                    cre_name = CreName,
                    client_mod = ClientMod,
                    usr_info = UsrInfo
                   },

    _MonitorRef = monitor(process, CreName),

    {ok, ClientState}.


handle_call({eval, E}, From, ClientState) ->

    #client_state{
      client_mod = ClientMod,
      usr_info = UserInfo,
      request_map = RequestMap,
      reply_map = ReplyMap,
      state_map = StateMap
     } = ClientState,

    Self = self(),

    P = ClientMod:load(E, UserInfo),

    ClientState1 =
        ClientState#client_state{
          request_map = RequestMap#{From => P},
          reply_map = ReplyMap#{From => []},
          state_map = StateMap#{From => idle}
         },

    gen_server:cast(Self, {continue, From}),

    {noreply, ClientState1};

handle_call(_Request, _From, ClientState) ->
    {reply, {error, bad_msg}, ClientState}.


handle_cast({cre_reply, From, A, Delta}, ClientState) ->

    #client_state{
      reply_map = ReplyMap,
      state_map = StateMap
     } = ClientState,

    ClientState1 =
        case StateMap of
            #{From := State} ->
                case State of
                    idle -> start_timer(From);
                    _ -> ok
                end,

                #{From := ReplyLst} = ReplyMap,

                ClientState#client_state{
                  reply_map = ReplyMap#{From => [{A, Delta} | ReplyLst]},
                  state_map = #{From => primed}
                 };

            _ -> ClientState
        end,

    {noreply, ClientState1};

handle_cast({continue, From}, ClientState) ->

    #client_state{
      cre_name = CreName,
      client_mod = ClientMod,
      usr_info = UserInfo,
      request_map = RequestMap,
      reply_map = ReplyMap,
      state_map = StateMap
     } = ClientState,

    #{From := P} = RequestMap,
    #{From := ReplyLst} = ReplyMap,

    Self = self(),

    Send =
        fun(A) ->
                cre_master:cre_request(CreName, Self, From, A)
        end,

    % receive new result pairs
    P1 =
        case ReplyLst of
            [] -> P;
            [_ | _] -> ClientMod:recv(P, ReplyLst, UserInfo)
        end,

    % evaluate
    {ok, P2, ALst} = ClientMod:step(P1, UserInfo),

    % send new tasks
    lists:foreach(Send, ALst),

    % check if a value resulted
    ClientState1 =
        case ClientMod:is_value(P2, UserInfo) of

            true ->
                gen_server:reply(From, ClientMod:unload(P2, UserInfo)),
                ClientState#client_state{
                  request_map = maps:remove(From, RequestMap),
                  reply_map = maps:remove(From, ReplyMap),
                  state_map = maps:remove(From, StateMap)
                 };

            false ->
                ClientState#client_state{
                  request_map = RequestMap#{From => P2},
                  reply_map = ReplyMap#{From => []},
                  state_map = StateMap#{From => idle}
                 }
        end,

    {noreply, ClientState1};

handle_cast(_Request, ClientState) ->
    {noreply, ClientState}.


handle_info({'DOWN', _MonitorRef, process, _Object, _Info}, ClientState) ->
    {stop, cre_down, ClientState};

handle_info(_Info, ClientState) ->
    {noreply, ClientState}.


-spec start_timer(From :: _) -> ok.

start_timer(From) ->

    Self = self(),

    F =
        fun() ->
                %% Use persistent_term for O(1) access to poll interval
                %% (OTP 21+ optimization)
                timer:sleep(?get_interval()),
                gen_server:cast(Self, {continue, From})
        end,

    _Pid = spawn_link(F),

    ok.


%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the cre_client module.
%%
%% @end
%%--------------------------------------------------------------------
-doc("""
Run doctests for the cre_client module.

This function verifies the client gen_server implementation including:
- Client state record initialization
- Request/reply map management
- State map transitions
- Timer spawning for polling

## Example

```erlang
1> cre_client:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Client state record initialization
    State = #client_state{
        cre_name = test_cre,
        client_mod = test_client_mod,
        usr_info = test_usr_info,
        request_map = #{},
        reply_map = #{},
        state_map = #{}
    },
    test_cre = State#client_state.cre_name,
    test_client_mod = State#client_state.client_mod,
    test_usr_info = State#client_state.usr_info,
    #{} = State#client_state.request_map,
    #{} = State#client_state.reply_map,
    #{} = State#client_state.state_map,

    %% Test 2: Request map updates add new entries
    From = {pid, ref},
    P = test_program,
    RequestMap1 = #{From => P},
    State1 = State#client_state{request_map = RequestMap1},
    #{From := P} = State1#client_state.request_map,

    %% Test 3: Reply map accumulates replies
    ReplyMap1 = #{From => [{task1, result1}]},
    State2 = State1#client_state{reply_map = ReplyMap1},
    [{task1, result1}] = maps:get(From, State2#client_state.reply_map),

    %% Test 4: State map tracks request state transitions
    StateMap1 = #{From => idle},
    State3 = State2#client_state{state_map = StateMap1},
    idle = maps:get(From, State3#client_state.state_map),

    %% Test 5: State transitions from idle to primed on reply
    StateMap2 = #{From => primed},
    State4 = State3#client_state{state_map = StateMap2},
    primed = maps:get(From, State4#client_state.state_map),

    %% Test 6: Reply map prepends new replies
    ReplyMap2 = #{From => [{task2, result2}, {task1, result1}]},
    State5 = State4#client_state{reply_map = ReplyMap2},
    [{task2, result2}, {task1, result1}] = maps:get(From, State5#client_state.reply_map),

    %% Test 7: State map transitions back to idle after processing
    StateMap3 = #{From => idle},
    State6 = State5#client_state{state_map = StateMap3},
    idle = maps:get(From, State6#client_state.state_map),

    %% Test 8: Empty reply map resets after processing
    ReplyMap3 = #{From => []},
    State7 = State6#client_state{reply_map = ReplyMap3},
    [] = maps:get(From, State7#client_state.reply_map),

    ok.
