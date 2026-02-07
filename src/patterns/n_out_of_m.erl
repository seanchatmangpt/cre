%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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

-module(n_out_of_m).
-moduledoc """
N out of M Pattern (WCP-22 Partial Join variant) for YAWL.

This module implements the N out of M pattern as a gen_pnet behaviour.

## Pattern Description

The N out of M pattern (a variant of WCP-22 Partial Join) merges multiple
concurrent branches, proceeding when N out of M branches have completed.
The quorum can be any number from 1 to M, allowing flexible synchronization.

## Examples

Get the list of places:

```erlang
> n_out_of_m:place_lst().
[p_start,p_branch_pool,p_running,p_completed,p_quorum_met,p_remaining,p_output]
```

Get the list of transitions:

```erlang
> n_out_of_m:trsn_lst().
[t_split,t_execute,t_complete,t_check_quorum,t_proceed,t_complete_all]
```

Get presets for transitions:

```erlang
> n_out_of_m:preset(t_split).
[p_start]
```

```erlang
> n_out_of_m:preset(t_check_quorum).
[p_completed]
```

```erlang
> n_out_of_m:preset(t_execute).
[p_branch_pool]
```

## Petri Net Structure

Places:
- `p_start` - Start of the workflow
- `p_branch_pool` - Pool of all branches
- `p_running` - Currently running branches
- `p_completed` - Completed branches (count toward quorum)
- `p_quorum_met` - Quorum has been reached
- `p_remaining` - Remaining branches to complete
- `p_output` - Final output

Transitions:
- `t_split` - Split into M branches
- `t_execute` - Execute a branch
- `t_complete` - Complete a branch
- `t_check_quorum` - Check if N branches completed
- `t_proceed` - Proceed when quorum met
- `t_complete_all` - Complete when all done (optional)

## Soundness Properties

- **Option to complete:** Always true (quorum reachable)
- **Proper completion:** Proceeds when exactly N branches complete
- **No dead transitions:** All branches execute and complete
""".
-behaviour(gen_yawl).

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3
]).

-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%% API exports
-export([
    new/3,
    start/2,
    run/2,
    get_state/1,
    execute/3,
    get_quorum/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(n_out_of_m_state, {
    m :: pos_integer(),  %% Total number of branches
    n :: pos_integer(),  %% Quorum required
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = [] :: [{pos_integer(), term()}],
    quorum_met = false :: boolean(),
    wait_for_all = false :: boolean(),  %% Whether to wait for all M after quorum
    log_id :: binary() | undefined
}).

-type n_out_of_m_state() :: #n_out_of_m_state{}.
-export_type([n_out_of_m_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new N out of M pattern state.
%%
%% @param N The quorum number (how many branches must complete).
%% @param M The total number of branches.
%% @param BranchFuns List of M functions to execute.
%% @return A new n_out_of_m_state record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(N :: pos_integer(), M :: pos_integer(), BranchFuns :: [function()]) ->
          n_out_of_m_state().

new(N, M, BranchFuns) when is_integer(N), N >= 1, is_integer(M), M >= N,
                          is_list(BranchFuns), length(BranchFuns) =:= M ->
    LogId = generate_log_id(),
    #n_out_of_m_state{
        m = M,
        n = N,
        branch_funs = BranchFuns,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the N out of M workflow as a gen_yawl process.
%%
%% @param N The quorum number (how many branches must complete).
%% @param BranchFuns List of functions to execute.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(N :: pos_integer(), BranchFuns :: [function()]) ->
          {ok, pid()} | {error, term()}.

start(N, BranchFuns) when is_integer(N), N >= 1, is_list(BranchFuns), length(BranchFuns) >= N ->
    M = length(BranchFuns),
    NOutOfMState = new(N, M, BranchFuns),
    gen_yawl:start_link(?MODULE, NOutOfMState, []).

%%--------------------------------------------------------------------
%% @doc Runs the N out of M workflow synchronously.
%%
%% @param N The quorum number (how many branches must complete).
%% @param BranchFuns List of functions to execute.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(N :: pos_integer(), BranchFuns :: [function()]) ->
          {ok, [{pos_integer(), term()}]} | {error, term()}.

run(N, BranchFuns) when is_integer(N), N >= 1, is_list(BranchFuns), length(BranchFuns) >= N ->
    case start(N, BranchFuns) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, Results} ->
                    gen_yawl:stop(Pid),
                    {ok, Results};
                {error, Reason} ->
                    gen_yawl:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the N out of M workflow.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, n_out_of_m_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the N out of M pattern with given input data.
%%
%% @param N The quorum number (how many branches must complete).
%% @param BranchFuns List of functions to execute.
%% @param InputData Input data to pass to each branch.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(N :: pos_integer(), BranchFuns :: [function()], InputData :: term()) ->
          {ok, [{pos_integer(), term()}]} | {error, term()}.

execute(N, BranchFuns, InputData) when is_integer(N), N >= 1,
                                      is_list(BranchFuns), length(BranchFuns) >= N ->
    M = length(BranchFuns),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all branches
    Pids = lists:map(fun({Fun, Index}) ->
        spawn(fun() ->
            try
                Result = Fun(InputData),
                Parent ! {Ref, {branch_complete, Index}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {branch_error, Index}, {Error, Reason, Stack}}
            end
        end)
    end, lists:zip(BranchFuns, lists:seq(1, M))),

    %% Collect N results (quorum)
    collect_quorum(Ref, Pids, N, M, [], []).

%%--------------------------------------------------------------------
%% @doc Gets the current quorum count for the running workflow.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return {ok, {N, M, Completed}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_quorum(Pid :: pid()) ->
          {ok, {pos_integer(), pos_integer(), non_neg_integer()}} | {error, term()}.

get_quorum(Pid) ->
    gen_yawl:call(Pid, get_quorum).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the N out of M Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_branch_pool',
        'p_running',
        'p_completed',
        'p_quorum_met',
        'p_remaining',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the N out of M Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_split',
        't_execute',
        't_complete',
        't_check_quorum',
        't_proceed',
        't_complete_all'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: n_out_of_m_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_split') -> ['p_start'];
preset('t_execute') -> ['p_branch_pool'];
preset('t_complete') -> ['p_running'];
preset('t_check_quorum') -> ['p_completed'];
preset('t_proceed') -> ['p_quorum_met'];
preset('t_complete_all') -> ['p_remaining'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: n_out_of_m_state()) ->
          boolean().

is_enabled('t_split', _Mode, _UsrInfo) ->
    true;
is_enabled('t_execute', #{'p_branch_pool' := Tokens}, _UsrInfo) when length(Tokens) > 0 ->
    true;
is_enabled('t_complete', #{'p_running' := Tokens}, _UsrInfo) when length(Tokens) > 0 ->
    true;
is_enabled('t_check_quorum', #{'p_completed' := Completed}, #n_out_of_m_state{n = N}) when length(Completed) >= N ->
    true;
is_enabled('t_proceed', #{'p_quorum_met' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_complete_all', #{'p_remaining' := Remaining}, #n_out_of_m_state{m = M, completed = Completed}) when length(Remaining) + length(Completed) =:= M ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: n_out_of_m_state()) ->
          {produce, map()} | abort.

fire('t_split', #{'p_start' := [start]}, #n_out_of_m_state{m = M, branch_funs = Funs} = State) ->
    %% Create branch tokens
    BranchTokens = [{{branch, I}, Fun} || {I, Fun} <- lists:zip(lists:seq(1, M), Funs)],
    log_event(State, <<"NOutOfM">>, <<"Split">>, #{<<"m">> => M}),
    {produce, #{
        'p_start' => [],
        'p_branch_pool' => BranchTokens
    }};

fire('t_execute', #{'p_branch_pool' := [Token | Rest]}, State) ->
    %% Execute a branch
    case Token of
        {{branch, Index}, Fun} ->
            log_event(State, <<"NOutOfM">>, <<"Execute">>, #{<<"branch">> => Index}),
            {produce, #{
                'p_branch_pool' => Rest,
                'p_running' => [{{branch, Index}, Fun}]
            }}
    end;

fire('t_complete', #{'p_running' := [{{branch, Index}, _Fun} | Rest]}, #n_out_of_m_state{results = Results} = State) ->
    %% Complete a branch
    NewState = State#n_out_of_m_state{
        completed = [Index | State#n_out_of_m_state.completed]
    },
    log_event(State, <<"NOutOfM">>, <<"BranchComplete">>, #{
        <<"branch">> => Index,
        <<"completed_count">> => length(NewState#n_out_of_m_state.completed)
    }),
    {produce, #{
        'p_running' => Rest,
        'p_completed' => [Index]
    }};

fire('t_check_quorum', #{'p_completed' := Completed}, #n_out_of_m_state{n = N, m = M, quorum_met = false} = State) when length(Completed) >= N ->
    %% Quorum met
    RemainingCount = M - length(Completed),
    NewState = State#n_out_of_m_state{quorum_met = true},
    log_event(State, <<"NOutOfM">>, <<"QuorumMet">>, #{
        <<"n">> => N,
        <<"completed">> => length(Completed),
        <<"remaining">> => RemainingCount
    }),
    case RemainingCount of
        0 ->
            %% All done
            {produce, #{
                'p_completed' => [],
                'p_output' => [{quorum_met, Completed}]
            }};
        _ ->
            {produce, #{
                'p_completed' => [],
                'p_quorum_met' => [quorum, Completed],
                'p_remaining' => [remaining, RemainingCount]
            }}
    end;

fire('t_proceed', #{'p_quorum_met' := [quorum, Completed]}, State) ->
    %% Proceed with quorum results
    log_event(State, <<"NOutOfM">>, <<"Proceed">>, #{
        <<"completed">> => Completed
    }),
    {produce, #{
        'p_quorum_met' => [],
        'p_output' => [{quorum_met, Completed}]
    }};

fire('t_complete_all', #{'p_remaining' := [remaining, _Count]}, #n_out_of_m_state{completed = Completed} = State) ->
    %% Wait for all remaining (optional behavior)
    log_event(State, <<"NOutOfM">>, <<"CompleteAll">>, #{
        <<"all_completed">> => Completed
    }),
    {produce, #{
        'p_remaining' => [],
        'p_output' => [{all_complete, Completed}]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: n_out_of_m_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: n_out_of_m_state()) ->
          {ok, n_out_of_m_state()}.

init(NOutOfMState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"NOutOfM">>}) of
        {ok, LogId} ->
            State1 = NOutOfMState#n_out_of_m_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, NOutOfMState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(get_quorum, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #n_out_of_m_state{n = N, m = M, completed = Completed} ->
            {reply, {ok, {N, M, length(Completed)}}, NetState};
        _ ->
            {reply, {error, invalid_state}, NetState}
    end;
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) ->
          ok.

terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #n_out_of_m_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, [{pos_integer(), term()}]} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #n_out_of_m_state{completed = Completed, results = Results} ->
                            {ok, Results ++ [{I, complete} || I <- Completed]};
                        _ ->
                            {error, no_results}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Collects results until quorum is reached.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec collect_quorum(Ref :: reference(), Pids :: [pid()], N :: pos_integer(),
                      M :: pos_integer(), Acc :: [{pos_integer(), term()}],
                      Errors :: [term()]) ->
          {ok, [{pos_integer(), term()}]} | {error, term()}.

collect_quorum(_Ref, _Pids, N, _M, Acc, _Errors) when length(Acc) >= N ->
    %% Quorum reached
    {ok, lists:reverse(Acc)};
collect_quorum(Ref, Pids, N, M, Acc, Errors) when length(Pids) =:= 0, length(Acc) + length(Errors) =:= M ->
    case length(Acc) >= N of
        true ->
            {ok, lists:reverse(Acc)};
        false ->
            {error, {quorum_not_met, N, length(Acc), length(Errors)}}
    end;
collect_quorum(Ref, Pids, N, M, Acc, Errors) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            collect_quorum(Ref, Pids, N, M, [{Index, Result} | Acc], Errors);
        {Ref, {branch_error, Index}, {Error, Reason, _Stack}} ->
            collect_quorum(Ref, Pids, N, M, Acc, [{Index, Error, Reason} | Errors])
    after 30000 ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"n_out_of_m_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Generates a unique case ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().

generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Logs an XES event.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec log_event(State :: n_out_of_m_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#n_out_of_m_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% DocTests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
