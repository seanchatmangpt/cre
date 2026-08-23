%% -*- erlang -*-
%% @doc Thread Merge Pattern (WCP-41) for YAWL.
%%
%% This module implements Thread Merge pattern (WCP-41) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Thread Merge pattern waits for all independent parallel threads to complete
%% before continuing. Unlike simple merge, this ensures synchronization
%% across multiple execution paths that were created independently.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, triggers thread creation
%%     p_thread1..N    - Each thread's completion point
%%     p_merged        - All threads have completed
%%     p_end           - Final place, workflow complete
%%
%%   Transitions:
%%     t_split         - Create multiple threads
%%     t_complete1..N   - Each thread completes
%%     t_merge         - Merge all completed threads
%%     t_finish        - Complete the workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always reachable after all threads complete</li>
%%   <li><b>Proper completion:</b> Only proceeds when all threads done</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(thread_merge).
-moduledoc """
Thread Merge Pattern (WCP-41) for YAWL.

This pattern synchronizes multiple independent threads, waiting for all
to complete before proceeding.

## Example

```erlang
%% Create a thread merge pattern with 4 threads
State = thread_merge:new(4),
{ok, Pid} = gen_yawl:start_link(thread_merge, State, []).

%% All 4 threads must complete before merge proceeds
```

## API

- `new/1` - Create new thread merge with N threads
- `new/2` - Create thread merge with custom thread list
- `get_thread_count/1` - Get number of threads
- `is_merged/1` - Check if merge has occurred
- `get_completed_threads/1` - Get list of completed threads

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

%% gen_yawl callbacks
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
    new/1,
    new/2,
    get_thread_count/1,
    is_merged/1,
    get_completed_threads/1,
    get_thread_place/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(merge_state, {
    threads :: [atom()],
    thread_count = 4 :: pos_integer(),
    merged = false :: boolean(),
    completed_threads = [] :: [atom()],
    log_id :: binary() | undefined
}).

-type merge_state() :: #merge_state{}.
-export_type([merge_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Thread Merge pattern state with N threads.

## Example

```erlang
> State = thread_merge:new(4).
#merge_state{thread_count = 4, threads = [...], ...}
```
""".
-spec new(ThreadCount :: pos_integer()) -> merge_state().
new(ThreadCount) when is_integer(ThreadCount), ThreadCount > 0 ->
    Threads = [list_to_existing_atom("p_thread" ++ integer_to_list(N))
               || N <- lists:seq(1, ThreadCount)],
    LogId = generate_log_id(),
    #merge_state{
        threads = Threads,
        thread_count = ThreadCount,
        merged = false,
        completed_threads = [],
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Creates a new Thread Merge pattern state with custom thread list.

## Example

```erlang
> State = thread_merge:new([p_thread1, p_thread2, p_custom]).
#merge_state{threads = [p_thread1, p_thread2, p_custom], ...}
```
""".
-spec new(Threads :: [atom()], ThreadCount :: pos_integer()) -> merge_state().
new(Threads, ThreadCount) when is_list(Threads), is_integer(ThreadCount), ThreadCount > 0 ->
    LogId = generate_log_id(),
    #merge_state{
        threads = Threads,
        thread_count = ThreadCount,
        merged = false,
        completed_threads = [],
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Gets the number of threads to merge.

## Example

```erlang
> State = thread_merge:new(4),
> thread_merge:get_thread_count(State).
4
```
""".
-spec get_thread_count(State :: merge_state()) -> pos_integer().
get_thread_count(#merge_state{thread_count = Count}) ->
    Count.

%%--------------------------------------------------------------------
-doc """
Checks if the merge has occurred.

## Example

```erlang
> State0 = thread_merge:new(4),
> thread_merge:is_merged(State0).
false
> State1 = State0#merge_state{merged = true},
> thread_merge:is_merged(State1).
true
```
""".
-spec is_merged(State :: merge_state()) -> boolean().
is_merged(#merge_state{merged = Merged}) ->
    Merged.

%%--------------------------------------------------------------------
-doc """
Gets the list of completed threads.

## Example

```erlang
> State = #merge_state{completed_threads = [p_thread1, p_thread2]},
> thread_merge:get_completed_threads(State).
[p_thread1, p_thread2]
```
""".
-spec get_completed_threads(State :: merge_state()) -> [atom()].
get_completed_threads(#merge_state{completed_threads = Threads}) ->
    Threads.

%%--------------------------------------------------------------------
-doc """
Gets the place atom for a specific thread number.

## Example

```erlang
> thread_merge:get_thread_place(1).
p_thread1
```
""".
-spec get_thread_place(N :: pos_integer()) -> atom().
get_thread_place(N) when is_integer(N), N > 0 ->
    list_to_existing_atom("p_thread" ++ integer_to_list(N)).

%%--------------------------------------------------------------------
-doc """
Gets the completion transition for a specific thread number.

## Example

```erlang
> thread_merge:get_complete_transition(1).
t_complete1
```
""".
-spec get_complete_transition(N :: pos_integer()) -> atom().
get_complete_transition(N) when is_integer(N), N > 0 ->
    list_to_existing_atom("t_complete" ++ integer_to_list(N)).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Thread Merge Petri net.

```erlang
> thread_merge:place_lst().
[p_start, p_thread1, p_thread2, p_thread3, p_thread4, p_merged, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_merged, p_end] ++
    [get_thread_place(N) || N <- lists:seq(1, 4)].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Thread Merge Petri net.

```erlang
> thread_merge:trsn_lst().
[t_split, t_complete1, t_complete2, t_complete3, t_complete4, t_merge, t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_split, t_merge, t_finish] ++
    [get_complete_transition(N) || N <- lists:seq(1, 4)].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: merge_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> thread_merge:preset(t_split).
[p_start]
> thread_merge:preset(t_merge).
[p_thread1, p_thread2, p_thread3, p_thread4]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_split) -> [p_start];
preset(Trsn) ->
    TrsnStr = atom_to_list(Trsn),
    case TrsnStr of
        "t_complete" ++ Rest ->
            Num = list_to_integer(Rest),
            [get_thread_place(Num)];
        _ ->
            []
    end;
preset(t_merge) ->
    %% All threads must be present (completed)
    [get_thread_place(N) || N <- lists:seq(1, 4)];
preset(t_finish) -> [p_merged];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: merge_state()) ->
          boolean().

is_enabled(t_split, _Mode, _UsrInfo) ->
    true;
is_enabled(Trsn, Mode, _UsrInfo) when is_atom(Trsn) ->
    TrsnStr = atom_to_list(Trsn),
    case TrsnStr of
        "t_complete" ++ Rest ->
            Num = list_to_integer(Rest),
            Place = get_thread_place(Num),
            maps:is_key(Place, Mode);
        _ ->
            false
    end;
is_enabled(t_merge, Mode, #merge_state{threads = Threads}) ->
    %% All threads must have tokens (be completed)
    lists:all(fun(Place) -> maps:is_key(Place, Mode) end, Threads);
is_enabled(t_finish, #{p_merged := [merged]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: merge_state()) ->
          {produce, map()} | {produce, map(), merge_state()} | abort.

fire(t_split, #{p_start := [start]}, #merge_state{threads = Threads, log_id = LogId} = State) ->
    log_event(LogId, <<"ThreadMerge">>, <<"Split">>, #{
        <<"thread_count">> => length(Threads),
        <<"threads">> => [atom_to_binary(T) || T <- Threads]
    }),
    %% Produce a token in each thread
    Produce = lists:foldl(fun(Thread, Acc) ->
        Acc#{Thread => [token]}
    end, #{}, Threads),
    {produce, #{
        p_start => [],
        p_merged => []
    }, State};

fire(Trsn, Mode, #merge_state{completed_threads = Completed, threads = Threads, log_id = LogId} = State) ->
    %% Check which thread completed
    TrsnStr = atom_to_list(Trsn),
    case TrsnStr of
        "t_complete" ++ Rest ->
            Num = list_to_integer(Rest),
            Place = get_thread_place(Num),
            case maps:is_key(Place, Mode) of
                true ->
                    log_event(LogId, <<"ThreadMerge">>, <<"ThreadComplete">>, #{
                        <<"thread">> => atom_to_binary(Place),
                        <<"completed_count">> => length(Completed) + 1,
                        <<"total_count">> => length(Threads)
                    }),
                    NewCompleted = [Place | Completed],
                    {produce, #{
                        Place => [done]
                    }, State#merge_state{completed_threads = NewCompleted}};
                false ->
                    abort
            end;
        _ ->
            abort
    end;

fire(t_merge, Mode, #merge_state{completed_threads = Completed, threads = Threads, log_id = LogId} = State) ->
    %% Verify all threads are complete
    AllComplete = lists:all(fun(T) -> lists:member(T, Completed) end, Threads),
    case AllComplete of
        true ->
            log_event(LogId, <<"ThreadMerge">>, <<"Merge">>, #{
                <<"completed_threads">> => [atom_to_binary(T) || T <- Completed]
            }),
            MergedMap = lists:foldl(fun(T, Acc) -> maps:put(T, [], Acc) end, #{}, Threads),
            {produce, MergedMap#{
                p_merged => [merged]
            }, State#merge_state{merged = true}};
        false ->
            abort
    end;

fire(t_finish, #{p_merged := [merged]}, #merge_state{log_id = LogId} = State) ->
    log_event(LogId, <<"ThreadMerge">>, <<"Complete">>, #{}),
    {produce, #{
        p_merged => [],
        p_end => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: merge_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: merge_state()) ->
          {ok, merge_state()}.

init(MergeState) ->
    LogId = MergeState#merge_state.log_id,
    case yawl_xes:new_log(#{<<"process">> => <<"ThreadMerge">>}) of
        {ok, XesLogId} ->
            State1 = MergeState#merge_state{log_id = XesLogId},
            yawl_xes:log_case_start(XesLogId, generate_case_id()),
            {ok, State1};
        _ when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, MergeState};
        _ ->
            {ok, MergeState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_thread_count, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Count = get_thread_count(UsrInfo),
    {reply, {ok, Count}, NetState};
handle_call(is_merged, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Merged = is_merged(UsrInfo),
    {reply, {ok, Merged}, NetState};
handle_call(get_completed_threads, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Threads = get_completed_threads(UsrInfo),
    {reply, {ok, Threads}, NetState};
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
        #merge_state{log_id = LogId} when LogId =/= undefined ->
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
%% @private
%% @doc Generates a unique log ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"thread_merge_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique case ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Logs an XES event.
%% @end
%%--------------------------------------------------------------------
-spec log_event(LogId :: binary() | undefined, Concept :: binary(),
                Lifecycle :: binary(), Data :: map()) -> ok.
log_event(undefined, _Concept, _Lifecycle, _Data) ->
    ok;
log_event(LogId, Concept, Lifecycle, Data) ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test new/1
%%--------------------------------------------------------------------
new_test() ->
    State = new(4),
    ?assert(is_record(State, merge_state)),
    ?assertEqual(4, State#merge_state.thread_count),
    ?assertEqual(4, length(State#merge_state.threads)),
    ?assertEqual(false, State#merge_state.merged).

%%--------------------------------------------------------------------
%% Test new/2
%%--------------------------------------------------------------------
new_custom_test() ->
    State = new([p_thread1, p_thread2, p_custom], 3),
    ?assert(lists:member(p_thread1, State#merge_state.threads)),
    ?assert(lists:member(p_custom, State#merge_state.threads)).

%%--------------------------------------------------------------------
%% Test get_thread_count/1
%%--------------------------------------------------------------------
get_thread_count_test() ->
    State = #merge_state{thread_count = 5},
    ?assertEqual(5, get_thread_count(State)).

%%--------------------------------------------------------------------
%% Test is_merged/1
%%--------------------------------------------------------------------
is_merged_test() ->
    State0 = #merge_state{merged = false},
    ?assertNot(is_merged(State0)),
    State1 = #merge_state{merged = true},
    ?assert(is_merged(State1)).

%%--------------------------------------------------------------------
%% Test get_completed_threads/1
%%--------------------------------------------------------------------
get_completed_threads_test() ->
    State = #merge_state{completed_threads = [p_thread1, p_thread2]},
    ?assertEqual([p_thread1, p_thread2], get_completed_threads(State)).

%%--------------------------------------------------------------------
%% Test get_thread_place/1
%%--------------------------------------------------------------------
get_thread_place_test() ->
    ?assertEqual(p_thread1, get_thread_place(1)),
    ?assertEqual(p_thread3, get_thread_place(3)).

%%--------------------------------------------------------------------
%% Test get_complete_transition/1
%%--------------------------------------------------------------------
get_complete_transition_test() ->
    ?assertEqual(t_complete1, get_complete_transition(1)),
    ?assertEqual(t_complete3, get_complete_transition(3)).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_merged, Places)),
    ?assert(lists:member(p_end, Places)),
    ?assert(lists:member(p_thread1, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_split, Transitions)),
    ?assert(lists:member(t_merge, Transitions)),
    ?assert(lists:member(t_finish, Transitions)),
    ?assert(lists:member(t_complete1, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_split)),
    ?assertEqual([p_thread1], preset(t_complete1)),
    ?assert(lists:member(p_thread1, preset(t_merge))),
    ?assertEqual([p_merged], preset(t_finish)).

-endif.
