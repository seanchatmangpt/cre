%% -*- erlang -*-
%% @doc Thread Split Pattern (WCP-42) for YAWL.
%%
%% This module implements Thread Split pattern (WCP-42) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Thread Split pattern creates multiple independent parallel execution paths
%% from a single starting point. Unlike Parallel Split, these threads
%% operate independently without requiring synchronization at the end.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, triggers split
%%     p_thread1..N    - Each independent thread path
%%     p_end           - Final place, any thread can end here
%%
%%   Transitions:
%%     t_split         - Split into multiple threads
%%     t_finish1..N    - Each thread can independently finish
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Each thread can complete independently</li>
%%   <li><b>Proper completion:</b> No waiting for other threads</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(thread_split).
-moduledoc """
Thread Split Pattern (WCP-42) for YAWL.

This pattern creates multiple independent parallel execution paths from a single
starting point. Threads operate independently without synchronization.

## Example

```erlang
%% Create a thread split with 4 independent threads
State = thread_split:new(4),
{ok, Pid} = gen_yawl:start_link(thread_split, State, []).

%% Each thread executes independently
%% Thread 1 can complete without waiting for others
```

## API

- `new/1` - Create new thread split with N branches
- `new/2` - Create thread split with custom branch list
- `get_branch_count/1` - Get number of threads
- `is_split/1` - Check if split has occurred
- `get_active_threads/1` - Get list of active threads

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
    fire/3,
    %% API exports
    new/1,
    new/2,
    get_branch_count/1,
    is_split/1,
    get_active_threads/1,
    get_thread_place/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(split_state, {
    branches :: [atom()],
    branch_count = 4 :: pos_integer(),
    split = false :: boolean(),
    active_threads = [] :: [atom()],
    log_id :: binary() | undefined
}).

-type split_state() :: #split_state{}.
-export_type([split_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Thread Split pattern state with N branches.

## Example

```erlang
> State = thread_split:new(4).
#split_state{branch_count = 4, branches = [...], ...}
```
""".
-spec new(BranchCount :: pos_integer()) -> split_state().
new(BranchCount) when is_integer(BranchCount), BranchCount > 0 ->
    Branches = [list_to_existing_atom("p_thread" ++ integer_to_list(N))
                || N <- lists:seq(1, BranchCount)],
    LogId = generate_log_id(),
    #split_state{
        branches = Branches,
        branch_count = BranchCount,
        split = false,
        active_threads = [],
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Creates a new Thread Split pattern state with custom branch list.

## Example

```erlang
> State = thread_split:new([p_thread1, p_thread2, p_custom]).
#split_state{branches = [p_thread1, p_thread2, p_custom], ...}
```
""".
-spec new(Branches :: [atom()], BranchCount :: pos_integer()) -> split_state().
new(Branches, BranchCount) when is_list(Branches), is_integer(BranchCount), BranchCount > 0 ->
    LogId = generate_log_id(),
    #split_state{
        branches = Branches,
        branch_count = BranchCount,
        split = false,
        active_threads = [],
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Gets the number of branch threads.

## Example

```erlang
> State = thread_split:new(4),
> thread_split:get_branch_count(State).
4
```
""".
-spec get_branch_count(State :: split_state()) -> pos_integer().
get_branch_count(#split_state{branch_count = Count}) ->
    Count.

%%--------------------------------------------------------------------
-doc """
Checks if the split has occurred.

## Example

```erlang
> State0 = thread_split:new(4),
> thread_split:is_split(State0).
false
> State1 = State0#split_state{split = true},
> thread_split:is_split(State1).
true
```
""".
-spec is_split(State :: split_state()) -> boolean().
is_split(#split_state{split = Split}) ->
    Split.

%%--------------------------------------------------------------------
-doc """
Gets the list of currently active threads.

## Example

```erlang
> State = #split_state{active_threads = [p_thread1, p_thread2]},
> thread_split:get_active_threads(State).
[p_thread1, p_thread2]
```
""".
-spec get_active_threads(State :: split_state()) -> [atom()].
get_active_threads(#split_state{active_threads = Threads}) ->
    Threads.

%%--------------------------------------------------------------------
-doc """
Gets the place atom for a specific thread number.

## Example

```erlang
> thread_split:get_thread_place(1).
p_thread1
> thread_split:get_thread_place(2).
p_thread2
```
""".
-spec get_thread_place(N :: pos_integer()) -> atom().
get_thread_place(N) when is_integer(N), N > 0 ->
    list_to_existing_atom("p_thread" ++ integer_to_list(N)).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Thread Split Petri net.

Places are dynamically generated based on branch count.
```erlang
> thread_split:place_lst().
[p_start, p_thread1, p_thread2, p_thread3, p_thread4, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_end] ++
    [get_thread_place(N) || N <- lists:seq(1, 4)].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Thread Split Petri net.

Transitions are dynamically generated based on branch count.
```erlang
> thread_split:trsn_lst().
[t_split, t_finish1, t_finish2, t_finish3, t_finish4]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_split] ++
    [list_to_existing_atom("t_finish" ++ integer_to_list(N))
     || N <- lists:seq(1, 4)].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: split_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> thread_split:preset(t_split).
[p_start]
> thread_split:preset(t_finish1).
[p_thread1]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_split) -> [p_start];
preset(Trsn) ->
    case atom_to_list(Trsn) of
        "t_finish" ++ Rest ->
            Num = list_to_integer(Rest),
            [get_thread_place(Num)];
        _ ->
            []
    end;
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: split_state()) ->
          boolean().

is_enabled(t_split, _Mode, _UsrInfo) ->
    true;
is_enabled(t_finish, #{p_thread1 := [token]}, _UsrInfo) ->
    true;
is_enabled(Trsn, Mode, #split_state{branches = Branches}) when is_atom(Trsn) ->
    %% Check if any thread has a token
    TrsnStr = atom_to_list(Trsn),
    case TrsnStr of
        "t_finish" ++ Rest ->
            Num = list_to_integer(Rest),
            Place = get_thread_place(Num),
            lists:member(Place, Branches) andalso maps:is_key(Place, Mode);
        _ ->
            false
    end;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: split_state()) ->
          {produce, map()} | {produce, map(), split_state()} | abort.

fire(t_split, #{p_start := [start]}, #split_state{branches = Branches, log_id = LogId} = State) ->
    log_event(LogId, <<"ThreadSplit">>, <<"Split">>, #{
        <<"branch_count">> => length(Branches),
        <<"branches">> => [atom_to_binary(B) || B <- Branches]
    }),
    %% Produce a token in each branch
    Produce = lists:foldl(fun(Branch, Acc) ->
        Acc#{Branch => [token]}
    end, #{}, Branches),
    {produce, #{
        p_start => [],
        p_end => []
    }, State#split_state{
        split = true,
        active_threads = Branches
    }};

fire(Trsn, Mode, #split_state{active_threads = Active, log_id = LogId} = State) ->
    %% Check which thread finished
    TrsnStr = atom_to_list(Trsn),
    case TrsnStr of
        "t_finish" ++ Rest ->
            Num = list_to_integer(Rest),
            Place = get_thread_place(Num),
            case maps:is_key(Place, Mode) of
                true ->
                    log_event(LogId, <<"ThreadSplit">>, <<"ThreadComplete">>, #{
                        <<"thread">> => atom_to_binary(Place)
                    }),
                    %% Thread completes independently
                    NewActive = lists:delete(Place, Active),
                    {produce, #{
                        Place => [],
                        p_end => [done]
                    }, State#split_state{active_threads = NewActive}};
                false ->
                    abort
            end;
        _ ->
            abort
    end;

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: split_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: split_state()) ->
          {ok, split_state()}.

init(SplitState) ->
    LogId = SplitState#split_state.log_id,
    case yawl_xes:new_log(#{<<"process">> => <<"ThreadSplit">>}) of
        {ok, XesLogId} ->
            State1 = SplitState#split_state{log_id = XesLogId},
            yawl_xes:log_case_start(XesLogId, generate_case_id()),
            {ok, State1};
        _ when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, SplitState};
        _ ->
            {ok, SplitState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_branch_count, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Count = get_branch_count(UsrInfo),
    {reply, {ok, Count}, NetState};
handle_call(is_split, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Split = is_split(UsrInfo),
    {reply, {ok, Split}, NetState};
handle_call(get_active_threads, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Threads = get_active_threads(UsrInfo),
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
        #split_state{log_id = LogId} when LogId =/= undefined ->
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
    <<"thread_split_", Hex/binary>>.

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
    ?assert(is_record(State, split_state)),
    ?assertEqual(4, State#split_state.branch_count),
    ?assertEqual(4, length(State#split_state.branches)),
    ?assertEqual(false, State#split_state.split).

%%--------------------------------------------------------------------
%% Test new/2
%%--------------------------------------------------------------------
new_custom_test() ->
    State = new([p_thread1, p_thread2, p_custom], 3),
    ?assert(lists:member(p_thread1, State#split_state.branches)),
    ?assert(lists:member(p_thread2, State#split_state.branches)),
    ?assert(lists:member(p_custom, State#split_state.branches)).

%%--------------------------------------------------------------------
%% Test get_branch_count/1
%%--------------------------------------------------------------------
get_branch_count_test() ->
    State = #split_state{branch_count = 5},
    ?assertEqual(5, get_branch_count(State)).

%%--------------------------------------------------------------------
%% Test is_split/1
%%--------------------------------------------------------------------
is_split_test() ->
    State0 = #split_state{split = false},
    ?assertNot(is_split(State0)),
    State1 = #split_state{split = true},
    ?assert(is_split(State1)).

%%--------------------------------------------------------------------
%% Test get_active_threads/1
%%--------------------------------------------------------------------
get_active_threads_test() ->
    State = #split_state{active_threads = [p_thread1, p_thread2]},
    ?assertEqual([p_thread1, p_thread2], get_active_threads(State)).

%%--------------------------------------------------------------------
%% Test get_thread_place/1
%%--------------------------------------------------------------------
get_thread_place_test() ->
    ?assertEqual(p_thread1, get_thread_place(1)),
    ?assertEqual(p_thread2, get_thread_place(2)),
    ?assertEqual(p_thread5, get_thread_place(5)).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_end, Places)),
    ?assert(lists:member(p_thread1, Places)),
    ?assert(lists:member(p_thread2, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_split, Transitions)),
    ?assert(lists:member(t_finish1, Transitions)),
    ?assert(lists:member(t_finish2, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_split)),
    ?assertEqual([p_thread1], preset(t_finish1)),
    ?assertEqual([p_thread2], preset(t_finish2)).

-endif.
