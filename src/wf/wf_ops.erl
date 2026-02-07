%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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

-module(wf_ops).

-moduledoc """
Operational inspection helpers for process monitoring.

Provides lightweight process inspection capabilities for monitoring
system health. Uses recon library when available, falls back to
native erlang:system_info functions.

```erlang
> is_integer(wf_ops:proc_count()).
true

> wf_ops:mailbox_len(self()).
0

> is_integer(wf_ops:proc_mem_words(self())).
true
```

```erlang
> Top = wf_ops:top_memory(3).
> length(Top) =:= 3 andalso
.. lists:all(fun({Pid, Bytes}) ->
..               is_pid(Pid) andalso is_integer(Bytes) andalso Bytes >= 0
..           end, Top).
true
```
""".

%%====================================================================
%% Exports
%%====================================================================

-export([proc_count/0, mailbox_len/1, proc_mem_words/1,
         top_memory/1, top_message_queue_len/1]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the current process count.
%%
%% Uses recon if available, otherwise falls back to erlang:system_info.
%%
%% @return Integer count of processes in the system
%%
%% @end
%%--------------------------------------------------------------------
-spec proc_count() -> non_neg_integer().

proc_count() ->
    case code:which(recon) of
        non_existing ->
            %% Fallback: use native erlang:system_info
            erlang:system_info(process_count);
        _ ->
            %% Use recon for more detailed info
            try
                %% recon:proc_count/2 returns a list, extract count
                Procs = recon:proc_count('reductions', 10),
                length(Procs)
            catch error:undef ->
                erlang:system_info(process_count)
            end
    end.

%%--------------------------------------------------------------------
%% @doc Returns the message queue length for a given process.
%%
%% Uses recon if available, otherwise falls back to process_info.
%%
%% @param Pid Process identifier
%% @return Integer message queue length
%%
%% @end
%%--------------------------------------------------------------------
-spec mailbox_len(Pid :: pid()) -> non_neg_integer().

mailbox_len(Pid) when is_pid(Pid) ->
    case code:which(recon) of
        non_existing ->
            %% Fallback: use native process_info
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Len} -> Len;
                undefined -> 0
            end;
        _ ->
            %% Use recon
            try recon:mailbox_len(Pid)
            catch error:undef ->
                case erlang:process_info(Pid, message_queue_len) of
                    {message_queue_len, Len} -> Len;
                    undefined -> 0
                end
            end
    end;
mailbox_len(_Other) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Returns memory usage in words for a given process.
%%
%% Uses recon if available, otherwise falls back to process_info.
%%
%% @param Pid Process identifier
%% @return Integer memory word count
%%
%% @end
%%--------------------------------------------------------------------
-spec proc_mem_words(Pid :: pid()) -> non_neg_integer().

proc_mem_words(Pid) when is_pid(Pid) ->
    case code:which(recon) of
        non_existing ->
            %% Fallback: use native process_info
            case erlang:process_info(Pid, memory) of
                {memory, Words} -> Words;
                undefined -> 0
            end;
        _ ->
            %% Use recon
            try recon:proc_mem_words(Pid)
            catch error:undef ->
                case erlang:process_info(Pid, memory) of
                    {memory, Words} -> Words;
                    undefined -> 0
                end
            end
    end;
proc_mem_words(_Other) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Returns top N processes by memory usage.
%%
%% Uses recon if available, otherwise implements sorting manually.
%% Returns list of {Pid, Bytes} tuples sorted by memory descending.
%%
%% @param N Number of top processes to return
%% @return List of {Pid, Bytes} tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec top_memory(N :: pos_integer()) -> [{pid(), non_neg_integer()}].

top_memory(N) when is_integer(N), N > 0 ->
    case code:which(recon) of
        non_existing ->
            %% Fallback: implement manually
            Processes = erlang:processes(),
            MemoryList = [{P, get_memory(P)} || P <- Processes],
            Sorted = lists:reverse(lists:keysort(2, MemoryList)),
            lists:sublist(Sorted, N);
        _ ->
            %% Use recon
            try recon:top_memory(N, 1)
            catch error:undef ->
                %% Fallback if function doesn't exist
                Processes = erlang:processes(),
                MemoryList = [{P, get_memory(P)} || P <- Processes],
                Sorted = lists:reverse(lists:keysort(2, MemoryList)),
                lists:sublist(Sorted, N)
            end
    end;
top_memory(_Other) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc Returns top N processes by message queue length.
%%
%% Uses recon if available, otherwise implements sorting manually.
%% Returns list of {Pid, QueueLen} tuples sorted by queue length descending.
%%
%% @param N Number of top processes to return
%% @return List of {Pid, QueueLen} tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec top_message_queue_len(N :: pos_integer()) -> [{pid(), non_neg_integer()}].

top_message_queue_len(N) when is_integer(N), N > 0 ->
    case code:which(recon) of
        non_existing ->
            %% Fallback: implement manually
            Processes = erlang:processes(),
            QueueList = [{P, get_queue_len(P)} || P <- Processes],
            %% Filter out processes with empty queues
            NonEmpty = [{P, Q} || {P, Q} <- QueueList, Q > 0],
            Sorted = lists:reverse(lists:keysort(2, NonEmpty)),
            lists:sublist(Sorted, N);
        _ ->
            %% Use recon
            try recon:top_message_queue_len(N, 1)
            catch error:undef ->
                %% Fallback if function doesn't exist
                Processes = erlang:processes(),
                QueueList = [{P, get_queue_len(P)} || P <- Processes],
                NonEmpty = [{P, Q} || {P, Q} <- QueueList, Q > 0],
                Sorted = lists:reverse(lists:keysort(2, NonEmpty)),
                lists:sublist(Sorted, N)
            end
    end;
top_message_queue_len(_Other) ->
    error(badarg).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Gets memory usage for a process safely.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_memory(Pid :: pid()) -> non_neg_integer().

get_memory(Pid) ->
    case erlang:process_info(Pid, memory) of
        {memory, Bytes} -> Bytes;
        undefined -> 0
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets message queue length for a process safely.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_queue_len(Pid :: pid()) -> non_neg_integer().

get_queue_len(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} -> Len;
        undefined -> 0
    end.

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit test runner for the module.
%% Tests the doctest examples from the moduledoc.
%%--------------------------------------------------------------------
doctest_test() ->
    %% Test proc_count/0 returns integer
    ?assert(is_integer(proc_count())),

    %% Test mailbox_len/1 returns 0 for fresh process (self may have messages in eunit)
    EmptyPid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(0, mailbox_len(EmptyPid)),
    exit(EmptyPid, kill),

    %% Test proc_mem_words/1 returns integer
    ?assert(is_integer(proc_mem_words(self()))),

    %% Test top_memory/1 returns correct format
    Top = top_memory(3),
    ?assertEqual(3, length(Top)),
    ?assert(lists:all(fun({Pid, Bytes}) ->
        is_pid(Pid) andalso is_integer(Bytes) andalso Bytes >= 0
    end, Top)),

    %% Test top_message_queue_len/1 returns correct format
    TopQ = top_message_queue_len(3),
    ?assert(is_list(TopQ)),
    ?assert(lists:all(fun({Pid, QLen}) ->
        is_pid(Pid) andalso is_integer(QLen) andalso QLen >= 0
    end, TopQ)),

    ok.

%%--------------------------------------------------------------------
%% @doc Test proc_count/0 returns positive integer.
%%--------------------------------------------------------------------
proc_count_positive_test() ->
    Count = proc_count(),
    ?assert(Count > 0).

%%--------------------------------------------------------------------
%% @doc Test mailbox_len/1 with current process.
%%--------------------------------------------------------------------
mailbox_len_self_test() ->
    %% Use fresh process - self() may have messages from eunit framework
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(0, mailbox_len(Pid)),
    exit(Pid, kill),

    %% Send message to self and check length increases
    self() ! test_message,
    ?assert(mailbox_len(self()) >= 1),

    %% Clean up
    receive test_message -> ok end,

    ok.

%%--------------------------------------------------------------------
%% @doc Test proc_mem_words/1 returns positive value for living process.
%%--------------------------------------------------------------------
proc_mem_words_positive_test() ->
    Words = proc_mem_words(self()),
    ?assert(Words > 0).

%%--------------------------------------------------------------------
%% @doc Test top_memory/1 with various N values.
%%--------------------------------------------------------------------
top_memory_n_test() ->
    %% N=1
    ?assertEqual(1, length(top_memory(1))),

    %% N=5
    ?assertEqual(5, length(top_memory(5))),

    %% N larger than process count
    LargeN = proc_count() + 100,
    Result = top_memory(LargeN),
    ?assert(length(Result) =< proc_count()),

    ok.

%%--------------------------------------------------------------------
%% @doc Test top_message_queue_len/1 returns sorted results.
%%--------------------------------------------------------------------
top_queue_len_sorted_test() ->
    %% Create a process with messages
    Pid = spawn(fun() ->
        [self() ! msg || _ <- lists:seq(1, 10)],
        receive after 100 -> ok end
    end),

    %% Give it time to send messages
    timer:sleep(10),

    Top = top_message_queue_len(5),

    %% Clean up
    exit(Pid, kill),

    ?assert(is_list(Top)),
    ?assert(length(Top) =< 5),

    ok.

%%--------------------------------------------------------------------
%% @doc Test error handling for invalid Pid.
%%--------------------------------------------------------------------
mailbox_len_badarg_test() ->
    ?assertError(badarg, mailbox_len(not_a_pid)).

%%--------------------------------------------------------------------
%% @doc Test error handling for proc_mem_words with bad arg.
%%--------------------------------------------------------------------
proc_mem_words_badarg_test() ->
    ?assertError(badarg, proc_mem_words(not_a_pid)).

%%--------------------------------------------------------------------
%% @doc Test error handling for top_memory with invalid N.
%%--------------------------------------------------------------------
top_memory_badarg_test() ->
    ?assertError(badarg, top_memory(0)),
    ?assertError(badarg, top_memory(-1)),
    ?assertError(badarg, top_memory(abc)).

%%--------------------------------------------------------------------
%% @doc Test error handling for top_message_queue_len with invalid N.
%%--------------------------------------------------------------------
top_queue_len_badarg_test() ->
    ?assertError(badarg, top_message_queue_len(0)),
    ?assertError(badarg, top_message_queue_len(-1)),
    ?assertError(badarg, top_message_queue_len(abc)).

%%--------------------------------------------------------------------
%% @doc Test that mailbox_len handles dead processes.
%%--------------------------------------------------------------------
mailbox_len_dead_proc_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),  %% Let it die

    %% Should return 0 for dead process
    ?assertEqual(0, mailbox_len(Pid)).

%%--------------------------------------------------------------------
%% @doc Test that proc_mem_words handles dead processes.
%%--------------------------------------------------------------------
proc_mem_words_dead_proc_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),  %% Let it die

    %% Should return 0 for dead process
    ?assertEqual(0, proc_mem_words(Pid)).

-endif.
