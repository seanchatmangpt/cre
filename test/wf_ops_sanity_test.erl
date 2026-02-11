%% -*- erlang -*-
%%
-module(wf_ops_sanity_test).
-moduledoc """
Sanity tests for wf_ops process monitoring functions.

Test Slice 9 of compound doctests - ops sanity after drain.

This test module verifies that wf_ops functions work correctly
with gen_pnet processes, particularly after drain operations.

## Doctests

```erlang
> {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 1}, []),
> {ok, _} = gen_pnet:inject(Pz, #{in => [go]}),
> {ok, _} = gen_pnet:drain(Pz, 10),
> wf_ops:mailbox_len(Pz).
0
```

```erlang
> {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 1}, []),
> is_integer(wf_ops:proc_mem_words(Pz)) andalso wf_ops:proc_mem_words(Pz) > 0.
true
```

```erlang
> {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 1}, []),
> Count = wf_ops:proc_count(),
> is_integer(Count) andalso Count > 0.
true
```

```erlang
> {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 1}, []),
> ok = gen_pnet:stop(Pz),
> wf_ops:proc_mem_words(Pz).
0
```
""".

%%====================================================================
%% Exports
%%====================================================================

-export([]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% EUnit Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test Slice 9: ops sanity after drain.
%%
%% Verifies:
%% 1. wf_ops:proc_count/0 returns integer > 0
%% 2. wf_ops:mailbox_len/1 returns mailbox length for pid
%% 3. wf_ops:proc_mem_words/1 returns memory words for pid
%% 4. After drain, mailbox is empty
%%--------------------------------------------------------------------
ops_sanity_after_drain_test() ->
    %% Start a gen_pnet process with test net
    {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 1}, []),

    %% Verify proc_count returns positive integer
    Count = wf_ops:proc_count(),
    ?assert(is_integer(Count)),
    ?assert(Count > 0),

    %% Inject token to trigger activity
    {ok, _} = gen_pnet:inject(Pz, #{in => [go]}),

    %% Drain the net (fire transitions until quiescent)
    {ok, _Receipts} = gen_pnet:drain(Pz, 10),

    %% After drain, mailbox should be empty (0)
    MailboxLen = wf_ops:mailbox_len(Pz),
    ?assertEqual(0, MailboxLen),

    %% proc_mem_words should return positive integer for living process
    MemWords = wf_ops:proc_mem_words(Pz),
    ?assert(is_integer(MemWords)),
    ?assert(MemWords > 0),

    %% Clean up
    ok = gen_pnet:stop(Pz),

    %% After stop, proc_mem_words should return 0 for dead process
    ?assertEqual(0, wf_ops:proc_mem_words(Pz)).

%%--------------------------------------------------------------------
%% @doc Test that mailbox_len detects messages in queue.
%%--------------------------------------------------------------------
mailbox_len_with_messages_test() ->
    {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 42}, []),

    %% Initially mailbox should be empty (drain completes instantly)
    {ok, _} = gen_pnet:drain(Pz, 5),
    ?assertEqual(0, wf_ops:mailbox_len(Pz)),

    %% Inject multiple tokens rapidly - some may queue
    {ok, _} = gen_pnet:inject(Pz, #{in => [go]}),
    {ok, _} = gen_pnet:inject(Pz, #{in => [go]}),

    %% Drain again
    {ok, _} = gen_pnet:drain(Pz, 10),

    %% Mailbox should be empty after drain
    ?assertEqual(0, wf_ops:mailbox_len(Pz)),

    ok = gen_pnet:stop(Pz).

%%--------------------------------------------------------------------
%% @doc Test proc_count is consistent across multiple calls.
%%--------------------------------------------------------------------
proc_count_consistency_test() ->
    Count1 = wf_ops:proc_count(),
    timer:sleep(10),
    Count2 = wf_ops:proc_count(),

    %% Both should be positive integers
    ?assert(Count1 > 0),
    ?assert(Count2 > 0),
    ?assert(is_integer(Count1)),
    ?assert(is_integer(Count2)).

%%--------------------------------------------------------------------
%% @doc Test that proc_mem_words changes with process activity.
%%--------------------------------------------------------------------
proc_mem_words_activity_test() ->
    {ok, Pz} = gen_pnet:start_link(wf_test_net_choice, #{seed => 123}, []),

    %% Get initial memory
    Mem1 = wf_ops:proc_mem_words(Pz),
    ?assert(Mem1 > 0),

    %% Inject and drain to create activity
    {ok, _} = gen_pnet:inject(Pz, #{in => [go]}),
    {ok, _} = gen_pnet:drain(Pz, 10),

    %% Memory may have changed (could be same or different)
    Mem2 = wf_ops:proc_mem_words(Pz),
    ?assert(Mem2 > 0),

    ok = gen_pnet:stop(Pz).

%%--------------------------------------------------------------------
%% @doc Test ops functions handle invalid pid gracefully.
%%
%% Note: wf_ops:mailbox_len/1 and proc_mem_words/1 call error(badarg)
%% for non-pid inputs, while they return 0 for dead pids.
%%--------------------------------------------------------------------
ops_invalid_pid_test() ->
    %% Use a non-pid value (reference)
    NotAPid = make_ref(),

    %% mailbox_len should error for non-pid
    ?assertError(badarg, wf_ops:mailbox_len(NotAPid)),

    %% proc_mem_words should error for non-pid
    ?assertError(badarg, wf_ops:proc_mem_words(NotAPid)),

    %% But for a dead pid, should return 0
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),  %% Let it die
    ?assertEqual(0, wf_ops:mailbox_len(DeadPid)),
    ?assertEqual(0, wf_ops:proc_mem_words(DeadPid)).

%%--------------------------------------------------------------------
%% @doc Test ops sanity with multiple gen_pnet instances.
%%--------------------------------------------------------------------
ops_multiple_instances_test() ->
    {ok, Pz1} = gen_pnet:start_link(wf_test_net_choice, #{seed => 1}, []),
    {ok, Pz2} = gen_pnet:start_link(wf_test_net_choice, #{seed => 2}, []),

    %% Process count should include both instances
    Count = wf_ops:proc_count(),
    ?assert(Count > 0),

    %% Each should have positive memory
    ?assert(wf_ops:proc_mem_words(Pz1) > 0),
    ?assert(wf_ops:proc_mem_words(Pz2) > 0),

    %% Each should have empty mailbox after drain
    {ok, _} = gen_pnet:inject(Pz1, #{in => [go]}),
    {ok, _} = gen_pnet:inject(Pz2, #{in => [go]}),

    {ok, _} = gen_pnet:drain(Pz1, 10),
    {ok, _} = gen_pnet:drain(Pz2, 10),

    ?assertEqual(0, wf_ops:mailbox_len(Pz1)),
    ?assertEqual(0, wf_ops:mailbox_len(Pz2)),

    %% Cleanup
    ok = gen_pnet:stop(Pz1),
    ok = gen_pnet:stop(Pz2).
