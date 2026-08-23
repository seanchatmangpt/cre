%%%-------------------------------------------------------------------
%%% @doc f5_fault_injector - Fortune-5 Fault Injection Utilities
%%%
%%% Provides fault injection capabilities for testing system resilience
%%% against various failure scenarios including process crashes, timeouts,
%%% and resource exhaustion.
%%%
%%% Functions:
%%% - kill_process/1: Abruptly terminate a process
%%% - block_process/2: Block a process for specified duration
%%% - induce_timeout/2: Cause a timeout in a gen_server call
%%% - spawn_blocker/1: Spawn a process that blocks message handling
%%% - crash_reason/1: Return a simulated crash reason
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(f5_fault_injector).

%% API
-export([kill_process/1]).
-export([block_process/2]).
-export([induce_timeout/2]).
-export([spawn_blocker/1]).
-export([crash_reason/0]).
-export([simulate_crash/1]).

%% Types
-type duration_ms() :: pos_integer().
-type crash_reason() :: term().

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% @doc Kill a process using exit signal.
%%
%% Simulates an abrupt process termination to test fault handling.
%%
%% @param Pid Process to kill
%% @returns true if signal was sent
-spec kill_process(pid()) -> boolean().
kill_process(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            exit(Pid, kill),
            true;
        false ->
            false
    end;
kill_process(_Other) ->
    {error, invalid_pid}.

%% @doc Block a process for specified duration.
%%
%% Sends a blocking message to the target process that will
%% cause it to sleep for the given duration. This simulates
%% a hung or slow-responding process.
%%
%% @param Pid Process to block
%% @param DurationMs Duration to block in milliseconds
%% @returns ok if block message sent
-spec block_process(pid(), duration_ms()) -> ok | {error, term()}.
block_process(Pid, DurationMs) when is_pid(Pid), is_integer(DurationMs), DurationMs > 0 ->
    Pid ! {f5_fault_block, DurationMs, self()},
    ok;
block_process(_Pid, _Duration) ->
    {error, invalid_arguments}.

%% @doc Induce a timeout in a gen_server call.
%%
%% Simulates a timeout by blocking the target process
%% longer than the specified timeout value.
%%
%% @param Pid Process to cause timeout in
%% @param TimeoutMs Timeout value to exceed
%% @returns ok
-spec induce_timeout(pid(), timeout()) -> ok.
induce_timeout(Pid, TimeoutMs) when is_pid(Pid) ->
    %% Block for longer than the timeout
    block_process(Pid, TimeoutMs + 100),
    ok.

%% @doc Spawn a blocker process that floods target with messages.
%%
%% Creates a process that continuously sends messages to the
%% target, potentially causing message queue overload.
%%
%% @param TargetPid Target process to flood
%% @returns BlockerPid
-spec spawn_blocker(pid()) -> pid().
spawn_blocker(TargetPid) when is_pid(TargetPid) ->
    spawn_link(fun() ->
        blocker_loop(TargetPid, 0)
    end).

%% @doc Return a random crash reason for testing.
%%
%% Generates various crash reasons to simulate different
%% failure scenarios.
%%
%% @returns A crash reason term
-spec crash_reason() -> crash_reason().
crash_reason() ->
    Reasons = [
        {badarith, []},
        {badarg, []},
        {badmatch, 42},
        {case_clause, unexpected_value},
        {function_clause, []},
        {if_clause, []},
        {try_clause, error},
        {undef, []},
        timeout,
        {badarity, {fun() -> ok end, []}},
        {error, simulated_error},
        {exit, simulated_exit},
        {throw, simulated_throw}
    ],
    lists:nth(rand:uniform(length(Reasons)), Reasons).

%% @doc Simulate a crash with specific reason.
%%
%% Used to trigger controlled crashes in test scenarios.
%%
%% @param Reason The crash reason to use
-spec simulate_crash(crash_reason()) -> no_return().
simulate_crash(Reason) ->
    exit(Reason).

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

%% @private Loop for blocker process
blocker_loop(TargetPid, Count) ->
    case is_process_alive(TargetPid) of
        true ->
            TargetPid ! {f5_blocker_msg, Count},
            %% Small delay to avoid CPU overload
            timer:sleep(1),
            blocker_loop(TargetPid, Count + 1);
        false ->
            %% Target died, stop blocking
            exit(normal)
    end.
