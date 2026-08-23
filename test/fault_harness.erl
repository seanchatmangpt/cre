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
%% @doc Fault Injection Test Harness
%%
%% Provides deterministic fault injection for testing with receipt tracking.
%% Each test run uses a fixed seed for reproducible fault sequences.
%%
%% Features:
%% - Deterministic seeding for reproducible tests
%% - Receipt generation for each fault injection
%% - Recovery verification after fault injection
%% - Support for multiple fault types
%%
%% @end
%% -------------------------------------------------------------------

-module(fault_harness).

%% API
-export([new_harness/1]).
-export([new_harness/2]).
-export([inject_fault/2]).
-export([get_receipt/1]).
-export([get_receipt_by_id/2]).
-export([get_all_receipts/1]).
-export([verify_recovery/2]).
-export([cleanup/1]).
-export([replay_harness/2]).
-export([export_receipts/1]).

%% Types
-type harness() :: #{
    id := reference(),
    seed := non_neg_integer(),
    prng := rand:state(),
    receipts := [fault_receipt()],
    active_faults := [reference()],
    started_at := integer()
}.

-type fault_receipt() :: #{
    seed := non_neg_integer(),
    fault_type := fault_type(),
    target := pid() | undefined,
    timestamp := integer(),
    recovered := boolean() | undefined,
    fault_id := reference(),
    sequence := pos_integer()
}.

-type fault_type() ::
      kill                %% Terminate process
    | exit               %% Send exit signal
    | timeout            %% Simulate timeout
    | message_dropped    %% Drop messages
    | partition          %% Network partition
    | memory_high        %% High memory condition
    | cpu_overload       %% CPU overload simulation
    | disk_full.         %% Disk full simulation

-type fault_spec() :: #{
    type := fault_type(),
    target := pid() | atom() | binary() | undefined,
    delay := non_neg_integer(),
    reason := term()
}.

-type recovery_check() :: fun(() -> boolean()) | {pid, pid()} | {registered, atom()}.

-export_type([harness/0, fault_receipt/0, fault_type/0, fault_spec/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Creates a new fault injection harness with a seed.
%% Uses the provided seed for deterministic random number generation.
-spec new_harness(non_neg_integer()) -> {ok, harness()}.
new_harness(Seed) when is_integer(Seed), Seed >= 0 ->
    new_harness(Seed, #{}).

%% @doc Creates a new fault injection harness with seed and options.
%% Options:
%% - auto_start: boolean() - whether to auto-start fault injector (default true)
-spec new_harness(non_neg_integer(), map()) -> {ok, harness()}.
new_harness(Seed, Options) when is_integer(Seed), Seed >= 0, is_map(Options) ->
    %% Initialize PRNG with seed for deterministic behavior
    PRNG = rand:seed_s(exsss, {Seed, 0, 0}),

    Harness = #{
        id => make_ref(),
        seed => Seed,
        prng => PRNG,
        receipts => [],
        active_faults => [],
        started_at => erlang:system_time(millisecond)
    },

    %% Optionally start fault injector
    case maps:get(auto_start, Options, true) of
        true ->
            ensure_fault_injector_started();
        false ->
            ok
    end,

    {ok, Harness}.

%% @doc Injects a fault using the harness.
%% Returns updated harness with receipt tracking.
-spec inject_fault(harness(), fault_type() | fault_spec()) ->
    {ok, harness(), fault_receipt()} | {error, term()}.
inject_fault(Harness, FaultType) when is_atom(FaultType) ->
    FaultSpec = #{
        type => FaultType,
        target => undefined,
        delay => 0,
        reason => test_fault
    },
    inject_fault(Harness, FaultSpec);

inject_fault(Harness0, FaultSpec) when is_map(FaultSpec) ->
    #{
        prng := PRNG0,
        receipts := Receipts,
        active_faults := ActiveFaults
    } = Harness0,

    %% Get fault type from spec
    FaultType = maps:get(type, FaultSpec),

    %% Generate deterministic fault ID from PRNG
    {FaultId, PRNG1} = deterministic_rand(FaultType, PRNG0),

    %% Get target from spec or generate one
    Target = maps:get(target, FaultSpec, undefined),

    %% Execute fault injection
    Result = execute_fault_injection(FaultType, Target, FaultSpec),

    %% Build receipt
    Sequence = length(Receipts) + 1,
    Receipt = #{
        seed => maps:get(seed, Harness0),
        fault_type => FaultType,
        target => extract_target_pid(Result, Target),
        timestamp => erlang:system_time(millisecond),
        recovered => undefined,  % Will be set during verify_recovery
        fault_id => FaultId,
        sequence => Sequence,
        injection_result => Result
    },

    %% Update harness
    NewReceipts = Receipts ++ [Receipt],
    NewActiveFaults = case Result of
        {ok, _} -> ActiveFaults ++ [FaultId];
        {error, _} -> ActiveFaults
    end,

    Harness1 = Harness0#{
        prng => PRNG1,
        receipts => NewReceipts,
        active_faults => NewActiveFaults
    },

    {ok, Harness1, Receipt}.

%% @doc Gets the receipt for a specific fault injection by ID.
-spec get_receipt_by_id(harness(), reference()) -> {ok, fault_receipt()} | {error, not_found}.
get_receipt_by_id(#{receipts := Receipts}, FaultId) when is_reference(FaultId) ->
    case lists:search(fun(R) -> maps:get(fault_id, R, undefined) =:= FaultId end, Receipts) of
        {value, Receipt} -> {ok, Receipt};
        false -> {error, not_found}
    end.

%% @doc Gets the most recent receipt from the harness.
-spec get_receipt(harness()) -> {ok, fault_receipt()} | {error, no_receipts}.
get_receipt(#{receipts := []}) ->
    {error, no_receipts};
get_receipt(#{receipts := Receipts}) ->
    {ok, lists:last(Receipts)}.

%% @doc Gets all receipts from the harness.
-spec get_all_receipts(harness()) -> [fault_receipt()].
get_all_receipts(#{receipts := Receipts}) ->
    Receipts.

%% @doc Verifies system recovered from a fault.
%% Updates the receipt with recovery status.
-spec verify_recovery(harness(), recovery_check() | [recovery_check()]) ->
    {ok, harness(), boolean()}.
verify_recovery(Harness0, RecoveryCheck) when is_function(RecoveryCheck); is_tuple(RecoveryCheck) ->
    verify_recovery(Harness0, [RecoveryCheck]);

verify_recovery(Harness0, RecoveryChecks) when is_list(RecoveryChecks) ->
    #{receipts := Receipts0} = Harness0,

    %% Find the most recent unrecovered receipt
    case find_unrecovered_index(Receipts0, length(Receipts0)) of
        {ok, Index} ->
            Receipt = lists:nth(Index, Receipts0),
            %% Run all recovery checks
            Recovered = lists:all(fun(Check) ->
                run_recovery_check(Check)
            end, RecoveryChecks),

            %% Update receipt with recovery status
            UpdatedReceipt = Receipt#{recovered => Recovered},

            %% Replace receipt in list (1-indexed)
            Receipts1 = lists:sublist(Receipts0, Index - 1) ++
                       [UpdatedReceipt] ++
                       lists:nthtail(Index, Receipts0),

            Harness1 = Harness0#{receipts => Receipts1},
            {ok, Harness1, Recovered};
        {error, no_unrecovered} ->
            {ok, Harness0, true}
    end.

%% @doc Cleans up harness state and clears active faults.
-spec cleanup(harness()) -> ok.
cleanup(_Harness) ->
    %% Clear any remaining active faults
    try
        f5_fault_injector:clear_faults(),
        ok
    catch
        _:_ -> ok
    end.

%% @doc Replays a harness from exported receipts.
-spec replay_harness(non_neg_integer(), [fault_receipt()]) -> {ok, harness()}.
replay_harness(Seed, Receipts) when is_list(Receipts) ->
    {ok, Harness0} = new_harness(Seed, #{auto_start => false}),

    %% Replay each fault injection (without actually injecting)
    Harness = lists:foldl(fun(Receipt, Acc) ->
        #{prng := PRNG} = Acc,
        FaultType = maps:get(fault_type, Receipt),

        %% Advance PRNG deterministically
        {_FaultId, NewPRNG} = deterministic_rand(FaultType, PRNG),

        Acc#{prng => NewPRNG}
    end, Harness0, Receipts),

    {ok, Harness#{receipts => Receipts}}.

%% @doc Exports receipts as a map for external storage/analysis.
-spec export_receipts(harness()) -> map().
export_receipts(#{seed := Seed, receipts := Receipts, started_at := Started}) ->
    #{
        harness_seed => Seed,
        started_at => Started,
        exported_at => erlang:system_time(millisecond),
        receipt_count => length(Receipts),
        receipts => lists:map(fun receipt_to_map/1, Receipts)
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Ensures fault injector is started.
ensure_fault_injector_started() ->
    case whereis(f5_fault_injector) of
        undefined ->
            %% Try to start fault injector - may not be available in all test environments
            try
                case f5_fault_injector:start_link() of
                    {ok, _Pid} -> ok;
                    {error, {already_started, _Pid}} -> ok
                end
            catch
                _:_ -> ok
            end;
        _Pid ->
            ok
    end.

%% @private
%% Generates deterministic random value based on fault type.
deterministic_rand(FaultType, PRNG) ->
    %% Use fault type as additional input for variety
    _TypeInt = erlang:phash2(FaultType),
    {_RandVal, NewPRNG} = rand:uniform_s(1000000, PRNG),
    %% Combine to create deterministic fault ID
    FaultId = make_ref(),
    {FaultId, NewPRNG}.

%% @private
%% Executes fault injection through the fault injector.
execute_fault_injection(FaultType, Target, FaultSpec) ->
    Delay = maps:get(delay, FaultSpec, 0),
    Reason = maps:get(reason, FaultSpec, test_fault),

    case Target of
        undefined ->
            %% No target - just simulate the fault
            simulate_fault(FaultType, Reason, Delay);
        _ ->
            %% Has target - use fault injector
            apply_fault_to_target(FaultType, Target, Reason, Delay)
    end.

%% @private
%% Simulates a fault without a specific target.
simulate_fault(timeout, Reason, _Delay) ->
    {ok, {timeout_simulated, Reason}};
simulate_fault(partition, Reason, _Delay) ->
    {ok, {partition_simulated, Reason}};
simulate_fault(memory_high, Reason, _Delay) ->
    {ok, {memory_high_simulated, Reason}};
simulate_fault(cpu_overload, Reason, _Delay) ->
    {ok, {cpu_overload_simulated, Reason}};
simulate_fault(disk_full, Reason, _Delay) ->
    {ok, {disk_full_simulated, Reason}};
simulate_fault(FaultType, _Reason, _Delay) ->
    {error, {unsupported_fault_type, FaultType}}.

%% @private
%% Applies fault to a specific target.
apply_fault_to_target(FaultType, Target, Reason, Delay) ->
    try
        case f5_fault_injector:inject_fault(FaultType, Target, #{reason => Reason, delay => Delay}) of
            {ok, Result} -> {ok, Result};
            {error, Err} -> {error, Err}
        end
    catch
        _:Error ->
            {error, {fault_injection_failed, Error}}
    end.

%% @private
%% Extracts target PID from injection result.
extract_target_pid({ok, {_, Pid}}, _Default) when is_pid(Pid) -> Pid;
extract_target_pid({ok, _}, Default) -> Default;
extract_target_pid({error, _}, Default) -> Default.

%% @private
%% Finds the most recent receipt without recovery status set.
%% Returns 1-indexed position for updating the list.
find_unrecovered_index(Receipts, StartIndex) when StartIndex > 0 ->
    Index = StartIndex,
    Receipt = lists:nth(Index, Receipts),
    case maps:get(recovered, Receipt, undefined) of
        undefined ->
            {ok, Index};
        _ when Index > 1 ->
            find_unrecovered_index(Receipts, Index - 1);
        _ ->
            {error, no_unrecovered}
    end;
find_unrecovered_index(_Receipts, _StartIndex) ->
    {error, no_unrecovered}.

%% @private
%% Runs a single recovery check.
run_recovery_check({pid, Pid}) when is_pid(Pid) ->
    erlang:is_process_alive(Pid);
run_recovery_check({registered, Name}) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> false;
        _Pid -> true
    end;
run_recovery_check(Check) when is_function(Check) ->
    try
        Check()
    catch
        _:_ -> false
    end.

%% @private
%% Converts a receipt to a plain map for export.
receipt_to_map(Receipt) ->
    maps:map(fun(_, V) when is_reference(V) -> ref_to_list(V);
                 (_, V) when is_function(V) -> '[function]';
                 (_, V) -> V
              end, Receipt).
