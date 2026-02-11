%%% @doc WF Executor - Bytecode Interpreter and Reduction Loop
%%%
%%% This module implements the core execution engine: a tight reduction loop
%%% that steps through bytecode opcodes and maintains the execution state.
%%% Each reduction step advances the program counter, updates context, and
%%% emits trace events.
%%%
%%% The executor is designed to be called in quanta (N steps at a time)
%%% to avoid monopolizing the Erlang scheduler.
%%%
%%% @end
-module(wf_exec).

-export([
    exec_init/2,
    exec_step/1,
    exec_steps/2,
    exec_until_effect/1,
    exec_until_halt/1,
    exec_status/1,
    is_halted/1,
    is_yielded/1,
    get_result/1
]).

-export_type([
    exec_result/0,
    exec_status/0
]).

%%% TYPES ===================================================================

%% Result from a single step
-type exec_result() ::
      {continue, wf_vm:exec_state()}                     % Reduction continues
    | {yield, wf_term:effect_spec(), wf_vm:exec_state()} % Yield for effect
    | {halt, ok, wf_vm:exec_state()}                     % Normal termination
    | {error, term(), wf_vm:exec_state()}.               % Error termination

%% Execution status for introspection
-type exec_status() :: #{
    state := running | halted | yielded | error,
    pc := non_neg_integer(),
    steps := non_neg_integer(),
    stack_depth := non_neg_integer(),
    last_opcode := atom() | undefined
}.

%%% INITIALIZATION ===========================================================

%% @doc Initialize execution state from a compiled pattern.
-spec exec_init(
    Compiled :: wf_compile:compiled(),
    InitCtx :: wf_term:context()
) -> wf_vm:exec_state().
exec_init({program, Program, _EntryPC, _ExitPC, _Size, _Metadata}, InitCtx) ->
    wf_vm:exec_state(
        Program,
        0,                      % PC = 0
        [],                     % empty stack
        InitCtx,                % initial context
        #{},                    % no join counters yet
        #{},                    % no cancel flags yet
        []                      % empty trace
    ).

%%% EXECUTION ================================================================

%% @doc Execute a single reduction step.
-spec exec_step(State :: wf_vm:exec_state()) -> exec_result().
exec_step(State) ->
    PC = wf_vm:exec_pc(State),
    Program = wf_vm:exec_program(State),

    case lists:nth(PC + 1, Program, undefined) of
        undefined ->
            %% Reached end of program
            {halt, ok, wf_vm:exec_add_trace_event(State, halt, halt)};

        Opcode ->
            %% Execute the opcode
            execute_opcode(Opcode, PC, State)
    end.

%% @doc Execute N reduction steps or until halt/yield.
-spec exec_steps(State :: wf_vm:exec_state(), NumSteps :: non_neg_integer()) ->
    {FinalState :: wf_vm:exec_state(), StepsExecuted :: non_neg_integer()}.
exec_steps(State, NumSteps) ->
    exec_steps_loop(State, NumSteps, 0).

-spec exec_steps_loop(
    State :: wf_vm:exec_state(),
    RemainingSteps :: non_neg_integer(),
    ExecutedSteps :: non_neg_integer()
) -> {wf_vm:exec_state(), non_neg_integer()}.
exec_steps_loop(State, 0, Executed) ->
    {State, Executed};
exec_steps_loop(State, Remaining, Executed) ->
    case exec_step(State) of
        {continue, NewState} ->
            exec_steps_loop(NewState, Remaining - 1, Executed + 1);
        {yield, _Spec, NewState} ->
            {NewState, Executed + 1};
        {halt, _Status, NewState} ->
            {NewState, Executed + 1};
        {error, _Reason, NewState} ->
            {NewState, Executed + 1}
    end.

%% @doc Execute until an effect is yielded.
-spec exec_until_effect(State :: wf_vm:exec_state()) -> exec_result().
exec_until_effect(State) ->
    case exec_step(State) of
        {yield, Spec, NewState} ->
            {yield, Spec, NewState};
        {halt, Status, NewState} ->
            {halt, Status, NewState};
        {error, Reason, NewState} ->
            {error, Reason, NewState};
        {continue, NewState} ->
            exec_until_effect(NewState)
    end.

%% @doc Execute until halt (successful or error).
-spec exec_until_halt(State :: wf_vm:exec_state()) -> exec_result().
exec_until_halt(State) ->
    case exec_step(State) of
        {continue, NewState} ->
            exec_until_halt(NewState);
        Other ->
            Other
    end.

%% @doc Get execution status.
-spec exec_status(State :: wf_vm:exec_state()) -> exec_status().
exec_status(State) ->
    PC = wf_vm:exec_pc(State),
    Stack = wf_vm:exec_stack(State),
    Trace = wf_vm:exec_trace(State),
    Program = wf_vm:exec_program(State),

    LastOpcode = case Trace of
        [] -> undefined;
        _ -> lists:last(Trace)
    end,

    CurrentOpcode = case lists:nth(PC + 1, Program, undefined) of
        undefined -> undefined;
        Op -> wf_vm:opcode_type(Op)
    end,

    ExecState = case CurrentOpcode of
        halt -> halted;
        undefined -> halted;
        _ -> running
    end,

    #{
        state => ExecState,
        pc => PC,
        steps => length(Trace),
        stack_depth => length(Stack),
        last_opcode => CurrentOpcode
    }.

%% @doc Check if execution has halted.
-spec is_halted(State :: wf_vm:exec_state()) -> boolean().
is_halted(State) ->
    Status = exec_status(State),
    maps:get(state, Status) == halted.

%% @doc Check if execution has yielded on an effect.
-spec is_yielded(State :: wf_vm:exec_state()) -> boolean().
is_yielded(State) ->
    Program = wf_vm:exec_program(State),
    PC = wf_vm:exec_pc(State),
    case lists:nth(PC + 1, Program, undefined) of
        {effect_yield, _} -> true;
        _ -> false
    end.

%% @doc Get the final result of execution.
-spec get_result(State :: wf_vm:exec_state()) -> {ok, wf_term:context()} | {error, term()}.
get_result(State) ->
    case is_halted(State) of
        true ->
            {ok, wf_vm:exec_ctx(State)};
        false ->
            {error, not_halted}
    end.

%%% OPCODE EXECUTION ========================================================

%% @doc Execute a single opcode.
-spec execute_opcode(
    Opcode :: wf_vm:opcode(),
    PC :: non_neg_integer(),
    State :: wf_vm:exec_state()
) -> exec_result().

%% Task: enter, call, exit
execute_opcode({task_enter, Name, Fun}, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, task_enter, task_enter),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

execute_opcode({task_call, Fun}, PC, State) ->
    Ctx = wf_vm:exec_ctx(State),
    case catch Fun(Ctx) of
        {ok, NewCtx} ->
            NewState = wf_vm:exec_add_trace_event(State, task_ok, task_call),
            {continue, wf_vm:exec_set_pc(
                wf_vm:exec_set_ctx(NewState, NewCtx),
                PC + 1
            )};
        {error, Reason} ->
            NewState = wf_vm:exec_add_trace_event(State, task_error, task_call),
            {error, Reason, NewState};
        {effect, Spec, ContCtx} ->
            NewState = wf_vm:exec_add_trace_event(State, effect_yield, task_call),
            {yield, Spec, wf_vm:exec_set_pc(
                wf_vm:exec_set_ctx(NewState, ContCtx),
                PC + 1
            )};
        Other ->
            NewState = wf_vm:exec_add_trace_event(State, task_error, task_call),
            {error, {invalid_task_return, Other}, NewState}
    end;

execute_opcode(task_exit, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, task_exit, task_exit),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

%% Sequence
execute_opcode({seq_enter, Name}, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, seq_enter, seq_enter),
    Frame = wf_vm:frame(seq, {Name}),
    {continue, wf_vm:exec_set_pc(
        wf_vm:exec_push_frame(NewState, Frame),
        PC + 1
    )};

execute_opcode(seq_exit, PC, State) ->
    {Frame, NewState1} = wf_vm:exec_pop_frame(State),
    NewState2 = wf_vm:exec_add_trace_event(NewState1, seq_exit, seq_exit),
    {continue, wf_vm:exec_set_pc(NewState2, PC + 1)};

%% Parallel fork
execute_opcode({par_fork, N}, PC, State) ->
    %% Initialize join counter for next PAR_JOIN
    Joins = wf_vm:exec_joins(State),
    NewJoins = maps:put(PC + N, {0, N}, Joins),
    NewState = wf_vm:exec_add_trace_event(State, par_fork, par_fork),
    {continue, wf_vm:exec_set_pc(
        wf_vm:exec_set_joins(NewState, NewJoins),
        PC + 1
    )};

%% Parallel join
execute_opcode({par_join, all}, PC, State) ->
    Joins = wf_vm:exec_joins(State),
    {Current, Expected} = wf_vm:join_get(Joins, PC),
    if
        Current >= Expected ->
            %% All branches completed
            NewJoins = wf_vm:join_reset(Joins, PC),
            NewState = wf_vm:exec_add_trace_event(State, par_join, par_join),
            {continue, wf_vm:exec_set_pc(
                wf_vm:exec_set_joins(NewState, NewJoins),
                PC + 1
            )};
        true ->
            %% Not all branches done yet, wait
            NewState = wf_vm:exec_add_trace_event(State, par_join_wait, par_join),
            {continue, State}
    end;

execute_opcode({par_join, xor_merge}, PC, State) ->
    %% XOR merge: take first available, discard others
    NewState = wf_vm:exec_add_trace_event(State, par_join_xor, par_join),
    Joins = wf_vm:exec_joins(State),
    NewJoins = wf_vm:join_reset(Joins, PC),
    {continue, wf_vm:exec_set_pc(
        wf_vm:exec_set_joins(NewState, NewJoins),
        PC + 1
    )};

execute_opcode({par_join, sync_merge}, PC, State) ->
    %% Synchronizing merge: coordinate branches
    NewState = wf_vm:exec_add_trace_event(State, par_join_sync, par_join),
    Joins = wf_vm:exec_joins(State),
    NewJoins = wf_vm:join_reset(Joins, PC),
    {continue, wf_vm:exec_set_pc(
        wf_vm:exec_set_joins(NewState, NewJoins),
        PC + 1
    )};

execute_opcode({par_join, {first_n, N}}, PC, State) ->
    %% First-N join: proceed when first N branches complete
    Joins = wf_vm:exec_joins(State),
    {Current, _Expected} = wf_vm:join_get(Joins, PC),
    if
        Current >= N ->
            NewJoins = wf_vm:join_reset(Joins, PC),
            NewState = wf_vm:exec_add_trace_event(State, par_join_first_n, par_join),
            {continue, wf_vm:exec_set_pc(
                wf_vm:exec_set_joins(NewState, NewJoins),
                PC + 1
            )};
        true ->
            {continue, State}
    end;

%% XOR choice
execute_opcode({xor_choose, _Branches, DefaultIdx}, PC, State) ->
    %% For now, always pick the default branch
    %% TODO: support conditions in branches
    NewState = wf_vm:exec_add_trace_event(State, xor_choose, xor_choose),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

%% Loop back
execute_opcode({loop_back, {max_iter, MaxIter}}, PC, State) ->
    %% TODO: track iteration count
    NewState = wf_vm:exec_add_trace_event(State, loop_back, loop_back),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

execute_opcode({loop_back, _Condition}, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, loop_back, loop_back),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

%% Deferred choice
execute_opcode({defer_race, _Branches}, PC, State) ->
    %% TODO: implement signal waiting
    NewState = wf_vm:exec_add_trace_event(State, defer_race, defer_race),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

%% Join wait (generalized)
execute_opcode({join_wait, all}, PC, State) ->
    Joins = wf_vm:exec_joins(State),
    {Current, Expected} = wf_vm:join_get(Joins, PC),
    if
        Current >= Expected ->
            NewJoins = wf_vm:join_reset(Joins, PC),
            NewState = wf_vm:exec_add_trace_event(State, join_wait_done, join_wait),
            {continue, wf_vm:exec_set_pc(
                wf_vm:exec_set_joins(NewState, NewJoins),
                PC + 1
            )};
        true ->
            {continue, State}
    end;

execute_opcode({join_wait, _Policy}, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, join_wait, join_wait),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

%% Effects
execute_opcode({effect_yield, Spec}, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, effect_yield, effect_yield),
    {yield, Spec, NewState};

execute_opcode(effect_resume, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, effect_resume, effect_resume),
    {continue, wf_vm:exec_set_pc(NewState, PC + 1)};

%% Cancellation
execute_opcode({cancel_scope_enter, ScopeId, ExitPC}, PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, cancel_enter, cancel_scope_enter),
    Frame = wf_vm:frame(cancel, {ScopeId, ExitPC}),
    {continue, wf_vm:exec_set_pc(
        wf_vm:exec_push_frame(NewState, Frame),
        PC + 1
    )};

execute_opcode({cancel_scope_exit, ScopeId}, PC, State) ->
    {Frame, State1} = wf_vm:exec_pop_frame(State),
    Cancel = wf_vm:exec_cancel(State1),
    NewState = wf_vm:exec_add_trace_event(State1, cancel_exit, cancel_scope_exit),
    case maps:get(ScopeId, Cancel, false) of
        true ->
            %% Scope was cancelled, jump to exit PC
            {ScopeId, ExitPC} = wf_vm:frame_data(Frame),
            {continue, wf_vm:exec_set_pc(NewState, ExitPC)};
        false ->
            {continue, wf_vm:exec_set_pc(NewState, PC + 1)}
    end;

%% Multiple instances
execute_opcode({mi_spawn, Policy}, PC, State) ->
    Ctx = wf_vm:exec_ctx(State),
    case wf_mi:spawn_instances(Policy, Ctx) of
        {ok, Count, Contexts} ->
            %% Create MI state and push frame
            MIState = #{
                policy => Policy,
                total => Count,
                completed => 0,
                instances => #{},
                contexts => Contexts
            },
            Frame = wf_vm:frame(mi, MIState),
            %% Initialize join counter for the MI_JOIN ahead
            Joins = wf_vm:exec_joins(State),
            %% Find the MI_JOIN opcode location (scan forward)
            JoinPC = find_mi_join_pc(wf_vm:exec_program(State), PC + 1),
            NewJoins = maps:put(JoinPC, {0, Count}, Joins),
            NewState = wf_vm:exec_add_trace_event(State, mi_spawn, mi_spawn),
            {continue, wf_vm:exec_set_pc(
                wf_vm:exec_push_frame(
                    wf_vm:exec_set_joins(NewState, NewJoins),
                    Frame
                ),
                PC + 1
            )};
        {error, Reason} ->
            NewState = wf_vm:exec_add_trace_event(State, mi_spawn_error, mi_spawn),
            {error, {mi_spawn_failed, Reason}, NewState}
    end;

execute_opcode({mi_join, Policy}, PC, State) ->
    %% Pop MI frame
    {Frame, State1} = wf_vm:exec_pop_frame(State),
    MIState = wf_vm:frame_data(Frame),
    Total = wf_mi:get_instance_count(MIState),
    Completed = wf_mi:get_completed_count(MIState),

    %% Check if join condition is met
    case wf_mi:should_join(Policy, Total, Completed) of
        true ->
            %% All instances completed, collect contexts
            Contexts = maps:get(contexts, MIState),
            MergedCtx = wf_mi:collect_contexts(Contexts),
            Joins = wf_vm:exec_joins(State1),
            NewJoins = wf_vm:join_reset(Joins, PC),
            NewState = wf_vm:exec_add_trace_event(State1, mi_join_done, mi_join),
            {continue, wf_vm:exec_set_pc(
                wf_vm:exec_set_ctx(
                    wf_vm:exec_set_joins(NewState, NewJoins),
                    MergedCtx
                ),
                PC + 1
            )};
        false ->
            %% Not all instances done yet, wait
            NewState = wf_vm:exec_add_trace_event(State1, mi_join_wait, mi_join),
            {continue, wf_vm:exec_push_frame(NewState, Frame)}
    end;

%% Halt
execute_opcode(halt, _PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, halt, halt),
    {halt, ok, NewState};

%% Error
execute_opcode({error, Reason}, _PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, error, error),
    {error, Reason, NewState};

%% Unknown opcode
execute_opcode(Op, _PC, State) ->
    NewState = wf_vm:exec_add_trace_event(State, error, unknown),
    {error, {unknown_opcode, Op}, NewState}.

%%% HELPER FUNCTIONS ========================================================

%% @doc Find the PC of the next MI_JOIN opcode (for join counter setup).
-spec find_mi_join_pc(Program :: [wf_vm:opcode()], StartPC :: non_neg_integer()) ->
    non_neg_integer().
find_mi_join_pc(Program, StartPC) ->
    find_mi_join_pc_loop(Program, StartPC, 0).

-spec find_mi_join_pc_loop(
    Program :: [wf_vm:opcode()],
    CurrentPC :: non_neg_integer(),
    Depth :: non_neg_integer()
) -> non_neg_integer().
find_mi_join_pc_loop([], _PC, _Depth) ->
    %% Not found, return 0 as fallback
    0;
find_mi_join_pc_loop([Op | Rest], PC, Depth) ->
    case wf_vm:opcode_type(Op) of
        mi_join ->
            PC;
        _ ->
            find_mi_join_pc_loop(Rest, PC + 1, Depth)
    end.


