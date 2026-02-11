%%% @doc WF Kernel Primitives Test Suite
%%%
%%% Unit tests for each opcode in isolation according to WF_ARCHITECTURE.md.
%%% Tests cover:
%%% - seq, par, xor, join, loop, defer, cancel_scope, mi, task opcodes
%%% - Stack operations
%%% - Join counters
%%% - Cancellation flags
%%% - Trace event generation
%%%
%%% @end
-module(wf_test_primitives).

-include_lib("eunit/include/eunit.hrl").

%%% ==========================================================================
%%% TASK OPCODE TESTS
%%% ==========================================================================

task_enter_test() ->
    %% Test TASK_ENTER opcode: pushes task frame, advances PC
    Fun = fun(_Ctx) -> {ok, #{}} end,
    Program = [
        wf_vm:op_task_enter(test_task, Fun),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    ?assertEqual(1, length(Trace)),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(task_enter, Type),
    ?assertEqual(task_enter, Opcode).

task_call_success_test() ->
    %% Test TASK_CALL with successful result
    Fun = fun(Ctx) -> {ok, maps:put(result, success, Ctx)} end,
    Program = [
        wf_vm:op_task_call(Fun),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{data => initial}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Ctx = wf_vm:exec_ctx(NewState),
    ?assertEqual(success, maps:get(result, Ctx)),
    ?assertEqual(initial, maps:get(data, Ctx)).

task_call_error_test() ->
    %% Test TASK_CALL with error result
    Fun = fun(_Ctx) -> {error, task_failed} end,
    Program = [
        wf_vm:op_task_call(Fun),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {error, task_failed, NewState} = wf_exec:exec_step(State),

    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(task_error, Type).

task_call_effect_yield_test() ->
    %% Test TASK_CALL yielding an effect
    Spec = {effect, http_call, #{url => "http://example.com"}, req1},
    Fun = fun(Ctx) -> {effect, Spec, Ctx} end,
    Program = [
        wf_vm:op_task_call(Fun),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {yield, ReturnedSpec, NewState} = wf_exec:exec_step(State),

    ?assertEqual(Spec, ReturnedSpec),
    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(effect_yield, Type).

task_exit_test() ->
    %% Test TASK_EXIT opcode
    Program = [
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(task_exit, Type),
    ?assertEqual(task_exit, Opcode).

task_full_sequence_test() ->
    %% Test full TASK_ENTER → TASK_CALL → TASK_EXIT sequence
    Fun = fun(Ctx) -> {ok, maps:put(counter, 1, Ctx)} end,
    Program = [
        wf_vm:op_task_enter(my_task, Fun),
        wf_vm:op_task_call(Fun),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    %% Execute all steps
    {FinalState, Steps} = wf_exec:exec_steps(State, 10),

    ?assertEqual(4, Steps),
    ?assert(wf_exec:is_halted(FinalState)),
    Ctx = wf_vm:exec_ctx(FinalState),
    ?assertEqual(1, maps:get(counter, Ctx)).

%%% ==========================================================================
%%% SEQUENCE OPCODE TESTS
%%% ==========================================================================

seq_enter_test() ->
    %% Test SEQ_ENTER: pushes seq frame onto stack
    Program = [
        wf_vm:op_seq_enter(my_seq),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Stack = wf_vm:exec_stack(NewState),
    ?assertEqual(1, length(Stack)),
    [Frame] = Stack,
    ?assertEqual(seq, wf_vm:frame_type(Frame)).

seq_exit_test() ->
    %% Test SEQ_EXIT: pops seq frame from stack
    Frame = wf_vm:frame(seq, {my_seq}),
    Program = [
        wf_vm:op_seq_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [Frame], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Stack = wf_vm:exec_stack(NewState),
    ?assertEqual(0, length(Stack)).

seq_enter_exit_pair_test() ->
    %% Test SEQ_ENTER and SEQ_EXIT work as matching pair
    Program = [
        wf_vm:op_seq_enter(test_seq),
        wf_vm:op_seq_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 10),

    ?assertEqual(3, Steps),
    Stack = wf_vm:exec_stack(FinalState),
    ?assertEqual(0, length(Stack)).

%%% ==========================================================================
%%% PARALLEL OPCODE TESTS
%%% ==========================================================================

par_fork_test() ->
    %% Test PAR_FORK: initializes join counter
    Program = [
        wf_vm:op_par_fork(3),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Joins = wf_vm:exec_joins(NewState),
    %% PAR_FORK(3) at PC=0 sets join counter at PC=3
    {Current, Expected} = wf_vm:join_get(Joins, 3),
    ?assertEqual(0, Current),
    ?assertEqual(3, Expected).

par_join_all_wait_test() ->
    %% Test PAR_JOIN(all): waits if not all branches complete
    Joins = #{5 => {2, 3}},  % 2 out of 3 branches complete
    Program = [
        wf_vm:op_par_join(all),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 5, [], #{}, Joins, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    %% Should not advance PC
    ?assertEqual(5, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(par_join_wait, Type).

par_join_all_proceed_test() ->
    %% Test PAR_JOIN(all): proceeds when all branches complete
    Joins = #{5 => {3, 3}},  % All 3 branches complete
    Program = [
        wf_vm:op_par_join(all),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 5, [], #{}, Joins, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    %% Should advance PC and reset join counter
    ?assertEqual(6, wf_vm:exec_pc(NewState)),
    NewJoins = wf_vm:exec_joins(NewState),
    ?assertEqual({0, 1}, wf_vm:join_get(NewJoins, 5)).

par_join_xor_merge_test() ->
    %% Test PAR_JOIN(xor_merge): takes first, discards others
    Joins = #{10 => {1, 5}},
    Program = [
        wf_vm:op_par_join(xor_merge),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 10, [], #{}, Joins, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    %% Should proceed immediately
    ?assertEqual(11, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(par_join_xor, Type).

par_join_sync_merge_test() ->
    %% Test PAR_JOIN(sync_merge): synchronizing merge
    Program = [
        wf_vm:op_par_join(sync_merge),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(par_join_sync, Type).

par_join_first_n_wait_test() ->
    %% Test PAR_JOIN({first_n, 2}): wait until 2 branches complete
    Joins = #{15 => {1, 5}},  % Only 1 out of 5 complete
    Program = [
        wf_vm:op_par_join({first_n, 2}),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 15, [], #{}, Joins, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    %% Should wait
    ?assertEqual(15, wf_vm:exec_pc(NewState)).

par_join_first_n_proceed_test() ->
    %% Test PAR_JOIN({first_n, 2}): proceed when 2 branches complete
    Joins = #{15 => {2, 5}},  % 2 out of 5 complete
    Program = [
        wf_vm:op_par_join({first_n, 2}),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 15, [], #{}, Joins, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    %% Should proceed
    ?assertEqual(16, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(par_join_first_n, Type).

%%% ==========================================================================
%%% XOR CHOICE OPCODE TESTS
%%% ==========================================================================

xor_choose_test() ->
    %% Test XOR_CHOOSE: selects one branch
    Branches = [branch1, branch2, branch3],
    Program = [
        wf_vm:op_xor_choose(Branches, 0),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(xor_choose, Type),
    ?assertEqual(xor_choose, Opcode).

%%% ==========================================================================
%%% JOIN WAIT OPCODE TESTS
%%% ==========================================================================

join_wait_all_test() ->
    %% Test JOIN_WAIT(all): generalized join with all policy
    Joins = #{8 => {3, 3}},
    Program = [
        wf_vm:op_join_wait(all),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 8, [], #{}, Joins, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(9, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(join_wait_done, Type).

join_wait_other_policy_test() ->
    %% Test JOIN_WAIT with non-all policy
    Program = [
        wf_vm:op_join_wait(sync_merge),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, _, _, _, _, _} = hd(Trace),
    ?assertEqual(join_wait, Type).

%%% ==========================================================================
%%% LOOP OPCODE TESTS
%%% ==========================================================================

loop_back_max_iter_test() ->
    %% Test LOOP_BACK with max_iter condition
    Program = [
        wf_vm:op_loop_back({max_iter, 5}),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(loop_back, Type),
    ?assertEqual(loop_back, Opcode).

loop_back_condition_test() ->
    %% Test LOOP_BACK with arbitrary condition
    Condition = {while, fun(Ctx) -> maps:get(continue, Ctx, false) end},
    Program = [
        wf_vm:op_loop_back(Condition),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)).

%%% ==========================================================================
%%% DEFER OPCODE TESTS
%%% ==========================================================================

defer_race_test() ->
    %% Test DEFER_RACE: deferred choice waiting for signals
    Branches = [signal1, signal2, signal3],
    Program = [
        wf_vm:op_defer_race(Branches),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(defer_race, Type),
    ?assertEqual(defer_race, Opcode).

%%% ==========================================================================
%%% CANCELLATION OPCODE TESTS
%%% ==========================================================================

cancel_scope_enter_test() ->
    %% Test CANCEL_SCOPE_ENTER: pushes cancel frame
    Program = [
        wf_vm:op_cancel_scope_enter(my_scope, 10),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Stack = wf_vm:exec_stack(NewState),
    ?assertEqual(1, length(Stack)),
    [Frame] = Stack,
    ?assertEqual(cancel, wf_vm:frame_type(Frame)),
    {ScopeId, ExitPC} = wf_vm:frame_data(Frame),
    ?assertEqual(my_scope, ScopeId),
    ?assertEqual(10, ExitPC).

cancel_scope_exit_not_cancelled_test() ->
    %% Test CANCEL_SCOPE_EXIT when scope not cancelled
    Frame = wf_vm:frame(cancel, {my_scope, 20}),
    Program = [
        wf_vm:op_cancel_scope_exit(my_scope),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [Frame], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    %% Should advance normally (PC+1)
    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Stack = wf_vm:exec_stack(NewState),
    ?assertEqual(0, length(Stack)).

cancel_scope_exit_cancelled_test() ->
    %% Test CANCEL_SCOPE_EXIT when scope is cancelled
    Frame = wf_vm:frame(cancel, {my_scope, 20}),
    Cancel = #{my_scope => true},
    Program = lists:duplicate(25, wf_vm:op_halt()),
    State = wf_vm:exec_state(Program, 0, [Frame], #{}, #{}, Cancel, []),

    {halt, ok, NewState} = wf_exec:exec_step(State),

    %% Should jump to exit PC (20)
    ?assertEqual(20, wf_vm:exec_pc(NewState)).

cancel_scope_enter_exit_pair_test() ->
    %% Test CANCEL_SCOPE_ENTER and EXIT as a pair
    Program = [
        wf_vm:op_cancel_scope_enter(test_scope, 2),
        wf_vm:op_cancel_scope_exit(test_scope),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 10),

    ?assertEqual(3, Steps),
    Stack = wf_vm:exec_stack(FinalState),
    ?assertEqual(0, length(Stack)).

%%% ==========================================================================
%%% MULTIPLE INSTANCES OPCODE TESTS
%%% ==========================================================================

mi_spawn_fixed_test() ->
    %% Test MI_SPAWN with fixed policy
    Program = [
        wf_vm:op_mi_spawn({fixed, 5}),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(mi_spawn, Type),
    ?assertEqual(mi_spawn, Opcode).

mi_join_test() ->
    %% Test MI_JOIN: waits for all instances to complete
    Policy = {fixed, 3},
    Program = [
        wf_vm:op_mi_join(Policy),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(mi_join, Type),
    ?assertEqual(mi_join, Opcode).

mi_spawn_join_pair_test() ->
    %% Test MI_SPAWN and MI_JOIN as a pair
    Program = [
        wf_vm:op_mi_spawn({fixed, 3}),
        wf_vm:op_mi_join({fixed, 3}),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 10),

    ?assertEqual(3, Steps),
    ?assert(wf_exec:is_halted(FinalState)).

%%% ==========================================================================
%%% EFFECT OPCODE TESTS
%%% ==========================================================================

effect_yield_test() ->
    %% Test EFFECT_YIELD: yields execution
    Spec = {effect, db_query, #{table => users}, query_123},
    Program = [
        wf_vm:op_effect_yield(Spec),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {yield, ReturnedSpec, NewState} = wf_exec:exec_step(State),

    ?assertEqual(Spec, ReturnedSpec),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(effect_yield, Type),
    ?assertEqual(effect_yield, Opcode).

effect_resume_test() ->
    %% Test EFFECT_RESUME: resumes from yield
    Program = [
        wf_vm:op_effect_resume(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, NewState} = wf_exec:exec_step(State),

    ?assertEqual(1, wf_vm:exec_pc(NewState)),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(effect_resume, Type),
    ?assertEqual(effect_resume, Opcode).

%%% ==========================================================================
%%% HALT AND ERROR OPCODE TESTS
%%% ==========================================================================

halt_test() ->
    %% Test HALT: successful termination
    Program = [
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{data => final}, #{}, #{}, []),

    {halt, ok, NewState} = wf_exec:exec_step(State),

    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(halt, Type),
    ?assertEqual(halt, Opcode),
    ?assert(wf_exec:is_halted(NewState)),
    {ok, FinalCtx} = wf_exec:get_result(NewState),
    ?assertEqual(final, maps:get(data, FinalCtx)).

error_test() ->
    %% Test ERROR: error termination
    Reason = {test_error, details},
    Program = [
        wf_vm:op_error(Reason)
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {error, ReturnedReason, NewState} = wf_exec:exec_step(State),

    ?assertEqual(Reason, ReturnedReason),
    Trace = wf_vm:exec_trace(NewState),
    {_, Type, Opcode, _, _, _, _} = hd(Trace),
    ?assertEqual(error, Type),
    ?assertEqual(error, Opcode).

%%% ==========================================================================
%%% STACK OPERATIONS TESTS
%%% ==========================================================================

stack_push_pop_test() ->
    %% Test stack frame push and pop operations
    State = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),

    %% Push a frame
    Frame1 = wf_vm:frame(seq, {seq1}),
    State1 = wf_vm:exec_push_frame(State, Frame1),
    Stack1 = wf_vm:exec_stack(State1),
    ?assertEqual(1, length(Stack1)),

    %% Push another frame
    Frame2 = wf_vm:frame(par, {par1}),
    State2 = wf_vm:exec_push_frame(State1, Frame2),
    Stack2 = wf_vm:exec_stack(State2),
    ?assertEqual(2, length(Stack2)),

    %% Pop frames in LIFO order
    {PoppedFrame2, State3} = wf_vm:exec_pop_frame(State2),
    ?assertEqual(par, wf_vm:frame_type(PoppedFrame2)),
    Stack3 = wf_vm:exec_stack(State3),
    ?assertEqual(1, length(Stack3)),

    {PoppedFrame1, State4} = wf_vm:exec_pop_frame(State3),
    ?assertEqual(seq, wf_vm:frame_type(PoppedFrame1)),
    Stack4 = wf_vm:exec_stack(State4),
    ?assertEqual(0, length(Stack4)).

stack_underflow_test() ->
    %% Test stack underflow detection
    State = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),
    ?assertError(stack_underflow, wf_vm:exec_pop_frame(State)).

stack_frame_types_test() ->
    %% Test all frame types can be created
    Frames = [
        wf_vm:frame(seq, {seq_data}),
        wf_vm:frame(par, {par_data}),
        wf_vm:frame(choice, {choice_data}),
        wf_vm:frame(join, {join_data}),
        wf_vm:frame(loop, {loop_data}),
        wf_vm:frame(defer, {defer_data}),
        wf_vm:frame(cancel, {cancel_data}),
        wf_vm:frame(mi, {mi_data}),
        wf_vm:frame(task, {task_data}),
        wf_vm:frame(effect, {effect_data})
    ],

    Types = [wf_vm:frame_type(F) || F <- Frames],
    ?assertEqual([seq, par, choice, join, loop, defer, cancel, mi, task, effect], Types).

%%% ==========================================================================
%%% JOIN COUNTER TESTS
%%% ==========================================================================

join_counter_increment_test() ->
    %% Test join counter increment
    Joins = #{},

    %% First increment at PC=10
    Joins1 = wf_vm:join_increment(Joins, 10),
    {Current1, Expected1} = wf_vm:join_get(Joins1, 10),
    ?assertEqual(1, Current1),
    ?assertEqual(1, Expected1),

    %% Second increment
    Joins2 = wf_vm:join_increment(Joins1, 10),
    {Current2, Expected2} = wf_vm:join_get(Joins2, 10),
    ?assertEqual(2, Current2),
    ?assertEqual(1, Expected2).

join_counter_get_default_test() ->
    %% Test getting non-existent counter returns default
    Joins = #{},
    {Current, Expected} = wf_vm:join_get(Joins, 999),
    ?assertEqual(0, Current),
    ?assertEqual(1, Expected).

join_counter_reset_test() ->
    %% Test join counter reset
    Joins = #{15 => {5, 5}},

    Joins1 = wf_vm:join_reset(Joins, 15),
    {Current, Expected} = wf_vm:join_get(Joins1, 15),
    ?assertEqual(0, Current),
    ?assertEqual(1, Expected).

join_counter_multiple_pcs_test() ->
    %% Test multiple join points tracked independently
    Joins = #{},
    Joins1 = wf_vm:join_increment(Joins, 10),
    Joins2 = wf_vm:join_increment(Joins1, 20),
    Joins3 = wf_vm:join_increment(Joins2, 10),

    {Current10, _} = wf_vm:join_get(Joins3, 10),
    {Current20, _} = wf_vm:join_get(Joins3, 20),
    ?assertEqual(2, Current10),
    ?assertEqual(1, Current20).

%%% ==========================================================================
%%% CANCELLATION FLAGS TESTS
%%% ==========================================================================

cancel_flag_set_test() ->
    %% Test setting cancellation flags
    Cancel = #{},
    Cancel1 = maps:put(scope1, true, Cancel),

    ?assertEqual(true, maps:get(scope1, Cancel1)),
    ?assertEqual(false, maps:get(scope2, Cancel1, false)).

cancel_flag_check_test() ->
    %% Test cancellation flag checking in exec state
    Cancel = #{active_scope => true},
    State = wf_vm:exec_state([], 0, [], #{}, #{}, Cancel, []),

    CancelFlags = wf_vm:exec_cancel(State),
    ?assertEqual(true, maps:get(active_scope, CancelFlags)),
    ?assertEqual(false, maps:get(inactive_scope, CancelFlags, false)).

cancel_flag_multiple_scopes_test() ->
    %% Test multiple cancellation scopes
    Cancel = #{
        scope_a => true,
        scope_b => false,
        scope_c => true
    },

    ?assertEqual(true, maps:get(scope_a, Cancel)),
    ?assertEqual(false, maps:get(scope_b, Cancel)),
    ?assertEqual(true, maps:get(scope_c, Cancel)).

%%% ==========================================================================
%%% TRACE EVENT TESTS
%%% ==========================================================================

trace_event_generation_test() ->
    %% Test trace events are generated for each step
    Fun = fun(Ctx) -> {ok, Ctx} end,
    Program = [
        wf_vm:op_task_enter(task1, Fun),
        wf_vm:op_task_call(Fun),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 10),

    Trace = wf_vm:exec_trace(FinalState),
    ?assertEqual(4, length(Trace)),

    %% Check trace event sequence numbers
    Seqs = [Seq || {Seq, _, _, _, _, _, _} <- Trace],
    ?assertEqual([0, 1, 2, 3], Seqs).

trace_event_structure_test() ->
    %% Test trace event structure
    Program = [wf_vm:op_halt()],
    State = wf_vm:exec_state(Program, 0, [], #{data => test}, #{}, #{}, []),

    {halt, ok, NewState} = wf_exec:exec_step(State),

    Trace = wf_vm:exec_trace(NewState),
    ?assertEqual(1, length(Trace)),
    [{Seq, Type, Opcode, Ctx, Timestamp, Scope, CancelSignal}] = Trace,

    ?assertEqual(0, Seq),
    ?assertEqual(halt, Type),
    ?assertEqual(halt, Opcode),
    ?assertEqual(test, maps:get(data, Ctx)),
    ?assert(is_integer(Timestamp)),
    ?assert(is_list(Scope)),
    ?assert(is_boolean(CancelSignal)).

trace_event_scope_tracking_test() ->
    %% Test trace events track scope nesting
    Program = [
        wf_vm:op_seq_enter(outer),
        wf_vm:op_seq_enter(inner),
        wf_vm:op_seq_exit(),
        wf_vm:op_seq_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, _} = wf_exec:exec_steps(State, 10),

    Trace = wf_vm:exec_trace(FinalState),
    ?assertEqual(5, length(Trace)),

    %% Extract scope depth at each step
    ScopeDepths = [length(Scope) || {_, _, _, _, _, Scope, _} <- Trace],
    %% seq_enter: 0->1, seq_enter: 1->2, seq_exit: 2->1, seq_exit: 1->0, halt: 0
    ?assertEqual([0, 1, 2, 1, 0], ScopeDepths).

%%% ==========================================================================
%%% INTEGRATION TESTS
%%% ==========================================================================

sequence_of_tasks_test() ->
    %% Test executing a sequence of tasks
    Task1 = fun(Ctx) -> {ok, maps:put(step, 1, Ctx)} end,
    Task2 = fun(Ctx) -> {ok, maps:put(step, 2, Ctx)} end,
    Task3 = fun(Ctx) -> {ok, maps:put(step, 3, Ctx)} end,

    Program = [
        wf_vm:op_task_enter(t1, Task1),
        wf_vm:op_task_call(Task1),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(t2, Task2),
        wf_vm:op_task_call(Task2),
        wf_vm:op_task_exit(),
        wf_vm:op_task_enter(t3, Task3),
        wf_vm:op_task_call(Task3),
        wf_vm:op_task_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 20),

    ?assertEqual(10, Steps),
    ?assert(wf_exec:is_halted(FinalState)),
    Ctx = wf_vm:exec_ctx(FinalState),
    ?assertEqual(3, maps:get(step, Ctx)).

nested_sequences_test() ->
    %% Test nested sequence scopes
    Program = [
        wf_vm:op_seq_enter(outer),
        wf_vm:op_seq_enter(middle),
        wf_vm:op_seq_enter(inner),
        wf_vm:op_seq_exit(),
        wf_vm:op_seq_exit(),
        wf_vm:op_seq_exit(),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 20),

    ?assertEqual(7, Steps),
    Stack = wf_vm:exec_stack(FinalState),
    ?assertEqual(0, length(Stack)).

parallel_fork_join_test() ->
    %% Test parallel fork followed by join
    Program = [
        wf_vm:op_par_fork(3),
        wf_vm:op_par_join(all),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    %% After fork, join counter should be set
    {continue, State1} = wf_exec:exec_step(State),
    Joins1 = wf_vm:exec_joins(State1),
    {Current, Expected} = wf_vm:join_get(Joins1, 1),
    ?assertEqual(0, Current),
    ?assertEqual(3, Expected),

    %% Manually set join counter to simulate branch completion
    Joins2 = maps:put(1, {3, 3}, Joins1),
    State2 = wf_vm:exec_set_joins(State1, Joins2),

    %% Join should now proceed
    {continue, State3} = wf_exec:exec_step(State2),
    ?assertEqual(2, wf_vm:exec_pc(State3)).

cancel_scope_with_task_test() ->
    %% Test cancellation scope around a task
    Fun = fun(Ctx) -> {ok, maps:put(executed, true, Ctx)} end,
    Program = [
        wf_vm:op_cancel_scope_enter(test_scope, 5),
        wf_vm:op_task_enter(t1, Fun),
        wf_vm:op_task_call(Fun),
        wf_vm:op_task_exit(),
        wf_vm:op_cancel_scope_exit(test_scope),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, Steps} = wf_exec:exec_steps(State, 20),

    ?assertEqual(6, Steps),
    ?assert(wf_exec:is_halted(FinalState)),
    Ctx = wf_vm:exec_ctx(FinalState),
    ?assertEqual(true, maps:get(executed, Ctx)).

multiple_join_points_test() ->
    %% Test multiple independent join points
    Program = [
        wf_vm:op_par_fork(2),     % PC=0, join at PC=1
        wf_vm:op_par_join(all),   % PC=1
        wf_vm:op_par_fork(3),     % PC=2, join at PC=3
        wf_vm:op_par_join(all),   % PC=3
        wf_vm:op_halt()           % PC=4
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    %% Execute first fork
    {continue, State1} = wf_exec:exec_step(State),
    Joins1 = wf_vm:exec_joins(State1),
    ?assertMatch({0, 2}, wf_vm:join_get(Joins1, 1)),

    %% Simulate first join completion
    Joins2 = maps:put(1, {2, 2}, Joins1),
    State2 = wf_vm:exec_set_joins(wf_vm:exec_set_pc(State1, 1), Joins2),

    {continue, State3} = wf_exec:exec_step(State2),
    ?assertEqual(2, wf_vm:exec_pc(State3)),

    %% Execute second fork
    {continue, State4} = wf_exec:exec_step(State3),
    Joins4 = wf_vm:exec_joins(State4),
    ?assertMatch({0, 3}, wf_vm:join_get(Joins4, 3)).

context_propagation_test() ->
    %% Test context propagates through execution
    Task1 = fun(Ctx) -> {ok, maps:put(a, 1, Ctx)} end,
    Task2 = fun(Ctx) -> {ok, maps:put(b, 2, Ctx)} end,
    Task3 = fun(Ctx) ->
        A = maps:get(a, Ctx),
        B = maps:get(b, Ctx),
        {ok, maps:put(sum, A + B, Ctx)}
    end,

    Program = [
        wf_vm:op_task_call(Task1),
        wf_vm:op_task_call(Task2),
        wf_vm:op_task_call(Task3),
        wf_vm:op_halt()
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {FinalState, _} = wf_exec:exec_steps(State, 10),

    Ctx = wf_vm:exec_ctx(FinalState),
    ?assertEqual(1, maps:get(a, Ctx)),
    ?assertEqual(2, maps:get(b, Ctx)),
    ?assertEqual(3, maps:get(sum, Ctx)).

pc_progression_test() ->
    %% Test PC advances correctly through various opcodes
    Program = [
        wf_vm:op_seq_enter(s1),    % PC=0 -> PC=1
        wf_vm:op_seq_exit(),        % PC=1 -> PC=2
        wf_vm:op_par_fork(2),       % PC=2 -> PC=3
        wf_vm:op_halt()             % PC=3
    ],
    State = wf_vm:exec_state(Program, 0, [], #{}, #{}, #{}, []),

    {continue, State1} = wf_exec:exec_step(State),
    ?assertEqual(1, wf_vm:exec_pc(State1)),

    {continue, State2} = wf_exec:exec_step(State1),
    ?assertEqual(2, wf_vm:exec_pc(State2)),

    {continue, State3} = wf_exec:exec_step(State2),
    ?assertEqual(3, wf_vm:exec_pc(State3)),

    {halt, ok, State4} = wf_exec:exec_step(State3),
    ?assertEqual(3, wf_vm:exec_pc(State4)).
