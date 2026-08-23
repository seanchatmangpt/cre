%%% @doc WF Bytecode VM - Opcode Definitions and Data Structures
%%%
%%% This module defines the bytecode instruction set and core data structures
%%% for the WF Substrate VM. The VM executes bytecode produced by wf_compile
%%% in a tight reduction loop (wf_exec).
%%%
%%% @end
-module(wf_vm).

%% Opcode type and accessors
-export([
    %% Opcode constructors
    op_seq_enter/1,
    op_seq_exit/0,
    op_par_fork/1,
    op_par_join/1,
    op_xor_choose/2,
    op_join_wait/1,
    op_loop_back/1,
    op_defer_race/1,
    op_task_enter/2,
    op_task_call/1,
    op_task_exit/0,
    op_effect_yield/1,
    op_effect_resume/0,
    op_cancel_scope_enter/2,
    op_cancel_scope_exit/1,
    op_mi_spawn/1,
    op_mi_join/1,
    op_halt/0,
    op_error/1,

    %% Opcode type checking
    is_opcode/1,
    opcode_type/1,
    opcode_arity/1,

    %% Exec state constructors and accessors
    exec_state/7,
    exec_program/1,
    exec_pc/1,
    exec_stack/1,
    exec_ctx/1,
    exec_joins/1,
    exec_cancel/1,
    exec_trace/1,
    exec_set_pc/2,
    exec_set_stack/2,
    exec_set_ctx/2,
    exec_set_joins/2,
    exec_set_cancel/2,
    exec_set_trace/2,
    exec_add_trace_event/3,
    exec_push_frame/2,
    exec_pop_frame/1,

    %% Stack frame constructors and accessors
    frame/2,
    frame_type/1,
    frame_data/1,

    %% Join counter operations
    join_increment/2,
    join_get/2,
    join_reset/2,

    %% Tracing
    trace_event/5
]).

-export_type([
    opcode/0,
    exec_state/0,
    stack_frame/0,
    join_counters/0,
    cancel_flags/0,
    trace_log/0,
    trace_event/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% Bytecode opcode type
-type opcode() ::
      {seq_enter, atom()}                           % SEQ_ENTER(name)
    | seq_exit                                      % SEQ_EXIT
    | {par_fork, non_neg_integer()}                 % PAR_FORK(N)
    | {par_join, join_policy()}                     % PAR_JOIN(policy)
    | {xor_choose, [any()], non_neg_integer()}     % XOR_CHOOSE(branches, default_idx)
    | {join_wait, join_policy()}                    % JOIN_WAIT(policy)
    | {loop_back, any()}                            % LOOP_BACK(condition)
    | {defer_race, [any()]}                         % DEFER_RACE(branches)
    | {task_enter, atom(), fun()}                   % TASK_ENTER(name, fun)
    | {task_call, fun()}                            % TASK_CALL(fun)
    | task_exit                                     % TASK_EXIT
    | {effect_yield, term()}                        % EFFECT_YIELD(spec)
    | effect_resume                                 % EFFECT_RESUME
    | {cancel_scope_enter, atom(), non_neg_integer()}  % CANCEL_SCOPE_ENTER(id, exit_pc)
    | {cancel_scope_exit, atom()}                   % CANCEL_SCOPE_EXIT(id)
    | {mi_spawn, any()}                             % MI_SPAWN(policy)
    | {mi_join, any()}                              % MI_JOIN(policy)
    | halt                                          % HALT
    | {error, term()}.                              % ERROR(reason)

%% Join policy type (must match wf_term)
-type join_policy() ::
      all
    | xor_merge
    | sync_merge
    | {first_n, non_neg_integer()}
    | {n_of_m, non_neg_integer(), non_neg_integer()}.

%% Execution state (main VM state)
-type exec_state() :: {
    [opcode()],              % Bytecode program
    non_neg_integer(),       % Program counter
    [stack_frame()],         % Scope/frame stack
    context(),               % User context
    join_counters(),         % Join counters per join point
    cancel_flags(),          % Cancellation flags per scope
    trace_log()              % Trace events (for observability & replay)
}.

%% Stack frame (execution context for scopes)
-type stack_frame() :: {
    frame_type(),            % Type of frame
    frame_id(),              % Identifier for this frame
    frame_data()             % Payload (depends on frame type)
}.

-type frame_type() ::
      seq | par | choice | join | loop | defer
    | cancel_scope | mi | task | effect.

-type frame_id() :: atom() | {atom(), non_neg_integer()}.

-type frame_data() :: any().  % Frame-specific data

%% User context
-type context() :: #{
    data => any(),
    signals => [term()],
    results => #{atom() => any()},
    token_data => #{atom() => any()}
}.

%% Join counters: maps join point PC to {Current, Expected}
-type join_counters() :: #{non_neg_integer() => {non_neg_integer(), non_neg_integer()}}.

%% Cancellation flags: maps scope ID to boolean (true = cancelled)
-type cancel_flags() :: #{atom() => boolean()}.

%% Trace log: sequence of events
-type trace_log() :: [trace_event()].

-type trace_event() :: {
    non_neg_integer(),       % seq
    atom(),                  % type
    atom(),                  % opcode
    context(),               % ctx
    non_neg_integer(),       % timestamp
    [atom()],                % scope
    boolean()                % cancel_signal
}.

%%% OPCODE CONSTRUCTORS =====================================================

%% @doc Create a SEQ_ENTER opcode.
-spec op_seq_enter(Name :: atom()) -> opcode().
op_seq_enter(Name) when is_atom(Name) ->
    {seq_enter, Name}.

%% @doc Create a SEQ_EXIT opcode.
-spec op_seq_exit() -> opcode().
op_seq_exit() ->
    seq_exit.

%% @doc Create a PAR_FORK opcode.
-spec op_par_fork(N :: non_neg_integer()) -> opcode().
op_par_fork(N) when is_integer(N), N > 0 ->
    {par_fork, N}.

%% @doc Create a PAR_JOIN opcode.
-spec op_par_join(Policy :: join_policy()) -> opcode().
op_par_join(Policy) ->
    {par_join, Policy}.

%% @doc Create an XOR_CHOOSE opcode.
-spec op_xor_choose(Branches :: [any()], DefaultIdx :: non_neg_integer()) -> opcode().
op_xor_choose(Branches, DefaultIdx) when is_list(Branches), is_integer(DefaultIdx) ->
    {xor_choose, Branches, DefaultIdx}.

%% @doc Create a JOIN_WAIT opcode.
-spec op_join_wait(Policy :: join_policy()) -> opcode().
op_join_wait(Policy) ->
    {join_wait, Policy}.

%% @doc Create a LOOP_BACK opcode.
-spec op_loop_back(Condition :: any()) -> opcode().
op_loop_back(Condition) ->
    {loop_back, Condition}.

%% @doc Create a DEFER_RACE opcode.
-spec op_defer_race(Branches :: [any()]) -> opcode().
op_defer_race(Branches) when is_list(Branches) ->
    {defer_race, Branches}.

%% @doc Create a TASK_ENTER opcode.
-spec op_task_enter(Name :: atom(), Fun :: fun()) -> opcode().
op_task_enter(Name, Fun) when is_atom(Name), is_function(Fun) ->
    {task_enter, Name, Fun}.

%% @doc Create a TASK_CALL opcode.
-spec op_task_call(Fun :: fun()) -> opcode().
op_task_call(Fun) when is_function(Fun) ->
    {task_call, Fun}.

%% @doc Create a TASK_EXIT opcode.
-spec op_task_exit() -> opcode().
op_task_exit() ->
    task_exit.

%% @doc Create an EFFECT_YIELD opcode.
-spec op_effect_yield(Spec :: term()) -> opcode().
op_effect_yield(Spec) ->
    {effect_yield, Spec}.

%% @doc Create an EFFECT_RESUME opcode.
-spec op_effect_resume() -> opcode().
op_effect_resume() ->
    effect_resume.

%% @doc Create a CANCEL_SCOPE_ENTER opcode.
-spec op_cancel_scope_enter(ScopeId :: atom(), ExitPC :: non_neg_integer()) -> opcode().
op_cancel_scope_enter(ScopeId, ExitPC) when is_atom(ScopeId), is_integer(ExitPC) ->
    {cancel_scope_enter, ScopeId, ExitPC}.

%% @doc Create a CANCEL_SCOPE_EXIT opcode.
-spec op_cancel_scope_exit(ScopeId :: atom()) -> opcode().
op_cancel_scope_exit(ScopeId) when is_atom(ScopeId) ->
    {cancel_scope_exit, ScopeId}.

%% @doc Create an MI_SPAWN opcode.
-spec op_mi_spawn(Policy :: any()) -> opcode().
op_mi_spawn(Policy) ->
    {mi_spawn, Policy}.

%% @doc Create an MI_JOIN opcode.
-spec op_mi_join(Policy :: any()) -> opcode().
op_mi_join(Policy) ->
    {mi_join, Policy}.

%% @doc Create a HALT opcode (successful termination).
-spec op_halt() -> opcode().
op_halt() ->
    halt.

%% @doc Create an ERROR opcode (error termination).
-spec op_error(Reason :: term()) -> opcode().
op_error(Reason) ->
    {error, Reason}.

%%% OPCODE INTROSPECTION ====================================================

%% @doc Check if a term is a valid opcode.
-spec is_opcode(Term :: term()) -> boolean().
is_opcode({seq_enter, Name}) -> is_atom(Name);
is_opcode(seq_exit) -> true;
is_opcode({par_fork, N}) -> is_integer(N), N > 0;
is_opcode({par_join, Policy}) -> is_join_policy(Policy);
is_opcode({xor_choose, Branches, DefaultIdx}) ->
    is_list(Branches), is_integer(DefaultIdx), DefaultIdx >= 0;
is_opcode({join_wait, Policy}) -> is_join_policy(Policy);
is_opcode({loop_back, _Condition}) -> true;
is_opcode({defer_race, Branches}) -> is_list(Branches);
is_opcode({task_enter, Name, Fun}) -> is_atom(Name), is_function(Fun);
is_opcode({task_call, Fun}) -> is_function(Fun);
is_opcode(task_exit) -> true;
is_opcode({effect_yield, _Spec}) -> true;
is_opcode(effect_resume) -> true;
is_opcode({cancel_scope_enter, ScopeId, ExitPC}) ->
    is_atom(ScopeId), is_integer(ExitPC), ExitPC >= 0;
is_opcode({cancel_scope_exit, ScopeId}) -> is_atom(ScopeId);
is_opcode({mi_spawn, _Policy}) -> true;
is_opcode({mi_join, _Policy}) -> true;
is_opcode(halt) -> true;
is_opcode({error, _Reason}) -> true;
is_opcode(_) -> false.

%% @doc Get the type atom of an opcode.
-spec opcode_type(Opcode :: opcode()) -> atom().
opcode_type({seq_enter, _}) -> seq_enter;
opcode_type(seq_exit) -> seq_exit;
opcode_type({par_fork, _}) -> par_fork;
opcode_type({par_join, _}) -> par_join;
opcode_type({xor_choose, _, _}) -> xor_choose;
opcode_type({join_wait, _}) -> join_wait;
opcode_type({loop_back, _}) -> loop_back;
opcode_type({defer_race, _}) -> defer_race;
opcode_type({task_enter, _, _}) -> task_enter;
opcode_type({task_call, _}) -> task_call;
opcode_type(task_exit) -> task_exit;
opcode_type({effect_yield, _}) -> effect_yield;
opcode_type(effect_resume) -> effect_resume;
opcode_type({cancel_scope_enter, _, _}) -> cancel_scope_enter;
opcode_type({cancel_scope_exit, _}) -> cancel_scope_exit;
opcode_type({mi_spawn, _}) -> mi_spawn;
opcode_type({mi_join, _}) -> mi_join;
opcode_type(halt) -> halt;
opcode_type({error, _}) -> error.

%% @doc Get the arity of an opcode (for pattern matching).
-spec opcode_arity(Opcode :: opcode()) -> non_neg_integer().
opcode_arity({seq_enter, _}) -> 1;
opcode_arity(seq_exit) -> 0;
opcode_arity({par_fork, _}) -> 1;
opcode_arity({par_join, _}) -> 1;
opcode_arity({xor_choose, _, _}) -> 2;
opcode_arity({join_wait, _}) -> 1;
opcode_arity({loop_back, _}) -> 1;
opcode_arity({defer_race, _}) -> 1;
opcode_arity({task_enter, _, _}) -> 2;
opcode_arity({task_call, _}) -> 1;
opcode_arity(task_exit) -> 0;
opcode_arity({effect_yield, _}) -> 1;
opcode_arity(effect_resume) -> 0;
opcode_arity({cancel_scope_enter, _, _}) -> 2;
opcode_arity({cancel_scope_exit, _}) -> 1;
opcode_arity({mi_spawn, _}) -> 1;
opcode_arity({mi_join, _}) -> 1;
opcode_arity(halt) -> 0;
opcode_arity({error, _}) -> 1.

%%% EXEC STATE OPERATIONS ===================================================

%% @doc Create a new exec_state.
-spec exec_state(
    Program :: [opcode()],
    PC :: non_neg_integer(),
    Stack :: [stack_frame()],
    Ctx :: context(),
    Joins :: join_counters(),
    Cancel :: cancel_flags(),
    Trace :: trace_log()
) -> exec_state().
exec_state(Program, PC, Stack, Ctx, Joins, Cancel, Trace) ->
    {Program, PC, Stack, Ctx, Joins, Cancel, Trace}.

-spec exec_program(State :: exec_state()) -> [opcode()].
exec_program({Program, _, _, _, _, _, _}) -> Program.

-spec exec_pc(State :: exec_state()) -> non_neg_integer().
exec_pc({_, PC, _, _, _, _, _}) -> PC.

-spec exec_stack(State :: exec_state()) -> [stack_frame()].
exec_stack({_, _, Stack, _, _, _, _}) -> Stack.

-spec exec_ctx(State :: exec_state()) -> context().
exec_ctx({_, _, _, Ctx, _, _, _}) -> Ctx.

-spec exec_joins(State :: exec_state()) -> join_counters().
exec_joins({_, _, _, _, Joins, _, _}) -> Joins.

-spec exec_cancel(State :: exec_state()) -> cancel_flags().
exec_cancel({_, _, _, _, _, Cancel, _}) -> Cancel.

-spec exec_trace(State :: exec_state()) -> trace_log().
exec_trace({_, _, _, _, _, _, Trace}) -> Trace.

-spec exec_set_pc(State :: exec_state(), NewPC :: non_neg_integer()) -> exec_state().
exec_set_pc({P, _, S, C, J, Ca, T}, NewPC) ->
    {P, NewPC, S, C, J, Ca, T}.

-spec exec_set_stack(State :: exec_state(), NewStack :: [stack_frame()]) -> exec_state().
exec_set_stack({P, PC, _, C, J, Ca, T}, NewStack) ->
    {P, PC, NewStack, C, J, Ca, T}.

-spec exec_set_ctx(State :: exec_state(), NewCtx :: context()) -> exec_state().
exec_set_ctx({P, PC, S, _, J, Ca, T}, NewCtx) ->
    {P, PC, S, NewCtx, J, Ca, T}.

-spec exec_set_joins(State :: exec_state(), NewJoins :: join_counters()) -> exec_state().
exec_set_joins({P, PC, S, C, _, Ca, T}, NewJoins) ->
    {P, PC, S, C, NewJoins, Ca, T}.

-spec exec_set_cancel(State :: exec_state(), NewCancel :: cancel_flags()) -> exec_state().
exec_set_cancel({P, PC, S, C, J, _, T}, NewCancel) ->
    {P, PC, S, C, J, NewCancel, T}.

-spec exec_set_trace(State :: exec_state(), NewTrace :: trace_log()) -> exec_state().
exec_set_trace({P, PC, S, C, J, Ca, _}, NewTrace) ->
    {P, PC, S, C, J, Ca, NewTrace}.

%% @doc Add a trace event to the execution state.
-spec exec_add_trace_event(
    State :: exec_state(),
    EventType :: atom(),
    Opcode :: atom()
) -> exec_state().
exec_add_trace_event(State, EventType, Opcode) ->
    Trace = exec_trace(State),
    Seq = length(Trace),
    Ctx = exec_ctx(State),
    Stack = exec_stack(State),
    Scope = [frame_id(F) || F <- Stack],
    Cancel = exec_cancel(State),
    CancelSignal = is_cancelled(Scope, Cancel),
    Event = trace_event(Seq, EventType, Opcode, Ctx, Scope),
    NewTrace = Trace ++ [Event],
    exec_set_trace(State, NewTrace).

%% @doc Push a frame onto the execution stack.
-spec exec_push_frame(State :: exec_state(), Frame :: stack_frame()) -> exec_state().
exec_push_frame(State, Frame) ->
    Stack = exec_stack(State),
    exec_set_stack(State, [Frame | Stack]).

%% @doc Pop a frame from the execution stack.
-spec exec_pop_frame(State :: exec_state()) -> {stack_frame(), exec_state()}.
exec_pop_frame(State) ->
    Stack = exec_stack(State),
    case Stack of
        [Frame | Rest] ->
            {Frame, exec_set_stack(State, Rest)};
        [] ->
            error(stack_underflow)
    end.

%%% STACK FRAME OPERATIONS ==================================================

%% @doc Create a stack frame.
-spec frame(Type :: frame_type(), Data :: frame_data()) -> stack_frame().
frame(Type, Data) when is_atom(Type) ->
    {Type, make_frame_id(Type), Data}.

%% @doc Get the type of a frame.
-spec frame_type(Frame :: stack_frame()) -> frame_type().
frame_type({Type, _, _}) -> Type.

%% @doc Get the ID of a frame.
-spec frame_id(Frame :: stack_frame()) -> atom() | {atom(), non_neg_integer()}.
frame_id({_, Id, _}) -> Id.

%% @doc Get the data of a frame.
-spec frame_data(Frame :: stack_frame()) -> frame_data().
frame_data({_, _, Data}) -> Data.

-spec make_frame_id(Type :: frame_type()) -> atom() | {atom(), non_neg_integer()}.
make_frame_id(Type) when is_atom(Type) ->
    {Type, erlang:unique_integer([positive])}.

%%% JOIN COUNTER OPERATIONS =================================================

%% @doc Increment the join counter at a specific PC.
-spec join_increment(Joins :: join_counters(), PC :: non_neg_integer()) -> join_counters().
join_increment(Joins, PC) ->
    case maps:get(PC, Joins, {0, 1}) of
        {Current, Expected} ->
            maps:put(PC, {Current + 1, Expected}, Joins)
    end.

%% @doc Get the current count and expected count at a PC.
-spec join_get(Joins :: join_counters(), PC :: non_neg_integer()) -> {non_neg_integer(), non_neg_integer()}.
join_get(Joins, PC) ->
    maps:get(PC, Joins, {0, 1}).

%% @doc Reset the join counter at a PC.
-spec join_reset(Joins :: join_counters(), PC :: non_neg_integer()) -> join_counters().
join_reset(Joins, PC) ->
    maps:remove(PC, Joins).

%%% TRACE EVENT CONSTRUCTION ================================================

%% @doc Create a trace event.
-spec trace_event(
    Seq :: non_neg_integer(),
    Type :: atom(),
    Opcode :: atom(),
    Ctx :: context(),
    Scope :: [atom()]
) -> trace_event().
trace_event(Seq, Type, Opcode, Ctx, Scope) ->
    {
        Seq,
        Type,
        Opcode,
        Ctx,
        erlang:monotonic_time(microsecond),
        Scope,
        false
    }.

%%% HELPER FUNCTIONS ========================================================

-spec is_join_policy(Policy :: term()) -> boolean().
is_join_policy(all) -> true;
is_join_policy(xor_merge) -> true;
is_join_policy(sync_merge) -> true;
is_join_policy({first_n, N}) -> is_integer(N), N >= 1;
is_join_policy({n_of_m, N, M}) -> is_integer(N), is_integer(M), N >= 1, M >= N;
is_join_policy(_) -> false.

-spec is_cancelled(Scope :: [atom()], Cancel :: cancel_flags()) -> boolean().
is_cancelled([], _Cancel) -> false;
is_cancelled([ScopeId | Rest], Cancel) ->
    case maps:get(ScopeId, Cancel, false) of
        true -> true;
        false -> is_cancelled(Rest, Cancel)
    end.

%%% TESTS ===================================================================

opcode_construction_test_() ->
    [
        ?_assert(is_opcode(op_seq_enter(test))),
        ?_assert(is_opcode(op_seq_exit())),
        ?_assert(is_opcode(op_par_fork(2))),
        ?_assert(is_opcode(op_join_wait(all))),
        ?_assert(is_opcode(op_task_enter(my_task, fun(_) -> {ok, #{}} end))),
        ?_assert(is_opcode(op_halt())),
        ?_assert(is_opcode(op_error(reason)))
    ].

exec_state_operations_test_() ->
    State = exec_state([], 0, [], #{}, #{}, #{}, []),
    [
        ?_assertEqual(0, exec_pc(State)),
        ?_assertEqual([], exec_stack(State)),
        ?_assertEqual(#{}, exec_ctx(State)),
        ?_assertMatch({_, _, _, _, _, _, _}, exec_set_pc(State, 5))
    ].

join_counter_test_() ->
    Joins = #{},
    Joins1 = join_increment(Joins, 10),
    [
        ?_assertEqual({1, 1}, join_get(Joins1, 10)),
        ?_assertEqual({2, 1}, join_get(join_increment(Joins1, 10), 10))
    ].

