%%%-------------------------------------------------------------------
%%% @doc ln_vm - Virtual machine for executing compiled bytecode.
%%%
%%% The VM executes bytecode produced by ln_compile with step-by-step
%%% semantics, supporting joins, cancellation, effects, and scheduling.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_vm).

%% API
-export([init/1]).
-export([step/1]).
-export([execute_opcode/2]).
-export([is_complete/1]).
-export([get_result/1]).
-export([set_program_counter/2]).

%% Types
-export_type([state/0, opcode/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type pc() :: non_neg_integer().

-type label() :: non_neg_integer().

-type join_id() :: reference().

-type scope_id() :: term().

-type frame_id() :: reference().

-type bindings() :: #{atom() => term()}.

-record(frame, {
    id :: frame_id(),
    parent :: frame_id() | undefined,
    bindings :: bindings(),
    pc :: pc(),
    return_pc :: pc() | undefined,
    scope_id :: scope_id() | undefined
}).

-type frame() :: #frame{}.

-record(join_state, {
    id :: join_id(),
    policy :: term(),
    pending :: non_neg_integer(),
    results :: [term()],
    waiting :: [frame_id()]
}).

-type opcode() :: {op_task_start, atom()}
                | {op_task_complete, term()}
                | {op_fork, [label()], label()}
                | {op_join_wait, join_id(), label()}
                | {op_xor_branch, [label()], label()}
                | {op_xor_choose, reference(), label()}
                | {op_scope_enter, scope_id()}
                | {op_scope_exit, scope_id()}
                | {op_halt, term()}.

-record(vm_state, {
    pc :: pc(),
    frames :: #{frame_id() => frame()},
    current_frame :: frame_id() | undefined,
    stack :: [frame_id()],
    joins :: #{join_id() => #join_state{}},
    scopes :: #{scope_id() => running | cancelling | cancelled},
    scope_parents :: #{scope_id() => scope_id() | undefined},
    result :: term() | undefined,
    status :: running | halted | blocked
}).

-opaque state() :: #vm_state{}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Initialize VM state from bytecode.
-spec init(#{program := [{label(), opcode()}]}) -> state().
init(#{program := _Program}) ->
    #vm_state{
        pc = 0,
        frames = #{},
        current_frame = undefined,
        stack = [],
        joins = #{},
        scopes = #{},
        scope_parents = #{},
        result = undefined,
        status = running
    }.

%% @doc Execute one step of the VM.
-spec step(state()) -> {ok, state()} | {halt, state()} | {error, term()}.
step(#vm_state{status = halted} = State) ->
    {halt, State};
step(#vm_state{status = blocked} = State) ->
    {ok, State};
step(#vm_state{} = State) ->
    case fetch_instruction(State) of
        {ok, {_Label, Opcode}, NewState} ->
            execute_opcode(Opcode, NewState);
        {error, _} = Error ->
            Error
    end.

%% @doc Execute an opcode in the VM state.
-spec execute_opcode(opcode(), state()) -> {ok, state()} | {halt, state()} | {error, term()}.
execute_opcode({op_task_start, TaskId}, #vm_state{pc = PC} = State) ->
    %% Create a new frame for the task
    FrameId = make_ref(),
    Frame = #frame{
        id = FrameId,
        parent = undefined,
        bindings = #{},
        pc = PC,
        return_pc = undefined,
        scope_id = undefined
    },
    NewState = State#vm_state{
        current_frame = FrameId,
        frames = maps:put(FrameId, Frame, State#vm_state.frames),
        stack = [FrameId | State#vm_state.stack]
    },
    {ok, increment_pc(NewState)};

execute_opcode({op_task_complete, _Result}, #vm_state{stack = []} = State) ->
    %% No stack - this is the final result
    {halt, State#vm_state{status = halted}};
execute_opcode({op_task_complete, Result}, #vm_state{stack = [_Current | Rest]} = State) ->
    %% Pop the current frame
    NewState = State#vm_state{
        stack = Rest,
        result = Result
    },
    case Rest of
        [] -> {halt, NewState#vm_state{status = halted}};
        [ParentId | _] ->
            %% Return to parent frame
            {ok, NewState#vm_state{current_frame = ParentId}}
    end;

execute_opcode({op_fork, BranchLabels, JoinLabel}, State) ->
    %% Create frames for each branch and set up join
    JoinId = make_ref(),
    JoinState = #join_state{
        id = JoinId,
        policy = all,
        pending = length(BranchLabels),
        results = [],
        waiting = []
    },
    %% For now, we'll execute branches sequentially in deterministic order
    %% In a full implementation, this would manage parallel execution
    NewState = State#vm_state{
        joins = maps:put(JoinId, JoinState, State#vm_state.joins),
        pc = hd(BranchLabels)
    },
    {ok, NewState};

execute_opcode({op_join_wait, JoinId, _ContinueLabel}, #vm_state{joins = Joins} = State) ->
    case maps:get(JoinId, Joins) of
        #join_state{pending = 0} ->
            %% All branches complete - continue
            {ok, increment_pc(State)};
        #join_state{} ->
            %% Still waiting - block
            {ok, State#vm_state{status = blocked}}
    end;

execute_opcode({op_xor_branch, _BranchLabels, EndLabel}, State) ->
    %% For deterministic execution, always take first branch
    %% In nondeterministic mode, this would record a choice
    {ok, State};

execute_opcode({op_xor_choose, _XorId, EndLabel}, State) ->
    {ok, State#vm_state{pc = EndLabel}};

execute_opcode({op_scope_enter, ScopeId}, State) ->
    NewState = State#vm_state{
        scopes = maps:put(ScopeId, running, State#vm_state.scopes)
    },
    {ok, increment_pc(NewState)};

execute_opcode({op_scope_exit, ScopeId}, #vm_state{scopes = Scopes} = State) ->
    NewState = State#vm_state{
        scopes = maps:remove(ScopeId, Scopes)
    },
    {ok, increment_pc(NewState)};

execute_opcode({op_halt, Reason}, State) ->
    {halt, State#vm_state{result = Reason, status = halted}}.

%% @doc Check if VM execution is complete.
-spec is_complete(state()) -> boolean().
is_complete(#vm_state{status = halted}) ->
    true;
is_complete(#vm_state{pc = PC, stack = []}) when PC =:= 0 ->
    true;
is_complete(_) ->
    false.

%% @doc Get the result from VM execution.
-spec get_result(state()) -> term() | undefined.
get_result(#vm_state{result = Result}) ->
    Result.

%% @doc Set the program counter.
-spec set_program_counter(state(), pc()) -> state().
set_program_counter(#vm_state{} = State, PC) ->
    State#vm_state{pc = PC}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Fetch the current instruction.
fetch_instruction(#vm_state{pc = PC} = State) ->
    %% In a real implementation, this would lookup bytecode
    %% For now, return a placeholder
    {ok, {PC, {op_halt, eof}}, State}.

%% @doc Increment the program counter.
increment_pc(#vm_state{pc = PC} = State) ->
    State#vm_state{pc = PC + 1}.
