%%% @doc WF Pattern Compiler - Translates AST to Bytecode
%%%
%%% This module compiles workflow patterns (wf_term AST) to bytecode
%%% that can be executed by wf_exec. The compiler performs a single pass
%%% that generates a flat bytecode sequence with proper jump targets
%%% for control flow structures.
%%%
%%% @end
-module(wf_compile).

-export([
    compile/1,
    compile/2,
    is_compiled/1,
    program_size/1,
    to_string/1
]).

-export_type([
    compiled/0,
    compile_options/0
]).

%%% TYPES ===================================================================

%% Compiled pattern: bytecode + metadata
-type compiled() :: {
    program,
    [wf_vm:opcode()],        % Bytecode instructions
    non_neg_integer(),        % Entry point (always 0 initially)
    non_neg_integer(),        % Expected exit PC (for validation)
    non_neg_integer(),        % Number of instructions
    #{atom() => any()}        % Stats, scope info, etc.
}.

-type compile_options() :: #{
    optimize => boolean(),
    trace_level => none | basic | full,
    validate => boolean()
}.

%%% API =====================================================================

%% @doc Compile a workflow pattern to bytecode.
-spec compile(Pattern :: wf_term:wf_term()) -> {ok, compiled()} | {error, term()}.
compile(Pattern) ->
    compile(Pattern, #{}).

%% @doc Compile a workflow pattern with options.
-spec compile(
    Pattern :: wf_term:wf_term(),
    Options :: compile_options()
) -> {ok, compiled()} | {error, term()}.
compile(Pattern, Options) ->
    case wf_term:is_valid(Pattern) of
        false ->
            {error, {invalid_pattern, Pattern}};
        true ->
            try
                Compiled = do_compile(Pattern, Options),
                {ok, Compiled}
            catch
                error:Reason ->
                    {error, {compilation_failed, Reason}}
            end
    end.

%% @doc Check if a term is a compiled pattern.
-spec is_compiled(Term :: term()) -> boolean().
is_compiled({program, _, _, _, _}) -> true;
is_compiled(_) -> false.

%% @doc Get the size (instruction count) of a compiled program.
-spec program_size(Compiled :: compiled()) -> non_neg_integer().
program_size({program, Program, _, _, _}) ->
    length(Program).

%% @doc Convert compiled bytecode to a readable string.
-spec to_string(Compiled :: compiled()) -> string().
to_string({program, Program, _, _, _}) ->
    format_program(Program, 0).

%%% COMPILATION =============================================================

-spec do_compile(
    Pattern :: wf_term:wf_term(),
    Options :: compile_options()
) -> compiled().
do_compile(Pattern, Options) ->
    {Program, ExitPC, _Env} = compile_term(Pattern, 0, #{}, Options),
    Metadata = #{
        pattern_size => wf_term:term_size(Pattern),
        bytecode_size => length(Program),
        optimization => maps:get(optimize, Options, true),
        trace_level => maps:get(trace_level, Options, basic)
    },
    {
        program,
        Program,
        0,                  % entry_pc always 0
        ExitPC,             % exit_pc
        length(Program),    % size
        Metadata            % metadata
    }.

%% @doc Compile a single pattern term, returning {Program, NextPC, Environment}
-spec compile_term(
    Term :: wf_term:wf_term(),
    StartPC :: non_neg_integer(),
    Env :: map(),
    Options :: compile_options()
) -> {[wf_vm:opcode()], non_neg_integer(), map()}.

%% Task
compile_term({task, Name, Fun}, PC, Env, _Options) ->
    Op1 = wf_vm:op_task_enter(Name, Fun),
    Op2 = wf_vm:op_task_call(Fun),
    Op3 = wf_vm:op_task_exit(),
    {[Op1, Op2, Op3], PC + 3, Env};

%% Sequence: compile P, then Q
compile_term({seq, P, Q}, PC, Env, Options) ->
    {ProgP, PCAfterP, Env1} = compile_term(P, PC, Env, Options),
    {ProgQ, PCAfterQ, Env2} = compile_term(Q, PCAfterP, Env1, Options),
    {ProgP ++ ProgQ, PCAfterQ, Env2};

%% Parallel: fork into N branches, then join with AND
compile_term({par, Branches}, PC, Env, Options) ->
    N = length(Branches),
    ForkOp = wf_vm:op_par_fork(N),
    {BranchProgs, PCAfterBranches, Env1} =
        compile_branches(Branches, PC + 1, Env, Options),
    JoinOp = wf_vm:op_par_join(all),
    Program = [ForkOp] ++ BranchProgs ++ [JoinOp],
    {Program, PCAfterBranches + 1, Env1};

%% Exclusive choice: select one branch, cancel others
compile_term({choice, Branches}, PC, Env, Options) ->
    ChooseOp = wf_vm:op_xor_choose(Branches, 0),
    {BranchProgs, PCAfterBranches, Env1} =
        compile_branches(Branches, PC + 1, Env, Options),
    Program = [ChooseOp] ++ BranchProgs,
    {Program, PCAfterBranches, Env1};

%% Generalized join with policy
compile_term({join, Policy, Branches}, PC, Env, Options) ->
    N = length(Branches),
    ForkOp = wf_vm:op_par_fork(N),
    {BranchProgs, PCAfterBranches, Env1} =
        compile_branches(Branches, PC + 1, Env, Options),
    JoinOp = wf_vm:op_join_wait(Policy),
    Program = [ForkOp] ++ BranchProgs ++ [JoinOp],
    {Program, PCAfterBranches + 1, Env1};

%% Loop
compile_term({loop, Policy, Body}, PC, Env, Options) ->
    LoopStartPC = PC,
    {BodyProg, PCAfterBody, Env1} = compile_term(Body, PC, Env, Options),
    LoopBackOp = wf_vm:op_loop_back(Policy),
    Program = BodyProg ++ [LoopBackOp],
    {Program, PCAfterBody + 1, Env1};

%% Deferred choice
compile_term({defer, Branches}, PC, Env, Options) ->
    DeferOp = wf_vm:op_defer_race(Branches),
    {BranchProgs, PCAfterBranches, Env1} =
        compile_branches(Branches, PC + 1, Env, Options),
    Program = [DeferOp] ++ BranchProgs,
    {Program, PCAfterBranches, Env1};

%% Cancellation scope
compile_term({cancel_scope, {ScopeType, ScopeId}, Body}, PC, Env, Options) ->
    EnterOp = wf_vm:op_cancel_scope_enter(ScopeId, 0),  % Exit PC unknown yet
    {BodyProg, PCAfterBody, Env1} = compile_term(Body, PC + 1, Env, Options),
    ExitOp = wf_vm:op_cancel_scope_exit(ScopeId),
    %% Patch the exit PC in the enter opcode
    EnterOpPatched = wf_vm:op_cancel_scope_enter(ScopeId, PCAfterBody + 1),
    Program = [EnterOpPatched] ++ BodyProg ++ [ExitOp],
    {Program, PCAfterBody + 1, Env1};

%% Multiple instances
compile_term({mi, Policy, Body}, PC, Env, Options) ->
    SpawnOp = wf_vm:op_mi_spawn(Policy),
    {BodyProg, PCAfterBody, Env1} = compile_term(Body, PC + 1, Env, Options),
    JoinOp = wf_vm:op_mi_join(Policy),
    Program = [SpawnOp] ++ BodyProg ++ [JoinOp],
    {Program, PCAfterBody + 1, Env1}.

%% Helper: compile a list of branches
-spec compile_branches(
    Branches :: [wf_term:wf_term()],
    StartPC :: non_neg_integer(),
    Env :: map(),
    Options :: compile_options()
) -> {[wf_vm:opcode()], non_neg_integer(), map()}.
compile_branches([], PC, Env, _Options) ->
    {[], PC, Env};
compile_branches([Branch | Rest], PC, Env, Options) ->
    {ProgBranch, PCAfterBranch, Env1} = compile_term(Branch, PC, Env, Options),
    {ProgRest, PCFinal, Env2} = compile_branches(Rest, PCAfterBranch, Env1, Options),
    {ProgBranch ++ ProgRest, PCFinal, Env2}.

%%% FORMATTING ==============================================================

-spec format_program([wf_vm:opcode()], non_neg_integer()) -> string().
format_program([], _PC) ->
    "";
format_program([Op | Rest], PC) ->
    OpStr = format_opcode(Op, PC),
    OpStr ++ "\n" ++ format_program(Rest, PC + 1).

-spec format_opcode(wf_vm:opcode(), non_neg_integer()) -> string().
format_opcode(Op, PC) ->
    Type = wf_vm:opcode_type(Op),
    io_lib:format("~4w: ~w", [PC, Type]).


