%%%-------------------------------------------------------------------
%%% @doc ln_compile - Plan to bytecode compiler.
%%%
%%% Compiles plan terms into executable bytecode for efficient
%%% execution without runtime AST walking.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_compile).

%% API
-export([compile/1]).
-export([opcode_name/1]).

%% Types
-export_type([bytecode/0, opcode/0, label/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type label() :: non_neg_integer().
-type join_id() :: reference().
-type scope_id() :: term().

-type opcode() :: {op_task_start, atom()}
                | {op_task_complete, term()}
                | {op_fork, [label()], label()}
                | {op_join_wait, join_id(), label()}
                | {op_xor_branch, [label()], label()}
                | {op_xor_choose, reference(), label()}
                | {op_scope_enter, scope_id()}
                | {op_scope_exit, scope_id()}
                | {op_defer_start, [label()], label()}
                | {op_defer_wait, reference(), label()}
                | {op_loop_check, label(), label()}
                | {op_loop_iter, label()}
                | {op_wait_check, term(), label()}
                | {op_halt, term()}.

-type program() :: [{label(), opcode()}].

-type join_table() :: #{join_id() => {label(), non_neg_integer(), term()}}.
-type scope_table() :: #{scope_id() => {label(), label()}}.

-type bytecode() :: #{
    program => program(),
    joins => join_table(),
    scopes => scope_table(),
    entry_label => label()
}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Compile a plan term to bytecode.
-spec compile(ln_plan:plan()) -> {ok, bytecode()} | {error, term()}.
compile(Plan) ->
    case ln_plan:validate(Plan) of
        ok ->
            {Program, Joins, Scopes} = compile_plan(Plan, 0, #{}, #{}),
            {ok, #{
                program => lists:reverse(Program),
                joins => Joins,
                scopes => Scopes,
                entry_label => 0
            }};
        {error, _} = Error ->
            Error
    end.

%% @doc Get human-readable name for opcode.
-spec opcode_name(opcode()) -> atom().
opcode_name({op_task_start, _}) -> op_task_start;
opcode_name({op_task_complete, _}) -> op_task_complete;
opcode_name({op_fork, _, _}) -> op_fork;
opcode_name({op_join_wait, _, _}) -> op_join_wait;
opcode_name({op_xor_branch, _, _}) -> op_xor_branch;
opcode_name({op_xor_choose, _, _}) -> op_xor_choose;
opcode_name({op_scope_enter, _}) -> op_scope_enter;
opcode_name({op_scope_exit, _}) -> op_scope_exit;
opcode_name({op_defer_start, _, _}) -> op_defer_start;
opcode_name({op_defer_wait, _, _}) -> op_defer_wait;
opcode_name({op_loop_check, _, _}) -> op_loop_check;
opcode_name({op_loop_iter, _}) -> op_loop_iter;
opcode_name({op_wait_check, _, _}) -> op_wait_check;
opcode_name({op_halt, _}) -> op_halt.

%%%-------------------------------------------------------------------
%%% Internal compilation functions
%%%-------------------------------------------------------------------

%% Compile a plan starting at a label.
compile_plan({task, TaskId}, Label, Joins, Scopes) ->
    NextLabel = Label + 2,
    Program = [
        {NextLabel - 1, {op_task_start, TaskId}},
        {NextLabel, {op_task_complete, undefined}}
    ],
    {Program, Joins, Scopes};

compile_plan({seq, Plans}, Label, Joins, Scopes) ->
    compile_seq(Plans, Label, Joins, Scopes);

compile_plan({par, Plans}, Label, Joins, Scopes) ->
    compile_par(Plans, Label, Joins, Scopes);

compile_plan({xor, Plans}, Label, Joins, Scopes) ->
    compile_xor(Plans, Label, Joins, Scopes);

compile_plan({join, Policy, Plans}, Label, Joins, Scopes) ->
    compile_join(Policy, Plans, Label, Joins, Scopes);

compile_plan({loop, Policy, Plan}, Label, Joins, Scopes) ->
    compile_loop(Policy, Plan, Label, Joins, Scopes);

compile_plan({defer, Plans}, Label, Joins, Scopes) ->
    compile_defer(Plans, Label, Joins, Scopes);

compile_plan({scope, ScopeId, Plan}, Label, Joins, Scopes) ->
    compile_scope(ScopeId, Plan, Label, Joins, Scopes);

compile_plan({mi, Policy, Plan}, Label, Joins, Scopes) ->
    compile_mi(Policy, Plan, Label, Joins, Scopes);

compile_plan({wait, MatchSpec}, Label, Joins, Scopes) ->
    WaitLabel = Label,
    ResumeLabel = Label + 1,
    Program = [
        {WaitLabel, {op_wait_check, MatchSpec, ResumeLabel}}
    ],
    {Program, Joins, Scopes}.

%% Compile sequence: execute plans one after another.
compile_seq([], Label, Joins, Scopes) ->
    {[{Label, {op_halt, normal}}], Joins, Scopes};
compile_seq([Plan], Label, Joins, Scopes) ->
    compile_plan(Plan, Label, Joins, Scopes);
compile_seq([Plan | Rest], Label, Joins, Scopes) ->
    {Prog1, Joins1, Scopes1} = compile_plan(Plan, Label, Joins, Scopes),
    NextLabel = Label + length(Prog1),
    {Prog2, Joins2, Scopes2} = compile_seq(Rest, NextLabel, Joins1, Scopes1),
    {Prog1 ++ Prog2, Joins2, Scopes2}.

%% Compile parallel: fork all branches, join at end.
compile_par(Plans, Label, Joins, Scopes) ->
    JoinId = make_ref(),
    BranchCount = length(Plans),
    {BranchProgs, NextLabel, Joins1, Scopes1} = compile_branches(Plans, Label + 2, Joins, Scopes),
    JoinLabel = NextLabel,
    Program = [
        {Label, {op_fork, [L || {L, _} <- BranchProgs], JoinLabel}}
    ] ++ BranchProgs ++ [
        {JoinLabel, {op_join_wait, JoinId, JoinLabel + 1}}
    ],
    Joins2 = Joins1#{JoinId => {JoinLabel, BranchCount, all}},
    {Program, Joins2, Scopes1}.

%% Compile xor: exclusive choice.
compile_xor(Plans, Label, Joins, Scopes) ->
    XorId = make_ref(),
    {BranchProgs, NextLabel, Joins1, Scopes1} = compile_branches(Plans, Label + 2, Joins, Scopes),
    EndLabel = NextLabel,
    Program = [
        {Label, {op_xor_branch, [L || {L, _} <- BranchProgs], EndLabel}}
    ] ++ BranchProgs,
    {Program, Joins1, Scopes1}.

%% Compile join with policy.
compile_join(Policy, Plans, Label, Joins, Scopes) ->
    JoinId = make_ref(),
    BranchCount = length(Plans),
    {BranchProgs, NextLabel, Joins1, Scopes1} = compile_branches(Plans, Label + 2, Joins, Scopes),
    JoinLabel = NextLabel,
    Program = [
        {Label, {op_fork, [L || {L, _} <- BranchProgs], JoinLabel}}
    ] ++ BranchProgs ++ [
        {JoinLabel, {op_join_wait, JoinId, JoinLabel + 1}}
    ],
    Joins2 = Joins1#{JoinId => {JoinLabel, BranchCount, Policy}},
    {Program, Joins2, Scopes1}.

%% Compile loop.
compile_loop(Policy, Plan, Label, Joins, Scopes) ->
    LoopHead = Label,
    {BodyProg, NextLabel, Joins1, Scopes1} = compile_plan(Plan, Label + 2, Joins, Scopes),
    LoopEnd = NextLabel,
    Program = [
        {LoopHead, {op_loop_check, LoopHead + 1, LoopEnd + 1}}
    ] ++ BodyProg ++ [
        {LoopEnd, {op_loop_iter, LoopHead}}
    ],
    {Program, Joins1, Scopes1}.

%% Compile defer (external choice).
compile_defer(Plans, Label, Joins, Scopes) ->
    DeferId = make_ref(),
    {BranchProgs, NextLabel, Joins1, Scopes1} = compile_branches(Plans, Label + 2, Joins, Scopes),
    EndLabel = NextLabel,
    Program = [
        {Label, {op_defer_start, [L || {L, _} <- BranchProgs], EndLabel}}
    ] ++ BranchProgs,
    {Program, Joins1, Scopes1}.

%% Compile scope.
compile_scope(ScopeId, Plan, Label, Joins, Scopes) ->
    EnterLabel = Label,
    {BodyProg, NextLabel, Joins1, Scopes1} = compile_plan(Plan, Label + 1, Joins, Scopes),
    ExitLabel = NextLabel,
    Program = [
        {EnterLabel, {op_scope_enter, ScopeId}}
    ] ++ BodyProg ++ [
        {ExitLabel, {op_scope_exit, ScopeId}}
    ],
    Scopes2 = Scopes1#{ScopeId => {EnterLabel, ExitLabel}},
    {Program, Joins1, Scopes2}.

%% Compile multiple instances.
compile_mi({concurrent, N}, Plan, Label, Joins, Scopes) ->
    %% Unroll N parallel instances
    Plans = lists:duplicate(N, Plan),
    compile_par(Plans, Label, Joins, Scopes);
compile_mi({sequential, N}, Plan, Label, Joins, Scopes) ->
    %% Unroll N sequential instances
    Plans = lists:duplicate(N, Plan),
    compile_seq(Plans, Label, Joins, Scopes);
compile_mi(one_for_one, Plan, Label, Joins, Scopes) ->
    %% Create fork with individual join points
    compile_par([Plan], Label, Joins, Scopes).

%% Compile multiple branch plans, returning label mapping.
compile_branches(Plans, StartLabel, Joins, Scopes) ->
    compile_branches(Plans, StartLabel, Joins, Scopes, []).

compile_branches([], Label, Joins, Scopes, Acc) ->
    {lists:reverse(Acc), Label, Joins, Scopes};
compile_branches([Plan | Rest], Label, Joins, Scopes, Acc) ->
    {Prog, NextLabel, Joins1, Scopes1} = compile_plan(Plan, Label, Joins, Scopes),
    compile_branches(Rest, NextLabel, Joins1, Scopes1, [{Label, Prog} | Acc]).
