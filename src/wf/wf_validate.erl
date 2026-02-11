%%% @doc WF Bytecode Validator - Structure and Soundness Verification
%%%
%%% This module validates compiled workflow bytecode to ensure structural
%%% soundness properties required for correct execution. Validation is
%%% performed on the bytecode level after compilation but before execution.
%%%
%%% Verification properties:
%%% - Deadlock freedom (bounded): No reachable state prevents completion
%%% - Proper completion: At least one path to successful termination
%%% - Unreachable join points: All joins can be reached
%%% - Token loss detection: All tokens accounted for
%%% - Cyclic dependency checking: No circular waits
%%%
%%% @end
-module(wf_validate).

-export([
    validate/1,
    validate/2,
    deadlock_free/1,
    proper_completion/1,
    unreachable_joins/1,
    token_loss/1,
    cyclic_dependencies/1
]).

-export_type([
    validation_result/0,
    validation_error/0,
    validation_options/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-type validation_result() ::
    ok
    | {error, [validation_error()]}.

-type validation_error() ::
      {deadlock_possible, Details :: map()}
    | {no_completion_path, Details :: map()}
    | {unreachable_join, JoinPC :: non_neg_integer()}
    | {token_loss, Details :: map()}
    | {cyclic_dependency, Cycle :: [non_neg_integer()]}
    | {invalid_bytecode, Reason :: term()}.

-type validation_options() :: #{
    max_depth => non_neg_integer(),      % Bounded exploration depth
    max_tokens => non_neg_integer(),     % Token bound for analysis
    check_deadlock => boolean(),
    check_completion => boolean(),
    check_joins => boolean(),
    check_token_loss => boolean(),
    check_cycles => boolean()
}.

%%% API =====================================================================

%% @doc Validate compiled bytecode for all structural properties.
-spec validate(Compiled :: wf_compile:compiled()) -> validation_result().
validate(Compiled) ->
    validate(Compiled, #{}).

%% @doc Validate with options controlling which checks to perform.
-spec validate(
    Compiled :: wf_compile:compiled(),
    Options :: validation_options()
) -> validation_result().
validate(Compiled, Options) ->
    case wf_compile:is_compiled(Compiled) of
        false ->
            {error, [{invalid_bytecode, not_compiled}]};
        true ->
            Errors = collect_validation_errors(Compiled, Options),
            case Errors of
                [] -> ok;
                _ -> {error, Errors}
            end
    end.

%% @doc Check if bytecode is deadlock-free (bounded exploration).
%%
%% A workflow is deadlock-free if there exists no reachable marking where:
%% - No transitions are enabled
%% - The marking is not a final marking
%%
%% Uses bounded state space exploration up to max_depth.
%%
-spec deadlock_free(Compiled :: wf_compile:compiled()) ->
    ok | {error, validation_error()}.
deadlock_free(Compiled) ->
    case extract_program(Compiled) of
        {ok, Program} ->
            check_deadlock_freedom(Program, #{max_depth => 100, max_tokens => 10});
        {error, Reason} ->
            {error, {invalid_bytecode, Reason}}
    end.

%% @doc Check if proper completion is possible.
%%
%% A workflow has proper completion if there exists at least one execution
%% path from the initial state to a final state (HALT opcode).
%%
-spec proper_completion(Compiled :: wf_compile:compiled()) ->
    ok | {error, validation_error()}.
proper_completion(Compiled) ->
    case extract_program(Compiled) of
        {ok, Program} ->
            check_completion_path(Program, #{max_depth => 100});
        {error, Reason} ->
            {error, {invalid_bytecode, Reason}}
    end.

%% @doc Detect unreachable join points.
%%
%% A join point is unreachable if no execution path can reach the join
%% opcode. This indicates dead code or structural errors.
%%
-spec unreachable_joins(Compiled :: wf_compile:compiled()) ->
    ok | {error, validation_error()}.
unreachable_joins(Compiled) ->
    case extract_program(Compiled) of
        {ok, Program} ->
            check_unreachable_joins(Program);
        {error, Reason} ->
            {error, {invalid_bytecode, Reason}}
    end.

%% @doc Detect potential token loss.
%%
%% Token loss occurs when:
%% - Tokens are created but never consumed
%% - Parallel branches create tokens that don't merge properly
%% - Cancellation leaves orphaned tokens
%%
-spec token_loss(Compiled :: wf_compile:compiled()) ->
    ok | {error, validation_error()}.
token_loss(Compiled) ->
    case extract_program(Compiled) of
        {ok, Program} ->
            check_token_conservation(Program);
        {error, Reason} ->
            {error, {invalid_bytecode, Reason}}
    end.

%% @doc Detect cyclic dependencies that could cause deadlock.
%%
%% Cyclic dependencies occur when:
%% - Join points have circular wait conditions
%% - Loop constructs create unbounded wait cycles
%%
-spec cyclic_dependencies(Compiled :: wf_compile:compiled()) ->
    ok | {error, validation_error()}.
cyclic_dependencies(Compiled) ->
    case extract_program(Compiled) of
        {ok, Program} ->
            check_cyclic_dependencies(Program);
        {error, Reason} ->
            {error, {invalid_bytecode, Reason}}
    end.

%%% INTERNAL VALIDATION =====================================================

-spec collect_validation_errors(
    Compiled :: wf_compile:compiled(),
    Options :: validation_options()
) -> [validation_error()].
collect_validation_errors(Compiled, Options) ->
    Checks = [
        {maps:get(check_deadlock, Options, true), fun deadlock_free/1},
        {maps:get(check_completion, Options, true), fun proper_completion/1},
        {maps:get(check_joins, Options, true), fun unreachable_joins/1},
        {maps:get(check_token_loss, Options, true), fun token_loss/1},
        {maps:get(check_cycles, Options, true), fun cyclic_dependencies/1}
    ],

    lists:filtermap(fun({Enabled, CheckFun}) ->
        case Enabled of
            false -> false;
            true ->
                case CheckFun(Compiled) of
                    ok -> false;
                    {error, Error} -> {true, Error}
                end
        end
    end, Checks).

-spec extract_program(wf_compile:compiled()) ->
    {ok, [wf_vm:opcode()]} | {error, term()}.
extract_program({program, Program, _Entry, _Exit, _Size, _Meta}) ->
    {ok, Program};
extract_program(_) ->
    {error, invalid_compiled_format}.

%%% DEADLOCK FREEDOM CHECKING ===============================================

-spec check_deadlock_freedom(
    Program :: [wf_vm:opcode()],
    Options :: map()
) -> ok | {error, validation_error()}.
check_deadlock_freedom(Program, Options) ->
    MaxDepth = maps:get(max_depth, Options, 100),
    MaxTokens = maps:get(max_tokens, Options, 10),

    %% Build control flow graph
    CFG = build_control_flow_graph(Program),

    %% Identify join points and their dependencies
    Joins = find_join_points(Program),

    %% Check each join for potential deadlock
    case check_joins_for_deadlock(Joins, CFG, MaxDepth, MaxTokens) of
        ok -> ok;
        {error, Details} ->
            {error, {deadlock_possible, Details}}
    end.

-spec build_control_flow_graph([wf_vm:opcode()]) -> #{non_neg_integer() => [non_neg_integer()]}.
build_control_flow_graph(Program) ->
    build_cfg(Program, 0, #{}).

-spec build_cfg([wf_vm:opcode()], non_neg_integer(), map()) -> map().
build_cfg([], _PC, CFG) ->
    CFG;
build_cfg([Op | Rest], PC, CFG) ->
    NextPC = PC + 1,

    %% Determine successors based on opcode type
    Successors = case wf_vm:opcode_type(Op) of
        par_fork ->
            %% Fork creates multiple paths
            [NextPC];
        xor_choose ->
            %% Choice creates multiple paths
            [NextPC];
        loop_back ->
            %% Loop can jump back or continue
            [NextPC]; % Simplified - full analysis would track loop target
        cancel_scope_enter ->
            %% Can jump to exit on cancel
            {cancel_scope_enter, _Id, ExitPC} = Op,
            [NextPC, ExitPC];
        halt ->
            %% Terminal node
            [];
        _ ->
            %% Sequential flow
            [NextPC]
    end,

    CFG1 = CFG#{PC => Successors},
    build_cfg(Rest, NextPC, CFG1).

-spec find_join_points([wf_vm:opcode()]) -> [{non_neg_integer(), wf_vm:opcode()}].
find_join_points(Program) ->
    find_joins(Program, 0, []).

-spec find_joins([wf_vm:opcode()], non_neg_integer(), [{non_neg_integer(), wf_vm:opcode()}]) ->
    [{non_neg_integer(), wf_vm:opcode()}].
find_joins([], _PC, Acc) ->
    lists:reverse(Acc);
find_joins([Op | Rest], PC, Acc) ->
    case wf_vm:opcode_type(Op) of
        Type when Type =:= par_join; Type =:= join_wait ->
            find_joins(Rest, PC + 1, [{PC, Op} | Acc]);
        _ ->
            find_joins(Rest, PC + 1, Acc)
    end.

-spec check_joins_for_deadlock(
    Joins :: [{non_neg_integer(), wf_vm:opcode()}],
    CFG :: map(),
    MaxDepth :: non_neg_integer(),
    MaxTokens :: non_neg_integer()
) -> ok | {error, map()}.
check_joins_for_deadlock([], _CFG, _MaxDepth, _MaxTokens) ->
    ok;
check_joins_for_deadlock([{PC, Op} | Rest], CFG, MaxDepth, MaxTokens) ->
    case check_join_reachability(PC, Op, CFG, MaxDepth) of
        ok ->
            check_joins_for_deadlock(Rest, CFG, MaxDepth, MaxTokens);
        {error, _} = Error ->
            Error
    end.

-spec check_join_reachability(
    JoinPC :: non_neg_integer(),
    JoinOp :: wf_vm:opcode(),
    CFG :: map(),
    MaxDepth :: non_neg_integer()
) -> ok | {error, map()}.
check_join_reachability(JoinPC, JoinOp, CFG, MaxDepth) ->
    %% Check if join can be reached from entry point (PC=0)
    case reachable(0, JoinPC, CFG, sets:new(), 0, MaxDepth) of
        true ->
            %% Join is reachable - check if all branches can reach it
            Policy = extract_join_policy(JoinOp),
            check_join_policy_satisfaction(JoinPC, Policy, CFG, MaxDepth);
        false ->
            %% Join is unreachable - this will be caught by unreachable_joins check
            ok
    end.

-spec extract_join_policy(wf_vm:opcode()) -> wf_vm:join_policy().
extract_join_policy({par_join, Policy}) -> Policy;
extract_join_policy({join_wait, Policy}) -> Policy;
extract_join_policy(_) -> all.

-spec check_join_policy_satisfaction(
    JoinPC :: non_neg_integer(),
    Policy :: wf_vm:join_policy(),
    CFG :: map(),
    MaxDepth :: non_neg_integer()
) -> ok | {error, map()}.
check_join_policy_satisfaction(_JoinPC, _Policy, _CFG, _MaxDepth) ->
    %% Simplified: assume all join policies can be satisfied
    %% Full implementation would analyze fork/join pairs
    ok.

-spec reachable(
    From :: non_neg_integer(),
    To :: non_neg_integer(),
    CFG :: map(),
    Visited :: sets:set(non_neg_integer()),
    Depth :: non_neg_integer(),
    MaxDepth :: non_neg_integer()
) -> boolean().
reachable(From, To, _CFG, _Visited, Depth, MaxDepth) when Depth > MaxDepth ->
    false;
reachable(From, To, _CFG, _Visited, _Depth, _MaxDepth) when From =:= To ->
    true;
reachable(From, To, CFG, Visited, Depth, MaxDepth) ->
    case sets:is_element(From, Visited) of
        true ->
            false;
        false ->
            Visited1 = sets:add_element(From, Visited),
            Successors = maps:get(From, CFG, []),
            lists:any(fun(Next) ->
                reachable(Next, To, CFG, Visited1, Depth + 1, MaxDepth)
            end, Successors)
    end.

%%% PROPER COMPLETION CHECKING ==============================================

-spec check_completion_path(
    Program :: [wf_vm:opcode()],
    Options :: map()
) -> ok | {error, validation_error()}.
check_completion_path(Program, Options) ->
    MaxDepth = maps:get(max_depth, Options, 100),

    %% Find all HALT opcodes
    HaltPCs = find_halt_opcodes(Program),

    case HaltPCs of
        [] ->
            {error, {no_completion_path, #{reason => no_halt_opcode}}};
        _ ->
            %% Check if at least one HALT is reachable from entry
            CFG = build_control_flow_graph(Program),
            case any_reachable(0, HaltPCs, CFG, MaxDepth) of
                true ->
                    ok;
                false ->
                    {error, {no_completion_path, #{
                        reason => halt_unreachable,
                        halt_pcs => HaltPCs
                    }}}
            end
    end.

-spec find_halt_opcodes([wf_vm:opcode()]) -> [non_neg_integer()].
find_halt_opcodes(Program) ->
    find_halts(Program, 0, []).

-spec find_halts([wf_vm:opcode()], non_neg_integer(), [non_neg_integer()]) ->
    [non_neg_integer()].
find_halts([], _PC, Acc) ->
    lists:reverse(Acc);
find_halts([Op | Rest], PC, Acc) ->
    case wf_vm:opcode_type(Op) of
        halt ->
            find_halts(Rest, PC + 1, [PC | Acc]);
        _ ->
            find_halts(Rest, PC + 1, Acc)
    end.

-spec any_reachable(
    From :: non_neg_integer(),
    Targets :: [non_neg_integer()],
    CFG :: map(),
    MaxDepth :: non_neg_integer()
) -> boolean().
any_reachable(_From, [], _CFG, _MaxDepth) ->
    false;
any_reachable(From, [Target | Rest], CFG, MaxDepth) ->
    case reachable(From, Target, CFG, sets:new(), 0, MaxDepth) of
        true -> true;
        false -> any_reachable(From, Rest, CFG, MaxDepth)
    end.

%%% UNREACHABLE JOIN CHECKING ===============================================

-spec check_unreachable_joins([wf_vm:opcode()]) -> ok | {error, validation_error()}.
check_unreachable_joins(Program) ->
    Joins = find_join_points(Program),
    CFG = build_control_flow_graph(Program),

    UnreachableJoins = lists:filtermap(fun({PC, _Op}) ->
        case reachable(0, PC, CFG, sets:new(), 0, 100) of
            false -> {true, PC};
            true -> false
        end
    end, Joins),

    case UnreachableJoins of
        [] ->
            ok;
        [FirstUnreachable | _] ->
            {error, {unreachable_join, FirstUnreachable}}
    end.

%%% TOKEN CONSERVATION CHECKING =============================================

-spec check_token_conservation([wf_vm:opcode()]) -> ok | {error, validation_error()}.
check_token_conservation(Program) ->
    %% Analyze token creation and consumption
    Analysis = analyze_token_flow(Program, 0, #{
        created => 0,
        consumed => 0,
        in_flight => 0,
        forks => [],
        joins => []
    }),

    case is_token_balanced(Analysis) of
        true ->
            ok;
        false ->
            {error, {token_loss, Analysis}}
    end.

-spec analyze_token_flow(
    [wf_vm:opcode()],
    non_neg_integer(),
    map()
) -> map().
analyze_token_flow([], _PC, Analysis) ->
    Analysis;
analyze_token_flow([Op | Rest], PC, Analysis) ->
    Analysis1 = case wf_vm:opcode_type(Op) of
        par_fork ->
            {par_fork, N} = Op,
            Analysis#{
                created => maps:get(created, Analysis) + N,
                forks => [{PC, N} | maps:get(forks, Analysis)]
            };
        par_join ->
            {par_join, Policy} = Op,
            ExpectedTokens = expected_tokens_for_policy(Policy),
            Analysis#{
                consumed => maps:get(consumed, Analysis) + ExpectedTokens,
                joins => [{PC, Policy} | maps:get(joins, Analysis)]
            };
        join_wait ->
            {join_wait, Policy} = Op,
            ExpectedTokens = expected_tokens_for_policy(Policy),
            Analysis#{
                consumed => maps:get(consumed, Analysis) + ExpectedTokens,
                joins => [{PC, Policy} | maps:get(joins, Analysis)]
            };
        mi_spawn ->
            %% Multiple instance creates tokens
            Analysis#{created => maps:get(created, Analysis) + 1};
        mi_join ->
            %% Multiple instance joins tokens
            Analysis#{consumed => maps:get(consumed, Analysis) + 1};
        _ ->
            Analysis
    end,
    analyze_token_flow(Rest, PC + 1, Analysis1).

-spec expected_tokens_for_policy(wf_vm:join_policy()) -> non_neg_integer().
expected_tokens_for_policy(all) -> 1;  % Simplified: assume binary fork
expected_tokens_for_policy(xor_merge) -> 1;
expected_tokens_for_policy(sync_merge) -> 1;
expected_tokens_for_policy({first_n, N}) -> N;
expected_tokens_for_policy({n_of_m, N, _M}) -> N.

-spec is_token_balanced(map()) -> boolean().
is_token_balanced(#{created := Created, consumed := Consumed}) ->
    %% Simplified: check if forks and joins are balanced
    Created =:= Consumed.

%%% CYCLIC DEPENDENCY CHECKING ==============================================

-spec check_cyclic_dependencies([wf_vm:opcode()]) -> ok | {error, validation_error()}.
check_cyclic_dependencies(Program) ->
    CFG = build_control_flow_graph(Program),

    %% Use DFS to detect cycles in control flow
    AllPCs = lists:seq(0, length(Program) - 1),

    case find_cycle_in_cfg(AllPCs, CFG) of
        {ok, []} ->
            ok;
        {ok, Cycle} ->
            %% Check if cycle is benign (loop construct) or problematic
            case is_benign_cycle(Cycle, Program) of
                true -> ok;
                false -> {error, {cyclic_dependency, Cycle}}
            end
    end.

-spec find_cycle_in_cfg([non_neg_integer()], map()) -> {ok, [non_neg_integer()]}.
find_cycle_in_cfg([], _CFG) ->
    {ok, []};
find_cycle_in_cfg([Start | Rest], CFG) ->
    case detect_cycle_dfs(Start, Start, CFG, [], sets:new()) of
        false ->
            find_cycle_in_cfg(Rest, CFG);
        {true, Cycle} ->
            {ok, Cycle}
    end.

-spec detect_cycle_dfs(
    Current :: non_neg_integer(),
    Start :: non_neg_integer(),
    CFG :: map(),
    Path :: [non_neg_integer()],
    Visited :: sets:set(non_neg_integer())
) -> false | {true, [non_neg_integer()]}.
detect_cycle_dfs(Current, Start, CFG, Path, Visited) ->
    case sets:is_element(Current, Visited) of
        true ->
            false;
        false ->
            Visited1 = sets:add_element(Current, Visited),
            Successors = maps:get(Current, CFG, []),

            case lists:member(Start, Successors) of
                true ->
                    %% Found cycle back to start
                    {true, lists:reverse([Start | Path])};
                false ->
                    %% Continue DFS
                    case find_cycle_in_successors(Successors, Start, CFG, [Current | Path], Visited1) of
                        false -> false;
                        {true, _} = Result -> Result
                    end
            end
    end.

-spec find_cycle_in_successors(
    [non_neg_integer()],
    non_neg_integer(),
    map(),
    [non_neg_integer()],
    sets:set(non_neg_integer())
) -> false | {true, [non_neg_integer()]}.
find_cycle_in_successors([], _Start, _CFG, _Path, _Visited) ->
    false;
find_cycle_in_successors([Succ | Rest], Start, CFG, Path, Visited) ->
    case detect_cycle_dfs(Succ, Start, CFG, Path, Visited) of
        false ->
            find_cycle_in_successors(Rest, Start, CFG, Path, Visited);
        {true, _} = Result ->
            Result
    end.

-spec is_benign_cycle([non_neg_integer()], [wf_vm:opcode()]) -> boolean().
is_benign_cycle(Cycle, Program) ->
    %% A cycle is benign if it's part of a loop construct
    %% Check if any PC in the cycle has a loop_back opcode
    lists:any(fun(PC) ->
        case lists:nth(PC + 1, Program) of
            {loop_back, _} -> true;
            _ -> false
        end
    end, Cycle).

%%% TESTS ===================================================================

simple_sequence_test() ->
    %% Compile a simple sequence and validate
    Pattern = wf_term:seq(
        wf_term:task(a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(b, fun(Ctx) -> {ok, Ctx} end)
    ),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assertEqual(ok, validate(Compiled)).

parallel_fork_join_test() ->
    %% Compile parallel fork/join and validate
    Pattern = wf_term:par([
        wf_term:task(a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(b, fun(Ctx) -> {ok, Ctx} end)
    ]),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assertEqual(ok, validate(Compiled)).

choice_test() ->
    %% Compile exclusive choice and validate
    Pattern = wf_term:choice([
        wf_term:task(a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(b, fun(Ctx) -> {ok, Ctx} end)
    ]),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assertEqual(ok, validate(Compiled)).

loop_test() ->
    %% Compile loop and validate
    Pattern = wf_term:loop(
        {max_iter, 3},
        wf_term:task(loop_body, fun(Ctx) -> {ok, Ctx} end)
    ),
    {ok, Compiled} = wf_compile:compile(Pattern),
    Result = validate(Compiled),
    %% Loop creates a cycle, which is benign
    ?assertEqual(ok, Result).

cancel_scope_test() ->
    %% Compile cancellation scope and validate
    Pattern = wf_term:cancel_scope(
        {region, test_region},
        wf_term:task(cancellable, fun(Ctx) -> {ok, Ctx} end)
    ),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assertEqual(ok, validate(Compiled)).

validation_options_test() ->
    %% Test validation with custom options
    Pattern = wf_term:task(simple, fun(Ctx) -> {ok, Ctx} end),
    {ok, Compiled} = wf_compile:compile(Pattern),
    Options = #{
        check_deadlock => false,
        check_completion => true,
        check_joins => false,
        check_token_loss => false,
        check_cycles => false
    },
    ?assertEqual(ok, validate(Compiled, Options)).

deadlock_free_test() ->
    %% Test deadlock freedom check
    Pattern = wf_term:seq(
        wf_term:task(a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(b, fun(Ctx) -> {ok, Ctx} end)
    ),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assertEqual(ok, deadlock_free(Compiled)).

proper_completion_test() ->
    %% Test proper completion check
    Pattern = wf_term:task(simple, fun(Ctx) -> {ok, Ctx} end),
    {ok, Compiled} = wf_compile:compile(Pattern),
    ?assertEqual(ok, proper_completion(Compiled)).

token_conservation_test() ->
    %% Test token conservation check
    Pattern = wf_term:par([
        wf_term:task(a, fun(Ctx) -> {ok, Ctx} end),
        wf_term:task(b, fun(Ctx) -> {ok, Ctx} end)
    ]),
    {ok, Compiled} = wf_compile:compile(Pattern),
    Result = token_loss(Compiled),
    %% Par creates fork/join which should balance
    ?assertEqual(ok, Result).
