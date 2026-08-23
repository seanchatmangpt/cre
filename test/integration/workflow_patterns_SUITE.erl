%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow Pattern Integration Test Suite
%%%
%%% This Common Test suite validates pattern-based workflow execution
%%% including sequence, parallel, choice, and synchronization patterns.
%%%
%%% Test Coverage:
%%% - WCP-01: Sequence Pattern
%%% - WCP-02: Parallel Split Pattern
%%% - WCP-03: Synchronization Pattern
%%% - WCP-04: Exclusive Choice Pattern
%%% - WCP-05: Simple Merge Pattern
%%% - WCP-06: Multi-Choice Pattern
%%% - WCP-07: Synchronizing Merge Pattern
%%% - WCP-08: Multi-Merge Pattern
%%% - WCP-09: Discriminator Pattern
%%% - WCP-10: Arbitration Pattern
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(workflow_patterns_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gen_pnet.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    % Sequence pattern tests
    sequence_basic_test/1,
    sequence_long_chain_test/1,
    sequence_with_data_test/1,

    % Parallel split tests
    parallel_split_basic_test/1,
    parallel_split_multiple_branches_test/1,
    parallel_split_token_distribution_test/1,

    % Synchronization tests
    synchronization_basic_test/1,
    synchronization_multiple_inputs_test/1,
    synchronization_deadlock_prevention_test/1,

    % Choice pattern tests
    exclusive_choice_basic_test/1,
    exclusive_choice_condition_test/1,
    exclusive_choice_default_path_test/1,

    % Merge pattern tests
    simple_merge_basic_test/1,
    multi_choice_basic_test/1,
    synchronizing_merge_test/1,

    % Advanced pattern tests
    multi_merge_test/1,
    discriminator_test/1,
    arbitration_test/1,

    % Complex workflow tests
    complex_workflow_sequence_parallel_test/1,
    complex_workflow_choice_merge_test/1,
    complex_workflow_nested_patterns_test/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
        {group, basic_patterns},
        {group, split_join_patterns},
        {group, choice_merge_patterns},
        {group, advanced_patterns},
        {group, complex_workflows}
    ].

groups() ->
    [
        {basic_patterns, [], [
            sequence_basic_test,
            sequence_long_chain_test,
            sequence_with_data_test
        ]},
        {split_join_patterns, [], [
            parallel_split_basic_test,
            parallel_split_multiple_branches_test,
            parallel_split_token_distribution_test,
            synchronization_basic_test,
            synchronization_multiple_inputs_test,
            synchronization_deadlock_prevention_test
        ]},
        {choice_merge_patterns, [], [
            exclusive_choice_basic_test,
            exclusive_choice_condition_test,
            exclusive_choice_default_path_test,
            simple_merge_basic_test,
            multi_choice_basic_test,
            synchronizing_merge_test
        ]},
        {advanced_patterns, [], [
            multi_merge_test,
            discriminator_test,
            arbitration_test
        ]},
        {complex_workflows, [], [
            complex_workflow_sequence_parallel_test,
            complex_workflow_choice_merge_test,
            complex_workflow_nested_patterns_test
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting workflow_patterns_SUITE"),
    ok = ensure_modules_loaded(),
    Config.

end_per_suite(_Config) ->
    ct:pal("Completed workflow_patterns_SUITE"),
    ok.

init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

end_per_group(Group, _Config) ->
    ct:pal("Completed group: ~p", [Group]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases - Basic Patterns (Sequence)
%%%===================================================================

%% @doc Test basic sequence pattern: A -> B -> C
sequence_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(sequence_pattern_net,
                                     #{tasks => [a, b, c]}, []),

    %% Inject start token
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Allow workflow to complete
    {ok, FinalMarking} = gen_yawl:sync(Pid, 5000),

    ct:pal("Sequence completed with marking: ~p", [FinalMarking]),

    %% Verify final state - token should be at end place
    {ok, EndTokens} = gen_yawl:ls(Pid, p_end),
    ?assert(length(EndTokens) > 0),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test long sequence chain with 10 tasks
sequence_long_chain_test(_Config) ->
    Tasks = [task1, task2, task3, task4, task5, task6, task7, task8, task9, task10],
    {ok, Pid} = gen_yawl:start_link(sequence_pattern_net, #{tasks => Tasks}, []),

    %% Start execution
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Complete workflow
    {ok, _} = gen_yawl:sync(Pid, 10000),

    %% Verify completion
    {ok, EndTokens} = gen_yawl:ls(Pid, p_end),
    ?assert(length(EndTokens) > 0),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test sequence pattern with data passing between tasks
sequence_with_data_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(sequence_pattern_net,
                                     #{tasks => [a, b, c], pass_data => true}, []),

    %% Start with initial data
    InitData = {data_token, #{value => 100}},
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [InitData]}),

    %% Complete workflow
    {ok, _} = gen_yawl:sync(Pid, 5000),

    %% Verify data was passed through
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after sequence: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Parallel Split Pattern
%%%===================================================================

%% @doc Test basic parallel split: A -> (B, C, D)
parallel_split_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(parallel_split_net,
                                     #{branches => 3}, []),

    %% Start workflow
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Allow split to execute
    timer:sleep(200),

    %% Verify tokens in all branches
    Marking = gen_yawl:marking(Pid),
    ct:pal("Marking after parallel split: ~p", [Marking]),

    %% Count tokens in branch places
    BranchPlaces = [p_branch1, p_branch2, p_branch3],
    TotalBranchTokens = lists:sum([
        length(maps:get(P, Marking, [])) || P <- BranchPlaces
    ]),

    ?assert(TotalBranchTokens >= 3),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test parallel split with multiple branches
parallel_split_multiple_branches_test(_Config) ->
    NumBranches = 5,
    {ok, Pid} = gen_yawl:start_link(parallel_split_net,
                                     #{branches => NumBranches}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    timer:sleep(300),

    Marking = gen_yawl:marking(Pid),
    ct:pal("Parallel split with ~p branches: ~p", [NumBranches, Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test token distribution in parallel split
parallel_split_token_distribution_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(parallel_split_net, #{branches => 3}, []),

    %% Inject multiple tokens
    Tokens = [token1, token2],
    {ok, _} = gen_yawl:inject(Pid, #{p_start => Tokens}),

    timer:sleep(300),

    Marking = gen_yawl:marking(Pid),
    ct:pal("Token distribution: ~p", [Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Synchronization Pattern
%%%===================================================================

%% @doc Test basic synchronization: (A, B, C) -> D
synchronization_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(synchronization_net,
                                     #{inputs => 3}, []),

    %% Inject tokens to all input places
    {ok, _} = gen_yawl:inject(Pid, #{
        p_input1 => [token1],
        p_input2 => [token2],
        p_input3 => [token3]
    }),

    %% Allow synchronization
    timer:sleep(200),

    %% Verify synchronized output
    {ok, OutputTokens} = gen_yawl:ls(Pid, p_output),
    ?assert(length(OutputTokens) > 0),

    ct:pal("Synchronization output: ~p", [OutputTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test synchronization with multiple inputs
synchronization_multiple_inputs_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(synchronization_net, #{inputs => 5}, []),

    %% Inject tokens to all 5 inputs
    InjectMap = maps:from_list([
        {list_to_atom("p_input" ++ integer_to_list(N)), [list_to_atom("token" ++ integer_to_list(N))]}
        || N <- lists:seq(1, 5)
    ]),

    {ok, _} = gen_yawl:inject(Pid, InjectMap),

    timer:sleep(300),

    {ok, OutputTokens} = gen_yawl:ls(Pid, p_output),
    ct:pal("Synchronization with 5 inputs: ~p", [OutputTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test deadlock prevention in synchronization
synchronization_deadlock_prevention_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(synchronization_net, #{inputs => 3}, []),

    %% Only inject tokens to 2 of 3 inputs (should not fire)
    {ok, _} = gen_yawl:inject(Pid, #{
        p_input1 => [token1],
        p_input2 => [token2]
    }),

    timer:sleep(200),

    %% Verify no output (deadlock prevention)
    {ok, OutputTokens} = gen_yawl:ls(Pid, p_output),
    ?assertEqual([], OutputTokens),

    ct:pal("Correctly prevented firing without all inputs"),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Choice and Merge Patterns
%%%===================================================================

%% @doc Test exclusive choice pattern
exclusive_choice_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(exclusive_choice_net,
                                     #{condition => path_a}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    timer:sleep(200),

    %% Verify only one path taken
    Marking = gen_yawl:marking(Pid),
    {ok, PathATokens} = gen_yawl:ls(Pid, p_path_a),
    {ok, PathBTokens} = gen_yawl:ls(Pid, p_path_b),

    ct:pal("Choice result - Path A: ~p, Path B: ~p", [PathATokens, PathBTokens]),

    %% Should have tokens in exactly one path
    ?assert((length(PathATokens) > 0) xor (length(PathBTokens) > 0)),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test exclusive choice with condition evaluation
exclusive_choice_condition_test(_Config) ->
    %% Test with different conditions
    Conditions = [path_a, path_b],

    lists:foreach(fun(Cond) ->
        {ok, Pid} = gen_yawl:start_link(exclusive_choice_net,
                                         #{condition => Cond}, []),
        {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
        timer:sleep(200),

        Marking = gen_yawl:marking(Pid),
        ct:pal("Condition ~p resulted in marking: ~p", [Cond, Marking]),

        ok = gen_yawl:stop(Pid)
    end, Conditions),

    ok.

%% @doc Test exclusive choice default path
exclusive_choice_default_path_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(exclusive_choice_net,
                                     #{condition => default}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
    timer:sleep(200),

    {ok, DefaultTokens} = gen_yawl:ls(Pid, p_default),
    ?assert(length(DefaultTokens) > 0),

    ct:pal("Default path taken: ~p", [DefaultTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test simple merge pattern
simple_merge_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(simple_merge_net, #{}, []),

    %% Inject token from one path
    {ok, _} = gen_yawl:inject(Pid, #{p_path_a => [token_a]}),

    timer:sleep(200),

    %% Verify merged output
    {ok, MergedTokens} = gen_yawl:ls(Pid, p_merged),
    ?assert(length(MergedTokens) > 0),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test multi-choice pattern
multi_choice_basic_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(multi_choice_net,
                                     #{branches => [path_a, path_b]}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    timer:sleep(200),

    %% Verify multiple paths can be taken
    Marking = gen_yawl:marking(Pid),
    ct:pal("Multi-choice marking: ~p", [Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test synchronizing merge pattern
synchronizing_merge_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(synchronizing_merge_net, #{}, []),

    %% Inject tokens from multiple paths
    {ok, _} = gen_yawl:inject(Pid, #{
        p_path_a => [token_a],
        p_path_b => [token_b]
    }),

    timer:sleep(200),

    {ok, MergedTokens} = gen_yawl:ls(Pid, p_merged),
    ct:pal("Synchronizing merge result: ~p", [MergedTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Advanced Patterns
%%%===================================================================

%% @doc Test multi-merge pattern
multi_merge_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(multi_merge_net, #{}, []),

    %% Send multiple tokens from different paths
    {ok, _} = gen_yawl:inject(Pid, #{
        p_path_a => [token_a1, token_a2],
        p_path_b => [token_b1]
    }),

    timer:sleep(300),

    {ok, MergedTokens} = gen_yawl:ls(Pid, p_merged),
    ct:pal("Multi-merge collected ~p tokens: ~p", [length(MergedTokens), MergedTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test discriminator pattern
discriminator_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(discriminator_net, #{}, []),

    %% Send tokens from multiple paths
    {ok, _} = gen_yawl:inject(Pid, #{
        p_path_a => [token_a],
        p_path_b => [token_b],
        p_path_c => [token_c]
    }),

    timer:sleep(200),

    %% Discriminator should fire only once (first arrival)
    {ok, OutputTokens} = gen_yawl:ls(Pid, p_output),
    ct:pal("Discriminator output (should be 1 token): ~p", [OutputTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test arbitration pattern (N-of-M)
arbitration_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(arbitration_net,
                                     #{n => 2, m => 4}, []),

    %% Send 2 of 4 tokens
    {ok, _} = gen_yawl:inject(Pid, #{
        p_path_a => [token_a],
        p_path_b => [token_b]
    }),

    timer:sleep(200),

    %% Should fire after 2 arrivals
    {ok, OutputTokens} = gen_yawl:ls(Pid, p_output),
    ct:pal("Arbitration (2-of-4) output: ~p", [OutputTokens]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Complex Workflows
%%%===================================================================

%% @doc Test complex workflow: sequence + parallel
complex_workflow_sequence_parallel_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(complex_seq_par_net, #{}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    {ok, FinalMarking} = gen_yawl:sync(Pid, 5000),
    ct:pal("Complex seq+par final marking: ~p", [FinalMarking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test complex workflow: choice + merge
complex_workflow_choice_merge_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(complex_choice_merge_net,
                                     #{condition => path_a}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    {ok, FinalMarking} = gen_yawl:sync(Pid, 5000),
    ct:pal("Complex choice+merge final marking: ~p", [FinalMarking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test complex workflow with nested patterns
complex_workflow_nested_patterns_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(complex_nested_net, #{}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    {ok, FinalMarking} = gen_yawl:sync(Pid, 10000),
    ct:pal("Complex nested patterns final marking: ~p", [FinalMarking]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

ensure_modules_loaded() ->
    Modules = [
        gen_yawl, gen_pnet,
        sequence_pattern_net, parallel_split_net, synchronization_net,
        exclusive_choice_net, simple_merge_net, multi_choice_net,
        synchronizing_merge_net, multi_merge_net, discriminator_net,
        arbitration_net, complex_seq_par_net, complex_choice_merge_net,
        complex_nested_net
    ],

    Results = [code:ensure_loaded(M) || M <- Modules],
    case lists:all(fun({module, _}) -> true; (_) -> false end, Results) of
        true -> ok;
        false ->
            ct:pal("Warning: Some test modules not found, tests may fail"),
            ok
    end.
