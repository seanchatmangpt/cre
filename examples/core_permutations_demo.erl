%% @doc
%% Core Permutations Demo - 80/20 Usage Patterns
%%
%% This module demonstrates the most common permutation patterns used
%% in 80% of real-world YAWL workflows:
%% 1. Sequential workflows
%% 2. Parallel execution
%% 3. Conditional branching
%% 4. Error handling and recovery
%%
%% Usage:
%% 1. Compile: erlc -I ../include -o ../src ../examples/core_permutations_demo.erl
%% 2. Run: erl -pa ../src -s core_permutations_demo demo -s init stop
%% @end
-module(core_permutations_demo).

-export([demo/0, demo_sequential/0, demo_parallel/0,
         demo_conditional/0, demo_error_handling/0]).

%%====================================================================
%% Core Permutation Demonstrations
%%====================================================================

%% @doc
%% Run all core permutation demos
%% @end
demo() ->
    io:format("=== Core Permutations 80/20 Demo ===~n"),

    %% Basic operations
    io:format("~n~n=== 1. Basic Marking Operations ===~n"),
    demo_basic_operations(),

    %% Sequential workflow
    io:format("~n~n=== 2. Sequential Workflow ===~n"),
    demo_sequential(),

    %% Parallel execution
    io:format("~n~n=== 3. Parallel Execution ===~n"),
    demo_parallel(),

    %% Conditional branching
    io:format("~n~n=== 4. Conditional Branching ===~n"),
    demo_conditional(),

    %% Error handling
    io:format("~n~n=== 5. Error Handling ===~n"),
    demo_error_handling(),

    io:format("~n=== Demo Complete ===~n").

%%====================================================================
%% Basic Operations
%%====================================================================

demo_basic_operations() ->
    %% Create empty marking
    M0 = pnet_marking:new([start, process, validate, complete]),
    io:format("Empty marking: ~p~n", [M0]),

    %% Add initial token
    M1 = pnet_marking:set(M0, start, [init]),
    {ok, StartTokens} = pnet_marking:get(M1, start),
    io:format("Start tokens: ~p~n", [StartTokens]),

    %% Define and execute a single transition
    Move = #{mode => #{start => [init]}, produce => #{process => [work]}},
    {ok, M2} = pnet_marking:apply(M1, Move),
    {ok, ProcessTokens} = pnet_marking:get(M2, process),
    io:format("After move: ~p~n", [ProcessTokens]),

    %% Continue to completion
    Move2 = #{mode => #{process => [work]}, produce => #{complete => [done]}},
    {ok, M3} = pnet_marking:apply(M2, Move2),
    {ok, FinalTokens} = pnet_marking:get(M3, complete),
    io:format("Final state: ~p~n", [FinalTokens]).

%%====================================================================
%% Sequential Workflow Demo
%%====================================================================

demo_sequential() ->
    %% Define a 3-step sequential workflow
    Workflow = [
        #{mode => #{start => [init]}, produce => #{step1 => [task1]}},
        #{mode => #{step1 => [task1]}, produce => #{step2 => [task2]}},
        #{mode => #{step2 => [task2]}, produce => #{complete => [done]}}
    ],

    %% Execute sequential workflow
    FinalState = execute_sequential(Workflow,
                                  pnet_marking:new([start, step1, step2, complete])),

    {ok, CompleteTokens} = pnet_marking:get(FinalState, complete),
    io:format("Sequential workflow completed with: ~p~n", [CompleteTokens]).

%% Execute steps in sequence
execute_sequential([], Marking) -> Marking;
execute_sequential([Step | Rest], Marking) ->
    {ok, NewMarking} = pnet_marking:apply(Marking, Step),
    io:format("Step executed: ~p~n", [NewMarking]),
    execute_sequential(Rest, NewMarking).

%%====================================================================
%% Parallel Execution Demo
%%====================================================================

demo_parallel() ->
    %% Define parallel branches
    BranchFunctions = [
        fun(X) -> X * 2 end,
        fun(X) -> X + 10 end,
        fun(X) -> X div 2 end
    ],

    %% Execute in parallel
    Result = parallel_execution(BranchFunctions, 8),
    io:format("Parallel execution result: ~p~n", [Result]).

%% Execute multiple functions in parallel
parallel_execution(Funs, Input) ->
    BranchCount = length(Funs),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all branches
    Pids = lists:map(fun({Fun, Index}) ->
        spawn(fun() ->
            Result = Fun(Input),
            Parent ! {Ref, {branch_complete, Index}, Result}
        end)
    end, lists:zip(Funs, lists:seq(1, BranchCount))),

    %% Wait for all branches
    wait_all_results(Ref, Pids, BranchCount, #{}).

%% Wait for all parallel results
wait_all_results(_Ref, _Pids, 0, Acc) -> {ok, Acc};
wait_all_results(Ref, Pids, Remaining, Acc) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            wait_all_results(Ref, Pids, Remaining - 1,
                            Acc#{Index => Result});
        {Ref, {branch_error, Index, Error}} ->
            {error, {branch_failed, Index, Error}}
    after 5000 ->
        {error, timeout}
    end.

%%====================================================================
%% Conditional Branching Demo
%%====================================================================

demo_conditional() ->
    %% Test different scenarios
    test_order_processing(1500),   % Premium
    test_order_processing(500),    % Standard
    test_order_processing(50),     % Basic
    test_order_processing("invalid").

% Order processing with conditional branching
process_order(Order) ->
    case Order of
        Amount when is_number(Amount), Amount > 1000 ->
            process_premium_order(Amount);
        Amount when is_number(Amount), Amount > 100 ->
            process_standard_order(Amount);
        Amount when is_number(Amount) ->
            process_basic_order(Amount);
        _ ->
            {error, invalid_order}
    end.

% Test wrapper
test_order_processing(Order) ->
    Result = process_order(Order),
    io:format("Order ~p processed as: ~p~n", [Order, Result]).

% Premium order processing
process_premium_order(Amount) ->
    io:format("Processing premium order: ~p~n", [Amount]),
    {ok, #{type => premium, amount => Amount, discount => 0.15}}.

% Standard order processing
process_standard_order(Amount) ->
    io:format("Processing standard order: ~p~n", [Amount]),
    {ok, #{type => standard, amount => Amount, discount => 0.05}}.

% Basic order processing
process_basic_order(Amount) ->
    io:format("Processing basic order: ~p~n", [Amount]),
    {ok, #{type => basic, amount => Amount, discount => 0.0}}.

%%====================================================================
%% Error Handling Demo
%%====================================================================

demo_error_handling() ->
    %% Test various error scenarios
    io:format("Testing insufficient tokens...~n"),
    test_insufficient_tokens(),

    io:format("Testing invalid transitions...~n"),
    test_invalid_transition(),

    io:format("Testing timeout scenarios...~n"),
    test_timeout_handling(),

    io:format("Testing recovery...~n"),
    test_error_recovery().

% Test insufficient tokens error
test_insufficient_tokens() ->
    M0 = pnet_marking:new([start, task]),
    M1 = pnet_marking:set(M0, start, [one_token]),

    % Try to consume more tokens than available
    Move = #{mode => #{start => [missing_token]}, produce => #{task => []}},
    case pnet_marking:apply(M1, Move) of
        {error, insufficient} ->
            io:format("✓ Insufficient tokens error handled correctly~n");
        {ok, _} ->
            io:format("✗ Unexpected success~n")
    end.

% Test invalid transition handling
test_invalid_transition() ->
    M0 = pnet_marking:new([start, end]),

    % Try to fire transition without proper marking
    Move = #{mode => #{missing_place => [token]}, produce => #{end => []}},
    case pnet_marking:apply(M0, Move) of
        {error, insufficient} ->
            io:format("✓ Invalid transition handled correctly~n");
        {ok, _} ->
            io:format("✗ Unexpected success~n")
    end.

% Test timeout handling
test_timeout_handling() ->
    SlowOperation = fun() ->
        timer:sleep(2000),
        result
    end,

    case with_timeout(SlowOperation, 1000, fallback) of
        fallback ->
            io:format("✓ Timeout handled correctly~n");
        _ ->
            io:format("✗ Timeout not handled~n")
    end.

% Test error recovery
test_error_recovery() ->
    % Start with invalid marking
    InvalidMarking = #{not_a_place => [token]},

    case recover_from_invalid(InvalidMarking) of
        {ok, ValidMarking} ->
            io:format("✓ Recovery successful: ~p~n", [ValidMarking]);
        {error, Reason} ->
            io:format("✗ Recovery failed: ~p~n", [Reason])
    end.

% Timeout wrapper
with_timeout(Fun, Timeout, Fallback) ->
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Result = Fun(),
        self() ! {Ref, {result, Result}}
    end),

    receive
        {Ref, {result, Res}} -> Res;
    after Timeout ->
        Fallback()
    end.

% Error recovery function
recover_from_invalid(Marking) ->
    case pnet_types:is_marking(Marking) of
        true ->
            {ok, Marking};
        false ->
            % Create valid marking from existing keys
            ValidPlaces = maps:keys(Marking),
            {ok, pnet_marking:new(ValidPlaces)}
    end.

%%====================================================================
%% Utility Functions
%%====================================================================

% Helper for quick marking display
print_marking(Marking) ->
    io:format("Marking state:~n"),
    lists:foreach(fun({Place, Tokens}) ->
        io:format("  ~p: ~p~n", [Place, Tokens])
    end, maps:to_list(Marking)).

% Helper for safe marking operations
safe_apply(Marking, Move) ->
    try
        {ok, NewMarking} = pnet_marking:apply(Marking, Move),
        {ok, NewMarking}
    catch
        Error:Reason ->
            io:format("Error applying move: ~p:~p~n", [Error, Reason]),
            {error, Reason}
    end.