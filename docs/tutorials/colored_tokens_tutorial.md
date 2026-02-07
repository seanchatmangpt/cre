# Colored Tokens Tutorial

**Working with data-carrying tokens in YAWL workflows**

---

## What You'll Learn

In this tutorial, you will:

- Understand colored token structure and types
- Create and manipulate tokens
- Use tokens for data flow between tasks
- Implement token transformation patterns
- Handle token lifecycles and expiration

**Time required**: 45 minutes
**Prerequisites**: Completed [Basic Patterns Tutorial](basic_patterns_tutorial.md)

---

## Part 1: Understanding Colored Tokens

### What are Colored Tokens?

In classical Petri nets, tokens are unmarked - they only indicate presence. Colored tokens (from Colored Petri Nets) carry data payloads, enabling data-aware workflows.

**Comparison:**

| Aspect | Classical Tokens | Colored Tokens |
|---------|------------------|----------------|
| **Purpose** | Indicate state | Indicate state + carry data |
| **Structure** | Simple marker | Record with fields |
| **Capabilities** | Presence/absence | Complex data flow |

### Token Structure

```erlang
-record(colored_token, {
    id          :: binary(),              % Unique identifier
    type        :: token_type(),          % Token type classification
    payload     :: term(),                % Data payload
    metadata    :: map(),                 % Additional metadata
    created_at  :: integer(),             % Creation timestamp
    expires_at  :: integer() | undefined, % Optional expiration
    trace_id    :: binary() | undefined   % OTEL trace correlation
}).
```

### Token Types

```erlang
-type token_type() ::
    initial              |  % Workflow start token
    task_start           |  % Task beginning
    task_complete        |  % Task finished
    branch               |  % Branch routing
    merge                |  % Merge synchronization
    data                 |  % Data carrying
    control              |  % Control flow
    cancel               |  % Cancellation
    error                |  % Error condition
    milestone            |  % Milestone reached
    completion.          %  % Workflow completion
```

---

## Part 2: Creating Tokens

### Helper Module for Token Creation

Create `token_helpers.erl`:

```erlang
%% @doc Colored token creation and manipulation utilities
-module(token_helpers).

%% Token creation API
-export([
    initial_token/1,
    initial_token/2,
    task_token/2,
    task_token/3,
    completion_token/1,
    completion_token/2,
    branch_token/2,
    data_token/2,
    error_token/2,
    control_token/1,
    cancel_token/1
]).

%% Token inspection API
-export([
    get_token_type/1,
    get_payload/1,
    get_metadata/1,
    is_expired/1,
    token_age/1
]).

%% Token manipulation API
-export([
    transform_payload/2,
    merge_tokens/1,
    clone_token/1,
    update_metadata/2,
    set_expiration/2
]).

%%====================================================================
%% Token Creation Functions
%%====================================================================

%% @doc Creates an initial workflow token
-spec initial_token(Data :: term()) -> colored_token().
initial_token(Data) ->
    initial_token(Data, #{}).

initial_token(Data, Metadata) when is_map(Metadata) ->
    #colored_token{
        id = generate_id(),
        type = initial,
        payload = Data,
        metadata = Metadata,
        created_at = erlang:system_time(millisecond),
        trace_id = maybe_generate_trace_id()
    }.

%% @doc Creates a task start token
-spec task_token(TaskId :: binary(), InputData :: term()) -> colored_token().
task_token(TaskId, InputData) ->
    task_token(TaskId, InputData, #{}).

task_token(TaskId, InputData, Metadata) ->
    #colored_token{
        id = generate_id(),
        type = task_start,
        payload = #{
            task_id => TaskId,
            input => InputData
        },
        metadata = Metadata,
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a task completion token
-spec completion_token(TaskId :: binary()) -> colored_token().
completion_token(TaskId) ->
    completion_token(TaskId, #{}).

completion_token(TaskId, Metadata) ->
    #colored_token{
        id = generate_id(),
        type = task_complete,
        payload = #{
            task_id => TaskId,
            completed_at => erlang:system_time(millisecond)
        },
        metadata = Metadata,
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a branch token for parallel execution
-spec branch_token(BranchId :: atom(), Data :: term()) -> colored_token().
branch_token(BranchId, Data) ->
    #colored_token{
        id = generate_id(),
        type = branch,
        payload = #{
            branch_id => BranchId,
            data => Data
        },
        metadata = #{branch => BranchId},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a data-carrying token
-spec data_token(DataType :: atom(), Data :: term()) -> colored_token().
data_token(DataType, Data) ->
    #colored_token{
        id = generate_id(),
        type = data,
        payload = #{
            data_type => DataType,
            value => Data
        },
        metadata = #{data_type => DataType},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates an error token
-spec error_token(Reason :: term(), Stack :: term()) -> colored_token().
error_token(Reason, Stack) ->
    #colored_token{
        id = generate_id(),
        type = error,
        payload = #{
            reason => Reason,
            stack => Stack,
            timestamp => erlang:system_time(millisecond)
        },
        metadata = #{error => true},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a control token
-spec control_token(ControlType :: atom()) -> colored_token().
control_token(ControlType) ->
    #colored_token{
        id = generate_id(),
        type = control,
        payload = #{control => ControlType},
        metadata = #{},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a cancel token
-spec cancel_token(Reason :: term()) -> colored_token().
cancel_token(Reason) ->
    #colored_token{
        id = generate_id(),
        type = cancel,
        payload = #{reason => Reason},
        metadata = #{cancelled_at => erlang:system_time(millisecond)},
        created_at = erlang:system_time(millisecond)
    }.

%%====================================================================
%% Token Inspection Functions
%%====================================================================

%% @doc Extracts token type
-spec get_token_type(colored_token()) -> token_type().
get_token_type(#colored_token{type = Type}) ->
    Type.

%% @doc Extracts token payload
-spec get_payload(colored_token()) -> term().
get_payload(#colored_token{payload = Payload}) ->
    Payload.

%% @doc Extracts token metadata
-spec get_metadata(colored_token()) -> map().
get_metadata(#colored_token{metadata = Metadata}) ->
    Metadata.

%% @doc Checks if token is expired
-spec is_expired(colored_token()) -> boolean().
is_expired(#colored_token{expires_at = undefined}) ->
    false;
is_expired(#colored_token{expires_at = ExpiresAt}) ->
    erlang:system_time(millisecond) >= ExpiresAt.

%% @doc Gets token age in milliseconds
-spec token_age(colored_token()) -> non_neg_integer().
token_age(#colored_token{created_at = CreatedAt}) ->
    erlang:system_time(millisecond) - CreatedAt.

%%====================================================================
%% Token Manipulation Functions
%%====================================================================

%% @doc Transforms token payload with a function
-spec transform_payload(colored_token(), fun((term()) -> term())) -> colored_token().
transform_payload(Token = #colored_token{payload = Payload}, TransformFun) ->
    Token#colored_token{payload = TransformFun(Payload)}.

%% @doc Merges multiple tokens into one
-spec merge_tokens([colored_token()]) -> colored_token().
merge_tokens(Tokens) when is_list(Tokens), length(Tokens) > 0 ->
    MergedPayload = merge_payloads([T#colored_token.payload || T <- Tokens]),
    MergedMetadata = merge_metadata([T#colored_token.metadata || T <- Tokens]),

    #colored_token{
        id = generate_id(),
        type = merge,
        payload = MergedPayload,
        metadata = MergedMetadata#{
            merged_from => [T#colored_token.id || T <- Tokens],
            merged_at => erlang:system_time(millisecond)
        },
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Clones a token with new ID
-spec clone_token(colored_token()) -> colored_token().
clone_token(Token = #colored_token{}) ->
    Token#colored_token{
        id = generate_id(),
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Updates token metadata
-spec update_metadata(colored_token(), map() | fun((map()) -> map())) -> colored_token().
update_metadata(Token = #colored_token{metadata = Metadata}, UpdateMap) when is_map(UpdateMap) ->
    Token#colored_token{metadata = maps:merge(Metadata, UpdateMap)};
update_metadata(Token = #colored_token{metadata = Metadata}, UpdateFun) when is_function(UpdateFun) ->
    Token#colored_token{metadata = UpdateFun(Metadata)}.

%% @doc Sets token expiration time
-spec set_expiration(colored_token(), integer() | infinity) -> colored_token().
set_expiration(Token = #colored_token{}, infinity) ->
    Token#colored_token{expires_at = undefined};
set_expiration(Token = #colored_token{}, Milliseconds) when is_integer(Milliseconds) ->
    ExpiresAt = erlang:system_time(millisecond) + Milliseconds,
    Token#colored_token{expires_at = ExpiresAt}.

%%====================================================================
%% Internal Functions
%%====================================================================

generate_id() ->
    <<(integer_to_binary(erlang:unique_integer([positive])))/binary,
      "_",(integer_to_binary(erlang:system_time(millisecond)))/binary>>.

maybe_generate_trace_id() ->
    case application:get_env(opentelemetry, enabled) of
        {ok, true} ->
            %% In real implementation, use OTEL trace ID
            generate_id();
        _ ->
            undefined
    end.

merge_payloads(Payloads) ->
    lists:foldl(fun(Payload, Acc) ->
        maps:merge(Acc, Payload)
    end, #{}, Payloads).

merge_metadata(MetadataList) ->
    lists:foldl(fun(Metadata, Acc) ->
        maps:merge(Acc, Metadata)
    end, #{}, MetadataList).
```

---

## Part 3: Data Flow with Colored Tokens

### Example: Order Processing Pipeline

Create a workflow that uses colored tokens to pass order data through tasks:

```erlang
%% @doc Order processing with colored tokens
-module(order_processing_tokens).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

-export([run/1]).

-include_lib("include/token_types.hrl").

%% API
run(OrderData) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{order => OrderData}),
    wait_for_completion(Pid, 30000).

wait_for_completion(Pid, Timeout) ->
    receive
        {order_processed, Result} -> {ok, Result};
        {order_failed, Reason} -> {error, Reason}
    after Timeout -> timeout
    end.

%% gen_pnet callbacks
place_lst() ->
    [p_input, p_validating, p_validated, p_processing, p_processed, p_complete].

trsn_lst() -> [t_validate, t_approve, t_process, t_finalize].

init_marking(p_input, UsrInfo) ->
    Order = maps:get(order, UsrInfo, #{}),
    [token_helpers:initial_token(Order)];
init_marking(_, _) -> [].

preset(t_validate) -> [p_input];
preset(t_approve) -> [p_validating];
preset(t_process) -> [p_validated];
preset(t_finalize) -> [p_processed];
preset(_) -> [].

is_enabled(t_validate, #{p_input := [T]}, _) when T =/= [] -> true;
is_enabled(t_approve, #{p_validating := [T]}, _) when T =/= [] -> true;
is_enabled(t_process, #{p_validated := [T]}, _) when T =/= [] -> true;
is_enabled(t_finalize, #{p_processed := [T]}, _) when T =/= [] -> true;
is_enabled(_, _, _) -> false.

fire(t_validate, #{p_input := [Token]}, _) ->
    Order = token_helpers:get_payload(Token),

    %% Validate order
    case validate_order(Order) of
        {ok, ValidatedOrder} ->
            io:format("Order validated: ~p~n", [ValidatedOrder]),

            %% Create validation token
            ValidationToken = token_helpers:data_token(
                validated_order,
                ValidatedOrder
            ),

            {produce, #{
                p_input => [],
                p_validating => [ValidationToken]
            }};
        {error, Reason} ->
            io:format("Order validation failed: ~p~n", [Reason]),
            ErrorToken = token_helpers:error_token(Reason, []),
            self() ! {order_failed, Reason},
            {produce, #{p_input => [], p_validating => [ErrorToken]}}
    end;

fire(t_approve, #{p_validating := [Token]}, _) ->
    %% Extract validated order
    ValidatedOrder = token_helpers:get_payload(Token),

    %% Add approval metadata
    ApprovedToken = token_helpers:update_metadata(Token, #{
        approved => true,
        approved_by => system,
        approved_at => erlang:system_time(millisecond)
    }),

    io:format("Order approved~n"),

    {produce, #{
        p_validating => [],
        p_validated => [ApprovedToken]
    }};

fire(t_process, #{p_validated := [Token]}, _) ->
    Order = token_helpers:get_payload(Token),

    %% Process order (calculate totals, etc.)
    ProcessedOrder = process_order(Order),

    io:format("Order processed: ~p~n", [ProcessedOrder]),

    ProcessedToken = token_helpers:transform_payload(Token, fun(_) ->
        ProcessedOrder
    end),

    {produce, #{
        p_validated => [],
        p_processed => [ProcessedToken]
    }};

fire(t_finalize, #{p_processed := [Token]}, _) ->
    Order = token_helpers:get_payload(Token),

    %% Create completion token with final results
    CompletionToken = token_helpers:completion_token(<<"order_processing">>, #{
        final_status => complete,
        order_id => maps:get(order_id, Order, undefined),
        total => maps:get(total, Order, 0)
    }),

    io:format("Order finalized: ~p~n", [CompletionToken]),

    self() ! {order_processed, token_helpers:get_payload(CompletionToken)},

    {produce, #{
        p_processed => [],
        p_complete => [CompletionToken]
    }};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

%% Internal functions
validate_order(Order) ->
    case maps:get(customer_id, Order, undefined) of
        undefined -> {error, missing_customer_id};
        CustomerId when is_binary(CustomerId) ->
            {ok, Order#{validation_status => valid}}
    end.

process_order(Order) ->
    Items = maps:get(items, Order, []),
    Subtotal = lists:foldl(fun(Item, Acc) ->
        maps:get(price, Item, 0) + Acc
    end, 0, Items),
    Tax = Subtotal * 0.1,
    Total = Subtotal + Tax,

    Order#{
        subtotal => Subtotal,
        tax => Tax,
        total => Total,
        processing_status => complete
    }.
```

---

## Part 4: Token Transformation Patterns

### Pattern 1: Sequential Data Transformation

Transform data as it flows through sequential tasks:

```erlang
%% @doc Sequential data transformation with tokens
-module(sequential_transform).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([run/2]).

-define(TOKEN(Type, Data), #colored_token{
    id => generate_id(),
    type => Type,
    payload => Data,
    created_at => erlang:system_time(millisecond)
}).

run(InputData, TransformFuns) when is_list(TransformFuns) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        input_data => InputData,
        transforms => TransformFuns,
        stage => 1
    })),
    wait_for_result(Pid).

wait_for_result(Pid) ->
    receive
        {transform_complete, Result} -> {ok, Result}
    after 30000 -> timeout
    end.

place_lst() -> [p_stage_1, p_stage_2, p_stage_3, p_done].
trsn_lst() -> [t_transform_1, t_transform_2, t_transform_3, t_complete].

init_marking(p_stage_1, UsrInfo) ->
    Data = maps:get(input_data, UsrInfo, undefined),
    [?TOKEN(initial, Data)];
init_marking(_, _) -> [].

preset(t_transform_1) -> [p_stage_1];
preset(t_transform_2) -> [p_stage_2];
preset(t_transform_3) -> [p_stage_3];
preset(t_complete) -> [p_done];
preset(_) -> [].

is_enabled(T, Mode, _) ->
    Place = case T of
        t_transform_1 -> p_stage_1;
        t_transform_2 -> p_stage_2;
        t_transform_3 -> p_stage_3;
        t_complete -> p_done
    end,
    case maps:get(Place, Mode, []) of
        [_] -> true;
        _ -> false
    end.

fire(t_transform_1, #{p_stage_1 := [Token]}, UsrInfo) ->
    apply_transform(Token, 1, UsrInfo, p_stage_2);
fire(t_transform_2, #{p_stage_2 := [Token]}, UsrInfo) ->
    apply_transform(Token, 2, UsrInfo, p_stage_3);
fire(t_transform_3, #{p_stage_3 := [Token]}, UsrInfo) ->
    apply_transform(Token, 3, UsrInfo, p_done);
fire(t_complete, #{p_done := [Token]}, _) ->
    self() ! {transform_complete, token_helpers:get_payload(Token)},
    {produce, #{p_done => []}};
fire(_, _, _) -> abort.

apply_transform(Token, Stage, UsrInfo, NextPlace) ->
    Transforms = maps:get(transforms, UsrInfo, []),
    TransformFun = lists:nth(Stage, Transforms),

    InputData = token_helpers:get_payload(Token),
    OutputData = TransformFun(InputData),

    io:format("Stage ~p transform: ~p -> ~p~n", [Stage, InputData, OutputData]),

    NewToken = ?TOKEN(transformed, OutputData),
    CurrentPlace = element(2, element(Stage, lists:zip(lists:seq(1, 3), [p_stage_1, p_stage_2, p_stage_3]))),

    {produce, #{
        CurrentPlace => [],
        NextPlace => [NewToken]
    }}.

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.
```

### Usage Example:

```erlang
%% Define transformation pipeline
Transforms = [
    fun(Data) -> maps:put(step1, Data * 2, Data) end,
    fun(Data) -> maps:put(step2, Data + 10, Data) end,
    fun(Data) -> maps:put(step3, Data * 3, Data) end
],

%% Run the pipeline
sequential_transform:run(5, Transforms).
%% Result: 5 -> step1:10 -> step2:20 -> step3:60
```

---

### Pattern 2: Parallel Data Distribution

Distribute data to multiple branches using branch tokens:

```erlang
%% @doc Parallel data distribution with tokens
-module(parallel_distribution).
-behaviour(gen_pnet).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([run/3]).

-define(TOKEN(Type, Data), #colored_token{
    id => generate_id(),
    type => Type,
    payload => Data,
    created_at => erlang:system_time(millisecond)
}).

run(InputData, BranchCount, BranchFun) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        input_data => InputData,
        branch_count => BranchCount,
        branch_function => BranchFun
    })),
    wait_for_results(Pid).

wait_for_results(Pid) ->
    receive
        {distribution_complete, Results} -> {ok, Results}
    after 30000 -> timeout
    end.

place_lst() -> [p_input, p_branch_1, p_branch_2, p_branch_3, p_collector, p_done].
trsn_lst() -> [t_distribute, t_collect].

init_marking(p_input, UsrInfo) ->
    Data = maps:get(input_data, UsrInfo, undefined),
    [?TOKEN(initial, Data)];
init_marking(_, _) -> [].

preset(t_distribute) -> [p_input];
preset(t_collect) -> [p_branch_1, p_branch_2, p_branch_3];
preset(_) -> [].

is_enabled(t_distribute, #{p_input := [_]}, _) -> true;
is_enabled(t_collect, Mode, _) ->
    lists:any(fun(P) -> maps:get(P, Mode, []) =/= [] end,
               [p_branch_1, p_branch_2, p_branch_3]);
is_enabled(_, _, _) -> false.

fire(t_distribute, #{p_input := [Token]}, UsrInfo) ->
    Data = token_helpers:get_payload(Token),
    Count = maps:get(branch_count, UsrInfo, 3),

    %% Create branch tokens with data
    Branches = lists:map(fun(N) ->
        token_helpers:branch_token(
            list_to_existing_atom("branch_" ++ integer_to_list(N)),
            maps:put(branch_num, N, Data)
        )
    end, lists:seq(1, Count)),

    io:format("Distributed data to ~p branches~n", [Count]),

    %% Distribute to branch places
    lists:foldl(fun({BT, BP}, Acc) ->
        maps:put(BP, [BT], Acc)
    end, #{p_input => []}, lists:zip(Branches, [p_branch_1, p_branch_2, p_branch_3]));

fire(t_collect, Mode, UsrInfo) ->
    %% Collect all available branch results
    BranchResults = lists:filtermap(fun(P) ->
        case maps:get(P, Mode, []) of
            [Token|_] ->
                BranchId = maps:get(branch_id, Token#colored_token.payload, undefined),
                Result = process_branch(Token, UsrInfo),
                {true, {BranchId, Result}};
            _ -> false
        end
    end, [p_branch_1, p_branch_2, p_branch_3]),

    %% Check if all branches complete
    Count = maps:get(branch_count, UsrInfo, 3),
    case length(BranchResults) of
        N when N >= Count ->
            io:format("All branches collected: ~p results~n", [BranchResults]),
            self() ! {distribution_complete, BranchResults},
            {produce, maps:from_list([{P, []} || P <- [p_branch_1, p_branch_2, p_branch_3, p_collector, p_done]])};
        _ ->
            %% Partial collection
            io:format("Partial collection: ~p/~p~n", [length(BranchResults), Count]),
            {produce, maps:from_list([{P, []} || P <- [p_branch_1, p_branch_2, p_branch_3]])}
    end;

fire(_, _, _) -> abort.

process_branch(Token, UsrInfo) ->
    BranchFun = maps:get(branch_function, UsrInfo),
    Data = Token#colored_token.payload,
    BranchFun(Branch, Data).

generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.
```

---

## Part 5: Token Lifecycle Management

### Expiration Handling

```erlang
%% @doc Token expiration handling
-module(token_expiration).

-export([check_token/1, renew_token/2, cancel_token/1]).

%% @doc Checks if token is expired
-spec check_token(colored_token()) ->
    {ok, colored_token()} | {error, expired}.
check_token(Token = #colored_token{expires_at = undefined}) ->
    {ok, Token};
check_token(Token = #colored_token{expires_at = ExpiresAt}) ->
    case erlang:system_time(millisecond) of
        Now when Now >= ExpiresAt ->
            {error, expired};
        Now ->
            {ok, Token}
    end.

%% @doc Renews token expiration
-spec renew_token(colored_token(), Milliseconds :: integer()) -> colored_token().
renew_token(Token, Milliseconds) ->
    ExpiresAt = erlang:system_time(millisecond) + Milliseconds,
    Token#colored_token{expires_at = ExpiresAt}.

%% @doc Cancels token (sets as expired)
-spec cancel_token(colored_token()) -> colored_token().
cancel_token(Token) ->
    Token#colored_token{
        expires_at = erlang:system_time(millisecond) - 1,
        type = cancel
    }.
```

---

## Exercises

### Exercise 1: Token Validation Pipeline

Create a workflow that:

1. Takes input data as an initial token
2. Validates the data (creates validated token or error token)
3. Transforms the data if valid (creates transformed token)
4. Completes with final result token

### Exercise 2: Multi-Stage Token Aggregation

Create a workflow that:

1. Splits input into 3 branches using branch tokens
2. Each branch processes its token
3. Merges all tokens using `merge_tokens/1`
4. Returns aggregated result

### Exercise 3: Token Metadata Tracking

Implement token metadata tracking that records:

- Processing time at each stage
- Which tasks handled the token
- Error history (if any)
- Custom audit trail

---

## Summary: Token Design Best Practices

| Practice | Description | Example |
|----------|-------------|---------|
| **Type Safety** | Use appropriate token types for each stage | `task_start` for beginnings |
| **Immutability** | Don't modify existing tokens, create new ones | `clone_token/1` |
| **Expiration** | Set timeouts for time-sensitive tokens | `set_expiration/2` |
| **Metadata** | Store audit trail in metadata | timestamps, task IDs |
| **Error Handling** | Use error tokens for failures | `error_token/2` |

---

**Next Steps**: Continue to [Workflow Migration Tutorial](workflow_migration_tutorial.md) to learn about migrating from the old YAWL system.
