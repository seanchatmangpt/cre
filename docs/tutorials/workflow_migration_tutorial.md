# Workflow Migration Tutorial

**Migrating from old YAWL (gen_statem) to new gen_pnet-based YAWL**

---

## What You'll Learn

In this tutorial, you will:

- Understand the differences between gen_statem and gen_pnet backends
- Use the migration adapter for gradual transition
- Convert existing workflows to gen_pnet
- Run both backends in parallel for testing
- Complete the migration process

**Time required**: 60 minutes
**Prerequisites**: Completed [Getting Started](getting_started.md)

---

## Part 1: Understanding the Migration

### Why Migrate?

| Feature | gen_statem (Old) | gen_pnet (New) |
|---------|------------------|----------------|
| **Formal Model** | Ad-hoc state machine | Colored Petri nets |
| **Concurrency** | Manual coordination | Native parallel semantics |
| **Verification** | Limited | Soundness checking |
| **Data Flow** | Workitem-based | Token-based |
| **Performance** | Good | Better for parallel workloads |

### Migration Strategy

CRE supports three migration modes:

```erlang
%% config/sys.config
{yawl, [
    {pnet_backend, gen_pnet},  % gen_pnet | gen_statem | dual
    {pnet_migration, [
        {enable_dual_mode, true},
        {migration_complete, false},
        {compatibility_layer, true}
    ]}
]}.
```

**Modes:**

1. **gen_statem**: Old backend only (default)
2. **gen_pnet**: New backend only
3. **dual**: Both backends running

---

## Part 2: Using the Migration Adapter

### Adapter Module

The `yawl_pnet_adapter` module provides conversion functions:

```erlang
%% @doc Migration adapter between gen_statem and gen_pnet
-module(yawl_pnet_adapter).

%% Conversion functions
-export([
    statem_to_pnet/1,
    pnet_to_statem/1,
    workitem_to_token/1,
    token_to_workitem/1
]).

%% Migration functions
-export([
    migrate_workflow/1,
    can_migrate/1,
    migration_status/1,
    validate_migration/1
]).

%% @doc Converts gen_statem state to gen_pnet marking
-spec statem_to_pnet(State :: term()) ->
    #{atom() => [colored_token()]}.
statem_to_pnet(#{workitems := WorkItems, status := Status}) ->
    maps:fold(fun(_WorkitemId, Workitem, Acc) ->
        Place = case Workitem#workitem.status of
            enabled -> waiting;
            started -> active;
            completed -> completed;
            cancelled -> cancelled;
            failed -> error
        end,
        Token = workitem_to_token(Workitem),
        Acc#{Place => [Token | maps:get(Place, Acc, [])]}
    end, #{}, WorkItems).

%% @doc Converts gen_pnet marking to gen_statem state
-spec pnet_to_statem(Marking :: map()) -> map().
pnet_to_statem(Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        case Place of
            active -> lists:foldl(fun(T, A) -> add_workitem(T, active, A) end, Acc, Tokens);
            completed -> lists:foldl(fun(T, A) -> add_workitem(T, completed, A) end, Acc, Tokens);
            _ -> Acc
        end
    end, #{}, Marking).

%% @doc Converts workitem to token
-spec workitem_to_token(Workitem :: #workitem{}) -> colored_token().
workitem_to_token(Workitem) ->
    #colored_token{
        id = Workitem#workitem.id,
        type = task_start,
        payload = Workitem#workitem.data,
        metadata = #{
            task_id => Workitem#workitem.task_id,
            status => Workitem#workitem.status
        },
        created_at => Workitem#workitem.created_at
    }.

%% @doc Converts token to workitem
-spec token_to_workitem(Token :: colored_token()) -> #workitem{}.
token_to_workitem(Token = #colored_token{}) ->
    #workitem{
        id = Token#colored_token.id,
        task_id = maps:get(task_id, Token#colored_token.metadata, undefined),
        status = maps:get(status, Token#colored_token.metadata, enabled),
        data = Token#colored_token.payload,
        created_at = Token#colored_token.created_at
    }.

%% @doc Migrates a running workflow from gen_statem to gen_pnet
-spec migrate_workflow(WorkflowId :: binary()) ->
    {ok, pid()} | {error, term()}.
migrate_workflow(WorkflowId) ->
    case yawl_legacy_statem:get_state(WorkflowId) of
        {ok, LegacyState} ->
            %% Convert legacy state to pnet marking
            Marking = statem_to_pnet(LegacyState),

            %% Start new gen_pnet instance
            PatternModule = maps:get(pattern_module, LegacyState),
            Config = maps:get(config, LegacyState),

            case yawl_pnet:start_workflow(PatternModule, Config) of
                {ok, Pid} ->
                    %% Inject migrated marking
                    lists:foreach(fun({Place, Tokens}) ->
                        lists:foreach(fun(Token) ->
                            yawl_pnet:inject_token(Pid, Place, Token)
                        end, Tokens)
                    end, maps:to_list(Marking)),

                    %% Stop legacy instance
                    yawl_legacy_statem:stop(WorkflowId),
                    {ok, Pid};
                {error, Reason} ->
                    {error, {migration_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {state_not_found, Reason}}
    end.

%% @doc Checks if workflow can be migrated
-spec can_migrate(WorkflowId :: binary()) ->
    {ok, boolean()} | {error, term()}.
can_migrate(WorkflowId) ->
    case yawl_legacy_statem:get_state(WorkflowId) of
        {ok, State} ->
            %% Check if pattern module has gen_pnet implementation
            PatternModule = maps:get(pattern_module, State),
            case code:is_loaded(PatternModule) of
                false ->
                    case code:load_file(PatternModule) of
                        {module, _} -> check_pnet_implementation(PatternModule);
                        {error, Reason} -> {error, Reason}
                    end;
                _ ->
                    check_pnet_implementation(PatternModule)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Gets migration status
-spec migration_status(WorkflowId :: binary()) ->
    {ok, map()} | {error, term()}.
migration_status(WorkflowId) ->
    case can_migrate(WorkflowId) of
        {ok, true} ->
            {ok, #{
                workflow_id => WorkflowId,
                can_migrate => true,
                status => ready
            }};
        {ok, false} ->
            {ok, #{
                workflow_id => WorkflowId,
                can_migrate => false,
                status => pattern_not_available
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validates migration was successful
-spec validate_migration(WorkflowId :: binary()) ->
    {ok, map()} | {error, term()}.
validate_migration(WorkflowId) ->
    case yawl_pnet:get_case_state(whereis_workflow_pid(WorkflowId)) of
        {ok, PnetState} ->
            Marking = maps:get(marking, PnetState, #{}),
            {ok, #{
                workflow_id => WorkflowId,
                migration_valid => true,
                place_count => maps:size(Marking),
                token_count => lists:sum([length(Ts) || Ts <- maps:values(Marking)])
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

check_pnet_implementation(Module) ->
    %% Check if module exports required gen_pnet callbacks
    RequiredCallbacks = [
        {place_lst, 0},
        {trsn_lst, 0},
        {init_marking, 2},
        {preset, 1},
        {is_enabled, 3},
        {fire, 3}
    ],
    HasAll = lists:all(fun({Func, Arity}) ->
        erlang:function_exported(Module, Func, Arity)
    end, RequiredCallbacks),
    {ok, HasAll}.

add_workitem(Token, Status, Acc) ->
    Workitem = token_to_workitem(Token),
    maps:put(Workitem#workitem.id, Workitem#workitem{status = Status}, Acc).

whereis_workflow_pid(WorkflowId) ->
    case global:whereis_name({workflow, WorkflowId}) of
        undefined -> {error, not_found};
        Pid -> Pid
    end.
```

---

## Part 3: Migrating a Simple Workflow

### Step 1: Identify Legacy Workflow

```erlang
%% Old gen_statem workflow
-module(order_workflow_legacy).
-behaviour(gen_statem).

%% State functions
-export([waiting_for_payment/3, processing/3, shipping/3]).

%% API
-export([start/1]).

start(OrderData) ->
    gen_statem:start_link(?MODULE, OrderData, []).

callback_mode() -> state_functions.

init(OrderData) ->
    {ok, waiting_for_payment, #{order => OrderData}}.

waiting_for_payment(cast, {payment_complete, PaymentId}, Data) ->
    io:format("Payment ~p complete~n", [PaymentId]),
    {next_state, processing, Data#{payment_id => PaymentId}};
waiting_for_payment(_, _, Data) ->
    {keep_state, Data}.

processing(cast, {process_complete}, Data) ->
    io:format("Order processing complete~n"),
    {next_state, shipping, Data};
processing(_, _, Data) ->
    {keep_state, Data}.

shipping(cast, {ship_complete, Tracking}, Data) ->
    io:format("Shipped with tracking: ~p~n", [Tracking]),
    {stop, normal, Data#{tracking => Tracking}};
shipping(_, _, Data) ->
    {keep_state, Data}.
```

### Step 2: Create gen_pnet Equivalent

```erlang
%% New gen_pnet workflow
-module(order_workflow_pnet).
-behaviour(gen_pnet).

-export([
    place_lst/0, trsn_lst/0,
    init_marking/2, preset/1, is_enabled/3, fire/3,
    code_change/3, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2
]).

%% API
-export([start/1]).

-define(TOKEN(Type, Data), #colored_token{
    id => generate_id(),
    type => Type,
    payload => Data,
    created_at => erlang:system_time(millisecond)
}).

start(OrderData) ->
    gen_pnet:start_link(?MODULE, #{order => OrderData}).

%% gen_pnet callbacks
place_lst() ->
    [p_start, p_payment, p_processing, p_shipping, p_done].

trsn_lst() -> [t_start, t_pay, t_process, t_ship, t_complete].

init_marking(p_start, UsrInfo) ->
    Order = maps:get(order, UsrInfo, #{}),
    [?TOKEN(initial, Order)];
init_marking(_, _) -> [].

preset(t_start) -> [p_start];
preset(t_pay) -> [p_payment];
preset(t_process) -> [p_processing];
preset(t_ship) -> [p_shipping];
preset(t_complete) -> [p_done];
preset(_) -> [].

is_enabled(t_start, #{p_start := [T]}, _) when T =/= [] -> true;
is_enabled(t_pay, #{p_payment := [T]}, _) when T =/= [] -> true;
is_enabled(t_process, #{p_processing := [T]}, _) when T =/= [] -> true;
is_enabled(t_ship, #{p_shipping := [T]}, _) when T =/= [] -> true;
is_enabled(t_complete, #{p_done := [T]}, _) when T =/= [] -> true;
is_enabled(_, _, _) -> false.

fire(t_start, _, _) ->
    {produce, #{p_start => [], p_payment => [?TOKEN(awaiting_payment, #{})]}};

fire(t_pay, #{p_payment := [Token]}, _) ->
    %% Trigger payment
    PaymentId = generate_payment_id(),
    io:format("Payment ~p initiated~n", [PaymentId]),

    NewToken = ?TOKEN(paid, #{
        payment_id => PaymentId,
        paid_at => erlang:system_time(millisecond)
    }),

    {produce, #{p_payment => [], p_processing => [NewToken]}};

fire(t_process, #{p_processing := [Token]}, _) ->
    io:format("Processing order~n"),

    NewToken = ?TOKEN(processed, #{
        processed_at => erlang:system_time(millisecond)
    }),

    {produce, #{p_processing => [], p_shipping => [NewToken]}};

fire(t_ship, #{p_shipping := [Token]}, _) ->
    Tracking = generate_tracking(),
    io:format("Shipping with tracking: ~p~n", [Tracking]),

    NewToken = ?TOKEN(shipped, #{
        tracking => Tracking,
        shipped_at => erlang:system_time(millisecond)
    }),

    {produce, #{p_shipping => [], p_done => [NewToken]}};

fire(t_complete, #{p_done := [Token]}, _) ->
    io:format("Order workflow complete!~n"),
    {produce, #{p_done => []}};

fire(_, _, _) -> abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast({payment_complete, PaymentId}, State) ->
    %% Handle payment completion
    io:format("Payment ~p complete~n", [PaymentId]),
    {noreply, State};
handle_cast({process_complete}, State) ->
    {noreply, State};
handle_cast({ship_complete, Tracking}, State) ->
    {noreply, State};
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.

%% Internal
generate_id() -> <<(integer_to_binary(erlang:unique_integer()))/binary>>.
generate_payment_id() -> <<"PAY_", (binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>.
generate_tracking() -> <<"TRK_", (binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>.
```

---

## Part 4: Running Dual Mode for Testing

### Configuration

```erlang
%% config/dual_mode.config
[
    {yawl, [
        {pnet_backend, dual},
        {pnet_migration, [
            {enable_dual_mode, true},
            {compare_results, true},
            {migration_complete, false}
        ]}
    ]}
].
```

### Dual Mode Selector

```erlang
%% @doc Backend selector for dual-mode operation
-module(backend_selector).

-export([select_backend/1, is_legacy_workflow/1]).

%% @doc Selects backend based on configuration and workflow
-spec select_backend(WorkflowId :: binary()) -> {module(), map()}.
select_backend(WorkflowId) ->
    Backend = application:get_env(yawl, pnet_backend, gen_statem),
    case Backend of
        dual ->
            %% Use gen_pnet for new workflows, gen_statem for legacy
            case is_legacy_workflow(WorkflowId) of
                true -> {yawl_legacy_statem, #{}};
                false -> {yawl_pnet, #{}}
            end;
        gen_pnet ->
            {yawl_pnet, #{}};
        gen_statem ->
            {yawl_legacy_statem, #{}}
    end.

%% @doc Checks if workflow is legacy (in migration table)
-spec is_legacy_workflow(WorkflowId :: binary()) -> boolean().
is_legacy_workflow(WorkflowId) ->
    case ets:lookup(migration_table, WorkflowId) of
        [{_, Status}] -> Status =:= legacy;
        [] -> false
    end.
```

---

## Part 5: Migration Checklist

### Pre-Migration

- [ ] Identify all workflows using gen_statem
- [ ] Test each workflow for correctness
- [ ] Document current behavior
- [ ] Create backup of workflow definitions
- [ ] Set up dual-mode configuration

### Migration

- [ ] Create gen_pnet equivalent module
- [ ] Implement gen_pnet callbacks
- [ ] Test token conversion with adapter
- [ ] Run both backends in parallel
- [ ] Compare results for consistency

### Post-Migration

- [ ] Verify all workflows complete correctly
- [ ] Check performance metrics
- [ ] Update documentation
- [ ] Set backend to gen_pnet
- [ ] Remove legacy code

---

## Part 6: Common Migration Patterns

### Pattern 1: State Machine to Places

**gen_statem:**
```erlang
waiting_for_payment(cast, Event, Data) -> ...
```

**gen_pnet:**
```erlang
place_lst() -> [p_waiting_for_payment, ...].
is_enabled(t_pay, #{p_waiting_for_payment := [T]}, _) -> true.
fire(t_pay, _, _) -> ...
```

### Pattern 2: State Data to Token Payload

**gen_statem:**
```erlang
StateData = #{order => Order, customer => Customer}
```

**gen_pnet:**
```erlang
Token = #colored_token{
    payload = #{order => Order, customer => Customer}
}
```

### Pattern 3: Event Handling to Transitions

**gen_statem:**
```erlang
handle_cast({payment_complete, PaymentId}, State) -> ...
```

**gen_pnet:**
```erlang
fire(t_payment_complete, _, _) -> ...
```

---

## Exercise: Migrate a Workflow

Migrate this gen_statem workflow to gen_pnet:

```erlang
%% Legacy approval workflow
-module(approval_legacy).
-behaviour(gen_statem).

-export([pending_approval/3, approved/3, rejected/3]).
-export([start/1]).

start(Request) ->
    gen_statem:start_link(?MODULE, Request, []).

callback_mode() -> state_functions.

init(Request) ->
    {ok, pending_approval, #{request => Request}}.

pending_approval(cast, {approve, Approver}, Data) ->
    {next_state, approved, Data#{approver => Approver}};
pending_approval(cast, {reject, Reason}, Data) ->
    {next_state, rejected, Data#{reason => Reason}};
pending_approval(_, _, Data) ->
    {keep_state, Data}.

approved(cast, complete, Data) ->
    {stop, normal, Data};
approved(_, _, Data) ->
    {keep_state, Data}.

rejected(cast, complete, Data) ->
    {stop, normal, Data};
rejected(_, _, Data) ->
    {keep_state, Data}.
```

**Requirements:**

1. Create `approval_pnet.erl` with equivalent behavior
2. Use places: `p_pending`, `p_approved`, `p_rejected`, `p_done`
3. Use tokens to carry request data, approver, and rejection reason
4. Test both implementations produce same results

---

## Summary: Migration Best Practices

| Practice | Description |
|----------|-------------|
| **Test First** | Write tests for legacy behavior before migrating |
| **Incremental** | Migrate one workflow at a time |
| **Dual Mode** | Run both backends during transition |
| **Compare** | Verify outputs match between backends |
| **Monitor** | Watch for performance differences |

---

**Congratulations!** You've completed the tutorial series. You now have the knowledge to:

- Create and run YAWL workflows with gen_pnet
- Use basic and advanced workflow patterns
- Work with colored tokens for data flow
- Migrate from legacy YAWL implementations

For more information, see:
- [YAWL Patterns Reference](../YAWL_PATTERNS_REFERENCE.md)
- [gen_pnet API Specification](../yawl_patterns/GEN_PNET_API_SPECIFICATION.md)
- [Architecture Documentation](../ARCHITECTURE.md)
