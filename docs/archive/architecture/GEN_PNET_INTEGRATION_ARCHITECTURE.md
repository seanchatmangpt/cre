# YAWL gen_pnet Integration Architecture

**Version**: 1.0.0
**Date**: 2026-02-05
**Status**: Design Specification

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Architecture Overview](#architecture-overview)
3. [gen_pnet Behavior Wrapper Specification](#gen_pnet-behavior-wrapper-specification)
4. [Pattern-to-Petri-Net Mapping](#pattern-to-petri-net-mapping)
5. [Colored Token Structure](#colored-token-structure)
6. [Migration Path from gen_statem](#migration-path-from-gen_statem)
7. [Record Definitions](#record-definitions)
8. [Interface Specifications](#interface-specifications)

---

## Executive Summary

This document defines the architecture for integrating YAWL (Yet Another Workflow Language) with the gen_pnet OTP behavior. The integration enables native Petri net semantics for workflow execution while maintaining backward compatibility with existing YAWL patterns.

**Key Objectives:**
- Implement all 43 YAWL patterns as gen_pnet callback modules
- Define colored token structure for data-carrying tokens
- Provide migration path from gen_statem to gen_pnet
- Maintain YAWL token semantics and cancellation patterns
- Achieve performance parity with existing implementation

---

## Architecture Overview

### System Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         YAWL gen_pnet Layer                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐              │
│  │   yawl_pnet  │  │  Pattern     │  │   Colored    │              │
│  │   (Wrapper)  │──│  Registry    │──│   Token      │              │
│  └──────────────┘  └──────────────┘  └──────────────┘              │
│         │                  │                  │                     │
│         │                  ▼                  │                     │
│         │      ┌─────────────────────┐       │                     │
│         │      │  Pattern Modules    │       │                     │
│         │      │  (43 gen_pnet mods) │       │                     │
│         │      └─────────────────────┘       │                     │
│         │                  │                  │                     │
│         ▼                  ▼                  ▼                     │
│  ┌──────────────────────────────────────────────────────────┐      │
│  │                    gen_pnet Behavior                      │      │
│  │  • place_lst/0  • trsn_lst/0  • init_marking/2           │      │
│  │  • preset/1     • is_enabled/3 • fire/3                  │      │
│  │  • trigger/3    • handle_call/3 • handle_cast/2          │      │
│  └──────────────────────────────────────────────────────────┘      │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Application Layer                               │
│  • yawl_engine    • yawl_orchestrator  • yawl_persistence           │
└─────────────────────────────────────────────────────────────────────┘
```

### Component Responsibilities

| Component | Responsibility |
|-----------|---------------|
| **yawl_pnet** | gen_pnet behavior wrapper providing YAWL-specific extensions |
| **Pattern Registry** | Dynamic loading and management of 43 pattern modules |
| **Pattern Modules** | Individual gen_pnet implementations for each YAWL pattern |
| **Colored Token** | Data-carrying token structure and transformation utilities |
| **Migration Layer** | Compatibility layer for gen_statem to gen_pnet transition |

---

## gen_pnet Behavior Wrapper Specification

### Module: `yawl_pnet.erl`

The `yawl_pnet` module provides a gen_pnet behavior implementation with YAWL-specific extensions.

#### Behavior Declaration

```erlang
-module(yawl_pnet).
-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([init/1,
         place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3,
         trigger/3,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% YAWL-specific API
-export([start_workflow/2,
         start_workflow/3,
         get_case_state/1,
         get_marking/1,
         inject_token/3,
         cancel_case/1,
         register_pattern/2,
         list_patterns/0]).
```

#### State Record

```erlang
-record(yawl_pnet_state, {
    case_id             :: yawl_types:case_id(),
    workflow_id         :: yawl_types:workflow_id(),
    pattern_module      :: module(),
    pattern_config      :: map(),
    usr_info            :: term(),
    markings            :: #{atom() => [yawl_types:colored_token()]},
    active_transitions  :: #{atom() => reference()},
    completed_places    :: [atom()],
    metadata            :: map(),
    observers           :: [pid()],
    created_at          :: integer(),
    started_at          :: integer() | undefined,
    completed_at        :: integer() | undefined,
    cancellation_token  :: reference() | undefined
}).
```

#### gen_pnet Callbacks

##### `init/1`

```erlang
%% @doc Initializes the YAWL Petri net instance
-spec init(Args :: term()) -> {ok, UsrInfo :: term()}.

init({PatternModule, PatternConfig}) ->
    logger:info("Initializing YAWL gen_pnet: pattern=~p config=~p",
                [PatternModule, PatternConfig]),
    UsrInfo = #{
        pattern_module => PatternModule,
        pattern_config => PatternConfig,
        case_id => generate_case_id(),
        workflow_id => maps:get(workflow_id, PatternConfig, undefined)
    },
    {ok, UsrInfo};
```

##### `place_lst/0`

```erlang
%% @doc Returns list of places in the YAWL Petri net
-spec place_lst() -> [atom()].

place_lst() ->
    %% Standard YAWL places
    [
        input,           % Input condition
        output,          % Output condition
        active,          % Active tasks
        completed,       % Completed tasks
        waiting,         % Waiting for synchronization
        cancelled,       % Cancelled tasks
        error,           % Error state
        data_buffer,     % Data transformation buffer
        join_buffer,     % Join synchronization buffer
        branch_control,  % Branch routing control
        milestone        % Milestone checkpoint
    ].
```

##### `trsn_lst/0`

```erlang
%% @doc Returns list of transitions in the YAWL Petri net
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        %% Control flow transitions
        start,           % Start workflow
        complete,        % Complete task
        split_and,       % AND split
        split_or,        % OR split
        split_xor,       % XOR split
        join_and,        % AND join
        join_or,         % OR join
        join_xor,        % XOR join

        %% Multiple instance transitions
        spawn_instances, % Spawn multiple instances
        complete_one,    % Complete one instance
        sync_all,        % Synchronize all instances

        %% Cancellation transitions
        cancel_activity, % Cancel single activity
        cancel_case,     % Cancel entire case
        cancel_region,   % Cancel region

        %% Data flow transitions
        transform_data,  % Transform data
        distribute_data, % Distribute data
        accumulate_data  % Accumulate data
    ].
```

##### `init_marking/2`

```erlang
%% @doc Returns initial marking for a place
-spec init_marking(Place :: atom(), UsrInfo :: map()) -> [yawl_types:colored_token()].

init_marking(input, _UsrInfo) ->
    [yawl_types:initial_token()];
init_marking(_Place, _UsrInfo) ->
    [].
```

##### `preset/1`

```erlang
%% @doc Returns preset places for a transition
-spec preset(Trsn :: atom()) -> [atom()].

preset(start)           -> [input];
preset(complete)        -> [active];
preset(split_and)       -> [active];
preset(split_or)        -> [active, branch_control];
preset(split_xor)       -> [active, branch_control];
preset(join_and)        -> [join_buffer];
preset(join_or)         -> [join_buffer];
preset(join_xor)        -> [join_buffer];
preset(spawn_instances) -> [active];
preset(complete_one)    -> [active];
preset(sync_all)        -> [join_buffer];
preset(cancel_activity) -> [active];
preset(cancel_case)     -> [active, waiting, join_buffer];
preset(cancel_region)   -> [active];
preset(transform_data)  -> [active, data_buffer];
preset(distribute_data) -> [data_buffer];
preset(accumulate_data) -> [data_buffer].
```

##### `is_enabled/3`

```erlang
%% @doc Checks if transition is enabled in given mode
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: map()) -> boolean().

is_enabled(start, #{input := [Token]}, _UsrInfo) when Token =/= [] ->
    true;
is_enabled(complete, #{active := [Token]}, _UsrInfo) when Token =/= [] ->
    true;
is_enabled(split_and, #{active := [Token], branch_control := Control}, _UsrInfo) ->
    case yawl_types:get_token_type(Token) of
        workflow_start -> true;
        task_complete -> maps:get(split_type, Control, and) =:= and
    end;
is_enabled(join_and, #{join_buffer := Tokens}, _UsrInfo) ->
    length(Tokens) >= maps:get(required_count, _UsrInfo, 2);
is_enabled(cancel_case, _Mode, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.
```

##### `fire/3`

```erlang
%% @doc Fires transition, producing tokens on output places
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: map()) ->
          abort | {produce, #{atom() => [yawl_types:colored_token()]}}.

fire(start, #{input := [InputToken]}, UsrInfo) ->
    case yawl_types:get_token_type(InputToken) of
        initial ->
            {produce, #{
                active => [yawl_types:task_token(first_task, InputToken)],
                branch_control => [yawl_types:control_token(sequence)]
            }};
        _ ->
            abort
    end;

fire(split_and, #{active := [Token]}, UsrInfo) ->
    case yawl_types:get_token_data(Token) of
        #{branches := Branches} when length(Branches) > 1 ->
            ActiveTokens = [yawl_types:task_token(B, Token) || B <- Branches],
            {produce, #{
                active => ActiveTokens
            }};
        _ ->
            abort
    end;

fire(join_and, #{join_buffer := Tokens}, UsrInfo) ->
    RequiredCount = maps:get(required_count, UsrInfo, 2),
    case length(Tokens) of
        N when N >= RequiredCount ->
            MergedData = yawl_types:merge_token_data(Tokens),
            {produce, #{
                active => [yawl_types:task_token(next_task, MergedData)],
                join_buffer => []
            }};
        _ ->
            abort
    end;

fire(cancel_case, _Mode, _UsrInfo) ->
    {produce, #{
        cancelled => [yawl_types:cancel_token(case_cancelled)],
        active => [],
        waiting => [],
        join_buffer => []
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.
```

##### `trigger/3`

```erlang
%% @doc Called when token is produced on a place
-spec trigger(Place :: atom(), Token :: yawl_types:colored_token(),
              NetState :: #net_state{}) -> pass | drop.

trigger(output, Token, _NetState) ->
    case yawl_types:get_token_type(Token) of
        completion ->
            logger:info("Workflow completed: token=~p", [Token]),
            pass;
        _ ->
            pass
    end;
trigger(error, Token, _NetState) ->
    logger:error("Error token produced: token=~p", [Token]),
    pass;
trigger(_Place, _Token, _NetState) ->
    pass.
```

#### YAWL-Specific API Functions

##### `start_workflow/2,3`

```erlang
%% @doc Starts a new YAWL workflow as gen_pnet instance
-spec start_workflow(PatternModule :: module(), PatternConfig :: map()) ->
          {ok, pid()} | {error, term()}.

start_workflow(PatternModule, PatternConfig) ->
    start_workflow(PatternModule, PatternConfig, []).

start_workflow(PatternModule, PatternConfig, Options) ->
    ServerName = maps:get(server_name, Options, undefined),
    case ServerName of
        undefined ->
            gen_pnet:start_link(?MODULE, {PatternModule, PatternConfig}, Options);
        _ ->
            gen_pnet:start_link(ServerName, ?MODULE, {PatternModule, PatternConfig}, Options)
    end.
```

##### `get_case_state/1`

```erlang
%% @doc Gets current workflow case state
-spec get_case_state(Pid :: pid()) -> {ok, map()} | {error, term()}.

get_case_state(Pid) ->
    try
        Marking = gen_pnet:marking(Pid),
        UsrInfo = gen_pnet:usr_info(Pid),
        {ok, #{
            marking => Marking,
            case_id => maps:get(case_id, UsrInfo),
            workflow_id => maps:get(workflow_id, UsrInfo),
            pattern_module => maps:get(pattern_module, UsrInfo)
        }}
    catch
        _:_ -> {error, not_running}
    end.
```

##### `inject_token/3`

```erlang
%% @doc Injects a token into a specific place
-spec inject_token(Pid :: pid(), Place :: atom(),
                   Token :: yawl_types:colored_token()) -> ok | {error, term()}.

inject_token(Pid, Place, Token) ->
    gen_pnet:cast(Pid, {inject_token, Place, Token}).
```

---

## Pattern-to-Petri-Net Mapping

### Pattern Category: Basic Control Flow (WCP-01 to WCP-06)

#### WCP-01: Sequence

**YAWL Specification:** Execute tasks in sequential order

**Petri Net Structure:**
```
Places:  [p_start, p_task1, p_task2, ..., p_end]
Transitions: [t_start, t_complete_task1, t_complete_task2, ..., t_end]

Flow: p_start --t_start--> p_task1 --t_complete_task1--> p_task2 --...--> p_end
```

**gen_pnet Implementation:**

```erlang
-module(yawl_wcp01_sequence).
-behaviour(gen_pnet).

place_lst() -> [p_start, p_task1, p_task2, p_end].
trsn_lst() -> [t_start, t_complete_task1, t_complete_task2, t_end].

init_marking(p_start, _UsrInfo) -> [initial];
init_marking(_Place, _UsrInfo) -> [].

preset(t_start) -> [p_start];
preset(t_complete_task1) -> [p_task1];
preset(t_complete_task2) -> [p_task2];
preset(t_end) -> [p_end].

is_enabled(t_start, #{p_start := [Token]}, _UsrInfo) when Token =/= [] -> true;
is_enabled(t_complete_task1, #{p_task1 := [Token]}, _UsrInfo) when Token =/= [] -> true;
is_enabled(t_complete_task2, #{p_task2 := [Token]}, _UsrInfo) when Token =/= [] -> true;
is_enabled(t_end, #{p_end := [Token]}, _UsrInfo) when Token =/= [] -> true;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(t_start, _Mode, _UsrInfo) ->
    {produce, #{p_task1 => [task_token(task1)]}};
fire(t_complete_task1, _Mode, _UsrInfo) ->
    {produce, #{p_task2 => [task_token(task2)]}};
fire(t_complete_task2, _Mode, _UsrInfo) ->
    {produce, #{p_end => [completion_token]}};
fire(t_end, _Mode, _UsrInfo) ->
    {produce, #{}}.
```

#### WCP-02: Parallel Split (AND)

**YAWL Specification:** Split into N concurrent branches

**Petri Net Structure:**
```
Places:  [p_split, p_branch_1, p_branch_2, ..., p_branch_N]
Transitions: [t_split]

Flow: p_split --t_split--> p_branch_1, p_branch_2, ..., p_branch_N
```

**gen_pnet Implementation:**

```erlang
-module(yawl_wcp02_parallel_split).
-behaviour(gen_pnet).

place_lst() ->
    [p_split, p_branch_1, p_branch_2, p_branch_3].

trsn_lst() -> [t_split].

init_marking(p_split, _UsrInfo) -> [initial];
init_marking(_Place, _UsrInfo) -> [].

preset(t_split) -> [p_split].

is_enabled(t_split, #{p_split := [Token]}, UsrInfo) ->
    case maps:get(branch_count, UsrInfo, 2) of
        N when N > 1 -> true;
        _ -> false
    end;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(t_split, _Mode, UsrInfo) ->
    BranchCount = maps:get(branch_count, UsrInfo, 2),
    BranchTokens = lists:duplicate(BranchCount, branch_token),
    {produce, #{
        p_branch_1 => BranchTokens,
        p_branch_2 => BranchTokens,
        p_branch_3 => BranchTokens
    }}.
```

### Pattern Category: Advanced Branching and Synchronization (WCP-07 to WCP-12)

#### WCP-04: Exclusive Choice (XOR)

**YAWL Specification:** Choose exactly one branch based on condition

**Petri Net Structure:**
```
Places:  [p_choice, p_branch_1, p_branch_2, p_selected]
Transitions: [t_choose_1, t_choose_2]

Flow: p_choice --t_choose_1--> p_branch_1
      p_choice --t_choose_2--> p_branch_2
```

**gen_pnet Implementation:**

```erlang
-module(yawl_wcp04_exclusive_choice).
-behaviour(gen_pnet).

place_lst() -> [p_choice, p_branch_1, p_branch_2, p_selected].

trsn_lst() -> [t_choose_1, t_choose_2].

init_marking(p_choice, _UsrInfo) -> [initial];
init_marking(_Place, _UsrInfo) -> [].

preset(t_choose_1) -> [p_choice];
preset(t_choose_2) -> [p_choice].

is_enabled(t_choose_1, #{p_choice := [Token]}, UsrInfo) ->
    ConditionFun = maps:get(condition_fun, UsrInfo),
    case yawl_types:get_token_data(Token) of
        Data -> ConditionFun(Data, branch_1)
    end;
is_enabled(t_choose_2, #{p_choice := [Token]}, UsrInfo) ->
    ConditionFun = maps:get(condition_fun, UsrInfo),
    case yawl_types:get_token_data(Token) of
        Data -> ConditionFun(Data, branch_2)
    end;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(t_choose_1, _Mode, _UsrInfo) ->
    {produce, #{p_branch_1 => [task_token(branch_1)], p_choice => []}};
fire(t_choose_2, _Mode, _UsrInfo) ->
    {produce, #{p_branch_2 => [task_token(branch_2)], p_choice => []}}.
```

### Pattern Category: Multiple Instances (WCP-13 to WCP-17)

#### WCP-13: Multiple Instances (Static)

**YAWL Specification:** Create fixed number of parallel instances

**Petri Net Structure:**
```
Places:  [p_start, p_spawn, p_instances, p_complete, p_end]
Transitions: [t_spawn, t_complete_one, t_sync_all]

Flow: p_start --t_spawn--> p_instances --t_complete_one--> p_complete --t_sync_all--> p_end
```

**gen_pnet Implementation:**

```erlang
-module(yawl_wcp13_multi_instance_static).
-behaviour(gen_pnet).

place_lst() -> [p_start, p_spawn, p_instances, p_complete, p_end].

trsn_lst() -> [t_spawn, t_complete_one, t_sync_all].

init_marking(p_start, _UsrInfo) -> [initial];
init_marking(_Place, _UsrInfo) -> [].

preset(t_spawn) -> [p_start];
preset(t_complete_one) -> [p_instances];
preset(t_sync_all) -> [p_complete].

is_enabled(t_spawn, #{p_start := [Token]}, UsrInfo) ->
    maps:get(instance_count, UsrInfo, 1) > 0;
is_enabled(t_complete_one, #{p_instances := Tokens}, _UsrInfo) when Tokens =/= [] ->
    true;
is_enabled(t_sync_all, #{p_complete := Tokens}, UsrInfo) ->
    length(Tokens) >= maps:get(instance_count, UsrInfo, 1);
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(t_spawn, _Mode, UsrInfo) ->
    Count = maps:get(instance_count, UsrInfo, 1),
    InstanceTokens = [instance_token(I) || I <- lists:seq(1, Count)],
    {produce, #{p_instances => InstanceTokens}};
fire(t_complete_one, #{p_instances := [Token | Rest]}, _UsrInfo) ->
    {produce, #{
        p_instances => Rest,
        p_complete => [completion_token]
    }};
fire(t_sync_all, _Mode, _UsrInfo) ->
    {produce, #{p_end => [workflow_complete]}}.
```

### Pattern Category: Cancellation (WCP-19 to WCP-21)

#### WCP-19: Cancel Activity

**YAWL Specification:** Cancel a single running activity

**Petri Net Structure:**
```
Places:  [p_active, p_cancel, p_completed, p_cancelled]
Transitions: [t_complete, t_cancel]

Flow: p_active --t_complete--> p_completed
      p_active --t_cancel--> p_cancelled
```

**gen_pnet Implementation:**

```erlang
-module(yawl_wcp19_cancel_activity).
-behaviour(gen_pnet).

place_lst() -> [p_active, p_cancel, p_completed, p_cancelled].

trsn_lst() -> [t_complete, t_cancel].

init_marking(p_active, _UsrInfo) -> [initial].

preset(t_complete) -> [p_active];
preset(t_cancel) -> [p_active, p_cancel].

is_enabled(t_complete, #{p_active := [Token]}, _UsrInfo) when Token =/= [] -> true;
is_enabled(t_cancel, Mode, _UsrInfo) ->
    maps:get(p_cancel, Mode, []) =/= [] orelse maps:get(p_active, Mode, []) =/= [].

fire(t_complete, _Mode, _UsrInfo) ->
    {produce, #{p_completed => [completion_token], p_active => []}};
fire(t_cancel, _Mode, _UsrInfo) ->
    {produce, #{p_cancelled => [cancel_token], p_active => [], p_cancel => []}}.
```

---

## Colored Token Structure

### Token Definition

Colored tokens carry data payloads through the Petri net, enabling data-aware workflow patterns.

#### Token Record

```erlang
%% File: include/yawl_types.hrl

-record(colored_token, {
    id          :: binary(),              % Unique token ID
    type        :: token_type(),          % Token type classification
    payload     :: term(),                % Data payload
    metadata    :: map(),                 % Token metadata
    created_at  :: integer(),             % Creation timestamp
    expires_at  :: integer() | undefined, % Optional expiration
    trace_id    :: binary() | undefined   % OTEL trace correlation
}).

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
    completion.          % % Workflow completion

-type colored_token() :: #colored_token{}.
```

### Token Construction Functions

```erlang
%% File: src/yawl_pnet_tokens.erl

-module(yawl_pnet_tokens).

%% @doc Creates an initial workflow token
-spec initial_token(Data :: term()) -> colored_token().
initial_token(Data) ->
    #colored_token{
        id = generate_token_id(),
        type = initial,
        payload = Data,
        metadata = #{},
        created_at = erlang:system_time(millisecond),
        trace_id = opentelemetry:generate_trace_id()
    }.

%% @doc Creates a task start token
-spec task_token(TaskId :: binary(), InputData :: term()) -> colored_token().
task_token(TaskId, InputData) ->
    #colored_token{
        id = generate_token_id(),
        type = task_start,
        payload = #{task_id => TaskId, input => InputData},
        metadata = #{task_id => TaskId},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a branch token for parallel execution
-spec branch_token(BranchId :: atom(), Data :: term()) -> colored_token().
branch_token(BranchId, Data) ->
    #colored_token{
        id = generate_token_id(),
        type = branch,
        payload = #{branch_id => BranchId, data => Data},
        metadata = #{branch_id => BranchId},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a data-carrying token
-spec data_token(DataType :: atom(), Data :: term()) -> colored_token().
data_token(DataType, Data) ->
    #colored_token{
        id = generate_token_id(),
        type = data,
        payload = #{data_type => DataType, value => Data},
        metadata = #{data_type => DataType},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Creates a cancel token
-spec cancel_token(Reason :: term()) -> colored_token().
cancel_token(Reason) ->
    #colored_token{
        id = generate_token_id(),
        type = cancel,
        payload = #{reason => Reason},
        metadata = #{cancelled_at => erlang:system_time(millisecond)},
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Merges multiple tokens into one
-spec merge_tokens([colored_token()]) -> colored_token().
merge_tokens(Tokens) when is_list(Tokens) ->
    MergedPayload = merge_payloads([T#colored_token.payload || T <- Tokens]),
    #colored_token{
        id = generate_token_id(),
        type = merge,
        payload = MergedPayload,
        metadata = #{
            merged_from => [T#colored_token.id || T <- Tokens],
            merged_at => erlang:system_time(millisecond)
        },
        created_at = erlang:system_time(millisecond)
    }.

%% @doc Extracts token type
-spec get_token_type(colored_token()) -> token_type().
get_token_type(#colored_token{type = Type}) ->
    Type.

%% @doc Extracts token payload
-spec get_token_payload(colored_token()) -> term().
get_token_payload(#colored_token{payload = Payload}) ->
    Payload.

%% @doc Transforms token payload
-spec transform_payload(colored_token(), fun((term()) -> term())) -> colored_token().
transform_payload(Token = #colored_token{payload = Payload}, TransformFun) ->
    Token#colored_token{payload = TransformFun(Payload)}.
```

---

## Migration Path from gen_statem

### Migration Strategy

The migration from gen_statem to gen_pnet follows a phased approach to maintain backward compatibility.

### Phase 1: Compatibility Layer

#### Module: `yawl_pnet_adapter.erl`

```erlang
-module(yawl_pnet_adapter).

%% @doc Converts gen_statem state to gen_pnet marking
-spec statem_to_pnet(State :: term()) -> #{atom() => [colored_token()]}.

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
```

### Phase 2: Dual-Mode Operation

#### Configuration Flag

```erlang
%% File: config/sys.config

{yawl, [
    {pnet_backend, gen_pnet},  % gen_pnet | gen_statem | dual
    {pnet_migration, [
        {enable_dual_mode, true},
        {migration_complete, false},
        {compatibility_layer, true}
    ]}
]}.
```

#### Runtime Backend Selection

```erlang
-module(yawl_backend_selector).

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
```

### Phase 3: State Migration

#### Migration Module

```erlang
-module(yawl_state_migration).

%% @doc Migrates a running workflow from gen_statem to gen_pnet
-spec migrate_workflow(WorkflowId :: binary()) -> {ok, pid()} | {error, term()}.

migrate_workflow(WorkflowId) ->
    case yawl_legacy_statem:get_state(WorkflowId) of
        {ok, LegacyState} ->
            %% Convert legacy state to pnet marking
            Marking = yawl_pnet_adapter:statem_to_pnet(LegacyState),

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
```

---

## Record Definitions

### Complete Type Definitions

```erlang
%%====================================================================
%% File: include/yawl_types.hrl
%%====================================================================

%%--------------------------------------------------------------------
%% Colored Token Records
%%--------------------------------------------------------------------

-record(colored_token, {
    id          :: binary(),
    type        :: token_type(),
    payload     :: term(),
    metadata    :: map(),
    created_at  :: integer(),
    expires_at  :: integer() | undefined,
    trace_id    :: binary() | undefined
}).

-type token_type() ::
    initial | task_start | task_complete | branch | merge |
    data | control | cancel | error | milestone | completion.

-type colored_token() :: #colored_token{}.

%%--------------------------------------------------------------------
%% Workflow Case Records
%%--------------------------------------------------------------------

-record(yawl_case, {
    case_id         :: binary(),
    workflow_id     :: binary(),
    pattern_module  :: module(),
    status          :: case_status(),
    markings        :: #{atom() => [colored_token()]},
    active_trans    :: #{atom() => reference()},
    variables       :: map(),
    observers       :: [pid()],
    created_at      :: integer(),
    started_at      :: integer() | undefined,
    completed_at    :: integer() | undefined,
    metadata        :: map()
}).

-type case_status() ::
    initialized | running | suspended | completed | cancelled | failed.

%%--------------------------------------------------------------------
%% Pattern Configuration Records
%%--------------------------------------------------------------------

-record(pattern_config, {
    pattern_id      :: binary(),
    pattern_type    :: atom(),
    places          :: [atom()],
    transitions     :: [atom()],
    initial_marking :: map(),
    preset_map      :: #{atom() => [atom()]},
    postset_map     :: #{atom() => [atom()]},
    guards          :: #{atom() => fun()},
    actions         :: #{atom() => fun()},
    metadata        :: map()
}).

%%--------------------------------------------------------------------
%% Transition Records
%%--------------------------------------------------------------------

-record(transition, {
    id              :: atom(),
    name            :: binary(),
    input_places    :: [atom()],
    output_places   :: [atom()],
    guard_fn        :: fun() | undefined,
    fire_fn         :: fun(),
    priority        :: non_neg_integer()
}).

-type transition() :: #transition{}.

%%--------------------------------------------------------------------
%% Place Records
%%--------------------------------------------------------------------

-record(place, {
    id              :: atom(),
    name            :: binary(),
    capacity        :: non_neg_integer() | unlimited,
    initial_tokens  :: [colored_token()],
    current_tokens  :: [colored_token()],
    type            :: place_type()
}).

-type place_type() ::
    input | output | active | waiting | synchronizing |
    data_buffer | branch_control | milestone | error.

%%--------------------------------------------------------------------
%% gen_pnet Extension Records
%%--------------------------------------------------------------------

-record(yawl_pnet_opts, {
    enable_colored     :: boolean(),
    enable_tracing     :: boolean(),
    enable_metrics     :: boolean(),
    max_instances      :: non_neg_integer() | unlimited,
    timeout_ms         :: non_neg_integer() | infinity,
    cancellation_mode  :: immediate | graceful
}).

-record(pnet_metrics, {
    tokens_produced   :: non_neg_integer(),
    tokens_consumed   :: non_neg_integer(),
    transitions_fired :: non_neg_integer(),
    places_marked     :: non_neg_integer(),
    errors_encountered :: non_neg_integer()
}).
```

---

## Interface Specifications

### Pattern Module Interface

All YAWL pattern modules must implement the following interface:

```erlang
%%====================================================================
%% YAWL Pattern Module Interface
%%====================================================================

-callback pattern_id() -> binary().

-callback pattern_info() -> #{
    name := binary(),
    category := atom(),
    wcp_number := {integer(), integer()},
    description := binary()
}.

-callback init_marking(Place :: atom(), UsrInfo :: map()) -> [colored_token()].

-callback preset(Transition :: atom()) -> [atom()].

-callback is_enabled(Transition :: atom(), Mode :: map(), UsrInfo :: map()) ->
    boolean().

-callback fire(Transition :: atom(), Mode :: map(), UsrInfo :: map()) ->
    abort | {produce, #{atom() => [colored_token()]}}.

-callback trigger(Place :: atom(), Token :: colored_token(),
                  NetState :: #net_state{}) -> pass | drop.

-optional_callbacks([trigger/3]).
```

### Registry Interface

```erlang
%%====================================================================
%% Pattern Registry Interface
%%====================================================================

-module(yawl_pattern_registry).

%% Pattern registration
-export([register_pattern/2, unregister_pattern/1, get_pattern/1]).

%% Pattern discovery
-export([list_patterns/0, list_patterns_by_category/1,
         find_pattern_by_wcp/1]).

%% Pattern validation
-export([validate_pattern/1, verify_soundness/1]).

%% @doc Registers a pattern module
-spec register_pattern(PatternId :: binary(), Module :: module()) ->
    ok | {error, term()}.

%% @doc Unregisters a pattern module
-spec unregister_pattern(PatternId :: binary()) -> ok.

%% @doc Gets pattern module by ID
-spec get_pattern(PatternId :: binary()) ->
    {ok, module()} | {error, not_found}.

%% @doc Lists all registered patterns
-spec list_patterns() -> [{binary(), module()}].

%% @doc Lists patterns by category
-spec list_patterns_by_category(Category :: atom()) ->
    [{binary(), module()}].

%% @doc Finds pattern by WCP number
-spec find_pattern_by_wcp(WCPNumber :: {integer(), integer()}) ->
    {ok, module()} | {error, not_found}.

%% @doc Validates pattern implementation
-spec validate_pattern(Module :: module()) ->
    {ok, map()} | {error, term()}.

%% @doc Verifies pattern soundness properties
-spec verify_soundness(Module :: module()) ->
    {ok, map()} | {error, term()}.
```

---

## Appendix A: Complete Pattern Mapping Table

| WCP # | Pattern Name | Module | Places | Transitions | Complexity |
|-------|--------------|--------|--------|-------------|-------------|
| WCP-01 | Sequence | yawl_wcp01_sequence | 3+N | 2+N | Low |
| WCP-02 | Parallel Split | yawl_wcp02_parallel_split | 1+N | 1 | Low |
| WCP-03 | Synchronization | yawl_wcp03_synchronization | N+1 | 1 | Low |
| WCP-04 | Exclusive Choice | yawl_wcp04_exclusive_choice | 1+N | N | Low |
| WCP-05 | Simple Merge | yawl_wcp05_simple_merge | N+1 | N | Low |
| WCP-06 | Multiple Choice | yawl_wcp06_multiple_choice | 1+N | N | Medium |
| WCP-07 | Structured Sync Merge | yawl_wcp07_structured_sync_merge | N+1 | 1 | Medium |
| WCP-08 | Multiple Merge | yawl_wcp08_multiple_merge | N+1 | N | Medium |
| WCP-09 | Discriminator | yawl_wcp09_discriminator | N+2 | 2 | Medium |
| WCP-10 | Arbitrary Cycles | yawl_wcp10_arbitrary_cycles | 4+ | 2+ | High |
| WCP-11 | Implicit Termination | yawl_wcp11_implicit_term | 3 | 2 | Low |
| WCP-12 | Multi-Inst No Sync | yawl_wcp12_multi_no_sync | 3 | 2 | Medium |
| WCP-13 | Multi-Inst Static | yawl_wcp13_multi_static | 4 | 3 | Medium |
| WCP-14 | Multi-Inst Sync | yawl_wcp14_multi_sync | 5 | 4 | Medium |
| WCP-15 | Multi-Inst Dynamic | yawl_wcp15_multi_dynamic | 5 | 4 | High |
| WCP-16 | Deferred Choice | yawl_wcp16_deferred_choice | 3+N | 1+N | Medium |
| WCP-17 | Interleaved Routing | yawl_wcp17_interleaved | 4 | 3 | High |
| WCP-18 | Milestone | yawl_wcp18_milestone | 3 | 2 | Medium |
| WCP-19 | Cancel Activity | yawl_wcp19_cancel_activity | 3 | 2 | Medium |
| WCP-20 | Cancel Case | yawl_wcp20_cancel_case | N+2 | N+1 | High |
| WCP-21 | Cancel Region | yawl_wcp21_cancel_region | N+3 | N+2 | High |

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-05
**Author**: YAWL gen_pnet Integration Team
