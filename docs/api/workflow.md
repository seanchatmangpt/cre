# CRE Workflow and Core API Reference

This document provides comprehensive API documentation for the CRE (Common Runtime Environment) core workflow execution modules, including the gen_pnet and gen_yawl OTP behaviors, YAWL compilation, workflow utilities, and client APIs.

## Table of Contents

1. [Core OTP Behaviors](#core-otp-behaviors)
2. [YAWL Compilation](#yawl-compilation)
3. [PNET Pure Helpers](#pnet-pure-helpers)
4. [Workflow Utilities](#workflow-utilities)
5. [Client API](#client-api)
6. [Message Contracts](#message-contracts)

---

## Core OTP Behaviors

### gen_pnet - Generic Petri Net Behavior

**Module:** `src/core/gen_pnet.erl`

The core OTP behavior module for implementing Petri net workflows as Erlang/OTP gen_server processes.

#### Type Definitions

```erlang
-type name() :: atom() |
                {atom(), atom()} |
                {global, _} |
                {via, atom(), _} |
                pid().

-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

-type start_link_result() :: {ok, pid()} |
                             ignore |
                             {error, _}.

-type prop() :: {debug, [log | statistics | trace | {_, _}]} |
                {hibernate_after, infinity | non_neg_integer()} |
                {spawn_opt, [link | monitor | {_, _}]} |
                {timeout, infinity | non_neg_integer()}.
```

#### Exported API Functions

##### Lifecycle Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `start_link/3` | `(NetMod::atom(), NetArg::term(), Options::[prop()]) -> start_link_result()` | Starts an unregistered gen_pnet instance |
| `start_link/4` | `(ServerName::server_name(), NetMod::atom(), InitArg::term(), Options::[prop()]) -> start_link_result()` | Starts a registered gen_pnet instance |
| `stop/1` | `(Name::name()) -> ok` | Stops the gen_pnet process |

##### Query Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `ls/2` | `(Name::name(), Place::atom()) -> {ok, [_]} \| {error, #bad_place{}}` | Query tokens on a place |
| `marking/1` | `(Name::name()) -> #{atom() => [_]}` | Query the marking map of all places |
| `usr_info/1` | `(Name::name()) -> _` | Query user info from net instance |
| `stats/1` | `(Name::name()) -> #stats{}` | Query throughput statistics |
| `state_property/3` | `(Name::name(), Pred::fun((...) -> ok \| {error,_}), PlaceLst::[atom()]) -> ok \| {error,_}` | Check if predicate about state holds |

##### Control Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `call/2` | `(Name::name(), Request::term()) -> term()` | Synchronous request (5s timeout) |
| `call/3` | `(Name::name(), Request::term(), Timeout::non_neg_integer()\|infinity) -> term()` | Synchronous request with explicit timeout |
| `cast/2` | `(Name::name(), Request::term()) -> ok` | Asynchronous request |
| `reply/2` | `(Client::{pid(),gen_server:reply_tag()}, Reply::term()) -> ok` | Reply to deferred call |
| `inject/2` | `(Name::name(), ProduceMap::#{atom()=>[_]}) -> {ok,Receipt} \| {error,Reason}` | Inject tokens into the net |
| `step/1` | `(Name::name()) -> abort \| {ok,Receipt}` | Fire at most one enabled transition |
| `drain/2` | `(Name::name(), MaxSteps::non_neg_integer()) -> {ok,[Receipt]} \| {error,limit}` | Fire transitions until none enabled or max steps |
| `reset_stats/1` | `(Name::name()) -> ok` | Clear statistics |

##### State Accessor Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `get_ls/2` | `(Place::atom(), NetState::#net_state{}) -> [_]` | Extract tokens on a place from state |
| `get_usr_info/1` | `(NetState::#net_state{}) -> _` | Extract user info from state |
| `get_stats/1` | `(NetState::#net_state{}) -> #stats{}` | Extract stats from state |

#### Callback Functions

##### Structure Callbacks

```erlang
-callback place_lst() -> [atom()].
-callback trsn_lst() -> [atom()].
-callback init_marking(Place::atom(), UsrInfo::_) -> [_].
-callback preset(Trsn::atom()) -> [atom()].
-callback is_enabled(Trsn::atom(), Mode::#{atom()=>[_]}, UsrInfo::_) -> boolean().
-callback fire(Trsn::atom(), Mode::#{atom()=>[_]}, UsrInfo::_) ->
              abort | {produce, #{atom()=>[_]}}.
```

##### Interface Callbacks

```erlang
-callback init(NetArg::_) -> _.
-callback trigger(Place::atom(), Token::_, NetState::#net_state{}) -> pass | drop.
-callback handle_call(Request::_, From::{pid(),_}, NetState::#net_state{}) ->
              {reply,_} | {reply,_,#{atom()=>[_]}} | noreply |
              {noreply,#{atom()=>[_]}} | {stop,_,_}.
-callback handle_cast(Request::_, NetState::#net_state{}) ->
              noreply | {noreply,#{atom()=>[_]}} | {stop,_}.
-callback handle_info(Info::_, NetState::#net_state{}) ->
              noreply | {noreply,#{atom()=>[_]}} | {stop,_}.
-callback code_change(OldVsn::_, NetState::#net_state{}, Extra::_) -> {ok,#net_state{}} | {error,_}.
-callback terminate(Reason::_, NetState::#net_state{}) -> ok.
```

#### Record Definitions

```erlang
-record(bad_place, {name}).          % Error for non-existent place
-record(net_state, {
          marking,                   % #{atom() => [tokens]}
          net_mod,                   % Callback module
          usr_info,                  % User-defined state
          stats,                     % #stats{} | undefined
          tstart,                    % Stats timestamp
          cnt                        % Fire counter
         }).
-record(stat, {t, fps}).             % Single measurement
-record(stats, {current, hi, lo}).   % Aggregated statistics
```

#### Usage Example

```erlang
%% Define a simple net module
-module(my_net).
-behaviour(gen_pnet).

place_lst() -> [start, step1, done].
trsn_lst() -> [t1].

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

preset(t1) -> [start].

is_enabled(t1, #{start := [init]}, _UsrInfo) -> true;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

fire(t1, #{start := []}, _UsrInfo) ->
    {produce, #{step1 => [done]}};
fire(_Trsn, _Mode, _UsrInfo) -> abort.

init(_NetArg) -> #{}.
handle_call(_Request, _From, _NetState) -> {reply, ok}.
handle_cast(_Request, _NetState) -> noreply.
handle_info(_Info, _NetState) -> noreply.
code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.
terminate(_Reason, _NetState) -> ok.
trigger(_Place, _Token, _NetState) -> pass.

%% Start and use
{ok, Pid} = gen_pnet:start_link(my_net, {}, []),
{ok, Tokens} = gen_pnet:ls(Pid, step1).
```

---

### gen_yawl - YAWL Wrapper with Enhanced fire/3

**Module:** `src/core/gen_yawl.erl`

Wrapper around gen_pnet that supports 3-tuple returns from fire/3 for automatic user info updates.

#### Key Difference from gen_pnet

The `fire/3` callback supports an enhanced return type:

```erlang
%% Standard gen_pnet return
{produce, #{atom() => [term()]}}
abort

%% Enhanced gen_yawl return (3-tuple)
{produce, #{atom() => [term()]}, NewUsrInfo :: term()}
```

When the 3-tuple is returned, `usr_info` is automatically updated in the net state.

#### Exported API Functions

All functions have the same signatures as gen_pnet:

| Function | Specification | Description |
|----------|---------------|-------------|
| `start_link/3` | `(NetMod::atom(), NetArg::term(), Options::[prop()]) -> start_link_result()` | Start unregistered |
| `start_link/4` | `(ServerName, NetMod, InitArg, Options) -> start_link_result()` | Start registered |
| `ls/2` | `(Name, Place) -> {ok, [term()]} \| {error, #bad_place{}}` | Query tokens |
| `marking/1` | `(Name) -> #{atom() => [term()]}` | Query marking |
| `usr_info/1` | `(Name) -> term()` | Query user info (can be updated via fire/3) |
| `stats/1` | `(Name) -> #stats{}` | Query statistics |
| `reset_stats/1` | `(Name) -> ok` | Clear statistics |
| `stop/1` | `(Name) -> ok` | Stop process |
| `call/2` | `(Name, Request) -> term()` | Synchronous call |
| `call/3` | `(Name, Request, Timeout) -> term()` | Sync with timeout |
| `cast/2` | `(Name, Request) -> ok` | Asynchronous cast |
| `reply/2` | `(Client, Reply) -> ok` | Reply to deferred call |
| `state_property/3` | `(Name, Pred, PlaceLst) -> ok \| {error,_}` | Check predicate |

#### Type Definitions

```erlang
-type fire_result() ::
    abort |
    {produce, #{atom() => [term()]}} |
    {produce, #{atom() => [term()]}, term()}.  % 3-tuple with new usr_info
```

#### Usage Example

```erlang
-module(my_workflow).
-behaviour(gen_yawl).

%% ... structure callbacks same as gen_pnet ...

%% Enhanced fire/3 with state update
fire(t1, #{start := []}, _UsrInfo) ->
    %% Return 3-tuple to update usr_info with new state
    {produce, #{step1 => [done]}, #{step1_completed => true}}.

%% usr_info will now contain #{step1_completed => true}
```

---

## YAWL Compilation

### yawl_compile - YAWL Compiler

**Module:** `src/core/yawl_compile.erl`

Compiles YAWL 2.1/2.2 specifications into gen_pnet compatible Petri net modules.

#### Main Compilation API

| Function | Specification | Description |
|----------|---------------|-------------|
| `compile/2` | `(Spec::spec(), Options::map()) -> {ok, compile_result()} \| {error,Reason}` | Compile to in-memory modules |
| `compile_to_file/3` | `(Spec, Options, OutputDir) -> {ok,[file:filename_all()]} \| {error,Reason}` | Compile and write to files |

#### Code Generation API

| Function | Specification | Description |
|----------|---------------|-------------|
| `generate_module/2` | `(NetId::net_id(), NetInfo::net_info()) -> {ok,binary()} \| {error,Reason}` | Generate complete module code |
| `generate_places/1` | `(NetInfo::net_info()) -> [place()]` | Generate list of place atoms |
| `generate_transitions/1` | `(NetInfo::net_info()) -> [transition()]` | Generate list of transition atoms |

#### Type Definitions

```erlang
-type spec() :: wf_spec:yawl_spec().
-type task_id() :: wf_spec:task_id().
-type place() :: atom().
-type transition() :: atom().
-type net_id() :: binary().

-type compile_option() ::
    {seed, non_neg_integer()} |
    {module_prefix, binary()} |
    {output_dir, file:filename_all()} |
    {include_source, boolean()} |
    {gen_observer, boolean()}.

-type compile_options() :: #{
    seed := non_neg_integer(),
    module_prefix := binary(),
    output_dir => file:filename_all(),
    include_source => boolean(),
    gen_observer => boolean()
}.

-type compile_result() :: #{
    spec_id := binary(),
    modules := #{binary() => module()},
    places := #{binary() => [place()]},
    transitions := #{binary() => [transition()]},
    net_info := #{binary() => map()}
}.
```

#### Options

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `seed` | non_neg_integer() | 0 | For deterministic random number generation |
| `module_prefix` | binary() | <<"yawl_">> | Prefix for generated module names |
| `output_dir` | file:filename_all() | undefined | Directory for writing compiled modules |
| `include_source` | boolean() | false | Include original YAWL source in docs |
| `gen_observer` | boolean() | false | Generate observer callbacks |

---

### yawl_compiled - Compiled Spec Accessors

**Module:** `src/core/yawl_compiled.erl`

Provides pure accessor functions for compiled YAWL specifications.

#### Net Accessor Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `net/2` | `(Compiled::compiled_spec(), NetId::binary()) -> {ok,net_info()} \| {error,not_found}` | Get net info by ID |
| `tasks/2` | `(Compiled, NetId) -> {ok,[atom()]} \| {error,not_found}` | Get task list |
| `places/2` | `(Compiled, NetId) -> {ok,[atom()]} \| {error,not_found}` | Get place list |
| `transitions/2` | `(Compiled, NetId) -> {ok,[atom()]} \| {error,not_found}` | Get transition list |
| `flows/3` | `(Compiled, NetId, FromTask::atom()) -> {ok,[flow_info()]} \| {error,not_found}` | Get flows from a task |
| `net_module/2` | `(Compiled, NetId) -> {ok,module()} \| {error,not_found}` | Get implementing module |

#### Metadata Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `get_metadata/1` | `(Compiled::compiled_spec()) -> #{title:=binary(), version:=binary()}` | Get metadata with defaults |
| `original_spec/1` | `(Compiled::compiled_spec()) -> wf_spec:yawl_spec() \| undefined` | Get original spec |

#### Validation Functions

| Function | Specification | Description |
|----------|---------------|-------------|
| `is_compiled/1` | `(term()) -> boolean()` | Check if term is valid compiled spec |
| `validate/1` | `(Compiled::compiled_spec()) -> ok \| {error,[validation_error()]}` | Validate compiled spec |

---

### yawl_validate - YAWL Specification Validator

**Module:** `src/core/yawl_validate.erl`

Validates YAWL 2.1 specifications for correctness and consistency.

#### Main Validation API

| Function | Specification | Description |
|----------|---------------|-------------|
| `validate/1` | `(Spec::specification()) -> validation_result()` | Validate complete spec |
| `validate_spec/1` | `(Spec::specification()) -> validation_result()` | Alias for validate/1 |

#### Individual Validation Checks

| Function | Specification | Description |
|----------|---------------|-------------|
| `check_tasks/1` | `(specification()) -> [validation_error()]` | Validate task definitions |
| `check_flows/1` | `(specification()) -> [validation_error()]` | Validate flow definitions |
| `check_decompositions/1` | `(specification()) -> [validation_error()]` | Validate decompositions |
| `check_variables/1` | `(specification()) -> [validation_error()]` | Validate variable usage |

#### Error Reporting

| Function | Specification | Description |
|----------|---------------|-------------|
| `format_errors/1` | `([validation_error()]) -> [binary()]` | Format errors for display |

---

## PNET Pure Helpers

### pnet_types - Type Definitions

**Module:** `src/pnet/pnet_types.erl`

Provides total type validators for Petri net data structures.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A place in the Petri net |
| `trsn()` | `atom()` | A transition |
| `token()` | `term()` | Any Erlang term can be a token |
| `marking()` | `#{place() => [token()]}` | Maps places to token multisets |
| `consume_map()` | `#{place() => [token()]}` | Tokens to be consumed |
| `produce_map()` | `#{place() => [token()]}` | Tokens to be produced |
| `mode()` | `#{place() => [token()]}` | Token availability |
| `cmode()` | `{binding(), mode()}` | Colored mode |
| `move()` | `#{trsn := trsn(), mode := mode() \| cmode(), produce := produce_map()}` | Transition firing |
| `receipt()` | Audit record with hashes and timestamp | Execution audit record |

#### Exported Functions

All type validation functions are total (never crash):

```erlang
-spec is_place(term()) -> boolean().
-spec is_trsn(term()) -> boolean().
-spec is_token(term()) -> boolean().
-spec is_var(term()) -> boolean().
-spec is_marking(term()) -> boolean().
-spec is_consume_map(term()) -> boolean().
-spec is_produce_map(term()) -> boolean().
-spec is_mode(term()) -> boolean().
-spec is_binding(term()) -> boolean().
-spec is_cmode(term()) -> boolean().
-spec is_move(term()) -> boolean().
-spec is_receipt(term()) -> boolean().
```

---

### pnet_marking - Marking Algebra

**Module:** `src/pnet/pnet_marking.erl`

Provides multiset marking algebra operations.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A place in the Petri net |
| `token()` | `term()` | Any Erlang term can be a token |
| `marking()` | `#{place() => [token()]}` | Maps places to token multisets |

#### Exported Functions

```erlang
-spec new(Places :: [place()]) -> marking().
```
Creates a new empty marking with the given places.

```erlang
-spec get(Marking :: marking(), Place :: place()) -> {ok, [token()]}.
```
Gets tokens at a specific place. Total function (never crashes).

```erlang
-spec set(Marking :: marking(), Place :: place(), Tokens :: [token()]) -> marking().
```
Sets tokens at a specific place.

```erlang
-spec add(Marking :: marking(), ProduceMap :: produce_map()) -> marking().
```
Adds tokens via multiset union. Respects multiplicity.

```erlang
-spec take(Marking :: marking(), ConsumeMap :: consume_map()) ->
          {ok, marking()} | {error, insufficient}.
```
Takes tokens via multiset subtraction.

```erlang
-spec apply(Marking :: marking(), Move :: move()) ->
          {ok, marking()} | {error, insufficient}.
```
Applies a move atomically (consume then produce).

```erlang
-spec snapshot(Marking :: marking()) -> marking().
```
Creates a snapshot (copy) of the marking.

```erlang
-spec hash(Marking :: marking()) -> binary().
```
Computes stable hash independent of insertion order.

---

### pnet_mode - Mode Enumeration

**Module:** `src/pnet/pnet_mode.erl`

Provides input token selection enumeration for deterministic modes.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A place in the Petri net |
| `token()` | `term()` | Any Erlang term can be a token |
| `marking()` | `#{place() => [token()]}` | Current marking |
| `var()` | `atom()` | Variable name in colored Petri nets |
| `binding()` | `#{var() => term()}` | Variable bindings |
| `cmode()` | `{binding(), mode()}` | Colored mode |
| `net_mod()` | `module()` | Net module |
| `usr_info()` | `term()` | User context |

#### Exported Functions

```erlang
-spec preset_counts(PresetPlaces :: [place()]) -> #{place() => non_neg_integer()}.
```
Returns count of tokens needed from each preset place.

```erlang
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) -> [mode()].
```
Enumerates all possible modes given current marking.

```erlang
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                 UsrInfo :: usr_info(), NetMod :: net_mod()) ->
          {ok, [cmode()]} | {error, term()}.
```
Enumerates colored modes with variable bindings.

---

### pnet_choice - Deterministic Choice

**Module:** `src/pnet/pnet_choice.erl`

Provides deterministic random choice with pure RNG state threading.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `rand_state()` | `rand:state()` | Opaque RNG state |
| `weighted(Elem)` | `{Elem, non_neg_integer()}` | Weighted element |

#### Exported Functions

```erlang
-spec seed(integer()) -> rand_state().
```
Creates RNG state from integer seed using exrop algorithm.

```erlang
-spec pick(List :: [E], RandState :: rand_state()) ->
          {E, rand_state()} | {error, empty}.
```
Uniformly picks random element from non-empty list.

```erlang
-spec pick_weighted(WeightedList :: [weighted(E)], RandState :: rand_state()) ->
          {E, rand_state()} | {error, empty | bad_weights}.
```
Picks element based on weights (probability proportional to weight).

---

### pnet_receipt - Receipt Tracking

**Module:** `src/pnet/pnet_receipt.erl`

Provides immutable audit records for state transitions.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `move()` | Complete transition firing | Transition firing record |
| `receipt()` | Audit record | Execution audit record |

#### Exported Functions

```erlang
-spec make(BeforeHash :: binary(), AfterHash :: binary(), Move :: move()) -> receipt().
```
Creates receipt from state transition with timestamp.

```erlang
-spec timestamp() -> integer().
```
Gets current timestamp in milliseconds using monotonic time.

```erlang
-spec effects(Receipt :: receipt()) ->
    {silent, receipt()} |
    {single_production, receipt()} |
    {multiple_production, receipt()}.
```
Classifies receipt by production effects.

---

## Workflow Utilities

### wf_audit_log - Audit Logging

**Module:** `src/wf/wf_audit_log.erl`

Audit logging for workflow execution events.

### wf_cancel - Cancellation Regions

**Module:** `src/wf/wf_cancel.erl`

Cancellation region management for workflows.

### wf_conc - Concuerror Spec Generator

**Module:** `src/wf/wf_conc.erl`

Generates Concuerror specifications for concurrent testing.

### wf_pool - Process Pool

**Module:** `src/wf/wf_pool.erl`

Process pool for workflow worker management.

### wf_pool_worker - Pool Worker

**Module:** `src/wf/wf_pool_worker.erl`

Individual worker implementation for process pools.

### wf_rules - Rules Engine

**Module:** `src/wf/wf_rules.erl`

Rules engine for workflow decision logic.

### wf_yawl_pred - YAWL Predicates

**Module:** `src/wf/wf_yawl_pred.erl`

Predicate utilities for YAWL workflow evaluation.

### wf_mi - Multi-Instance Tasks

**Module:** `src/wf/wf_mi.erl`

Multi-instance task execution support.

### wf_ops - Process Operations

**Module:** `src/wf/wf_ops.erl`

Common operations on workflow processes.

### wf_store - State Persistence

**Module:** `src/wf/wf_store.erl`

State persistence and recovery for workflows.

### wf_scope - Scope Management

**Module:** `src/wf/wf_scope.erl`

Scope management for workflow variables.

### wf_task - Task Constructors

**Module:** `src/wf/wf_task.erl`

Constructor functions for workflow tasks.

### wf_prop - Property Testing

**Module:** `src/wf/wf_prop.erl`

Property-based testing utilities for workflows.

### wf_time - Time Management

**Module:** `src/wf/wf_time.erl`

Time-related utilities for workflow execution.

### wf_timer - ISO 8601 Duration Parser

**Module:** `src/wf/wf_timer.erl`

Parses ISO 8601 duration strings.

### wf_timerq - Timer Queue

**Module:** `src/wf/wf_timerq.erl`

Timer queue management for scheduled tasks.

---

## Client API

### cre_client Module

**Module:** `src/client/cre_client.erl`

Generic client gen_server for interacting with CRE master processes.

#### Public API Functions

```erlang
-spec start_link(CreName, ClientMod, ClientArg) -> {ok, Pid} | {error, Reason}.
```
Starts an anonymous CRE client gen_server.

**Parameters:**
- `CreName` - Name or pid of the CRE master process
- `ClientMod` - Module implementing the client callbacks
- `ClientArg` - Argument passed to `ClientMod:init/1`

```erlang
-spec start_link(ClientName, CreName, ClientMod, ClientArg) -> {ok, Pid} | {error, Reason}.
```
Starts a named CRE client gen_server.

**Parameters:**
- `ClientName` - Atom name to register the client process
- `CreName` - Name or pid of the CRE master process
- `ClientMod` - Module implementing the client callbacks
- `ClientArg` - Argument passed to `ClientMod:init/1`

```erlang
-spec eval(ClientName, E) -> Result.
```
Evaluates a workflow expression. Blocking call that returns when workflow completes.

```erlang
-spec cre_reply(ClientName, I, A, Delta) -> ok.
```
Handles a reply from the CRE master asynchronously.

```erlang
-spec stop(ClientName) -> ok.
```
Stops the client process gracefully.

#### Callback Interface

Client modules must implement the following callbacks:

```erlang
-callback init(InitArg :: _) -> UsrInfo :: _.
-callback is_value(E :: _, UsrInfo :: _) -> boolean().
-callback step(E :: _, UsrInfo :: _) -> {ok, _, [_]}.
-callback recv(E :: _, ReplyLst :: [{_, _}], UsrInfo :: _) -> _.
-callback load(_, UserInfo :: _) -> _.
-callback unload(_, UserInfo :: _) -> _.
```

#### Client State Record

```erlang
-record(client_state, {
          cre_name,           % Target CRE instance
          client_mod,         % Client callback module
          usr_info,           % User-specific info
          request_map = #{},  % Pending requests
          reply_map = #{},    % Received replies
          state_map = #{}     % Client state
         }).
```

---

### cre_yawl_client Module

**Module:** `src/client/cre_yawl_client.erl`

YAWL-specific client implementation for workflow execution.

#### Public API Functions

```erlang
-spec start_link(CreName, WorkflowExpr) -> {ok, pid()} | {error, term()}.
-spec start_link(ClientName, CreName, WorkflowExpr) -> {ok, pid()} | {error, term()}.
```
Start a YAWL client linked to the CRE master process.

```erlang
-spec execute_workflow(ClientPid, InitialData) -> {ok, workflow_result()} | {error, term()}.
```
Execute a complete workflow synchronously.

```erlang
-spec execute_pattern(ClientPid, Pattern, InputData) -> {ok, workflow_result()} | {error, term()}.
```
Execute a single YAWL pattern with input data.

```erlang
-spec compose_patterns(Patterns, Options) -> workflow_expr().
```
Compose multiple patterns into a single workflow expression.

```erlang
-spec get_workflow_state(ClientPid) -> {ok, map()} | {error, term()}.
```
Return the current execution state of the workflow.

```erlang
-spec get_workflow_results(ClientPid) -> {ok, workflow_result()} | {error, term()}.
```
Return the accumulated results from workflow execution.

```erlang
-spec terminate_workflow(ClientPid) -> ok.
```
Gracefully terminate a running workflow.

#### Pattern Support

The client supports all YAWL workflow patterns:

**Control Flow Patterns:**
- `sequence` - Sequential task execution
- `parallel_split` - Execute tasks in parallel
- `synchronization` - Wait for all tasks to complete
- `exclusive_choice` - Select one branch conditionally
- `simple_merge` - Merge from single source
- `multi_choice` - Select multiple branches
- `synchronizing_merge` - Wait for multiple sources
- `multi_merge` - Merge from multiple sources
- `discriminator` - Select based on condition
- `arbitration` - Select one of multiple options

**Data Flow Patterns:**
- `param_pass` - Pass parameters between tasks
- `data_transform` - Transform data between tasks
- `data_distribute` - Distribute data to multiple tasks
- `data_accumulate` - Accumulate data from multiple tasks
- `data_visibility` - Control data visibility scope

**Resource Patterns:**
- `resource_create` - Create new workflow resource
- `role_allocate` - Allocate based on role
- `resource_start` - Start resource execution
- `role_distribute` - Distribute based on roles
- `capability_allocate` - Allocate by capability

---

## Message Contracts

### Client Messages

Messages sent between client and CRE master:

```erlang
% Request evaluation
{eval, Expr :: term(), From :: {pid(), term()}}

% Worker reply
{cre_reply, From :: {pid(), term()}, Arg :: term(), Delta :: term()}
```

### Worker Messages

Messages sent to/from workflow workers:

```erlang
% Task execution request
{execute, TaskId :: atom(), Arg :: term()}

% Task completion response
{task_complete, TaskId :: atom(), Result :: term()}
```

### Observer Messages

Messages for workflow observation:

```erlang
% Subscribe to workflow events
{subscribe, ObserverPid :: pid(), Filter :: map()}

% Workflow event notification
{workflow_event, EventType :: atom(), EventData :: map()}
```

---

## Compilation

After modifying any workflow or core module, run:

```bash
rebar3 compile
```

This ensures all changes are correctly compiled and type-checked.

---

## Testing

Workflow modules use EUnit for unit tests and Common Test for integration tests:

```bash
# Run all tests
rebar3 eunit

# Run specific module test
rebar3 eunit --module=gen_pnet

# Run integration tests
rebar3 ct

# Run dialyzer for type checking
rebar3 dialyzer
```

---

**Last Updated:** 2026-02-09
