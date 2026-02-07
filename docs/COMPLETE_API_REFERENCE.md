# COMPLETE API REFERENCE

## Table of Contents

1. [Core OTP Behaviors](#core-otp-behaviors)
   - [1. gen_pnet - Generic Petri Net Behavior](#1-gen_pnet---generic-petri-net-behavior)
   - [2. gen_yawl - YAWL Wrapper with Enhanced fire/3](#2-gen_yawl---yawl-wrapper-with-enhanced-fire3)

2. [YAWL Compilation](#yawl-compilation)
   - [3. yawl_compile - YAWL Compiler](#3-yawl_compile---yawl-compiler)
   - [4. yawl_compiled - Compiled Spec Accessors](#4-yawl_compiled---compiled-spec-accessors)
   - [5. yawl_validate - YAWL Validator](#5-yawl_validate---yawl-validator)

3. [PNET Pure Helpers](#pnet-pure-helpers)
   - [6. pnet_types - Type Definitions](#6-pnet_types---type-definitions)
   - [7. pnet_marking - Marking Algebra](#7-pnet_marking---marking-algebra)
   - [8. pnet_mode - Mode Enumeration](#8-pnet_mode---mode-enumeration)
   - [9. pnet_choice - Deterministic Choice](#9-pnet_choice---deterministic-choice)
   - [10. pnet_receipt - Receipt Tracking](#10-pnet_receipt---receipt-tracking)

4. [Workflow Utilities](#workflow-utilities)
   - [11. wf_audit_log - Audit Logging](#11-wf_audit_log---audit-logging)
   - [12. wf_cancel - Cancellation Regions](#12-wf_cancel---cancellation-regions)
   - [13. wf_conc - Concuerror Spec Generator](#13-wf_conc---concuerror-spec-generator)
   - [14. wf_pool - Process Pool](#14-wf_pool---process-pool)
   - [15. wf_pool_worker - Pool Worker](#15-wf_pool_worker---pool-worker)
   - [16. wf_rules - Rules Engine](#16-wf_rules---rules-engine)
   - [17. wf_yawl_pred - YAWL Predicates](#17-wf_yawl_pred---yawl-predicates)
   - [18. wf_mi - Multi-Instance Tasks](#18-wf_mi---multi-instance-tasks)
   - [19. wf_ops - Process Operations](#19-wf_ops---process-operations)
   - [20. wf_store - State Persistence](#20-wf_store---state-persistence)
   - [21. wf_scope - Scope Management](#21-wf_scope---scope-management)
   - [22. wf_task - Task Constructors](#22-wf_task---task-constructors)
   - [23. wf_prop - Property Testing](#23-wf_prop---property-testing)
   - [24. wf_time - Time Management](#24-wf_time---time-management)
   - [25. wf_timer - ISO 8601 Duration Parser](#25-wf_timer---iso-8601-duration-parser)
   - [26. wf_timerq - Timer Queue](#26-wf_timerq---timer-queue)

5. [Message Contracts](#message-contracts)
   - [Client Messages](#client-messages)
   - [Worker Messages](#worker-messages)
   - [Observer Messages](#observer-messages)

6. [Type Definitions](#type-definitions)

---

## Core OTP Behaviors

### 1. gen_pnet - Generic Petri Net Behavior

**File:** `src/core/gen_pnet.erl`
**Lines:** 1,165
**Status:** Complete

The core OTP behavior module for implementing Petri net workflows as Erlang/OTP gen_server processes. Provides automatic transition firing, token processing, and statistics tracking.

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
| `ls/2` | `(Name::name(), Place::atom()) -> {ok, [_]} \| {error, #bad_place{}}` | Query tokens on a place (total function) |
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

### 2. gen_yawl - YAWL Wrapper with Enhanced fire/3

**File:** `src/core/gen_yawl.erl`
**Lines:** 1,030
**Status:** Complete

Wrapper around gen_pnet that supports 3-tuple returns from fire/3 for automatic user info updates. This is particularly useful for YAWL workflows where state needs to be updated during transition firing.

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

**All functions have the same signatures as gen_pnet:**

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

#### Internal Record

```erlang
-record(wrapper_state, {
          net_mod :: atom(),      % Callback module
          net_state :: term()     % Wrapped gen_pnet state
         }).
```

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

### 3. yawl_compile - YAWL Compiler

**File:** `src/core/yawl_compile.erl`
**Lines:** 959
**Status:** Complete

Compiles YAWL 2.1/2.2 specifications into gen_pnet compatible Petri net modules. Each YAWL decomposition becomes a standalone Erlang module.

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
| `generate_place_lst/1` | `(Places) -> binary()` | Generate place_lst/0 code |
| `generate_trsn_lst/1` | `(Transitions) -> binary()` | Generate trsn_lst/0 code |

#### Internal Helper Functions (Exported for Testing)

| Function | Specification | Description |
|----------|---------------|-------------|
| `sanitize_atom_name/1` | `(binary()\|atom()\|list()) -> atom()` | Convert to valid Erlang atom |
| `place_atom/2` | `(binary()\|atom(), binary()) -> atom()` | Create place atom with suffix |
| `transition_atom/2` | `(binary()\|atom(), binary()) -> atom()` | Create transition atom with prefix |
| `build_flow_map/1` | `(spec()) -> #{binary() => [{task_id(),task_id()}]}` | Build flow mapping |
| `extract_net_tasks/2` | `(spec(), binary()) -> [task_id()]` | Get tasks for a net |

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

-type net_info() :: #{
    id := binary(),
    tasks := [task_id()],
    input_condition := binary(),
    output_condition := binary(),
    flows := [{task_id(), task_id(), binary() | undefined}],
    split_types := #{task_id() => atom()},
    join_types := #{task_id() => atom()}
}.
```

#### Generated Module Structure

Each compiled module implements all gen_pnet callbacks:

```erlang
-module(yawl_NetId).
-behaviour(gen_pnet).

%% Structure callbacks
place_lst() -> [input, output, task1, task2, ...].
trsn_lst() -> [t_task1, t_task2, ...].
init_marking(Place, UsrInfo) -> ...  % initial tokens
preset(Transition) -> ...             % input places
is_enabled(Transition, Mode, UsrInfo) -> ...
fire(Transition, Mode, UsrInfo) -> ...

%% Interface callbacks
init(NetArg) -> UsrInfo.
handle_call/3, handle_cast/2, handle_info/2, ...
```

#### Options

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `seed` | non_neg_integer() | 0 | For deterministic random number generation |
| `module_prefix` | binary() | <<"yawl_">> | Prefix for generated module names |
| `output_dir` | file:filename_all() | undefined | Directory for writing compiled modules |
| `include_source` | boolean() | false | Include original YAWL source in docs |
| `gen_observer` | boolean() | false | Generate observer callbacks |

### 4. yawl_compiled - Compiled Spec Accessors

**File:** `src/core/yawl_compiled.erl`
**Lines:** 681
**Status:** Complete

Provides pure accessor functions for compiled YAWL specifications. All functions are total (never crash).

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

#### Type Definitions

```erlang
-type compiled_spec() :: #{
    original := wf_spec:yawl_spec(),
    nets := #{binary() => net_info()},
    metadata := metadata()
}.

-type net_info() :: #{
    id := binary(),
    module := module(),
    places := [atom()],
    transitions := [atom()],
    tasks := [atom()],
    flows := [flow_info()]
}.

-type flow_info() :: #{
    from := atom(),
    to := atom(),
    predicate := binary() | none,
    default := boolean()
}.

-type metadata() :: #{
    title := binary(),
    version := binary()
}.

-type validation_error() :: binary().
```

### 5. yawl_validate - YAWL Specification Validator

**File:** `src/core/yawl_validate.erl`
**Lines:** 1,205
**Status:** Complete

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

#### Type Definitions

```erlang
-type specification() :: #{
    id => binary(),
    name => binary(),
    version => binary() | undefined,
    decomposition => term(),
    tasks => #{binary() => task()},
    conditions => #{binary() => condition()},
    flows => [flow()],
    data_mappings => [mapping()]
}.

-type task() :: #{
    id => binary(),
    name => binary(),
    type => atomic | composite | multiple_instance,
    split_type => 'and' | 'or' | 'xor' | undefined,
    join_type => 'and' | 'or' | 'xor' | undefined,
    decomposition => binary() | undefined,
    min_instances => non_neg_integer() | undefined,
    max_instances => non_neg_integer() | unlimited | undefined,
    continuation_threshold => non_neg_integer() | undefined
}.

-type condition() :: #{
    id => binary(),
    type => input_condition | output_condition,
    expression => binary() | undefined
}.

-type flow() :: #{
    id => binary(),
    source => binary(),
    target => binary(),
    predicate => binary() | undefined
}.

-type mapping() :: #{
    task_id => binary(),
    input => [#{variable => binary(), expression => binary()}],
    output => [#{variable => binary(), expression => binary()}]
}.

-type validation_error() :: #{
    type => required | structure | semantic | reference,
    severity => error | warning,
    message => binary(),
    location => binary() | undefined,
    code => atom()
}.

-type validation_result() :: {ok, [validation_error()]} | {error, [validation_error()]}.
```

#### Error Codes

| Code | Type | Severity | Description |
|------|------|----------|-------------|
| `missing_id` | required | error | Specification ID is required |
| `missing_name` | required | error | Specification name is required |
| `missing_task_id` | required | error | Task ID is required |
| `missing_task_type` | required | error | Task type is required |
| `invalid_task_type` | structure | error | Invalid task type |
| `invalid_split_type` | structure | error | Invalid split type |
| `invalid_join_type` | structure | error | Invalid join type |
| `mi_split_must_be_and` | semantic | error | Multiple instance tasks must use AND split |
| `xor_split_and_join_warning` | semantic | warning | XOR split with AND join may cause deadlock |
| `source_not_found` | reference | error | Flow source not found |
| `target_not_found` | reference | error | Flow target not found |
| `self_loop` | semantic | error | Flow cannot have same source and target |
| `duplicate_flow` | semantic | warning | Duplicate flow between same nodes |
| `isolated_node` | semantic | warning | Node has no incoming or outgoing flows |
| `circular_decomposition` | semantic | error | Circular decomposition reference |

---

## PNET Pure Helpers

### 6. pnet_types - Type Definitions

**File:** `src/pnet/pnet_types.erl`
**Lines:** 557
**Status:** Complete

Provides total type validators for Petri net data structures. All validators are total functions (never crash) and return boolean.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A place in the Petri net where tokens reside |
| `trsn()` | `atom()` | A transition that consumes and produces tokens |
| `token()` | `term()` | Any Erlang term can be a token |
| `marking()` | `#{place() => [token()]}` | Maps places to their token multisets |
| `consume_map()` | `#{place() => [token()]}` | Specifies tokens to be consumed |
| `produce_map()` | `#{place() => [token()]}` | Specifies tokens to be produced |
| `mode()` | `#{place() => [token()]}` | Token availability for transition firing |
| `var()` | `atom()` | Variable name in colored Petri nets |
| `binding()` | `#{var() => term()}` | Maps variables to concrete values |
| `cmode()` | `{binding(), mode()}` | Colored mode combining binding with token mode |
| `move()` | `#{trsn := trsn(), mode := mode() \| cmode(), produce := produce_map()}` | Complete transition firing record |
| `receipt()` | `#{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}` | Execution audit record |

#### Exported Functions

##### Basic Type Validators

```erlang
-spec is_place(term()) -> boolean().
```
- Returns `true` if term is an atom (valid place)
- Never crashes

```erlang
-spec is_trsn(term()) -> boolean().
```
- Returns `true` if term is an atom (valid transition)
- Never crashes

```erlang
-spec is_token(term()) -> boolean().
```
- Always returns `true` (any Erlang term is a valid token)
- Never crashes

```erlang
-spec is_var(term()) -> boolean().
```
- Returns `true` if term is an atom (valid variable name)
- Never crashes

##### State Type Validators

```erlang
-spec is_marking(term()) -> boolean().
```
- Returns `true` if term is a map with atom keys and list values
- Never crashes

```erlang
-spec is_consume_map(term()) -> boolean().
```
- Returns `true` if term is a map with atom keys and list values
- Never crashes

```erlang
-spec is_produce_map(term()) -> boolean().
```
- Returns `true` if term is a map with atom keys and list values
- Never crashes

```erlang
-spec is_mode(term()) -> boolean().
```
- Returns `true` if term is a map with atom keys and list values
- Never crashes

##### Colored Type Validators

```erlang
-spec is_binding(term()) -> boolean().
```
- Returns `true` if term is a map with atom keys (variables)
- Values can be any term
- Never crashes

```erlang
-spec is_cmode(term()) -> boolean().
```
- Returns `true` if term is a 2-tuple of `{binding(), mode()}`
- Accepts `{}` as empty binding representation
- Never crashes

##### Execution Type Validators

```erlang
-spec is_move(term()) -> boolean().
```
- Returns `true` if term is a map with required keys: `trsn`, `mode`, `produce`
- Validates nested types
- Never crashes

```erlang
-spec is_receipt(term()) -> boolean().
```
- Returns `true` if term is a map with required keys: `before_hash`, `after_hash`, `move`, `ts`
- Validates nested types
- Never crashes

### 7. pnet_marking - Marking Algebra

**File:** `src/pnet/pnet_marking.erl`
**Lines:** 483
**Status:** Complete

Provides multiset marking algebra operations (places -> bag of tokens).

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A place in the Petri net |
| `token()` | `term()` | Any Erlang term can be a token |
| `marking()` | `#{place() => [token()]}` | Maps places to token multisets |
| `consume_map()` | `#{place() => [token()]}` | Tokens to be removed |
| `produce_map()` | `#{place() => [token()]}` | Tokens to be added |
| `move()` | `#{mode := consume_map(), produce := produce_map()}` | Transition firing |

#### Exported Functions

##### Creation and Basic Operations

```erlang
-spec new(Places :: [place()]) -> marking().
```
- Creates a new empty marking with the given places
- All places initialized with empty token lists
- Example: `new([p1, p2])` -> `#{p1 => [], p2 => []}`

```erlang
-spec get(Marking :: marking(), Place :: place()) -> {ok, [token()]}.
```
- Gets tokens at a specific place
- Returns `{ok, []}` for unknown places (total function)
- Never crashes

```erlang
-spec set(Marking :: marking(), Place :: place(), Tokens :: [token()]) -> marking().
```
- Sets tokens at a specific place
- Replaces current token list
- Place must exist in marking

##### Marking Algebra Operations

```erlang
-spec add(Marking :: marking(), ProduceMap :: produce_map()) -> marking().
```
- Adds tokens via multiset union
- Appends tokens to existing tokens
- Creates places if they don't exist
- Respects multiplicity: `[a,b] + [a] = [a,a,b]`

```erlang
-spec take(Marking :: marking(), ConsumeMap :: consume_map()) ->
          {ok, marking()} | {error, insufficient}.
```
- Takes tokens via multiset subtraction
- Returns `{ok, Marking}` if all tokens available with sufficient multiplicity
- Returns `{error, insufficient}` otherwise
- Example: taking `[a]` from `[a,a,b]` leaves `[a,b]`

```erlang
-spec apply(Marking :: marking(), Move :: move()) ->
          {ok, marking()} | {error, insufficient}.
```
- Applies a move atomically (consume then produce)
- Move contains `mode` (tokens to consume) and `produce` (tokens to add)
- Returns error if consumption fails

##### Inspection and Utilities

```erlang
-spec snapshot(Marking :: marking()) -> marking().
```
- Creates a snapshot (copy) of the marking
- Due to immutability, returns same marking
- Provides semantic clarity for API users

```erlang
-spec hash(Marking :: marking()) -> binary().
```
- Computes stable hash independent of insertion order
- Uses SHA-256 on canonical representation
- Sorts by place name and token values for consistency

### 8. pnet_mode - Mode Enumeration

**File:** `src/pnet/pnet_mode.erl`
**Lines:** 352
**Status:** Complete

Provides input token selection enumeration for deterministic modes.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `place()` | `atom()` | A place in the Petri net |
| `token()` | `term()` | Any Erlang term can be a token |
| `marking()` | `#{place() => [token()]}` | Current marking of the net |
| `mode()` | `#{place() => [token()]}` | Token availability for transition |
| `var()` | `atom()` | Variable name in colored Petri nets |
| `binding()` | `#{var() => term()}` | Variable bindings |
| `cmode()` | `{binding(), mode()}` | Colored mode with bindings |
| `net_mod()` | `module()` | Net module implementing `pnet_net` behavior |
| `usr_info()` | `term()` | User context for net callbacks |

#### Exported Functions

##### Mode Enumeration

```erlang
-spec preset_counts(PresetPlaces :: [place()]) ->
          #{place() => non_neg_integer()}.
```
- Returns count of tokens needed from each preset place
- Handles multiplicity: `[p1,p2,p1]` -> `#{p1 => 2, p2 => 1}`
- Used to determine how many tokens to select from each input place

```erlang
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) -> [mode()].
```
- Enumerates all possible modes given current marking
- Returns list of valid token selections
- Empty list if no mode possible (insufficient tokens)
- Deterministic ordering (term order)
- Examples:
  - Preset `[p1,p2]`, marking `#{p1 => [a,b], p2 => [c]}` -> 2 modes
  - Preset `[p1,p1]`, marking `#{p1 => [a,b,c]}` -> 3 combinations

##### Colored Net Extension

```erlang
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                 UsrInfo :: usr_info(), NetMod :: net_mod()) ->
          {ok, [cmode()]} | {error, term()}.
```
- Enumerates colored modes with variable bindings
- Calls net module's `cmodes/3` callback if available
- Falls back to basic enumeration with empty bindings
- Returns `{ok, [{Binding, Mode}]}` on success
- Handles errors gracefully with fallback

### 9. pnet_choice - Deterministic Choice

**File:** `src/pnet/pnet_choice.erl`
**Lines:** 334
**Status:** Complete

Provides deterministic random choice with pure RNG state threading.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `rand_state()` | `rand:state()` | Opaque RNG state from rand module |
| `weighted(Elem)` | `{Elem, non_neg_integer()}` | Weighted element for choice |

#### Exported Functions

##### RNG State Management

```erlang
-spec seed(integer()) -> rand_state().
```
- Creates RNG state from integer seed
- Uses `exrop` algorithm for cross-platform determinism
- Same seed always produces same sequence of choices
- Example: `seed(42)` creates deterministic state

##### Choice Operations

```erlang
-spec pick(List :: [E], RandState :: rand_state()) ->
          {E, rand_state()} | {error, empty}.
```
- Uniformly picks random element from non-empty list
- Returns `{Element, NewState}` tuple
- Same seed + same list = same element (deterministic)
- Returns `{error, empty}` for empty list
- Advances RNG state

```erlang
-spec pick_weighted(WeightedList :: [weighted(E)], RandState :: rand_state()) ->
          {E, rand_state()} | {error, empty | bad_weights}.
```
- Picks element based on weights
- Probability proportional to weight
- Zero-weight elements never selected
- Returns `{error, empty}` for empty list
- Returns `{error, bad_weights}` for invalid weights or zero total weight
- Validates weights are non-negative integers

### 10. pnet_receipt - Receipt Tracking

**File:** `src/pnet/pnet_receipt.erl`
**Lines:** 183
**Status:** Complete

Provides immutable audit records for state transitions.

#### Exported Types

| Type | Definition | Description |
|------|------------|-------------|
| `move()` | `#{trsn := atom(), mode := pnet_types:mode() \| pnet_types:cmode(), produce := pnet_types:produce_map()}` | Complete transition firing |
| `receipt()` | `#{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}` | Execution audit record |

#### Exported Functions

##### Receipt Creation

```erlang
-spec make(BeforeHash :: binary(),
           AfterHash :: binary(),
           Move :: move()) -> receipt().
```
- Creates receipt from state transition
- Takes before/after marking hashes and executed move
- Validates move contains required fields (`trsn`, `mode`, `produce`)
- Adds timestamp using `timestamp/0`
- Returns complete receipt map

```erlang
-spec timestamp() -> integer().
```
- Gets current timestamp in milliseconds
- Uses `erlang:monotonic_time(millisecond)`
- Monotonic (always increasing)
- Suitable for ordering receipts

##### Receipt Analysis

```erlang
-spec effects(Receipt :: receipt()) ->
    {silent, receipt()} |
    {single_production, receipt()} |
    {multiple_production, receipt()}.
```
- Classifies receipt by production effects
- Returns `{silent, Receipt}` if no tokens produced (empty produce map)
- Returns `{single_production, Receipt}` if tokens to exactly one place
- Returns `{multiple_production, Receipt}` if tokens to multiple places
- Useful for routing based on transition effects

---

---