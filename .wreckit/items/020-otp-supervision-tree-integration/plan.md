# OTP Supervision Tree Integration Implementation Plan

## Implementation Plan Title
Unified Workflow Case Supervision Tree for CRE Runtime

## Overview
Create a unified `wf_case_sup` supervisor that provides production-grade OTP supervision for all workflow case types (gen_yawl, wf_engine, and custom implementations). This supervisor will integrate into the existing CRE supervision tree under `cre_sup`, providing fault tolerance, clean shutdown/restart, and proper lifecycle management for workflow cases.

The implementation builds upon existing supervision patterns in the codebase (`yawl_workflow_supervisor`, `yawl_supervisor`, `active_token_sup`) while providing a unified, extensible approach that supports multiple workflow execution engines.

## Current State

The CRE system has a mature supervision tree with the following structure:

**Existing Supervisors:**
- `cre_sup` (top-level, `/Users/sac/cre/src/app/cre_sup.erl:96-295`) - one_for_one strategy, 8 children
- `yawl_workflow_supervisor` (`/Users/sac/cre/src/app/yawl_workflow_supervisor.erl:1-99`) - simple_one_for_one, manages gen_yawl processes
- `yawl_supervisor` (`/Users/sac/cre/src/yawl/yawl_supervisor.erl:1-588`) - feature-rich gen_yawl supervisor with gproc registry
- `active_token_sup` (`/Users/sac/cre/src/active/active_token_sup.erl:1-93`) - one_for_one for token processes
- `license_sup` - simple one_for_one supervisor

**Workflow Execution Engines:**
- `gen_yawl` (`/Users/sac/cre/src/core/gen_yawl.erl:1-1556`) - gen_server wrapper around gen_pnet
- `wf_engine` (`/Users/sac/cre/src/wf/wf_engine.erl:1-1594`) - gen_server managing multiple cases internally

**What's Missing:**
- No unified supervisor that handles both gen_yawl and wf_engine cases
- No case-level process isolation (wf_engine cases are data within the engine process)
- No consistent case lifecycle management across execution engines
- No unified case registry for lookup and monitoring

### Key Discoveries:

1. **Pattern Found:** The `yawl_workflow_supervisor` uses `simple_one_for_one` strategy (lines 85-89) which is perfect for dynamic case creation - all children share the same child spec template.

2. **Pattern Found:** The `active_token_sup` provides a clean API wrapper pattern with `start_token/4` and `terminate_token/1` (lines 51-65) that we should follow.

3. **Constraint:** `wf_engine` manages cases as data within a single gen_server process (line 171 in wf_engine.erl: `cases = #{} :: #{case_id() => wf_case()}`), not as separate processes. This means we need to either:
   - Keep wf_engine as-is (cases are not supervised individually)
   - Create case runner processes that delegate to wf_engine
   - Migrate wf_engine to one-process-per-case model

4. **Integration Point:** `cre_sup` line 259-266 shows the pattern for adding a new supervisor child spec. We'll add `wf_case_sup` following this pattern.

5. **Registry Pattern:** `yawl_supervisor` uses gproc for process registration (lines 442-456) with metadata support. We should use this pattern for case lookup.

## Desired End State

A unified `wf_case_sup` supervisor that:

1. **Manages multiple case types:** Supports gen_yawl workflows, wf_engine cases, and custom implementations
2. **Proper OTP supervision:** Uses simple_one_for_one strategy with temporary restart for completed cases
3. **Clean lifecycle:** Start, stop, query cases via consistent API
4. **Case registry:** gproc-based lookup by case_id with metadata tracking
5. **Fault tolerance:** Case crashes are isolated and logged
6. **Integration:** Registered as child under `cre_sup` between `yawl_workflow_supervisor` and `yawl_worklist`

**Verification:**
- All existing tests pass without modification
- New supervisor accepts start_link/0 and starts successfully under cre_sup
- Cases can be started via `wf_case_sup:start_case/3` and stopped via `wf_case_sup:stop_case/1`
- `wf_case_sup:list_cases/0` returns all active cases
- `wf_case_sup:find_case/1` locates cases by ID
- Supervisor tree visible via `supervisor:which_children(cre_sup)`

## What We're NOT Doing

**Explicitly Out of Scope:**

1. **Modifying existing supervisors:** `yawl_workflow_supervisor` and `yawl_supervisor` remain unchanged
2. **Refactoring wf_engine:** The wf_engine continues to manage cases internally as data (not converting to one-process-per-case)
3. **Effect worker supervision:** Per-item effect workers are not supervised (deferred to future work)
4. **Distributed case execution:** All cases run on the same node (no cross-node supervision)
5. **Hot code upgrade:** No appup/relup files created (standard OTP upgrade assumed)
6. **Case data persistence:** No new persistence layer (delegates to existing yawl_persistence)
7. **Backward compatibility breakage:** All existing APIs continue working unchanged
8. **Migration utilities:** No tools to migrate from old supervisors to new (opt-in only)

## Implementation Approach

**High-Level Strategy:**

Create a new `wf_case_sup` module that serves as a unified entry point for workflow case supervision. The supervisor uses `simple_one_for_one` strategy for dynamic case creation. Each case is wrapped in a `wf_case_runner` gen_server that:

1. Delegates to the appropriate execution engine (gen_yawl, wf_engine, or custom)
2. Tracks case metadata (case_id, spec_id, status, timestamps)
3. Registers with gproc for lookup
4. Provides uniform query interface
5. Handles graceful shutdown

**Design Rationale:**

- **simple_one_for_one:** Perfect fit for dynamic children with identical child spec (pattern from yawl_workflow_supervisor:84-98)
- **Case runner wrapper:** Isolates execution engines from supervision concerns, allows mixed engine types under same supervisor
- **gproc registry:** Proven pattern in yawl_supervisor:442-456, supports metadata queries
- **temporary restart:** Completed cases shouldn't restart (prevents zombie processes)
- **No wf_engine refactoring:** Cases remain as data within wf_engine to minimize risk

**Risk Mitigation:**

- **Zero breaking changes:** New supervisor is additive, existing code unaffected
- **Incremental adoption:** Teams can migrate to wf_case_sup at their own pace
- **Fallback to existing:** If issues arise, can use yawl_workflow_supervisor directly
- **Isolated testing:** New module has independent test suite

---

## Phases

### Phase 1: Create wf_case_sup Module

#### Overview
Create the core supervisor module with basic case lifecycle management. This provides the foundation for unified case supervision.

#### Changes Required:

##### 1. Create wf_case_sup.erl
**File:** `/Users/sac/cre/src/wf/wf_case_sup.erl`
**Changes:** New module implementing supervisor behavior with simple_one_for_one strategy

```erlang
%% -*- erlang -*-
%% @doc Workflow Case Supervisor
%%
%% Unified supervisor for workflow case execution across multiple engines.
%% Provides fault tolerance, clean lifecycle, and case registry.
%% @end

-module(wf_case_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_case/3, stop_case/1, stop_case/2]).
-export([list_cases/0, find_case/1, get_case_status/1]).
-export([case_count/0]).

%% Supervisor callbacks
-export([init/1]).

%% Types
-type case_id() :: binary().
-type spec_id() :: binary() | atom().
-type case_options() :: #{
    type => gen_yawl | wf_engine | custom,
    timeout => pos_integer() | infinity,
    auto_continue => boolean()
}.
-type case_info() :: #{
    case_id => case_id(),
    spec_id => spec_id(),
    pid => pid() | undefined,
    status => running | completed | cancelled | failed,
    started_at => integer()
}.

-export_type([case_id/0, spec_id/0, case_options/0, case_info/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the workflow case supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new workflow case under supervision.
-spec start_case(SpecId, InitialData, Options) -> {ok, pid()} | {error, term()} when
      SpecId :: spec_id(),
      InitialData :: map(),
      Options :: case_options().
start_case(SpecId, InitialData, Options) ->
    supervisor:start_child(?MODULE, [SpecId, InitialData, Options]).

%% @doc Stops a workflow case gracefully.
-spec stop_case(case_id()) -> ok | {error, term()}.
stop_case(CaseId) ->
    stop_case(CaseId, 5000).

%% @doc Stops a workflow case with timeout.
-spec stop_case(case_id(), pos_integer()) -> ok | {error, term()}.
stop_case(CaseId, Timeout) ->
    case find_case(CaseId) of
        {ok, Pid} ->
            case supervisor:terminate_child(?MODULE, Pid) of
                ok -> ok;
                {error, not_found} -> ok;  % Already terminated
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @doc Lists all active cases.
-spec list_cases() -> [case_info()].
list_cases() ->
    Children = supervisor:which_children(?MODULE),
    lists:filtermap(
        fun({_Id, Pid, _Type, _Modules}) ->
            case Pid of
                undefined -> false;
                _ when is_pid(Pid) ->
                    try
                        {ok, Info} = wf_case_runner:get_info(Pid),
                        {true, Info}
                    catch
                        _:_ -> false
                    end
            end
        end,
        Children
    ).

%% @doc Finds a case by ID.
-spec find_case(case_id()) -> {ok, pid()} | {error, not_found}.
find_case(CaseId) ->
    case gproc:lookup_local_name({wf_case, CaseId}) of
        {Pid, _Value} -> {ok, Pid};
        undefined -> {error, not_found}
    end.

%% @doc Gets the current status of a case.
-spec get_case_status(case_id()) -> {ok, case_info()} | {error, not_found}.
get_case_status(CaseId) ->
    case find_case(CaseId) of
        {ok, Pid} ->
            try
                {ok, Info} = wf_case_runner:get_info(Pid),
                {ok, Info}
            catch
                _:_ -> {error, not_responding}
            end;
        Error ->
            Error
    end.

%% @doc Returns the count of active cases.
-spec case_count() -> non_neg_integer().
case_count() ->
    length(supervisor:which_children(?MODULE)).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => case_instance,
        start => {wf_case_runner, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [wf_case_runner]
    },
    {ok, {SupFlags, [ChildSpec]}}.
```

##### 2. Create wf_case_runner.erl
**File:** `/Users/sac/cre/src/wf/wf_case_runner.erl`
**Changes:** New gen_server that wraps workflow execution engines

```erlang
%% -*- erlang -*-
%% @doc Workflow Case Runner
%%
%% Gen_server that wraps workflow execution engines (gen_yawl, wf_engine, custom).
%% Provides uniform interface for case lifecycle management.
%% @end

-module(wf_case_runner).
-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([get_info/1]).
-export([cancel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Types
-type case_type() :: gen_yawl | wf_engine | custom.
-type case_status() :: pending | running | completed | cancelled | failed.

-record(case_state, {
    case_id :: binary(),
    spec_id :: binary() | atom(),
    case_type :: case_type(),
    status :: case_status(),
    workflow_pid :: pid() | undefined,
    started_at :: integer(),
    completed_at :: integer() | undefined,
    data :: map(),
    options :: map()
}).

-type state() :: #case_state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts a case runner.
-spec start_link(SpecId, InitialData, Options) -> {ok, pid()} | {error, term()} when
      SpecId :: binary() | atom(),
      InitialData :: map(),
      Options :: map().
start_link(SpecId, InitialData, Options) ->
    gen_server:start_link(?MODULE, [SpecId, InitialData, Options], []).

%% @doc Gets case information.
-spec get_info(pid()) -> {ok, map()} | {error, term()}.
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info).

%% @doc Cancels a running case.
-spec cancel(pid()) -> ok | {error, term()}.
cancel(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, cancel).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([term()]) -> {ok, state()}.
init([SpecId, InitialData, Options]) ->
    CaseId = generate_case_id(),
    CaseType = maps:get(type, Options, gen_yawl),
    Now = erlang:system_time(millisecond),

    State = #case_state{
        case_id = CaseId,
        spec_id = SpecId,
        case_type = CaseType,
        status = pending,
        started_at = Now,
        data = InitialData,
        options = Options
    },

    % Register with gproc
    gproc:reg_local_name({wf_case, CaseId}, CaseId),
    gproc:reg_local_prop({wf_case, CaseId}, spec_id, SpecId),
    gproc:reg_local_prop({wf_case, CaseId}, started_at, Now),

    % Start workflow based on type
    case start_workflow(CaseType, SpecId, CaseId, InitialData, Options) of
        {ok, WfPid} ->
            {ok, State#case_state{workflow_pid = WfPid, status = running}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(get_info, _From, #case_state{} = State) ->
    Info = #{
        case_id => State#case_state.case_id,
        spec_id => State#case_state.spec_id,
        type => State#case_state.case_type,
        status => State#case_state.status,
        started_at => State#case_state.started_at,
        completed_at => State#case_state.completed_at,
        workflow_pid => State#case_state.workflow_pid
    },
    {reply, {ok, Info}, State};

handle_call(cancel, _From, #case_state{workflow_pid = undefined} = State) ->
    {reply, {error, not_running}, State};
handle_call(cancel, _From, #case_state{workflow_pid = WfPid, status = running} = State) ->
    case State#case_state.case_type of
        gen_yawl ->
            gen_yawl:stop(WfPid);
        wf_engine ->
            %% wf_engine doesn't have per-case processes, signal via engine
            ok
    end,
    {reply, ok, State#case_state{status = cancelled}};
handle_call(cancel, _From, State) ->
    {reply, {error, invalid_status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', WfPid, Reason}, #case_state{workflow_pid = WfPid} = State) ->
    case Reason of
        normal ->
            {noreply, State#case_state{status = completed,
                                       completed_at = erlang:system_time(millisecond)}};
        shutdown ->
            {noreply, State#case_state{status = cancelled,
                                       completed_at = erlang:system_time(millisecond)}};
        _ ->
            {noreply, State#case_state{status = failed}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #case_state{case_id = CaseId}) ->
    % Unregister from gproc
    gproc:unreg_local_name({wf_case, CaseId}),
    gproc:unreg_local_prop({wf_case, CaseId}, spec_id),
    gproc:unreg_local_prop({wf_case, CaseId}, started_at),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec start_workflow(case_type(), term(), binary(), map(), map()) ->
    {ok, pid()} | {error, term()}.
start_workflow(gen_yawl, SpecId, CaseId, InitialData, Options) ->
    NetMod = SpecId,  % SpecId is the net module for gen_yawl
    NetArg = maps:get(net_arg, Options, #{}),
    GenYawlOptions = maps:get(gen_yawl_options, Options, []),
    gen_yawl:start_link(undefined, NetMod, NetArg, GenYawlOptions);

start_workflow(wf_engine, SpecId, _CaseId, InitialData, Options) ->
    %% For wf_engine, we'd need to create a case within the engine
    %% This is deferred - wf_engine cases remain unsupervised at process level
    {error, wf_engine_not_supported};

start_workflow(custom, SpecId, CaseId, InitialData, Options) ->
    %% Custom implementations provide their own start_module/start_function
    case {maps:get(start_module, Options, undefined), maps:get(start_function, Options, undefined)} of
        {Mod, Fun} when Mod =/= undefined, Fun =/= undefined ->
            apply(Mod, Fun, [SpecId, CaseId, InitialData, Options]);
        _ ->
            {error, {missing_custom_config, start_module, start_function}}
    end.

%% @private
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Module compiles: `erlc -I /Users/sac/cre/src -o /Users/sac/cre/src/wf /Users/sac/cre/src/wf/wf_case_sup.erl`
- [ ] Module compiles: `erlc -I /Users/sac/cre/src -o /Users/sac/cre/src/wf /Users/sac/cre/src/wf/wf_case_runner.erl`
- [ ] Dialyzer passes: `dialyzer --src -I /Users/sac/cre/src /Users/sac/cre/src/wf/wf_case_sup.erl`
- [ ] Dialyzer passes: `dialyzer --src -I /Users/sac/cre/src /Users/sac/cre/src/wf/wf_case_runner.erl`

##### Manual Verification:
- [ ] `wf_case_sup` module can be loaded: `l(wf_case_sup).` returns `{module, wf_case_sup}`
- [ ] `wf_case_runner` module can be loaded: `l(wf_case_runner).` returns `{module, wf_case_runner}`
- [ ] Exports are correct: `wf_case_sup:module_info(exports).` contains expected functions
- [ ] No syntax errors in doctests

**Note:** Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Integrate wf_case_sup into cre_sup

#### Overview
Add `wf_case_sup` as a child of `cre_sup`, following the existing pattern for supervisor children.

#### Changes Required:

##### 1. Modify cre_sup.erl
**File:** `/Users/sac/cre/src/app/cre_sup.erl`
**Changes:** Add wf_case_sup child spec after yawl_workflow_supervisor (line 266)

```erlang
%% After line 266 (after WorkflowSupSpec), add:

    CaseSupSpec = #{
        id => wf_case_sup,
        start => {wf_case_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [wf_case_sup]
    },

%% Then update line 295 to include CaseSupSpec in the list:
%% Old: {ok, {SupFlags, [ChildSpec, TimeoutSpec, XesSpec, ApprovalSpec, WorkflowSupSpec, WorklistSpec, RegistrySpec, LicenseSupSpec]}}.
%% New:
    {ok, {SupFlags, [ChildSpec, TimeoutSpec, XesSpec, ApprovalSpec, WorkflowSupSpec, CaseSupSpec, WorklistSpec, RegistrySpec, LicenseSupSpec]}}.
```

Also update doctest_test/0 to expect 9 children instead of 8:

```erlang
%% Line 345: Change from 8 = length(Children) to:
    9 = length(Children),

%% Line 394: Change from 6 = length(WorkerCount) to:
    7 = length(WorkerCount),
%% And 2 = length(SupCount) to:
    3 = length(SupCount),
```

Update child IDs test (lines 362-371):

```erlang
%% Add wf_case_sup to the list:
    true = lists:member(wf_case_sup, ChildIds),
```

#### Success Criteria:

##### Automated Verification:
- [ ] cre_sup compiles: `erlc -I /Users/sac/cre/src -o /Users/sac/cre/src/app /Users/sac/cre/src/app/cre_sup.erl`
- [ ] Doctests pass: `erlc -DTEST +export_all /Users/sac/cre/src/app/cre_sup.erl && erl -noshell -eval "cre_sup:doctest_test(), halt()."`
- [ ] Application starts: `erl -noshell -eval "application:ensure_all_started(cre), halt()."`

##### Manual Verification:
- [ ] CRE application starts successfully: `application:start(cre).`
- [ ] wf_case_sup is running: `erlang:whereis(wf_case_sup)` returns a PID
- [ ] Supervisor tree includes wf_case_sup: `supervisor:which_children(cre_sup)` contains `{wf_case_sup, Pid, supervisor, [wf_case_sup]}`
- [ ] Existing services (cre_master, yawl_timeout, etc.) still running

**Note:** Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Add Unit Tests

#### Overview
Create comprehensive unit tests for the new supervisor and case runner modules.

#### Changes Required:

##### 1. Create wf_case_sup_tests.erl
**File:** `/Users/sac/cre/test/wf_case_sup_tests.erl`
**Changes:** New test module

```erlang
%% -*- erlang -*-
%% @doc Unit tests for wf_case_sup

-module(wf_case_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

setup() ->
    {ok, Pid} = wf_case_sup:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% Test Cases
%%====================================================================

wf_case_sup_start_stop_test() ->
    {ok, Pid} = setup(),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(wf_case_sup)),
    cleanup(Pid),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(wf_case_sup)).

wf_case_sup_list_cases_empty_test() ->
    Pid = setup(),
    ?assertEqual([], wf_case_sup:list_cases()),
    cleanup(Pid).

wf_case_sup_find_case_not_found_test() ->
    Pid = setup(),
    CaseId = <<"nonexistent_case">>,
    ?assertEqual({error, not_found}, wf_case_sup:find_case(CaseId)),
    cleanup(Pid).

wf_case_sup_case_count_test() ->
    Pid = setup(),
    ?assertEqual(0, wf_case_sup:case_count()),
    cleanup(Pid).

%%====================================================================
%% Test Generators
%%====================================================================

wf_case_sup_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"start and stop", fun wf_case_sup_start_stop_test/0},
         {"list cases empty", fun wf_case_sup_list_cases_empty_test/0},
         {"find case not found", fun wf_case_sup_find_case_not_found_test/0},
         {"case count", fun wf_case_sup_case_count_test/0}
     ]}.
```

##### 2. Create wf_case_runner_tests.erl
**File:** `/Users/sac/cre/test/wf_case_runner_tests.erl`
**Changes:** New test module

```erlang
%% -*- erlang -*-
%% @doc Unit tests for wf_case_runner

-module(wf_case_runner_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

wf_case_runner_generate_case_id_test() ->
    CaseId = wf_case_runner_test_support:generate_case_id(),
    ?assert(is_binary(CaseId)),
    ?assertEqual(<<"case_">>, binary:part(CaseId, {0, 5})),
    ?assert(byte_size(CaseId) > 10).

%% Note: Full integration tests require actual workflow modules
%% These would be added in a follow-up phase
```

#### Success Criteria:

##### Automated Verification:
- [ ] Test files compile: `erlc -I /Users/sac/cre/src -o /Users/sac/cre/test /Users/sac/cre/test/wf_case_sup_tests.erl`
- [ ] Test files compile: `erlc -I /Users/sac/cre/src -o /Users/sac/cre/test /Users/sac/cre/test/wf_case_runner_tests.erl`
- [ ] Tests pass: `erl -noshell -eval "eunit:test(wf_case_sup_tests, [verbose]), halt()."`
- [ ] Tests pass: `erl -noshell -eval "eunit:test(wf_case_runner_tests, [verbose]), halt()."`

##### Manual Verification:
- [ ] All tests execute without errors
- [ ] Test coverage report shows >80% for new modules

**Note:** Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Integration Testing

#### Overview
Create integration tests that verify the supervisor works with actual workflow engines.

#### Changes Required:

##### 1. Create integration test file
**File:** `/Users/sac/cre/test/wf_case_integration_tests.erl`
**Changes:** New integration test module

```erlang
%% -*- erlang -*-
%% @doc Integration tests for wf_case_sup

-module(wf_case_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(cre),
    ok.

cleanup(_Arg) ->
    application:stop(cre),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"wf_case_sup starts under cre_sup", fun test_wf_case_sup_under_cre_sup/0},
         {"wf_case_sup list cases", fun test_list_cases/0},
         {"wf_case_sup case count", fun test_case_count/0}
     ]
    }.

test_wf_case_sup_under_cre_sup() ->
    Children = supervisor:which_children(cre_sup),
    ?assert(lists:keymember(wf_case_sup, 1, Children)),
    {wf_case_sup, Pid, supervisor, [wf_case_sup]} = lists:keyfind(wf_case_sup, 1, Children),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(wf_case_sup)).

test_list_cases() ->
    Cases = wf_case_sup:list_cases(),
    ?assert(is_list(Cases)),
    %% Initially empty
    ?assertEqual(0, length(Cases)).

test_case_count() ->
    ?assert(is_integer(wf_case_sup:case_count())),
    ?assertEqual(0, wf_case_sup:case_count()).
```

#### Success Criteria:

##### Automated Verification:
- [ ] Integration test compiles
- [ ] Tests pass with CRE application running
- [ ] No errors in test output

##### Manual Verification:
- [ ] Start CRE: `application:start(cre).`
- [ ] Verify wf_case_sup is child of cre_sup: `supervisor:which_children(cre_sup)`
- [ ] Query cases: `wf_case_sup:list_cases()`
- [ ] Check case count: `wf_case_sup:case_count()`
- [ ] Stop CRE: `application:stop(cre)` - verify clean shutdown

**Note:** Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Documentation

#### Overview
Add module documentation, examples, and integration guide.

#### Changes Required:

##### 1. Update README or create WF_CASE_SUPERVISION.md
**File:** `/Users/sac/cre/docs/wf_case_supervision.md` (or update existing README)
**Changes:** Add documentation for wf_case_sup usage

```markdown
# Workflow Case Supervision

## Overview

The `wf_case_sup` module provides unified supervision for workflow case execution.
It supports multiple workflow engines (gen_yawl, wf_engine, custom) under a single
supervision tree.

## Basic Usage

### Starting a Case

```erlang
% Start a gen_yawl workflow case
{ok, CasePid} = wf_case_sup:start_case(
    my_workflow_module,  % SpecId (net module for gen_yawl)
    #{data => #{amount => 100}},  % Initial data
    #{type => gen_yawl}  % Options
).

% Get case status
{ok, Info} = wf_case_sup:get_info(CasePid).
```

### Listing Active Cases

```erlang
Cases = wf_case_sup:list_cases().
% Returns: [#{case_id => ..., spec_id => ..., status => running, ...}]
```

### Stopping a Case

```erlang
ok = wf_case_sup:stop_case(CaseId).
```

## Architecture

```
cre_sup
  └── wf_case_sup (simple_one_for_one)
        ├── wf_case_runner (case_abc123) → gen_yawl process
        ├── wf_case_runner (case_def456) → gen_yawl process
        └── wf_case_runner (case_ghi789) → custom workflow
```

## Migration from Existing Supervisors

- `yawl_workflow_supervisor` - Continue using for gen_yawl-only workflows
- `yawl_supervisor` - Use for advanced features (pause/resume, gproc metadata)
- `wf_case_sup` - Use for unified case management across engine types

## Future Work

- Effect worker supervision
- Per-case metrics and telemetry
- Distributed case execution across nodes
```

#### Success Criteria:

##### Automated Verification:
- [ ] Documentation file created
- [ ] Code examples compile without errors

##### Manual Verification:
- [ ] Documentation is clear and complete
- [ ] Examples work when copied into Erlang shell
- [ ] Architecture diagram accurately reflects implementation

**Note:** Complete all automated verification, then pause for manual confirmation. This is the final phase.

---

## Testing Strategy

### Unit Tests:
- Supervisor behavior (init, start_link, child specs)
- Case runner lifecycle (init, terminate, handle_call)
- gproc registration/unregistration
- Error handling (invalid case IDs, duplicate starts)
- Case ID generation uniqueness

### Integration Tests:
- Supervisor starts under cre_sup
- Cases can be started and stopped
- Case registry lookup works
- Multiple cases can run simultaneously
- Supervisor shuts down cleanly
- Existing services unaffected

### Manual Testing Steps:
1. Start CRE application: `application:start(cre).`
2. Verify wf_case_sup is running: `erlang:whereis(wf_case_sup).`
3. Check supervisor tree: `supervisor:which_children(cre_sup).`
4. List cases (should be empty): `wf_case_sup:list_cases().`
5. Start a test case (requires actual workflow module)
6. Verify case appears in list: `wf_case_sup:list_cases().`
7. Get case info: `wf_case_sup:get_case_status(CaseId).`
8. Stop case: `wf_case_sup:stop_case(CaseId).`
9. Verify case removed from list
10. Stop CRE: `application:stop(cre).`

## Migration Notes

**No migration required** - This is additive functionality. Existing code using `yawl_workflow_supervisor` or `yawl_supervisor` continues to work unchanged.

**Adoption path:**
1. New code can use `wf_case_sup` directly
2. Existing code can migrate incrementally by changing supervisor calls
3. Both old and new supervisors can coexist indefinitely

## References

- Research: `/Users/sac/cre/.wreckit/items/020-otp-supervision-tree-integration/research.md`
- Supervisor pattern: `/Users/sac/cre/src/app/yawl_workflow_supervisor.erl:84-98`
- Registry pattern: `/Users/sac/cre/src/yawl/yawl_supervisor.erl:442-456`
- Integration point: `/Users/sac/cre/src/app/cre_sup.erl:259-266`
- Case runner pattern: `/Users/sac/cre/src/active/active_token_sup.erl:51-65`
- wf_engine architecture: `/Users/sac/cre/src/wf/wf_engine.erl:149-177`
