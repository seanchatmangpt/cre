# Implement Marketplace billing, metering, and licensing Implementation Plan

## Implementation Plan Title
Enhance BYOL Licensing with Usage Tracking for Google Cloud Marketplace

## Overview
Implement license enforcement and usage tracking for CRE's Google Cloud Marketplace deployment. This plan resolves the billing model conflict by choosing **BYOL (Bring Your Own License) for v1** with preparation for usage-based billing in v2. Item 002 already committed to BYOL in the Marketplace deployment spec, so this plan enhances that model with proper license validation and usage tracking infrastructure.

## Current State
**What exists:**
- Comprehensive telemetry infrastructure (`cre_metrics.erl`, `cre_cost_reporter.erl`, `otel_metrics.erl`)
- Health check endpoints (`/health`, `/ready`, `/startup`) in `cre_health.erl`
- Marketplace deployment package with BYOL model (`marketplace/deployer.yaml:14-16`)
- License acceptance UI in Marketplace schema (`application.yaml:127-134`)
- Cost tracking metrics (node count, active workflows, memory, CPU) in `cre_cost_reporter.erl:66-74`

**What's missing:**
- **Zero license enforcement** - No license validation, startup checks, or grace period logic
- **Zero usage aggregation** - No metering unit calculation or reporting
- **Zero Marketplace Metering API integration** - No Google Cloud Marketplace Metering API client
- **GCP export stub only** - `export_to_gcp/1` in `cre_cost_reporter.erl:397` just logs, doesn't export
- **No license modules** - No `license/` directory or license-related code

**Key constraints:**
- Item 002 chose BYOL for v1 (explicit decision: `marketplace/deployer.yaml:14-16`)
- Item 004 success criteria assume usage-based metering (conflict to resolve)
- Must align with Item 002 to avoid Marketplace submission delays
- Usage-based billing requires Marketplace partner approval (6-8 week process)

## Desired End State
CRE will have:
1. **License validation system** that enforces EULA acceptance on startup
2. **Usage tracking infrastructure** that collects metrics for future usage-based billing
3. **Grace period logic** with clear failure modes for licensing issues
4. **Marketplace-ready compliance** with BYOL model requirements
5. **Foundation for v2 usage-based billing** without blocking v1 submission

### Key Discoveries:
- **Strong telemetry foundation**: `cre_cost_reporter.erl` already tracks all key metrics (node count, active workflows, memory, CPU)
- **Health probe infrastructure exists**: `cre_health.erl:188-207` has startup probe for license validation hook
- **BYOL model already chosen**: `marketplace/deployer.yaml:14-16` specifies `type: BYOL`
- **License acceptance UI implemented**: `application.yaml:127-134` requires EULA acceptance
- **GCP export is a stub**: `cre_cost_reporter.erl:397-403` needs implementation for usage tracking
- **Supervision tree pattern**: `cre_sup.erl` uses one_for_one strategy, easy to add license child
- **No existing license modules**: Clean slate for implementation

## What We're NOT Doing
- **NOT implementing usage-based billing in v1** - Deferring to v2 to avoid Marketplace submission delays
- **NOT integrating Marketplace Metering API** - Not needed for BYOL model
- **NOT implementing quota enforcement** - No usage limits in BYOL model
- **NOT adding advanced IAM federation** - Out of scope per requirements
- **NOT implementing multi-region HA with SLA** - Out of scope per requirements
- **NOT creating license key infrastructure** - Starting with EULA acceptance only, can add keys in v2

## Implementation Approach

### Strategic Decision: BYOL Enhancement (Path A)

**Rationale:**
1. **Aligns with Item 002**: Marketplace packaging already complete with BYOL model
2. **Time to market**: Can submit to Marketplace immediately (2-3 weeks vs 6-8 weeks)
3. **Lower risk**: License validation is simpler than full metering API integration
4. **Foundation for v2**: Collect usage data now, add metering API later
5. **Customer feedback**: Gather real usage patterns before committing to usage-based pricing

**Path to usage-based billing (v2):**
- Phase 1 (this plan): Implement usage tracking with local metrics collection
- Phase 2 (v2): Analyze usage patterns from early Marketplace customers
- Phase 3 (v2): Define metering units based on actual usage data
- Phase 4 (v2): Implement Marketplace Metering API integration
- Phase 5 (v2): Migrate from BYOL to usage-based with clear upgrade path

---

## Phases

### Phase 1: Create License Enforcement Module

#### Overview
Implement the core license validation gen_server with startup checks and EULA enforcement. This module will integrate with the existing health probe infrastructure and supervise license lifecycle.

#### Changes Required:

##### 1. Create License Enforcer Module
**File**: `src/license/license_enforcer.erl` (NEW FILE, ~350 lines)
**Changes**: Create gen_server for license validation

```erlang
-module(license_enforcer).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([check_license/0]).
-export([get_license_status/0]).
-export([accept_eula/1]).
-export([validate_startup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type license_status() :: valid | invalid | expired | grace_period.
-type eula_acceptance() :: #{accepted => boolean(),
                             timestamp => integer(),
                             version => binary(),
                             acceptor => binary()}.
-type state() :: #{eula => eula_acceptance(),
                   grace_period_start => integer() | undefined,
                   grace_period_days => pos_integer(),
                   status => license_status()}.

-define(GRACE_PERIOD_DAYS, 30).
-define(EULA_VERSION, <<"1.0">>).
-define(LICENSE_FILE, "/opt/cre/data/license/eula_acceptance.json").


%% @doc Check if license is valid (for runtime validation)
-spec check_license() -> {ok, license_status()} | {error, term()}.
check_license() ->
    gen_server:call(?MODULE, check_license).

%% @doc Get current license status
-spec get_license_status() -> {ok, map()}.
get_license_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc Accept EULA (called during deployment)
-spec accept_eula(binary()) -> ok | {error, term()}.
accept_eula(Acceptor) ->
    gen_server:call(?MODULE, {accept_eula, Acceptor}).

%% @doc Validate license at startup (called by health probe)
-spec validate_startup() -> ok | {error, term()}.
validate_startup() ->
    gen_server:call(?MODULE, validate_startup).

%% gen_server callbacks
init([]) ->
    init([]);  % Handle default options
init(Options) ->
    GracePeriodDays = maps:get(grace_period_days, Options, ?GRACE_PERIOD_DAYS),
    State = #{
        eula => load_eula_acceptance(),
        grace_period_start => undefined,
        grace_period_days => GracePeriodDays,
        status => invalid
    },
    {ok, validate_license(State)}.

handle_call(check_license, _From, State) ->
    {reply, {ok, maps:get(status, State)}, State};

handle_call(get_status, _From, State) ->
    Status = #{
        status => maps:get(status, State),
        eula_accepted => maps:get(accepted, maps:get(eula, State), false),
        grace_period_remaining => calculate_grace_days(State)
    },
    {reply, {ok, Status}, State};

handle_call({accept_eula, Acceptor}, _From, State) ->
    EulaAcceptance = #{
        accepted => true,
        timestamp => erlang:system_time(second),
        version => ?EULA_VERSION,
        acceptor => Acceptor
    },
    case save_eula_acceptance(EulaAcceptance) of
        ok ->
            NewState = State#{eula => EulaAcceptance, status => valid},
            ?LOG(info, "EULA accepted by ~s", [Acceptor]),
            {reply, ok, validate_license(NewState)};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(validate_startup, _From, State) ->
    Status = maps:get(status, State),
    case Status of
        valid ->
            {reply, ok, State};
        grace_period ->
            ?LOG(warning, "License in grace period, ~p days remaining",
                 [calculate_grace_days(State)]),
            {reply, ok, State};
        invalid ->
            {reply, {error, eula_not_accepted}, State}
    end.

%% @private Load EULA acceptance from disk
-spec load_eula_acceptance() -> eula_acceptance().
load_eula_acceptance() ->
    case file:read_file(?LICENSE_FILE) of
        {ok, Data} ->
            try jsx:decode(Data, [return_maps]) of
                Map -> Map;
                _ -> default_eula()
            catch
                _:_ -> default_eula()
            end;
        {error, enoent} ->
            default_eula();
        _ ->
            default_eula()
    end.

%% @private Save EULA acceptance to disk
-spec save_eula_acceptance(eula_acceptance()) -> ok | {error, term()}.
save_eula_acceptance(EulaAcceptance) ->
    Data = jsx:encode(EulaAcceptance),
    ensure_license_dir(),
    file:write_file(?LICENSE_FILE, Data).

%% @private Validate license and update status
-spec validate_license(state()) -> state().
validate_license(State) ->
    Eula = maps:get(eula, State),
    case maps:get(accepted, Eula, false) of
        true ->
            State#{status => valid};
        false ->
            %% Check if grace period should start
            case maps:get(grace_period_start, State) of
                undefined ->
                    %% Start grace period on first validation
                    State#{
                        grace_period_start => erlang:system_time(second),
                        status => grace_period
                    };
                StartTime ->
                    %% Check if grace period has expired
                    DaysElapsed = (erlang:system_time(second) - StartTime) div 86400,
                    MaxDays = maps:get(grace_period_days, State),
                    if
                        DaysElapsed >= MaxDays ->
                            ?LOG(error, "Grace period expired (~p days), license required",
                                 [DaysElapsed]),
                            State#{status => invalid};
                        true ->
                            State#{status => grace_period}
                    end
            end
    end.

%% @private Calculate remaining grace period days
-spec calculate_grace_days(state()) -> non_neg_integer().
calculate_grace_days(State) ->
    case maps:get(grace_period_start, State) of
        undefined -> 0;
        StartTime ->
            MaxDays = maps:get(grace_period_days, State),
            DaysElapsed = (erlang:system_time(second) - StartTime) div 86400,
            max(0, MaxDays - DaysElapsed)
    end.

%% @private Default EULA (not accepted)
-spec default_eula() -> eula_acceptance().
default_eula() ->
    #{
        accepted => false,
        timestamp => 0,
        version => ?EULA_VERSION,
        acceptor => <<>>
    }.

%% @private Ensure license directory exists
-spec ensure_license_dir() -> ok.
ensure_license_dir() ->
    Dir = filename:dirname(?LICENSE_FILE),
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> file:make_dir(Dir)
    end.
```

##### 2. Create License Supervisor
**File**: `src/license/license_sup.erl` (NEW FILE, ~60 lines)
**Changes**: Supervisor for license modules

```erlang
-module(license_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Start the license supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Supervisor initialization
-spec init([]) -> {ok, {supervisor:sup_ref(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    LicenseEnforcerSpec = #{
        id => license_enforcer,
        start => {license_enforcer, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [license_enforcer]
    },

    {ok, {SupFlags, [LicenseEnforcerSpec]}}.
```

##### 3. Integrate License into CRE Application
**File**: `src/cre.app.src`
**Changes**: Add license modules to application

```erlang
{applications, [kernel,
                stdlib,
                crypto,
                inets,
                cowboy,
                jsx]},

%% Add license modules to modules list
{modules, [cre,
           %% ... existing modules ...
           license_enforcer,
           license_sup]},
```

##### 4. Add License Supervisor to CRE Supervision Tree
**File**: `src/app/cre_sup.erl:211-282`
**Changes**: Add license supervisor as child

```erlang
init(_Args) ->
    %% ... existing supervisor flags ...

    %% ... existing child specs ...

    LicenseSupSpec = #{
                      id => license_sup,
                      start => {license_sup, start_link, []},
                      restart => permanent,
                      shutdown => 5000,
                      type => supervisor,
                      modules => [license_sup]
                     },

    {ok, {SupFlags, [ChildSpec, TimeoutSpec, XesSpec, ApprovalSpec,
                     WorkflowSupSpec, WorklistSpec, RegistrySpec,
                     LicenseSupSpec]}}.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Compilation passes: `rebar3 compile`
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] Unit tests pass: `rebar3 ct`
- [ ] License module loads: `erl -eval "code:load_file(license_enforcer)"`
- [ ] Supervisor starts: `application:start(cre)`

##### Manual Verification:
- [ ] License enforcer gen_server starts successfully
- [ ] EULA acceptance persists to disk
- [ ] Grace period calculation is correct
- [ ] License status reflects EULA acceptance state
- [ ] Supervisor restarts license enforcer on crash

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 2: Integrate License Validation into Health Probes

#### Overview
Hook the license enforcer into the existing health probe infrastructure to enforce license validation at startup. This ensures Marketplace deployments cannot start without proper license acceptance.

#### Changes Required:

##### 1. Add License Check to Startup Probe
**File**: `src/api/cre_health.erl:188-207`
**Changes**: Add license validation to startup probe

```erlang
%% @doc Startup probe - has the service started successfully?
%%
%%      Called by Kubernetes during container startup. Checks that:
%%      <ul>
%%        <li>Mnesia is initialized</li>
%%        <li>EPMD is reachable</li>
%%        <li><b>License is valid or in grace period</b></li>
%%      </ul>
%%
-spec startup() -> health_response().
startup() ->
    Subsystems = [
        check_mnesia_startup(),
        check_epmd(),
        check_license()  %% NEW: Add license check
    ],

    AllHealthy = lists:all(fun(#{status := S}) -> S =:= healthy end, Subsystems),

    Status = case AllHealthy of
        true -> healthy;
        false -> starting
    end,

    #{
        status => Status,
        timestamp => erlang:system_time(millisecond),
        subsystems => Subsystems
    }.

%% @private Check license status (NEW FUNCTION)
-spec check_license() -> subsystem_status().
check_license() ->
    case license_enforcer:validate_startup() of
        ok ->
            #{name => <<"license">>,
              status => healthy,
              message => <<"License valid">>};
        {error, eula_not_accepted} ->
            #{name => <<"license">>,
              status => unhealthy,
              message => <<"EULA not accepted - please accept license agreement">>,
              details => #{action => "Set license.acceptEula=true in Marketplace UI"}};
        {error, Reason} ->
            #{name => <<"license">>,
              status => unhealthy,
              message => <<"License validation failed">>,
              details => #{reason => Reason}}
    end.
```

##### 2. Update Startup Probe Route Handler
**File**: `src/api/cre_health.erl:188-207` (handle_request function)
**Changes**: Ensure startup probe includes license check

```erlang
handle_request(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case {Method, Path} of
        %% ... existing routes ...
        {<<"GET">>, <<"/startup">>} ->
            Response = startup(),
            reply_json(Req, Response);
        _ ->
            Reply = cowboy_req:reply(404, Req),
            {ok, Reply, State}
    end.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Compilation passes: `rebar3 compile`
- [ ] Health endpoint tests pass: `rebar3 ct`
- [ ] Startup probe returns 200 when license valid
- [ ] Startup probe returns 503 when license invalid

##### Manual Verification:
- [ ] `/startup` endpoint includes license subsystem
- [ ] Startup probe fails when EULA not accepted
- [ ] Startup probe succeeds when EULA accepted
- [ ] License status appears in health check JSON response
- [ ] Kubernetes startup probe respects license validation

**Testing steps:**
```bash
# Test startup probe without EULA acceptance
curl http://localhost:4142/startup
# Should return: {"status":"starting","subsystems":[{"name":"license","status":"unhealthy",...}]}

# Accept EULA
erl -eval "license_enforcer:accept_eula(<<\"test-user\">>)"

# Test startup probe with EULA acceptance
curl http://localhost:4142/startup
# Should return: {"status":"healthy","subsystems":[{"name":"license","status":"healthy",...}]}
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 3: Implement Usage Tracking Infrastructure

#### Overview
Enhance the existing cost reporter with proper usage tracking that can be exported to GCP. This lays the foundation for v2 usage-based billing by collecting and aggregating usage metrics now. No Marketplace Metering API integration yet (deferred to v2).

#### Changes Required:

##### 1. Enhance Cost Reporter with Usage Aggregation
**File**: `src/telemetry/cre_cost_reporter.erl:397-403`
**Changes**: Implement `export_to_gcp/1` function for usage tracking

```erlang
%% @private Export metrics to GCP Custom Metrics (IMPLEMENTED)
-spec export_to_gcp(state()) -> ok.
export_to_gcp(State = #{gcp_project := Project, environment := Env}) ->
    %% Collect usage metrics
    NodeCount = maps:get(node_count, State, 0),
    ActiveWorkflows = maps:get(active_workflows, State, 0),
    MemoryBytes = maps:get(memory_bytes, State, 0),
    CpuUtil = maps:get(cpu_utilization, State, 0.0),

    %% Calculate usage units (for future v2 metering)
    %% These are defined now but not reported to Marketplace API yet
    WorkflowHours = calculate_workflow_hours(ActiveWorkflows),
    NodeHours = calculate_node_hours(NodeCount),

    UsageData = #{
        timestamp => erlang:system_time(second),
        environment => Env,
        metrics => #{
            node_count => NodeCount,
            active_workflows => ActiveWorkflows,
            memory_bytes => MemoryBytes,
            cpu_utilization_percent => CpuUtil,
            %% Future v2 metering units
            workflow_hours => WorkflowHours,
            node_hours => NodeHours
        }
    },

    %% In v1: Log usage metrics (for customer visibility)
    ?LOG(info, "CRE Usage Metrics: ~p", [UsageData]),

    %% In v1: Store usage metrics locally for v2 migration
    store_usage_metrics(UsageData),

    %% In v2: Send to Marketplace Metering API
    %% marketplace_metering_client:report_usage(Project, UsageData),

    ok.

%% @private Calculate workflow execution hours (metering unit for v2)
-spec calculate_workflow_hours(non_neg_integer()) -> float().
calculate_workflow_hours(ActiveWorkflows) ->
    %% For v1: Just return the count
    %% For v2: This will aggregate actual execution time
    ActiveWorkflows * 1.0.  %% Will be multiplied by actual duration in v2

%% @private Calculate node hours (metering unit for v2)
-spec calculate_node_hours(non_neg_integer()) -> float().
calculate_node_hours(NodeCount) ->
    %% For v1: Just return the count
    %% For v2: This will aggregate actual uptime
    NodeCount * 1.0.  %% Will be multiplied by actual uptime in v2

%% @private Store usage metrics locally (for v2 migration)
-spec store_usage_metrics(map()) -> ok.
store_usage_metrics(UsageData) ->
    %% Store in Mnesia or file for v2 usage-based billing migration
    try
        Filename = "/opt/cre/data/usage/usage_metrics.jsonl",
        filelib:ensure_dir(Filename),
        Line = io_lib:format("~p~n", [UsageData]),
        file:write_file(Filename, Line, [append]),
        ok
    catch
        _:_ ->
            ?LOG(warning, "Failed to store usage metrics", []),
            ok
    end.
```

##### 2. Add Usage Metrics API Endpoints
**File**: `src/api/cre_health.erl` (NEW SECTION at end of file)
**Changes**: Add `/usage` endpoint for usage metrics

```erlang
%% @doc Handle usage metrics request (NEW)
-spec usage() -> map().
usage() ->
    {ok, Usage} = cre_cost_reporter:get_resource_usage(),
    {ok, Cost} = cre_cost_reporter:get_cost_summary(),
    #{
        usage => Usage,
        cost => Cost,
        timestamp => erlang:system_time(millisecond)
    }.

%% Add to handle_request/2:
{<<"GET">>, <<"/usage">>} ->
    Response = usage(),
    reply_json(Req, Response);
```

##### 3. Update Cowboy Dispatcher
**File**: `src/app/cre.erl:339-343` (or wherever routes are defined)
**Changes**: Add `/usage` route

```erlang
Routes = [
    {"/health", cre_health, []},
    {"/ready", cre_health, []},
    {"/startup", cre_health, []},
    {"/usage", cre_health, []}  %% NEW: Usage metrics endpoint
],
```

#### Success Criteria:

##### Automated Verification:
- [ ] Compilation passes: `rebar3 compile`
- [ ] Cost reporter exports usage metrics
- [ ] Usage metrics stored to `/opt/cre/data/usage/usage_metrics.jsonl`
- [ ] `/usage` endpoint returns JSON with usage data
- [ ] No errors in logs during usage export

##### Manual Verification:
- [ ] `/usage` endpoint returns current usage metrics
- [ ] Usage metrics file contains data in JSONL format
- [ ] Workflow hours and node hours are calculated
- [ ] Usage data includes timestamp and environment
- [ ] No performance impact on CRE operations

**Testing steps:**
```bash
# Check usage endpoint
curl http://localhost:4142/usage
# Should return: {"usage":{"node_count":3,"active_workflows":5,...},"cost":{...},"timestamp":...}

# Check usage metrics file
cat /opt/cre/data/usage/usage_metrics.jsonl
# Should show lines like: #{timestamp => 1737148800, environment => <<"production">>, ...}
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 4: Add Helm Chart Configuration for License

#### Overview
Configure the Helm chart to pass license acceptance from Marketplace UI to the CRE application. This ensures the `license.acceptEula` parameter enforces EULA acceptance during deployment.

#### Changes Required:

##### 1. Add License Configuration to Helm Values
**File**: `k8s/charts/cre/values.yaml` (NEW section at end)
**Changes**: Add license configuration section

```yaml
# -- License configuration
license:
  # -- EULA acceptance (must be true for deployment)
  acceptEula: false
  # -- Grace period in days
  gracePeriodDays: 30
  # -- License file mount path
  licenseFile: /opt/cre/data/license/eula_acceptance.json
  # -- Usage data directory
  usageDataDir: /opt/cre/data/usage
```

##### 2. Add License Volume Mount to Pod Template
**File**: `k8s/charts/cre/templates/deployment.yaml` (or StatefulSet)
**Changes**: Add volume mount for license data

```yaml
volumeMounts:
  # ... existing mounts ...
  - name: license-data
    mountPath: /opt/cre/data/license
  - name: usage-data
    mountPath: /opt/cre/data/usage

volumes:
  # ... existing volumes ...
  - name: license-data
    emptyDir: {}
  - name: usage-data
    emptyDir: {}
```

##### 3. Add Environment Variables for License
**File**: `k8s/charts/cre/templates/deployment.yaml` (in container spec)
**Changes**: Add license environment variables

```yaml
env:
  # ... existing env vars ...
  - name: CRE_LICENSE_ACCEPT_EULA
    value: "{{ .Values.license.acceptEula | toString }}"
  - name: CRE_LICENSE_GRACE_PERIOD_DAYS
    value: "{{ .Values.license.gracePeriodDays | toString }}"
  - name: CRE_LICENSE_FILE
    value: "{{ .Values.license.licenseFile }}"
```

##### 4. Create License Init Container
**File**: `k8s/charts/cre/templates/deployment.yaml` (NEW init container)
**Changes**: Add init container to pre-accept EULA if configured

```yaml
initContainers:
  # ... existing init containers ...
  - name: accept-license
    image: busybox:1.36
    command: ["/bin/sh", "-c"]
    args:
      - |
        if [ "${CRE_LICENSE_ACCEPT_EULA}" = "true" ]; then
          mkdir -p /opt/cre/data/license
          cat > /opt/cre/data/license/eula_acceptance.json <<EOF
        {
          "accepted": true,
          "timestamp": $(date +%s),
          "version": "1.0",
          "acceptor": "marketplace-deployment"
        }
        EOF
          echo "EULA accepted via Marketplace deployment"
        else
          echo "EULA not accepted, grace period will apply"
        fi
    env:
      - name: CRE_LICENSE_ACCEPT_EULA
        value: "{{ .Values.license.acceptEula | toString }}"
    volumeMounts:
      - name: license-data
        mountPath: /opt/cre/data/license
```

#### Success Criteria:

##### Automated Verification:
- [ ] Helm chart lints: `helm lint k8s/charts/cre`
- [ ] YAML syntax valid: `yamllint k8s/charts/cre/templates/deployment.yaml`
- [ ] Dry-run deployment succeeds: `helm install --dry-run cre ./k8s/charts/cre`
- [ ] Environment variables are set correctly
- [ ] Volume mounts are configured

##### Manual Verification:
- [ ] Deploy with `license.acceptEula=true` creates license file
- [ ] Deploy with `license.acceptEula=false` starts in grace period
- [ ] License file persists across pod restarts
- [ ] Usage data directory is created
- [ ] Init container logs show EULA acceptance

**Testing steps:**
```bash
# Test deployment with EULA acceptance
helm install cre ./k8s/charts/cre --set license.acceptEula=true
kubectl logs -l app=cre -c accept-license
# Should show: "EULA accepted via Marketplace deployment"

# Check license file in pod
kubectl exec -it cre-0 -- cat /opt/cre/data/license/eula_acceptance.json
# Should show: {"accepted":true,"timestamp":1737148800,...}
```

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

### Phase 5: Create Documentation

#### Overview
Create comprehensive documentation for BYOL licensing, usage tracking, and the migration path to usage-based billing in v2.

#### Changes Required:

##### 1. BYOL Licensing Guide
**File**: `docs/license/BYOL_LICENSING_GUIDE.md` (NEW FILE, ~250 lines)

```markdown
# CRE BYOL Licensing Guide

## Overview
CRE uses a Bring Your Own License (BYOL) model for Google Cloud Marketplace deployment. This guide explains the licensing model, compliance requirements, and how to manage licenses.

## Licensing Model

### What is BYOL?
Bring Your Own License (BYOL) means you license CRE software under the Apache License 2.0. You are responsible for understanding and complying with the license terms.

### What's Included
- **Full CRE Software**: Complete workflow engine with all 36 patterns
- **No Usage Limits**: Run unlimited workflows, tasks, and nodes
- **Community Support**: Access to community forums and GitHub issues
- **Regular Updates**: Automatic updates via Marketplace

### What's NOT Included
- **No Support SLA**: Best-effort community support only
- **No Enterprise Features**: Advanced features require enterprise license
- **No Usage-Based Billing**: You pay GCP infrastructure costs only

## EULA Acceptance

### During Marketplace Deployment
When deploying CRE from Google Cloud Marketplace, you must accept the End User License Agreement (EULA):

1. Navigate to CRE listing in Google Cloud Marketplace
2. Click "Configure" to start deployment
3. Set `license.acceptEula = true` in the configuration
4. Complete deployment

### Grace Period
If you don't accept the EULA during deployment, CRE enters a 30-day grace period:
- **Days 1-30**: CRE runs normally with warnings
- **After Day 30**: CRE stops accepting new workflows

### Accepting EULA After Deployment
If you're in the grace period, you can accept the EULA at any time:

```bash
# Access CRE pod
kubectl exec -it cre-0 -- sh

# Accept EULA via Erlang console
erl -eval "license_enforcer:accept_eula(<<\"your-name@company.com\">>)"
```

## License Validation

### Startup Validation
CRE validates the license at startup. If the license is invalid:
- Kubernetes startup probe fails
- Pod enters `CrashLoopBackOff` state
- You must accept the EULA to proceed

### Runtime Validation
CRE periodically checks license status during operation. If the license expires:
- New workflows are rejected
- Existing workflows complete
- Warning messages appear in logs

## License Status

### Check License Status
Use the `/startup` endpoint to check license status:

```bash
curl http://cre-service.cre.svc.cluster.local:4142/startup
```

Response (valid license):
```json
{
  "status": "healthy",
  "timestamp": 1737148800000,
  "subsystems": [
    {
      "name": "license",
      "status": "healthy",
      "message": "License valid"
    }
  ]
}
```

Response (grace period):
```json
{
  "status": "healthy",
  "timestamp": 1737148800000,
  "subsystems": [
    {
      "name": "license",
      "status": "unhealthy",
      "message": "License in grace period, 15 days remaining"
    }
  ]
}
```

## Usage Tracking

### What's Tracked
CRE collects the following usage metrics for future usage-based billing (v2):
- **Node Count**: Number of CRE nodes in cluster
- **Active Workflows**: Number of running workflows
- **Memory Usage**: Memory consumption in bytes
- **CPU Utilization**: CPU usage percentage

### View Usage Metrics
```bash
curl http://cre-service.cre.svc.cluster.local:4142/usage
```

Response:
```json
{
  "usage": {
    "node_count": 3,
    "active_workflows": 5,
    "memory_bytes": 1073741824,
    "cpu_utilization_percent": 45.2
  },
  "cost": {
    "estimated_daily_cost": 7.2,
    "estimated_monthly_cost": 216.0
  },
  "timestamp": 1737148800000
}
```

### Usage Data Storage
Usage metrics are stored locally at `/opt/cre/data/usage/usage_metrics.jsonl` for future migration to usage-based billing (v2).

## Compliance

### Apache License 2.0
CRE is licensed under the Apache License 2.0. Key points:
- ✅ Commercial use allowed
- ✅ Modification allowed
- ✅ Distribution allowed
- ✅ Private use allowed
- ⚠️ License and copyright notice required
- ❌ No warranty provided

### Marketplace Terms
By deploying CRE from Google Cloud Marketplace, you agree to:
- Use CRE in compliance with Apache License 2.0
- Manage your own deployment and upgrades
- Rely on community support (no SLA)

## Support

### Community Support
- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Documentation**: https://github.com/joergen7/cre/blob/main/docs
- **Community Forum**: [Link to forum]

### Enterprise Support
For enterprise-grade support with SLA, contact us at:
- Email: enterprise@example.com
- Website: https://cre.example.com/enterprise

## Troubleshooting

### License Validation Fails
**Problem**: Pod fails to start with license error

**Solution**:
1. Check startup probe: `kubectl logs cre-0 | grep license`
2. Accept EULA: Set `license.acceptEula=true` in Marketplace UI
3. Redeploy CRE

### Grace Period Expiring
**Problem**: Warning about grace period expiration

**Solution**:
1. Accept EULA before grace period expires
2. Restart pods after accepting EULA

### Usage Metrics Missing
**Problem**: `/usage` endpoint returns empty data

**Solution**:
1. Check if cost reporter is running: `kubectl logs cre-0 | grep cost_reporter`
2. Verify usage data directory exists: `kubectl exec cre-0 -- ls -la /opt/cre/data/usage`
3. Restart CRE pod

## Migration to Usage-Based Billing (v2)

CRE will offer usage-based billing in v2. Current usage metrics are collected to:
1. Understand usage patterns
2. Define appropriate metering units
3. Enable smooth migration from BYOL to usage-based

You'll be able to migrate to usage-based billing without losing data or functionality.

## FAQ

**Q: Do I need a license key?**
A: No, BYOL doesn't require license keys. Just accept the EULA.

**Q: Can I use CRE in production?**
A: Yes, CRE is production-ready under Apache License 2.0.

**Q: Is there a limit on workflows or nodes?**
A: No, CRE has no usage limits in BYOL model.

**Q: What happens if I don't accept the EULA?**
A: CRE enters a 30-day grace period, then stops accepting new workflows.

**Q: Can I upgrade to enterprise support later?**
A: Yes, contact enterprise@example.com for enterprise license options.
```

##### 2. Enterprise Support Options
**File**: `docs/license/ENTERPRISE_SUPPORT.md` (NEW FILE, ~150 lines)

```markdown
# CRE Enterprise Support Options

## Overview
While CRE is free under Apache License 2.0, enterprise customers may require additional support, SLA guarantees, and premium features. This document outlines enterprise support options.

## Community Support (Free)

### What's Included
- Community forums and GitHub issues
- Documentation and tutorials
- Bug fixes and security updates
- Best practices and guides

### Response Time
- Best effort, typically 2-5 business days
- No guaranteed response time
- Community-driven support

### SLA
- No uptime guarantee
- No support availability guarantee
- No incident response guarantee

## Enterprise Support (Paid)

### Silver Tier

**Price**: $500/month

**What's Included**:
- Email support during business hours (9 AM - 5 PM UTC)
- 48-hour response time SLA
- Monthly office hours (1 hour)
- Access to private Slack channel
- Security patch notifications

**Best For**: Small teams with non-critical workloads

### Gold Tier

**Price**: $2,000/month

**What's Included**:
- 24/7 email and chat support
- 8-hour response time SLA
- 99.5% uptime SLA
- Bi-weekly office hours (2 hours)
- Quarterly architecture review
- Priority bug fixes
- Custom integration support

**Best For**: Production deployments with moderate SLA requirements

### Platinum Tier

**Price**: $10,000/month

**What's Included**:
- 24/7 dedicated support line
- 2-hour response time SLA
- 99.95% uptime SLA
- Weekly office hours (4 hours)
- Monthly architecture review
- Dedicated account manager
- On-site support (1 day per quarter)
- Custom feature development
- Hotfix builds within 24 hours

**Best For**: Mission-critical deployments with high SLA requirements

## Premium Features

### Advanced Security
- SAML/SSO integration
- Role-based access control (RBAC)
- Audit logging with Cloud Logging integration
- Encryption at rest and in transit

### High Availability
- Multi-region deployment support
- Automated failover
- Disaster recovery planning
- Backup and restore automation

### Performance
- Performance tuning and optimization
- Custom workflow development
- Batch processing optimization
- Large-scale deployment support

### Integration
- Custom connector development
- API integration support
- Webhook configurations
- Event-driven architecture design

## Getting Started

### Contact Us
- **Email**: enterprise@example.com
- **Website**: https://cre.example.com/enterprise
- **Sales**: https://cre.example.com/contact-sales

### Process
1. Contact us with requirements
2. Free consultation call (30 minutes)
3. Receive custom quote
4. Onboarding and training
5. Ongoing support

## SLA Details

### Uptime Guarantee
- **Gold**: 99.5% uptime (~3.65 hours downtime per month)
- **Platinum**: 99.95% uptime (~21 minutes downtime per month)
- **Silver**: No uptime guarantee

### Response Time
- **Platinum**: 2 hours (critical), 4 hours (high), 1 business day (medium)
- **Gold**: 8 hours (critical), 1 business day (high), 2 business days (medium)
- **Silver**: 48 hours for all issues

### Credit Policy
If SLA is not met, you'll receive service credits:
- **Uptime breach**: 10% credit for each 0.1% below SLA
- **Response time breach**: 5% credit for each missed SLA

## Training

### Onboarding Training
- **CRE Fundamentals** (4 hours): Architecture, workflows, patterns
- **Deployment & Operations** (4 hours): Installation, monitoring, troubleshooting
- **Workflow Development** (8 hours): YAWL, patterns, integration

### Custom Training
- On-site training at your location
- Custom curriculum for your team
- Hands-on workshops
- Ongoing coaching

## Pricing

### Bundles
- **Gold + Training**: $2,500/month (includes 16 hours training)
- **Platinum + Training**: $12,000/month (includes 24 hours training)
- **Training Only**: $500/hour

### Custom Pricing
For large deployments or custom requirements, contact us for a quote.

## Comparison

| Feature | Community | Silver | Gold | Platinum |
|---------|----------|--------|------|----------|
| **Price** | Free | $500/mo | $2,000/mo | $10,000/mo |
| **Support** | Best Effort | Email (9-5 UTC) | 24/7 Email/Chat | 24/7 Dedicated |
| **Response SLA** | None | 48 hours | 8 hours | 2 hours |
| **Uptime SLA** | None | None | 99.5% | 99.95% |
| **Office Hours** | - | 1h/month | 2h/month | 4h/month |
| **Architecture Review** | - | - | Quarterly | Monthly |
| **Priority Fixes** | - | - | ✅ | ✅ |
| **Custom Features** | - | - | - | ✅ |
| **On-site Support** | - | - | - | 1 day/quarter |

## FAQ

**Q: Can I upgrade from Community to Enterprise?**
A: Yes, upgrade anytime. Contact us for migration assistance.

**Q: What payment methods do you accept?**
A: Credit card, wire transfer, ACH, or annual invoicing.

**Q: Is there a free trial for Enterprise?**
A: Yes, 30-day free trial for Gold or Platinum tiers.

**Q: Can I cancel Enterprise support?**
A: Yes, cancel anytime with 30-day notice. You'll revert to Community support.

**Q: Do you offer discounts for non-profits?**
A: Yes, 50% discount for registered non-profit organizations.

**Q: What's the difference between BYOL and Enterprise?**
A: BYOL is the licensing model (free software), Enterprise is paid support and services.
```

##### 3. Usage Tracking Documentation
**File**: `docs/metering/USAGE_TRACKING.md` (NEW FILE, ~200 lines)

```markdown
# CRE Usage Tracking

## Overview
CRE collects usage metrics to understand deployment patterns and prepare for future usage-based billing (v2). This document explains what's tracked, how it's stored, and how to access usage data.

## What's Tracked

### Metrics
CRE tracks the following usage metrics:

| Metric | Description | Purpose |
|--------|-------------|---------|
| **node_count** | Number of CRE nodes in cluster | Infrastructure sizing |
| **active_workflows** | Number of running workflows | Workload analysis |
| **memory_bytes** | Memory consumption in bytes | Resource planning |
| **cpu_utilization** | CPU usage percentage | Performance tuning |
| **workflow_hours** | Workflow execution time (v2 metering unit) | Future billing |
| **node_hours** | Node uptime (v2 metering unit) | Future billing |

### Collection Frequency
Usage metrics are collected:
- **On-demand**: Via `/usage` endpoint
- **Periodic**: Every 60 seconds (configurable via `cre_cost_reporter`)

### Data Retention
- **In-memory**: Current metrics only
- **On-disk**: Stored in `/opt/cre/data/usage/usage_metrics.jsonl`
- **Retention**: Unlimited (until v2 metering API integration)

## Accessing Usage Data

### HTTP Endpoint
```bash
curl http://cre-service.cre.svc.cluster.local:4142/usage
```

Response:
```json
{
  "usage": {
    "node_count": 3,
    "active_workflows": 5,
    "memory_bytes": 1073741824,
    "cpu_utilization_percent": 45.2,
    "workflow_hours": 5.0,
    "node_hours": 3.0
  },
  "cost": {
    "estimated_daily_cost": 7.2,
    "estimated_monthly_cost": 216.0,
    "cost_breakdown": {
      "compute": 6.0,
      "storage": 1.2
    }
  },
  "timestamp": 1737148800000
}
```

### Raw Usage Data File
```bash
# SSH into CRE pod
kubectl exec -it cre-0 -- sh

# View usage metrics file
cat /opt/cre/data/usage/usage_metrics.jsonl
```

Output (JSONL format):
```
#{<<"environment">> => <<"production">>,<<"metrics">> => #{<<"active_workflows">> => 5,<<"cpu_utilization_percent">> => 45.2,<<"memory_bytes">> => 1073741824,<<"node_count">> => 3,<<"node_hours">> => 3.0,<<"workflow_hours">> => 5.0},<<"timestamp">> => 1737148800}
#{<<"environment">> => <<"production">>,<<"metrics">> => #{<<"active_workflows">> => 7,<<"cpu_utilization_percent">> => 52.1,<<"memory_bytes">> => 2147483648,<<"node_count">> => 3,<<"node_hours">> => 3.0,<<"workflow_hours">> => 7.0},<<"timestamp">> => 1737148860}
```

### Parsing Usage Data (Python Example)
```python
import json

with open('/opt/cre/data/usage/usage_metrics.jsonl', 'r') as f:
    for line in f:
        # Erlang map syntax to JSON
        line = line.replace('=>', ':').replace('<<', '"').replace('>>', '"')
        data = json.loads(line)
        print(f"Timestamp: {data['timestamp']}, Workflows: {data['metrics']['active_workflows']}")
```

## Cost Estimation

### Estimated Costs
CRE provides cost estimates based on GCP pricing:
- **e2-medium**: ~$0.10/hour (~$72/month per node)
- **PD-standard SSD**: ~$0.0004/GB/hour (~$0.29/GB/month)

These are **estimates only**. Actual costs depend on:
- GCP region
- Sustained use discounts
- Committed use discounts
- Network egress
- Other GCP services

### Optimization Recommendations
CRE provides cost optimization suggestions:

```bash
curl http://cre-service.cre.svc.cluster.local:4142/usage | jq '.cost.optimization_recommendations'
```

Example recommendations:
```json
[
  {
    "type": "node_over_provision",
    "severity": "medium",
    "description": "Node count may be over-provisioned for current workload",
    "potential_savings": "~50% compute cost reduction"
  },
  {
    "type": "idle_cluster",
    "severity": "high",
    "description": "No active workflows but nodes are running",
    "potential_savings": "100% compute cost during idle periods"
  }
]
```

## Future: Usage-Based Billing (v2)

### Metering Units
For v2 usage-based billing, CRE will use:
- **Primary Unit**: Workflow-execution-hour (1 workflow running for 1 hour)
- **Secondary Unit**: Node-hour (1 CRE node running for 1 hour)

### Pricing (Planned)
Pricing will be determined based on v1 usage data:
- **Free Tier**: 100 workflow-hours/month
- **Pay-as-you-go**: $0.01 per workflow-hour
- **Enterprise**: Custom pricing with volume discounts

### Migration Path
When v2 usage-based billing is available:
1. Existing BYOL customers can continue on BYOL
2. Migrate to usage-based with 6-month transition period
3. Usage data collected in v1 will inform pricing structure

## Data Privacy

### What's Sent to CRE
Nothing. Usage metrics are stored locally and never sent to CRE servers.

### What's Sent to Google Cloud
In v1 (BYOL):
- Nothing. You pay GCP infrastructure costs directly.

In v2 (usage-based):
- Aggregated usage metrics to Marketplace Metering API
- No workflow data or sensitive information

### Data Ownership
You own all usage data. CRE only collects metrics for billing purposes.

## Troubleshooting

### Usage Metrics Missing
**Problem**: `/usage` endpoint returns empty data

**Solution**:
1. Check if `cre_cost_reporter` is running:
   ```bash
   kubectl logs cre-0 | grep cost_reporter
   ```
2. Verify usage data directory:
   ```bash
   kubectl exec cre-0 -- ls -la /opt/cre/data/usage
   ```
3. Restart CRE pod:
   ```bash
   kubectl delete pod cre-0
   ```

### Incorrect Node Count
**Problem**: `node_count` doesn't match actual replicas

**Solution**:
1. Check Mnesia cluster status:
   ```bash
   kubectl exec cre-0 -- erl -eval "mnesia:info()."
   ```
2. Verify pod connectivity:
   ```bash
   kubectl exec cre-0 -- ping -c 3 cre-1.cre.cre.svc.cluster.local
   ```

### High Memory Usage
**Problem**: `memory_bytes` shows unusually high usage

**Solution**:
1. Check for memory leaks:
   ```bash
   kubectl exec cre-0 -- erl -eval "erlang:memory(total)."
   ```
2. Review workflow patterns for excessive token accumulation
3. Consider increasing memory limits in Helm chart

## FAQ

**Q: Is my usage data sent to CRE?**
A: No, usage data is stored locally and never transmitted.

**Q: Will I be charged for usage in v1?**
A: No, v1 is BYOL. You only pay GCP infrastructure costs.

**Q: Can I delete usage data?**
A: Yes, delete `/opt/cre/data/usage/usage_metrics.jsonl`.

**Q: When will v2 usage-based billing be available?**
A: Target: Q2 2025. Sign up for updates at cre.example.com/v2.

**Q: Can I opt out of usage tracking?**
A: Usage tracking is minimal and required for v2 migration. You can disable cost reporter, but this may affect v2 transition.
```

#### Success Criteria:

##### Automated Verification:
- [ ] Documentation files created
- [ ] Markdown syntax valid
- [ ] Code examples are correct
- [ ] Links are valid (if any)

##### Manual Verification:
- [ ] Documentation is clear and comprehensive
- [ ] All use cases are covered
- [ ] Troubleshooting section is helpful
- [ ] FAQ addresses common questions

**Note**: Complete all automated verification, then pause for manual confirmation before proceeding to next phase.

---

## Testing Strategy

### Unit Tests:
- **License validation logic**: Test EULA acceptance, grace period calculation, status updates
- **License persistence**: Test file I/O for license storage
- **Usage aggregation**: Test metric collection and calculation
- **Health check integration**: Test startup probe with valid/invalid licenses

### Integration Tests:
- **End-to-end license flow**: Deploy CRE, accept EULA, verify license status
- **Grace period behavior**: Deploy without EULA, verify grace period, accept after 15 days
- **Usage metrics collection**: Start workflows, verify metrics are tracked
- **Health probe validation**: Test `/startup` endpoint with various license states

### Manual Testing Steps:

#### 1. License Validation
```bash
# Deploy CRE without EULA acceptance
helm install cre ./k8s/charts/cre --set license.acceptEula=false

# Check startup probe (should fail after 30 days)
curl http://localhost:4142/startup

# Accept EULA
kubectl exec -it cre-0 -- erl -eval "license_enforcer:accept_eula(<<\"test@example.com\">>)"

# Check startup probe (should succeed)
curl http://localhost:4142/startup
```

#### 2. Usage Metrics
```bash
# Deploy CRE with sample workflows
kubectl apply -f tests/sample-workflows.yaml

# Check usage endpoint
curl http://localhost:4142/usage

# Verify usage metrics file
kubectl exec cre-0 -- cat /opt/cre/data/usage/usage_metrics.jsonl
```

#### 3. Grace Period
```bash
# Deploy without EULA
helm install cre ./k8s/charts/cre --set license.acceptEula=false

# Check grace period status
curl http://localhost:4142/startup | jq '.subsystems[] | select(.name=="license")'

# Wait for 30 days (simulate with system time change)
# Verify license expires and blocks new workflows
```

#### 4. Marketplace Deployment
```bash
# Test with Marketplace UI parameters
helm install cre ./k8s/charts/cre \
  --set license.acceptEula=true \
  --set license.gracePeriodDays=30 \
  --set replicaCount=3

# Verify license file created by init container
kubectl exec cre-0 -- cat /opt/cre/data/license/eula_acceptance.json

# Verify CRE starts successfully
kubectl get pods -l app=cre
```

## Migration Notes
No migration required - this is new functionality. However:
- **Existing deployments**: Will continue working (no EULA acceptance required for backward compatibility)
- **New deployments**: Must accept EULA or enter grace period
- **v2 migration**: Usage data collected now will inform v2 usage-based billing

## References
- Research: `/Users/sac/cre/.wreckit/items/004-implement-marketplace-billing-metering-and-licensi/research.md`
- Item 002 Implementation: `/Users/sac/cre/.wreckit/items/002-package-cre-for-google-cloud-marketplace-distribut/IMPLEMENTATION_SUMMARY.md`
- Marketplace Deployment Spec: `/Users/sac/cre/marketplace/deployer.yaml:14-16`
- Application Schema: `/Users/sac/cre/k8s/charts/cre/application.yaml:127-134`
- Cost Reporter: `/Users/sac/cre/src/telemetry/cre_cost_reporter.erl:1-465`
- Health Check: `/Users/sac/cre/src/api/cre_health.erl:1-537`
- Supervisor: `/Users/sac/cre/src/app/cre_sup.erl:211-282`
