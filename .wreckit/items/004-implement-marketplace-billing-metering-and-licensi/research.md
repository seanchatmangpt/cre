# Research: Implement Marketplace billing, metering, and licensing

**Date**: 2025-01-18
**Item**: 004-implement-marketplace-billing-metering-and-licensi

## Research Question
Marketplace distribution requires billing integration, usage metering, and license enforcement to support commercial deployment models.

**Motivation:** Enables commercial distribution models via Google Marketplace with proper usage tracking and license enforcement.

**Success criteria:**
- Metering integration via Marketplace Metering API
- Usage metrics defined: active workflows, worker-hours, task executions
- License enforcement with startup validation and grace periods
- Clear failure modes for licensing issues

**Technical constraints:**
- Must choose one billing model initially: BYOL, usage-based, or free OSS with paid support
- Metering unit definition must be resolved

**In scope:**
- Metering API integration
- Usage metric definition and reporting
- License enforcement logic
**Out of scope:**
- Advanced IAM federation
- Multi-region HA with SLA

**Signals:** priority: critical, urgency: Open decision required - must resolve billing model before implementation

## Summary

CRE has **extensive existing telemetry infrastructure** but **zero billing/metering/licensing implementation**. The codebase provides excellent foundation for metering with comprehensive metrics collection already in place, but completely lacks:

1. **Marketplace Metering API integration** - No Google Cloud Marketplace Metering API client
2. **Usage aggregation logic** - No metering unit calculation or aggregation
3. **License enforcement** - No license validation, startup checks, or grace period logic
4. **Billing model decision** - BYOL vs usage-based billing not resolved

The **existing telemetry foundation** is strong:
- `cre_metrics.erl` (274 lines) - Comprehensive metrics registry with counters, gauges, histograms
- `cre_cost_reporter.erl` (465 lines) - Cost tracking with node count, active workflows, resource usage
- `otel_metrics.erl` (376 lines) - OpenTelemetry metrics API wrapper
- `yawl_telemetry_prometheus.erl` (237 lines) - Prometheus metrics export
- Health endpoints (`cre_health.erl`) - Liveness, readiness, startup probes already implemented

The **critical blocker** is the **billing model decision**: Item 002 (Marketplace packaging) chose **BYOL** for v1 Marketplace deployment, explicitly deferring usage-based billing to v2. This decision directly impacts the implementation approach for item 004:

- **If BYOL**: No metering integration needed, only license acceptance UI (already implemented in `application.yaml:127-134`)
- **If usage-based**: Requires Marketplace Metering API integration, usage aggregation, reporting logic

**Current state**: Marketplace deployment package (Item 002) is complete with BYOL model. This creates a **decision conflict** - Item 004 assumes usage-based metering is required, but Item 002 already committed to BYOL for v1.

## Current State Analysis

### Existing Implementation

#### Telemetry Infrastructure (STRONG FOUNDATION)

**Metrics Collection System**
- **File**: `src/telemetry/cre_metrics.erl:1-274`
  - Central metrics registry with OpenTelemetry integration
  - Petri net metrics: `pnet_transitions_total`, `pnet_transition_duration_ms`, `pnet_tokens`, `pnet_throughput_fps`
  - Pattern metrics: `pattern_executions_total`, `pattern_execution_duration_ms`, `pattern_errors_total`
  - YAWL metrics: `yawl_cases_total`, `yawl_case_duration_ms`, `yawl_compilations_total`
  - System metrics: `cre_memory_bytes`, `cre_process_count`
  - Helper functions: `transition_fired/2,3`, `case_started/1`, `case_completed/2`, `token_count/1`

**Cost Reporting (BILLING-READY METRICS)**
- **File**: `src/telemetry/cre_cost_reporter.erl:1-465`
  - gen_server for cost tracking and export
  - **Key metrics**:
    - `?COST_NODE_COUNT` - Number of CRE nodes in cluster (line 66)
    - `?COST_ACTIVE_WORKFLOWS` - Number of active YAWL workflows (line 67)
    - `?COST_MEMORY_BYTES` - CRE memory usage in bytes (line 68)
    - `?COST_PROCESS_COUNT` - Number of CRE processes (line 69)
    - `?COST_CPU_UTILIZATION` - CPU utilization percentage (line 70)
    - `?COST_ESTIMATED_HOURLY/DAILY/MONTHLY` - Cost estimates (lines 72-74)
  - **Cost constants** (lines 76-78):
    - `?COST_PER_NODE_HOUR = 0.10` (e2-medium approximate)
    - `?COST_PER_GB_HOUR = 0.0004` (PD-standard approximate)
  - **API functions**:
    - `update_node_count/1` - Update node count metric (line 116)
    - `update_active_workflows/1` - Update active workflows count (line 121)
    - `get_resource_usage/0` - Get current resource usage (line 105)
    - `get_cost_summary/0` - Get cost summary with estimates (line 110)
    - `get_optimization_recommendations/0` - Cost optimization suggestions (line 136)
  - **GCP export stub**: `export_to_gcp/1` (line 397) - Logs export intention, not implemented

**OpenTelemetry Metrics API**
- **File**: `src/telemetry/otel_metrics.erl:1-376`
  - Wrapper for OpenTelemetry metrics operations
  - Counter, gauge, histogram registration and recording
  - Label support for metric dimensions

**Prometheus Exporter**
- **File**: `src/telemetry/prometheus_exporter.erl:1-249`
  - HTTP endpoint on port 9091 for `/metrics` (Prometheus text format)
  - JSON export via `/metrics` with `Accept: application/json`
  - gen_server for managing metrics export endpoint
  - Health check endpoint at `/health`

**YAWL Telemetry**
- **File**: `src/wf/yawl_telemetry_prometheus.erl:1-237`
  - YAWL-specific Prometheus metrics
  - Workflow execution tracking
  - Performance metrics export

#### Health Check Infrastructure

**Health Probes**
- **File**: `src/api/cre_health.erl:1-537`
  - `/health` - Liveness probe (lines 113-165)
  - `/ready` - Readiness probe (lines 167-186)
  - `/startup` - Startup probe (lines 188-207)
  - JSON response format with subsystem status (lines 84-96)
  - Mnesia, EPMD, worker pool checks (lines 327-498)
  - **Already routed** in Cowboy dispatcher (src/app/cre.erl:339-343)

#### Marketplace Deployment (BYOL MODEL CHOSEN)

**GKE Application Schema**
- **File**: `k8s/charts/cre/application.yaml:1-218`
  - **BYOL licensing parameter** (lines 127-134):
    ```yaml
    - name: license.acceptEula
      title: "Accept License Agreement"
      description: "I accept the Apache License 2.0 for CRE software"
      type: boolean
      default: false
      constraints:
        - expression: "params.license.acceptEula == true"
          errorMessage: "You must accept the license agreement to proceed"
    ```
  - 15+ configurable parameters for deployment
  - Output variables for deployment instructions

**Marketplace Deployment Spec**
- **File**: `marketplace/deployer.yaml:1-53`
  - **Billing model specification** (lines 14-16):
    ```yaml
    billing:
      type: BYOL
      license: Apache-2.0
    ```
  - Helm chart reference for deployment
  - Default values for cluster configuration

**Marketplace License Terms**
- **File**: `marketplace/LICENSE.txt:1-27`
  - Apache License 2.0 terms
  - Marketplace deployment terms (lines 18-26):
    - No support SLA included (community support only)
    - User responsible for managing deployment
    - Provided "AS IS" without warranties

### Key Files

#### Telemetry & Metrics (BILLING FOUNDATION)
- `src/telemetry/cre_metrics.erl:1-274` - Central metrics registry, all metric definitions
- `src/telemetry/cre_cost_reporter.erl:1-465` - Cost tracking, node/workflow counts, GCP export stub
- `src/telemetry/otel_metrics.erl:1-376` - OpenTelemetry API wrapper
- `src/telemetry/prometheus_exporter.erl:1-249` - Prometheus HTTP endpoint (/metrics)
- `src/wf/yawl_telemetry_prometheus.erl:1-237` - YAWL-specific metrics

#### Health & Status
- `src/api/cre_health.erl:1-537` - Health check endpoints (/health, /ready, /startup)
- `src/app/cre.erl:339-343` - Cowboy dispatcher with health routes

#### Marketplace Artifacts (BYOL MODEL)
- `k8s/charts/cre/application.yaml:127-134` - License acceptance parameter
- `marketplace/deployer.yaml:14-16` - BYOL billing model specification
- `marketplace/LICENSE.txt:18-26` - Marketplace deployment terms
- `k8s/charts/cre/values.yaml:1-303` - Helm chart configuration

#### Documentation
- `docs/gcp/GCP_MARKETPLACE_READINESS.md:1-386` - Marketplace readiness assessment
- `.wreckit/items/002-package-cre-for-google-cloud-marketplace-distribut/research.md:1-400` - Marketplace packaging research
- `.wreckit/items/002-package-cre-for-google-cloud-marketplace-distribut/IMPLEMENTATION_SUMMARY.md:1-257` - BYOL decision summary

## Technical Considerations

### Dependencies

#### External Dependencies (for Usage-Based Billing)
- **Google Cloud Marketplace Metering API** - NOT INTEGRATED
  - Requires API client for usage reporting
  - Need `google-cloud-marketplace-metering` library or REST client
  - Authentication via Workload Identity or service account
  - Endpoint: `https://marketplace-metering.googleapis.com/v1`

#### Internal Modules to Integrate
- `src/telemetry/cre_cost_reporter.erl` - Already tracks node count, active workflows
- `src/telemetry/cre_metrics.erl` - Metrics registry for usage tracking
- `src/api/cre_health.erl` - Startup probe hook for license validation
- `src/telemetry/cloud_logging_backend.erl` - For logging license events

### Patterns to Follow

#### Existing Telemetry Patterns
- **gen_server pattern**: `cre_cost_reporter` uses gen_server for state management (lines 10-26)
- **Metric registration**: `otel_metrics:register_counter/gauge/histogram` pattern (cre_metrics.erl:98-139)
- **Periodic export**: `timer:send_interval/2` for scheduled reports (cre_cost_reporter.erl:156)
- **Label-based dimensions**: Maps for metric labels (e.g., `#{environment => <<"production">>}`)
- **Async operations**: Non-blocking metrics export (prometheus_exporter.erl)

#### Marketplace Integration Patterns (from Item 002)
- **Workload Identity**: No service account keys (terraform/gcp/modules/security/main.tf:176)
- **GCP authentication**: Application Default Credentials (ADC) pattern
- **Configuration via application.yaml**: Marketplace UI parameters
- **Helm values for runtime config**: No secrets in values.yaml

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Billing model decision conflict** - Item 002 chose BYOL, Item 004 assumes usage-based | HIGH - Implementation blocked on decision | **MUST RESOLVE**: Confirm if Item 004 should implement usage-based metering (v2 feature) or enhance BYOL with license validation |
| **No Marketplace Metering API client** - Zero integration exists | HIGH - Usage-based billing requires API client | Implement Metering API client in Erlang/httpc or use Google Cloud client libraries |
| **Metering unit definition unresolved** - Success criteria specify metrics but not units | MEDIUM - Cannot implement reporting without units | Define metering units: workflow-execution-hour, active-node-hour, task-count |
| **License enforcement not implemented** - No validation or grace period logic | HIGH - Required for commercial licensing model | Implement license validation gen_server with startup hook in cre_health.erl |
| **Cost estimation accuracy** - cre_cost_reporter uses rough estimates ($0.10/node-hour) | MEDIUM - Estimates may not reflect actual GCP costs | Document as estimates only, integrate with GCP Billing API for actual costs (v2) |
| **Grace period implementation** - No pattern in codebase for license grace periods | MEDIUM - Must design grace period state machine | Follow existing gen_server patterns (cre_cost_reporter), add timer-based grace period |
| **Marketplace review requirements** - BYOL requires license terms, usage-based requires metering plan | HIGH - Marketplace submission will be rejected without proper billing setup | Follow Item 002 BYOL approach for v1, defer usage-based to v2 |

## Recommended Approach

### CRITICAL DECISION REQUIRED

**Item 002 (Marketplace Packaging) already chose BYOL for v1 deployment** (2025-01-18):
- `marketplace/deployer.yaml:14-16` specifies `type: BYOL`
- `application.yaml:127-134` implements license acceptance UI
- Implementation summary states: "BYOL model simplifies Marketplace submission (no usage metering needed)"
- Usage-based billing explicitly deferred to v2

**Item 004 success criteria assume usage-based metering**:
- "Metering integration via Marketplace Metering API"
- "Usage metrics defined: active workflows, worker-hours, task executions"

**Two paths forward:**

#### Path A: Enhance BYOL Model (RECOMMENDED for v1)
**Rationale**: Align with Item 002 decision, implement Marketplace submission now, add usage-based billing in v2

**Scope**:
1. **License validation logic**:
   - Create `src/license/license_enforcer.erl` gen_server
   - Implement startup validation hook in `cre_health.erl`
   - Add grace period logic (e.g., 30-day trial, then require license key)
   - License key validation via signature or API call
   - Clear failure modes: read-only mode, shutdown, degraded functionality

2. **License acceptance enforcement**:
   - Enforce `license.acceptEula` parameter from application.yaml
   - Store acceptance timestamp in Mnesia or persistent term
   - Log acceptance event to Cloud Logging

3. **Documentation**:
   - BYOL licensing guide for customers
   - License key acquisition process (if applicable)
   - Grace period behavior documentation
   - Enterprise support options (paid tier)

4. **Preparation for v2 usage-based billing**:
   - Implement usage tracking using existing `cre_cost_reporter` metrics
   - Add `export_to_gcp/1` implementation in `cre_cost_reporter.erl:397`
   - Design metering unit schema (workflow-hour, node-hour)
   - Document migration path from BYOL to usage-based

**Files to create**:
- `src/license/license_enforcer.erl` - License validation gen_server
- `src/license/license_key.erl` - License key validation (if using keys)
- `k8s/charts/cre/templates/license-configmap.yaml` - License configuration
- `docs/license/BYOL_LICENSING_GUIDE.md` - Customer-facing guide

**Files to modify**:
- `src/api/cre_health.erl:188-207` - Add license validation to startup probe
- `src/telemetry/cre_cost_reporter.erl:397-403` - Implement GCP export for usage tracking
- `k8s/charts/cre/application.yaml:127-134` - Add license key parameter (if applicable)
- `src/cre.app.src` - Add license application dependency

**Estimated effort**: 2-3 weeks

#### Path B: Implement Usage-Based Billing (v2 feature)
**Rationale**: Full commercial licensing model with metering, defers Marketplace submission

**Scope**:
1. **Marketplace Metering API integration**:
   - Create `src/metering/marketplace_metering_client.erl`
   - Implement Metering API v1 endpoints
   - Authentication via Workload Identity
   - Batch usage reporting (e.g., hourly, daily)

2. **Usage aggregation logic**:
   - Extend `cre_cost_reporter.erl` with metering unit calculation
   - Define metering units:
     - `workflow-execution-hour`: 1 workflow running for 1 hour
     - `active-node-hour`: 1 CRE node running for 1 hour
     - `task-execution-count`: 1 task execution
   - Aggregate metrics across cluster nodes
   - Handle edge cases: node restarts, cluster scaling

3. **License enforcement with metering**:
   - Implement usage quota limits (e.g., 1000 workflow-hours/month)
   - Throttle or warn when approaching quota
   - Graceful degradation when quota exceeded
   - Usage reporting UI

4. **Billing integration**:
   - Integrate with Google Cloud Billing API
   - Cost estimation and forecasting
   - Invoice generation support
   - Usage breakdown by customer/project

**Files to create**:
- `src/metering/marketplace_metering_client.erl` - Metering API client
- `src/metering/usage_aggregator.erl` - Usage calculation and aggregation
- `src/metering/quota_enforcer.erl` - Usage quota enforcement
- `src/license/metered_license.erl` - License with usage limits
- `k8s/charts/cre/templates/metering-configmap.yaml` - Metering configuration

**Files to modify**:
- `src/telemetry/cre_cost_reporter.erl` - Add metering unit export
- `src/api/cre_health.erl:188-207` - Add quota check to startup probe
- `k8s/charts/cre/application.yaml` - Replace BYOL with usage-based parameters
- `marketplace/deployer.yaml:14-16` - Change billing type to `usage-based`
- `terraform/gcp/modules/security/main.tf` - Add Metering API IAM roles

**Estimated effort**: 6-8 weeks

**Dependencies**:
- Google Cloud Marketplace partner account approval
- Metering API access request
- Billing model pricing strategy
- Legal review of usage-based terms

### Decision Matrix

| Criterion | Path A (BYOL) | Path B (Usage-Based) |
|-----------|---------------|----------------------|
| **Time to Market** | ✅ 2-3 weeks | ❌ 6-8 weeks + approval time |
| **Marketplace Readiness** | ✅ Ready now (Item 002 complete) | ❌ Requires additional work |
| **Implementation Complexity** | ✅ Low (license validation only) | ❌ High (API integration, aggregation) |
| **Commercial Model** | ⚠️ Limited (free OSS + paid support) | ✅ Full usage-based pricing |
| **Revenue Potential** | ⚠️ Support contracts only | ✅ Usage-based revenue |
| **Customer Flexibility** | ✅ No usage limits | ⚠️ Constrained by quotas |
| **Item 002 Alignment** | ✅ Consistent with BYOL decision | ❌ Contradicts Item 002 |
| **v2 Migration Path** | ✅ Can add usage-based later | N/A |

### RECOMMENDATION: Path A (Enhance BYOL)

**Reasons**:
1. **Alignment with Item 002**: Marketplace packaging is complete with BYOL model
2. **Time to market**: Can submit to Marketplace immediately
3. **Foundation for v2**: Implement usage tracking now, add metering API later
4. **Lower risk**: License validation is simpler than full metering integration
5. **Customer feedback**: Gather real usage data before committing to usage-based pricing

**Path to usage-based billing (v2)**:
1. Collect usage metrics with `cre_cost_reporter` (enhance GCP export)
2. Analyze usage patterns from early Marketplace customers
3. Define metering units based on actual usage data
4. Implement Marketplace Metering API integration
5. Migrate from BYOL to usage-based with clear upgrade path

## Open Questions

1. **Billing Model Decision** (CRITICAL BLOCKER):
   - Should Item 004 implement usage-based metering (contradicts Item 002 BYOL decision)?
   - Or should Item 004 enhance BYOL with license validation (consistent with Item 002)?
   - **Recommendation**: Confirm with product owner - item 002 explicitly chose BYOL for v1

2. **License Key Strategy** (if Path A):
   - Will CRE use license keys or just EULA acceptance?
   - If keys: What format? Public key cryptography? Signed JWTs?
   - Who issues license keys? Self-signed or CA-signed?
   - **Recommendation**: Start with EULA acceptance only, add license keys for enterprise tier (v2)

3. **Grace Period Duration** (if Path A):
   - How long is the trial period? 30 days? 90 days?
   - What happens after grace period? Read-only mode? Shutdown?
   - Can grace period be extended? How?
   - **Recommendation**: 30-day trial, then require license key for continued use

4. **Metering Unit Definition** (if Path B):
   - Primary unit: workflow-execution-hour, active-node-hour, or task-execution?
   - How to handle partial units? Round up? Prorate?
   - Free tier allowance? (e.g., 100 workflow-hours/month)
   - **Recommendation**: Use workflow-execution-hour as primary unit, 100 free tier

5. **Usage Aggregation Frequency** (if Path B):
   - Report to Marketplace Metering API: hourly, daily, weekly?
   - How to handle API failures? Retry? Buffer?
   - Data retention for usage history?
   - **Recommendation**: Daily aggregation with hourly batching, 7-day retry buffer

6. **Quota Enforcement Strategy** (if Path B):
   - Soft quota (warning only) or hard quota (block new workflows)?
   - Notification mechanism? Email? Cloud Monitoring alert?
   - Overage charges? Or hard stop?
   - **Recommendation**: Soft quota at 80%, hard quota at 100%, email alerts

7. **Multi-Cluster Metering** (if Path B):
   - How to aggregate usage across multiple CRE clusters?
   - Single Marketplace subscription per GCP project or per cluster?
   - Resource sharing or isolated quotas?
   - **Recommendation**: One subscription per GCP project, aggregated usage

8. **Migration Path from BYOL to Usage-Based** (if Path A then B):
   - How to migrate existing BYOL customers to usage-based?
   - Grandfather clause? Free migration period?
   - Data continuity for usage tracking?
   - **Recommendation**: Offer 6-month migration window, maintain usage tracking from day 1

## Appendix: File Inventory

### Files to Create (Path A - BYOL Enhancement)
- `src/license/license_enforcer.erl` - License validation gen_server (~300 lines)
- `src/license/license_sup.erl` - License supervisor (~50 lines)
- `src/license/LICENSE` - License module docs (~50 lines)
- `k8s/charts/cre/templates/license-configmap.yaml` - License configuration (~30 lines)
- `docs/license/BYOL_LICENSING_GUIDE.md` - Customer guide (~200 lines)
- `docs/license/ENTERPRISE_SUPPORT.md` - Paid support options (~150 lines)

### Files to Modify (Path A - BYOL Enhancement)
- `src/api/cre_health.erl:188-207` - Add license check to startup probe
- `src/telemetry/cre_cost_reporter.erl:397-403` - Implement usage tracking export
- `src/cre.app.src` - Add license application dependency
- `k8s/charts/cre/application.yaml:127-134` - Enhance license parameters
- `k8s/charts/cre/values.yaml` - Add license configuration section
- `src/app/cre_sup.erl` - Add license supervisor to supervision tree

### Files to Create (Path B - Usage-Based Billing)
- `src/metering/marketplace_metering_client.erl` - Metering API client (~400 lines)
- `src/metering/usage_aggregator.erl` - Usage calculation (~300 lines)
- `src/metering/quota_enforcer.erl` - Quota enforcement (~250 lines)
- `src/metering/metering_sup.erl` - Metering supervisor (~50 lines)
- `src/license/metered_license.erl` - License with usage limits (~200 lines)
- `k8s/charts/cre/templates/metering-configmap.yaml` - Metering config (~50 lines)
- `docs/metering/USAGE_BASED_LICENSING.md` - Customer guide (~300 lines)
- `docs/metering/METERING_API_REFERENCE.md` - API documentation (~200 lines)

### Files to Modify (Path B - Usage-Based Billing)
- `src/telemetry/cre_cost_reporter.erl:1-465` - Add metering unit export
- `src/api/cre_health.erl:188-207` - Add quota check to startup probe
- `k8s/charts/cre/application.yaml:1-218` - Replace BYOL with usage-based params
- `marketplace/deployer.yaml:14-16` - Change billing type to usage-based
- `terraform/gcp/modules/security/main.tf:177-200` - Add Metering API IAM roles
- `src/cre.app.src` - Add metering application dependency
- `src/app/cre_sup.erl` - Add metering supervisor to supervision tree

### Existing Files to Reference (No Changes)
- `src/telemetry/cre_metrics.erl:1-274` - Metrics registry patterns
- `src/telemetry/otel_metrics.erl:1-376` - OpenTelemetry API patterns
- `src/telemetry/prometheus_exporter.erl:1-249` - HTTP export pattern
- `src/api/cre_health.erl:1-537` - Health probe patterns
- `marketplace/deployer.yaml:1-53` - BYOL billing model
- `k8s/charts/cre/application.yaml:1-218` - License acceptance UI
