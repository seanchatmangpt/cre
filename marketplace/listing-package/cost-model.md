# CRE Cost Model

This document explains the pricing and infrastructure costs for CRE (Common Runtime Environment) on Google Cloud Marketplace.

## Pricing Model

### Bring Your Own License (BYOL)

CRE is distributed under the **Apache-2.0 license** and available on GCP Marketplace as a **Bring Your Own License (BYOL)** offering.

**What this means for you:**

- ✅ **Software is Free**: No licensing fees for CRE (open source)
- ✅ **Pay for Infrastructure**: Only pay Google Cloud infrastructure costs
- ✅ **No Usage Fees**: No per-workflow, per-execution, or per-user fees
- ✅ **Transparent Billing**: All costs visible in your GCP billing account

### Total Cost of Ownership

Your total cost = **Google Cloud infrastructure costs only**

---

## Google Cloud Infrastructure Costs

You are responsible for the following GCP costs when deploying CRE:

### Compute Costs (GKE)

| Resource | Cost Factor | Typical Cost (us-central1) |
|----------|-------------|---------------------------|
| **GKE Nodes** | Machine type × count | e2-medium: ~$15-25/month/node |
| **Regional Cluster** | Multi-zone HA | + control plane fee (~$0.10/hour) |
| **Spot VMs** (optional) | Up to 60-90% discount | e2-medium spot: ~$2-5/month/node |

**Recommended Machine Types**:
- **Development/Test**: `e2-small` (1 vCPU, 2 GB) - ~$10-15/month
- **Small Production**: `e2-medium` (2 vCPU, 4 GB) - ~$15-25/month
- **Medium Production**: `e2-large` (2 vCPU, 8 GB) - ~$30-40/month

### Storage Costs

| Resource | Cost Factor | Typical Cost |
|----------|-------------|--------------|
| **Persistent Disks** | Size × storage type | SSD: $0.20/GB/month, HDD: $0.04/GB/month |
| **Cloud Spanner** (optional) | Nodes, storage, throughput | $65+/month/node |
| **Cloud Storage** (backups) | Size × storage class | Standard: $0.026/GB/month |

**Typical CRE Storage Usage**:
- **Small Deployment**: 10 GiB SSD = ~$2/month
- **Medium Deployment**: 50 GiB SSD = ~$10/month
- **Backup Storage**: 20 GiB = ~$0.50/month

### Network Costs

| Resource | Cost Factor | Typical Cost |
|----------|-------------|--------------|
| **Load Balancer** | Per LB fee | ~$18/month (standard LB) |
| **Cloud NAT** | Per NAT gateway + data processing | ~$30/month + ~$0.045/GB |
| **Network Egress** | Data leaving GCP region | Free ingress, egress varies by destination |

### Monitoring and Logging Costs

| Resource | Cost Factor | Typical Cost |
|----------|-------------|--------------|
| **Cloud Monitoring** | Metrics ingested | ~$5-15/month (small deployment) |
| **Cloud Logging** | Logs ingested + retention | ~$2-10/month (varies by volume) |

---

## Cost Optimization

### Recommended Configuration for Cost Efficiency

#### Minimum Viable Production

**Best for**: Small teams, development/testing, non-critical workloads

| Component | Configuration | Monthly Cost |
|-----------|---------------|--------------|
| **GKE Cluster** | 1 x e2-medium node, zonal | ~$15-25 |
| **CRE Pods** | 1 replica | Included in node cost |
| **Storage** | 10 GiB standard persistent disk | ~$0.40 |
| **Load Balancer** | Regional load balancer | ~$18 |
| **Cloud NAT** (if private cluster) | NAT gateway | ~$30 |
| **Monitoring/Logging** | Basic metrics, 30-day log retention | ~$5 |
| **Total** | | **~$70-110/month** |

#### High Availability Production

**Best for**: Production workloads, HA requirements, business-critical processes

| Component | Configuration | Monthly Cost |
|-----------|---------------|--------------|
| **GKE Cluster** | 3 x e2-medium nodes, regional | ~$45-75 |
| **CRE Pods** | 3 replicas (1 per node) | Included in node cost |
| **Storage** | 50 GiB SSD persistent disk (×3) | ~$30 |
| **Load Balancer** | Regional load balancer | ~$18 |
| **Cloud NAT** (if private cluster) | NAT gateway | ~$30 |
| **Cloud Spanner** (optional, for multi-region) | 1 node | ~$65 |
| **Monitoring/Logging** | Enhanced metrics, 30-day log retention | ~$15 |
| **Backup Storage** | 100 GiB Cloud Storage | ~$2.60 |
| **Total** | | **~$210-250/month** |

#### Cost-Optimized with Spot VMs

**Best for**: Stateful workflows with HA, cost-sensitive workloads

| Component | Configuration | Monthly Cost |
|-----------|---------------|--------------|
| **GKE Cluster** | 1 regular + 2 spot e2-medium nodes | ~$25-35 |
| **CRE Pods** | 3 replicas with pod disruption budget | Included in node cost |
| **Storage** | 50 GiB SSD persistent disk (×3) | ~$30 |
| **Load Balancer** | Regional load balancer | ~$18 |
| **Cloud NAT** (if private cluster) | NAT gateway | ~$30 |
| **Monitoring/Logging** | Basic metrics, 30-day log retention | ~$5 |
| **Total** | | **~$110-130/month** |

**Note**: CRE automatically handles Spot VM preemptions with task retry.

### Cost Saving Tips

#### 1. Use Spot VMs for Stateful Workloads

- **Savings**: Up to 60-90% compared to regular VMs
- **How CRE Handles Preemption**: Automatic task retry, Mnesia data survives pod restart
- **Best For**: Workflow processing, batch jobs, non-real-time workloads
- **Not Recommended For**: Real-time APIs, ultra-low latency requirements

```yaml
# Example: Spot node pool configuration
apiVersion: v1
kind: PersistentVolume
metadata:
  name: cre-spot-pv
spec:
  capacity:
    storage: 50Gi
  accessModes:
  - ReadWriteOnce
  storageClassName: premium-rwo  # Use SSD for faster restarts
```

#### 2. Right-Size Resources with Autoscaling

- **Horizontal Pod Autoscaler (HPA)**: Scale pods based on CPU/workflow queue
- **Cluster Autoscaler**: Scale nodes based on pod pending
- **Vertical Pod Autoscaler**: Adjust CPU/memory requests based on usage

```bash
# Enable HPA
kubectl autoscale deployment cre -n cre --min=3 --max=10 --cpu-percent=70

# Enable cluster autoscaler
gcloud container clusters update cre-cluster \
  --enable-autoscaling \
  --min-nodes=3 \
  --max-nodes=10
```

#### 3. Optimize Logging and Monitoring

- **Log Levels**: Use `INFO` in production, `DEBUG` only in development
- **Log Filtering**: Exclude verbose logs from Cloud Logging export
- **Metric Sampling**: Reduce OpenTelemetry trace sampling rate (e.g., 0.1 = 10%)

```bash
# Set log level via environment variable
kubectl set env deployment cre -n cre CRE_LOG_LEVEL=info

# Set trace sampling rate
kubectl set env deployment cre -n cre CRE_OTEL_SAMPLING=0.1
```

#### 4. Choose Appropriate Backup Strategy

| Workload Criticality | Backup Frequency | Retention | Monthly Cost (100 GiB) |
|---------------------|------------------|-----------|------------------------|
| **Development** | Daily | 7 days | ~$0.10 |
| **Production** | Daily | 30 days | ~$0.50 |
| **Mission-Critical** | Hourly | 1 year | ~$2-5 |

#### 5. Use Reserved Instances for Long-Term Workloads

- **Commitment**: 1-year or 3-year commitment
- **Savings**: Up to 60% off on-demand pricing
- **Best For**: Stable, predictable workloads

```bash
# Create reserved instance
gcloud compute commitments create cre-commitment \
  --region=us-central1 \
  --resources=vcpu=2,memory=8GB \
  --plan=12-month
```

---

## Total Cost of Ownership Examples

### Example 1: Small Production Deployment

**Scenario**: Small business, 1000 workflows/day, regional HA, 10-day backup retention

| Component | Quantity | Unit Cost | Monthly Cost |
|-----------|----------|-----------|--------------|
| GKE Nodes (e2-medium) | 3 | $20/node | $60 |
| Load Balancer | 1 | $18 | $18 |
| Cloud NAT | 1 | $30 | $30 |
| Persistent Disk (SSD) | 50 GiB | $0.20/GB | $10 |
| Cloud Monitoring | Basic | $5 | $5 |
| Cloud Logging | Minimal | $2 | $2 |
| Cloud Storage (backups) | 20 GiB | $0.026/GB | $0.52 |
| **Total** | | | **~$125-135/month** |

**Annual Cost**: ~$1,500-1,600/year

### Example 2: Medium Production Deployment

**Scenario**: Mid-size company, 10000 workflows/day, autoscaling, Cloud Spanner, 30-day backup retention

| Component | Quantity | Unit Cost | Monthly Cost |
|-----------|----------|-----------|--------------|
| GKE Nodes (e2-medium) | 3-6 (autoscaling) | $20/node | $90-120 |
| Load Balancer | 1 | $18 | $18 |
| Cloud NAT | 1 | $30 | $30 |
| Persistent Disk (SSD) | 150 GiB | $0.20/GB | $30 |
| Cloud Spanner | 1 node | $65/node | $65 |
| Cloud Monitoring | Enhanced | $15 | $15 |
| Cloud Logging | Moderate | $10 | $10 |
| Cloud Storage (backups) | 100 GiB | $0.026/GB | $2.60 |
| **Total** | | | **~$270-310/month** |

**Annual Cost**: ~$3,200-3,700/year

### Example 3: Cost-Optimized Deployment

**Scenario**: Cost-conscious startup, 5000 workflows/day, Spot VMs, 7-day backup retention

| Component | Quantity | Unit Cost | Monthly Cost |
|-----------|----------|-----------|--------------|
| GKE Nodes (1 regular + 2 spot e2-medium) | 3 | Mixed | ~$30 |
| Load Balancer | 1 | $18 | $18 |
| Cloud NAT | 1 | $30 | $30 |
| Persistent Disk (SSD) | 100 GiB | $0.20/GB | $20 |
| Cloud Monitoring | Basic | $5 | $5 |
| Cloud Logging | Minimal | $2 | $2 |
| Cloud Storage (backups) | 50 GiB | $0.026/GB | $1.30 |
| **Total** | | | **~$105-115/month** |

**Annual Cost**: ~$1,250-1,400/year (**~60% savings** vs regular VMs)

---

## Cost Monitoring

### View CRE Costs in Google Cloud Console

1. Navigate to **Billing** → **Reports**
2. Filter by **Kubernetes Engine** and **Cluster Name**
3. Group by **SKU** to see cost breakdown

### Set Budget Alerts

```bash
# Create budget alert
gcloud billing budgets create $BILLING_ACCOUNT_ID \
  --display-name="CRE Monthly Budget" \
  --budget-amount=200USD \
  --threshold-rule=percent=90
```

### Track Cost per Workflow

CRE exports workflow execution metrics to Cloud Monitoring. Use these to calculate cost per workflow:

```
Cost per Workflow = Total Monthly Cost / Workflows Executed
```

Example: $100/month ÷ 10,000 workflows = **$0.01 per workflow**

---

## Billing

### How You're Billed

- **Infrastructure Costs**: Billed by Google Cloud through your standard GCP billing account
- **CRE Software**: **FREE** - No licensing fees, no usage fees
- **No Hidden Fees**: No per-workflow, per-execution, or per-user fees
- **Transparent Pricing**: All costs are visible in your GCP billing dashboard

### Billing Account Setup

When you deploy CRE from Google Cloud Marketplace:

1. CRE is linked to your existing GCP billing account
2. Infrastructure costs appear under "Kubernetes Engine"
3. No separate invoice or payment for CRE software

---

## Cost Comparison with Alternatives

### Commercial Workflow Engines

| Product | Pricing Model | Estimated Annual Cost (10k workflows/day) |
|---------|---------------|-------------------------------------------|
| **CRE** | BYOL (infrastructure only) | ~$3,000-4,000 |
| **Camunda** | Per-process-instance fees | ~$15,000-25,000+ |
| **Appian** | Per-user + per-process fees | ~$20,000-40,000+ |
| **IBM BPM** | Per-process-instance + PVU licenses | ~$25,000-50,000+ |
| **Pega** | Per-user + per-case fees | ~$30,000-60,000+ |

**CRE Savings**: **~85-95%** compared to commercial workflow engines

---

## Pricing Inquiries

### Enterprise Support and Custom Solutions

For enterprise requirements including:

- **Custom SLA Commitments**: 99.9%, 99.95%, 99.99% uptime guarantees
- **Priority Support**: 24/7 support, < 1-hour response time
- **Custom Integration Services**: Workflow design, implementation, training
- **On-Premise Deployment**: CRE on-premise (BYOL)

**Contact**: cre-support@common-runtime.org

---

## Related Documentation

- **[Operations Guide - Scaling](operations-guide.md)** - Autoscaling configuration
- **[Autoscaling Metrics](../../src/telemetry/autoscaling_metrics.erl)** - Custom metrics for HPA
- **[Spot VM Configuration](../../k8s/gcp/spot-nodepool.yaml)** - Spot VM setup
- **[Cost Reporter](../../src/telemetry/cre_cost_reporter.erl)** - Cost tracking integration

---

## Cost Calculator

Use this formula to estimate your monthly CRE costs:

```
Monthly Cost =
  (Node Count × Node Cost) +
  Load Balancer Fee +
  Cloud NAT Fee (if private cluster) +
  (Storage GB × Storage Cost) +
  Monitoring/Logging +
  Backup Storage +
  Network Egress (if applicable)
```

**Example**:
```
Monthly Cost =
  (3 nodes × $20) +
  $18 +
  $30 +
  (50 GB × $0.20) +
  $15 +
  $10 +
  $2
  = $60 + $18 + $30 + $10 + $15 + $10 + $2
  = ~$145/month
```

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
**Billing Questions**: cre-support@common-runtime.org
