# CRE Pricing

## Pricing Model

### Bring Your Own License (BYOL)

CRE is distributed under the Apache-2.0 license and available on GCP Marketplace as a **Bring Your Own License (BYOL)** offering.

**What this means:**
- The software is free to use (Apache-2.0 open source)
- You pay only for Google Cloud infrastructure costs
- No licensing fees for CRE itself
- No per-workflow or per-execution fees

## Google Cloud Infrastructure Costs

You are responsible for the following GCP costs when deploying CRE:

### Compute Costs
- **GKE Cluster Nodes**: Based on machine type (e2-medium recommended for production)
- **Regional Cluster**: Additional cost for regional HA (3 zones)
- **Spot VMs**: Optional cost savings for stateless workloads

### Storage Costs
- **Persistent Disks**: For stateful workloads requiring PVCs
- **Cloud Spanner**: If using Spanner adapter for workflow state
- **Cloud Storage**: For backup artifacts

### Network Costs
- **Load Balancers**: For external ingress
- **Cloud NAT**: For private cluster egress
- **Network Egress**: Standard GCP egress charges

### Monitoring Costs
- **Cloud Monitoring**: Based on metrics ingested
- **Cloud Logging**: Based on logs ingested and retained

## Cost Optimization

### Recommended Configuration for Cost Efficiency

**Minimum Viable Production:**
- 1 x e2-medium node (2 vCPU, 4 GB memory)
- ~$15-30/month depending on region

**High Availability Production:**
- 3 x e2-medium nodes across 3 zones
- Regional GKE cluster
- ~$50-100/month depending on region

### Cost Saving Tips

1. **Use Spot VMs**: For stateless workflow processing
   - Up to 60-90% savings compared to regular VMs
   - Automatic preemption handling built into CRE

2. **Right-size Resources**: Monitor with Cloud Monitoring dashboards
   - Use HPA for automatic scaling based on load
   - Start with e2-medium, scale up as needed

3. **Optimize Logging**: Configure log retention and filtering
   - Reduce logs ingested to Cloud Logging
   - Use appropriate log levels (INFO in production, DEBUG in dev)

4. **Backup Strategy**: Choose appropriate backup frequency
   - Daily backups for most workloads
   - Hourly only for critical workflows

## Total Cost of Ownership Example

### Small Production Deployment
**Scenario**: 1000 workflows/day, 10 patterns average, regional HA

| Component | Monthly Cost |
|-----------|--------------|
| GKE Nodes (3 x e2-medium) | $45-75 |
| Load Balancer | $18 |
| Cloud NAT | $30 |
| Cloud Monitoring | ~$5 |
| Cloud Logging (minimal) | ~$2 |
| **Total** | **~$100-130/month** |

### Medium Production Deployment
**Scenario**: 10000 workflows/day, autoscaling, regional HA

| Component | Monthly Cost |
|-----------|--------------|
| GKE Nodes (3-6 x e2-medium) | $90-150 |
| Load Balancer | $18 |
| Cloud NAT | $30 |
| Cloud Spanner (optional) | $65+ |
| Cloud Monitoring | ~$15 |
| Cloud Logging | ~$10 |
| **Total** | **~$230-290/month** |

## Billing

- **Infrastructure**: Billed by Google Cloud through standard GCP billing
- **CRE Software**: Free (Apache-2.0 license)
- **No Hidden Fees**: No per-workflow, per-execution, or per-user fees

## Pricing Inquiries

For enterprise pricing inquiries including:
- Custom SLA commitments
- Priority support packages
- Custom integration services

Contact: cre-support@common-runtime.org

## Related Documentation
- [Cost Monitoring](/src/telemetry/cre_cost_reporter.erl)
- [Autoscaling Metrics](/src/telemetry/autoscaling_metrics.erl)
- [Spot VM Configuration](/k8s/gcp/spot-nodepool.yaml)
