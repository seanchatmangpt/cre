# SLO/SLA Definitions and Business Commitments

## Overview

This document defines Service Level Objectives (SLOs) and Service Level Agreements (SLAs) for all critical services.

**Key Difference:**
- **SLO**: Internal target (engineering commitment)
- **SLA**: External promise (customer contract with penalties)

## API Service

### API Availability

**Metric:** Percentage of successful requests (non-5xx status codes)

**Definitions:**
| Level | Target | Allowed Downtime | Duration |
|-------|--------|-----------------|----------|
| SLO | 99.9% | 43.2 minutes | 30 days |
| SLA | 99.95% | 21.6 minutes | 30 days |

**Downtime Calculation:**
```
Downtime (minutes) = Total minutes × (1 - Availability%)
```

Examples:
- 99.9% SLO allows 43.2 minutes of downtime per month
- 99.95% SLA allows 21.6 minutes of downtime per month

**Consequences of SLA Breach:**
- 5-10% monthly service credit (prorated)
- Escalation to VP Engineering
- Customer notification
- Incident review required

**Measurement:**
```promql
(sum(rate(http_requests_total{status!~"5.."}[30d]))
 / sum(rate(http_requests_total[30d]))) * 100
```

### API Latency

**Metric:** Request response time (95th percentile)

**Definitions:**
| Level | Target | Requirement | Duration |
|-------|--------|-------------|----------|
| SLO | 200ms | P95 < 200ms | 1 hour |
| SLA | 500ms | P99 < 500ms | 5 minutes |

**What This Means:**
- P95 = 95% of requests respond within target time
- P99 = 99% of requests respond within target time
- Measured from request received to response sent

**SLO Breach Conditions:**
- P95 latency > 200ms for 10 consecutive minutes
- Automatic escalation alert sent

**SLA Breach Conditions:**
- P99 latency > 500ms for 5 consecutive minutes
- Customer notification required
- 5% service credit possible

**Measurement:**
```promql
histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))
histogram_quantile(0.99, rate(http_request_duration_seconds_bucket[5m]))
```

### Error Rate

**Metric:** Percentage of failed requests (5xx errors)

**Definitions:**
| Level | Target | Threshold | Duration |
|-------|--------|-----------|----------|
| SLO | < 0.1% | > 0.1% | 5 minutes |
| SLA | < 0.05% | > 0.05% | 15 minutes |

**Impact:**
- 0.1% error rate = 1 error per 1,000 requests
- 0.05% error rate = 1 error per 2,000 requests

**Escalation:**
- SLO breach: Investigation required
- SLA breach: Immediate incident response

## Database Service

### Database Availability

**Metric:** Percentage of successful database queries

**Definitions:**
| Level | Target | Annual Downtime | Type |
|-------|--------|-----------------|------|
| SLO | 99.95% | 4.38 hours/year | Targets |
| SLA | 99.99% | 52.6 minutes/year | Customer commitment |

**What Counts as Downtime:**
- Unable to execute queries
- Connection pool exhausted
- Replication failures
- Backup failures affecting recovery

**SLA Breach Penalties:**
- 99.9% - 99.95%: 5% credit
- 99.0% - 99.9%: 10% credit
- < 99.0%: 30% credit

**Measurement:**
```promql
(sum(rate(db_query_total{status="success"}[365d]))
 / sum(rate(db_query_total[365d]))) * 100
```

### Data Durability

**Metric:** Successful persistence of write operations

**Definitions:**
| Level | Target | Implication |
|-------|--------|-----------|
| SLO | 99.999% | 0 to 1 lost record per year |
| SLA | 99.999999% | Virtually no data loss |

**Data Loss Scenarios:**
- Lost transactions
- Corrupted data
- Unrecoverable failures
- Catastrophic failure without backup

**SLA Breach Impact:**
- Automatic credit: 100%
- Legal review required
- Incident investigation mandatory
- Post-incident analysis required

### Data Consistency

**Metric:** Replication lag in multi-region setup

**Definitions:**
| Level | Target | Acceptable Lag |
|-------|--------|----------------|
| SLO | 100ms | 95% of replicas |
| SLA | 1 second | 99% of replicas |

**Implications:**
- 100ms lag: Minimal consistency issues
- 1 second lag: May see stale data briefly
- > 10 seconds: Significant consistency concerns

## Message Queue Service

### Message Delivery

**Metric:** Successful delivery of enqueued messages

**Definitions:**
| Level | Target | Implication | Duration |
|-------|--------|-----------|----------|
| SLO | 99% | 1 undelivered per 100 | 5 min window |
| SLA | 99.9% | 1 undelivered per 1000 | 30 day |

**Failure Scenarios:**
- Message lost before processing
- Processing failure without retry
- Dead-letter queue overflow
- Infinite retry loop

**SLA Breach:**
- 5-10% service credit
- Root cause analysis required

### Processing Latency

**Metric:** Time from enqueue to processing completion

**Definitions:**
| Level | Target | Requirement |
|-------|--------|-------------|
| SLO | 5 minutes | 95% within 5 min |
| SLA | 15 minutes | 99% within 15 min |

**Business Impact:**
- 5 minute SLO: Real-time processing
- 15 minute SLA: Acceptable batch window

## Kubernetes Cluster

### Pod Availability

**Metric:** Percentage of healthy pods in Running state

**Definitions:**
| Level | Target | Implication |
|-------|--------|-----------|
| SLO | 99.9% | 1 pod crash per 1000 pod-days |
| SLA | 99% | 1 pod crash per 100 pod-days |

**Measurement:**
```promql
(count(kube_pod_status_phase{phase="Running"})
 / count(kube_pod_status_phase)) * 100
```

### Node Availability

**Metric:** Percentage of healthy nodes in Ready state

**Definitions:**
| Level | Target | Impact |
|-------|--------|--------|
| SLO | 99.5% | 1 node failure per 200 node-days |
| SLA | 99.0% | 1 node failure per 100 node-days |

**SLA Breach:**
- Triggers automatic failover
- 5% service credit for affected services
- Node replacement initiated

## Cache Service

### Cache Hit Rate

**Metric:** Percentage of cache hits vs total requests

**Definitions:**
| Level | Target | Performance | Implication |
|-------|--------|-------------|-----------|
| SLO | 95% | Good | 5 cache misses per 100 |
| SLA | 90% | Acceptable | 10 cache misses per 100 |

**Performance Impact:**
- 95% hit rate: DB load reduced by 95%
- 90% hit rate: DB load reduced by 90%
- < 90%: Database strain increases

**SLO Breach Investigation:**
- Check for cache invalidation issues
- Verify key distribution
- Review eviction policies

## Security & Compliance

### Vulnerability Patching

**Metric:** Critical vulnerabilities without patches

**Definitions:**
| Level | Target | Requirement |
|-------|--------|-------------|
| SLO | 0 | Zero tolerance |
| SLA | 0 | Legal/contractual |

**SLA Breach Consequences:**
- Service shutdown possible
- Legal action possible
- Regulatory fines
- Customer termination rights

### Backup & Recovery

**Metric:** Backup freshness and recovery capability

**Definitions:**
| Parameter | Target | Meaning |
|-----------|--------|---------|
| RTO | 1 hour | Recovery time objective |
| RPO | 15 minutes | Recovery point objective |

**Implications:**
- RPO 15 min: Max 15 minutes of data loss
- RTO 1 hour: Service restored within 1 hour
- Tested quarterly (mandatory)

**SLA Violation:**
- Backup older than 1 hour: CRITICAL
- Recovery time > 1 hour: Customer escalation
- Untested backup: Regulatory violation

## Customer Support

### Support Response Times

**Metric:** Time from report to first response

**Definitions:**
| Severity | SLO | SLA | Escalation |
|----------|-----|-----|-----------|
| Critical | 15 min | 30 min | VP Engineering |
| High | 1 hour | 2 hours | Engineering Manager |
| Medium | 4 hours | 8 hours | Support Manager |
| Low | 24 hours | 48 hours | Support Team |

**Critical Issue Example:**
- Services completely unavailable
- Data loss occurring
- Security breach in progress
- Multiple customers affected

**SLA Breach Penalties:**
- Response time miss: 1% credit per hour overdue
- Resolution time miss: 3% credit per hour overdue

## Error Budget Policy

### What is Error Budget?

Error budget = (1 - SLO Target) × Time Period

**Example (API Service):**
```
Monthly error budget = (1 - 0.999) × 43,200 minutes
                     = 0.001 × 43,200
                     = 43.2 minutes of allowed downtime
```

### Budget Tracking

**When Budget is High (> 50%):**
- ✅ Feature development approved
- ✅ Experimental changes allowed
- ✅ Deploy to production during business hours

**When Budget is Medium (20-50%):**
- ⚠️ Feature development cautious
- ⚠️ Only critical changes
- ⚠️ Extra review required
- ⚠️ Canary deployments mandatory

**When Budget is Low (< 20%):**
- 🔴 Feature freeze
- 🔴 Only stability improvements
- 🔴 All changes require VP approval
- 🔴 Rollback plans mandatory

**When Budget is Exhausted (0%):**
- 🛑 Complete feature freeze
- 🛑 All resources to stability
- 🛑 Daily incident reviews
- 🛑 C-suite notification
- 🛑 Possible service restrictions

## SLA Credits

### Credit Calculation

**Monthly Availability → Credit:**
| Availability | Credit | Impact |
|--------------|--------|---------|
| 99.95% - 99.99% | 5% | Minimal |
| 99.9% - 99.95% | 10% | Noticeable |
| 99.0% - 99.9% | 25% | Significant |
| < 99.0% | 100% | Full refund |

**Example:**
```
Monthly bill: $10,000
Availability: 99.92% (between SLO and SLA)
Credit: 10% = $1,000 credit
```

### Credit Request Process

1. Customer notices SLA breach
2. Support team validates
3. Engineering confirms
4. Credits applied automatically
5. Customer notification sent

## Exclusions from SLA

The following do NOT count against SLA:

1. **Customer Fault**
   - Incorrect API usage
   - Invalid requests
   - Client-side errors

2. **External Factors**
   - DDoS attacks
   - ISP outages
   - Customer network issues

3. **Scheduled Maintenance**
   - Announced in advance
   - During maintenance window
   - Excludes unplanned changes

4. **Force Majeure**
   - Natural disasters
   - War, terrorism
   - Extreme weather
   - Government action

## Quarterly Review

Every quarter, SLO/SLA targets are reviewed:

**Review Items:**
- [ ] Actual achievement vs. targets
- [ ] Trends (improving/degrading)
- [ ] External factors impacting
- [ ] Target adjustments needed
- [ ] Action items

**Possible Outcomes:**
- ✅ Targets appropriate → Continue
- ⬆️ Too easy → Increase targets
- ⬇️ Too hard → Decrease targets
- 🔄 Change methodology → Update tracking

## Related Documents

- [Prometheus Rules](./prometheus/rules/)
- [Grafana SLO/SLA Dashboard](./grafana/dashboards/slo-sla-dashboard.json)
- [Alert Rules](./prometheus/rules/slo.yml)
- [SLA Breach Procedures](./SETUP.md)

## References

- [Google SRE: SLOs](https://sre.google/slo/)
- [Prometheus: Recording Rules](https://prometheus.io/docs/prometheus/latest/configuration/recording_rules/)
- [SLA Best Practices](https://www.atlassian.com/incident-management/sla)
