# Enterprise Cloud Monitoring Solution

Production-ready monitoring, alerting, and observability platform with SLO/SLA tracking for enterprise applications.

## Features

### Core Monitoring
- **Prometheus**: Time-series database with 30-day retention (configurable)
- **Grafana**: Beautiful dashboards and data visualization
- **Alertmanager**: Intelligent alert routing and deduplication
- **Node Exporter**: Comprehensive system metrics
- **cAdvisor**: Container and pod metrics
- **kube-state-metrics**: Kubernetes cluster state (optional)

### Advanced Capabilities
- **SLO Monitoring**: Service Level Objective tracking with error budget
- **SLA Tracking**: Service Level Agreement compliance monitoring
- **Multi-cloud**: GCP Cloud Monitoring integration
- **Log Aggregation**: Loki integration (optional)
- **Distributed Tracing**: Jaeger integration (optional)
- **Long-term Storage**: VictoriaMetrics integration (optional)

### Alert Management
- Multi-channel routing (Slack, PagerDuty, OpsGenie, email)
- Alert grouping and deduplication
- Intelligent inhibition rules
- SLA breach escalation
- Critical alert handling

### Pre-built Dashboards
1. **System Overview** - Infrastructure metrics
2. **Application Metrics** - API performance, errors, latency
3. **SLO/SLA Dashboard** - Error budget, availability trends

## Quick Start

### Prerequisites
- Docker & Docker Compose (for local setup)
- OR Kubernetes 1.19+ (for production)
- 4GB+ RAM, 20GB+ disk space

### Local Development

```bash
# Clone and navigate
cd /home/user/cre/monitoring

# Start monitoring stack
docker-compose up -d

# Access services
# Prometheus: http://localhost:9090
# Grafana: http://localhost:3000 (admin/admin)
# Alertmanager: http://localhost:9093
```

### Kubernetes Production

```bash
# Deploy monitoring namespace and RBAC
kubectl apply -f kubernetes/monitoring-namespace.yaml

# Deploy Prometheus
kubectl apply -f kubernetes/prometheus-deployment.yaml

# Deploy Alertmanager
kubectl apply -f kubernetes/alertmanager-deployment.yaml

# Deploy Grafana (recommended: use Helm)
helm install grafana grafana/grafana --namespace monitoring
```

See [SETUP.md](./SETUP.md) for detailed configuration.

## Directory Structure

```
monitoring/
├── prometheus/                 # Prometheus configuration
│   ├── prometheus.yml         # Main configuration
│   └── rules/                 # Alert and SLO/SLA rules
│       ├── alerts.yml         # Alert definitions
│       ├── slo.yml            # SLO tracking rules
│       └── sla.yml            # SLA compliance rules
├── grafana/                    # Grafana configuration
│   ├── dashboards/            # Pre-built dashboards
│   │   ├── system-overview.json
│   │   ├── application-metrics.json
│   │   └── slo-sla-dashboard.json
│   └── provisioning/          # Auto-provisioning configs
│       ├── datasources/
│       └── dashboards/
├── alertmanager/              # Alertmanager configuration
│   └── alertmanager.yml       # Alert routing & receivers
├── gcp-cloud-monitoring/      # GCP integration
│   └── gcp-monitoring.yaml    # Cloud Monitoring setup
├── kubernetes/                # Kubernetes manifests
│   ├── monitoring-namespace.yaml
│   ├── prometheus-deployment.yaml
│   └── alertmanager-deployment.yaml
├── docker-compose.yml         # Local development stack
├── SETUP.md                   # Detailed setup guide
└── README.md                  # This file
```

## Configuration Overview

### Prometheus (`prometheus/prometheus.yml`)

Configures:
- **Global settings**: Scrape interval (15s default), evaluation interval
- **Data sources**: Kubernetes API, nodes, pods, applications, GCP
- **Alert rules**: System, application, SLO/SLA alerts
- **Remote storage**: VictoriaMetrics or GCP Cloud Monitoring

Key scrape targets:
```yaml
- Kubernetes API Server
- Kubernetes Nodes (kubelet)
- Kubernetes Pods
- kube-state-metrics
- Node Exporter
- cAdvisor
- Application metrics
- Istio/Service mesh
- Custom applications
- GCP Cloud Monitoring (Stackdriver)
```

### Alert Rules

#### System Alerts (`prometheus/rules/alerts.yml`)
- CPU usage > 80%
- Memory usage > 85%
- Disk space > 90%
- System load critical
- Network errors
- Node not ready
- Container CPU/memory high

#### SLO Rules (`prometheus/rules/slo.yml`)
- API Availability: 99.9% target
- API Latency: P95 < 200ms
- Error budget tracking
- Burn rate monitoring
- Database availability: 99.95%
- Cache hit rate: 95%
- Pod availability: 99.9%

#### SLA Rules (`prometheus/rules/sla.yml`)
- API Availability: 99.95% (customer-facing SLA)
- API Latency: P99 < 500ms
- Database Availability: 99.99%
- Message Delivery: 99.9%
- Data Durability: 99.999999%
- Backup/Recovery: RTO 1h, RPO 15m
- Security: Zero unpatched critical vulnerabilities
- Support Response: Critical < 1 hour

### Alertmanager (`alertmanager/alertmanager.yml`)

Routes alerts by:
- **Severity**: Critical, Warning, Info
- **Component**: System, Kubernetes, Database, Security
- **SLA Type**: SLA/SLO breaches get priority routing

Receivers:
- `critical-receiver`: Slack (critical-alerts) + PagerDuty + Email
- `sla-breach-receiver`: Slack (sla-breaches) + PagerDuty + Executive email
- `warning-receiver`: Slack (warnings)
- `kubernetes-receiver`: Slack (kubernetes)
- `database-receiver`: Slack (database-alerts) + PagerDuty
- `security-receiver`: Slack (security-alerts) + PagerDuty + Email

### Grafana Dashboards

**System Overview**
- Memory usage trends
- Disk space gauge
- CPU usage by mode
- System load average
- Network traffic

**Application Metrics**
- Application health status
- Success rate (2xx vs 4xx/5xx)
- P95 latency
- Requests per second
- Request duration percentiles
- Container memory/CPU

**SLO/SLA Dashboard**
- API Availability vs SLO (99.9%) and SLA (99.95%)
- Error budget remaining
- Latency vs SLO target
- Burn rate tracking
- Availability trends

## Metrics & SLIs

### Key Performance Indicators

| Metric | Target | SLO | SLA | Alert |
|--------|--------|-----|-----|-------|
| API Availability | 99.95% | 99.9% (1h) | 99.95% (30d) | < 99.9% for 5m |
| API Latency P95 | 200ms | - | 500ms | > 200ms for 10m |
| Error Rate | < 0.1% | - | - | > 5% for 5m |
| Database Uptime | 99.99% | - | 99.99% (1y) | Breach = CRITICAL |
| Pod Availability | 99.9% | 99.9% (1h) | - | < 99.9% for 5m |
| Cache Hit Rate | 95%+ | 95% (1h) | - | < 95% for 10m |

### Recording Rules

Prometheus automatically computes:
- `slo:api_availability:ratio_1h` - Hourly availability
- `slo:api_availability:ratio_30d` - Monthly availability
- `slo:api_latency:p95_1h` - Latency percentiles
- `slo:error_budget:remaining_percentage` - Error budget
- `slo:burn_rate:1h` - How fast we're burning error budget
- `sla:api_availability:ratio_monthly` - SLA metric
- `sla:api_availability:downtime_minutes_monthly` - Estimated downtime

## Integration Examples

### Exposing Metrics from Applications

**Python (using prometheus_client):**
```python
from prometheus_client import Counter, Histogram, Gauge

requests = Counter('http_requests_total', 'Total requests', ['method', 'status'])
latency = Histogram('http_request_duration_seconds', 'Request duration')
active = Gauge('active_connections', 'Active connections')

@latency.time()
def handle_request():
    requests.labels(method='GET', status=200).inc()
```

**Kubernetes Pod Annotation:**
```yaml
annotations:
  prometheus.io/scrape: "true"
  prometheus.io/port: "8080"
  prometheus.io/path: "/metrics"
```

### Custom Alert Query Examples

```promql
# Error rate spike
rate(http_requests_total{status=~"5.."}[5m]) > 0.05

# Latency degradation
histogram_quantile(0.95, http_request_duration_seconds_bucket) > 0.5

# Memory pressure
(container_memory_working_set_bytes / container_spec_memory_limit_bytes) > 0.9

# Disk space critical
(1 - (node_filesystem_avail_bytes / node_filesystem_size_bytes)) > 0.90

# Pod crash loop
rate(kube_pod_container_status_restarts_total[15m]) > 0.1
```

## Environment Variables

Required for production:
```bash
# Slack integration
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/..."

# PagerDuty integration
export PAGERDUTY_ROUTING_KEY="your-routing-key"
export PAGERDUTY_ROUTING_KEY_SLA="sla-routing-key"
export PAGERDUTY_ROUTING_KEY_SECURITY="security-routing-key"

# OpsGenie integration
export OPSGENIE_API_KEY="your-api-key"

# SMTP for email notifications
export SMTP_USERNAME="alerts@example.com"
export SMTP_PASSWORD="your-password"

# GCP Cloud Monitoring
export GCP_PROJECT_ID="your-project-id"
```

## Best Practices

### Alerting
1. **Alert on symptoms, not causes**
   - Good: "API latency > 500ms"
   - Bad: "CPU > 80%"

2. **Add runbooks**
   - Every critical alert should have a runbook
   - Example: `runbook: "https://runbooks.example.com/high-error-rate"`

3. **Avoid alert fatigue**
   - Set appropriate thresholds
   - Use `for:` clause (e.g., `for: 5m`)
   - Use inhibition rules

### Metrics
1. **Use consistent naming**: `job_action_unit`
   - Example: `http_request_duration_seconds`

2. **Label carefully**: Don't create high cardinality
   - Good: `{method="GET", status="200"}`
   - Bad: `{user_id="12345", session="abc123"}`

3. **Record heavy computations**: Use recording rules
   - Saves query performance
   - Pre-aggregates before alerting

### SLOs/SLAs
1. **Choose achievable targets**
   - 99.9% = 8.76h/month downtime
   - 99.95% = 21.6m/month downtime
   - 99.99% = 4.32s/month downtime

2. **Define error budget wisely**
   - Use to prioritize feature work
   - Stop feature work if budget exhausted

3. **Make SLAs public**
   - Customer trust depends on transparency
   - Clear compensation for breaches

## Production Checklist

- [ ] Configure persistent storage (Kubernetes PVCs or Docker volumes)
- [ ] Set up backup strategy (automated snapshots)
- [ ] Configure multi-region failover (if applicable)
- [ ] Verify alert routing to all channels
- [ ] Set resource limits and requests
- [ ] Enable SSL/TLS certificates
- [ ] Configure network policies
- [ ] Set up monitoring alerting (meta-monitoring)
- [ ] Create runbooks for all critical alerts
- [ ] Test alert routing and notifications
- [ ] Configure log retention
- [ ] Set up cost monitoring (GCP)

## Troubleshooting

See [SETUP.md - Troubleshooting](./SETUP.md#troubleshooting) for detailed troubleshooting guide.

Common issues:
1. **Alerts not firing**: Check Prometheus rule evaluation
2. **High memory usage**: Reduce retention or cardinality
3. **Missing metrics**: Verify scrape configs and targets
4. **Grafana datasource error**: Check Prometheus connectivity
5. **Alerts not routing**: Verify Alertmanager config and receivers

## Support & Resources

- **Prometheus Docs**: https://prometheus.io/docs/
- **Grafana Docs**: https://grafana.com/docs/
- **Alertmanager Docs**: https://prometheus.io/docs/alerting/
- **SRE Book (SLOs)**: https://sre.google/slo/
- **Kubernetes Monitoring**: https://kubernetes.io/docs/tasks/debug-application-cluster/

## License

This monitoring configuration is provided as-is for enterprise use.

## Contributing

To improve this monitoring setup:
1. Test changes locally with Docker Compose
2. Validate Prometheus YAML syntax
3. Test alert rules before production deployment
4. Document any custom metrics or alerts
5. Update SLO/SLA targets based on business needs

## Version History

- **v1.0** (2026-02) - Initial enterprise monitoring setup
  - Prometheus with SLO/SLA rules
  - Grafana dashboards
  - Alertmanager routing
  - GCP Cloud Monitoring integration
  - Kubernetes manifests
  - Docker Compose for local development
