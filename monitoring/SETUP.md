# Enterprise Cloud Monitoring Setup Guide

Complete guide for setting up cloud monitoring, custom dashboards, SLO/SLA monitoring, alerting policies, and Prometheus/Grafana integration.

## Overview

This monitoring solution includes:

- **Prometheus**: Time-series database with alerting engine
- **Grafana**: Data visualization and dashboarding
- **Alertmanager**: Alert routing, grouping, and deduplication
- **Node Exporter**: System metrics collection
- **cAdvisor**: Container metrics collection
- **VictoriaMetrics**: Long-term metric storage (optional)
- **Loki**: Log aggregation (optional)
- **Jaeger**: Distributed tracing (optional)
- **GCP Cloud Monitoring**: Cloud-native monitoring integration

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Metric Sources                             │
├─────────────────────────────────────────────────────────────┤
│ Prometheus │ Node Exporter │ cAdvisor │ Custom Apps          │
│ kube-state-metrics │ Istio │ GCP Metrics                    │
└────────────────────┬────────────────────────────────────────┘
                     │ (scrape)
┌────────────────────▼────────────────────────────────────────┐
│         Prometheus + Alert Rules                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ SLO Rules │ SLA Rules │ Alert Rules                  │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────┬────────────────────────────────────────┘
                     │ (store) │ (evaluate)
        ┌────────────┴──────────┬──────────────┐
        │                       │              │
        ▼                       ▼              ▼
   ┌─────────┐         ┌──────────────┐  ┌──────────┐
   │Grafana  │         │ Alertmanager │  │ External │
   │Dashboard│         │ (route alerts)  │ Storage  │
   └─────────┘         └──────────────┘  └──────────┘
        │                       │
        │                  ┌────┴────────────┐
        │                  │                 │
        ▼                  ▼                 ▼
    ┌──────────┐    ┌─────────────┐  ┌──────────────┐
    │Web UI    │    │Slack/Email/ │  │PagerDuty/    │
    │(3000)    │    │Webhook      │  │OpsGenie      │
    └──────────┘    └─────────────┘  └──────────────┘
```

## Quick Start

### Local Development (Docker Compose)

```bash
# Navigate to monitoring directory
cd /home/user/cre/monitoring

# Start the monitoring stack
docker-compose up -d

# Or with optional logging and tracing
docker-compose --profile logging --profile tracing up -d

# Check status
docker-compose ps

# View logs
docker-compose logs -f prometheus
docker-compose logs -f grafana
docker-compose logs -f alertmanager
```

**Access:**
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3000 (admin/admin)
- Alertmanager: http://localhost:9093
- Node Exporter: http://localhost:9100/metrics

### Kubernetes Deployment

```bash
# Create monitoring namespace and RBAC
kubectl apply -f monitoring/kubernetes/monitoring-namespace.yaml

# Deploy Prometheus
kubectl apply -f monitoring/kubernetes/prometheus-deployment.yaml

# Deploy Alertmanager
kubectl apply -f monitoring/kubernetes/alertmanager-deployment.yaml

# Deploy Grafana (using Helm recommended)
helm repo add grafana https://grafana.github.io/helm-charts
helm install grafana grafana/grafana --namespace monitoring

# Verify deployment
kubectl get all -n monitoring
kubectl logs -f deployment/alertmanager -n monitoring
```

## Configuration

### 1. Prometheus Configuration

**Location:** `prometheus/prometheus.yml`

Key configurations:
- **Global settings**: Scrape interval, evaluation interval, external labels
- **Scrape configs**: Data source definitions (Kubernetes, GCP, applications)
- **Alert rules**: Alert definitions with thresholds
- **Remote write**: Long-term storage configuration

**Updating scrape configs:**

```yaml
# Add new scrape target
- job_name: 'my-application'
  static_configs:
    - targets: ['localhost:8080']
  scrape_interval: 15s
  scrape_timeout: 10s
```

**Reload configuration without downtime:**

```bash
# For Docker Compose
curl -X POST http://localhost:9090/-/reload

# For Kubernetes
kubectl rollout restart statefulset/prometheus -n monitoring
```

### 2. Alert Rules

**Location:** `prometheus/rules/`

- `alerts.yml` - System, Kubernetes, application, network alerts
- `slo.yml` - Service Level Objective definitions and alerts
- `sla.yml` - Service Level Agreement definitions and breaches

**Adding new alert:**

```yaml
- alert: MyCustomAlert
  expr: 'metric_name > threshold'
  for: 5m
  labels:
    severity: warning
    component: myservice
  annotations:
    summary: "Alert summary"
    description: "Alert description"
    runbook: "https://runbooks.example.com/my-alert"
```

### 3. SLO/SLA Definitions

**SLO Examples:**
- API Availability: 99.9% (8.76 hours/month downtime allowed)
- API Latency: 95% of requests < 200ms
- Database Availability: 99.95% (2.16 minutes/month downtime)
- Cache Hit Rate: 95% minimum
- Queue Processing: 99% within 5 minutes

**SLA Examples:**
- API Availability: 99.95% (21.6 minutes/month downtime)
- Database Availability: 99.99% (4.32 seconds/month downtime)
- Message Delivery: 99.9% success rate
- Data Durability: 99.999999% (11.6 seconds/year)
- Support Response: Critical < 1 hour

**Customizing SLOs:**

Edit `prometheus/rules/slo.yml`:

```yaml
# Change SLO target
- record: slo:api_availability:ratio_1h
  expr: |
    (
      sum(rate(http_requests_total{status!~"5.."}[1h]))
      /
      sum(rate(http_requests_total[1h]))
    ) * 100
```

### 4. Alertmanager Configuration

**Location:** `alertmanager/alertmanager.yml`

Key configurations:
- **Global settings**: Resolution timeout, notification channels
- **Routing**: Alert routing rules with group settings
- **Receivers**: Notification channels (Slack, PagerDuty, email, etc.)
- **Inhibition rules**: Alert suppression conditions

**Setting up Slack integration:**

```bash
# Get webhook URL from Slack (Incoming Webhooks)
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"

# Update alertmanager config with webhook
```

**Setting up PagerDuty:**

```bash
export PAGERDUTY_ROUTING_KEY="your-routing-key"
export PAGERDUTY_ROUTING_KEY_SLA="your-sla-routing-key"
```

### 5. Grafana Dashboards

**Location:** `grafana/dashboards/`

Pre-built dashboards:
- `system-overview.json` - System metrics (CPU, memory, disk, network)
- `application-metrics.json` - Application performance metrics
- `slo-sla-dashboard.json` - SLO/SLA tracking and error budget

**Importing dashboards:**

Option 1: Automatic (via provisioning)
```bash
# Dashboards auto-import from provisioning directory
# No action needed
```

Option 2: Manual
1. Open Grafana: http://localhost:3000
2. Go to Dashboard → Import
3. Upload JSON file or paste JSON content
4. Select Prometheus datasource

**Creating custom dashboards:**

1. Open Grafana
2. Create → Dashboard
3. Add panels with Prometheus queries
4. Example query: `rate(http_requests_total[5m])`
5. Save and export as JSON

### 6. GCP Cloud Monitoring Integration

**Location:** `gcp-cloud-monitoring/gcp-monitoring.yaml`

**Setup:**

1. Enable GCP monitoring APIs:
```bash
gcloud services enable monitoring.googleapis.com \
  logging.googleapis.com \
  cloudtrace.googleapis.com
```

2. Create service account:
```bash
gcloud iam service-accounts create prometheus-sa \
  --display-name="Prometheus Cloud Monitoring"

gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:prometheus-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/monitoring.metricWriter"
```

3. Configure Prometheus remote write:
```yaml
remote_write:
  - url: 'https://monitoring.googleapis.com/openmetrics.googleapis.com/v1/projects/YOUR_PROJECT_ID/location/global/prometheus/api/v1/write'
    bearer_token_file: /var/run/secrets/cloud.google.com/service_account/token
```

## Monitoring Your Applications

### Exposing Metrics

Applications should expose metrics in Prometheus format at `/metrics`:

```python
from prometheus_client import Counter, Histogram, Gauge
import time

# Counter
request_count = Counter('http_requests_total', 'Total HTTP requests', ['method', 'status'])

# Histogram
request_duration = Histogram('http_request_duration_seconds', 'HTTP request duration')

# Gauge
active_connections = Gauge('active_connections', 'Active connections')

@request_duration.time()
def handle_request():
    # Your logic
    pass
```

### Kubernetes Pod Annotation

For automatic scraping in Kubernetes:

```yaml
apiVersion: v1
kind: Pod
metadata:
  annotations:
    prometheus.io/scrape: "true"
    prometheus.io/port: "8080"
    prometheus.io/path: "/metrics"
spec:
  containers:
  - name: myapp
    ports:
    - containerPort: 8080
```

### Custom Metrics Examples

```bash
# Request rate
rate(http_requests_total[5m])

# Error rate
rate(http_requests_total{status=~"5.."}[5m])

# Latency percentiles
histogram_quantile(0.95, http_request_duration_seconds_bucket)

# Memory usage
container_memory_working_set_bytes / container_spec_memory_limit_bytes

# Disk usage
(1 - (node_filesystem_avail_bytes / node_filesystem_size_bytes)) * 100
```

## Alerting Best Practices

### Alert Severity Levels

- **Critical**: Immediate action required, customer impact
- **Warning**: Should be addressed soon, potential customer impact
- **Info**: Informational, no immediate action needed

### Alert Examples

**Application Alert:**
```yaml
- alert: HighErrorRate
  expr: 'rate(http_requests_total{status=~"5.."}[5m]) > 0.05'
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "Error rate above 5%"
    runbook: "https://runbooks.example.com/high-error-rate"
```

**SLO Alert:**
```yaml
- alert: SLOApiAvailabilityBreach
  expr: 'slo:api_availability:ratio_1h < 99.9'
  for: 5m
  labels:
    severity: critical
    slo: api_availability
  annotations:
    summary: "API Availability SLO breach"
```

**Resource Alert:**
```yaml
- alert: HighMemoryUsage
  expr: '(1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) > 0.85'
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "Memory usage above 85%"
```

## Troubleshooting

### Prometheus

**Alerts not firing:**
```bash
# Check if alerts are loaded
curl http://localhost:9090/api/v1/rules

# Check for errors in logs
docker logs prometheus
# or
kubectl logs -f deployment/prometheus -n monitoring
```

**High memory usage:**
```bash
# Reduce retention time
--storage.tsdb.retention.time=15d  # default 30d

# Reduce metric cardinality
# - Limit label combinations
# - Use metric_relabel_configs to drop unnecessary metrics
```

**Slow queries:**
```bash
# Enable query logging
--query.log-file=/var/log/prometheus-queries.log

# Monitor query performance
curl 'http://localhost:9090/api/v1/query?query=rate(http_requests_total[5m])'
```

### Grafana

**Datasource not connecting:**
```bash
# Verify Prometheus is accessible
curl http://prometheus:9090/-/healthy

# Check network connectivity
docker exec grafana ping prometheus  # or kubectl exec
```

**Dashboards not loading:**
```bash
# Check provisioning logs
docker logs grafana
# or
kubectl logs -f deployment/grafana -n monitoring

# Verify dashboard JSON syntax
jq . monitoring/grafana/dashboards/*.json
```

### Alertmanager

**Alerts not being routed:**
```bash
# Check configuration
curl http://localhost:9093/api/v1/alerts

# Verify routing rules
curl http://localhost:9093/api/v1/status

# Test Slack webhook (if using Slack)
curl -X POST $SLACK_WEBHOOK_URL \
  -H 'Content-Type: application/json' \
  -d '{"text":"Test message"}'
```

## Maintenance

### Backup

**Prometheus Data:**
```bash
# Docker
docker exec prometheus tar czf prometheus_backup.tar.gz /prometheus

# Kubernetes
kubectl exec -n monitoring prometheus-0 -- \
  tar czf /prometheus_backup.tar.gz /prometheus
```

**Grafana:**
```bash
# Export all dashboards
for id in $(curl -s http://admin:admin@localhost:3000/api/search | jq -r '.[].id'); do
  curl -s http://admin:admin@localhost:3000/api/dashboards/id/$id > dashboard_$id.json
done
```

### Cleanup

**Remove old metrics:**
```bash
# In prometheus.yml, adjust retention
--storage.tsdb.retention.time=30d  # older data auto-deleted
```

**Delete specific metrics:**
```bash
# Use admin API
curl -X DELETE 'http://localhost:9090/api/v1/admin/tsdb/delete_series?match[]=metric_name{}'
```

## Performance Tuning

### Prometheus

```bash
# Increase memory for large deployments
--storage.tsdb.max-block-duration=2h
--storage.tsdb.min-block-duration=2h
--query.max-samples=100000000

# Enable compression
--storage.tsdb.encoding=snappy
```

### Grafana

```bash
# Optimize rendering
GF_RENDERING_SERVER_URL=http://renderer:8081

# Increase row limit
GF_PANELS_PLUGIN_ADMIN_EXTERNAL_MANAGE_ENABLED=true
```

## Security

### Network Policies

```yaml
# Restrict traffic to monitoring namespace only
kind: NetworkPolicy
metadata:
  name: monitoring-restrict
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: monitoring
```

### Authentication

```bash
# Enable Grafana authentication
GF_SECURITY_ADMIN_PASSWORD=strongpassword
GF_SECURITY_ALLOW_EMBED_FRAMES=false
GF_SECURITY_STRICT_TRANSPORT_SECURITY=true
```

### SSL/TLS

```yaml
# For Kubernetes, use cert-manager
apiVersion: cert-manager.io/v1
kind: Certificate
metadata:
  name: prometheus-cert
spec:
  secretName: prometheus-tls
  dnsNames:
  - prometheus.example.com
```

## Additional Resources

- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
- [Alertmanager Documentation](https://prometheus.io/docs/alerting/latest/overview/)
- [SLO Guide](https://sre.google/slo/)
- [Kubernetes Monitoring](https://kubernetes.io/docs/tasks/debug-application-cluster/resource-metrics-pipeline/)

## Support

For issues or questions:
1. Check logs: `docker logs <service>` or `kubectl logs <pod> -n monitoring`
2. Review configuration files
3. Test queries in Prometheus UI
4. Verify connectivity between services
5. Check resource limits and availability
