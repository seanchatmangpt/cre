# GCP Marketplace Deployment Guide

## Complete File Structure

```
gcp-marketplace/
├── deployer/
│   ├── Dockerfile                      # Deployer container image
│   └── deploy.sh                       # Orchestration script (executable)
├── manifest/
│   ├── application.yaml                # Main app descriptor with metadata
│   └── schema.yaml                     # Deployment UI form configuration
├── rbac/
│   ├── service-account.yaml            # 4 service accounts (app, metering, admin, backup)
│   ├── cluster-role.yaml               # Cluster-wide roles
│   ├── role.yaml                       # Namespace-scoped roles (app, admin, backup)
│   └── role-binding.yaml               # Role bindings
├── billing/
│   └── usage-metering-agent.yaml       # Metering agent + ConfigMaps
├── k8s/
│   ├── deployment.yaml                 # Application deployment
│   ├── service.yaml                    # Services (API, admin, metrics)
│   └── hpa.yaml                        # Horizontal Pod Autoscaler
├── pricing.yaml                        # Complete pricing configuration
├── README.md                           # Main documentation
└── DEPLOYMENT_GUIDE.md                 # This file
```

## Step-by-Step Deployment

### 1. Prerequisites

```bash
# Install required tools
gcloud components install kubectl

# Set your GCP project
export PROJECT_ID="your-project-id"
gcloud config set project $PROJECT_ID

# Enable required APIs
gcloud services enable \
  container.googleapis.com \
  cloudapis.googleapis.com \
  servicecontrol.googleapis.com
```

### 2. Build Deployer Image

```bash
cd /home/user/cre/gcp-marketplace

# Build the deployer container
docker build -t gcr.io/${PROJECT_ID}/enterprise-app-deployer:1.0.0 \
  -f deployer/Dockerfile .

# Push to Google Container Registry
docker push gcr.io/${PROJECT_ID}/enterprise-app-deployer:1.0.0
```

### 3. Configure Partner Portal

1. **Access Partner Portal**
   - Go to: https://console.cloud.google.com/partner/marketplace
   - Sign in with your partner account

2. **Create New Product**
   - Click "Create Product"
   - Select "Kubernetes Application"
   - Upload deployer image: `gcr.io/${PROJECT_ID}/enterprise-app-deployer:1.0.0`

3. **Upload Manifests**
   - Upload `manifest/application.yaml`
   - Upload `manifest/schema.yaml`
   - Configure pricing from `pricing.yaml`

4. **Configure Billing**
   - Set up Service Account for billing
   - Configure usage metrics from `billing/usage-metering-agent.yaml`
   - Map metrics to pricing tiers

### 4. Test Deployment Locally

```bash
# Create test namespace
kubectl create namespace test-app

# Set environment variables
export NAME="test-enterprise-app"
export NAMESPACE="test-app"
export DEPLOYER_SERVICE_ACCOUNT="deployer-sa"
export APP_SERVICE_ACCOUNT="app-sa"
export METERING_SERVICE_ACCOUNT="metering-sa"
export REPORTING_SECRET="test-reporting-secret"

# Deploy RBAC
for file in rbac/*.yaml; do
  envsubst < "$file" | kubectl apply -f -
done

# Deploy application
envsubst < k8s/deployment.yaml | kubectl apply -f -
envsubst < k8s/service.yaml | kubectl apply -f -
envsubst < k8s/hpa.yaml | kubectl apply -f -

# Deploy billing agent
envsubst < billing/usage-metering-agent.yaml | kubectl apply -f -

# Verify deployment
kubectl get pods -n test-app
kubectl get svc -n test-app
```

### 5. Validation Checklist

- [ ] Deployer image builds successfully
- [ ] All manifests pass `kubectl apply --dry-run`
- [ ] RBAC permissions are correctly configured
- [ ] Service accounts have minimal required permissions
- [ ] Billing agent deploys and reports metrics
- [ ] Application pods start and pass health checks
- [ ] Services expose correct ports
- [ ] HPA scales based on load
- [ ] Network policies enforce security
- [ ] Backup service account can access volumes

### 6. Partner Portal Submission

1. **Complete Product Information**
   - Product name: "Enterprise Application Platform"
   - Description from README.md
   - Category: "Infrastructure & Operations"
   - Pricing model: "Hybrid (Subscription + Usage)"

2. **Configure Pricing**
   - Base price: $5,000/month
   - Usage metrics: instance_time, cpu_usage, memory_usage, storage_usage, api_requests
   - Tiered pricing from `pricing.yaml`

3. **Set Support Tiers**
   - Enterprise: 24/7, 15-min response
   - Professional: 8x5, 1-hour response
   - Starter: Community support

4. **Compliance & Certifications**
   - Upload SOC2 compliance documentation
   - Upload HIPAA compliance documentation
   - Upload ISO27001 certification

5. **Submit for Review**
   - Complete technical review
   - Submit for marketplace approval

## Configuration Options

### schema.yaml Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `name` | string | - | Application instance name |
| `namespace` | string | - | Kubernetes namespace |
| `replicaCount` | int | 3 | Number of replicas |
| `resources.requests.cpu` | string | 2000m | CPU request |
| `resources.requests.memory` | string | 4Gi | Memory request |
| `persistence.enabled` | bool | true | Enable persistence |
| `persistence.size` | string | 100Gi | Storage size |
| `autoscaling.enabled` | bool | true | Enable auto-scaling |
| `autoscaling.minReplicas` | int | 3 | Min replicas |
| `autoscaling.maxReplicas` | int | 20 | Max replicas |
| `sso.enabled` | bool | false | Enable SSO |
| `monitoring.enabled` | bool | true | Enable monitoring |

### Environment Variables

Deploy script (`deployer/deploy.sh`) uses:

- `NAME` - Application name
- `NAMESPACE` - Target namespace
- `DEPLOYER_SERVICE_ACCOUNT` - Deployer SA
- `APP_SERVICE_ACCOUNT` - Application SA
- `METERING_SERVICE_ACCOUNT` - Metering SA
- `REPORTING_SECRET` - Billing secret
- `REPLICA_COUNT` - Number of replicas (default: 3)
- `MONITORING_ENABLED` - Enable monitoring (default: true)
- `SECURITY_NETWORK_POLICIES` - Enable network policies (default: true)

## Billing Integration

### Metrics Tracked

1. **instance_time** (int, hours)
   - Tracks active instance hours
   - Reported every 5 minutes
   - Pricing: $2.00-2.50/hour (tiered)

2. **cpu_usage** (double, vcpu-hours)
   - Tracks vCPU consumption
   - Reported every 1 minute
   - Pricing: $0.03-0.05/vcpu-hour (tiered)

3. **memory_usage** (double, GB-hours)
   - Tracks memory consumption
   - Reported every 1 minute
   - Pricing: $0.006-0.01/GB-hour (tiered)

4. **storage_usage** (double, GB-hours)
   - Tracks persistent volume usage
   - Reported every 1 hour
   - Pricing: $0.001/GB-hour (1TB included)

5. **api_requests** (int, requests)
   - Tracks API calls
   - Reported every 1 minute
   - Pricing: $0.00005/request (1M free)

### Metering Agent

The usage metering agent (`ubbagent`) runs as a sidecar:
- Collects metrics from Kubernetes metrics API
- Aggregates and buffers metrics locally
- Reports to GCP Service Control API
- Handles retry and failure scenarios
- Maintains local state for reliability

## Security Considerations

### RBAC Best Practices

1. **Principle of Least Privilege**
   - Each service account has minimal required permissions
   - Namespace-scoped roles where possible
   - ClusterRole only for read-only cluster metrics

2. **Service Account Isolation**
   - Separate SAs for app, metering, admin, backup
   - No shared credentials
   - Token auto-mounting controlled

3. **Network Policies**
   - Deny-all default policy
   - Allow only required ingress/egress
   - Namespace isolation

### Pod Security

1. **Security Context**
   - Non-root user (UID 1000)
   - Read-only root filesystem
   - No privilege escalation
   - Seccomp profile enabled

2. **Resource Limits**
   - CPU/memory requests and limits
   - Prevents resource exhaustion
   - Enables proper scheduling

## Monitoring & Observability

### Prometheus Metrics

Application exposes metrics at `:9090/metrics`:
- `app_requests_total` - Total requests
- `app_requests_duration_seconds` - Request latency
- `app_errors_total` - Error count
- `app_active_connections` - Active connections

### Health Checks

1. **Liveness Probe** (`/health/live`)
   - Checks if application is alive
   - Failures trigger restart

2. **Readiness Probe** (`/health/ready`)
   - Checks if application can serve traffic
   - Failures remove from load balancer

3. **Startup Probe** (`/health/startup`)
   - Checks initial startup
   - Allows slow-starting apps

### Grafana Dashboards

Pre-configured dashboards available:
- Application performance metrics
- Resource utilization
- Business KPIs
- Billing metrics
- SLA compliance

## Troubleshooting

### Common Issues

**Deployment fails with RBAC errors**
```bash
# Check service account permissions
kubectl auth can-i --list --as=system:serviceaccount:${NAMESPACE}:${APP_SERVICE_ACCOUNT}

# Verify role bindings
kubectl get rolebindings -n ${NAMESPACE}
kubectl get clusterrolebindings | grep ${NAME}
```

**Billing agent not reporting**
```bash
# Check agent logs
kubectl logs -l app=usage-metering-agent -n ${NAMESPACE}

# Verify reporting secret
kubectl get secret ${REPORTING_SECRET} -n ${NAMESPACE} -o yaml

# Check ConfigMap
kubectl get cm ${NAME}-ubbagent-config -n ${NAMESPACE} -o yaml
```

**Pods not starting**
```bash
# Check pod events
kubectl describe pod -l app.kubernetes.io/name=${NAME} -n ${NAMESPACE}

# Check resource availability
kubectl top nodes
kubectl describe nodes

# Check image pull
kubectl get events -n ${NAMESPACE} | grep Failed
```

**Auto-scaling not working**
```bash
# Check HPA status
kubectl get hpa -n ${NAMESPACE}
kubectl describe hpa ${NAME}-hpa -n ${NAMESPACE}

# Verify metrics server
kubectl top pods -n ${NAMESPACE}

# Check target metrics
kubectl get --raw /apis/metrics.k8s.io/v1beta1/namespaces/${NAMESPACE}/pods
```

## Support & Resources

- **Documentation**: https://docs.yourcompany.com
- **API Reference**: https://docs.yourcompany.com/api
- **Support Portal**: https://support.yourcompany.com
- **GCP Marketplace**: https://console.cloud.google.com/marketplace

## Version History

### 1.0.0 (2024-01-15)
- Initial release
- Enterprise-grade deployment
- Hybrid pricing model
- Advanced RBAC
- Usage metering integration
- Auto-scaling support
- Compliance features

---

**Next Steps:**
1. Customize `application.yaml` with your product details
2. Update `schema.yaml` with your configuration options
3. Modify `pricing.yaml` with your pricing model
4. Build and test deployer image
5. Submit to GCP Marketplace Partner Portal
