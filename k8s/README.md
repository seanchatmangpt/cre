# Kubernetes Manifests for CRE

This directory contains Kubernetes manifests for deploying the CRE YAWL Workflow Engine.

## Directory Structure

```
k8s/
├── namespace.yaml       # Namespace definitions
├── configmap.yaml       # Configuration maps
├── secret.yaml          # Secret templates (DO NOT commit actual secrets)
├── deployment.yaml      # Deployment definitions
├── service.yaml         # Service definitions
├── ingress.yaml         # Ingress rules
├── hpa.yaml             # Horizontal Pod Autoscaler
├── pvc.yaml             # Persistent Volume Claims
├── pdb.yaml             # Pod Disruption Budgets
├── networkpolicy.yaml   # Network policies
└── README.md            # This file
```

## Quick Start

### Prerequisites

- Kubernetes cluster (v1.24+)
- `kubectl` configured
- Container registry credentials

### Initial Setup

1. **Create namespaces:**
   ```bash
   kubectl apply -f namespace.yaml
   ```

2. **Create secrets:**
   ```bash
   # Generate secure values
   export ERLANG_COOKIE=$(openssl rand -base64 32)
   export DB_PASSWORD=$(openssl rand -base64 16)
   export API_KEY=$(openssl rand -hex 16)

   # Create production secrets
   kubectl create secret generic cre-secrets \
     --from-literal=erlang-cookie="$ERLANG_COOKIE" \
     --from-literal=db-password="$DB_PASSWORD" \
     --from-literal=api-key="$API_KEY" \
     -n cre-prod

   # Create staging secrets
   kubectl create secret generic cre-secrets \
     --from-literal=erlang-cookie="$ERLANG_COOKIE-staging" \
     --from-literal=db-password="$DB_PASSWORD-staging" \
     --from-literal=api-key="$API_KEY-staging" \
     -n cre-staging
   ```

3. **Apply all manifests:**
   ```bash
   # Production
   kubectl apply -f configmap.yaml
   kubectl apply -f deployment.yaml
   kubectl apply -f service.yaml
   kubectl apply -f ingress.yaml
   kubectl apply -f hpa.yaml
   kubectl apply -f pvc.yaml
   kubectl apply -f pdb.yaml
   kubectl apply -f networkpolicy.yaml
   ```

### Staging Environment

For staging, replace the namespace with `-n cre-staging` in all commands, or use:
```bash
kubectl apply -k k8s/overlays/staging/
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CRE_DEFAULT_PORT` | HTTP port | 4142 |
| `CRE_LOG_LEVEL` | Log level | info |
| `YAWL_STATELESS_CHECKPOINT_DIR` | Checkpoint directory | /opt/cre/checkpoints |
| `YAWL_STATELESS_MAX_EXECUTIONS` | Max concurrent executions | 1000 |
| `YAWL_TIMEOUT_DEFAULT_TIMEOUT` | Default timeout (ms) | 30000 |

### Resource Limits

Production:
- CPU: 500m - 2000m
- Memory: 1Gi - 4Gi

Staging:
- CPU: 250m - 1000m
- Memory: 512Mi - 2Gi

## Scaling

### Manual Scaling

```bash
kubectl scale deployment cre --replicas=5 -n cre-prod
```

### Autoscaling

The HPA is configured with:
- Min replicas: 3 (prod), 2 (staging)
- Max replicas: 10 (prod), 5 (staging)
- Target CPU: 70%
- Target memory: 80%

## Monitoring

### Check Pod Status

```bash
kubectl get pods -n cre-prod
kubectl logs -f deployment/cre -n cre-prod
```

### Port Forward for Local Access

```bash
kubectl port-forward svc/cre 4142:4142 -n cre-prod
```

## Health Checks

- **Liveness Probe:** `/status.json` every 10s
- **Readiness Probe:** `/status.json` every 5s
- **Startup Probe:** `/status.json` every 5s for 150s

## Backup and Restore

### Backup Mnesia Data

```bash
kubectl exec -it deployment/cre -n cre-prod -- \
  /opt/cre/bin/cre eval "mnesia:backup('/opt/cre/checkpoints/backup.')"
```

### Restore from Backup

```bash
kubectl exec -it deployment/cre -n cre-prod -- \
  /opt/cre/bin/cre eval "mnesia:restore('/opt/cre/checkpoints/backup.', [])"
```

## Troubleshooting

### Check Events

```bash
kubectl get events -n cre-prod --sort-by='.lastTimestamp'
```

### Describe Pod

```bash
kubectl describe pod -l app=cre -n cre-prod
```

### Exec into Pod

```bash
kubectl exec -it deployment/cre -n cre-prod -- /bin/sh
```

## Security Notes

1. **Never commit actual secrets** to version control
2. Use separate secrets for staging and production
3. Rotate Erlang cookie regularly
4. Enable RBAC for restricted access
5. Use NetworkPolicies to limit traffic
