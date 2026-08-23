# CRE - Common Runtime Environment for Google Cloud Marketplace

## Overview

CRE (Common Runtime Environment) is a production-grade workflow engine implementing the YAWL (Yet Another Workflow Language) specification with 36 workflow patterns, built on Erlang/OTP for high reliability and fault tolerance.

## Quick Start

### Prerequisites

- GKE cluster version 1.25 or higher
- kubectl configured to access your cluster
- At least 3 nodes available (for 3-node CRE cluster)

### Deployment

1. **Deploy from Marketplace:**
   - Navigate to CRE listing on Google Cloud Marketplace
   - Click "Get Started"
   - Configure deployment parameters (node count, storage, resources)
   - Accept Apache License 2.0
   - Click "Deploy"

2. **Verify Deployment:**
   ```bash
   kubectl get pods -n cre
   kubectl get svc -n cre
   ```

3. **Access CRE:**
   ```bash
   kubectl port-forward -n cre svc/cre 4142:4142
   curl http://localhost:4142/health
   ```

## Configuration

### Scaling

CRE supports horizontal pod autoscaling:

```yaml
autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 80
```

### Persistence

CRE uses persistent volumes for Mnesia database:

```yaml
persistence:
  enabled: true
  size: 10Gi
  storageClass: premium-rwo
```

### Resource Limits

Default resource configuration:

```yaml
resources:
  requests:
    cpu: 500m
    memory: 512Mi
  limits:
    cpu: 2000m
    memory: 2Gi
```

## Monitoring

CRE exposes metrics at `/status.json`:

```bash
kubectl port-forward -n cre svc/cre 4142:4142
curl http://localhost:4142/status.json
```

Health checks:
- Liveness: `/health`
- Readiness: `/ready`
- Startup: `/startup`

## Architecture

CRE runs as a StatefulSet with:
- **3+ nodes** for Mnesia clustering
- **Headless service** for cluster communication
- **Pod Disruption Budget** for high availability
- **Horizontal Pod Autoscaler** (optional)

## Support

- Documentation: https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md
- Issues: https://github.com/joergen7/cre/issues
- License: Apache License 2.0

## License

CRE is licensed under the Apache License 2.0. By deploying CRE from Google Cloud Marketplace, you agree to the terms of this license.

See [LICENSE](https://github.com/joergen7/cre/blob/main/LICENSE) for details.
