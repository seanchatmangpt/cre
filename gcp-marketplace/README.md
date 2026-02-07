# GCP Marketplace Application - Enterprise Platform

Complete GCP Marketplace application manifest with deployer, RBAC, pricing, and billing integration for Fortune 5 enterprise deployments.

## Overview

This repository contains a production-ready GCP Marketplace application configuration featuring:

- **Enterprise-grade deployment** with high availability
- **Comprehensive RBAC** with fine-grained permissions
- **Advanced billing integration** with usage metering
- **Hybrid pricing model** (subscription + usage-based)
- **Multi-tier support** (Enterprise, Professional, Starter)
- **Compliance-ready** (SOC2, HIPAA, ISO27001)
- **Auto-scaling** with custom metrics
- **99.99% SLA** support

## Architecture

```
gcp-marketplace/
├── deployer/               # Deployment container
│   ├── Dockerfile         # Deployer image definition
│   └── deploy.sh          # Deployment orchestration script
├── manifest/              # Application manifests
│   ├── application.yaml   # Main application descriptor
│   └── schema.yaml        # UI schema for deployment form
├── rbac/                  # RBAC configuration
│   ├── service-account.yaml
│   ├── cluster-role.yaml
│   ├── role.yaml
│   └── role-binding.yaml
├── billing/               # Billing integration
│   └── usage-metering-agent.yaml
├── k8s/                   # Kubernetes resources
│   ├── deployment.yaml
│   ├── service.yaml
│   └── hpa.yaml
├── pricing.yaml           # Pricing configuration
└── README.md
```

## Quick Start

### Build Deployer Image

```bash
cd gcp-marketplace
docker build -t gcr.io/YOUR_PROJECT/deployer:1.0.0 -f deployer/Dockerfile .
docker push gcr.io/YOUR_PROJECT/deployer:1.0.0
```

### Deploy

```bash
kubectl apply -f manifest/application.yaml
```

## Pricing Model

### Enterprise Plan ($5,000/month base)

**Included:**
- 3 application instances
- 24/7 Premium Support
- 99.99% SLA
- Compliance packs (SOC2, HIPAA, ISO27001)

**Usage-based:**
- Additional instances: $2.00-2.50/hour
- vCPU: $0.03-0.05/vcpu-hour
- Memory: $0.006-0.01/GB-hour
- Storage: $0.001/GB-hour (1TB included)
- API requests: $0.00005/request (1M free)

## Billing Metrics

| Metric | Description | Unit | Interval |
|--------|-------------|------|----------|
| instance_time | Active instance hours | hours | 5 min |
| cpu_usage | vCPU consumption | vcpu-hours | 1 min |
| memory_usage | Memory consumption | GB-hours | 1 min |
| storage_usage | Persistent storage | GB-hours | 1 hour |
| api_requests | API call count | requests | 1 min |

## RBAC Configuration

### Service Accounts
- **App Service Account** - Application runtime
- **Metering Service Account** - Metrics collection
- **Admin Service Account** - Administrative operations
- **Backup Service Account** - Backup operations

### Roles
- **Application Role** - Manage deployments, services, configs
- **Admin Role** - Full namespace access
- **Metering ClusterRole** - Read-only cluster metrics

## Enterprise Features

- **High Availability**: Multi-replica deployment (default: 3)
- **Auto-scaling**: CPU/memory-based scaling (3-20 pods)
- **Security**: Network policies, pod security, encryption
- **Compliance**: SOC2, HIPAA, PCI-DSS modes
- **Monitoring**: Prometheus, Grafana integration
- **Backup & DR**: Automated backups, 30-day retention

## Support

**Enterprise Support (24/7)**
- Email: enterprise-support@yourcompany.com
- Phone: +1-800-XXX-XXXX
- Response time: 15 minutes (critical)

## License

Enterprise Commercial License
Copyright © 2024 Your Company Name
