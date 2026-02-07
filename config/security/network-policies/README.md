# Network Policies for CRE Platform

## Overview

This directory contains Kubernetes NetworkPolicy resources implementing zero-trust networking for the CRE platform.

## Policy Architecture

```
┌─────────────────────────────────────────────┐
│     Default Deny-All (Baseline)             │
├─────────────────────────────────────────────┤
│     Essential Services (DNS, Metadata)      │
├─────────────────────────────────────────────┤
│     Application-Specific Allow Rules        │
├─────────────────────────────────────────────┤
│     Namespace Isolation                     │
├─────────────────────────────────────────────┤
│     External Egress Controls                │
└─────────────────────────────────────────────┘
```

## Policy Files

| File | Purpose | Priority |
|------|---------|----------|
| `00-default-deny-all.yaml` | Deny all ingress/egress by default | **Critical** |
| `10-allow-dns.yaml` | Allow DNS resolution | High |
| `20-cre-app-policies.yaml` | Application-specific rules | High |
| `30-namespace-isolation.yaml` | Prevent cross-namespace traffic | High |
| `40-external-egress.yaml` | Control external access | Medium |

## Deployment Order

**IMPORTANT**: Apply policies in numerical order to avoid breaking connectivity.

```bash
# 1. Label namespaces first
kubectl label namespace cre-system monitoring-enabled=true
kubectl label namespace monitoring monitoring-enabled=true

# 2. Apply policies in order
kubectl apply -f 00-default-deny-all.yaml
kubectl apply -f 10-allow-dns.yaml
kubectl apply -f 20-cre-app-policies.yaml
kubectl apply -f 30-namespace-isolation.yaml
kubectl apply -f 40-external-egress.yaml

# 3. Verify no disruption
kubectl get networkpolicies --all-namespaces
```

## Testing Network Policies

### Test DNS Resolution

```bash
kubectl run -it --rm test-dns \
  --image=busybox \
  --namespace=cre-system \
  --restart=Never \
  -- nslookup kubernetes.default.svc.cluster.local
```

**Expected**: DNS resolution succeeds

### Test Cross-Namespace Communication

```bash
# Should FAIL - cross-namespace traffic is blocked by default
kubectl run -it --rm test-cross-ns \
  --image=busybox \
  --namespace=cre-system \
  --restart=Never \
  -- wget -O- http://prometheus-server.monitoring.svc.cluster.local
```

**Expected**: Connection timeout or refused

### Test External HTTPS Access

```bash
# Should SUCCEED for pods with allow-external-egress label
kubectl run -it --rm test-external \
  --image=curlimages/curl \
  --namespace=cre-system \
  --labels="app=cre-app,allow-external-egress=true" \
  --restart=Never \
  -- curl -I https://www.google.com
```

**Expected**: HTTP 200 response

### Test Internal Communication

```bash
# Test pod-to-pod communication within namespace
kubectl run -it --rm test-internal \
  --image=busybox \
  --namespace=cre-system \
  --restart=Never \
  -- wget -O- http://cre-app-service:8080/health
```

**Expected**: Health check succeeds

## Monitoring Network Policy Enforcement

### View Applied Policies

```bash
# List all network policies
kubectl get networkpolicies --all-namespaces

# Describe specific policy
kubectl describe networkpolicy allow-cre-app-ingress -n cre-system
```

### Monitor Denied Connections

```bash
# View CNI logs for denied connections (GKE with Calico)
kubectl logs -n kube-system -l k8s-app=calico-node

# View network policy events
kubectl get events --all-namespaces --field-selector reason=NetworkPolicyViolation
```

### Audit Network Traffic

```bash
# Install tcpdump in a pod for debugging
kubectl debug -it pod/cre-app-xxx -n cre-system \
  --image=nicolaka/netshoot \
  --target=cre-app-container \
  -- tcpdump -i eth0 -n
```

## Common Issues and Solutions

### Issue: Pod cannot resolve DNS

**Symptom**: `nslookup` fails, cannot resolve service names

**Solution**: Verify `10-allow-dns.yaml` is applied:
```bash
kubectl get networkpolicy allow-dns-access -n cre-system
```

### Issue: Pod cannot reach external services

**Symptom**: `curl https://external-api.com` times out

**Solution**: Add `allow-external-egress: "true"` label:
```bash
kubectl label pod cre-app-xxx allow-external-egress=true -n cre-system
```

### Issue: Prometheus cannot scrape metrics

**Symptom**: Missing metrics in Prometheus dashboard

**Solution**: Verify monitoring namespace is labeled:
```bash
kubectl label namespace cre-system monitoring-enabled=true
kubectl apply -f 20-cre-app-policies.yaml
```

## Policy Customization

### Adding a New Application

1. Create namespace-specific deny-all policies in `00-default-deny-all.yaml`
2. Add application-specific rules in a new file `2X-<app>-policies.yaml`
3. Test connectivity before applying to production

### Allowing New External Service

1. Identify required destination IPs/ports
2. Add egress rule in `40-external-egress.yaml`:
   ```yaml
   - to:
     - ipBlock:
         cidr: 203.0.113.0/24  # External service CIDR
     ports:
     - protocol: TCP
       port: 8443
   ```

## Security Best Practices

1. **Start with Deny-All**: Always apply default deny policies first
2. **Least Privilege**: Only allow necessary traffic
3. **Label-Based Selection**: Use labels for flexible policy application
4. **Regular Audits**: Review policies quarterly for unused rules
5. **Testing**: Test all policies in staging before production
6. **Documentation**: Document all custom policy decisions

## CNI Requirements

Network Policies require a CNI plugin that supports them. GKE supports:
- **GKE Dataplane V2** (Cilium) - Recommended for advanced policies
- **Calico** - Full NetworkPolicy support
- **Default GKE CNI** - Basic NetworkPolicy support

Verify CNI support:
```bash
gcloud container clusters describe ${CLUSTER_NAME} \
  --region=${REGION} \
  --format="value(networkConfig.datapathProvider)"
```

## References

- [Kubernetes Network Policies](https://kubernetes.io/docs/concepts/services-networking/network-policies/)
- [GKE Network Policy](https://cloud.google.com/kubernetes-engine/docs/how-to/network-policy)
- [Calico Network Policy](https://docs.projectcalico.org/security/kubernetes-network-policy)
- [Network Policy Recipes](https://github.com/ahmetb/kubernetes-network-policy-recipes)
