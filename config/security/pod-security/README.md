# Pod Security Standards for CRE Platform

## Overview

This directory contains Pod Security Standards configuration implementing secure pod configurations across all namespaces.

## Pod Security Admission

Pod Security Admission (PSA) replaces the deprecated PodSecurityPolicy (PSP) API and provides three built-in profiles:

### Security Profiles

| Profile | Description | Use Case |
|---------|-------------|----------|
| **Privileged** | Unrestricted policy | kube-system, system components |
| **Baseline** | Minimally restrictive | Monitoring, ingress controllers |
| **Restricted** | Heavily restricted, security hardened | Application workloads |

## Namespace Configuration

| Namespace | Enforce | Audit | Warn |
|-----------|---------|-------|------|
| cre-system | restricted | restricted | restricted |
| monitoring | baseline | restricted | restricted |
| external-secrets | restricted | restricted | restricted |
| ingress-nginx | baseline | baseline | restricted |
| kube-system | privileged | baseline | baseline |

## Restricted Profile Requirements

To comply with the **restricted** profile, pods MUST:

### Security Context Requirements

1. **Run as non-root**: `runAsNonRoot: true`
2. **Drop all capabilities**: `capabilities.drop: [ALL]`
3. **No privilege escalation**: `allowPrivilegeEscalation: false`
4. **Read-only root filesystem**: `readOnlyRootFilesystem: true`
5. **Seccomp profile**: `seccompProfile.type: RuntimeDefault`

### Prohibited Settings

- `privileged: true`
- `hostNetwork: true`
- `hostPID: true`
- `hostIPC: true`
- `hostPath` volumes
- Dangerous capabilities (SYS_ADMIN, NET_ADMIN, etc.)

## Deployment

### Apply Namespace Labels

```bash
kubectl apply -f namespace-labels.yaml
```

### Verify Configuration

```bash
# Check namespace labels
kubectl get namespaces --show-labels

# Verify pod security configuration
kubectl label namespace cre-system --list | grep pod-security
```

Expected output:
```
pod-security.kubernetes.io/enforce=restricted
pod-security.kubernetes.io/enforce-version=latest
pod-security.kubernetes.io/audit=restricted
pod-security.kubernetes.io/audit-version=latest
pod-security.kubernetes.io/warn=restricted
pod-security.kubernetes.io/warn-version=latest
```

## Testing Pod Security

### Test Compliant Pod

```bash
# Should succeed
kubectl apply -f restricted-pod-example.yaml
kubectl get pod cre-app-restricted-example -n cre-system
```

### Test Non-Compliant Pod

```bash
# Should be rejected
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Pod
metadata:
  name: test-privileged
  namespace: cre-system
spec:
  containers:
  - name: nginx
    image: nginx
    securityContext:
      privileged: true
EOF
```

Expected error:
```
Error from server (Forbidden): error when creating "STDIN": pods "test-privileged" is forbidden:
violates PodSecurity "restricted:latest": privileged (container "nginx" must not set securityContext.privileged=true)
```

### Test Baseline Pod in Restricted Namespace

```bash
# Should be rejected with warning
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Pod
metadata:
  name: test-baseline
  namespace: cre-system
spec:
  containers:
  - name: nginx
    image: nginx
    # Missing security context
EOF
```

## Migration from PodSecurityPolicy

### Identify PSP Usage

```bash
# List existing PodSecurityPolicies
kubectl get psp

# Check which pods are using PSPs
kubectl get pods --all-namespaces -o jsonpath='{range .items[*]}{.metadata.namespace}{"\t"}{.metadata.name}{"\t"}{.metadata.annotations.kubernetes\.io/psp}{"\n"}{end}'
```

### Migration Steps

1. **Audit Current Configuration**
   ```bash
   # Check which pods would violate restricted profile
   kubectl label namespace cre-system pod-security.kubernetes.io/audit=restricted --overwrite
   kubectl get events -n cre-system --field-selector reason=PodSecurity
   ```

2. **Add Warning Mode First**
   ```bash
   kubectl label namespace cre-system pod-security.kubernetes.io/warn=restricted --overwrite
   ```

3. **Fix Non-Compliant Pods**
   - Update securityContext settings
   - Remove privileged containers
   - Add required security constraints

4. **Enable Enforcement**
   ```bash
   kubectl label namespace cre-system pod-security.kubernetes.io/enforce=restricted --overwrite
   ```

5. **Remove PodSecurityPolicies**
   ```bash
   kubectl delete psp --all
   ```

## Security Context Examples

### Application Container

```yaml
securityContext:
  allowPrivilegeEscalation: false
  runAsNonRoot: true
  runAsUser: 1000
  capabilities:
    drop:
    - ALL
  readOnlyRootFilesystem: true
  seccompProfile:
    type: RuntimeDefault
```

### Init Container

```yaml
initContainers:
- name: init-setup
  image: busybox:latest
  command: ['sh', '-c', 'echo "Initializing..."']
  securityContext:
    allowPrivilegeEscalation: false
    runAsNonRoot: true
    runAsUser: 1000
    capabilities:
      drop:
      - ALL
    readOnlyRootFilesystem: true
    seccompProfile:
      type: RuntimeDefault
  volumeMounts:
  - name: tmp
    mountPath: /tmp
```

### Sidecar Container

```yaml
- name: cloud-sql-proxy
  image: gcr.io/cloud-sql-connectors/cloud-sql-proxy:latest
  args:
  - "--structured-logs"
  - "--port=5432"
  - "project:region:instance"
  securityContext:
    allowPrivilegeEscalation: false
    runAsNonRoot: true
    capabilities:
      drop:
      - ALL
    readOnlyRootFilesystem: true
    seccompProfile:
      type: RuntimeDefault
```

## Troubleshooting

### Issue: Pod rejected with "must not set securityContext.privileged=true"

**Solution**: Remove `privileged: true` from container securityContext

### Issue: Pod rejected with "must run as non-root"

**Solution**: Add to both pod and container securityContext:
```yaml
runAsNonRoot: true
runAsUser: 1000
```

### Issue: Application cannot write to filesystem

**Solution**: Use writable volumes for temporary data:
```yaml
volumeMounts:
- name: tmp
  mountPath: /tmp
volumes:
- name: tmp
  emptyDir: {}
```

### Issue: Application needs specific Linux capabilities

**Solution**: Request specific capabilities (if allowed by policy):
```yaml
capabilities:
  drop:
  - ALL
  add:
  - NET_BIND_SERVICE  # Only if absolutely necessary
```

## Advanced Security

### AppArmor Integration

```yaml
metadata:
  annotations:
    container.apparmor.security.beta.kubernetes.io/app: localhost/cre-app-restricted
```

### Seccomp Custom Profile

```yaml
securityContext:
  seccompProfile:
    type: Localhost
    localhostProfile: profiles/cre-app-restricted.json
```

### gVisor Runtime Class

```yaml
spec:
  runtimeClassName: cre-restricted  # Uses gVisor for additional isolation
```

## Monitoring and Auditing

### View Pod Security Violations

```bash
# View audit events
kubectl get events -n cre-system \
  --field-selector reason=PodSecurity \
  --sort-by='.lastTimestamp'

# View warnings in pod describe
kubectl describe pod <pod-name> -n cre-system | grep Warning
```

### Aggregate Violations Across Cluster

```bash
# Count violations by namespace
kubectl get events --all-namespaces \
  --field-selector reason=PodSecurity \
  -o jsonpath='{range .items[*]}{.involvedObject.namespace}{"\n"}{end}' \
  | sort | uniq -c
```

## Security Best Practices

1. **Default to Restricted**: Use restricted profile for all application namespaces
2. **Gradual Enforcement**: Start with audit/warn before enforcing
3. **Immutable Root FS**: Always use readOnlyRootFilesystem: true
4. **No Root**: Never run containers as root (uid 0)
5. **Drop Capabilities**: Always drop all capabilities, add only what's needed
6. **Seccomp**: Use RuntimeDefault or custom seccomp profiles
7. **Resource Limits**: Always set memory and CPU limits
8. **Version Pinning**: Use latest for continuous updates

## References

- [Pod Security Standards](https://kubernetes.io/docs/concepts/security/pod-security-standards/)
- [Pod Security Admission](https://kubernetes.io/docs/concepts/security/pod-security-admission/)
- [GKE Pod Security](https://cloud.google.com/kubernetes-engine/docs/how-to/pod-security-admission)
- [Seccomp](https://kubernetes.io/docs/tutorials/security/seccomp/)
- [AppArmor](https://kubernetes.io/docs/tutorials/security/apparmor/)
