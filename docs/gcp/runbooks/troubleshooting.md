# CRE GCP Troubleshooting Runbook

**Common issues and solutions for CRE workflow engine on Google Cloud Platform.**

---

## Table of Contents

1. [Troubleshooting Methodology](#troubleshooting-methodology)
2. [Quick Diagnostics](#quick-diagnostics)
3. [Common Issues](#common-issues)
4. [Performance Issues](#performance-issues)
5. [Networking Issues](#networking-issues)
6. [Database Issues](#database-issues)
7. [Crash Analysis](#crash-analysis)
8. [Troubleshooting Flowcharts](#troubleshooting-flowcharts)
9. [Escalation Contacts](#escalation-contacts)

---

## Troubleshooting Methodology

### Diagnostic Flowchart

```
Issue Reported
      |
      v
Is service reachable?
      |
      +-- NO --> Check Network/DNS/LB
      |
      +-- YES --> Check Pod Health
                      |
                      +-- Pods crashing --> Check Logs/Events
                      |
                      +-- Pods unhealthy --> Check Resources/Config
                      |
                      +-- Pods healthy --> Check Application Logic
```

### Data Gathering Checklist

Before troubleshooting, gather:

```bash
# 1. Cluster status
kubectl cluster-info
kubectl get nodes
kubectl top nodes

# 2. Pod status
kubectl get pods -n cre
kubectl top pods -n cre
kubectl describe pods -n cre

# 3. Recent events
kubectl get events -n cre --sort-by='.lastTimestamp'

# 4. Logs
kubectl logs -n cre -l app=cre --tail=100
kubectl logs -n cre -l app=cre --previous --tail=100

# 5. Service status
kubectl get svc -n cre
kubectl get endpoints -n cre

# 6. Resource usage
kubectl describe nodes | grep -A 5 "Allocated resources"
```

---

## Quick Diagnostics

### Health Check Script

```bash
#!/bin/bash
# scripts/runbooks/diagnostics.sh

set -euo pipefail

NAMESPACE="${NAMESPACE:-cre}"
PROJECT_ID="${PROJECT_ID:-your-project-id}"

echo "=== CRE Diagnostics ==="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo

# 1. Pod Health
echo "1. Pod Health"
kubectl get pods -n ${NAMESPACE}
echo

# 2. Pod Conditions
echo "2. Pod Conditions"
for pod in $(kubectl get pods -n ${NAMESPACE} -o name); do
  echo "  ${pod}:"
  kubectl get ${pod} -n ${NAMESPACE} -o jsonpath='{range .status.conditions[*]}    {.type}={.status}{ "\n"}{end}'
done
echo

# 3. Resource Usage
echo "3. Resource Usage"
kubectl top pods -n ${NAMESPACE}
kubectl top nodes
echo

# 4. Service Endpoints
echo "4. Service Endpoints"
kubectl get endpoints -n ${NAMESPACE}
echo

# 5. Recent Events
echo "5. Recent Events (last 1 hour)"
kubectl get events -n ${NAMESPACE} --field-selector=lastTimestamp>$(date -u -d '1 hour ago' +%Y-%m-%dT%H:%M:%SZ) || echo "  No recent events"
echo

# 6. CRE Process Status
echo "6. CRE Process Status"
POD=$(kubectl get pod -n ${NAMESPACE} -l app=cre -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")
if [ -n "${POD}" ]; then
  kubectl exec -n ${NAMESPACE} ${POD} -- \
    /opt/cre/bin/cre_eval "erlang:process_info(whereis(cre_master), status)." 2>/dev/null || echo "  CRE master not running"
else
  echo "  No CRE pods found"
fi
echo

echo "=== Diagnostics Complete ==="
```

---

## Common Issues

### Issue: Pods in CrashLoopBackOff

**Symptoms:**
```
NAME                    READY   STATUS             RESTARTS   AGE
cre-7d9f4b8c6-x9k2p     0/1     CrashLoopBackOff   7          23m
```

**Diagnosis:**

```bash
# Check pod logs
kubectl logs cre-7d9f4b8c6-x9k2p -n cre

# Check previous instance logs
kubectl logs cre-7d9f4b8c6-x9k2p -n cre --previous

# Describe pod for events
kubectl describe pod cre-7d9f4b8c6-x9k2p -n cre

# Common causes to check in logs:
# - "no match of right hand value" - Code error
# - "badarg" - Invalid argument
# - "timeout" - Resource constraint
# - "connection refused" - Dependency unavailable
```

**Resolution:**

```bash
# If configuration error:
kubectl edit configmap cre-config -n cre

# If image error:
kubectl set image deployment/cre cre=gcr.io/${PROJECT_ID}/cre:correct-tag -n cre

# If resource constraint:
kubectl set resources deployment/cre --limits=cpu=2,memory=4Gi -n cre

# Restart pods
kubectl rollout restart deployment/cre -n cre
```

---

### Issue: Pods Pending (Not Scheduling)

**Symptoms:**
```
NAME                    READY   STATUS    RESTARTS   AGE
cre-7d9f4b8c6-x9k2p     0/1     Pending   0          5m
```

**Diagnosis:**

```bash
# Describe pod to see why it's not scheduling
kubectl describe pod cre-7d9f4b8c6-x9k2p -n cre

# Check node capacity
kubectl describe nodes | grep -A 5 "Allocated resources"

# Check for taints/tolerations mismatch
kubectl get nodes -o custom-columns=NAME:.metadata.name,TAINTS:.spec.taints
```

**Common Causes and Solutions:**

| Cause | Solution |
|-------|----------|
| Insufficient resources | Add nodes or scale up node pool |
| Taint mismatch | Add tolerations to deployment |
| PVC not bound | Check PV/PVC binding |
| Image pull error | Verify image exists and credentials |

**Resolution:**

```bash
# Add nodes
gcloud container node-pools resize general \
  --cluster=cre-cluster \
  --region=${REGION} \
  --num-nodes=5 \
  --project=${PROJECT_ID}

# Or remove resource requests temporarily
kubectl set resources deployment/cre --requests=cpu=100m,memory=128Mi -n cre
```

---

### Issue: ImagePullBackOff

**Symptoms:**
```
NAME                    READY   STATUS              RESTARTS   AGE
cre-7d9f4b8c6-x9k2p     0/1     ImagePullBackOff   0          2m
```

**Diagnosis:**

```bash
# Check image name and tag
kubectl get deployment cre -n cre -o jsonpath='{.spec.template.spec.containers[0].image}'

# Verify image exists
gcloud container images list-tags gcr.io/${PROJECT_ID}/cre \
  --project=${PROJECT_ID}

# Check image pull secrets
kubectl get pods cre-7d9f4b8c6-x9k2p -n cre -o jsonpath='{.spec.imagePullSecrets}'
```

**Resolution:**

```bash
# Verify image exists
docker pull gcr.io/${PROJECT_ID}/cre:tag

# Create image pull secret (if using private registry)
kubectl create secret docker-registry gcr-json-key \
  --docker-server=https://gcr.io \
  --docker-username=_json_key \
  --docker-password="$(cat /path/to/key.json)" \
  --namespace=cre

# Patch service account with image pull secret
kubectl patch serviceaccount default -n cre \
  -p '{"imagePullSecrets": [{"name": "gcr-json-key"}]}'

# Restart deployment
kubectl rollout restart deployment/cre -n cre
```

---

### Issue: Service Not Accessible

**Symptoms:**
- Cannot connect to service from external IP
- Connection refused errors
- Timeout errors

**Diagnosis:**

```bash
# Check service exists
kubectl get svc -n cre

# Check endpoints
kubectl get endpoints -n cre

# Check ingress (if using)
kubectl get ingress -n cre

# Port forward test
kubectl port-forward -n cre svc/cre-service 4142:4142
curl http://localhost:4142/api/v1/health
```

**Resolution:**

```bash
# If no endpoints: Check pod labels
kubectl get pods -n cre --show-labels

# Fix label mismatch
kubectl label pod <pod-name> app=cre --overwrite -n cre

# If firewall issue: Check GCP firewall rules
gcloud compute firewall-rules list \
  --filter="name:cre-*" \
  --project=${PROJECT_ID}

# If ingress issue: Describe ingress
kubectl describe ingress cre-ingress -n cre
```

---

## Performance Issues

### Issue: High CPU Usage

**Diagnosis:**

```bash
# Check top consumers
kubectl top pods -n cre --sort-by=cpu

# Check process-level CPU in pod
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "recon:proc_count()."

# Check scheduler usage
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "recon:scheduler_usage(1)."
```

**Resolution:**

```bash
# Scale horizontally
kubectl scale deployment/cre --replicas=10 -n cre

# Or increase CPU limits
kubectl set resources deployment/cre --limits=cpu=4 -n cre

# Check for infinite loops in code
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    {ok, Stats} = gen_pnet:stats(whereis(cre_net)),
    io:format('Transition fires: ~p~n', [Stats#stats.current#stat.fps]).
  "
```

---

### Issue: High Memory Usage

**Diagnosis:**

```bash
# Check memory usage
kubectl top pods -n cre --sort-by=memory

# Check for memory leaks
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "recon:bin_leak(100)."

# Check ETS tables
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "ets:i()."
```

**Resolution:**

```bash
# Increase memory limits
kubectl set resources deployment/cre --limits=memory=8Gi -n cre

# Check for large ETS tables
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    lists:foreach(fun(T) ->
      Size = mnesia:table_info(T, size),
      Memory = ets:info(T, memory),
      io:format('~s: ~p records, ~p bytes~n', [T, Size, Memory])
    end, mnesia:system_info(tables)).
  "

# Restart to clear accumulated memory
kubectl rollout restart deployment/cre -n cre
```

---

### Issue: Slow Response Times

**Diagnosis:**

```bash
# Measure response time
time curl http://$(kubectl get svc -n cre cre-service -o jsonpath='{.spec.clusterIP}'):4142/api/v1/health

# Check message queue lengths
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    Pids = [whereis(N) || N <- [cre_master, cre_worker]],
    lists:foreach(fun(P) ->
      {message_queue_len, QLen} = erlang:process_info(P, message_queue_len),
      io:format('~p: ~p messages~n', [P, QLen])
    end, Pids).
  "

# Check for blocked processes
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "recon:grep_count('<<\"blocked\">>')."
```

**Resolution:**

```bash
# Scale worker pool
kubectl patch deployment cre-worker -n cre -p '{"spec":{"replicas":20}}'

# Check Mnesia transaction locks
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:system_info(lock_queue)."

# Enable dirty reads for non-critical data
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:activity(dirty_reads, fun() -> ok end)."
```

---

## Networking Issues

### Issue: Pod-to-Pod Communication Failures

**Diagnosis:**

```bash
# Test from one pod to another
kubectl run -it --rm debug --image=busybox --restart=Never -n cre -- \
  wget -O- http://cre-service:4142/api/v1/health

# Check network policies
kubectl get networkpolicies -n cre

# Check pod IPs
kubectl get pods -n cre -o wide
```

**Resolution:**

```bash
# If network policy blocking:
kubectl delete networkpolicy <policy-name> -n cre

# If DNS issue:
kubectl run -it --rm debug --image=busybox --restart=Never -n cre -- \
  nslookup cre-service.cre.svc.cluster.local

# If CNI issue:
kubectl logs -n kube-system -l k8s-app=calico-node --tail=50
```

---

### Issue: External Connectivity Failures

**Diagnosis:**

```bash
# Check Cloud NAT status
gcloud compute routers nats list \
  --region=${REGION} \
  --project=${PROJECT_ID}

# Test external connectivity
kubectl run -it --rm debug --image=busybox --restart=Never -n cre -- \
  wget -O- https://www.google.com

# Check firewall rules
gcloud compute firewall-rules list \
  --filter="direction:EGRESS" \
  --project=${PROJECT_ID}
```

**Resolution:**

```bash
# Create Cloud NAT if missing
gcloud compute routers nats create cre-nat \
  --router=cre-router \
  --router-region=${REGION} \
  --nat-all-subnet-ip-ranges \
  --auto-allocate-nat-external-ips \
  --project=${PROJECT_ID}
```

---

## Database Issues

### Issue: Mnesia Transaction Deadlocks

**Symptoms:**
- Transactions timing out
- `{aborted, {deadlock, [...]}}` errors
- Requests hanging indefinitely

**Diagnosis:**

```bash
# Check for stuck transactions
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:system_info(lock_queue)."

# Check table info
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    lists:foreach(fun(T) ->
      case mnesia:table_info(T, where_to_write) of
        [] -> io:format('~s: WARNING - no write nodes~n', [T]);
        Nodes -> ok
      end
    end, mnesia:system_info(tables)).
  "
```

**Resolution:**

```bash
# Kill stuck transactions
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    {ok, LockQueue} = mnesia:system_info(lock_queue),
    lists:foreach(fun({Pid, _}) ->
      exit(Pid, kill)
    end, LockQueue).
  "

# Use async_dirty for non-critical writes
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:activity(async_dirty, fun() -> ok end)."
```

---

### Issue: Mnesia Partition

**Symptoms:**
- Nodes not seeing each other
- `{no_exists, Table}` errors
- Inconsistent data between nodes

**Diagnosis:**

```bash
# Check cluster status
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "mnesia:system_info(running_db_nodes)."

# Check if nodes are connected
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "net_adm:ping('cre@cre-0.cre.cre.svc.cluster.local')."
```

**Resolution:**

```bash
# Force reconnect
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "
    case mnesia:change_config(extra_db_nodes, ['cre@cre-0.cre.cre.svc.cluster.local']) of
      {ok, Nodes} -> io:format('Connected to: ~p~n', [Nodes]);
      {error, Reason} -> io:format('Error: ~p~n', [Reason])
    end.
  "

# Restart Mnesia on partitioned node
kubectl exec -n cre deployment/cre -- \
  /opt/cre/bin/cre_eval "application:stop(mnesia), application:start(mnesia)."
```

---

## Crash Analysis

### Analyzing Crash Dumps

```bash
# Find crash dump files
kubectl exec -n cre deployment/cre -- \
  find /opt/cre -name "erl_crash.dump*"

# Copy crash dump locally
kubectl cp -n cre deployment/cre:/opt/cre/erl_crash.dump ./crash.dump

# Analyze with webtool
erl -eval "webtool:start()" -s crashdump_viewer view_file crash.dump
```

### Key Information to Extract

```erlang
% Extract from crash dump
% 1. Exit reason
erlang:halt(1).

% 2. Process info at crash
erlang:process_info(self(), [current_function, message_queue_len, heap_size]).

% 3. Scheduler info
erlang:system_info(scheduler_online).

% 4. Memory breakdown
erlang:memory().
```

---

## Troubleshooting Flowcharts

### Pod Issue Flowchart

```
Pod Not Working
      |
      v
Check Pod Status
      |
      +-- Pending --> Check Resources/Taints/PVC
      |                   |
      |                   +-- Insufficient Resources --> Add Nodes
      |                   +-- Taint Mismatch --> Add Tolerations
      |                   +-- PVC Pending --> Check Storage Class
      |
      +-- CrashLoopBackOff --> Check Logs
      |                             |
      |                             +-- Config Error --> Fix ConfigMap
      |                             +-- Image Error --> Fix Image Tag
      |                             +-- Resource Error --> Increase Limits
      |
      +-- Running but Not Ready --> Check Readiness Probe
      |                             |
      |                             +-- Probe Failing --> Check App Logs
      |
      +-- ImagePullBackOff --> Check Image/Secrets
                                |
                                +-- Image Missing --> Push Image
                                +-- Auth Error --> Update Secret
```

### Performance Issue Flowchart

```
Performance Degraded
      |
      v
Measure Metrics
      |
      +-- High CPU --> Check Process Count/Scheduler Usage
      |                   |
      |                   +-- Too Many Processes --> Scale Horizontally
      |                   +-- Scheduler Saturation --> Add Nodes
      |
      +-- High Memory --> Check for Leaks/ETS Tables
      |                      |
      |                      +-- Memory Leak --> Restart Pods
      |                      +-- Large ETS --> Clean Tables
      |
      +-- Slow Response --> Check Queue Lengths/Locks
                             |
                             +-- Long Queues --> Scale Workers
                             +-- DB Locks --> Review Transactions
```

---

## Escalation Contacts

| Role | Name | Contact | Hours |
|------|------|---------|-------|
| On-Call Engineer | CRE Ops | oncall@company.com | 24/7 |
| Infrastructure Lead | Infra Team | infra@company.com | Business Hours |
| Engineering Manager | CRE Leadership | eng-manager@company.com | Business Hours |
| GCP Support | Google Cloud | gcp-support | 24/7 |

### Escalation Criteria

| Severity | Criteria | Escalate After |
|----------|----------|----------------|
| S1 | Service completely down | 15 minutes |
| S2 | Major functionality broken | 1 hour |
| S3 | Performance degradation | 4 hours |
| S4 | Minor issues | Next business day |

---

## Quick Reference Commands

### Diagnostic Commands

```bash
# Quick health check
kubectl get pods -n cre && kubectl top pods -n cre

# View logs
kubectl logs -f -l app=cre -n cre

# Describe resources
kubectl describe pod -l app=cre -n cre
kubectl describe node
kubectl describe svc cre-service -n cre

# Check events
kubectl get events -n cre --sort-by='.lastTimestamp'

# Port forward for testing
kubectl port-forward -n cre svc/cre-service 4142:4142
```

### Resolution Commands

```bash
# Restart deployment
kubectl rollout restart deployment/cre -n cre

# Scale up/down
kubectl scale deployment/cre --replicas=N -n cre

# Edit resources
kubectl set resources deployment/cre --limits=cpu=X,memory=Y -n cre

# Update image
kubectl set image deployment/cre cre=gcr.io/$PROJECT_ID/cre:tag -n cre

# Rollback
kubectl rollout undo deployment/cre -n cre
```

---

*Last Updated: 2025-02-09*
*For CRE version 0.3.0+*
