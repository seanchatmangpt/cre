# GCP Node Auto-Discovery for CRE Clustering

## Overview

The GCP Discovery module provides automatic node discovery for CRE clustering on Google Kubernetes Engine (GKE). It supports DNS-based service discovery using headless services and Kubernetes API-based pod watching.

## Features

- **DNS-based Discovery**: Query headless service for pod addresses
- **K8s Native Integration**: Use Kubernetes API for pod discovery
- **StatefulSet Support**: Watch StatefulSet for pod changes with stable network identities
- **Deployment Support**: Dynamic pod discovery for Deployments
- **Pod Replacement Handling**: Handle pod restart gracefully
- **No Hard-coded IPs**: All addresses from DNS or API
- **Retry Logic**: Automatic retry with exponential backoff

## Architecture

```
                    ┌─────────────────────┐
                    │   CRE Cluster       │
                    │   (gen_server)      │
                    └──────────┬──────────┘
                               │
                               │ uses
                               ▼
                    ┌─────────────────────┐
                    │   gcp_discovery     │
                    │   Module            │
                    └──────────┬──────────┘
                               │
                 ┌─────────────┴─────────────┐
                 │                           │
                 ▼                           ▼
         ┌───────────────┐          ┌───────────────┐
         │ DNS Queries   │          │ K8s API Watch │
         │ (A/SRV)       │          │ (Polling)     │
         └───────────────┘          └───────────────┘
                 │                           │
                 ▼                           ▼
         ┌───────────────┐          ┌───────────────┐
         │ Headless      │          │ StatefulSet/  │
         │ Service       │          │ Deployment    │
         └───────────────┘          └───────────────┘
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CRE_SERVICE_NAME` | Headless service name | `cre-service` |
| `CRE_NAMESPACE` | Kubernetes namespace | `default` |
| `CRE_NODE_NAME` | Erlang node name prefix | `cre` |
| `CRE_CLUSTER_DISCOVERY_METHOD` | Discovery method (`gcp`, `dns`, `env`, `static`, `none`) | `gcp` |
| `CRE_DNS_DOMAIN` | DNS domain suffix | `cluster.local` |
| `CRE_CLUSTER_MAX_RETRIES` | Max connection retry attempts | `5` |
| `POD_IP` | Current pod IP address | - |
| `POD_NAME` | Current pod name | - |
| `HOSTNAME` | Pod hostname | - |

## Module API

### discover_peers/0

```erlang
discover_peers() -> [node()]
```

Discovers peer nodes using GKE DNS-based discovery with default configuration.

### discover_peers/1

```erlang
discover_peers(Config) -> [node()]
    Config = #{
        service_name => string(),
        namespace => string(),
        node_name => string(),
        dns_domain => string(),
        use_pod_dns => boolean()
    }
```

Discovers peer nodes with custom configuration.

### watch_statefulset/1

```erlang
watch_statefulset(StatefulSetName) -> {ok, WatchRef} | {error, Reason}
    StatefulSetName = string()
    WatchRef = {pid(), reference()}
```

Starts watching a StatefulSet for pod changes.

### watch_deployment/1

```erlang
watch_deployment(DeploymentName) -> {ok, WatchRef} | {error, Reason}
    DeploymentName = string()
    WatchRef = {pid(), reference()}
```

Starts watching a Deployment for pod changes.

### build_node_list/1

```erlang
build_node_list(Addresses) -> [node()]
    Addresses = [string() | inet:ip4_address()]
```

Builds a list of Erlang node names from pod addresses.

### build_node_list/2

```erlang
build_node_list(Addresses, Config) -> [node()]
    Config = #{
        node_name => string(),
        exclude_self => boolean()
    }
```

Builds a list of Erlang node names with custom configuration.

### stop_watch/1

```erlang
stop_watch(WatchRef) -> ok
    WatchRef = {pid(), reference()}
```

Stops watching a Kubernetes resource.

## Usage Examples

### Basic Cluster Joining with GCP Discovery

```erlang
%% Start cluster manager with GCP discovery
{ok, _Pid} = cluster:start_link([{discovery_method, gcp}]),

%% Join cluster using auto-discovery
ok = cluster:join_cluster(),

%% Get current cluster members
Nodes = cluster:get_nodes(),

%% Leave cluster gracefully
ok = cluster:leave_cluster().
```

### Custom Discovery Configuration

```erlang
%% Discover peers with custom config
Config = #{
    service_name => "my-cre-service",
    namespace => "production",
    node_name => "cre",
    dns_domain => "cluster.local"
},
Peers = gcp_discovery:discover_peers(Config),

%% Join specific peers
ok = cluster:join_cluster(Peers).
```

### Watching StatefulSet Changes

```erlang
%% Start watching a StatefulSet
{ok, WatchRef} = gcp_discovery:watch_statefulset("cre-statefulset"),

%% ... do work ...

%% Stop watching
ok = gcp_discovery:stop_watch(WatchRef).
```

### Building Node List from Addresses

```erlang
%% Build node list from pod IPs
Addresses = ["10.0.0.1", "10.0.0.2", "10.0.0.3"],
Nodes = gcp_discovery:build_node_list(Addresses),

%% Build with custom node name and exclude self
Config = #{
    node_name => "myapp",
    exclude_self => true
},
FilteredNodes = gcp_discovery:build_node_list(Addresses, Config).
```

## GKE Deployment

### Headless Service

```yaml
apiVersion: v1
kind: Service
metadata:
  name: cre-service
  namespace: cre-prod
spec:
  clusterIP: None  # Headless service
  selector:
    app: cre
  ports:
  - name: epmd
    port: 4369
  - name: dist
    port: 4142
```

### StatefulSet

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: cre
  namespace: cre-prod
spec:
  serviceName: cre-service
  replicas: 3
  selector:
    matchLabels:
      app: cre
  template:
    metadata:
      labels:
        app: cre
    spec:
      containers:
      - name: cre
        image: gcr.io/project/cre:latest
        env:
        - name: CRE_SERVICE_NAME
          value: "cre-service"
        - name: CRE_NAMESPACE
          value: "cre-prod"
        - name: CRE_NODE_NAME
          value: "cre"
        - name: CRE_CLUSTER_DISCOVERY_METHOD
          value: "gcp"
        - name: POD_IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
```

### ConfigMap Integration

The ConfigMap at `/Users/sac/cre/k8s/configmap.yaml` includes GCP service discovery configuration:

```yaml
# GCP Service Discovery Configuration
CRE_SERVICE_NAME: "cre-service"
CRE_NAMESPACE: "cre-prod"
CRE_NODE_NAME: "cre"
CRE_CLUSTER_DISCOVERY_METHOD: "gcp"
CRE_DNS_DOMAIN: "cluster.local"
CRE_CLUSTER_MAX_RETRIES: "5"
```

## DNS Resolution

### Headless Service Pattern

GKE headless services return multiple A records, one for each pod:

```
$ nslookup cre-service.cre-prod.svc.cluster.local
Server:    10.0.0.10
Address:   10.0.0.10#53

Name:      cre-service.cre-prod.svc.cluster.local
Address 1: 10.4.0.1
Address 2: 10.4.0.2
Address 3: 10.4.0.3
```

### Per-Pod DNS Pattern

StatefulSet pods have stable DNS names:

```
cre-0.cre-service.cre-prod.svc.cluster.local
cre-1.cre-service.cre-prod.svc.cluster.local
cre-2.cre-service.cre-prod.svc.cluster.local
```

## Error Handling

The module handles various error conditions:

- **DNS not found**: Returns empty list, logs warning
- **Network partition**: Triggers healing strategy
- **Pod restart**: Automatic re-discovery
- **Node replacement**: Handled via DNS polling

## Testing

Run the EUnit tests:

```bash
rebar3 eunit -m gcp_discovery
```

Run cluster integration tests:

```bash
rebar3 eunit --application cre
```

## Integration with cluster.erl

The `cluster` module in `/Users/sac/cre/src/db/cluster.erl` integrates GCP discovery:

```erlang
%% Set discovery method to GCP
ok = cluster:set_discovery_method(gcp),

%% Discover peers using GCP
Peers = cluster:discover_peers(),

%% Join cluster
ok = cluster:join_cluster().
```

## Files

- Source: `/Users/sac/cre/src/cluster/gcp_discovery.erl`
- Tests: `/Users/sac/cre/test/cluster/gcp_discovery_tests.erl`
- Integration: `/Users/sac/cre/src/db/cluster.erl`
- ConfigMap: `/Users/sac/cre/k8s/configmap.yaml`
