# CRE Deployment Guide

This guide covers deploying the CRE (Common Runtime Environment) workflow engine in production environments.

## Table of Contents

- [System Requirements](#system-requirements)
- [OTP Version Requirements](#otp-version-requirements)
- [Installation](#installation)
- [Configuration](#configuration)
- [Starting the Application](#starting-the-application)
- [Production Considerations](#production-considerations)
- [Monitoring and Logging](#monitoring-and-logging)
- [Backup and Recovery](#backup-and-recovery)
- [Upgrade Procedures](#upgrade-procedures)

---

## System Requirements

### Minimum Requirements

| Resource | Minimum |
|----------|---------|
| CPU | 2 cores |
| Memory | 4 GB |
| Disk | 20 GB SSD |
| Network | 1 Gbps |

### Recommended Requirements

| Resource | Recommended |
|----------|-------------|
| CPU | 8+ cores |
| Memory | 16+ GB |
| Disk | 100+ GB SSD |
| Network | 10 Gbps |

### Operating System Support

- **Linux**: Ubuntu 20.04+, CentOS 8+, RHEL 8+
- **macOS**: 10.15+ (development only)
- **Windows**: WSL2 (development only)

---

## OTP Version Requirements

CRE requires Erlang/OTP 25 through 28.

| OTP Version | Status | Notes |
|-------------|--------|-------|
| 25.x | Supported | Minimum supported version |
| 26.x | Supported | Stable |
| 27.x | Supported | Latest stable |
| 28.x | Supported | Tested with compatibility overrides |

### Installing Erlang/OTP

```bash
# Using asdf (recommended)
asdf install erlang 27.2
asdf global erlang 27.2

# Using kerl (alternative)
kerl build 27.2 27.2
kerl install 27.2 ~/erlang/27.2

# Verify installation
erl -version
```

### Installing rebar3

```bash
curl -O https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

---

## Installation

### From Source

```bash
# Clone repository
git clone https://github.com/joergen7/cre.git
cd cre

# Fetch dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests
rebar3 eunit
```

### Building a Release

```bash
# Create production release
rebar3 release

# The release will be in _build/default/rel/cre/
_build/default/rel/cre/bin/cre console
```

### Docker Deployment

```dockerfile
FROM erlang:27-alpine

# Install build dependencies
RUN apk add --no-cache git build-base openssl-dev

# Set working directory
WORKDIR /app

# Copy source files
COPY . .

# Build application
RUN rebar3 get-deps && \
    rebar3 compile && \
    rebar3 release

# Create runtime user
RUN addgroup -g 1000 cre && \
    adduser -D -u 1000 -G cre cre

# Set permissions
RUN chown -R cre:cre /app

USER cre

# Expose ports
# 4142 - Default CRE web service
# 4369 - EPMD
# 9100-9200 - Distributed Erlang
EXPOSE 4142 4369

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:4142/[status.json] || exit 1

# Start application
CMD ["/app/_build/default/rel/cre/bin/cre", "foreground"]
```

---

## Configuration

### Configuration via `cre_config`

CRE uses `persistent_term` storage for configuration values (OTP 21+ optimization). Configuration is initialized at application startup via `cre_config:init/0`.

#### Default Configuration Values

| Configuration Key | Default Value | Description |
|-------------------|---------------|-------------|
| `cre_default_port` | 4142 | HTTP web service port |
| `cre_status_route` | `/[status.json]` | Status endpoint |
| `cre_history_route` | `/history.json` | History endpoint |
| `cre_client_poll_interval` | 250 | Client poll interval (ms) |
| `cre_auth_pbkdf2_iterations` | 100000 | Password hash iterations |
| `cre_auth_default_session_timeout` | 3600 | Session timeout (seconds) |
| `cre_auth_min_password_length` | 8 | Minimum password length |
| `yawl_stateless_checkpoint_dir` | `priv/checkpoints` | Checkpoint directory |
| `yawl_stateless_max_executions` | 1000 | Max concurrent executions |
| `yawl_stateless_execution_ttl` | 3600000 | Execution TTL (ms) |
| `yawl_stateless_ttl_cleanup_interval` | 60000 | TTL cleanup interval (ms) |
| `yawl_timeout_default_timeout` | 30000 | Default timeout (ms) |
| `yawl_timeout_deadlock_interval` | 5000 | Deadlock check interval (ms) |
| `yawl_timeout_resource_check_interval` | 60000 | Resource check interval (ms) |

### Setting Configuration Values

Configuration can be set via `cre_config:set/2`:

```erlang
% Set custom port
cre_config:set(cre_default_port, 8080).

% Set custom timeout
cre_config:set(yawl_timeout_default_timeout, 60000).

% Reload all configuration
cre_config:reload().
```

### Environment Variables

Secrets are managed via environment variables with `CRE_` prefix:

| Environment Variable | Description |
|---------------------|-------------|
| `CRE_COOKIE` | Erlang distribution cookie |
| `CRE_DB_PASSWORD` | Database password |
| `CRE_API_KEY` | API authentication key |

```bash
# Set required secrets
export CRE_COOKIE="your-secret-cookie-here"
export CRE_API_KEY="your-api-key-here"
```

### Runtime Configuration (vm.args)

```
-name cre@127.0.0.1
-setcookie cre_secret_cookie
+K true
+A 128
+P 2097152
+Q 65536
+sdio 2 2
```

### Application Configuration (sys.config)

```erlang
[
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h,
                #{formatter => {logger_formatter, #{}}}}
        ]}
    ]},
    {cre, [
        {default_port, 4142},
        {poll_interval, 250}
    ]}
].
```

---

## Starting the Application

### Development Mode

```bash
# Start Erlang shell with CRE
rebar3 shell

# Or using the application module
erl -pa _build/default/lib/cre/ebin -s cre
```

### Production Mode (Release)

```bash
# Start as daemon
_build/default/rel/cre/bin/cre start

# Start in foreground
_build/default/rel/cre/bin/cre foreground

# Start interactive console
_build/default/rel/cre/bin/cre console

# Stop
_build/default/rel/cre/bin/cre stop

# Restart
_build/default/rel/cre/bin/cre restart

# Ping to check if running
_build/default/rel/cre/bin/cre ping
```

### Distributed Mode

```bash
# Start with long node name
erl -name cre@host.example.com -setcookie mycookie -s cre

# Start with short name (local network only)
erl -sname cre -setcookie mycookie -s cre

# Connect to remote node
erl -sname client -setcookie mycookie
> erlang:spawn_node('cre@host.example.com').
```

### Connecting Nodes

```erlang
% On node1:
erl -sname cre1 -setcookie shared_secret

% On node2:
erl -sname cre2 -setcookie shared_secret

% From node2, connect to node1:
net_adm:ping(cre1@hostname).
```

---

## Production Considerations

### Resource Limits

Configure system limits for production:

```bash
# /etc/security/limits.conf
creuser soft nofile 1048576
creuser hard nofile 1048576
creuser soft nproc 1048576
creuser hard nproc 1048576
```

### Systemd Service

Create `/etc/systemd/system/cre.service`:

```ini
[Unit]
Description=CRE Workflow Engine
Documentation=https://github.com/joergen7/cre
After=network-online.target
Wants=network-online.target

[Service]
Type=notify
User=creuser
Group=creuser
WorkingDirectory=/opt/cre
ExecStart=/opt/cre/bin/cre foreground
ExecStop=/opt/cre/bin/cre stop
Restart=on-failure
RestartSec=5s

# Security
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ReadWritePaths=/var/log/cre /var/lib/cre
ProtectHome=true

# Resource Limits
LimitNOFILE=1048576
LimitNPROC=1048576

# Environment
Environment="CRE_COOKIE=/etc/cre/cookie"
EnvironmentFile=-/etc/default/cre

[Install]
WantedBy=multi-user.target
```

### Kernel Tuning

```bash
# /etc/sysctl.conf
net.ipv4.tcp_keepalive_time = 60
net.ipv4.tcp_keepalive_intvl = 10
net.ipv4.tcp_keepalive_probes = 6
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
fs.file-max = 1048576

# Apply
sudo sysctl -p
```

### Firewall Configuration

```bash
# Essential ports
sudo ufw allow 22/tcp    # SSH
sudo ufw allow 4142/tcp  # CRE web service
sudo ufw allow 4369/tcp  # EPMD
sudo ufw allow 9100:9200/tcp  # Distributed Erlang

# Enable firewall
sudo ufw enable
```

### Circuit Breaker Configuration

CRE includes a circuit breaker (`yawl_breaker`) for preventing cascading failures:

```erlang
% Configure circuit breaker thresholds
Breaker = yawl_breaker:new(my_service),
% Default: 5 failures before opening
% Default: 60s timeout before half-open
% Default: 3 successes to close

% Check if request allowed
case yawl_breaker:check_breaker(my_service) of
    allow -> execute_request();
    deny -> handle_circuit_open()
end

% Record result
case execute_request() of
    {ok, Result} ->
        yawl_breaker:record_success(my_service),
        Result;
    {error, Reason} ->
        yawl_breaker:record_failure(my_service),
        {error, Reason}
end
```

---

## Monitoring and Logging

### Health Checks

CRE provides health check endpoints:

```bash
# Check service health
curl http://localhost:4142/[status.json]

# Check history
curl http://localhost:4142/history.json
```

### Health Module (`yawl_health`)

Built-in health checks for production monitoring:

```erlang
% Run all health checks
yawl_health:check_health().

% Run specific checks
yawl_health:check_health([mnesia, node_health]).

% Readiness probe (for Kubernetes)
yawl_health:readiness_probe().

% Liveness probe
yawl_health:liveness_probe().

% Register custom health check
yawl_health:register_check(my_check, fun() -> {ok, healthy} end).
```

### Status API

The status endpoint returns JSON with:

```json
{
  "cre_info": {
    "load": 0.5,
    "n_wrk": 10
  },
  "node_info": [
    {
      "node": "cre@localhost",
      "n_wrk": 10,
      "load": 0.5
    }
  ],
  "app_info": {
    "queued": [],
    "active": [],
    "complete": []
  }
}
```

### Logging Configuration

```erlang
% Configure logger
logger:set_primary_config(level, info).

% Add file handler
logger:add_handler(file_handler, logger_disk_log_h,
    #{config => #{
        file => "/var/log/cre/cre.log",
        max_no_files => 10,
        max_no_bytes => 10485760
    }}).

% Set log format
logger:set_handler_config(default, formatter,
    {logger_formatter, #{template => [time," ",level," ",msg,"\n"]}}).
```

### Telemetry

CRE supports Prometheus telemetry export:

```erlang
% Telemetry configuration
% See yawl_telemetry_prometheus module for metrics
```

---

## Backup and Recovery

### Workflow State Persistence

CRE uses Mnesia for workflow state persistence:

```erlang
% Initialize database
wf_persistence:init_db().

% Save workflow case
wf_persistence:save_case(Case).

% Load workflow case
wf_persistence:load_case(<<"case-id">>).

% List active cases (for recovery)
wf_persistence:list_active_cases().

% Delete case
wf_persistence:delete_case(<<"case-id">>).
```

### Mnesia Backup

```bash
# Backup Mnesia schema
erl -sname backup -setcookie cre_secret -mnesia dir '"/var/lib/cre/mnesia"' \
    -eval "mnesia:backup('/var/backups/cre/backup.')"

# Restore from backup
erl -sname restore -setcookie cre_secret -mnesia dir '"/var/lib/cre/mnesia"' \
    -eval "mnesia:restore('/var/backups/cre/backup.', [])"
```

### Checkpoint/Recovery

```erlang
% Create checkpoint
wf_persistence:create_checkpoint(EngineState).

% Restore from checkpoint
wf_persistence:restore_from_checkpoint().
```

### Backup Script

```bash
#!/bin/bash
# /usr/local/bin/cre-backup

BACKUP_DIR="/var/backups/cre"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p "$BACKUP_DIR"

# Backup Mnesia
erl -noshell -sname backup -setcookie cre_secret \
    -eval "mnesia:backup('$BACKUP_DIR/backup_$DATE.')" \
    -s init stop

# Backup configuration
tar -czf "$BACKUP_DIR/config_$DATE.tar.gz" /etc/cre/

# Cleanup old backups (keep 30 days)
find "$BACKUP_DIR" -name "*.tar.gz" -mtime +30 -delete
find "$BACKUP_DIR" -name "backup.*" -mtime +30 -delete

echo "Backup completed: $BACKUP_DIR/backup_$DATE"
```

---

## Upgrade Procedures

### Hot Code Upgrade

CRE supports hot code loading:

```bash
# Build new version
rebar3 compile

# Load new modules on running node
erl -sname cre -setcookie cre_secret -remsh cre@nodename
> l(Module).
> c:lc().
```

### Rolling Upgrade (Multi-Node)

```bash
#!/bin/bash
# Rolling upgrade script

NEW_VERSION=$1
NODES=("cre@node1" "cre@node2" "cre@node3")

for node in "${NODES[@]}"; do
    echo "Upgrading $node..."

    # Drain node (stop accepting new work)
    ssh $node "cre_admin:drain('$node')."

    # Wait for active work to complete
    while ssh $node "cre_master:get_status('$node')." | grep -q '"active".*\['; do
        sleep 5
    done

    # Deploy new version
    ssh $node "cd /opt/cre && git pull && rebar3 release"

    # Restart node
    ssh $node "/opt/cre/bin/cre restart"

    # Wait for health check
    until curl -f http://$(echo $node | sed 's/cre@//'):4142/[status.json] > /dev/null 2>&1; do
        sleep 10
    done

    echo "$node upgraded successfully"
done
```

### Zero-Downtime Deployment

For zero-downtime deployment:

1. Deploy new version alongside running version
2. Use Erlang distribution to migrate state
3. Switch traffic using load balancer
4. Retire old version after verification

### Downgrade Procedures

```bash
# Stop current version
_build/default/rel/cre/bin/cre stop

# Restore previous release
cd /opt/cre
git checkout previous-version
rebar3 release

# Start previous version
_build/default/rel/cre/bin/cre start
```

---

## Troubleshooting

### Common Issues

**Node connection failures:**
```bash
# Check EPMD
epmd -names

# Verify cookie consistency
cat /etc/cre/cookie

# Check network connectivity
netstat -an | grep 4369
```

**Memory issues:**
```bash
# Check memory usage
ps aux | grep beam.smp

# Check crash dumps
ls -la /var/log/cre/erl_crash.dump

# Monitor in shell
> erlang:memory(total).
> recon:bin_leak(100).
```

**Performance issues:**
```bash
# Check process count
> erlang:system_info(process_count).

# Check scheduler utilization
> recon:scheduler_usage(1).

# Check ETS tables
> ets:i().
```

### Debug Mode

```bash
# Start with debug logging
erl -sname cre -setcookie cre -s cre logger level debug
```

---

## Additional Resources

- [API Reference](./API_REFERENCE.md)
- [Architecture Overview](./ARCHITECTURE.md)
- [YAWL Patterns Guide](./YAWL_PATTERNS_GUIDE.md)
- [Testing Guide](./TESTING.md)
