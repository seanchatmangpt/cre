# CRE YAWL Workflow Engine - Production Deployment Guide

This guide covers production deployment of CRE, including system requirements, configuration, scaling, monitoring, and security considerations.

## 🎯 Prerequisites

### System Requirements

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| **CPU Cores** | 2 | 8+ |
| **Memory** | 4GB | 16GB+ |
| **Disk Space** | 20GB | 100GB+ SSD |
| **Network** | 1Gbps | 10Gbps |
| **OTP Version** | 25.0+ | 27.0+ |

### Operating Systems

- **Linux**: Ubuntu 20.04+, CentOS 8+, RHEL 8+
- **macOS**: 10.15+ (for development)
- **Windows**: WSL2 (for development)

### Dependencies

```bash
# Install system dependencies
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    git \
    curl \
    wget \
    openssl

# Install Erlang/OTP 25.0+
# Use ASDF or Erlang Solutions package manager
asdf install erlang 25.3.2.9

# Install rebar3
curl -L -o rebar3 https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

## 🚀 Installation Methods

### Method 1: From Hex (Recommended for Production)

```bash
# Create a new release project
mkdir cre-deployment
cd cre-deployment

# Add CRE as dependency
cat > rebar.config << 'EOF'
{deps, [
    {cre, "0.2.1"}
]}.
EOF

# Fetch dependencies
rebar3 deps

# Create release
rebar3 release
```

### Method 2: From GitHub with Release

```bash
# Clone and build
git clone https://github.com/your-org/cre.git
cd cre
git checkout v0.2.1

# Build release
rebar3 deps
rebar3 compile
rebar3 release
```

### Method 3: Docker Deployment

```bash
# Build Docker image
cat > Dockerfile << 'EOF'
FROM erlang:25.3-alpine

# Install dependencies
RUN apk add --no-cache \
    build-base \
    git \
    openssl

# Copy and build
COPY . /app
WORKDIR /app
RUN rebar3 deps && rebar3 compile

# Create runtime user
RUN adduser -D -s /bin/sh creuser
USER creuser

# Expose ports
EXPOSE 8080 4369 8081

# Start CRE
CMD ["./_build/default/rel/cre/bin/cre", "console"]
EOF

# Build and run
docker build -t cre:latest .
docker run -d \
    --name cre-server \
    -p 8080:8080 \
    -p 4369:4369 \
    -e CRE_DASHBOARD_ENABLED=true \
    cre:latest
```

## ⚙️ Configuration

### System Configuration (`sys.config`)

```erlang
% sys.config
[
    {kernel, [
        % Increase process limit
        {system_limits, [{max_processes, 1048576}]},
        % Enable distributed features
        {distributed, [{cre, [{name, cre@node}, {cookie, cre_secret}]}]},
        % Set net kernel tick
        {net_ticktime, 60}
    ]},

    {sasl, [
        % Enable SASL logging
        {sasl_error_logger, {file, "/var/log/cre/sasl.log"}},
        {error_logger_mf_dir, "/var/log/cre"},
        {error_logger_mf_size, 10485760},
        {error_logger_mf_max, 5}
    ]},

    {cre, [
        % Core Configuration
        {app_name, "CRE Workflow Engine"},
        {version, "0.2.1"},
        {log_level, info},

        % Telemetry Configuration
        {telemetry_enabled, true},
        {telemetry_exporter, prometheus},
        {telemetry_interval, 10000},

        % Task Configuration
        {task_timeout, 30},
        {max_retries, 3},
        {worker_timeout, 5000},

        % Dashboard Configuration
        {dashboard, [
            {enabled, true},
            {port, 8080},
            {bind_address, "0.0.0.0"},
            {ssl, [
                {enabled, false},  % Set to true for HTTPS
                {certfile, "/etc/cre/ssl/server.crt"},
                {keyfile, "/etc/cre/ssl/server.key"}
            ]},
            {auth, [
                {enabled, true},
                {provider, jwt},
                {secret, "your-jwt-secret"},
                {token_expiry, 3600}
            ]}
        ]},

        % Human-in-the-Loop Configuration
        {human_in_the_loop, [
            {enabled, true},
            {notification, [
                {email, [
                    {enabled, true},
                    {smtp_server, "smtp.gmail.com"},
                    {smtp_port, 587},
                    {username, "your-email@gmail.com"},
                    {password, "your-password"},
                    {from, "CRE Workflow Engine <noreply@company.com>"}
                ]},
                {slack, [
                    {enabled, false},
                    {webhook_url, "https://hooks.slack.com/services/YOUR/WEBHOOK"}
                ]}
            ]},
            {llm_integration, [
                {enabled, true},
                {provider, openai},
                {api_key, "your-openai-api-key"},
                {model, "gpt-4"},
                {max_tokens, 1000},
                {temperature, 0.3}
            ]}
        ]},

        % Cache Configuration
        {cache, [
            {enabled, true},
            {type, ets},
            {max_size, 1000000},
            {ttl, 3600}
        ]},

        % Database Configuration (if using persistence)
        {database, [
            {type, postgresql},
            {host, "localhost"},
            {port, 5432},
            {database, "cre_workflow"},
            {username, "cre_user"},
            {password, "cre_password"},
            {pool_size, 10},
            {timeout, 30000}
        ]}
    ]}
].
```

### Environment Variables

```bash
# Set environment variables in /etc/default/cre
export CRE_CONFIG_PATH=/etc/cre/sys.config
export CRE_LOG_DIR=/var/log/cre
export CRE_PID_FILE=/var/run/cre.pid
export CRE_USER=creuser
export CRE_GROUP=creuser

# Dashboard settings
export CRE_DASHBOARD_ENABLED=true
export CRE_DASHBOARD_PORT=8080
export CRE_DASHBOARD_SSL_ENABLED=false

# Telemetry settings
export CRE_TELEMETRY_ENABLED=true
export CRE_TELEMETRY_EXPORTER=prometheus
export CRE_OTEL_EXPORTER_URL=http://localhost:4317

# Security settings
export CRE_COOKIE=cre_secret_key
export CRE_NODE_NAME=cre@$(hostname)
```

## 🏢 Production Deployment

### Step 1: Prepare System

```bash
# Create user and directories
sudo useradd -r -s /bin/false creuser
sudo mkdir -p /opt/cre /var/log/cre /var/run/cre /etc/cre
sudo chown -R creuser:creuser /opt/cre /var/log/cre /var/run/cre /etc/cre

# Copy files
sudo cp -r _build/default/rel/cre /opt/cre/
sudo cp sys.config /etc/cre/
sudo cp -r ssl /etc/cre/

# Set permissions
sudo chmod +x /opt/cre/cre/bin/cre
sudo chmod 600 /etc/cre/sys.config
sudo chmod 600 /etc/cre/ssl/*
```

### Step 2: Systemd Service

```bash
# Create systemd service file
cat > /etc/systemd/system/cre.service << 'EOF'
[Unit]
Description=CRE YAWL Workflow Engine
Documentation=https://github.com/your-org/cre
After=network.target network-online.target
Wants=network-online.target

[Service]
Type=simple
User=creuser
Group=creuser
WorkingDirectory=/opt/cre/cre
ExecStart=/opt/cre/cre/bin/cre console
ExecReload=/bin/kill -HUP $MAINPID
PIDFile=/var/run/cre.pid
Restart=on-failure
RestartSec=5s
EnvironmentFile=-/etc/default/cre

# Security settings
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ReadWritePaths=/var/log/cre /var/run/cre
ProtectHome=true
RemoveIPC=true

# Resource limits
LimitNOFILE=1048576
LimitNPROC=1048576

[Install]
WantedBy=multi-user.target
EOF
```

### Step 3: Start Service

```bash
# Reload systemd and start service
sudo systemctl daemon-reload
sudo systemctl enable cre
sudo systemctl start cre

# Check status
sudo systemctl status cre
journalctl -u cre -f
```

## 🔧 Scaling Deployment

### Horizontal Scaling

```mermaid
graph TB
    subgraph "Load Balancer"
        LB[Nginx/HAProxy]
    end

    subgraph "CRE Cluster"
        A[cre@node1] --> LB
        B[cre@node2] --> LB
        C[cre@node3] --> LB
    end

    subgraph "Database"
        D[PostgreSQL] -.-> A
        D -.-> B
        D -.-> C
    end

    subgraph "Message Queue"
        E[RabbitMQ] -.-> A
        E -.-> B
        E -.-> C
    end
```

### Multi-Node Configuration

1. **Generate Cookie File**:
```bash
# Create shared cookie
openssl rand -base64 32 > /etc/cre/cookie
chmod 400 /etc/cre/cookie
```

2. **Update sys.config**:
```erlang
% For each node
{kernel, [
    {distributed, [
        {cre, [
            {name, cre@node1},
            {cookie, 'shared-cookie'}
        ]}
    ]},
    {sync_nodes_mandatory, ['cre@node1', 'cre@node2', 'cre@node3']},
    {net_ticktime, 60}
]}
```

3. **Start Nodes**:
```bash
# On node1
export CRE_NODE_NAME=cre@node1
/opt/cre/cre/bin/cre start

# On node2
export CRE_NODE_NAME=cre@node2
/opt/cre/cre/bin/cre start --cookie-file /etc/cre/cookie

# On node3
export CRE_NODE_NAME=cre@node3
/opt/cre/cre/bin/cre start --cookie-file /etc/cre/cookie
```

### Auto-scaling Workers

```erlang
% Configure worker pool scaling
{cre, [
    {workers, [
        {min_workers, 5},
        {max_workers, 50},
        {worker_strategy, dynamic},
        {scale_threshold, 80},  % Scale at 80% utilization
        {scale_down_interval, 300000},  % 5 minutes
        {max_concurrent_tasks, 1000}
    ]}
]}.
```

## 🔒 Security Hardening

### Network Security

1. **Firewall Configuration**:
```bash
# Open only necessary ports
sudo ufw allow ssh
sudo ufw allow 8080/tcp   # Dashboard
sudo ufw allow 4369/tcp   # EPMD
sudo ufw allow 9100-9200/tcp  # Distributed Erlang
sudo ufw enable
```

2. **SSL/TLS Configuration**:
```erlang
% Enable HTTPS for dashboard
{dashboard, [
    {ssl, [
        {enabled, true},
        {certfile, "/etc/cre/ssl/server.crt"},
        {keyfile, "/etc/cre/ssl/server.key"},
        {cafile, "/etc/cre/ssl/ca.crt"},
        {verify, verify_none},
        {versions, [tlsv1.2, tlsv1.3]},
        {ciphers, ssl:cipher_suites('tlsv1.2', 'ecdhe_rsa', 'aes_256_gcm')}
    ]}
]}.
```

### Application Security

1. **Authentication**:
```erlang
{dashboard, [
    {auth, [
        {enabled, true},
        {provider, oauth2},
        {oauth2_config, [
            {provider, azure_ad},
            {client_id, "your-client-id"},
            {client_secret, "your-client-secret"},
            {tenant_id, "your-tenant-id"},
            {scopes, ["openid", "profile", "email"]}
        ]}
    ]}
]}.
```

2. **API Keys**:
```erlang
{api, [
    {enabled, true},
    {auth_type, api_key},
    {api_key_header, "X-API-Key"},
    {rate_limit, [
        {enabled, true},
        {requests_per_minute, 100}
    ]}
]}.
```

### Database Security

1. **Connection Security**:
```erlang
{database, [
    {ssl, true},
    {ssl_opts, [{verify, verify_none}]},
    {pool_size, 10},
    {max_overflow, 20}
]}.
```

2. **Backups**:
```bash
# Daily backup script
cat > /usr/local/bin/cre-backup << 'EOF'
#!/bin/bash
BACKUP_DIR="/var/backups/cre"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p $BACKUP_DIR

# Backup workflow definitions
/opt/cre/cre/bin/cre backup-workflows $BACKUP_DIR/workflows_$DATE.json.gz

# Backup XES logs
find /var/log/cre -name "*.xes" -type f -exec gzip {} \; -exec mv {}.gz $BACKUP_DIR/ \;

# Rotate backups
find $BACKUP_DIR -name "*.json.gz" -mtime +30 -delete
find $BACKUP_DIR -name "*.xes.gz" -mtime +7 -delete

# Upload to S3 (optional)
if command -v aws &> /dev/null; then
    aws s3 sync $BACKUP_DIR s3://your-backup-bucket/cre/
fi

EOF

chmod +x /usr/local/bin/cre-backup
```

## 📊 Monitoring & Observability

### Prometheus Configuration

```yaml
# prometheus.yml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'cre-dashboard'
    static_configs:
      - targets: ['localhost:8080']
    metrics_path: '/metrics'
    scrape_interval: 10s

  - job_name: 'cre-otlp'
    static_configs:
      - targets: ['localhost:4317']
    scrape_interval: 30s
```

### Grafana Dashboard

Import the provided Grafana dashboard with the following panels:

1. **Workflows Executed**
   - Prometheus query: `cre_workflows_total`

2. **Task Success Rate**
   - Prometheus query: `rate(cre_task_success_total[5m]) / rate(cre_task_total[5m])`

3. **Response Time (p95)**
   - Prometheus query: `histogram_quantile(0.95, rate(cre_task_duration_seconds_bucket[5m]))`

4. **Worker Utilization**
   - Prometheus query: `rate(cre_worker_tasks_total[5m])`

### Logging Configuration

```erlang
% Enhanced logging
{logger, [
    {handler, default, logger_std_h,
        {formatter, logger_formatter, [
            {template, "~p [~p] ~p: ~p~n"},
            {metadata, [time, level, pid, node, component, workflow_id]}
        ]}},
    {handler, file, logger_disk_log_h,
        {formatter, logger_json_formatter, []},
        {config, [
            {file, "/var/log/cre/cre.log"},
            {type, wrap},
            {max_no_bytes, 10485760},
            {max_no_files, 5}
        ]}}
]}.
```

## 🚀 Performance Tuning

### Erlang VM Tuning

```erlang
% vm.args
-name cre@node
-setcookie cre_secret
-vm_memory_reservation 0.6
-vm_memory_limit 2GB
+pc unicode
+K true
+A 128
+Q 65536
+P 2097152
+zdbbl 2097152
+zdbbl 2097152

# For high-load systems
+sssd true +sssd_cpu 8 +sssd_io 8 +sssd_ncpu 8
```

### System Tuning

```bash
# /etc/sysctl.conf
# Increase file descriptor limits
fs.file-max = 1048576

# Increase network buffer size
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
net.ipv4.tcp_rmem = 4096 87380 16777216
net.ipv4.tcp_wmem = 4096 65536 16777216

# TCP keepalive
net.ipv4.tcp_keepalive_time = 60
net.ipv4.tcp_keepalive_intvl = 10
net.ipv4.tcp_keepalive_probes = 6

# Apply changes
sudo sysctl -p
```

### Kernel Tuning

```bash
# /etc/security/limits.conf
creuser soft nofile 1048576
creuser hard nofile 1048576
creuser soft nproc 1048576
creuser hard nproc 1048576
```

## 🔧 Maintenance Operations

### Health Checks

```bash
# Health check script
cat > /usr/local/bin/cre-healthcheck << 'EOF'
#!/bin/bash

# Check dashboard accessibility
if ! curl -f http://localhost:8080/health > /dev/null 2>&1; then
    echo "Health check failed"
    exit 1
fi

# Check database connectivity
if ! pg_isready -h localhost -U cre_user > /dev/null 2>&1; then
    echo "Database check failed"
    exit 1
fi

# Check memory usage
MEMORY_USAGE=$(ps -p $(pidof beam.smp) -o %mem --no-headers)
if (( $(echo "$MEMORY_USAGE > 90" | bc -l) )); then
    echo "High memory usage: ${MEMORY_USAGE}%"
    exit 1
fi

echo "All health checks passed"
exit 0
EOF

chmod +x /usr/local/bin/cre-healthcheck
```

### Backup and Recovery

```bash
# Full backup script
cat > /usr/local/bin/cre-backup-full << 'EOF'
#!/bin/bash
BACKUP_DIR="/var/backups/cre"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="cre_backup_$DATE.tar.gz"

# Create backup directory
mkdir -p $BACKUP_DIR

# System backup
cd /opt/cre/cre
tar -czf $BACKUP_DIR/$BACKUP_FILE .

# Database backup (if configured)
if [ -n "$DATABASE_URL" ]; then
    pg_dump $DATABASE_URL | gzip > $BACKUP_DIR/database_$DATE.sql.gz
fi

# Configuration backup
tar -czf $BACKUP_DIR/config_$DATE.tar.gz /etc/cre/

# Cleanup old backups (keep 7 days)
find $BACKUP_DIR -name "*.tar.gz" -mtime +7 -delete

echo "Backup completed: $BACKUP_DIR/$BACKUP_FILE"
EOF

chmod +x /usr/local/bin/cre-backup-full
```

### Rolling Updates

```bash
# Rolling update script
cat > /usr/local/bin/cre-rolling-update << 'EOF'
#!/bin/bash

NEW_VERSION=$1
CLUSTER_NODES=("cre@node1" "cre@node2" "cre@node3")

for node in "${CLUSTER_NODES[@]}"; do
    echo "Updating node: $node"

    # Stop the node
    ssh $node "sudo systemctl stop cre"

    # Update code
    ssh $node "cd /opt/cre && git pull origin main && rebar3 release"

    # Start the node
    ssh $node "sudo systemctl start cre"

    # Wait for health check
    until curl -f http://$(echo $node | sed 's/cre@//'):8080/health > /dev/null 2>&1; do
        echo "Waiting for node $node to be healthy..."
        sleep 10
    done

    echo "Node $node updated successfully"
done

echo "Rolling update completed"
EOF

chmod +x /usr/local/bin/cre-rolling-update
```

## 🚨 Troubleshooting

### Common Issues

1. **Node Connection Failure**
```bash
# Check cookie consistency
ls -la /etc/cre/cookie
# Ensure all nodes have the same cookie

# Check EPMD status
epmd -names
```

2. **Memory Issues**
```bash
# Monitor memory usage
ps -p $(pidof beam.smp) -o pid,%mem,cmd

# Check crash dumps
ls /var/log/cre/erl_crash.dump
```

3. **Performance Degradation**
```bash
# Check system resources
htop
iotop

# Check database queries
psql -c "SELECT query, calls, total_time FROM pg_stat_statements ORDER BY total_time DESC LIMIT 10;"
```

### Log Analysis

```bash
# Follow logs
journalctl -u cre -f

# Analyze XES logs
find /var/log/cre -name "*.xes" -exec grep -H "error" {} \;

# Pattern analysis for common failures
grep "timeout" /var/log/cre/cre.log | head -20
grep "worker.*failed" /var/log/cre/cre.log | head -20
```

---

This deployment guide provides comprehensive instructions for deploying CRE in production environments. For additional help, refer to the [API Reference](./API_REFERENCE.md) and [Architecture Overview](./ARCHITECTURE.md).