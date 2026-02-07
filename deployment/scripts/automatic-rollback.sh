#!/bin/bash

################################################################################
# Automatic Rollback Trigger Script
# Monitors deployment health and automatically triggers rollback on anomalies
#
# Usage:
#   ./automatic-rollback.sh [options]
#
# Options:
#   --deployment-id        ID of the deployment to monitor
#   --check-interval       Health check interval in seconds (default: 5)
#   --monitoring-duration  Total monitoring duration in seconds (default: 300)
#   --error-rate-threshold Error rate threshold for rollback (default: 0.10)
#   --latency-threshold    Latency threshold in ms for rollback (default: 5000)
#   --consecutive-failures Consecutive failures before rollback (default: 3)
#   --namespace            Kubernetes namespace (default: default)
################################################################################

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Configuration
DEPLOYMENT_ID="${1:-}"
CHECK_INTERVAL=${CHECK_INTERVAL:-5}
MONITORING_DURATION=${MONITORING_DURATION:-300}
ERROR_RATE_THRESHOLD=${ERROR_RATE_THRESHOLD:-0.10}
LATENCY_THRESHOLD=${LATENCY_THRESHOLD:-5000}
CONSECUTIVE_FAILURES=${CONSECUTIVE_FAILURES:-3}
NAMESPACE=${NAMESPACE:-"default"}

# State tracking
MONITOR_ID="monitor-$(date +%s)"
LOG_FILE="/tmp/monitor-${MONITOR_ID}.log"
STATE_FILE="/tmp/rollback-triggers.state"

# Trigger counters
declare -A FAILURE_COUNTERS
declare -A LAST_CHECK_TIME

# Parse arguments
while [[ $# -gt 1 ]]; do
    case $2 in
        --deployment-id) DEPLOYMENT_ID=$3; shift 2 ;;
        --check-interval) CHECK_INTERVAL=$3; shift 2 ;;
        --monitoring-duration) MONITORING_DURATION=$3; shift 2 ;;
        --error-rate-threshold) ERROR_RATE_THRESHOLD=$3; shift 2 ;;
        --latency-threshold) LATENCY_THRESHOLD=$3; shift 2 ;;
        --consecutive-failures) CONSECUTIVE_FAILURES=$3; shift 2 ;;
        --namespace) NAMESPACE=$3; shift 2 ;;
        *) echo "Unknown option: $2"; exit 1 ;;
    esac
done

# Error handling
trap 'cleanup' EXIT INT TERM

cleanup() {
    log_info "Cleanup: Stopping monitoring"
}

# Logging functions
log_info() {
    local msg="$1"
    echo -e "${BLUE}[INFO]${NC} $msg" | tee -a "$LOG_FILE"
}

log_success() {
    local msg="$1"
    echo -e "${GREEN}[SUCCESS]${NC} $msg" | tee -a "$LOG_FILE"
}

log_warn() {
    local msg="$1"
    echo -e "${YELLOW}[WARNING]${NC} $msg" | tee -a "$LOG_FILE"
}

log_error() {
    local msg="$1"
    echo -e "${RED}[ERROR]${NC} $msg" | tee -a "$LOG_FILE"
}

# Metric collection and evaluation
collect_deployment_metrics() {
    # Query Prometheus or similar monitoring system
    # Returns JSON with error_rate, latency, throughput, etc.

    local error_rate=$(query_error_rate)
    local latency=$(query_latency)
    local throughput=$(query_throughput)
    local cpu_usage=$(query_cpu_usage)
    local memory_usage=$(query_memory_usage)

    echo "{
        \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",
        \"error_rate\": $error_rate,
        \"latency\": $latency,
        \"throughput\": $throughput,
        \"cpu_usage\": $cpu_usage,
        \"memory_usage\": $memory_usage
    }"
}

query_error_rate() {
    # Query from Prometheus
    # Example: curl -s 'http://prometheus:9090/api/v1/query?query=rate(http_requests_total%5B1m%5D)'
    # For demo, return simulated value
    local base_rate=0.02
    local variation=$(awk "BEGIN {print (rand() - 0.5) * 0.02}")
    awk "BEGIN {printf \"%.4f\", $base_rate + $variation}"
}

query_latency() {
    # Query P99 latency from monitoring
    local base_latency=100
    local variation=$(awk "BEGIN {print (rand() - 0.5) * 50}")
    awk "BEGIN {printf \"%.0f\", $base_latency + $variation}"
}

query_throughput() {
    # Query requests per second
    local base_throughput=1000
    local variation=$(awk "BEGIN {print (rand() - 0.5) * 200}")
    awk "BEGIN {printf \"%.0f\", $base_throughput + $variation}"
}

query_cpu_usage() {
    # Query CPU usage percentage
    awk "BEGIN {printf \"%.1f\", rand() * 50}"
}

query_memory_usage() {
    # Query memory usage percentage
    awk "BEGIN {printf \"%.1f\", 40 + rand() * 20}"
}

# Health check functions
check_deployment_health() {
    local deployment=$1

    # Check pod status
    local ready_replicas=$(kubectl get deployment "$deployment" -n "$NAMESPACE" \
        -o jsonpath='{.status.readyReplicas}' 2>/dev/null || echo "0")
    local desired_replicas=$(kubectl get deployment "$deployment" -n "$NAMESPACE" \
        -o jsonpath='{.spec.replicas}' 2>/dev/null || echo "0")

    if [[ "$ready_replicas" -lt "$desired_replicas" ]]; then
        return 1
    fi

    return 0
}

check_service_availability() {
    local service=$1

    # Check if service has endpoints
    local endpoints=$(kubectl get endpoints "$service" -n "$NAMESPACE" \
        -o jsonpath='{.subsets[0].addresses}' 2>/dev/null | wc -w || echo "0")

    if [[ "$endpoints" -eq 0 ]]; then
        return 1
    fi

    return 0
}

check_database_health() {
    # Check database connectivity
    # In real implementation, run actual health check queries
    return 0
}

check_dependencies() {
    # Check external service dependencies
    local dependencies=("redis" "elasticsearch" "external-api")

    for dep in "${dependencies[@]}"; do
        if ! kubectl get service "$dep" -n "$NAMESPACE" &>/dev/null; then
            log_warn "Dependency $dep not found"
            return 1
        fi
    done

    return 0
}

# Anomaly detection
detect_anomalies() {
    local metrics=$1

    local error_rate=$(echo "$metrics" | jq -r '.error_rate')
    local latency=$(echo "$metrics" | jq -r '.latency')
    local cpu_usage=$(echo "$metrics" | jq -r '.cpu_usage')
    local memory_usage=$(echo "$metrics" | jq -r '.memory_usage')

    local anomalies=()

    # Check error rate
    if (( $(echo "$error_rate > $ERROR_RATE_THRESHOLD" | bc -l) )); then
        anomalies+=("ERROR_RATE_HIGH: ${error_rate} > ${ERROR_RATE_THRESHOLD}")
        increment_failure_counter "error_rate"
    else
        reset_failure_counter "error_rate"
    fi

    # Check latency
    if (( $(echo "$latency > $LATENCY_THRESHOLD" | bc -l) )); then
        anomalies+=("LATENCY_HIGH: ${latency}ms > ${LATENCY_THRESHOLD}ms")
        increment_failure_counter "latency"
    else
        reset_failure_counter "latency"
    fi

    # Check resource usage
    if (( $(echo "$cpu_usage > 80" | bc -l) )); then
        anomalies+=("CPU_HIGH: ${cpu_usage}%")
        increment_failure_counter "cpu_usage"
    else
        reset_failure_counter "cpu_usage"
    fi

    if (( $(echo "$memory_usage > 85" | bc -l) )); then
        anomalies+=("MEMORY_HIGH: ${memory_usage}%")
        increment_failure_counter "memory_usage"
    else
        reset_failure_counter "memory_usage"
    fi

    for anomaly in "${anomalies[@]}"; do
        echo "$anomaly"
    done
}

increment_failure_counter() {
    local trigger=$1
    FAILURE_COUNTERS[$trigger]=$((${FAILURE_COUNTERS[$trigger]:-0} + 1))
}

reset_failure_counter() {
    local trigger=$1
    FAILURE_COUNTERS[$trigger]=0
}

should_trigger_rollback() {
    for trigger in "${!FAILURE_COUNTERS[@]}"; do
        if [[ ${FAILURE_COUNTERS[$trigger]} -ge $CONSECUTIVE_FAILURES ]]; then
            return 0
        fi
    done

    return 1
}

# Rollback execution
trigger_rollback() {
    local reason="$1"
    local current_deployment="$2"
    local previous_deployment="$3"

    log_error "ROLLBACK TRIGGERED: $reason"

    # Get previous version
    local previous_version=$(kubectl get deployment "$previous_deployment" -n "$NAMESPACE" \
        -o jsonpath='{.spec.template.metadata.labels.version}' 2>/dev/null || echo "unknown")

    log_warn "Executing automatic rollback to v${previous_version}"

    # Execute rollback script
    if [[ -x "$(dirname "$0")/rollback.sh" ]]; then
        bash "$(dirname "$0")/rollback.sh" "$previous_deployment" "$NAMESPACE"
    else
        log_error "Rollback script not found or not executable"
        return 1
    fi

    log_success "Rollback completed"
}

# Monitoring loop
start_health_monitoring() {
    local start_time=$(date +%s)
    local end_time=$((start_time + MONITORING_DURATION))

    log_info "Starting health monitoring for ${MONITORING_DURATION}s"
    log_info "Check interval: ${CHECK_INTERVAL}s"
    log_info "Error rate threshold: $ERROR_RATE_THRESHOLD"
    log_info "Latency threshold: ${LATENCY_THRESHOLD}ms"

    while [[ $(date +%s) -lt $end_time ]]; do
        # Collect metrics
        local metrics=$(collect_deployment_metrics)

        # Detect anomalies
        local anomalies=$(detect_anomalies "$metrics")

        if [[ -n "$anomalies" ]]; then
            log_warn "Anomalies detected:"
            while IFS= read -r anomaly; do
                log_warn "  - $anomaly"
            done <<< "$anomalies"
        else
            log_info "Health check passed"
        fi

        # Check overall health
        local failure_count=0
        for trigger in "${!FAILURE_COUNTERS[@]}"; do
            if [[ ${FAILURE_COUNTERS[$trigger]} -gt 0 ]]; then
                failure_count=$((failure_count + 1))
            fi
        done

        if [[ $failure_count -gt 0 ]]; then
            log_warn "Active failure triggers: $failure_count"
        fi

        # Check if rollback should be triggered
        if should_trigger_rollback; then
            local triggered_triggers=""
            for trigger in "${!FAILURE_COUNTERS[@]}"; do
                if [[ ${FAILURE_COUNTERS[$trigger]} -ge $CONSECUTIVE_FAILURES ]]; then
                    triggered_triggers="$triggered_triggers $trigger(${FAILURE_COUNTERS[$trigger]})"
                fi
            done

            trigger_rollback "Consecutive failures: $triggered_triggers" "current" "previous"
            return 1
        fi

        # Health checks
        if ! check_deployment_health "app"; then
            log_error "Deployment health check failed"
            trigger_rollback "Deployment health check failed" "current" "previous"
            return 1
        fi

        if ! check_database_health; then
            log_error "Database health check failed"
            trigger_rollback "Database health check failed" "current" "previous"
            return 1
        fi

        if ! check_dependencies; then
            log_warn "Dependency check failed"
        fi

        # Wait for next check
        sleep "$CHECK_INTERVAL"
    done

    log_success "Monitoring completed without issues"
    return 0
}

# Reporting
generate_monitoring_report() {
    local end_time=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local report_file="/tmp/monitor-${MONITOR_ID}-report.md"

    cat > "$report_file" <<EOF
# Deployment Health Monitoring Report

**Monitor ID**: $MONITOR_ID
**Monitoring Duration**: ${MONITORING_DURATION}s
**Check Interval**: ${CHECK_INTERVAL}s
**Report Generated**: $end_time

## Thresholds
- Error Rate: $ERROR_RATE_THRESHOLD
- Latency: ${LATENCY_THRESHOLD}ms
- Consecutive Failures Before Rollback: $CONSECUTIVE_FAILURES

## Status
- Monitoring: COMPLETED
- Rollback Required: NO

## Failure Counters
EOF

    for trigger in "${!FAILURE_COUNTERS[@]}"; do
        echo "- $trigger: ${FAILURE_COUNTERS[$trigger]}" >> "$report_file"
    done

    log_success "Report generated: $report_file"
}

# Main execution
main() {
    log_info "================================"
    log_info "Automatic Rollback Monitor"
    log_info "================================"
    log_info "Monitor ID: $MONITOR_ID"
    [[ -n "$DEPLOYMENT_ID" ]] && log_info "Deployment ID: $DEPLOYMENT_ID"
    log_info "Namespace: $NAMESPACE"

    # Start monitoring
    if start_health_monitoring; then
        generate_monitoring_report
        log_success "Monitoring completed successfully"
    else
        log_error "Monitoring ended with errors"
        exit 1
    fi
}

# Execute main function
main "$@"
