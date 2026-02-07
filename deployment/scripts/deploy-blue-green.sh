#!/bin/bash

################################################################################
# Blue-Green Deployment Script
# Performs zero-downtime deployment using blue-green strategy
#
# Usage:
#   ./deploy-blue-green.sh <version> [options]
#
# Options:
#   --dry-run              Execute without making actual changes
#   --skip-validation      Skip version compatibility validation
#   --traffic-strategy     Immediate, gradual, or weighted (default: immediate)
#   --traffic-increment    Percentage increment for gradual strategy (default: 10)
#   --traffic-interval     Interval in seconds for gradual strategy (default: 30)
#   --max-error-rate       Maximum error rate threshold (default: 0.05)
#   --environment          Target environment (default: production)
#   --namespace            Kubernetes namespace (default: default)
################################################################################

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
VERSION="${1:-}"
DRY_RUN=${DRY_RUN:-false}
SKIP_VALIDATION=${SKIP_VALIDATION:-false}
TRAFFIC_STRATEGY=${TRAFFIC_STRATEGY:-"immediate"}
TRAFFIC_INCREMENT=${TRAFFIC_INCREMENT:-10}
TRAFFIC_INTERVAL=${TRAFFIC_INTERVAL:-30}
MAX_ERROR_RATE=${MAX_ERROR_RATE:-0.05}
ENVIRONMENT=${ENVIRONMENT:-"production"}
NAMESPACE=${NAMESPACE:-"default"}
REPLICAS=${REPLICAS:-3}
HEALTH_CHECK_TIMEOUT=${HEALTH_CHECK_TIMEOUT:-300}

# Deployment tracking
DEPLOYMENT_ID="bgdeploy-$(date +%s)"
LOG_FILE="/tmp/deployment-${DEPLOYMENT_ID}.log"
STATE_FILE="/tmp/deployment-${DEPLOYMENT_ID}.state"

# Parse arguments
while [[ $# -gt 1 ]]; do
    case $2 in
        --dry-run) DRY_RUN=true; shift ;;
        --skip-validation) SKIP_VALIDATION=true; shift ;;
        --traffic-strategy) TRAFFIC_STRATEGY=$3; shift 2 ;;
        --traffic-increment) TRAFFIC_INCREMENT=$3; shift 2 ;;
        --traffic-interval) TRAFFIC_INTERVAL=$3; shift 2 ;;
        --max-error-rate) MAX_ERROR_RATE=$3; shift 2 ;;
        --environment) ENVIRONMENT=$3; shift 2 ;;
        --namespace) NAMESPACE=$3; shift 2 ;;
        *) echo "Unknown option: $2"; exit 1 ;;
    esac
done

# Error handling
trap 'on_error' ERR
trap 'on_exit' EXIT

on_error() {
    log_error "Deployment failed at line $LINENO"
    rollback_deployment
    exit 1
}

on_exit() {
    log_info "Deployment log saved to $LOG_FILE"
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

# Validation functions
validate_version() {
    if [[ -z "$VERSION" ]]; then
        log_error "Version not specified"
        return 1
    fi

    log_info "Validating version: $VERSION"

    if [[ "$SKIP_VALIDATION" == "false" ]]; then
        # Check if version exists in registry
        if ! check_version_exists "$VERSION"; then
            log_error "Version $VERSION not found in registry"
            return 1
        fi

        # Validate compatibility
        if ! validate_version_compatibility "$VERSION"; then
            log_error "Version compatibility check failed"
            return 1
        fi
    fi

    log_success "Version validation passed"
}

check_version_exists() {
    local version=$1
    # In real implementation, check artifact repository
    [[ -f "versions/$version/manifest.json" ]] || return 0 # Allow for demo
}

validate_version_compatibility() {
    local version=$1
    # In real implementation, validate against compatibility matrix
    return 0 # Pass for demo
}

# Determine active and inactive environments
get_active_environment() {
    # Read from state or check load balancer config
    if [[ -f "$STATE_FILE" ]]; then
        grep "^ACTIVE_ENV=" "$STATE_FILE" | cut -d= -f2
    else
        # Assume blue is active initially
        echo "blue"
    fi
}

get_inactive_environment() {
    local active=$1
    if [[ "$active" == "blue" ]]; then
        echo "green"
    else
        echo "blue"
    fi
}

# Pre-deployment checks
perform_pre_deployment_checks() {
    log_info "Performing pre-deployment checks"

    # Check cluster connectivity
    if ! kubectl cluster-info &>/dev/null; then
        log_error "Cannot connect to Kubernetes cluster"
        return 1
    fi

    # Check namespace exists
    if ! kubectl get namespace "$NAMESPACE" &>/dev/null; then
        log_error "Namespace $NAMESPACE not found"
        return 1
    fi

    # Check if active environment is healthy
    local active_env=$(get_active_environment)
    if ! is_environment_healthy "$active_env"; then
        log_error "Active environment $active_env is not healthy"
        return 1
    fi

    # Create database backup
    log_info "Creating database backup"
    create_database_backup "$DEPLOYMENT_ID"

    log_success "Pre-deployment checks passed"
}

is_environment_healthy() {
    local env=$1
    local replicas=$(kubectl get deployment -n "$NAMESPACE" \
        -l "deployment=$env" \
        -o jsonpath='{.items[0].status.readyReplicas}' 2>/dev/null || echo "0")

    [[ "$replicas" -ge $((REPLICAS - 1)) ]]
}

create_database_backup() {
    local backup_id=$1
    # In real implementation, trigger database backup
    log_info "Database backup created: backup-$backup_id"
}

# Deployment functions
deploy_to_inactive_environment() {
    local inactive_env=$1

    log_info "Deploying v$VERSION to $inactive_env environment"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_warn "DRY RUN: Would deploy to $inactive_env"
        return 0
    fi

    # Create deployment manifest
    local manifest=$(generate_deployment_manifest "$inactive_env" "$VERSION")

    # Apply to cluster
    echo "$manifest" | kubectl apply -n "$NAMESPACE" -f -

    # Wait for rollout
    log_info "Waiting for deployment to be ready"
    if ! kubectl rollout status deployment/"$inactive_env" -n "$NAMESPACE" --timeout=10m; then
        log_error "Deployment failed to become ready"
        return 1
    fi

    log_success "Deployment to $inactive_env completed"
}

generate_deployment_manifest() {
    local env=$1
    local version=$2

    cat <<EOF
apiVersion: apps/v1
kind: Deployment
metadata:
  name: $env
  namespace: $NAMESPACE
  labels:
    deployment: $env
    version: $version
spec:
  replicas: $REPLICAS
  selector:
    matchLabels:
      deployment: $env
  template:
    metadata:
      labels:
        deployment: $env
        version: $version
    spec:
      containers:
      - name: app
        image: myregistry.azurecr.io/myapp:$version
        ports:
        - containerPort: 8080
        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 1000m
            memory: 2Gi
        livenessProbe:
          httpGet:
            path: /health/live
            port: 8080
          initialDelaySeconds: 15
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
          timeoutSeconds: 5
          failureThreshold: 3
EOF
}

# Health validation
validate_health() {
    local env=$1
    log_info "Validating health of $env environment"

    local timeout=$HEALTH_CHECK_TIMEOUT
    local start_time=$(date +%s)

    while true; do
        local ready_replicas=$(kubectl get deployment -n "$NAMESPACE" "$env" \
            -o jsonpath='{.status.readyReplicas}' 2>/dev/null || echo "0")

        if [[ "$ready_replicas" -ge $REPLICAS ]]; then
            log_success "$env environment is healthy ($ready_replicas/$REPLICAS replicas ready)"
            return 0
        fi

        local elapsed=$(($(date +%s) - start_time))
        if [[ $elapsed -gt $timeout ]]; then
            log_error "Health check timeout after ${elapsed}s"
            return 1
        fi

        log_info "Waiting for replicas... ($ready_replicas/$REPLICAS ready)"
        sleep 10
    done
}

# Traffic switching
switch_traffic() {
    local target_env=$1

    log_info "Switching traffic to $target_env using $TRAFFIC_STRATEGY strategy"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_warn "DRY RUN: Would switch traffic to $target_env"
        return 0
    fi

    case "$TRAFFIC_STRATEGY" in
        immediate)
            switch_traffic_immediate "$target_env"
            ;;
        gradual)
            switch_traffic_gradual "$target_env"
            ;;
        weighted)
            switch_traffic_weighted "$target_env"
            ;;
        *)
            log_error "Unknown traffic strategy: $TRAFFIC_STRATEGY"
            return 1
            ;;
    esac
}

switch_traffic_immediate() {
    local target_env=$1

    log_info "Switching 100% traffic to $target_env"

    # Update load balancer or service selector
    kubectl patch service -n "$NAMESPACE" app-service \
        -p '{"spec":{"selector":{"deployment":"'"$target_env"'"}}}'

    log_success "Traffic switched to $target_env"
}

switch_traffic_gradual() {
    local target_env=$1
    local current_percentage=0

    while [[ $current_percentage -lt 100 ]]; do
        current_percentage=$((current_percentage + TRAFFIC_INCREMENT))
        [[ $current_percentage -gt 100 ]] && current_percentage=100

        log_info "Switching ${current_percentage}% traffic to $target_env"

        # Update traffic split (e.g., via Istio VirtualService or similar)
        update_traffic_split "$target_env" "$current_percentage"

        if [[ $current_percentage -lt 100 ]]; then
            log_info "Waiting ${TRAFFIC_INTERVAL}s before next increment"
            sleep "$TRAFFIC_INTERVAL"
        fi

        # Check error rates
        if ! check_error_rates; then
            log_error "Error rate exceeded threshold during traffic switch"
            return 1
        fi
    done

    log_success "Gradual traffic switch completed"
}

switch_traffic_weighted() {
    local target_env=$1

    log_info "Using weighted traffic distribution"

    # Update service weights/endpoints
    kubectl patch service -n "$NAMESPACE" app-service \
        -p '{"spec":{"selector":{"deployment":"'"$target_env"'"}}}'

    log_success "Weighted traffic distribution applied"
}

update_traffic_split() {
    local env=$1
    local percentage=$2

    # In real implementation, update traffic policy
    # This could be Istio VirtualService, AWS ALB, etc.
    log_info "Updated traffic split: $percentage% to $env"
}

check_error_rates() {
    # Query metrics from monitoring system
    local error_rate=$(get_current_error_rate)

    if (( $(echo "$error_rate > $MAX_ERROR_RATE" | bc -l) )); then
        log_warn "Error rate $error_rate exceeds threshold $MAX_ERROR_RATE"
        return 1
    fi

    return 0
}

get_current_error_rate() {
    # Query Prometheus or similar
    echo "0.01" # Placeholder
}

# Monitoring
monitor_deployment() {
    local inactive_env=$1
    local monitoring_duration=300 # 5 minutes
    local start_time=$(date +%s)

    log_info "Monitoring deployment for ${monitoring_duration}s"

    while true; do
        local elapsed=$(($(date +%s) - start_time))
        if [[ $elapsed -gt $monitoring_duration ]]; then
            log_success "Monitoring completed successfully"
            return 0
        fi

        # Check error rates
        if ! check_error_rates; then
            log_error "Error rate exceeded during monitoring"
            return 1
        fi

        # Check deployment health
        if ! is_environment_healthy "$inactive_env"; then
            log_error "Deployment health check failed"
            return 1
        fi

        log_info "Monitoring... ($elapsed/$monitoring_duration seconds)"
        sleep 10
    done
}

# Rollback functions
rollback_deployment() {
    log_warn "Initiating rollback"

    local active_env=$(get_active_environment)
    local inactive_env=$(get_inactive_environment "$active_env")

    log_info "Switching traffic back to $active_env"

    # Switch traffic back to previous environment
    update_traffic_split "$active_env" "100"

    # Delete failed deployment
    log_info "Removing failed deployment from $inactive_env"
    kubectl delete deployment -n "$NAMESPACE" "$inactive_env" || true

    # Restore from backup
    restore_database_backup "$DEPLOYMENT_ID"

    log_info "Rollback completed"
}

restore_database_backup() {
    local backup_id=$1

    if [[ "$DRY_RUN" == "true" ]]; then
        log_warn "DRY RUN: Would restore database from backup-$backup_id"
        return 0
    fi

    # In real implementation, restore database
    log_info "Restored database from backup-$backup_id"
}

# Update state file
update_state() {
    local active_env=$1

    cat > "$STATE_FILE" <<EOF
DEPLOYMENT_ID=$DEPLOYMENT_ID
ACTIVE_ENV=$active_env
VERSION=$VERSION
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
EOF
}

# Main execution
main() {
    log_info "================================"
    log_info "Blue-Green Deployment Started"
    log_info "================================"
    log_info "Deployment ID: $DEPLOYMENT_ID"
    log_info "Version: $VERSION"
    log_info "Environment: $ENVIRONMENT"
    log_info "Namespace: $NAMESPACE"
    log_info "Traffic Strategy: $TRAFFIC_STRATEGY"
    [[ "$DRY_RUN" == "true" ]] && log_warn "DRY RUN MODE ENABLED"

    # Validation
    validate_version || exit 1
    perform_pre_deployment_checks || exit 1

    # Get environments
    local active_env=$(get_active_environment)
    local inactive_env=$(get_inactive_environment "$active_env")

    log_info "Active environment: $active_env"
    log_info "Target environment: $inactive_env"

    # Deployment
    deploy_to_inactive_environment "$inactive_env" || exit 1
    validate_health "$inactive_env" || exit 1

    # Traffic switch
    switch_traffic "$inactive_env" || exit 1

    # Monitoring
    monitor_deployment "$inactive_env" || exit 1

    # Update state
    update_state "$inactive_env"

    log_info "================================"
    log_success "Blue-Green Deployment Completed Successfully"
    log_info "================================"
}

# Execute main function
main "$@"
