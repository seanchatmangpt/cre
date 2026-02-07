#!/bin/bash
#
# Enterprise Application Deployer Script
# Handles deployment, validation, and post-deployment configuration
#

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Environment validation
validate_environment() {
    log_info "Validating deployment environment..."

    # Check required environment variables
    local required_vars=(
        "NAME"
        "NAMESPACE"
        "DEPLOYER_SERVICE_ACCOUNT"
        "APP_SERVICE_ACCOUNT"
        "METERING_SERVICE_ACCOUNT"
        "REPORTING_SECRET"
    )

    for var in "${required_vars[@]}"; do
        if [[ -z "${!var:-}" ]]; then
            log_error "Required environment variable $var is not set"
            return 1
        fi
    done

    log_info "Environment validation passed"
    return 0
}

# Pre-deployment checks
pre_deployment_checks() {
    log_info "Running pre-deployment checks..."

    # Check cluster connectivity
    if ! kubectl cluster-info &>/dev/null; then
        log_error "Cannot connect to Kubernetes cluster"
        return 1
    fi

    # Check namespace
    if ! kubectl get namespace "$NAMESPACE" &>/dev/null; then
        log_info "Creating namespace $NAMESPACE"
        kubectl create namespace "$NAMESPACE"
    fi

    # Verify RBAC permissions
    if ! kubectl auth can-i create deployments -n "$NAMESPACE" --as="system:serviceaccount:$NAMESPACE:$DEPLOYER_SERVICE_ACCOUNT" &>/dev/null; then
        log_error "Insufficient RBAC permissions for deployer service account"
        return 1
    fi

    log_info "Pre-deployment checks passed"
    return 0
}

# Deploy RBAC resources
deploy_rbac() {
    log_info "Deploying RBAC resources..."

    # Apply RBAC manifests with variable substitution
    for manifest in /data/rbac/*.yaml; do
        log_info "Applying $(basename "$manifest")"
        envsubst < "$manifest" | kubectl apply -f -
    done

    log_info "RBAC resources deployed successfully"
}

# Deploy application resources
deploy_application() {
    log_info "Deploying application resources..."

    # Create admin credentials secret
    local admin_password
    admin_password=$(openssl rand -base64 32)

    kubectl create secret generic "${NAME}-admin-credentials" \
        --namespace="$NAMESPACE" \
        --from-literal=username=admin \
        --from-literal=password="$admin_password" \
        --dry-run=client -o yaml | kubectl apply -f -

    log_info "Admin password: $admin_password"

    # Apply Kubernetes manifests with variable substitution
    for manifest in /data/k8s/*.yaml; do
        log_info "Applying $(basename "$manifest")"
        envsubst < "$manifest" | kubectl apply -f -
    done

    log_info "Application resources deployed successfully"
}

# Deploy billing agent
deploy_billing_agent() {
    log_info "Deploying usage metering agent..."

    # Apply billing configuration
    envsubst < /data/billing/usage-metering-agent.yaml | kubectl apply -f -

    log_info "Usage metering agent deployed successfully"
}

# Wait for resources to be ready
wait_for_ready() {
    log_info "Waiting for application to be ready..."

    local timeout="${WAIT_FOR_READY_TIMEOUT:-600}"
    local interval=5
    local elapsed=0

    # Wait for deployments
    while [[ $elapsed -lt $timeout ]]; do
        if kubectl wait --for=condition=available \
            --timeout="${interval}s" \
            deployment -l "app.kubernetes.io/name=${NAME}" \
            -n "$NAMESPACE" &>/dev/null; then
            log_info "All deployments are ready"
            return 0
        fi
        elapsed=$((elapsed + interval))
        log_info "Waiting for deployments... ($elapsed/$timeout seconds)"
    done

    log_error "Timeout waiting for deployments to be ready"
    return 1
}

# Post-deployment configuration
post_deployment_config() {
    log_info "Running post-deployment configuration..."

    # Apply application manifest
    envsubst < /data/manifest/application.yaml | kubectl apply -f -

    # Configure monitoring if enabled
    if [[ "${MONITORING_ENABLED:-true}" == "true" ]]; then
        log_info "Configuring monitoring..."
        # Apply ServiceMonitor if Prometheus is available
        if kubectl get crd servicemonitors.monitoring.coreos.com &>/dev/null; then
            kubectl apply -f - <<EOF
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: ${NAME}-metrics
  namespace: ${NAMESPACE}
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: ${NAME}
  endpoints:
  - port: metrics
    interval: 30s
EOF
        fi
    fi

    # Configure network policies if enabled
    if [[ "${SECURITY_NETWORK_POLICIES:-true}" == "true" ]]; then
        log_info "Applying network policies..."
        kubectl apply -f - <<EOF
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: ${NAME}-network-policy
  namespace: ${NAMESPACE}
spec:
  podSelector:
    matchLabels:
      app.kubernetes.io/name: ${NAME}
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: ${NAMESPACE}
    ports:
    - protocol: TCP
      port: 8080
  egress:
  - to:
    - namespaceSelector: {}
    ports:
    - protocol: TCP
      port: 443
    - protocol: TCP
      port: 80
EOF
    fi

    # Configure pod disruption budget
    kubectl apply -f - <<EOF
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: ${NAME}-pdb
  namespace: ${NAMESPACE}
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app.kubernetes.io/name: ${NAME}
EOF

    log_info "Post-deployment configuration completed"
}

# Run deployment tests
run_tests() {
    log_info "Running deployment tests..."

    # Test 1: Check pod status
    local pod_count
    pod_count=$(kubectl get pods -l "app.kubernetes.io/name=${NAME}" -n "$NAMESPACE" --field-selector=status.phase=Running -o json | jq '.items | length')

    if [[ $pod_count -lt 1 ]]; then
        log_error "No running pods found"
        return 1
    fi

    log_info "Test passed: $pod_count pods running"

    # Test 2: Check service endpoints
    if ! kubectl get endpoints "${NAME}-api" -n "$NAMESPACE" -o jsonpath='{.subsets[*].addresses[*].ip}' | grep -q .; then
        log_error "No service endpoints found"
        return 1
    fi

    log_info "Test passed: Service endpoints configured"

    # Test 3: Check billing agent
    if kubectl get pods -l "app=usage-metering-agent" -n "$NAMESPACE" --field-selector=status.phase=Running &>/dev/null; then
        log_info "Test passed: Billing agent running"
    else
        log_warn "Billing agent not running"
    fi

    log_info "All deployment tests passed"
}

# Print deployment summary
print_summary() {
    log_info "=========================================="
    log_info "Deployment Summary"
    log_info "=========================================="
    log_info "Application Name: $NAME"
    log_info "Namespace: $NAMESPACE"
    log_info ""
    log_info "Access the admin console:"
    log_info "  kubectl port-forward svc/${NAME}-admin 8080:80 -n ${NAMESPACE}"
    log_info ""
    log_info "Retrieve admin password:"
    log_info "  kubectl get secret ${NAME}-admin-credentials -n ${NAMESPACE} -o jsonpath='{.data.password}' | base64 -d"
    log_info ""
    log_info "View application logs:"
    log_info "  kubectl logs -l app.kubernetes.io/name=${NAME} -n ${NAMESPACE}"
    log_info ""
    log_info "View billing metrics:"
    log_info "  kubectl logs -l app=usage-metering-agent -n ${NAMESPACE}"
    log_info "=========================================="
}

# Main deployment flow
main() {
    log_info "Starting enterprise application deployment..."
    log_info "Application: ${NAME:-unknown}"
    log_info "Namespace: ${NAMESPACE:-unknown}"

    # Run deployment steps
    validate_environment || exit 1
    pre_deployment_checks || exit 1
    deploy_rbac || exit 1
    deploy_application || exit 1
    deploy_billing_agent || exit 1
    wait_for_ready || exit 1
    post_deployment_config || exit 1
    run_tests || exit 1

    print_summary

    log_info "Deployment completed successfully!"
}

# Execute main function
main "$@"
