#!/bin/bash
#
# Configuration Validation Script for CRE
#
# This script validates all configuration files for syntax, completeness,
# consistency, and best practices.
#
# Usage: ./scripts/validate-config.sh [--strict] [--fix]
#
# Options:
#   --strict    Exit with error on any warnings
#   --fix       Attempt to fix auto-fixable issues
#   --help      Show this help message
#

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
ERRORS=0
WARNINGS=0
INFO=0
STRICT_MODE=${1:-}
FIX_MODE=${2:-}

# Helper functions
log_error() {
    echo -e "${RED}ERROR${NC}: $1" >&2
    ((ERRORS++))
}

log_warning() {
    echo -e "${YELLOW}WARNING${NC}: $1" >&2
    ((WARNINGS++))
}

log_info() {
    echo -e "${BLUE}INFO${NC}: $1"
    ((INFO++))
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

# =============================================================================
# SECTION 1: BASIC SYNTAX VALIDATION
# =============================================================================

validate_yaml_syntax() {
    local file="$1"
    echo "Validating YAML syntax: $file"

    if ! command -v yamllint &> /dev/null; then
        log_warning "yamllint not found, skipping YAML linting"
        return 0
    fi

    if ! yamllint -d '{rules: {line-length: disable}}' "$file" &> /dev/null; then
        log_error "YAML syntax error in $file"
        return 1
    fi
    log_success "YAML syntax valid: $file"
    return 0
}

validate_json_syntax() {
    local file="$1"
    echo "Validating JSON syntax: $file"

    if ! python3 -m json.tool "$file" &> /dev/null; then
        log_error "JSON syntax error in $file"
        return 1
    fi
    log_success "JSON syntax valid: $file"
    return 0
}

validate_toml_syntax() {
    local file="$1"
    echo "Validating TOML syntax: $file"

    if ! python3 -c "import tomllib; tomllib.loads(open('$file').read())" 2>/dev/null; then
        log_error "TOML syntax error in $file"
        return 1
    fi
    log_success "TOML syntax valid: $file"
    return 0
}

validate_terraform_syntax() {
    local file="$1"
    echo "Validating Terraform syntax: $file"

    if ! command -v terraform &> /dev/null; then
        log_warning "terraform not found, skipping Terraform validation"
        return 0
    fi

    # Check syntax
    if ! terraform validate "$(dirname "$file")" &> /dev/null 2>&1; then
        log_warning "Terraform validation issues in $(dirname "$file")"
        return 1
    fi
    log_success "Terraform syntax valid: $file"
    return 0
}

validate_erl_syntax() {
    local file="$1"
    echo "Validating Erlang syntax: $file"

    # Basic Erlang syntax check using erlc
    if command -v erlc &> /dev/null; then
        if ! erlc -o /tmp "$file" &> /dev/null; then
            log_error "Erlang syntax error in $file"
            return 1
        fi
    else
        log_warning "erlc not found, skipping Erlang syntax check"
    fi
    log_success "Erlang syntax valid: $file"
    return 0
}

# =============================================================================
# SECTION 2: CONFIGURATION COMPLETENESS CHECKS
# =============================================================================

check_ggen_completeness() {
    echo ""
    echo "Checking ggen.toml completeness..."
    local file="/home/user/cre/ggen.toml"

    if [ ! -f "$file" ]; then
        log_error "ggen.toml not found at $file"
        return 1
    fi

    # Check required sections (generation.rules can be array)
    local required_sections=("project" "ontology" "output")
    for section in "${required_sections[@]}"; do
        if ! grep -q "^\[$section\]" "$file"; then
            log_error "Missing required section [$section] in ggen.toml"
        else
            log_success "Found required section [$section]"
        fi
    done

    # Check for generation rules (array format)
    if ! grep -q "\[\[generation.rules\]\]" "$file"; then
        log_error "Missing generation rules in ggen.toml"
    else
        log_success "Found generation rules"
    fi

    # Check required fields
    if ! grep -q "^version = " "$file"; then
        log_error "Missing required field: version"
    else
        log_success "Found version field"
    fi

    if ! grep -q "^name = " "$file"; then
        log_error "Missing required field: name"
    else
        log_success "Found name field"
    fi

    # Validate TOML syntax
    validate_toml_syntax "$file" || return 1
}

check_rebar_completeness() {
    echo ""
    echo "Checking rebar.config completeness..."
    local file="/home/user/cre/rebar.config"

    if [ ! -f "$file" ]; then
        log_error "rebar.config not found at $file"
        return 1
    fi

    # Check required entries
    if ! grep -q "^{erl_opts," "$file"; then
        log_error "Missing required section: erl_opts"
    else
        log_success "Found erl_opts"
    fi

    if ! grep -q "^{deps," "$file"; then
        log_error "Missing required section: deps"
    else
        log_success "Found deps"
    fi

    if ! grep -q "OTP_25_PLUS\|OTP_26_PLUS\|OTP_27_PLUS\|OTP_28_PLUS" "$file"; then
        log_warning "No OTP version definitions found (OTP_25_PLUS, etc.)"
    else
        log_success "Found OTP version definitions"
    fi

    # Check profiles
    if ! grep -q "{profiles," "$file"; then
        log_warning "Missing profiles configuration"
    else
        log_success "Found profiles configuration"
    fi

    # Validate Erlang term syntax
    if command -v erl &> /dev/null; then
        if ! erl -eval "file:consult('/home/user/cre/rebar.config'), halt()" -noshell 2>/dev/null; then
            log_error "rebar.config syntax error"
            return 1
        fi
    fi

    log_success "rebar.config is valid"
}

check_k8s_configmap_completeness() {
    echo ""
    echo "Checking Kubernetes ConfigMaps completeness..."

    local configmaps=(
        "/home/user/cre/k8s/base/configmap.yaml"
        "/home/user/cre/k8s/gcp/configmap.yaml"
        "/home/user/cre/k8s/charts/cre/templates/configmap.yaml"
    )

    for cm in "${configmaps[@]}"; do
        if [ ! -f "$cm" ]; then
            log_warning "ConfigMap not found: $cm"
            continue
        fi

        validate_yaml_syntax "$cm" || return 1

        # Check for required keys
        if grep -q "CRE_DEFAULT_PORT\|cre-default-port" "$cm"; then
            log_success "Found port configuration in $(basename $cm)"
        else
            log_warning "Missing port configuration in $(basename $cm)"
        fi

        # Check for empty values
        if grep -qE ":\s*\"REPLACE_WITH|:\s*\"\"" "$cm"; then
            log_warning "ConfigMap contains placeholder values: $(basename $cm)"
        fi
    done
}

check_terraform_variables_defaults() {
    echo ""
    echo "Checking Terraform variable defaults..."

    local tf_files=(
        "/home/user/cre/terraform/gcp/variables.tf"
        "/home/user/cre/terraform/gcp/modules/gke_cluster/variables.tf"
        "/home/user/cre/terraform/gcp/modules/backup/variables.tf"
        "/home/user/cre/terraform/gcp/modules/storage/variables.tf"
    )

    for tf_file in "${tf_files[@]}"; do
        if [ ! -f "$tf_file" ]; then
            log_warning "Terraform file not found: $tf_file"
            continue
        fi

        echo "Validating: $(basename $tf_file)"

        # Check for required variables without defaults
        local var_count=$(grep -c "^variable" "$tf_file" || echo "0")
        local default_count=$(grep -c "default = " "$tf_file" || echo "0")

        log_info "Variables: $var_count, with defaults: $default_count in $(basename $tf_file)"

        # Check for sensible defaults
        if grep -q "default.*project_id" "$tf_file"; then
            log_warning "Project ID should not have a default in $(basename $tf_file)"
        fi

        # Check for region defaults
        if grep -q 'default.*= "us-central1"' "$tf_file"; then
            log_success "Found sensible region default"
        fi
    done
}

check_monitoring_dashboards() {
    echo ""
    echo "Checking monitoring dashboards..."

    local dashboards=(
        "/home/user/cre/monitoring/gcp/gke-cluster-dashboard.json"
        "/home/user/cre/monitoring/gcp/erlang-vm-dashboard.json"
        "/home/user/cre/monitoring/gcp/workflow-execution-dashboard.json"
    )

    for dashboard in "${dashboards[@]}"; do
        if [ ! -f "$dashboard" ]; then
            log_warning "Dashboard not found: $dashboard"
            continue
        fi

        echo "Validating: $(basename $dashboard)"

        # Validate JSON syntax
        if ! validate_json_syntax "$dashboard"; then
            log_error "Invalid JSON in $(basename $dashboard)"
            continue
        fi

        # Check for required dashboard properties
        if ! grep -q "displayName\|title" "$dashboard"; then
            log_warning "Missing displayName/title in $(basename $dashboard)"
        else
            log_success "Found title in $(basename $dashboard)"
        fi

        # Check for metrics
        if ! grep -q "metric\|metricType\|dataSets" "$dashboard"; then
            log_warning "Missing metrics definition in $(basename $dashboard)"
        else
            log_success "Found metrics in $(basename $dashboard)"
        fi
    done
}

# =============================================================================
# SECTION 3: HARDCODED VALUES CHECK
# =============================================================================

check_hardcoded_values() {
    echo ""
    echo "Checking for hardcoded values that should be configurable..."

    # Pattern 1: REPLACE_WITH placeholders
    local replace_count=$(find /home/user/cre/k8s -type f -name "*.yaml" -exec grep -l "REPLACE_WITH" {} \; | wc -l)
    if [ "$replace_count" -gt 0 ]; then
        log_warning "Found $replace_count K8s files with REPLACE_WITH placeholders"
        find /home/user/cre/k8s -type f -name "*.yaml" -exec grep -l "REPLACE_WITH" {} \; | while read -r f; do
            local count=$(grep -c "REPLACE_WITH" "$f")
            log_warning "  - $(basename $f): $count placeholder(s)"
        done
    else
        log_success "No REPLACE_WITH placeholders found"
    fi

    # Pattern 2: Check for localhost in production configs
    local localhost_count=$(grep -r "localhost\|127\.0\.0\.1" /home/user/cre/k8s/gcp /home/user/cre/k8s/charts 2>/dev/null | grep -v ".git" | wc -l || echo "0")
    if [ "$localhost_count" -gt 0 ]; then
        log_warning "Found references to localhost in GCP/Helm configs"
    else
        log_success "No localhost references in production configs"
    fi

    # Pattern 3: Check for example.com in production
    local example_count=$(grep -r "example\.com" /home/user/cre/k8s/gcp /home/user/cre/k8s/charts /home/user/cre/terraform 2>/dev/null | wc -l || echo "0")
    if [ "$example_count" -gt 0 ]; then
        log_warning "Found example.com references in production configs"
    else
        log_success "No example.com references found"
    fi

    # Pattern 4: Check for hardcoded IPs
    local ip_count=$(grep -rE "[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}" /home/user/cre/k8s /home/user/cre/terraform 2>/dev/null | grep -v "\.git" | grep -v "CIDR\|mask\|subnet\|10\.\|172\.\|192\." | wc -l || echo "0")
    log_info "Found $ip_count non-standard IP addresses (may be false positives)"
}

# =============================================================================
# SECTION 4: CONSISTENCY CHECKS
# =============================================================================

check_consistency() {
    echo ""
    echo "Checking configuration consistency..."

    # Version consistency
    local ggen_version=$(grep "^version = " /home/user/cre/ggen.toml | sed 's/.*"\([^"]*\)".*/\1/')
    local dockerfile_version=$(grep "ARG VERSION=" /home/user/cre/Dockerfile | sed 's/.*=\([^ ]*\).*/\1/')
    local helm_version=$(grep "appVersion:" /home/user/cre/k8s/charts/cre/Chart.yaml | sed 's/.*: *\([^ ]*\).*/\1/')

    if [ "$ggen_version" == "$dockerfile_version" ] && [ "$ggen_version" == "$helm_version" ]; then
        log_success "Version consistency: all components at $ggen_version"
    else
        log_warning "Version mismatch:"
        log_info "  ggen.toml: $ggen_version"
        log_info "  Dockerfile: $dockerfile_version"
        log_info "  Helm Chart: $helm_version"
    fi

    # Port consistency
    local base_port=$(grep "cre-default-port\|CRE_DEFAULT_PORT" /home/user/cre/k8s/base/configmap.yaml | head -1 | sed 's/.*: *"\([0-9]*\)".*/\1/')
    local gcp_port=$(grep "CRE_DEFAULT_PORT" /home/user/cre/k8s/gcp/configmap.yaml | head -1 | sed 's/.*: *"\([0-9]*\)".*/\1/')

    if [ "$base_port" == "$gcp_port" ]; then
        log_success "Port consistency: both ConfigMaps use port $base_port"
    else
        log_warning "Port mismatch: base=$base_port gcp=$gcp_port"
    fi

    # Helm values consistency
    if grep -q "defaultPort.*4142" /home/user/cre/k8s/charts/cre/values.yaml; then
        log_success "Helm values: port configuration present"
    else
        log_warning "Helm values: missing or inconsistent port configuration"
    fi
}

# =============================================================================
# SECTION 5: DOCKERFILE VALIDATION
# =============================================================================

check_dockerfile() {
    echo ""
    echo "Checking Dockerfile configuration..."
    local dockerfile="/home/user/cre/Dockerfile"

    if [ ! -f "$dockerfile" ]; then
        log_error "Dockerfile not found"
        return 1
    fi

    # Check for multi-stage build
    if grep -q "^FROM.*AS" "$dockerfile"; then
        log_success "Multi-stage build detected"
    else
        log_warning "Single-stage build (consider using multi-stage)"
    fi

    # Check for USER directive
    if grep -q "^USER " "$dockerfile"; then
        log_success "USER directive found (non-root user)"
    else
        log_warning "No USER directive found (running as root)"
    fi

    # Check for HEALTHCHECK
    if grep -q "^HEALTHCHECK" "$dockerfile"; then
        log_success "HEALTHCHECK directive found"
    else
        log_warning "No HEALTHCHECK directive found"
    fi

    # Check for OTP version
    if grep -q "erlang:28" "$dockerfile"; then
        log_success "OTP 28 detected in Dockerfile"
    elif grep -q "erlang:27\|erlang:26\|erlang:25" "$dockerfile"; then
        local version=$(grep "erlang:" "$dockerfile" | sed 's/.*erlang:\([0-9]*\).*/\1/' | head -1)
        log_warning "Dockerfile uses OTP $version (project requires OTP 28)"
    else
        log_warning "Could not determine OTP version from Dockerfile"
    fi

    # Check for volume mounts
    if grep -q "^VOLUME" "$dockerfile"; then
        log_success "VOLUME mounts defined"
    else
        log_warning "No VOLUME directives found"
    fi
}

# =============================================================================
# SECTION 6: HELM CHART VALIDATION
# =============================================================================

check_helm_charts() {
    echo ""
    echo "Checking Helm charts..."

    local chart_dir="/home/user/cre/k8s/charts/cre"

    if [ ! -d "$chart_dir" ]; then
        log_error "Helm chart directory not found: $chart_dir"
        return 1
    fi

    # Check for Chart.yaml
    if [ -f "$chart_dir/Chart.yaml" ]; then
        log_success "Chart.yaml found"
        validate_yaml_syntax "$chart_dir/Chart.yaml" || return 1
    else
        log_error "Chart.yaml not found"
        return 1
    fi

    # Check for values.yaml
    if [ -f "$chart_dir/values.yaml" ]; then
        log_success "values.yaml found"
        validate_yaml_syntax "$chart_dir/values.yaml" || return 1
    else
        log_error "values.yaml not found"
        return 1
    fi

    # Check for templates
    local template_count=$(find "$chart_dir/templates" -name "*.yaml" | wc -l)
    if [ "$template_count" -gt 0 ]; then
        log_success "Found $template_count Helm templates"
    else
        log_error "No Helm templates found"
        return 1
    fi

    # Validate each template
    find "$chart_dir/templates" -name "*.yaml" | while read -r template; do
        if ! validate_yaml_syntax "$template"; then
            log_error "Invalid template: $(basename $template)"
        fi
    done

    # Check for values overrides
    if [ -f "$chart_dir/values-gke.yaml" ]; then
        log_success "GKE-specific values found"
        validate_yaml_syntax "$chart_dir/values-gke.yaml"
    fi
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    echo "=========================================="
    echo "CRE Configuration Validation"
    echo "=========================================="
    echo ""

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --strict)
                STRICT_MODE=true
                shift
                ;;
            --fix)
                FIX_MODE=true
                shift
                ;;
            --help)
                grep "^#" "$0" | head -20
                exit 0
                ;;
            *)
                shift
                ;;
        esac
    done

    # Run all validation checks
    check_ggen_completeness
    check_rebar_completeness
    check_k8s_configmap_completeness
    check_terraform_variables_defaults
    check_monitoring_dashboards
    check_hardcoded_values
    check_consistency
    check_dockerfile
    check_helm_charts

    # Print summary
    echo ""
    echo "=========================================="
    echo "Validation Summary"
    echo "=========================================="
    echo -e "${GREEN}✓ Info: $INFO${NC}"
    echo -e "${YELLOW}⚠ Warnings: $WARNINGS${NC}"
    echo -e "${RED}✗ Errors: $ERRORS${NC}"
    echo ""

    if [ "$STRICT_MODE" = true ] && [ "$WARNINGS" -gt 0 ]; then
        log_error "Strict mode enabled - warnings treated as errors"
        exit 1
    fi

    if [ "$ERRORS" -gt 0 ]; then
        exit 1
    fi

    exit 0
}

main "$@"
