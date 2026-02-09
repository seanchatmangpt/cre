#!/bin/bash
# deploy-dashboards.sh
#
# Deploys CRE Cloud Monitoring dashboards and alert policies to GCP.
#
# Usage:
#   ./scripts/deploy-dashboards.sh [PROJECT_ID]
#
# Requirements:
#   - gcloud CLI installed and configured
#   - Active GCP account with appropriate permissions
#   - Cloud Monitoring API enabled

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Project configuration
PROJECT_ID="${1:-}"
DASHBOARDS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/monitoring/gcp"

# Function to print colored messages
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if gcloud is installed
check_gcloud() {
    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI is not installed. Please install it first:"
        echo "  https://cloud.google.com/sdk/docs/install"
        exit 1
    fi
    log_success "gcloud CLI found"
}

# Function to check if user is authenticated
check_auth() {
    if ! gcloud auth list --filter="status:ACTIVE" --format="value(account)" | grep -q "@"; then
        log_error "No active gcloud account. Please run: gcloud auth login"
        exit 1
    fi
    local account
    account=$(gcloud auth list --filter="status:ACTIVE" --format="value(account)")
    log_success "Authenticated as: $account"
}

# Function to set or verify project
setup_project() {
    if [[ -z "$PROJECT_ID" ]]; then
        PROJECT_ID=$(gcloud config get-value project 2>/dev/null || echo "")
        if [[ -z "$PROJECT_ID" ]]; then
            log_error "No project specified and no default project set."
            echo "Usage: $0 [PROJECT_ID]"
            echo "Or set default: gcloud config set project PROJECT_ID"
            exit 1
        fi
        log_info "Using default project: $PROJECT_ID"
    else
        gcloud config set project "$PROJECT_ID"
        log_success "Project set to: $PROJECT_ID"
    fi

    # Verify project exists
    if ! gcloud projects describe "$PROJECT_ID" &>/dev/null; then
        log_error "Project $PROJECT_ID not found or insufficient permissions"
        exit 1
    fi
}

# Function to enable required APIs
enable_apis() {
    log_info "Enabling required APIs..."

    local apis=(
        "monitoring.googleapis.com"
        "cloudmonitoring.googleapis.com"
    )

    for api in "${apis[@]}"; do
        if gcloud services list --enabled --project="$PROJECT_ID" | grep -q "$api"; then
            log_info "  $api already enabled"
        else
            log_info "  Enabling $api..."
            gcloud services enable "$api" --project="$PROJECT_ID"
        fi
    done

    log_success "Required APIs enabled"
}

# Function to deploy a single dashboard
deploy_dashboard() {
    local dashboard_file="$1"
    local dashboard_name
    dashboard_name=$(basename "$dashboard_file" .json)

    log_info "Deploying dashboard: $dashboard_name"

    # Use gcloud monitoring dashboards create command
    # Note: The API expects a specific format, so we use the REST API via curl
    local access_token
    access_token=$(gcloud auth print-access-token)

    local response
    response=$(curl -s -X POST \
        "https://monitoring.googleapis.com/v3/projects/${PROJECT_ID}/dashboards" \
        -H "Authorization: Bearer $access_token" \
        -H "Content-Type: application/json" \
        -d @"$dashboard_file" \
        -w "\n%{http_code}")

    local http_code
    http_code=$(echo "$response" | tail -n1)
    local body
    body=$(echo "$response" | sed '$d')

    if [[ "$http_code" =~ ^2 ]]; then
        local dashboard_id
        dashboard_id=$(echo "$body" | grep -o '"name":"[^"]*"' | head -1 | cut -d'"' -f4)
        log_success "  Deployed: $dashboard_id"

        # Save dashboard ID for reference
        echo "$dashboard_id" > "${dashboard_file}.id"
    else
        log_error "  Failed to deploy dashboard (HTTP $http_code)"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        return 1
    fi
}

# Function to deploy alert policies
deploy_alert_policies() {
    local alert_file="$DASHBOARDS_DIR/alert-policies.yaml"

    if [[ ! -f "$alert_file" ]]; then
        log_warning "No alert policies file found at $alert_file"
        return 0
    fi

    log_info "Alert policies deployment requires manual setup via Cloud Console"
    log_info "Alert policies file: $alert_file"
    log_info "Or use gcloud alpha monitoring policies create --policy-from-file=<policy>.yaml"

    # For now, just display the alert policies
    log_warning "The following alert policies are defined:"
    grep -E '^\s+displayName:' "$alert_file" | sed 's/^\s*/  /' || true
}

# Function to list existing dashboards
list_dashboards() {
    log_info "Existing dashboards in project $PROJECT_ID:"

    local access_token
    access_token=$(gcloud auth print-access-token)

    curl -s -X GET \
        "https://monitoring.googleapis.com/v3/projects/${PROJECT_ID}/dashboards" \
        -H "Authorization: Bearer $access_token" \
        | jq -r '.dashboards[] | "  - \(.displayName) (\(.name))"' || true
}

# Main deployment function
main() {
    echo "=================================="
    echo "CRE Cloud Monitoring Deployment"
    echo "=================================="
    echo

    check_gcloud
    check_auth
    setup_project
    enable_apis
    echo

    # List existing dashboards
    list_dashboards
    echo

    # Deploy dashboards
    log_info "Deploying dashboards from: $DASHBOARDS_DIR"
    echo

    local dashboard_count=0
    local success_count=0

    for dashboard_file in "$DASHBOARDS_DIR"/*.json; do
        if [[ -f "$dashboard_file" ]]; then
            ((dashboard_count++))
            if deploy_dashboard "$dashboard_file"; then
                ((success_count++))
            fi
        fi
    done

    echo
    log_info "Dashboards deployed: $success_count/$dashboard_count"
    echo

    # Handle alert policies
    deploy_alert_policies

    echo
    log_success "Deployment complete!"
    echo
    echo "View dashboards at:"
    echo "  https://console.cloud.google.com/monitoring/dashboards?project=$PROJECT_ID"
}

# Run main function
main "$@"
