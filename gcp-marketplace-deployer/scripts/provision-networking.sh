#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Starting network infrastructure provisioning..."

# Set defaults
default_env VPC_NAME "${APP_NAME}-vpc"
default_env SUBNET_NAME "${APP_NAME}-subnet"
default_env SUBNET_CIDR "10.0.0.0/24"
default_env SECONDARY_RANGE_PODS "${APP_NAME}-pods"
default_env SECONDARY_RANGE_SERVICES "${APP_NAME}-services"
default_env PODS_CIDR "10.1.0.0/16"
default_env SERVICES_CIDR "10.2.0.0/16"

# Enable required APIs
enable_api "compute.googleapis.com"

# Create VPC if it doesn't exist
if ! resource_exists "vpc" "${VPC_NAME}"; then
    log_info "Creating VPC: ${VPC_NAME}"
    gcloud compute networks create "${VPC_NAME}" \
        --project="${PROJECT_ID}" \
        --subnet-mode=custom \
        --bgp-routing-mode=regional
    log_success "VPC created: ${VPC_NAME}"
else
    log_info "VPC already exists: ${VPC_NAME}"
fi

# Create subnet if it doesn't exist
if ! resource_exists "subnet" "${SUBNET_NAME}"; then
    log_info "Creating subnet: ${SUBNET_NAME}"
    gcloud compute networks subnets create "${SUBNET_NAME}" \
        --project="${PROJECT_ID}" \
        --network="${VPC_NAME}" \
        --region="${REGION}" \
        --range="${SUBNET_CIDR}" \
        --secondary-range="${SECONDARY_RANGE_PODS}=${PODS_CIDR}" \
        --secondary-range="${SECONDARY_RANGE_SERVICES}=${SERVICES_CIDR}" \
        --enable-private-ip-google-access \
        --enable-flow-logs
    log_success "Subnet created: ${SUBNET_NAME}"
else
    log_info "Subnet already exists: ${SUBNET_NAME}"
fi

# Create firewall rules
log_info "Creating firewall rules..."

# Allow internal communication
gcloud compute firewall-rules create "${VPC_NAME}-allow-internal" \
    --project="${PROJECT_ID}" \
    --network="${VPC_NAME}" \
    --allow=tcp,udp,icmp \
    --source-ranges="${SUBNET_CIDR},${PODS_CIDR},${SERVICES_CIDR}" \
    --quiet 2>/dev/null || log_warning "Firewall rule may already exist"

# Allow SSH (for debugging)
gcloud compute firewall-rules create "${VPC_NAME}-allow-ssh" \
    --project="${PROJECT_ID}" \
    --network="${VPC_NAME}" \
    --allow=tcp:22 \
    --source-ranges=0.0.0.0/0 \
    --quiet 2>/dev/null || log_warning "Firewall rule may already exist"

# Allow health checks
gcloud compute firewall-rules create "${VPC_NAME}-allow-health-check" \
    --project="${PROJECT_ID}" \
    --network="${VPC_NAME}" \
    --allow=tcp \
    --source-ranges=130.211.0.0/22,35.191.0.0/16 \
    --quiet 2>/dev/null || log_warning "Firewall rule may already exist"

# Create Cloud NAT for private cluster internet access
log_info "Setting up Cloud NAT..."

# Create Cloud Router
gcloud compute routers create "${APP_NAME}-router" \
    --project="${PROJECT_ID}" \
    --network="${VPC_NAME}" \
    --region="${REGION}" \
    --quiet 2>/dev/null || log_warning "Router may already exist"

# Create NAT configuration
gcloud compute routers nats create "${APP_NAME}-nat" \
    --project="${PROJECT_ID}" \
    --router="${APP_NAME}-router" \
    --region="${REGION}" \
    --nat-all-subnet-ip-ranges \
    --auto-allocate-nat-external-ips \
    --quiet 2>/dev/null || log_warning "NAT may already exist"

log_success "Network infrastructure provisioned successfully"

# Export network details for other scripts
export VPC_NAME
export SUBNET_NAME
export SECONDARY_RANGE_PODS
export SECONDARY_RANGE_SERVICES
