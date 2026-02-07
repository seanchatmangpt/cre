#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Starting GKE cluster provisioning..."

# Set defaults
default_env CLUSTER_NAME "${APP_NAME}-gke"
default_env NODE_COUNT "3"
default_env MACHINE_TYPE "n1-standard-4"
default_env DISK_SIZE "100"
default_env ENABLE_AUTOSCALING "true"
default_env MIN_NODES "1"
default_env MAX_NODES "10"
default_env ENABLE_PRIVATE_CLUSTER "true"
default_env MASTER_IPV4_CIDR_BLOCK "172.16.0.0/28"
default_env GKE_VERSION ""  # Use default stable version

# Enable required APIs
enable_api "container.googleapis.com"

# Check if cluster exists
if resource_exists "cluster" "${CLUSTER_NAME}"; then
    log_warning "GKE cluster already exists: ${CLUSTER_NAME}"
    log_info "Using existing cluster..."
    export CLUSTER_EXISTS="true"
else
    export CLUSTER_EXISTS="false"
fi

# Build GKE create command
if [[ "${CLUSTER_EXISTS}" == "false" ]]; then
    log_info "Creating GKE cluster: ${CLUSTER_NAME}"

    # Base command
    cmd="gcloud container clusters create ${CLUSTER_NAME}"
    cmd+=" --project=${PROJECT_ID}"
    cmd+=" --region=${REGION}"
    cmd+=" --network=${VPC_NAME}"
    cmd+=" --subnetwork=${SUBNET_NAME}"
    cmd+=" --cluster-secondary-range-name=${SECONDARY_RANGE_PODS}"
    cmd+=" --services-secondary-range-name=${SECONDARY_RANGE_SERVICES}"
    cmd+=" --num-nodes=${NODE_COUNT}"
    cmd+=" --machine-type=${MACHINE_TYPE}"
    cmd+=" --disk-size=${DISK_SIZE}"
    cmd+=" --disk-type=pd-standard"
    cmd+=" --image-type=COS_CONTAINERD"
    cmd+=" --service-account=${GKE_SA_EMAIL}"
    cmd+=" --enable-stackdriver-kubernetes"
    cmd+=" --enable-ip-alias"
    cmd+=" --enable-autorepair"
    cmd+=" --enable-autoupgrade"
    cmd+=" --maintenance-window-start=2023-01-01T00:00:00Z"
    cmd+=" --maintenance-window-duration=4h"
    cmd+=" --maintenance-window-recurrence='FREQ=WEEKLY;BYDAY=SA'"
    cmd+=" --addons=HorizontalPodAutoscaling,HttpLoadBalancing,GcePersistentDiskCsiDriver"

    # Add Workload Identity if enabled
    if [[ "${WORKLOAD_IDENTITY_ENABLED:-true}" == "true" ]]; then
        cmd+=" --workload-pool=${PROJECT_ID}.svc.id.goog"
        cmd+=" --enable-shielded-nodes"
    fi

    # Add autoscaling if enabled
    if [[ "${ENABLE_AUTOSCALING}" == "true" ]]; then
        cmd+=" --enable-autoscaling"
        cmd+=" --min-nodes=${MIN_NODES}"
        cmd+=" --max-nodes=${MAX_NODES}"
    fi

    # Add private cluster settings if enabled
    if [[ "${ENABLE_PRIVATE_CLUSTER}" == "true" ]]; then
        cmd+=" --enable-private-nodes"
        cmd+=" --enable-private-endpoint"
        cmd+=" --master-ipv4-cidr=${MASTER_IPV4_CIDR_BLOCK}"
        cmd+=" --no-enable-master-authorized-networks"
    fi

    # Add version if specified
    if [[ -n "${GKE_VERSION}" ]]; then
        cmd+=" --cluster-version=${GKE_VERSION}"
    fi

    # Add labels
    cmd+=" --labels=app=${APP_NAME},managed-by=marketplace"

    # Execute cluster creation
    log_info "Executing: ${cmd}"
    eval "${cmd}"

    log_success "GKE cluster created: ${CLUSTER_NAME}"
else
    log_info "Skipping cluster creation, using existing cluster"
fi

# Get cluster credentials
log_info "Configuring kubectl access..."
gcloud container clusters get-credentials "${CLUSTER_NAME}" \
    --region="${REGION}" \
    --project="${PROJECT_ID}"

# Verify cluster access
log_info "Verifying cluster access..."
kubectl cluster-info
kubectl get nodes

# Create node pool for stateful workloads (if needed)
if [[ "${CREATE_STATEFUL_POOL:-false}" == "true" ]]; then
    log_info "Creating stateful workload node pool..."

    gcloud container node-pools create "stateful-pool" \
        --cluster="${CLUSTER_NAME}" \
        --region="${REGION}" \
        --project="${PROJECT_ID}" \
        --machine-type="${MACHINE_TYPE}" \
        --num-nodes=1 \
        --enable-autoscaling \
        --min-nodes=1 \
        --max-nodes=3 \
        --node-labels=workload-type=stateful \
        --node-taints=workload-type=stateful:NoSchedule \
        --service-account="${GKE_SA_EMAIL}" \
        --disk-size="${DISK_SIZE}" \
        --disk-type=pd-ssd \
        --quiet 2>/dev/null || log_warning "Stateful pool may already exist"
fi

log_success "GKE cluster provisioned and configured successfully"

# Export cluster information
export CLUSTER_NAME
export CLUSTER_EXISTS
