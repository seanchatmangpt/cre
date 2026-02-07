#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Starting Cloud SQL database provisioning..."

# Set defaults
default_env DB_INSTANCE_NAME "${APP_NAME}-db"
default_env DB_VERSION "POSTGRES_14"
default_env DB_TIER "db-n1-standard-2"
default_env DB_STORAGE_SIZE "20"
default_env DB_STORAGE_TYPE "PD_SSD"
default_env DB_BACKUP_ENABLED "true"
default_env DB_BACKUP_START_TIME "03:00"
default_env DB_NAME "${APP_NAME//-/_}_db"
default_env DB_USER "${APP_NAME//-/_}_user"
default_env DATABASE_ENABLED "true"

# Skip if database is not enabled
if [[ "${DATABASE_ENABLED}" != "true" ]]; then
    log_info "Database provisioning skipped (not enabled)"
    exit 0
fi

# Enable required APIs
enable_api "sqladmin.googleapis.com"
enable_api "servicenetworking.googleapis.com"

# Check if database instance exists
if resource_exists "database" "${DB_INSTANCE_NAME}"; then
    log_warning "Cloud SQL instance already exists: ${DB_INSTANCE_NAME}"
    log_info "Using existing instance..."
    export DB_EXISTS="true"
else
    export DB_EXISTS="false"
fi

# Generate database password
DB_PASSWORD=$(openssl rand -base64 32 | tr -d "=+/" | cut -c1-25)

# Create private service connection for VPC
log_info "Setting up private service connection..."

# Allocate IP range for private service connection
gcloud compute addresses create "${APP_NAME}-db-range" \
    --global \
    --purpose=VPC_PEERING \
    --prefix-length=16 \
    --network="${VPC_NAME}" \
    --project="${PROJECT_ID}" \
    --quiet 2>/dev/null || log_warning "IP range may already exist"

# Create private service connection
gcloud services vpc-peerings connect \
    --service=servicenetworking.googleapis.com \
    --ranges="${APP_NAME}-db-range" \
    --network="${VPC_NAME}" \
    --project="${PROJECT_ID}" \
    --quiet 2>/dev/null || log_warning "VPC peering may already exist"

# Create Cloud SQL instance
if [[ "${DB_EXISTS}" == "false" ]]; then
    log_info "Creating Cloud SQL instance: ${DB_INSTANCE_NAME}"

    cmd="gcloud sql instances create ${DB_INSTANCE_NAME}"
    cmd+=" --project=${PROJECT_ID}"
    cmd+=" --database-version=${DB_VERSION}"
    cmd+=" --tier=${DB_TIER}"
    cmd+=" --region=${REGION}"
    cmd+=" --network=projects/${PROJECT_ID}/global/networks/${VPC_NAME}"
    cmd+=" --no-assign-ip"
    cmd+=" --storage-size=${DB_STORAGE_SIZE}"
    cmd+=" --storage-type=${DB_STORAGE_TYPE}"
    cmd+=" --storage-auto-increase"
    cmd+=" --maintenance-release-channel=production"
    cmd+=" --maintenance-window-day=SAT"
    cmd+=" --maintenance-window-hour=3"
    cmd+=" --enable-point-in-time-recovery"

    # Add backup configuration if enabled
    if [[ "${DB_BACKUP_ENABLED}" == "true" ]]; then
        cmd+=" --backup"
        cmd+=" --backup-start-time=${DB_BACKUP_START_TIME}"
    fi

    # Add labels
    cmd+=" --database-flags=cloudsql.iam_authentication=on"

    log_info "Executing: ${cmd}"
    eval "${cmd}"

    log_info "Waiting for Cloud SQL instance to be ready..."
    retry_command 30 10 "gcloud sql instances describe ${DB_INSTANCE_NAME} --project=${PROJECT_ID} | grep -q 'RUNNABLE'"

    log_success "Cloud SQL instance created: ${DB_INSTANCE_NAME}"
else
    log_info "Skipping instance creation, using existing instance"
fi

# Set root password
log_info "Setting root password..."
gcloud sql users set-password postgres \
    --instance="${DB_INSTANCE_NAME}" \
    --password="${DB_PASSWORD}" \
    --project="${PROJECT_ID}" \
    --quiet || log_warning "Password may already be set"

# Create database
log_info "Creating database: ${DB_NAME}"
gcloud sql databases create "${DB_NAME}" \
    --instance="${DB_INSTANCE_NAME}" \
    --project="${PROJECT_ID}" \
    --quiet 2>/dev/null || log_warning "Database may already exist"

# Create database user
log_info "Creating database user: ${DB_USER}"
DB_USER_PASSWORD=$(openssl rand -base64 32 | tr -d "=+/" | cut -c1-25)

gcloud sql users create "${DB_USER}" \
    --instance="${DB_INSTANCE_NAME}" \
    --password="${DB_USER_PASSWORD}" \
    --project="${PROJECT_ID}" \
    --quiet 2>/dev/null || log_warning "User may already exist"

# Grant privileges (done via Cloud SQL Proxy or direct connection)
log_info "Database user created with password"

# Get connection name
DB_CONNECTION_NAME=$(gcloud sql instances describe "${DB_INSTANCE_NAME}" \
    --project="${PROJECT_ID}" \
    --format="value(connectionName)")

# Get private IP
DB_PRIVATE_IP=$(gcloud sql instances describe "${DB_INSTANCE_NAME}" \
    --project="${PROJECT_ID}" \
    --format="value(ipAddresses[0].ipAddress)")

log_success "Cloud SQL database provisioned successfully"
log_info "Connection name: ${DB_CONNECTION_NAME}"
log_info "Private IP: ${DB_PRIVATE_IP}"
log_info "Database: ${DB_NAME}"
log_info "User: ${DB_USER}"

# Export database connection details
export DB_INSTANCE_NAME
export DB_CONNECTION_NAME
export DB_PRIVATE_IP
export DB_NAME
export DB_USER
export DB_USER_PASSWORD
export DB_PASSWORD  # Root password

# Store credentials for Kubernetes secret creation
export DATABASE_URL="postgresql://${DB_USER}:${DB_USER_PASSWORD}@${DB_PRIVATE_IP}:5432/${DB_NAME}"
