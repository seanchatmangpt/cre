#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Running post-deployment configuration..."

# Wait for service to get external IP
log_info "Waiting for LoadBalancer IP..."
for i in {1..30}; do
    EXTERNAL_IP=$(kubectl get svc "${APP_NAME}" -n "${NAMESPACE}" \
        -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    if [[ -n "${EXTERNAL_IP}" ]]; then
        log_success "LoadBalancer IP: ${EXTERNAL_IP}"
        export EXTERNAL_IP
        break
    fi

    if [[ $i -eq 30 ]]; then
        log_warning "Timeout waiting for LoadBalancer IP"
        EXTERNAL_IP="<pending>"
    fi

    sleep 10
done

# Create DNS record if Cloud DNS zone is configured
if [[ -n "${DNS_ZONE:-}" ]] && [[ -n "${DNS_NAME:-}" ]] && [[ "${EXTERNAL_IP}" != "<pending>" ]]; then
    log_info "Creating DNS record..."

    gcloud dns record-sets transaction start \
        --zone="${DNS_ZONE}" \
        --project="${PROJECT_ID}" || true

    gcloud dns record-sets transaction add "${EXTERNAL_IP}" \
        --name="${DNS_NAME}" \
        --ttl=300 \
        --type=A \
        --zone="${DNS_ZONE}" \
        --project="${PROJECT_ID}" || true

    gcloud dns record-sets transaction execute \
        --zone="${DNS_ZONE}" \
        --project="${PROJECT_ID}" || log_warning "DNS record creation failed"

    log_success "DNS record created: ${DNS_NAME} -> ${EXTERNAL_IP}"
fi

# Configure monitoring and alerting
log_info "Setting up monitoring..."

# Create uptime check
if [[ "${EXTERNAL_IP}" != "<pending>" ]]; then
    gcloud monitoring uptime create "${APP_NAME}-uptime-check" \
        --project="${PROJECT_ID}" \
        --display-name="${APP_NAME} Uptime Check" \
        --resource-type=uptime-url \
        --host="${EXTERNAL_IP}" \
        --path="/healthz" \
        --port=80 \
        --check-interval=60s \
        --timeout=10s \
        --quiet 2>/dev/null || log_warning "Uptime check may already exist"
fi

# Set up log-based metrics
log_info "Creating log-based metrics..."
gcloud logging metrics create "${APP_NAME}-errors" \
    --project="${PROJECT_ID}" \
    --description="Error count for ${APP_NAME}" \
    --log-filter='resource.type="k8s_container"
resource.labels.namespace_name="'"${NAMESPACE}"'"
resource.labels.container_name="'"${APP_NAME}"'"
severity>=ERROR' \
    --value-extractor='EXTRACT(textPayload)' \
    --quiet 2>/dev/null || log_warning "Metric may already exist"

# Create notification channel (example - adjust based on needs)
if [[ -n "${ALERT_EMAIL:-}" ]]; then
    log_info "Creating notification channel..."
    CHANNEL_ID=$(gcloud alpha monitoring channels create \
        --project="${PROJECT_ID}" \
        --display-name="${APP_NAME} Alerts" \
        --type=email \
        --channel-labels=email_address="${ALERT_EMAIL}" \
        --format="value(name)" 2>/dev/null || echo "")

    if [[ -n "${CHANNEL_ID}" ]]; then
        log_success "Notification channel created"
        export CHANNEL_ID
    fi
fi

# Configure backup for stateful resources if needed
if [[ "${ENABLE_BACKUPS:-false}" == "true" ]]; then
    log_info "Configuring backup policies..."
    # Add backup configuration here based on your needs
fi

# Run database migrations if script exists
if [[ -f "/scripts/db-migrate.sh" ]]; then
    log_info "Running database migrations..."
    /scripts/db-migrate.sh || log_warning "Database migrations failed"
fi

# Initialize application data if script exists
if [[ -f "/scripts/init-data.sh" ]]; then
    log_info "Initializing application data..."
    /scripts/init-data.sh || log_warning "Data initialization failed"
fi

# Store deployment metadata in ConfigMap
log_info "Storing deployment metadata..."
kubectl create configmap "${APP_NAME}-deployment-info" \
    --namespace="${NAMESPACE}" \
    --from-literal=deployment-time="$(get_timestamp)" \
    --from-literal=version="${APP_VERSION:-1.0.0}" \
    --from-literal=external-ip="${EXTERNAL_IP}" \
    --from-literal=cluster="${CLUSTER_NAME}" \
    --from-literal=project="${PROJECT_ID}" \
    --from-literal=region="${REGION}" \
    --dry-run=client -o yaml | kubectl apply -f -

log_success "Post-deployment configuration completed"
