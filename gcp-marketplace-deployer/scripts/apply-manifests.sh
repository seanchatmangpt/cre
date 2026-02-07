#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_info "Applying Kubernetes manifests..."

# Create ConfigMap with configuration
log_info "Creating ConfigMap..."
kubectl create configmap "${APP_NAME}-config" \
    --namespace="${NAMESPACE}" \
    --from-literal=APP_NAME="${APP_NAME}" \
    --from-literal=PROJECT_ID="${PROJECT_ID}" \
    --from-literal=REGION="${REGION}" \
    --dry-run=client -o yaml | kubectl apply -f -

# Create database connection secret if database is enabled
if [[ "${DATABASE_ENABLED:-true}" == "true" ]]; then
    log_info "Creating database credentials secret..."
    kubectl create secret generic "${APP_NAME}-db-credentials" \
        --namespace="${NAMESPACE}" \
        --from-literal=database-url="${DATABASE_URL}" \
        --from-literal=db-host="${DB_PRIVATE_IP}" \
        --from-literal=db-name="${DB_NAME}" \
        --from-literal=db-user="${DB_USER}" \
        --from-literal=db-password="${DB_USER_PASSWORD}" \
        --from-literal=db-connection-name="${DB_CONNECTION_NAME}" \
        --dry-run=client -o yaml | kubectl apply -f -
fi

# Create service account key secret
if [[ -n "${SA_KEY_BASE64:-}" ]]; then
    log_info "Creating service account key secret..."
    kubectl create secret generic "${APP_NAME}-sa-key" \
        --namespace="${NAMESPACE}" \
        --from-literal=key.json="${SA_KEY_BASE64}" \
        --dry-run=client -o yaml | kubectl apply -f -
fi

# Apply manifests directory
if [[ -d "/data/manifests" ]]; then
    log_info "Applying manifests from /data/manifests..."
    for manifest in /data/manifests/*.yaml; do
        if [[ -f "$manifest" ]]; then
            log_info "Applying: $(basename $manifest)"
            # Substitute environment variables in manifests
            envsubst < "$manifest" | kubectl apply -n "${NAMESPACE}" -f -
        fi
    done
fi

# Apply deployment manifest
log_info "Creating deployment..."
cat <<EOF | kubectl apply -n "${NAMESPACE}" -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: ${APP_NAME}
  labels:
    app: ${APP_NAME}
spec:
  replicas: ${REPLICAS:-2}
  selector:
    matchLabels:
      app: ${APP_NAME}
  template:
    metadata:
      labels:
        app: ${APP_NAME}
    spec:
      serviceAccountName: ${APP_SA_NAME}
      containers:
      - name: ${APP_NAME}
        image: ${IMAGE_REPOSITORY}:${IMAGE_TAG}
        ports:
        - containerPort: 8080
          name: http
        env:
        - name: PROJECT_ID
          value: "${PROJECT_ID}"
        - name: REGION
          value: "${REGION}"
        - name: APP_NAME
          value: "${APP_NAME}"
        envFrom:
        - configMapRef:
            name: ${APP_NAME}-config
        resources:
          requests:
            memory: "${MEMORY_REQUEST:-256Mi}"
            cpu: "${CPU_REQUEST:-100m}"
          limits:
            memory: "${MEMORY_LIMIT:-512Mi}"
            cpu: "${CPU_LIMIT:-500m}"
        livenessProbe:
          httpGet:
            path: /healthz
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
EOF

# Create Service
log_info "Creating service..."
cat <<EOF | kubectl apply -n "${NAMESPACE}" -f -
apiVersion: v1
kind: Service
metadata:
  name: ${APP_NAME}
  labels:
    app: ${APP_NAME}
spec:
  type: LoadBalancer
  selector:
    app: ${APP_NAME}
  ports:
  - port: 80
    targetPort: 8080
    protocol: TCP
    name: http
EOF

# Create HorizontalPodAutoscaler
log_info "Creating HorizontalPodAutoscaler..."
cat <<EOF | kubectl apply -n "${NAMESPACE}" -f -
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: ${APP_NAME}
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: ${APP_NAME}
  minReplicas: ${MIN_REPLICAS:-2}
  maxReplicas: ${MAX_REPLICAS:-10}
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
EOF

log_success "All manifests applied successfully"
