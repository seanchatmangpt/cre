#!/bin/bash
set -euo pipefail

source /scripts/utils.sh

log_section "Deployment Information"

# Get service information
EXTERNAL_IP=$(kubectl get svc "${APP_NAME}" -n "${NAMESPACE}" \
    -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "<pending>")

# Get pod information
POD_COUNT=$(kubectl get pods -n "${NAMESPACE}" -l app="${APP_NAME}" \
    --field-selector=status.phase=Running -o json | jq '.items | length')

echo ""
echo "╔════════════════════════════════════════════════╗"
echo "║         DEPLOYMENT COMPLETED SUCCESSFULLY      ║"
echo "╚════════════════════════════════════════════════╝"
echo ""

echo "📋 Application Information:"
echo "   Name:           ${APP_NAME}"
echo "   Namespace:      ${NAMESPACE}"
echo "   Version:        ${APP_VERSION:-1.0.0}"
echo ""

echo "🌐 Access Information:"
if [[ "${EXTERNAL_IP}" != "<pending>" ]]; then
    echo "   External IP:    ${EXTERNAL_IP}"
    echo "   Application URL: http://${EXTERNAL_IP}"
    if [[ -n "${DNS_NAME:-}" ]]; then
        echo "   DNS Name:       ${DNS_NAME}"
        echo "   URL:            http://${DNS_NAME}"
    fi
else
    echo "   External IP:    Pending (check 'kubectl get svc -n ${NAMESPACE}')"
fi
echo ""

echo "☸️  Kubernetes Resources:"
echo "   Cluster:        ${CLUSTER_NAME}"
echo "   Region:         ${REGION}"
echo "   Running Pods:   ${POD_COUNT}"
echo "   Service Type:   LoadBalancer"
echo ""

if [[ "${DATABASE_ENABLED:-true}" == "true" ]]; then
    echo "🗄️  Database Information:"
    echo "   Instance:       ${DB_INSTANCE_NAME}"
    echo "   Connection:     ${DB_CONNECTION_NAME}"
    echo "   Database:       ${DB_NAME}"
    echo "   Private IP:     ${DB_PRIVATE_IP}"
    echo ""
fi

echo "🔐 IAM & Security:"
echo "   GCP Project:    ${PROJECT_ID}"
echo "   GKE SA:         ${GKE_SA_EMAIL}"
echo "   App SA:         ${APP_SA_EMAIL}"
if [[ "${WORKLOAD_IDENTITY_ENABLED:-true}" == "true" ]]; then
    echo "   Workload ID:    Enabled"
fi
echo ""

echo "🌐 Networking:"
echo "   VPC:            ${VPC_NAME}"
echo "   Subnet:         ${SUBNET_NAME}"
echo "   Private Cluster: ${ENABLE_PRIVATE_CLUSTER:-true}"
echo ""

echo "📊 Useful Commands:"
echo "   View pods:      kubectl get pods -n ${NAMESPACE}"
echo "   View logs:      kubectl logs -f -n ${NAMESPACE} -l app=${APP_NAME}"
echo "   View service:   kubectl get svc ${APP_NAME} -n ${NAMESPACE}"
echo "   Describe:       kubectl describe deployment ${APP_NAME} -n ${NAMESPACE}"
echo "   Port forward:   kubectl port-forward -n ${NAMESPACE} svc/${APP_NAME} 8080:80"
echo ""

echo "📝 Next Steps:"
echo "   1. Verify application is running:"
echo "      kubectl get pods -n ${NAMESPACE}"
echo ""
echo "   2. Test the application:"
if [[ "${EXTERNAL_IP}" != "<pending>" ]]; then
    echo "      curl http://${EXTERNAL_IP}/healthz"
else
    echo "      (Wait for LoadBalancer IP assignment)"
fi
echo ""
echo "   3. View application logs:"
echo "      kubectl logs -f -n ${NAMESPACE} -l app=${APP_NAME}"
echo ""
echo "   4. Scale the application:"
echo "      kubectl scale deployment ${APP_NAME} -n ${NAMESPACE} --replicas=3"
echo ""

if [[ "${DATABASE_ENABLED:-true}" == "true" ]]; then
    echo "   5. Connect to database:"
    echo "      cloud_sql_proxy -instances=${DB_CONNECTION_NAME}=tcp:5432"
    echo ""
fi

echo "🔗 Resources:"
echo "   GCP Console:    https://console.cloud.google.com/kubernetes/workload?project=${PROJECT_ID}"
echo "   GKE Cluster:    https://console.cloud.google.com/kubernetes/clusters/details/${REGION}/${CLUSTER_NAME}?project=${PROJECT_ID}"
if [[ "${DATABASE_ENABLED:-true}" == "true" ]]; then
    echo "   Cloud SQL:      https://console.cloud.google.com/sql/instances/${DB_INSTANCE_NAME}/overview?project=${PROJECT_ID}"
fi
echo ""

echo "════════════════════════════════════════════════"
echo "For support, visit: https://github.com/your-org/your-app"
echo "════════════════════════════════════════════════"
echo ""
