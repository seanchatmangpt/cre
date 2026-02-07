#!/bin/bash
# Test Workload Identity configuration

set -euo pipefail

echo "=== Workload Identity Validation ==="

PROJECT_ID=$(gcloud config get-value project)
CLUSTER_NAME="cre-cluster"
REGION="us-central1"
TEST_NAMESPACE="cre-system"
TEST_KSA="cre-app"
TEST_GSA="cre-app-workload@${PROJECT_ID}.iam.gserviceaccount.com"

# Test 1: Verify Workload Identity is enabled on cluster
echo "Test 1: Checking Workload Identity on cluster..."
WI_POOL=$(gcloud container clusters describe "$CLUSTER_NAME" \
  --region="$REGION" \
  --format='value(workloadIdentityConfig.workloadPool)' || echo "")

if [ -z "$WI_POOL" ]; then
  echo "❌ FAIL: Workload Identity not enabled on cluster"
  exit 1
else
  echo "✅ PASS: Workload Identity pool: $WI_POOL"
fi

# Test 2: Verify Google Service Account exists
echo "Test 2: Checking Google Service Account..."
if gcloud iam service-accounts describe "$TEST_GSA" &>/dev/null; then
  echo "✅ PASS: Google Service Account exists: $TEST_GSA"
else
  echo "❌ FAIL: Google Service Account not found: $TEST_GSA"
  exit 1
fi

# Test 3: Verify IAM binding
echo "Test 3: Checking IAM binding..."
IAM_POLICY=$(gcloud iam service-accounts get-iam-policy "$TEST_GSA" --format=json)
EXPECTED_MEMBER="serviceAccount:${PROJECT_ID}.svc.id.goog[${TEST_NAMESPACE}/${TEST_KSA}]"

if echo "$IAM_POLICY" | grep -q "$EXPECTED_MEMBER"; then
  echo "✅ PASS: IAM binding exists for $EXPECTED_MEMBER"
else
  echo "❌ FAIL: IAM binding not found for $EXPECTED_MEMBER"
  exit 1
fi

# Test 4: Verify Kubernetes Service Account annotation
echo "Test 4: Checking Kubernetes Service Account..."
if kubectl get sa "$TEST_KSA" -n "$TEST_NAMESPACE" &>/dev/null; then
  KSA_ANNOTATION=$(kubectl get sa "$TEST_KSA" -n "$TEST_NAMESPACE" \
    -o jsonpath='{.metadata.annotations.iam\.gke\.io/gcp-service-account}')

  if [ "$KSA_ANNOTATION" = "$TEST_GSA" ]; then
    echo "✅ PASS: Kubernetes ServiceAccount has correct annotation"
  else
    echo "❌ FAIL: Annotation mismatch. Expected: $TEST_GSA, Got: $KSA_ANNOTATION"
    exit 1
  fi
else
  echo "❌ FAIL: Kubernetes ServiceAccount not found"
  exit 1
fi

# Test 5: Test Workload Identity from a pod
echo "Test 5: Testing Workload Identity authentication..."
cat > /tmp/test-workload-identity.yaml <<EOF
apiVersion: v1
kind: Pod
metadata:
  name: test-workload-identity
  namespace: $TEST_NAMESPACE
spec:
  serviceAccountName: $TEST_KSA
  containers:
  - name: test
    image: google/cloud-sdk:alpine
    command: ['sh', '-c', 'gcloud auth list && sleep 30']
  restartPolicy: Never
EOF

kubectl apply -f /tmp/test-workload-identity.yaml
kubectl wait --for=condition=ready pod/test-workload-identity -n "$TEST_NAMESPACE" --timeout=60s || true

# Check logs for authentication
sleep 5
LOGS=$(kubectl logs test-workload-identity -n "$TEST_NAMESPACE" || echo "")

if echo "$LOGS" | grep -q "$TEST_GSA"; then
  echo "✅ PASS: Workload Identity authentication successful"
else
  echo "❌ FAIL: Workload Identity authentication failed"
  echo "Pod logs:"
  echo "$LOGS"
  kubectl delete -f /tmp/test-workload-identity.yaml --ignore-not-found
  rm -f /tmp/test-workload-identity.yaml
  exit 1
fi

# Cleanup
kubectl delete -f /tmp/test-workload-identity.yaml --ignore-not-found
rm -f /tmp/test-workload-identity.yaml

# Test 6: Verify no service account keys exist
echo "Test 6: Checking for service account keys..."
KEYS=$(gcloud iam service-accounts keys list \
  --iam-account="$TEST_GSA" \
  --filter="keyType=USER_MANAGED" \
  --format='value(name)' || echo "")

if [ -z "$KEYS" ]; then
  echo "✅ PASS: No user-managed service account keys found"
else
  echo "❌ FAIL: User-managed keys exist (should use Workload Identity instead):"
  echo "$KEYS"
  exit 1
fi

echo ""
echo "=== Workload Identity Validation Complete ==="
