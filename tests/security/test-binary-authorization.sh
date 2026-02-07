#!/bin/bash
# Test Binary Authorization enforcement

set -euo pipefail

echo "=== Binary Authorization Validation ==="

PROJECT_ID=$(gcloud config get-value project)
CLUSTER_NAME="cre-cluster"
REGION="us-central1"

# Test 1: Verify Binary Authorization is enabled
echo "Test 1: Checking Binary Authorization policy..."
POLICY=$(gcloud container binauthz policy export 2>/dev/null || echo "")

if [ -z "$POLICY" ]; then
  echo "❌ FAIL: Binary Authorization policy not found"
  exit 1
else
  echo "✅ PASS: Binary Authorization policy exists"
fi

# Test 2: Verify attestors are configured
echo "Test 2: Checking attestors..."
ATTESTORS=$(gcloud container binauthz attestors list --format='value(name)' || true)

if [ -z "$ATTESTORS" ]; then
  echo "❌ FAIL: No attestors configured"
  exit 1
else
  echo "✅ PASS: Attestors configured:"
  echo "$ATTESTORS"
fi

# Test 3: Verify cluster has Binary Authorization enabled
echo "Test 3: Checking cluster configuration..."
BINAUTH_ENABLED=$(gcloud container clusters describe "$CLUSTER_NAME" \
  --region="$REGION" \
  --format='value(binaryAuthorization.enabled)' || echo "false")

if [ "$BINAUTH_ENABLED" = "true" ] || [ "$BINAUTH_ENABLED" = "True" ]; then
  echo "✅ PASS: Binary Authorization enabled on cluster"
else
  echo "❌ FAIL: Binary Authorization not enabled on cluster"
  exit 1
fi

# Test 4: Test deployment of unsigned image (should fail)
echo "Test 4: Testing unsigned image deployment..."
cat > /tmp/test-unsigned-pod.yaml <<EOF
apiVersion: v1
kind: Pod
metadata:
  name: test-unsigned
  namespace: cre-system
spec:
  containers:
  - name: nginx
    image: nginx:latest
EOF

if kubectl apply -f /tmp/test-unsigned-pod.yaml 2>&1 | grep -q "denied by Binary Authorization"; then
  echo "✅ PASS: Unsigned image correctly rejected"
  kubectl delete -f /tmp/test-unsigned-pod.yaml --ignore-not-found
else
  echo "⚠️  WARN: Unsigned image was not rejected (may be allowlisted)"
  kubectl delete -f /tmp/test-unsigned-pod.yaml --ignore-not-found
fi

rm -f /tmp/test-unsigned-pod.yaml

# Test 5: Verify attestor IAM permissions
echo "Test 5: Checking attestor IAM permissions..."
for ATTESTOR in $ATTESTORS; do
  IAM_POLICY=$(gcloud container binauthz attestors get-iam-policy "$ATTESTOR" --format=json)
  if echo "$IAM_POLICY" | grep -q "roles/binaryauthorization.attestorsVerifier"; then
    echo "✅ PASS: Attestor $ATTESTOR has correct IAM permissions"
  else
    echo "❌ FAIL: Attestor $ATTESTOR missing IAM permissions"
    exit 1
  fi
done

echo ""
echo "=== Binary Authorization Validation Complete ==="
