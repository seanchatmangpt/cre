#!/bin/bash
# Test VPC Service Controls enforcement

set -euo pipefail

echo "=== VPC Service Controls Validation ==="

# Test 1: Verify perimeter exists
echo "Test 1: Checking VPC-SC perimeter configuration..."
PERIMETER_NAME=$(gcloud access-context-manager perimeters list \
  --policy="$(gcloud access-context-manager policies list --format='value(name)')" \
  --format='value(name)' | grep cre-production || true)

if [ -z "$PERIMETER_NAME" ]; then
  echo "❌ FAIL: Production perimeter not found"
  exit 1
else
  echo "✅ PASS: Production perimeter found: $PERIMETER_NAME"
fi

# Test 2: Verify project is in perimeter
echo "Test 2: Checking project is within perimeter..."
PROJECT_ID=$(gcloud config get-value project)
PERIMETER_PROJECTS=$(gcloud access-context-manager perimeters describe "$PERIMETER_NAME" \
  --policy="$(gcloud access-context-manager policies list --format='value(name)')" \
  --format='value(status.resources)')

if echo "$PERIMETER_PROJECTS" | grep -q "projects/${PROJECT_ID}"; then
  echo "✅ PASS: Project is within perimeter"
else
  echo "❌ FAIL: Project is not within perimeter"
  exit 1
fi

# Test 3: Test unauthorized access (should fail)
echo "Test 3: Testing unauthorized access..."
# Attempt to access from an unauthorized location (simulated)
# This test would need to be run from outside the perimeter
echo "⚠️  MANUAL: Test access from unauthorized IP to verify denial"

# Test 4: Verify restricted services
echo "Test 4: Checking restricted services configuration..."
RESTRICTED_SERVICES=$(gcloud access-context-manager perimeters describe "$PERIMETER_NAME" \
  --policy="$(gcloud access-context-manager policies list --format='value(name)')" \
  --format='value(status.restrictedServices)')

for service in "storage.googleapis.com" "bigquery.googleapis.com" "secretmanager.googleapis.com"; do
  if echo "$RESTRICTED_SERVICES" | grep -q "$service"; then
    echo "✅ PASS: $service is restricted"
  else
    echo "❌ FAIL: $service is not restricted"
    exit 1
  fi
done

# Test 5: Verify ingress policies
echo "Test 5: Checking ingress policies..."
INGRESS_POLICIES=$(gcloud access-context-manager perimeters describe "$PERIMETER_NAME" \
  --policy="$(gcloud access-context-manager policies list --format='value(name)')" \
  --format='value(status.ingressPolicies)')

if [ -n "$INGRESS_POLICIES" ]; then
  echo "✅ PASS: Ingress policies are configured"
else
  echo "⚠️  WARN: No ingress policies configured"
fi

echo ""
echo "=== VPC Service Controls Validation Complete ==="
