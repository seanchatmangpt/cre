#!/bin/bash
set -e

IMAGE="${1:?Usage: $0 IMAGE_URI}"

echo "Scanning ${IMAGE} for security vulnerabilities..."

# Run Trivy scan
trivy image --severity CRITICAL,HIGH --format json ${IMAGE} > scan-results.json

# Check for CRITICAL vulnerabilities
CRITICAL_COUNT=$(jq '[.Results[].Vulnerabilities[]? | select(.Severity == "CRITICAL")] | length' scan-results.json)

if [ "${CRITICAL_COUNT}" -gt 0 ]; then
  echo "FOUND ${CRITICAL_COUNT} CRITICAL VULNERABILITIES"
  jq '.Results[].Vulnerabilities[] | select(.Severity == "CRITICAL")' scan-results.json
  exit 1
fi

# Check for HIGH vulnerabilities
HIGH_COUNT=$(jq '[.Results[].Vulnerabilities[]? | select(.Severity == "HIGH")] | length' scan-results.json)

echo "CRITICAL: ${CRITICAL_COUNT}"
echo "HIGH: ${HIGH_COUNT}"

if [ "${HIGH_COUNT}" -gt 10 ]; then
  echo "WARNING: More than 10 HIGH vulnerabilities found"
  exit 1
fi

echo "Security scan passed"
