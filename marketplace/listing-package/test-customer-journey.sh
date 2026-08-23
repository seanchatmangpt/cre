#!/bin/bash
# Test Customer Documentation Journey
# This script validates that customers can navigate through the documentation

set -e

echo "======================================"
echo "CRE Customer Journey Test"
echo "======================================"
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

TESTS_PASSED=0
TESTS_FAILED=0

# Function to check file exists
check_file() {
  local file=$1
  local description=$2

  echo -n "Checking: $description... "
  if [ -f "$file" ]; then
    echo -e "${GREEN}✓ PASS${NC}"
    ((TESTS_PASSED++))
    return 0
  else
    echo -e "${RED}✗ FAIL${NC} (file not found: $file)"
    ((TESTS_FAILED++))
    return 1
  fi
}

# Function to check link in file
check_link() {
  local file=$1
  local link=$2
  local description=$3

  echo -n "Checking: $description... "
  if grep -q "$link" "$file"; then
    echo -e "${GREEN}✓ PASS${NC}"
    ((TESTS_PASSED++))
    return 0
  else
    echo -e "${RED}✗ FAIL${NC} (link not found: $link in $file)"
    ((TESTS_FAILED++))
    return 1
  fi
}

echo "Scenario 1: First-Time Marketplace Customer"
echo "--------------------------------------------"
echo ""

# Step 1: Customer arrives at Marketplace listing
check_file "marketplace/listing-package/README.md" "Marketplace README exists"

# Step 2: Reads overview
check_file "marketplace/listing-package/overview.md" "Overview document exists"
check_link "marketplace/listing-package/README.md" "overview.md" "README links to overview"

# Step 3: Reads architecture
check_file "marketplace/listing-package/architecture.md" "Architecture document exists"
check_link "marketplace/listing-package/README.md" "architecture.md" "README links to architecture"

# Step 4: Reads deployment guide
check_file "marketplace/listing-package/deployment-guide.md" "Deployment guide exists"
check_link "marketplace/listing-package/README.md" "deployment-guide.md" "README links to deployment guide"

# Step 5: Reads operations guide
check_file "marketplace/listing-package/operations-guide.md" "Operations guide exists"
check_link "marketplace/listing-package/README.md" "operations-guide.md" "README links to operations guide"

# Step 6: Encounters issue, finds troubleshooting
check_link "marketplace/listing-package/operations-guide.md" "troubleshooting" "Operations guide mentions troubleshooting"

# Step 7: Needs support, finds SUPPORT.md
check_file "docs/gcp/marketplace/SUPPORT.md" "Support document exists"

echo ""
echo "Scenario 2: Upgrading Customer"
echo "-------------------------------"
echo ""

# Step 1: Customer has CRE 0.3.0 deployed
check_file "docs/gcp/marketplace/UPGRADE.md" "Upgrade guide exists"

# Step 2: Reads UPGRADE.md
check_link "docs/gcp/marketplace/README.md" "UPGRADE.md" "Marketplace README links to upgrade guide"

# Step 3: Follows rolling upgrade procedure
check_link "docs/gcp/marketplace/UPGRADE.md" "rolling upgrade" "Upgrade guide describes rolling upgrade"

# Step 4: Encounters issue, follows rollback procedure
check_link "docs/gcp/marketplace/UPGRADE.md" "rollback" "Upgrade guide describes rollback"

echo ""
echo "Scenario 3: Troubleshooting Customer"
echo "-------------------------------------"
echo ""

# Step 1: Customer's CRE deployment has issues
check_file "marketplace/listing-package/operations-guide.md" "Operations guide exists"

# Step 2: Navigates to troubleshooting runbook
check_link "marketplace/listing-package/operations-guide.md" "troubleshooting" "Operations guide links to troubleshooting"

# Step 3: Follows diagnostic flowchart
check_link "docs/gcp/runbooks/README.md" "Incident Response" "Runbooks index has incident response flow"

# Step 4: Identifies issue
check_file "docs/gcp/runbooks/troubleshooting.md" "Troubleshooting runbook exists"

# Step 5: Applies fix or escalates
check_link "docs/gcp/runbooks/README.md" "Escalation Contacts" "Runbooks index has escalation contacts"

echo ""
echo "Scenario 4: Security/Compliance Review"
echo "---------------------------------------"
echo ""

# Step 1: Review security model
check_file "marketplace/listing-package/security-model.md" "Security model exists"

# Step 2: Review compliance
check_link "marketplace/listing-package/security-model.md" "SOC 2" "Security model mentions SOC 2"
check_link "marketplace/listing-package/security-model.md" "HIPAA" "Security model mentions HIPAA"

# Step 3: Review legal documents
check_file "marketplace/submission-package/legal/LICENSE.txt" "License exists"
check_file "marketplace/submission-package/legal/PRIVACY_POLICY.txt" "Privacy policy exists"
check_file "marketplace/submission-package/legal/SUPPORT_SLA.txt" "SLA exists"

echo ""
echo "Scenario 5: Cost Estimation"
echo "----------------------------"
echo ""

# Step 1: Review pricing
check_file "marketplace/listing-package/cost-model.md" "Cost model exists"

# Step 2: Understand BYOL model
check_link "marketplace/listing-package/cost-model.md" "BYOL" "Cost model explains BYOL"

# Step 3: Estimate costs
check_link "marketplace/listing-package/cost-model.md" "Total Cost of Ownership" "Cost model has TCO examples"

echo ""
echo "======================================"
echo "Test Results"
echo "======================================"
echo ""
echo -e "${GREEN}Tests Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Tests Failed: $TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
  echo -e "${GREEN}✓ All customer journey tests passed!${NC}"
  exit 0
else
  echo -e "${RED}✗ Some tests failed. Please review the output above.${NC}"
  exit 1
fi
