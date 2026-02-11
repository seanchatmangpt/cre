# CRE Security Scan Execution Examples

This document shows example commands and expected outputs for security scanning.

## Example 1: Local image scan with automatic tool installation

**Command:**
```bash
bash scripts/security-scan.sh \
  --docker-build \
  --install-tools
```

**What it does:**
1. Build the Docker image (cre:0.3.0)
2. Install Trivy if not present
3. Install Syft if not present
4. Scan the image for vulnerabilities
5. Generate SBOMs in 3 formats
6. Create a summary report

**Expected output files:**
```
security-reports/
├── trivy-scan.txt          # Human-readable vulnerability report
├── trivy-scan.json         # Machine-readable vulnerabilities
├── trivy-scan.sarif        # CI/CD integration format
├── sbom.spdx.json          # SPDX format SBOM
├── sbom.cyclonedx.json     # CycloneDX format SBOM (JSON)
├── sbom.cyclonedx.xml      # CycloneDX format SBOM (XML)
└── security-report.md      # Summary report
```

---

## Example 2: Scan an already-built image

**Command:**
```bash
docker build -t cre:0.3.0 -f Dockerfile .
bash scripts/security-scan.sh --image cre:0.3.0 --install-tools
```

**What it does:**
1. Use existing Docker image
2. Install tools if needed
3. Perform vulnerability scan
4. Generate SBOMs

---

## Example 3: Vulnerability scanning only (skip SBOM)

**Command:**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --skip-sbom \
  --install-tools
```

**Output files:**
```
security-reports/
├── trivy-scan.txt
├── trivy-scan.json
└── trivy-scan.sarif
```

---

## Example 4: SBOM generation only (skip vulnerability scan)

**Command:**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --skip-trivy \
  --install-tools
```

**Output files:**
```
security-reports/
├── sbom.spdx.json
├── sbom.cyclonedx.json
└── sbom.cyclonedx.xml
```

---

## Example 5: Analyzing vulnerability scan results

**Count vulnerabilities by severity (requires jq):**
```bash
cat security-reports/trivy-scan.json | jq '.Results[]?.Vulnerabilities[] | .Severity' | sort | uniq -c
```

**Find CRITICAL vulnerabilities:**
```bash
cat security-reports/trivy-scan.json | jq '.Results[]?.Vulnerabilities[] | select(.Severity=="CRITICAL")'
```

**Extract CVE details:**
```bash
cat security-reports/trivy-scan.json | jq '.Results[]?.Vulnerabilities[] | {id: .VulnerabilityID, severity: .Severity, fixed: .FixedVersion}'
```

---

## Example 6: Analyzing Software Bill of Materials

**Count packages:**
```bash
cat security-reports/sbom.cyclonedx.json | jq '.components | length'
```

**List all packages:**
```bash
cat security-reports/sbom.cyclonedx.json | jq '.components[] | "\(.name)@\(.version)"' | sort
```

**Extract Package URLs (PURLs) for supply chain analysis:**
```bash
cat security-reports/sbom.cyclonedx.json | jq '.components[] | .purl' | sort
```

**Find packages with embedded vulnerabilities:**
```bash
cat security-reports/sbom.cyclonedx.json | jq '.vulnerabilities[] | .id' | sort
```

---

## Example 7: GCP Cloud Build integration

**Command:**
```bash
gcloud builds submit --config=cloudbuild.yaml
```

**What it does:**
1. Trigger Cloud Build pipeline
2. Build multi-architecture images
3. Run Trivy scanning
4. Generate SBOMs
5. Push images to Artifact Registry
6. Upload reports to Cloud Storage

**View results:**
```bash
gsutil ls gs://PROJECT-security-reports/cre/
```

---

## Example 8: Scanning with custom severity levels

**Only CRITICAL vulnerabilities:**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --severity CRITICAL \
  --skip-sbom
```

**CRITICAL and HIGH:**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --severity CRITICAL,HIGH \
  --skip-sbom
```

**All levels (CRITICAL, HIGH, MEDIUM, LOW):**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --severity CRITICAL,HIGH,MEDIUM,LOW \
  --skip-sbom
```

---

## Example 9: Custom output directory

**Command:**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --output /tmp/cre-scan-results \
  --install-tools
```

**Results location:**
```
/tmp/cre-scan-results/
├── trivy-scan.txt
├── trivy-scan.json
├── trivy-scan.sarif
├── sbom.spdx.json
├── sbom.cyclonedx.json
├── sbom.cyclonedx.xml
└── security-report.md
```

---

## Example 10: Complete compliance workflow

**Step 1: Build image**
```bash
docker build -t cre:0.3.0 -f Dockerfile .
```

**Step 2: Run security scan**
```bash
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --output ./security-scan-2024-02-11
```

**Step 3: Review results**
```bash
cat ./security-scan-2024-02-11/security-report.md
head -50 ./security-scan-2024-02-11/trivy-scan.txt
```

**Step 4: Analyze vulnerabilities (if jq available)**
```bash
cat ./security-scan-2024-02-11/trivy-scan.json | jq '.Results[].Vulnerabilities[] | select(.Severity=="CRITICAL")'
```

**Step 5: Archive for compliance**
```bash
tar -czf cre-0.3.0-security-scan.tar.gz ./security-scan-2024-02-11/
sha256sum cre-0.3.0-security-scan.tar.gz > cre-0.3.0-security-scan.tar.gz.sha256
```

**Step 6: Upload to marketplace submission package**
```
# Include SBOMs and scan reports in submission
# Add to GCP Marketplace documentation
```

---

## Example 11: Install specific tools

**Install Trivy only:**
```bash
bash scripts/security-scan.sh --install-tools --skip-sbom --skip-trivy
```

**Install Syft only:**
```bash
bash scripts/security-scan.sh --install-tools --skip-trivy --skip-sbom
```

---

## Example 12: Monitoring vulnerability trends

**Create dated scan directory:**
```bash
SCAN_DATE=$(date +%Y-%m-%d)
bash scripts/security-scan.sh \
  --image cre:0.3.0 \
  --output "./security-scans/${SCAN_DATE}"
```

**Compare scans over time:**
```bash
# List vulnerability counts by date
for dir in security-scans/*/; do
  date=$(basename "$dir")
  count=$(jq '.Results[]?.Vulnerabilities[]?.VulnerabilityID' "$dir/trivy-scan.json" 2>/dev/null | wc -l)
  echo "$date: $count vulnerabilities"
done
```

---

## Example 13: Custom analysis with jq

**Group vulnerabilities by package:**
```bash
cat security-reports/trivy-scan.json | jq -r '
  .Results[]?.Vulnerabilities[] |
  "\(.PkgName): \(.VulnerabilityID) (\(.Severity))"' |
  sort
```

**Find packages with multiple vulnerabilities:**
```bash
cat security-reports/trivy-scan.json | jq -r '
  [.Results[]?.Vulnerabilities[] |
  {pkg: .PkgName, cve: .VulnerabilityID}] |
  group_by(.pkg) |
  map(select(length > 1)) |
  .[] | "Package: \(.[0].pkg) - \(length) vulnerabilities"'
```

**Extract fixed versions:**
```bash
cat security-reports/trivy-scan.json | jq -r '
  .Results[]?.Vulnerabilities[] |
  select(.FixedVersion != "" and .FixedVersion != null) |
  "\(.PkgName): \(.InstalledVersion) -> \(.FixedVersion)"' |
  sort
```

---

## Example 14: CI/CD integration with SARIF

**Upload SARIF to GitHub:**
```bash
# SARIF file is already generated as trivy-scan.sarif
# GitHub Actions can automatically detect and display it

# Example GitHub Actions step:
# - name: Upload Trivy scan to GitHub
#   uses: github/codeql-action/upload-sarif@v2
#   with:
#     sarif_file: 'security-reports/trivy-scan.sarif'
```

**Process SARIF programmatically:**
```bash
# Count SARIF results by level
cat security-reports/trivy-scan.sarif | jq '.runs[].results | map(.level) | group_by(.) | map({level: .[0], count: length})'
```

---

## Quick Reference

| Task | Command |
|------|---------|
| Full scan (build + scan + SBOM) | `bash scripts/security-scan.sh --docker-build --install-tools` |
| Scan existing image | `bash scripts/security-scan.sh --image cre:0.3.0 --install-tools` |
| Vulnerabilities only | `bash scripts/security-scan.sh --image cre:0.3.0 --skip-sbom --install-tools` |
| SBOM only | `bash scripts/security-scan.sh --image cre:0.3.0 --skip-trivy --install-tools` |
| Get help | `bash scripts/security-scan.sh --help` |
| View Trivy text report | `cat security-reports/trivy-scan.txt` |
| View Trivy JSON data | `cat security-reports/trivy-scan.json \| jq .` |
| Count packages in SBOM | `cat security-reports/sbom.cyclonedx.json \| jq '.components \| length'` |
| List all packages | `cat security-reports/sbom.cyclonedx.json \| jq '.components[] \| .name'` |

