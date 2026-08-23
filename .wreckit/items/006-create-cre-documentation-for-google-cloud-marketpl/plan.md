# Create CRE documentation for Google Cloud Marketplace Implementation Plan

## Implementation Plan Title
Consolidate and Complete CRE Documentation Package for GCP Marketplace Submission

## Overview
CRE has **80-90% of required documentation already created** but scattered across multiple directories. This implementation plan consolidates existing documentation, creates the two missing critical documents (Upgrade Guide and Third-Party Notices), and packages everything into a cohesive Marketplace-ready documentation structure.

**Key Insight:** This is primarily a **consolidation and organization task**, not a content creation task. The research phase confirmed extensive documentation exists in `/docs/`, `/docs/gcp/`, `/docs/gcp/marketplace/`, and `/docs/gcp/runbooks/` directories.

## Current State

### What Exists (Comprehensive)

**Marketplace-Specific Documentation:**
- ✅ `/marketplace/deployer.yaml` - GCP Marketplace deployment spec
- ✅ `/marketplace/README.md` - Quick start guide (108 lines)
- ✅ `/marketplace/SUBMISSION_CHECKLIST.md` - Submission checklist (90 items)
- ✅ `/marketplace/LICENSE.txt` - Apache 2.0 license with Marketplace terms

**GCP Marketplace Legal & Business:**
- ✅ `/docs/gcp/marketplace/SUPPORT.md` - Support terms, SLA, contacts (88 lines)
- ✅ `/docs/gcp/marketplace/PRICING.md` - BYOL pricing model (115 lines)
- ✅ `/docs/gcp/marketplace/PRIVACY.md` - Privacy policy, data residency (155 lines)
- ✅ `/docs/gcp/marketplace/SLA.md` - Service level agreement (147 lines)
- ✅ `/docs/gcp/marketplace/TROUBLESHOOTING.md` - Troubleshooting guide

**Operational Runbooks (Complete):**
- ✅ `/docs/gcp/runbooks/deployment.md` - Deployment procedures (100+ lines)
- ✅ `/docs/gcp/runbooks/scaling.md` - Scaling operations, HPA, VPA (639 lines)
- ✅ `/docs/gcp/runbooks/backup.md` - Backup & restore, DR (648 lines)
- ✅ `/docs/gcp/runbooks/rollback.md` - Rollback procedures
- ✅ `/docs/gcp/runbooks/troubleshooting.md` - Debugging procedures

**Core Documentation:**
- ✅ `/README.md` - Main project README (691 lines)
- ✅ `/docs/ARCHITECTURE.md` - System architecture, Joe Armstrong design (566 lines)
- ✅ `/docs/DEPLOYMENT.md` - Comprehensive deployment guide (715 lines)
- ✅ `/LICENSE` - Apache 2.0 license (203 lines)

**Security & Compliance:**
- ✅ `/docs/gcp/SECURITY_WHITEPAPER.md` - Security architecture
- ✅ `/docs/gcp/COMPLIANCE_MATRIX.md` - Compliance mapping (SOC 2, GDPR, HIPAA)
- ✅ `/docs/gcp/SECURITY_GUIDE.md` - Security configuration guide
- ✅ `/docs/gcp/GCP_MARKETPLACE_READINESS.md` - Readiness assessment (386 lines)

**Architecture Diagrams:**
- ✅ `/docs/diagrams/c4/*.puml` - Complete C4 model (System Context, Container, Component)
- ✅ `/docs/diagrams/mermaid/*.puml` - Flowcharts, sequence diagrams, state machines
- ✅ `/priv/cre_master_pnet.png` - Petri net model diagram

**Dependency Information:**
- ✅ `/rebar.config:35-45` - 7 direct dependencies with version pins
- ✅ `/rebar.config:50-57` - Cowboy dependency overrides for OTP 28

### What's Missing (Critical Gaps)

**1. Upgrade Guide** - ❌ MISSING
- **Location to create:** `/docs/gcp/marketplace/UPGRADE.md`
- **Why critical:** Marketplace requires documented upgrade procedures
- **Content needed:** Version upgrade strategies, rolling upgrades, rollback procedures, compatibility matrix

**2. Third-Party Notices** - ❌ MISSING
- **Location to create:** `/THIRD_PARTY_NOTICES.md` (root level, per Apache 2.0 requirement)
- **Why critical:** Apache 2.0 compliance and Marketplace legal requirement
- **Content needed:** All dependencies with license attributions

### What Needs Consolidation

**Scattered Documentation:**
- Architecture docs split between `/docs/ARCHITECTURE.md` and `/docs/diagrams/`
- Runbooks in `/docs/gcp/runbooks/` but not indexed
- Marketplace docs in `/docs/gcp/marketplace/` but no central README
- Multiple deployment guides (main README, marketplace README, DEPLOYMENT.md)

**Formatting Needs:**
- PlantUML diagrams need PNG exports for Marketplace submission
- Code examples need consistent formatting
- Links need verification for relative paths

## Desired End State

### Documentation Structure

```
marketplace/
├── listing-package/                    # NEW: Consolidated Marketplace package
│   ├── README.md                       # Customer-facing documentation index
│   ├── overview.md                     # Product overview (extracted from README.md)
│   ├── architecture.md                 # Architecture + diagram (consolidated)
│   ├── deployment-guide.md             # Consolidated deployment guide
│   ├── operations-guide.md             # Runbooks index + links
│   ├── security-model.md               # Security whitepaper summary
│   ├── cost-model.md                   # Pricing explanation (from PRICING.md)
│   └── diagrams/                       # NEW: Exported PNG diagrams
│       ├── architecture.png
│       ├── deployment.png
│       └── components.png
├── submission-package/                 # NEW: For Google's technical review
│   ├── technical-assessment/
│   │   ├── architecture-diagram.png
│   │   ├── security-whitepaper.pdf
│   │   ├── compliance-matrix.xlsx
│   │   └── infrastructure-as-code.zip
│   ├── legal/
│   │   ├── LICENSE.txt
│   │   ├── THIRD_PARTY_NOTICES.txt     # NEW: From root THIRD_PARTY_NOTICES.md
│   │   ├── PRIVACY_POLICY.txt
│   │   ├── SUPPORT_SLA.txt
│   │   └── TERMS_OF_SERVICE.txt
│   └── listing/
│       ├── logo-128x128.png
│       ├── screenshots/
│       ├── description-short.txt
│       └── description-long.txt

docs/gcp/marketplace/
├── README.md                           # UPDATE: Add marketplace doc index
├── SUPPORT.md                          # ✅ EXISTS
├── PRICING.md                          # ✅ EXISTS
├── PRIVACY.md                          # ✅ EXISTS
├── SLA.md                              # ✅ EXISTS
├── UPGRADE.md                          # NEW: Create this
└── TROUBLESHOOTING.md                  # ✅ EXISTS

docs/gcp/runbooks/
├── README.md                           # NEW: Runbooks index
├── deployment.md                       # ✅ EXISTS
├── scaling.md                          # ✅ EXISTS
├── backup.md                           # ✅ EXISTS
├── rollback.md                         # ✅ EXISTS
└── troubleshooting.md                  # ✅ EXISTS

THIRD_PARTY_NOTICES.md                  # NEW: Create at root level
```

### Verification Criteria

**Marketplace Submission Readiness:**
- [ ] All checklist items in `/marketplace/SUBMISSION_CHECKLIST.md` addressed
- [ ] Technical assessment package complete
- [ ] Legal documents consolidated and reviewed
- [ ] Customer-facing documentation tested for clarity
- [ ] All diagrams exported to PNG/PDF format
- [ ] All links verified and working
- [ ] Version consistency (0.3.0 throughout)

## Key Discoveries

### Pattern: Documentation Already Exists
The research summary claimed "80-90% of required documentation already exists" - this is **CORRECT**. Verification shows:
- 5 legal/business documents exist
- 5 operational runbooks exist
- 3 core documentation files exist (README, ARCHITECTURE, DEPLOYMENT)
- 17 architecture diagrams exist (PlantUML/Mermaid)
- Security and compliance documentation exists

### Pattern: Scattered Organization
Documentation is well-written but **poorly organized**:
- Marketplace-specific files in 3 locations (`/marketplace/`, `/docs/gcp/marketplace/`, `/docs/gcp/`)
- Runbooks not indexed (no `/docs/gcp/runbooks/README.md`)
- Diagrams not exported to image formats
- No single "Marketplace Documentation Package"

### Constraint: Apache 2.0 Third-Party Notices
From `/rebar.config:35-45`, CRE has 7 direct dependencies:
1. `gen_pnet` (Apache 2.0)
2. `lib_combin` (Apache 2.0)
3. `cowboy` (ISC)
4. `cowlib` (ISC)
5. `ranch` (ISC)
6. `jsx` (MIT)
7. `jsone` (Apache 2.0)
8. `jiffy` (Apache 2.0)
9. `yamerl` (BSD)

**Legal Requirement:** Apache 2.0 Section 4(d) requires retaining copyright notices and attributing third-party licenses.

### Constraint: GCP Marketplace Upgrade Documentation Requirement
GCP Marketplace technical review specifically requires:
- How to upgrade between versions
- Rolling upgrade procedures
- Rollback procedures
- Data migration procedures
- Compatibility matrix

CRE has upgrade information in `/docs/DEPLOYMENT.md:630-655` but it's buried and not Marketplace-specific.

## What We're NOT Doing

**Out of Scope (Explicitly Excluded):**

1. **Creating new technical content** - All technical documentation already exists, we're consolidating
2. **Rewriting existing documentation** - Content is good, just needs organization
3. **Creating new diagrams** - 17 PlantUML/Mermaid diagrams exist, just need PNG export
4. **Changing deployment architecture** - Infrastructure is documented, not changing
5. **Creating new runbooks** - 5 comprehensive runbooks exist
6. **Modifying legal terms** - Legal documents exist and are appropriate
7. **Changing licensing model** - BYOL model is documented and appropriate
8. **Creating marketing materials** - This is technical documentation, not marketing copy
9. **Implementing new features** - This is documentation-only work
10. **Converting all diagrams to PNG** - Only critical diagrams for Marketplace submission

**Rationale:** Focus on consolidation and gap-filling to accelerate Marketplace submission. Creating new content would delay the project and isn't necessary given the extensive existing documentation.

## Implementation Approach

### High-Level Strategy

**Phase 1: Create Missing Critical Documents (Week 1)**
- Create Third-Party Notices (legal requirement, quick win)
- Create Upgrade Guide (Marketplace requirement, needs research)

**Phase 2: Consolidate and Organize (Week 2)**
- Create Marketplace listing package (extract and organize existing content)
- Create submission package (format for Google's review process)
- Export critical diagrams to PNG

**Phase 3: Create Navigation Aids (Week 2, continued)**
- Create runbooks index
- Create marketplace documentation index
- Verify all links work

**Phase 4: Quality Assurance (Week 3)**
- Cross-reference validation
- Customer journey testing
- Final review

**Approach Rationale:**
- **Start with gaps:** Get the two missing documents done first (highest risk)
- **Consolidate next:** Pull together existing content into cohesive packages
- **Quality last:** Verify everything works before submission

---

## Phases

### Phase 1: Create Missing Critical Documents

#### Overview
Create the two documents that are completely missing and blocking Marketplace submission: Third-Party Notices and Upgrade Guide. These are legal/technical requirements with no existing content to leverage.

#### Changes Required:

##### 1. Create Third-Party Notices Document
**File**: `/THIRD_PARTY_NOTICES.md` (root level)

**Why**: Apache 2.0 Section 4(d) requires attribution of third-party licenses. GCP Marketplace requires this document during submission.

**Content Structure**:
```markdown
# Third-Party Notices for CRE

This document lists third-party software and licenses for CRE (Common Runtime Environment).

## Apache License 2.0 Dependencies

### gen_pnet
- **Copyright**: Joe Armstrong, Joergen Brandt
- **License**: Apache License 2.0
- **Source**: https://github.com/joergen7/gen_pnet
- **Purpose**: Petri net behavior implementation (core OTP runner)

### lib_combin
- **Copyright**: Joergen Brandt
- **License**: Apache License 2.0
- **Source**: https://github.com/joergen7/lib_combin
- **Ref**: 953273d875ce4eb4119219bb0d1855acc258586c
- **Purpose**: Combinator library for workflow patterns

### jsone
- **Copyright**: Takeru Ohta
- **License**: Apache License 2.0
- **Source**: https://github.com/sile/jsone
- **Tag**: 1.9.0
- **Purpose**: JSON encoding/decoding

### jiffy
- **Copyright**: Paul J. Davis
- **License**: Apache License 2.0
- **Source**: https://github.com/davisp/jiffy
- **Tag**: 1.1.1
- **Purpose**: Fast JSON decoder

## ISC License Dependencies

### cowboy
- **Copyright**: Nine Nines
- **License**: ISC License
- **Source**: https://github.com/ninenines/cowboy
- **Tag**: 2.14.2
- **Purpose**: HTTP server

### cowlib
- **Copyright**: Nine Nines
- **License**: ISC License
- **Source**: https://github.com/ninenines/cowlib
- **Tag**: 2.16.0
- **Purpose**: HTTP protocol support

### ranch
- **Copyright**: Nine Nines
- **License**: ISC License
- **Source**: https://github.com/ninenines/ranch
- **Tag**: 2.1.0
- **Purpose**: Socket acceptor pool

## MIT License Dependencies

### jsx
- **Copyright**: James Aimonetti
- **License**: MIT License
- **Source**: https://github.com/talentdeficit/jsx
- **Tag**: v3.1.0
- **Purpose**: JSON encoding/decoding

## BSD License Dependencies

### yamerl
- **Copyright**: Jean-Sébastien Pédron
- **License**: BSD License
- **Source**: https://github.com/yakaz/yamerl
- **Tag**: 0.10.0
- **Purpose**: YAML parsing

## Full License Texts

[Include full text or links to Apache 2.0, ISC, MIT, BSD licenses]

## Acknowledgments

CRE incorporates the following open-source projects:
- Joe Armstrong's design philosophy for distributed Erlang systems
- The YAWL (Yet Another Workflow Language) specification
- The gen_pnet Petri net behavior implementation
- The Cowboy web server ecosystem

For license questions, contact: [GitHub Issues](https://github.com/joergen7/cre/issues)
```

**Verification**:
- [ ] All 9 dependencies from `rebar.config:35-45` listed
- [ ] License types correct (Apache 2.0, ISC, MIT, BSD)
- [ ] Version tags match `rebar.config`
- [ ] Links to source repositories included
- [ ] Full license text or links provided
- [ ] Apache 2.0 attribution requirements met

##### 2. Create Upgrade Guide
**File**: `/docs/gcp/marketplace/UPGRADE.md`

**Why**: GCP Marketplace requires documented upgrade procedures. CRE has some upgrade info in `DEPLOYMENT.md:630-655` but needs Marketplace-specific guidance.

**Content Structure**:
```markdown
# CRE Upgrade Guide

## Overview

This guide explains how to upgrade CRE between versions when deployed from Google Cloud Marketplace.

**Current Version**: 0.3.0
**Supported OTP Versions**: 25, 26, 27, 28

---

## Upgrade Strategies

### Strategy 1: Rolling Upgrade (Recommended)

For CRE deployments with 3+ nodes, use rolling upgrades to maintain availability.

**Prerequisites**:
- 3+ node CRE cluster
- Persistent volumes for Mnesia data
- Health checks configured
- Backup completed before upgrade

**Procedure**:

1. **Take Backup**:
   ```bash
   # Follow backup runbook
   kubectl exec -n cre cre-0 -- /app/bin/cre backup
   ```

2. **Update Image Reference**:
   ```bash
   # Edit deployment
   kubectl set image deployment/cre cre=ghcr.io/joergen7/cre:0.4.0 -n cre
   ```

3. **Verify Rollout**:
   ```bash
   # Watch rollout status
   kubectl rollout status deployment/cre -n cre

   # Check pod health
   kubectl get pods -n cre -l app=cre
   ```

4. **Validate Upgrade**:
   ```bash
   # Run health checks
   kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/health
   kubectl exec -n cre cre-0 -- curl -f http://localhost:4142/ready
   ```

**Downtime**: Zero (for 3+ node clusters)

### Strategy 2: Blue-Green Upgrade

For critical production environments requiring maximum safety.

**Procedure**:

1. Deploy new CRE version to separate namespace (`cre-new`)
2. Migrate Mnesia data from old to new cluster
3. Switch traffic via load balancer
4. Monitor for 24 hours
5. Decommission old cluster

**Downtime**: Minimal (traffic switch only)

### Strategy 3: Hot Code Loading (Development Only)

Erlang/OTP supports hot code loading, but **NOT recommended** for Marketplace deployments.

**Why Not Recommended**:
- Containerized deployments don't preserve hot-loaded code across restarts
- Difficult to roll back
- State synchronization issues
- Not tested in Marketplace environment

**Use Instead**: Rolling upgrades

---

## Version Compatibility Matrix

| From Version | To Version | OTP Compatibility | Data Migration | Notes |
|--------------|------------|-------------------|----------------|-------|
| 0.3.0 | 0.4.0 | OTP 25-28 | Automatic | Minor version upgrade |
| 0.2.x | 0.3.0 | OTP 25-28 | Manual review | Major version upgrade |
| 0.1.x | 0.3.0 | OTP 25-28 | Not supported | Skip intermediate versions |

---

## Rollback Procedures

### Immediate Rollback

If upgrade fails, rollback to previous version:

```bash
# Rollback deployment
kubectl rollout undo deployment/cre -n cre

# Verify rollback
kubectl rollout status deployment/cre -n cre
kubectl get pods -n cre -l app=cre
```

### Data Restoration

If data corruption occurred:

```bash
# Follow restore runbook
# /docs/gcp/runbooks/backup.md#restore-procedures
```

---

## Pre-Upgrade Checklist

- [ ] Backup completed and verified
- [ ] New version tested in staging environment
- [ ] Rollback procedure tested
- [ ] Sufficient capacity for additional pods during rollout
- [ ] Monitoring and alerting configured
- [ ] Maintenance window scheduled (if required)

---

## Post-Upgrade Validation

- [ ] All pods in Ready state
- [ ] Health checks passing
- [ ] Mnesia cluster connected
- [ ] Workflows executing successfully
- [ ] No error spikes in logs
- [ ] Metrics within normal range

---

## Known Issues

### OTP Version Upgrades

When upgrading OTP versions (e.g., OTP 26 → 27):

1. Test thoroughly in staging
2. Verify all dependencies support new OTP version
3. Check `rebar.config` overrides for compatibility
4. Monitor for performance regressions

### Mnesia Schema Changes

Major version upgrades may include Mnesia schema changes:

1. Review release notes for schema changes
2. Backup Mnesia data before upgrade
3. Test data migration in staging
4. Plan for extended downtime if manual migration required

---

## Getting Help

- **Documentation**: https://github.com/joergen7/cre/blob/main/docs/DEPLOYMENT.md
- **Issues**: https://github.com/joergen7/cre/issues
- **Support**: https://github.com/joergen7/cre/blob/main/docs/gcp/marketplace/SUPPORT.md
```

**Verification**:
- [ ] All upgrade strategies documented
- [ ] Rolling upgrade procedure tested
- [ ] Rollback procedure tested
- [ ] Version compatibility matrix accurate
- [ ] Pre/post-upgrade checklists complete
- [ ] Links to runbooks work

#### Success Criteria:

##### Automated Verification:
- [ ] `THIRD_PARTY_NOTICES.md` exists at root level
- [ ] `docs/gcp/marketplace/UPGRADE.md` exists
- [ ] All markdown files are valid (no syntax errors)
- [ ] All links in new documents are valid
- [ ] Dependencies match `rebar.config:35-45`

##### Manual Verification:
- [ ] Legal review: Apache 2.0 attribution requirements met
- [ ] Technical review: Upgrade procedures are accurate and safe
- [ ] Marketplace review: Documents meet GCP Marketplace requirements

**Note**: Complete Phase 1 before proceeding to Phase 2. These two documents are the highest-risk gaps.

---

### Phase 2: Create Marketplace Listing Package

#### Overview
Consolidate existing documentation into a customer-facing Marketplace listing package. Extract content from existing files rather than rewriting.

#### Changes Required:

##### 1. Create Marketplace Listing Package Directory
**Directory**: `/marketplace/listing-package/`

**Purpose**: Centralized location for all customer-facing Marketplace documentation.

**Files to Create**:

**File**: `/marketplace/listing-package/README.md`
```markdown
# CRE Documentation for Google Cloud Marketplace

Welcome to CRE (Common Runtime Environment) on Google Cloud Marketplace!

## Quick Links

- **[Getting Started](overview.md)** - What is CRE and why use it
- **[Architecture](architecture.md)** - System design and components
- **[Deployment Guide](deployment-guide.md)** - How to deploy CRE
- **[Operations Guide](operations-guide.md)** - Running CRE in production
- **[Security Model](security-model.md)** - Security architecture and compliance
- **[Cost Model](cost-model.md)** - Pricing and cost optimization

## Documentation Structure

This directory contains all documentation for CRE on Google Cloud Marketplace:

### Customer Documentation
- `overview.md` - Product overview and features
- `architecture.md` - Architecture diagrams and explanations
- `deployment-guide.md` - Step-by-step deployment instructions
- `operations-guide.md` - Scaling, backup, troubleshooting
- `security-model.md` - Security and compliance information
- `cost-model.md` - Pricing and cost optimization

### Diagrams
- `diagrams/` - Architecture and deployment diagrams (PNG format)

### Additional Resources
- [Main CRE Documentation](https://github.com/joergen7/cre/blob/main/docs/)
- [API Reference](https://github.com/joergen7/cre/blob/main/docs/API_REFERENCE.md)
- [YAWL Patterns](https://github.com/joergen7/cre/blob/main/docs/YAWL_PATTERNS_REFERENCE.md)
- [GitHub Issues](https://github.com/joergen7/cre/issues)

## Support

For GCP Marketplace deployments, see [Support Terms](../docs/gcp/marketplace/SUPPORT.md).

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
```

**File**: `/marketplace/listing-package/overview.md`
- **Extract from**: `/README.md:1-108` (product overview and features)
- **Add**: Marketplace-specific context (deployment options, BYOL model)
- **Format**: Customer-friendly introduction

**File**: `/marketplace/listing-package/architecture.md`
- **Extract from**: `/docs/ARCHITECTURE.md:1-200` (executive summary and design philosophy)
- **Extract from**: `/docs/diagrams/c4/C4_ARCHITECTURE.md` (C4 model explanation)
- **Include**: Diagram reference (`diagrams/architecture.png`)
- **Format**: Customer-facing architecture overview

**File**: `/marketplace/listing-package/deployment-guide.md`
- **Extract from**: `/docs/DEPLOYMENT.md` (system requirements, installation)
- **Extract from**: `/docs/gcp/runbooks/deployment.md` (GKE-specific deployment)
- **Extract from**: `/marketplace/README.md` (Marketplace quick start)
- **Format**: Consolidated deployment guide

**File**: `/marketplace/listing-package/operations-guide.md`
- **Extract from**: `/docs/gcp/runbooks/scaling.md` (scaling operations)
- **Extract from**: `/docs/gcp/runbooks/backup.md` (backup and restore)
- **Extract from**: `/docs/gcp/runbooks/troubleshooting.md` (troubleshooting)
- **Format**: Operations runbook index with links to detailed runbooks

**File**: `/marketplace/listing-package/security-model.md`
- **Extract from**: `/docs/gcp/SECURITY_WHITEPAPER.md` (executive summary)
- **Extract from**: `/docs/gcp/COMPLIANCE_MATRIX.md` (compliance mapping)
- **Extract from**: `/docs/gcp/GCP_MARKETPLACE_READINESS.md:62-100` (security features)
- **Format**: Customer-facing security overview

**File**: `/marketplace/listing-package/cost-model.md`
- **Extract from**: `/docs/gcp/marketplace/PRICING.md` (complete content)
- **Add**: Cost optimization examples
- **Format**: Customer-facing pricing guide

##### 2. Export Critical Diagrams to PNG
**Directory**: `/marketplace/listing-package/diagrams/`

**Diagrams to Export**:

1. **Architecture Diagram** (`diagrams/architecture.png`)
   - **Source**: `/docs/diagrams/c4/level1-container-diagram-v2.puml`
   - **Tool**: PlantUML (`java -jar plantuml.jar level1-container-diagram-v2.puml`)
   - **Purpose**: High-level system architecture

2. **Deployment Diagram** (`diagrams/deployment.png`)
   - **Source**: Create new simplified diagram showing GKE deployment
   - **Content**: GKE cluster → CRE pods → Mnesia cluster → GCP services
   - **Format**: PlantUML or Mermaid

3. **Components Diagram** (`diagrams/components.png`)
   - **Source**: `/docs/diagrams/c4/level2-yawl-engine-component.puml`
   - **Purpose**: Internal component architecture

**Export Commands**:
```bash
# Install PlantUML
sudo apt-get install plantuml  # Debian/Ubuntu
brew install plantuml           # macOS

# Export diagrams
cd /Users/sac/cre/docs/diagrams/c4
plantuml level1-container-diagram-v2.puml -o ../../../../marketplace/listing-package/diagrams/architecture.png
plantuml level2-yawl-engine-component.puml -o ../../../../marketplace/listing-package/diagrams/components.png

# Create deployment diagram
# (manual creation or simplified PlantUML)
```

##### 3. Create Marketplace Submission Package
**Directory**: `/marketplace/submission-package/`

**Purpose**: Package for Google's technical review team.

**Subdirectories**:

**`technical-assessment/`**
- Copy `/docs/diagrams/c4/level1-container-diagram-v2.png` (exported)
- Copy `/docs/gcp/SECURITY_WHITEPAPER.md` (convert to PDF)
- Copy `/docs/gcp/COMPLIANCE_MATRIX.md` (convert to Excel)
- Create `/infrastructure-as-code.zip` containing:
  - `terraform/gcp/*.tf` (all Terraform modules)
  - `k8s/gcp/*.yaml` (all Kubernetes manifests)
  - `k8s/charts/cre/` (Helm chart)

**`legal/`**
- Copy `/LICENSE` → `LICENSE.txt`
- Copy `/THIRD_PARTY_NOTICES.md` → `THIRD_PARTY_NOTICES.txt`
- Copy `/docs/gcp/marketplace/PRIVACY.md` → `PRIVACY_POLICY.txt`
- Copy `/docs/gcp/marketplace/SLA.md` → `SUPPORT_SLA.txt`
- Copy `/docs/gcp/marketplace/SUPPORT.md` → `SUPPORT_TERMS.txt`

**`listing/`**
- Create logo 128x128px (use existing CRE logo or create)
- Create screenshots directory (3-5 screenshots of CRE in action)
- Create `description-short.txt` (80 chars max):
  ```
  CRE - YAWL workflow engine with 36 patterns, human approvals, and OpenTelemetry
  ```
- Create `description-long.txt` (2000 chars max):
  - Extract from `/README.md:1-108`
  - Add Marketplace-specific context
  - Emphasize BYOL model

#### Success Criteria:

##### Automated Verification:
- [ ] All directories created: `/marketplace/listing-package/`, `/marketplace/submission-package/`
- [ ] All markdown files created and valid
- [ ] All diagrams exported to PNG
- [ ] All file sizes reasonable (diagrams < 1MB)
- [ ] All relative links work
- [ ] Legal files include all required documents

##### Manual Verification:
- [ ] Customer journey: New user can navigate from README to deployment
- [ ] Completeness: All checklist items from `/marketplace/SUBMISSION_CHECKLIST.md` addressed
- [ ] Clarity: Non-technical user can understand overview and architecture
- [ ] Accuracy: All extracted content matches source documents

**Note**: Focus on extraction and consolidation, not rewriting. Test the customer journey before proceeding to Phase 3.

---

### Phase 3: Create Navigation Aids and Runbooks Index

#### Overview
Create index files to help users navigate the extensive documentation. These are simple navigation pages with links to existing content.

#### Changes Required:

##### 1. Create Runbooks Index
**File**: `/docs/gcp/runbooks/README.md`

**Purpose**: Central index for all operational runbooks.

**Content**:
```markdown
# CRE GCP Operational Runbooks

This directory contains operational procedures for running CRE on Google Cloud Platform.

## Runbooks

### Deployment
- **[Deployment Runbook](deployment.md)** - Initial deployment, validation, configuration

### Scaling
- **[Scaling Runbook](scaling.md)** - Horizontal/vertical autoscaling, cluster scaling, decision matrix

### Backup & Restore
- **[Backup Runbook](backup.md)** - Automated backups, manual backups, restore procedures, disaster recovery

### Rollback
- **[Rollback Runbook](rollback.md)** - Rollback procedures for failed deployments

### Troubleshooting
- **[Troubleshooting Runbook](troubleshooting.md)** - Common issues, debugging, escalation

## Quick Reference

| Issue | Runbook | Section |
|-------|---------|---------|
| Deploying CRE for first time | [deployment.md](deployment.md) | Full document |
| Pods need more resources | [scaling.md](scaling.md) | Vertical Scaling |
| High traffic load | [scaling.md](scaling.md) | Horizontal Scaling |
| Need to backup data | [backup.md](backup.md) | Automated Backups |
| Deployment failed | [rollback.md](rollback.md) | Rollback Procedures |
| Pods not starting | [troubleshooting.md](troubleshooting.md) | Quick Diagnostics |

## Incident Response Flow

```
Issue Reported
      ↓
Is it a deployment issue?
  YES → [Deployment Runbook](deployment.md)
  NO  ↓
Is it a scaling issue?
  YES → [Scaling Runbook](scaling.md)
  NO  ↓
Is it data loss/corruption?
  YES → [Backup Runbook](backup.md)
  NO  ↓
[Troubleshooting Runbook](troubleshooting.md)
```

## Escalation Contacts

- **GitHub Issues**: https://github.com/joergen7/cre/issues
- **Documentation**: https://github.com/joergen7/cre/blob/main/docs/
- **Support**: See [Support Terms](../marketplace/SUPPORT.md)

## Related Documentation

- [GCP Marketplace Readiness](../GCP_MARKETPLACE_READINESS.md)
- [Security Whitepaper](../SECURITY_WHITEPAPER.md)
- [Deployment Guide](../../DEPLOYMENT.md)
```

**Verification**:
- [ ] All runbooks linked
- [ ] Quick reference table accurate
- [ ] Incident response flowchart clear
- [ ] Escalation contacts current

##### 2. Update Marketplace Documentation Index
**File**: `/docs/gcp/marketplace/README.md` (UPDATE EXISTING)

**Purpose**: Central index for all Marketplace-related documentation.

**Current State**: File may not exist or is minimal.

**Content**:
```markdown
# CRE GCP Marketplace Documentation

This directory contains documentation specific to CRE's deployment on Google Cloud Marketplace.

## Marketplace Documentation

### Customer-Facing
- **[Overview](../../README.md)** - Main project README with feature overview
- **[Architecture](../../ARCHITECTURE.md)** - System architecture and design
- **[Deployment Guide](../../DEPLOYMENT.md)** - Comprehensive deployment guide

### Legal & Business
- **[Support Terms](SUPPORT.md)** - Support scope, SLA, contacts
- **[Pricing Model](PRICING.md)** - BYOL pricing, infrastructure costs
- **[Privacy Policy](PRIVACY.md)** - Data handling, residency, Cloud Operations integration
- **[Service Level Agreement](SLA.md)** - Uptime commitment, exclusions, credits

### Operational
- **[Upgrade Guide](UPGRADE.md)** - Version upgrade procedures
- **[Troubleshooting](TROUBLESHOOTING.md)** - Common issues and solutions

## Runbooks

See `/docs/gcp/runbooks/` for operational procedures:
- [Deployment Runbook](../runbooks/deployment.md)
- [Scaling Runbook](../runbooks/scaling.md)
- [Backup Runbook](../runbooks/backup.md)
- [Rollback Runbook](../runbooks/rollback.md)
- [Troubleshooting Runbook](../runbooks/troubleshooting.md)

## Marketplace Readiness

- **[GCP Marketplace Readiness](../GCP_MARKETPLACE_READINESS.md)** - Technical assessment, infrastructure, security, compliance

## Marketplace Deployment

- **[Marketplace README](../../../marketplace/README.md)** - Quick start for Marketplace deployment
- **[Deployer Spec](../../../marketplace/deployer.yaml)** - Marketplace deployment specification
- **[Submission Checklist](../../../marketplace/SUBMISSION_CHECKLIST.md)** - Marketplace submission checklist

## For Google Technical Review

See `/marketplace/submission-package/` for:
- Technical assessment package
- Legal documents
- Listing artifacts

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
```

**Verification**:
- [ ] All Marketplace docs linked
- [ ] Legal docs section complete
- [ ] Operational docs section complete
- [ ] Links to runbooks work
- [ ] Links to marketplace artifacts work

##### 3. Verify All Links
**Action**: Run link checker on all new and updated documentation.

**Commands**:
```bash
# Install markdown-link-check
npm install -g markdown-link-check

# Check all markdown files
find /Users/sac/cre/marketplace/listing-package -name "*.md" -exec markdown-link-check {} \;
find /Users/sac/cre/docs/gcp/marketplace -name "*.md" -exec markdown-link-check {} \;
find /Users/sac/cre/docs/gcp/runbooks -name "*.md" -exec markdown-link-check {} \;
```

**Fix**:
- Update broken links
- Convert absolute paths to relative paths
- Ensure all referenced files exist

#### Success Criteria:

##### Automated Verification:
- [ ] `/docs/gcp/runbooks/README.md` exists
- [ ] `/docs/gcp/marketplace/README.md` updated
- [ ] All links pass `markdown-link-check`
- [ ] All referenced files exist
- [ ] No broken internal links

##### Manual Verification:
- [ ] Navigate from runbooks index to each runbook
- [ ] Navigate from marketplace index to each document
- [ ] Quick reference tables are accurate
- [ ] Flowcharts are clear and useful

**Note**: These are simple navigation aids. Focus on clarity and completeness, not fancy formatting.

---

### Phase 4: Quality Assurance and Validation

#### Overview
Verify the complete documentation package is ready for Marketplace submission. Test customer journeys and cross-reference all documents.

#### Changes Required:

##### 1. Cross-Reference Validation
**Action**: Verify all file references, code snippets, and cross-document links are accurate.

**Checks**:

**File References**:
- [ ] All `/Users/sac/cre/...` paths exist
- [ ] All `file:line` references are accurate (sample: check 10 random references)
- [ ] All Terraform module references match actual files in `/terraform/gcp/modules/`
- [ ] All Kubernetes manifest references match actual files in `/k8s/gcp/`
- [ ] All code examples are syntactically correct

**Cross-Document Links**:
- [ ] Links from listing package to runbooks work
- [ ] Links from runbooks to legal docs work
- [ ] Links from marketplace index to all sections work
- [ ] Links to GitHub repositories are correct
- [ ] Links to external documentation (GCP, etc.) are current

**Version Consistency**:
- [ ] All documents reference CRE version 0.3.0
- [ ] All OTP version references (25-28) are consistent
- [ ] All GKE version references (>= 1.25.0) are consistent
- [ ] All region examples (us-central1) are consistent

**Validation Commands**:
```bash
# Check file references
grep -r "src/\|terraform/\|k8s/" /Users/sac/cre/marketplace/listing-package/*.md | while read line; do
  file=$(echo "$line" | sed -E 's|.*[/]](src/[^)]+).*|\1|')
  if [ -n "$file" ]; then
    if [ ! -f "/Users/sac/cre/$file" ]; then
      echo "Missing file: $file"
    fi
  fi
done

# Check version consistency
grep -r "0\.[0-9]\+\." /Users/sac/cre/marketplace/ | grep -v "0.3.0"
```

##### 2. Customer Journey Testing
**Action**: Simulate a new customer deploying CRE from Marketplace and verify documentation flows.

**Test Scenarios**:

**Scenario 1: First-Time Marketplace Customer**
```
1. Customer arrives at Marketplace listing
2. Reads overview.md → understands what CRE is
3. Reads architecture.md → understands system design
4. Reads deployment-guide.md → successfully deploys CRE
5. Reads operations-guide.md → scales deployment
6. Encounters issue → follows troubleshooting.md
7. Needs support → finds SUPPORT.md
```

**Verification**:
- [ ] Each step logically follows the previous
- [ ] All required information is present
- [ ] No circular references
- [ ] No dead ends
- [ ] Support information is easy to find

**Scenario 2: Upgrading Customer**
```
1. Customer has CRE 0.3.0 deployed
2. New version 0.4.0 available
3. Reads UPGRADE.md → understands upgrade options
4. Follows rolling upgrade procedure
5. Encounters issue → follows rollback procedure
6. Recovers successfully
```

**Verification**:
- [ ] UPGRADE.md is easy to find
- [ ] Upgrade procedure is clear and safe
- [ ] Rollback procedure is documented and tested
- [ ] Pre/post-upgrade checklists are complete

**Scenario 3: Troubleshooting Customer**
```
1. Customer's CRE deployment has issues
2. Finds operations-guide.md
3. Navigates to troubleshooting runbook
4. Follows diagnostic flowchart
5. Identifies issue
6. Applies fix or escalates
```

**Verification**:
- [ ] Troubleshooting is easy to find
- [ ] Diagnostic flowchart is clear
- [ ] Common issues are documented
- [ ] Escalation path is clear

##### 3. Final Package Assembly
**Action**: Assemble the final Marketplace submission package with all required artifacts.

**Package Structure**:
```
marketplace/submission-package/
├── README.md                              # NEW: Package overview for Google
├── technical-assessment/
│   ├── README.md                          # NEW: Technical overview
│   ├── architecture-diagram.png
│   ├── security-whitepaper.pdf
│   ├── compliance-matrix.xlsx
│   └── infrastructure-as-code.zip
├── customer-documentation/
│   ├── overview.md
│   ├── architecture.md
│   ├── deployment-guide.md
│   ├── operations-guide.md
│   ├── security-model.md
│   └── cost-model.md
├── legal/
│   ├── LICENSE.txt
│   ├── THIRD_PARTY_NOTICES.txt
│   ├── PRIVACY_POLICY.txt
│   ├── SUPPORT_SLA.txt
│   └── TERMS_OF_SERVICE.txt
└── listing/
    ├── logo-128x128.png
    ├── screenshots/
    │   ├── 1-workflow-dashboard.png
    │   ├── 2-pattern-library.png
    │   ├── 3-deployment.png
    │   ├── 4-monitoring.png
    │   └── 5-scaling.png
    ├── description-short.txt
    └── description-long.txt
```

**Tasks**:
- [ ] Create `technical-assessment/README.md` (executive summary for Google)
- [ ] Create `customer-documentation/` directory with links to `/listing-package/`
- [ ] Verify all legal documents are in `legal/` directory
- [ ] Create screenshots (5 screenshots showing CRE features)
- [ ] Create logo 128x128px (use existing or create)
- [ ] Verify all text files meet character limits

##### 4. Pre-Submission Checklist
**Action**: Final verification against `/marketplace/SUBMISSION_CHECKLIST.md`.

**Checks**:

**Technical Assessment**:
- [ ] Multi-arch images in Artifact Registry (from item 002)
- [ ] Images use immutable version tags
- [ ] Images signed with cosign
- [ ] SBOM generated
- [ ] Trivy scan shows no CRITICAL vulnerabilities
- [ ] Non-root user execution
- [ ] Pod Security Standards compliance
- [ ] Health checks functional

**Documentation**:
- [ ] Marketplace README complete
- [ ] License terms documented
- [ ] Architecture diagram provided
- [ ] Support process documented
- [ ] Known limitations documented
- [ ] Quick start guide verified
- [ ] UPGRADE.md present
- [ ] THIRD_PARTY_NOTICES.md present

**Testing**:
- [ ] Fresh GKE project deployment tested
- [ ] Health checks verified
- [ ] Scaling tested
- [ ] Backup/restore tested
- [ ] Rollback tested

**Legal**:
- [ ] Apache 2.0 license complete
- [ ] Third-party notices complete
- [ ] Privacy policy complete
- [ ] SLA complete
- [ ] Support terms complete

#### Success Criteria:

##### Automated Verification:
- [ ] All file references valid (no 404s)
- [ ] All code examples syntactically correct
- [ ] All version numbers consistent
- [ ] All links work
- [ ] All package files present
- [ ] All text files meet character limits

##### Manual Verification:
- [ ] Customer journey scenarios work end-to-end
- [ ] Documentation is clear and accurate
- [ ] All checklist items in SUBMISSION_CHECKLIST.md addressed
- [ ] Legal review complete (Apache 2.0 compliance)
- [ ] Technical review complete (accuracy of procedures)

**Note**: This is the final phase. Thoroughly test everything before declaring complete. Any issues found should be fixed immediately.

---

## Testing Strategy

### Unit Tests

**Not applicable** - This is documentation-only work. No code changes.

### Integration Tests

**Link Validation**:
```bash
# Test all links
markdown-link-check /Users/sac/cre/marketplace/listing-package/*.md
markdown-link-check /Users/sac/cre/docs/gcp/marketplace/*.md
markdown-link-check /Users/sac/cre/docs/gcp/runbooks/*.md
```

**File Existence Tests**:
```bash
# Test all referenced files exist
grep -oE 'src/[a-z_/_]+\.(erl|hrl)' /Users/sac/cre/marketplace/listing-package/*.md | sort -u | while read file; do
  [ -f "/Users/sac/cre/$file" ] || echo "Missing: $file"
done
```

### Manual Testing Steps

1. **Read-through of all new documents**:
   - Check for clarity, accuracy, completeness
   - Verify technical content is correct
   - Check for typos and formatting issues

2. **Customer journey simulation**:
   - Follow Scenario 1 (first-time customer) end-to-end
   - Follow Scenario 2 (upgrading customer) end-to-end
   - Follow Scenario 3 (troubleshooting customer) end-to-end

3. **Cross-reference validation**:
   - Click every link in every document
   - Verify every file reference exists
   - Verify every code snippet is syntactically correct

4. **Legal review**:
   - Verify Apache 2.0 attribution requirements met
   - Verify third-party licenses are correctly attributed
   - Verify privacy policy is accurate
   - Verify SLA terms are appropriate

5. **Technical review**:
   - Verify upgrade procedures are safe and accurate
   - Verify runbook procedures are tested and correct
   - Verify architecture diagrams match actual system
   - Verify cost model is accurate

## Migration Notes

**Not applicable** - This is documentation-only work. No data or system migrations.

## References

### Research
- `/Users/sac/cre/.wreckit/items/006-create-cre-documentation-for-google-cloud-marketpl/research.md`

### Source Documentation (to extract from)
- `/Users/sac/cre/README.md` - Main project README
- `/Users/sac/cre/docs/ARCHITECTURE.md` - System architecture
- `/Users/sac/cre/docs/DEPLOYMENT.md` - Deployment guide
- `/Users/sac/cre/docs/gcp/GCP_MARKETPLACE_READINESS.md` - Marketplace readiness
- `/Users/sac/cre/docs/gcp/marketplace/SUPPORT.md` - Support terms
- `/Users/sac/cre/docs/gcp/marketplace/PRICING.md` - Pricing model
- `/Users/sac/cre/docs/gcp/marketplace/PRIVACY.md` - Privacy policy
- `/Users/sac/cre/docs/gcp/marketplace/SLA.md` - Service level agreement
- `/Users/sac/cre/docs/gcp/marketplace/TROUBLESHOOTING.md` - Troubleshooting
- `/Users/sac/cre/docs/gcp/runbooks/deployment.md` - Deployment runbook
- `/Users/sac/cre/docs/gcp/runbooks/scaling.md` - Scaling runbook
- `/Users/sac/cre/docs/gcp/runbooks/backup.md` - Backup runbook
- `/Users/sac/cre/docs/gcp/runbooks/rollback.md` - Rollback runbook
- `/Users/sac/cre/docs/gcp/runbooks/troubleshooting.md` - Troubleshooting runbook
- `/Users/sac/cre/docs/gcp/SECURITY_WHITEPAPER.md` - Security architecture
- `/Users/sac/cre/docs/gcp/COMPLIANCE_MATRIX.md` - Compliance mapping
- `/Users/sac/cre/docs/diagrams/c4/C4_ARCHITECTURE.md` - C4 model

### Marketplace Artifacts (existing)
- `/Users/sac/cre/marketplace/deployer.yaml` - Marketplace deployment spec
- `/Users/sac/cre/marketplace/README.md` - Marketplace quick start
- `/Users/sac/cre/marketplace/SUBMISSION_CHECKLIST.md` - Submission checklist
- `/Users/sac/cre/marketplace/LICENSE.txt` - Marketplace license

### Dependency Information
- `/Users/sac/cre/rebar.config:35-45` - Dependency list
- `/Users/sac/cre/rebar.config:50-57` - Cowboy overrides

### Related Work Items
- Item 002: Package CRE for Google Cloud Marketplace (container images, infrastructure)
