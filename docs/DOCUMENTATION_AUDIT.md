# CRE Documentation Audit & Reorganization Plan

**Date:** 2026-02-08
**Purpose:** Documentation consolidation and organization

---

## Audit Findings

### Current State

- **Total markdown files:** 199+
- **Root level files:** ~120+ markdown files
- **Subdirectories:** 14 (tutorials, patterns, reference, diagrams, etc.)

### Issues Identified

#### 1. Multiple Index Files
- `README.md` - Project overview
- `INDEX.md` - Documentation index
- `DOCUMENTATION_INDEX.md` - Detailed workflow patterns index (REMOVED - consolidated into INDEX.md)

**Action:** Keep `INDEX.md` as the master index, remove redundant files.

#### 2. Files in Wrong Locations

The following files should be moved to subdirectories:

| File | Should Move To |
|------|----------------|
| `yawl_*_analysis.md` (multiple files) | `analysis/yawl/` |
| `generative_analysis_*.md` | `analysis/generative/` |
| `GA_CONSTITUTION_*.md` | `features/genetic-algorithms/` |
| `anti_patterns_guide.md` | `patterns/` or `guides/` |
| `architecture_hybrid_proposals.md` | `architecture/` |
| `compilation_pipeline_diagrams.md` | `diagrams/` |
| `decision_trees.md` | `diagrams/` or `guides/` |
| `plantuml_architecture_diagrams.md` | `diagrams/` |
| `state_machine_diagrams.md` | `diagrams/` |
| `traceability_matrix.md` | `guides/` |
| `comparison_matrices.md` | `guides/` |
| `evaluation_rubrics.md` | `training/` |
| `training_materials.md` | `training/` |
| `workshop_materials.md` | `training/` |
| `tutorial_exercises.md` | `training/` or `tutorials/` |
| `slide_deck_content.md` | `training/` |
| `case_study_*.md` | `case-studies/` |
| `AGI_SYMPOSIUM_*.md` | `case-studies/` |
| `BCD_CRE_Retrospective.md` | `case-studies/` |
| `FINAL_MERGE_SUMMARY.md` | `case-studies/` |
| `implementation_roadmap.md` | `planning/` |
| `PLAN_GAP_ANALYSIS.md` | `planning/` |
| `roadmap_executive_summary.md` | `planning/` |
| `innovation_*.md` | `planning/` |
| `tooling_innovation_roadmap.md` | `planning/` |
| `tool_configuration_guide.md` | `guides/` |
| `strategy_plugin_system_specification.md` | `features/strategies/` |
| `patterns_rl_strategies.md` | `features/strategies/` |
| `paper_analysis_*.md` | `papers/` |
| `executor_pattern_analysis.md` | `analysis/` |

#### 3. Naming Inconsistencies

- Mix of `YAWL_PATTERN_REFERENCE.md` and `YAWL_PATTERNS_REFERENCE.md`
- Inconsistent use of underscores vs hyphens
- Some files use ALL_CAPS, others use lowercase

**Recommendation:** Establish and document naming conventions.

#### 4. Duplicate/Overlapping Content

- Multiple pattern reference files with similar content
- Multiple quick reference cards
- Overlapping API documentation

**Action:** Consolidate or clearly differentiate purposes.

#### 5. Missing Documentation

- No comprehensive "Getting Started" for each module
- Missing migration guides for some versions
- No style guide for documentation contributors

---

## Proposed Directory Structure

```
docs/
├── README.md                          # Project overview (keep)
├── INDEX.md                           # Master documentation index (keep)
├── QUICK_START.md                     # Quick start guide (keep)
│
├── tutorials/                         # Tutorial series (existing, enhance)
│   ├── README.md
│   ├── getting_started.md
│   ├── basic_patterns_tutorial.md
│   ├── advanced_patterns_tutorial.md
│   ├── colored_tokens_tutorial.md
│   ├── handler_development.md
│   └── workflow_migration_tutorial.md
│
├── guides/                            # How-to guides (new)
│   ├── human_in_the_loop.md          # move from root
│   ├── deployment.md                  # move from root
│   ├── telemetry.md                   # move from root, rename
│   ├── timeout_configuration.md       # move from root, rename
│   ├── order_fulfillment_example.md   # move from root
│   ├── tool_configuration.md
│   └── migration/
│       ├── otp_25_28.md              # move from root
│       └── legacy_systems.md
│
├── reference/                         # Reference docs (existing)
│   ├── api_reference.md
│   ├── exception_handling.md
│   ├── glossary.md                    # move from root
│   └── faq.md                         # move from root
│
├── api/                               # API documentation (new)
│   ├── complete_api_reference.md      # move from root
│   ├── client_api.md                  # move from root
│   ├── patterns_api.md                # move from root
│   ├── mining_modules_api.md          # move from root
│   └── core/
│       ├── gen_pnet.md                # from yawl_patterns/
│       └── gen_yawl.md                # from yawl_patterns/
│
├── patterns/                          # Pattern docs (existing, enhance)
│   ├── PATTERN_IMPLEMENTATION_GUIDE.md
│   ├── ADVANCED_PATTERNS.md
│   ├── WDP_PATTERNS.md
│   ├── WRP_PATTERNS.md
│   ├── core_patterns_guide.md         # move from root
│   ├── patterns_reference.md          # move from root
│   ├── pattern_examples.md            # move from root
│   ├── patterns_workbook.md           # move from root
│   ├── pattern_catalog.md             # move from root
│   ├── anti_patterns_guide.md         # move from root
│   └── reference_card.md              # move from root
│
├── pnet/                              # Petri Net docs (new)
│   ├── types/
│   │   ├── guide.md                   # move from root
│   │   ├── tutorial.md                # move from root
│   │   ├── api_reference.md           # move from root
│   │   └── quick_reference.md         # move from root
│   ├── marking/
│   │   ├── tutorial.md                # move from root
│   │   ├── algebra.md                 # move from root
│   │   ├── implementation.md          # move from root
│   │   ├── api_reference.md           # move from root
│   │   ├── quick_reference.md         # move from root
│   │   └── tests.md                   # move from root
│   └── mode/
│       ├── guide.md                   # move from root
│       ├── tutorial.md                # move from root
│       └── quick_reference.md         # move from root
│
├── architecture/                      # Architecture docs (existing, enhance)
│   ├── system_overview.md             # keep
│   ├── design_principles.md           # move from root
│   ├── hybrid_proposals.md            # move from root
│   ├── hybrid_architecture/           # new
│   └── diagrams/
│       ├── compilation_pipeline.md    # move from root
│       ├── state_machines.md          # move from root
│       ├── plantuml.md                # move from root
│       └── decision_trees.md          # move from root
│
├── operations/                        # Operations docs (new)
│   ├── testing/
│   │   ├── testing.md                 # move from root
│   │   ├── test_organization.md       # move from root
│   │   └── verification_report.md     # move from root
│   ├── performance.md                 # move from root
│   ├── troubleshooting.md             # move from root
│   ├── debugging.md                   # move from root
│   ├── known_issues.md                # move from root
│   └── build_system.md                # move from root
│
├── features/                          # Feature-specific docs (new)
│   ├── genetic-algorithms/
│   │   ├── constitution_schema.md     # move from root
│   │   ├── examples.md                # move from root
│   │   └── validation.md              # move from root
│   ├── strategies/
│   │   ├── plugin_system.md           # move from root
│   │   └── rl_strategies.md           # move from root
│   └── human-in-the-loop/
│       └── (keep guide in guides/)
│
├── rust/                              # Rust modules (new)
│   ├── implementation_guide.md        # move from root
│   ├── quick_reference.md             # move from root
│   └── erlang_integration.md          # move from root
│
├── analysis/                          # Analysis docs (new)
│   ├── yawl/
│   │   ├── yengine_analysis.md        # move from root
│   │   ├── dataflow_analysis.md       # move from root
│   │   ├── interfaces_analysis.md     # move from root
│   │   ├── logging_analysis.md        # move from root
│   │   ├── persistence_analysis.md    # move from root
│   │   ├── resourcing_analysis.md     # move from root
│   │   ├── specification_analysis.md  # move from root
│   │   ├── marking_analysis.md        # move from root
│   │   ├── mi_analysis.md             # move from root
│   │   ├── exception_analysis.md      # move from root
│   │   ├── java_analysis.md           # move from root
│   │   ├── timer_analysis.md          # move from root
│   │   ├── architecture_comparison.md # move from root
│   │   ├── recommendations.md         # move from root
│   │   ├── net_analysis.md            # move from root
│   │   ├── netrunner_analysis.md      # move from root
│   │   ├── ytask_analysis.md          # move from root
│   │   ├── pattern_comparison.md      # move from root
│   │   ├── verification_checklist.md  # move from root
│   │   └── resetnet_analysis.md       # move from root
│   ├── generative/
│   │   ├── chapter_6_2_6_3.md         # move from root
│   │   ├── chapters_3_6_and_4_1.md    # move from root
│   │   └── diagrams.md                 # move from root
│   └── other/
│       ├── executor_pattern.md        # move from root
│       ├── pattern_implementation.md  # move from root
│       ├── pattern_enhancements.md    # move from root
│       └── token_protocol.md          # move from root
│
├── planning/                          # Planning docs (new)
│   ├── roadmap.md                     # move from root
│   ├── gap_analysis.md                # move from root
│   ├── executive_summary.md           # move from root
│   ├── innovation_opportunities.md    # move from root
│   └── tooling_roadmap.md             # move from root
│
├── training/                          # Training materials (new)
│   ├── materials.md                   # move from root
│   ├── workshops.md                   # move from root
│   ├── exercises.md                   # move from root
│   ├── rubrics.md                     # move from root
│   └── slides.md                      # move from root
│
├── case-studies/                      # Case studies (new)
│   ├── agi_symposium.md               # move from root
│   ├── agi_simulation_complete.md     # move from root
│   ├── agi_issues.md                  # move from root
│   ├── bcd_retrospective.md           # move from root
│   └── final_merge_summary.md         # move from root
│
├── development/                       # Development docs (new)
│   ├── contributing.md                # move from root
│   ├── release_notes/
│   │   ├── 0.3.0.md                   # move from root
│   │   └── 0.3.0_summary.md           # move from root
│   └── schema_validation.md           # move from root
│
├── diagrams/                          # Diagrams (existing, enhance)
│   ├── c4/
│   │   └── ...
│   └── mermaid/
│       └── ...
│
├── yawl_patterns/                    # YAWL pattern specifics (existing)
│   ├── README.md
│   ├── YAWL_ARCHITECTURE.md
│   ├── YAWL_INTEGRATION_GUIDE.md
│   └── *.dot                          # Pattern diagrams
│
├── examples/                          # Code examples (existing)
│   └── ...
│
├── example_workflows/                 # YAML workflows (existing)
│   └── ...
│
├── papers/                            # Research papers (existing)
│   ├── README.md
│   ├── PAPER_SUMMARIES.md
│   └── *.pdf                          # PDF files
│
├── book/                              # Book chapters (existing)
│   └── ...
│
├── generative_analysis_book/          # Generative analysis (existing)
│   └── ...
│
├── mermaid-diagrams/                  # Mermaid diagrams (existing)
│   └── ...
│
├── verification_scripts/              # Verification tools (existing)
│   └── ...
│
└── old/                               # Archived docs (existing)
    └── ...
```

---

## Naming Conventions

### File Names
- Use `snake_case` for all new files
- Use descriptive names that indicate content
- Avoid ALL_CAPS except for acronyms in titles (within the file, not filename)

### Directory Names
- Use `snake_case` for all directories
- Keep names short but descriptive
- Pluralize for collections (e.g., `tutorials/`, `guides/`, `patterns/`)

### Title Case
Within markdown files, use Title Case for headings. This is a project convention.

---

## Implementation Steps

1. **Phase 1: Create Directory Structure**
   - Create new directories
   - Do not move files yet

2. **Phase 2: Update Documentation**
   - Update INDEX.md with new structure
   - Update README.md with new paths
   - Create stub README files in new directories

3. **Phase 3: Move Files**
   - Move files systematically
   - Update internal links
   - Test all links

4. **Phase 4: Cleanup**
   - Remove empty directories
   - Archive truly obsolete files
   - Update gitignore if needed

---

## Priority Actions

### High Priority
1. Create `guides/` directory for how-to guides
2. Create `operations/` directory for testing, deployment, troubleshooting
3. Create `api/` directory for API documentation
4. Create `pnet/` directory for Petri Net documentation
5. Create `case-studies/` for case study documents

### Medium Priority
1. Create `analysis/` directory for analysis documents
2. Create `training/` directory for training materials
3. Create `planning/` directory for roadmap documents
4. Create `features/` directory for feature-specific docs
5. Create `rust/` directory for Rust module documentation

### Low Priority
1. Consolidate duplicate content
2. Establish style guide
3. Create missing documentation
4. Archive old content

---

## Notes

- This plan preserves existing directory structures
- Changes focus on organization, not content modification
- All moves should be done with git to preserve history
- Update INDEX.md after each move to avoid broken links

**Status:** Plan created, pending implementation
**Next Step:** Begin Phase 1 - Create new directory structure
