# CRE - Common Runtime Environment

Erlang/OTP YAWL workflow engine with Petri Net patterns. **OTP 28 REQUIRED**.

## 🚨 CRITICAL: DOCKER-ONLY WORKFLOW

**ABSOLUTE RULE: ALL WORK MUST BE DONE INSIDE DOCKER CONTAINERS.**

- ✅ **ALLOWED**: Docker, docker buildx, docker compose, container execution
- ❌ **FORBIDDEN**: Running scripts directly on host computer
- ❌ **FORBIDDEN**: Direct rebar3 execution on host (use Docker instead)
- ❌ **FORBIDDEN**: Any host system modifications outside Docker

**Docker-First Development:**
```bash
# Build multi-arch images (AMD64 and ARM64)
docker buildx bake --load

# Run development container
docker run -it --rm -v $(pwd):/work -w /work cre:0.3.0 sh

# Run tests in container
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit

# Validate deployment readiness
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 sh -c "rebar3 compile && rebar3 ct"
```

## gVisor Cloud Environments

**Claude Code on Web runs in gVisor sandbox**, which meets our containerization requirements. The SessionStart.sh hook automatically bootstraps OTP 28 in these environments using pre-built static binaries. gVisor provides syscall-level isolation similar to containers, making it suitable for CRE development. When Docker isn't available (cloud IDEs, web-based Claude), SessionStart.sh falls back to native tooling. BEAM VM startup time in gVisor: ~420-440ms (measured with scripts/measure_beam_startup.sh).

## GCP Marketplace Deployment

**Target:** GCP Marketplace submission with first-time approval

**Deployment Artifacts:**
- Multi-arch Docker images (linux/amd64, linux/arm64)
- GKE Regional Cluster manifests (15+ YAML files)
- Terraform modules for infrastructure
- Helm charts for Kubernetes deployment
- SBOM generation (SPDX, CycloneDX)
- Security scanning (Trivy)

## OTP 28 Upgrade

**Status:** IN PROGRESS - Current Dockerfile uses OTP 27, needs OTP 28 migration

**Breaking Changes to Address:**
- User permission issues in erlang:28-alpine during build
- Doc attribute compatibility (use @doc comments, not -doc attributes)
- NIF loading changes for Rust components
- Logger API changes (check for deprecated functions)

## Architecture

Single OTP runner (`gen_yawl`) wrapping `gen_pnet`. Everything else is pure stateless helpers.

- `src/core/` - gen_pnet runtime (DO NOT modify without understanding full callback chain)
- `src/pnet/` - Petri Net algebra (types, markings, modes, choice)
- `src/wf/` - Workflow utilities (timers, tasks, scopes)
- `src/yawl/` - YAWL compilation, validation, execution
- `src/patterns/` - 43 YAWL workflow control-flow patterns
- `test/` - EUnit and Common Test suites

## Critical Rules

- **DOCKER-ONLY WORKFLOW** - ALL operations must run inside Docker containers
- Source in `src/`, tests in `test/`, docs in `docs/`. NEVER put files in project root.
- MUST run `rebar3 compile` after modifying `.erl` or `.hrl` files to verify correctness.
- See @.claude/rules/erlang.md for Erlang code conventions.
- See @.claude/rules/testing.md for test conventions.
- See @docs/gcp/GCP_MARKETPLACE_READINESS.md for GCP Marketplace requirements.
