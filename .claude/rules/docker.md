---
paths:
  - "*"
---

# Docker-Only Workflow

**ABSOLUTE RULE: ALL WORK MUST BE DONE INSIDE DOCKER CONTAINERS.**

## Allowed Operations
- Docker CLI commands (docker, docker buildx, docker compose)
- Container execution
- gVisor sandbox environments (Claude Code Web)

## Forbidden Operations
- Running scripts directly on host computer
- Direct rebar3 execution on host (use Docker instead)
- Any host system modifications outside Docker

## Docker Commands

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

Claude Code on Web runs in gVisor sandbox, which meets containerization requirements. SessionStart.sh bootstraps OTP 28 using pre-built static binaries. gVisor provides syscall-level isolation similar to containers.

When Docker isn't available (cloud IDEs, web-based Claude), SessionStart.sh falls back to native tooling. BEAM VM startup in gVisor: ~420-440ms.
