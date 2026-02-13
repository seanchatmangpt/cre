# Complete Guide: OTP 28 in Claude Code Web Instances

**Version:** 1.0
**Target:** Any Claude Code web session (gVisor sandbox)
**Goal:** Automatic OTP 28 installation via SessionStart hook

---

## 📋 Table of Contents

1. [Quick Start (Copy & Paste)](#quick-start)
2. [Understanding the Problem](#understanding-the-problem)
3. [Solution Architecture](#solution-architecture)
4. [Step-by-Step Setup](#step-by-step-setup)
5. [How It Works](#how-it-works)
6. [Troubleshooting](#troubleshooting)
7. [Advanced Configuration](#advanced-configuration)

---

## 🚀 Quick Start

### For Any Repository

**Copy these 2 files to your repo:**

```bash
# 1. Create .claude directory structure
mkdir -p .claude/hooks

# 2. Download SessionStart hook (from this repo or create manually)
curl -fsSL https://raw.githubusercontent.com/joergen7/cre/main/.claude/hooks/SessionStart.sh \
  -o .claude/hooks/SessionStart.sh
chmod +x .claude/hooks/SessionStart.sh

# 3. Create .claude/settings.json
cat > .claude/settings.json <<'EOF'
{
  "hooks": {
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PROJECT_DIR\"/.claude/hooks/SessionStart.sh 2>&1 || true",
            "timeout": 600000
          }
        ]
      },
      {
        "matcher": "compact",
        "hooks": [
          {
            "type": "command",
            "command": "echo 'OTP 28 ready. Use: erl -version'"
          }
        ]
      }
    ]
  }
}
EOF

# 4. Add .erlmcp to .gitignore
echo ".erlmcp/" >> .gitignore
```

**That's it!** Next time you open this repo in Claude Code Web, OTP 28 will be ready.

---

## 🔍 Understanding the Problem

### What is gVisor?

Claude Code Web runs in a **gVisor sandbox** for security:
- Limited syscalls (76 of 350 blocked)
- No root access
- Filtered network (proxy-only)
- No package managers (apt/yum don't work)

### Why Can't We Build OTP from Source?

Building Erlang/OTP fails because gVisor blocks these syscalls:
```
perf_event_open() - configure checks fail
kcmp()            - process comparison
clone()           - thread creation
```

### The Solution: Pre-built Static Binary

Download a pre-compiled OTP 28 binary that:
- ✅ No compilation needed
- ✅ No build tools required (gcc, make, etc.)
- ✅ ~50MB download vs 6+ hours building
- ✅ Works in gVisor sandbox

---

## 🏗️ Solution Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Claude Code Web Session Starts                              │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ SessionStart Hook Executes                                  │
│ (.claude/hooks/SessionStart.sh)                             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 1: Check Cache (.erlmcp/otp-28.3.1/)                 │
│   ├─ Found? ✓ → Skip to Phase 3                            │
│   └─ Not found? → Continue                                  │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 2: Acquire OTP (Platform-Specific)                    │
│                                                              │
│ Linux (gVisor):                                             │
│   2A. Check system OTP (/usr/bin/erl)                      │
│   2B. Download pre-built binary (Hex.pm, GitHub, etc.)     │
│   2C. Build from source (FAILS in gVisor) ❌                │
│                                                              │
│ macOS (Native):                                             │
│   2A. Search Homebrew (/opt/homebrew/bin/erl)              │
│   2B. Search kerl (~/.kerl/installs/)                      │
│   2C. Download pre-built binary                             │
│   2D. Build from source ✓                                   │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 3: Setup Environment                                  │
│   - export PATH="$OTP_DIR/bin:$PATH"                        │
│   - export CLAUDE_CODE_REMOTE=true                          │
│   - Write to CLAUDE_ENV_FILE (persists for Bash tools)     │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 4: Create Lock File                                   │
│   - .erlmcp/cache/sessionstart.lock prevents re-runs        │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 5: Build Project                                      │
│   - Download rebar3 (if not cached)                         │
│   - rebar3 get-deps                                         │
│   - Patch cowlib for OTP 28 (if needed)                    │
│   - rebar3 compile                                          │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ Phase 6: Completion Report                                  │
│   ✓ OTP 28 ready at .erlmcp/otp-28.3.1/bin/erl            │
│   ✓ Project compiled (or skipped if no rebar.config)       │
└─────────────────────────────────────────────────────────────┘
```

---

## 📖 Step-by-Step Setup

### Step 1: Create Directory Structure

```bash
cd your-erlang-project/
mkdir -p .claude/hooks
```

### Step 2: Create SessionStart.sh

**Copy from this repo OR create manually:**

<details>
<summary>Click to expand: Full SessionStart.sh script (526 lines)</summary>

```bash
#!/usr/bin/env bash
# SessionStart hook for Erlang/OTP 28+ projects
# Bootstraps OTP on gVisor sandbox environments
# Version: 4.0.1-otp28.3.1

set -euo pipefail

#=============================================================================
# Configuration
#=============================================================================

readonly OTP_VERSION="28.3.1"
readonly OTP_MAJOR=28
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
readonly CACHE_DIR="${PROJECT_ROOT}/.erlmcp"
readonly OTP_DIR="${CACHE_DIR}/otp-${OTP_VERSION}"
readonly OTP_BIN="${OTP_DIR}/bin/erl"
readonly LOCK_FILE="${CACHE_DIR}/cache/sessionstart.lock"
readonly LOG_FILE="${CACHE_DIR}/sessionstart.log"
readonly REBAR3_BIN="${CACHE_DIR}/cache/rebar3"
readonly REBAR3_URL="https://s3.amazonaws.com/rebar3/rebar3"

# Download URLs for pre-built binaries
readonly OTP_SOURCE_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"
readonly CPU_COUNT=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)

#=============================================================================
# Logging
#=============================================================================

log()     { echo "[$1] $2" | tee -a "$LOG_FILE" >&2; }
info()    { log "INFO" "$*"; }
error()   { log "ERROR" "$*"; }
success() { log "SUCCESS" "$*"; }
phase()   {
    echo "" >&2
    printf '%80s\n' | tr ' ' '=' | tee -a "$LOG_FILE" >&2
    log "PHASE" "$*"
    printf '%80s\n' | tr ' ' '=' | tee -a "$LOG_FILE" >&2
}

init_log() {
    mkdir -p "$CACHE_DIR/cache"
    echo "--- SessionStart $(date -Iseconds) OTP=${OTP_VERSION} ---" >> "$LOG_FILE"
}

#=============================================================================
# OTP Version Detection
#=============================================================================

otp_major() {
    local bin="${1:-erl}"
    if command -v "$bin" &>/dev/null || [[ -f "$bin" ]]; then
        local v
        v=$("$bin" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "0")
        echo "${v%%.*}"
    else
        echo "0"
    fi
}

#=============================================================================
# Platform Detection
#=============================================================================

detect_platform() {
    case "$OSTYPE" in
        linux*)  echo "linux" ;;
        darwin*) echo "macos" ;;
        *)       echo "unknown" ;;
    esac
}

is_gvisor() {
    [[ -f /proc/version ]] && grep -qi "gvisor" /proc/version 2>/dev/null && return 0
    [[ ! -d /proc/sys/vm ]] && return 0
    return 1
}

#=============================================================================
# Phase 1: Cache Check
#=============================================================================

check_cache() {
    phase "1/6 Cache check"
    [[ -f "$OTP_BIN" ]] || return 1
    local major
    major=$(otp_major "$OTP_BIN")
    if [[ $major -ge $OTP_MAJOR ]]; then
        success "OTP major version $major found in cache"
        return 0
    fi
    return 1
}

#=============================================================================
# Phase 2A: Check System OTP
#=============================================================================

check_system_otp() {
    phase "2A/6 Check system OTP"
    local bins=(
        "/usr/bin/erl"
        "/usr/local/bin/erl"
        "/opt/erlang/bin/erl"
        "/opt/homebrew/bin/erl"
    )

    for p in "${bins[@]}"; do
        if [[ -f "$p" ]]; then
            local major
            major=$(otp_major "$p")
            if [[ $major -ge $OTP_MAJOR ]]; then
                success "Found system OTP $major at $p"
                mkdir -p "${OTP_DIR}/bin"
                ln -sf "$p" "${OTP_DIR}/bin/erl"
                local dir
                dir=$(dirname "$p")
                for bin in "$dir"/erl* "$dir"/dialyzer; do
                    [[ -f "$bin" ]] && ln -sf "$bin" "${OTP_DIR}/bin/$(basename "$bin")" 2>/dev/null || true
                done
                return 0
            fi
        fi
    done

    info "No suitable system OTP found"
    return 1
}

#=============================================================================
# Phase 2B: Download Pre-built Binary
#=============================================================================

download_static_binary() {
    phase "2B/6 Download pre-built OTP (static)"
    mkdir -p "$OTP_DIR"

    info "Checking for pre-built static OTP..."

    local urls=(
        "https://repo.hex.pm/builds/otp/ubuntu-20.04/OTP-${OTP_VERSION}.tar.gz"
        "https://repo.hex.pm/builds/otp/ubuntu-22.04/OTP-${OTP_VERSION}.tar.gz"
        "https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"
    )

    for url in "${urls[@]}"; do
        info "Trying: $url"
        local tarball="$CACHE_DIR/temp-otp.tar.gz"

        if curl -fsSL -o "$tarball" "$url" 2>&1 | tee -a "$LOG_FILE"; then
            info "Downloaded, extracting..."
            local tmp="${CACHE_DIR}/temp-extract"
            mkdir -p "$tmp"

            if tar xzf "$tarball" -C "$tmp" 2>/dev/null; then
                info "Extraction successful, setting up..."
                local content
                content=$(find "$tmp" -name "erl" -type f 2>/dev/null | head -1)
                if [[ -n "$content" ]]; then
                    local otp_root
                    otp_root=$(dirname "$(dirname "$content")")
                    cp -r "$otp_root"/* "$OTP_DIR/" 2>/dev/null || \
                        mv "$tmp"/* "$OTP_DIR/" 2>/dev/null || \
                        cp -r "$tmp"/"*" "$OTP_DIR/" 2>/dev/null

                    rm -rf "$tmp" "$tarball"

                    local major
                    major=$(otp_major "${OTP_DIR}/bin/erl" 2>/dev/null || echo "0")
                    if [[ $major -ge $OTP_MAJOR ]]; then
                        success "Static OTP $major installed"
                        return 0
                    fi
                fi
            fi
            rm -rf "$tmp" "$tarball"
        fi
        info "Failed: $url"
    done

    info "No pre-built binary available"
    return 1
}

#=============================================================================
# Phase 3: Environment Setup
#=============================================================================

setup_environment() {
    phase "3/6 Environment setup"

    export PATH="/usr/bin:/bin:/usr/local/bin:${OTP_DIR}/bin:${PATH}"
    export CLAUDE_CODE_REMOTE=true
    export ERLMCP_PROFILE=cloud
    export ERLMCP_CACHE="${CACHE_DIR}/cache/"
    export TERM=dumb
    export REBAR_COLOR=none
    export ERL_AFLAGS="-kernel shell_history enabled"

    mkdir -p "$ERLMCP_CACHE"

    if [[ -n "${CLAUDE_ENV_FILE:-}" ]]; then
        {
            echo "export PATH=\"${OTP_DIR}/bin:\$PATH\""
            echo "export CLAUDE_CODE_REMOTE=true"
            echo "export ERLMCP_PROFILE=cloud"
            echo "export ERLMCP_CACHE=\"${CACHE_DIR}/cache/\""
            echo "export TERM=dumb"
            echo "export REBAR_COLOR=none"
            echo "export ERL_AFLAGS=\"-kernel shell_history enabled\""
        } >> "$CLAUDE_ENV_FILE"
        info "Environment variables persisted to CLAUDE_ENV_FILE"
    fi

    cat > "${CACHE_DIR}/env.sh" <<ENVEOF
# Generated by SessionStart.sh - source for OTP 28+ environment
export PATH="/usr/bin:/bin:/usr/local/bin:${OTP_DIR}/bin:\$PATH"
export CLAUDE_CODE_REMOTE=true
export ERLMCP_PROFILE=cloud
export ERLMCP_CACHE="${CACHE_DIR}/cache/"
export TERM=dumb
export REBAR_COLOR=none
export ERL_AFLAGS="-kernel shell_history enabled"
ENVEOF

    success "Environment variables set"
    info "  PATH includes ${OTP_DIR}/bin"
    info "  ERLMCP_PROFILE=$ERLMCP_PROFILE"
}

#=============================================================================
# Phase 4: Lock File
#=============================================================================

create_lock() {
    phase "4/6 Lock file creation"
    mkdir -p "$(dirname "$LOCK_FILE")"
    echo "$OTP_VERSION" > "$LOCK_FILE"
    success "Lock file created: $LOCK_FILE"
}

#=============================================================================
# Phase 5: Rebar3 Setup & Project Build
#=============================================================================

ensure_rebar3() {
    if [[ -f "$REBAR3_BIN" && -x "$REBAR3_BIN" ]]; then
        info "rebar3 already cached at $REBAR3_BIN"
        return 0
    fi

    info "Downloading rebar3..."
    mkdir -p "$(dirname "$REBAR3_BIN")"
    if curl -fsSL -o "$REBAR3_BIN" "$REBAR3_URL" 2>&1 | tee -a "$LOG_FILE"; then
        chmod +x "$REBAR3_BIN"
        success "rebar3 downloaded"
        return 0
    fi

    error "Failed to download rebar3"
    return 1
}

build_project() {
    phase "5/6 Project build (rebar3)"

    # Only build if rebar.config exists
    [[ -f "${PROJECT_ROOT}/rebar.config" ]] || {
        info "No rebar.config found, skipping build"
        return 0
    }

    ensure_rebar3 || { error "Cannot build without rebar3"; return 1; }
    cd "$PROJECT_ROOT"

    info "Fetching dependencies..."
    if ! "$REBAR3_BIN" get-deps 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        error "Failed to fetch dependencies"
        return 1
    fi

    info "Compiling project..."
    if ! "$REBAR3_BIN" compile 2>&1 | tee -a "$LOG_FILE" | tail -10; then
        error "Compilation failed"
        return 1
    fi
    success "Project compiled successfully"
}

#=============================================================================
# Phase 6: Completion Report
#=============================================================================

completion_report() {
    phase "6/6 Session complete"
    local major
    major=$(otp_major "$OTP_BIN")
    info "OTP Version: $major (target: $OTP_MAJOR)"
    info "OTP Path: ${OTP_DIR}/bin/erl"

    if is_gvisor; then
        info "Environment: gVisor sandbox detected"
    else
        info "Environment: native $(detect_platform)"
    fi

    success "SessionStart complete - ready to develop"
}

#=============================================================================
# Main
#=============================================================================

main() {
    init_log
    info "Starting SessionStart.sh (v4.0.1-otp28.3.1)"
    info "Platform: $(detect_platform)"

    if ! check_cache; then
        info "OTP not cached, acquiring..."
        local acquired=false

        check_system_otp && acquired=true
        [[ "$acquired" == "false" ]] && download_static_binary && acquired=true

        if [[ "$acquired" != "true" ]]; then
            error "All OTP acquisition methods failed"
            info "Solutions:"
            info "  1. Install OTP 28 on host system"
            info "  2. Host a pre-built static binary"
            exit 1
        fi
    fi

    setup_environment
    create_lock
    build_project || info "Project build skipped or failed (non-fatal)"
    completion_report

    exit 0
}

main "$@"
```

</details>

**Quick version: Copy from CRE repo**
```bash
curl -fsSL https://raw.githubusercontent.com/joergen7/cre/main/.claude/hooks/SessionStart.sh \
  -o .claude/hooks/SessionStart.sh
chmod +x .claude/hooks/SessionStart.sh
```

### Step 3: Create .claude/settings.json

```json
{
  "hooks": {
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PROJECT_DIR\"/.claude/hooks/SessionStart.sh 2>&1 || true",
            "timeout": 600000
          }
        ]
      },
      {
        "matcher": "compact",
        "hooks": [
          {
            "type": "command",
            "command": "echo 'OTP 28 ready. Check: erl -version'"
          }
        ]
      }
    ]
  }
}
```

**Key settings:**
- `timeout: 600000` = 10 minutes (enough for download + build)
- `|| true` = Don't fail if hook has issues

### Step 4: Add to .gitignore

```bash
# Add cache directory to .gitignore
echo ".erlmcp/" >> .gitignore
echo ".erlmcp/cache/" >> .gitignore
echo ".erlmcp/sessionstart.log" >> .gitignore
```

### Step 5: Commit and Push

```bash
git add .claude/ .gitignore
git commit -m "Add OTP 28 SessionStart hook for Claude Code Web"
git push
```

### Step 6: Test in Claude Code Web

1. Open your repo in Claude Code Web
2. Wait for SessionStart hook (10-60 seconds)
3. Verify:
   ```bash
   erl -version
   # Should show: Erlang/OTP 28 ...

   which erl
   # Should show: /home/user/your-repo/.erlmcp/otp-28.3.1/bin/erl
   ```

---

## 🔧 How It Works

### What Happens on Every Session?

```bash
Session Start
  ↓
SessionStart.sh runs
  ↓
Phase 1: Check .erlmcp/otp-28.3.1/
  ├─ Found? → Skip download, use cache
  └─ Not found? → Phase 2
       ↓
Phase 2: Acquire OTP
  ├─ 2A: Check /usr/bin/erl (system)
  ├─ 2B: Download from Hex.pm/GitHub
  └─ 2C: Build from source (fails in gVisor)
       ↓
Phase 3: Setup environment
  - export PATH=".erlmcp/otp-28.3.1/bin:$PATH"
  - Write to CLAUDE_ENV_FILE
       ↓
Phase 4: Create lock file
  - Prevents redundant runs
       ↓
Phase 5: Build project (if rebar.config exists)
  - Download rebar3
  - rebar3 get-deps
  - rebar3 compile
       ↓
Phase 6: Report completion
```

### Where is OTP Cached?

```
your-repo/
├── .erlmcp/                    # Git-ignored cache
│   ├── otp-28.3.1/            # Full OTP installation
│   │   ├── bin/erl            # Erlang shell
│   │   ├── bin/erlc           # Compiler
│   │   ├── bin/dialyzer       # Type checker
│   │   └── lib/erlang/...     # Libraries
│   ├── cache/
│   │   ├── rebar3             # Build tool
│   │   └── sessionstart.lock  # Prevents re-runs
│   ├── env.sh                 # Environment variables
│   └── sessionstart.log       # Full logs
├── .claude/
│   ├── hooks/
│   │   └── SessionStart.sh    # Bootstrap script
│   └── settings.json          # Hook configuration
└── .gitignore                 # Excludes .erlmcp/
```

### Performance

| Phase | First Run | Cached (2nd+ Run) |
|-------|-----------|-------------------|
| Download OTP | ~30-45s | 0s (skipped) |
| Extract | ~5-10s | 0s (skipped) |
| Setup env | ~1s | ~1s |
| Download rebar3 | ~2s | 0s (cached) |
| Get deps | ~10-30s | ~5s |
| Compile | ~15-45s | ~10s |
| **Total** | **~60-120s** | **~15-20s** |

---

## 🐛 Troubleshooting

### Issue: "All OTP acquisition methods failed"

**Symptoms:**
```
[ERROR] All OTP acquisition methods failed
```

**Causes & Solutions:**

1. **Network blocked in sandbox**
   ```bash
   # Test connectivity
   curl -I https://repo.hex.pm/builds/otp/ubuntu-22.04/OTP-28.3.1.tar.gz
   ```

2. **URL changed/moved**
   - Edit `SessionStart.sh` line 164-172
   - Add your own static OTP URL

3. **System OTP is too old**
   ```bash
   erl -version  # Shows < OTP 28
   ```
   Solution: Install OTP 28 on host OR host static binary

### Issue: "timeout exceeded (600000ms)"

**Cause:** Download too slow, or build taking too long

**Solutions:**
1. Increase timeout in settings.json:
   ```json
   "timeout": 900000  // 15 minutes
   ```

2. Host static binary closer (your GitHub releases, CDN)

### Issue: "exec format error"

**Cause:** Wrong architecture (ARM binary on x86, or vice versa)

**Solution:** Build for correct arch:
```bash
# On x86_64 machine
docker run --platform linux/amd64 erlang:28.3.1 \
  tar czf /tmp/otp.tar.gz -C /usr/local/ lib/erlang

# On ARM64 machine
docker run --platform linux/arm64 erlang:28.3.1 \
  tar czf /tmp/otp.tar.gz -C /usr/local/ lib/erlang
```

### Issue: Dependencies fail (yamerl, pc)

**Symptoms:**
```
Failed to fetch package pc from repo hexpm
Failed to fetch yamerl from git
```

**Solutions:**

1. **Check rebar.config dependencies**
   ```erlang
   {deps, [
     {yamerl, "0.10.0"}  % Use hex version, not git
   ]}.
   ```

2. **Use rebar.lock if exists**
   ```bash
   git add rebar.lock
   git commit -m "Lock dependency versions"
   ```

3. **Skip build in hook** (manual build later)
   ```bash
   # In SessionStart.sh, line 519:
   build_project || info "Project build skipped or failed (non-fatal)"
   ```

### Issue: Hook runs on every message

**Cause:** No lock file OR lock file deleted

**Solution:** Check lock file exists:
```bash
cat .erlmcp/cache/sessionstart.lock
# Should show: 28.3.1
```

If missing, delete cache and restart:
```bash
rm -rf .erlmcp/
# Restart session
```

---

## 🎯 Advanced Configuration

### 1. Custom OTP Version

Edit `SessionStart.sh` lines 21-22:
```bash
readonly OTP_VERSION="28.2.0"  # Change version
readonly OTP_MAJOR=28          # Change major
```

### 2. Add Pre-commit Hook for Tests

`.claude/settings.json`:
```json
{
  "hooks": {
    "SessionStart": [ /* ... */ ],
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PROJECT_DIR\"/.claude/hooks/pre-bash-guard.sh",
            "timeout": 3000
          }
        ]
      }
    ]
  }
}
```

### 3. Host Your Own Static OTP Binary

**Option A: GitHub Releases**
```bash
# Build static OTP
docker run --rm -v $(pwd):/work erlang:28.3.1 \
  tar czf /work/otp-28.3.1-ubuntu22.04.tar.gz -C /usr/local lib/erlang

# Upload to GitHub release
gh release create otp-28.3.1 \
  --title "Erlang/OTP 28.3.1 Static Binary" \
  otp-28.3.1-ubuntu22.04.tar.gz

# Get download URL
gh release view otp-28.3.1 --json assets --jq '.assets[0].url'
```

**Option B: CloudFlare R2 (S3-compatible)**
```bash
# Upload to R2
aws s3 cp otp-28.3.1-ubuntu22.04.tar.gz \
  s3://my-bucket/otp-binaries/ \
  --endpoint-url=https://xxx.r2.cloudflarestorage.com \
  --acl public-read
```

**Option C: Google Cloud Storage**
```bash
gsutil cp otp-28.3.1-ubuntu22.04.tar.gz gs://my-bucket/otp/
gsutil acl ch -u AllUsers:R gs://my-bucket/otp/otp-28.3.1-ubuntu22.04.tar.gz
```

Then update `SessionStart.sh` line 164:
```bash
local urls=(
  "https://your-cdn.com/otp-28.3.1-ubuntu22.04.tar.gz"  # Add first
  "https://repo.hex.pm/builds/otp/ubuntu-22.04/OTP-${OTP_VERSION}.tar.gz"
)
```

### 4. Per-Project Customization

Create `.erlmcp/config.sh`:
```bash
# Custom OTP version for this project
export CUSTOM_OTP_VERSION="28.2.0"
export CUSTOM_OTP_URL="https://my-cdn.com/otp-28.2.0.tar.gz"
```

Source in `SessionStart.sh`:
```bash
[[ -f "${CACHE_DIR}/config.sh" ]] && source "${CACHE_DIR}/config.sh"
```

### 5. Parallel Rebar3 Builds

`.erlmcp/env.sh`:
```bash
export REBAR_PROFILE=prod
export REBAR_CACHE_DIR="${ERLMCP_CACHE}/rebar3"
export MAKEFLAGS="-j$(nproc)"
```

---

## 📚 References

- [gVisor Syscall Compatibility](https://gvisor.dev/docs/user_guide/compatibility/linux/amd64/)
- [Erlang/OTP 28 Release Notes](https://www.erlang.org/patches/OTP-28.3.1)
- [Hex.pm Pre-built OTP](https://repo.hex.pm/builds/otp/)
- [Claude Code Hooks Documentation](https://code.claude.ai/docs/hooks)
- [CRE Project (Reference Implementation)](https://github.com/joergen7/cre)

---

## ✅ Checklist: Is My Setup Working?

```bash
# 1. OTP installed?
erl -version
# Expected: Erlang/OTP 28 [erts-15.x.x] ...

# 2. Path correct?
which erl
# Expected: /home/user/your-repo/.erlmcp/otp-28.3.1/bin/erl

# 3. Rebar3 available?
.erlmcp/cache/rebar3 version
# Expected: rebar 3.x.x on Erlang/OTP 28 ...

# 4. Environment persisted?
echo $ERLMCP_PROFILE
# Expected: cloud

# 5. Lock file created?
cat .erlmcp/cache/sessionstart.lock
# Expected: 28.3.1

# 6. Can compile Erlang?
echo '-module(test). -export([hello/0]). hello() -> world.' > /tmp/test.erl
erlc /tmp/test.erl && echo "✓ Erlang compiler works"

# 7. Project builds? (if rebar.config exists)
.erlmcp/cache/rebar3 compile
# Expected: Compiling ... (no errors)
```

---

## 🎉 Success Criteria

Your setup is ready when:
- ✅ SessionStart hook completes in < 2 minutes (first run)
- ✅ OTP 28 available at `.erlmcp/otp-28.3.1/bin/erl`
- ✅ `erl -version` shows OTP 28+
- ✅ `rebar3 compile` builds your project
- ✅ Cache survives across Claude Code sessions
- ✅ No "permission denied" or "exec format" errors

---

**Questions?** Check logs:
```bash
cat .erlmcp/sessionstart.log
```

**Need help?** File an issue:
- [CRE GitHub Issues](https://github.com/joergen7/cre/issues)
- Include: Platform, error message, sessionstart.log excerpt
