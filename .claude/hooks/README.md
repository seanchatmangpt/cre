# Claude Code Hooks

This directory contains hooks that run automatically during Claude Code sessions.

## Hooks Overview

### SessionStart.sh

**Purpose:** Bootstrap Erlang/OTP 28 in Claude Code web sessions (gVisor sandbox)

**When it runs:** Automatically when a Claude Code web session starts

**What it does:**
1. Checks if OTP 28 is cached (`.erlmcp/otp-28.3.1/`)
2. If not cached, downloads pre-built binary from:
   - Hex.pm (https://repo.hex.pm/builds/otp/)
   - GitHub releases
   - System installation (/usr/bin/erl)
3. Extracts OTP to `.erlmcp/otp-28.3.1/`
4. Sets up environment variables (PATH, ERLMCP_PROFILE, etc.)
5. Creates lock file to prevent re-runs
6. Downloads rebar3 if not cached
7. Builds project (if rebar.config exists)

**Performance:**
- First run: 60-120 seconds
- Cached runs: 15-20 seconds

**Configuration:** `.claude/settings.json`

### pre-bash-guard.sh

**Purpose:** Pre-execution safety checks for Bash commands

**When it runs:** Before every Bash tool execution

**What it does:**
- Validates commands before execution
- Prevents destructive operations in protected directories
- Enforces project rules (Docker-only workflow, etc.)

**Configuration:** `.claude/settings.json` → `PreToolUse` hook

### stop-check.sh

**Purpose:** Session cleanup and validation

**When it runs:** When Claude Code session stops

**What it does:**
- Checks for uncommitted changes
- Validates project state
- Cleans up temporary files

**Configuration:** `.claude/settings.json` → `Stop` hook

---

## Hook Configuration

Hooks are configured in `.claude/settings.json`:

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
      }
    ],
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
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PROJECT_DIR\"/.claude/hooks/stop-check.sh",
            "timeout": 10000
          }
        ]
      }
    ]
  }
}
```

---

## Debugging Hooks

### View SessionStart logs
```bash
cat .erlmcp/sessionstart.log
```

### Test SessionStart manually
```bash
bash .claude/hooks/SessionStart.sh
```

### Check environment setup
```bash
source .erlmcp/env.sh
erl -version
```

### Verify OTP installation
```bash
.erlmcp/otp-28.3.1/bin/erl -version
```

---

## Modifying Hooks

### Change OTP version

Edit `SessionStart.sh` lines 21-22:
```bash
readonly OTP_VERSION="28.2.0"  # Change here
readonly OTP_MAJOR=28
```

### Add custom download URL

Edit `SessionStart.sh` line 164:
```bash
local urls=(
  "https://your-cdn.com/otp-28.3.1.tar.gz"  # Add first
  "https://repo.hex.pm/builds/otp/ubuntu-22.04/OTP-${OTP_VERSION}.tar.gz"
)
```

### Increase timeout

Edit `.claude/settings.json`:
```json
"timeout": 900000  // 15 minutes
```

---

## Documentation

- [Quick Start Guide](../../docs/OTP28_QUICKSTART.md) - Copy-paste setup
- [Complete Guide](../../docs/OTP28_CLAUDE_CODE_WEB_GUIDE.md) - Architecture & troubleshooting
- [gVisor Setup](../../docs/GVISOR_OTP_SETUP.md) - Technical background

---

## Cache Structure

```
.erlmcp/                         # Git-ignored cache directory
├── otp-28.3.1/                 # Full OTP installation
│   ├── bin/
│   │   ├── erl                 # Erlang shell
│   │   ├── erlc                # Compiler
│   │   └── dialyzer            # Type checker
│   └── lib/erlang/...          # OTP libraries
├── cache/
│   ├── rebar3                  # Build tool (cached)
│   └── sessionstart.lock       # Prevents duplicate runs
├── env.sh                      # Environment variables
└── sessionstart.log            # Full execution log
```

---

## Common Issues

### "All OTP acquisition methods failed"
- Check network: `curl -I https://repo.hex.pm/`
- Check logs: `cat .erlmcp/sessionstart.log`
- Manually test download URLs in SessionStart.sh

### Hook timeout
- Increase timeout in `.claude/settings.json`
- Check if download is slow (try different URL)

### Wrong OTP version
- Delete cache: `rm -rf .erlmcp/`
- Edit `OTP_VERSION` in SessionStart.sh
- Restart session

### Cache not persisting
- Check .gitignore includes `.erlmcp/`
- Verify lock file exists: `cat .erlmcp/cache/sessionstart.lock`

---

**Maintainer:** CRE Project Team
**Last Updated:** 2025-02-13
