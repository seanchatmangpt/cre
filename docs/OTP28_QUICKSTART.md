# OTP 28 Quick Start - Claude Code Web

**Goal:** Get Erlang/OTP 28 working in ANY Claude Code web session in < 5 minutes

---

## 🚀 Copy-Paste Setup (3 Commands)

```bash
# 1. Create hook directory
mkdir -p .claude/hooks

# 2. Download SessionStart hook
curl -fsSL https://raw.githubusercontent.com/joergen7/cre/main/.claude/hooks/SessionStart.sh \
  -o .claude/hooks/SessionStart.sh && chmod +x .claude/hooks/SessionStart.sh

# 3. Create settings
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
      }
    ]
  }
}
EOF

# 4. Ignore cache directory
echo ".erlmcp/" >> .gitignore

# 5. Commit
git add .claude/ .gitignore
git commit -m "Add OTP 28 SessionStart hook"
git push
```

**Done!** Next Claude Code web session will have OTP 28 automatically.

---

## ✅ Verify It Works

```bash
# Check version
erl -version
# Expected: Erlang/OTP 28 [erts-15.x.x] ...

# Check location
which erl
# Expected: /home/user/your-repo/.erlmcp/otp-28.3.1/bin/erl

# Test compile
echo '-module(test). -export([hello/0]). hello() -> world.' > /tmp/test.erl
erlc /tmp/test.erl && echo "✓ Works!"
```

---

## 📖 Full Documentation

For troubleshooting, advanced config, and architecture:
- [Complete Guide](./OTP28_CLAUDE_CODE_WEB_GUIDE.md)
- [gVisor Setup Details](./GVISOR_OTP_SETUP.md)

---

## 🐛 Common Issues

### "All OTP acquisition methods failed"
```bash
# Check logs
cat .erlmcp/sessionstart.log

# Test network
curl -I https://repo.hex.pm/builds/otp/ubuntu-22.04/OTP-28.3.1.tar.gz
```

### Hook timeout
```json
// Increase timeout in .claude/settings.json
"timeout": 900000  // 15 minutes
```

### Wrong architecture
```bash
# Check platform
uname -m
# Must match OTP binary arch (usually x86_64)
```

---

**Questions?** [File an issue](https://github.com/joergen7/cre/issues)
