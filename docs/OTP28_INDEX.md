# OTP 28 Setup Documentation - Index

**Get Erlang/OTP 28 working in Claude Code web sessions (gVisor sandbox)**

---

## 📚 Documentation Guides

### 1. **Quick Start** → [OTP28_QUICKSTART.md](./OTP28_QUICKSTART.md)
**For:** First-time users who want to get started immediately
**Time:** 5 minutes
**Content:**
- 3-command copy-paste setup
- Verification steps
- Common error quick-fixes

### 2. **Complete Guide** → [OTP28_CLAUDE_CODE_WEB_GUIDE.md](./OTP28_CLAUDE_CODE_WEB_GUIDE.md)
**For:** Users who want to understand the system
**Time:** 20 minutes read
**Content:**
- Problem explanation (gVisor limitations)
- Solution architecture (6-phase bootstrap)
- Step-by-step setup with explanations
- Troubleshooting guide (7 common issues)
- Advanced configuration (custom OTP, hosting binaries)
- Performance metrics
- Full reference

### 3. **gVisor Technical Background** → [GVISOR_OTP_SETUP.md](./GVISOR_OTP_SETUP.md)
**For:** Technical users interested in gVisor limitations
**Time:** 10 minutes read
**Content:**
- gVisor syscall limitations
- Why building from source fails
- Static binary approach
- Architecture decisions

### 4. **Hook Implementation** → [.claude/hooks/README.md](../.claude/hooks/README.md)
**For:** Maintainers and advanced users
**Time:** 15 minutes read
**Content:**
- Hook configuration details
- Debugging procedures
- Modification guide
- Cache structure

---

## 🎯 Choose Your Path

### Path A: "Just make it work" (Recommended for most)
1. Read: [OTP28_QUICKSTART.md](./OTP28_QUICKSTART.md)
2. Copy-paste 4 commands
3. Done!

### Path B: "I want to understand it"
1. Read: [OTP28_CLAUDE_CODE_WEB_GUIDE.md](./OTP28_CLAUDE_CODE_WEB_GUIDE.md)
2. Follow step-by-step setup
3. Read architecture section
4. Bookmark troubleshooting

### Path C: "I need to customize it"
1. Read: [OTP28_CLAUDE_CODE_WEB_GUIDE.md](./OTP28_CLAUDE_CODE_WEB_GUIDE.md)
2. Jump to "Advanced Configuration" section
3. Read: [.claude/hooks/README.md](../.claude/hooks/README.md)
4. Modify SessionStart.sh

### Path D: "Why does gVisor block builds?"
1. Read: [GVISOR_OTP_SETUP.md](./GVISOR_OTP_SETUP.md)
2. Read: [gVisor Syscall Compatibility](https://gvisor.dev/docs/user_guide/compatibility/linux/amd64/)

---

## 📖 Quick Reference

### Files You Need

```
your-repo/
├── .claude/
│   ├── hooks/
│   │   └── SessionStart.sh          ← Copy from CRE repo
│   └── settings.json                ← Configure hook execution
└── .gitignore                       ← Add .erlmcp/ exclusion
```

### Download Links

```bash
# SessionStart.sh (latest version)
curl -fsSL https://raw.githubusercontent.com/joergen7/cre/main/.claude/hooks/SessionStart.sh \
  -o .claude/hooks/SessionStart.sh

# settings.json (minimal example)
curl -fsSL https://raw.githubusercontent.com/joergen7/cre/main/.claude/settings.json \
  -o .claude/settings.json
```

### Verification Commands

```bash
# OTP version
erl -version

# OTP location
which erl

# Test compile
echo '-module(test). -export([hello/0]). hello() -> world.' > /tmp/test.erl
erlc /tmp/test.erl

# Check cache
ls -lh .erlmcp/otp-28.3.1/bin/erl

# View logs
cat .erlmcp/sessionstart.log
```

---

## 🧭 Use Cases

### Use Case 1: New Erlang Project
**Goal:** Start fresh Erlang project with OTP 28

**Steps:**
1. Create repo: `git init my-erlang-app`
2. Add hook: [OTP28_QUICKSTART.md](./OTP28_QUICKSTART.md)
3. Create `rebar.config`:
   ```erlang
   {erl_opts, [debug_info]}.
   {deps, []}.
   {shell, [{apps, [my_app]}]}.
   ```
4. Push and open in Claude Code web
5. Develop!

### Use Case 2: Migrate Existing Project
**Goal:** Add OTP 28 support to existing Erlang project

**Steps:**
1. Clone repo
2. Follow: [OTP28_QUICKSTART.md](./OTP28_QUICKSTART.md)
3. Update `rebar.config` minimum OTP:
   ```erlang
   {minimum_otp_vsn, "28.0"}.
   ```
4. Test locally in Docker:
   ```bash
   docker run -it --rm -v $(pwd):/work -w /work erlang:28.3.1 sh
   rebar3 compile
   ```
5. Commit and push

### Use Case 3: Team Collaboration
**Goal:** Ensure all team members have OTP 28

**Steps:**
1. Add SessionStart hook to main branch
2. Document in project README:
   ```markdown
   ## Development Setup

   This project uses OTP 28. If using Claude Code web:
   1. The SessionStart hook will auto-install OTP 28
   2. Wait 60s on first session
   3. Verify: `erl -version`

   Local development: Install OTP 28 via:
   - macOS: `brew install erlang@28`
   - Ubuntu: `sudo apt-get install erlang`
   - Docker: `docker run -it erlang:28.3.1`
   ```
3. Add to CI/CD (GitHub Actions):
   ```yaml
   - uses: erlef/setup-beam@v1
     with:
       otp-version: '28.3.1'
   ```

### Use Case 4: Fork CRE Project
**Goal:** Use CRE as template with OTP 28 ready

**Steps:**
1. Fork: https://github.com/joergen7/cre
2. Clone your fork
3. Open in Claude Code web
4. OTP 28 already configured! ✅

---

## 🐛 Troubleshooting Index

| Error | See |
|-------|-----|
| "All OTP acquisition methods failed" | [Complete Guide § Troubleshooting](./OTP28_CLAUDE_CODE_WEB_GUIDE.md#issue-all-otp-acquisition-methods-failed) |
| "timeout exceeded (600000ms)" | [Complete Guide § Troubleshooting](./OTP28_CLAUDE_CODE_WEB_GUIDE.md#issue-timeout-exceeded-600000ms) |
| "exec format error" | [Complete Guide § Troubleshooting](./OTP28_CLAUDE_CODE_WEB_GUIDE.md#issue-exec-format-error) |
| Dependencies fail (yamerl, pc) | [Complete Guide § Troubleshooting](./OTP28_CLAUDE_CODE_WEB_GUIDE.md#issue-dependencies-fail-yamerl-pc) |
| Hook runs on every message | [Complete Guide § Troubleshooting](./OTP28_CLAUDE_CODE_WEB_GUIDE.md#issue-hook-runs-on-every-message) |
| Build from source fails | [gVisor Setup § Troubleshooting](./GVISOR_OTP_SETUP.md#troubleshooting) |
| Wrong OTP version | [Hooks README § Common Issues](../.claude/hooks/README.md#wrong-otp-version) |

---

## 📊 Performance Metrics

| Metric | First Run | Cached Run |
|--------|-----------|------------|
| Download OTP | 30-45s | 0s |
| Extract | 5-10s | 0s |
| Setup env | 1s | 1s |
| Download rebar3 | 2s | 0s |
| Get deps | 10-30s | 5s |
| Compile | 15-45s | 10s |
| **Total** | **60-120s** | **15-20s** |

**Cache location:** `.erlmcp/` (git-ignored, persists across sessions)

---

## 🔗 External Resources

- [Erlang/OTP 28 Release Notes](https://www.erlang.org/patches/OTP-28.3.1)
- [Erlang/OTP GitHub Releases](https://github.com/erlang/otp/releases)
- [Hex.pm Pre-built OTP Binaries](https://repo.hex.pm/builds/otp/)
- [gVisor Syscall Compatibility Table](https://gvisor.dev/docs/user_guide/compatibility/linux/amd64/)
- [Claude Code Hooks Documentation](https://code.claude.ai/docs/hooks)
- [rebar3 Documentation](https://rebar3.org/)

---

## 🤝 Contributing

Found an issue? Have an improvement?

1. **Bug reports:** [GitHub Issues](https://github.com/joergen7/cre/issues)
2. **Documentation improvements:** Submit PR to `docs/`
3. **Hook improvements:** Submit PR to `.claude/hooks/`

**Include:**
- Platform (gVisor, macOS, native Linux)
- OTP version
- Error message
- Excerpt from `.erlmcp/sessionstart.log`

---

## 📝 Version History

| Version | Date | Changes |
|---------|------|---------|
| 4.0.1 | 2025-02-13 | Complete documentation set |
| 4.0.0 | 2025-02-11 | gVisor compatibility |
| 3.0.0 | 2025-02-09 | Multi-platform support |
| 2.0.0 | 2025-02-01 | SessionStart hook |
| 1.0.0 | 2025-01-15 | Initial OTP 28 migration |

---

**Maintained by:** CRE Project Team
**License:** Apache-2.0
**Repository:** https://github.com/joergen7/cre
