# OTP 28 Setup for Claude Code Web (gVisor Sandbox)

> **Quick Start:** See [OTP28_QUICKSTART.md](./OTP28_QUICKSTART.md) for copy-paste setup
>
> **Complete Guide:** See [OTP28_CLAUDE_CODE_WEB_GUIDE.md](./OTP28_CLAUDE_CODE_WEB_GUIDE.md) for architecture and troubleshooting

---

## Problem

Claude Code Web runs bash commands in a **gVisor sandbox** which has significant limitations:

- Many Linux syscalls are **blocked or unimplemented** (76 of 350 syscalls)
- Building Erlang/OTP from source fails due to missing syscalls like:
  - `perf_event_open` - used by configure checks
  - `kcmp` - process comparison
  - Various module/clone operations
- Network access is **filtered through a proxy** - only allowed domains work
- Standard package managers (apt, yum) may not work

## Solution: Automated SessionStart Hook

The CRE project provides a **SessionStart hook** that automatically:
1. Downloads pre-built OTP 28 binaries (from Hex.pm or GitHub)
2. Caches locally in `.erlmcp/otp-28.3.1/`
3. Sets up environment variables
4. Downloads and caches rebar3
5. Builds your project

**Setup time:** 60-120 seconds (first run), 15-20 seconds (cached)

### Quick Start (For Claude Code Web Users)

If OTP 28 is already hosted:

1. **Add the OTP URL to your environment** or let SessionStart try default locations
2. The SessionStart hook will automatically download and extract it
3. Cached locally in `.erlmcp/otp-28.3.1/`

### Building a Static OTP Binary (One-time Setup)

Run this **outside** the sandbox (local machine, CI, GitHub Actions):

```bash
# From your project root
./scripts/build_static_otp.sh 28.3.1

# This creates: otp-28.3.1-linux-x86_64-static.tar.gz
```

#### Host the Binary

Choose one of these options:

**Option A: GitHub Releases (Recommended)**
```bash
# Create a release and upload
gh release create otp-28.3.1 \
  --title "Erlang/OTP 28.3.1 Static Binary" \
  --notes "Pre-built OTP for gVisor sandbox environments" \
  otp-28.3.1-linux-x86_64-static.tar.gz
```

**Option B: S3 or CloudFlare R2**
```bash
aws s3 cp otp-28.3.1-linux-x86_64-static.tar.gz \
  s3://my-bucket/otp-binaries/ --acl public-read
```

**Option C: Any static file host** (Netlify Drop, Cloudflare Pages, etc.)

### Update SessionStart.sh

Once hosted, update the `OTP_STATIC_URL` in `.claude/hooks/SessionStart.sh`:

```bash
readonly OTP_STATIC_URL="https://your-host/otp-28.3.1-linux-x86_64-static.tar.gz"
```

## Verification

Test that OTP works in the sandbox:

```bash
# Should show: 28
erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().'

# Should compile your project
rebar3 compile
```

## Troubleshooting

### "All OTP acquisition methods failed"

**Cause:** No OTP found in cache, system, or download failed.

**Solutions:**
1. Check internet access from the sandbox
2. Verify the OTP download URL is accessible
3. Ensure the URL allows unauthenticated downloads

### Build from source fails with "configure: error"

**Cause:** gVisor blocks required syscalls for building.

**Solution:** You MUST use a pre-built binary. Building from source will not work in gVisor.

### "exec format error" when running erl

**Cause:** Architecture mismatch (e.g., ARM binary on x86).

**Solution:** Build the static binary for the correct target platform (usually x86_64-linux).

## Architecture Decisions

### Why Static Binary?

1. **gVisor compatibility:** No syscall issues during extraction
2. **No build tools needed:** gcc, make, autoconf not required in sandbox
3. **Fast download:** ~50MB vs 6+ hours building from source
4. **Reliable:** Works consistently across sandbox runs

### Why Not Docker/OCI?

1. gVisor already containers the execution
2. Docker-in-Docker has compatibility issues
3. Simpler to just extract a tarball

### Alternative: Install on Host

If you control the host system running Claude Code Web:

```bash
# On the host (outside sandbox)
sudo apt-get install erlang
```

SessionStart will detect and use the system installation.

## References

- [gVisor Syscall Compatibility](https://gvisor.dev/docs/user_guide/compatibility/linux/amd64/)
- [Erlang/OTP Releases](https://github.com/erlang/otp/releases)
- [Claude Code Sandboxing](https://code.claude.com/docs/en/sandboxing)
