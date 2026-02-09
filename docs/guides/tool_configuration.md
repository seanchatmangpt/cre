# CRE Development Environment - Tool Configuration Guide

This guide provides comprehensive instructions for setting up the CRE (Common Runtime Environment) development environment for Generative Analysis work. It covers Erlang/OTP installation, rebar3 configuration, CRE compilation, editor setup, Docker environments, and Git hooks.

---

## Table of Contents

1. [Erlang/OTP Setup](#1-erlangotp-setup)
2. [rebar3 Configuration](#2-rebar3-configuration)
3. [CRE Compilation](#3-cre-compilation)
4. [Editor Configuration](#4-editor-configuration)
5. [Docker Environment](#5-docker-environment)
6. [Git Hooks](#6-git-hooks)
7. [Verification](#7-verification)
8. [Troubleshooting](#8-troubleshooting)

---

## 1. Erlang/OTP Setup

CRE requires Erlang/OTP 25.0 or higher. The project is tested on OTP 25, 26, 27, and 28.

### Version Requirements

| Component | Minimum Version | Recommended |
|-----------|----------------|-------------|
| Erlang/OTP | 25.0 | 27.2 or 28.0 |
| rebar3 | 3.0.0 | 3.24.0+ |

### Installing Erlang/OTP by Platform

#### macOS

**Option 1: Homebrew (Recommended)**

```bash
# Install Erlang/OTP
brew install erlang

# Verify installation
erl -version
# Should show: Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 27.x or 28.x

# Install rebar3
brew install rebar3

# Verify rebar3
rebar3 version
```

**Option 2: asdf (Version Manager)**

```bash
# Install asdf if not already installed
brew install asdf

# Add asdf to shell (add to ~/.zshrc or ~/.bashrc)
echo -e "\n. $(brew --prefix asdf)/libexec/asdf.sh" >> ~/.zshrc
source ~/.zshrc

# Install Erlang plugin
asdf plugin add erlang

# Install Erlang 27.2 (includes required dependencies)
asdf install erlang 27.2
asdf global erlang 27.2

# Install rebar3 plugin
asdf plugin add rebar3
asdf install rebar3 latest
asdf global rebar3 latest

# Verify
erl -version
rebar3 version
```

**Option 3: Kerl (Erlang Version Manager)**

```bash
# Install kerl
brew install kerl

# List available versions
kerl list builds

# Build and install Erlang 27.2
kerl build 27.2 27.2
kerl install 27.2 ~/kerl/27.2

# Activate (add to shell profile)
. ~/kerl/27.2/activate

# Verify
erl -version
```

#### Linux (Ubuntu/Debian)

**Option 1: Package Manager**

```bash
# Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb

# Update and install
sudo apt-get update
sudo apt-get install esl-erlang elixir

# Install rebar3
sudo apt-get install rebar3

# Verify
erl -version
rebar3 version
```

**Option 2: asdf**

```bash
# Install dependencies
sudo apt-get install -y build-essential autoconf m4 libncurses5-dev \
  libssl-dev libwxgtk3.2-dev libgl1-mesa-dev libglu1-mesa-dev \
  libpng-dev libssh-dev unixodbc-dev xsltproc fop libxml2-utils

# Install asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.13.1
echo '. "$HOME/.asdf/asdf.sh"' >> ~/.bashrc
echo '. "$HOME/.asdf/completions/asdf.bash"' >> ~/.bashrc
source ~/.bashrc

# Install Erlang
asdf plugin add erlang
asdf install erlang 27.2
asdf global erlang 27.2

# Install rebar3
asdf plugin add rebar3
asdf install rebar3 latest
asdf global rebar3 latest
```

#### Windows

**Option 1: WSL2 (Recommended)**

Follow the Linux instructions within WSL2 Ubuntu.

**Option 2: Native Windows**

1. Download from [Erlang/OTP Official Site](https://www.erlang.org/downloads)
2. Run the installer (OTP 27.2 or 28.0 recommended)
3. Download rebar3 from [GitHub](https://github.com/erlang/rebar3/releases)
4. Place `rebar3.exe` in your PATH

### Erlang Configuration for CRE

CRE uses the following compiler options defined in `rebar.config`:

```erlang
{erl_opts, [
    debug_info,           % Include debug info
    bin_opt_info,         % Binary compilation info
    {platform_define, "^[0-9]+", 'OTP_25_PLUS'},  % OTP 25+ flag
    {doc, "excerpt"},     % Enable documentation storage for doctests
    {src_dirs, ["src", "src/core", "src/pnet", "src/wf",
                "src/yawl", "src/patterns", "src/api",
                "src/integration", "src/http", "src/app", "src/nato"]}
]}.
```

---

## 2. rebar3 Configuration

### Installing rebar3

#### Download Directly (Platform-Independent)

```bash
# Download rebar3
curl -L -o rebar3 https://s3.amazonaws.com/rebar3/rebar3

# Make executable
chmod +x rebar3

# Optionally move to PATH
sudo mv rebar3 /usr/local/bin/

# Verify
rebar3 version
```

#### Use Project-Local rebar3 (CRE Recommended)

```bash
# Copy to project root
cp /path/to/rebar3 /Users/sac/cre/

# Use via ./rebar3
cd /Users/sac/cre
./rebar3 version
```

### CRE rebar3 Configuration

Located in `/Users/sac/cre/rebar.config`:

```erlang
%% Project version for CRE 0.3.0
%% Minimum OTP version: 25.0
%% Tested on: OTP 25, 26, 27, 28

{deps, [
    {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {branch, "master"}}},
    {lib_combin, {git, "https://github.com/joergen7/lib_combin.git", {ref, "953273d875ce4eb4119219bb0d1855acc258586c"}}},
    {cowboy, {git, "https://github.com/ninenines/cowboy.git", {tag, "2.14.2"}}},
    {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}},
    {ranch, {git, "https://github.com/ninenines/ranch.git", {tag, "2.1.0"}}},
    {jsx, {git, "https://github.com/talentdeficit/jsx.git", {tag, "v3.1.0"}}},
    {jsone, {git, "https://github.com/sile/jsone.git", {tag, "1.9.0"}}},
    {yamerl, {git, "https://github.com/yakaz/yamerl.git", {tag, "0.10.0"}}}
]}.

%% OTP 28 compatibility: override cowboy's cowlib dep
{overrides, [
    {override, cowboy, [
        {deps, [
            {cowlib, {git, "https://github.com/ninenines/cowlib.git", {tag, "2.16.0"}}},
            {ranch, {git, "https://github.com/ninenines/ranch.git", {tag, "2.1.0"}}}
        ]}
    ]}
]}.

%% Profiles
{profiles, [
    {test, [
        {cover_enabled, false},
        {erl_opts, [debug_info, {doc, "excerpt"}, {d, 'TEST'}, {i, "src/wf"}, {i, "include"}]},
        {deps, [{meck, "0.9.2"}]}
    ]},
    {concuerror, [{deps, [{concuerror, "0.21.0"}]}]}
]}.

%% Dialyzer configuration
{dialyzer, [
    {warnings, [unmatched_returns, error_handling, underspecs]},
    {plt_extra_apps, [lib_combin, gen_pnet, jsone, xmerl]},
    {get_warnings, true},
    {include_dirs, ["include"]},
    {exclude_apps, []}
]}.
```

### Common rebar3 Commands

```bash
# Compile the project
rebar3 compile

# Clean build artifacts
rebar3 clean

# Get/update dependencies
rebar3 get-deps
rebar3 upgrade

# Run tests
rebar3 eunit           # EUnit tests
rebar3 ct              # Common Test suites

# Static analysis
rebar3 dialyzer        # Type analysis
rebar3 xref            # Cross-reference checks

# Documentation
rebar3 edoc            # Generate API docs

# Interactive shell
rebar3 shell

# Format code
rebar3 efmt -c         # Format all files
rebar3 efmt -c --check # Check format without modifying

# Build release
rebar3 as prod tar
```

---

## 3. CRE Compilation

### Initial Setup

```bash
# Navigate to CRE directory
cd /Users/sac/cre

# Fetch dependencies
./rebar3 get-deps

# Compile the project
./rebar3 compile

# Verify compilation
ls -la _build/default/lib/cre/ebin/
```

### Compilation Profiles

#### Default Profile

```bash
# Standard compilation
./rebar3 compile

# Output location
_build/default/lib/cre/ebin/
```

#### Test Profile

```bash
# Compile with test dependencies
./rebar3 as test compile

# Output location
_build/test/lib/cre/ebin/

# Includes meck (mocking library)
```

#### Production Profile

```bash
# Build production release
./rebar3 as prod compile
./rebar3 as prod tar

# Output location
_build/prod/rel/cre/
```

### Handling Compile Race Condition

CRE has a known intermittent race condition with `yawl_persistence.beam` file renaming. Use the provided script:

```bash
# Use the workaround script for reliable compilation
./scripts/run_eunit.sh
```

The script pre-creates the `ebin` directory before compilation:

```bash
#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."
rm -rf _build/test
mkdir -p _build/test/lib/cre/ebin
rebar3 as test compile
rebar3 eunit "$@"
```

### Verifying Compilation

```bash
# Start Erlang shell with CRE loaded
./rebar3 shell

# In the shell, verify application loads
1> application:load(cre).
ok

2> application:which_applications().
[{cre,"Common Runtime Environment for distributed programming languages","0.3.0"},
 {kernel,"ERTS  CXC 138 10","9.0.5"},
 {stdlib,"ERTS  CXC 138 10","4.3"},
 ...]
```

---

## 4. Editor Configuration

### VS Code

Install the Erlang extension:

```bash
# Install VS Code Erlang extension
code --install-extension pgourlain.erlang
```

Create `.vscode/settings.json`:

```json
{
    "erlang.rebarPath": "${workspaceFolder}/rebar3",
    "erlang.erlangPath": "/usr/local/bin",
    "erlang.languageServerPath": "${workspaceFolder}/_build/default/lib/els_core",
    "files.associations": {
        "*.erl": "erlang",
        "*.hrl": "erlang",
        "*.app.src": "erlang",
        "*.config": "erlang"
    },
    "editor.formatOnSave": true,
    "[erlang]": {
        "editor.defaultFormatter": "pgourlain.erlang",
        "editor.tabSize": 4,
        "editor.insertSpaces": true
    },
    "files.exclude": {
        "**/_build": true,
        "**/*.beam": true,
        "**/erl_crash.dump": true
    }
}
```

Create `.vscode/tasks.json`:

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Compile CRE",
            "type": "shell",
            "command": "./rebar3 compile",
            "group": {
                "kind": "build",
                "isDefault": true
            },
            "problemMatcher": []
        },
        {
            "label": "Run EUnit Tests",
            "type": "shell",
            "command": "./scripts/run_eunit.sh",
            "group": {
                "kind": "test",
                "isDefault": true
            },
            "problemMatcher": []
        },
        {
            "label": "Format Code",
            "type": "shell",
            "command": "./rebar3 efmt -c",
            "problemMatcher": []
        },
        {
            "label": "Dialyzer Analysis",
            "type": "shell",
            "command": "./rebar3 dialyzer",
            "problemMatcher": []
        },
        {
            "label": "Full Check",
            "type": "shell",
            "command": "./rebar3 compile && ./rebar3 eunit && ./rebar3 dialyzer",
            "group": "test",
            "problemMatcher": []
        }
    ]
}
```

### Emacs

Add to `~/.emacs.d/init.el` or `~/.config/emacs/init.el`:

```elisp
;; Erlang mode configuration
(require 'erlang-start)

;; Set erlang root (adjust to your system)
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/bin" exec-path))
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

;; Auto-load Erlang mode for .erl and .hrl files
(setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hrl$" . erlang-mode) auto-mode-alist))

;; CRE project-specific settings
(add-hook 'erlang-mode-hook
          (lambda ()
            ;; Use 4 spaces for indentation
            (setq erlang-indent-level 4)
            (setq indent-tabs-mode nil)

            ;; Set rebar3 for compilation
            (setq compile-command "rebar3 compile")

            ;; Enable font locking for improved syntax highlighting
            (font-lock-mode 1)))

;; Flycheck for Erlang (requires flycheck package)
(require 'flycheck)
(add-hook 'erlang-mode-hook 'flycheck-mode)

;; Use rebar3 for syntax checking
(setq flycheck-erlang-rebar3-executable "rebar3")

;; Company mode for autocompletion (requires company package)
(require 'company)
(add-hook 'erlang-mode-hook 'company-mode)
```

### Vim/Neovim

Create `~/.vim/ftplugin/erlang.vim`:

```vim
" Erlang filetype plugin for CRE development

" Use 4 spaces for indentation
setlocal expandtab
setlocal shiftwidth=4
setlocal tabstop=4
setlocal softtabstop=4

" Path settings for CRE
setlocal path+=src/,src/core/,src/pnet/,src/wf/,src/yawl/
setlocal path+=src/patterns/,src/api/,src/http/,src/integration/
setlocal path+=src/app/,include/
setlocal includeexpr=substitute(v:fname,'\\.hrl','','')

" Build commands
setlocal makeprg=rebar3\ compile
setlocal errorformat=%f:%l:\ %m

" Abbreviations for common Erlang constructs
iabbr <buffer> amodule -*- erlang -*-<CR>-module(<C-r>()).<CR>-author("").<CR><CR>-export([]).
iabbr <buffer> aspec -spec <C-r>() -> <C-r>().

" Syntax highlighting
syntax on
filetype plugin indent on
```

For Neovim with LSP support, create `~/.config/nvim/lua/erlang.lua`:

```lua
-- Erlang LSP configuration for Neovim
require('lspconfig').erlangls.setup({
    cmd = {"erlang_ls"},
    settings = {
        erlang = {
            otp_path = "/usr/local/lib/erlang",
            rebar3_config = "/Users/sac/cre/rebar.config"
        }
    },
    root_dir = require('lspconfig').util.root_pattern('rebar.config')
})
```

### Formatting with EFmt

CRE uses EFmt for code formatting:

```bash
# Format all files
rebar3 efmt -c

# Format specific file
rebar3 efmt -c src/my_module.erl

# Check format without modifying
rebar3 efmt -c --check

# Format and write in-place
rebar3 efmt -w src/
```

Editor integration:

- **VS Code**: Uses the Erlang extension's built-in formatter
- **Emacs**: Add `(add-hook 'before-save-hook 'efmt-format-before-save)` to your config
- **Vim**: `autocmd BufWritePre *.erl :!efmt %`

---

## 5. Docker Environment

CRE provides Docker configurations for isolated development environments.

### Development Container

Located at `/Users/sac/cre/.devcontainer/Dockerfile.test`:

```dockerfile
FROM erlang:28

# Install build tools
RUN apt-get update && apt-get install -y \
    git \
    curl \
    wget \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install rebar3
RUN git clone --depth 1 https://github.com/erlang/rebar3 /tmp/rebar3 && \
    cd /tmp/rebar3 && \
    ./bootstrap && \
    mv rebar3 /usr/local/bin/

WORKDIR /workspace

COPY rebar.config ./
COPY src ./src
COPY test ./test
COPY include ./include

RUN rebar3 get-deps && rebar3 compile

CMD ["rebar3", "ct"]
```

### Docker Compose Services

Located at `/Users/sac/cre/.devcontainer/docker-compose.test.yml`:

```yaml
version: '3.8'

services:
  cre-test:
    build:
      context: ..
      dockerfile: .devcontainer/Dockerfile.test
    container_name: cre-yawl-test
    volumes:
      - ../..:/workspace
    working_dir: /workspace
    ports:
      - "4142:4142"   # CRE HTTP port
      - "3000:3000"   # Dashboard port
    environment:
      - ERL_AFLAGS=-kernel shell_history enabled
      - REBAR_COLOR=1
    command: /bin/sh -c "rebar3 get-deps && rebar3 compile && rebar3 ct --verbose"

  cre-shell:
    build:
      context: ..
      dockerfile: .devcontainer/Dockerfile.test
    container_name: cre-yawl-shell
    volumes:
      - ../..:/workspace
    working_dir: /workspace
    ports:
      - "4142:4142"
      - "3000:3000"
    environment:
      - ERL_AFLAGS=-kernel shell_history enabled
      - REBAR_COLOR=1
    stdin_open: true
    tty: true
    command: /bin/zsh

  cre-interactive:
    image: erlang:27
    container_name: cre-yawl-interactive
    volumes:
      - ../..:/workspace
    working_dir: /workspace
    ports:
      - "4142:4142"
      - "3000:3000"
      - "9100:9100"  # Erlang observer
    environment:
      - ERL_AFLAGS=-kernel shell_history enabled -sname yawltest
      - REBAR_COLOR=1
      - LONGNAME=cre-yawl-dev
    stdin_open: true
    tty: true
    depends_on:
      - cre-test
    command: /bin/zsh
```

### Using Docker for Development

```bash
# Navigate to devcontainer directory
cd /Users/sac/cre/.devcontainer

# Build and run tests
docker-compose -f docker-compose.test.yml up cre-test

# Start interactive shell
docker-compose -f docker-compose.test.yml run --rm cre-shell

# Start with Erlang observer support
docker-compose -f docker-compose.test.yml run --rm cre-interactive

# Stop all services
docker-compose -f docker-compose.test.yml down

# Remove volumes
docker-compose -f docker-compose.test.yml down -v
```

### Production Docker Image

Located at `/Users/sac/cre/Dockerfile.production`:

```dockerfile
# Multi-stage production Dockerfile
FROM erlang:28-alpine AS builder

RUN apk add --no-cache \
    git curl build-base openssl-dev

RUN git clone --depth 1 https://github.com/erlang/rebar3 /tmp/rebar3 && \
    cd /tmp/rebar3 && \
    ./bootstrap && \
    mv rebar3 /usr/local/bin/ && \
    rm -rf /tmp/rebar3

WORKDIR /build

COPY rebar.config ./
COPY src ./src
COPY include ./include

RUN rebar3 get-deps && \
    rebar3 compile && \
    rebar3 as prod tar

# Runtime stage
FROM alpine:3.20

RUN apk add --no-cache \
    openssl ncurses-libs libstdc++ tzdata curl

RUN addgroup -g 1000 cre && \
    adduser -D -u 1000 -G cre cre

COPY --from=builder /build/_build/prod/rel/cre /opt/cre

RUN mkdir -p /opt/cre/data /opt/cre/log /opt/cre/checkpoints && \
    chown -R cre:cre /opt/cre

USER cre
WORKDIR /opt/cre

EXPOSE 4142

HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:4142/status.json || exit 1

STOPSIGNAL SIGTERM

ENTRYPOINT ["/opt/cre/bin/cre", "foreground"]
```

Build and run production image:

```bash
# Build production image
docker build -f Dockerfile.production -t cre-yawl:0.3.0 .

# Run container
docker run -d \
  --name cre-yawl \
  -p 4142:4142 \
  -v cre-data:/opt/cre/data \
  -v cre-log:/opt/cre/log \
  cre-yawl:0.3.0

# Check logs
docker logs -f cre-yawl

# Check health
curl http://localhost:4142/status.json
```

---

## 6. Git Hooks

CRE uses Git hooks for pre-commit validation to ensure code quality.

### Pre-Commit Hook

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
set -e

echo "Running CRE pre-commit checks..."

# Get list of staged Erlang files
ERL_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.erl$' || true)

if [ -z "$ERL_FILES" ]; then
    echo "No Erlang files staged. Skipping checks."
    exit 0
fi

echo "Checking code format..."
if ! rebar3 efmt -c --check; then
    echo "Code formatting check failed. Run: rebar3 efmt -c"
    exit 1
fi

echo "Compiling..."
if ! rebar3 compile; then
    echo "Compilation failed. Please fix errors before committing."
    exit 1
fi

echo "Running quick tests..."
if ! ./scripts/run_doctests.sh > /dev/null 2>&1; then
    echo "Doctests failed. Run: ./scripts/run_doctests.sh"
    exit 1
fi

echo "Pre-commit checks passed!"
exit 0
```

Make it executable:

```bash
chmod +x .git/hooks/pre-commit
```

### Pre-Push Hook

Create `.git/hooks/pre-push`:

```bash
#!/bin/bash
set -e

echo "Running CRE pre-push checks..."

echo "Full compilation check..."
rebar3 clean -a
rebar3 compile

echo "Running all tests..."
rebar3 eunit
rebar3 ct

echo "Static analysis..."
rebar3 dialyzer || echo "Dialyzer warnings found (non-blocking)"
rebar3 xref || echo "XRef warnings found (non-blocking)"

echo "Pre-push checks completed!"
exit 0
```

Make it executable:

```bash
chmod +x .git/hooks/pre-push
```

### Commit Message Hook

Create `.git/hooks/commit-msg`:

```bash
#!/bin/bash

# Enforce structured commit messages
COMMIT_MSG_FILE=$1
COMMIT_MSG=$(cat "$COMMIT_MSG_FILE")

# Pattern: type(scope): subject
PATTERN="^(feat|fix|docs|style|refactor|perf|test|chore|pattern)(\(.+\))?: .{1,50}"

if ! echo "$COMMIT_MSG" | head -n1 | grep -qE "$PATTERN"; then
    echo "Invalid commit message format."
    echo ""
    echo "Expected format: type(scope): subject"
    echo ""
    echo "Types: feat, fix, docs, style, refactor, perf, test, chore, pattern"
    echo "Scopes: yawl, pnet, wf, api, http, telemetry, docs"
    echo ""
    echo "Example: feat(yawl): add arbitrary cycle pattern"
    exit 1
fi

exit 0
```

Make it executable:

```bash
chmod +x .git/hooks/commit-msg
```

### CI/CD Integration

CRE uses GitHub Actions for CI. Located at `.github/workflows/ci.yml`:

```yaml
name: build
on:
  push:
    branches: [master]
  pull_request:
    branches: [master]
jobs:
  ci:
    name: Run checks over ${{matrix.otp}}
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: ['25.0', '26.0', '27.0']
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{matrix.otp}}
          rebar3-version: '3.24.0'
      - run: rebar3 compile
      - run: rebar3 xref
      - run: rebar3 efmt -c
      - run: rebar3 eunit
      - run: rebar3 dialyzer
      - run: rebar3 edoc
```

---

## 7. Verification

After setup, verify your environment:

```bash
# Navigate to CRE directory
cd /Users/sac/cre

# 1. Check Erlang version
erl -version
# Expected: Erlang (BEAM) emulator version 25.x, 26.x, 27.x, or 28.x

# 2. Check rebar3 version
./rebar3 version
# Expected: version 3.24.0 or higher

# 3. Compile the project
./rebar3 compile
# Expected: Compilation successful with no errors

# 4. Run EUnit tests
./scripts/run_eunit.sh
# Expected: All tests pass

# 5. Run doctests
./scripts/run_doctests.sh
# Expected: All doctests passed

# 6. Start interactive shell
./rebar3 shell
# In shell:
# 1> application:start(cre).
# ok
# 2> cre:hello().
# "Hello from CRE!"
# 3> q().
```

### Verification Checklist

- [ ] Erlang/OTP 25+ installed
- [ ] rebar3 3.0+ installed
- [ ] CRE compiles without errors
- [ ] EUnit tests pass
- [ ] Doctests pass
- [ ] Can start `rebar3 shell`
- [ ] Can load CRE application
- [ ] Editor configured for Erlang syntax
- [ ] Git hooks installed (optional)
- [ ] Docker builds successfully (optional)

---

## 8. Troubleshooting

### Common Issues

#### "Module not found" Error

**Symptoms**: Compiler cannot find a module.

**Solutions**:
1. Verify the module is in a valid source directory (`src/` or subdirectories)
2. Run `rebar3 clean` then `rebar3 compile`
3. Verify module name matches filename (e.g., `my_module.erl` contains `-module(my_module).`)

#### "PLT not found" Dialyzer Error

**Symptoms**: Dialyzer fails with PLT-related error.

**Solution**:
```bash
# Build PLT (takes time on first run)
rebar3 dialyzer
```

#### EUnit "beam rename" Race Condition

**Symptoms**: Intermittent failure renaming `yawl_persistence.beam`.

**Solution**:
```bash
# Use the workaround script
./scripts/run_eunit.sh
```

#### Git Hooks Not Executing

**Symptoms**: Hooks installed but not running.

**Solutions**:
1. Verify hooks are executable: `ls -l .git/hooks/`
2. Check file system permissions
3. Ensure hooks use proper shebang: `#!/usr/bin/env bash`

#### Docker Build Fails

**Symptoms**: Docker image build fails.

**Solutions**:
1. Clean Docker cache: `docker system prune -a`
2. Verify base image is available: `docker pull erlang:28`
3. Check Docker daemon is running

### Getting Help

- **Documentation**: See `docs/` directory
- **Tests**: Look at `test/` for usage examples
- **Issues**: Report problems on GitHub
- **Interactive Debugging**: Use `rebar3 shell` for testing

### Development Commands Reference

| Command | Description |
|---------|-------------|
| `./rebar3 compile` | Compile source code |
| `./rebar3 shell` | Interactive Erlang shell |
| `./rebar3 eunit` | Run EUnit tests |
| `./rebar3 ct` | Run Common Test suites |
| `./rebar3 dialyzer` | Type analysis |
| `./rebar3 xref` | Cross-reference analysis |
| `./rebar3 edoc` | Generate documentation |
| `./rebar3 efmt -c` | Format code |
| `./scripts/run_doctests.sh` | Run all doctests |
| `./scripts/run_eunit.sh` | EUnit with compile workaround |

---

## Appendix: Platform-Specific Notes

### macOS Notes

- Homebrew Erlang installation may require Xcode Command Line Tools
- asdf requires modifying shell config (`~/.zshrc` or `~/.bash_profile`)
- macOS may limit file watchers; increase with `fs.inotify.max_user_watches`

### Linux Notes

- Some distros require building Erlang from source for latest versions
- SELinux may interfere with Erlang distributed communication
- Package managers may have older Erlang versions

### Windows Notes

- WSL2 is strongly recommended over native Windows
- Native Windows Erlang may have path separator issues
- Git Bash or similar required for shell scripts

---

For more information, see:

- [Build System Guide](BUILD_SYSTEM.md)
- [Contributing Guide](CONTRIBUTING.md)
- [CRE Architecture](ARCHITECTURE.md)
- [rebar3 Documentation](https://rebar3.org/docs/)
- [Erlang/OTP Documentation](https://www.erlang.org/doc/)
