# cre migration plan v26.11.16

Standing: UNVERIFIED

## Identity
- Path: /Users/sac/cre
- Remote: https://github.com/seanchatmangpt/cre.git (fork of joergen7/cre)
- Branch@sha: otp27/ssl-hardening @ fe3db7b6 (NO upstream tracking)
- Last commit: 2026-10-13; commits: 472; size: 4765 MB
- Languages/build: Erlang/OTP (rebar3), Justfile, Makefile, Docker, k8s, cloudbuild.yaml
- Nested repos: `vendors/yawl` (1.0 GB, remote seanchatmangpt/yawl, shows as modified but has
  no `.gitmodules` entry -- broken submodule mapping); `vendors/wf-substrate` (343 MB)

## Observed state
- Dirty: 1 (`vendors/yawl` pointer)
- Untracked: 52, grouped: `.claude/rules/`, `.claude/skills/`, `.cursor/`, `Justfile`, `attic/`,
  `docs/jira/` (whole dir untracked, incl. v26.9.19), `docs/ontology/`, `docs/papers/*SYNTHES*`
- Ahead/behind: n/a (no upstream on current branch); `master` tracks origin/master;
  `merge-all-remote-branches` ahead 2; four `otp27/*` branches with no upstream
- Stash: 430 (census); unpushed local branches: 5
- Build dirs: no `_build` present. `vendors/` 1.4 GB, `docs/` 285 MB (papers)
- Junk at top level: files literally named `-A`, `-i`, `5`, `grep` (shell-redirect accidents),
  stray `*.beam` files, `concuerror_report.txt`, `erl_crash`-style leftovers
- Submodules: `git submodule status` errors ("no submodule mapping for vendors/yawl")

## What it is
CRE: a YAWL workflow engine in Erlang/OTP (36 of 43 YAWL patterns), with human-in-the-loop
approvals, OpenTelemetry and a web dashboard. Requires OTP 28 per CLAUDE.md.

## Classification
ACTIVE (census agrees; current branch is a month old with 5 unpushed branches of OTP27 work).

## Migration risk
HIGH. Would lose: current branch `otp27/ssl-hardening` and 3 sibling `otp27/*` branches (no
upstream), `merge-all-remote-branches` ahead 2, 430 stashes, untracked `docs/jira/`,
`docs/ontology/`, `.claude/` rules/skills, and the `vendors/yawl` nested repo state.

## Recommended action
COMMIT-PUSH
1. `git -C /Users/sac/cre add -A && git commit -m "preserve(v26.11.16): jira/ontology docs + rules"`
2. `git -C /Users/sac/cre push -u origin otp27/ssl-hardening`
3. `git -C /Users/sac/cre push -u origin otp27/gb-trees-timerq otp27/maybe-expressions otp27/native-json otp27/process-aliases merge-all-remote-branches`
4. `git -C /Users/sac/cre/vendors/yawl status` then push its branch:
   `git -C /Users/sac/cre/vendors/yawl push -u origin HEAD`
5. DECISION: fix `vendors/yawl` -- either add a proper `.gitmodules` entry or move it out of
   the tree and depend on it via rebar.config.
6. DECISION: 430 stashes -- archive via `git bundle create /Volumes/ARCHIVE/cre.bundle --all`.
7. Cleanup: `rm -f /Users/sac/cre/-A /Users/sac/cre/-i /Users/sac/cre/5 /Users/sac/cre/grep`
   (use `rm -- -A -i` form) and `rm -rf _build vendors/*/_build`.
8. New machine: `git clone https://github.com/seanchatmangpt/cre.git` then re-clone
   `https://github.com/seanchatmangpt/yawl` into `vendors/yawl` if step 5 keeps it nested.

## First commands on the new machine
`rebar3 compile && rebar3 eunit` (OTP 28 required) -- stated, not run.

## See Also
- ../v26.9.19/ (previous jira dir, currently untracked)
- /Users/sac/docs/jira/v26.11.16/README.md (master index)
