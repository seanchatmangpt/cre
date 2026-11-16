# cre: land or delete remote branch `claude/setup-otp-startup-script-2xUjr`

- Standing: OPEN
- Created: 2026-09-19 (v26.9.19 gh survey wave)
- Source: remote branch `claude/setup-otp-startup-script-2xUjr` — not merged into `master`, no open PR
- Evidence: `git branch -r --no-merged origin/master` lists it; absent from `gh pr list` heads

## Work to complete
- Decide: open a PR (`gh pr create -R seanchatmangpt/cre --head claude/setup-otp-startup-script-2xUjr`) or delete (`git push origin --delete claude/setup-otp-startup-script-2xUjr`).
- If superseded, delete; otherwise land through review.

## Acceptance
- After `git fetch --prune`, `git branch -r --no-merged origin/master` no longer lists `claude/setup-otp-startup-script-2xUjr`.

## History
- 2026-09-19 | OPEN | survey found PR-less unmerged branch | claude/setup-otp-startup-script-2xUjr | decision pending
