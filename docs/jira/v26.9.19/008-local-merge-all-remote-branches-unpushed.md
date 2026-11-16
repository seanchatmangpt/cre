# cre: push local branch `merge-all-remote-branches` (ahead 2)

- Standing: OPEN
- Created: 2026-09-19 (v26.9.19 gh survey wave)
- Source: local branch `merge-all-remote-branches` is ahead 2 its upstream
- Evidence: `git for-each-ref --format='%(refname:short) %(upstream:track)'` → `merge-all-remote-branches` ahead 2

## Work to complete
- Push: `git push origin merge-all-remote-branches` (fetch first; reconcile if upstream moved).
- Or discard the local commits if they are obsolete.

## Acceptance
- `git for-each-ref` shows `merge-all-remote-branches` in sync (no ahead marker).

## History
- 2026-09-19 | OPEN | survey found unpushed commits | merge-all-remote-branches ahead 2 | push pending
