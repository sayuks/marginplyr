# Retiring installed Review-ready admission

The maintainer accepted the simplified repository-owned runner in
[#1114](https://github.com/sayuks/marginplyr/issues/1114).
[Local checks](../design/agents/local-checks.md) and the
[runner guide](review-ready-runner.md) describe the active repository entry.

## Completed Mac cutover

The one-time drained cutover completed on 2026-11-02 at 01:08 UTC. Immediately
before removal, the installed CLI's `--status` returned exit 0 with
`requests: []`. The operation held the old admission, evidence and any request
manager locks after acquiring them without waiting, and held the new stable
heavy lock through the mutations. No waiting or running check was cancelled.

The operation removed only the installed
`/Users/sayuks/.local/bin/marginplyr-review-ready`, the obsolete checker-owned
`~/.local/state/marginplyr/review-ready/`, and the old checker's
`review-ready-failures` directory under `tools::R_user_dir("marginplyr", "cache")`.
The new `~/.local/state/marginplyr/review-ready-heavy.lock` remains in place.
Unrelated tools, libraries, caches, research, history and OS reports were outside
the operation's targets.

The marginplyr sections in user-wide Codex `AGENTS.md` and Claude `CLAUDE.md`
were replaced together with the explicit repository R entry, five mandatory
checks, AI scope judgment, shared heavy slot and original execution ownership.
The operation verified that all unrelated instruction bytes were preserved.
A checkout lacking the new `--scope`/`--reason` interface needs an update before
checking; there is no installed-CLI reinstall or fallback path.

The cutover's terminal exit 0 establishes retirement and instruction replacement.
[PR #1124](https://github.com/sayuks/marginplyr/pull/1124) records real `source`
and `full` results by pinned SHA, retained execution handle and terminal outcome;
cutover alone is not a package-check pass.

Old worktrees and their existing sessions were not inventoried or migrated.
They remain outside migration scope, with no individual refresh, compatibility
launcher, status/recovery adapter or automatic migration service.

## History

The installed generation coordinated a heavy slot and later added automatic
classification, stage reuse, authenticated evidence and strict recovery.
Those mechanisms are retired by the 2026-11-02 simplification. Its historical
contract is preserved in [the earlier guide](https://github.com/sayuks/marginplyr/blob/1a9dd00a/tools/review-ready-admission.md);
[ADR 0038](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md)
records the superseded evidence decision. Historical commands are not current
setup instructions.
