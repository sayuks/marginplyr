---
status: accepted
---

# Run Review-ready from the target worktree

For [#1114](https://github.com/sayuks/marginplyr/issues/1114), retain the
repository-owned R entry and adopt the maintainer's 2026-11-02 simplification.
The [specification](../specs/review-ready-repository-runner.md) records the
accepted requirements; [local checks](../agents/local-checks.md) and the
[runner guide](../../tools/review-ready-runner.md) own operation.

## Decision

Invoke `Rscript tools/review-ready.R --scope source|full --reason TEXT` from the
target root. Git supplies that worktree's implementation. Both forms run the
same five checks against a pinned committed source copy before requesting any
heavy capacity. The AI decides whether package build/check is required and
records its reason. A source-only result explicitly records the heavy omission.

After successful baseline checks, `full` waits on one stable OS-backed advisory
lock across this user's marginplyr clones/worktrees on the Mac, then starts
automatically. Its package build/check runs the complete suite and snapshots
once with four testthat workers. Preserve ordinary process/progress reporting,
the same execution handle, and bounded same-agent investigation. Focused tests
retain their separate per-worktree allowance with parallelism disabled.

Use focused existing process and locking libraries with base R rather than a
coordination service. Ordinary cancellation collects owned child work before
releasing capacity. Abrupt termination uses library cleanup and accepts that
a subsequent check may begin before every descendant stops. The runner has
no strict crash-recovery platform or automatic native-crash retry.

Group runner-owned artifacts under `.review-ready/runs/<run-id>/` in the
submitting worktree, outside Git administration and excluded from Git/source
builds. Apply one seven-day eligibility deadline from recorded UTC start to all
artifacts and outcomes. A later invocation cleans only inactive, safely owned
expired runs in that worktree. Keep stable lock targets and unrelated user data.

Routine invocation excludes infrastructure self-tests and real README/site
regeneration. Retained runner/progress regression suites execute in every-PR
CI; change-specific development checks remain appropriate.

## Amendment and history

The earlier accepted direction added main-based workload registration,
authenticated stage reuse under [ADR 0038](0038-reuse-unaffected-review-ready-stage-evidence.md),
strict recovery, private renderer capability validation, key rotation and
completion-based history retention. Those requirements are superseded by
[#1114](https://github.com/sayuks/marginplyr/issues/1114); the repository-owned
entry decision remains. The
[earlier ADR](https://github.com/sayuks/marginplyr/blob/1a9dd00a/design/adr/0039-run-review-ready-from-the-target-worktree.md)
preserves the full prior decision. Its earlier R bootstrap merged in
[PR #1117](https://github.com/sayuks/marginplyr/pull/1117) without establishing
local cutover.

## Adoption

Drain old checks before retiring the installed CLI, obsolete owned state and
coordination machinery. Update repository and user-wide instructions together.
Old worktrees and their running sessions are outside migration scope; no fleet
inventory, per-session update or compatibility launcher is required.
The installed command remains operational until this one-time cutover is
complete. The [retirement guide](../../tools/review-ready-admission.md) records
the transition. This ADR's accepted status alone does not assert implementation
or successful rollout.
