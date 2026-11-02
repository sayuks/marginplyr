# Repository-owned Review-ready runner

[Local checks](../design/agents/local-checks.md) owns scope judgment and agent
workflow. The [specification](../design/specs/review-ready-repository-runner.md)
and [ADR 0039](../design/adr/0039-run-review-ready-from-the-target-worktree.md)
own the accepted design. The repository entry below became active in the
[drained Mac cutover](review-ready-admission.md) on 2026-11-02. Cutover alone is
not a check result; [PR #1124](https://github.com/sayuks/marginplyr/pull/1124)
records per-revision `source` and `full` results.

## Setup and invocation

Install `DESCRIPTION`'s `Config/Needs/review` development tools and make `jarl`
and `Rscript` available on PATH. These tools are separate from package runtime
Imports. The checked-out repository supplies the runner; adopting another
revision through Git updates it without installing a separate launcher.
If the checkout lacks the `--scope`/`--reason` interface, report that the checkout
must be updated before Review-ready can run. The retired installed command has
no reinstall or fallback path; old worktrees are outside migration scope.

From a clean committed worktree root:

```sh
Rscript tools/review-ready.R --scope source --reason 'Explain the heavy-check omission.'
Rscript tools/review-ready.R --scope full --reason 'Explain why package build/check is needed.'
```

Both forms run the five mandatory checks before any shared-slot request.
`source` ends with an explicit heavy-check omission. `full` waits inside the
same command, then starts automatically when its OS advisory lock is available.
Keep the same execution handle through checking, cleanup and terminal output.
The result always names the submitted SHA, scope and reason.

The lock at `~/.local/state/marginplyr/review-ready-heavy.lock` is shared by
this user's marginplyr clones/worktrees on the Mac. The account home is resolved
independently of caller HOME/cache overrides. Its stable target is an operating primitive, not an expiring run artifact.
Keep it in place while owners or waiters may exist. Focused tests have the
separate per-worktree allowance in local checks; four package workers are not
a user-wide process cap.

## Cancellation

Cancel the original invocation through its retained execution handle. An
ordinary cancellation stops and collects its owned child work before releasing
the heavy slot. A cancelled waiter must never start later. Retain available
logs and collect the terminal cancellation result before editing and resubmitting.

Abrupt R termination uses the existing process library's cleanup facilities.
It may release the lock before every descendant stops; strict crash-time
exclusion is outside this contract. Record actual Mac cleanup observations
and their limits during adoption. A free lock or absent process is not a
successful check result. There is no recovery command, watchdog, durable
recovery ledger or automatic retry.

## Artifacts and retention

Each invocation records one UTC start timestamp and groups all substantial
runner-owned data in `<worktree-root>/.review-ready/runs/<run-id>/`, outside
Git's administrative directory. This includes the pinned source and archive,
built tarball, check/install work, progress events, logs, result, and supported
child temporary/cache output. Git and R source builds exclude `.review-ready/`.
Supported child environment settings direct owned temporary and cache output
there without a separate renderer capability registry.

One lifetime covers every artifact and every outcome: seven consecutive
24-hour periods from the recorded UTC start time. Reads, log writes and a
missing finish record do not renew or change that clock. On a later invocation
in the same worktree, the runner may delete an expired inactive run directory
as one unit. An OS-backed liveness guard protects active data. Skip data whose
ownership or safety cannot be established, and report cleanup problems.

Seven days establishes deletion eligibility, not a maximum on-disk age. Other
worktrees are never swept; an unused worktree retains eligible data until its
next invocation. Deleting a worktree can remove its logs. Stable shared locks,
installed developer tools/libraries, unrelated shared caches, tracked research,
Codex history and OS crash reports are outside routine cleanup.

## Development and CI verification

Routine invocation runs the baseline and chosen heavy portion only. It does
not run infrastructure self-tests or regenerate the real README/site, including
on a branch with no earlier result. CI runs the retained runner and dedicated
progress regression suites on every PR without path conditions:

```sh
Rscript tools/test-review-ready-runner.R
Rscript tools/test-review-ready-progress.R
```

The command/process boundary is the approved test seam: tiny disposable
repositories/packages establish pinned inputs, scopes, admission, failure,
cancellation and retention. Progress fixtures exercise real workers, passing
and failing packages, and startup/exit/teardown diagnostics. Fixture results
are separate from real package-check and rollout evidence. Workflow contracts,
CI artifact checks and the release matrix retain their independent roles.
