# Repository-owned Review-ready runner

[Local checks](../design/agents/local-checks.md) owns scope judgment and agent
workflow. The [specification](../design/specs/review-ready-repository-runner.md)
and [ADR 0039](../design/adr/0039-run-review-ready-from-the-target-worktree.md)
own the accepted design. The repository entry below became active in the
[drained Mac cutover](review-ready-admission.md) on 2026-11-02. Cutover alone is
not a check result; [PR #1124](https://github.com/sayuks/marginplyr/pull/1124)
records per-revision `source` and `full` results.

The supported operational target is the maintainer's Mac. Its shared local
heavy slot coordinates that user's clones and worktrees; macOS CI validates
the runner and progress process contracts. Package portability remains covered
by the package OS checks and release matrix.

## Setup and invocation

Install `DESCRIPTION`'s `Config/Needs/review` development tools and make `jarl`
and `Rscript` available on PATH. These tools are separate from package runtime
Imports. The checked-out repository supplies the runner; adopting another
revision through Git updates it without installing a separate launcher.
If the checkout lacks the `--scope`/`--reason` interface, report that the checkout
must be updated before Review-ready can run. The retired installed command has
no reinstall or fallback path; old worktrees are outside migration scope.

Full scope also needs a working TeX distribution with `pdflatex` on PATH for
the PDF reference manual. [TinyTeX](https://yihui.org/tinytex/) is one option;
the CI process fixtures install its full bundle. Source scope does not need TeX.

After the six checks, full scope prepares the running DuckDB engine's JSON
extension before requesting the heavy slot. It reuses the matching user cache
offline, or obtains the signed binary from `https://extensions.duckdb.org`.
Preparation failure ends the invocation before admission; JSON coverage is not
silently skipped. The parent retains a cache lease until its children finish
and passes the absolute extension path to build/check. Source scope does not
prepare JSON.

Prepare the same cache separately when desired:

```sh
Rscript tools/setup-duckdb-json.R
```

The cache is under `tools::R_user_dir("marginplyr", "cache")/duckdb-json`, keyed
by native DuckDB version and platform. Older entries remain until explicitly
removed with `Rscript tools/setup-duckdb-json.R --remove VERSION PLATFORM`;
removal refuses an entry in use. This developer cache is outside run-artifact
retention. `MARGINPLYR_DUCKDB_JSON_EXTENSION` selects an existing binary instead;
an invalid explicit path fails without repair or substitution.

Ordinary `devtools::test()` and `testthat::test_local()` prepare at the start of
a selected JSON test file, before its cases. Other files may already have run.
A focused selection excluding JSON files does not acquire JSON, and
`pkgload::load_all()` alone does not prepare it. JSON cases only load the
prepared extension. CRAN and explicit minimal/other-backend verification retain
optional skips; a local required setup failure is an error. The
[accepted design](../design/specs/local-duckdb-json-tests.md) owns these
boundaries.

`MARGINPLYR_REQUIRED_DUCKDB_JSON=true` declares required JSON coverage. Explicit
`false` permits optional JSON only when DuckDB is not already required through
`MARGINPLYR_REQUIRED_SUGGESTS`; an existing promise cannot be weakened by that
setting. Ordinary source tests default to required JSON, while installed checks
retain optional behavior unless their entry declares it required. A missing or
too-old DuckDB R package fails required preparation; setup does not install R
packages.

From a clean committed worktree root:

```sh
Rscript tools/review-ready.R --scope source --reason 'Explain the heavy-check omission.'
Rscript tools/review-ready.R --scope full --reason 'Explain why package build/check is needed.'
```

Both forms run repository contract consistency and the six mandatory checks in
[local checks](../design/agents/local-checks.md#boundary-and-scope) before any shared-slot request.
The documentation check runs `roxygen2::roxygenise()` in the pinned source
copy and refuses any filename/content difference from the preserved
`documentation-before/` copy, including changes outside `man/` and `NAMESPACE`.
Generation errors or differences stop before spelling, linters and admission;
the submitting worktree stays unchanged. Inspect the reported diff, regenerate
and commit corrections explicitly before a fresh invocation. `roxygen2` is a
required review tool for both scopes; README/site rendering remains separate.
First, submitted priority metadata must contain unique literal filename stems
matching every discovered test exactly once. Missing or obsolete entries fail
this same preflight; helpers are outside native test discovery.
Invalid metadata fails before those checks and admission, reporting the
submitted SHA, duplicate stems and positions. The runner preserves committed
source; repairs use the recovery procedure below.
`source` ends with an explicit heavy-check omission. After those checks pass,
`full` atomically registers under a shared metadata lock. Acceptance sequence,
not launch time, orders live waiters across clones and worktrees. Only the
oldest live waiter may acquire the existing heavy lock; it starts automatically
through the same command. The runner reports its sequence, changes in earlier
live-waiter count, and actual elapsed wait at admission, without an ETA.

Metadata access uses short interruptible blocking acquisitions. Followers poll
live coordination, while the oldest waiter makes one interruptible capacity
acquisition outside the metadata lock. OS process identities include creation
time; only an ended identity is probed, and pruning still requires positively
acquiring its invocation guard. This avoids repeated failed nonblocking lock
acquisition and keeps descriptors bounded. Suspended live owners retain their
place. Lock, state and ambiguous-ownership errors fail visibly.
Keep the same execution handle through checking, cleanup and terminal output.
The result always names the submitted SHA, scope and reason. `result.rds` also
records `repository_contracts` as `not-run`, `failed` or `passed`, including
normal failures before admission. The console names the stage and mismatch.
The stage reads only the pinned archive and shares its static R comparison
with `Rscript tools/verify-ci-artifacts.R`. It compares workflow contracts and
the independently maintained `Config/Needs/review` package sequence, including
order and duplicates. Malformed declarations fail; valid version constraints
do not add a version-pin policy. The dependency-only comparison is also shared
with `Rscript .github/scripts/verify-cran-preflight.R` and uses base R, preserving
the release-contract job's environment. Workflow parsing uses the already-declared
`yaml` and `jsonlite` review tools, adding no R prerequisite. The routine stage
invokes neither actionlint, the complete preflight nor package work. CI retains
actionlint syntax and verifier-wiring checks.

The lock at `~/.local/state/marginplyr/review-ready-heavy.lock` is shared by
this user's marginplyr clones/worktrees on the Mac. The account home is resolved
independently of caller HOME/cache overrides. Its stable target is an operating primitive, not an expiring run artifact.
Keep it in place while owners or waiters may exist. Its sibling
`review-ready-fifo/` holds a stable metadata lock, the acceptance counter and
live invocation guards/records. Neither lock targets nor sequencing expire with
run retention. No timing history or completed-result ledger is stored there.
Focused tests have the separate per-worktree allowance in local checks; eight
package workers are not a user-wide process cap.

## Cancellation

Cancel the original invocation through its retained execution handle. An
ordinary cancellation stops and collects its owned child work before releasing
the heavy slot. A cancelled waiter unregisters and must never start later. An
abruptly ended waiter's OS guard permits successors to prune it; an interrupted
registration's unreferenced guard is reclaimed by the same ownership check.
Retrying creates a new identity and sequence. Retain available logs and collect
the terminal cancellation result before editing and resubmitting.

Abrupt R termination uses the existing process library's cleanup facilities.
It may release the lock before every descendant stops; strict crash-time
exclusion is outside this contract. The
[adoption evidence](../investigation/review-ready-fifo-adoption-2026-11-03.md)
records actual Mac cleanup observations and their limits. A free lock or absent
process is not a successful check result. There is no recovery command, watchdog, durable
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

## File timing evidence

Full scope records worker file intervals in the run's `timing/` directory.
`manifest.rds` names the pinned SHA, discovered files, archived test input bytes
and hashes, dependency versions and environment. PID-owned logs pair wall and
worker elapsed clocks at file entry and exit; `*.worker.rds` identifies each
recorder environment. `file-results.tsv` records skip and other expectation
counts, once per file, from the ordinary buffered reporter. Reporter replay
is not a duration clock.

`evidence.rds` validates those observations after the invocation's terminal
result. Eligibility requires the same successful full SHA in `result.rds`,
exactly one valid pair for every discovered file, and the native eight-worker
announcement. Each file explicitly marks all-skipped and empty results; neither
supplies promotion evidence. Failed, cancelled, incomplete or malformed data
cannot authorize a priority update. Source scope creates no timing evidence.
All timing artifacts share the seven-day policy above; later execution uses
only the committed priority.

The common recorder supports **testthat 3.3.2** only. It checks the complete
`queue_task(path, shuffle = FALSE)` body and signature before tracing entry and
exit. This private boundary includes file cleanup while excluding worker
helper/setup and final teardown. A new version requires the real-worker
fixtures below before adding support. Unsupported instrumentation or recording
and output errors visibly disable timing eligibility, preserving the complete
check's actual result without retrying dispatched tests. Individual test files
carry no recording statements; ordinary `test_check()` owns execution.

## Priority proposals and explicit adoption

Complete registration before invoking either scope: append new files in native
discovery order and remove deleted names. A rename removes the old name and
appends the new one. `Rscript tools/verify-test-priority.R` runs the shared
validation used by Review-ready and CI, without executing package tests.

After timing finalization, a successful eligible full invocation writes one
`priority-*/` proposal directory under that run. It contains `explanation.md`,
`proposal.rds` and, only when membership/order changes, `priority.patch` for
`DESCRIPTION`. Generation never edits the pinned source, tarball or installed
metadata. The explanation identifies the measured SHA, archived input hashes,
file eligibility, current and proposed positions and eight-worker greedy
schedule estimates. These models are not observed speedups or guarantees.
Source scope and failed/incomplete/unsupported timing supply no proposal.
Earlier four-worker evidence is ineligible for the eight-worker model.
Proposal errors report the reason and preserve the actual check result and
ordinary NOTE handling.

An archived measurement's repeated literal registrations are read by first
occurrence, matching native testthat dispatch. The diagnostic, explanation and
proposal RDS record that normalization. Archived bytes and hashes stay intact;
every ordinary evidence eligibility check still applies. Current and proposed
metadata remain strict, and empty entries or glob registrations are refused
even in historical metadata.

Every discovered file, including an all-skipped file, must already be registered.
Proposals change order only. Stable insertion crosses adjacent eligible
observations only when the later file is both more than 50% and more than ten
seconds slower, and is selected only for at least ten seconds of modeled
whole-suite improvement over the current committed order. These thresholds
apply equally to newly registered files. The report also compares the selected
order with the measured invocation's native dispatch.

After the task's first successful full result, inspect `explanation.md` and
`proposal.rds` before review or reporting completion. Account for the measured
durations, current order, optional reordering and all
four schedule estimates (`native`, `current`, `candidate`, `selected`).
Proposal generation alone does not complete this step.

Apply an eligible changed proposal in the current task, including a follow-up
commit in the same PR when implementation is otherwise complete.
An unchanged priority completes adoption without an edit or another source check. Immediately before
application, recalculate against the target worktree's current priority:

```sh
Rscript tools/propose-test-priority.R --run /absolute/path/to/.review-ready/runs/RUN
git apply --check /absolute/path/to/RUN/priority-NEW/priority.patch
git apply /absolute/path/to/RUN/priority-NEW/priority.patch
```

Use the newly printed directory, review its explanation and diff, then commit
the priority change. An unchanged priority has no patch. Do not
blindly concatenate concurrent whole-list patches: follow the
[integration workflow](../design/agents/integration.md), then recalculate. A
priority-only change since measurement permits recalculation. Changed test or
helper bytes/membership, DESCRIPTION beyond priority, a changed dependency/R
environment, or missing/expired evidence defers adoption until the next
ordinarily required successful full measurement. Recalculation never requests
another suite execution or renews the run's seven-day deadline.

The priority commit receives its own applicable verification for its own SHA.
Choose its scope under [Local checks](../design/agents/local-checks.md#boundary-and-scope),
including the measured-priority-only `source` case. The measurement pass
certifies only the measured SHA. Inspect and record any
proposal from that validation, then complete this task without another priority
edit or check solely for that proposal. A new implementation change or explicit
user request can start a later adoption; a validation proposal alone cannot.
Record the measured SHA, proposal directory, current order,
optional-reordering decision, schedule estimates and modeled gains in the PR's
verification record, with the adopted commit or a concrete reason for deferral
or no change. Summarize the disposition in the completion response. Distinguish
modeled scheduling gains from observed runtimes. If timing or a proposal is
unavailable, record its diagnostic and complete the actual check outcome.
Only membership/order is committed, without durations, timestamps or history.
A fresh clone uses that setting through ordinary `test_check()` without any
measurement artifacts. FIFO admission is independent of proposal adoption.

## Duplicate priority recovery

An implementing agent repairs a confirmed literal duplicate in its submitted
priority within the active task, including one inherited from main. A separate
issue alone does not complete this repair. Use an explicit Git edit that
retains the first occurrence; validate strict parsing and exact native ordered
discovery before/after, including every discovered file once. Use disposable
copies for investigation while an invocation is active.

| Detection point | Action before review readiness |
| --- | --- |
| Before invocation or in failed preflight | Collect the failed terminal result, repair and commit, then check the new clean SHA at the scope its implementation requires. An incomplete full run supplies no successful full evidence. |
| While waiting or checking | Follow *Required fix confirmed* in local checks: cancel through the retained original handle and collect terminal completion and owned children before editing, committing or resubmitting. |
| After a successful full invocation | Collect and preserve that terminal success for its SHA, record proposal failure separately, repair and commit metadata, and revalidate the repaired SHA under local checks. Full success does not certify the later repair. |
| Only in archived measurement metadata | With valid target metadata, recalculate the proposal through the historical reader, inspect its disposition and report normalization. Historical duplication alone needs no source edit or suite. |
| In a proposed patch or integration result | Reject the invalid state, repair the target and recalculate using still-eligible evidence and the assessed current main. Validate the resulting SHA before review readiness. |

If an invocation finishes before planned cancellation, collect its actual
terminal result and use the completed case. Preserve failures as failures.
The metadata-only `source` allowance is defined by
[local checks](../design/agents/local-checks.md#boundary-and-scope).

If recovery is unavailable or evidence expired or changed, repair the tracked
duplicate and record why optimization is deferred to the next ordinarily
required full check. Keep the original retention deadline and evidence checks;
do not edit manifests or run a suite solely to replace timings. Verification
required by the actual repair remains necessary.

Glob registrations, empty entries, ambiguous intent, or a changed native
ordered filename vector fall outside mechanical duplicate removal. Establish
intended dispatch from the specification and obtain a maintainer decision only
if it remains unresolved. Report the concrete blocker while retaining the
implementation and valid evidence. Tracked metadata must be valid before
claiming final readiness. A validation proposal alone starts no second
optimization cycle; a new duplicate is an unresolved defect, so investigate
its recurrence rather than repeatedly applying the same patch.

## Development and CI verification

Routine invocation runs the baseline and chosen heavy portion only. It does
not run infrastructure self-tests or regenerate the real README/site, including
on a branch with no earlier result. CI runs the retained runner and dedicated
progress regression suites on macOS for every PR without path conditions:

```sh
Rscript tools/test-review-ready-runner.R
Rscript tools/test-review-ready-progress.R
Rscript tools/test-review-ready-priority.R
```

The command/process boundary is the approved test seam: tiny disposable
repositories/packages establish pinned inputs, scopes, admission, failure,
cancellation and retention. Progress fixtures exercise real workers, passing
and failing packages, and startup/exit/teardown diagnostics. Fixture results
are separate from real package-check and rollout evidence. Workflow contracts,
CI artifact syntax/wiring checks and the release matrix retain their independent
roles. Static workflow and review dependency comparison is mandatory in both
local scopes; it is distinct from the excluded infrastructure suites. Changes to these tools
require their relevant command/process fixtures as development verification.
Scope follows cumulative package effects under local checks, including its
package-unaffected infrastructure case and exact-SHA heavy omissions.

## FIFO adoption

The FIFO implementation follows [ADR 0040](../design/adr/0040-learn-local-test-order-and-admit-heavy-checks-fifo.md)
and its [accepted specification](../design/specs/review-ready-adaptive-order-fifo.md#fifo-heavy-admission).
The maintainer-Mac adoption completed on 2026-11-03 for
`604f1aebfeb954234e8c672d773c5906670621ec`; the
[evidence record](../investigation/review-ready-fifo-adoption-2026-11-03.md)
separates real-account drain and the successful full result from isolated
cross-clone/worktree process fixtures. No pre-FIFO owner or waiter remained
at the observed boundary. Older nonparticipating runners still preserve
heavy exclusion but can overtake participating requests; FIFO covers upgraded
participants. Old worktrees and their sessions are not migrated.
