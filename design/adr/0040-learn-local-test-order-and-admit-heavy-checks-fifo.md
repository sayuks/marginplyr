---
status: accepted
---

# Keep test priorities versioned and admit heavy checks FIFO

## Registration amendment, 2026-11-05

Accept [registration before measurement](../specs/review-ready-preregistration.md)
to avoid a metadata-only commit and source check when measurement finds no
useful reordering. Append new files before full and apply the existing
reordering thresholds to all registered files. This replaces the rejection of
pre-measurement registration and the separate measured new-file insertion
rule below. The trade-off is that smaller new-file ordering improvements may
be left unapplied. Registration does not claim successful execution.

The runner and CI share complete-registration validation. The linked amendment
owns registration and reordering; the following retains measurement and FIFO
boundaries.

## Implemented measurement and FIFO design

The original task requested measured heavy-first test dispatch and FIFO local
heavy admission. Keep the tracked start-first setting as the persistent order
source and retain the existing heavy lock, pinned source and terminal-result
authority. The [specification](../specs/review-ready-adaptive-order-fifo.md)
records the agreed ordinary-dispatch workflow and independent FIFO requirements.
Local worker counts and matching proposal models follow
[ADR 0039](0039-run-review-ready-from-the-target-worktree.md).
The maintainer accepted the replacement specification on 2026-11-03, including
the private timing hook after comparing public-only alternatives, and confirmed
the R CMD check verification obligation.

A dedicated timing CI pipeline is not an approved requirement. The maintainer
clarified that asking about CI was exploratory, and reopened the prior approval.
The earlier statement that CI-centralized measurement had been chosen no longer
carries authority. Existing issues #1155 and #1158 now cover local timing and
Git priority proposals; #1156 and #1157 retain independent FIFO implementation
and adoption. Their previous canonical-CI requirements are superseded.

The agreed replacement boundaries use measurements from one already-requested,
successful complete full Review-ready check, without a shared timing database
or rolling local history. Adopt an eligible proposal in the current task,
including a follow-up commit in the same pull request; the new revision still
needs its applicable verification. Keep ordinary `test_check()` and register
new files at the end before measurement. The
separate private worker-timing hook is an accepted maintenance dependency,
isolated in one common helper from test dispatch. Declare supported testthat
versions and guard the function shape before recording. Supporting a new
version requires actual-worker measurement fixtures that prove file identity
and inclusion of file cleanup. The calibrated 3.3.2 target is `queue_task()`;
individual test files need no manual timing statements.

The implemented recorder must receive its applicable source-tarball full check
with recording enabled and diagnostics inspected under the existing NOTE policy.
The experimental zero-NOTE results do not certify the implementation or make
the private hook a stable public interface.

Changing an already registered file under the same name does not make it
unregistered. All files are registered before measurement.
Optional reorderings cross only differences greater than both
50% and ten seconds, and are adopted only for at least ten seconds of modeled
whole-suite improvement over the current committed order. Adopt the first eligible change
even when implementation is otherwise complete; validation of that adoption
does not itself authorize a second priority edit/check cycle. The runner guide's
*Priority proposals and explicit adoption* owns inspection and reporting steps.
Require median incremental hook, recording and proposal cost below one second
on the maintainer Mac at the reference scale of 196 files and four workers,
measured during implementation acceptance rather than every invocation.
Recalculate a stale priority-only proposal against the current list using
eligible evidence; defer proposals when measured test inputs or dependency
environment changed, or evidence expired. An unsupported timing hook disables
recording and proposals with a diagnostic while ordinary tracked-priority
execution continues. Recording or calculation errors disable proposals while
preserving the complete check's actual outcome. Genuine check and FIFO ownership failures
remain failures, and no dispatched suite is retried.

Validate tracked priority metadata before local checks and in CI: concurrent
valid registrations can merge into a duplicate list. Current and proposed
lists require unique literal stems. An archived measurement may contain
repeated literal stems; interpret those by first occurrence, as native
testthat does, while preserving raw evidence and all eligibility checks.
The runner guide's *Duplicate priority recovery* owns the agent's repair
workflow; local checks owns verification scope for the repaired SHA.

The three-run local calibration measured the selected native dispatch and
supports priority proposals, not an observed speedup under reordered execution.
The replacement decision is accepted; implementation and operational evidence
remain separate obligations.

FIFO remains independent: use minimal live coordination with the established
heavy lock, preserve bounded waiting from #1135, and retain ADR 0039's process
ownership and crash limits. Its adoption does not require a timing CI job.
The runner implements FIFO under #1156; [ADR 0039](0039-run-review-ready-from-the-target-worktree.md)
and the runner guide carry its ownership and operational contract.
The independent Mac adoption under #1157 completed on 2026-11-03 for
`604f1aebfeb954234e8c672d773c5906670621ec`, with
[drain, process and real full evidence](../../investigation/review-ready-fifo-adoption-2026-11-03.md).
That adoption preserves the same stable heavy-lock target, live-only FIFO
coordination, seven-day run retention and accepted abrupt-termination limits;
it requires no timing deployment or priority update.
