---
status: accepted
---

# Keep test priorities versioned and admit heavy checks FIFO

The original task requested measured heavy-first test dispatch and FIFO local
heavy admission. Keep the tracked start-first setting as the persistent order
source and retain the existing heavy lock, pinned source and terminal-result
authority. The [specification](../specs/review-ready-adaptive-order-fifo.md)
records the agreed ordinary-dispatch workflow and independent FIFO requirements.
The maintainer accepted the replacement specification on 2026-11-03, including
the private timing hook after comparing public-only alternatives, and confirmed
the R CMD check verification obligation.

A dedicated timing CI pipeline is not an approved requirement. The maintainer
clarified that asking about CI was exploratory, and reopened the prior approval.
The earlier statement that CI-centralized measurement had been chosen no longer
carries authority. The CI measurement/update issues are `needs-info`; their
previous bodies are context rather than approved implementation scope.

The agreed replacement boundaries use measurements from one already-requested,
successful complete full Review-ready check, without a shared timing database
or rolling local history. Proposals may join the next ordinary code-change
commit in the same pull request; the new revision still needs its applicable
verification. Keep ordinary `test_check()`, accept native append-last ordering
for unregistered files until a measured priority update is committed, and use
that tracked priority for later checks. First-run unregistered priority does
not justify a private execution adapter or provisional registration. The
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
unregistered. Every eligible newly measured file, including lightweight files,
belongs in the next registration proposal.
Optional existing-file reorderings cross only differences greater than both
50% and ten seconds, and are adopted only for at least ten seconds of modeled
whole-suite improvement over registration alone. Without a remaining ordinary
code change, defer adoption instead of triggering a priority-only recheck loop.
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

The three-run local calibration measured the selected native dispatch and
supports priority proposals, not an observed speedup under reordered execution.
The replacement decision is accepted; implementation and operational evidence
remain separate obligations.

FIFO remains independent: use minimal live coordination with the established
heavy lock, preserve bounded waiting from #1135, and retain ADR 0039's process
ownership and crash limits. Its adoption does not require a timing CI job.
Implementation, reconciliation with
[ADR 0039](0039-run-review-ready-from-the-target-worktree.md) and actual rollout
remain pending.
