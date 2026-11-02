---
status: accepted
---

# Learn test order in CI and admit heavy checks FIFO

Static priorities age as test files are added, while one OS lock provides
exclusion without first-accepted-first-served admission. The maintainer chose
CI-centralized timing and Git-managed priorities, alongside FIFO local heavy
admission after mandatory baseline checks. The
[specification](../specs/review-ready-adaptive-order-fifo.md) owns the proposed
measurement eligibility, noise margins, Git workflow and admission requirements.

Keep the tracked start-first setting as the persistent order authority. One
canonical CI configuration publishes worker timings and a reviewable update
patch. Applying and merging that patch remain ordinary Git actions; local
Review-ready and fresh clones consume only committed configuration. This trades
immediate cross-worktree learning for portability and visible changes without a
local timing history. Conservative promotions reduce timing-noise churn.

Use minimal live FIFO records with the existing heavy advisory lock, preserving
bounded waiting from #1135. This is a limited exception to the accepted runner's
exclusion of a custom queue manager, without a service or general recovery
platform. Retain pinned source, terminal-result authority and ADR 0039's crash
limits.

## Decision history

The maintainer approved the initial amendment on 2026-11-02, including FIFO,
then rejected local timing dependencies. The shared cache and rolling-median
proposals are withdrawn. The subsequent local-measurement patch proposal is
replaced by canonical CI measurement. The maintainer approved the four-ticket
breakdown, published as #1155, #1158, #1156 and #1157 with native blocking
relationships. The user-requested
[local calibration](../../investigation/review-ready-order-calibration-2026-11-02.md)
supported conservative initial margins while exposing negligible follow-up
changes; the specification requires visible modeled benefit and deliberate Git
integration. Implementation, reconciliation with
[ADR 0039](0039-run-review-ready-from-the-target-worktree.md) and adoption remain
pending.
