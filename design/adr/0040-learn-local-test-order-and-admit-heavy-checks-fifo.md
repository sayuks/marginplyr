---
status: proposed
---

# Keep test priorities versioned and admit heavy checks FIFO

The original task requested measured heavy-first test dispatch and FIFO local
heavy admission. Keep the tracked start-first setting as the persistent order
source and retain the existing heavy lock, pinned source and terminal-result
authority. The [specification](../specs/review-ready-adaptive-order-fifo.md)
separates the reopened ordering workflow from FIFO requirements.

A dedicated timing CI pipeline is not an approved requirement. The maintainer
clarified that asking about CI was exploratory, and reopened the prior approval.
The earlier statement that CI-centralized measurement had been chosen no longer
carries authority. The CI measurement/update issues are `needs-info`; their
previous bodies are context rather than approved implementation scope.

The smaller recommendation is to measure within an already-requested full
Review-ready check and propose worthwhile changes to the existing tracked
priority, without a shared timing database or rolling local history. This
replacement workflow awaits confirmation. The three-run local calibration
remains valid evidence, independent of where production measurement is placed.

FIFO remains independent: use minimal live coordination with the established
heavy lock, preserve bounded waiting from #1135, and retain ADR 0039's process
ownership and crash limits. Its adoption does not require a timing CI job.
Implementation, reconciliation with
[ADR 0039](0039-run-review-ready-from-the-target-worktree.md) and actual rollout
remain pending.
