# Verification-only Review-ready follow-ups

Status: historical; superseded by the consolidated 2026-11-02
[#1114](https://github.com/sayuks/marginplyr/issues/1114) specification in
[repository-owned Review-ready runner](review-ready-repository-runner.md).
[ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md) records
the amended decision. [Local checks](../agents/local-checks.md) owns the active
entry and rollout state.

The maintainer accepted stage reuse for
[#1110](https://github.com/sayuks/marginplyr/issues/1110) on 2026-10-31, and
[PR #1115](https://github.com/sayuks/marginplyr/pull/1115) implemented it.
The original contract compared cumulative changes with a pinned merge-base and
used authenticated ancestor execution evidence with complete input/environment
identities and later-failure invalidation. The simplified runner executes its
five-check baseline every time, uses AI scope judgment, and does not reuse
stages or maintain authenticated evidence.

The [original specification](https://github.com/sayuks/marginplyr/blob/1a9dd00a/design/specs/review-ready-verification-followups.md)
preserves the acceptance cases and operating assumptions of that generation.
It is history, not a current implementation or rollout requirement.
