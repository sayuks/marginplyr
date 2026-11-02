# Coordinate local review-ready checks across worktrees

Status: historical; superseded by the consolidated 2026-11-02
[#1114](https://github.com/sayuks/marginplyr/issues/1114) specification in
[repository-owned Review-ready runner](review-ready-repository-runner.md).
[ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md) records
the amended decision. [Local checks](../agents/local-checks.md) owns the active
entry and rollout state.

The original shared-slot safeguard addressed concurrent four-worker checks in
[#1065](https://github.com/sayuks/marginplyr/issues/1065).
[#1108](https://github.com/sayuks/marginplyr/issues/1108) subsequently added
independently supervised Documentation/Repository paths. The simplified design
retains one heavy slot across this user's clones/worktrees, but runs the five
mandatory checks before waiting and lets the AI choose source or full with a
reason. Automatic classification, independent supervision protocols and strict
recovery are retired.

The [original specification](https://github.com/sayuks/marginplyr/blob/1a9dd00a/design/specs/review-ready-resource-admission.md)
preserves the acceptance cases and operating assumptions of that generation.
It is history, not a current implementation or rollout requirement.
