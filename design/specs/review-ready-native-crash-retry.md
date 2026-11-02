# Recover a local Review-ready check after a native rendering crash

Status: historical; superseded by the consolidated 2026-11-02
[#1114](https://github.com/sayuks/marginplyr/issues/1114) specification in
[repository-owned Review-ready runner](review-ready-repository-runner.md).
[ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md) records
the amended decision. [Local checks](../agents/local-checks.md) owns the active
entry and rollout state.

The maintainer accepted a single local macOS retry for a narrowly recognized
Quarto/Deno rendering crash in [#712](https://github.com/sayuks/marginplyr/issues/712)
on 2026-09-28. It preserved first-attempt diagnostics and distinguished a retry
pass from an ordinary pass. The investigation did not establish a root-cause
fix. The simplified runner now ends a failed invocation once and retains its
ordinary logs; it has no crash recognizer or retry outcome.

The [original specification](https://github.com/sayuks/marginplyr/blob/1a9dd00a/design/specs/review-ready-native-crash-retry.md)
preserves the acceptance cases and operating assumptions of that generation.
It is history, not a current implementation or rollout requirement.
