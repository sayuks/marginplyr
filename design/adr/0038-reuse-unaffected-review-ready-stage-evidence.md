---
status: superseded
---

# Reuse unaffected Review-ready stage evidence

Superseded on 2026-11-02 by the consolidated
[#1114](https://github.com/sayuks/marginplyr/issues/1114) simplification and
[ADR 0039](0039-run-review-ready-from-the-target-worktree.md).

## Historical decision

For [#1110](https://github.com/sayuks/marginplyr/issues/1110), this ADR retained
the cumulative final-tree comparison against the unique merge-base with local
`main` while permitting compatible ancestor evidence to discharge unaffected
stage obligations. [PR #1115](https://github.com/sayuks/marginplyr/pull/1115)
implemented registered source/environment identities, authenticated observations
and evidence, failure invalidation, and cross-clone reuse. Moving the comparison
base to the last successful commit was rejected because it lost cumulative
obligations.

The [original decision](https://github.com/sayuks/marginplyr/blob/1a9dd00a/design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md)
and [historical specification](../specs/review-ready-verification-followups.md)
preserve that history. They do not impose current runtime requirements.

## Superseding decision

Each invocation now runs all five mandatory source/environment checks at its
pinned committed SHA. The AI supplies `source` or `full` and a reviewable
reason. The new runner does not classify paths, reuse stages or authenticate
evidence. Its per-worktree artifacts use one seven-day lifetime from UTC start.
[Local checks](../agents/local-checks.md) owns scope and outcome reporting;
[the runner guide](../../tools/review-ready-runner.md) owns execution and storage.
The installed generation was retired in the drained Mac cutover on
2026-11-02, recorded in the [retirement guide](../../tools/review-ready-admission.md).
That operational change does not establish a package-check result.
