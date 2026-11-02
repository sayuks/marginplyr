# Repository-owned Review-ready runner

Status: accepted consolidated target from
[#1114](https://github.com/sayuks/marginplyr/issues/1114), 2026-11-02.
[ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md) records
the amended decision. Implementation and real Mac rollout acceptance are
separate: [local checks](../agents/local-checks.md#rollout-state) records the
active entry after the drained Mac cutover on 2026-11-02.
[PR #1124](https://github.com/sayuks/marginplyr/pull/1124) records per-revision
real `source` and `full` results.
The entry spelling is fixed as
`Rscript tools/review-ready.R --scope source|full --reason TEXT`.

## Problem Statement

Running several marginplyr worktrees concurrently can start several four-worker test suites on the same Mac and make it unresponsive. Review-ready has also accumulated routing, evidence reuse, infrastructure self-tests, recovery, and delivery machinery whose complexity is disproportionate to the original coordination problem. Infrastructure self-tests can take longer than the package tests they precede.

The maintainer wants a small, understandable local workflow: preserve useful checks and progress reporting, serialize the expensive package check, and avoid waiting for that slot merely to discover a lint error. Light documentation changes must still receive the five mandatory source/environment checks. The AI should decide whether the expensive local package check is necessary by reasoning about the change, rather than introducing another automatic classifier.

The installed legacy CLI and instructions directing agents to use it must eventually be retired together. Local check by-products must have one simple retention policy instead of accumulating indefinitely.

## Solution

Use one repository-owned R entry with two explicit execution forms. Both run the same five mandatory checks against a pinned committed revision. The AI decides and records whether local package build/check is needed. A five-check-only invocation ends after those checks without requesting the shared slot. A heavy invocation requests the slot only after those checks pass, then automatically starts package build and the complete source-tarball check when admitted.

One heavy Review-ready check may run at a time across this user's marginplyr clones and worktrees on the Mac; its testthat suite uses four workers. Keep ordinary polling of the same execution handle, existing progress reporting, the existing investigation loop, and the existing bounded additional focused-test policy. Remove automatic crash retries, routine local infrastructure self-tests, automatic AI stop/resume messages, stage reuse, and custom coordination services. Retained infrastructure tests run in CI on every relevant PR.

Keep all runner-managed artifacts for an invocation together in one ignored directory inside its submitting worktree, outside Git's administrative directory. Seven days after the recorded UTC start time, that directory becomes eligible for deletion on the next invocation in the same worktree. Never delete active data.

## User Stories

1. As a maintainer, I want one repository-owned R entry, so that the checked-out code supplies the check implementation without synchronizing an installed launcher.
2. As a maintainer using several worktrees, I want expensive Review-ready checks serialized across them, so that their four-worker suites do not all overload my Mac at once.
3. As a maintainer using separate clones, I want the same shared slot to apply across clones, so that another clone cannot accidentally bypass the load limit.
4. As a developer, I want four testthat workers within the admitted package check, so that a single check retains its existing useful parallelism.
5. As an agent, I want checks to use the submitted committed revision even after waiting, so that I can identify exactly what the result establishes.
6. As a reviewer, I want the result to name the submitted SHA, so that later edits cannot be presented as already checked.
7. As an agent making a light documentation change, I want all five mandatory checks to run, so that omitting the expensive package check does not omit basic verification.
8. As a maintainer, I want the AI to judge whether local package build/check is necessary from the diff and its effects, so that another path-based classifier is unnecessary.
9. As a reviewer, I want the reason for omitting local package build/check recorded, so that the AI's judgment is reviewable.
10. As an agent, I want an explicit five-check-only execution form, so that a justified omission does not require waiting for the heavy slot.
11. As a reviewer, I want a five-check-only result distinguished from a full package-check pass, so that the result cannot overstate what ran.
12. As a developer, I want test dependency declarations checked mechanically, so that this validation does not depend on AI recollection.
13. As a developer, I want required optional test dependencies and their versions checked mechanically, so that a green suite cannot merely reflect avoidable skips.
14. As a developer, I want package spelling checked before joining the heavy-check wait, so that spelling failures are found promptly.
15. As a developer, I want jarl checked before joining the heavy-check wait, so that a lint failure does not waste a long admission wait.
16. As a developer, I want lintr to run with the package loaded before joining the heavy-check wait, so that internal symbols are resolved correctly and lint failures are found promptly.
17. As an agent, I want a failed mandatory check to end the invocation, so that build/check does not proceed after a known failure.
18. As a maintainer, I want fixes made and committed explicitly before a new invocation, so that the runner does not silently edit code or retry against an unclear revision.
19. As an agent waiting for capacity, I want the command to start automatically when the slot becomes available, so that no admission acknowledgment is needed.
20. As an agent, I want to retain and poll the same execution handle through terminal completion, so that I do not launch duplicate checks or infer success from partial output.
21. As a maintainer, I want no queued AI resumption messages, so that a stale message cannot restart a conversation after its work is already complete.
22. As an agent, I want the existing meaningful investigation loop retained while waiting or checking, so that I can use that time to examine the submitted change.
23. As an agent, I want the existing bounded focused-test allowance retained, so that I can test a concrete hypothesis without launching another full suite.
24. As a maintainer, I want the distinction between the shared heavy slot and extra focused tests documented, so that four workers is not mistaken for an account-wide process cap.
25. As an observer, I want existing elapsed-time, completed-file, percentage, and last-file progress reporting preserved, so that a long package check remains understandable.
26. As a developer, I want native worker startup and failure diagnostics visible, so that failures before or after reporter completion are not hidden.
27. As a reviewer, I want the terminal check result to determine the verdict, so that progress output is never mistaken for success.
28. As a maintainer, I want check failures to stop without an automatic native-crash retry, so that the runner avoids a special retry classification subsystem.
29. As an agent, I want ordinary cancellation to stop owned child work before releasing capacity, so that the next admitted check does not overlap normally cancelled work.
30. As an agent, I want cancellation during admission waiting to prevent later automatic start, so that a cancelled request stays cancelled.
31. As a maintainer, I want abrupt-termination cleanup to use existing libraries with explicit limits, so that strict crash recovery does not recreate a custom supervision platform.
32. As a developer making routine package changes, I want infrastructure regression suites excluded from the normal local command, so that those suites do not dominate every check.
33. As a maintainer, I want retained infrastructure tests executed in CI on every relevant PR, so that removing them from routine local orchestration does not leave them untested.
34. As a maintainer, I want actual CI invocations for retained progress and runner tests, so that registering a test is not mistaken for executing it.
35. As a maintainer, I want all runner-managed source copies, archives, check outputs, caches, logs, and results grouped per invocation, so that cleanup is understandable.
36. As a maintainer, I want output storage inside the worktree but outside Git's administrative directory, so that artifacts have an ordinary local location without entering Git or the source package.
37. As a maintainer, I want one seven-day lifetime for all artifacts and outcomes, so that separate success, failure, cache, and log retention rules are unnecessary.
38. As a maintainer, I want retention measured from one recorded UTC start time, so that log updates and missing finish records do not change the deadline.
39. As a maintainer, I want expired data deleted on a later invocation in the same worktree, so that no background cleanup service is required.
40. As a maintainer, I want cleanup restricted to the current worktree and inactive owned data, so that a check cannot sweep another worktree or delete live work.
41. As a maintainer, I want unrelated installed tools, libraries, caches, research files, and OS reports left alone, so that retention cleanup stays within its ownership boundary.
42. As a maintainer who is the only legacy CLI user, I want a one-time retirement and instruction update, so that permanent compatibility machinery is unnecessary.
43. As a maintainer, I want old worktrees and their existing AI sessions excluded from migration, so that adoption does not become a fleet-upgrade exercise.
44. As a maintainer, I want obsolete checker-owned state removed only after old work has stopped, so that retirement does not interrupt live work or keep unneeded history forever.
45. As an implementer, I want the old ADRs and operational guidance reconciled with this specification, so that future agents do not receive contradictory entry-point, reuse, or retention instructions.

## Implementation Decisions

1. **Entry and scope.** Keep a repository-tracked R command as the interface. Implement a small orchestration boundary around existing check, progress, process, and locking facilities. Prefer existing focused R libraries and base R over a new admission service, custom queue manager, or general recovery framework.
2. **Two explicit execution forms.** Support the five mandatory checks alone and the same checks followed by heavy package verification. The AI explicitly supplies the chosen scope and records its reasoning. Exact option spelling is an implementation detail. This is not a new automatically inferred classification system.
3. **AI decision boundary.** The AI examines the submitted diff and its effects to decide whether local package build/check is required. Package-affecting changes require it. Light documentation, prose, or research changes may omit it when their effects justify omission. Neither a directory name, extension, nor exclusion from the source package alone proves that omission is justified. The AI may omit only the heavy portion, never the mandatory five-check baseline.
4. **Pinned input.** Check one clean committed revision, pinned at invocation, through a disposable committed-source copy. All preflight and heavy checks use that revision. Waiting does not switch to a later HEAD, and an earlier result does not certify later changes. Do not introduce cross-run stage reuse or a branch's first-success requirement.
5. **Mandatory checks.** A successful invocation requires the mechanical test dependency-declaration scan, required optional test Suggests presence/version check, package spelling, jarl, and package-aware lintr. Load the package before lintr so namespace-dependent symbol resolution works. Preserve these existing checks rather than substituting AI judgment for their execution.
6. **Pre-admission failure.** Run the five checks before requesting the heavy slot. A failure ends that invocation without joining admission waiting or starting build/check. The agent corrects the issue, commits the correction, and starts a new invocation. The command does not edit code or silently retry failed checks.
7. **Five-check-only completion.** After the five checks succeed, report their outcome, submitted SHA, heavy-check omission, and the agent's reason, then exit without requesting the heavy slot. Do not report this as a full package-check pass. The same artifact and retention policy applies to this form.
8. **Heavy-check admission.** Use one stable OS-backed advisory lock for this user's marginplyr clones/worktrees on the Mac. After successful preflight, wait inside the command and start automatically when admitted. Do not add an agent-ready handshake. Keep the same invocation and execution identity through waiting, checking, cleanup, and terminal completion.
9. **Heavy-check contents.** Under that slot, build a source tarball and run the existing as-CRAN package-check contents, including examples, vignette rebuilding, the complete test suite, and snapshots once, with four testthat workers and without coverage measurement. Preserve the existing required-Suggests enforcement and local/offline check environment. Do not copy unrelated complexity merely because it exists in an older runner.
10. **Check outcomes.** ERROR, WARNING, nonzero process exit, and timeout fail the invocation. Print every NOTE; classifications outside the existing shared policy need a written disposition before review readiness. Retain the ordinary result and logs. Remove the native-rendering-crash recognizer, automatic retry, retry eligibility rules, and passed-after-retry outcome.
11. **Progress.** Preserve existing custom test progress: elapsed time, completed files versus total, percentage, last completed file, native worker-count announcement, and worker startup/exit/teardown diagnostics. Keep observing diagnostics through terminal completion, including failures before reporter startup or after reporter completion. Percentages describe file completion, not passing tests or an estimate of remaining time. Progress is observational; only the terminal result determines the verdict.
12. **AI observation.** Retain ordinary polling of the same execution handle until its terminal result. Do not infer completion from elapsed time, partial logs, or process absence, and do not silently replace a lost invocation. Avoid repeated idle narration. Do not implement AI stop/resume coordination or completion-message queues; zero-token waiting is not a guarantee of this design.
13. **Investigation policy retained.** Preserve the existing same-agent hypothesis, investigation, evidence, and disposition loop while waiting or checking. Investigate meaningful hypotheses; once they are exhausted, checkpoint and wait on the original invocation. Retaining the loop does not require endless activity or repeated waiting messages.
14. **Focused-test policy retained.** Each submitting worktree may have at most one additional focused-test or reproduction execution at a time, with testthat parallelism disabled. Select the target explicitly, inspect its helpers and owned child work, and use a separate execution handle. Do not reconstruct a full suite through successive selections. Stop and collect that work before the next focused execution and when handling the Review-ready terminal transition.
15. **Concurrency boundary.** The shared slot covers the heavy Review-ready package check, not all R processes. Additional permitted focused tests may overlap across worktrees; there is no aggregate account-wide cap for them. Five worktrees can add five nonparallel focused-test executions alongside the four-worker package check. Do not introduce another scheduler to change that explicitly retained policy.
16. **Ordinary cancellation.** Stop the invocation's owned children and confirm their exit before releasing heavy-check capacity. A cancelled waiter must not later start checking. Retain available logs and report cancellation through the original handle.
17. **Abrupt termination.** Use existing process-library cleanup facilities when the R process itself is terminated abruptly. The maintainer accepts that a subsequent check may begin before all descendants stop. Do not recreate strict crash-time exclusion, a custom watchdog, or a durable recovery ledger. Verify actual library behavior on the target Mac instead of claiming normal R cleanup always runs after forced termination.
18. **Infrastructure verification.** Exclude infrastructure regression suites and real site/README regeneration verification from the routine local orchestration. Do not force an initial infrastructure self-test solely because a branch has no prior result. Keep appropriate change-specific development verification and CI. Run retained infrastructure tests on every relevant PR without adding change-path conditions for this simplification, and explicitly wire every retained runner/progress regression suite into CI. Remove suites for deleted machinery rather than preserving their old coverage counts.
19. **Artifacts.** Group all substantial runner-owned data per invocation in a dedicated output directory at the submitting worktree root, outside Git's administrative directory. This includes committed-source copies and archives, built tarballs, check/install work, supported child temporary/cache outputs, progress data, logs, and results. Exclude this output root from both Git tracking and R source builds. Use supported child environment settings to keep owned temporary/cache data under the run directory without recreating an isolation capability registry.
20. **Retention.** Record one UTC start timestamp when creating the run directory. Seven days means seven consecutive 24-hour periods from that recorded timestamp for all outcomes and artifact types. Reading or updating files does not renew it; a missing finish record does not require a different clock. On a subsequent invocation, remove eligible inactive run directories as a unit, only in the current worktree. Use a small existing OS-backed liveness guard, skip active or safety-undetermined data, and report cleanup problems rather than deleting uncertain data.
21. **Retention limits and exclusions.** Seven days is deletion eligibility, not a maximum on-disk age: an unused worktree is not swept by invocations elsewhere. Do not promise to preserve logs after worktree deletion. Stable shared-lock targets are operating primitives, not retained run history; do not expire or recreate them while owners or waiters may exist. Installed developer packages/tools, unrelated shared caches, tracked research, Codex history, and OS-owned crash reports are outside routine cleanup scope.
22. **One-time retirement.** After existing old checks finish or are cancelled and their work stops, adopt the simplified R entry, uninstall the legacy CLI, remove obsolete delivery/coordination machinery, and update repository plus user-wide Codex/Claude instructions together. Clean up obsolete checker-owned state and diagnostics after shutdown. Old worktrees and already-running sessions are explicitly outside migration scope: do not inventory, update, or provide a compatibility launcher for them.
23. **Documentation reconciliation.** Amend or supersede conflicting operational guidance and ADR decisions as part of eventual implementation and rollout. Preserve the useful repository-owned-entry decision while removing requirements for stage reuse, authenticated evidence, strict recovery, and migration of old worktrees that conflict with this approved simplification. Retain decision history without retaining the retired runtime machinery.

## Testing Decisions

- **Primary seam:** reuse the already-discussed repository R command/process boundary. Observe its exit/result, streamed output, owned child behavior, lock contention, and retained files. Prefer this highest existing seam over adding test-only service APIs or separately testing each orchestration helper.
- **Prior art:** existing disposable-package progress fixtures and runner process-contract fixtures already exercise real subprocesses, passing/failing tests, worker startup/exit/teardown failures, and process/lock boundaries. Reuse the applicable parts and retire tests for removed protocols. Registration consistency is not evidence that a fixture actually executes.
- **Good tests establish external behavior:** use tiny disposable repositories/packages and isolated fixture state to prove the requested workflow. Do not snapshot internal function layout, reproduce the implementation in assertions, or require the production package suite for every fixture case.
- **Mandatory baseline and scope:** prove that a light documentation-only invocation still executes all five checks, reports the AI-supplied heavy-check omission and reason, and never requests the heavy slot. Prove that heavy requests execute the same baseline first. A failed preflight must return failure without waiting for capacity or starting build/check.
- **Pinned revision:** submit one committed revision, hold capacity elsewhere, make later changes, and prove that the admitted check and its result still identify and use the submitted revision. An omitted heavy check must never be represented as a full package-check pass.
- **Real shared exclusion:** launch invocations from distinct worktrees and separate clones, prove at most one heavy check is admitted, then prove a waiter starts automatically when the slot is released. Preserve real four-worker behavior within the admitted package check. Do not assert a machine-wide four-process cap that contradicts the retained focused-test allowance.
- **Check execution and failure:** exercise successful and failing package-check fixtures, including preflight errors, check warnings/errors, nonzero exits, and timeout reporting. Preserve ordinary NOTE handling. Prove a supported native rendering failure ends once without an automatic retry or a passed-after-retry result.
- **Progress:** exercise real passing and failing disposable packages, file-completion reporting, actual worker-count announcements, startup failure, unexpected exit, and teardown failure. Prove diagnostics remain visible before progress events and after reporter completion, and that progress completion cannot override a failing terminal result.
- **Cancellation:** exercise a waiting cancellation that never subsequently starts, and an ordinary running cancellation with owned child work that stops before the slot becomes available. Exercise the available abrupt-parent cleanup on the actual Mac and document its accepted limits rather than asserting strict crash-time exclusion.
- **Artifacts and expiry:** demonstrate owned output confinement and exclusion from Git/source builds. Cover just-before/at/after the seven-day threshold from the recorded UTC start time, modified logs not extending retention, a killed run without a finish record, active/uncertain runs being protected, and another worktree's data remaining untouched. Verify success and failure use the same policy and deletion removes an eligible run as a unit.
- **Routine versus infrastructure checks:** prove a normal local invocation does not launch infrastructure regression suites, including on a branch without prior check history. Ensure every retained infrastructure suite has an actual every-PR CI invocation; specifically retain coverage for the preserved progress functionality. Ordinary package tests may use the reporter without running its regression suite.
- **Retirement acceptance:** verify the installed legacy entry and obsolete live machinery are removed at cutover, current shared instructions name the new entry, unrelated user data is untouched, and no old-worktree inventory or migration is required. Reconcile ADR and operational guidance conflicts before claiming rollout complete.
- **Execution policy:** infrastructure integration fixtures run in their CI/change-specific context, not as another prerequisite of every routine local invocation. One-time adoption evidence must cover the actual Mac and real execution handles. Record those acceptance results separately from this specification.

## Out of Scope

- Automatic three/four-way change classification, cross-run or cross-worktree stage reuse, authenticated evidence stores, key rotation, or a branch-baseline success requirement.
- A custom admission server, queue manager, watchdog, general recovery platform, strict forced-termination exclusion, or a scheduler for every R command.
- Automatic AI stopping/resumption, queued completion messages, or a zero-token waiting guarantee.
- Automatic native-crash retries or permanent legacy compatibility code.
- Migration, inspection, or individual instruction refresh of old worktrees and their existing AI sessions.
- Scheduled cleanup, sweeping other worktrees, distinct artifact lifetimes, permanent result storage, and cleanup of unrelated user/tool/OS data.
- Coverage measurement inside routine Review-ready, replacing the release matrix, or redesigning unrelated CI and release checks.

## Further Notes

- This is the consolidated specification for #1114 following the maintainer's 2026-11-02 design reset. Earlier interview proposals are historical where they conflict. Implementation and adoption evidence are tracked separately in #1121, #1122 and #1123.
- The agreed output layout is `<worktree-root>/.review-ready/runs/<run-id>/`, explicitly outside `.git`. The directory must be ignored by both Git and R builds during implementation.
- The retained six verification contents were established from historical commit `b5f851a25b227eb511127b4fef19d60817bbbc71`, immediately before shared admission was introduced. Reuse those verification contents, not the entire historical runner implementation.
- #1117 merged an earlier R bootstrap without establishing local cutover. The [retirement guide](../../tools/review-ready-admission.md) records the drained Mac cutover on 2026-11-02.
- This target intentionally contradicts parts of accepted ADR 0038 and ADR 0039: reusable authenticated evidence, strict recovery, temporary-work cleanup before the common retention deadline, and compulsory old-worktree migration are not retained. ADR 0038 is superseded and ADR 0039 amended accordingly; their historical texts remain available through pinned links.
- The existing CI audit found a gate-contract invocation, a runner-process fixture invocation, and a renderer fixture invocation, but no direct workflow invocation for the dedicated progress fixture. CI rewiring must follow the simplified retained implementation. Required-status branch protection was not verified; do not claim these checks are already required merge statuses.
- A prior Codex notification experiment is not part of this target. Do not carry its probe, callback, or queued-message behavior into the implementation.
