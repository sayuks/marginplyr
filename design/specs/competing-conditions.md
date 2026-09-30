# Competing conditions during interruption

Decision: accepted under the maintainer's delegation. Specified: 2026-09-30.
Amended by the maintainer: 2026-10-01, available-handler boundary.
[Issue #756](https://github.com/sayuks/marginplyr/issues/756) owns implementation;
[ADR 0035](../adr/0035-preserve-competing-conditions-at-interruption.md) owns the
priority, handler boundary, and rejected alternatives. The implementation pull
request records validation and publication evidence for this specification.

## Outcome and retained information

Select the outcome from conditions caught while package handlers remain
available within the package-owned operation boundary. The table does not
extend capture into an older caller handler where R makes those package
handlers unavailable.

| Conditions caught before propagation | Escaping outcome |
| --- | --- |
| Execution error, successful cleanup, no interrupt | Original execution error under ADRs 0015/0021 and #754 |
| Caught user interrupt, successful cleanup | Interrupt, with the original interrupt retained |
| Execution error followed by interrupt during successful cleanup | Interrupt, with the original execution error retained as its parent |
| Failed cleanup, with execution error, interrupt, or both | Error reporting incomplete cleanup, with the triggering conditions and cleanup failure retained |
| Warning replay fails while an error or interrupt is pending | Pending outcome; replay does not replace it |
| Warning replay fails without a pending error or interrupt | Replay failure, retaining the existing warning-only behavior |

An interrupt outcome must inherit `interrupt` and `condition`, without acquiring
`error` or `marginplyr_error`. An interrupt handler receives it; an error-only
handler must not absorb it as an ordinary error. An interrupt class alone does
not establish the correct signaling protocol. With no exiting interrupt
handler, retain native R interruption behavior for top-level recovery and the
caller's configured interruption hooks, comparing with native controls on the
same R version. Returning calling handlers must first see the enriched
notification; they may then receive a bare native notification during fallback.
That is the same cancellation, not a second operation or a promise of one
notification per interrupt. No new stable condition subclass is promised.

For an interrupt carrying competing diagnostic information, `$interrupt` retains
the original interrupt object, and `$parent` retains the original execution
error when one preceded it. Preserve that error's own parent chain, class,
diagnostic, and Condition context; do not relink or reclassify it. If a replay
failure is caught during interrupt unwind, `$replay_error` retains it separately
from the execution-error parent. Absent information may be `NULL` or absent.
A lone interrupt with no additional information needs no wrapper.

A cleanup failure remains an ordinary error outside `marginplyr_error`, as it
is not a call correction within the public interface. Its diagnostic identifies
the failed rollback or release step; `$cleanup` retains the cleanup condition,
and `$parent` retains the triggering execution error or interrupt outcome above.
If interruption is caught before propagation, that interrupt outcome is the
cleanup error's parent, retaining any earlier execution error. This also applies
when the first interrupt arrives after cleanup has already failed, during
diagnostic preparation. A further caught interrupt must not erase a cleanup
failure already observed or replace the first retained interrupt.
Preserve original-condition objects in these fields; wording and narrower
classes remain implementation details.

These structured fields are the selected interface for competing outcomes, not
a requirement to wrap every External condition. Without an interrupt, retain
#754's ordinary-error behavior, including its warning-handler observations and
short-circuit on replay failure. Warning-only `warn = 2` continues to fail.

## Deferral and warning replay

Keep the resource boundary in ADR 0031 and the
[SQLite catchable-interruption amendment](sqlite-b-direct.md#catchable-interruption-amendment-755).
An interrupt arriving during protected cleanup waits for the owned rollback and
release attempt. On successful cleanup, propagate the pending interrupt with
the earlier execution error attached. Resolve cleanup-queued interruption within
the operation's outcome: do not return only the execution error and leave that
same queued interrupt to replace it during subsequent caller inspection or
unrelated work. Do not silently retry materialization.

After successful release of ordinary materialization, an interrupt may prevent
delivery of the R value but must not undo completed database work. Inner release
still leaves commit and rollback to the caller. Outcome selection does not
extend the resource owner's lifetime beyond that existing boundary.

Attempt buffered-warning replay during interrupt unwind in the existing
first-occurrence order, with the existing identity, count, and Condition context.
Calling warning handlers can observe and muffle a replayed warning normally.
An error caught during replay, including R's `warn = 2` conversion after
returning warning handlers, ends replay and is retained as `$replay_error`;
the interrupt still escapes. Remaining warnings are not promised delivery,
and replay is not restarted. Do not change the caller's `warn` option to
implement this policy.

A further interrupt caught during replay ends the remaining replay and joins
the pending cancellation. If an ordinary execution error was pending instead,
the interrupt becomes the outcome and retains that error as its parent. Keep
the first interrupt once cancellation has been established; additional
interrupts do not replace its diagnostic information or rerun cleanup. Runtime
signals may coalesce, so their exact number is not promised. Do not shield
arbitrary user warning handlers for the duration of their work.

When an older caller warning handler runs, R makes newer package handlers
unavailable. Errors or interrupts raised there, and caller restarts that leave
the operation, follow native R control flow when they bypass package handling.
They may replace an already pending interrupt or execution error without
retaining it, and a handler failure is not guaranteed in `$replay_error`.
This includes an older handler reached by a nested warning inside another
callback. Preserve normal handler availability and existing caller restart
tokens; do not isolate, re-register, or silently wrap caller callbacks.
Fatal signals and process termination likewise gain no completion guarantee.

Optional caller guidance may show a caller-owned catcher inside the warning
callback. It can retain the callback's failure separately, then muffle that
warning and subsequent warnings. The caller decides how to handle the operation
outcome and callback failure, including a successful value with a captured
failure. This does not add a package function, replay restart, or guarantee over
noncooperating older handlers.

## Incomplete cleanup and limits

Failed rollback may leave destination mutations. Successful rollback followed
by failed release may restore data while leaving the owned savepoint and
transaction active. Neither establishes completed recovery or same-session
reuse. Report the failure and retained causes; do not automatically reconnect,
take over an outer caller transaction, or retry cleanup until it happens to
succeed. Observe unavailable state as unavailable, including a blocked observer
or invalid connection, rather than interpreting it as restored or absent data.

An invalid connection or backend-aborted whole transaction cannot support the
conditional restoration guarantee. Native SQLite statement cancellation and
timeouts, including their effects on a whole transaction, remain unverified by
checkpoint SIGINT evidence. A controlled whole-transaction rollback is not
evidence of native cancellation. These are backend/resource limits rather than
automatic-repair obligations. Interrupts after the package operation has left
its boundary belong to the subsequent caller operation.

## Observable acceptance

Use public Margin summaries and their direct SQLite `compute()` boundary. Keep
controlled injection and supervisor-delivered actual SIGINT as separate evidence.
Each injected boundary needs a reached notification, proof of already completed
side effects, a healthy control, and immediate observation before retry, caller
transaction decisions, reconnect, or test disposal.

| Scenario | Required observations |
| --- | --- |
| SQLite execution error, real rollback-to completed, interrupt before real cleanup release | Prior destination restored; savepoint released; interrupt contains original execution error; no stale queued interrupt replaces the outcome during later inspection |
| Earlier local branch warnings, later branch interrupt | Earlier side effects prove execution; calling handler observes replay; interrupt escapes at `warn = 1` and `warn = 2`; at 2 the conversion error is separately available |
| Replay warning muffled at `warn = 2` | Warning handler runs; no conversion error is invented; pending interrupt survives |
| Outer warning handler raises an error or further interrupt that bypasses package handling | Native caller outcome escapes; earlier cancellation or execution-error retention is not promised; original handler failure is not replaced by a package-generated error |
| Further interrupt during interrupt-triggered rollback or between rollback and release | Successful cleanup finishes once; first interrupt remains observable; no recursive materialization or cleanup retry |
| Further interrupt caught during replay or diagnostic selection | First interrupt retains earlier causes; an established incomplete-cleanup error remains the outcome; no successful value is returned |
| Failed rollback or release, with interruption before or after cleanup failure | Failed step, cleanup condition, earlier execution error and first interrupt remain observable in the selected fields; residual state recorded without a recovery/reuse claim |
| Invalid connection or simulated whole-transaction abort | Cleanup failure and trigger observable; no automatic repair or caller-transaction takeover; simulation distinct from native cancellation |
| Interrupt after successful materialization release | Completed work remains; caller ownership and visibility follow existing inner/outer release boundary |
| Ordinary error and warning-only controls | #754's class/diagnostic/cause and replay observations retained; warning-only `warn = 2` still errors |

For SQLite, independently specify an old destination with distinct data/schema/
index, source rows, and an unrelated sentinel. Observe same-session state and
persistent state through a separate observer connection immediately. Establish
absence of the owned savepoint and successful next transaction before using
retry as additional evidence; successful retry alone proves neither. Include
both `in_transaction` values and an outer caller transaction, preserving earlier
caller work and separate commit/rollback choices. Use #755's existing matrix
for broader destination and release-boundary coverage.

For the local sequence, record warning production, arrival at the later branch,
replay, escaping condition, unchanged input, restored options, and a subsequent
valid summary in that session. Disable only warnings as an interrupt control;
remove the interrupt as a healthy or warning-only control. Inspect original
objects and parent chains, not only rendered messages.

Exercise outer warning-handler errors and interrupts with both a pending
interrupt and a pending execution error. Compare native caller outcomes and
retained object identity instead of accepting those cases as successful
competing-cause preservation. Also cover selected older handlers, ordinary
direct invocation of a registered callback, nested warnings, and a caller
restart token captured before the operation. Optional cooperative examples
must preserve their separate failure record and state their narrower scope.

Actual SIGINT needs a flushed checkpoint and matching worker PID before delivery.
Capture the public operation's raw outcome before diagnostic rendering or
deferred-signal probes; also observe the subsequent probe. A worker replacing
its captured error with a later interrupt does not prove package preservation
of both. Include interrupt handlers, calling handlers, and separate subprocesses
with an error-only handler or no interrupt handler. Compare native interruption
controls with default and custom `options(error)` and `options(interrupt)`
settings for top-level recovery and hook invocation, where the native version
supports them. Verify the first enriched notification and permit native fallback
notifications to returning calling handlers, without pinning terminal wording
or exact notification counts.

Run controlled second-interrupt and cleanup-failure cases. Record actual-SIGINT
second-interrupt, native-statement, timeout, platform, and dependency coverage
explicitly; an unverified case is a limit, not a passing cell. Retain source
snapshot and environment. The existing
[fault hunt](../../investigation/exception-safety-recovery-2026-09-30.md) and
[#755 acceptance](../../investigation/sqlite-interruption-recovery-2026-09-30.md)
are evidence for their recorded snapshots, not verification of this policy.

## Implementation and publication

Scope implementation to condition selection at the existing SQLite savepoint owner
and portable summary warning-replay boundary. Select the smallest mechanism
that satisfies the outcomes; this decision prescribes neither handler nesting
nor an interrupt-resignaling helper. No general backend recovery framework,
new public option, warning-history store, staging table, unrequested input read,
or automatic connection repair is required.

The [signaling investigation](../../investigation/condition-resignaling-2026-09-30.md)
records a measured candidate: notify with the enriched condition, and enter the
native interruption path only if no exiting handler takes that notification.
It established handler dispatch and native-hook behavior on its recorded
environment, not integrated operation cleanup, actual SIGINT, or other versions.
Use it as reproducible prior art rather than a required handler design.

Publish implemented outcomes and structured fields in
`R/marginplyr-package.R`'s *Errors and warnings*. Keep the materialization
contract in `R/summarize_with_margins.R` and SQLite guidance in
`vignettes/database_backends.qmd` aligned with ADR 0031. Regenerate affected
help and run applicable documentation/site verifiers. Public guidance must not
omit the available-handler boundary or describe bypass cases as retained causes.

Run the Review-ready check on the clean committed implementation and applicable
release-matrix checks under `design/agents/local-checks.md`; record the exact
snapshot and terminal outcome. #756 owns that implementation after this decision,
retaining completed #754 and #755 guarantees. Interrupts, errors, warnings, and
savepoints are borrowed technical vocabulary; no glossary term is added.
