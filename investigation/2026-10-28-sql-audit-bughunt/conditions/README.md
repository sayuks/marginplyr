# SQL condition-observer checks

Investigated: 2026-10-28
Repository snapshot: `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

These disposable harnesses compared SQL auditing off and on through the public
SQLite `summarize_with_margins()` path, enabling the observed Margin label
collision check. The first two
harnesses replaced the owned `dplyr::collect()` boundary with controlled
condition fixtures; the DBI harness retained real SQLite send/fetch/clear work
and wrapped DBI calls for condition injection. No marginplyr observer or
renderer was replaced.
These are not claims that a stock SQLite driver emits these compound conditions.
They load the checkout selected by the caller, not an installed marginplyr.
No package implementation or package tests were edited.

Run from the checkout root:

```sh
Rscript /path/to/conditions/exit-matrix.R
Rscript /path/to/conditions/severity-matrix.R
Rscript /path/to/conditions/dbi-fetch-recovery.R
```

An optional first argument selects a different checkout. All three commands exited
0 on the snapshot above. The sibling `.log` files retain their final output.
No new product defect was established.

## Cleanup and recovered interrupts

`exit-matrix.R` completed 16 off/on pairs: a resumable interrupt raised in the
owned operation body or in its `on.exit()` cleanup, bare `signalCondition()` or
native `rlang::interrupt()`, followed by normal return, a nonlocal restart
escape, a later error, or a later interrupt. It compared caller outcome,
delivered condition classes, and cleanup order. The detailed record asserted
`returned`, `unknown`, `errored`, or `interrupted`, respectively.

Final output: `PASS: 16 cleanup/resumption pairs`.

## Ordered severity combinations

`severity-matrix.R` completed 2,304 off/on pairs. It crossed all 64 nonempty
ordered subsets/permutations of `error`, `warning`, `interrupt`, and `message`
with `signalCondition()`, `warning()`, and `stop()`; `warn = -1/0/1/2`; and
outer observation, warning muffling, or a driver-local recovery restart.
Each leaf also inherited from `condition`.

It compared caller outcome class/text, delivered condition classes/text, and
cleanup actions. Each audited internal row retained exactly one invocation
summary for the leaf when the leaf carried a recorded severity; message-only
leaves had no severity summary. Summary phase and boundary handling were
checked. Normal-return status and terminal error/interrupt status were asserted
when their severity was unambiguous. A condition inheriting from both error
and interrupt did not receive an invented status expectation.

A `stop()` supplied only a warning/message condition can reach R's default
fatal stop without matching an error/interrupt catch. The harness uses a
labelled outer escape restart for that case, identically in both audit modes.
That protection is a harness boundary, not evidence of an ordinary driver path.

The message diagnostic sink uses a temporary file, restores and closes it on
exit, and replays it on a harness failure. The final log's ten warning messages
are deliberate fixture diagnostics left by warning-delivery controls.

Final output: `PASS: 2304 severity pairs across 64 ordered severity sets`.

## Live DBI fetch and result cleanup

`dbi-fetch-recovery.R` completed 24 off/on pairs with real SQLite collection.
An injected bare or native resumable interrupt ran immediately before fetch,
immediately after fetch, or in the DBI fetch wrapper's `on.exit()` cleanup.
Each recovered path then returned normally, escaped by a nonlocal restart,
raised a later error, or raised a later interrupt.

Each pair compared result SQL or original condition text/classes, delivered
interrupt classes, and cleanup order. Both modes crossed exactly one wrapper boundary for
`dbSendQuery()`, `dbFetch()`, and `dbClearResult()`. The send and clear wrappers
forwarded to SQLite. Fetch forwarded exactly once when control reached its
real method, and zero times for a terminal pre-fetch exit. These counts did
not treat wrapper entry as proof of a completed driver operation. The audited row's
terminal status and ordered condition-summary count agreed with the path.
A subsequent `SELECT 42` verified connection usability after every pair.

Final output: `PASS: 24 live DBI fetch/cleanup recovery pairs`.

## Limits and rejected candidates

Earlier investigation already bounded formatter-generated cancellation and a
recovered error followed by an unrelated private-seam escape. They were not
promoted to new findings here. Arbitrary formatter side effects, unsupported
renderer return shapes, wire-level observation, server completion, and every
native cancellation timing were not established by these finite checks.
