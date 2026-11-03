# Measured test order and FIFO Review-ready admission

Status: accepted, 2026-11-03. The maintainer selected the narrow private timing
hook after comparing public-only alternatives and confirmed the specification
after checking its R CMD check implications. Ordinary testthat dispatch and the
other agreed decisions below remain, with no first-run priority guarantee for
unregistered files. This records specification agreement. FIFO implementation
and Mac adoption are recorded
under *FIFO policy boundary and adoption* below; timing/order work remains
independent.
[ADR 0040](../adr/0040-learn-local-test-order-and-admit-heavy-checks-fifo.md)
records the scope boundary. Existing package CI is unchanged.

## Scope and requirements

The task has two independent goals:

1. Use measured test-file duration to keep expensive files, including new files,
   from being dispatched too late in later Review-ready checks.
2. Admit heavy Review-ready requests in acceptance order across this user's
   clones and worktrees.

The existing tracked `Config/testthat/start-first` setting remains the priority
source. Preserve the repository command, source/full judgment, five baseline
checks, pinned committed source, one shared heavy slot, four workers, complete
suite/snapshots, terminal-result authority and retained execution ownership.

A shared local timing database, per-worktree rolling history and mandatory
CI artifact retrieval are not execution prerequisites. Add no dedicated timing
CI job. Existing CI checks and runner regression
fixtures retain their existing validation role.

This specification replaces the previous CI-dependent ordering proposal. That proposal's
earlier approval does not authorize implementing or republishing it.

## Agreed measurement and application boundaries

Reuse the full Review-ready check that the caller already requested. Measure
worker file intervals within that check, rather than introduce a separate
measurement-only full run or dedicated CI job. Keep observations with the
invocation's ordinary artifacts; they are outputs of that invocation, not a
persistent store required by later test execution.

Use the one successful complete full invocation to prepare a proposal. Routine
operation requires neither three repeated full invocations nor a rolling median.
Retained run artifacts are evidence for that invocation, not timing history
consulted by later execution.

After a successful complete check, use its eligible measurements and the
committed priority to prepare an update proposal. The proposal records its SHA,
file identities and reasons for moving files. Failed, interrupted, incomplete
or all-skipped observations provide no promotion evidence. Controller reporter
replay cannot supply worker duration; measurement surrounds actual file work
and cleanup with startup and worker-wide teardown kept separate.

The [three-run calibration](../../investigation/review-ready-order-calibration-2026-11-02.md)
remains evidence for conservative margins. Its >50% and >10-second stable
promotions reduced changes but still admitted a later proposal with only
0.006 seconds of modeled incremental benefit. The agreed criteria below
therefore distinguish constructing a candidate from adopting its reordering.

A candidate priority update becomes effective through an explicit Git commit
in the current task. That follow-up commit may be in the same pull
request; neither a separate pull request nor a main merge is required before
that worktree can use it. Git integration shares the priority with other
worktrees. Its new SHA needs its own applicable verification; the measurement
result cannot certify it. Later validation must not automatically apply another
proposal or create an edit/recheck loop. A
fresh clone executes the committed priority without a prior local history.
Concurrent or stale proposals follow the existing
[integration workflow](../agents/integration.md).

The replacement specification is agreed. The maintainer authorized updating
the existing four issue slices and retaining their blocking edges to match this specification; the
old CI-dependent requirements are superseded.

## Agreed ordinary testthat dispatch

testthat 3.3.2 supports tracked start-first patterns, followed by remaining
files alphabetically. Its `test_dir()` reads the setting itself, so passing
`start_first` through `test_check(...)` duplicates an internally supplied
argument. A public discovery function accepting an order is not an execution
override. See the [parallel-testing guide](https://testthat.r-lib.org/articles/parallel.html)
and [versioned implementation](https://github.com/r-lib/testthat/blob/v3.3.2/R/test-files.R).

Use ordinary `test_check()` dispatch with the committed priority. A file absent
from that list follows registered priorities alphabetically until a priority
update is committed. After successful measurement, propose its tracked priority
using the registration and adoption rules below. A later check can use the
updated committed order without local timing history.

Accepting initial append-last behavior avoids a private execution adapter and
pre-measurement provisional registration. The original goal is learning from
one check for later checks; first-run priority is not a requirement. Do not
introduce a pre-commit missing-file prefix or rewrite the runtime order.

Adopt a narrow private worker-timing hook as an explicit maintenance dependency.
Keep it in one common instrumentation helper rather than add timing statements
to individual test files. For the calibrated testthat 3.3.2 implementation,
record entry and exit of `queue_task()` around file execution and file cleanup.
The hook observes that work; ordinary `test_check()` still owns dispatch.

Declare supported testthat versions and check the supported function shape
before recording. A newly supported version requires actual-worker fixtures
that establish file identity, matched timing boundaries and inclusion of file
cleanup. Worker-wide startup, setup and final teardown remain outside the file
interval. Unsupported versions disable recording and proposals as specified
below. Controller reporter replay is not a replacement clock. Ordinary package
tests and CI continue using the committed setting without this local recorder.

Here, "unregistered" is an observable Git-state property, not proof that a file
has never run. A newly added or renamed file is unregistered; changing the body
under an already registered filename does not make it unregistered. Such a
change is measured in the next full check and can inform a later priority
proposal. Successful measurement alone does not change registration: only a
committed priority update does.

The calibration measured native testthat dispatch, which placed unregistered
files after the registered list. It therefore matches the agreed first-run
ordering, but its modeled proposals still do not establish an observed speedup
after adoption. Dispatch priority never guarantees worker start timestamps or
completion order. The design worktree changes no production priority or
execution path.

## Agreed registration of newly measured files

Include every successfully and validly measured unregistered file in the next
priority-update proposal, including lightweight files. All-skipped files and
files without eligible measurements stay unregistered. This records all
eligible observations as priority membership, not only expensive promotions.
The commit records membership and order, not historical duration samples.
Generating the proposal does not itself register a file.

Remove entries for deleted test files as priority-list maintenance. A renamed
file therefore loses its old entry and follows the unregistered-file rules at
its new name. These membership updates do not depend on the optional
existing-file reordering threshold.

## Agreed reordering and adoption criteria

Keep new-file registration separate from optional changes to the relative order
of already registered files. Construct a registration-only baseline by placing
eligible new files using their measured duration while preserving the relative
order of existing entries. Process new files by descending measured duration,
breaking new-file ties by filename. Insert each before the first existing entry
with a strictly shorter eligible measured duration, or append it if none exists.
Equal duration is not a shorter-duration match. Ineligible observations do not
supply comparison evidence.

For an optional existing-file reordering candidate, use stable insertion: move
a file left past its predecessor only when both observations are eligible and
it is both more than 50% and more than 10 seconds slower. Comparisons within
those margins preserve order.

Adopt that optional reordering only when a greedy four-worker replay estimates
at least ten seconds of whole-suite improvement over the registration-only
baseline. Both schedules use the same invocation's measured file durations;
this calculation requires no additional test execution. If the estimate is
smaller, retain the existing relative order and keep eligible registrations in
the proposal. Report the adopted proposal's estimated effect against the actual
native registered-first dispatch of the measured invocation as well. An
estimate is not a measured speedup or a statistical guarantee; ten seconds is
the initial operating threshold.

Inspect and adopt the first eligible changed proposal in the current task,
even when implementation is otherwise complete. Use eligible, available evidence
and current integration checks. An adopted new SHA still needs its usual
verification. Inspect and record that validation's proposal, then stop adoption
for this task unless a new implementation change or explicit user request
requires it. The runner guide's *Priority proposals and explicit adoption*
defines the required inspection and disposition record.

## Agreed instrumentation overhead acceptance bound

Record small worker-local file-boundary events, not a disk event for every
test or expectation. Compute the single agreed priority proposal once after a
successful full invocation; routine execution does not repeat the calibration's
multi-policy or three-run research analysis.

The [component cost investigation](../../investigation/review-ready-ordering-overhead-2026-11-03.md)
measures recording and proposal calculation separately. It is neither a
production implementation benchmark nor a measured whole-suite speedup.

On the maintainer Mac at the reference scale of 196 files and four workers,
verify that the implementation's median incremental cost of hook setup,
recording, parsing, computation and proposal output stays below one second. Record the
sample distribution and method rather than infer this result from estimated
scheduling gains. This is an implementation acceptance measurement, not an
additional routine full check, timing CI job or runtime failure threshold.

## Source-tarball check compatibility

Verify the implemented recorder with timing enabled in its applicable full
source-tarball check, including dependency, code and test diagnostics and the
terminal NOTE count. The three calibrated full checks had zero ERRORs,
WARNINGs and NOTEs with R 4.6.1 and testthat 3.3.2; that is evidence for the
experimental helper, not certification of the eventual implementation or
later dependency versions. Apply the existing local-checks NOTE policy to the
implementation result. Do not treat absence of a diagnostic as a public API
or compatibility guarantee.

## Agreed stale-proposal and failure policy

When only tracked priority changes after measurement, recalculate against that
priority using the same eligible run evidence rather than blindly applying an
old patch. If the measured test set, test/helper contents or required dependency
environment changed, or the evidence expired or is unavailable, defer that
proposal until the next ordinarily required successful full measurement. No
additional full run is forced solely to recover an ordering proposal. Existing Git integration
and the new revision's verification remain necessary.

If the private timing hook is unsupported, disable recording and proposal
generation with a visible diagnostic, and execute ordinary `test_check()` with
the tracked priority. Timing-recorder, proposal-output or calculation failures
likewise disable the proposal and report the reason while preserving the
complete check's actual outcome. Isolate only optimization
failures: never swallow a package-test, worker or ordinary runner failure. Do
not retry a suite after dispatch has begun. FIFO state/ownership errors retain
their separate visible-failure policy below.

Activate recording only for full Review-ready. All invocations retain ordinary
`test_check()` execution; recording observes worker boundaries without replacing
test dispatch. Every path must execute the same discovered files exactly once
with ordinary failure, snapshot and parallel
semantics. Do not rewrite a pinned source, tarball or installed DESCRIPTION to
inject an invocation's order. The committed priority changes only through the
explicit Git application workflow above.

## FIFO heavy admission

After the five checks succeed, atomically register a full request and allocate
its sequence under a short-lived shared metadata lock. Concurrent registrations
are ordered by successful registration, not command launch, timestamps, PID,
branch name or expected duration. Report the accepted sequence and number of
live requests ahead; report admission with actual elapsed wait. Recompute the
count on polling and print changes only, with no ETA.

Only the oldest live waiting request may acquire the existing stable
`review-ready-heavy.lock`. Hold the metadata lock briefly for registration,
liveness pruning and admission transitions, never through waiting or build/check.
Preserve bounded-resource waiting from #1135 and its implementation in PR #1152;
do not reintroduce repeated failed nonblocking lock acquisition. Release the
metadata lock before any blocking wait for capacity or another request owner.
Remove the admitted request from the waiting set in that transition, retaining
its active ownership record until cleanup. New
arrivals cannot overtake earlier live waiters by polling more often. Keep the
heavy lock through owned-process cleanup. No agent-ready acknowledgment or
priority lane is added.

If A is running and B then C register, admission is A -> B -> C. If B cancels
or dies while waiting, C follows A. If B is alive but suspended, C waits: age
alone never forfeits its place. If A's baseline is still running when B
registers, B precedes A. FIFO prevents overtaking, not long checks or all agent
token consumption.

### Cancellation, stale requests and state failures

Each registration has a never-reused identity and an OS-backed liveness guard
held by that invocation. Under the metadata lock, prune a registration only
after positively acquiring its guard. A PID, worktree path or age threshold
is insufficient. Hold the guard before publishing registration. Keep only the
acceptance counter and live coordination records, not completed-request
history. Prune unreferenced guards from interrupted registrations only after
positively establishing that their ownership ended.

Ordinary waiting cancellation unregisters the request and returns a terminal
cancellation through the retained handle; it can never start later. Abrupt
waiter death releases its guard so polling successors can prune it. Retrying
creates a new invocation and sequence.

Unreadable or incompatible queue state, ambiguous liveness and metadata-lock IO
errors fail the affected invocation visibly with retained artifacts. They never
authorize bypassing a possibly earlier live waiter. Ordinary lock contention
waits. Run retention never resets stable lock targets or acceptance sequencing.

Running cancellation collects owned children before releasing capacity. Abrupt
running-parent termination retains ADR 0039's accepted descendant-cleanup limits;
FIFO adds neither strict crash-time exclusion nor automatic retries.

## FIFO policy boundary and adoption

Minimal FIFO bookkeeping remains separate from timing history. It coordinates
live processes using the existing heavy-lock target; it is not a measurement
store, CI service, daemon or general recovery platform. Preserve bounded waiting
from [#1135](https://github.com/sayuks/marginplyr/issues/1135) /
[PR #1152](https://github.com/sayuks/marginplyr/pull/1152).

Reconcile the accepted runner guidance when FIFO is implemented. Drain pre-FIFO
checks and waiters before claiming adoption: older runners still preserve
exclusion but may overtake participating requests. No old-session migration,
fleet inventory or compatibility launcher is required. Actual Mac adoption is
separate evidence from disposable CI fixtures or design approval.

The #1156 implementation merged in PR #1163. The independent #1157 Mac
adoption completed on 2026-11-03 for
`604f1aebfeb954234e8c672d773c5906670621ec`; its
[evidence record](../../investigation/review-ready-fifo-adoption-2026-11-03.md)
holds the pre-adoption empty-state observation, isolated process outcomes and
real full terminal result. The stable heavy lock and live coordination were
preserved. No timing deployment, priority update or old-worktree migration
was part of that adoption. Older nonparticipating runners remain outside
the FIFO guarantee, and the abrupt-termination limits above still apply.

## Implementation tickets

The existing issue numbers and vertical slices are retained with the approved
local workflow. Their native blocking relationships are:

- [#1155](https://github.com/sayuks/marginplyr/issues/1155): record eligible
  worker file durations within an ordinarily requested local full check.
  No blocker; no dedicated timing CI workflow.
- [#1158](https://github.com/sayuks/marginplyr/issues/1158): generate and apply
  stable Git-tracked priority proposals from those local measurements.
  Blocked by #1155.
- [#1156](https://github.com/sayuks/marginplyr/issues/1156): implement independent
  FIFO heavy admission. Blocked by #1135, the bounded-wait repair.
- [#1157](https://github.com/sayuks/marginplyr/issues/1157): establish actual Mac
  FIFO adoption with recorded evidence. Blocked by #1156, with no dependency on
  timing collection or order-maintenance automation.

These are implementation work, distinct from this specification and archived
calibration evidence. The previous canonical-CI ticket requirements do not apply.

The calibration records remain unchanged. No production runner, test priority,
CI workflow or live queue was changed by this specification correction.
