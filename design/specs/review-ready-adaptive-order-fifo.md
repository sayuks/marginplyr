# Measured test order and FIFO Review-ready admission

Status: ordering workflow reopened, 2026-11-02. The maintainer clarified that
the CI question was exploratory, not a request to introduce a dedicated timing
pipeline. The previous record of CI as an agreed requirement is withdrawn.
A replacement ordering workflow awaits confirmation. FIFO remains the original
independent request; implementation and adoption are not claimed.
[ADR 0040](../adr/0040-learn-local-test-order-and-admit-heavy-checks-fifo.md)
records the scope boundary. Existing package CI is unchanged.

## Requirements and unresolved choices

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
CI artifact retrieval are not execution prerequisites. A dedicated measurement
CI job is not an agreed requirement. Existing CI checks and runner regression
fixtures retain their existing validation role.

The source of measurements and the update/application workflow are reopened.
Do not treat earlier approval of the CI-dependent ticket breakdown as authority
to implement that design after this clarification.

## Recommended smaller proposal — not yet approved

Reuse the full Review-ready check that the caller already requested. Measure
worker file intervals within that check, rather than introduce a separate
measurement-only full run or dedicated CI job. Keep observations with the
invocation's ordinary artifacts; they are outputs of that invocation, not a
persistent store required by later test execution.

After a successful complete check, use its eligible measurements and the
committed priority to prepare an update proposal. The proposal records its SHA,
file identities and reasons for moving files. Failed, interrupted, incomplete
or all-skipped observations provide no promotion evidence. Controller reporter
replay cannot supply worker duration; measurement surrounds actual file work
and cleanup with startup and worker-wide teardown kept separate.

The [three-run calibration](../../investigation/review-ready-order-calibration-2026-11-02.md)
remains evidence for conservative margins. Its >50% and >10-second stable
promotions are a candidate initial rule, not an assurance of zero churn or an
automatic merge decision. Include estimated whole-suite benefit so an agent can
distinguish worthwhile changes from negligible suggestions.

A candidate priority update would become effective through an explicit Git
commit at a normal work boundary. Its new SHA needs its own applicable
verification; the measurement result cannot certify it. Later validation must
not automatically apply another proposal or create an edit/recheck loop. A
fresh clone executes the committed priority without a prior local history.
Concurrent or stale proposals follow the existing
[integration workflow](../agents/integration.md).

Before implementing or republishing the ordering tickets, confirm whether this
smaller proposal is the chosen replacement, when a proposal should be generated,
and how adoption avoids unnecessary extra verification for negligible gains.
The recommendation does not create a new approved implementation scope.

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
metadata lock before any blocking wait for capacity or another request owner. Remove the admitted request from the waiting set in
that transition, retaining its active ownership record until cleanup. New
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

## Ticket status after reconsideration

- [#1155](https://github.com/sayuks/marginplyr/issues/1155), the dedicated CI
  measurement proposal, is `needs-info` with its previous scope retained only
  as context. It is not approved implementation work.
- [#1158](https://github.com/sayuks/marginplyr/issues/1158), the CI-dependent
  priority-update proposal, is also `needs-info` until its input and workflow
  are agreed again. Existing dependency links are retained pending revision.
- [#1156](https://github.com/sayuks/marginplyr/issues/1156) retains the independent
  FIFO request and its dependency on #1135.
- [#1157](https://github.com/sayuks/marginplyr/issues/1157) retains actual Mac FIFO
  adoption, blocked by #1156. Its CI-timing prerequisite was removed; FIFO
  adoption does not depend on choosing or implementing the ordering workflow.

The calibration records remain unchanged. No production runner, test priority,
CI workflow or live queue was changed by this specification correction.
