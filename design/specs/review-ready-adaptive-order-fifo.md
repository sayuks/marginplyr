# CI-measured test order and FIFO Review-ready admission

Status: accepted and ticketed, 2026-11-02. The maintainer approved FIFO,
rejected local timing-history dependencies, chose CI-centralized measurement,
and approved the implementation-ticket breakdown. Implementation and rollout
remain pending. This revision replaces local measurement/update automation and
the earlier shared timing-cache proposal.
[ADR 0040](../adr/0040-learn-local-test-order-and-admit-heavy-checks-fifo.md)
records the decision history and amendment to the accepted
[runner specification](review-ready-repository-runner.md).

## Goals and boundaries

Use one canonical CI measurement to improve subsequent test-file ordering
through Git-managed configuration. Admit local heavy checks in acceptance
order, preventing repeated overtaking of an earlier waiter.

`DESCRIPTION`'s `Config/testthat/start-first` remains the sole persistent
priority authority. A checkout at a given SHA needs neither a home-directory
cache nor previous measurements. CI publishes measurements and a reviewable
update patch as artifacts; applying, committing and merging that patch are
ordinary agent/reviewer actions, not automatic CI writes to Git.

Retain local Review-ready's repository entry, source/full judgment, five checks,
pinned input, one shared heavy slot, four workers, complete suite and snapshots,
terminal-result authority, original execution handle, ordinary cancellation and
bounded same-agent investigation. It uses its committed order without querying
CI artifacts or measuring files for automatic order updates. Shared FIFO state
coordinates current processes on the same machine and contains no timing
history.

## Canonical CI measurement

Use one macOS / R-release, four-worker, noncoverage configuration with the
complete test suite, snapshots enabled and registered required Suggests
asserted present. Do not mix CRAN-semantic snapshot-skipping jobs, coverage
measurements, backend-withholding jobs, other platforms or R versions. Preserve
those jobs' existing responsibilities rather than silently changing their
semantics. A dedicated main-push/manual job is the recommended initial host;
PR infrastructure fixtures verify this feature without requiring an additional
full measurement suite on every PR.

Measure worker elapsed time around each file's execution including its file
cleanup. Queue waiting, worker startup, worker-wide helper/setup and final
worker teardown are outside file duration. Controller callbacks are buffered
and replayed, so their arrival times are not measurements.

Publish an artifact with run/attempt identity, measured SHA, environment and
dependency versions, actual worker count, discovered file identities, and
per-file start/end/duration/skip status. Retain diagnostics on failure, but mark
measurements eligible only after a successful complete check and validation of
one matching start/end pair per discovered file, finite nonnegative durations
and the expected worker configuration. Failed, cancelled, partial, duplicate
or malformed observations cannot produce an update. All-skipped files provide
no promotion evidence.

Use opt-in worker instrumentation, isolating any required private testthat hook.
Preserve normal `test_check()` execution, helpers/setup, snapshots, reporters,
cleanup and failure behavior. Unsupported instrumentation or invalid timings
must suppress a proposal with a visible explanation, never turn a failed test
into a pass or launch a replacement suite. Configure finite CI artifact
retention; no rolling timing store is needed for future execution.

## Stable priority proposals and Git integration

Generate a deterministic patch and summary from one eligible canonical CI
result. Include its source SHA/run, moved files and measured evidence. The
proposal includes every discovered file exactly once and removes obsolete
literal entries. Newly added heavy files can be promoted after their first
eligible measurement. Retain no three-sample median, cross-run timing history,
raw seconds or refreshing timestamps in tracked configuration.

The initial conservative rule promotes a file only if it is both more than
50 percent and more than 10 seconds slower than a file it overtakes. Starting
from normal priority/discovery order, use stable insertion: process each next
file, moving it left while both margins hold against the immediately preceding
file. Stop at a close or ineligible comparison. Equal/short comparisons preserve
existing priority. A new file starts at its normal unlisted position. This is
coarse heavy-first ordering, not a statistical error bound or an optimal sort;
a severe single-run outlier can still influence a reviewable proposal.

A 180s new file can overtake an existing 40s file; existing 100s and 110s files
keep their relative order. An unchanged list produces no configuration diff.
Do not create measurement-only commits or force every worktree to merge every
new measurement.

The [three-run local calibration](../../investigation/review-ready-order-calibration-2026-11-02.md)
supported these margins as an initial filter, not a zero-churn guarantee or a
hosted-CI result. Include baseline/candidate four-worker greedy makespan
estimates and their difference in the proposal summary, clearly labelled as
modeled benefit. Agents may decline negligible suggestions at the normal Git
integration boundary; a nonempty patch alone does not justify merging it.

Allow an agent to select the identified CI artifact and apply its proposal to
an appropriate branch through the normal PR workflow. Reject or regenerate
stale proposals when relevant measured test contents or priority configuration
changed; follow the existing [integration workflow](../agents/integration.md).
Do not combine concurrent proposals by concatenating lists or blindly selecting
one conflict side. An independent fresh clone after Git integration must use
the updated priority with no local measurement history or artifact access.

A priority commit has a new SHA and receives the applicable verification under
[local checks](../agents/local-checks.md). Prior CI or local evidence does not
certify it. Subsequent validation may publish another suggestion but never
applies or commits it, avoiding a measure/edit/recheck loop. All invocations keep
using their original committed order while running; CI never edits its tested
source or installed metadata in place.

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

## Policy amendment and adoption

The proposed exception to the accepted runner's exclusion of a custom queue
manager is limited to minimal FIFO bookkeeping. Timing history remains outside
local execution dependencies. CI artifacts are evidence for a Git update, not
an online ordering service. No daemon, watchdog, durable result ledger, AI
notification/resumption or focused-test scheduler is introduced.

The FIFO implementation follows the bounded-wait repair in
[#1135](https://github.com/sayuks/marginplyr/issues/1135) /
[PR #1152](https://github.com/sayuks/marginplyr/pull/1152), preserving its resource
and cancellation guarantees. Reconcile ADR 0039, the accepted runner
specification, runner guide and local-check guidance as behavior is implemented.
Keep the heavy-lock target. Drain pre-FIFO checks and waiters before claiming
adoption: an older runner respects exclusion but may overtake participants.
FIFO covers upgraded participants without old-session migration, fleet
inventory or a compatibility launcher. Unknown queue formats fail visibly
rather than starting independent queues.

## Implementation slices

1. [#1155: Canonical CI worker timings](https://github.com/sayuks/marginplyr/issues/1155)
   publishes eligible worker measurements with real worker fixtures and preserved
   check verdicts. No blocking ticket.
2. [#1158: Stable priority updates](https://github.com/sayuks/marginplyr/issues/1158)
   produces noise-resistant patches, reports estimated whole-suite benefit and
   verifies Git-based application in a fresh clone. Blocked by #1155.
3. [#1156: FIFO heavy admission](https://github.com/sayuks/marginplyr/issues/1156)
   provides bounded waiting, cancellation, dead-waiter pruning and real
   cross-clone/worktree process fixtures. Blocked by existing #1135, independently
   of either CI slice.
4. [#1157: Actual Mac adoption](https://github.com/sayuks/marginplyr/issues/1157)
   reconciles operational guidance and records retained-handle/cutover evidence.
   Blocked by #1156 only.

Each ticket owns its regression coverage and applicable policy updates. Actual
Mac adoption remains distinct from CI fixture results. Native GitHub blocking
relationships record the approved edges. This specification and its calibration
changed no production runner, FIFO state or tracked priority.
