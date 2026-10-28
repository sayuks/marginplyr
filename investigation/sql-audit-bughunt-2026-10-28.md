# SQL audit boundary stress investigation

Investigated: 2026-10-28
Repository snapshot: `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

## Result and scope

No new reproducible defect was established. The investigation used a new
worktree from `main`; fetching `origin/main` confirmed the same snapshot.
Three parallel investigations covered conditions and rendering, record
ownership and processes, and actual internal SQL. The coordinating process
reviewed their assertions, independently reran selected controls, and reran
the existing focused suite and differential harnesses. No package source,
package test, public contract, dependency, or generated document was changed.

The maintainer requested the `to-tickets` workflow and delegated ticket
granularity and publication. Candidate findings were checked against the
contract and issue history before ticketing. No new issue was published:
the confirmed historical defects were closed, and the additional candidates
below did not establish another repair slice. No existing issue was modified.

The observation boundary came from [Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
[ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
It covered construction/inspection records, prepared result SQL, internal
label counts, zero-row metadata, and dialect/probe controls. Later collection
was a comparison control and was checked for record stability; it was not
treated as part of the construction record. Arbitrary callback SQL,
driver-private statements, server receipt, durable history, and crash
recovery were outside this feature's contract.

The [companion runbook](2026-10-28-sql-audit-bughunt/README.md) describes the
retained scripts, observations, commands, and expected exits. All database
inputs were synthetic. Fresh scripts loaded the checkout selected by their
working directory. The snapshot above identifies the implementation tested;
passing finite matrices did not establish exhaustive correctness.

## New boundary checks

### Condition combinations and recovery

The new public SQLite fixture compared auditing off/on for all 64 ordered
nonempty subsets of `error`, `warning`, `interrupt`, and `message`, signalled
with `signalCondition()`, `warning()`, or `stop()`, under `warn = -1/0/1/2`
and three outer handling modes. All 2,304 pairs agreed on returned classes or condition diagnostics,
delivered classes/messages, and cleanup actions. The detailed view retained
one invocation summary per observed severity-bearing leaf, with its phase
and boundary handling. Unambiguous error-only/interrupt-only terminal
outcomes and normal returns had their expected statuses. A compound
error/interrupt class was not used to assert one unique terminal category.

An additional 16 pairs placed recoverable bare or R-generated interrupts in
the backend body or its `on.exit()`, followed by normal return, restart escape,
an error, or another interrupt. Caller behavior and cleanup order agreed;
the observed terminal status was respectively `returned`, `unknown`,
`errored`, or `interrupted`. Controlled backend bindings supplied these
conditions through the public Margin verb; a stock driver was not claimed
to emit every synthetic class combination.

A further 24 pairs injected recovery conditions before/after an actual SQLite
fetch or in its wrapper cleanup. Both modes crossed one send, fetch, and clear
wrapper; send/clear forwarded to SQLite, while terminal pre-fetch exits did
not forward fetch. Cleanup, ordered condition counts, and terminal statuses
agreed, and a subsequent `SELECT 42` verified connection usability. These
fixtures distinguished reaching a DBI boundary from completing its operation.

### Ownership, getters, and workers

The public accessor matrix checked 36 valid argument pairs, including named
and attributed logical values, and 14 invalid arguments. Saved records were
compared by serialization after each access or refusal. A dbplyr translation
extension recursively entered public Margin calls to depth eight, retaining
510 active snapshots. Parent ownership, entry-time settings, inactive child
calls, caught child refusals, later replacement, and caller-held copies passed
2,173 assertions. No implementation binding was replaced by this fixture.

Two workers each under fork, callr, and PSOCK constructed independent SQL
and empty local records. Fresh workers' initial no-record refusal was checked
by class and message; fork workers inherited the pre-fork snapshot. Every
worker left the parent's serialized record unchanged. The process matrix
passed 31 assertions. The SQL in these ownership fixtures used dbplyr
simulators, not a live database. PSOCK required permission to open a temporary
local socket in the execution environment.

### Live SQL boundaries

The new SQLite/DuckDB/PostgreSQL matrix compared 726 off/on pairs. It crossed
35 special column names and three public operations with safe/colliding
source values, 13 labels with safe/colliding values, and six SQL source types.
The names included `.env`, `.data`, Boolean words, function names, diagnostic
field names, quotes, whitespace, and Japanese text. The 477 successful calls
and 249 expected collision refusals agreed on collected row multisets,
conditions, and internal SQL at the generic DBI boundary.

Each audit setting reached 758 internal `dbSendQuery()` crossings: 516 label
counts and 242 zero-row metadata reads. The detailed view recorded these as
`returned` and the 267 result rows as `prepared`, all without condition
summaries. Explicit later collection left those records unchanged. The SQL
comparison was client-side generic-boundary evidence, not a network trace or
a server-submission guarantee.

Another 13 off/on pairs covered missing SQL functions, dropped source tables,
PostgreSQL server notices, statement timeout, a read-only caller transaction,
and an already-aborted transaction. Their 18 internal rows were nine `errored`
and nine `returned`, with five unexecuted `prepared` results. The nine errors
retained invocation summaries with the expected boundary handling. The cold
read-only transaction probe retained its savepoint, failing question,
rollback, numeric control, rollback, and release independently; the caller's
transaction remained usable. The aborted transaction's failed savepoint was
recorded as handled and was followed by the original public refusal.

PostgreSQL's `RAISE WARNING` printed a server notice through RPostgres rather
than signalling an R warning in these runs. The empty condition summaries
therefore did not establish real-driver R-warning capture. The synthetic
condition matrices supplied that separate observation check. PostgreSQL was
17.11 in a disposable Unix-socket-only cluster, stopped after each run.

## Fresh reruns of existing controls

The host used R 4.6.1 on arm64 macOS. The retained version transcript includes
DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5, RPostgres 1.4.10, dplyr 1.2.1,
dbplyr 2.6.0, rlang 1.3.0, lifecycle 1.0.5, Arrow 25.0.1, dtplyr 1.3.3,
testthat 3.3.2, and pkgload 1.5.3.

| Control | Observed result |
| --- | --- |
| Focused package suite | All 18 Sent-query/SQL-audit files completed, exit 0, without test failures, warnings, or skips |
| SQL simulators | All 528 off/on pairs across 16 exported simulators agreed on results, conditions, later SQL, and RNG state |
| Seeded live differential | All 480 SQLite/DuckDB pairs agreed: 288 successful collections and 192 matching refusals or collection errors |
| Non-SQL differential | All 432 pairs passed across Arrow, dtplyr, and data.table input shapes |
| Arrow readers | All 48 pairs passed, including nonconsumption and record isolation |
| Storage/copies | Nine formatter cases, 45 cleared weak references, 405 record replacements, 11 inactive settings, 26 invalid accesses, and two active copies passed |
| Native SIGINT stress | 10,000 iterations, 9,986 caught interrupts, no active owner after exit; independent records and saved settings passed |

The initial focused-suite attempt loaded only the parent process's checkout.
Subprocess fixtures then found an older installed marginplyr without `details`
and failed. The corrected run installed this checkout into a fresh isolated
library and propagated it through `R_LIBS`; its terminal exit was 0. The
framework's expected `Interrupting test` notification did not prevent the
controlled recovery assertions from running. The stale-install attempt was
not counted as a package defect or passing evidence.

The non-SQL and reader runs printed three Arrow CPU-cache `sysctl` permission
diagnostics during initialization. Their matrix assertions completed with
exit 0. Native interruption counts depended on scheduling; the stress check
established its observed invariants, not coverage of every delivery window.

## Candidates not promoted to tickets

- A SQLite column declared with `COLLATE NOCASE` holding `"total"` was refused
  as colliding with Margin label `"Total"`. Disabling the check returned both
  spellings. This did not establish a defect: SQL value equivalence follows
  the database's comparison relation, a distinction also retained by
  [ADR 0010](../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md).
  R string inequality alone did not establish that these keys were distinct
  under the input's comparison semantics. Independent `BINARY`, `NOCASE`, and
  `RTRIM` controls compared the actual SQL equality answer, checked/unchecked
  public outcomes, returned count status, and saved-record stability.
- Unsorted DuckDB rows could arrive in different orders across off/on runs.
  Row multisets, SQL, and conditions agreed. No ordering failure was inferred
  from a case that did not request Margin order.
- Compound severity pilots needed a harness-specific outer restart for
  `stop()` of a condition lacking both error and interrupt classes. This
  made the comparison runnable without treating a default R exit as a
  product defect. Final captures used the corrected fixture.
- Previously reported formatter cancellation and recovered-error/nonlocal-exit
  candidates remained bounded by the
  [2026-10-27 investigation](sql-audit-bughunt-2026-10-27.md).
  No new public reproduction resolved those earlier limitations.

The prior SQL `.env` collision defect
[#930](https://github.com/sayuks/marginplyr/issues/930) was already closed.
Fresh special-name checks reached the repaired internal count and its normal
acceptance/refusal paths; the old finding was not republished.

## Repository verification and limits

Both linters (`jarl check .` and package-aware lintr after `load_all()`),
the context-budget verifier, and the document-reference verifier passed.
The companion manifest identifies the delivered scripts and synthetic
observations. Saved backend captures were independently revalidated without
a running server, and the coordinator also repeated the complete new live
runner, ownership/process fixtures, cleanup matrix, and DBI recovery matrix.

This repository-only change added dated investigation material under the
existing package exclusion. The package-affecting Review-ready check was not
required by [the local-check policy](../design/agents/local-checks.md).
No CI run was monitored. The investigation did not establish behavior on all
DBI drivers, other operating systems or R versions, every cancellation timing,
every extension method, or a performance bound.
