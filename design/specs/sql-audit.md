# Internal SQL diagnostics

Status: implemented contract, 2026-10-24. The maintainer accepted the latest-call
record, internal-only accessor option, and detailed outcome/condition view.
`include_result` is implemented by #790; #791 adds `details` and the owned
operation observer across the internal recording sites below.

## Current requirement

The caller wants to inspect auxiliary SQL that a public marginplyr call uses
internally and that cannot be recovered merely by viewing the final result's
SQL. Examples are the observed Margin label collision check, zero-row metadata
retrieval, dialect probes, and their control statements. The maintainer explicitly
said those internal queries are sufficient for this task.

This replaces the earlier pursuit of later `collect()` / `pull()` / `compute()`,
connection-wide observation, or automatic tracking of derived results. Those
paths were considered and are excluded by the new request, not silently omitted.
The [superseded discussion](sql-audit-broad-scope-2026-10-24.md) preserves prior
agreements and proposals. Its automatic SQLite storage, driver-admission,
general bound-value retention, and stop-on-storage-failure choices are not
requirements of this smaller diagnostic feature. The latest agreement replaces
that persistence plan with the existing latest-call lifetime and explicit
caller-controlled export.

The scope is SQL prepared for package-owned auxiliary work in Margin
construction and Grouping-plan inspection. It includes a statement generated
through dbplyr for that work, not just SQL text literally constructed in R/.
It does not promise every private driver/network statement, arbitrary caller
callback SQL, or a server audit. Final result SQL remains available through the
existing accessor for compatibility, but is distinct from internal execution.

## Established implementation facts

The existing interface already addresses the core SQL-viewing need:

- `options(marginplyr.audit_sql = TRUE)` enables recording before a call.
- `last_sent_queries()` returns that call's two-column `purpose` / `sql` tibble.
- `last_sent_queries(include_result = FALSE)` selects internal rows.
- `last_sent_queries(details = TRUE)` selects stored outcomes and conditions.
- Only the `result` purpose is a stable public value. Excluding it selects
  internal rows without treating other current purpose names as fixed API.
- Each independent tracked call replaces the last record. A failed call leaves
  its own earlier rows readable. This is not an accumulated history.
- A nested call owns the record while active; its parent regains its own record
  afterwards. The accessor does not retain the entire nested call tree.
- `.data` is evaluated before the current record starts. Enabling the option
  afterwards does not reconstruct an earlier call, and reading never reruns it.

| Internal work | Existing purpose, descriptive rather than a stable enum | Capture point |
| --- | --- | --- |
| Observed Margin label collision | `observed_label_collision` | Render before internal `collect()` |
| Zero-row selection/type metadata | `selection_proxy` | Render before internal `collect()`; the SQL path is DuckDB |
| Share dialect question | `share_dialect` | Render before internal `collect()` |
| Numeric control after a failed dialect question | `share_dialect_control` | Render before internal `collect()` |
| RPostgres probe savepoint / rollback / release | `share_dialect_transaction` | Literal before `dbExecute()` |

These are facts from `R/sent-queries.R`, `grouping_selection_proxy()`,
`check_observed_label_collision()`, `probe_share_dialect_answer()`, and
`execute_share_probe_control()`. The
[recording investigation](../../investigation/sql-audit-recording-2026-10-13.md)
records the inventory and public-document discrepancies.

The label check does not fetch all unique values. It aggregates collision
counts in the database and retrieves one result row, with a count per checked
column. A small result can still require examining source data. Declared factor
levels, missing labels, or no remaining columns can avoid that query. Recording
does not turn the check on; `.check_margin_label = TRUE` requests the check.

The SQL is a prepared representation, not interception of actual DBI arguments
or proof of receipt/completion. The detailed view adds observed outcomes and
condition summaries. Render errors leave `NA` with a render summary; handled
probe errors remain visible at their owned invocation boundary. Cached probes
are not rerun merely to populate diagnostics. Non-SQL work has no SQL rows.

## Internal SQL workflow

```r
old <- options(marginplyr.audit_sql = TRUE)
tryCatch({
  q <- summarize_with_margins(
    sales_db,
    total = sum(amount, na.rm = TRUE),
    .grouping = rollup(region),
    .check_margin_label = TRUE
  )
  queries <- last_sent_queries()
  internal <- last_sent_queries(include_result = FALSE)
  print(internal$purpose)
  writeLines(internal$sql[!is.na(internal$sql)])
  # Rows with NA SQL remain visible in `internal`; they are render failures.
}, finally = options(old))
```

After a tracked call fails, read `last_sent_queries()` from an error handler or
at the next prompt before another tracked call replaces it. Restoring the option
does not alter the last call's saved recording setting. Do not silently discard
rows with missing SQL when presenting a supposedly complete diagnostic record.

## Accepted interface and lifetime

Reuse the existing option and accessor instead of adding connection setup,
start/stop sessions, a separate SQL logger, or replacement retrieval functions.
Keep recording off by default, and preserve the existing exact-TRUE activation
and entry-time setting. A bad option value remains inactive rather than changing
the query's behavior; the accessor explains when the call was not recorded.

The signature is `last_sent_queries(include_result = TRUE, details = FALSE)`:

- The no-argument call retains its existing shape and meaning.
- `include_result = FALSE` is a convenience for the existing result exclusion.
  It returns the internal SQL without requiring knowledge of unstable purposes.
- `details = TRUE` adds diagnostic fields to the tibble. It does not execute
  SQL or regenerate the record on access. Detail is captured while the existing
  audit option is enabled; the accessor argument selects a view afterwards.
- Both arguments accept one nonmissing logical value. Invalid accessor
  arguments raise a Package condition without altering the saved record.
- `include_result = FALSE` removes only the unexecuted result row; it retains
  internal rows with failed rendering or failed invocation. The empty result
  has the selected view's column types. No rows are added for a cache hit or
  for a check that did not request SQL.

Retain only the most recent call's own record and active parent
state in memory, following ADR 0027's existing ownership. A caller may copy or
export it before the next call. There is no automatic file, storage dependency,
crash-recovery guarantee, or accumulated history in this feature. This bounds
history lifetime, not the size of a single SQL statement, diagnostic, or nested
call stack. Process-local ownership and the existing parallel-worker boundary
remain unchanged. A caller-held copy is not changed by a later call.

The no-argument accessor still refuses when no tracked record exists or the
record's saved audit setting was off. An audited call with no recorded SQL
returns zero rows, including after validation refusal. Zero rows establish
neither that the public call succeeded nor that caller code did no work.
Preserve input forcing and nested ownership: if `.data` fails before the outer
record begins, the accessor still describes the previous owner, not that failed
outer call. During B nested in A, B is visible; after B exits, A is visible.
This is the current call's own diagnostic, not an accumulated call-tree trace.

## Detailed view

The original request asked that success, errors, and warnings be considered.
The final agreement includes those details for the owned internal operations.
The detailed tibble contains the following columns in order:

| Field | Meaning |
| --- | --- |
| `purpose`, `sql` | Existing character columns; missing SQL stays `NA_character_` |
| `source` | Character: `render` for an attempted query render, or `literal` for a SQL string supplied by the package |
| `status` | Character: the observed state of the package-owned operation, defined below |
| `conditions` | List column of ordered condition-summary tibbles; empty when none were observed |

Each condition-summary tibble contains `classes` (a list column of character
class vectors), `message` (character diagnostic text), `phase` (`render` or
`invocation`), and `handling` (`handled`, `suppressed`, `propagated`, or
`unknown`). Retain observed errors, warnings, and interrupts. Handling describes
the owned observation boundary only: propagated does not mean uncaught by the
user later. State handling only when known. Do not retain whole condition
objects, backtrace environments, database results, or connection objects.

| Status | Meaning |
| --- | --- |
| `prepared` | A row exists, but no owned backend invocation has been observed; rendering may have failed, as its SQL and conditions show |
| `invoking` | Control reached the invocation boundary immediately before the owned backend call and no terminal observation exists yet; this can be read while a call is active |
| `returned` | The owned operation returned normally, possibly with warnings |
| `errored` | An error was observed leaving the owned operation, even if package logic later handled it |
| `interrupted` | An interrupt was observed leaving the owned operation |
| `unknown` | The owning call has unwound without an established terminal observation for an invocation attempt |

Record the row before the owned invocation and update its observed status when
that operation exits. A recorded render is not an actual-send acknowledgement.
Likewise, `invoking` marks an attempt at the package-owned boundary, not proof
that a driver body ran or a request reached the server.
For the compatibility `result` row, status is prepared, never executed merely
because the public call returned. A render failure keeps missing SQL and its
condition without causing a second query or changing normal warning delivery.
Source records how SQL was obtained or attempted; it does not assert render
success or actual DBI submission. A later invocation outcome is independent
of diagnostic rendering, so missing SQL must not force status to `errored`.
Never convert an unmatched invocation to success merely because unwinding ended.

A successful label-count query followed by a collision refusal is a successful
internal query and a refused public call, not a failed SQL execution. A dialect
probe can error and be handled as an expected capability test. Warnings remain
separate from normal return/error status. Observation must be inside existing
probe handling when needed to retain conditions that are otherwise muffled.
No automatic EXPLAIN, extra fetch, or changed caller warning settings is allowed.
Do not replay a suppressed condition or emit the same warning again while
reading the record. With warning-to-error settings, status follows the actual
observed unwind rather than a separate diagnostic evaluation.

The focused acceptance checks below delimit this implementation. It is
not enough to wrap the public call in an outer handler and claim all internal
conditions were observed. Validation that runs no internal SQL has no SQL row;
the feature is not a general R-expression logger. Cache and no-SQL explanations
belong in the usage contract; they must not masquerade as executed statements.
Public-call validation errors are not attached to an unrelated SQL row. In
particular, `returned` on the collision-count query is compatible with the
subsequent collision error seen by the caller; it does not report success of
the entire public call. General public-call diagnostics are outside this view.

The observer covers the package-owned render and invocation boundaries,
including internally handled probe conditions and cleanup invocations. It
does not claim every driver-private condition or wire-level statement. Preserve
the original value, error propagation, cleanup behavior, and warning delivery.
Each existing package-owned recording site must have an explicit completion and
condition-observation path; an outer handler alone is not that evidence.

## Acceptance scenarios for the implementation

| Scenario | Required observation and compatibility |
| --- | --- |
| Default accessor | Same two character columns, recording order, purposes, and ownership as before |
| Internal-only view | Removes `result` without removing failed or missing-SQL internal rows |
| Normal label-count query | Render source, one internal operation, `returned`; no extra query caused by recording |
| Label collision refusal | Count query still `returned`; public call raises its original collision error; earlier rows remain readable |
| Zero-row metadata | Records the SQL and owned collection outcome without implying a source-row scan or fetched source data |
| Probe raises and control succeeds | Separate rows and conditions; handled probe error is not relabelled as public-call failure |
| Probe cache or no columns to check | No artificial query row and no forced probe execution |
| Render fails | Missing SQL and render condition retained; normal invocation/error policy unchanged |
| Warning and warning-to-error | Condition summary belongs to the correct phase; original delivery, suppression, and resulting status preserved |
| Invocation error, cleanup error, or interrupt | Record each observed owned operation separately; preserve original control flow; unknown completion is not fabricated |
| Nested A/B, including B failure | Active-owner semantics, saved settings, and A's own row updates survive B without leaking B's rows into A |
| Inactive recording and later activation | No retrospective reconstruction; accessor's existing refusal remains |
| Data-frame, dtplyr, Arrow, or validation-only call | No fabricated SQL; zero-row view retains its schema |
| Later collect, pull, compute, or independent SQL | Does not change the construction/inspection record |
| Getter called twice or saved before a later call | Read-only stable view and an independent caller-held copy |

Use live backends where a path requires them: SQLite for the label-count case,
DuckDB for SQL selection metadata, and RPostgres for savepoint controls. These
are validation fixtures, not a new three-driver whitelist. Retain existing SQL
backend coverage and optional-dependency guards. Controlled condition fixtures
can establish render failures, warnings, and interrupts without claiming that
an ordinary database must produce them. Driver-private behavior beyond these
boundaries is not established by these checks.

## Documentation contract

- Make the accessor page the single contract: how to enable before a call,
  view internal SQL, distinguish the unexecuted result, read after failure,
  and save the record before replacement.
- Replace claims such as “what a call actually sent” and “in the order it sent
  them” with prepared representations and recording order. Missing SQL means
  rendering failed, not a proven absence of a SQL form.
- Update the shared query-policy explanation inherited by the Margin verbs
  and the package overview, as well as the accessor page. Their explanations
  must describe the same observation boundary; keep the already-identified
  withdrawn exactness claim about `is.data.frame()` out of the revised wording.
- In the database guide, show `.check_margin_label = TRUE` and zero-row
  metadata separately from `show_query(q)`. Explain that the former computes
  counts, that optional/cached work may issue no query, and that reading the
  record does not activate or repeat that work.
- Link inspection's possible zero-row metadata request to the accessor. Keep
  SQL/non-SQL and backend-specific examples accurate. Later retrieval stays
  explicitly outside this feature.
- Preserve the current return type and reentry behavior unless an explicit
  later decision changes them. Regenerate Rd from roxygen if public sources
  are edited.

ADR 0037's 2026-10-24 amendment owns the narrowed scope and interface decision;
this specification owns the detailed contract and acceptance cases. The match
with the earlier local-diagnostic direction is not evidence that its extension
exists or that a particular observer has passed those cases.

## Decision record and delivery boundary

The maintainer accepted Q12 and Q13's recommendations on 2026-10-24: latest-call
memory with caller-controlled export, the existing option/accessor with an
internal-only view, and opt-in detailed display of internal outcomes and
conditions. This resolves the remaining product choices for this specification.
The exact column/state definitions above make that accepted direction concrete;
they do not assert implementation or empirical completeness.

The implementation adds public diagnostics and tests without a storage or
connection dependency. It establishes the owned observation boundary rather
than driver-private wire behavior. No performance, crash-durability, or
server-level audit guarantee has been established or adopted.

The approved implementation slices are tracked in GitHub:

1. [#790](https://github.com/sayuks/marginplyr/issues/790) adds the internal-only
   view while preserving the default two-column output.
2. [#791](https://github.com/sayuks/marginplyr/issues/791) adds the detailed
   outcome and condition view across all owned recording sites.

Each slice includes its tests and public-document updates.
