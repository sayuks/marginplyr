# SQL audit extension and configuration bug hunt

Investigated: 2026-10-26
Repository snapshot: `6b91254016dbcce60cc2cc69071f01f69eb8084e`

## Findings and scope

Two defects were reproduced through public entry points after the SQL-audit
repairs investigated on 2026-10-25. Both required unusual user extensions or
configuration, so both were assessed as P3. No package implementation, existing
test, dependency, generated documentation, or public contract was changed.

| Finding | Evidence | Repair ticket |
| --- | --- | --- |
| Diagnostic SQL rendering delivered additional messages | Real SQLite connection/dialect extension, ordinary and bare message signals, one message off versus four on | [#858](https://github.com/sayuks/marginplyr/issues/858) |
| A malformed audit option executed an S3 method and failed inspection | Classed FALSE with an `is.na` method, local public inspection, ordinary FALSE control succeeded | [#859](https://github.com/sayuks/marginplyr/issues/859) |

The tickets were independent, complete repair slices with no blocking edges.
Neither required a preliminary refactor. They were published with `bug` and
`ready-for-agent` labels using the `to-tickets` workflow; the maintainer had
delegated ticket granularity and dependencies without a confirmation step.

The observation boundary came from
[Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
[ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
Later collection/materialization, arbitrary callback SQL, driver-private
statements, persistence, and server receipt remained outside that boundary.
The probes did collect results separately as comparison controls, then checked
that those later operations left the construction record unchanged.

The [companion directory](2026-10-26-sql-audit-extensions/) contains assertion
reproductions, comparison scripts, and captured synthetic observations. The
scripts load the checkout selected at execution time; the snapshot above
identifies the code investigated. Commands below run from the checkout root.

## Diagnostic rendering delivered additional messages

The live fixture extended an ordinary in-memory SQLite connection through
dbplyr's connection/dialect dispatch. Its aggregate translator signaled a fixed
message condition and then forwarded every argument to the original SQLite
SUM translator. It replaced no package or driver function and preserved the
generated aggregate SQL.

The same public `summarize_with_margins()` call requested an observed Margin
label collision check. With auditing disabled, only the real internal count
query translated SUM and delivered the message. With auditing enabled, its
diagnostic render and the compatibility result render delivered additional
messages. The result render translated SUM once for each of two grouping sets.

| Signal spelling | Messages with auditing off | Messages with auditing on | Collected values |
| --- | ---: | ---: | --- |
| `message(leaf)` | 1 | 4 | `a = 1`, `b = 2`, `Total = 3`, identical |
| `signalCondition(leaf)` without a muffle restart | 1 | 4 | Same identical values |

Every delivered condition was identical to the fixture's original leaf. Lazy
result classes agreed. The detailed view held `returned` for the internal
count query and `prepared` for the result, with empty condition-summary
tibbles. Messages are outside that stored severity schema; the defect was
their additional delivery, not an omitted message-summary row.

The reproductions isolated diagnostic delivery from backend work and result
mutation: values, classes, and original condition identity agreed before the
final transparency assertion failed. `render_sent_query_sql()` handled
interrupts, warnings, and errors but installed no message isolation. This
explained both the extra delivery and the empty summaries without requiring
another backend query. The successful standard-backend comparisons below
limited the conclusion: no stock SQLite translator was shown to emit this
message. P3 reflected that extension requirement.

```sh
Rscript investigation/2026-10-26-sql-audit-extensions/render-messages.R ordinary
Rscript investigation/2026-10-26-sql-audit-extensions/render-messages.R bare
```

Each command exited 1 at `identical(off$messages, on$messages)` on the
investigated snapshot, after printing `1` off and `4` on. An optional second
argument writes an RDS observation to a fresh path before the assertion.
Both modes were independently repeated by the coordinating process.

The repair ticket requires diagnostic-only message isolation while preserving
actual invocation messages, existing compound-condition/cancellation behavior,
and restart ownership. It does not add messages to the diagnostic schema.

## A malformed option executed user code

The smallest reproduction used only a local data frame and
`inspect_grouping()`. Ordinary FALSE left auditing inactive and returned the
Grouping-plan inspection tibble. A classed FALSE whose `is.na` method raised
instead caused `simpleError("audit option method ran")` before inspection.

The entry-time `isTRUE(getOption("marginplyr.audit_sql"))` check in
`reset_sent_queries()` dispatched the class's `is.na` method. The ordinary
FALSE control and absence of any SQL backend isolated this from grouping
selection, SQL translation, or driver behavior. ADR 0027's *Four answers,
distinguished* explicitly required garbage option values not to fail a Margin
call. That requirement did not imply support for arbitrary S3 option classes;
the defect was executing their behavior while deciding whether auditing was
inactive.

```sh
Rscript investigation/2026-10-26-sql-audit-extensions/option-dispatch.R
```

The command exited 1 at the assertion that the classed-FALSE call completed
normally. Its ordinary-FALSE control returned a tibble. An optional argument
writes the observation to a fresh RDS path before the assertion.

A broader fixture covered all six public spellings, local and simulated SQL
inputs, and classed FALSE/TRUE: 24 observations all raised the same method
error. Twenty had successful ordinary-FALSE controls. Four SQL nesting
observations had existing unsupported-backend refusals and were excluded from
the successful-control count. Ordinary TRUE still produced an audited empty
local record; ordinary FALSE retained the accessor's unaudited-call refusal.
The classed configuration and custom method limited this finding to P3.

## Passing controls

R was 4.6.1 on arm64 macOS. The library versions were dplyr 1.2.1, dbplyr 2.6.0,
DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5, RPostgres 1.4.10, Arrow 25.0.1,
dtplyr 1.3.3, rlang 1.3.0, lifecycle 1.0.5, testthat 3.3.2, and pkgload 1.5.3.
Arrow and dtplyr were available but were not given a new live matrix in this
investigation. Four parallel roles examined conditions, ownership, backend
operations, and independent reproduction/comparison.

| Control | Observed result |
| --- | --- |
| Focused existing suite | All 16 SQL-audit/Sent-query test files completed without failures, warnings, or skips after installing this checkout into an isolated library |
| Simulated dialect comparison | All 128 pairs, across 16 exported simulators and eight scenarios, agreed on construction results/conditions and later SQL/conditions |
| New live comparison | SQLite 69, DuckDB 69, PostgreSQL 70: all 208 off/on pairs agreed on compared fields |
| Internal SQL coverage | All 198 internal rows agreed with traced generic DBI crossings after the narrow PostgreSQL normalization below |
| Compatibility result SQL | All 77 result rows agreed with later explicit rendering |
| PostgreSQL caller transaction | Still active after the cold share probe; `SELECT 9` succeeded before explicit rollback |
| Existing public lifetime fixture | All 80 cases passed |
| Existing storage/copy fixture | Nine formatter cases, 45 cleared weak references, 405 replacements, 11 inactive settings, 26 invalid accessor calls, two active snapshot copies passed |

The new live matrix used plain, filtered/mutated, grouped, arranged,
windowed, joined, unioned, literal-query, limited, and empty inputs. It covered
inspection, summary, expansion, contextual shares, nesting refusals, unusual
Unicode/quoted column names, and a cold share probe in a PostgreSQL caller
transaction. It compared construction SQL, warnings, error diagnostics, RNG
state, later SQL, and collected row multisets. All saved records remained
unchanged after explicit later rendering/collection.

Audited rows numbered 66 on SQLite, 123 on DuckDB, and 86 on PostgreSQL:
275 total, comprising 198 internal and 77 result rows. Their statuses were
`prepared`, `returned`, or `errored`. Each label described one matched off/on
scenario; these counts were not counts of distinct SQL algorithms.

The DBI comparison traced generic `dbSendQuery()` and `dbExecute()` crossings,
not network traffic. For the PostgreSQL transaction fixture alone, consecutive
forwarding copies of its four savepoint controls were collapsed. Generated
savepoint suffixes were normalized in the compared observations. No other
missing-statement allowance was made.

Collected data were sorted by all output columns for multiset comparison.
Five DuckDB cases differed in original row order: four unsorted share cases
and expansion ties after a union. This established equality of rows, not Margin
order or ordering within tied keys. The script did not assert a backend's
ordering guarantees from those comparisons. The PostgreSQL 17 instance used
a disposable cluster and Unix socket with TCP listening disabled; it was
stopped after the comparisons.

## Replaying the evidence

Saved observations can be checked without a running database:

```sh
Rscript investigation/2026-10-26-sql-audit-extensions/compare-advanced.R \
  investigation/2026-10-26-sql-audit-extensions/evidence
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/compare-simulators.R \
  investigation/2026-10-26-sql-audit-extensions/evidence
```

Both exited 0. The live comparator required every backend pair and exact
69/69/70 case counts; any data/SQL coverage difference failed it. The retained
live generator accepted backend, `on`/`off`, and a fresh output path, plus Unix
socket and port for PostgreSQL. It assumed a disposable database named
`postgres` for that backend. For example, fresh SQLite observations could be
generated with:

```sh
Rscript investigation/2026-10-26-sql-audit-extensions/live-advanced.R \
  sqlite off /tmp/sql-audit-fresh-sqlite-off.rds
```

The generator's delivery form added explicit connection/trace cleanup and
PostgreSQL location arguments after the original captures. A repeat of all
69 SQLite cases agreed with the original observations. It did not alter the
case expressions or repair package behavior.

For the focused suite, an initial parent-only load failed eight subprocess
scenarios because the ordinary installed marginplyr lacked `details`.
This was an environment mismatch, not another package finding. The corrected
run installed the selected checkout and propagated that library to children:

```sh
audit_library=$(mktemp -d)
R CMD INSTALL --library="$audit_library" .
R_LIBS="$audit_library" NOT_CRAN=true Rscript -e \
  'testthat::test_local(filter = "sent-quer|sql-audit", reporter = "summary")'
```

The successful transcript is retained as `evidence/focused-suite.log`.
The test framework printed an expected `Interrupting test` line for its
controlled recovered-interrupt fixture; the process exited 0.
The lifetime and storage fixtures were the existing `matrix.R` and
`storage-extra.R` under the earlier cancellation/resumption companion's
`controls` directory; their fresh outputs are retained here. The option
matrix, both focused defect captures, backend pairs, simulator pairs, and
comparison transcripts are also retained. `MANIFEST.sha256` hashes the
companion scripts and evidence, excluding the manifest itself. Trailing spaces
were removed from text transcripts; the saved RDS observations were unchanged.

## Rejected or bounded candidates

A recovered error followed by an unrelated nonlocal exit looked capable of
leaving `errored` at the internal observer seam. The public Margin wrapper's
exiting error handler unwound the recovery restart first, so no public
reproduction was established. It was not ticketed.

Formatter-induced interruption/nonlocal exit was not counted: the
[earlier hunt](sql-audit-bug-hunt-2026-10-25.md) had already recorded the policy
distinction between interruption caused by formatting and a user cancellation.
No new evidence resolved it. Standard ownership, promise forcing, copied
views, retention, and cleanup interruption produced no additional defect in
the exercised controls. Passing finite matrices did not establish exhaustive
correctness, server completion, native cancellation safety, or performance.

The investigation used deterministic, failing public-path assertions rather
than implementing repairs. Fix and post-fix regression phases were deliberately
left to the two independent tickets, as requested. No CI run was monitored.
