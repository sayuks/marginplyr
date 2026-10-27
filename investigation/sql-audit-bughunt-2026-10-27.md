# SQL audit bug hunt

Investigated: 2026-10-27
Repository snapshot: `13dd7c30ac38fe3fc82e74c561d8795a6c4b6d17`

## Result and scope

One P2 defect was reproduced in an internal query observed by SQL auditing:
observed Margin label collision checking failed for a SQL grouping dimension
named `.env`. It was published as
[#930](https://github.com/sayuks/marginplyr/issues/930), with `bug` and
`ready-for-agent` labels and no blockers. The SQL observer correctly recorded
that failure. No new defect in recording, ownership, or condition observation
was established by this run.

The investigation used a new worktree from `main`. Fetching the remote main
ref confirmed the snapshot above. Three parallel roles examined lifetime and
reentry, condition observation and rendering, and internal SQL/backend work.
The coordinating process independently reproduced the finding on SQLite,
compared audit-on/off behavior, reran the focused suite, and checked issue
history. No package implementation, package test, dependency, generated help,
or public contract was changed.

The boundary came from [Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
[ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
It covered prepared result SQL and owned auxiliary operations: observed label
counts, zero-row metadata, dialect questions, numeric controls, and PostgreSQL
savepoint controls. Later collection/materialization, callback SQL,
driver-private statements, server receipt, and durable history were outside
that boundary. Explicit collection was used as a comparison control and checked
for changes to the saved construction record.

The [companion runbook](2026-10-27-sql-audit-bughunt/README.md) describes the
retained scripts, evidence, fresh-output arguments, and expected exits.
The scripts load the checkout from which they run; the snapshot above names
the investigated implementation. These were finite checks, not a proof of
exhaustive correctness.

## F1: collision checking failed for a SQL `.env` dimension

A live SQLite source needed only one column and one row:

```r
input <- dplyr::copy_to(con, data.frame(.env = "a"), "env_minimal")
summarize_with_margins(
  input,
  rows = dplyr::n(),
  .grouping = rollup(dplyr::all_of(".env")),
  .check_margin_label = TRUE
)
```

The expected result had detail `.env = "a"` and Grand total `.env = "Total"`,
each with count one. Construction instead raised `Failed to interpolate {x}`,
caused by `Cannot translate a <rlang_ctxt_pronoun> object to SQL.` The same
failure replaced the intended collision refusal when the source value was
`"Total"`.

| Control | Observation |
| --- | --- |
| Audit disabled or enabled | Same failure with checking enabled |
| Checking disabled | Correct rows were collected |
| SQLite, DuckDB, PostgreSQL | Failure reproduced on all three live backends |
| Summarization and expansion | Both failed at the internal label count |
| Quoted SQL collision count | Returned zero for `"a"`, one for `"Total"` |
| Ordinary name or `.data` | Checking succeeded |
| NULL/NA Margin label | No observed collision query was required |

The generated aggregate in `check_observed_label_collision()` used
`margin_safe_column_reads()`, which left this SQL input's name unchanged, and
`rlang::data_sym()` to construct the column read. The resulting expression
reached dbplyr's pronoun interpretation. A direct dplyr count with either
`.data$.env` or `.data[[".env"]]` also failed, including with an ordinary output
alias; the evidence therefore did not establish changing `$` to `[[` as a
repair. A quoted SQL count succeeded. This isolated the package-generated
query from database identifier support and from later result labelling.

The audit-on failure retained the collision row with missing SQL and status
`errored`. Its two summaries described the handled diagnostic render failure
and the propagated invocation failure. The defect was in the requested query,
not an audit-dependent change in behavior. A repaired query would return before
any subsequent collision refusal, which belongs to the public call rather
than the returned internal count. ADR 0020's requested collision-check contract
was the applicable query policy.

The special-name matrix contained 216 observations across nine names, three
backends, two verbs, audit on/off, and checking on/off. `.env` with checking on
failed in all 12 applicable observations. `...` and `..1` encountered existing
dplyr name refusals with checking either on or off and were not counted as a
new defect. Ordinary names, `.data`, Boolean-word names, `T`, and `F` supplied
successful controls.

The coordinator's independent public-contract replay retained eight SQLite
observations and verified the raw SQL and ordinary-name controls. Four desired
behaviors failed: noncolliding acceptance and collision refusal, each with
auditing off and on. Its final assertion exited 1 on the investigated snapshot.
The replay and its captured result are retained in the companion directory.

Open #928 required Boolean-word and `.env` source columns together and failed
at source preparation even with checking disabled. F1 needed only `.env`,
required checking, and affected three SQL backends. Closed #702 concerned local
label conversion, and #731 concerned SQL result-key preservation. These were
checked with their complete issue bodies and comments; neither supplied a
working internal collision aggregate for this case. The existing SQL `.env`
result tests exercised lazy inputs without enabling the optional observed
collision query.

The maintainer delegated ticket granularity and publication. The `to-tickets`
workflow therefore produced one independently verifiable repair slice, #930,
without a confirmation round. The slice includes public behavior, the actual
owned SQL path, audit compatibility, and regression checks. No prefactor or
blocking ticket was needed, and no pre-existing issue was modified or closed.

## Passing controls

The host used R 4.6.1 on arm64 macOS. The retained version transcript includes
dplyr 1.2.1, dbplyr 2.6.0, DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5,
RPostgres 1.4.10, Arrow 25.0.1, dtplyr 1.3.3, data.table 1.18.6.1,
rlang 1.3.0, lifecycle 1.0.5, testthat 3.3.2, and pkgload 1.5.3.
The disposable PostgreSQL server reported 17.11.

| Check | Observed result |
| --- | --- |
| Focused existing suite | All 18 Sent-query/SQL-audit files completed, exit 0, without failures, warnings, or skips |
| New seeded SQLite/DuckDB differential | 480 off/on pairs agreed: 288 successful collection pairs and 192 matching refusals or collection errors |
| Existing simulator harness rerun | All 528 pairs across 16 exported simulators agreed on construction, conditions, later rendering, and RNG state |
| New backend SQL correspondence matrix | 135 cases agreed on internal SQL versus generic `dbSendQuery()` crossings, observed status, and empty condition summaries |
| New special-name matrix | 216 observations isolated F1 and the independent name-refusal controls described above |
| New public lifetime matrix | 31 cases passed across local/SQLite/DuckDB inputs and six public spellings |
| New non-SQL differential | 432 off/on pairs passed across nine input shapes, six spellings, and eight scenarios |
| New Arrow reader differential | 48 pairs passed, including accepted inspection and refused construction without consuming the reader |
| New SQL-parent/non-SQL-child reentry | 216 cases preserved the existing DuckDB metadata row, parent ownership, and saved audit setting |
| New compound probe conditions | 48 off/on pairs agreed across overlapping severity classes, three signal forms, and four warning levels |
| Existing native SIGINT stress | 10,000 iterations, 9,975 caught interrupts; no active owner remained after exit; later records/settings passed |

The seeded differential covered summary/expansion, rollups, cubes, products,
duplicate grouping sets, source missingness, missing/string Margin labels,
three valid Margin orders, optional collision checks, and Total shares with
caller-established source eligibility. It compared collected row multisets,
warning class/text, and error class/text/phase. It also checked that collection
did not alter the saved record. It did not establish ordering within tied keys.
An earlier pilot passed logical values to the string-valued `.sort` argument;
those validation-only comparisons were excluded from the reported 480 pairs.

The backend correspondence matrix covered ordinary, factor, empty, missing,
and Japanese-named inputs with summarization, expansion, inspection, and three
label policies. Legitimate public refusals were included. A cold PostgreSQL
probe in a caller transaction separately retained seven internal rows: label
count, savepoint, failing dialect question, rollback, numeric control,
rollback, release. The probe had `errored`/`handled`; the controls returned, and
the transaction remained usable. DBI generic traces were client-side evidence:
`dbSendStatement()` forwarding to `dbSendQuery()` produced duplicate trace
entries for each control, not extra wire submissions. The disposable server
was stopped after the run.

The non-SQL shapes were Arrow Table, RecordBatch, Dataset and queries over
each, immutable/mutable dtplyr steps, and data.table. The comparisons included
ordinary results and refusals, typed selections, nesting, input immutability,
empty default/detailed schemas, later SQL calls, and both SQL/non-SQL reentry
directions. Reader cases additionally checked nonconsumption. Arrow's
unspecified row order was compared as a multiset. Its initialization printed
three sandbox-denied CPU-cache `sysctl` diagnostics; all matrix assertions
completed successfully.

The compound-condition fixture supplied controlled backend conditions rather
than claiming a stock driver emitted them. Warning/interrupt overlaps were
compared under `warn = -1/0/1/2` and `stop()`, `signalCondition()`, and
`warning()`. Existing suppression and error handling explained the recorded
`returned`, `interrupted`, and `errored` outcomes without a new discrepancy.

The initial focused-suite attempt loaded the checkout only in its parent
process. Child fixtures found an older installed marginplyr without `details`
and failed. Installing this checkout in an isolated library and putting it
first in `.libPaths()` corrected that mismatch; the retained final suite used
that installation. The test framework's expected `Interrupting test` message
was checked against returned test results: the relevant assertions ran.

## Bounded and rejected candidates

- A recovered error followed by an unrelated exit at the private observer seam
  did not establish a public reproduction: the Margin error boundary unwound
  the recovery first.
- A condition formatter that raised a new interrupt could stop diagnostic work.
  The exercise did not distinguish an injected formatter cancellation from a
  user cancellation that must propagate, so it was not ticketed.
- Mutating a getter's shared R storage with `data.table::set()` bypassed ordinary
  copy-on-modify semantics. Standard assignment/copy and later-call behavior
  passed; no separate supported API defect was established.
- Mocked render returns of zero or multiple SQL strings did not establish a
  legal return from the SQL-select extension path used by these operations.
  Unsupported renderer output was not promoted into a package finding.

No performance bound, crash recovery, server-completion guarantee, or safety
for every native cancellation timing was established. Repairs and post-fix
regression tests were left to #930, as requested. No CI run was monitored.
