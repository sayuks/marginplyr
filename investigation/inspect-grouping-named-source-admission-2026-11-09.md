# Grouping inspection: named DuckDB source admission

Investigated: 2026-11-09
Tested main: `5c667c956fd50ba39be621bf1b617a17abda7b14`

A new worktree started from freshly fetched main. Three fresh-context agents
independently generated candidates from implementation, public contracts and
processing-path consistency before sharing their first results. Each executed
against a separate archive of the same SHA. The parent separately prepared
hand-enumerated expectations and small input cases. Two independently repairable
defects were confirmed and filed as [#1525][direct] and [#1526][view]. An opaque
table-function residual was added as [evidence on existing #1506][opaque], rather
than counted as another new cause. No package implementation or test was changed.

## Contracts and preparation

The [inspection][inspection], [constructor][constructors] and
[identity-helper][identity] references supplied the public contracts. Exploration
followed public capture, argument evaluation, nested-name admission, column
selection, metadata acquisition, expansion, normalization, duplicates, identifiers
and formatting, through the corresponding Margin operations. It covered existing
implementation rather than just the latest diff.

The team read `CLAUDE.md`, its `@AGENTS.md` closure, `CONTEXT.md`, the domain and
local-check instructions, and relevant ADRs in full: 0002, 0005, 0007, 0008, 0009,
0013, 0015, 0019, 0020, 0026, 0027 and 0029. Missing ranges after truncated reads
were retrieved. Additional implementation and test reads followed targeted
searches. The recent [evaluated-capture][prior-evaluated], [quoted-factory][prior-csv],
[deep inspection][prior-deep], [systematic inspection][prior-systematic] and
[metadata inspection][prior-metadata] notes and relevant repair history were
examined. Counts from those notes were not counted as this investigation's work.

The tested main included PR #1524. Its full body and review/fix history, and the
related #1506/#1519 bodies and comments, distinguished this investigation from
the previously reported explicit-reader and filename replacement-scan failures.
ADR 0020's relation-admission amendment required following stored-view definitions
before binding where their safety was not established by a name.

## Confirmed: direct captured identifiers skipped admission

Typed inspection of a direct table capture sampled CSV values behind a stored
view. The view was given a filename suffix specifically to separate this path
from the ordinary-name defect below:

```r
library(marginplyr)
library(dplyr)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
p <- tempfile(fileext = ".csv")
writeLines(c("k", "101"), p)
DBI::dbExecute(con, paste0(
  'CREATE VIEW "reader.csv" AS SELECT * FROM read_csv(',
  DBI::dbQuoteString(con, p), ', header=true)'
))
x <- tbl(con, "reader.csv")
options(marginplyr.audit_sql = TRUE)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "(k)"
writeLines(c("k", "source_tail"), p)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "()"
last_sent_queries()$purpose
# "selection_proxy"
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(p)
```

Both typed calls should have refused before inference. View creation and initial
capture were explicit setup; the later edit changed the inspected type of that
same captured input. That established source-value processing during inspection,
independently of the number of returned rows or recorded statements.

`validate_duckdb_csv_query()` visited SQL objects and recursively traversed
lists/calls, but did not submit the captured identifier/table-path carrier to
admission. The equivalent raw SQL `SELECT * FROM "reader.csv"` correctly refused
after catalog-only admission. Thus fixing ordinary-name filtering alone would
leave this entry path untouched. [#1525][direct] retained the minimized public
reproduction, environment, impact, separating control and acceptance criteria.

## Confirmed: ordinary SQL relation names skipped view dependencies

Changing the same setup to `CREATE VIEW ordinary AS SELECT * FROM read_csv(...)`
and capturing `tbl(con, dbplyr::sql("SELECT * FROM ordinary"), vars = "k")`
produced the same `(k)` to `()` change when only the CSV value changed. This form
already entered the SQL walker, so it isolated a second missing boundary.

`validate_duckdb_csv_metadata()` resolved top-level catalog relations only for
CSV/JSON filename-suffix candidates. It never inspected `ordinary`'s definition.
The corresponding filename-suffixed view in raw SQL was correctly refused.
Widening the walker's input carriers would therefore not fix this filter, and
widening the filter would not make a direct identifier enter the walker. The
two tickets had no blocking relationship or established prerequisite refactor.

The parent reproduced direct and raw captures, nested views, renaming,
`filter(FALSE)` and joins, in both `.by` and `.grouping`. All 128 source,
derivation, value and selector observations were recorded separately from the
24 source/audit/value comparisons. Explicit-schema views and ordinary tables
retained stable numeric metadata; direct explicit auto-detected readers refused.
Caller-query serialization stayed unchanged. Name-only inspection succeeded
with zero owned SQL after the source file was removed, including after failed
typed inspection. The cross-backend agent independently reproduced the
ordinary-view witness in its own pinned archive.

Each successful unsafe typed call recorded one returned `selection_proxy` and
no catalog admission. The statement's zero-row limit did not prevent source
inference during binding. DuckDB returned the inferred type correctly; the
defect was marginplyr requesting that work under its stronger inspection
contract. No hosted source, external data, billing effect or large-file timing
was measured. [#1526][view] retained this independently repairable condition.

Existing unsafe-view regressions in `test-duckdb-csv-metadata.R` entered through
raw SQL with filename-suffixed view names. Their source-value controls would
detect unsafe admission on those paths, but did not exercise the direct
identifier or the ordinary SQL view name above. The selected
`test-inspect-grouping.R` passed in a disposable archive with parallel execution
disabled; that passing file did not establish the missing source-work guarantees.

## Existing cause: opaque table-function evidence

A top-level `hidden_reader()` table macro whose body selected `read_csv(...)`,
and `query('SELECT * FROM read_csv(...)')`, also changed their inspected numeric
plan solely with the CSV value. Direct reader calls and a filename-suffixed
stored view containing the macro were correctly refused through raw SQL.
Authored function heads were omitted from relation dependency parsing, and the
reader-token scan left dynamic SQL strings opaque.

The [existing #1506 comment][prior-opaque] had already reported
`query_table(path)` as an indirect reader reaching the same unguarded source
inference mechanism. After checking that report and #1519's narrower replacement
scan scope, the new macro and `query()` witnesses were consolidated into
[additional evidence on #1506][opaque]. No new Issue was opened for those input
variants, and the closed Issue's state was left unchanged. Safe constant-macro
success tests were also identified; the evidence did not justify indiscriminately
refusing all table macros.

## Executed coverage and hypothesis dispositions

Each row records a hypothesis, its evidence and execution result, the verdict,
and what would warrant the next confirmation. Counts denote distinct comparisons
and are not added into one score.

| Hypothesis and evidence | Result / verdict | Next confirmation |
| --- | --- | --- |
| Plan algebra could alter ordered sets, fixed keys, bits or identifiers. | Parent: 13 hand-enumerated specifications across keep/drop/error, 39 comparisons, all matched. B: 120 seeded ASTs across all five constructors, nesting, composites and overlap, compared against an independent enumerator under keep/drop; all matched. | A distinct algebraic boundary or dependency version. |
| Duplicate optimization could change plan order above its small-family path. | B: 12 ten-unit cubes, 1,024 occurrences each, in four placements and all three policies matched independent bitmask enumeration. A: 180 generated specifications matched keep-with-first-occurrences versus drop. The latter was a shared-path consistency check, not an independent expansion oracle. | A new selection-dependent optimization boundary. |
| Width or fixed keys could corrupt mask types. | B: 0/1/30/31/32/64 dimensions with only two sets and a fixed key matched. At 31 dimensions the maximum was integer 2,147,483,647; at 32+ identifiers were `NA_integer_` with complete named integer bits. The 32-dimension Margin helper correctly refused. | A different helper or backend limit. |
| Separators or non-syntactic names could corrupt exact identity or display. | Commas, equals, parentheses, pronouns, backticks, braces, Unicode and newline names preserved list names and identifiers. Parent verified text as display, including empty values. `...`/`..1` fixtures were refused identically by native dplyr selection. | Another accepted name form; text ambiguity alone was expressly permitted. |
| Inspection could disagree with Margin-operation identifiers. | Parent checked `.id` and bare `grouping_id()` separately from its hand oracle. B checked data.frame, tibble, data.table, dtplyr and live SQLite, fixed partitions, missing keys and duplicate keep/drop. All correspondences matched. | Other dialect/version evidence. |
| Capture or overlapping column/specification names could lose scope or evaluations. | A: bare/injected/double-quosure, promise, embrace, numbered-dot and distinct lexical scopes followed ADR 0026. Sixty constructor-boundary combinations and 224 syntax/return-value comparisons matched typed tidyselect values or exact external class/message and caller-function counts. | A distinct capture boundary; no executable promising candidate remained. |
| Derived typed proxies could change metadata or caller inputs. | C: 22 dtplyr shapes, nine plain join/set forms with empty/nonempty inputs and seven predicates, and 84 DuckDB/Arrow predicate plans matched explicit-collect controls where admitted. Unsupported factor/shaped derivations refused; sources stayed unchanged. Numeric Arrow dictionaries matched their specified factor representation. | Other constructible classes or derivations. |
| Lazy predicates or repeated calls could expose source values or stale records. | Predicate witnesses saw zero-length lazy columns, recorded SQL followed enabled state, and later local inspection cleared prior SQL. The CSV binding witnesses required separate source-value observation and were confirmed above. | Other control transfer or backend conditions. |
| Invalid public calls might be silently accepted. | B's 15 option, duplicate, overlap, rename, empty-selection, rowwise, `.drop = FALSE` and grouped-`.by` refusals were Package conditions. Forged specification internals were excluded under ADR 0015 rather than promoted to public defects. | A constructible invalid public path. |

Expected values and generated input syntax were separate. The parent's small
families were written out by hand; simple R derived included/omitted vectors,
named integer absence bits and weighted masks from those families, without using
the internal compiler. B independently enumerated generated models. Full list
contents and column types were compared; text was checked against its documented
display grammar. Operation comparisons exercised shared package paths and were
not presented as independent proof of the expansion algorithm.

Harness failures were excluded from findings. B corrected an empty-vector names
attribute in its oracle and confirmed reserved-name refusals upstream. A replaced
a malformed hand-written parenthesis fixture with a structural AST. The parent's
first audit-disabled probe tried reading unavailable SQL records; the corrected
probe recorded the unaudited state without that access. Its final public-contract
assertion failed as intended only after completing controls and cleaning up the
connection/file, because all three unsafe entry shapes returned plans. Earlier
stopped harnesses were not described as passes.

## Provenance, duplicate searches and limits

Fetching main again before publication returned the tested SHA. All-state
snapshots contained 785 Issues with comments and 737 PR bodies before the new
reports. Searches included `inspect`, `view` with `metadata`, `stored view`,
`named view`, `ordinary view`, `view admission`, `table capture`, `table macro`,
`query_table`, and CSV/DuckDB inference combinations. Full related reports and
PR #1524's review/fix history were read; its filename-suffixed view corrections
did not establish either missing entry path. New Issues used `bug` and
`ready-for-agent`, with no blockers, under the requested `to-tickets` workflow.

Parent evidence stayed under
`/private/tmp/marginplyr-inspect-root-20261109/`: `oracle.R`, `views.R`,
`view-matrix.R`, `boundaries.R`, their logs and CSV records. Agent evidence stayed
under `/private/tmp/marginplyr-inspect-capture.v1FJKk/`,
`/private/tmp/marginplyr-public-contract.HbVKgZ/` and
`/private/tmp/marginplyr-cross-backend-20261109/`. Each retained short
hypothesis/evidence/result/verdict/next records. The Issues and existing-Issue
comment retained public minimal replays rather than depending on temporary files.

All owned executions were collected to terminal completion. Parent oracle
session 8732 and view sessions 86091/89272 ended at exit 0; corrected boundary
session 92482 ended at the expected desired-contract assertion, exit 1, after
cleanup. Each source copy ran one reproduction at a time with
`TESTTHAT_PARALLEL=false`. No complete suite, package build or render was used
for exploratory feedback. The focused inspection test was development evidence,
not Review-ready evidence.

Environment: arm64 macOS Tahoe 26.6.2, R 4.6.1, marginplyr 0.1.0 from the tested
SHA; dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1, tibble 3.3.1,
vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1, DBI 1.3.0, DuckDB 1.5.5,
RSQLite 3.53.3 and Arrow 25.0.1. Databases and data were locally owned,
temporary and synthetic; connections were closed.

After combining the independent reports, the team checked local and lazy paths,
both formats, successful and refused calls, value/name/type/column-order controls,
input ownership, source work separate from metadata counts, and repeated record
state. Each confirmed cause was followed into adjacent paths before returning
to unrelated hypotheses. No concrete executable promising hypothesis remained
in these finite lanes. Other OS/dependency versions, live PostgreSQL/SQL Server,
remote sources, arbitrary dispatched methods and unbounded SQL/expression graphs
were not established. They require new fixtures and independently observable
source-work evidence; finite matching comparisons do not prove universal safety.

## Submission effects

Only this dated note was added. Its consumers were investigation readers; it
changed no package implementation, tests, dependencies, generated documentation,
installed file, build behavior or verification tooling. The cumulative baseline
was the merge-base with freshly fetched main. These effects justified
Review-ready `source`, with package build/check, complete suite and snapshots
omitted. No NEWS entry was needed for this research record. Revision-specific
verification, main compatibility and review records belonged on the PR.

[direct]: https://github.com/sayuks/marginplyr/issues/1525
[view]: https://github.com/sayuks/marginplyr/issues/1526
[opaque]: https://github.com/sayuks/marginplyr/issues/1506#issuecomment-6728826764
[prior-opaque]: https://github.com/sayuks/marginplyr/issues/1506#issuecomment-6721240758
[inspection]: ../R/inspect-grouping.R
[constructors]: ../R/grouping-spec.R
[identity]: ../R/contextual-helpers.R
[prior-evaluated]: inspect-grouping-evaluated-captures-and-source-work-2026-11-08.md
[prior-csv]: inspect-grouping-capture-and-csv-boundaries-2026-11-08.md
[prior-deep]: inspect-grouping-deep-bughunt-2026-11-08.md
[prior-systematic]: inspect-grouping-systematic-2026-11-07.md
[prior-metadata]: inspect-grouping-bughunt-2026-11-07.md
