# Expansion S4 prototypes and DuckDB source admission

Investigated: 2026-11-09
Investigation SHA: `5c667c956fd50ba39be621bf1b617a17abda7b14`
Scope: functional investigation; no implementation changes

Freshly fetched `main` was pinned at the SHA above. The investigation branch
`investigate/expand-grouping-2026-11-09` and three detached worktrees started
there. Three fresh-context agents independently generated candidates from
implementation, public contracts and processing consistency before receiving
one another's first reports. The coordinator separately built a row-copy
reference and small input factories. All executions used R, synthetic data,
serial reproduction processes per worktree, and explicitly disabled testthat
parallelism. Database work used owned temporary in-memory databases and files.

Two previously unreported defects were published with `bug` and
`ready-for-agent`: [#1528][s4] and [#1533][calls]. Two independently reproduced
DuckDB relation entries matched [#1525][ident] and [#1526][views], which appeared
during the final duplicate check; no duplicate Issues were opened. Each new
ticket had no blockers, its own contract, minimum example, expected/observed
behavior, environment, controls and acceptance criteria. The Issues described
independently verifiable repairs, without implementation changes in this branch.

## Contracts and historical context

The public `expand_with_margins()` reference supplied the row-copy, fixed-key,
label, factor, column-order, occurrence-identifier, Margin-order and lazy-input
contracts. `grouping_set()` and `inspect_grouping()` supplied constructor
grammar, selection reading, duplicate policy and inspection semantics. CONTEXT
and ADRs 0001–0003, 0005–0009, 0012–0013, 0016, 0018, 0020, 0026, 0029 and
0031–0033 supplied lifecycle, metadata, type/attribute, source-read and
identifier boundaries. The investigation covered capture, preflight,
compilation, replacement, branch combination, identifiers, restoration and
finalization across the existing implementation, rather than only its latest
diff.

Relevant history included the October 29 expansion hunt, November 7 typed and
systematic contract investigations, November 8 language/replacement-scan and
inspection investigations, and merged #1511/#1524. Their successful matrices
and excluded upstream failures were evidence for choosing new conditions, not
proof about this SHA. In particular, the October 29 Period exclusion concerned
ordinary dtplyr union corruption, whereas the new S4 witness needed no union.
The #1524 view checks were also read against the actual fixture names and
query carriers they exercised.

## Independent reference and comparator

Five grouping families were listed by hand, independently of the package's
compiled Grouping plan: a two-dimension rollup, cube, composite rollup,
explicit repeated sets and a Cartesian product. The reference repeated input
row indices once per listed grouping-set occurrence, retained fixed keys and
payloads, and replaced only absent dimensions. Input generation was separate
from those literal sets. Explicit selectors kept the tracking column from
changing selection meaning.

Comparisons normalized unspecified physical order by occurrence and tracking
ID, then used `identical()` over the complete public frame. They checked the
source-row × occurrence multiset, fixed and non-key values, column order,
types, factor levels/ordering and classed-column attributes where the contract
retained them. Four deliberately corrupted results established detection of
missing rows, excess rows, wrong occurrences and changed payloads. Sorted
contract comparisons additionally checked the requested key order, with
unspecified equal-key ties treated separately.

The tracking ID distinguished physical source rows whose business values were
duplicates. Plan-only and row-count-only checks were insufficient for both
new findings: the S4 failure occurred after successful inspection, and the
DuckDB queries returned zero rows while their binders still inferred types
from source values.

## Finding: multi-slot S4 metadata prevented safe dtplyr expansion (#1528)

```r
library(marginplyr)
x <- dtplyr::lazy_dt(tibble::tibble(k = lubridate::period(month = 1)))
dplyr::collect(dplyr::mutate(x, k = as.character(k)))
# k = "1m 0d 0H 0M 0S"
dplyr::collect(expand_with_margins(x, .grouping = grouping_set(k)))
# Column `second` (size 0) must match the data frame (size 1).
# This is an internal error that was detected in the vctrs package.
```

One included-only set was enough. The expected result was the one-row
character frame returned by ordinary mutation and local collect-first
expansion. There was no omitted-value write, branch union, `.id`, sorting or
summary expression to explain the failure. `Interval` independently reached
the same boundary through its `span` field.

The internal data.table zero-row truncation emptied `.Data` but left Period's
auxiliary row-sized slots, or Interval's `start`, at the source length.
`margin_column_info()` then called `vec_init()` on inconsistent metadata.
Vector-level `vec_slice(key, integer())` produced consistent zero-row slots and
supported initialization. This isolated an internally introduced metadata
failure from a dependency's answer to the caller's ordinary operation.

Period/Interval × zero/one/three rows gave four nonempty text-label failures
and two empty successes. Included-only NULL labels reached the same failure.
For all six fixtures, native input collection, ordinary text mutation, fixed
keys, payload-only expansion, collect-first expansion, name-only inspection and
typed `where(inherits)` inspection succeeded. Source serialization stayed
unchanged. Inspection equality therefore did not establish usable expansion
metadata.

The parent replayed the one-row text witness in a separate fresh archive of
refetched, unchanged main. Ordinary mutation and local expansion passed before
the desired complete-frame assertion exited 1. The earlier upstream Period
union failure did not account for this included-only character operation.
ADR 0020 also required faithful metadata or an actionable Package refusal for
unsupported metadata work; the dependency-invariant error supplied neither.
The ticket bounded its successful-operation requirement to the working
ordinary text control rather than promising arbitrary S4 union support.

Existing root/head/metadata tests used ordinary columns or S4 numeric classes
with only `.Data`. `test-dtplyr-numeric-metadata.R` observed numeric-method
inputs during inspection and explicit collection, without an auxiliary
row-sized slot or expansion prototype. These actual fixtures did not detect
the demonstrated failure.

## Findings matching relation-entry Issues (#1525, #1526)

A stored view over `read_csv_auto()` was created while a synthetic CSV
contained `k = 101`. With a captured SQL input `SELECT * FROM ordinary_source`,
typed inspection returned `(k)`. Changing only that one value to `source_tail`
changed the same input's inspected inclusion to `()`. Path, header, row count,
view and lazy query were unchanged. This established value inference during
binding, rather than merely checking file existence.

Changing only the view name to `suffix.csv` correctly Package-refused through
the SQL-fragment entry. Ordinary named `tbl(src_dbi(con), name, vars = "k")`
instead reached the reader even with that suffix. The two entries were
independent: the SQL relation loop resolved only suffix-like candidates, while
the captured-query walker did not process ordinary identifier carriers.

The independent agent completed 30 view/value/entry controls and 24 adjacent
construction/inspection observations, plus four file-removed concrete-table/
CTE controls. Nested ordinary views, typed `.by` and expansion construction
also reached the reader. Established-schema views, concrete tables and a
same-named constant CTE retained numeric metadata; concrete-table and CTE
expansions still returned two correct occurrence rows after file removal.
Name-only view inspection succeeded without the file, while typed inspection
reached DuckDB's IO error. Caller lazy-query bytes stayed unchanged.

Auditing distinguished returned `selection_proxy` work from the suffix-view
refusal's catalog-only read. Existing saved-view regressions used suffix-like
names through SQL fragments, so neither their fixture values nor their carrier
forms exercised the two omitted entries. The coordinator independently
replayed the ordinary-name payload witness and desired refusal in a fresh
archive; the latter exited 1.

The final live tracker refresh found [#1525][ident] and [#1526][views] already
reported on the same SHA. Their full bodies and comments were read. They
matched these two causes, so this investigation linked them rather than
publishing its prepared combined-view draft.

## Finding: authored source calls concealed inference (#1533)

```r
library(marginplyr)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
path <- tempfile(fileext = ".csv")
writeLines(c("k", "101"), path)
DBI::dbExecute(con, paste0(
  "CREATE MACRO hidden_reader() AS TABLE SELECT * FROM read_csv_auto(",
  DBI::dbQuoteString(con, path), ")"
))
x <- dplyr::tbl(con, dbplyr::sql("SELECT * FROM hidden_reader()"), vars = "k")
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "(k)"
writeLines(c("k", "source_tail"), path)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "()"
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(path)
```

This witness used no stored view. Authored-SQL admission excluded function
relations from ordinary relation lookup and separately recognized visible
CSV/JSON reader heads. The source call reached neither the reader check nor
stored-view function admission. Fixing base-relation iteration alone would
leave this entry; fixing the call entry alone would leave #1525/#1526.

Directly writing `read_csv_auto()` correctly refused. The table macro also
reproduced through typed `.by` and expansion construction. A separate parent
replay in the fresh main archive changed the typed plan with only the CSV
value, retained input bytes and exited 1 at the desired-refusal assertion.
Twelve further source/value comparisons confirmed nested table macros, a
scalar macro and `query(<literal reader SQL>)`; constant macros and `range()`
stayed numeric for both contents. Those controls completed with exit 0.

The #1524 tests placed macro and `query()` calls inside suffix-named stored
views, where a separate function check applied. They did not supply those
calls directly as the authored source. This was normal DuckDB inference
invoked under marginplyr's stronger no-source-read contract, rather than an
alleged database inference defect. The acceptance criteria retained safe
ordinary table functions and explicit execution.

## Hypothesis ledger and finite scope

Counts overlap and do not represent unique covered inputs.

| Hypothesis and evidence | Execution result | Disposition and next evidence |
| --- | --- | --- |
| Row copies, duplicate occurrences, typed/text labels and restoration could change a complete result. | 600 exact handwritten-reference comparisons over ten scalar/temporal/factor types, zero/one/three rows, five families and keep/drop; four comparator corruption controls detected. | Refuted in this matrix; a complete-frame counterexample would reopen it. |
| Consistent renaming, input row/column permutation or unrelated columns could alter semantics. | 360 exact comparisons, four name maps, five families, two labels and three sort requests. Included internal-looking, pronoun, Boolean, rowid, newline, backtick and Unicode names. | No row-bag/type/value defect; row permutations were normalized and non-key column order followed the input. |
| Extended scalar and geometry keys could diverge from ordinary mutation/union. | 252 comparisons over hms, integer64, units, list-of, raw, POSIXlt and sfc, zero/one/three rows, fixed/dimension roles and all sorts; no successful-result mismatch. | Raw and sfc typed-missing refusals were representability policy, not lost rows. Matching dependency failures were excluded from successful-result claims. |
| Less common local temporal classes could lose values or classes. | 42 ordinary-operation comparisons: 41 exact results and one matching zoo error. | Refuted for ts/zoo/yearmon/yearqtr/Period/Duration/Interval cases; timeDate admission and native rcrd/POSIXlt dtplyr row-size failures remained dependency exclusions. |
| Structured keys, duplicate policy and ordering could disagree with the contract oracle. | 1,260 comparisons across 14 types, three column roles, five families, keep/drop and all sorts; no strict mismatch or one-sided error. | Both-sided errors were not separately counted; this was not 1,260 successful outputs. Surv recursion reproduced at the ordinary boundary and was excluded. |
| Large overlapping families could misapply drop/error or renumber occurrences. | 32 independent AST families, 512–8,194 raw occurrences and 1–16 retained sets; exact drop outputs and duplicate-specific Package refusal for every error case. | Refuted within this bound; no scalability conclusion beyond it. |
| Implicit fixed keys could differ from explicit `.by` with missing/structured keys. | 468 strict comparisons over factor, list, packed, matrix and NA-level factor keys, three carriers, one/two keys, mixed labels, duplicates and sort modes. Results were ungrouped and source bytes unchanged. | Refuted for these cases. Admission-flattened data.table fixtures were excluded before interpreting results. |
| DuckDB scalar/nested SQL types could be altered by omitted branches. | 22 exact direct SQL-reference comparisons, empty/populated; decimal, integer widths, UUID, interval, time, nanosecond timestamp, blob, list, struct and map. | No package-only mismatch. BIT, fixed array and UNION fixtures failed ordinary driver preparation/collection and were excluded; driver nanosecond conversion warning remained delegated. |
| SQLite anchors, duplicate occurrences and special names could alter carried values or types. | 180 exact comparisons over five handwritten families, zero/one/three rows, NULL/text labels, all sorts and ordinary/Boolean/pronoun/internal-looking names; 12 direct materialization controls also matched. | Refuted for these cases. Expectations started from the caller's database input after ordinary upload conversion, and selectors used explicit names. |
| Hidden Arrow partition names could overwrite dimensions or boundary aliases. | 12 boundary endpoints matched after normalizing equal-key ties. | Refuted for those captured source graphs; the first strict tie-order mismatch was an oracle error, not a value difference. |
| Multi-slot S4 proxy slicing could invalidate an otherwise safe dimension. | Minimal Period and independent Interval witnesses, working native/local controls and two separate desired-result failures. | Confirmed as #1528; no implementation repair here. |
| Saved DuckDB relations could conceal inference. | Payload-sensitive ordinary/nested view witnesses and contrasting suffix/named-input controls. | Matched #1525/#1526; follow those tickets. |
| Authored source calls could conceal inference independently of stored views. | Table/nested/scalar macro and literal query-source witnesses, safe constant/range controls and independent red assertion. | Confirmed as #1533; follow its source-call acceptance cases. |

Early harness errors were kept separate from package observations. Assigning
logical `NA` into complex data produced a partial complex missing value in the
first reference; native typed indexing corrected it before all 600 comparisons
were rerun. The first column-permutation reference incorrectly kept payload
columns in their old order; it was corrected and all 360 rerun. The contract
agent corrected a reserved-word fixture, strict type comparison, and AST
dimension-order expectation before its final runs. An initial DuckDB type loop
stopped at an upstream BIT preparation error; the final loop explicitly
classified all six ordinary failures. None of those interrupted or incorrect
reference runs counted as package findings. SQLite's first reference used
pre-upload NaN values, although ordinary SQLite upload had already converted
them to missing, and bare `.env` selection changed tidyselect meaning. The
final reference used the database input and explicit `all_of()` selections.
An intermediate scratch parse failure was excluded; the stable final command
reran all 180 comparisons and exited 0 with no mismatches.

## Freshness, tracker checks, environment and limits

Main was refetched before independent archive replay and again before Issue
publication; each time it resolved to the investigation SHA. Duplicate checks
downloaded all 785 Issues then available and 500 recent PR bodies, searched
those inventories for Period/Interval/lubridate/slot and stored-view/reader
terms, and used all-state GitHub searches for `Period`, `"stored view"`,
`view`, `"hidden_reader"` and metadata/macro. The related #1506/#1516/#1519
bodies/comments and #1524 history were inspected. Live refreshes then found
#1525/#1526, whose two entries were already reported. No matching S4-prototype
or authored-source-call ticket was found before publishing #1528/#1533.

Environment: macOS Tahoe 26.6.2 arm64, R 4.6.1; marginplyr 0.1.0 at the pinned
SHA; dplyr 1.2.1, vctrs 0.7.3, tibble 3.3.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, DuckDB 1.5.5, DBI 1.3.0, RSQLite 3.53.3, Arrow 25.0.1,
lubridate 1.9.5, rlang 1.3.0 and tidyselect 1.2.1. Lubridate was a reproducer
prerequisite, not a newly declared package dependency. The ticket left
regression-fixture selection and any dependency declaration to implementation.
No prefactoring dependency was established between the repair slices.

R scripts, CSVs, logs and RDS results were kept in task-owned temporary areas:
`/private/tmp/eh-root-01a1bd80*` and `/private/tmp/eh-{a,b,c}-01a1bd80/`.
The minimum examples above and ticket examples retained the reproducible
evidence beyond those temporary artifacts. All reproduction executions were
collected to terminal outcomes. No package suite, build or render was used as
exploratory evidence.

The three reports were reconciled against capture, algebra, replacement,
combination, restoration, names, fixed keys, identifiers, empty input, source
reuse, construction/execution and failure paths. No concrete promising
executable hypothesis remained unresolved after the adjacent source-call
controls. This finite investigation did not establish absence of other bugs.
Other R/OS/dependency versions, arbitrary extension methods, larger source
graphs and unprovisioned live database servers remained unverified. Such
claims would need independent reproductions in those environments; simulated
SQL was not treated as server-execution evidence.

## Submission effects

Only this dated research note was added. Package code, tests, generated
documentation, dependencies and workflows were unchanged. Its code blocks
were documentary reproductions, with no build/test/generation consumers;
`investigation/` was excluded by `.Rbuildignore`. The branch therefore selected
Review-ready `source` against the fetched-main merge-base, with the mandatory
repository consistency and six checks. Package build/check, the complete suite
and snapshots were intentionally omitted; the PR verification record held the
submitted SHA and actual terminal result. No full-result timing proposal was
generated. This internal research record required no NEWS bullet.

[s4]: https://github.com/sayuks/marginplyr/issues/1528
[calls]: https://github.com/sayuks/marginplyr/issues/1533
[ident]: https://github.com/sayuks/marginplyr/issues/1525
[views]: https://github.com/sayuks/marginplyr/issues/1526
