# Expansion, caller-owned Grouping language and CSV replacement scans

Investigated: 2026-11-08
Investigation SHA: `d6f1f45824ddc04fd25fcd67860a016b2f9b6ea0`
Scope: functional investigation; no implementation changes

Freshly fetched main was pinned at the SHA above. A new investigation branch
and three independent detached worktrees started there. Three fresh-context
agents generated implementation, public-contract and processing-consistency
candidates independently before sharing their first reports. The coordinator
separately prepared input factories and handwritten row-copy expectations.

Two residual functional defects were confirmed and published as independent
`bug`, `ready-for-agent` Issues: [#1517][factory] and [#1519][csv]. Both followed
the merged repair in [PR #1511][repair], but exercised previously unreported
conditions. No package implementation, tests or dependency declarations were
changed. Both tickets had no blockers and included contracts, minimal examples,
expected/actual results, environment, separating controls and acceptance criteria.

## Contracts, history and comparison method

The public expansion reference promised one source-row copy per grouping-set
occurrence, with omitted dimensions replaced. The constructor and inspection
references supplied the grammar and top-level factory contract. CONTEXT and
ADRs 0007/0008/0026 supplied capture and specification rules; 0009/0013 supplied
occurrence identity and inspection; 0012/0016 supplied factor restoration and
the boundary of delegated attributes; 0018 supplied Margin order; 0020/0029
supplied source-read and input-protection boundaries; 0031–0033 supplied SQLite
and declared-type boundaries. Reading and source inspection covered capture,
preflight, algebra, branch replacement/combination, identifiers, restoration,
metadata, source reuse and failure paths across the existing implementation.

Recent context included the dated expansion functional-validation and contract
investigations, inspection-capture/CSV investigation, empty Nest conversion and
summary/share mutation investigations, and the merged fixes for #1489 and
#1502–#1506. Their evidence was historical context, not a substitute for
execution at the pinned SHA. In particular, the broad successful matrices in
the earlier contract investigation guided additional shaped/recursive and
language-boundary probes rather than restricting exploration to recent diffs.

The coordinator wrote five families by hand: rollup, cube, composite rollup,
explicit repeated sets and a Cartesian product. Expected rows were made by
repeating source indices once per occurrence, then changing only dimensions
absent from that manually listed set. A separate input generator supplied
zero/one/three-row inputs, missing fixed keys, duplicate business values,
factor payload and zoned temporal payload. Explicit selectors kept the tracking
column from altering column-selection meaning. Duplicate-set keep/drop and
typed-missing/text labels were independent inputs.

Comparisons checked full public column order, values, types, classed-column
attributes, factor levels/ordering, structured shapes and source-row/occurrence
multiplicity. Unspecified row order was normalized by occurrence and tracking
ID. The tracking ID distinguished physical copies even when business values
were duplicates. No expectation relied solely on an internally compiled plan.
Eight intentionally corrupted results established that the comparator detected
missing/extra rows, wrong occurrences, changed payload, identifier/fixed-key
type drift, column permutation and changed factor domains.

## Finding: public capture changed caller-owned language (#1517)

A valid NSE factory inspecting its argument as language selected `rollup(a)`
when evaluated before the Margin verb and `rollup(b)` when written inline:

```r
library(marginplyr)
d <- data.frame(a = c(1L, 2L), b = c(3L, 4L))
make <- function(expr) {
  raw <- substitute(expr)
  if (identical(raw, quote(grouping_set(, a)))) rollup(a) else rollup(b)
}
s <- make(grouping_set(, a))
expand_with_margins(d, .grouping = s, .id = "sid")
# a = c("1", "2", "Total", "Total"); b = c(3L, 4L, 3L, 4L)
expand_with_margins(d, .grouping = make(grouping_set(, a)), .id = "sid")
# b = c("3", "4", "Total", "Total"); a = c(1L, 2L, 1L, 2L)
```

Both returned four rows with identifiers `1L, 1L, 2L, 2L`, so cardinality and
identifier-only assertions missed the wrong omitted dimension, values, types
and leading-column order. The inner constructor-shaped call was language data;
the factory did not execute it. The complete top-level factory was valid under
the public contract accepting any expression returning a specification.

Public preprocessing annotated that language with `marginplyr_keep_empty`.
The factory's template comparison consequently selected a different branch.
Literal capture spellings were protected after #1505, but quotation aliases,
computed callees and custom NSE factories were not. `q <- base::quote` followed
by a template-reading factory reproduced through inspection and all four Margin
verbs. A language-editing alias factory also falsely rejected a valid empty set.
Ordinary rlang capture and literal `base::quote` controls preserved the stored
result; removing the interior empty language position removed the difference.

The opposite boundary shared this cause: `quote <- identity` made an ordinary
executed call look like an opaque capture. Inline
`.grouping = quote(grouping_set(a, , !!!list()))` accepted `(a)` while the
stored form correctly refused Argument 2. Rebinding `substitute` and
`expression` likewise bypassed refusal across all five constructors and all
five public consumers: 75 conditions. Alias/parenthesized/computed quotation
heads yielded 20 wrong complete results; five literal-quote controls passed.
Forced defaults were checked separately and did not reproduce.

The coordinator independently reproduced the alias result and shadowed-head
refusal difference in a fresh source archive after refetching unchanged main.
The desired full-frame equality assertion exited 1. The existing
`test-grouping-quoted-factories.R` assertions would detect these differences if
fed the new calls, but their actual inputs covered recognized literal heads
and supplied function bodies. They did not exercise aliases or custom NSE
factories. Issue #1517 grouped the two opacity directions as one public-capture
repair slice, related to closed #1505/#1489 rather than duplicating their
passing literal witnesses.

## Finding: CSV replacement scans sampled values during inspection (#1519)

DuckDB's replacement-scan spelling escaped the explicit CSV-reader guard:

```r
library(marginplyr)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", shared_home = FALSE)
path <- tempfile(fileext = ".csv")
x <- dplyr::tbl(
  con, dbplyr::sql(paste0("SELECT * FROM ", DBI::dbQuoteString(con, path))),
  vars = "k"
)
inspect_grouping(x, .grouping = grouping_set(k))$included
# "(k)" even before the file exists
writeLines(c("k", "101"), path)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "(k)"
writeLines(c("k", "source_tail"), path)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "()"
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(path)
```

Only one data row changed: path, header, SQL, lazy object and row count stayed
fixed. The type-dependent result therefore established source-value inference,
not merely file validation. Typed inspection was required to refuse before
sampling under the inspection/query contract. The inferred types themselves
were ordinary DuckDB behavior; invoking inference without caller authorization
was marginplyr's defect.

Changing only the source syntax to explicit `read_csv()` correctly Package-refused
before a metadata query. Name-only inspection worked for both spellings with
the file absent. An explicit schema with `auto_detect = false` retained numeric
metadata even when the data row contained an incompatible string, and Parquet
replacement metadata remained available. Typed `.by` showed the same source
sampling; expansion construction also sent a selection-proxy query. Auditing
showed one returned `selection_proxy` wrapped in `system.main.query(...)` with
`LIMIT 0`, so statement counts and zero returned rows did not detect this work.
After file removal the unguarded typed path reached DuckDB's IO error instead
of the explicit reader's Package refusal. Caller lazy-query bytes were unchanged.

The coordinator independently replayed the payload witness and all separating
controls in a fresh main source archive. The desired-refusal assertion exited
1. The merged #1506 tests covered explicit CSV calls and unsafe reader
arguments but omitted `FROM 'path.csv'`. Issue #1519 recorded this residual
replacement-scan condition separately from the public-capture repair.

## Finite execution and hypothesis ledger

Counts overlap and are not a count of unique covered inputs.

| Hypothesis and evidence | Execution result | Disposition and next evidence |
| --- | --- | --- |
| Source copies, duplicate occurrences, fixed missing keys, typed labels and restoration could change the row bag or carried columns. | 690 exact full-frame matches across 13 factories, five handwritten families, zero/one/three rows, keep/drop and representable NULL/text labels. Included haven labelled/SPSS, zoned POSIXct, Date, difftime, ordered factor, list, packed frame and array. | Refuted within this matrix. A complete-frame counterexample would reopen its path. |
| Consistent renaming, input row/column permutation or unrelated columns could change expansion semantics. | 160 exact comparisons over five scalar/structured families and four name maps, including internal-looking, Boolean-word, pronoun, newline, Unicode and data.table-like names; eight comparator negative controls detected corruption. | Refuted for accepted names. The preliminary `..1` input-name probe hit the dependency's existing invalid-name refusal; it was not an accepted-result mismatch. |
| Shaped/classed columns could differ under eager combination or typed omission. | 105 exact ordinary-mutate/binary-union comparisons and 18 matching delegated conversion failures; Date/POSIXct/difftime arrays, NA-level factors, AsIs list arrays, poly/ftable and special/empty/duplicate inner names. | No package-only signal. Three invalid zero-row fixtures were excluded before either operation. |
| Simultaneous recursive dimensions could mix omission prototypes or factor sentinels. | 72 exact complete-frame matches, including deeply packed/zero-column frames, zero-width arrays, list arrays, factor domains, fixed keys, mixed labels and repeated/composite sets; input bytes unchanged. | Refuted for these boundaries. Arbitrary extension methods remained unmeasured. |
| Overlapping union/product/cube sets could break first-occurrence drop or IDs. | Independent ordered AST algebra: 1,797 matching keep/drop/error comparisons from 600 candidates, with one over the 128-occurrence bound excluded. | Refuted within bounded families. Early harness format/empty-vector mistakes were corrected, not counted as package findings. |
| Immediately invoked literal defaults could lose authored empties. | Fifteen forced-default refusal pairs across five constructors and three forcing bodies matched; seven initial supplied/unused/nested/default controls matched. | Refuted. No pending executable default variant remained. |
| Quotation spelling could disagree with actual capture behavior. | Wrong complete results, false refusal and required-refusal bypasses above; ordinary capture and literal controls separated the cause. | Confirmed #1517. No implementation changes. |
| dtplyr helper bindings or arithmetic overwrites could yield unsafe metadata. | Generated-helper protection held; a global head.data.table override was not invoked during inspection. Eight zero-row shaped proxies and 64 accepted overwrite/alias prototypes matched explicit schema; unsupported populated/complex and eight parenthesized cases safely refused. | Refuted/documented refusal. No root mutation was observed. |
| Deferred failures could corrupt a shared source or make retries disagree. | Arrow: 18 scenarios at zero/one/four rows, three sorts and collect/compute, 12 nonempty forced errors and 36 exact successful retry endpoints. dtplyr: 24 scenarios over filter/cached-rename/union/cached-join, three sorts and NULL/text labels, 24 forced errors and 48 exact retry frames. Construction/show-query ran no callback; caller sources stayed unchanged. | Refuted for measured graphs. Other failure or interruption schedules would need new evidence. |
| CSV binding could escape the new explicit-reader protection. | Payload-only changes altered typed plans, while explicit-reader, schema, absent-file/name-only and Parquet controls separated the cause. Independent archive replay agreed. | Confirmed #1519. Catalog/server-side expansion was outside this evidence. |

Existing expansion duplicate-policy assertions checked counts/names but did not
alone establish complete payload/occurrence correspondence. The independent
row oracle addressed that gap. Factor-array and inner-name tests asserted
shape/value/type or delegated refusal, and metadata/shared-source tests asserted
safety and ownership; their existence was not described as covering every
cross-product measured here. Preliminary transformation/fixture/log-print
errors were retained separately, corrected, and not counted as passing runs.

After combining all three reports, the coordinator revisited capture/grammar,
set algebra, branch creation/replacement/combination, identifiers, empty input,
factor and structured restoration, fixed missing keys, name allocation,
metadata, lazy execution, reuse, input protection and failure paths. No
executable promising hypothesis remained unresolved in that coverage map.
This finite result did not establish absence of other defects.

## Tracker reconciliation, environment and limits

All-state snapshots retrieved 777 Issue bodies and 732 PR bodies below the
1,500-item limits. Targeted all-state searches covered quote/alias,
Grouping/shadow, quote/capture and CSV replacement; the latter returned zero
Issues and zero PRs. Full bodies and comments for #1489, #1505 and #1506 and
the #1511 repair were compared. The two new conditions were residual paths
after that repair, with separate reproductions and regression gaps. Main was
refetched before publication and remained the investigation SHA.

The observed environment was arm64 macOS Tahoe 26.6.2, R 4.6.1, marginplyr
0.1.0, dplyr 1.2.1, dbplyr 2.6.0, tibble 3.3.1, vctrs 0.7.3, rlang 1.3.0,
tidyselect 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB
1.5.5, DBI 1.3.0 and RSQLite 3.53.3. Reproductions used R with
TESTTHAT_PARALLEL=false and one execution per worktree at a time. Original
execution handles were retained through terminal completion. Only synthetic
data and owned local in-memory databases/temporary files were used, and owned
database connections were disconnected.

Hosted databases, live PostgreSQL/other servers, other OS/dependency versions,
arbitrary S3/S4/Arrow extension methods, remote filesystems and exhaustive
interruption schedules were untested. The older Date/POSIXct SQLite
materialization interpretation question remained historical unresolved context;
this investigation did not remeasure or resolve that contract question.
Arbitrary DuckDB server-side view/macro definitions were not expanded. No
full suite or package build was used as exploratory evidence.

Temporary scripts, logs, exact result tables, environment records and ledgers
remained under `/private/tmp/mplyr-20261108-1540-evidence/`:

- `root/`: `oracle.R`, `transform.R`, `dotnames.R`, `confirm.R`,
  `confirm-csv.R`, `csv-controls.R` and their successful/desired-contract logs.
- `a/`: `structured.R`, `recursive-cross.R`, CSV results and `ledger.md`.
- `b/`: `algebra-probe.R`, capture/default/shadow/alias/NSE probes,
  `capture-regression.R`, result CSVs and `findings.md`.
- `c/`: metadata/overwrite/global-method probes, Arrow/dtplyr failure-retry
  probes, `duckdb-csv-replacement.R`, `csv-replacement-assert.R` and logs.
- Coordinator tracker snapshots, search results and the two published Issue
  body drafts were stored at the evidence root. `current-source/` was a
  disposable archive of the refetched investigation SHA.

Run scripts individually with Rscript; coordinator confirmation scripts take
the source directory as their first argument. Temporary artifacts carry no
repository retention guarantee. The minimal examples above and linked Issues
retain the essential evidence.

## Submission effects

This submission added dated research prose for readers. Its temporary scripts
were not package or verification consumers, and it changed no package code,
tests, examples, vignettes, generated/installed files, dependencies, build
exclusions or tools. These effects justified Review-ready `source` from the
merge-base with freshly fetched main: package build/check, complete suite and
snapshots omitted. The pinned submitted SHA, integration assessment, mandatory
outcomes and separate review record belonged on the PR. No behavior change
required a NEWS entry. CI monitoring was outside the requested work.

[factory]: https://github.com/sayuks/marginplyr/issues/1517
[csv]: https://github.com/sayuks/marginplyr/issues/1519
[repair]: https://github.com/sayuks/marginplyr/pull/1511
