# Grouping inspection: quoted factories and CSV binding

Investigated: 2026-11-08
Tested main: `2298c561633591844576992ef98e039af264a6c4`

A new worktree started from freshly fetched main. Three fresh-context agents
generated independent candidates from implementation, public contracts and
processing-path consistency before sharing their initial reports. The parent
separately constructed ordered-set expectations and generated small inputs.
Two independently repairable defects were confirmed and reported:
[#1505][marker] and [#1506][csv]. No package implementation or package test was
changed.

## Contracts and preparation

The [constructor][constructors], [inspection][inspection] and
[identity-helper][identity] references supplied the public contracts. The
investigation traced capture/evaluation, nested argument admission, selection,
metadata acquisition, expansion, duplicate handling, masks, formatting and
Margin-operation correspondence, rather than only recent changes. Relevant
decisions included ADRs 0002, 0005, 0007, 0008, 0009, 0013, 0019, 0020, 0026,
0027, 0029 and 0032. Existing tests were inspected after deriving failure
hypotheses to determine whether they detected the particular boundary.

The recent [deep inspection][prior], [systematic inspection][systematic] and
[metadata inspection][metadata-prior] notes, adjacent investigation records and
relevant implementation history were consulted. Main included the #1461–#1465,
#1469, #1481 and #1489 repairs. The full #1489 and #832 bodies/comments and
PR #1501's body/review history distinguished earlier reports from the findings
below. #1489's comment already documented an alias/factory residual; it was
not counted as a new defect here.

## Confirmed: capture changed quoted factory inputs

The top-level `.grouping` argument accepted any expression returning a
specification. A factory editing quoted language therefore had to retain its
ordinary R meaning. This minimal example instead differed between stored and
inline forms:

```r
library(marginplyr)
d <- data.frame(a = 1L)
drop_last <- function(expr) {
  expr[[length(expr)]] <- NULL
  eval(expr)
}
s <- drop_last(quote(grouping_set(, a)))
inspect_grouping(d, .grouping = s)$included
# "()"
inspect_grouping(d, .grouping = drop_last(quote(grouping_set(, a))))
# Argument 1 of `grouping_set()` is empty.
```

The factory removed `a` before evaluating the remaining call, whose only empty
position was trailing and was correctly omitted outside public capture. Public
capture traversed inside `quote()` and attached `marginplyr_keep_empty = TRUE`
to the language object. The edit retained that attribute, so constructor capture
incorrectly preserved and refused the empty position.

The variant `quote(grouping_set(a, , b))` retained `a` outside inspection but
falsely refused Argument 2 inline. It reproduced through all four Margin verbs
and dtplyr/Arrow inspection. A template-matching factory silently selected `b`
instead of `a`; marker mutation also crossed `expression()` and supplied
function bodies. These were consolidated into one cause, not separate tickets.

Plain `rlang::enquo()` / `eval_tidy()` preserved the expected factory result
with the same dependencies. The minimal witness used no injection, so ordinary
`!!`/`!!!` semantics did not explain it. Separate archives independently
reproduced the defect, followed by a fresh current-main confirmation.

The constructor-argument tests covered literal/stored authored empties,
trailing commas and `missing(x)` inside caller functions, but did not inspect
quoted-language factory inputs or edit them before constructor execution.
Their passing conditions would not detect this failure. [#1505][marker]
recorded the contract, minimal reproduction, impact, environment and acceptance
criteria; it required preserving the #1489 authored-empty protection.

## Confirmed: CSV type inference sampled source rows during inspection

A lazy DuckDB input captured only the name `k` from a raw `read_csv` query.
Creating it with explicit `vars = "k"` did not require binding the file. Name-only
inspection succeeded with zero owned SQL even before the synthetic CSV existed.
Typed inspection subsequently succeeded by inferring types from its contents:

```r
library(marginplyr)
library(dplyr)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", shared_home = FALSE)
path <- tempfile(fileext = ".csv")
x <- tbl(con, dbplyr::sql(paste0(
  "SELECT * FROM read_csv(", DBI::dbQuoteString(con, path), ", header=true)"
)), vars = "k")
options(marginplyr.audit_sql = TRUE)
inspect_grouping(x, .grouping = grouping_set(k))
stopifnot(nrow(last_sent_queries()) == 0L)
writeLines(c("k", "101"), path)
inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included
# "(k)"
writeLines(c("k", "source_tail"), path)
inspect_grouping(x, .grouping = grouping_set(where(is.character)))$included
# "(k)"
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(path)
```

The path, header, input object and row count stayed fixed; only the source value
changed. Both type answers preceded explicit collection. Each typed call
recorded one returned `selection_proxy` whose SQL wrapped a `LIMIT 0` query in
`system.main.query(...)`. The record described construction and outcome, not
independent proof of source nonexecution.

The [DuckDB 1.5 documentation][csv-doc] described CSV type detection as sampling
file rows and attempting value conversions. Its default sample was 20,480 rows;
`sample_size=-1` sampled the entire file. Both default and full sampling
reproduced locally; one data row sufficed. `read_csv_auto` and typed `.by`
confirmed adjacent paths of the same cause.

Native `collect(head(x, 0L))` had the same inferred type. This was expected
upstream behavior, not an allegation that DuckDB inferred incorrectly.
marginplyr invoked it under the stronger no-source-read inspection contract.
An explicit-schema control retained its declared type across changed values.
With a declared integer column and an incompatible text tail, inspection
succeeded while explicit collection raised the conversion error.

The zero-row exemption in ADR 0020 warranted metadata acquisition without source
work; zero returned rows alone did not warrant sampling values. The existing
dynamic-PIVOT protection reinforced that distinction on DuckDB but did not
prevent CSV binder sampling. A separate investigator independently reproduced
the CSV case and challenged the exemption interpretation before it was accepted.
[#1506][csv] covered this mechanism independently of the quoted-factory defect.

Inspection/query-policy tests covered ordinary zero-row reads and dynamic PIVOT
with source-work observation, but searches found no `read_csv`/`sniff_csv`
regression there. Counting the admitted metadata call would still report one
call in this witness and would not detect its binder's source work.

## Executed coverage and candidate dispositions

The parent hand-enumerated a composite rollup, composite cube and asymmetric
left-major product before using a separate syntax builder and recursive reference
enumerator. Expected fixed/included/omitted vectors, ordered occurrences, bits
and integer masks came from simple R operations, never the internal compiler.
List output was compared exactly, including types/class/names. Text output was
checked as display, allowing its documented separator ambiguity.

Counts below describe different comparisons and are not combined into a score.

| Hypothesis and evidence | Result / disposition | Next evidence |
| --- | --- | --- |
| Nested algebra, overlap or duplicate policies could change ordered plans. | Parent: 323 models, 1,742 exact full-plan matches and 196 expected duplicate refusals. Another 70 large models (512/729 occurrences) passed 210 keep/drop/error comparisons. | New algebraic boundaries or larger distinct families. |
| Public identity could disagree with operations or lazy carriers. | Parent: 30 local and 12 explicitly collected lazy operations matched `.id` and bare `grouping_id()`; separately, 32 models over dtplyr, Arrow, DuckDB and SQLite produced 696 exact plans and 72 expected refusals across both formats. | Other backends/versions. Shared-path operation agreement was not used as the independent oracle. |
| Formatting, fixed keys, duplicates or bit boundaries could corrupt values/types. | B: 324 independently enumerated keep/drop plans and 162 implicit/explicit fixed-key comparisons matched; 162 default/error comparisons included 58 exact duplicate-position diagnostics. Thirty, 31 and 32 dimensions with few sets and fixed keys, high/low bits, typed empty selections, separators, Unicode and newline names matched. | Other encodings, bounded-witness diagnostics or dimension families. |
| Binding retention or forwarded environments could change the selected column. | A: five binding spellings read once; two occurrences read twice; three constructor-head forms invoked once; qualified forwarded quosure retained its lexical selection. Bare/forwarded column-plus-specification names refused ambiguity, explicit disambiguation succeeded. | A distinct deferred-binding/capture shape. The quoted marker boundary was confirmed separately. |
| Derived type metadata could disagree with local collection. | C: 247 comparisons matched: 85 dtplyr aliases/arithmetic, 90 join/set, 45 Arrow and 27 DuckDB derived-input comparisons. Unsafe scalar overwrite and promoted-key cases had existing refusal protection. | Other constructible type/derivation families. |
| Inspection could mutate sources, consume readers or leak recording state. | C: 46 assertions matched over Mutable join roots on either side, ordinary DuckDB sequence-observed queries, audit-setting/reentry/failure/reset cases, SQLite captured names and Arrow reader batches. CSV sampling was isolated as a separate cause. | Other control transfers and source-binding mechanisms. |
| Dynamic PIVOT wrapped in another source construct could bypass protection. | Upstream rejected the attempted macro and `query_table` subquery constructions; sequence controls remained unchanged. Not a package defect. | A constructible different preparation shape. |
| Literal empty/splice, early injection or callback failures could establish another bug. | Canonical authored-empty witnesses correctly refused. A pre-captured quosure had already processed its splice upstream; a deliberately rebound namespace callback recursively called itself. These were not new defects. The reported alias residual in #1489 remained existing evidence. | Distinct capture boundary with ordinary R controls. |

The final cross-lane check included exact and display formats, local and lazy
inputs, valid and rejected calls, type/name/value/order controls, input
immutability and repeated recording state. Each confirmed cause was followed
through adjacent paths before returning to other candidates. No concrete
executable promising hypothesis remained pending within these finite lanes.

## Provenance, duplicate checks and limits

The pinned archive SHA-256 was
`67a22fd3e241b87260995d6c68941b97c7ae632ed9d1d25f485f4d585006dbbb`.
B compared 5,782 files against a new extraction with no extra/missing/changed
files; C independently re-archived and compared sources with no differences.
A detected an older pre-existing temporary source, excluded its initial logs,
and reran in a newly created pinned archive before reporting findings.

Scripts, short hypothesis/evidence/result/disposition/next records and logs
were kept outside the submitting worktree under
`/tmp/marginplyr-inspect-parent-20261108` and `/tmp/marginplyr-inspect-{A,B,C}`.
They were temporary investigation evidence, not committed replay assets.
The self-contained Issues retained minimal reproductions. Parent `oracle.R`,
`large.R`, `lazy.R`, `marker-replay.R` observation mode and corrected
`main-confirm.R` exited 0. `marker-replay.R --contract` exited 1 because inline
and stored plans differed. That assertion could pass after repair.

Harness errors were excluded: A corrected a script parse error; C's 39 completed
assertions preceded an upstream rejection of a fixture named with SQLite's
reserved `sqlite_` prefix, and the remaining seven ran with a valid name. That
earlier script was not called an overall pass. The parent's first current-main
control wrongly expected an explicit-schema read of a nonexistent path to bind;
DuckDB rejected it. The corrected existing-file incompatible-tail control passed.

Each R reproduction acquired one shared file lock before package loading,
explicitly disabled testthat parallel execution, and finished before the next
execution used that slot. All original handles were collected to terminal
completion, including parent 48939, 62724, 86168, 73541 and 79313. No full suite,
package build or rendering was used as exploratory feedback. All database
fixtures were synthetic and locally owned; no hosted database was contacted.

Fresh main remained the tested SHA before current-main replays and publication.
All-state searches included `marginplyr_keep_empty`, `quotation capture`,
`quoted grouping` and `read_csv`; local searches covered 772 Issues and 400
recent PRs. The marker and CSV-specific queries returned zero prior reports;
the broader quotation search led to #832 and unrelated summary rewrites.
Searches were completed through the API after an initial `gh search` query
encoding error. Both new Issues were independent, had no blockers and used
`bug` / `ready-for-agent` labels under the requested `to-tickets` workflow.

Environment: arm64 macOS Tahoe 26.6.2; R 4.6.1; marginplyr 0.1.0 from the tested
SHA; dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1, tibble 3.3.1,
vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1, DBI 1.3.0, DuckDB 1.5.5,
RSQLite 3.53.3 and Arrow 25.0.1.

Other OS/dependency versions, live PostgreSQL/SQL Server, remote files,
JSON/extension table-function inference, arbitrary dispatched methods and
unbounded graphs/resource limits were not established. They require their own
fixtures and source-work observations. The CSV finding did not measure remote
I/O, cost or large-file runtime, and implied no such measurement.

## Submission effects

Only this dated note was added. Its consumers were investigation readers; it
changed no package source, tests, examples, generated documentation, dependency,
build rule or verification tooling. The cumulative baseline was the merge-base
with freshly fetched main. These effects justified Review-ready `source`, with
package build/check, complete suite and snapshots omitted. No NEWS entry was
needed for this research record. Submitted-SHA verification, current-main
compatibility and separate Standards/Spec review records belonged on the PR.

[marker]: https://github.com/sayuks/marginplyr/issues/1505
[csv]: https://github.com/sayuks/marginplyr/issues/1506
[csv-doc]: https://duckdb.org/docs/1.5/data/csv/auto_detection
[constructors]: ../R/grouping-spec.R
[inspection]: ../R/inspect-grouping.R
[identity]: ../R/grouping-context.R
[prior]: inspect-grouping-deep-bughunt-2026-11-08.md
[systematic]: inspect-grouping-systematic-2026-11-07.md
[metadata-prior]: inspect-grouping-bughunt-2026-11-07.md
