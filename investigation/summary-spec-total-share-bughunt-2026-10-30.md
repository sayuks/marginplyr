# Follow-up bug hunt: summaries, Grouping specifications, and Total shares

Investigated: 2026-10-30
Repository snapshot: `28c449aa03fd5289916f0673e41c848cc126999e`

The investigation began in a new worktree created from fetched `main`, on
branch `bughunt/summary-spec-total-share-2026-10-30`. Local `main` and
`origin/main` agreed on the snapshot above. Four investigators divided
Grouping specifications, ordinary summaries and backend execution, contextual
shares, and summary-expression scope. The investigations from
[October 28](summarize-grouping-share-bughunt-2026-10-28.md) and
[October 29](summary-spec-share-bughunt-2026-10-29.md), including the
[follow-up](summary-grouping-share-followup-bughunt-2026-10-29.md), guided the
remaining boundaries. Their dated findings were not rewritten.

The requested scope was investigation and ticket publication. No package
implementation, package tests, dependency metadata, generated documentation,
or build exclusions were changed. The user delegated ticket boundaries and
publication, so the interactive breakdown checkpoint in `to-tickets` was
omitted. The independent findings became separate `bug`, `ready-for-agent`
tickets, with no blocking edges.

| Finding | Ticket | Priority | Established effect |
| --- | --- | --- | --- |
| F1 | [#1070](https://github.com/sayuks/marginplyr/issues/1070) | P2 | A function-local selector silently selected an outer environment's different column |
| F2 | [#1072](https://github.com/sayuks/marginplyr/issues/1072) | P2 | A caller warning was mistaken for Arrow absorption, refusing a supported lazy summary |

## F1: deferred local selections lost their execution scope

```r
data <- tibble::tibble(
  g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7)
)
cols <- "y"
dplyr::summarise(
  data,
  (function(cols) dplyr::across(dplyr::all_of(cols), sum))("x"),
  .by = g
)
# g = a, b; x = 3, 3
summarize_with_margins(
  data,
  (function(cols) dplyr::across(dplyr::all_of(cols), sum))("x"),
  .by = g
)
# g = a, b; y = 11, 7
```

The caller selected x through the inline function's own argument. The Margin
summary selected y through the enclosing environment's same-named binding.
Removing that enclosing binding made only the Margin call fail with
`object 'cols' not found`; ordinary dplyr still returned the x totals.

The same boundary affected `pick()`, `if_any()`, and `if_all()` inside an
inline function. For the predicate helpers, testing selected values against
four produced zero matches under ordinary dplyr but two and one matches under
the Margin verb. An inline type-predicate argument to `where()` and an
`across()` inside `local()` reproduced the scope loss. Parenthesizing the
across head or inserting an identity wrapper did not repair it. A literal x
selection and an enclosing binding also naming x were successful controls.
An explicitly injected quosure kept its intentionally captured environment.

Three hypotheses were separated: the resolver was given the original dot's
environment, dplyr's expression expansion selected a different execution
path, or planning reused an earlier selection value. A transient trace
observed callback `cols = x` and resolver `cols = y` within each of the two
group evaluations. The callback ran twice for those two groups. Wrapper and
parenthesis controls retained the failure, and literal-selection controls
retained the intended values. The resolver's explicit original-environment
argument explained both the missing-binding and wrong-column outcomes.

`local_summary_selection_expr()` embedded the summary quosure environment
into the generated deferred selection. `rewrite_summary_selections()` carried
that environment while traversing the written function body; a function's
subsequently created execution frame could not replace it. The ordinary
lexical-lookup promise in CONTEXT's *Contextual helper* entry and ADR 0019's
evaluation-boundary amendments supplied the expected behavior. Closed #604
concerned preceding-summary mask visibility, and #701 concerned planning a
first packed selection beside shares; neither recorded this runtime lexical
scope loss.

The retained coverage loop exercised three local input classes, fixed keys
and rollups, no share and unrelated Total/eligible Parent shares, four
selectors, and absent/matching/conflicting outer bindings: 180 cases. It
established 60 silent divergences, 60 missing-binding failures, and 60
successful matching-binding controls. A second investigator independently
reproduced all four helpers. No broader lazy-backend closure support was
inferred from this local finding.

Replay: `selection-scope.R --assert` exited 1 on equality with ordinary
dplyr. Report mode printed both results. `selection-scope-coverage.R`
verified the complete bounded matrix and exited 0 after checking its ordinary
controls and capturing the investigated divergences.

## F2: a caller warning was treated as evidence of Arrow absorption

```r
input <- arrow::arrow_table(data.frame(x = 1))
offset <- function() {
  warning("pulling data into R", call. = FALSE)
  1
}
dplyr::summarise(input, total = sum(x) + offset())
# Caller warning; returns an arrow_dplyr_query, collecting to total = 2.
summarize_with_margins(input, total = sum(x) + offset())
# marginplyr_error: Arrow cannot evaluate this summary, so Arrow would
# read the whole input to compute it.
```

The caller's scalar offset was evaluated while Arrow built a supported query.
Ordinary dplyr returned a lazy query; collecting it returned two. The Margin
call instead raised its absorption refusal, and an enclosing warning handler
received no caller warning. Changing only the warning text to an unrelated
message made the Margin call stay lazy, deliver the identical warning
condition to the caller, and return the correct values.

The hypotheses were a message-substring false positive, genuine Arrow
absorption, and a change in callback evaluation or private aliases. The
ordinary lazy-query control and the text-only change distinguished the first
from the others. The final reproduction used a preconstructed condition to
check identity, rather than merely comparing rendered warning text.

All 36 combinations reproduced the false refusal: Table, RecordBatch,
Dataset and a query over each; three marker-containing texts; and simple or
custom warning classes. The texts included case variation and the negative
sentence `Caller says: not pulling data into R here.`. Dataset controls were
especially useful because those inputs refused unsupported summaries rather
than absorbing them. The coordinating investigator independently replayed
the matrix and the desired-behavior assertion.

`is_absorbing_backend_warning()` checked the Arrow input class and searched
the warning's diagnostic for `pulling data into R`, without establishing its
origin. ADR 0025 explained why the actual Arrow warning had no dedicated
class and why its text was used; that mechanism did not make this caller
warning a backend response. CONTEXT's *External condition* contract supplied
the propagation requirement. Closed #254 and its discussion concerned
blocking genuine absorption; the caller-warning false positive was absent
from that ticket and the other issue-search results.

A genuine unsupported paste aggregate remained a separate control: ordinary
Arrow returned a local frame, and the Margin call correctly refused it.
The finding did not propose removing that protection, weakening the
supported Arrow-version range, or reading source rows to classify a warning.

Replay: `arrow-warning-reproduction.R` exited 0 after checking the ordinary,
changed-text, condition-identity, and genuine-absorption controls and reporting
36 mismatches. `--assert` intentionally exited 1 against the desired warning
propagation and lazy result.

## Exploration and bounded negative results

The retained scripts and logs were saved under
[`2026-10-30-summary-spec-total-share-hunt`](2026-10-30-summary-spec-total-share-hunt).
The counts below described separate bounded feedback loops, rather than an
exhaustive input space or disjoint sets of API calls.

| Loop | Executed scope | Established result |
| --- | --- | --- |
| Grouping oracle | Seed 20261030; 700 generated trees, keep/drop duplicates; local, dtplyr, Arrow Table, simulated PostgreSQL, plus live SQLite/DuckDB for the first 100 trees | 6,000 complete ordered-set, Grouping-bit, Grouping-identifier and set-identifier comparisons passed |
| Grouping boundaries | Empty frames and specifications, empty fixed-key selections, injected-quosure environments/depths, constructor/active-binding evaluation counts, S3 selection dispatch | 1,118 assertions passed |
| Ordinary expression differential | 34 forms × four preceding-summary policies × named/unnamed | 272 comparisons; eight raw differences were the intentional protection of quoted pick language, not new defects |
| Local result shapes | 19 result shapes × 19 alternate shapes × named/unnamed | 722 comparisons agreed on ordinary dplyr values/schema or paired refusal |
| Ordinary backend summaries | 39 forms × populated/empty × three extra-name layouts × five inputs | 1,170 comparisons: 970 ordinary successes, 200 paired errors; 33 documented Arrow absorption refusals and no unexplained difference |
| No-summary identifiers | Populated/empty × five inputs × four plans × fixed/unfixed keys | 80 results matched an ordinary per-grain dplyr oracle |
| Share denominator oracle | Seed 103010; 40 fixtures × eight duplicate-heavy plans × four inputs × fixed/unfixed keys | 2,560 Margin summaries matched the independently selected structural denominator |
| Share names | 45 spellings × source/output/fixed roles × local/dtplyr | 266 ratio checks passed; four refusals retained the ordinary unsupported `...` restriction |
| dtplyr measures | 42 spellings × bare/data-pronoun/across/pick source forms, with share/no-share controls | 168 paired cases introduced no share-specific failure or value difference |

The Grouping oracle included all five constructor kinds, nested composites,
overlap, identity products, repeated occurrences, injected selections,
redundant parentheses, and fourteen unusual-name candidates. The boundary
loop covered ordinary valid zero-column frames both with and without rows.
Its first exploratory oracle had a harness-only names-attribute mismatch and
a bare `.data` selection that ordinary tidyselect also refused; the counted
run corrected both. Forged specification internals outside the public
constructor grammar were not promoted to user-facing defects.

The shape loop included frame/zero-column/NULL/list/matrix/array/table,
Date/raw/complex and ordinary scalar shapes. The backend loop used local,
dtplyr, Arrow Table, SQLite and DuckDB. The Arrow-only differences were
checked against the documented absorption boundary rather than counted as
ordinary equality failures. An exploratory zero-column frame summary caused
a native Arrow crash in the ordinary dplyr control, before the Margin call;
that experiment established no package-specific defect. Its crashed process
was collected separately and supplied none of the completed loop counts.

The share oracle retained duplicate occurrences and covered ordinary,
overlapping, repeated, composite and empty-component rollups, cubes,
arbitrary repeated sets and products. It selected the strictly coarser parent
and Grand total occurrence independently from the inspected grouping sets,
then matched missing-safe fixed partitions and parent keys. Ratios used the
source summary values displayed by that backend. Missing numerators,
zero/missing denominators, negative values, empty inputs and missing keys were
included. SQL sums were explicitly coalesced to zero, so SQL empty-sum
behavior was not assumed equal to R's. The numerical SQL loop explicitly
disabled share-source checking; separate enabled-check source-type controls
exercised the database type rules. Arrow's documented contextual-share
refusal was excluded from the numerical backend set.

Additional source-attribute, ordinary-summary cardinality, per-output across
provenance, SQL source-type, and NA/NaN/infinite-key probes were checked with
ordinary or no-share controls. Unrecognized inert capture spellings such as
bare `alist()` and `bquote()` were excluded: the public summary reference
explicitly described their conservative traversal. The qualified supported
captures were successful controls.

A complex fixed-key candidate succeeded in individual dtplyr summary branches
but failed when a rollup combined them. Ordinary per-grain summaries, projected
to identical column order and combined with ordinary `union_all()`, failed
with the same class and diagnostic. Two investigators independently isolated
the failure to data.table's set-operation type check. It was classified as the
underlying combination operation's complex-type limitation, not an additional
package-specific defect; `complex-union-control.R` retained that distinction.

No new defect in Grouping specification expansion or inspection, or in the
numeric Parent/Total share denominator calculations, was established by these
loops. That negative result was limited to the tested grammar, fixtures,
data representations, backend shapes and dependency versions. Simulated
PostgreSQL inspection established compilation only. No live PostgreSQL,
hosted SQL service, different package version, unbounded plan, arbitrary
extension class, or concurrency/interruption behavior was established.

## Replay and publication checks

Run each retained R script from the repository root. It loads that checkout
through pkgload. Report scripts checked their ordinary controls and retained
known bad behavior; the two `--assert` modes deliberately failed on the
investigated snapshot. They were investigation signals, not failing package
regression tests or implementation changes.

```sh
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/grouping-oracle.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/grouping-boundaries.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/ordinary-expression-sweep.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/shape-differential.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/backend-one-set.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/no-summary-id.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/duplicate-oracle.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/selection-scope.R --assert
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/selection-scope-coverage.R
Rscript investigation/2026-10-30-summary-spec-total-share-hunt/arrow-warning-reproduction.R --assert
```

`replay-status.json` recorded the retained terminal outcomes and
`manifest.json` recorded artifact hashes. Adjacent logs retained the measured
results, with trailing whitespace normalized for publication. The scripts
made no assumption about the original absolute worktree
path; the DuckDB probes used `shared_home = FALSE`.

R 4.6.1 (2026-06-24), aarch64 macOS; dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, RSQLite 3.53.3,
DuckDB 1.5.5, rlang 1.3.0, tidyselect 1.2.1 and pkgload 1.5.3 were used.

The artifact change was Repository-only under the local-check policy:
`investigation/` was excluded from the source package and drove no generation.
The common `/Users/sayuks/.local/bin/marginplyr-review-ready` entry point was
used for this worktree as instructed. Its exact submitted commit and terminal
outcome, the publication checks, and the review record were recorded on the
pull request. CI monitoring was outside the requested scope.

The separate document-reference verifier failed on the focused-test recipe's
external `focus_workspace/run-focused.R` script. Running it in the untouched
main worktree at the same snapshot reproduced the identical exit-1 result:
257 paths across 368 maintained documents, with that one generated external
path rejected. The investigation notes were outside that scan. Existing
[#1074](https://github.com/sayuks/marginplyr/issues/1074) already described the
baseline defect, so no additional ticket or repair was made here.
`doc-references-baseline.log` retained the independent baseline result.
