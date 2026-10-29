# Follow-up bug hunt: summaries, Grouping specifications, and contextual shares

Investigated: 2026-10-29
Repository snapshot: `ee4321789ca60b6259c53df2852f1574aaede584`

The investigation began in a new worktree created from `main`, on branch
`bughunt/summary-grouping-share-2026-10-29`. Local `main`, fetched
`origin/main`, and the initial worktree HEAD agreed on the snapshot above.
Four investigators divided Grouping specification/inspection, local summary
evaluation, contextual shares, and derived backend inputs. Findings were
minimized against ordinary dplyr and package-contract controls before tickets
were published.

The requested scope was investigation and ticketing. No package implementation,
package tests, dependency metadata, or generated documentation was changed.
The user delegated ticket boundaries and publication, so the interactive
breakdown checkpoint in `to-tickets` was omitted. Four independent tickets were
published with `bug` and `ready-for-agent`; none blocked another.

| Finding | Ticket | Priority | Established effect |
| --- | --- | --- | --- |
| F1 | [#1020](https://github.com/sayuks/marginplyr/issues/1020) | P2 | An unnamed mixed frame/scalar summary silently split into NA-filled columns |
| F2 | [#1021](https://github.com/sayuks/marginplyr/issues/1021) | P2 | Computed pronoun subscripts bypassed both contextual-share dependency rules |
| F3 | [#1022](https://github.com/sayuks/marginplyr/issues/1022) | P2 | Share-source validation broke working dtplyr aggregates over input measure `.BY` |
| F4 | [#1023](https://github.com/sayuks/marginplyr/issues/1023) | P2 | An arranged Arrow input retained obsolete sort references across summary aliases |

## F1: unnamed mixed outputs lost their original incompatibility

```r
data <- data.frame(g = c(1, 1, 2), x = 1:3)
frame_or_scalar <- function(x) {
  if (length(x) > 1L) data.frame(z = sum(x)) else sum(x)
}
dplyr::summarise(data, frame_or_scalar(x), .by = g)
# Refused: results must be compatible vectors across groups.
summarize_with_margins(data, frame_or_scalar(x), .by = g)
# g = 1: z = 3, assigned-name column = NA
# g = 2: z = NA, assigned-name column = 3
```

The same unnamed summary returned a data frame in one group and an integer in
another. Ordinary dplyr refused that incompatible pair. The Margin verb
instead returned two output columns, filling each absent value with `NA`.
Reversing group encounter order reversed those output columns. An explicitly
named summary retained the ordinary type refusal.

Seven non-frame shapes, three local input classes, and two encounter orders
produced 42 ordinary refusals and 42 Margin acceptances. A 16-by-16 result-shape
sweep found 48 differences, all frame/non-frame mixtures; the other 208 pairs
agreed on values or both refused. Homogeneous frame/scalar and sequential
expanded-output controls passed. The mixed closure ran once per group.

Three hypotheses were separated: per-value wrapping erased the original type
distinction, private key restoration split the output, or duplicate evaluation
changed the closure's result. A direct dplyr control wrapping only scalar values
into one-column frames reproduced the split without private keys. Callback
counts excluded duplicate evaluation.

`wrap_assigned_local_summaries()` used `local_assigned_summary_value()` to
preserve immediate frame expansion under ADR 0028. That helper returned frames
unchanged and converted scalars into frames before dplyr combined group result
types. Its carrier therefore hid the original incompatibility and let vctrs
union the frame columns. Closed #430 and #619 covered naming and expansion
timing, rather than cross-group type compatibility.

The finding was limited to heterogeneous results within one grouping-set
branch. A separate probe varying schemas only between grouping sets hit the
existing branch-schema refusal; the ticket did not promise new support for
arbitrary between-set schema variation.

Replay: `summary-probes.R --assert` exited 1 on the missing type refusal.
Report mode printed the successful and refused controls; `--wide` added the
shape matrix and local-expression differential.

## F2: evaluated pronoun subscripts disappeared from dependency analysis

```r
data <- tibble::tibble(
  g = c("a", "b"), x = c(1, 3), first = c(100, 300)
)
summarize_with_margins(
  data, value = sum(x), first = share_of_total(value),
  derived = sum((.data)[[paste0("fir", "st")]]),
  .grouping = rollup(g)
)
# first = 0.25, 0.75, 1; derived = 100, 300, 400
```

The ordinary summary consumed the source input's `first` payload while the
returned `first` column held shares. The documented same-call dependency rule
required refusal. Literal parenthesized subscripts were refused correctly;
unparenthesized `.data` capture also retained the check. Computed calls and a
lexical name holding the target string exposed the missing path. Reversing the
direction also admitted a share source that read an earlier ordinary alias,
contrary to the self-contained-source rule in ADRs 0010 and 0017.

A second boundary concerned reads *inside* an environment-pronoun index:

```r
foo <- 10
bar <- 30
data <- data.frame(
  g = c("a", "b"), x = c(1, 3), first = c("foo", "bar")
)
summarize_with_margins(
  data, total = sum(x), first = share_of_total(total),
  derived = (.env)[[dplyr::first(first)]],
  .grouping = rollup(g)
)
# first = 0.25, 0.75, 1; derived = 10, 30, 10
```

The index read the old source payload, so this also silently bypassed the
ordinary-summary/earlier-share refusal. A source selecting an environment
binding with a condition on an earlier ordinary alias bypassed the opposite
rule. Bare, parenthesized and qualified subscripts reproduced that boundary.
Independent literal environment lookups remained valid controls.

Both helpers reproduced both dependency directions. The data-pronoun loop
established 12 violations and the environment-index extension another 12.
Direct caller spellings reproduced the failures without injected language.
Successful literal refusals excluded a blanket exception for parentheses;
independent dynamic input reads distinguished forbidden dependencies from
ordinary lookups.

`expression_data_symbols()` recognized the pronouns but returned no target
for computed data subscripts, or the lexical index variable rather than its
target. Its environment-pronoun branch returned before walking the evaluated
index. These were two sites of one delivery: evaluated subscripts must retain
the dependencies they resolve and evaluate. An unresolved target needed a
conservative representation, rather than caller-code execution to guess it.
Closed #918 covered parenthesized literal and symbol reads, not these computed
subscripts. The silent wrong-answer examples were established locally; lazy
backends' own translation failures were not described as successful execution.

Replay: `share-probes.R --focused --assert` covered the data-pronoun cases;
`environment-index-probes.R --assert` covered the index-expression extension.
Both exited 1 against the desired dependency refusals.

## F3: the `.BY` input measure lost working aggregate evaluation

```r
source <- dtplyr::lazy_dt(tibble::tibble(
  g = c("a", "b"), .BY = c(1, 3)
))
summarize_with_margins(
  source, value = sum(.BY), fraction = share_of_total(value),
  .grouping = rollup(g)
) |>
  dplyr::collect()
# cannot change value of locked binding for '.BY'
```

Ordinary dtplyr grouped summaries returned 1, 3; a no-share Margin rollup
returned 1, 3, 4. Adding either helper failed at collection.
Direct aggregation and preceding ordinary `across(.BY, sum, .names = "value")`
both reproduced: four minimal failing combinations. Renaming only the input
measure made the shares succeed with 0.25, 0.75, 1. Construction stayed lazy
and the source table remained unchanged.

Candidate explanations were an upstream grouping-key restriction, changed
aggregate evaluation under package validation, denominator matching, and source
mutation. Ordinary/no-share and unchanged-source controls excluded the first
and last; failure during the initial wrapped aggregate excluded denominator
matching. An upstream pass-through wrapper around the same aggregate reproduced
the locked binding. data.table verbose output distinguished `GForce optimized j`
from `GForce is on, but not activated` under the wrapper. Sum, mean, min, max,
median, sd and var reproduced that upstream wrapped/unwrapped boundary;
`dtplyr-aggregate-probes.R` retained the seven-function control.

The package's direct and `across()` scalar/type checks introduced that wrapper
without shielding this input spelling. The source output was ordinary `value`:
closed #696 concerned special output names, and #703/#952/#999 concerned
Margin dimensions or fixed keys. User functions and special names already
failing under ordinary dtplyr were excluded. The ticket retained source type
and cardinality validation and ADR 0020's lazy construction requirement.

Replay: `share-probes.R --focused --assert` included these four failing calls,
ordinary/no-share/renamed controls, and the optional `--trace` wrapper probe.

## F4: arranged Arrow input exposed private summary keys to stale sort metadata

```r
source <- dplyr::arrange(
  arrow::arrow_table(data.frame(g = c("a", "b"), x = c(1, 3))), g
)
dplyr::summarise(source, total = sum(x), .by = g) |>
  dplyr::collect()
# g = a, b; total = 1, 3
summarize_with_margins(source, total = sum(x), .by = g) |>
  dplyr::collect()
# Invalid sort key column: No match for FieldRef.Name(g)
```

The Margin call constructed an Arrow query. Its collection failed against a
schema holding the package's private grouping key instead of `g`. An
included-only `grouping_set(g)` failed too, under every `.sort` option.

All 48 focused combinations failed: Table/Dataset, empty/populated,
ascending/descending source order, fixed/dimension grouping, and three sort
choices. Ordinary grouped-summary and explicit lazy-collapse controls passed.
A second investigator independently replayed the six minimal group-role/sort
cases. The controls checked values and schema; the broad backend differential
compared row multisets and did not independently establish Margin order.

The hypotheses were an unsupported caller aggregate, inherited source sort
metadata crossing the private-key projection, and final Margin ordering.
Ordinary summaries excluded the first and `.sort = "none"` excluded a failure
specific to final ordering. Copying the key to a private alias before an
ordinary Arrow summary reproduced the stale sort field. Passing the source
through public `dplyr::collapse()` made the Margin summary collect without
materializing or reading the source during construction.

The summary adapter lacked the lazy input-schema boundary already present at
the expansion boundary fixed by #895. That issue concerned re-expansion;
#541 concerned duplicate Grouping-bit sort expressions. Neither covered this
summary input boundary. ADRs 0018 and 0020 governed output ordering and query
laziness.

Ordinary Arrow grand summaries that themselves discarded the arranged column
also failed upstream. They were excluded from the ordinary-success evidence.
The smallest finding retained that column as a public grouped key and worked
under ordinary dplyr. Replay: `arrow-probes.R --assert` exited 1 after printing
all 48 failures and successful ordinary/collapse controls.

## Exploration, prior evidence, and bounded negative results

The prior investigations
[2026-10-28](summarize-grouping-share-bughunt-2026-10-28.md) and
[earlier 2026-10-29](summary-spec-share-bughunt-2026-10-29.md) guided the gaps
explored. Their dated claims were not amended. Their constructor evaluation,
empty-pick, special fixed-key, signed dot-dot, independent-across-source, sf
metadata and parsed-language findings were distinguished from the new tickets.

All retained scripts and captured results were saved in
[`2026-10-29-summary-grouping-share-bughunt`](2026-10-29-summary-grouping-share-bughunt).
Counts below described separate bounded loops, not one exhaustive input space.

| Loop | Executed scope | Established result |
| --- | --- | --- |
| Grouping selections | 70 supported unusual-name layouts × 51 forms × two positions | 7,140 comparisons matched direct tidyselect success values or outer condition classes |
| Identifier boundaries | 0, 1, 2, 30, 31, 32, 33 dimensions | 14 checks passed, including maximum integer mask and documented larger-plan inspection/helper behavior |
| Mutable dtplyr inspection | 156 typed-selection cases and source-state checks | 312 assertions passed without changing caller tables |
| Nested factories | Five unrecognized factory spellings, top-level/nested | Ten boundary assertions passed |
| Local result shapes | 42 mixed-output controls; 256 shape pairs | F1 reproduced; 48 pair differences all belonged to F1 |
| Local expression differential | 1,200 seeded sequences | 752 ordinary successes and 448 paired errors; no additional value/schema difference |
| Share denominator oracle | 14 fixtures × six plans × two fixed-key policies × four inputs | 672 results and 27,160 row/source denominator ratios passed |
| Derived backend inputs | Eight transformations × five inputs × three plans × three sorts × two duplicate policies | 720 comparisons: 666 successful comparisons, 36 documented dtplyr refusals and 18 F4 failures; no successful-result value mismatch |
| Focused share boundaries | Four `.BY` failures, 12 computed data-pronoun violations | F2/F3 reproduced with ordinary/no-share/literal/renamed controls |
| Environment indices | Three forms × two helpers × two dependency directions | 12 F2 violations; six independent environment controls passed |
| Arranged Arrow summary | 48 combinations | F4 reproduced; ordinary grouped and lazy-collapse controls passed |

The Grouping loop reported `Final checks: 7476 signals: 0`. It included
punctuation, keywords, dot/pronoun/dot-dot spellings, whitespace, Japanese,
Unicode normalization, typed predicates, operator qualifications, empty and
invalid selections, and renaming refusals. A first scanner's 692 raw signals
were reduced to the existing unsupported `...` input-name restriction; direct
dplyr controls reproduced it. The supported-name replay above excluded it.
No new Grouping specification expansion or inspection defect was established.

The local expression loop varied overwrites, NULL omission, packed/list outputs,
frame expansion, selectors, function forms and empty input. The share oracle
matched each ratio to the denominator source value displayed in the same
backend's result, within missing-safe fixed partitions; local, dtplyr, SQLite
and DuckDB participated. Arrow's documented share refusal was excluded.
An initial share-oracle invocation ended in a harness error and was not counted
as evidence; the completed fresh invocation supplied the reported counts.

The derived-input loop covered joins, anti-joins, union, distinct, filtering
and arithmetic, ordering, renaming, and identity on local, dtplyr, DuckDB,
SQLite and Arrow inputs. Its 36 dtplyr metadata refusals followed ADR 0020;
ordinary materialization established the comparison data but was never used as
a hidden Margin-operation read. The loop normalized row order and factor
representation and used each backend's collected input as its local oracle.
Its output was a diagnostic report, not an assertion that every refusal was a
bug. Seeds were 68291 for Grouping, 82910 for local sequences and 20261029 for
the share oracle; the derived fixture itself was deterministic.

Named contextual-helper argument spellings were not promoted without an
established violated contract. A function-valued parent diagnostic retaining a
private group label was also not promoted, because the Condition context
contract permitted leaving a context the restater could not reconstruct.
No live PostgreSQL/hosted service, other dependency version, arbitrary S3
extension, interruption stress, or unbounded plan was established by these
loops.

## Replay and publication checks

Run from the repository root. Every retained R script loaded that checkout
through pkgload. Successful report modes captured known bad behavior and
verified controls; `--assert` modes deliberately exited 1 on the investigated
snapshot. These were investigation signals, not failed package regression tests.

```sh
Rscript investigation/2026-10-29-summary-grouping-share-bughunt/grouping-probes.R
Rscript investigation/2026-10-29-summary-grouping-share-bughunt/summary-probes.R --wide
Rscript investigation/2026-10-29-summary-grouping-share-bughunt/share-probes.R
Rscript investigation/2026-10-29-summary-grouping-share-bughunt/environment-index-probes.R
Rscript investigation/2026-10-29-summary-grouping-share-bughunt/arrow-probes.R
Rscript investigation/2026-10-29-summary-grouping-share-bughunt/backend-derived-probes.R
```

`replay-status.json` recorded the coordinating agent's retained terminal exit
codes. Adjacent report and assertion logs recorded output. The artifact manifest
recorded retained bytes. Historical temporary exploratory scripts were not
needed to replay the reported loops.

The existing focused package suite completed with exit 0:

```sh
NOT_CRAN=true Rscript -e 'devtools::test(filter="^(grouping-interface|grouping-plan|summarize-operation|share|share-backends|margin-order)$", reporter="summary")'
```

R 4.6.1 on aarch64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, RSQLite 3.53.3, DuckDB 1.5.5, Arrow 25.0.1,
rlang 1.3.0, tidyselect 1.2.1 and pkgload 1.5.3 were used.

This was a Repository-only change under `design/agents/local-checks.md`:
`investigation/` was excluded from the source package and drove no generation.
The Package-affecting Review-ready check was not required. Publication checks
and the review record were recorded on the pull request. CI monitoring was
outside the requested scope.
