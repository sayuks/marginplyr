# Grouping specification, summary, and contextual-share bug hunt

Investigated: 2026-10-27

Baseline: `13dd7c30ac38fe3fc82e74c561d8795a6c4b6d17` (local `main` and
`origin/main` agreed when the investigation began).

The investigation used a new main-based worktree and changed no package
implementation, tests, generated documentation, or dependencies. Five
reproducible defects were registered as independent `bug` / `ready-for-agent`
tickets. None required another ticket to land first. Closed related issues
were read for scope; they were not reopened or modified.

## Environment and replay

Measurements used macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5,
RSQLite 3.53.3, rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3,
tibble 3.3.1, and bit64 4.8.6. Package code was loaded from the worktree
with `pkgload::load_all()`.

From the repository root, with those investigation dependencies installed:

```sh
Rscript investigation/2026-10-27-margin-summary-share-bug-hunt/reproduce.R
```

The script asserts the intended public behavior, catches failures to continue
through every finding, and exits 1 if any assertion fails. On the inspected
baseline, its five ordinary-operation controls passed and F1–F5 failed. These
intentional failures were evidence of unresolved defects, not passing package
tests. [reproduce-output.txt](reproduce-output.txt) preserves the baseline run
with trailing whitespace removed.
The probes are excluded from the source package by the existing investigation
build exclusion; they are not a new package test suite.

## Confirmed findings

| Finding | Issue | Observable failure | Blocking tickets |
| --- | --- | --- | --- |
| F1 | [#918](https://github.com/sayuks/marginplyr/issues/918) | Parenthesized `.data` bypassed share dependency checks | None |
| F2 | [#919](https://github.com/sayuks/marginplyr/issues/919) | Lazy shares disagreed with their visible denominator summaries | None |
| F3 | [#920](https://github.com/sayuks/marginplyr/issues/920) | integer64 keys caused an internal dtplyr share error | None |
| F4 | [#921](https://github.com/sayuks/marginplyr/issues/921) | Display-labelled dtplyr margins failed with fixed `.N` | None |
| F5 | [#922](https://github.com/sayuks/marginplyr/issues/922) | Valid Arrow dot-dot summary names failed in internal column operations | None |

### F1: parentheses around the data pronoun bypassed dependency validation

With input `g = c("a", "b")`, `x = c(1, 3)`, and
`first = c(100, 300)`, this call was incorrectly accepted:

```r
summarize_with_margins(
  data, v = sum(x), first = share_of_total(v),
  out = sum((.data)$first), .grouping = rollup(g)
)
```

The result displayed `first = .25, .75, 1` but returned
`out = 100, 300, 400`, from the same-named input column. Replacing only
`(.data)$first` with `.data$first` raised the intended `marginplyr_error`
refusing an ordinary summary's use of an earlier share. Literal
`(.data)[["first"]]` and multiple redundant parentheses also escaped.

The opposite direction escaped too: `first = sum(x)`, followed by
`v = sum((.data)$first)` and a share sourced from `v`, was admitted despite
the self-contained-source requirement. Both Parent and Total shares
reproduced. SQL simulator construction admitted the escaped forms; dtplyr
admitted double-subscript access, while dollar access could instead leak its
own alias-use error.

In `expression_data_symbols()` in `R/share.R`, the special pronoun branches
tested whether the object operand was a literal `.data` symbol. Whole-call
unwrapping did not unwrap that operand. Generic dollar/subscript traversal
then failed to report the column name. ADR 0010 owns both dependency rules;
ADR 0017 transfers them to Total shares. Closed #872 concerned reflective
environment lookups, rather than this operand-shape gap.

### F2: lazy relation reuse changed the denominator behind the visible result

A deterministic numeric scalar summary exposed this on dtplyr:

```r
counter <- 0L
stamp <- function(x) {
  counter <<- counter + 1L
  as.double(counter)
}
query <- summarize_with_margins(
  dtplyr::lazy_dt(data), v = stamp(x), s = share_of_total(v),
  .grouping = rollup(g), .margin_label = NULL, .id = "set"
)
dplyr::collect(query)
```

Construction left the counter at zero. One collection displayed
`v = 4, 5, 6`, but returned `s = 4/3, 5/3, 1`. The visible Grand total
source value was 6, requiring `4/6, 5/6, 1`. Parent shares reproduced.
Requesting both helpers displayed `v = 10, 11, 12`, with Parent shares
`10/9, 11/9, 1` and Total shares `10/6, 11/6, 1`, despite their identical
denominator in a one-dimension rollup. The eager control displayed `1, 2, 3`
and coherent shares `1/3, 2/3, 1`.

Each lazy join side reevaluated the staged relation; a manual ordinary
dtplyr self-join confirmed that this was normal backend behavior. The defect
was its exposure through the stronger Margin contract: ADR 0017 states that
a share's denominator is a row of the result the caller can see. The eligible
scalar-source rules in ADR 0010 and ADR 0017 contained no purity or determinism
restriction in the inspected snapshot. The finding concerns incorrect ratios,
not a general callback-count promise.

Live DuckDB also reproduced with `v = dbplyr::sql("random()")` and the default
share-source check. After `SELECT setseed(0.5)`, a combined request displayed
approximately `.662, .664, .0754`, with Parent shares `.844, .846, 1` and
Total shares `1.68, 1.68, 1`; the visible-row ratios were approximately
`8.788, 8.808, 1`. SQLite reproduced with its documented source-check opt-out,
but its integer-valued random result made DuckDB's double-valued example the
cleaner SQL evidence.

The ticket requires result/denominator coherence while preserving ADR 0020.
It does not authorize implicit construction-time `collect()` or `compute()`;
any different supported volatile-summary contract requires an explicit decision.

### F3: integer64 grouping keys reached an invalid unnamed key holder

For `g = bit64::as.integer64(c(1, 2))` and `x = c(1, 3)`, an ordinary
dtplyr Margin rollup with `.margin_label = NULL` correctly returned source
summaries `1, 3, 4`. Adding either share helper made collection raise:

```text
Error in vctrs::vec_duplicate_any(vctrs::new_data_frame(list(...))):
  STRING_ELT() can only be applied to a 'character vector', not a 'NULL'
```

Ordinary dtplyr grouped sums and local shares succeeded. Fixed integer64 keys
also reproduced, including a fixed-only Total share without a variable
Grouping specification. Large distinct integers above double's exact range
and missing keys reproduced in the broader key probes.

An in-memory trace, removed after the probe, established that the grouped-result
guard received intact integer64 keys and the integer Grouping set identifier.
It built a fully unnamed multi-column data frame for vctrs. The independent
control `vec_duplicate_any(new_data_frame(list(id, g)))` raised the same
internal error; naming both columns returned `FALSE`. An unnamed integer64
column alone also returned `FALSE`. Closed #652 repaired display collisions
in this guard; this was a distinct structural failure before valid keys could
be tested for duplication.

### F4: a fixed `.N` collided with the generated branch-size expression

For fixed `.N = c("u", "v")`, dimension `g = c("a", "b")`, and
`v = c(1L, 3L)`, this constructed a lazy result but failed at collection:

```r
summarize_with_margins(
  dtplyr::lazy_dt(data), total = sum(v),
  .by = all_of(".N"), .grouping = rollup(g)
)
```

The error was `cannot change value of locked binding for '.N'`; the expected
four rows had totals `1, 3, 1, 3`. Inherited grouping, all Margin-order
choices, and a public identifier reproduced. One-set plans, fixed-only
summaries, `NULL`/`NA_character_` labels, and the equivalent ordinary dtplyr
labelled union succeeded. Renaming only the fixed key also removed the failure.

The ranked hypotheses were unsafe branch labelling, union name handling, and
Margin ordering. Changing sort/identifier did not help; typed-missing labels
did. The generated call used `rep("Total", .N)` after the subtotal had restored
the fixed `.N` column. `label_margin_branch()` in `R/margin-label.R` used
`dplyr::n()` for branch size when the omitted summary dimension was absent;
dtplyr translated that to the special `.N`. Safe-name handling covered the
omitted dimension rather than the retained fixed key. Closed #695 had covered
special fixed keys but its fixed-key tests used an empty variable set, missing
this labelled-omission combination.

### F5: internal Arrow selection rejected a valid ordinary output name

An Arrow table holding `x = 1:2` supported ordinary
`summarise(source, ..1 = sum(x))`, including collection, returning `..1 = 3`.
The corresponding Margin call, with no grouping at all, raised
`vctrs_error_names_cannot_be_dot_dot` during construction. `...` and other
positive dot-dot-number names, included-key plans, and rollups reproduced.

The no-grouping traceback reached `label_margin_branch()` and
`reorder_margin_columns()` in `R/utils.R`: the non-data-frame path selected
columns and revalidated unrelated summary output names. With an included key,
the internal key-restoration `rename()` in `summarize_margin_union()` failed
earlier for the same reason. Ordinary Arrow evaluation ruled out an unsupported
summary expression. Closed #827 repaired the local case, leaving these lazy
column operations outside its tests.

dtplyr showed the same premature construction failure, although its ordinary
default tibble collection also applies name repair. Ordinary dtplyr
`as.data.frame()` supported the names. The ticket therefore preserves that
conversion path without promising a change to dtplyr's collection policy.
Ordinary SQL summary naming already refused these names and was excluded.

## Breadth, controls, and excluded leads

The exploratory harnesses used fixed seeds where inputs were random. Their
purpose was to find counterexamples, not to establish complete backend support.

| Area | Executed comparison | Observed outcome |
| --- | --- | --- |
| Grouping grammar | 1,500 valid random trees, seed 2765, depth at most 3; all five kinds, composites, overlap, empty identities, occurrence order, keep/drop, and Grouping bits against an independent evaluator | All matched |
| Grouping selection | 1,500 random selections, seed 7931, against direct tidyselect; checked acceptance and exact accepted column names | All matched; deliberately invalid selections could differ in condition class |
| Typed selection backends | 14 selections × 5 constructors × dtplyr/Arrow/DuckDB comparisons to local inspection | 210 comparisons matched |
| dtplyr copied-column metadata | 100 source/destination type combinations, comparing zero-row metadata with collected metadata | All matched |
| Ordinary summaries | 32 named/unnamed expression forms, including frames, pick, eval/capture, closures, and across | 64 local dplyr comparisons matched |
| Across evaluation | 4 call wrappers × 15 function forms × 7 unpack forms × 3 selectors | 1,260 local comparisons matched results and factory invocation counts |
| Ordinary cube summaries | 40 generated inputs × 4 backends, seed 927 | 160 executions; local/dtplyr matched, SQL all-missing SUM differences were upstream behavior |
| Contextual shares | 14 random inputs × 4 plans × fixed/no-fixed × sum/count/mean × 3 lazy backends, seed 2765 | 1,008 comparisons matched; composite used `rollup(grouping_set(g, h))` |
| Independent share oracle | 30 random inputs; source summaries and matching denominators derived directly from source rows | 9,669 row assertions passed |
| Public names | 52 summary names × 5 backends; 37 key/identifier names × 3 roles × 5 backends × 2 sort choices | Identified F4/F5; reserved source names rejected by ordinary tidyselect were excluded |
| Cross-backend edge fixtures | 7 fixtures × 5 backends × 3 sort choices, including empty data, missing keys, logical keys, nonfinite values, and Unicode | No additional confirmed Margin defect |

Additional local/dtplyr probes covered factor NA levels, Date, POSIXct,
difftime, list/matrix/packed keys, nested independent data masks, and reading
numeric/factor/Date dimensions inside summaries while display labels changed.
No additional package defect was established. Raw/complex/list-key rejections
already imposed by data.table were excluded. Arrow distinguished signed zeros
in grouping; SQLite collapsed NaN to NULL and exposed different ordinary
numeric/logical prototypes. Those backend differences were not ticketed.
Column-less dtplyr summaries returning zero rows were already an explicit
documented backend boundary in *Summaries a lazy backend cannot carry*.

The grammar audit ran existing focused tests with:

```r
devtools::test(
  filter = paste0(
    "^(grouping-plan|inspect-grouping|injected-nested-grouping|",
    "cyclic-grouping|grouping-selection-budget|selection-helper-arguments)$"
  ),
  stop_on_failure = TRUE
)
```

It reported 1,914 passes, zero failures, warnings, or skips, in 11.4 seconds.
These tests complemented the exploratory probes; they did not assert F1–F5.

## Measured performance candidate, not a confirmed bug

`inspect_grouping()` of a cube containing repeated selections of the same
column, with `.duplicates = "drop"`, returned exactly two sets. Compilation
still expanded the raw cube before dropping duplicates:

| Repeated arguments | Returned sets | Elapsed seconds |
| --- | --- | --- |
| 1 | 2 | 0.036 |
| 6 | 2 | 0.049 |
| 10 | 2 | 0.016 |
| 14 | 2 | 0.190 |
| 18 | 2 | 3.345 |

`expand_cube()` generated all subsets before duplicate handling in
`compile_grouping_spec_impl()`; names-only inspection also compiled twice.
Measurements stopped at 18 arguments. This suggested an optimization for
duplicate-heavy `drop` plans, but no output-sensitive runtime guarantee was
found. Cube construction was expressly combinatorial, and `keep` exposed every
occurrence by contract. The observation was retained here without a bug ticket.

## Limits and publication

This was a local investigation of the pinned snapshot, not a proof that every
input, R version, or remote dialect was correct. No live PostgreSQL, BigQuery,
Snowflake, or other hosted database was tested. Arrow shares remained an
explicitly unsupported feature. No fixes were attempted.

The research artifacts were Repository-only changes under
`design/agents/local-checks.md`; no Review-ready package boundary was required.
Before publication, both repository linters and the context-budget and
document-reference verifiers were run. Their terminal results were recorded
in the pull request. CI monitoring was outside the requested work.
