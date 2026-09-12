# Property-based testing of Margin semantics

Investigated: 2026-09-12

## Summary

This investigation derived executable properties from marginplyr's repository
contracts and searched generated inputs without changing production code.

- Properties executed: 7 families.
- Generated cases: 3,350 across two seeds.
- Property assertions: 21,408.
- `CONFIRMED VIOLATION`: 2.
- `NO VIOLATION FOUND`: 5 property families as a whole, plus the unaffected
  backend/precondition regions of the two failing families.
- `INVALID PROPERTY`: 2 candidate properties rejected during harness
  calibration.
- `INCONCLUSIVE`: execution behavior on live PostgreSQL and the other SQL
  dialects not available in this environment.
- Discarded checks: 28 Arrow sort checks matching a confirmed failing
  precondition, discarded only after its minimal counterexample was saved.
- Existing suite: `testthat::test_local(".", reporter = "summary",
  stop_on_failure = TRUE)` passed in full after both violations were reproduced.

The executable harness is
[`property-based-testing-margin-semantics.R`](property-based-testing-margin-semantics.R).
It is repository-only investigation code and is not sourced by the package or
its test suite.

## Environment and reproducibility

The working tree was loaded from commit
`2da0fcd41ff901ef9e1804c32eda9144aa287f6a` with R 4.6.1. Relevant package
versions were dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1,
DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5, testthat 3.3.2, and pkgload 1.5.3.

The completed sweeps were:

```sh
MARGINPLYR_PBT_ITERATIONS=500 \
  Rscript investigation/property-based-testing-margin-semantics.R

MARGINPLYR_PBT_MODE=backends MARGINPLYR_PBT_BACKEND_ITERATIONS=75 \
  Rscript investigation/property-based-testing-margin-semantics.R

MARGINPLYR_PBT_MODE=nesting MARGINPLYR_PBT_NESTING_ITERATIONS=150 \
  Rscript investigation/property-based-testing-margin-semantics.R

MARGINPLYR_PBT_SEED=20260913 MARGINPLYR_PBT_ITERATIONS=250 \
  Rscript investigation/property-based-testing-margin-semantics.R

MARGINPLYR_PBT_SEED=20260913 MARGINPLYR_PBT_MODE=backends \
  MARGINPLYR_PBT_BACKEND_ITERATIONS=25 \
  Rscript investigation/property-based-testing-margin-semantics.R

MARGINPLYR_PBT_SEED=20260913 MARGINPLYR_PBT_MODE=nesting \
  MARGINPLYR_PBT_NESTING_ITERATIONS=50 \
  Rscript investigation/property-based-testing-margin-semantics.R
```

The default seed was `20260912`; the independent second seed was `20260913`.
Each generated failure recorded its seed, ordinal, complete case object, and
property name in `/private/tmp/marginplyr-pbt-failure.rds`. Shrinking was a
deterministic delta-debug pass: remove rows, fixed keys, dimensions, grouping
sets, summaries, options, and output columns one at a time, retaining a change
only while the same public failure remained. Values and names were then reduced
independently. Both confirmed failures remained on zero-row inputs, so no
random value was load-bearing.

Neither `quickcheck` nor `hedgehog` was installed or added to `DESCRIPTION`.
The custom harness was chosen because the difficult value being generated was
one coherent object containing a typed data frame, a legal grouping-plan AST,
backend constraints, and an oracle. A generic atomic or data-frame generator
would still require custom generation and shrinking to preserve those joint
preconditions.

## Contract basis and executed properties

The candidate relations from
[`metamorphic-testing-margin-semantics.md`](metamorphic-testing-margin-semantics.md)
were treated as established public-seam relations and widened over generated
input. The main direct sources were:

- [`grouping-spec.R`](../R/grouping-spec.R): union, prefix, subset, Cartesian
  product, empty set, and composite-dimension semantics.
- [`inspect-grouping.R`](../R/inspect-grouping.R): exact list format, plan
  order, Grouping bits, and Grouping identifiers.
- [`summarize_with_margins.R`](../R/summarize_with_margins.R): fixed keys,
  typed-missing labels, duplicate occurrences, result identity, Margin order,
  ordinary dplyr summary behavior, backend coverage, and shares.
- [`expand_with_margins.R`](../R/expand_with_margins.R): one input-row copy per
  grouping set and the documented expand-then-summarize workflow.
- [`nest_with_margins.R`](../R/nest_with_margins.R): source-row membership,
  `.keep`, and the local/dtplyr boundary.
- [ADR 0009](../design/adr/0009-distinguish-grouping-set-identifiers-from-grouping-identifiers.md),
  [ADR 0017](../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md),
  and [ADR 0018](../design/adr/0018-order-margin-results-by-grouping-structure.md).

| Property | APIs and executed backends | Preconditions and exclusions | Existing overlap and generated-test value | Result |
|---|---|---|---|---|
| Constructor algebra resolves to the combinatorial grouping-set oracle; bits and identifiers agree with every resolved set | `inspect_grouping()`; backend-independent plan executed locally | 1–7 dimensions; legal rollup, cube, product, nested union, and composite ASTs; more than 31 dimensions excluded | Existing tests hold representative fixed plans. Generation varied partitions, composites, duplicates, and non-syntactic names together. | `NO VIOLATION FOUND` |
| A Margin summary equals ordinary `dplyr::summarise()` applied separately to each grouping set on the same backend | `summarize_with_margins()` and synonym; local, dtplyr, RSQLite, DuckDB, Arrow | `n()` and `sum(..., na.rm = TRUE)`; typed-missing Margin label; supported expressions only; backend-owned aggregate types/NULL behavior retained | Existing metamorphic coverage compared direct and expanded summaries on one fixture. Generation varied row count, type, cardinality, missingness, set count, and duplicate occurrences. | `NO VIOLATION FOUND` |
| Expansion emits one correctly typed and labelled copy of every source row for every grouping-set occurrence | `expand_with_margins()`; local, dtplyr, RSQLite, DuckDB, Arrow | `.duplicates = "keep"`, `.margin_label = NULL`, `.id` retained; backend storage conversions honored | Existing tests use fixed row shapes. A unique generated row id made loss, duplication, or cross-occurrence movement observable. | `NO VIOLATION FOUND` |
| `.sort = "first"` and `"last"` equal the documented structural key over fixed keys, Grouping bits, missingness, values, and occurrence id | summary and expand; local, dtplyr, RSQLite, DuckDB, Arrow | Default unspecified order excluded; Arrow dictionary limitations honored; known duplicate-bit Arrow cases discarded only after confirmation | Existing tests cover fixed examples and individual boundary dimensions, not arbitrary grouping masks that make two dimensions share a bit pattern. | **`CONFIRMED VIOLATION` on Arrow**; no other violation found |
| Positive Total shares sum to one per fixed partition and occurrence; positive Parent shares sum to one beneath each immediate rollup parent; Grand total Parent shares equal one | `share_of_total()`, `share_of_parent()` through summary; local, dtplyr, RSQLite, DuckDB | Positive integer source and rollup; RSQLite eligibility explicitly established and `.check_share_source = FALSE`; Arrow excluded by contract | Existing metamorphic coverage uses one fixture. Generation varied partitions, missing keys, type, cardinality, and 1–4 rollup dimensions. | **`CONFIRMED VIOLATION` on dtplyr with a literal backtick join key**; no arithmetic violation found |
| Nest cells contain exactly the source rows represented by their outer key; dropping kept grouping keys equals `.keep = FALSE`; row permutation is immaterial; nest and nest-by agree apart from documented grouping | `nest_with_margins()`, `nest_by_with_margins()`, and expansion oracle; local, dtplyr | Non-empty inputs; 1–3 rollup dimensions; cell order and backend-owned cell class excluded | Existing metamorphic coverage uses one fixture. Generation varied row count, missing and duplicated keys, types, names, fixed partitions, and dimension count. | `NO VIOLATION FOUND` |
| `.id`, `grouping_bit()`, and bare `grouping_id()` agree with the inspected occurrence and absence-mask oracle | inspect and summary; all summary backends above | Up to 7 plan dimensions; collected remote scalar types canonicalized because their exact type is backend-owned | Existing identity tests use explicit masks. Generation crossed arbitrary repeated sets with values, types, and missing labels. | `NO VIOLATION FOUND` |

The generated data axes were row counts 0–24 locally and 0–16 lazily; 0–2
fixed keys; 1–4 data dimensions and 1–7 plan-only dimensions; 1–8 explicit
grouping-set occurrences; duplicate occurrences; character, numeric, logical,
factor, and ordered-factor keys; unused factor levels; missing and duplicated
values; and names including spaces, punctuation, reserved words, non-ASCII
text, and a literal backtick. Summary measures included negative, zero,
positive, and missing integers; share measures were positive by precondition.

Example-based tests alone are insufficient for the two highest-value regions:
the failure conditions are interactions. The dtplyr violation needs a share
join, a fixed key, and one particular legal name character. The Arrow violation
needs an opted-in sort, at least two columns, and equal nonconstant grouping-bit
patterns. None is represented by a small list of independently chosen boundary
fixtures.

## Confirmed violation 1: dtplyr shares reject a fixed key containing a backtick

### Property

A name-based `.by` selection accepted by dplyr remains a fixed partition key
for a supported dtplyr share calculation. The share reference explicitly
supports lazy dtplyr input and composite dimensions; the summary reference
defines `.by` as tidy-select and states that fixed-key semantics are shared by
local and lazy tables.

### Minimal counterexample

- Zero source rows.
- Two source columns.
- One fixed key whose complete name is the single character `` ` ``.
- One ordinary grouping dimension.
- One numeric scalar summary and one Total share.

```r
library(marginplyr)

x <- setNames(
  data.frame(character(), character()),
  c("`", "g")
)

summarize_with_margins(
  dtplyr::lazy_dt(x),
  total = dplyr::n(),
  share = share_of_total(total),
  .by = dplyr::all_of("`"),
  .grouping = rollup(g)
) |>
  dplyr::collect()
```

Expected: the same zero-row result shape as local execution. Actual:

```text
argument specifying columns received non-existing column(s): cols[1]='NA'
```

Using one row and the more readable name `a\`b` produces the same failure,
reported as a request for nonexistent column `a`. Parent shares fail on the
same fixed-key path.

The boundary probe was an ordinary dtplyr join:

```r
dplyr::left_join(
  dtplyr::lazy_dt(setNames(data.frame("", 1L), c("a`b", "v"))),
  dtplyr::lazy_dt(setNames(data.frame("", 2L), c("a`b", "w"))),
  by = "a`b",
  na_matches = "na"
) |>
  dplyr::collect()
```

It fails identically, while ordinary dtplyr `summarise(.by = all_of("a`b"))`
and marginplyr without a share succeed. The immediate cause is therefore the
dtplyr/data.table join path, not marginplyr's selection or summary compiler.
marginplyr exposes it because the row-matched share adapter delegates fixed-key
matching to `dplyr::left_join(..., by = join_names, na_matches = "na")` in
[`share.R`](../R/share.R).

### Scope, coverage, and severity

- Affected API/backend: `summarize_with_margins()` and its synonym when either
  share helper needs a denominator join; dtplyr only.
- Unaffected: share-free Margin summaries; ordinary `.by` grouping; share plans
  in which every occurrence is its own denominator and no join is built.
- Existing tests missed it because dtplyr share fixtures use syntactic fixed
  keys such as `fixed`; non-syntactic share tests exercise expression and
  diagnostic text, not a literal backtick in a dtplyr join key.
- Missing input dimension: interaction of unusual valid column name × fixed
  partition × dtplyr share join.
- Severity: **Medium**. It is a complete failure for legal public input, but
  limited to dtplyr shares and has a rename-before-call workaround.

## Confirmed violation 2: Arrow Margin order rejects repeated bit expressions

### Property

An opted-in Margin order works for every supported grouping structure.
`grouping_set(a, b)` nested in `rollup()` is explicitly a composite dimension,
whose columns are added or removed together. The order contract says a
composite needs no special rule because its columns share one Grouping bit.

### Minimal counterexample

- Zero source rows.
- Two character columns.
- One true two-column composite dimension.
- Expansion with one opted-in Margin order.

```r
library(marginplyr)

x <- arrow::Table$create(
  data.frame(a = character(), b = character())
)

expand_with_margins(
  x,
  .grouping = rollup(grouping_set(a, b)),
  .sort = "last"
) |>
  dplyr::collect()
```

Expected: a zero-row collected result, as local execution returns. Actual:

```text
Invalid: Invalid sort key column: Multiple matches for
FieldRef.Name(dplyr::if_else(.data[["..marginplyr_sort_1"]] %in% 2L, 1L, 0L))
```

The same failure occurs with `.sort = "first"`, with populated input, and
through `summarize_with_margins()`. `.id` and caller-visible Grouping-bit
summaries are not required.

[`margin_order_terms()`](../R/margin-operation.R) emits one bit term per plan
column. Columns in a composite dimension have the same omitted-occurrence ids,
so their expressions deparse to the same Arrow field name. Arrow materializes
both expressions and then refuses the ambiguous sort field. Arbitrary explicit
grouping sets can produce the same failure whenever two independently declared
dimensions happen to have the same nonconstant inclusion pattern.

### Scope, coverage, and severity

- Affected API/backend: `summarize_with_margins()` and synonym, and
  `expand_with_margins()`; Arrow; `.sort = "first"` or `"last"`.
- Unaffected: `.sort = "none"`; local, dtplyr, RSQLite, and DuckDB in the
  executed sweep; Arrow plans whose varying bit expressions are unique.
- Existing tests missed it because the test named “Arrow orders a composite
  factor dimension under expansion” uses `rollup(c(region, store))`. Public
  inspection shows that spelling creates two scalar dimensions and three
  rollup sets. The documented composite spelling is
  `rollup(grouping_set(region, store))`, which creates two sets and triggers the
  failure. The local composite test does use a nested `grouping_set()`.
- Missing input dimension: grouping-set structure × identical bit pattern ×
  Arrow expression naming × opted-in order.
- Severity: **Medium**. It makes a documented backend/API/option combination
  unusable; omitting `.sort` or collecting before ordering is a workaround.

## Rejected and inconclusive properties

Two candidates were classified `INVALID PROPERTY`, not package failures:

1. **Resolved dimension order equals source column order.** A nested union can
   first mention a later source column. The contract assigns meaning to the
   Grouping plan's own order, not the source data's order. The oracle was
   corrected to first appearance in the specification.
2. **A SQL aggregate over all missing values equals local R's aggregate.**
   `sum(..., na.rm = TRUE)` over an all-`NULL` SQLite group returns SQL `NULL`,
   while local R returns zero. The share reference explicitly says the source
   summary is the one the backend computes. The backend oracle was therefore
   changed to ordinary dplyr aggregation on the same backend.

Harness artifacts were kept distinct: an early generator allowed candidate
dimensions never named by any grouping set; Arrow rejected the unsupported
`.data[[name]]` aggregate spelling; turning dbplyr's normal missing-value
warning into an error made a supported sum look invalid; and zero-length
canonical vectors initially retained their storage type. Each disappeared
when the generator precondition or comparison was corrected and did not remain
as a public-seam counterexample.

Execution against live PostgreSQL, SQL Server, MySQL, Oracle, Redshift,
Snowflake, Spark SQL, and Teradata was unavailable. Simulator rendering would
not establish execution semantics, so generalizing the successful RSQLite and
DuckDB results to those dialects is `INCONCLUSIVE`.

## Property coverage gaps

The exploration found these weakly covered dimensions in the existing suite:

- Literal backticks inside otherwise valid column names, especially where a
  backend join rather than a selection consumes the name.
- True nested `grouping_set()` composite dimensions on Arrow; the existing
  test's `c(...)` spelling exercises separate scalar dimensions.
- Arbitrary grouping plans in which independent dimensions have identical
  inclusion masks.
- Interactions among `.sort`, `.duplicates = "keep"`, `.id`, and repeated bit
  patterns beyond the generated cases run here.
- Generated non-missing Margin labels and per-dimension named labels; this
  harness deliberately used typed-missing labels to make structural identity
  an unambiguous oracle.
- Aggregates other than `n()` and additive `sum()`, including backend-specific
  `mean()`, `min()`, `max()`, and multi-summary dependencies.
- More than seven generated plan dimensions and the documented 31-bit
  `grouping_id()` boundary.
- Empty nesting semantics as a generated property; non-empty nesting was used
  so the intentional `nest()`/`nest_by()` zero-key difference did not invalidate
  interface agreement.
- Live execution on confirmed native SQL backends other than DuckDB.

## Recommended durable tests

The two confirmed counterexamples should become focused regression tests after
their fixes. They belong beside the existing dtplyr Total-share comparison in
`test-share-backends.R` and the Arrow Margin-order tests in
`test-margin-order.R`. Both should assert the minimal public call first; the
Arrow test should use the documented `grouping_set()` composite spelling, and a
separate arbitrary `grouping_sets()` case should pin two independent dimensions
with the same mask.

Three generated properties are worth retaining at modest scale in the durable
suite: constructor algebra against the plan oracle, local summary/expansion
against per-set dplyr oracles, and share conservation over positive measures.
A fixed seed should be printed with every failure, while two or three explicit
boundary cases remain outside the random stream so seed changes cannot remove
empty input, factor, literal-backtick, or repeated-mask coverage.

No permanent PBT dependency is recommended from this investigation alone.
The existing testthat stack can run a small deterministic generator and the two
minimal regressions without adding another optional Suggest, release-matrix
registration, or backend skip path. The main maintenance cost here is the
domain-specific generator and its precondition-preserving shrinker; neither a
generic data-frame generator nor a framework removes that work.

If the repository later adopts several such generated suites, prefer direct
[`hedgehog`](https://search.r-project.org/CRAN/refmans/hedgehog/html/hedgehog.html)
use over
[`quickcheck`](https://stat.ethz.ch/CRAN/web/packages/quickcheck/refman/quickcheck.html).
Hedgehog supplies integrated shrinking, recursive/composable generators,
discards, and testthat integration with a small dependency surface. Quickcheck
provides convenient R data-frame generators and testthat integration, but
builds on Hedgehog and imports a wider convenience stack; marginplyr would
still need the same custom grouping-plan generator. At that point the automatic
shrink tree could justify the new Suggest and CI registration. Until then,
focused regression fixtures plus a bounded seed-fixed property loop have lower
maintenance cost.
