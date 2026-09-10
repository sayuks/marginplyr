# Metamorphic testing of Margin semantics

Investigated: 2026-09-10

Related issue: [#527](https://github.com/sayuks/marginplyr/issues/527)

## Summary

This investigation derived metamorphic relations from marginplyr's documented
contracts and exercised the public API without changing production code.

- Candidate relation families considered: 31.
- Adopted and executed: 25.
- `CONFIRMED VIOLATION`: 1 relation family.
- `NO VIOLATION FOUND`: 24 relation families.
- `RELATION INVALID`: 6 candidates rejected from the executable set.
- `INCONCLUSIVE`: no adopted relation as a whole; live PostgreSQL and the
  generic SQL dialects other than RSQLite were not executable in this
  environment, so backend generalization beyond the executed backends remains
  inconclusive.
- Automated checks: 7,349, plus the minimized reproducer and targeted boundary
  probes.
- Existing suite: `testthat::test_local(".", reporter = "summary",
  stop_on_failure = TRUE)` passed in full after the violation was reproduced.

The confirmed violation is a silent result-type change on RSQLite's portable
`UNION ALL` path. Reordering semantically equivalent grouping sets changes an
all-missing character Margin dimension from `character` to `logical` when the
Grand total set is first. It affects `summarize_with_margins()` (and its
British-spelling synonym) and `expand_with_margins()` with `NULL` or
`NA_character_` Margin labels.

The throwaway harnesses were kept outside the repository:

- `/private/tmp/marginplyr-metamorphic.R`
- `/private/tmp/marginplyr-metamorphic-backends.R`
- `/private/tmp/marginplyr-metamorphic-algebra.R`
- `/private/tmp/marginplyr-metamorphic-shares.R`
- `/private/tmp/marginplyr-metamorphic-nesting.R`
- `/private/tmp/marginplyr-metamorphic-relabel.R`
- `/private/tmp/repro-sqlite-grouping-set-order-type.R`

## Contract used to derive the relations

The relations below came from repository contracts rather than generic
expectations about grouping:

- [Grouping specifications](../R/grouping-spec.R) define `grouping_sets()` as
  union, `rollup()` as hierarchical prefixes, `cube()` as every subset,
  `grouping_spec()` as a Cartesian product, and a nested `grouping_set()` as one
  composite dimension.
- [The Margin summary reference](../R/summarize_with_margins.R) states that
  confirmed native SQL and portable `UNION ALL` backends have the same
  semantics. It also states that `NA_character_` and `NULL` insert typed missing
  values.
- The same reference explicitly limits structural equivalence between `.by`
  and an always-included grouping dimension: label conversion and collision
  validation need not be equivalent.
- [ADR 0009](../design/adr/0009-make-grouping-plan-occurrences-first-class.md)
  makes `.id` occurrence- and plan-order-dependent, while a bare
  `grouping_id()` corresponds to the plan's absence mask.
- [ADR 0012](../design/adr/0012-make-margin-labels-type-stable.md) distinguishes
  source missing values, factor NA levels, and typed-missing Margin labels.
  Factor level order is part of the contract only on factor-restoring backends.
- [ADR 0018](../design/adr/0018-centralize-margin-ordering-policy.md) makes row
  order unspecified under `.sort = "none"`; an opted-in Margin order is
  structural, puts missing values last, and is independent of display labels.
  Factor values follow restored level order.
- [ADR 0010](../design/adr/0010-make-shares-structural-margin-operations.md) and
  [ADR 0017](../design/adr/0017-recompute-shares-from-the-final-grouping-plan.md)
  define Parent and Total shares, fixed-key partitions, missing/zero behavior,
  and Grand total denominators.
- [The nesting reference](../R/nest_with_margins.R) promises that nested cells
  hold the source rows they represent; `.keep = TRUE` retains original
  pre-Margin keys. Element classes and cross-backend cell classes are not
  promised.

Backend exclusions were applied before comparison. Arrow shares were not
tested because the public contract rejects them. SQLite share checks used
`.check_share_source = FALSE` only after the harness itself established that
the source was `sum(integer)`, as the Converting-dialect contract requires.
Nesting was tested only on local and dtplyr inputs. Physical order was compared
only under `.sort`; otherwise results were compared as multisets. `.id` and
`grouping_id()` were excluded wherever the transformation was documented to
renumber them.

## Candidate and executed relations

`Gap` says why the relation added information beyond the existing suite.

| MR | Source input | Transformation | Expected relation | APIs / backends | Preconditions and exclusions | Existing coverage and gap | Result |
|---|---|---|---|---|---|---|---|
| 1 | Small tables with duplicate and missing keys | Permute rows | Summary and expanded-row multisets are unchanged; ordered summary output is unchanged | summary, expand; local, dtplyr, RSQLite, DuckDB, Arrow | Order ignored unless `.sort` is set; order-sensitive aggregates excluded | Existing tests use fixed row orders; no generated permutation sweep | No violation |
| 2 | One Grouping plan | Permute `grouping_sets()` occurrences | Values and types are unchanged after aligning dimension columns; `.id` is excluded | summary, expand; all executed backends | No duplicate-set ambiguity; physical order and occurrence identifiers excluded | Existing tests check duplicate cardinality and SQL shape, not permutation plus all-missing types | **Confirmed violation on RSQLite** |
| 3 | Character keys | Apply a bijection to key values, then invert it on output | Aggregates and structural bits commute with relabeling | summary; all executed backends | Margin labels outside the image; comparison is multiset-based | No generated relabel sweep | No violation |
| 4 | Factor keys with unused levels | Relabel observations and levels bijectively in the same level positions | Values commute; restoring backends preserve corresponding levels | summary; local, dtplyr, DuckDB; value-only on Arrow/RSQLite | Ordered rank is unchanged; Arrow/RSQLite factor limitations honored | Existing factor tests are example-based | No violation |
| 5 | Any two-dimensional input | Replace `rollup(a, b)` with its explicit three grouping sets | Same plan family and same result multiset | inspect, summary, nest; executed applicable backends | `.id` retained because explicit order matches rollup order | Representative expected values exist; generated missing/empty inputs were absent | No violation |
| 6 | Any two- or three-dimensional input | Replace `cube()` with all explicit subsets | Same plan family and result multiset | inspect, summary; local plus summary backend matrix | Explicit set order aligned when `.id` was compared | Plan compilation has fixed examples, not systematic data execution | No violation |
| 7 | Three-dimensional input | Permute dimensions passed to `cube()` | Same family of grouping sets and same value multiset | inspect, summary; local | Dimension column order and `grouping_id()` bit order excluded | No all-six-permutations execution test | No violation |
| 8 | Two dimensions | Replace `cube(a, b)` with `grouping_spec(rollup(a), rollup(b))` | Same four grouping sets and results | inspect, summary; local plus backend matrix | Plan identifiers aligned only where order matched | Existing product example does not establish this algebra over generated data | No violation |
| 9 | Independent grouping families | Swap Cartesian-product operands | Same semantic set family and result multiset | inspect, summary; local | Column order and identifiers excluded | No direct commutativity property | No violation |
| 10 | Three independent families | Reassociate nested `grouping_spec()` | Same semantic set family and result multiset | inspect, summary; local | Duplicate sets dropped | No associativity property | No violation |
| 11 | Three independent sets | Flatten or nest `grouping_sets()` | Same union after duplicate policy | inspect; local | `.duplicates = "drop"` | Nesting grammar is tested, union associativity is not | No violation |
| 12 | Composite dimension plus scalar dimension | Expand composite `cube()` explicitly | Same grouping sets and result multiset | inspect, summary; local | Composite columns move together | Existing test checks masks for one example only | No violation |
| 13 | Composite dimension plus scalar dimension | Expand composite `rollup()` explicitly | Same grouping sets and result multiset | inspect; local | Composite columns move together | No explicit algebraic comparison | No violation |
| 14 | One grouping set | Reverse columns inside `grouping_set()` and duplicate it | The two occurrences are duplicate sets | inspect; local | Comparison is set-semantic, not column-order-semantic | Duplicate policy tests do not target within-set order | No violation |
| 15 | Fixed key plus rollup | Move fixed key into `grouping_spec(grouping_set(fixed), rollup(...))` | Same grouping-set structure and aggregate values | inspect, summary; local | Type conversion and collision checks explicitly excluded | Existing grouped-vs-`.by` test does not cover documented product equivalence | No violation |
| 16 | Additive summaries | Run direct Margin summary or expand first and ordinarily summarize by keys plus `.id` | Same totals and counts | summary, expand; local, dtplyr, RSQLite, DuckDB, Arrow | `.id` retained to distinguish typed-missing margins from source NA | Documented example exists; generated boundary/backend sweep did not | No violation |
| 17 | Plan containing Grand total set | Compare its Grand total with ordinary aggregation over the input | Same totals/counts within fixed partitions | summary; all executed backends | Fixed keys aligned; empty partition behavior retained | One arbitrary-set example; no generated empty/missing sweep | No violation |
| 18 | Additive and mean summaries | Duplicate every source row | Sums/counts double; means and shares remain unchanged | summary, shares; local, dtplyr, RSQLite, DuckDB, Arrow where applicable | Missing aggregates remain missing; no non-additive summaries | No systematic duplication property | No violation |
| 19 | Positive numeric share source | Scale all measures by a common nonzero constant | Source summary scales; Parent and Total shares do not | summary shares; local, dtplyr, RSQLite, DuckDB | Arrow excluded; SQLite source eligibility caller-established | Existing tests hand-code a few ratios | No violation |
| 20 | Positive numeric rollup/cube | Sum shares over each denominator partition | Total shares sum to 1 per fixed key and occurrence; Parent shares sum to 1 under each parent | summary shares; local, dtplyr, RSQLite, DuckDB | Positive totals avoid zero/missing denominator cases | No conservation property in suite | No violation |
| 21 | Rollup or cube with Total share | Replace constructor with an equivalent explicit plan | Total share is unchanged | summary shares; local, dtplyr, RSQLite, DuckDB | Same Grand total and fixed partitions | Plan admission is tested, plan-form invariance is not | No violation |
| 22 | Nestable input | Permute source rows | Outer groups and nested row multisets are unchanged | nest, nest-by; local, dtplyr | Inner order not assumed | Fixed examples only | No violation |
| 23 | Same nesting request | Swap `nest_with_margins()` and `nest_by_with_margins()` | Outer keys and cells match; only documented rowwise grouping differs | nest, nest-by; local, dtplyr | Result class/grouping excluded | One interface example; no generated cell comparison | No violation |
| 24 | Nested result with `.keep = TRUE` | Project keys out of each cell | Equals `.keep = FALSE`, with identical outer keys and cell cardinalities | nest, nest-by; local, dtplyr | Element subclasses ignored | Cardinality examples exist; generated projection relation did not | No violation |
| 25 | Same data, two non-colliding Margin labels | Change only display label under `.sort` and normalize margin cells by Grouping bits | Row order and non-label values are unchanged | all Margin verbs conceptually; summary on all executed backends | Explicit `.sort`; factor-level position kept separate | Order tests use one default label | No violation |

The executed input range was deterministic: 0--8 rows; two ordinary values
plus `NA` for fixed and variable keys; duplicated rows; negative, zero,
positive, and missing measures; factor inputs with unused levels; and positive
integer measures for conservation checks. The backend matrix used local data
frames, dtplyr 1.3.3, RSQLite 3.53.3, DuckDB 1.5.5, and Arrow 25.0.1 with
dplyr 1.2.1 and dbplyr 2.6.0.

## Confirmed violation

### Grouping-set order changes a typed-missing dimension's collected type

**Classification:** `CONFIRMED VIOLATION`

**Violated relation.** Permuting distinct `grouping_sets()` occurrences may
change Grouping-plan order, result column order, and `.id`, but it must not
change the source type represented by a typed-missing Margin dimension. After
aligning columns and excluding occurrence identity, the results should have
the same values and types.

**Minimal reproduction.** This is the full reproducer; every remaining
element is load-bearing. Removing the dimension leaves no type to observe,
removing either grouping set removes the permutation, and using a non-missing
label or adding a non-missing source value removes the failure.

```r
pkgload::load_all(".", quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(con))

remote <- dplyr::copy_to(
  con,
  data.frame(a = character()),
  "type_repro",
  temporary = TRUE
)

detail_first <- summarize_with_margins(
  remote,
  .grouping = grouping_sets(grouping_set(a), grouping_set()),
  .margin_label = NULL
) |>
  dplyr::collect()

total_first <- summarize_with_margins(
  remote,
  .grouping = grouping_sets(grouping_set(), grouping_set(a)),
  .margin_label = NULL
) |>
  dplyr::collect()

c(detail_first = typeof(detail_first$a), total_first = typeof(total_first$a))
# detail_first total_first
#   "character"   "logical"
```

The red-capable command was run three times and failed identically each time:

```sh
Rscript /private/tmp/repro-sqlite-grouping-set-order-type.R
```

It reports:

```text
detail_first: rows=1, typeof(a)=character
total_first:  rows=1, typeof(a)=logical
Error: Equivalent grouping-set orders must preserve the typed-missing column
```

**Expected relation.** Both outputs contain one typed missing value of the
source dimension's database type, so `typeof(a)` should be `"character"` in
both results.

**Actual result.** The displayed values and cardinality agree, but the
total-first result silently becomes `logical`. The same difference occurs for:

- one-row inputs whose dimension is `NA_character_`;
- integer and double all-missing dimensions (typed result versus logical);
- `.margin_label = NA_character_` as well as `NULL`; and
- `expand_with_margins()` (including its zero-row result schema).

One non-missing dimension value or a non-missing Margin label such as
`"Total"` hides the defect by giving SQLite a runtime value from which it can
infer a type.

**Contract basis.** [The Margin summary reference](../R/summarize_with_margins.R)
says that `NULL` and `NA_character_` use typed missing values and that portable
`UNION ALL` has the same semantics as the native path.
[Grouping specifications](../R/grouping-spec.R) define `grouping_sets()` as a
union. [ADR 0012](../design/adr/0012-make-margin-labels-type-stable.md)
independently states that an omitted dimension receives a typed missing value.

**Affected API and backend.** Confirmed on `summarize_with_margins()`,
`summarise_with_margins()`, and `expand_with_margins()` with live RSQLite,
which uses the generic SQL portable adapter. Nesting does not support SQL
inputs. The same probes passed on local, dtplyr, DuckDB, and Arrow. Other
generic SQL dialects and live PostgreSQL were not available, so they are not
claimed affected or unaffected.

**Why existing tests do not detect it.** The closest live RSQLite no-summary
test uses `c("x", "x", "y", NA)` and `rollup(a)`, so a non-missing value and
detail-first plan give the compound column a character type. The fallback
dialect matrix inspects SQL shape on simulated non-missing data rather than
collecting a live all-missing result. The live SQLite Margin-label test uses a
non-missing `"Total"` label. None permutes grouping-set occurrences while
holding an all-missing typed dimension and a typed-missing label constant. The
full existing suite therefore remains green.

**Severity:** medium. Values and row counts remain correct, but the defect is a
silent schema corruption at a public boundary. It can break downstream string
operations, type-stable joins, schema assertions, and empty-result pipelines.
The trigger is narrow: a generic SQL portable query, a typed-missing label, and
no non-missing runtime value for the affected dimension.

## Cause analysis

This section is evidence about the failure, not a production-code change.

1. The generic `sql` backend does not advertise `can_read_schema`, so
   `margin_column_info()` returns no prototypes.
2. For an omitted typed-missing dimension with no prototype,
   `label_margin_branch()` falls back to bare R `NA`, rendered as SQL `NULL`.
3. `summarize_margin_union()` builds and combines branches in Grouping-plan
   order.
4. With detail first, RSQLite sees a bare declared source column in the first
   compound SELECT and reports `character`. With total first, the first SELECT
   exposes only `NULL`; when every runtime value is also NULL, RSQLite reports
   `logical`.

The earlier evidence in
[the schema-versus-data-read investigation](share-source-schema-vs-data-read.md)
explains the same SQLite boundary: declared pass-through columns retain static
affinity, while computed/all-NULL expressions become logical when no value is
computed.

A direct probe showed that `CAST(NULL AS TEXT)` alone still collects as logical
through RSQLite when every value is NULL. Reordering the most detailed branch
first is also incomplete because an arbitrary grouping-set family need not
contain one set with every dimension. A future fix should therefore evaluate a
typed zero-row seed relation (for example, one that selects the actual source
dimension columns before the real branches) or another mechanism that gives
every compound output column declared source affinity. It must be checked
against [ADR 0020](../design/adr/0020-ban-unrequested-reads-of-lazy-inputs.md)'s
no-unrequested-read contract and must preserve Grouping set identifiers. No
such fix was implemented here.

## No violation found

The 24 passing relation families covered:

- input permutation and whole-input duplication;
- rollup, cube, explicit grouping sets, Cartesian products, associativity,
  commutativity, dimension permutations, and composite dimensions;
- direct aggregation versus expand-then-aggregate and Grand total versus
  ordinary aggregation;
- bijective character/factor relabeling, unused factor levels, factor level
  reconstruction, and label-independent Margin ordering;
- Parent/Total share scaling, duplication invariance, missing-safe fixed and
  variable keys, plan-form equivalence, and mass conservation; and
- nesting row membership, input permutation, `.keep` projection, rollup
  expansion, and agreement between the two nesting interfaces.

`NO VIOLATION FOUND` is bounded by the small deterministic search space and the
executed package/backend versions above; it is not a proof for arbitrary user
summary expressions or unavailable live database dialects.

## Rejected relations

These six candidates looked plausible but are invalid under the actual
marginplyr contract and were not used as bug oracles.

1. **Physical result order is invariant under input or Grouping-plan
   permutation.** Invalid when `.sort = "none"`; row order is explicitly
   unspecified.
2. **`.id` is invariant under grouping-set permutation or deduplication.**
   Invalid; `.id` is the one-based occurrence position after duplicate policy.
3. **A bare `grouping_id()` is invariant under dimension permutation.**
   Invalid; bit significance follows Grouping-plan dimension order.
4. **`.by = fixed` is fully interchangeable with an always-included Margin
   dimension.** Invalid beyond grouping-set structure: the latter participates
   in label conversion and collision checks.
5. **A missing displayed grouping value identifies a Margin row.** Invalid;
   source missing values and typed-missing margins require `.id`,
   `grouping_bit()`, or `grouping_id()`.
6. **`share_of_parent()` should commute with replacing a rollup by any
   equivalent cube or arbitrary grouping-set family.** Invalid; Parent share is
   admitted only for a pure rollup because the other plans do not select one
   unambiguous parent chain. Total share was tested instead.

## Recommended durable metamorphic tests

1. **Highest priority: RSQLite typed-missing grouping-set permutation.** Add a
   public-seam test with a zero-row `character` column and both
   `grouping_sets(grouping_set(a), grouping_set())` orders. Assert collected
   type and value equality for `NULL` and `NA_character_`, for both summary and
   expansion. Add a one-row all-missing neighbor so both empty-result schema
   and runtime all-NULL behavior are locked down.
2. **Constructor algebra over generated small inputs.** Keep a small,
   deterministic table of seeds and compare `rollup`/`cube`/product forms with
   explicit grouping sets through `inspect_grouping()` and
   `summarize_with_margins()`. Compare multisets and structural identity, not
   unspecified order or plan-dependent identifiers.
3. **Expand/direct-summary equivalence.** For additive summaries, group the
   expanded result by visible keys plus `.id` and compare it with direct Margin
   summarization. Include missing keys and empty inputs on local, one portable
   SQL backend, and one native SQL backend.
4. **Share conservation.** On positive generated measures, assert Total-share
   sums per fixed partition and occurrence and Parent-share sums per immediate
   parent. This checks many join keys and denominators without hand-authoring
   expected ratios.
5. **Nesting membership.** Compare each nested cell's source-row identifiers
   with the corresponding expanded branch, and assert `.keep = FALSE` is the
   projected form of `.keep = TRUE`, on local and dtplyr.

### Test-suite adoption decision

These relations should not all be copied verbatim into `testthat`. The
exploratory harness deliberately used broad repetition to search for defects;
the durable suite should retain a small number of deterministic witnesses that
protect independent contracts at public seams.

- The confirmed RSQLite violation should become a regression test in the same
  change that fixes it. Landing it earlier as an ordinary test would leave the
  main suite intentionally red; skipping or weakening it would not protect the
  contract.
- Constructor algebra, expand/direct-summary equivalence, share conservation,
  and nesting membership can be added before any production change because
  they pass and cover independent semantic seams that fixed expected-value
  examples do not cover.
- Each durable property should use an explicit, small table of deterministic
  cases rather than random sampling. Failure output must identify the seed
  case, transformation, API, and backend.
- Backend coverage should be selected by execution path rather than multiplied
  indiscriminately: local semantics, dtplyr translation, one portable SQL
  backend (RSQLite), one native SQL backend (DuckDB), and Arrow only where the
  API contract supports it.
- Row permutation, bijective relabeling, and exhaustive constructor
  permutations are useful periodic or development-time probes, but have lower
  value as mandatory per-commit checks once the higher-priority relations are
  present.

This gives the permanent suite substantially more semantic reach without
turning the full exploratory search space into a slow or opaque CI burden.

All proposed tests observe the public seams named by
[the architecture](../design/architecture.md); none needs to construct or
inspect the opaque Margin operation.
