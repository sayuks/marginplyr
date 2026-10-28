# Expansion and Grouping specification bug hunt

Investigated: 2026-10-29
Baseline: `58356a67a79d1d2961eac6b8865d30afdb751d5c` (`main`)
Scope: investigation and tickets; no package implementation changes

The investigation compared the documented public interface, the Grouping-plan
compiler, existing regressions, independent oracles, and live backend execution.
Four parallel tracks covered Grouping specifications, local/data.table/dtplyr
expansion, Arrow/DuckDB expansion, and SQL composition. Every finding below was
reproduced through the public interface and checked against an independent or
ordinary-backend control. All runtime observations refer to the baseline above.

The environment was macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1,
Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3, and PostgreSQL 17.11.
PostgreSQL ran in a disposable cluster with a private Unix socket and no TCP
listener. The probes used temporary tables. No production service was contacted.

## Findings and ticket boundaries

| Finding | Priority | Ticket | Independent deliverable |
| --- | --- | --- | --- |
| G1 | P2 | [#975](https://github.com/sayuks/marginplyr/issues/975) | Resolve ordinary pronoun-shaped subscripting with typed metadata |
| S1 | P2 | [#976](https://github.com/sayuks/marginplyr/issues/976) | Reuse a sorted SQLite result through its public schema |
| S2 | P2 | [#977](https://github.com/sayuks/marginplyr/issues/977) | Retain a selected PostgreSQL input inside every expansion branch |
| D1 | P2 | [#978](https://github.com/sayuks/marginplyr/issues/978) | Omit a DuckDB STRUCT dimension with a native typed NULL |
| A1 | P2 | [#978](https://github.com/sayuks/marginplyr/issues/978) | Omit an Arrow STRUCT dimension with a null parent |
| N1 | P2 | [#979](https://github.com/sayuks/marginplyr/issues/979) | Preserve supported dot-dot identifiers on Arrow and dtplyr |
| S3 | P3 | [#980](https://github.com/sayuks/marginplyr/issues/980) | Compose a wide SQLite plan within compound-query limits |

The seven findings were divided into six tickets, each able to start
independently. D1 and A1 shared one narrow typed-null prototype contract, so
one ticket required both backend representations and their distinct
regressions. N1 kept the same public identifier behavior together across two
small generated-selection sites. No prefactoring or blocking ticket was
necessary to make any reproduction verifiable.

## G1: ordinary .data subscripting selected the wrong key

A qualified or parenthesized `[[` head was an ordinary lexical call under
tidyselect. The names-only classifier instead treated an operand spelled
`.data` as the special pronoun form. A type-dependent subscript ran against the
integer names proxy, choosing the wrong fixed key. A wrapper promise could
retain that answer into the Grouping specification's canonical pass.

Run this block from the inspected repository root:

```r
pkgload::load_all(quiet = TRUE)
data <- tibble::tibble(
  region = c("east", "west"), amount = c(1, 1), segment = c("x", "y")
)
.data <- names(data)
first_numeric <- function() {
  which(vapply(tidyselect::peek_data(), is.numeric, logical(1)))[[1L]]
}
print(tidyselect::eval_select(
  quote(base::`[[`(.data, first_numeric())), data
))
print(inspect_grouping(
  data, .by = base::`[[`(.data, first_numeric()), .format = "list"
)$fixed)
wrapper <- function(data, chosen = first_numeric()) {
  inspect_grouping(
    data, .grouping = rollup(base::`[[`(.data, chosen)), .format = "list"
  )$included
}
print(wrapper(data))
print(inspect_grouping(
  data, .grouping = rollup(amount), .format = "list"
)$included)
```

Expected: `amount` was the selected fixed key and rollup dimension, matching
tidyselect. Observed: tidyselect returned `amount = 2`; both inspection calls
selected `region`. The direct `rollup(amount)` control returned `amount` and
the empty set. Expansion with the same `.by` also silently fixed `region`.

Renaming only the lexical operand to `plain_map` restored the correct answer.
Qualified, parenthesized, and parenthesized-qualified heads reproduced it.
All five constructors reproduced the wrapper variant. Local and dtplyr
inspection reproduced both wrong keys and wrong dimensions. A
character-selecting variant failed prematurely with
`attempt to select less than one element in get1index`, while direct typed
tidyselect succeeded. Bare `.data[[first_numeric()]]` retained tidyselect's
constant-subscript refusal and was excluded from the requested dynamic behavior.

The cause was `is_name_only_expr()`'s pronoun branch preceding its ordinary
bare-head restriction. `resolve_fixed_keys()` retained the names-proxy answer;
the discarded Grouping-specification pass could additionally force a wrapper
promise. The `<tidy-select>` constructor contract and inspection's typed
metadata contract supplied the expected behavior. ADRs 0005 and 0008 governed
early decisions and evaluation budgets. Closed #889 and #924 were read: neither
covered this pronoun-shaped branch.

## S1: a sorted SQLite result leaked private columns on re-expansion

```r
pkgload::load_all(quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
source <- dplyr::copy_to(
  con, data.frame(g = c("b", "a"), value = 1:2), "facts"
)
first <- expand_with_margins(
  source, .grouping = grouping_set(g), .margin_label = NULL, .sort = "last"
)
print(names(dplyr::collect(first)))
print(names(dplyr::collect(expand_with_margins(first))))
tryCatch(
  print(dplyr::collect(expand_with_margins(
    first, .grouping = rollup(g), .margin_label = NULL
  ))),
  error = function(cnd) cat(conditionMessage(cnd), "\n")
)
materialized <- dplyr::compute(first, analyze = FALSE)
print(dplyr::collect(expand_with_margins(
  materialized, .grouping = rollup(g), .margin_label = NULL
)))
DBI::dbDisconnect(con)
```

Expected: the no-grouping expansion retained only `g` and `value`; the rollup
returned four rows with those public columns. Observed: the first collection
had those two columns, but the second exposed `..marginplyr_order_1` and
`..marginplyr_order_2`. The rollup failed with
`SELECTs to the left and right of UNION ALL do not have the same number of result columns`.
The materialized-input control returned all four expected rows.

An unsorted first result succeeded. Both one-set and rollup first results
failed when sorted. The second result failed with and without a new identifier,
and with `none`, `first`, and `last` Margin order. A changed-query mutation,
a projection dropping a payload, and explicit materialization were successful
controls; an identity projection alone did not necessarily change the query.

`sql_render.marginplyr_sqlite_typed_result()` deliberately rendered a direct
sorted query with private sort columns, which its direct collector removed.
`sql_margin_input_relation()` reused that physical SQL with `SELECT *` while
registering only the public variable names. Unchanged branches carried the
private columns; generated omitted-dimension branches projected public columns.
The mismatch arose inside package composition. ADR 0031's direct-result
machinery and ADR 0016's public-column ordering supplied the boundary. No
survival of an earlier Margin order through a later verb was assumed.
Closed #895 covered a different Arrow reuse boundary.

## S2: PostgreSQL input ORDER BY and LIMIT escaped expansion branches

This block used a PostgreSQL connection configured through the ordinary
`PGHOST`, `PGPORT`, `PGUSER`, and `PGDATABASE` environment variables. The measured
run used the disposable PostgreSQL 17.11 cluster described above.

```r
pkgload::load_all(quiet = TRUE)
con <- DBI::dbConnect(RPostgres::Postgres())
source <- dplyr::copy_to(
  con, data.frame(g = c("c", "b", "a"), value = 1:3), "facts"
)
selected <- utils::head(dplyr::arrange(source, value), 2L)
empty_first <- grouping_sets(grouping_set(), grouping_set(g))
result <- expand_with_margins(
  selected, .grouping = empty_first, .margin_label = NULL, .id = "sid"
)
print(dplyr::collect(result))
tryCatch(
  print(dplyr::collect(expand_with_margins(
    selected, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
  ))),
  error = function(cnd) cat(conditionMessage(cnd), "\n")
)
materialized <- dplyr::compute(selected, analyze = FALSE)
print(nrow(dplyr::collect(expand_with_margins(
  materialized, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
))))
DBI::dbDisconnect(con)
```

Expected: the two selected input rows produced four copies, two per occurrence.
Observed: empty-first returned only two rows. Rollup failed at `UNION` with a
PostgreSQL syntax error. Materializing the selected input first returned four.
A head-only input reproduced both symptoms. An arrange-only input reproduced
the rollup syntax error. String labels succeeded in the tested cases because
they added a projection to the otherwise unchanged branch. Collapsing the
selected input was not a general remedy: the collapsed rollup still failed.

The live matrix used four input shapes (head, arrange, both, collapsed-both),
two occurrence orders, and NULL/string labels: 16 cases. A prior DuckDB parser
check of simulated PostgreSQL SQL agreed with the syntax defect; the live
PostgreSQL result established the actual wrong cardinality.

`expand_margin_union()` enclosed the rendered input only for SQLite and
DuckDB. PostgreSQL's unchanged branch therefore kept its input limit or order
at compound-query scope. PostgreSQL's [UNION documentation][postgres-union]
required parentheses for an individual operand carrying these clauses. Closed
#835 explicitly covered SQLite and DuckDB; this was its unaddressed PostgreSQL
counterpart, not a claim that those completed fixes had regressed.

[postgres-union]: https://www.postgresql.org/docs/17/queries-union.html

## D1: DuckDB STRUCT dimensions could not receive a missing Margin label

```r
pkgload::load_all(quiet = TRUE)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
source <- dplyr::tbl(con, dbplyr::sql(
  "SELECT {'x': 1, 'y': 'a'} AS g, 1 AS row_id"
))
tryCatch(
  expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
  ),
  error = function(cnd) cat(conditionMessage(cnd), "\n")
)
control <- dplyr::union_all(
  dplyr::mutate(source, sid = 1L),
  dplyr::mutate(source, g = dbplyr::sql("NULL"), sid = 2L)
)
print(dplyr::collect(dplyr::mutate(control, missing = is.na(g))))
DBI::dbDisconnect(con, shutdown = TRUE)
```

Expected: a lazy two-row expansion with a typed SQL NULL STRUCT in occurrence
two. Observed: construction failed with `g = <df[,2]>` and
`Unknown input type: list` from dbplyr's `partial_eval()`. The backend control
kept STRUCT and returned `missing = FALSE, TRUE`.

NULL and `NA_character_`, all three sort modes, and empty input reproduced the
failure. An included-only set and the native DuckDB Margin summary succeeded.
`margin_column_info()` handled the packed data-frame proxy before its DuckDB
SQL-NULL branch, and `label_margin_branch()` injected that R data frame into
SQL. The documented typed-missing label behavior was the expected contract;
no general support for non-missing structured labels was inferred.

## A1: omitted Arrow STRUCT dimensions remained valid structs

```r
pkgload::load_all(quiet = TRUE)
source <- arrow::Table$create(
  g = arrow::Array$create(
    data.frame(x = 1L, y = "a"),
    type = arrow::struct(x = arrow::int32(), y = arrow::utf8())
  ),
  row_id = 1L
)
result <- expand_with_margins(
  source, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
)
print(dplyr::collect(dplyr::mutate(result, missing = is.na(g))))
type <- arrow::schema(source)$GetFieldByName("g")$type
null_g <- arrow::Scalar$create(NA, type = arrow::null())$cast(type)
print(dplyr::collect(dplyr::mutate(source, g = null_g, missing = is.na(g))))
```

Expected: the omitted-dimension row had `missing = TRUE`. Observed: both
occurrences had `FALSE`; the exact-type null-scalar control had `TRUE`.
Computing the result exposed an all-valid parent STRUCT with null children.
Collecting to R visually hid that difference, because both forms became the
same packed all-NA row. Lazy null filtering therefore gave a different answer
from the promised typed-missing label.

Table, RecordBatch, InMemoryDataset, and query inputs, three sort modes, and
NULL/NA labels produced 24 nonempty reproductions. The corresponding 24 empty
cases retained zero rows. `margin_column_info()` built
`vec_init(proxy_data_frame, 1L)` and then `Scalar$create(type = STRUCT)`, which
created a valid parent. Its DictionaryType branch already used a cast null
scalar. This evidence concerned expansion only; native Arrow struct grouping
for summaries was unsupported and was excluded.

## N1: generated selections refused supported dot-dot identifiers

```r
pkgload::load_all(quiet = TRUE)
source <- arrow::Table$create(g = c("b", "a"), value = 1:2)
print(dplyr::collect(dplyr::mutate(source, "..1" := 1L)))
print(dplyr::collect(expand_with_margins(source, .id = "..1")))
tryCatch(
  expand_with_margins(
    source, .grouping = grouping_set(g), .margin_label = NULL, .id = "..1"
  ),
  error = function(cnd) cat(conditionMessage(cnd), "\n")
)
step <- dtplyr::lazy_dt(data.frame(value = 1:2))
print(data.table::as.data.table(dplyr::mutate(step, "..1" := 1L)))
tryCatch(
  expand_with_margins(step, .id = "..1"),
  error = function(cnd) cat(conditionMessage(cnd), "\n")
)
```

Expected: the accepted nonempty scalar identifier name survived construction
and supported backend conversion, with integer occurrence values. Observed:
construction raised `vctrs_error_names_cannot_be_dot_dot`. Arrow failed for any
nonempty dimensional plan, including a one-set plan; its no-dimension control
and direct mutate collected correctly. dtplyr failed even with no Grouping
specification. Direct dtplyr mutate and `as.data.table()` preserved the name
and integer values. Local expansion supported the names throughout.

Arrow's private dimension rename in `expand_margin_union()` validated the
unrelated new identifier. dtplyr's final `reorder_margin_columns()` called
`relocate()`, which imposed the same restriction. The dtplyr identifier sweep
covered 264 combinations; 36 failed, all involving `...`, `..1`, or `..2`.
The remaining 228 agreed with the local oracle.

Closed #827 and #922 were read. Their repaired local/summary-output behavior
did not cover these lazy expansion sites. SQL expansion also refused these
names, but ordinary SQL mutation already did so; this investigation did not
promote that delegated boundary into another finding. dtplyr's default tibble
collection name repair likewise remained outside this finding.

## S3: flat SQLite composition exceeded the compound-query limit

```r
pkgload::load_all(quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
data <- as.data.frame(as.list(stats::setNames(rep(1L, 9L), paste0("d", 1:9))))
source <- dplyr::copy_to(con, data, "facts")
print(nrow(inspect_grouping(source, .grouping = cube(dplyr::everything()))))
tryCatch(
  print(nrow(dplyr::collect(expand_with_margins(
    source, .grouping = cube(dplyr::everything()), .margin_label = NULL
  )))),
  error = function(cnd) cat(conditionMessage(cnd), "\n")
)
half <- paste(rep("SELECT 1 AS x", 256L), collapse = " UNION ALL ")
wrapped <- paste0(
  "SELECT * FROM (", half, ") UNION ALL SELECT * FROM (", half, ")"
)
print(nrow(DBI::dbGetQuery(con, wrapped)))
DBI::dbDisconnect(con)
```

Expected: one input row and 512 grouping-set occurrences produced 512 copies.
Observed: inspection resolved 512 occurrences, but collection failed with
`too many terms in compound SELECT`. The bounded-subquery control returned
512 rows on the same connection. A direct flat 512-term control failed.
The connection reported `MAX_COMPOUND_SELECT=500`; plans retaining exactly
500 repeated included-only sets also failed after adding the zero-row anchor,
while 498 and 499 passed, under both unsorted and sorted expansion.

SQLite's [implementation-limits reference][sqlite-limits] documented a default
of 500 terms and a configurable connection limit. The limit itself was a
backend property. The avoidable package translation limitation was flattening
the entire finite plan at `union_margin_branches()`, despite the same row bag
being representable by bounded compounds. This was a P3 wide-plan robustness
finding, not an unlimited-size guarantee or a request to change the external
condition class under ADR 0015. Fixing composition needed to account for
anchors without introducing construction-time data reads under ADR 0020.

## Coverage, successful controls, and excluded leads

- Grouping algebra used seed `20261029`: 2,500 specification trees, maximum
  depth two, zero to three arguments, three dimensions, composite dimensions,
  overlap, repetitions, unions, and products. Four specimens above 1,000 sets
  were skipped; 2,496 specimens / 4,992 plans matched an independent oracle
  under keep/drop. It compared exact included columns and plan order, set IDs,
  and a fixed key. Cube subsets used a Boolean grid; products used expand.grid.
- An independent base/vctrs expansion oracle used the same seed for 250
  random explicit plans. Duplicate occurrences, reordered dimensions, empty
  inputs, factor NA levels, and per-dimension NULL/string labels all matched.
- Local/dtplyr type probes covered 324 comparisons across 18 families,
  three labels, three sort modes, and empty/populated inputs; extended types
  added 108 comparisons. Families included integer64 above 2^53, temporal
  classes, ordered factors, list/list-of, packed/nested packed, matrices,
  arrays, zero-width structured keys, haven labels, Duration, and units.
- Special-name probes covered 6,680 comparisons: 334 distinct
  dimension/fixed-name pairs, two labels, two sort modes, and five eager/lazy
  derivations, including root/filter/mutate/cached dtplyr steps and data.table.
- SQLite's name sweep ran 768 cases across 32 names, four roles, three sort
  modes, and two labels, checking collection and materialization against local
  expansion. Its 48 failures involved dot-dot names already refused by the
  SQL backend; the other 720 passed.
- Arrow type probing exercised 128 combinations across numeric widths,
  decimal, temporal units/time zones, binary widths, strings, struct and
  list forms, with physical schema checks. DuckDB probing included integer
  widths, decimal, temporal types, list, struct, map, blob, UUID, and ENUM.
  Invalid fixtures and ordinary unsupported structured casts/sorts were
  excluded before interpreting package behavior.
- Existing dictionary, fixed-size-list, partition, and sorted Arrow
  re-expansion regressions were read to distinguish #750/#861/#893/#895
  from the new findings. Constructor grammar, injected/parenthesized capture,
  cycles, duplicate normalization, and the 31-dimension inspection-ID boundary
  received source and targeted executable checks.
- A malformed lubridate Period after dtplyr expansion also reproduced with
  ordinary union_all and was excluded under ADR 0016. Complex unions, list
  ordering, and structured-column flattening at lazy_dt admission likewise
  reproduced outside marginplyr. Hand-forged malformed specifications and
  callable specification predicates were excluded from supported-interface
  findings; the latter already had dated evidence in
  `investigation/reading-a-specification-tidyselect-called.md`.
[sqlite-limits]: https://www.sqlite.org/limits.html

These probes did not enumerate arbitrary R expressions, unbounded plans, every
SQL dialect, every package version, or unavailable backend services. Passing
cases were evidence for their measured inputs, not proof that the interface
was free of other defects.

## Verification of this investigation

The five existing focused test files for the grouping interface, Grouping plan,
inspection, expansion operation, and metamorphic semantics passed on the
baseline. The new findings were outside their exercised cases. No regression
or implementation fix was committed, as requested.

The dated note was excluded from the source package by the repository's
`investigation/` build exclusion. Under *Local review-ready checks*, this was a
Repository-only change, so the package Review-ready check was not required.
Final lint, reference, context-budget, diff, and review outcomes were recorded
in the pull request rather than duplicated into this dated evidence.
