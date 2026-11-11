# Second-pass hunt over Margin summaries, Grouping specifications and shares

Investigated: 2026-11-11
Revised: 2026-11-12 — live SQL Server execution by `.github/workflows/mssql-portable.yaml`
Repository snapshot: `22043a461c5c48662ff54ea0a1238f00ac316059`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented.

Five fresh-context investigators worked read-only in the same checkout from
separate starting points and reported before any candidate was shared: A from
the neighbourhood of the repairs merged since
`summary-spec-share-differential-hunt-2026-11-11.md` (#1545 to #1579), B from
the input shapes that note listed as not covered, C from Total shares under
argument and plan interactions, D from Grouping specifications as a summary
consumes them, E from what happens around a call (the returned object,
follow-up verbs, database and data.table state, repeated calls). The
coordinator probed global options, rendered SQL on every dbplyr simulator and
the PostgreSQL rendering, then replayed every reported finding in a fresh R
process from its own script.

Versions were R 4.6.1 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, duckdb 1.5.5, RSQLite 3.53.3, arrow 25.0.1, tidyselect
1.2.1, rlang 1.3.0, vctrs 0.7.3 and tibble 3.3.1. SQL work used process-owned
in-memory SQLite and DuckDB databases, or files under the session's temporary
directory, holding synthetic tables. No existing database or outside data was
read.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. Summaries were compared with ordinary dplyr summaries at
each explicit grain, bound in Grouping-plan order; C and D each wrote their own
plan expander, and C's fault-injected oracle produced 156 mismatches where the
real one produced none. Share denominators came from a separate summary at the
Grand total grain inside each fixed partition. Every cross-backend difference
was followed by the native control on the differing backend before it was
counted.

Each candidate was checked against the issue tracker (open and closed, through
`gh issue list --search` and the search API), `investigation/`,
`tests/testthat/`, `NEWS.md` and the ADR governing it.

## Confirmed findings

Each script under `investigation/2026-11-11-summary-spec-share-second-pass/`
is run from the repository root, prints the observed behaviour and exits zero.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| Caller parentheses in a lazy SQL summary | The pair was dropped from the rendered SQL, so `a - (b - c)` and `p & (q \| r)` evaluated as `a - b - c` and `p & q \| r`; values, and shares over them, were wrong with no diagnostic. | `sql-caller-parentheses.R` | [#1582](https://github.com/sayuks/marginplyr/issues/1582) |
| DuckDB input registered with `duckdb::duckdb_register()` | Every Margin verb refused it as a view whose definition "could hide source work"; the same calls worked before #1524. | `duckdb-registered-data-frame.R` | [#1583](https://github.com/sayuks/marginplyr/issues/1583) |
| Local unnamed `across()` with a constant `.names` after an output that reuses an input name | A valid call was refused with a template-count diagnostic describing two outputs where there was one. | `across-constant-names-after-overwrite.R` | [#1584](https://github.com/sayuks/marginplyr/issues/1584) |
| `.check_margin_label = TRUE` over an `arrange()`d Arrow or DuckDB input | The check's own query inherited the ordering and failed at construction, where the same summary without the check succeeded. | `arranged-input-label-check.R` | [#1585](https://github.com/sayuks/marginplyr/issues/1585) |
| Zero-row type anchor on generic SQL dialects | The anchor rendered `WHERE (0)` or `WHERE (FALSE)` and carried a dimension in its source type beside text-cast arms. Rendered SQL only. | `generic-sql-type-anchor.R` | [#1586](https://github.com/sayuks/marginplyr/issues/1586) |
| Grouping helper inside an aggregate on native grouping sets | DuckDB refused `GROUPING()` as an aggregate argument at collection; the four backends that substitute a literal returned the value. | `duckdb-helper-inside-aggregate.R` | [#1587](https://github.com/sayuks/marginplyr/issues/1587) |
| Unsorted dtplyr result that keeps a variable label | The result evaluated to a data.table with an invalid `.internal.selfref`; a later `mutate()` warned and `:=` did not update by reference. | `dtplyr-result-selfref.R` | [#1588](https://github.com/sayuks/marginplyr/issues/1588) |
| Local Margin order over a complex key | `NaN` and `NA` partitions interleaved, as double keys did before #1571. | `local-complex-nan-na-margin-order.R` | [#1589](https://github.com/sayuks/marginplyr/issues/1589) |
| A local summary whose type differs between grouping sets | vctrs' refusal named `marginplyr_column_<i>` in place of the caller's column. | `cross-set-type-internal-column-name.R` | [#1590](https://github.com/sayuks/marginplyr/issues/1590) |
| dtplyr dimension named like the first generated alias | A dimension named `..marginplyr_dtplyr_column_1` failed at collection in every set that omits it. | `dtplyr-alias-named-dimension.R` | [#1591](https://github.com/sayuks/marginplyr/issues/1591) |

The `to-tickets` workflow published these as ten independent repair slices
with no blocking edges and no prerequisite prefactoring; splitting had been
delegated without a confirmation checkpoint. Nine carry `bug` and
`ready-for-agent`. The helper inside an aggregate carries `bug` and
`ready-for-human`, because whether to refuse it or to compute it another way
is a specification choice.

### Caller parentheses in a lazy SQL summary

```r
d <- data.frame(g = c("a", "a", "b", "b"), h = c("x", "y", "x", "y"),
                v = 1:4, w = c(10L, 20L, 30L, 40L))
summarize_with_margins(
  sql_table,
  diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
  cond = sum(if_else(g == "a" & (h == "x" | v > 3L), w, 0L), na.rm = TRUE),
  .grouping = rollup(g)
)
```

Native `dplyr::summarise()` on the same SQLite and DuckDB tables at `(g)` and
`()`, the local data frame and dtplyr all returned `diff` `28, 64, 91` and
`cond` `10, 0, 10`. The Margin summary returned `diff` `26, 62, 89` and `cond`
`10, 40, 50` on both engines. The rendered SQL was `SUM(w) - SUM(v) - 1`, where
native dbplyr rendered `SUM(w) - (SUM(v) - 1)`. It reproduced with `.by` alone
and no `.grouping`, on the native and the portable adapter, and in the text
rendered for `simulate_postgres()` and `simulate_mssql()`.

A share over such a summary divided the wrong values:
`net = sum(w) - (sum(v) - 1L), sh = share_of_total(net)` under `rollup(a, b)`
gave `0.0899` on DuckDB where the local data frame gave `0.1099` for the same
row. `sum(v) - (grouping_id() - 1L)` was wrong on DuckDB as well, which is the
"caller's own parentheses around the helper" the earlier note recorded under
#1569; that repair wrapped the generated mask and did not reach this.

A compared 29 expressions with native dbplyr on both engines. Wrong values
came from a parenthesized `-` or `+` on the right of `-`, `&` beside a
parenthesized `|` in either order (inside `if_else()` and `case_when()` too), a
parenthesized difference on the right of `%%`, and a comparison against a
parenthesized difference. `sum(v) * (max(v) == 4L)` returned 0 or 1 on SQLite
in place of the product. Two parenthesized comparisons compared with each
other were silently wrong on SQLite and failed at collection on DuckDB.
`a / (b * c)`, `a / (b / c)`, `(a + b) * c`, `!(p | q)`, `-(a - b)` and
`(x - 1)^2` were correct, because dbplyr parenthesizes those itself.

`rewrite_grouping_expr()` walks every summary expression on every backend
through `rewrite_evaluated_call_parts()`, `map_call_parts()` and
`rebuild_static_call()`, and the rebuilt call has no `(` node. Where R
evaluates the result that changes nothing, since the tree already encodes the
grouping. dbplyr renders infix operators as text and relies on the explicit
node. The defect was present at `346bb281` (2026-09-01) and at four later
snapshots, so it predates the repairs A started from. ADR 0019's amendment on
redundant parentheses records that rewrites may normalize them and names a
diagnostic echo as the consequence.

### DuckDB input registered with `duckdb::duckdb_register()`

```r
duckdb::duckdb_register(con, "d_reg", d)
summarize_with_margins(tbl(con, "d_reg"), s = sum(v, na.rm = TRUE),
                       .by = k, .grouping = rollup(a))
```

Native `summarise()` over the registered frame succeeded. The Margin summary
(with and without `.grouping` or a share), `expand_with_margins()` and typed
`inspect_grouping()` raised "Can't safely determine column metadata for this
DuckDB view. Its definition could hide source work while binding a zero-row
query", advising the caller to "supply established reader metadata directly".
Name-only `inspect_grouping()` succeeded. With `R/` taken from `5c667c95^` the
same script returned rows for every call, so commit `5c667c95` (#1524,
2026-11-09) introduced the refusal.

`duckdb_register()` creates a temporary view whose `duckdb_views().sql` is
empty, and `validate_duckdb_view_metadata()` aborts on an empty or missing
definition. `tbl(con, sql("SELECT * FROM d_reg"))`, a filtered registered
frame, a stored view over it, and a real table joined to it reproduced. A real
table on a connection that also held a registration, `copy_to()`,
`duckdb_register_arrow()`, `arrow::to_duckdb()` and an ordinary stored view
were admitted and correct. The Rd's catalog-admission bullet and ADR 0020's
#1524 amendment list macros, `query()` and `query_table()`, cycles, and reader
inference; neither names a registered data frame, and no test does.

### Local unnamed `across()` with a constant `.names`

```r
d <- data.frame(g = c("a", "a", "b"), v = 1:3)
summarize_with_margins(d, v = sum(v), across(v, ~ .x / 2, .names = "half"),
                       .grouping = rollup(g))
```

`dplyr::summarise()` at each grain returned `half` `1.5, 1.5` and `3`. The
Margin summary raised "The `across()` `.names` template `half` must produce
one name per column/function pair, but it produced 1 for 2 outputs" on data
frames, tibbles and direct data.tables, with `.by` alone too.
`predictable_local_across_names()` appends each earlier output name to the
available names, so an output that reuses an input name is listed twice and
the selection proxy holds two columns called `v`. A template containing
`{.col}`, an earlier output under a new name, the `across()` written first and
the named equivalent `half = v / 2` all worked.

### `.check_margin_label = TRUE` over an arranged input

`summarize_with_margins(arrange(x, v), s = sum(v), .grouping = rollup(g),
.check_margin_label = TRUE)` failed at construction on Arrow with "Invalid
sort key column: No match for FieldRef.Name(v)" and on DuckDB with a binder
error, neither a Package condition. With the check off, and with the check on
over the unarranged input, the same call returned three rows; SQLite, dtplyr
and local inputs returned them in all three forms. The recorded
`observed_label_collision` query was `SELECT SUM(CASE WHEN ...) AS g FROM src
ORDER BY v`.

A grand summary over an arranged input fails natively on both backends, which
is why #1023 gave the summary branches a boundary.
`check_observed_label_collision()` builds its own grand summary on the
caller's input without one. `expand_with_margins()` shares the check and
reproduced; `.margin_label = NULL` or `NA` sends no query and did not.

### Zero-row type anchor on generic SQL dialects

This finding rests on rendered SQL and published dialect rules. No server of
the dialects named was executed; a local PostgreSQL could not be started
inside the session's sandbox.

For a backend of kind `sql` with more than one branch and at least one
grouping column, the portable adapter prepends
`sql_margin_type_anchor()`'s zero-row SELECT. For
`summarize_with_margins(x, s = sum(v, na.rm = TRUE), .grouping = rollup(year))`
with an integer `year` it rendered, on `simulate_mssql()`:

```sql
SELECT [year], NULL AS [s] FROM [d] WHERE (0)
UNION ALL
SELECT TRY_CAST([year] AS VARCHAR(MAX)) AS [year], [s] FROM (...)
UNION ALL
SELECT ... 'Total' AS [year] ...
```

and the same shape with `WHERE (FALSE)` on the Oracle, Redshift, Snowflake,
Teradata, HANA, Hive, Impala, MySQL, MariaDB, Spark SQL, DBI and ODBC
simulators. Two things follow.

The predicate is `dplyr::filter(.data, FALSE)` as dbplyr translates it. SQL
Server has no boolean literal and reports error 4145, "An expression of
non-boolean type specified in a context where a condition is expected", for
such a predicate. `filter(x, 1L == 0L)` rendered `WHERE (1 = 0)` on the eight
simulators tried.

The anchor selects the dimension in its source type while every other arm
casts it to text for the label. SQL Server resolves a UNION by data type
precedence, where `int` ranks above `varchar`; Oracle 19c requires
corresponding expressions "in the same data type group" and "does not perform
implicit conversion across data type groups"; PostgreSQL's rules, which
Redshift follows, fail when "the non-unknown inputs are not all of the same
type category"; Snowflake's documentation shows `Numeric value ... is not
recognized` for a number column unioned with text. A character dimension, a
typed-missing label, and a plan with one set rendered no mismatch. On SQLite
the same anchor is deliberate (ADR 0031), and only there is a labelled
dimension projected as text in the anchor.

`?summarize_with_margins` lists these dialects as verified by simulator and
says that verifies generation, not execution. #994 was accepted on the same
kind of evidence. Whether Oracle before 23ai accepts `WHERE (FALSE)` and
whether Teradata accepts it were not established from a primary source.

### Grouping helper inside an aggregate on native grouping sets

```r
summarize_with_margins(
  x, detail_only = sum(v * (1 - grouping_bit(b)), na.rm = TRUE),
  .grouping = rollup(a, b)
)
```

The local data frame, dtplyr, SQLite and Arrow returned the detail value on
`(a, b)` rows and 0 on margins. DuckDB failed at `collect()` with "Binder
Error: GROUPING function is not supported here"; the rendered SQL held
`SUM(v * (1.0 - GROUPING(b)))`. `max(grouping_id(), na.rm = TRUE)` failed the
same way. `.duplicates = "keep"` and `"drop"` on a plan with a duplicate set
failed too; DuckDB keeps duplicate sets on the native path.

SQL forbids `GROUPING()` as an aggregate argument, so the error is the
engine's own. What is marginplyr's is that `rewrite_grouping_expr()`
substitutes the native call at every position, while `?grouping_bit` describes
the native and literal paths as returning the same flag and places no
restriction on where a helper may appear. PostgreSQL was not executed.

### Unsorted dtplyr result that keeps a variable label

With a `label` attribute on a fixed key, a dimension or a summarized column,
`data.table::as.data.table()` of an unsorted dtplyr Margin summary had
`selfrefok` 0 and `truelength` 0. A dtplyr `union_all()` of the per-grain
summaries, which keeps the same label, had `selfrefok` 1. `mutate()` followed
by `collect()` on the Margin result emitted data.table's "A shallow copy of
this data.table was taken" warning, and a `:=` inside a function did not reach
the caller's table. Values were correct.

`union_dtplyr_margin_labels()` hands the `funion()` result to
`restore_margin_variable_labels()`, whose data frame branch assigns with
`[[<-`; outside a data.table-aware namespace that is the data frame method,
which copies. `.sort = "last"` or `"first"`, a result ending in a share or a
factor rebuild, an unlabelled source and `collect()` did not reproduce. #1024
concerned the same warning on the caller's input graph, not on the result.

### Local Margin order over a complex key

A complex fixed key holding `NaN+0i` and `NA` returned
`NaN a, NA a, NaN b, NA b, NaN Total, NA Total` under `.sort = "last"`, where
`dplyr::summarise(.by = x)` keeps the two as separate groups and the double
control in the same script is contiguous. `1+NaNi` reproduced, as did a
complex dimension. `margin_nan_sort_columns()` admits a non-array double only.
Date, POSIXct, difftime, list and classed-double keys were contiguous. Matrix
and packed keys interleave as well, which ADR 0018's amendment and #1571
decided; neither names complex.

### A local summary whose type differs between grouping sets

`summarize_with_margins(d, x = if (n() > 2) "a" else 1L, .grouping =
rollup(g))` raised ``Can't combine `..1$marginplyr_column_2` <integer> and
`..2$marginplyr_column_2` <character>``. `dplyr::bind_rows()` of the two
grains names `` `..1$x` ``. `bind_margin_branches()` renames columns to
`marginplyr_column_<i>` before `vctrs::vec_rbind()` and leaves the condition
as raised, so the index moves with column position. Unnamed, `across()` and
frame-valued summaries reproduced on data frames, tibbles and direct
data.tables. `test-native-classes.R` pins the class and not the message.

### dtplyr dimension named like the first generated alias

A dtplyr input whose dimension is named `..marginplyr_dtplyr_column_1` failed
with data.table's "x and y must have the same column names" under a rollup, a
cube and a union with the empty set. The local data frame, a direct
data.table and native dtplyr `summarise(.by = )` returned rows.
`label_margin_branch()` asks `margin_safe_column_reads()` for an alias for
`.N`, which allocates against the step's columns; in a branch that omits the
dimension its name is free, so the alias takes it and the restoring
`setnames` renames the freshly written dimension to `.N`. `_2` reproduced
when another dimension had a special name. The same name as a fixed key, a
summary output or `.id`, a set that never omits the dimension, and
`expand_with_margins()` were correct, as were 27 other generated prefixes in
five roles on six backends.

## Revisions (2026-11-12)

The section *Zero-row type anchor on generic SQL dialects* drew two
consequences from rendered text. `.github/workflows/mssql-portable.yaml`,
added on this branch, ran `tools/mssql-portable-summary.R` against SQL Server
2022 (16.0.4315.2) through odbc at `3b2ae6b1`, and its recorded answers
overturn one and confirm the other.

The predicate consequence was wrong for `collect()`. The server did refuse
`SELECT [year] FROM [#portable_summary] WHERE (0)` with "An expression of
non-boolean type specified in a context where a condition is expected", and
it refused the text `dbplyr::sql_render()` returned for every plan with an
anchor in the same words. `collect()` of those same queries did not send that
text: the statement the driver reported carried
`WHERE (cast(0 AS [BIT]) = 1)`, and a character rollup and a typed-missing
integer rollup returned rows equal to the local result. So on a live SQL
Server connection the anchor's predicate does not stop a Margin summary, and
the rendered text differs from the statement that runs. Why the two differ,
whether native dbplyr shows the same difference for `filter(x, FALSE)`, and
what `last_sent_queries()` records there were not examined. Nothing was
learned about `WHERE (FALSE)` on Oracle, Redshift, Snowflake or Teradata.

The arm-type consequence was confirmed. A rollup over an integer dimension
with the default text label, and the same plan with a fixed key and a Total
share, failed at `collect()` with "Conversion failed when converting the
varchar value 'Total' to data type int." A one-set plan, which has no anchor,
equalled the local result.

#1586 was rewritten to the confirmed symptom.

## Rejected candidates

- A share whose source is a `grouping_bit()` or `grouping_id()` output was
  accepted on five inputs with values following the ordinary rule: no test or
  ADR decides whether a helper output is an eligible source, and no value was
  wrong.
- dtplyr `case_when(grouping_id() == 0L ~ ..., TRUE ~ ...)` failed with a
  length mismatch: native dtplyr fails for constant conditions, rendering the
  default as `rep(TRUE, .N)`.
- DuckDB `paste0("L", grouping_id())` gave `"L3.0"` and
  `as.character(grouping_id())` `"1.0"`: `?grouping_bit` leaves the numeric
  type to the backend.
- DuckDB `- - grouping_id()` rendered `--`, an SQL comment: native dbplyr
  renders `- -max(k)` the same way.
- The cube's plan order followed the number of omitted dimensions: no order is
  documented beyond the two-dimension example.
- A grouped input overlapping `.grouping` was refused in wording that names
  `.by`: `test-grouping-leading-errors.R` pins it.
- A `RecordBatchReader`, directly or under a query, was refused, and remained
  unconsumed afterwards: documented.
- Arrow struct and list keys, `in_schema()` on DuckDB, DuckDB BIT, UNION,
  BIGNUM and fixed ARRAY columns, a dtplyr list key, and SQLite integer `sum()`
  overflow failed: each native control failed identically.
- A SQLite untyped column holding `1` and `1.0` displayed differently by grain
  under a string label: SQLite's group representative, plus the displayed-text
  ordering the earlier note rejected.
- A SQLite NOCASE dimension holding `'total'` collided with `"Total"`: the
  database's own equality.
- A named matrix dimension lost its column names beside margins:
  `bind_rows()` of the per-grain summaries drops them (ADR 0043).
- The Grand total row had a share of 1 over a zero or missing source:
  `test-share.R` decides it.
- A sorted lazy result followed by `count()`, `summarise()` or `union_all()`
  failed: disclaimed, and rejected in the earlier note.
- SQLite `compute()` left `sqlite_stat` tables; a permanent DuckDB
  `compute(overwrite = TRUE)` onto the source destroyed it: native dbplyr.
- A share collected as logical after `head()` or `filter()` on SQLite when
  every value was missing: the type is promised for direct `collect()` and
  `compute()`.
- A DuckDB lazy result kept the level order captured at build after the ENUM
  was redefined: the display-label section disclaims schema-change detection.
- `simulate_access()` refused a share with "Window function `max()` is not
  supported by this database": dbplyr's translation for that dialect.
- Matrix and packed keys interleaved `NaN` and `NA`: decided by ADR 0018.

## Covered without a finding

- Recent repairs: 67 unnamed-summary argument sets on data frames and
  data.tables against per-grain oracles; 32 special names as fixed key and as
  dimension under both shares on dtplyr; 61 expressions around the helpers on
  five backends (the mask itself was correct); 70 random Arrow orderings with
  long and alias-like names.
- Input shapes: Arrow tables, parquet, feather and CSV datasets, hive and
  directory partitions, 18 query shapes and 18 column types, with files
  overwritten after `open_dataset()` to confirm nothing read them before
  collection; DuckDB views, attached databases, collations, identifier
  equivalence against every private alias, and 21 column types with shares;
  SQLite mixed storage classes, declared-versus-stored mismatches, NOCASE and
  RTRIM, `WITHOUT ROWID`, attached databases; packed, matrix, array and list
  columns as dimensions, fixed keys and summaries on local inputs; keyed and
  indexed data.tables.
- Total shares: about 4,100 generated calls on data frames, tibbles,
  data.tables, dtplyr, SQLite and DuckDB over random plans to depth three,
  every `.duplicates`, `.sort` and `.margin_label` form, zero to three fixed
  keys through `.by` and `group_by()`, several shares and sources at once,
  query-shaped inputs, and 25 follow-up verb chains; a 36-probe naming and
  refusal matrix that agreed on every backend; edge values including a zero or
  missing Grand total.
- Grouping specifications: 1,025 generated specifications across five
  backends for values, column order, `.id` numbering and agreement with
  `inspect_grouping()`, and 1,010 calls for exact Margin order; selections
  that match nothing, overlap, rename or repeat; helpers with 30, 31 and 32
  columns; zero-row inputs for every specification kind on seven input kinds.
- Around a call: follow-up verbs on results from six backends; the SQLite and
  DuckDB `compute()` matrix; no relation, macro, type or prepared statement
  left after success, refusal or a collection error; read-only databases; an
  open transaction rolled back; the source changed between build and collect;
  the caller's data.table unchanged in names, key, indices, attributes,
  column addresses and self-reference; lazy results built in closures and
  collected in another order.
- Session state: 15 sets of global options (`scipen`, `OutDec`, `digits`,
  `warn = 2`, `stringsAsFactors`, data.table optimization and others) on six
  backends gave identical results; a stateful summary was evaluated once per
  group per set and its share stayed consistent with the displayed source.
- Rendered SQL: seven PostgreSQL renderings, native and portable, with shares,
  Parent shares, `.id` and both Margin orders, executed on DuckDB and equal to
  the local result; six call shapes rendered on all sixteen dbplyr simulators.

## Not covered

No remote dialect was executed. Pool-like connection wrappers, interruption
during `compute()`, Arrow extension, run-end-encoded and union types, DuckDB
ICU collations, SQLite custom collations and virtual tables were not
examined. `arrow::to_duckdb()` of a Margin result raised `std::exception`
where the native control worked, and was not pursued. Whether another
definition-less catalog view exists besides a registered data frame, whether
other callers of `margin_safe_column_reads()` pass a name absent from the
step, the dropped parentheses in the expansion and nesting verbs, and #1545
to #1557 were not re-probed.
