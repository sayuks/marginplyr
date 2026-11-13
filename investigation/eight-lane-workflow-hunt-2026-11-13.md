# Eight-lane workflow hunt: generated cases, recent fixes, PostgreSQL, types, contracts, dialects, composition and mutants

Investigated: 2026-11-13
Revised: 2026-11-13 — the triage decisions and replays recorded under *Revisions (2026-11-13)* in this note
Repository snapshot: `6165f4768400a28520cc8977e056da0db49a32ed`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented. The snapshot was
built outside the checkout and installed into a private library, and every
investigator observed that installed build.

Eight fresh-context investigators worked read-only from separate starting
points and reported before any candidate was shared:

| Lane | Starting point | Model and effort | Candidates |
| --- | --- | --- | --- |
| 1 | Generated specification, data and backend cases against a plain dplyr summary at each grain | claude-opus-5-5, high | 0 |
| 2 | Shapes next to the bug fixes merged from 2026-10-30 | claude-opus-5-5, high | 2 |
| 3 | The native grouping-sets path on a real PostgreSQL 17, against DuckDB | claude-opus-5-5, high | 2 |
| 4 | Column classes in every role, verb and backend | claude-opus-5-5, high | 1 |
| 5 | `sqlite-typed-order.R`, `duckdb-csv-metadata.R`, `margin-label.R`, `factor.R`, `backend-metadata.R` and `static-bindings.R` read against the ADRs and `CONTEXT.md` | claude-opus-5-5, xhigh | 4 |
| 6 | Every dbplyr simulated dialect against the specifications, for the `UNION ALL` fallback SQL | claude-sonnet-5-5, medium | 1 |
| 7 | Grouped and row-wise inputs, and Margin results passed on to Margin verbs, joins and `compute()` | claude-sonnet-5-5, high | 2 |
| 8 | Surviving mutants of `share.R` and `summary-selections.R`, followed to the test hole and then to the behaviour | claude-sonnet-5-5, high | 0 |

Each of the twelve candidates then went through three checks. A duplicate
lookup (claude-sonnet-5-5, low) searched the issue tracker, open and closed,
through `gh issue list --search`, `investigation/`, `tests/testthat/` and
`NEWS.md`. A refuter on the other model from the finder (claude-fable-5-2 for a
candidate found on claude-opus-5-5, claude-opus-5-5 for one found on
claude-sonnet-5-5, both at high effort) argued that the candidate was not a
defect, looking for the right native control and for an ADR, Rd sentence or
earlier decision that made the behaviour intended. The coordinator replayed
every candidate in a fresh R process, read the code each mechanism names, and
decided. A second round then covered only the paths next to the confirmed
findings; *Second round* says how it was checked.

Versions were R 4.6.1 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, duckdb 1.5.5, RSQLite 3.53.3, RPostgres 1.4.10, DBI 1.3.0,
arrow 25.0.1, tidyr 1.3.2, rlang 1.3.0, vctrs 0.7.3, sf 1.1.2 and haven 2.5.5.
SQL work used process-owned in-memory SQLite and DuckDB databases, files under
the session's temporary directory, and a temporary PostgreSQL 17.11 cluster the
maintainer had started, reached over a Unix socket. Every PostgreSQL object was
a synthetic table inside a schema the investigation created and dropped. No
existing database or outside data was read.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. Summaries and shares were compared with ordinary dplyr
summaries at each explicit grain, divided by hand and bound in Grouping-plan
order; expansions and nested cells with a row-copy oracle; PostgreSQL results
with native dbplyr at each grain on the same connection, DuckDB agreement being
a lead and not an oracle. The DuckDB preflight candidates were measured by
value dependence: whether a typed plan changed when only one data row of a file
changed, with a native lazy construction over an absent file and an
explicit-schema reader as controls.

Every difference on a backend was followed by the native control on that
backend before it was counted, and the refutation step repeated that choice
independently. In the first round it changed the standing of five candidates:
three were rejected, and two were narrowed to a specification question.

## Confirmed findings

Each script under `investigation/2026-11-13-eight-lane-workflow/` is run from
the repository root, prints the observed behaviour and exits zero.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| A SQL input whose top node is a join, with an x side ordered by a raw `sql()` term or a constant | The Margin summary, and the expansion whenever it moves a column, failed at construction with a dbplyr-internal purrr error. | `join-input-raw-sql-order.R` | [#1640](https://github.com/sayuks/marginplyr/issues/1640) |
| A sorted summary on the native grouping-sets path over a window-ordered input, with every label typed-missing and no `.id` | The result's `ORDER BY` ended in the input's window-order term, a column the result does not have, and `collect()` failed in the database. | `native-sort-window-ordered-input.R` | [#1641](https://github.com/sayuks/marginplyr/issues/1641) |
| `PIVOT_WIDER` and `PIVOT_LONGER` over a CSV file on DuckDB | The CSV guard that refuses `PIVOT`, `UNPIVOT` and `FROM` admitted them, and the zero-row metadata query sniffed the file. | `duckdb-pivot-wider-csv-sampling.R` | [#1642](https://github.com/sayuks/marginplyr/issues/1642) |
| A summary with two or more grouping sets over a row-limited input on a generic SQL backend | The first `UNION ALL` operand carried the input's `ORDER BY` and `LIMIT`, and the engine refused the statement. | `generic-sql-limited-input-anchor.R` | [#1643](https://github.com/sayuks/marginplyr/issues/1643) |
| Direct `collect()` of a SQLite Margin result with dbplyr's deprecated `cte` argument | The call was refused, where native `collect()` and the package's own `compute()` accept the argument with a deprecation warning. | `sqlite-collect-deprecated-cte.R` | [#1644](https://github.com/sayuks/marginplyr/issues/1644) |

The fourteen tickets of this note were published as independent repair slices,
without a confirmation checkpoint, as the maintainer had delegated. The ten
under *Confirmed findings* and in the *Second round* table carry `bug` and
`ready-for-agent`; the four under *Filed for a decision* carry `bug` and
`ready-for-human`. #1649 gathers four statements of one scan that are edited
and tested in one place, and says it shares a function with #1642.

### A join input ordered by a term that names no column

```r
top <- tbl(con, "t") |> arrange(dbplyr::sql("v DESC NULLS LAST")) |> head(3)
joined <- left_join(top, tbl(con, "u"), by = "g")
collect(summarise(group_by(joined, h), n = n()))            # 2 rows
summarize_with_margins(joined, n = n(), .grouping = rollup(h))
#> <purrr_error_indexed> In index: 1. Caused by error: Result must be length 1, not 0.
```

SQLite and DuckDB failed alike for a left, inner, right, semi and anti join,
for a `filter()` or `group_by()` after the join, and for `arrange(1L)`.
`expand_with_margins()` failed with `.id` on both and returned rows when the
plan moved no column; `inspect_grouping()` returned. Native `collect()`,
`summarise()`, `count()`, `mutate()` and `union_all()` of the same input
succeeded. A `mutate()` after the join, an ordering on the y side, and
`arrange(desc(v))` before the join were correct.

#1622 reported the same failure for an input whose top node is a select query,
and PR #1631 repaired it. By reading, `drop_columnless_window_order()` returns
early unless the lazy query is a `lazy_select_query`, while a join query
inherits `order_vars` from its x side; the trace reached dbplyr's
`rename_order()` through `summary_selection_proxy()`'s own `select()`.
Widening that guard in memory made the summary equal the per-grain expectation
on both backends. Native `relocate()` of the join fails the same way, which
#1622 had already ruled insufficient: the caller wrote no `select()`. #1622's
acceptance list named no join, so this is a neighbouring input class and not an
unmet criterion. A set-operation input fails natively at `collect()` and is not
counted.

### A sorted native summary over a window-ordered input

```r
input <- tbl(con, "t") |> dbplyr::window_order(v)
summarize_with_margins(input, s = sum(v, na.rm = TRUE), .grouping = rollup(a),
                       .margin_label = NULL, .sort = "last") |> collect()
#> PostgreSQL: column "v" does not exist
#> DuckDB:     Referenced column "v" not found in FROM clause!
```

The rendered `ORDER BY` was the documented key followed by `v`. PostgreSQL
17.11 and DuckDB 1.5.5 failed for `window_order(v)`, `arrange(v)` and
`arrange(v) |> head(3)`, under `"last"` and `"first"`, for `NULL` and
`NA_character_` labels. The same call returned the hand-computed rows with the
default label, a mixed label list, `.id`, `.sort = "none"`, an unordered
input, a window order over a result column, on SQLite, and for
`expand_with_margins()`.

*Margin order* gives the key as the result's own leading grouping columns and
says "the order does not depend on the label"; ADR 0018's amendments say a
Margin order clears "a window ordering that cannot reproduce it".
`test-sql-arranged-summary.R`, added for #1596, asserts that a DuckDB summary
over an `arrange()`d input collects under each `.sort` value, with the default
label only. By reading and by a trace on
`dbplyr:::add_arrange()`, `order_margin_result()` calls `arrange()` on a native
summary that still holds the input's `order_vars`, and dbplyr appends them to
the new `ORDER BY`; `forget_margin_window_order()` runs afterwards. With a text
label or `.id` an earlier projection had already dropped the term.

Native `summarise(.by = a) |> arrange(a)` on the same input fails for the same
reason and succeeds after `window_order()`. That control is the right kind of
object, and it is the one point against the finding. It is counted because the
caller wrote `.sort`, not `arrange()`, and the same call's success depends only
on the label.

### `PIVOT_WIDER` and `PIVOT_LONGER` over a CSV file

`duckdb_keywords()` lists `pivot_wider` and `pivot_longer` beside `pivot` and
`unpivot`. With `tbl(con, sql("PIVOT_WIDER 'f.csv' ON g IN ('a', 'b') USING
first(v) GROUP BY k"), vars = ...)`, `inspect_grouping(.grouping =
grouping_set(where(is.numeric)))` returned `(a, b)` for a numeric file and
`()` after one data row was changed to text; the `PIVOT` spelling was refused
for both. `summarize_with_margins()` built, with `relation_catalog`,
`selection_proxy` and `result` recorded. With the file absent the native lazy
table built, and typed inspection reached DuckDB's IO error where `PIVOT` gets
the Package refusal. The same spelling over a table, and over `read_csv()` with
`columns` and `auto_detect = false`, kept one plan for both files.

ADR 0020's amendment for #1519 says the CSV refusal covers filename relations
and "equivalent quoted or escaped spellings in relation positions", and
`test-duckdb-csv-metadata.R` pins `PIVOT '<csv>'` as refused. By reading,
`duckdb_metadata_relations()` opens a relation position after `from`, `pivot`
and `unpivot` only. `SELECT ... FROM (SUMMARIZE 'f.csv')`, `DESCRIBE` and
`SHOW` passed the guard the same way; their output types are fixed, so no plan
can change, and the evidence is the CSV sniffer's error on a file holding
invalid UTF-8.

### A row-limited input on a generic SQL backend

```r
top <- head(arrange(tbl(con, "df"), a), 3)
summarize_with_margins(top, n = sum(v, na.rm = TRUE), .by = k, .grouping = rollup(a))
#> SELECT "k", CAST("a" AS TEXT) AS "a", NULL AS "n" FROM "df"
#> WHERE (FALSE) ORDER BY "a" LIMIT 3
#> UNION ALL ...
```

The connection was a `DBIConnection` subclass forwarding to an in-memory DuckDB
engine, so the package took its portable path and the statement ran: the engine
refused it at `UNION`. `head(t, 3)` and `.sort = "last"` failed alike. Native
per-set summaries combined with `union_all()` on the same connection returned
the five hand-computed rows, as did the Margin summary over the computed
input, a one-set summary, an unlimited `arrange()` and the expansion.
`simulate_mysql()`, `simulate_mssql()` and `simulate_oracle()` rendered the
same first operand with `LIMIT 3`, `TOP 3` and `FETCH FIRST 3 ROWS ONLY`;
`simulate_postgres()` did not, since it builds no anchor. No MySQL, SQL Server
or Oracle server was run: for those the refusal rests on the rendered text and
each engine's grammar.

*Database backend coverage* says other database backends use "a portable
`UNION ALL` query with one branch per grouping set and the same result". By
reading, `sqlite_margin_input()` seals a limited or ordered input into one
relation only for a `SQLiteConnection`, and `sql_margin_type_anchor()` then
builds `transmute(filter(.data, FALSE), ...)` on the unsealed input. #769 fixed
this mechanism for SQLite and #835 for expansions. Plain dbplyr renders the
same shape for a hand-written `union_all()` of two limited queries; here the
caller wrote neither the union nor the anchor. An unlimited `arrange()` on a
`lazy_frame()` rendered the same operand, and on a real table did not.

### `collect(cte = TRUE)` on a SQLite Margin result

`collect(x, cte = TRUE)` raised "Exactly one of `sql_options` or `cte` must be
supplied" for an unsorted summary, and dbplyr's unused-dots error for a sorted
summary, a sorted expansion and a typed-missing summary; `cte = FALSE` was
refused too. A native union of per-grain summaries, arranged or not, returned
its rows with `lifecycle_warning_deprecated`, as did the sorted DuckDB summary,
a one-set SQLite summary, and `compute(x, cte = TRUE)`.
`collect(x, sql_options = dbplyr::sql_options(cte = TRUE))` worked on every
branch.

`design/specs/sqlite-b-direct.md` asks for argument tests covering "deprecated
options where still supported", and three tests pin `compute(cte = TRUE)`. By
reading, `collect.marginplyr_sqlite_typed_result()` has no `cte` formal: the
delegated branch passes the caller's `cte` together with its own `sql_options
= NULL`, which is the pair dbplyr refuses, and the direct branch hands `cte` to
`db_collect()`'s dots check. The Rd and the ADRs say nothing about which
`collect()` arguments the result accepts, so declaring the deprecated spelling
unsupported there would also answer it.

## Filed for a decision

These stand as observed. Each needs a choice the contract does not make.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| A DuckDB macro called through a dbplyr-translated expression | The macro was not checked, the zero-row metadata query bound it, and its `read_csv_auto()` sniffed the file; the `sql()` spelling of the same call was refused. | `duckdb-translated-macro-csv-sampling.R` | [#1650](https://github.com/sayuks/marginplyr/issues/1650) |
| A stored DuckDB view over `read_parquet()`, `range()`, `generate_series()` or `unnest()` | Every Margin verb refused it, while the same SQL written directly and a view over the same file as a replacement scan were admitted. | `duckdb-view-over-table-function.R` | [#1651](https://github.com/sayuks/marginplyr/issues/1651) |
| A later summary reaching a share's name through a function held as a value | The summary read the hidden input column of that name and returned its values, with no condition. | `share-name-read-through-function-value.R` | [#1652](https://github.com/sayuks/marginplyr/issues/1652) |
| A dimension whose class has no character form, under a non-missing label | The native cast error was raised from a generated label step and blamed an internal `dplyr::summarize()` or `dplyr::mutate()` call. | `label-conversion-condition-context.R` | [#1653](https://github.com/sayuks/marginplyr/issues/1653) |

### A translated macro call

`mutate(t, y = hidden_first(v))` rendered `SELECT *, hidden_first(v) AS y FROM
t`, where `hidden_first()` is a macro reading a CSV through `read_csv_auto()`.
The typed plan was `(v, y)` for a numeric row and `(v)` after it was changed to
text; `summarise()` and `filter()` carrying the call behaved alike, and with
the file absent the native `mutate()` built and rendered while typed
inspection reached DuckDB's IO error. `mutate(y = sql("hidden_first(v)"))`
and the authored `SELECT` were refused for both files.

ADR 0020's amendment for #1506 says the first exemption "does not permit CSV
auto-detection to sample source values while DuckDB binds a zero-row query",
and the Rd says opaque macros are refused. By reading,
`validate_duckdb_csv_query()` walks the captured calls and quosures for `sql`
and table-path objects and never compares a call head with the catalog's macro
names; #1533 was scoped to authored SQL. The defect stands. The remedy is the
choice: the authored path refuses every macro, including `add_one(x) AS x + 1`,
so applying that rule to translated calls would refuse `mutate(y =
add_one(v))`, which returned rows.

### A stored view over a table function

ADR 0020's amendment for #1524 says "table functions whose safety is not
established are refused before binding", and the refusal is that rule: on the
stored-view path any function relation other than the six CSV and JSON readers
aborts. The contract does not say which table functions are established. The
authored path admits `range()`, `generate_series()`, `unnest()` and a Parquet
scan, pinned by `test-duckdb-authored-calls.R` and
`test-duckdb-csv-metadata.R`; a view over the same Parquet file written `FROM
'file.parquet'` was admitted; the Rd's list of refusals names no table
function; and the native zero-row types of the `read_parquet()` view came from
file metadata. The refuter read that at `5c667c95` only suffix-named relations
were followed into view definitions, so such a view was admitted before #1525
and #1526, whose criterion says "Preserve safe ordinary tables/CTEs/views";
that history was read, not run. Name-only `inspect_grouping()` returned.

### A share's name reached through a function value

```r
d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5), sh = c(100, 200, 300))
summarize_with_margins(d, s = sum(v), sh = share_of_total(s),
                       z = { f <- get; sum(f("sh")) }, .grouping = rollup(g))
#> z = 300, 300, 600
```

`sum(sh)`, `sum(get("sh"))`, `rm(sh)` after a local `sh`, and `get("rm")(sh)`
were refused with "Ordinary summaries cannot use an earlier Total share".
`f <- rm; f(sh)`, `f <- get`, `identity(get)("sh")`, `list(get)[[1]]("sh")` and
a caller-level alias of `get` returned the input column's sums. Without an
input column named like the share the call failed with "object 'sh' not
found". The ordinary-summary stage alone, native `summarise()` holding no `sh`
summary, returns the same 300 and 300.

The Rd says "A later summary cannot use a share in the same call", and #130's
contract says an undecidable shape is refused, "never" passed silently. The
header of `is_reflective_lookup()` names `f <- get` as the smallest case a
static walk cannot answer and declines to over-report; a test pins "a primitive
reached as a value is out of the walk's reach", with the comment that reaching
the mask "would be a silent miss"; and #1091's criteria excluded "an
opaque-side-effect inference promise". Here the value does reach the mask. An
alias written inside the expression is statically visible; a caller-level
alias or closure is not.

### The blamed call of a failed label conversion

A printable `vctrs_rcrd` subclass with a `format()` method and no character
cast worked as a dimension under a typed-missing label. Under the default
label, `summarize_with_margins()`, `expand_with_margins()` and
`nest_with_margins()` raised vctrs' "Can't convert" from the observed-collision
check with the call `dplyr::summarize(source, !!!checks)`, and with
`.check_margin_label = FALSE` from `dplyr::mutate(.data, !!!expressions)` with
an argument line quoting `marginplyr:::margin_display_character`. The refusal
is native: `mutate(d, r = as.character(r))` fails with the same cause.

`CONTEXT.md`'s *Condition context* says the blamed call is the Margin verb
"rather than the columns, the expressions, and the calls marginplyr allocated",
and #432 and #1573 moved the call of an External condition under it. ADR 0021,
`design/architecture.md` and `?marginplyr_conditions` scope the restatement to
the caller's own expressions, and the same entry says a context it cannot
restate "it leaves as it found it". The default-check rendering was
byte-identical to native dplyr running the same check, apart from the call. No
installed class tried — hilo, interval, period, hms, integer64, blob,
`numeric_version`, POSIXlt, `list_of` — lacks a character form.

## Revisions (2026-11-13)

A triage pass the same day decided the four tickets above, as the maintainer
then delegated, and replayed candidate answers in memory without editing the
checkout. Each decision lives on its ticket; this section holds what the
replays established and what changed.

- **A stored view over a table function.** #1651 was decided as a repair: a
  view's definition is held to the rule the same SQL meets when authored. With
  the one stored-view condition changed in memory to refuse a function relation
  only when it is `query` or `query_table`, views over `read_parquet()`,
  `range()` and `unnest()`, and a view over such a view, returned the
  hand-computed rows; views over `query('SELECT * FROM <csv>')`,
  `read_csv_auto()`, a CSV replacement scan, a table macro and a scalar macro
  stayed refused. The ticket carries `ready-for-agent`.
- **A translated macro call.** #1650 was decided as a repair: a caller-defined
  macro reached by the metadata statement is refused in every spelling. A check
  of R call heads refused `mutate(y = hidden_first(v))` but cannot see what a
  translation emits: with a caller macro named `lower`, `tolower(g)` rendered
  `LOWER(g)` and reached it. `sql_render.marginplyr_duckdb_selection()` holds
  the rendered zero-row statement once, inside the collect path; scanning that
  text there refused the translated call, the same call in a join's y input,
  `tolower()` under the caller macro and a `DBI::SQL()` CSV subquery, and kept
  the typed plans of ordinary translated, joined and windowed pipelines. The
  ticket carries `ready-for-agent`, is blocked by #1654, and took over #1648,
  which was closed as its duplicate.
- **DuckDB's own macros, found by that replay.** The same scan refused
  `na_if()`, which dbplyr renders as `NULLIF()`, and the reason held for
  authored SQL at the snapshot: `SELECT g, nullif(v, 0) AS v FROM t`, a view
  over it, and `date_add()`, `list_sum()`, `array_to_string()` and
  `current_database()` were refused as macro calls in a database holding no
  caller macro, while the `CASE` spelling and `coalesce()` returned `(v)`.
  `duckdb_functions()` listed 114 scalar and 4 table macro names, all in
  database `system`, and `duckdb_metadata_catalog()` reads them without
  distinction. A refuter on claude-opus-5-5 at high effort overturned the
  coordinator's premise that such a macro cannot hide source work, with two
  cases the coordinator then replayed: `histogram_values('<csv>', k)` is
  defined over `query_table()` and its zero-row type followed a data row, and a
  caller macro named `list_aggr` made the zero-row type of `list_sum([v])`
  follow one, because a system macro's body resolves its names in the caller's
  search path. What survived is the refusal when the catalog holds no caller
  macro. It was filed as
  [#1654](https://github.com/sayuks/marginplyr/issues/1654) with `bug` and
  `ready-for-agent`; its script is `duckdb-built-in-macro-refusal.R`. Only the
  statically linked extensions were loaded.
- **A macro called without parentheses.** The same refuter reported, and the
  coordinator replayed, that with a caller macro named `current_user`,
  `SELECT current_user AS u, v FROM t` gave `(u, v)` then `(v)`, where
  `current_user()` was refused. It was added to #1647, whose scan it escapes.
- **A share's name reached through a function value.** The behaviour stands:
  the header of `is_reflective_lookup()` already declines to follow a function
  held as a value, ADR 0010 promises the "statically detectable" errors, and a
  caller-level alias is invisible to any static walk. What is wrong is the
  header's sentence that such a value "is safe", measured for
  `sapply(c("share"), get)` only. #1652 was narrowed to removing that claim
  and carries `documentation` and `ready-for-agent`.
- **The blamed call of a failed label conversion.** #1653 was closed as
  `wontfix`. The refusal is native and correct; ADR 0021 scopes the
  restatement to the caller's own expressions and `CONTEXT.md` leaves a
  context that cannot be restated as it was found; moving the call alone would
  pair the Margin verb with an argument line the caller did not write; and the
  message names the dimension and the cause, the standard #454 was closed on.

## Rejected candidates

- On PostgreSQL with an `en_US.UTF-8` database collation, a sorted summary
  ordered character values as the database collates them, `_x a A b B Z`,
  where local data gives the C order. Native `arrange()` on the same connection
  returned the same order, a `COLLATE "C"` column switched both, and #1325 with
  `test-sqlite-cte-collation.R` and `test-sqlite-expansion-source-types.R` pins
  a collation-following lazy Margin order. The margin row's position did not
  depend on the collation. What remains is wording: the Rd sentence "A Margin
  order promises what `dplyr::arrange()` promises in the C locale, which it
  fixes" carries no scope since the #1633 trim.
- `nest_by_with_margins()` of a direct data.table returned a row-wise tibble
  holding `.internal.selfref`, which `dplyr::nest_by()` does not. The class
  vectors were identical, `rowwise(dt)` and `group_by(dt)` carry the same
  pointer natively, `:=` after `as.data.table()` and a `saveRDS()` round trip
  were clean, and `test-variable-labels.R` and `test-data-frame-subclass.R`
  leave the field out of the comparison as backend-owned.
- `expand_with_margins()` of an sf tibble returned class `sf`, `data.frame`
  where native `union_all()` keeps `tbl_df`. ADR 0044 says an extra frame class
  "does not promise complete class equivalence to that frame class's own
  methods" and ADR 0042 that its "existing S3/vctrs behavior is respected";
  `vctrs::vec_rbind()` of two sf tibbles returns `sf`, `data.frame` natively.
  The Rd states the class reference without that scope.
- The dtplyr half of the label-conversion candidate: native dtplyr raises the
  same bare `vctrs_error_cast` with no dplyr context.
- An unlimited `arrange()` on a `lazy_frame()` rendering `ORDER BY` before
  `UNION ALL` on the simulated dialects: a real table on the generic
  connection collected correctly.

## Second round

Four investigators took only the paths next to the confirmed findings: the
ordering and limit state of SQL inputs on SQLite, DuckDB and PostgreSQL
(claude-opus-5-5, high); spellings of the DuckDB preflight (claude-opus-5-5,
xhigh); the portable path executed on the generic connection
(claude-sonnet-5-5, medium); and the arguments of the SQLite result methods
(claude-opus-5-5, high). They reported fourteen candidates. One duplicate
lookup and three refuters on claude-fable-5-2, batched by code site, checked
them, and the coordinator replayed each from the two scripts named below.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| `compute()` of a SQLite Margin result | A grouped query derived from a result with a Boolean-word column lost its grouping and window order, and a later grouped summary returned one row; an unsorted result lost its usable window ordering; a sorted result over an expression-ordered input could not be computed. | `sqlite-compute-window-order-and-groups.R` | [#1645](https://github.com/sayuks/marginplyr/issues/1645) |
| A DuckDB CSV reader carrying `auto_detect = false` with `union_by_name = true` and no schema, or with the argument repeated | The reader was admitted and DuckDB still inferred the types from data rows. | `duckdb-preflight-neighbouring-spellings.R` | [#1646](https://github.com/sayuks/marginplyr/issues/1646) |
| A DuckDB macro called with dot-chained syntax in authored SQL or a stored view | `v.hidden_first()` escaped the macro check that refuses `hidden_first(v)`. | `duckdb-preflight-neighbouring-spellings.R` | [#1647](https://github.com/sayuks/marginplyr/issues/1647) |
| Verbatim SQL carried into a dbplyr verb as `DBI::SQL()` or `ident_q()` | The fragment was not inspected, where the same text as `sql()` is refused. | `duckdb-preflight-neighbouring-spellings.R` | [#1648](https://github.com/sayuks/marginplyr/issues/1648) |
| `UNION BY NAME TABLE`, a string literal split across lines, a `WITH RECURSIVE` self-reference and `USING KEY`, each naming a CSV file | The lexical scan saw no relation and the zero-row metadata query sniffed the file. | `duckdb-preflight-neighbouring-spellings.R` | [#1649](https://github.com/sayuks/marginplyr/issues/1649) |

Three results were the repair of a first-round finding and went onto its
ticket. The join input failed on PostgreSQL 17.11 as on SQLite and DuckDB. On
the generic connection, `select()`, `filter()`, `group_by()` and
`window_order()` over a limited input failed as `head()` alone does; about 290
further results over every verb, specification and option matched per-grain
native summaries. `compute(x, cte = TRUE)` on a SQLite result rendered its
zero-row `CREATE` with CTEs and its `INSERT` without, with identical rows.

### `compute()` of a SQLite Margin result

By reading, `compute.marginplyr_sqlite_typed_result()` ends with a plain
`tbl()` of the destination and restores neither `group_vars()` nor, for an
unsorted result, `op_sort()`; dbplyr's own `compute()` restores both, as the
DuckDB method does since #1019. A derived result reaches the method only when
it holds a Boolean-word column, which is why the same pipeline without one
kept `g` and `v`: `summarise(n = n())` after `compute()` returned one row of
12 where a 6, b 6 is the hand count and the native control's answer. Two
second-round lanes reported that shape independently.

The direct branch also hands dbplyr's `compute()` the result's zero-row type
anchor, which still carried the input's `order_vars`: for `arrange(-v)` the
anchor's window ordering was `-v` while the result's was empty, and dbplyr's
replay refused the term. ADR 0018 says a Margin result "records no window
ordering at all" and that clearing it "is what makes a sorted result
materializable". `arrange(desc(v))` computed; DuckDB and PostgreSQL computed
the same expansion. The unsorted shape rests on ADR 0018's `.sort = "none"`
amendment and the #1019 precedent, not on a sentence about SQLite `compute()`,
and a maintainer could rule it a choice. A sorted computed result remained an
ordered query with no window ordering, as #663 and #1625 decided.

### The DuckDB preflight's enumerations

Nine spellings passed the preflight. The measure was the one used for
`PIVOT_WIDER`: the typed plan for a file whose one data row is `101`, then
`source_tail`, with the native lazy table built over an absent file and an
explicit-schema reader as controls.

- `read_csv([...], auto_detect = false, union_by_name = true)` gave `(k)`
  then `()`; with `header = true, columns = {'k': 'BIGINT'}` added it gave
  `(k)` twice. `auto_detect = false, auto_detect = true` gave `(k)` then
  `()`: DuckDB applies the last value. By reading, the CSV branch is `safe <-
  any(disabled)`. ADR 0020's sentence for #1506 asks for "a literal
  `auto_detect = false`", which both readers carry, and its first sentence
  forbids the sampling. With a literal schema present no variant depended on
  the file.
- `SELECT g, v, v.hidden_first() AS y FROM t` gave `(v, y)` then `(v)`,
  where `hidden_first(v)` was refused. By reading, the call-head scan resolves
  a dotted name as a qualified one only.
- `mutate(y = !!DBI::SQL(<subquery over the CSV>))` and the same through
  `ident_q()` gave `(v, y)` then `(v)`, where `sql()` was refused. By reading,
  `validate_duckdb_csv_query()` validates objects inheriting `sql` or
  `dbplyr_table_path`.
- `... UNION ALL BY NAME TABLE "p.csv"`, and `FROM 'p.csv'` after `UNION ALL
  BY NAME` inside a parenthesised FROM-first query; `FROM 'p.c'` continued on
  the next line by `'sv'`; `WITH RECURSIVE "p.csv" AS (SELECT * FROM "p.csv")
  ...`; and `TABLE "p.csv"` after a `USING KEY` CTE each gave `(k)` then `()`,
  and the neighbour without `BY NAME`, the line break, `RECURSIVE` or `USING
  KEY` was refused. The refuter prototyped one change per spelling on a
  scratch copy of the scan; each captured only its own spelling.

The refuters read these, with `PIVOT_WIDER`, as one gap class: a lexical
approximation of DuckDB's grammar that admits what it does not model. They
found no structural change that closes all of them. Refusing every
CSV-suffixed literal contradicts ADR 0020's "Scalar strings and non-CSV
relations do not establish an inferred CSV source" and still misses the split
literal and the recursive CTE; DuckDB's own parser needs the json extension,
which could not be loaded, and would not settle the recursive CTE. In every
case the harm is source work at a time the caller did not ask for, never a
wrong value.

### Second-round candidates not counted

- On the native path an unsorted summary with typed-missing labels kept an
  input window-order term naming no result column, and a later `compute()` or
  `row_number()` failed. Native `summarise(.by)` kept the same term and failed
  the same way on DuckDB and PostgreSQL, the caller wrote the later verb, and
  ADR 0018 says "marginplyr adds no rule of its own for that case".
- A hive-partitioned `read_csv()` glob with an explicit schema took the type
  of its partition column from the directory name. No file content was read: a
  zero-byte file under `yr=2024` still bound `yr` as an integer. The same
  mechanism types Parquet partitions, which the tests admit, and no sentence of
  ADR 0020 or the Rd covers path listings.
- `sniff_csv()`, `U&''` literals, nested comments, eighteen further relation
  positions and about thirty other spellings were refused or harmless.

## Test holes without a defect

Lane 8 applied 75 mutants one at a time to a private copy, 42 in `R/share.R`
and 33 in `R/summary-selections.R`, against the focused test files only. 49
were killed and 26 survived. Every survivor was followed through the public
interface on the unmutated build with an independent expectation, and none
gave a wrong value, a wrong refusal or a native-control mismatch. The
survivors that an input would kill:

- `apply_share_ratios()` without `is.na(denominator)`: a non-`NaN` source under
  a `NaN` Grand total, `m = sum(v) / sum(w)` over `v = c(1, -1)`, `w = c(1,
  -1)`, where the build returned `NA_real_` for both shares.
- `validate_share_request()` without the fixed keys: `.by = g` with `g =
  share_of_total(s)`, refused by another check with other wording.
- `plan_share_expressions()` with a wrong flattened position: on dtplyr, a
  two-output `across()` share before `bad = range(v)`, which the build refused
  with `marginplyr_share_cardinality_error`.
- `rewrite_across_selection()` expanding `.names` without the unpack guard: a
  lazy `across()` with a literal `.names` and `.unpack = TRUE`.
- `known_across_function_names()` treating a backslash lambda as unknown:
  Arrow or dtplyr `across(c(x, w), \(v) sum(v), .names = "{.fn}_{.col}")`.
- `sql_single_share_array()`'s PostgreSQL `cardinality(a) = 1` changed to `>=
  1`: needs the PostgreSQL arm of the share-cardinality test on a server.

The others were equivalent mutants (ADR 0017 leaves the Grand total occurrence
unspecified), redundant passes, or sites reached only for a backend kind no
supported input produces. The remaining survivors in
`rewrite_summary_selections()` were run against the 32-file selection set
only.

Other lanes reported: no test for the equivalence of `distinct_cube_sets()` and
`distinct_resolved_product()` with a full expansion and first-occurrence
dedupe, which held in 1,440 generated plans; no live native-path test of a
window-ordered input under `.sort` with a typed-missing label; no test of
`collect(cte = )` on a SQLite Margin result; and no test admitting or refusing
a stored view over a table function.

## Covered without a finding

Counts are the investigators' reports.

- Generated cases: about 2,800 summaries and 2,800 expansions on each of data
  frames, tibbles, data.tables and dtplyr, and about 650 of each nesting verb;
  about 1,050 summaries and expansions on DuckDB and 1,090 on SQLite with 650
  inspections each; about 860 of each on Arrow; 250 of each verb over derived
  lazy queries; 1,440 duplicate-policy plan expansions; 90 plans of five and
  six dimensions with permuted `grouping_id()`; 70 observed-collision cases
  against each backend's native `as.character()`; 48 zero-row type cases
  against ADR 0033.
- Recent fixes: the neighbourhoods of #1599, #1600, #1601, #1602, #1603,
  #1605, #1608, #1609, #1611, #1612, #1618, #1619, #1620 and #1624, among them
  1,440 SQLite typed cases, 585 special-name share cases and 96 Grouping helper
  expressions against hand-computed bits.
- PostgreSQL: eighteen column types as typed-missing and as labelled
  dimensions against native dbplyr; `grouping_id()` and both shares against
  hand values; the share dialect probe inside a healthy and inside an aborted
  transaction, with its savepoint rows; `compute()` of an 800,001-row sorted
  result.
- Types: 21 classes as local dimensions, payloads, summary inputs and outputs;
  sf summaries with `st_union()`; integer64, hms, blob and tz across summaries
  and shares; eighteen Arrow physical types.
- Contract reading: DuckDB `ENUM` reconstruction, factor `NA` levels, label
  argument shapes, twenty static-binding shapes, SQLite direct `compute()`
  over six configurations.
- Simulated dialects: sixteen simulators over the specification matrix; 645
  parsed branches with Grouping identifiers equal to hand-computed bits; 256
  label and identifier quoting cases.
- Composition: grouped and row-wise inputs on local, dbplyr, dtplyr and Arrow
  against native verbs on the same grouped input; joins between Margin results
  with `na_matches` aligned; more than twenty chains of Margin verbs.
- Mutants: beside the 75 mutants, 103 local and 46 lazy selection expressions
  against native dplyr at each grain, and 180 generated share cases against a
  hand oracle on four backends.

## Not covered

No MySQL, MariaDB, SQL Server, Oracle, Snowflake or Redshift server was run,
and no DuckDB other than 1.5.5. The DuckDB json extension could not be loaded,
so no JSON reader or replacement scan was executed.
PostgreSQL plans above 4,096 grouping sets, ICU and non-deterministic
collations, range and composite types, and savepoints under a timeout or
interrupt were not tried. DuckDB `TIMESTAMPTZ` could not be run without the
ICU extension. tsibble and `labelled` were not installed. Arrow
`RecordBatchReader` inputs, datasets above 300 generated rows, the roughly 200
static scope functions of `R/share.R` as mutation targets, `backend-metadata.R`'s
dtplyr admission and Arrow proxies, and the *Left for a decision* items of
`margin-verbs-record-fourth-pass-hunt-2026-11-12.md` were not examined. For
the join input, `full_join()` was run by the refuter only. The second-round
refuters had no PostgreSQL connection, and each of them checked several
candidates in one pass.
