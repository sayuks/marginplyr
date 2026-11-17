

<!-- README.md is generated from README.qmd. Please edit that file -->

# marginplyr

### SQL-style grouping sets, rollups, and cubes for dplyr and dbplyr

Create detail rows, subtotals, and grand totals with one dplyr-style
summary. Use the same Grouping specification with a local data frame or
a lazy database table.

<!-- badges: start -->

[![R-CMD-check](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml)
[![altdoc](https://github.com/sayuks/marginplyr/actions/workflows/altdoc.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/altdoc.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/marginplyr/graph/badge.svg)](https://app.codecov.io/gh/sayuks/marginplyr)
[![lint.yaml](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml)
<!-- badges: end -->

## Installation

`marginplyr` is not on CRAN yet. Install it from
[GitHub](https://github.com/sayuks/marginplyr) with:

``` r
install.packages("pak")
pak::pkg_install("sayuks/marginplyr")
```

## A first report

The examples use `retail_sales`, a small synthetic data set of monthly
sales that ships with the package. This call reports January 2026
revenue by store, by region, and for the whole company:

``` r
library(marginplyr)

january_sales <- retail_sales |>
  dplyr::filter(year == 2026L, month == "Jan")

january_sales |>
  summarize_with_margins(
    revenue = sum(revenue),
    .grouping = rollup(region, store)
  )
#>   region         store revenue
#> 1   East        Boston    6000
#> 2   East      New York    3000
#> 3   East          <NA>    2200
#> 4   West San Francisco    7200
#> 5   West       Seattle    4900
#> 6   East         Total   11200
#> 7   West         Total   12100
#> 8  Total         Total   23300
```

`rollup(region, store)` asks for three *grouping sets*, the SQL name for
the columns one level of a report is grouped by:

``` text
(region, store)  store detail
(region)         region subtotal
()               company total
```

The result is one regular data frame—not three reports that you have to
build and combine yourself. Its subtotal and total rows are the report’s
*margins*: in each, a column the grouping set removed shows the Margin
label, `"Total"` by default. The `<NA>` store is real data, as *A total
is not the same thing as `NA`* below explains.

Row order is unspecified: the same call on the same data in a different
order can arrange the result differently. `.sort` asks for a Margin
order, which puts each subtotal with the rows it summarizes, and
`arrange()` gives any other presentation order. [Put each subtotal with
the rows it
summarizes](https://sayuks.github.io/marginplyr/vignettes/recipes.html#put-each-subtotal-with-the-rows-it-summarizes)
shows `.sort` at work and what it does not promise.

- Works with local data frames, including a plain `data.table`, and with
  lazy `dbplyr` tables, `dtplyr` steps, and Arrow tables and datasets.

- Runs most summaries on DuckDB and PostgreSQL as native
  `GROUP BY GROUPING SETS`.

- Uses a portable `UNION ALL` fallback for other SQL dialects and for
  every row expansion.

- Distinguishes a source `NA` from a dimension removed to create a
  total.

- Flags totals with `grouping_bit()` and `grouping_id()`, the
  counterparts of SQL `GROUPING()`, so identifying a total never depends
  on its display label.

- Calculates each summary’s share of its rollup parent with
  `share_of_parent()`, or of the grand total with `share_of_total()`.

- Can expand the source rows behind every subtotal, or nest them for
  local and `dtplyr` inputs.

New to the idea? Start with [Get
started](https://sayuks.github.io/marginplyr/vignettes/get_started.html),
which walks through the rest of the package on the same data. Know the
task and want the call? See [Recipes for common reporting
tasks](https://sayuks.github.io/marginplyr/vignettes/recipes.html). Need
to tell a margin from a source row, or one grouping set from another?
See [Grouping
identity](https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html).
Need report rows for keys the data does not contain? See [Complete
absent keys before
margins](https://sayuks.github.io/marginplyr/vignettes/completing_keys.html).
Already working with remote data? See [Database and lazy
backends](https://sayuks.github.io/marginplyr/vignettes/database_backends.html).

The function reference contains executable examples for composite
dimensions, tidy-select expressions, duplicate grouping sets, Cartesian
products, computed grouping columns, simulated SQL dialects, DuckDB, and
dtplyr.

## Grouping sets in plain language

A normal grouped summary has one grouping set: for example, one row per
store. A management report often needs several together: stores,
regions, and the whole company. marginplyr provides four ways to
describe them:

| Helper | Use it when | For `region` and `store` |
|----|----|----|
| `rollup()` | The dimensions form a hierarchy | store detail, region subtotal, company total |
| `grouping_sets()` | You need an exact list of unrelated grouping sets | only the combinations you request |
| `cube()` | You need every combination | detail, both one-dimensional subtotals, grand total |
| `grouping_spec()` | You need every grouping set of one specification crossed with every grouping set of another | with `rollup(year, month)`, its three grouping sets crossed with these three, nine in all |

Columns in `.by` stay fixed in every grouping set. For example, this
creates a separate store-to-company report for each year and month:

``` r
monthly_report <- retail_sales |>
  summarize_with_margins(
    units = sum(units),
    revenue = sum(revenue),
    .by = c(year, month),
    .grouping = rollup(region, store)
  )

monthly_report |>
  dplyr::filter(year == 2026L, month == "Jan")
#>   year month region         store units revenue
#> 1 2026   Jan   East        Boston     5    6000
#> 2 2026   Jan   East      New York    10    3000
#> 3 2026   Jan   East          <NA>    22    2200
#> 4 2026   Jan   West San Francisco     6    7200
#> 5 2026   Jan   West       Seattle    27    4900
#> 6 2026   Jan   East         Total    37   11200
#> 7 2026   Jan   West         Total    33   12100
#> 8 2026   Jan  Total         Total    70   23300
```

Use `.by` for report partitions that must never become totals, and
`.grouping` for dimensions that may be aggregated away. Get started runs
each helper on `retail_sales`.

## A total is not the same thing as `NA`

Some rows in `retail_sales` have `store = NA` because an online-direct
sale does not belong to a physical store. That source value must remain
different from a region subtotal where `store` was deliberately removed.

``` r
january_sales |>
  dplyr::filter(region == "East") |>
  summarize_with_margins(
    revenue = sum(revenue),
    store_is_total = grouping_bit(store),
    level = grouping_id(region, store),
    .grouping = rollup(region, store)
  )
#>   region    store revenue store_is_total level
#> 1   East   Boston    6000              0     0
#> 2   East New York    3000              0     0
#> 3   East     <NA>    2200              0     0
#> 4   East    Total   11200              1     1
#> 5  Total    Total   11200              1     3
```

`grouping_bit(store)` is `0` for the source `NA` row and `1` when the
rollup removed `store`. This is the role of the SQL `GROUPING()`
function; replacing every missing value with `"Total"` cannot preserve
that distinction. `grouping_id(region, store)` packs the same flags into
one number, with `store` as the lowest bit: `0` for store detail, `1`
for a region subtotal, and `3` for the company total.

## Compare each summary with its parent or the whole

`share_of_parent()` divides a preceding named summary by the same
summary in the next less detailed grouping set of a rollup. Name the
source summary first, then take its share:

``` r
january_sales |>
  summarize_with_margins(
    revenue = sum(revenue),
    revenue_share = share_of_parent(revenue),
    .grouping = rollup(region, store),
    .sort = "last"
  )
#>   region         store revenue revenue_share
#> 1   East        Boston    6000     0.5357143
#> 2   East      New York    3000     0.2678571
#> 3   East          <NA>    2200     0.1964286
#> 4   East         Total   11200     0.4806867
#> 5   West San Francisco    7200     0.5950413
#> 6   West       Seattle    4900     0.4049587
#> 7   West         Total   12100     0.5193133
#> 8  Total         Total   23300     1.0000000
```

`share_of_total()` divides by the grand total instead, within each fixed
`.by` group. `share_of_parent()` needs `.grouping` to be one `rollup()`,
and some lazy inputs refuse both helpers — Arrow always, and a SQL
connection whose dialect converts a non-numeric source to a number, or
one that cannot be asked, such as the simulated one below — as the
[`share_of_parent()`
reference](https://sayuks.github.io/marginplyr/man/share_of_parent.html)
states in full.

## Use the same report with a database

The Grouping plan — the ordered list of grouping sets that the Grouping
specification written in `.grouping` expands to, such as the three of
`rollup(region, store)` — becomes part of the lazy query. `show_query()`
inspects the SQL without collecting the result. The call writes
`na.rm = TRUE`, and a local call needs it too for the results to match:
locally a missing measure makes every subtotal and total that includes
it `NA`, while a SQL aggregate always drops missing values, and dbplyr
warns when `na.rm = TRUE` is absent:

``` r
postgres_sales <- dbplyr::tbl_lazy(
  retail_sales,
  con = dbplyr::simulate_postgres()
)

postgres_sales |>
  summarize_with_margins(
    revenue = sum(revenue, na.rm = TRUE),
    .grouping = rollup(region, store)
  ) |>
  dplyr::show_query()
#> <SQL>
#> SELECT
#>   CASE WHEN ("..marginplyr_grouping_1" = 1) THEN 'Total' WHEN NOT ("..marginplyr_grouping_1" = 1) THEN (CAST("region" AS TEXT)) END AS "region",
#>   CASE WHEN ("..marginplyr_grouping_2" = 1) THEN 'Total' WHEN NOT ("..marginplyr_grouping_2" = 1) THEN (CAST("store" AS TEXT)) END AS "store",
#>   "revenue"
#> FROM (
#>   SELECT
#>     "region",
#>     "store",
#>     SUM("revenue") AS "revenue",
#>     GROUPING("region") AS "..marginplyr_grouping_1",
#>     GROUPING("store") AS "..marginplyr_grouping_2"
#>   FROM "df"
#>   GROUP BY GROUPING SETS (("region", "store"), ("region"), ())
#> ) AS "q01"
```

Simulation verifies generated SQL but does not claim live execution
against every database server. The [database
guide](https://sayuks.github.io/marginplyr/vignettes/database_backends.html)
shows native SQL, which queries take the fallback, live DuckDB
execution, and `collect()`.

## Keep the rows behind each total

`expand_with_margins()` returns the source rows behind every grouping
set, and `nest_with_margins()` and `nest_by_with_margins()` keep them in
a list column, one row per detail group, subtotal, and total. They are
margin-aware counterparts, not drop-in replacements, for `tidyr::nest()`
and `dplyr::nest_by()`; Get started shows each one, and the
[`nest_with_margins()`](https://sayuks.github.io/marginplyr/man/nest_with_margins.html)
and
[`nest_by_with_margins()`](https://sayuks.github.io/marginplyr/man/nest_by_with_margins.html)
references state their contracts.

## Choose the tool for the job

These approaches solve related problems with different interfaces:

| Approach | Best fit | Result and execution model |
|----|----|----|
| Repeated `dplyr::summarize()` plus row binding | A small, one-off set of totals where explicit branches are clearest | You author and combine each grouping set |
| [`data.table`](https://rdatatable.gitlab.io/data.table/reference/groupingsets.html) grouping sets | A local data.table workflow | `rollup()`, `cube()`, and `groupingsets()` return one data.table |
| [`rollup`](https://cran.r-project.org/package=rollup) | Its dplyr-oriented grouped-data-frame-list workflow fits the analysis | Grouping sets are represented as a list before their summaries are combined |
| marginplyr | The same Grouping plan should work locally and on a lazy source | One data frame or lazy query, with native SQL or a portable fallback |

marginplyr is most useful when a report needs several grouping sets,
when the data should stay in a database until `collect()`, or when
subtotal identity and the records behind each total matter. For one
simple local subtotal, explicit dplyr code may be easier.

## Backend verification

What is verified on each backend, and how, is stated in one place:
*Database backend coverage* in the [`margin_verbs`
reference](https://sayuks.github.io/marginplyr/man/margin_verbs.html).

## Code of Conduct

Please note that the marginplyr project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.
