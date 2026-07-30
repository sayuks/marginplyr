
<!-- README.md is generated from README.Rmd. Please edit that file -->

# marginplyr

### SQL-style grouping sets, rollups, and cubes for dplyr and dbplyr

Create detail rows, subtotals, and grand totals with one dplyr-style
summary. Use the same grouping specification with a local data frame or
a lazy database table.

<!-- badges: start -->

[![R-CMD-check](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml)
[![altdoc](https://github.com/sayuks/marginplyr/actions/workflows/altdoc.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/altdoc.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/marginplyr/graph/badge.svg)](https://app.codecov.io/gh/sayuks/marginplyr)
[![lint.yaml](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml)
<a href = "https://sayuks.github.io/marginplyr/" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/marginplyr"></a>
<!-- badges: end -->

``` r
library(marginplyr)

january_sales <- retail_sales |>
  dplyr::filter(year == 2026L, month == "Jan")

january_sales |>
  summarize_with_margins(
    revenue = sum(revenue),
    .grouping = rollup(region, store)
  )
#> # A tibble: 8 × 3
#>   region store         revenue
#>   <chr>  <chr>           <dbl>
#> 1 East   Boston           6000
#> 2 East   New York         3000
#> 3 East   <NA>             2200
#> 4 West   San Francisco    7200
#> 5 West   Seattle          4900
#> 6 East   Total           11200
#> 7 West   Total           12100
#> 8 Total  Total           23300
```

`rollup(region, store)` asks for three reporting levels:

``` text
(region, store)  store detail
(region)         region subtotal
()               company total
```

The result is one regular data frame—not three reports that you have to
build and combine yourself.

- Works with local data frames and lazy `dbplyr` tables.

- Uses native `GROUP BY GROUPING SETS` on DuckDB and PostgreSQL.

- Uses a portable `UNION ALL` fallback for other SQL dialects.

- Distinguishes a source `NA` from a dimension removed to create a
  total.

- Exposes Grouping set identifiers and SQL-compatible absence masks without
  relying on display labels.

- Calculates each scalar summary’s share of its immediate rollup parent.

- Can expand or nest the source rows behind every subtotal.

New to the idea? Start with [Get
started](https://sayuks.github.io/marginplyr/vignettes/get_started.html).
Need to compare `.id`, Grouping bits, and Grouping identifiers? See
[Grouping
identity](https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html).
Need absent dimension keys? See [Complete absent keys before
margins](https://sayuks.github.io/marginplyr/vignettes/completing_keys.html).
Already working with remote data? See [Database and lazy
backends](https://sayuks.github.io/marginplyr/vignettes/database_backends.html).

## Installation

You can install `marginplyr` from CRAN:

``` r
install.packages("marginplyr")
```

You can install the development version from
[GitHub](https://github.com/sayuks/marginplyr) with:

``` r
install.packages("pak")
pak::pkg_install("sayuks/marginplyr")
```

## Grouping sets in plain language

A normal grouped summary has one grain: for example, one row per store.
A management report often needs several grains together: stores,
regions, and the whole company. SQL calls that collection of grains
*grouping sets*.

marginplyr provides three ways to describe them:

| Helper | Use it when | For `region` and `store` |
|----|----|----|
| `rollup()` | The dimensions form a hierarchy | store detail, region subtotal, company total |
| `grouping_sets()` | You need an exact list of unrelated views | only the combinations you request |
| `cube()` | You need every combination | detail, both one-dimensional subtotals, grand total |

Columns in `.by` stay fixed in every result level. For example, this
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
#> # A tibble: 8 × 6
#>    year month region store         units revenue
#>   <int> <chr> <chr>  <chr>         <int>   <dbl>
#> 1  2026 Jan   East   Boston            5    6000
#> 2  2026 Jan   East   New York         10    3000
#> 3  2026 Jan   East   <NA>             22    2200
#> 4  2026 Jan   West   San Francisco     6    7200
#> 5  2026 Jan   West   Seattle          27    4900
#> 6  2026 Jan   East   Total            37   11200
#> 7  2026 Jan   West   Total            33   12100
#> 8  2026 Jan   Total  Total            70   23300
```

Use `.by` for report partitions that must never become totals, and
`.grouping` for dimensions that may be aggregated away.

## Ask for the exact views you need

`grouping_sets()` combines an explicit list of grouping levels. An empty
`grouping_set()` requests the grand total:

``` r
retail_sales |>
  summarize_with_margins(
    revenue = sum(revenue),
    .grouping = grouping_sets(
      grouping_set(year, month),
      grouping_set(region, product),
      grouping_set()
    )
  )
#> # A tibble: 11 × 5
#>    year  month region product    revenue
#>    <chr> <chr> <chr>  <chr>        <dbl>
#>  1 2025  Jan   Total  Total        15200
#>  2 2025  Feb   Total  Total        18700
#>  3 2026  Jan   Total  Total        23300
#>  4 2026  Feb   Total  Total        30200
#>  5 Total Total East   Laptop       22800
#>  6 Total Total East   Monitor      11100
#>  7 Total Total East   Headphones    8000
#>  8 Total Total West   Laptop       27600
#>  9 Total Total West   Monitor      12000
#> 10 Total Total West   Headphones    5900
#> 11 Total Total Total  Total        87400
```

`cube()` creates every subset of its dimensions:

``` r
january_sales |>
  summarize_with_margins(
    revenue = sum(revenue),
    .grouping = cube(product, channel)
  )
#> # A tibble: 10 × 3
#>    product    channel revenue
#>    <chr>      <chr>     <dbl>
#>  1 Laptop     Store     13200
#>  2 Monitor    Online     6300
#>  3 Headphones Online     2200
#>  4 Headphones Store      1600
#>  5 Laptop     Total     13200
#>  6 Monitor    Total      6300
#>  7 Headphones Total      3800
#>  8 Total      Store     14800
#>  9 Total      Online     8500
#> 10 Total      Total     23300
```

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
#> # A tibble: 5 × 5
#>   region store    revenue store_is_total level
#>   <chr>  <chr>      <dbl>          <int> <int>
#> 1 East   Boston      6000              0     0
#> 2 East   New York    3000              0     0
#> 3 East   <NA>        2200              0     0
#> 4 East   Total      11200              1     1
#> 5 Total  Total      11200              1     3
```

`grouping_bit(store)` is `0` for the source `NA` row and `1` when the
rollup removed `store`. This is the role of the SQL `GROUPING()`
function; replacing every missing value with `"Total"` cannot preserve
that distinction.

## Use the same report with a database

The grouping plan becomes part of the lazy query. `show_query()`
inspects the SQL without collecting the result:

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

DuckDB and PostgreSQL use native grouping sets. Backends without
confirmed native support use a `UNION ALL` translation with the same
grouping plan. Simulation verifies generated SQL but does not claim live
execution against every database server. The [database
guide](https://sayuks.github.io/marginplyr/vignettes/database_backends.html)
shows native SQL, fallback SQL, live DuckDB execution, `collect()`, and
the verification status of each backend.

## Keep the rows behind each total

SQL-style aggregation is only the start. `nest_with_margins()` keeps the
source records for every detail group, subtotal, and total in a list
column:

``` r
nested_sections <- january_sales |>
  nest_with_margins(
    .grouping = rollup(region, store)
  )

# Convert to a tibble so nested data frames print compactly.
nested_sections |>
  dplyr::as_tibble() |>
  head()
#> # A tibble: 6 × 3
#>   region store         data
#>   <chr>  <chr>         <list>
#> 1 East   Boston        <tibble [1 × 6]>
#> 2 East   New York      <tibble [1 × 6]>
#> 3 East   <NA>          <tibble [1 × 6]>
#> 4 West   San Francisco <tibble [1 × 6]>
#> 5 West   Seattle       <tibble [2 × 6]>
#> 6 East   Total         <tibble [3 × 6]>

nested_sections$data[[1]]
#> # A tibble: 1 × 6
#>    year month product channel units revenue
#>   <int> <chr> <chr>   <chr>   <int>   <dbl>
#> 1  2026 Jan   Laptop  Store       5    6000
```

Use `.keep = TRUE` to retain the original grouping columns inside each
nested table. Use `nest_by_with_margins()` when the next calculation
should run row-wise against each nested report section, or
`expand_with_margins()` when downstream work needs expanded rows rather
than an immediate summary.

These are margin-aware counterparts, not drop-in replacements for
`tidyr::nest()` and `dplyr::nest_by()`. Their precise grouping, `.keep`,
empty-input, and lazy-backend contracts are documented in the function
reference and Get Started guide.

## Choose the tool for the job

These approaches solve related problems with different interfaces:

| Approach | Best fit | Result and execution model |
|----|----|----|
| Repeated `dplyr::summarize()` plus row binding | A small, one-off set of totals where explicit branches are clearest | You author and combine each grouping level |
| [`data.table`](https://rdatatable.gitlab.io/data.table/reference/groupingsets.html) grouping sets | A local data.table workflow | `rollup()`, `cube()`, and `groupingsets()` return one data.table |
| [`rollup`](https://cran.r-project.org/package=rollup) | Its dplyr-oriented grouped-data-frame-list workflow fits the analysis | Grouping levels are represented as a list before their summaries are combined |
| marginplyr | The same grouping plan should work locally and on a lazy source | One data frame or lazy query, with native SQL or a portable fallback |

marginplyr is most useful when a report has multiple grains, when the
data should stay in a database until `collect()`, or when subtotal
identity and the records behind each total matter. For one simple local
subtotal, explicit dplyr code may be easier.

## Backend verification

| Verification status | Backends |
|----|----|
| Live native execution tested | DuckDB |
| Native SQL generation tested | PostgreSQL |
| Fallback SQL generation tested | Access, SAP HANA, Hive, Impala, MariaDB, Microsoft SQL Server, MySQL, Oracle, Amazon Redshift, Snowflake, Spark SQL, SQLite, Teradata, and generic DBI/ODBC connections |
| Non-SQL lazy backend tested | Arrow, dtplyr |

The function reference contains executable examples for composite
dimensions, tidy-select expressions, duplicate grouping sets, Cartesian
products, computed grouping columns, simulated SQL dialects, DuckDB, and
dtplyr.

## Code of Conduct

Please note that the marginplyr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
