
<!-- README.md is generated from README.Rmd. Please edit that file -->

# marginplyr

<!-- badges: start -->

<a href = "https://CRAN.R-project.org/package=marginplyr" target = "_blank"><img src="https://www.r-pkg.org/badges/version/marginplyr"></a>
[![R-CMD-check](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml)
[![altdoc](https://github.com/sayuks/marginplyr/actions/workflows/altdoc.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/altdoc.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/marginplyr/graph/badge.svg)](https://app.codecov.io/gh/sayuks/marginplyr)
[![lint.yaml](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml)
<a href = "https://sayuks.github.io/marginplyr/" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/marginplyr"></a>
<!-- badges: end -->

`marginplyr` brings SQL-style `GROUPING SETS`, `ROLLUP`, and `CUBE` to
`dplyr`. The same grouping specification works with local data frames
and lazy database tables, with `"Total"` as the default display label.

## Installation

<!-- You can install `marginplyr` from CRAN: -->

<!-- ``` r -->

<!-- install.packages("marginplyr") -->

<!-- ``` -->

You can install the development version of `marginplyr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sayuks/marginplyr")
```

## From a monthly report to a database query

``` r
suppressPackageStartupMessages(library(marginplyr))
```

Imagine an analyst preparing a management report for a retail chain.
Each row of `retail_sales` records sales for one product, channel, and
store combination. Online-direct sales have no physical store, so
`store` is genuinely missing in some source rows.

``` r
dplyr::slice_head(retail_sales, n = 6)
#>   year month region         store    product channel units revenue
#> 1 2025   Jan   East        Boston     Laptop   Store     3    3600
#> 2 2025   Jan   East      New York    Monitor   Store     8    2400
#> 3 2025   Jan   East          <NA> Headphones  Online    15    1500
#> 4 2025   Jan   West San Francisco     Laptop   Store     4    4800
#> 5 2025   Jan   West       Seattle    Monitor  Online     6    1800
#> 6 2025   Jan   West       Seattle Headphones   Store    11    1100
```

### Store, region, and company totals

The first request is one independent report per month. `year` and
`month` belong to every result level, while `rollup(region, store)`
creates store detail, region subtotals, and a company total.

``` r
monthly_report <- summarize_with_margins(
  retail_sales,
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
#> 3  2026 Jan   East   Total            37   11200
#> 4  2026 Jan   East   <NA>             22    2200
#> 5  2026 Jan   Total  Total            70   23300
#> 6  2026 Jan   West   San Francisco     6    7200
#> 7  2026 Jan   West   Seattle          27    4900
#> 8  2026 Jan   West   Total            33   12100
```

`"Total"` means that a dimension was aggregated away. A source `NA` is
left as `NA`; it still represents an online-direct record with no
physical store.

Moving the period columns into the rollup changes the report hierarchy:

``` r
all_levels <- summarize_with_margins(
  retail_sales,
  revenue = sum(revenue),
  level = grouping_id(year, month, region, store),
  .grouping = rollup(year, month, region, store)
)

all_levels |>
  dplyr::filter(month == "Total")
#> # A tibble: 3 × 6
#>   year  month region store revenue level
#>   <chr> <chr> <chr>  <chr>   <dbl> <int>
#> 1 2025  Total Total  Total   33900     7
#> 2 2026  Total Total  Total   53500     7
#> 3 Total Total Total  Total   87400    15
```

This rollup includes store detail, region-month subtotals, company-month
totals, company-year totals, and the all-period total.

### Ask for exactly the totals you need

Finance does not always want a hierarchy. `grouping_sets()` selects
arbitrary views, and an empty `grouping_set()` requests the grand total.

``` r
summarize_with_margins(
  retail_sales,
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
#>  1 2025  Feb   Total  Total        18700
#>  2 2025  Jan   Total  Total        15200
#>  3 2026  Feb   Total  Total        30200
#>  4 2026  Jan   Total  Total        23300
#>  5 Total Total East   Headphones    8000
#>  6 Total Total East   Laptop       22800
#>  7 Total Total East   Monitor      11100
#>  8 Total Total Total  Total        87400
#>  9 Total Total West   Headphones    5900
#> 10 Total Total West   Laptop       27600
#> 11 Total Total West   Monitor      12000
```

Merchandising can instead explore every combination of product and sales
channel with `cube()`:

``` r
retail_sales |>
  dplyr::filter(year == 2026L, month == "Jan") |>
  summarize_with_margins(
    revenue = sum(revenue),
    .grouping = cube(product, channel)
  )
#> # A tibble: 10 × 3
#>    product    channel revenue
#>    <chr>      <chr>     <dbl>
#>  1 Headphones Online     2200
#>  2 Headphones Store      1600
#>  3 Headphones Total      3800
#>  4 Laptop     Store     13200
#>  5 Laptop     Total     13200
#>  6 Monitor    Online     6300
#>  7 Monitor    Total      6300
#>  8 Total      Online     8500
#>  9 Total      Store     14800
#> 10 Total      Total     23300
```

### Tell source missing values from totals

SQL uses `GROUPING()` because both a source missing value and an
aggregated dimension would otherwise appear as `NULL`. The marginplyr
helpers expose the same distinction:

``` r
retail_sales |>
  dplyr::filter(
    year == 2026L,
    month == "Jan",
    region == "East"
  ) |>
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
#> 3 East   Total      11200              1     1
#> 4 East   <NA>        2200              0     0
#> 5 Total  Total      11200              1     3
```

`store_is_total` is `0` for the source `NA` row and `1` for a subtotal
where the store dimension was removed.

### Backend support

marginplyr separates native grouping-set support from its portable
`UNION ALL` fallback:

| Execution path | Backends | Automated verification |
|----|----|----|
| Native `GROUP BY GROUPING SETS` | DuckDB, PostgreSQL | Live DuckDB execution; PostgreSQL SQL simulation |
| Portable `UNION ALL` | Access, SAP HANA, Hive, Impala, MariaDB, Microsoft SQL Server, MySQL, Oracle, Amazon Redshift, Snowflake, Spark SQL, SQLite, Teradata, and generic DBI/ODBC connections | SQL generation with dbplyr simulators |

Arrow and dtplyr are also tested lazy backends, but they are not SQL
database connections. Simulated coverage verifies generated SQL; it does
not claim live execution against every listed database server.

### Run the same report in DuckDB

Local data frames and lazy database tables use the same interface.
DuckDB and PostgreSQL translate the request to one native
`GROUP BY GROUPING SETS` query. Install the optional backend packages
before running this example:

``` r
install.packages(c("DBI", "duckdb"))
```

``` r
con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))

sales_db <- dplyr::copy_to(
  con,
  retail_sales,
  name = "retail_sales",
  temporary = TRUE,
  overwrite = TRUE
)

query <- sales_db |>
  dplyr::filter(year == 2026L, month == "Jan") |>
  summarize_with_margins(
    revenue = sum(revenue, na.rm = TRUE),
    level = grouping_id(region, store),
    .grouping = rollup(region, store),
    .sort = TRUE
  )

dplyr::show_query(query)
#> <SQL>
#> SELECT
#>   CASE WHEN ("..marginplyr_grouping_1" = 1) THEN 'Total' WHEN NOT ("..marginplyr_grouping_1" = 1) THEN (TRY_CAST(region AS TEXT)) END AS region,
#>   CASE WHEN ("..marginplyr_grouping_2" = 1) THEN 'Total' WHEN NOT ("..marginplyr_grouping_2" = 1) THEN (TRY_CAST(store AS TEXT)) END AS store,
#>   revenue,
#>   "level"
#> FROM (
#>   SELECT
#>     region,
#>     store,
#>     SUM(revenue) AS revenue,
#>     GROUPING(region) * 2.0 + GROUPING(store) AS "level",
#>     GROUPING(region) AS "..marginplyr_grouping_1",
#>     GROUPING(store) AS "..marginplyr_grouping_2"
#>   FROM retail_sales
#>   WHERE ("year" = 2026) AND ("month" = 'Jan')
#>   GROUP BY GROUPING SETS ((region, store), (region), ())
#> ) AS q01
#> ORDER BY region, store
dplyr::collect(query)
#> # A tibble: 8 × 4
#>   region store         revenue level
#>   <chr>  <chr>           <dbl> <dbl>
#> 1 East   Boston           6000     0
#> 2 East   New York         3000     0
#> 3 East   Total           11200     1
#> 4 East   <NA>             2200     0
#> 5 Total  Total           23300     3
#> 6 West   San Francisco    7200     0
#> 7 West   Seattle          4900     0
#> 8 West   Total           12100     1

DBI::dbDisconnect(con)
```

Backends without confirmed native support use a portable `UNION ALL`
translation with the same grouping semantics.

### Keep the rows behind each total

`union_all_with_margins()` expands source rows across grouping levels.
`nest_by_with_margins()` goes one step further and keeps a row-wise
detail table behind every visible group:

``` r
retail_sales |>
  dplyr::filter(year == 2026L, month == "Jan") |>
  nest_by_with_margins(
    .grouping = rollup(region, store)
  ) |>
  dplyr::mutate(
    records = nrow(data),
    revenue = sum(data$revenue)
  ) |>
  dplyr::select(-data)
#> # A tibble: 8 × 4
#> # Rowwise:  region, store
#>   region store         records revenue
#>   <chr>  <chr>           <int>   <dbl>
#> 1 East   Boston              1    6000
#> 2 East   New York            1    3000
#> 3 East   Total               3   11200
#> 4 East   <NA>                1    2200
#> 5 Total  Total               6   23300
#> 6 West   San Francisco       1    7200
#> 7 West   Seattle             2    4900
#> 8 West   Total               3   12100
```

The function reference contains executable examples for composite
dimensions, tidy-select expressions, duplicate grouping sets, Cartesian
products, simulated SQL dialects, and dtplyr.

The [Get started
guide](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
develops the complete story, including the difference between the union
made by `grouping_sets()` and the Cartesian product made by
`grouping_spec()`, plus a small
[`quartabs`](https://sayuks.github.io/quartabs/) reporting example.

## Code of Conduct

Please note that the marginplyr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
