
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

## Example

``` r
library(marginplyr)
#>
#> Attaching package: 'marginplyr'
#> The following object is masked from 'package:base':
#>
#>     grouping
```

### Hierarchical totals with `rollup()`

``` r
summarize_with_margins(
  mtcars,
  n = dplyr::n(),
  mean_mpg = mean(mpg, na.rm = TRUE),
  level = grouping_id(cyl, vs),
  .grouping = rollup(cyl, vs)
)
#> # A tibble: 9 × 5
#>   cyl   vs        n mean_mpg level
#>   <chr> <chr> <int>    <dbl> <int>
#> 1 4     0         1     26       0
#> 2 4     1        10     26.7     0
#> 3 4     Total    11     26.7     1
#> 4 6     0         3     20.6     0
#> 5 6     1         4     19.1     0
#> 6 6     Total     7     19.7     1
#> 7 8     0        14     15.1     0
#> 8 8     Total    14     15.1     1
#> 9 Total Total    32     20.1     3
```

### Arbitrary grouping sets, including the grand total

``` r
summarize_with_margins(
  mtcars,
  n = dplyr::n(),
  .grouping = grouping_sets(
    grouping_set(cyl, vs),
    grouping_set(cyl, gear),
    grouping_set()
  )
)
#> # A tibble: 14 × 4
#>    cyl   vs    gear      n
#>    <chr> <chr> <chr> <int>
#>  1 4     0     Total     1
#>  2 4     1     Total    10
#>  3 4     Total 3         1
#>  4 4     Total 4         8
#>  5 4     Total 5         2
#>  6 6     0     Total     3
#>  7 6     1     Total     4
#>  8 6     Total 3         2
#>  9 6     Total 4         4
#> 10 6     Total 5         1
#> 11 8     0     Total    14
#> 12 8     Total 3        12
#> 13 8     Total 5         2
#> 14 Total Total Total    32
```

`grouping_spec()` combines grouping families by Cartesian product.
Composite dimensions stay together inside a rollup or cube:

``` r
summarize_with_margins(
  mtcars,
  n = dplyr::n(),
  .grouping = grouping_spec(
    rollup(cyl, vs),
    cube(grouping_set(gear, carb), am)
  )
) |>
  dplyr::slice_head(n = 12)
#> # A tibble: 12 × 6
#>    cyl   vs    gear  carb  am        n
#>    <chr> <chr> <chr> <chr> <chr> <int>
#>  1 4     0     5     2     1         1
#>  2 4     0     5     2     Total     1
#>  3 4     0     Total Total 1         1
#>  4 4     0     Total Total Total     1
#>  5 4     1     3     1     0         1
#>  6 4     1     3     1     Total     1
#>  7 4     1     4     1     1         4
#>  8 4     1     4     1     Total     4
#>  9 4     1     4     2     0         2
#> 10 4     1     4     2     1         2
#> 11 4     1     4     2     Total     4
#> 12 4     1     5     2     1         1
```

### Quarto tabset reports

`nest_by_with_margins()` creates detail, subtotal, and grand-total
groups that can be rendered directly as nested Quarto tabs with
[`quartabs`](https://sayuks.github.io/quartabs/):

``` r
install.packages(c("knitr", "quartabs"))
```

``` r
# In a Quarto R chunk with: #| results: asis
mtcars |>
  nest_by_with_margins(.grouping = rollup(cyl, vs)) |>
  dplyr::mutate(
    report = list(
      data |>
        dplyr::summarize(
          rows = dplyr::n(),
          mean_mpg = mean(mpg)
        ) |>
        knitr::kable()
    )
  ) |>
  dplyr::ungroup() |>
  quartabs::render_tabset(
    tabset_vars = c(cyl, vs),
    output_vars = report
  )
```

The [Get started
guide](https://sayuks.github.io/marginplyr/vignettes/get_started.html#quarto-tabset-reports-with-quartabs)
contains a live, executable tabset.

DuckDB and PostgreSQL use one native `GROUP BY GROUPING SETS` query.
Other lazy backends use a portable `UNION ALL` adapter with the same
result semantics. `grouping()` and `grouping_id()` distinguish source
missing values from rows produced by aggregation.

See [Get
started](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
for more details.

## Code of Conduct

Please note that the marginplyr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
