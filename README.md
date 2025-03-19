
<!-- README.md is generated from README.Rmd. Please edit that file -->

# withmargins

<!-- badges: start -->
<!-- <a href = "https://cran.r-project.org/web/packages/withmargins/index.html" target = "_blank"><img src="https://www.r-pkg.org/badges/version/withmargins"></a> -->

[![R-CMD-check](https://github.com/sayuks/withmargins/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/withmargins/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/withmargins/graph/badge.svg)](https://app.codecov.io/gh/sayuks/withmargins)
[![lint.yaml](https://github.com/sayuks/withmargins/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/withmargins/actions/workflows/lint.yaml)
<a href = "https://sayuks.github.io/withmargins/" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/withmargins"></a>
<!-- badges: end -->

`withmargins` is an R package that summarize data with margins (totals)
and expand data by treating margins as category levels. Supports
‘tidyverse’ workflows for analysis and visualization.

## Installation

You can install `withmargins` from CRAN:

``` r
install.packages("withmargins")
```

You can install the development version of `withmargins` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sayuks/withmargins")
```

## Example

``` r
library(withmargins)
```

### Make a hierarchical margin for `cyl`, `vs`.

``` r
summarize_with_margins(
  mtcars,
  n = dplyr::n(),
  mpg = mean(mpg, na.rm = TRUE),
  .margins = c(cyl, vs),
)
#>     cyl    vs  n      mpg
#> 1 (all) (all) 32 20.09062
#> 2     4 (all) 11 26.66364
#> 3     4     0  1 26.00000
#> 4     4     1 10 26.73000
#> 5     6 (all)  7 19.74286
#> 6     6     0  3 20.56667
#> 7     6     1  4 19.12500
#> 8     8 (all) 14 15.10000
#> 9     8     0 14 15.10000
```

See [Get
started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
for more details.

## Code of Conduct

Please note that the withmargins project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
