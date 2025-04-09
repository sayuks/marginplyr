
<!-- README.md is generated from README.Rmd. Please edit that file -->

# marginplyr

<!-- badges: start -->

<a href = "https://CRAN.R-project.org/package=marginplyr" target = "_blank"><img src="https://www.r-pkg.org/badges/version/marginplyr"></a>
[![R-CMD-check](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/sayuks/marginplyr/graph/badge.svg)](https://app.codecov.io/gh/sayuks/marginplyr)
[![lint.yaml](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml/badge.svg)](https://github.com/sayuks/marginplyr/actions/workflows/lint.yaml)
<a href = "https://sayuks.github.io/marginplyr/" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/marginplyr"></a>
<!-- badges: end -->

`marginplyr` is an R package that summarize data with margins (totals)
and expand data by treating margins as category levels. Supports
[tidyverse](https://www.tidyverse.org/) workflows for analysis.

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
```

### Summarize with hierarchical margins for `cyl` and `vs`.

``` r
summarize_with_margins(
  mtcars,
  n = dplyr::n(),
  mpg = mean(mpg, na.rm = TRUE),
  .rollup = c(cyl, vs),
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
started](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
for more details.

## Code of Conduct

Please note that the marginplyr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
