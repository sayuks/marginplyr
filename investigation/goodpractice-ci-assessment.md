# goodpractice CI assessment

Investigated: 2026-09-08

## Question

Would a goodpractice CI gate add useful coverage to marginplyr after Jarl,
lintr, testthat, coverage, R CMD check, the release matrix, checktor, spelling,
documentation regeneration, and repository-specific structural verifiers?

The repository snapshot inspected was `a5a628d`. No goodpractice workflow or
dependency was added during this investigation.

## Primary sources

The assessment used goodpractice 1.2.0 from its [CRAN source
tarball](https://cran.r-project.org/src/contrib/goodpractice_1.2.0.tar.gz), the
package's [official overview](https://docs.ropensci.org/goodpractice/articles/goodpractice.html),
and the tagged source for [`gp()`](https://github.com/ropensci-review-tools/goodpractice/blob/v1.2.0/R/gp.R),
the [result API](https://github.com/ropensci-review-tools/goodpractice/blob/v1.2.0/R/api.R),
the [lintr preparation](https://github.com/ropensci-review-tools/goodpractice/blob/v1.2.0/R/prep_lintr.R),
and the [code-structure checks](https://github.com/ropensci-review-tools/goodpractice/blob/v1.2.0/R/chk_code_structure.R).
The official reference documents both [group selection](https://docs.ropensci.org/goodpractice/reference/checks_by_group.html)
and the [`gp()` interface](https://docs.ropensci.org/goodpractice/reference/gp.html).

## What the default run contains

In 1.2.0, `gp()` defaulted to `default_checks()`. On the installed package this
was 307 checks in 15 groups. The dominant groups were 213 checks backed by one
R CMD check and 58 checks backed by an explicit goodpractice-owned lintr set.
Other preparations ran covr, cyclocomp, roxygen2, spelling, urlchecker, and
source-tree analyses. The official overview describes the same preparation
model: expensive preparations run once and supply multiple checks.

The 1.2.0 DESCRIPTION required R >= 4.3.0 and imported covr, curl, cyclocomp,
desc, jsonlite, lintr, rcmdcheck, roxygen2, spelling, treesitter,
treesitter.r, urlchecker, and supporting packages. Selecting a small check set
reduces execution, but does not reduce that declared installation closure.

`gp()` returns a `goodPractice` object; it does not define CI failure by process
status. `results()`, `failed_checks()`, `failed_positions()`, and
`export_json()` expose results, so a blocking job would need a wrapper that
decides which `FALSE` and `NA` results fail. This follows from the tagged
[`gp()` implementation](https://github.com/ropensci-review-tools/goodpractice/blob/v1.2.0/R/gp.R)
and [result API](https://github.com/ropensci-review-tools/goodpractice/blob/v1.2.0/R/api.R).

## marginplyr measurement

goodpractice 1.2.0 was installed into an isolated temporary library and run as
`gp(path = ".")` under R 4.6.1. The run performed the package tests for covr,
then built and checked the package again through rcmdcheck. Its principal
preparation times were:

| Preparation | Elapsed |
|---|---:|
| covr | 1m 29.9s |
| cyclocomp | 23.2s |
| lintr | 23.4s |
| rcmdcheck | 2m 4.8s |
| urlchecker | 7.9s |

The result contained 275 passes, 28 failures, and four `NA` results. Some
checks belong to more than one group, so the group rows below are memberships
and do not sum to the unique-check total:

| Group | Pass | Fail | `NA` |
|---|---:|---:|---:|
| rcmdcheck | 213 | 0 | 0 |
| lintr | 34 | 24 | 0 |
| description | 13 | 0 | 0 |
| code structure | 3 | 1 | 0 |
| namespace | 4 | 1 | 0 |
| covr | 0 | 1 | 0 |
| cyclocomp | 0 | 1 | 0 |
| roxygen2 | 0 | 0 | 4 |
| Rd | 2 | 0 | 0 |
| revdep | 1 | 0 | 0 |
| package structure | 3 | 0 | 0 |
| spelling | 1 | 0 | 0 |
| urlchecker | 2 | 0 | 0 |
| vignette | 2 | 0 | 0 |

The roxygen2 preparation raised `object 'retail_sales' not found`. The four
dependent checks became `NA`, `failed_checks()` did not include them, and the R
process exited successfully. A naive `Rscript -e 'goodpractice::gp(".")'` CI
step would therefore pass despite an incomplete preparation.

## Overlap

| goodpractice area | Existing marginplyr authority | Assessment |
|---|---|---|
| R CMD check (213 checks) | Working-tree matrix plus release/devel/oldrel source-tarball matrix | Direct duplication, with weaker environment coverage. |
| covr | `test-coverage.yaml` and Codecov | Direct duplication; the measured 99.1% still failed because any uncovered line fails the goodpractice check. |
| lintr (58 checks) | Package-aware `lintr::lint_package()` plus Jarl 0.6.0 | Mostly overlapping engine and concerns, but with a much broader opinionated rule set. |
| DESCRIPTION and URLs | R CMD check plus the manual checktor release gate | Mostly duplicate; all measured checks passed. |
| spelling | `tests/spelling.R` under R CMD check | Duplicate; measured check passed. |
| roxygen2 and Rd | `document.yaml`, R CMD check, and documentation tests | Mostly duplicate; the measured preparation incompatibility produced four indeterminate results. |
| revdep | Release planning and CRAN/revdep tooling | The check only reported whether reverse dependencies existed; it did not run them. |
| package/vignette structure | R CMD check and repository-specific documentation verifiers | Mostly duplicate; all measured checks passed. |

The extra goodpractice lintr set found 649 source positions across 24 failed
checks. Many were deliberate fixtures or project-specific idioms rather than
new defects: missing arguments and unreachable statements are syntax-analysis
fixtures already annotated for Jarl; `attach()` and `sapply()` occur in tests
that verify those constructs; repeated named `i` elements form cli condition
bullets; and `dplyr::if_else(..., 1L, 0L)` was reported as a redundant logical
`ifelse`. The 244 `expect_identical` suggestions alone would create broad test
churn without increasing tested behaviour.

## Checks with genuinely distinct potential

The source-tree checks for a print method returning invisibly, `on.exit()`
using `add = TRUE`, and duplicate function bodies were distinct and all passed.
They are narrow correctness/maintenance checks, but no measured finding showed
that a new CI dependency was needed to protect this repository.

Function length, unused internal functions, and cyclomatic complexity were also
distinct. They were not suitable as blocking gates in this snapshot:

- the fixed 50-line rule reported 40 functions;
- the complexity >15 rule reported 13 functions, including static-expression
  analyzers whose branching is their domain;
- the unused-function rule reported nine names, but most are reached by
  generated calls, cli interpolation, or private-call strings that its static
  scan did not resolve.

These metrics can still identify refactoring candidates. They express design
preferences rather than a release invariant, and would require a large initial
baseline or repository-wide refactor before becoming blocking.

## Recommendation

Do not add the default goodpractice run to PR CI or the CRAN release gate.
For marginplyr it would rerun the most expensive existing checks, add a large
dependency closure, produce hundreds of known-noise positions, and still need
a custom wrapper to fail on both failed and indeterminate preparations.

Use goodpractice manually when planning a focused refactor, especially its
complexity and code-structure groups. If one of the three passing structural
checks later proves valuable through a real regression, adopt that invariant
directly or select only that named check; do not introduce the default suite.
Likewise, a useful goodpractice lintr finding should be enabled deliberately in
the existing lintr configuration, where package loading, suppressions, and CI
failure semantics already have one owner.
