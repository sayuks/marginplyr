## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Yusuke Sasaki <sayuks.dev@gmail.com>'

New submission
```

This is the expected new-submission NOTE, and it is the only note the local run
below reports. The dependency-only run does not perform the incoming checks
and reports no note.

## Test environments

Both runs check a source tarball produced by `R CMD build`, not the
development tree.

* Locally, on R 4.6.2, aarch64-apple-darwin23, macOS Tahoe 26.6.2, with
  Quarto 1.11.9: `R CMD check --as-cran`, every Suggested package installed.
  Installation, examples, tests, vignette re-building, and the PDF and HTML
  manuals all pass, and no test is skipped.
* On GitHub Actions, on `ubuntu-latest` with the current R release:
  `_R_CHECK_DEPENDS_ONLY_=true _R_CHECK_FORCE_SUGGESTS_=false R CMD check
  --as-cran --no-manual`, without the remote incoming checks, against a
  library holding only marginplyr's hard dependencies, testthat, the declared
  `VignetteBuilder`, the package that drives the check, and what those
  require; every other Suggested package is absent. Examples, tests, and
  vignettes all complete. Every skipped test is one whose optional backend is
  absent, and the testthat output names the missing package for each.
  `_R_CHECK_FORCE_SUGGESTS_=false` is what stops the check from treating the
  absent packages as a failure.

In both runs the tests are the subset `tests/testthat.R` registers for CRAN,
which is what runs unless the environment asks for the complete suite, as
`NOT_CRAN=true` does. It calls every exported
function and is held to a time budget, because the complete suite takes hours
in one process; that suite runs in the package's own continuous integration.

Neither entry quotes a test count, deliberately: a count is accurate on the day
it is measured and silently wrong afterwards. What these runs are offered as
evidence for is their status and the account of the skips, and both of those
survive a test being added.

## Optional backends

arrow, data.table, dtplyr, duckdb, RPostgres, RSQLite, and sf are Suggests, and
marginplyr works without them: a plain data frame needs none of them, and each
adds one optional path — a lazy backend for arrow, dtplyr, duckdb, RPostgres,
and RSQLite, and an accepted input class for data.table and sf. bit64, hms, and
S7 are Suggests that only tests use, for the column classes they define.
Examples, tests, and vignette sections that use one of these packages are
guarded, so a platform whose binaries are unavailable still checks cleanly
with only the corresponding coverage skipped. None of those packages is
installed in the dependency-only run above.

blob, DBI, lifecycle, stringr, tibble, tidyr, and withr are Suggests too, and
the dependency-only run says nothing about them: dplyr or dbplyr requires each,
so that run has them installed.

## Vignettes and Quarto

The vignettes are Quarto documents, which is why `SystemRequirements`
declares the Quarto command line tool. `R CMD build` renders them with Quarto,
so the tarball ships the rendered `inst/doc/*.html` files alongside the `.qmd`
sources; no `.html` file is committed to the repository.

Re-building the vignettes during a check needs Quarto on the check machine.
When it is absent, the `quarto` vignette engine does not fail the check: it
reports that the Quarto binary is unavailable and writes a placeholder HTML
file in place of each vignette. Please treat a check run without Quarto as
having no vignette coverage rather than as a passing one.

## Downstream dependencies

There are currently no downstream dependencies because this is a new
submission.
