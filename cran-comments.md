## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Yusuke Sasaki <sayuks.dev@gmail.com>'

New submission
```

This is the expected new-submission NOTE; both runs below report it and
nothing else.

## Test environments

Both runs check one source tarball produced by `R CMD build`, not the
development tree, on R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2, with
Quarto 1.9.38.

* `R CMD check --as-cran`, every Suggested package installed. Installation,
  examples, tests, vignette re-building, and the PDF and HTML manuals all
  pass. Every skipped test is an `expect_snapshot()` diagnostic, which
  testthat skips on CRAN by default.
* `_R_CHECK_DEPENDS_ONLY_=true _R_CHECK_FORCE_SUGGESTS_=false R CMD check
  --as-cran`, Suggested packages withheld. Examples, tests, and vignettes all
  complete. Every skipped test is either one of those same snapshot
  diagnostics or a test whose optional backend is absent, and the testthat
  output names the missing package for each of the latter.
  `_R_CHECK_FORCE_SUGGESTS_=false` is what stops `--as-cran` from treating the
  deliberately withheld packages as a failure.

Neither entry quotes a test count, deliberately. A count changes whenever a
test is added, so one written here is accurate on the day it is measured and
silently wrong afterwards, and a stale count reads exactly like a fresh one.
What these runs are offered as evidence for is the status line above and the
account of the skips, and both of those survive a test being added.

## Optional backends

arrow, data.table, dtplyr, duckdb, and RSQLite are Suggests, and marginplyr
works without them: local data frames need none of them, and each backend adds
one optional lazy path. Every example, test, and vignette section that uses one
of these packages is guarded, so a platform whose binaries are unavailable
still checks cleanly with only the corresponding coverage skipped. The
dependency-only run above is the evidence, because none of those packages is
installed in it.

DBI is a Suggest too, and is the one this argument does not reach: marginplyr
calls it directly, so it is declared, but dbplyr imports it, so it is present
in the dependency-only run and nothing there skips for its absence.

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
