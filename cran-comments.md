## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Yusuke Sasaki <sayuks.dev@gmail.com>'

New submission
```

This is the expected new-submission NOTE; the run reports nothing else.

## Test environments

Both runs below check the built source tarball, not the development tree, on
R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.5.2, with Quarto 1.9.38.

* Fully provisioned `R CMD check --as-cran`, every Suggested package
  installed: 0 errors, 0 warnings, 1 note. Installation, examples, tests,
  vignette re-building, and the PDF and HTML manuals all pass. 1487 tests
  pass; the 3 skips are the `expect_snapshot()` diagnostics that testthat
  skips on CRAN by default.
* `_R_CHECK_DEPENDS_ONLY_=true R CMD check`, Suggested packages absent:
  status OK, no notes. Examples, tests, and vignettes all complete. 1083
  tests pass; 66 of the 68 skips are backend tests whose optional package is
  missing, and the other 2 are the same snapshot skips as above.

## Optional backends

arrow, DBI, dtplyr, duckdb, and RSQLite are Suggests, and marginplyr works
without them: local data frames need none of them, and each backend adds one
optional lazy path. Every example, test, and vignette section that uses one of
these packages is guarded, so a platform whose binaries are unavailable still
checks cleanly with only the corresponding coverage skipped. The
dependency-only run above is the evidence, because none of those backends is
installed in it.

## Vignettes and Quarto

The four vignettes are Quarto documents, which is why `SystemRequirements`
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
