---
status: accepted
---

# Run a registered CRAN tier under CRAN semantics

A check that nobody asked for the complete suite runs only the test files
`tests/testthat.R` registers as the CRAN tier. CRAN expects a package's whole
check to finish within minutes, and the complete suite, which a check ran in
full whenever `NOT_CRAN` was unset, had grown to an estimated 2.4 hours in one
process. The [measurements](../../investigation/cran-tier-test-time-2026-11-19.md)
are the evidence for that estimate and for the tier's first membership.

## The switch

`tests/testthat.R` passes `test_check()` a `filter` naming the registered
files unless `NOT_CRAN` or `MARGINPLYR_COMPLETE_SUITE` is true. Registration
is opt-in: a new test file runs in the complete suite only, so a bug hunt that
adds files cannot spend CRAN's time without an edit to that one vector.

A filtered file is never loaded, so it contributes no skip. That is why the
tier is a filter and not a `skip_on_cran()` in each file: `verify-backend.R`
and `verify-suite-coverage.R` refuse a skip that names no withheld backend,
`verify-depends-only.R` reads the skip lines a minimal check prints, and
`cran-comments.md` accounts to CRAN for every skip.

`testthat::test_local()` and `devtools::test()` do not evaluate
`tests/testthat.R`, so they run the complete suite as before.

`r-lib/actions/setup-r` exports `NOT_CRAN=true` into a job that does not set
the variable, so a job meant to run what CRAN runs states `NOT_CRAN: false`,
and `verify-cran-test-budget.R` refuses a check that ran the complete suite
rather than report the tier's budget met.

## Membership

The tier is chosen to reach the public surface, not to be the fastest files:
some registered file calls every export, and some registered file is guarded
on every member of `optional_backends()`. `tools/test-cran-tier.R` asserts
both from the sources, and the second is also what lets `depends-only` go on
proving absence, since `verify-depends-only.R` needs each backend's skip line
from the tier's own run.

## The budget

`.github/scripts/cran-test-budget.R` holds the tier's run to 300 seconds, read
from the `proc.time()` trailer of the check's test log. It is enforced where
the tier runs with every Suggested package installed, as on CRAN — the
*CRAN tier* row of `R-CMD-check.yaml` and the local CRAN preflight — and by
`check-tarball.R` in the release matrix's `depends-only` and `tarball` jobs.

## Who runs which

| Run | Tests |
|---|---|
| CRAN, win-builder, local CRAN preflight | CRAN tier |
| `R-CMD-check.yaml`, *CRAN tier* row | CRAN tier |
| `release-matrix.yaml` `depends-only` and `tarball`, `rhub.yaml` | CRAN tier |
| `R-CMD-check.yaml`, other rows | complete suite under CRAN semantics, by `MARGINPLYR_COMPLETE_SUITE` |
| `release-matrix.yaml` `backend`, coverage, Review-ready `full` | complete suite, by `NOT_CRAN` |
| `release-matrix.yaml` `structure` | complete suite, through `test_local()` |

The other `R-CMD-check.yaml` rows keep the complete suite because they are the
only place every backend's tests run together on macOS, Windows, R-devel and
oldrel. This amends [ADR 0030](0030-require-complete-test-line-coverage.md),
which left every CRAN-emulating job its environment. No workflow job set
`NOT_CRAN` except `backend`, so `setup-r` set it to true in the jobs documented
as emulating CRAN; `R-CMD-check.yaml`, `depends-only` and `tarball` now set it
to false. `rhub.yaml` needs no edit: `r-hub/actions/setup-r` passes
`NOT_CRAN: false` to that action itself.

## Rejected

- **Opt-out, by marking slow files.** The default would stay "runs on CRAN",
  which is the default that produced the growth.
- **The fastest files up to the budget.** Membership would follow timing noise
  and say nothing about what CRAN's run covers.
- **Leaving the complete suite out of the tarball.** The `backend` jobs check
  the built tarball with the complete suite, so it has to ship.
