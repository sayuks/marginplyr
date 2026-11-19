# Test time under CRAN semantics and a candidate CRAN tier

Investigated: 2026-11-19

## Inputs and environment

The measurements used source SHA
`d0262f8561815d1b03234472449717109dcc48d0`, R 4.6.1 (2026-06-24), testthat
3.3.2 and Darwin 25.6.0 arm64. `tests/testthat/` held 344 `test-*.R` files.
Local runs used `testthat::test_local()` on a `git archive` copy with
`TESTTHAT_PARALLEL=false`.

## What ran under CRAN semantics

No file under `tests/testthat/` referred to `skip_on_cran()`, `on_cran()` or
`NOT_CRAN`, so a check with `NOT_CRAN` unset ran every test and skipped only
`expect_snapshot()` expectations. `DESCRIPTION` carried no
`Config/testthat/parallel` field; the parallel runs in CI and Review-ready
came from `TESTTHAT_PARALLEL` in their own environments, which CRAN does not
set.

## Time of the complete suite

`R-CMD-check.yaml` ran with `NOT_CRAN` unset and four test workers. Its
*Check package* step for that SHA (run of 2026-11-18) took:

| Row | Step time |
|---|---|
| ubuntu-latest (release) | 72 min 35 s |
| ubuntu-latest (oldrel-1) | 99 min 04 s |
| windows-latest (release) | 98 min 16 s |
| ubuntu-latest (devel) | 103 min 18 s |
| macos-latest (release) | 114 min 54 s |

The step's log was not read, so the share of that time spent in tests rather
than in building and the other checks was not measured.

`tools/coverage-shard-durations.tsv` recorded per-file seconds under covr for
338 of the files, from `test-coverage.yaml` run 43974125173. They summed to
23,052 s; 42 files exceeded 120 s and the largest,
`test-share-fresh-environment-exposure.R`, took 2,830.8 s.

Four files were timed locally, each in its own process, to relate that table
to an uninstrumented run:

| File | covr seconds | Local seconds |
|---|---|---|
| `test-across-naming-conditions.R` | 11.6 | 4.3 |
| `test-margin-label.R` | 19.4 | 7.4 |
| `test-grouping-plan.R` | 23.3 | 6.7 |
| `test-share-backends.R` | 48.0 | 19.4 |

The four took 37.8 s against 102.3 covr seconds, a ratio of 0.37. Applied to
the table's total, that estimates a serial complete suite at about 2.4 hours
on this machine. The estimate rests on four files and was not confirmed by a
complete serial run.

The retained formal preflight for
`f6d95bfb357cc164f12e83f73bf5324c54a661ef` (2026-09-21) recorded
`checking tests ... [79s/79s]`, with 35 test files. Those 35 files were all
still present; their covr seconds summed to 757.

## The test log records its own time

That preflight's `marginplyr.Rcheck/tests/testthat.Rout` ended with

```text
> proc.time()
   user  system elapsed 
 73.507   5.577  78.962 
```

which is the trailer `R CMD BATCH` writes, and agreed with the `[79s/79s]`
`00check.log` printed for the same run.

## A candidate tier

Thirty files were run in one serial process: 141.6 s, 12,220 passing
expectations, none failed or skipped. `test-query-policy.R` took 50.3 s of
that and `test-margin-order.R` 10.2 s. The other 28 summed to 80.1 s:

`dependency-floor`, `documentation`, `expand-operation`, `factor`,
`get-col-names`, `grouping-backends`, `grouping-interface`,
`grouping-patterns`, `grouping-plan`, `inspect-grouping`, `margin-display`,
`margin-id`, `margin-label`, `metamorphic-margin-semantics`, `native-classes`,
`nest-operation`, `nest-rows`, `optional-backends`, `package-metadata`,
`postgres-share-probe`, `retail-sales`, `sent-queries`, `sent-query-details`,
`sf-summary`, `share`, `share-backends`, `summarize-operation`, `utils`.

Every one of the 16 names `NAMESPACE` exported was called in at least four of
those 28 files, and each of the nine packages `optional_backends()` returned
was named by a `skip_if_suggest_absent()` or `suggest_available()` call in at
least one. `test-postgres-share-probe.R` passed its 53 expectations with no
PostgreSQL server running.

`test_local()` sets `NOT_CRAN=true`, so these runs executed the snapshot
expectations a CRAN check skips.

## The tier under `R CMD check`

Commit `37b5f3de4ba3baa203a4762f99c1aace9c093f5f` registered those 28 files
in `tests/testthat.R`. `R CMD build --no-build-vignettes --no-manual` on a
`git archive` copy of it produced a 1,297,493-byte tarball, and
`R CMD check --no-manual --ignore-vignettes --no-build-vignettes` on that
tarball, with `NOT_CRAN`, `MARGINPLYR_COMPLETE_SUITE` and `TESTTHAT_PARALLEL`
unset and `_R_CHECK_FORCE_SUGGESTS_=true`, ended `Status: OK`. Its
`testthat.Rout` reported `[ FAIL 0 | WARN 0 | SKIP 9 | PASS 10557 ]`, all nine
skips `On CRAN`, and an elapsed time of 77.041 s.
`verify-cran-test-budget.R` read that check directory as 77 s.

With `MARGINPLYR_HIDE_SUGGESTS` naming all nine members of
`optional_backends()`, the same 28 files ran through `test_local()` in 39.5 s:
5,915 passing expectations, no failure, and 290 skips whose reasons named
every one of the nine packages as not installed.

`tools/local-cran-preflight.R` was run twice from the agent sandbox without
`--output` and stopped at `R CMD build` both times: Quarto's Deno runtime
reported `unable to open database file` from `openKv` while rendering each
vignette. Setting `XDG_CACHE_HOME`, `XDG_DATA_HOME`, `XDG_CONFIG_HOME`,
`XDG_STATE_HOME` and `DENO_DIR` to writable directories did not change that.
The preflight's own check step was therefore not exercised on a real check.

## Size of the shipped sources

`tests/` was 5.0 MB uncompressed.

## What the existing gates read

- `verify-depends-only.R` required a `{<package>} is not installed` line in the
  test log for each member of `optional_backends()`.
- `verify-backend.R` and `verify-suite-coverage.R` failed on any skip whose
  reason was not such a line.
- `testthat::test_local()`, `test_dir()` and `test_file()` did not evaluate
  `tests/testthat.R`; `R CMD check` and covr did.
- `coverage_check_shard()` in `tools/coverage-check-lib.R` replaced the
  `filter` argument of the one top-level `test_check()` call in a copy of that
  file.
