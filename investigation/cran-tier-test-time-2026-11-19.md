# Test time under CRAN semantics and a candidate CRAN tier

Investigated: 2026-11-19
Revised: 2026-11-24 — `tests/testthat.R` and ADR 0045's *The budget* (#1760)

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

`R-CMD-check.yaml` ran with four test workers. Its *Check package* step for
that SHA (run of 2026-11-18) took:

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

## `NOT_CRAN` in the workflows

At that SHA no job in `R-CMD-check.yaml` or `release-matrix.yaml` set
`NOT_CRAN` except `backend`, which set it to `true`. Every job ran
`r-lib/actions/setup-r@v2`, whose `setup-r/src/installer.ts` at ref `v2`, read
on this date, held

```text
if (!process.env["NOT_CRAN"]) core.exportVariable("NOT_CRAN", "true");
```

so the jobs their comments described as emulating CRAN ran with
`NOT_CRAN=true`, and the step times above are for a complete suite run that
way. Their logs were not read to confirm the exported value.

## Size of the shipped sources

`tar -czf` over `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `NEWS.md`, `README.md`,
`R`, `man`, `tests`, `vignettes`, `inst` and `data` produced 1,351,680 bytes;
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

## The tier on a hosted Windows runner

Pull request #1736 ran `R-CMD-check.yaml`'s `windows-latest (release, CRAN
tier)` row at `69a6285cdc830b05f5ae1588018d2a74c9c2b5f6`, with every Suggested
package installed and one test process. Its check log held
`Running 'testthat.R' [258s]` and `[ FAIL 0 | WARN 0 | SKIP 9 | PASS 10557 ]`,
and `verify-cran-test-budget.R` reported 258 s against the 300 s budget: 3.35
times the 77.041 s the same 28 files took locally.

Commit `b254f96dec88fe9be3e10c1b5fab6ba1b76d8745` then removed `share-backends`,
which had taken 17.6 s of the 30-file local run. The local check described
under *The tier under `R CMD check`*, repeated on that commit, ended
`Status: OK` with `[ FAIL 0 | WARN 0 | SKIP 4 | PASS 9583 ]` and an elapsed
time of 64.823 s. `test-grouping-backends.R`, `test-sent-queries.R` and
`test-metamorphic-margin-semantics.R`, all still registered, held 4, 3 and 2
calls of `share_of_parent()` or `share_of_total()`. The 27 files were not
timed on Windows before that commit was pushed.

## Revisions (2026-11-24)

Issue #1760 took eight files out of the tier and lowered its budget from 300 s
to 100 s; `tests/testthat.R` and ADR 0045's *The budget* hold both. Nothing
above measured the tier on a machine CRAN checks with, and the budget above
was met by a tier that took 416 s there. The measurements behind the change
follow.

### The 28-file tier on Win-Builder

At `967c574ba0ecbb0587a3c6c5b6757ed6ae8a8bd2` the tier registered 28 files:
the 27 above and `suggest-guard-scan`, which commit `3663567e` added.
Win-Builder checked that revision's source tarball twice on R-devel
(2026-11-17 r90926 ucrt), Windows Server 2022 x64 (build 20348). Both checks
ended `Status: 1 NOTE`, and their `00check.log` files held:

| Check started (UTC) | `testthat.R` | R code for possible problems | Vignette rebuild | PDF manual | Examples |
|---|---|---|---|---|---|
| 2026-11-24 01:46:50 | 402 s | 81 s | 79 s | 29 s | 14 s |
| 2026-11-24 03:46:02 | 416 s | 78 s | 77 s | 29 s | 14 s |

The second check's `testthat.Rout` ended
`[ FAIL 0 | WARN 0 | SKIP 4 | PASS 9657 ]` and

```text
   user  system elapsed 
 393.64   22.93  416.09 
```

so the tests spent that time on the CPU. The first check's test log was not
retained. Issue #1760 recorded the whole checks at 692 s and 701 s, the
installs at 26 s and 27 s, and the tarball's SHA-256 as
`e2d453aecba35ab0e34e1ed37e7401cc6424875c70ecd750e61cb06b0bde275e`; the two
logs read here held none of those. By those figures everything but the tests
took 290 s and 285 s.

### The 28-file tier on hosted runners

At the same SHA, `verify-cran-test-budget.R` and `check-tarball.R` reported
these times for `testthat.R`:

| Workflow run | Job | Seconds |
|---|---|---|
| `R-CMD-check.yaml` 45190228623 | `windows-latest (release, CRAN tier)` | 158 |
| `release-matrix.yaml` 45211186226 | `Tarball windows-latest (release)` | 151 |
| | `Tarball macos-latest (release)` | 131 |
| | `Minimal dependencies (depends-only)` | 113 |
| | `Tarball ubuntu-latest (release)` | 99 |
| | `Tarball ubuntu-latest (oldrel-1)` | 99 |
| | `Tarball ubuntu-latest (devel)` | 85 |

Win-Builder's 416 s was 2.75 times the 151 s and 2.63 times the 158 s. The
`depends-only` log held
`[ FAIL 0 | WARN 0 | SKIP 232 | PASS 5816 ]`. `release-matrix.yaml` gave the
`Tarball` jobs the hard dependencies, rcmdcheck, testthat and quarto, and not
the optional backends; the *CRAN tier* row installed every Suggested package.

That row's time for the 28 files varied with the runner the job was given.
Five runs of `R-CMD-check.yaml` on 2026-11-23, each ending
`[ FAIL 0 | WARN 0 | SKIP 4 | PASS 9657 ]`, held:

| Workflow run | SHA | `testthat.R` | R code for possible problems |
|---|---|---|---|
| 45104988435 | `b69e8765` | 146 s | 20 s |
| 45136302536 | `9c2c7a6d` | 157 s | 20 s |
| 45190228623 | `967c574b` | 158 s | 21 s |
| 45188593591 | `9a7b222f` | 222 s | 29 s |
| 45125729002 | `3054adf9` | 259 s | 37 s |

The slowest run took 1.77 times the fastest, and its check of the R code,
which no test file changes, took 1.85 times as long. Win-Builder's 416 s was
between 1.61 and 2.85 times these.

### The tier with and without the six files

The six were `share`, `inspect-grouping`, `margin-label`, `grouping-plan`,
`nest-operation` and `sent-queries`. A copy of `967c574b` carrying #1760's
edits to `tests/` was built with
`R CMD build --no-build-vignettes --no-manual` and installed, and the
extracted tarball's tests were run by `testthat::test_dir()` with
`load_package = "installed"` and the filter `tests/testthat.R` builds, in
one process with `NOT_CRAN` and `MARGINPLYR_COMPLETE_SUITE` unset and
`TESTTHAT_PARALLEL=false`, on R 4.6.2 (2026-10-29), testthat 3.3.2 and
Darwin 25.6.0 arm64. The 28-file rows added the six names back to the filter.

| Files | Optional backends | Seconds | Expectations | Skips |
|---|---|---|---|---|
| 28 | installed | 68.2 | 9,663 | 4 |
| 22 | installed | 33.2, 33.3 | 4,531 | 1 |
| 28 | all nine named in `MARGINPLYR_HIDE_SUGGESTS` | 40.1 | 6,050 | 232 |
| 22 | all nine named in `MARGINPLYR_HIDE_SUGGESTS` | 16.5 | 2,499 | 146 |

No run had a failure or an error. The expectation counts are
`ListReporter`'s and include the skips. Issue #1760 recorded 63.6 s for the
28 files and 31.3 s and 31.5 s for the 22, from earlier runs of the same
kind on that date.

In the 28-file run with the backends installed, the six files took 7.46 s,
6.25 s, 5.89 s, 5.68 s, 5.30 s and 4.05 s in the order named above, 34.63 s
together. `test-grouping-backends.R` took 8.25 s, more than any other file,
and stayed registered at that point: it called 14 of the 16 exports, every one
but `last_sent_queries()` and `summarise_with_margins()`. Across the 22 files
every export was called in at least two, and every member of
`optional_backends()` was named by a `skip_if_suggest_absent()` or
`suggest_available()` call in at least one. `Rscript tools/test-cran-tier.R`
printed `CRAN tier fixtures passed.`

The 22 files were not run on Windows or on Win-Builder before that change was
committed. Scaling the hosted and Win-Builder times above by the local ratios,
33.2 / 68.2 with the backends installed and 16.5 / 40.1 with them hidden,
gave about 77 s and 74 s for the two hosted Windows jobs, about 46 s for
`depends-only`, and about 203 s for Win-Builder's tests in a whole check of
about 490 s. Those figures were estimates, and the 77 s rested on the 158 s
of a fast runner.

### The 22-file tier on hosted runners

The pull request for #1760 ran the 22 files at `66cb65e8` and `f5188eca`:

| Workflow run | Job | `testthat.R` | R code for possible problems |
|---|---|---|---|
| `R-CMD-check.yaml` 45344301936 | `windows-latest (release, CRAN tier)` | 123 s | 32 s |
| `R-CMD-check.yaml` 45347328164 | `windows-latest (release, CRAN tier)` | 137 s | 37 s |
| `release-matrix.yaml` 45347328439 | `Tarball windows-latest (release)` | 64 s | 36 s |
| | `Tarball macos-latest (release)` | 53 s | |
| | `Tarball ubuntu-latest (devel)` | 50 s | |
| | `Tarball ubuntu-latest (oldrel-1)` | 42 s | |
| | `Tarball ubuntu-latest (release)` | 40 s | |
| | `Minimal dependencies (depends-only)` | 34 s | |

Both *CRAN tier* rows were over the 100 s budget and failed
`verify-cran-test-budget.R`; every other job met it. The 137 s run ended
`[ FAIL 0 | WARN 0 | SKIP 1 | PASS 4528 ]` with a `proc.time()` trailer of
121.37 s user, 14.14 s system and 136.68 s elapsed. Its R code check took the
37 s of the slowest 28-file run above, and 137 s was 0.53 times that run's
259 s, against the local ratio of 0.49.

### The tier without two more files

`grouping-backends` and `summarize-operation` then left the tier, which held
20 files. They were the two slowest of the 22 in the local run above, at
8.44 s and 4.68 s. The same procedure gave:

| Files | Optional backends | Seconds | Expectations | Skips |
|---|---|---|---|---|
| 20 | installed | 19.5, 19.2 | 3,363 | 0 |
| 20 | all nine named in `MARGINPLYR_HIDE_SUGGESTS` | 9.3 | 2,005 | 68 |

No run had a failure or an error. Across the 20 files `grouping_bit()` and
`grouping_id()` were each called in one, `test-grouping-interface.R`, and
every other export in at least two; every member of `optional_backends()`
was named by a guard in at least one. With the backends hidden, each of the
nine produced a `{<package>} is not installed` skip: arrow 7, duckdb 10,
dtplyr 19, data.table 3, RSQLite 16, RPostgres 8, sf 2, hms 2 and S7 1.
`Rscript tools/test-cran-tier.R` printed `CRAN tier fixtures passed.`

The 20 files were not run on Windows or on Win-Builder before the change was
committed. Scaling by the local ratios, 19.35 / 33.25 of the 137 s gives about
80 s for the *CRAN tier* row on a runner of that speed, and 19.35 / 68.2 of
the 416 s gives about 118 s for Win-Builder's tests, in a whole check of about
403 s. Those figures are estimates.

### What CRAN said about check time

The CRAN Repository Policy, revision 6985 as read on this date, asked that
checking take "as little CPU time as possible" and gave no number for a
whole check.

On R-package-devel, Daniel Kelley wrote on 2026-07-13 that an update of `oce`
submitted on 30 June "was rejected on the same day because the build time
exceeded 10 minutes"
(<https://stat.ethz.ch/pipermail/r-package-devel/2026q3/012450.html>). Uwe
Ligges replied on 2026-07-14 that it was "hard to improve the timing as a lot
comes from documentation and offline code checks", and that this was why
"CRAN overruled the 10 min threshold" for that package
(<https://stat.ethz.ch/pipermail/r-package-devel/2026q3/012458.html>).
Neither post named the check machine or gave its times.
