# R package mutation testing tools

Investigated: 2026-09-12

## Question

Which source-mutation tool and execution method best support a focused
marginplyr campaign whose purpose is to inspect every survivor, rather than to
maximize a mutation score?

## Sources and versions

- `muttest` 0.3.0, released on CRAN on 2026-07-21. Its DESCRIPTION requires
  R >= 4.1.0 and does not impose an upper bound; CRAN also advertised an
  `r-devel` binary. The official release source is tag
  [`v0.3.0`](https://github.com/jakubsob/muttest/tree/v0.3.0), commit
  [`1c8effccb7d73c1da25d75491d5948ce94c613cc`](https://github.com/jakubsob/muttest/commit/1c8effccb7d73c1da25d75491d5948ce94c613cc),
  and the release metadata is on
  [CRAN](https://CRAN.R-project.org/package=muttest). These metadata make it a
  supported candidate for R 4.6; the campaign still has to prove the actual
  package/tool combination on its R 4.6.1 host.
- `mutator` 0.2.1 was released on CRAN on 2026-08-02. The evaluated official
  development source was commit
  [`9f5e703c7b2f6bbae3fd26a68eee5dc3b9275707`](https://github.com/PRL-PRG/mutator/commit/9f5e703c7b2f6bbae3fd26a68eee5dc3b9275707)
  (`DESCRIPTION` 0.2.2.9000); release metadata is on
  [CRAN](https://CRAN.R-project.org/package=mutator). Its engine requires
  C++17.
- rOpenSci `autotest` was not a substitute: its documentation defines
  “mutation” as mutating *inputs* scraped from examples, not mutating
  production source and running the existing suite
  ([official documentation](https://docs.ropensci.org/autotest/)).

## Comparison

`muttest` builds an explicit plan from caller-selected source files and
caller-selected mutators. A plan is an ordinary data frame and may be subset
before execution. Its supplied families cover arithmetic swaps, comparison
direction and strict/non-strict boundaries, logical swaps, condition negation,
boolean/NA/numeric/string literals, index shifts, explicit returns, and
statement deletion; `operator(from, to)` and `call_name(from, to)` permit a
narrow semantic probe. The authoritative definitions are
[`R/mutator-presets.R`](https://github.com/jakubsob/muttest/blob/v0.3.0/R/mutator-presets.R)
and the adjacent `R/mutator-*.R` files. `FullTestStrategy` runs
`testthat::test_dir()` and the default `PackageCopyStrategy(symlink = FALSE)`
copies the repository to a temporary directory before overwriting the copied
source. Each worker deletes its copy on exit
([runner](https://github.com/jakubsob/muttest/blob/v0.3.0/R/muttest.R),
[test strategies](https://github.com/jakubsob/muttest/blob/v0.3.0/R/test_strategy.R),
[copy strategy](https://github.com/jakubsob/muttest/blob/v0.3.0/R/project_copy_strategy.R)).
Version 0.3.0 also provides JSON records with exact locations and distinct
`Killed`, `Survived`, `NoCoverage`, and `RuntimeError` statuses
([reporter](https://github.com/jakubsob/muttest/blob/v0.3.0/R/reporter-json.R)).

`mutator` has stronger automatic orchestration: it verifies a baseline,
recognizes the package's `testthat` harness, can select covering test files,
stops on the first failure, calibrates a hard subprocess timeout, and can deep
copy `src/` and `tests/`. Its documented operator set covers arithmetic,
comparison and logical swaps, `!` removal, condition negation, scalar
replacement, explicit-return replacement, and deletion
([configuration](https://prl-prg.github.io/mutator/articles/configuration.html),
[engine source](https://github.com/PRL-PRG/mutator/blob/9f5e703c7b2f6bbae3fd26a68eee5dc3b9275707/src/ASTHandler.cpp)).
However, `mutate_package()` accepts file *exclusions*, not a positive list of
files or operator families, and its engine emits all enabled families together.
More importantly for this investigation, any non-timeout execution error is
normalized to `KILLED`, while the requested report must keep errors separate
([execution source](https://github.com/PRL-PRG/mutator/blob/9f5e703c7b2f6bbae3fd26a68eee5dc3b9275707/R/package-execution.R)).
Its default `cran = TRUE` also sets `NOT_CRAN=false`, which would omit
marginplyr's structural snapshots unless overridden. Its optional LLM
equivalence detector should not replace evidence-based survivor triage.

## Selection

Use `muttest` 0.3.0 for this focused campaign. Exact operator/file selection,
plan-row inspection, and separate runtime-error records outweigh `mutator`'s
automatic coverage selection. Use the full test strategy: marginplyr tests do
not follow a one-source-file/one-test-file mapping, so `FileTestStrategy` would
create false `NoCoverage` results.

The first high-signal scope is the grouping-plan compilation and expansion in
`R/grouping-plan.R`: `preflight_grouping_spec()`,
`compile_grouping_spec()`, `compile_grouping_spec_impl()`,
`expand_grouping_family()`, `expand_grouping_sets()`, `expand_rollup()`,
`expand_cube()`, and `expand_grouping_product()`. Start with comparison,
logical, condition, and boolean mutators; add numeric/index mutations only
where a literal or subscript is part of an observable boundary. Arithmetic
swaps are more valuable in the share-ratio code than in grouping-plan glue.
Statement, arbitrary string, broad NA/NULL, and return deletion are second-pass
operators because they predominantly produce trivial errors or diagnostic-text
noise in this package.

```r
plan <- muttest::muttest_plan(
  source_files = "R/grouping-plan.R",
  mutators = c(
    muttest::comparison_operators(),
    muttest::logical_operators(),
    muttest::condition_mutations(),
    muttest::boolean_literals()
  )
)

# muttest has no direct function selector. Its documented plan is subsettable;
# use the exact mutation location to retain the dated function region.
start_line <- vapply(
  plan$mutation,
  function(x) x$location$start$line,
  integer(1)
)
plan <- plan[start_line >= 559L & start_line <= 1107L, , drop = FALSE]

reporter <- muttest::JSONMutationReporter$new("/tmp/marginplyr-muttest.json")
Sys.setenv(NOT_CRAN = "true")
result <- muttest::muttest(
  plan,
  path = "tests/testthat",
  reporter = reporter,
  test_strategy = muttest::FullTestStrategy$new(load_package = "source"),
  copy_strategy = muttest::PackageCopyStrategy$new(symlink = FALSE),
  workers = 1L,
  timeout = 120000L
)
```

Install `muttest` into a throwaway library from CRAN or the pinned `v0.3.0`
tag; do not add it to marginplyr's DESCRIPTION. Run the campaign from a
disposable export/copy of the recorded marginplyr commit, not from the checkout.
The full-copy strategy then gives a second isolation layer. Record
`git status --porcelain` before and after; a worker crash may leave a directory
under the session temp directory, but cannot change the checkout.

The timeout is milliseconds and is not calibrated automatically. Establish a
full-suite baseline, choose an explicit generous multiple, and begin with one
worker to avoid backend/database contention. `muttest` records both a timeout
and a run-level crash as `RuntimeError`, and its own score counts errors as
detected. For this investigation, ignore that scoring convention: use the
JSON `statusReason` (`Timed out` for its timeout path) and logs to classify each
as `TIMEOUT` or `ERROR`, then rerun it alone before deciding it is a backend or
environment artifact. Keep `MARGINPLYR_REQUIRED_SUGGESTS` unset for the first
local campaign and record which optional packages are installed; dedicated
backend conclusions require the corresponding release-matrix environment.

## Confirmed limitations

- Neither tool proves semantic equivalence; every survivor still needs contract
  and reachability analysis.
- `muttest` does not execute `tests/testthat.R`; it calls `testthat::test_dir()`.
  For marginplyr's standard harness this is close to `test_local()`, but it is
  not a byte-for-byte reproduction of `test_check()`.
- `muttest` has no coverage-guided selection or fail-fast execution, and its
  ten-minute default timeout is too broad for useful classification.
- `mutator`'s source locations can be function-wide without its optional
  GitHub-only `imputesrcref`, its default coverage guidance assumes deterministic
  tests, and its error-to-kill normalization loses a category required here.
- `NOT_CRAN=true` exercises marginplyr's structural snapshots, but installed
  optional backends still determine which guarded tests run. A survivor from
  one host therefore cannot establish a backend-independent test gap.
