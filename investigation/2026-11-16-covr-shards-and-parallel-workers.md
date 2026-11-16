# Merging sharded covr results, and covr under testthat's parallel workers

Investigated: 2026-11-16
Tested tree: package sources and tests of `8f1b7e5b` (`origin/main`);
`tools/coverage-check*.R` from branch `t3/1694-shard-coverage`

Issue #1694 proposed sharding the coverage job and stated, for confirmation,
that testthat's parallel workers run in callr subprocesses that covr's
in-process tracing does not observe. This note records what was measured for
both questions with covr 3.6.5.9001 and testthat 3.3.2 on macOS.

## Parallel workers are observed

The statement in the issue does not hold for these versions.

`covr::package_coverage()` does not trace in its own process. It installs the
package into `install_path` and appends to the installed load script a
`reg.finalizer(ns, ..., onexit = TRUE)` that saves that process's counters to
a `covr_trace_*` file; every process that loads the instrumented namespace
writes one. After the tests, it reads every trace file in `install_path` and
`merge_coverage()` sums the counts per expression key. testthat's
`queue_teardown()` cooperates: under `in_covr()` it gives each worker a
30-second grace period to exit by closing its input instead of being killed,
so the finalizer runs.

A three-file package with `Config/testthat/parallel: true` and
`TESTTHAT_CPUS=3`, each test file calling a different function, was run
through `package_coverage(type = "tests")`. `testthat.Rout` showed
`Starting 3 test processes.` and three distinct worker PIDs, one per file.
`install_path` held five trace files: three with exactly one function's lines
counted twice, two (the test driver and an idle process) with zero counts.
The merged tally reported all three functions covered with value 2.

So `TESTTHAT_PARALLEL` is a measured remedy as well as sharding; the decision
between them belongs to `test-coverage.yaml`, not to this note.

## Sharded results merge to the unsharded verdict

Each shard's raw covr object was saved with `saveRDS()` and the merge summed
`value` per expression, matched by the source file's path below the shard's
copy plus the integer srcref, since each shard instruments its own copy.

Subset tree: the checkout with `tests/testthat` reduced to every 17th test
file in radix order (20 files: `test-across-argument-matching.R`,
`test-arrow-partition-expansion.R`, `test-data-frame-subclass.R`,
`test-dtplyr-dot-dot-identifiers.R`, `test-dtplyr-order-helpers.R`,
`test-dtplyr-special-names.R`, `test-expand-inner-names.R`,
`test-grouping-constructor-arguments.R`, `test-grouping-variadic-names.R`,
`test-margin-display.R`, `test-nest-local-list-converter.R`,
`test-postgres-expansion-input.R`, `test-selection-operators.R`,
`test-sf-expansion.R`, `test-share-enclosure-replacements.R`,
`test-share-list2env-rebinding.R`, `test-share-singleton-bindings.R`,
`test-sql-env-expansion.R`, `test-sqlite-boolean-env-input.R`,
`test-sqlite-savepoint-acquisition.R`), helpers and setup unchanged. An
unsharded `coverage-check.R --cobertura` and the two shards of `--shard i/2`
ran concurrently, then `--merge`.

- Unsharded: 23508 expectations passed, 855 s. Shards: 9221 and 14287
  expectations (sum 23508), 583 s and 356 s.
- Verdict output, from the `covr ...: 10875/17787 lines (61.14016%).` tally
  through all 6912 uncovered `file:line` entries: byte-identical.
- Cobertura: the same 17787 lines and the same zero-hit set. Per-line hits were
  not identical: 122 lines differed, all already covered, by a small constant
  (120 higher in the merge, 2 lower; total 255448211 against 255448017).

The differing lines are in `R/backend-metadata.R`, `R/conditions.R` and
`R/grouping-plan.R`, and differ by 1 or 5. The clearest are the lines of the
memo closure that fills `admitted` in `R/grouping-plan.R`: the cache miss runs
once per R process, and the two shards are one process more than the unsharded
run. Exact per-line equality therefore does not survive sharding, while the
zero-coverage set, which is all the verdict reads, does.

## A shard cannot delete the other shards' test files

The subset comparison above ran each shard from a copy with the other shards'
test files deleted. On the full suite that broke tests that read test files
they do not run. A four-way run of `8f1b7e5b`'s suite failed shard 4 five
times at `test-diagnostic-pluralization.R:248`, which asserts that each
pluralizing diagnostic's pinning test file exists under `test_path()`, for
`test-grouping-backends.R`, `test-grouping-bare-column.R`,
`test-assigned-summary-collisions.R`, `test-contextual-helpers.R` and
`test-execution-conditions.R`, none of them in shard 4. Shards therefore keep
every test file and select theirs through `test_check()`'s `filter`.

The same run's shard 2 failed in `test-duckdb-json-metadata.R` because the
local run had no prepared DuckDB JSON extension; that was the local
environment, not sharding, and the later runs set
`MARGINPLYR_DUCKDB_JSON_EXTENSION` to the cached extension as CI's
provisioning step does.

## Shard balance

Selecting every N-th file in radix order spreads the slow files unevenly
because a few files dominate: in the review-ready timing evidence of
`aa36fd56` (R CMD check without covr, eight workers),
`test-sqlite-wide-order.R` took 698 s and
`test-share-fresh-environment-exposure.R` 531 s of 8024 s. The largest
shard's share of the summed file durations, over the nine eligible timing runs
recorded between 2026-11-14 and 2026-11-16 (335 to 338 files):

| shards | largest share |
|---|---|
| 4 | 0.324 to 0.335 |
| 6 | 0.228 to 0.248 |
| 8 | 0.190 to 0.206 |

For `aa36fd56`, dealing files round-robin in the `Config/testthat/start-first`
order, which ranks them by measured duration, gave a largest-to-mean ratio of
1.30 for six shards and 1.35 for eight, against 1.37 and 1.52 for every N-th
file; alternating the dealing direction gave 1.36 and 1.37.

The full-suite comparison is recorded under *Full suite* below.

## Full suite

Trees: `b9c91e93` for the unsharded run and `111e68b1` for the shards, whose
`R/`, `tests/` and `tools/coverage-check.R` unsharded path are identical; the
shard command there selects files with the `filter`. Both runs set
`MARGINPLYR_DUCKDB_JSON_EXTENSION` to the locally cached v1.5.5 extension.
The unsharded run shared the machine with the shards, four at a time, and with
other sessions' work, so the durations show proportions rather than CI times.

- Unsharded `coverage-check.R --cobertura`: 306449 expectations passed,
  13455 s.
- `--shard i/8`, i = 1..8: all passed; 55602, 37057, 57335, 20236, 30739,
  29417, 36914 and 39149 expectations, summing to 306449; 1873, 1514, 2618,
  1773, 1061, 1679, 2013 and 3158 s, so the largest shard took 0.201 of the
  summed shard time. `--merge` took 6 s.
- Verdict output, `covr 3.6.5.9001: 17782/17787 lines (99.97189%).` and the
  five uncovered lines `R/share.R:8482`, `R/share.R:8552`, `R/share.R:8591`,
  `R/share.R:8596` and `R/static-bindings.R:395`: byte-identical.
- Cobertura: the same 17787 lines and the same five zero-hit lines. 562 covered
  lines had different hit counts (406 higher in the merge, 156 lower; total
  6377476286 against 6377450959), the per-process effect described above
  over eight processes instead of two.
