# Four-worker Review-ready test ordering

Investigated: 2026-10-30

## Measured sources and environment

Issue #1085 asked for actual worker start/end times after a completion-only
monitor left `test-sql-dot-dot-summary.R` last. The user additionally requested
duration ordering across the entire suite, including files outside the issue's
four-entry priority list.

The package/test baseline was `9e087a44` (main). The two measured commits were:

| Run | Commit | DESCRIPTION priority |
| --- | --- | --- |
| Baseline | `75d20af4e6dcefc22b2069ff69f3542fc7790db8` | Existing four entries, then alphabetical |
| Descending | `f3b57100ea6ce63e1f4fadc00500d2182eba911c` | All 186 files, descending baseline duration |

Only DESCRIPTION differed between those measured commits. Both used the same
temporary timing helper and unchanged test bodies. Both invoked
`/Users/sayuks/.local/bin/marginplyr-review-ready` from the dedicated worktree;
the common CLI admitted them sequentially. Its ordinary source-tarball check
set `TESTTHAT_PARALLEL=true`, `TESTTHAT_CPUS=4`, `NOT_CRAN=true`, and required
every registered optional Suggest. Both reported `Starting 4 test processes.`
and passed spelling, jarl, package-aware lintr, and R CMD check with zero
ERRORs, WARNINGs, and NOTEs. An earlier instrumentation commit, `e01e2103`,
failed lint before testing; it supplied no timing sample.

The host was an Apple M4 Mac mini, 10 cores (4 performance, 6 efficiency),
16 GB RAM, macOS Tahoe 26.6.2 / Darwin 25.6.0 arm64. R was 4.6.1
(2026-06-24), testthat 3.3.2, and Quarto CLI 1.10.18. The dependency versions
were recorded in `2026-10-30-review-ready-order/environment.txt`. No CPU affinity,
backend thread limit, or cold-cache reset was imposed. Shared admission excluded
another complete Review-ready gate, not every other source of host activity.

## Collection and reproduction

`2026-10-30-review-ready-order/helper-file-timing.R` was copied into
`tests/testthat/helper-file-timing.R` only for measurement. It traced entry/exit
of testthat's `test_one_file()` inside the actual worker, recording filename,
PID, UTC epoch seconds, and worker `proc.time()[["elapsed"]]`. Each worker wrote
its own TSV, avoiding shared append contention. Controller reporter callbacks
were buffered/replayed by testthat and were not used for these timestamps.
The trace surrounded file evaluation and testthat file reporting; worker
startup and final process teardown lay outside it.

The environment variable selected an existing absolute directory outside the
worktree, for example:

```sh
MARGINPLYR_TIMING_DIR=/private/tmp/marginplyr-1085-evidence/baseline /Users/sayuks/.local/bin/marginplyr-review-ready
```

Raw TSVs and all 186 paired file records were retained in the `baseline/` and
`descending/` subdirectories under `2026-10-30-review-ready-order/`. In each CSV,
`start_utc` and `end_utc` were UTC epoch seconds; `start_s` and `end_s` were
relative to that run's first file entry. `duration_s` was the difference of
worker elapsed clocks, not the subtraction of controller callback times.
The raw timestamps had six decimal places; the worker elapsed clock had
millisecond precision. CSV rows were ordered by descending duration, with
filename as the tie breaker.

From the repository root, the following regenerated each CSV and printed its
summary. The analyzer required exactly one start/end pair for each discovered
test file, four worker PIDs, matched pair ownership, and nonnegative durations:

```sh
python3 investigation/2026-10-30-review-ready-order/analyze.py investigation/2026-10-30-review-ready-order/baseline .
python3 investigation/2026-10-30-review-ready-order/analyze.py investigation/2026-10-30-review-ready-order/descending .
```

The baseline first file started at 13:44:57.098035 UTC; the descending run
started at 13:56:33.822121 UTC on the investigation date. For the ordered
measurement, every baseline CSV filename was converted to its testthat stem
and written to DESCRIPTION in that order. An explicit call to
`testthat::find_test_scripts(..., start_first = priority)` verified identical
order, no duplicates, and set equality with all 186 discovered files.

## Results

| Metric (seconds) | Baseline | Descending |
| --- | ---: | ---: |
| First worker file entry to final file exit | 563.212 | 472.880 |
| Last file exit minus penultimate file exit | 115.456 | 0.003 |
| Sum of all file durations | 1884.676 | 1873.514 |
| R CMD check's reported test stage elapsed | 570 | 478 |
| `sql-dot-dot-summary` start | 338.773 | 0.000 |
| `sql-dot-dot-summary` duration | 224.439 | 246.036 |
| `sql-dot-dot-summary` completion | 563.212 | 246.036 |

The file interval fell by 90.331 seconds (16.04%). The sum of file durations
fell by only 0.59%, while `sql-dot-dot-summary` itself took 9.62% longer in the
descending run. The benefit therefore did not depend on that file becoming
faster. All four workers finished their final files within 0.012 seconds of
each other in the descending sample.

The ten longest files had the same rank in both samples:

| File stem | Baseline seconds | Descending seconds |
| --- | ---: | ---: |
| sql-dot-dot-summary | 224.439 | 246.036 |
| sqlite-wide-plan-threshold | 125.203 | 124.325 |
| sql-expansion-input-limits | 86.031 | 85.831 |
| query-policy | 62.180 | 62.270 |
| sqlite-wide-expansion | 59.405 | 58.381 |
| sqlite-boolean-names | 59.341 | 56.348 |
| arrow-directory-partitions | 48.570 | 48.107 |
| dtplyr-special-nesting | 43.557 | 43.168 |
| dtplyr-special-fixed-shares | 38.866 | 37.982 |
| dtplyr-special-names | 32.428 | 32.651 |

## Scheduling and variability

Testthat 3.3.2's `order_test_scripts()` glob-matched each priority stem in
order and appended unlisted paths. `queue_setup()` submitted those paths to
four workers; each free worker took the next file. Concurrent worker startup
meant queue order did not require strictly ordered entry timestamps.

The old priority omitted the longest file. In the baseline, alphabetical
placement delayed its start until 338.773 seconds. The other workers finished
at 444.186, 445.632, and 447.755 seconds, leaving it alone until 563.212.
`test-sql-dot-dot-summary.R` contained nested combinations of signed names,
output/identifier roles, plans, sorting, empty inputs, shares, SQLite/DuckDB
adapters, and collection/materialization. Its measured duration and delayed
dispatch together explained the reproduced long tail. The original
`eb51e759` run mentioned by the issue had no worker start timestamps, so its
exact schedule could not be reconstructed from that completion monitor alone.

To separate ordering from duration changes, a four-worker greedy queue replay
held each sample's file durations fixed, assigning each next file to the
earliest free worker with no startup or dispatch overhead:

| Duration sample | Old order elapsed / tail | Baseline-descending order elapsed / tail |
| --- | ---: | ---: |
| Baseline | 559.787 / 116.828 | 471.184 / 0.017 |
| Descending | 575.476 / 140.309 | 468.395 / 0.006 |

Both fixed-duration replays predicted substantial tail reduction. These were
models, not extra suite runs. Actual dispatch overhead and changing contention
were measured only in the two admitted runs above.
The calculation was retained in `2026-10-30-review-ready-order/replay.py`:

```sh
python3 investigation/2026-10-30-review-ready-order/replay.py
```

For the 137 files taking at least one second in the baseline, the ratio of
descending to baseline duration ranged from 0.814 to 1.187, with median 0.973.
Short-file ranks changed, and two sequential runs did not establish a confidence
interval, a platform-independent ranking, or an optimal schedule. The stable
heavy ranks, near-equal total work, observed tail removal, and fixed-duration
replays supported using baseline descending priority for all measured files.
Re-sorting after every small rank swap would chase noise.

The timing hook was removed from the final package tests. No test body or
worker count was changed. Unlisted future files remained discoverable because
testthat appended them; measuring their durations would be needed to place
them by weight.
