# Three-run calibration of Review-ready ordering margins

Investigated: 2026-11-02

## Question and measured input

The maintainer requested three full checks to assess whether promoting a file
only when it was both more than 50% and more than 10 seconds slower than its
predecessor would find expensive new files without chasing small timing changes.

All three checks used committed experiment
`69599fb59b050cdab9f93e2b473117f8e643ac8e` on branch
`experiment/review-ready-order-calibration`. Its parent was the bounded-wait
repair in [PR #1152](https://github.com/sayuks/marginplyr/pull/1152), based on
main `a563444f831d793c08c15e94510ff09e3377c300`. That repair affected runner
waiting rather than package tests or priority. The experiment added only a
measurement helper; test bodies, snapshots and the tracked priority remained
unchanged for all three checks. No proposed order was applied during them.

The environment was macOS Tahoe 26.6.2 / Darwin 25.6.0 arm64, R 4.6.1,
testthat 3.3.2 and filelock 1.0.3. Each invocation used `--scope full`, four
testthat workers, `NOT_CRAN=true`, required-Suggests enforcement and no coverage.
The 295 installed package versions matched before and after the experiment.
There was no CPU pinning, backend thread cap or cold-cache reset. Shared heavy
admission serialized these checks, not all possible host activity.

These were local Mac measurements, not hosted CI measurements. They supported
an initial policy choice but did not establish CI hardware behavior or a
statistical bound on timing noise.

## Collection and terminal outcomes

The helper traced worker entry/exit of testthat's `queue_task()`, which
surrounded `test_one_file()` and its file cleanup. Each worker wrote its own
TSV with event, filename, PID, UTC time and worker elapsed clock. Duration was
the difference of worker elapsed clocks, not controller callback arrival time.
Worker startup, worker-wide setup and final teardown lay outside this interval.

A small eight-file fixture first established four actual workers and intervals
including 0.05 seconds of work plus 0.15 seconds of deferred cleanup. The real
measurements each had exactly one matched start/end pair for all 196 discovered
files, four worker PIDs and finite nonnegative durations.

Every original execution handle was retained through waiting, checking,
cleanup and terminal completion before the next requested invocation started.
All five mandatory baseline checks passed in each invocation. Each complete
source-tarball check, including snapshots, examples, vignette rebuilding and
manuals, exited zero with no ERROR, WARNING or NOTE.

| Run | Original handle | File interval (s) | Last-file tail (s) | Sum of file durations (s) | E / W / N |
| --- | --- | ---: | ---: | ---: | --- |
| 1 | `44332` | 621.482 | 81.499 | 2203.239 | 0 / 0 / 0 |
| 2 | `14899` | 639.743 | 81.133 | 2278.979 | 0 / 0 / 0 |
| 3 | `96347` | 640.175 | 83.185 | 2275.788 | 0 / 0 / 0 |

The file interval was first worker file entry to final worker file exit, not
the entire build/check duration. The last-file tail was final file exit minus
penultimate file exit. The observed overall file interval varied by about 3%.

The original artifact directory and exact terminal chunk for each run were
retained in the [execution record](2026-11-02-review-ready-order-calibration/executions.json).
One default-sandbox launch failed before test execution; it was recorded as a
setup failure and was not counted among the three measured full checks.

## The expensive unregistered file

The tracked priority contained 186 entries while discovery found 196 files.
The ten unlisted files followed the fixed priority. In every run,
`test-sqlite-boolean-composition.R` was last to finish, taking 93.746, 93.575
and 93.965 seconds. Its delayed dispatch left the measured 81–83 second tail.

The proposed rule promoted the same four new files in all three independent
proposals: `expand-inner-names`, `independent-selection-shares`,
`share-indirect-scope` and `sqlite-boolean-composition`. The last of these moved
from priority 196 to positions 4, 6 and 6. These were proposed queue positions,
not observed starts under a changed order.

Across the 151 files whose median duration was at least one second, the median
max/min ratio was 1.042 and the largest was 1.326. Individual heavy files also
varied: `query-policy` took 58.588, 67.611 and 67.687 seconds. The observations
therefore did not justify treating every change of measured rank as useful.

## Margin comparison and modeled benefit

The analysis used stable insertion, moving a file left only while both margins
held against the immediately preceding file. It preserved close comparisons.
For each proposal, a four-worker greedy replay evaluated the order with each
other run's durations held fixed. The table used the six held-out combinations,
excluding the proposal's own measurement run.

| Relative / absolute margin | Files promoted per proposal | Median held-out modeled gain (s) |
| --- | --- | ---: |
| >50% and >10s | 4 / 4 / 4 | 59.532 |
| >50% and >5s | 8 / 8 / 8 | 62.710 |
| >25% and >10s | 4 / 4 / 4 | 59.531 |
| >25% and >5s | 8 / 8 / 8 | 62.708 |
| >10% and >2s | 9 / 9 / 9 | 65.266 |
| Strict descending duration | 108 / 108 / 115 | 65.528 |

The proposed margins retained about 91% of strict sorting's median modeled
gain while promoting four files instead of more than a hundred. Their six
held-out gains ranged from 58.966 to 60.914 seconds. Lowering the absolute
margin to five seconds added approximately three seconds of modeled benefit.
The 25% / 10s alternative performed similarly; these samples did not establish
50% as the unique best relative threshold.

The replay assumed unchanged file duration under another schedule and omitted
worker startup/dispatch overhead. It did not measure a real speedup: caches,
contention and file duration could change after reordering. The observed test
intervals in the preceding table remained the baseline-order measurements.

## Residual changes after an update

Applying the three duration samples chronologically to the evolving proposed
order, without executing another suite, produced:

| Measurement applied | Files promoted | Crossed pairs | Modeled incremental gain (s) |
| --- | ---: | ---: | ---: |
| 1 | 4 | 485 | 59.531 |
| 2 | 2 | 4 | 0.006 |
| 3 | 0 | 0 | 0.000 |

All six sample permutations admitted one or two additional update steps after
the initial proposal. Reapplying identical inputs was idempotent; the later
changes came from different measured durations rather than repeated processing
of one sample. Thus the margins reduced sensitivity without guaranteeing a
stable configuration after one update.

The observations supported the proposed margins as a conservative initial
filter for this workload. They did not support committing every nonempty
proposal or using the per-file margins as an automatic merge criterion. A
whole-suite benefit estimate made the negligible second proposal distinguishable
from the approximately one-minute initial benefit. Production policy and the
reviewable update workflow belong to the specification and implementation
ticket, not to this dated note.

## Retained data and reproduction

The [data directory](2026-11-02-review-ready-order-calibration/files.csv)
contained file identities and original priority, package/environment versions,
the measurement helper, all twelve worker TSVs, the three console transcripts,
execution outcomes, per-file intervals and variability, candidate proposals,
margin comparisons, sequential-update calculations and counterfactual replays.
These were retained research evidence, not inputs to later test execution.
Original runner artifacts retained their ordinary seven-day policy.

From this repository root, the following regenerated the analysis from the
saved raw logs, without running tests or requiring the original run directories:

```sh
python3 investigation/2026-11-02-review-ready-order-calibration/analyze.py
```

Replaying the saved raw records reproduced the original summary exactly.
