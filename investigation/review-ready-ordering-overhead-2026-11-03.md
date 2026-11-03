# Cost of recording and computing Review-ready priorities

Investigated: 2026-11-03

## Question and inputs

The maintainer asked whether recording duration and computing priority proposals
would consume the test-time savings. The measurements below used component
benchmarks on the maintainer Mac, not another full suite or an instrumented
versus uninstrumented package-check comparison. No production implementation
or committed test priority was changed.

The specification worktree was at
`68fb4775bfe10370f45a5f9b0472f77cfe1db003`, with ongoing specification-only
edits. The recorder helper and saved 196-file observations were from the
[three-run calibration](review-ready-order-calibration-2026-11-02.md), whose
measured revision was `69599fb59b050cdab9f93e2b473117f8e643ac8e`.
The host ran macOS Tahoe 26.6.2 arm64 and R 4.6.1 with testthat 3.3.2.
No CPU affinity, host isolation or filesystem cache reset was imposed.

## Recording and hook setup

The existing recorder expressions were installed on an empty `queue_task(path)`
harness by changing only the trace target from the testthat namespace to a
temporary environment. This isolated the cost of timestamp construction,
trace dispatch, directory checks and the actual opening, appending and closing
of two small TSV records per file. Nine samples each performed five batches
of 196 calls, subtracting an otherwise identical untraced empty-call baseline.
Sample order alternated between traced-first and baseline-first. Reported
values were divided by five; they were serial totals, not four-worker wall
time measurements.

| Component | Minimum | Median | Maximum |
| --- | ---: | ---: | ---: |
| Incremental recording for 196 files | 14.820 ms | 16.998 ms | 20.002 ms |
| Real testthat trace installation for one loaded worker | 0.275 ms | 0.280 ms | 16.675 ms |

Each batch emitted 392 records totaling 26,072 bytes. Installing the original
helper on the actual loaded testthat namespace was measured separately nine
times, untracing after each sample; its first installation was the maximum.
No testthat task or package test was executed during that setup measurement.
These repeated measurements did not establish a fresh-process distribution
for all four workers, concurrent disk contention or real-check scheduling
effects. Writes used ordinary buffered storage without explicit `fsync`.

The [benchmark program](2026-11-03-review-ready-ordering-overhead/benchmark-recording.R),
[recording samples](2026-11-03-review-ready-ordering-overhead/recording-samples.csv),
[hook samples](2026-11-03-review-ready-ordering-overhead/initialization-samples.csv)
and [environment summary](2026-11-03-review-ready-ordering-overhead/recording-summary.txt)
retain the method and observations. To repeat the recorder benchmark, supply
this repository root and a separately created empty output directory as its
two arguments; it writes no results into the source tree.

## Parsing, calculation and proposal output in base R

A base-R saved-input prototype read and validated the four run-1 TSVs, matched
196 start/end pairs, checked archived completion evidence and the priority
field, inserted ten unregistered files without changing existing relative
order, built a >50% and >10-second stable-insertion candidate, and replayed
three four-worker schedules. It then wrote an uncompressed 8,274-byte proposal
RDS and a 5,479-byte text patch. The actual runner already had a parent R
process; measuring an additional Rscript launch was a separate conservative
scenario, not a requirement to launch another interpreter in production.

| Component | Minimum | Median | Maximum |
| --- | ---: | ---: | ---: |
| New Rscript including startup, seven samples | 123.030 ms | 144.692 ms | 148.011 ms |
| Pipeline inside those processes | 14.220 ms | 16.332 ms | 16.729 ms |
| Pipeline in an existing process, 30 iterations | 7.450 ms | 7.822 ms | 16.212 ms |

Fresh-process pipeline medians comprised 8.866 ms of reading/validation,
6.547 ms of registration/reordering/replay and 0.886 ms of serialization/write.
Existing-process medians for those components were 6.441, 0.715 and 0.668 ms.
Medians of components need not sum to the median of their total.

The recorded prototype selected registration alone. Its modeled schedules were
550.829 seconds for registration, 550.829 for the optional candidate, and
550.840 for unregistered-first dispatch. These agreed with an earlier Python
prototype using the same inputs; they were schedule estimates, not new suite
executions. `git apply --check` accepted the generated patch without applying it.
Neither Python nor R output formats or insertion tie rules were selected as
production interfaces by this experiment.

The first R driver reached a parse error in its later aggregation definitions
after running seven child pipelines and 30 in-process iterations. Its seven
inner results survived, but fresh outer-wall and warm timing values had not
been saved and were not reported. After recording that loss, one explicitly
authorized replacement batch ran with parse-only validation and a checkpoint
after every sample; its driver exited zero. The table reports that replacement
batch, not reconstructed measurements. The
[failure record](2026-11-03-review-ready-ordering-overhead/r-initial-attempt-status.txt)
retains this distinction.

The [driver](2026-11-03-review-ready-ordering-overhead/benchmark.R),
[pipeline](2026-11-03-review-ready-ordering-overhead/cost_pipeline.R),
[child entry](2026-11-03-review-ready-ordering-overhead/run_once.R),
[summary](2026-11-03-review-ready-ordering-overhead/r-summary.csv),
[fresh samples](2026-11-03-review-ready-ordering-overhead/r-fresh-process.csv),
[warm samples](2026-11-03-review-ready-ordering-overhead/r-warm-in-process.csv)
and [environment/hashes](2026-11-03-review-ready-ordering-overhead/r-environment.txt)
retain the method and numerical evidence. Reproduction accepts the repository
root and a new output directory as the driver's two arguments. It executes
seven fresh pipelines and 30 in-process iterations, not tests or Review-ready.

## Interpretation and limits

The component maxima were approximately 20 ms for all 196 serial recordings,
148 ms for a whole extra Rscript process, and 17 ms for one trace installation.
Adding four copies of that observed installation maximum yielded about
235 ms. This was a conservative arithmetic combination of component samples,
not an observed end-to-end runtime or a future upper bound. It was small
relative to ten seconds of modeled scheduling benefit.

The prototypes did not implement all-skipped eligibility, general reordering
patch generation or checking current test-file content against stale proposals.
They used buffered writes and warm filesystem caches, and did not measure
concurrent recorder contention, instrumentation effects inside real heavy
tests, or nonlocal storage. Consequently these results supported the
feasibility of a small overhead budget; they did not establish the completed
implementation's cost or an actual net package-check speedup. Production
instrumentation still needed its own acceptance measurement.
