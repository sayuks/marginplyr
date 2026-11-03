# Implemented worker recorder acceptance cost

Investigated: 2026-11-03

## Inputs and method

The #1155 recorder at `e5b5789418fe5e3511262e2c8a049e9fae09451d`
was measured on the maintainer Mac: macOS Tahoe 26.6.2 arm64, R 4.6.1,
testthat 3.3.2. The [identity](2026-11-03-review-ready-worker-timing/identity.txt)
and [session](2026-11-03-review-ready-worker-timing/session.txt) record the
recorder hash and dependency environment.

The [program](2026-11-03-review-ready-worker-timing/benchmark.R) used four
persistent PSOCK processes, each assigned 49 of 196 discovered filenames.
It installed the production recorder with only the trace namespace redirected
to a disposable environment. That environment retained the supported
`queue_task()` body and signature while its collaborators did no file work.
This isolated instrumentation cost without executing package tests. Separate
real-worker progress fixtures exercised the actual testthat boundary, cleanup,
snapshots, delayed controller replay and lifecycle failures.

Nine samples alternated traced-first and baseline-first order. Each worker
executed five batches, normalized to one batch, with actual PID-local TSV
writes. Each sample checked 1,960 observations: five times 196 start/end pairs.
Setup included parsing the production helper, shape validation, session output
and trace installation. The reported total conservatively summed all four
workers' setup and incremental recording times, then added preparation of the
complete source's input manifest (201 test files and all test/helper/data bytes).
The worker processes ran concurrently; summing their elapsed costs did not
claim a measured whole-suite wall-time increment.

## Observations

| Component, seconds | Minimum | Median | Maximum |
| --- | ---: | ---: | ---: |
| Four-worker setup, summed | 0.0150 | 0.0160 | 0.0280 |
| Incremental recording, 196 files summed | 0.0208 | 0.0252 | 0.0322 |
| Full input/environment preparation | 0.0570 | 0.0580 | 0.1290 |
| Conservative combined component cost | 0.0938 | 0.1004 | 0.1892 |

The [worker samples](2026-11-03-review-ready-worker-timing/worker-samples.csv)
and [aggregate samples](2026-11-03-review-ready-worker-timing/aggregate-samples.csv)
retain the distribution. The 0.1004-second median satisfied #1155's one-second
setup/recording slice on this host. No CPU isolation, cache reset or `fsync`
was imposed. This did not measure proposal calculation, adoption speedup,
controller expectation counting or final evidence parsing; #1158 owns the
combined recording-and-proposal acceptance measurement. No additional full
suite or timing CI workflow was run for this benchmark.

The implementation's own full source-tarball result belongs to its pinned
Review-ready artifacts and pull-request record, independently of this component
measurement.
