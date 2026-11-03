# Implemented worker recorder acceptance cost

Investigated: 2026-11-03
Revised: 2026-11-03 — controller recording and finalization evidence added below.

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

## Revisions (2026-11-03)

The initial setup/recording acceptance statement above was incomplete: it
omitted controller expectation counting and file-result writes. The Spec review
of PR #1161 identified that those operations belonged to #1155's recording
slice, independently of #1158's proposal calculation.

The [supplementary program](2026-11-03-review-ready-worker-timing/benchmark-controller.R)
used a disposable copy of `fe85e1bb4395f027a7fa4f78c3fdc994e070372d`, whose
ordinary full invocation completed with exit 0, four workers and 201/201 valid
file pairs, with zero ERRORs, WARNINGs and NOTEs. The retained run was
`20261103T061011Z-95400-174a815604ca9`. It read that invocation's actual
175,884 successful expectations and replayed an equivalent success result
through the public reporter callbacks, without executing package tests.
The baseline reporter came from main at
`6f80064f7dd9e0a1822cbadd25633559c1b0074d` (MD5
`ae7432672eb5a5d843c63f44f7de06df`).

Nine samples alternated baseline-first and recorded-first. Each replay used
196 file boundaries while retaining all 201 files' expectation volume, folding
the final five counts into the last boundary. This conservatively covered the
reference 196-file scale with the complete actual expectation volume. The
increment included the new counter callbacks and file-result writes; existing
reporter dispatch and progress output were present in both arms. Each sample
checked 196 output records with exactly 175,884 results. Evidence validation
and output were separately timed against all 201 actual file pairs. Temporary
replay/evidence outputs were deleted; the source invocation was read-only.

| Additional component, seconds | Minimum | Median | Maximum |
| --- | ---: | ---: | ---: |
| Incremental controller recording | 0.062 | 0.292 | 0.495 |
| Evidence validation and output | 0.036 | 0.045 | 0.083 |
| Complete recording components, including previous worker/input costs | 0.2086 | 0.4952 | 0.6394 |

The [controller samples](2026-11-03-review-ready-worker-timing/controller-samples.csv),
[environment](2026-11-03-review-ready-worker-timing/controller-session.txt) and
[complete component samples](2026-11-03-review-ready-worker-timing/complete-samples.csv)
retain this measurement. Complete costs sum the independent component samples
by sample index, retaining the earlier conservative sum across four workers.
The complete recording median was 0.4952 seconds, below the one-second budget;
even the sum of the separate component maxima was 0.7672 seconds. These were
component measurements under ordinary host noise, not a measured whole-suite
wall-time difference. No additional suite ran. Proposal calculation remained
outside this implementation slice and was still #1158's acceptance work.
