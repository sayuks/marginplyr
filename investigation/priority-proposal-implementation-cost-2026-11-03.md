# Cost of local priority proposal implementation

Investigated: 2026-11-03
Revised: 2026-11-03 (#1158) — measured-native comparison fix and repeated acceptance measurement

The #1158 acceptance measurement used the maintainer Mac, R 4.6.1,
testthat 3.3.2 and macOS Tahoe 26.6.2. The production helper hashes and complete
session were retained in the [summary](2026-11-03-priority-proposal-cost/summary.txt).
The [program](2026-11-03-priority-proposal-cost/benchmark.R) and
[nine samples](2026-11-03-priority-proposal-cost/samples.csv) retained the method
and distribution. One preceding sample was discarded as warmup.

The combined incremental cost had minimum **0.447 s**, median **0.486 s** and
maximum **0.544 s**, below the accepted one-second median bound. Each sample
included preparation of archived bytes/hashes and dependencies for 196 files,
four installs of the production hook on the real supported testthat function,
196 empty-task entry/exit recordings minus the uninstrumented baseline,
initial evidence parsing, revalidation, calculation and proposal output.

| Component | Median seconds |
| --- | ---: |
| Input preparation | 0.173 |
| Four real hook setups | 0.021 |
| Incremental recording for 196 tasks | 0.033 |
| Initial evidence parsing/output | 0.073 |
| Proposal revalidation, calculation and output | 0.198 |
| Combined per-sample total | 0.486 |

Component medians need not sum to the combined median. The harness copied test
inputs and retained 196 discovered files. It reused the production recording
expressions with only the target namespace and supported-shape predicate
adapted for empty tasks. The separately measured setup still checked the real
supported function shape. Recording was serial, covering the total 392 event
writes without measuring four-way IO contention. Worker process startup and
actual test work were excluded; no package suite or timing CI job ran for this
measurement. Synthetic successful metadata authorized only disposable output
inside the harness and was never used for a production priority update.

The full-check and real-worker fixtures supplied the separate execution and
eligibility evidence. This measurement established incremental component cost,
not an observed speedup from adopting a proposed schedule.

## Revisions (2026-11-03, #1158)

The submitted implementation at `d185caeb` recalculated its native comparison
from the target's current priority after a priority-only change. A disposable
eight-file reproduction changed that model from 120 to 110 seconds while
holding the measured observation sample fixed. The comparison needed to retain
the measured invocation's archived priority. The fix passed the regression
asserting that a priority-only recalculation retains its measured native order.

The same acceptance program was repeated after that fix. The
[follow-up samples](2026-11-03-priority-proposal-cost-followup/samples.csv) and
[summary with helper hashes](2026-11-03-priority-proposal-cost-followup/summary.txt)
recorded minimum **0.344 s**, median **0.454 s**, maximum **0.474 s** for the
combined incremental cost, again below one second. The original measurements
above describe their original helper hash; the follow-up records the fixed one.
