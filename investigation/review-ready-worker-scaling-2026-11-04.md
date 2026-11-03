# Review-ready worker count and shared capacity

Investigated: 2026-11-04

## Source and method

The maintainer requested measured comparisons of one slot with four, six or
eight workers and two slots with four workers each. All eight source-tarball
checks used `ff6fe1e541d0ea18cfcf2cb3c5d546869f40675a` on an Apple M4 with
four performance and six efficiency cores, 16 GiB RAM, R 4.6.1 and testthat
3.3.2. Each configuration submitted two jobs together. One-slot configurations
ran them sequentially; two-slot admission started both immediately.

The five mandatory baseline checks passed once before measurement. The
controller retained the existing shared FIFO heavy lock throughout all four
batches, excluding other ordinary Review-ready heavy checks. Each job received
its own extraction of the same committed archive and fresh private temporary
and cache directories. The pinned runner built one tarball and checked it once
with its ordinary offline `R CMD check --as-cran`, required Suggests,
`NOT_CRAN`, snapshots and native `test_check()` dispatch. Wrappers recorded
phase timestamps and changed `TESTTHAT_CPUS`; package and test bytes stayed
unchanged. Worker announcements, complete file start/end pairs, expectation
counts, test/helper hashes and dependency versions were checked across all
eight results.

The retained [controller](2026-11-04-review-ready-worker-scaling/controller.R)
and [worker](2026-11-04-review-ready-worker-scaling/worker.R) were the exact
experimental harness. To repeat, place both files in a fresh workspace and
run `Rscript controller.R <clean-worktree-root> <workspace>` from it against
the pinned checkout. This starts eight complete checks; retained numerical
results require no rerun. The
[dependency inventory](2026-11-04-review-ready-worker-scaling/dependencies.csv)
and [test input hashes](2026-11-04-review-ready-worker-scaling/test-inputs.csv)
recorded the measured environment and inputs.

## Observations

Times below were minutes:seconds from simultaneous pair submission to heavy
build/check completion. They excluded common baseline time and external FIFO
admission. Mean turnaround included each job's internal admission wait.

| Slots × workers | First result | Both complete | Mean turnaround | Peak summed RSS GiB | Peak host swap GiB |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 × 4 | 12:05.5 | 24:12.7 | 18:09.1 | 2.60 | 0 |
| 1 × 6 | 10:05.1 | 20:11.3 | 15:08.2 | 3.44 | 0 |
| 1 × 8 | 8:47.4 | 17:36.5 | 13:11.9 | 4.02 | 0 |
| 2 × 4 | 16:54.6 | 16:55.7 | 16:55.2 | 4.20 | 0.238 |

The [configuration summary](2026-11-04-review-ready-worker-scaling/configuration-summary.csv)
and [job summary](2026-11-04-review-ready-worker-scaling/job-summary.csv)
retain unrounded seconds, queue times, phase times, file work and CPU summaries.
One slot with eight workers reduced pair time and mean turnaround by 27.3%
relative to one slot with four. Two slots finished the pair another 40.8
seconds (3.9%) earlier than one slot with eight, but its first result took
1.92 times as long and its mean turnaround was 28.2% longer.

All eight checks exited zero with 212 files, FAIL 0 / WARN 0 / SKIP 0 /
PASS 181561 each, and zero R CMD check errors, warnings or notes. The original
execution handle, session 25984, was retained through cleanup and terminal
exit zero; the [execution record](2026-11-04-review-ready-worker-scaling/execution-record.txt)
identifies it. The full transient workspace was
`/private/tmp/marginplyr-worker-benchmark-20261104-16d1046316455`; it held the
source archive, individual logs, timing records, results and one-second
resource samples. Its continued availability was not guaranteed.

## Interpretation and limits

The one-second RSS samples summed owned descendants and could count shared
pages repeatedly; they were not unique physical memory. CPU busy and swap
were host-level observations. The native
[post-eight-worker snapshot](2026-11-04-review-ready-worker-scaling/vm-stat-after-1x8.txt)
had zero Swapins/Swapouts; the
[two-slot snapshot](2026-11-04-review-ready-worker-scaling/vm-stat-during-2x4-swap.txt)
recorded actual paging. The raw `ps` sin/sout fields described page I/O on
this Mac and were not treated as native Swapins/Swapouts.

Each configuration had one two-job batch, in order 1×4, 1×6, 1×8, 2×4.
Cache state, thermal state and unrelated host activity were not isolated or
randomized. These observations established this source/environment/host,
not a statistical guarantee. The experimental checks did not certify the
later implementation or authorize normal priority adoption. The selection
and operational contract belonged to ADR 0039.
