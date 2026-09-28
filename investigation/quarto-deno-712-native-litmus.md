# Bounded native Arm litmus for SQLite startup publication

Investigated: 2026-09-28
Revised: 2026-09-28 — operational disposition in `design/specs/review-ready-native-crash-retry.md`
Primary input: [the formal Arm check](quarto-deno-712-arm-model.md), [the symbolicated crash evidence](quarto-deno-712-followup.md), [#712](https://github.com/sayuks/marginplyr/issues/712)
Repository baseline: `e5be3359f4cc7c56256c2570009c55be62c0e710`

## Conclusion

The requested native comparison completed on the local Apple M4. Four
predeclared trials in original/barrier/barrier/original order each completed
1,000,000 iterations. Neither variant observed `alloc != 0 && init == 0`.
The original reader reached its callback load 104,448 times across its two
trials; the barrier reader reached it 88,534 times. The remaining iterations
observed an unpublished guard and skipped that load.

This was a bounded non-reproduction, **not evidence that the ordering
hypothesis was false**. The formal counterexample remained valid under its
model assumptions; this experiment did not connect it to a witnessed execution
on the M4. The reason for the historical null `xMutexInit` call remained
unconfirmed. No production mitigation was justified by these results.

## Question and predeclared contrast

The prior investigation had already identified the crash instruction and
validated an Arm model witness for this publication sequence. This step asked
whether an assembly implementation of that sequence would actually return the
inconsistent pair on the affected host. It measured pointer values before a
call; it neither called a null pointer nor ran Quarto, Deno, SQLite as a whole,
or Review-ready. No new full check or rendering was performed.

The execution plan was recorded before any concurrent trial in
[plan.json](quarto-deno-712-native-litmus/plan.json):

- Order A–B–B–A, where A retained the original reader ordering and B added one
  `DMB ISH` immediately before the callback load.
- 1,000,000 iterations per process, 4,000,000 total; no adaptive extension or
  retry. Each process had a 30-second watchdog and a 35-second outer timeout.
- Fixed per-role xorshift32 seeds, producing 0–63 delay-loop iterations before
  the measured function. Every trial used the same deterministic delay sequences.
  A loop iteration was `NOP; SUBS; B.NE`, not a calibrated time interval.
- Stop the sequence on timeout, unexpected values, or an inconsistent pair
  in B. Such a result would require a harness audit, not a mitigation claim.

The expected discriminator was an inconsistent pair in A and none in B. Zero
in both would leave the mechanism unconfirmed. This stopping rule overrode any
generic debugging advice to keep stressing until a failure appeared.

## Host, build, and verified conditions

| Property | Recorded observation |
| --- | --- |
| CPU | Apple M4, 4 Performance and 6 Efficiency cores |
| Cache line | 128 bytes from `hw.cachelinesize` |
| OS | macOS 26.6.2, build `25G83` |
| Compiler | Apple clang 17.0.0 (`clang-1700.6.3.2`) |
| Compiler target | `arm64-apple-darwin25.6.0` |
| Harness image | Mach-O 64-bit executable, arm64 only |
| Harness UUID | `0ED19942-3746-3906-A9B3-E2D3DC515F3C` |
| Translation | Harness itself reported `sysctl.proc_translated = 0` in all four trials |
| Slot layout | Distance 16 bytes; `init % 128 = 120`, `alloc % 128 = 8` in every trial |
| Scheduling | Default pthread scheduling; no affinity, QoS, or memory-ordering mode changes |

The original Deno image's slots were at unslid addresses `0x105df55f8` and
`0x105df5608`, with the same distance and cache-line offsets. Preserving this
cross-line placement mattered for the hardware contrast even though the prior
formal model did not model cache lines. It did not reproduce all surrounding
SQLite globals or their other users.

The compiler invocation was:

```sh
xcrun clang -arch arm64 -std=c11 -O2 -Wall -Wextra -Werror -pthread \
  harness.c probe.S -o native-litmus
```

The build had no compiler warnings. The exact source and executable SHA256
values were saved in the plan. The source files were
[harness.c](quarto-deno-712-native-litmus/harness.c) and
[probe.S](quarto-deno-712-native-litmus/probe.S). The complete
[disassembly](quarto-deno-712-native-litmus/disassembly.txt) confirmed the
measurement instructions and synchronization placement. A required host
inspection outside the filesystem sandbox enabled the process-local sysctl
query; no system setting or installed executable was changed.

Native execution was verified by both image architecture and the process's
translation query, following [Apple's Rosetta detection documentation](https://developer.apple.com/documentation/apple-silicon/about-the-rosetta-translation-environment).
This did not directly measure a CPU TSO control bit. No claim was made that
particular workers ran simultaneously on particular physical cores. Apple's
[scheduling guidance](https://developer.apple.com/videos/play/tech-talks/110147/)
does not make default scheduling a CPU-placement guarantee.

## Measurement and reset isolation

Both reader functions used `LDR guard; MOV saved_guard; CBNZ`, then overwrote
the pointer register with an independent address before `LDR callback`. Both
retained the trailing `DMB ISH`. B alone added the pre-load `DMB ISH`. The
writer used `STUR init=1; DMB ISH; STUR alloc=1`. Nonzero function addresses
were represented by 1, and null by 0. The prior note explains the selected
writer path, independent address calculation, and omitted callback execution.

The callback load and publication load used scratch registers corresponding
to the prior formal test. Results were saved only after the measurement.
Real code addresses, `ADRP` latency, other method-table stores, the null call,
and the process startup environment were not reproduced. The trailing barrier
was executed even for a hypothetical zero callback; as in the formal test,
this was a completion used to observe values, not the behavior after a crash.

Two persistent workers and a coordinator used numbered epochs:

```text
writer/reader measurement
  -> each worker publishes done with release
  -> coordinator acquires both done values
  -> coordinator reads the result and resets both slots
  -> coordinator publishes the next start with release
  -> each worker acquires that start
  -> next measurement
```

The first reset preceded the first start. All later resets followed both
completion acquisitions. The reader alone wrote its result; the coordinator
read it after acquiring reader completion. Control variables and the result
buffer occupied separate 128-byte-aligned storage from the measured slots.
The slots' reset and measurement accesses were entirely in the external
assembly file, so the C optimizer did not generate or relocate those accesses.
This was a check of the emitted program, not a portable ISO C data-race proof
for arbitrary compiler/assembly combinations.

The generated C11 epoch operations were `LDAPR` (RCpc acquire) and `STLR`
(release). Independent review checked that their acquire-to-later-access and
earlier-access-to-release ordering, connected by the matching epoch values,
separated reset from measurement. It did not require the additional
release-to-acquire ordering of `LDAR`. The relevant ordering rules were also
present in the already verified
[Arm model](https://github.com/herd/herdtools7/blob/cd32a878db23656a24bc5cb31f71b04d1400a7c9/herd/libdir/aarch64hwreqs.cat).
None of these boundary operations inserted an ordering instruction between A's
two measured loads.

## Executions and results

One serial selftest process preceded the four concurrent trials. For each
reader, it explicitly seeded `(init, alloc)` with `(0,0)`, `(1,1)`, and `(0,1)`;
all six observations matched. The last state deliberately produced the target
pair as a check that it could be recorded. **Those synthetic observations were
not concurrent witnesses** and were excluded from every trial counter below.

The unchanged binary then ran through [run_trials.py](quarto-deno-712-native-litmus/run_trials.py).
The driver checked the planned file hashes, refused to overwrite an existing
execution record, captured every result, and stopped on failure. The exact
commands and UTC start/finish times are in
[runs.json](quarto-deno-712-native-litmus/runs.json). All four completed between
03:17:28 and 03:17:30 UTC on the investigation date, with exit 0 and empty stderr.

| Order | Reader | Completed | Guard zero, load skipped | Guard 1 / callback 1 | Guard 1 / callback 0 | Measured seconds |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 1 | A: original | 1,000,000 | 942,769 | 57,231 | 0 | 0.175766 |
| 2 | B: reader barrier | 1,000,000 | 954,451 | 45,549 | 0 | 0.176864 |
| 3 | B: reader barrier | 1,000,000 | 957,015 | 42,985 | 0 | 0.192769 |
| 4 | A: original | 1,000,000 | 952,783 | 47,217 | 0 | 0.185684 |

These durations were the harness's monotonic measurement interval; process
start/setup overhead was recorded separately by the driver. The full JSON
output, including configurations, progress counters, serial selftest, and raw
log hashes, was preserved in [results.json](quarto-deno-712-native-litmus/results.json).
There were no discarded failures, incomplete trials, concurrent pilots, or
additional trial runs. No workload was increased after the zero results.

## What this added and what it did not

This established that the intended native contrast really ran: same arm64
image, same shared-slot layout, same iteration and delay bounds, and one
verified reader barrier difference. It added a concrete, reproducible hardware
experiment to the model and crash analysis. It did not produce a new crash or
a native witness of the model's problematic state.

Most iterations skipped the callback load. The experiment covered repeated
warm accesses by two workers, not the cold process startup and three SQLite
initializers visible in the reports. Thread placement, relative timing, and
branch prediction were uncontrolled. ABBA reduced a simple order confound;
it did not make iterations independent or identically distributed, or match
the two variants' scheduling. Zero-count statistical failure-rate bounds
would therefore have been unjustified.

The SQLite publication-ordering hypothesis remained the leading candidate
from the earlier crash and model evidence, without new positive hardware
support. Unexpected table reset, unrelated memory corruption, and missing
details of the real startup interleaving remained alternatives. These trials
did not reopen V8 or deno-dom as the immediate crash site; that question had
already been answered by symbolication. They also did not establish a matching
upstream fix, a safe production barrier patch, or a reason to change caches or
Quarto versions.

The highest-information missing observation remained the **pair of values
actually read by the failing Deno initializer**: retain the guard read that
selected the fast path together with the callback value immediately before
the indirect call, correlated with the same failure. That would distinguish
the proposed publication failure from an incorrect inference about the path.
A later re-read of the global guard would not substitute for its original
value. The existing `.ips` had overwritten that register, and the previous
LLDB launch had failed; obtaining this observation would require a workable
instrumented startup/debugging path. Repeating the warm litmus or full checks
until a passing count grew was not selected as the next step.

## Repository outcome and reproduction

Only investigation sources, evidence, and dated revision pointers were added.
No package/check implementation, installed Quarto/Deno, diagnostic retention,
retry policy, cache, or coverage gate changed. No upstream message or PR was
published and #712 was not closed. The work did not claim to have reproduced
the complete source-tarball `R CMD check --as-cran` failure.

The original scratch files, binary, and raw logs were under
`/private/tmp/marginplyr-712-native-litmus/`. The small source and evidence
files beside this note were retained because that directory is temporary.
To rerun on an arm64 Mac, compile the saved C and assembly with the command
above in a new scratch directory, record the new compiler/binary identity,
then run `./native-litmus selftest` followed by the four commands
`./native-litmus original 1000000`, `./native-litmus barrier 1000000`,
`./native-litmus barrier 1000000`, `./native-litmus original 1000000`.
Preserve every exit and log; a new run is separate evidence. The saved driver
additionally enforces file-hash checks and an outer timeout when supplied a
plan for that build. Never relabel a rebuilt binary as the recorded UUID/hash.

Independent review checked synchronization, emitted instructions, identities,
and counts without rerunning the trials. Document-reference and always-loaded
context-budget verifiers passed, as did saved-source/disassembly hash checks,
counter-total checks, and whitespace checks. No package validation was claimed
for these repository-only investigation artifacts.

## Revisions (2026-09-28)

The maintainer ended further root-cause investigation and accepted the
[bounded recovery policy](../design/specs/review-ready-native-crash-retry.md).
This superseded the recommendation to keep #712 open for more causal
evidence or further experiments. Closure was conditional on implementing
and verifying that policy under the [local-check contract](../design/agents/local-checks.md#one-retry-after-a-native-rendering-crash).
The historical failure mechanism remained unconfirmed; this disposition did
not alter the recorded evidence or claim that the crash had been prevented.
