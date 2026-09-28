# Formal Arm ordering check for SQLite startup in Deno

Investigated: 2026-09-28
Revised: 2026-09-28 — bounded hardware comparison in `investigation/quarto-deno-712-native-litmus.md`
Revised: 2026-09-28 (#712) — operational disposition in `design/specs/review-ready-native-crash-retry.md`
Primary input: [the preceding investigation](quarto-deno-712-followup.md), [#712](https://github.com/sayuks/marginplyr/issues/712), [PR #713](https://github.com/sayuks/marginplyr/pull/713)
Repository baseline: `e5be3359f4cc7c56256c2570009c55be62c0e710`

## Conclusion

The formal check found the proposed ordering failure **permitted by Arm's
model**: a reader could observe a published nonzero `xMutexAlloc`, skip
initialization, and then read zero from `xMutexInit`. Adding a reader barrier
before the second load forbade that outcome. Keeping only the existing
barrier after that load did not forbid it.

This established a counterexample to the publication protocol in the extracted
instruction sequence, under the stated model assumptions. It strengthened the
SQLite initialization-ordering hypothesis beyond the earlier source/disassembly
argument. It did **not** establish that either historical crash took this
execution on the Apple CPU, or that a particular source patch would prevent
#712. The root cause of the historical crashes remained unconfirmed.

## Starting evidence and scope

The previous investigation had already mapped both crash stacks to
`sqlite3MutexInit()` inside Deno's concurrent eager `CacheDB` initialization.
Both reports had `PC = FAR = x8 = 0` and a saved return address immediately after
the indirect `xMutexInit` call. Two other workers in each process were also
initializing SQLite. The exact executable was Quarto 1.10.18's arm64 Deno
2.7.14, UUID `4c4c4435-5555-3144-a10b-c1c14dc2d9bc`, with SQLite 3.50.2.
The prior note preserved full build identities, symbolication, and limits on
correlating the September 24 invocation with Quarto.

This follow-up tested the previously proposed next action. It did not repeat
the #713 rendering experiments. No Quarto, Deno workload, R check, or native
hardware litmus test was run in this step. The package SHA above identified
the repository baseline; the modeled code came from the already identified
bundled Deno binary, not a rebuilt marginplyr executable.

## Instruction abstraction

The relevant unslid Deno addresses were:

| Address | Instruction or role |
| --- | --- |
| `0x101c80efc` | Reader `LDR` of `xMutexAlloc` |
| `0x101c80f00` | `CBNZ` to the published-table path |
| `0x101c80f38` | Writer `STUR` of the nonzero `xMutexInit` pointer |
| `0x101c80fac` | Writer `DMB ISH` |
| `0x101c80fc4` | Writer `STUR` of the nonzero `xMutexAlloc` pointer |
| `0x101c80fc8` | Independent `ADRP` for the callback address |
| `0x101c80fcc` | Reader `LDR` of `xMutexInit` |
| `0x101c80fd0` | `BLR x8`, the null indirect call in the crash |
| `0x101c80fd4` | Existing post-call `DMB ISH` |

The two shared slots were separately aligned 64-bit locations, 16 bytes apart
(`0x105df55f8` and `0x105df5608`). The test represented null/non-null as 0/1
in `uint64_t init` and `uint64_t alloc`. It retained the ordering-relevant
loads, stores, conditional branch, and barriers. `STUR` addressing became
`STR` to the corresponding symbolic location. Other method-table stores and
stack bookkeeping were omitted. Normal shared memory in one inner-shareable
domain was assumed, as the model's output flag explicitly reported.

The writer represented the selected initialization path. A separate control
included its original zero/nonzero guard. The reader's guard value was copied
to `X6` for observation; `X8` was then overwritten with an independent base
before loading the callback, preserving the absence of an address dependency.
When the guard was zero, the test ended that reader path; the target condition
required a nonzero guard, so it did not use the omitted initialization path.

`BLR` and the exception were deliberately outside the model. The observation
was the callback value **before** the call. The existing post-call barrier was
represented as a post-load barrier, even on the null outcome: this was a
counterfactual completion, not a claim that a crashed process executed it.
Even this added trailing barrier failed to exclude the problematic read values.

## Tool and model identity

The unmodified [official herd7 web engine](https://diy.inria.fr/www/jerd.js)
was downloaded and executed locally through its published `runHerd` interface,
under Node `v26.7.0`, macOS 26.6.2 (`25G83`), arm64. Nothing from the crash
reports was submitted to a remote simulator. The engine's source hash was:

```text
5553987 bytes
SHA256 0fb55e9b6ee23521b08a6cea9cfc6145881337f6e73859760e9d30fb53ed721e
```

Its header identified `js_of_ocaml 5.4.0`; an exact herd release number or
engine source commit was not recovered. The executable asset hash, rather
than an inferred release label, identified the tool used. The driver followed
the official [web entry point](https://github.com/herd/herdtools7/blob/cd32a878db23656a24bc5cb31f71b04d1400a7c9/herd-www/jerd.ml).
Only `model aarch64.cat` was supplied as configuration; no model rules were
replaced, skipped, or weakened.

The embedded `aarch64.cat`, its recursive includes, and implicit `stdlib.cat`
(15 files) were extracted for identity checks, without modifying the engine.
Every file's recomputed Git blob hash matched the official
[herdtools7 tree at `cd32a878db23656a24bc5cb31f71b04d1400a7c9`](https://github.com/herd/herdtools7/tree/cd32a878db23656a24bc5cb31f71b04d1400a7c9/herd/libdir).
This identified the model contents, not the engine's build commit. The root
file also matched release 7.58, but six dependency files differed; describing
the whole execution as "herd7 7.58" would have been inaccurate.

[identity.json](quarto-deno-712-arm-model/identity.json) records all model hashes,
input and raw-output hashes, configuration, commands, runtime, and run order.
The model files retained Arm's authorship and licensing headers; their
semantics were used as distributed.

## Bounded experiments and results

Each of the following six tests was evaluated once, in this order. The target
condition in every test was `1:X6=1 /\ 1:X8=0`, meaning a nonzero publication
guard and a zero callback. These were exhaustive evaluations of small finite
programs, not repeated probabilistic reproduction runs.

| Order / input | Single comparison | Target outcome | Positive / negative witnesses |
| --- | --- | --- | --- |
| 1. [original](quarto-deno-712-arm-model/original.litmus) | Extracted protocol, existing trailing barrier retained | `Sometimes` | 1 / 2 |
| 2. [reader-dmb-ish](quarto-deno-712-arm-model/reader-dmb-ish.litmus) | Add `DMB ISH` before callback load, versus 1 | `Never` | 0 / 2 |
| 3. [reader-dmb-ishld](quarto-deno-712-arm-model/reader-dmb-ishld.litmus) | Use `DMB ISHLD` in place of the added full barrier, versus 2 | `Never` | 0 / 2 |
| 4. [reader-only](quarto-deno-712-arm-model/reader-only.litmus) | Remove writer barrier, retain reader `DMB ISH`, versus 2 | `Sometimes` | 1 / 2 |
| 5. [writer-guard](quarto-deno-712-arm-model/writer-guard.litmus) | Restore writer's initial `alloc` guard, versus 1 | `Sometimes` | 1 / 2 |
| 6. [reader-ldar](quarto-deno-712-arm-model/reader-ldar.litmus) | Change reader's first `LDR` to acquire `LDAR`, versus 1 | `Never` | 0 / 2 |

All six produced complete `Observation` results and empty stderr. The first
was a separate shell invocation with shell exit 0; its Node exit status was
not separately logged. The subsequent five had individual exit 0 and captured
timestamps in `run-log.json` (03:02:47–03:02:49 UTC). No failed evaluation or
hidden repeated attempt was discarded. Input generation and engine loading
before the first test did not evaluate a litmus.

[results.txt](quarto-deno-712-arm-model/results.txt) preserves the complete
non-graph output for all six. `Positive: 1` means one model witness, not one
real crash or a probability. The heading `Test ... Allowed` appeared even in
tests whose target outcome was forbidden; the `Observation` and witness counts
were the evidence for the table, consistent with the
[herd output documentation](https://diy.inria.fr/doc/herd.html).

## Why the contrast discriminates

The original [witness graph](quarto-deno-712-arm-model/original-witness.dot)
contained a control-dependency edge between the reader's loads, but no address
dependency. The publication store fed the first load, while the callback load
read the initial zero. The model's
[dependency and barrier rules](https://github.com/herd/herdtools7/blob/cd32a878db23656a24bc5cb31f71b04d1400a7c9/herd/libdir/aarch64hwreqs.cat)
did not turn that control dependency alone into an order between ordinary
loads. They did order the loads when the reader barrier was inserted.

For the bad outcome, the writer barrier ordered `W(init=1)` before
`W(alloc=1)`; the latter fed `R(alloc=1)`. Reading the old zero placed
`R(init=0)` before the new `W(init=1)` in the coherence-derived relation.
Adding reader ordering `R(alloc=1) -> R(init=0)` closed a forbidden cycle in
the model's `ob` relation. The original lacked that ordering edge. A barrier
after both reads could not supply it. The reader-only control demonstrated
that the successful contrast depended on the writer ordering too.

This was new evidence of an ordering weakness in the extracted SQLite sequence,
rather than a generic Deno/V8 crash resemblance. The modeled failure required
neither a corrupt cache file, deno-dom, input transformation, memory pressure,
nor another process. That independence did not prove those factors absent
from the historical executions; it showed they were unnecessary for this
candidate mechanism.

## Limits and next observation

The ordering hypothesis became the strongest explanation of the available
stack, registers, simultaneous SQLite initializers, and Arm instruction
sequence. Competing mechanisms such as an unexpected table reset or unrelated
memory corruption were not modeled or ruled out. The earlier low ranking of
V8, deno-dom, or `recipes.qmd` as the immediate failure site still depended on
the symbolicated crash evidence, not on these litmus outcomes.

A permitted architectural execution need not occur on a particular Apple
microarchitecture. This model did not establish scheduling, timing, historical
guard value, cache-line behavior, failure frequency, C/Rust language-level
data-race semantics, or safety of all SQLite initialization paths. No upstream
fix was newly matched by running it. The barrier and acquire variants were
diagnostic contrasts, not validated deployable mitigations.

The single next highest-information action was a **bounded native AArch64
litmus on the affected host**, using an assembly implementation of these
loads/stores and contrasting the original with the reader barrier. It should
count the exact `alloc != 0 && init == 0` observation, use synchronization
outside the measured window to prevent reset races, and preserve disassembly
and run bounds. An occurrence would connect the formal witness to this CPU;
zero occurrences would not refute the mechanism. This would precede another
full Review-ready run or a claim that a production change fixed #712.

## Reproduction and repository outcome

The original scratch directory was `/private/tmp/marginplyr-712-arm-model/`.
It retained the official engine, extracted models, provenance comparisons,
six complete stdout files including graphs, six stderr files, input generator,
driver, and five-run timestamp log. These host-temporary files were not assumed
durable; the small inputs, essential output, witness, and identities were
preserved beside this note.

The actual driver was:

```javascript
const fs = require('node:fs');
global.herd_output = text => process.stdout.write(text);
global.herd_stderr = text => process.stderr.write(text);
require('./jerd.js');
const litmus = fs.readFileSync(process.argv[2], 'utf8');
runHerd('', '', litmus, 'model aarch64.cat\n');
```

After obtaining `jerd.js` from the source above and checking its SHA256 against
the recorded identity, place that driver beside it as `run-herd.cjs`, then run
`node /path/to/run-herd.cjs /path/to/original.litmus` (or another saved input).
A later changed web asset is a different tool build; do not silently label
its result as this run. A native herd build can instead use the pinned model
tree and these same litmus files, recording its own engine identity.

Only investigation records were added. The preceding note received a dated
revision pointer; no previous finding was deleted. No package behavior,
Review-ready diagnostics, retry policy, cache, executable installation, or
check coverage changed, and no issue/PR message was sent. No speculative
mitigation was justified. #712 remained unresolved.

Document-reference and always-loaded context-budget verifiers passed. Saved
litmus inputs matched their recorded SHA256 values; whitespace checks passed
for the notes and evidence files. These were repository-only investigation
artifacts, so no package or Review-ready validation was claimed.

## Revisions (2026-09-28)

The proposed hardware comparison was completed in
[the native litmus investigation](quarto-deno-712-native-litmus.md). Four
predeclared trials on the local M4 completed one million iterations each,
with no inconsistent read pair in either original or reader-barrier variants.
The original variant reached its callback load 104,448 times. This added a
bounded non-reproduction, not a refutation of the formal witness or proof of
a preventive fix. The historical failure mechanism remained unconfirmed.

## Revisions (2026-09-28, #712)

The maintainer ended further root-cause investigation and accepted the
[bounded recovery policy](../design/specs/review-ready-native-crash-retry.md).
This superseded the recommendation to keep #712 open for more causal
evidence or further experiments. Closure was conditional on implementing
and verifying that policy under the [local-check contract](../design/agents/local-checks.md#one-retry-after-a-native-rendering-crash).
The historical failure mechanism remained unconfirmed; this disposition did
not alter the recorded evidence or claim that the crash had been prevented.
