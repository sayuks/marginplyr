# Follow-up evidence for the local Quarto/Deno crash

Investigated: 2026-09-28
Revised: 2026-09-28 — formal ordering check in `investigation/quarto-deno-712-arm-model.md`
Revised: 2026-09-28 (#712) — native comparison in `investigation/quarto-deno-712-native-litmus.md`
Revised: 2026-09-28 (#712, operational disposition) — operational disposition in `design/specs/review-ready-native-crash-retry.md`
Primary input: [#712](https://github.com/sayuks/marginplyr/issues/712), [PR #713](https://github.com/sayuks/marginplyr/pull/713)
Repository baseline: `e5be3359f4cc7c56256c2570009c55be62c0e710`

## Evidence map recorded before code changes or new render experiments

The issue body and discussion, PR description, both commits, all four changed
files, discussion and review record were read. GitHub returned no inline review
comments or formal reviews. The investigation also read the Review-ready entry
point, implementation and verifier, local-check contract, ADR 0030, and the
retained four-run harness, JSON records and console logs. The prior investigation
was treated as evidence rather than a recipe to repeat.

| Category | Evidence and limit |
| --- | --- |
| Established | #712 associated the September 28 SIGSEGV with `recipes.qmd` under the source-tarball check of `53446493aa2e4bf82e539000fe8629d3331a61f9`. Two existing `.ips` reports had the same arm64 Deno UUID and native stack offsets, on macOS 26.6.2 (`25G83`). The September 24 invocation was not independently correlated with Review-ready. |
| What #713 tested | Four standalone renders, in order: normal Deno cache, reuse, empty isolated Deno cache, reuse. Same archived SHA, temporary installed R library, Quarto 1.10.18 / Deno 2.7.14. All passed. Quarto's own cache stayed shared. Process inventories existed only at run boundaries. Two complete Review-ready validations on #713 commits also passed. |
| What #713 ruled out | Its deterministic fixtures established that the added failure-retention path preserved failures and did not retry. The four successful renders ruled out no intermittent crash mechanism. They did establish that the input did not fail on every rendering. |
| Still plausible | Native Deno/V8/Rust defects; Quarto startup or native/WASM integration; shared state; concurrency; resource pressure; check-specific environment or preceding work; input-sensitive transformations; arm64/macOS behavior; a damaged or incompatible bundled component. Their plausibility was unequal, not eliminated by passing runs. |
| Missing | A function identity for the recurring native frames; exact failing child argv/environment and effective cache paths; correlated process/resource observations at the failure; input-processing stage; matching upstream symbols/build identity or fix; a controlled reproduction. |
| Most discriminating | Resolve the common frames and instruction at the saved return address; inspect PC/ESR/registers and full stack; compare process lifetime with startup versus rendering; check loaded native components; compare bundled binary bytes with its official release artifact. These observations could select a subsystem before further renders. |

Existing evidence was under
`/private/tmp/marginplyr-712-experiment-mjom8hcr/` and
`~/Library/Logs/DiagnosticReports/deno-2026-09-{24-091840,28-101402}.ips`.
Follow-up scratch artifacts were assigned
`/private/tmp/marginplyr-712-followup/`; raw native reports and memory images
were not copied.

## Initial ranked hypotheses and discriminating observations

This ranking preceded new render experiments. None of the successful #713 runs
was counted as evidence against an intermittent mechanism.

| Rank | Hypothesis | For / against in available evidence | #713 coverage | Smallest new discriminator |
| --- | --- | --- | --- | --- |
| 1 | Deno native runtime or startup defect (Rust, V8, or a linked library) | Identical native stack on a Tokio worker and a null execution address; no function identity yet. A runtime crash can still be triggered by Quarto. | UUID and leading offsets only; stripped `atos`/`nm` failed. | Disassemble LR minus one instruction, map all frames to functions/source or exact build artifacts. |
| 2 | Quarto interaction with bundled Deno, including native deno-dom/FFI versus WASM | Both reports used the build bundled with Quarto; September 28 was associated with Quarto, while the September 24 invocation was unverified. | No native/WASM contrast. | Establish process lifetime and native images, inspect launcher/init source, verify path selection before designing one bounded contrast. |
| 3 | Shared Deno/Quarto cache or concurrent processes | State outlived disposable archives; no failure-time process/cache evidence. | Deno cache only, four successful renders; shared Quarto cache and boundary process inventories. | First identify whether the crashing subsystem accesses shared state; record effective paths and child identity in a targeted run. |
| 4 | R CMD check environment or preceding Review-ready work | Known failure reached check; standalone render is a different invocation. No controlled comparison linked it to check. | Standalone renders and separate passing full gates. | Read the actual vignette engine/argv/env chain; compare only a causally relevant difference. |
| 5 | Input-sensitive recipes transformation | Report named this vignette; subsequent renders passed. Neither establishes input dependence. | Four renders of the same committed input. | Determine whether the failing process had time/reached code to inspect this input. |
| 6 | arm64/macOS-specific behavior or incompatible/corrupt bundled native component | Both crashes shared OS/build/architecture/binary; no cross-host control or integrity proof. | Version/UUID observed, no official byte comparison. | Match official Deno/Quarto artifact hashes and map instructions to architecture-specific source. |
| 7 | Host memory/resource pressure | Preceding full suite could affect host state; no contemporaneous measurement. SIGSEGV alone does not establish exhaustion. | No resource-pressure control. | Inspect fault semantics first; seek a concrete allocation/resource path before pressure experiments. |

No production mitigation was selected from this initial evidence. The user's
bounded, evidence-first investigation took precedence over the generic bug
skill's repeated stress/reproduction-first workflow.

## Conclusion after native analysis

The immediate failure site was identified, and the mechanism was materially
narrowed: both reports described a NULL indirect call from **SQLite's
`sqlite3MutexInit` during Deno's concurrent eager cache initialization**.
They did not describe a faulting V8 frame or a deno-dom parsing frame.
The underlying reason the mutex callback was NULL remained unproved.
An arm64 memory-publication race was the leading specific hypothesis;
no matching, verified upstream fix or package mitigation was established.

### Symbolication and build identity

The installed executable was
`/Users/sayuks/.local/share/quarto/1.10.18/bin/tools/aarch64/deno`:
Deno 2.7.14, V8 `14.7.173.20-rusty`, TypeScript 5.9.2, arm64.
The Deno tag resolved to `2d674b25625bcc367853d00fe86f6e84390f88cb`.
Quarto was 1.10.18, R 4.6.1, and the R quarto package 1.5.1.

The absence of useful symbols in the stripped executable was overcome using
Deno's [release CI](https://github.com/denoland/deno/blob/v2.7.14/.github/workflows/ci.ts):
it generated a symcache before stripping and published it separately from
GitHub release assets. The
[official panic symbolicator](https://github.com/denoland/panic/blob/bebb25af952b81063c8e4b9e97b9e167af0cdfa4/www/routes/%5Bversion%5D/%5Btarget%5D/%5Btrace%5D.tsx)
identified the
[v2.7.14 arm64 macOS symcache](https://dl.deno.land/release/v2.7.14/deno-aarch64-apple-darwin.symcache).
Its DebugId UUID matched **both reports and the installed executable**:
`4c4c4435-5555-3144-a10b-c1c14dc2d9bc`. Decoding ran locally with the
symbolicator's WASM implementation; no native report was uploaded.

An official v2.7.14 Deno release executable was downloaded for a static
comparison, not installed or substituted into Quarto. Every named Mach-O
section's bytes agreed, including executable code, constants and initialized
data. Differences were confined to code-signature material in `__LINKEDIT`
and the corresponding load-command size fields. The installed executable was
signed by RStudio, the upstream executable by Deno Land. Thus whole-file hash
inequality did not indicate code corruption.

| Object | SHA-256 |
| --- | --- |
| Installed Quarto-bundled Deno | `392c815816cab54d1ed941c1e4dbca8c5cb92893e8a460117f3f722b3ace3173` |
| Official v2.7.14 Deno executable | `523144333a566adc29f17eebdab6526f592dd95ff0f3f63238054dafc3770a26` |
| Identical `__TEXT.__text`, 57,280,116 bytes | `d765995903cfc3f0c81d7d1e2c925f76495b3f4fd33060a63f28d90488d05dac` |
| Official matching symcache | `f7eb80b526ebfe7f12c1f71497b21d8d2cb584f42a0deac6d3621151a8d5b940` |

This ruled out changed/corrupted on-disk instruction bytes relative to that
official build. It did not establish integrity of past process memory or of
every other Quarto component.

### Native reports and the complete faulting stack

| Observation | September 24 | September 28 |
| --- | --- | --- |
| Report | `deno-2026-09-24-091840.ips` | `deno-2026-09-28-101402.ips` |
| Launch, +0900 | `09:18:38.4335` | `10:14:00.7794` |
| Capture, +0900 | `09:18:38.5802` | `10:14:00.8546` |
| Launch-to-capture interval | 146.7 ms | 75.2 ms |
| PID / faulting thread index | 96365 / 11 | 54342 / 12 |
| Deno image base | `0x100ce4000` | `0x102c48000` |
| Saved LR | `0x102964fd4` | `0x1048c8fd4` |
| Saved FP = SP | `0x16fd9a380` | `0x16e042380` |

Both reported native ARM-64 execution (`translated=false`), macOS 26.6.2
build `25G83`, `EXC_BAD_ACCESS` / `SIGSEGV`, codes `(1, 0)`,
`KERN_INVALID_ADDRESS at 0x0000000000000000`, and signal termination 11.
The crashed thread was named `tokio-runtime-worker`. **PC, FAR, and x8 were
all zero**, with ESR `0x82000006`, `(Instruction Abort) Translation fault`.
The first frame's zero UUID/base/size was an unresolved-address placeholder,
not a loaded native library. The stack pointer was aligned and the same
complete native call sequence was present in both reports; neither fact alone
excluded memory corruption. The reports supplied no contemporaneous host
memory-pressure measurement or useful VM allocation summary.

All twelve Deno return offsets agreed, followed by the same pthread frames.
The table summarizes inline Rust frames; complete decoder output was retained
as `symbolicated-offsets.json` in the scratch directory.

| Frame | Decimal image offset | Function or enclosing call path |
| --- | --- | --- |
| 0 | 0, unresolved image | NULL instruction address |
| 1 | 29888468 | `sqlite3MutexInit`; return address at its post-call `sqlite3MemoryBarrier` |
| 2 | 29886848 | `sqlite3_initialize` |
| 3 | 29888192 | `sqlite3_mutex_alloc` |
| 4 | 23717972 | `ensure_safe_sqlite_threading_mode` in `rusqlite::InnerConnection::open_with_flags` |
| 5 | 2798292 | `Connection::open` / `CacheDB::actually_open_connection` / `open_connection_and_init` |
| 6 | 2796796 | `CacheDB::open_connection` / `initialize` / `OnceCell::initialize` |
| 7 | 22493920 | `once_cell::imp::initialize_or_wait` |
| 8 | 2794828 | `once_cell::sync::OnceCell::get_or_try_init` |
| 9 | 2880900 | `CacheDB::spawn_eager_init_thread` / `spawn_blocking` / Tokio task polling |
| 10 | 25617872 | Tokio blocking-pool worker / `std::sys::backtrace::__rust_begin_short_backtrace` |
| 11 | 25634576 | blocking-pool thread closure / `FnOnce` shim |
| 12 | 24463852 | `std::sys::thread::unix::Thread::new::thread_start` |
| 13 | 27736, pthread image | `_pthread_start + 136` |
| 14 | 7196, pthread image | `thread_start + 8` |

Saved return addresses identify the instruction **after** a call. Looking up
29888464 (four bytes earlier) mapped the actual `blr x8` to
`sqlite3MutexInit`'s `sqlite3GlobalConfig.mutex.xMutexInit()` call in the
bundled `libsqlite3-sys 0.35.0` SQLite 3.50.2 amalgamation. The return address's
inline `sqlite3MemoryBarrier` name must not be mistaken for the instruction
that faulted.

The complete reported image lists comprised Deno, dyld,
`libsystem_kernel.dylib`, `libsystem_pthread.dylib`, the unresolved placeholder,
and, on September 28, `libsystem_platform.dylib`. Relevant UUIDs were:
dyld `74e52480-c2bd-3c8d-812d-95fe2b74a096`, kernel
`c6a4a4cb-92e6-3baf-aae0-e8306259209a`, pthread
`a373f0b0-9880-326a-88b4-dd8be4e33072`, and platform
`edb83a19-ec17-32de-9350-6145970a85d6`. No deno-dom/plugin image appeared.
This is an observation about these reports, not a guarantee that the image
inventory records every library ever loaded or unloaded.

### Other threads distinguish process concurrency from thread concurrency

In each report, two additional Tokio workers shared the same cache-opening
ancestry. On September 24, thread 12 was inside
`sqlite3_initialize` / `sqlite3_os_init` / `sqlite3_vfs_register`
(offsets 29887772, 29887816, 29899716, 29887384); thread 13 was in
`__psynch_mutexwait` beneath `sqlite3_initialize` (29887228).
On September 28, thread 11 was at that same mutex wait, while thread 13 was
registering SQLite builtins (`sqlite3StrICmp`, `sqlite3FunctionSearch`,
`sqlite3InsertBuiltinFuncs`, `sqlite3_initialize`; offsets 29899524, 30423804,
30384552, 29887348). All ten V8 worker threads were in condition waits.

This established overlapping SQLite initialization inside **one** Deno
process in each saved failure. It did not depend on reproducing a SIGSEGV.
Symbolicating both main threads also placed them in `run_script` / factory
workspace and npm setup / `NpmRegistryUrl::from_env` (URL parsing on September
24, environment lookup on September 28), strengthening the startup-stage
interpretation rather than an HTML transformation already in progress.

[Deno's `CliFactory::caches`](https://github.com/denoland/deno/blob/v2.7.14/cli/factory.rs)
eagerly opened dependency-analysis, Node-analysis and V8-code-cache databases
for `run`. Quarto's release launcher used `--no-check`, so the additional
type-checking caches did not apply. Each
[`CacheDB`](https://github.com/denoland/deno/blob/v2.7.14/cli/cache/cache_db.rs)
had an independent lock/OnceCell and a `spawn_blocking` initializer; the
per-database lock did not serialize process-wide SQLite initialization.
[rusqlite's threading-mode check](https://github.com/rusqlite/rusqlite/blob/v0.37.0/src/inner_connection.rs)
called `sqlite3_mutex_alloc(0)` **before `sqlite3_open_v2`**. Thus the faulting
call had not reached reading its database's contents or handling WAL errors.

Changing `DENO_DIR` changes storage location, not that concurrency. Absence
of other processes does not serialize these threads. `--no-code-cache` removes
one of the three warmups, leaving two, and `deno eval` does not enter the same
explicit warmup branch as `deno run`. These are reasons not to interpret such
controls as equivalent experiments.

### Specific remaining mechanism: publication/read ordering

The matching binary's relevant code, with addresses unslid to image base
`0x100000000`, was:

```asm
0x101c80efc  ldr  x8, [x8, #0x608]   ; xMutexAlloc
0x101c80f00  cbnz x8, 0x101c80fc8
; slow path writes default methods, including xMutexInit
0x101c80fac  dmb  ish
0x101c80fc4  stur x9, [x8, #0x6c]    ; publishes xMutexAlloc last
0x101c80fc8  adrp x8, 16757
0x101c80fcc  ldr  x8, [x8, #0x5f8]   ; xMutexInit
0x101c80fd0  blr  x8                 ; x8 = 0 in both reports
0x101c80fd4  dmb  ish
```

The writer's stores and barrier agreed with
[SQLite 3.50.2 `sqlite3MutexInit`](https://github.com/sqlite/sqlite/blob/version-3.50.2/src/mutex.c).
The reader's already-published branch contained ordinary loads separated by
a control dependency, with no acquire load or reader barrier before using
the callback. Arm's
[Memory Systems, Ordering, and Barriers guide, “Limits on reordering”](https://documentation-service.arm.com/static/62a304f231ea212bb662321d)
explains that a control dependency between loads does not guarantee ordering.
Consequently, observing a published allocation method while reading an older
NULL initialization callback was a concrete candidate; ordered writer stores
alone did not eliminate that candidate. The barrier after the indirect call
could not protect that call.

This remained an inference. The report's x8 had already been overwritten by
the callback load, so it did **not** preserve the earlier xMutexAlloc value
or prove which branch had run. No instruction-execution trace, formal model
result, or controlled failure established the CPU-level ordering. An
unexpected writer, configuration race or memory corruption remained possible.
There was additional instruction-level evidence for the fast path: both
reports preserved x10=57 and x9 outside the Deno text image, whereas the slow
path would leave both registers holding non-NULL mutex-allocation code
addresses immediately before the callback load. That supports taking the
published-method branch, assuming the saved register state is reliable; it
does not recover the guard's exact value or prove hardware reordering.

### Upstream matches and non-matches

| Source | What it added and why it did or did not match |
| --- | --- |
| [gglib #1064](https://github.com/mmogr/gglib/issues/1064) | Independent Apple Silicon report with simultaneous first SQLite connections, PC=0 and a NULL `xMutexInit` indirect call. This matched a specific signature, unlike a generic Deno segfault. It used macOS 27 and libsqlite3-sys 0.30.1, had no confirmed cause/fix, and did not prove the same root cause. |
| [Deno #18401](https://github.com/denoland/deno/pull/18401) | Introduced off-main-thread SQLite cache initialization for startup performance in 2023. Explained why concurrent entry existed, not a confirmed regression boundary. |
| [Deno #34873](https://github.com/denoland/deno/pull/34873) | Later shared-cache fix handled returned `SQLITE_BUSY`/recovery errors instead of deleting a busy file. Those operations occurred after the initialization point at issue, so it was not a matched fix. |
| [SQLite 2015 barrier change](https://github.com/sqlite/sqlite/commit/6081c1dbdf7730752bbde89ebb17d01bb30bf8f0) and [2020 change](https://github.com/sqlite/sqlite/commit/ffd3fd0c30d7b096adb54ccd8b0c8b907c86e803) | Added writer-publication and later initialization barriers. Both were already present in the observed binary; neither was an unapplied fix. The 2020 mutex barrier was after the callback. |
| [Quarto #14749](https://github.com/quarto-dev/quarto-cli/issues/14749) | Same-version startup symptom ultimately traced to `NODE_OPTIONS`/stdin handling and a hang, not this native fault. |
| [Quarto #14945](https://github.com/quarto-dev/quarto-cli/pull/14945) | #713's early-startup SIGSEGV report became more relevant given measured process lifetimes, but supplied no matching native stack or verified fix. |
| Deno #33374/#33375 and #34561 from #713 | The event-loop-delay fix was already included; the FFI change did not identify this SQLite call chain. No new evidence linked them to this fault. |

The bounded search covered Deno/Quarto/rusqlite issues using SQLite, mutex,
initialization, race, SIGSEGV and exact callback names, plus cache-file history
and SQLite mutex history. It established no matching unapplied fix, without
claiming that no such fix could exist. A broad V8 version sweep lost priority
once exact symbols identified SQLite. No arbitrary version comparison was run.

### Invocation differences retained as limits of #713

The R quarto 1.5.1 `quarto::html` vignette engine passed `output_format="html"`
and metadata setting `embed-resources=TRUE`, `minimal=TRUE`, `theme="none"`
and its vignette CSS. #713's standalone `quarto_render` calls omitted those
overrides. R CMD build/check and rcmdcheck also established different R library,
profile and `R_TESTS` conditions. Their exact values at the original failure
were not recoverable. These differences remained real, although a
recipes-specific HTML transformation was a lower-priority explanation for
the symbolicated startup fault.

Quarto's [deno-dom adapter](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/src/core/deno-dom.ts)
tried native `Deno.dlopen` and fell back to WASM on a loading error. Its
[launcher](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/package/scripts/common/quarto)
set `DENO_DOM_PLUGIN` from `QUARTO_DENO_DOM`; overriding `DENO_DOM_PLUGIN`
alone would not establish the desired contrast. A missing temporary
`QUARTO_DENO_DOM` path plus debug messages could verify fallback selection,
but that experiment was not performed after SQLite was identified.

## Ranked hypotheses after the new evidence

The initial table records #713's coverage. The following ranking incorporates
the symbolicated failures; lack of reproduction is not a negative finding.

| Rank | Hypothesis | Supporting evidence | Contrary evidence / limit | Smallest useful next discriminator |
| --- | --- | --- | --- | --- |
| 1 | SQLite mutex-method publication/read-order race during Deno startup on arm64 | Same NULL callback, simultaneous initializers in both failures, reader lacks acquire/barrier, independent matching SQLite signature. | Actual memory ordering was not observed; no controlled reproduction or confirmed upstream fix. | Model the exact writer/reader instructions under the Arm memory model, contrasting only a reader barrier before callback load; then reproduce the relevant SQLite path rather than full Review-ready. |
| 2 | Another writer/configuration race or memory corruption affecting SQLite's global mutex table | A zero callback and concurrent initialization are observed; saved reports lack the table contents/write history. | Matching on-disk code and repeatable call chain provide no evidence of arbitrary corruption; no offending writer was identified. | Trace writes to the mutex-method slots and calls to `sqlite3_config`/shutdown in a minimal Deno startup with an authorized working debugger. |
| 3 | Check environment, shared-state timing, other processes, or resource pressure triggers the startup race | Those conditions can change scheduling; original argv/environment/process-pressure evidence is missing. | They do not explain the NULL callback by themselves. Single-process parallelism is already sufficient to expose concurrent entry. No allocation-failure/OOM evidence appeared. | Once the initialization race is testable, vary one timing/environment condition while holding the native path fixed; do not repeat the entire gate. |
| 4 | Shared cache contents/WAL corruption is the direct fault | Deno is opening cache connections. | Fault occurs during library mutex setup before the failing connection calls `sqlite3_open_v2`; filesystem-cache errors are downstream. Empty-cache success in #713 was not the reason this was downgraded. | Inspect global-init failure in a harness using no database files; distinguish it from a returned database error. |
| 5 | Quarto native deno-dom/FFI failure, incompatible plugin, or recipes-specific transformation | September 28 was associated with Quarto; standalone/check transformations differ. September 24's invocation remained unverified. | Exact stack is Deno's SQLite startup, main threads are still in factory/npm setup, plugin image absent, lifetime below 150 ms. | Only revisit native/WASM or input contrasts if a subsequent report reaches the adapter/parser; verify selection in its debug log. |
| 6 | Direct V8 runtime defect | V8 is bundled and running worker threads. | Fault and callers map to SQLite/Rust cache startup; V8 workers wait. Indirect corruption remains logically possible without evidence. | A future V8 frame or sanitizer trace would be needed to promote this hypothesis. |
| 7 | Corrupted/incompatible Quarto-bundled Deno instruction bytes | Same bundled executable recurs. | All code/data sections matched official Deno 2.7.14; only signing differed. | No reinstall/version swap was warranted; future changed hashes or a matching upstream runtime fix would reopen this branch. |

Architecture specificity remained part of rank 1, not a proven macOS-only
restriction: both failures were arm64/macOS and there was no x86/Linux control.
Quarto could trigger a Deno startup defect without itself implementing the
faulty native code. Cache isolation and native/WASM switches would leave that
startup subsystem present.

## Experiments, commands and artifacts

The repository was inspected at
`e5be3359f4cc7c56256c2570009c55be62c0e710`, the head of draft #713.
The original failing package/input SHA remained
`53446493aa2e4bf82e539000fe8629d3331a61f9`; no rerender of that input or
full Review-ready/check run was added in this follow-up.

| Order | Observation / changed condition | Count and result |
| --- | --- | --- |
| 1 | Offline inspection of the two existing `.ips` files; LLDB disassembly without launching Deno | Both full fault stacks and other initializing threads mapped; NULL `blr x8` established. This was crash analysis, not reproduction. |
| 2 | Same-version official symcache and executable comparison | One symcache and one release executable retrieved. UUID matched; named sections matched. No alternate runtime version executed. |
| 3 | Read-only normal-path Deno metadata | `deno info --json` reported effective `denoDir=/Users/sayuks/Library/Caches/deno`. This did not retroactively validate every #713 run's effective path. |
| 4 | Minimal Deno startup, without Quarto, R, package input or shared Deno cache | One debugger launch attempt, after one isolated-cache `info --json` preflight. Preflight exit 0; LLDB exit 1 with `process exited with status -1 (no such process)`. No initialization breakpoint result, no SIGSEGV reproduction, and no successful workload result. No repeat. |

For observation 4, UTC start/end were
`2026-09-28T02:36:49.034662Z` / `2026-09-28T02:36:54.378329Z`.
The executable/path/version/hash were the bundled Deno recorded above.
The fresh directory was
`/private/tmp/marginplyr-712-followup/minimal-run-1/cache`;
`deno info --json` confirmed that exact effective path. A preflight may touch
metadata, so this was not claimed as an untouched first-use cache experiment.
The generated local input's SHA-256 was
`d7780b804db5f657ba247c959b2d818f11636c6bf12af45a39b173b9a8919e16`.
The target command was:

```sh
DENO_DIR=/private/tmp/marginplyr-712-followup/minimal-run-1/cache \
  /Users/sayuks/.local/share/quarto/1.10.18/bin/tools/aarch64/deno \
  run --no-check --no-config --no-lock --cached-only empty.js
```

LLDB was configured to observe SQLite entry points; it failed at process
launch. The cause of the debugger launch error was not established, and OS
debugging/security settings were not changed. Even a successful debugger run
would change scheduling and could establish reachability, not the original
failure rate or hardware reorder. Routine `--version`/identity queries were
metadata observations, not successful reproduction trials.

The successful offline LLDB analysis did not require starting a process:

```sh
xcrun lldb --batch \
  -o 'settings set target.preload-symbols false' \
  -o 'target create --no-dependents /Users/sayuks/.local/share/quarto/1.10.18/bin/tools/aarch64/deno' \
  -o 'disassemble --start-address 0x101c8094c --count 440'
```

Symbol lookup used the official panic project's
[`SymCache.lookup` decoder](https://github.com/denoland/panic/blob/bebb25af952b81063c8e4b9e97b9e167af0cdfa4/src/symbolicate.rs)
and published WASM wrapper locally under Node 26.7.0. The cache header's
16-byte DebugId at offset 8 matched the UUID, using the
[version-8 symcache format](https://github.com/getsentry/symbolic/blob/12.15.5/symbolic-symcache/src/raw.rs).

All follow-up scratch artifacts were under
`/private/tmp/marginplyr-712-followup/`:

- `issue-712.json`, `pr-713.json`, `pr-713-commits-full.json`,
  `pr-713-review-comments.json`, `pr-713.diff`: prior-evidence snapshots.
- `deno-disassembly.txt`, `symbolicated-offsets.json`,
  `symbolicated-threads.json`: offline instruction/function analysis,
  including return addresses and return-address-minus-four lookups.
- `binary-comparison.json`, `deno-aarch64-apple-darwin.symcache`,
  `deno-upstream.zip`, `deno-upstream`: matching symbols and static comparison.
- `deno-source/`, `sqlite3.c`, `rusqlite-0.37.0-inner_connection.rs`,
  `r-invocation-source.txt`, `sqlite-2020-barrier.json`: primary source extracts.
- `minimal-run-1/metadata.json` and its debugger/preflight logs: the single
  unsuccessful launch attempt, with timestamps and effective cache path.
- `upstream-findings.md`, `invocation-findings.md`, `symbolication.md`:
  detailed research working notes.

These temporary files may be removed by the host. The essential crash identity,
offsets, instruction sequence, source links and conclusions were preserved in
this repository note. No native report/core image was copied into it or the
scratch artifacts.

## Repository outcome and next action

Only this investigation note was added. #713's diagnostic preservation and
every coverage, snapshot, vignette and `R CMD check --as-cran` gate were left
intact. No retry, cache deletion, version pin, plugin switch or speculative
production mitigation was introduced. The immediate native failure was
identified, but #712's prevention criterion remained unmet and the issue
needed to stay open. No tracker message or pull request was published.
Document-reference and always-loaded context-budget verifiers passed;
the note was checked for whitespace errors. This was a repository-only
investigation note, so no package validation was represented as having run.

The single highest-information next action was a **formal Arm memory-model
check of the exact SQLite publication/reader instruction sequence**, comparing
the original with a reader barrier before loading `xMutexInit`. A permitted
`xMutexAlloc != 0` / `xMutexInit == 0` outcome only in the original would
establish a concrete ordering defect in the sequence; it would still need
connection to the observed execution and upstream validation before claiming
a preventive fix. This was preferable to more successful full checks, cache
clearing, or unrelated Deno-version comparisons.

## Revisions (2026-09-28)

The proposed formal check was completed in
[the Arm ordering investigation](quarto-deno-712-arm-model.md). Six bounded
herd7 evaluations found the problematic publication/callback read values
permitted in the original sequence and forbidden with a reader barrier or
acquire guard load. This replaced the missing formal check with a concrete
model witness. It did not establish that the historical Apple CPU executions
took that witness, and the root cause of #712 remained unconfirmed.

## Revisions (2026-09-28, #712)

The subsequent [native comparison](quarto-deno-712-native-litmus.md) completed
four predeclared trials on the local M4 with no inconsistent read pair. It
added hardware measurements while leaving both the formal counterexample and
the uncertainty about the historical crash mechanism intact.

## Revisions (2026-09-28, #712, operational disposition)

The maintainer ended further root-cause investigation and accepted the
[bounded recovery policy](../design/specs/review-ready-native-crash-retry.md).
This superseded the recommendation to keep #712 open for more causal
evidence or further experiments. Closure was conditional on implementing
and verifying that policy under the [local-check contract](../design/agents/local-checks.md#one-retry-after-a-native-rendering-crash).
The historical failure mechanism remained unconfirmed; this disposition did
not alter the recorded evidence or claim that the crash had been prevented.
