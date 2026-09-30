# Cause-preserving interruption notification

Investigated: 2026-09-30
Revised: 2026-09-30 — `investigation/rds-text-conversion-2026-09-30.md`
Repository baseline: `3f0b4e58d29cfcafff6bd38cfb93546816ed7dcc`
Scope: controlled R condition mechanics; no production package change or new SIGINT

## Question and environment

The question was whether an interrupt carrying an earlier execution error could
reach interrupt handlers without becoming an ordinary error, while retaining
native interruption behavior when no exiting handler took it. R 4.6.1 on Darwin
25.6.0 arm64 and rlang 1.3.0 were used. The scripts did not load marginplyr or
execute SQLite work; jsonlite encoded observations.

## Observations

Seventy separate `Rscript --vanilla` processes crossed two routes, seven handler
arrangements, and five native-hook configurations. Every assertion passed.
The candidate route notified an enriched condition with `signalCondition()`
and called the exported `rlang::interrupt()` only if that notification returned.
The comparison route called `rlang::interrupt()` directly.

| Boundary | Recorded observations |
| --- | --- |
| Exiting interrupt handler, with or without calling handler | Received the identical enriched condition, its original interrupt, original execution error and existing parent chain, and separate replay error |
| Exiting general condition handler | Received the identical enriched object before native fallback |
| Error-only handler | Was never called; interruption followed the native-control exit and hook behavior |
| No exiting handler | Process outcome and hook invocation matched the native control |
| Returning local calling or global handler | Received the enriched notification followed by a bare native notification |
| Original execution error | Serialized object remained unchanged |

Hook configurations were the default, function and expression `options(error)`,
function `options(interrupt)`, and both functions together. The candidate and
native control agreed on process exit and the invoked hook in each corresponding
case. With both set, the interrupt hook was invoked on this R build. Exact
terminal wording was not used as an acceptance assertion.

These observations established a candidate using public functions. They did not
establish that one cancellation produces exactly one notification to returning
handlers, and the observed fallback produced two. An exiting handler took the
first enriched object and prevented fallback.

## Primary sources read

- [Base condition handling](https://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html)
  described class-based calling and exiting handler dispatch and generic
  condition notification.
- [Base options](https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html)
  described configurable interruption/error handling.
- [rlang interruption](https://rlang.r-lib.org/reference/interrupt.html) and
  [condition signaling](https://rlang.r-lib.org/reference/cnd_signal.html)
  described their public interrupt APIs. A custom object was not an argument
  accepted by `interrupt()`.
- The versioned rlang source at
  [`ce8a147b712c425fa4eb1d351e621d07ce6e423b`](https://github.com/r-lib/rlang/blob/ce8a147b712c425fa4eb1d351e621d07ce6e423b/R/cnd-signal.R),
  `cnd_signal()`'s interrupt branch, called `interrupt()` rather than forwarding
  the enriched object. The companion native implementation was identified in
  the archived source metadata.
- The R Core source mirror at
  [`dd52bae9b393a268965d05bfb824cffdfb8d17e9`](https://github.com/wch/r-source/blob/dd52bae9b393a268965d05bfb824cffdfb8d17e9/src/main/errors.c),
  `getInterruptCondition()`, `signalInterrupt()`, and `onintrEx()`, explained
  fresh native notifications and hook selection. This was explanatory source
  evidence, not provenance of the installed R binary.

## Evidence and reproduction

[evidence.rds](https://github.com/sayuks/marginplyr/blob/056bae562385e627d6eb4ca2410183d594c0960f/investigation/condition-resignaling-2026-09-30/evidence.rds) retained runnable
`probe.R` and `run.py`, complete stdout/stderr/exit/event records for all 70
cases, summary assertions, script hashes, environment metadata, source URLs and
identities, and the source-reading notes. Its `files` list mapped names to raw
bytes. The environment omitted local host and login identifiers; R, dependency,
OS and architecture metadata were retained. Reading the archive did not execute
the scripts.

To reproduce in a disposable directory with R, rlang, jsonlite and Python:

```r
archive <- readRDS("investigation/condition-resignaling-2026-09-30/evidence.rds")
out <- tempfile("condition-resignaling-")
dir.create(out)
for (name in names(archive$files)) {
  writeBin(archive$files[[name]], file.path(out, name))
}
system2("python3", file.path(out, "run.py"))
```

The supervisor used a 15-second timeout for each newly launched process and
captured exit status and both output streams. It sent no operating-system
signal. A reproduction measured the local environment rather than reproducing
the recorded versions automatically.

## Limits

The probe did not integrate the SQLite savepoint owner or warning-replay loop,
drain a deferred SIGINT, exercise a second interrupt, verify arbitrary
handler-initiated nonlocal transfers, or establish other R/OS/dependency versions.
The stored replay error was a diagnostic payload, not a real warning conversion
inside a Margin operation. Resource restoration and true SIGINT precedence
remained separate implementation acceptance questions.

## Revisions (2026-09-30)

The [text-conversion investigation](rds-text-conversion-2026-09-30.md) established
that the archive's contents could be retained as text without loss. The original
archive above remains available at its pinned Git snapshot. Its files were
extracted byte-for-byte, and its nested environment and archive metadata were
stored as `.dput` with strict round-trip checks. The [evidence README](condition-resignaling-2026-09-30/README.md)
replaces the archive-extraction reproduction command above. The observations
and their limits were unchanged.
