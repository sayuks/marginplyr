# Public native APIs and warning-replay protocols

Investigated: 2026-10-01

## Finding

Further public-API combinations were tested for the external warning-handler
failure left open by #756/#764. None was a complete implementation of the
accepted [specification](../design/specs/competing-conditions.md). The
continuation experiment did not recover the condition; isolated callback replay
recovered it in ordinary nested handlers but changed handler availability and
caller restart behavior; exit-time `returnValue()` supplied only its default
for escaping failures. These results bound the tested mechanisms; they do
not establish that every supported architecture is impossible.

The official R-devel manual read on this date identified itself as R 4.7.0
Under development, 2026-09-25. Its *Condition handling and cleanup code* section
documented the same catchers, continuation protocol, and top-level isolation;
that section supplied no additional handler-stack registration or inspection
API. This was a check of that section, not an exhaustive API absence proof.

The [raw bundle](condition-handler-native-api-2026-10-01.rds) retains the minimal
C++/R sources and terminal outputs. All eight compilation/probe invocations
terminated with exit 0 on R 4.6.1, aarch64 macOS. Assertions in the unsuccessful
candidates establish their counterexamples, rather than acceptance of the
condition contract. No production code, ADR, specification, or existing note
was changed by these experiments.

## Continuing unwind inside a new catcher

[Writing R Extensions][r-exts] documents the C++ use of `R_UnwindProtect()`:
its cleanup callback may throw a C++ exception, after which `R_ContinueUnwind()`
resumes R's transfer. The experiment caught that C++ exception, installed a new
`R_tryCatch()` for errors and interrupts, and called `R_ContinueUnwind()` inside
it. This used the documented token protocol without reading its representation.

For custom errors and custom catchable interrupts, with single and nested caller
warning handlers, cleanup and continuation were reached, but the new catcher
never ran. The original outer catcher received the exact original object,
including its parent/environment payload. An `R_withCallingErrorHandler()`
control likewise did not observe the caller handler's failure. Normal return
was preserved.

The inspected [`R_ContinueUnwind()` implementation][context] resumes the saved
jump target directly through `R_jumpctxt()`; it does not signal the condition
again. Thus adding a catcher around continuation did not turn the transfer into
new handler dispatch. The manual supplies no condition getter for the token;
examining its private contents would be the unsupported representation dependency
already excluded by the [earlier investigation](condition-handler-replay-2026-09-30.md).

## Isolating and re-registering original callbacks

The second experiment obtained callbacks through documented
`withCallingHandlers(expr, ...)` arguments: it identified active calls with
[`sys.function()`/`sys.frame()`][sys] and evaluated `list(...)` in each matching
frame. Already forced handler promises stayed cached; the two constructor side
effects ran exactly twice. It also read `globalCallingHandlers()` through its
[documented getter][conditions]. It read no private handler locals or native
stack layout.

A small public `R_ToplevelExec()` wrapper evaluated replay in an isolated top
level. An outer protected list held its result, and an abnormal return was
explicitly `list(FALSE, NULL)`. Normal environment identity, GC-torture result
retention, restoration of caller handlers after return, and an unchanged `warn`
option passed controls. Inside isolation, the original callback functions were
registered above a package catcher, and ordinary `warning()` supplied dispatch
and the native `muffleWarning` restart.

With an older outer caller `tryCatch()`, the candidate retained the exact custom
handler error and environment payload; nested warning callbacks observed their
original order. Muffling at `warn = 2` prevented conversion, and a self-delivered
actual SIGINT within a callback remained interruptible and was caught as an
interrupt. This was a minimal replay experiment, not a supervisor checkpoint,
public-summary acceptance, or native-hook matrix.

Two native behaviors were not preserved:

- **Availability:** during a selected older calling handler, newer handlers and
  that selected handler remained live R frames but were unavailable for nested
  dispatch. Snapshot replay re-registered both, producing extra observations;
  the native control did not call them. The [condition documentation][conditions]
  specifies that a calling handler runs with only handlers below itself
  available. A list of live call frames was therefore not a list of available
  handlers.
- **Caller restart tokens:** an original token obtained with
  `findRestart("caller_escape")` failed inside isolation with
  `restart not on stack`, then worked after isolated return. The manual says
  that `R_ToplevelExec()` hides current top-level features. In the inspected
  [context implementation][context] it clears `R_RestartStack`, and
  [`invokeRestart()`][errors] requires the token's exit to be present on that
  stack. Merely retaining the original callback closure and token did not retain
  the restart protocol.

Further public-API variants addressed each counterexample partially. A
call-frame filter matching callback identity, a function-valued call head, the
condition class, and parent frame zero removed the unavailable callbacks in the
first example. An ordinary public `do.call(callback, list(cnd), envir = .GlobalEnv)`
then reproduced that pattern while the registered handler was still available;
the filter falsely removed it. Its native control observed the replayed warning.

A local proxy restart with the caller's name returned a transfer request from
isolation, and replay invoked the original restart after isolated return. That
named request reached the original restart handler once, in its original
environment, without changing `warn`. A callback invoking a previously captured
original token bypassed the proxy and still failed with `restart not on stack`.

These are defects in the candidate's replay protocol, not evidence that the
caller callback was intrinsically faulty. A complete route still needs an
accurate available-handler set and normal caller restart behavior while keeping
a package catcher below callbacks. Neither tested refinement supplied those
protocols for both controls. The handler-initiated nonlocal-transfer exclusion
in the specification does not by itself establish that replacing a usable
restart with a replay error is valid.

## Exit-time return-value observation

The documented experimental [`returnValue(default = ...)`][return-value] was
also tested in the package boundary's `on.exit()`. Healthy condition-as-value
return and an error caught inside that boundary exposed the exact returned
condition. A custom error and self-delivered actual SIGINT escaping from an
older external warning handler instead produced the default sentinel; the outer
handlers still received the original custom error and a native interrupt. The
`warn = 2` option remained unchanged.

The documentation specifies the default when exit by error or restart has no
return value. The inspected [context implementation][context] treats an
intermediate jump's return value as undefined, and [`do_returnValue()`][eval]
returns the supplied default when no exit return value is present. This getter
did not expose the pending condition that the inner catcher had missed. Raw
objects, including the original environment payload and sentinel, are retained
alongside the assertions.

## Reproduction

From the repository root, extract the retained raw bytes into a disposable
directory. The source adaptations below replace the original temporary
shared-library and observation-file pathnames with their extracted local
counterparts. No retained bytes in the bundle are edited.

```r
bundle <- readRDS("investigation/condition-handler-native-api-2026-10-01.rds")
target <- tempfile("marginplyr-native-replay-")
dir.create(target)
for (name in names(bundle$files)) {
  writeBin(bundle$files[[name]], file.path(target, name))
}
scripts <- c("probe.R", "toplevel-controls.R", "callback-snapshot-probe.R",
             "public-restart-probe.R", "availability-probe.R",
             "restart-bridge-probe.R", "return-value-probe.R")
for (name in scripts) {
  path <- file.path(target, name)
  source <- readLines(path)
  source <- gsub("/private/tmp/marginplyr-764-native-20261001/replay.so",
                 paste0("replay", .Platform$dynlib.ext), source, fixed = TRUE)
  source <- gsub("/private/tmp/marginplyr-764-research-20261001/return-value-observations.rds",
                 "return-value-observations.rds", source, fixed = TRUE)
  writeLines(source, path)
}
setwd(target)
stopifnot(system2(file.path(R.home("bin"), "R"),
                 c("CMD", "SHLIB", "replay.cpp")) == 0L)
for (script in scripts) {
  stopifnot(system2(file.path(R.home("bin"), "Rscript"), script) == 0L)
}
```

The source used `replay.so` on macOS; the extraction substitutes the platform's
`.Platform$dynlib.ext`. No other R version or platform was run. The expected
results include explicit confirmed failures of the
candidate protocols, so exit 0 means those measured results were reproduced.

[r-exts]: https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Condition-handling-and-cleanup-code
[conditions]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html
[sys]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/sys.parent.html
[context]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/main/context.c
[errors]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/main/errors.c
[return-value]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/trace.html
[eval]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/main/eval.c
