# Capturing failures in caller warning handlers during replay

Investigated: 2026-09-30

## Finding

A targeted handler-stack interleaving experiment captured the original condition
raised by an external calling warning handler, without suspending that handler,
changing `options(warn)`, replacing its body, or replacing R's warning signaling.
It also captured a native SIGINT delivered inside that handler. This establishes
an experimental implementation route, rather than impossibility of #756's
requirements. It did **not** establish a route acceptable for this repository's
Review-ready check or CRAN: the prototype required two R internal operations,
and `R CMD check` reported their use as a WARNING.

No supported API meeting all those constraints was identified in the R and
rlang interfaces examined below. The condition-handling guarantee remains the
one specified by `design/specs/competing-conditions.md`; this note grants no
exception to it or to the repository's check requirements.

## Why ordinary capture fails

R's [condition documentation][conditions] specifies that a calling handler runs
with only handlers below itself available. Thus a package's `tryCatch()` nested
inside the caller's `withCallingHandlers()` cannot catch an error or interrupt
raised by that caller's warning handler. This is a handler-dispatch restriction,
not ordinary lexical scope. `do_signalCondition()` and `signalInterrupt()` in
[R's `errors.c`][errors] implement it by advancing `R_HandlerStack` before
calling the selected handler.

`rlang::catch_cnd()` on the measured installation delegated to `tryCatch()`.
The [documented `try_fetch()` interface][try-fetch] also used calling-handler
semantics; it offered no insertion point below an already registered external
handler. Those functions therefore did not remove this restriction.

The following baseline distinguishes handler failure from warning conversion:

```r
failure <- simpleError("handler failure")
tryCatch(
  withCallingHandlers(
    tryCatch(warning("first"), error = function(e) "inner catcher"),
    warning = function(w) stop(failure)
  ),
  error = function(e) identical(e, failure)
)
# TRUE: the outer error handler, rather than the package's inner catcher, ran.
```

The [public-summary baseline](condition-handler-replay-2026-09-30/public-summary-baseline.R)
ran the then-uncommitted #756 implementation through `summarize_with_margins()`.
Two earlier branches produced side effects and a buffered warning; the later
branch established the first cancellation. At `warn = 2`, an external warning
handler's `stop(handler_error)` escaped as that exact error, losing cancellation.
A self-delivered native SIGINT in the same external handler escaped as a later
bare interrupt, with no retained first cancellation. Both runs reached replay,
preserved input/options, and allowed a subsequent valid summary. The
[source manifest](condition-handler-replay-2026-09-30/source-snapshot.txt)
identifies the measured production files; this baseline is deliberately a
regression observation, not a passing #756 acceptance test.

## Targeted internal candidate

The [runnable probe](condition-handler-replay-2026-09-30/probe.R) took a snapshot
of the handler stack before installing the package's error and interrupt
catchers. Inside that `tryCatch()`, it captured the new entries and constructed a
new pairlist containing those entries before and after each original entry.
When an external warning calling handler ran, a package catcher therefore
remained immediately below it. The caller's existing handler function and its
entry were retained, as were R's warning/restart and option machinery.

The prototype read the stack using
`.Internal(.addCondHands(NULL, NULL, parent.frame(), NULL, TRUE))` and replaced
it using `.Internal(.resetCondHands(...))`. It copied the pairlist spine rather
than decoding handler-entry classes or flags. The latter contain internal data,
including `CHARSXP` objects and calling flags; interpreting them in ordinary R
would add another internal-layout dependency. The captured package entries
shared the exiting-handler destination/result objects produced by `tryCatch()`.
Normal function-context restoration restored the original stack after replay in
the measured restoration control; a general production implementation would
need broader nested-unwind and platform testing.

On R 4.6.1, aarch64 macOS, these probes passed:

- A caller handler's `stop(failure)` retained the exact custom error object and
  its environment payload.
- A caller handler's controlled interrupt retained the exact custom interrupt
  object and its payload.
- Nested calling handlers retained their native observation order, and
  `muffleWarning` at `warn = 2` prevented conversion.
- A returning caller warning handler at `warn = 2` still led to R's warning
  conversion error, which the package catcher retained.
- A nested warning raised inside a caller warning handler could reach another
  caller handler that failed; its original failure was captured.
- A later warning after replay reached the caller's remaining handler normally.
- `tools::pskill(Sys.getpid(), 2L)` inside the external warning handler delivered
  an actual SIGINT, captured as a native `interrupt` condition.

This was a replay-boundary experiment, not integrated marginplyr acceptance.
The SIGINT was self-delivered in a fresh R process, not a supervisor checkpoint
experiment. It did not prove top-level hooks, cleanup integration, repeated
SIGINT, other R versions, or other operating systems.

## Compatibility and release constraint

The official [R 4.1.3 source archive][r413] was downloaded and its
`src/main/errors.c`, `src/main/context.c`, and `src/library/base/R/conditions.R`
were inspected. The archive SHA-256 was:

```text
15ff5b333c61094060b2a52e9c1d8ec55cc42dd029e39ca22abdaa909526fed6
```

That snapshot already supplied the stack-return behavior of `do_addCondHands()`,
the direct replacement in `do_resetCondHands()`, the five-element handler-entry
layout, and the dispatch restriction described above. The R 4.6 branch source
supplied the same relevant mechanisms. This was source compatibility evidence;
no R 4.1 runtime probe was performed, and it did not turn those operations into
supported APIs or promise stability between patches.

The [CRAN Source packages policy][cran-policy] explicitly states: “CRAN packages
should use only the public API.” It also expressly excludes `.Internal()` and
alternative access to undocumented base internals. Its rationale includes
breakage even in patched versions. Obscuring the calls from a checker or moving
the same dependency into compiled code would not resolve that policy conflict.

A disposable package containing the prototype was checked with
`R CMD check --no-manual --no-vignettes` on R 4.6.1. The
[raw log](condition-handler-replay-2026-09-30/00check.log) included this independent
warning under *checking R code for possible problems*:

```text
Found .Internal calls in the following functions:
  ‘interleave’ ‘read_stack’
with calls to .Internal functions
  ‘.addCondHands’ ‘.resetCondHands’
```

The log also contained an unrelated missing-help warning and metadata NOTE from
that deliberately minimal package. `tools:::.check_dotInternal()` independently
identified the same two calls. The installed `tools:::.check_packages()` body
classified a nonempty internal-call report as WARNING. A cleaner disposable
reproduction is provided by
[check-probe.R](condition-handler-replay-2026-09-30/check-probe.R). Its measured
[output](condition-handler-replay-2026-09-30/clean-check-output.txt) contained
1 WARNING (only the internal-call warning) and 1 source-preparation NOTE.

The repository's `design/agents/local-checks.md`, *Local review-ready checks*,
states that an ERROR or WARNING fails a package-check attempt. Shipping this
candidate therefore conflicted with an existing repository gate as well as the
external CRAN policy; maintaining #756's condition requirements alone did not
authorize removing either constraint.

## Other native mechanisms examined

[Writing R Extensions, Condition handling and cleanup code][r-exts] documented
`R_tryCatch()` as using the R-level mechanism. `R_withCallingErrorHandler()`
added an ordinary calling entry to the same stack in the inspected source, so
neither stayed active inside an older external warning handler.

`R_UnwindProtect()` could observe a nonlocal jump, but its cleanup callback
received a jump boolean, not the escaping condition. Its continuation token was
specified for resuming unwinding, not for exposing a condition. In
[`context.c`][context], the token internally held `R_ReturnedValue`; inspecting
that layout would be another unsupported dependency. For an exiting condition
handler that value could contain an original condition, while an unhandled
error/native top-level interrupt did not provide the same structured result.

`R_ToplevelExec()` isolated evaluation from the caller's condition handlers.
That would remove normal caller warning observation, a required part of the
replay behavior. `R_curErrorBuf()` exposed a diagnostic string rather than the
original error class, cause, and fields. Restarts could end replay once a
condition was observed, but supplied no supported way to observe a caller
handler's failure when all package catchers had already been removed from the
available handler stack.

## Reproduction and next boundary

```sh
Rscript investigation/condition-handler-replay-2026-09-30/probe.R
Rscript investigation/condition-handler-replay-2026-09-30/check-probe.R
Rscript investigation/condition-handler-replay-2026-09-30/public-summary-baseline.R
```

The [session snapshot](condition-handler-replay-2026-09-30/session-info.txt)
records the measured environment. No pre-existing installed package, global R configuration,
or production/test source was changed by this investigation; R CMD check installed
its probe package only into its disposable check directory.

The viable internal experiment and its explicit checker/policy failure are the
concrete alternatives established here. A supported implementation would need
an API enabling a package catcher to remain below caller calling handlers, or a
supported unwind callback receiving the original condition. This note does not
prove that every conceivable API or design is impossible; it bounds the APIs
examined and records an actual mechanism that passed the replay probes while
failing the release constraint.

[conditions]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/conditions.html
[errors]: https://raw.githubusercontent.com/wch/r-source/R-4-6-branch/src/main/errors.c
[context]: https://raw.githubusercontent.com/wch/r-source/trunk/src/main/context.c
[try-fetch]: https://rlang.r-lib.org/reference/try_fetch.html
[r-exts]: https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Condition-handling-and-cleanup-code
[cran-policy]: https://cran.r-project.org/web/packages/policies.html
[r413]: https://cran.r-project.org/src/base/R-4/R-4.1.3.tar.gz
