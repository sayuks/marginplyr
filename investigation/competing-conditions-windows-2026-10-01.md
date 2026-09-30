# Windows queued interruption and competing-condition follow-up

Investigated: 2026-10-01
Source snapshot: d0c5e1828904689033fa9a09af364df12ca6d28e
Scope: queued-event correction and scoped follow-up evidence; not completion of #756

## Observed Windows failure and source mechanism

At source `0548f07b4d9de289745b8a63738a215da43685a9`, the applicable jobs in
[release-matrix run 36731387718][matrix] succeeded, but the full
[Windows R-CMD-check job 109965184990][windows-check] stopped at
`SQLite cleanup interruption retains the earlier execution error`. Its
`testthat.Rout.fail` recorded R 4.6.1, `x86_64-w64-mingw32/x64`, and interruption
of that test. The matrix result therefore did not establish a passing full
Windows package check.

The inspected R-4-6-branch source was pinned to
`ec31e53d7a9dee08b4715f0c1fa9ef40b4655203`; rlang 1.3.0 was pinned to
`ce8a147b712c425fa4eb1d351e621d07ce6e423b`.

- rlang's [Windows `r_interrupt()`][rlang-cnd] set `UserBreak = 1` and called
  `R_CheckUserInterrupt()`. Its Unix path called `Rf_onintr()` directly.
- R's [`R_CheckUserInterrupt()`][errors] returned before event processing when
  interrupts were suspended. Its `onintrEx()` instead set
  `R_interrupts_pending` when reached under suspension. These were distinct
  ways to defer interruption.
- [`allowInterrupts()`][conditions-r] changed the suspension flag; it did not
  itself poll Windows events. [`do_syssleep()`][platform] delegated directly
  to the platform's `Rsleep()`.
- Windows [`Rsleep()`][extra] converted seconds to integer milliseconds with
  `1000 * timeint + 0.5`, then called `R_ProcessEvents()` only inside
  `while (ntime > 0)`. Zero seconds skipped that loop; 0.001 seconds entered it
  once. [`R_ProcessEvents()`][system] consumed `UserBreak` and called `onintr()`.
  Unix [`Rsleep()`][unix-sleep] called `R_CheckUserInterrupt()` before testing
  whether the requested interval had elapsed, including for zero seconds.

From those paths, a Windows `UserBreak` set during protected cleanup could
survive `allowInterrupts(Sys.sleep(0))` until a later event poll. The correction
to `allowInterrupts(Sys.sleep(0.001))` supplied the missing poll while the
operation's interrupt handler was still available. This was a source-derived
causal explanation, rather than a Windows runtime measurement of the fix.
The [Sys.sleep documentation][sleep-help] documented platform-dependent timing
resolution; one millisecond was the requested interval, not a measured latency.

## Fresh-code verification and retained evidence

The public SQLite `compute()` regression modeled the Windows event boundary by
deferring notification until a mocked base `Sys.sleep()` received a positive
integer-millisecond interval. Before the correction it captured the earlier
execution error and then a leftover interrupt at the subsequent probe. After
the correction it captured an interrupt retaining that execution error, with
a normally returning subsequent probe. The focused SQLite, competing-condition,
and execution-condition files passed. This established the regression model
and its integration with the public operation, not native Windows execution.

For committed source `d0c5e1828904689033fa9a09af364df12ca6d28e`, the supervisor
completed with terminal exit status 0: all 30 workers passed, comprising ten
controlled notifications, ten supervisor-delivered actual SIGINT cases, and
ten healthy controls. The measured environment was R 4.6.1 on Darwin 25.6.0,
arm64, with DBI 1.3.0, RSQLite 3.53.3, dbplyr 2.6.0, dplyr 1.2.1, rlang 1.3.0,
testthat 3.3.2, and pkgload 1.5.3. Reproduction from a clean committed source
uses a new evidence directory:

```sh
python3 tools/competing-conditions/run.py /private/tmp/764-new-acceptance
```

The 232-file
[archive](competing-conditions-windows-2026-10-01.rds), format
`marginplyr-competing-conditions-evidence-v1`, retained the original bytes,
source manifest, checkpoints, deliveries, environment records, outcomes, and
verdicts. The root run round-trip checked all bytes. Independent archive
readback checked all ten actual-SIGINT raw outcomes as interrupts outside the
error class and all ten subsequent probes as values:

```r
bundle <- readRDS("investigation/competing-conditions-windows-2026-10-01.rds")
read_archived <- function(name) {
  connection <- gzcon(rawConnection(bundle$files[[name]]))
  on.exit(close(connection))
  readRDS(connection)
}
paths <- names(bundle$files)
paths <- paths[grepl("-sigint/raw.rds$", paths)]
stopifnot(length(paths) == 10L)
for (path in paths) {
  outcome <- read_archived(path)
  immediate <- read_archived(sub("raw.rds$", "immediate.rds", path))
  stopifnot(
    outcome$kind == "interrupt",
    inherits(outcome$condition, "interrupt"),
    !inherits(outcome$condition, "error"),
    immediate$probe$kind == "value"
  )
}
```

Those observations did not establish the fixed Windows runtime, actual second
SIGINT, native-statement or timeout cancellation, or another R version.

## Additional supported-API capture boundary

The preceding
[handler-stack investigation](condition-handler-replay-2026-09-30.md) was prior
evidence, not rerun as new evidence here. A new fresh-process probe examined
`globalCallingHandlers()` as a supported capture route below an older calling
warning handler. The [documented order][conditions-help] made global handlers
last-resort handlers. R's [`do_addGlobHands()`][errors] also refused registration
with dynamic handlers on the stack.

On R 4.6.1, arm64 macOS, a pre-registered global handler retained the exact
custom error object, including its environment payload, and returned through a
replay restart. A native self-delivered SIGINT inside the warning handler was
also captured; a short interruptible sleep kept its delivery within that
handler. Adding an older caller `tryCatch(error = ...)` or
`tryCatch(interrupt = ...)` bypassed both package and global capture. These were
passing and failing call-shape controls of a partial supported route. They did
not supply the required general implementation or establish that every design
was impossible. The accepted external-handler outcome remained unresolved.

The following fresh-process reproduction retains the exact-error control:

```r
failure <- errorCondition("handler failure", payload = new.env())
seen <- NULL
replay <- function() {
  withRestarts(
    tryCatch(warning("replayed"), error = identity),
    end_replay = identity
  )
}
globalCallingHandlers(error = function(cnd) {
  seen <<- cnd
  invokeRestart("end_replay", cnd)
})
first <- withCallingHandlers(replay(), warning = function(w) stop(failure))
stopifnot(identical(first, failure), identical(seen, failure))
seen <- NULL
second <- tryCatch(
  withCallingHandlers(replay(), warning = function(w) stop(failure)),
  error = identity
)
stopifnot(identical(second, failure), is.null(seen))
globalCallingHandlers(NULL)
```

[matrix]: https://github.com/sayuks/marginplyr/actions/runs/36731387718
[windows-check]: https://github.com/sayuks/marginplyr/actions/runs/36731387778/job/109965184990
[rlang-cnd]: https://github.com/r-lib/rlang/blob/ce8a147b712c425fa4eb1d351e621d07ce6e423b/src/rlang/cnd.c
[errors]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/main/errors.c
[conditions-r]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/library/base/R/conditions.R
[platform]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/main/platform.c
[extra]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/gnuwin32/extra.c
[system]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/gnuwin32/system.c
[unix-sleep]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/unix/sys-std.c
[sleep-help]: https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.sleep.html
[conditions-help]: https://github.com/wch/r-source/blob/ec31e53d7a9dee08b4715f0c1fa9ef40b4655203/src/library/base/man/conditions.Rd
