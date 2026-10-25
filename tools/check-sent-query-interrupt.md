# SQL diagnostic interruption check

Run from the repository root:

```sh
Rscript tools/check-sent-query-interrupt.R
```

This POSIX-only check needs fork support and a working developer R library.
It loads the working tree, changes no package functions or ownership bindings,
and sends SIGINT only to its disposable R process. The sender is stopped and
reaped before independent SQL calls verify record replacement and saved audit
settings. The loop stops after 10,000 calls or 12 seconds; its sender also has
a 12-second bound. Exit 1 reports an ownership/record failure. Exit 2 is
inconclusive because no interrupt was caught in a usable window. Exit 0 reports
the invariants observed for the interrupts caught, not exhaustive scheduling
coverage. Signal coalescing and OS/R scheduling make delivery nondeterministic;
Windows is outside this check's platform coverage.

`tests/testthat/test-sent-queries-exit-unwind.R` supplies the focused regression.
On POSIX it places one native signal at an ownership-write checkpoint, leaving
the restoration function intact. A bounded R loop reaches ordinary interrupt
checks; `Sys.sleep()` is unsuitable there because its macOS event processing
can deliver SIGINT inside `suspendInterrupts()`. A separate platform-independent
backend fixture checks original condition propagation, nested visibility,
saved settings, retained parent rows and later status updates. Existing entry,
reentry and piped-call tests cover the rest of ADR 0027's lifetime contract.
