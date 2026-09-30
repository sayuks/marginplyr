# Competing-condition checkpoint acceptance

Investigated: 2026-09-30
Revised: 2026-09-30 — compressed-connection reproduction in the revisions section
Source snapshot: a1f833635fedea3d4e8f88ae9fb7988a4d6498a7
Scope: partial implementation of #756; not completion evidence

## Execution and retained evidence

The supervisor in `tools/competing-conditions/run.py` completed with exit
status 0. All 30 independently started workers completed with exit status 0
and a passing verdict. The compressed
`investigation/competing-conditions-acceptance-2026-09-30.rds` retained every
worker artifact as its original raw bytes, rather than a printed condition or
a text expansion of the objects. Its `files` list was round-trip checked after
serialization. `manifest.json` recorded the clean committed source SHA,
supervisor PID, and SHA-256 hashes of the supervisor, worker, and SQLite hook.

The execution used R 4.6.1 on Darwin 25.6.0, arm64, with DBI 1.3.0,
RSQLite 3.53.3, dbplyr 2.6.0, dplyr 1.2.1, rlang 1.3.0, testthat 3.3.2,
and pkgload 1.5.3. Each worker recorded its own environment and PID.

To inspect a preserved outcome without printing it first:

```r
bundle <- readRDS("investigation/competing-conditions-acceptance-2026-09-30.rds")
connection <- rawConnection(bundle$files[["013-summary-sigint/raw.rds"]])
outcome <- readRDS(connection)
close(connection)
```

Reproduction used a new directory outside the checkout:

```sh
python3 tools/competing-conditions/run.py /private/tmp/756-new-acceptance
```

The supervisor refused an uncommitted source state and an existing evidence
directory. It required a checkpoint matching the child PID and operation
before delivering SIGINT. Each case had a 30-second deadline.

## Revisions (2026-09-30)

The first raw-connection example above failed with `unknown input format`:
the preserved worker `raw.rds` files were gzip-compressed. The corrected
reproduction wrapped the connection in `gzcon()`:

```r
bundle <- readRDS("investigation/competing-conditions-acceptance-2026-09-30.rds")
connection <- gzcon(rawConnection(bundle$files[["013-summary-sigint/raw.rds"]]))
outcome <- readRDS(connection)
close(connection)
```

With this connection, all ten actual-SIGINT raw outcomes were independently
read and checked as interrupts outside the error class, with normally returned
post-capture probes and matching checkpoint/delivery PIDs. The manifest's
three script hashes also matched the measured source. The retained bytes and
the original measurement were unchanged.

## Cases and observations

Ten configurations ran in each of three modes: controlled `rlang::interrupt()`,
supervisor-delivered SIGINT, and no notification. Four summary configurations
crossed `warn = 1/2` with earlier warnings present/absent. Six SQLite
configurations crossed `in_transaction = FALSE/TRUE` with no caller
transaction, caller rollback, or caller commit.

For the summary checkpoint, both earlier groups had completed their visible
effects, with values `2, 5`. Warning buffering had therefore occurred before
notification in the warning cases. The worker saved `raw.rds` immediately
after capturing the public outcome, before a subsequent `Sys.sleep(0)` probe
or condition rendering. The immediate record retained the effects, replayed
conditions, unchanged input, and unchanged warning option. Both notification
modes returned an interrupt; the warning-to-error case retained the conversion
as `replay_error`. The no-notification controls retained normal warning/value
behavior. A later valid summary succeeded.

For the SQLite checkpoint, the DBI hook confirmed an INSERT completed and a
real rollback completed, after a known execution error and before cleanup
RELEASE. The immediate record retained the same-connection schema, previous
destination rows, caller sentinel rows, observer rows, transaction ownership,
and cleanup counts. The interrupt retained the earlier error and its existing
parent chain; the no-notification control returned that execution error.
Rollback and cleanup RELEASE each ran once. The destination was restored,
the package savepoint was absent, the caller's transaction remained usable,
and its later commit/rollback had the expected observer-visible result.
A subsequent transaction succeeded.

In all notification cases the first post-capture probe returned normally;
no queued notification escaped at that measured boundary. Controlled repeat
notifications were separately covered in `test-sqlite-interruption.R`.

## Limits and unresolved acceptance

These observations covered reached R checkpoints after real summary effects
and SQLite work. They did not establish cancellation inside a native SQLite
statement, timeout-driven cancellation, actual second SIGINT delivery, other
R versions, or other operating systems.

The competing-warning-handler requirement remained unresolved. An external
calling handler's own error or interrupt could leave the package replay
catcher, as reproduced in
`investigation/condition-handler-replay-2026-09-30.md`. The working internal
stack prototype in that investigation produced an R CMD check WARNING and
was not included in package code. The accepted precedence and cause-retention
requirements in ADR 0035 and the competing-conditions specification were not
weakened. Public documentation, rendered-site evidence, and complete #756
acceptance remained pending.

Two earlier supervisor attempts ended with status 1 because of harness defects:
checkpoint state was assigned in a test-local environment, then a negative
savepoint probe was counted as another cleanup attempt. Commits b3898d8 and
a1f8336 corrected those defects before this successful measurement. Neither
failed attempt supplied acceptance evidence.
