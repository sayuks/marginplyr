# SQLite catchable-interruption recovery acceptance

Investigated: 2026-09-30

## Snapshots and environment

Package behavior, deterministic tests, and public documentation were checked at
`a9d3cdf0c5b770d9608f57cc82fa231a8740f09e`. The final standalone supervisor
and worker were checked at `5c415271cd18c868042590e57a8f09ec71947284`;
`git diff a9d3cdf 5c41527 -- R tests man inst vignettes DESCRIPTION NAMESPACE README.md README.qmd`
was empty. The intervening commits changed only the package-excluded worker.

The workers ran R 4.6.1 on Darwin 25.6.0 arm64, with DBI 1.3.0, RSQLite
3.53.3, dbplyr 2.6.0, dplyr 1.2.1, rlang 1.3.0, testthat 3.3.2, and pkgload
1.5.3. Every case retained its own PID, source snapshot, environment, and
condition outcome. The supervisor checked the flushed reached notification's
PID and checkpoint before sending SIGINT to its own identified worker.

## Observations

All 120 separate-process cases passed: 114 supervisor-delivered SIGINT cases,
two healthy controls, two ordinary-error controls, and two controlled R
interrupt cases. The deterministic public-compute tests separately covered
mutation, caller ownership, unsorted and expansion results, before-acquisition
interruption, failed rollback/release, invalid connections, and simulated
whole-transaction abort. The abort simulation did not invoke native SQLite
cancellation.

Pre-release cases restored the old destination's values, physical schema,
indexes, sqlite_stat1 and available sqlite_stat4 samples, or removed the new
destination. Source and sentinel rows remained intact. An immediate state
record preceded any savepoint probe, retry, caller commit/rollback, reconnect,
or disposal. The captured savepoint could no longer be rolled back to;
without an outer transaction a new dbBegin succeeded and a later persistent
compute was visible from the separate observer connection.

Both in_transaction values passed. Persistent, temporary, attached-schema,
new/overwritten, sorted/unsorted and expansion representatives passed. Earlier
caller work survived pre-release recovery; separate commit and rollback cases
established continued caller ownership. After successful outermost release,
completed values remained independently visible. After inner release, caller
commit persisted the result and earlier work, while rollback restored the
pre-transaction state. No rollback of already released work was observed.

Eligible checkpoints explicitly serviced the queued SIGINT before continuing,
so fast result preparation could not cross release before R delivered it.
Protected acquisition, release and cleanup checkpoints returned across their
handoff before deferred signals were drained. The worker captured raw
conditions before draining and formatted diagnostics afterwards. Cleanup
SIGINT cases followed an ordinary execution failure; their resource assertions
and recorded outcomes did not select a competing-condition priority for #756.

The Review-ready invocation for the package snapshot exited 0: spelling,
jarl, package-aware lintr, and strict coverage passed; covr 3.6.5.9001 measured
8033/8033 lines. Source-tarball R CMD check --as-cran reported zero ERRORs,
WARNINGs, and NOTEs. The structural release-matrix check passed all 1230 tests
across five single-backend configurations. The installed working tree's site
render and verification passed 21 pages and the search index.

## Evidence and reproduction

The [evidence directory](sqlite-interruption-recovery-2026-09-30/) contains
acceptance.rds, the complete supervisor summary and manifest, archive
hashes, and the focused-test, Review-ready, structural and site logs. Each
archived case includes configuration, environment, reached/delivery records,
before/immediate/final state, worker log, and verdict. Each RDS archive holds a
`files` list mapping relative paths to the original raw file bytes; `readRDS()`
reads it without executing the archived scripts.

From a clean committed checkout, run:

```sh
python3 tools/sqlite-interruption/run.py /tmp/sqlite-interruption-acceptance
Rscript tools/review-ready-check.R
Rscript .github/scripts/verify-suite-coverage.R
```

The supervisor used POSIX SIGINT. These observations did not verify native
SQLite statement cancellation, native timeouts, second interrupts, process
termination, other OS/R/dependency versions, or #756's general condition
precedence. The adopted responsibility and limits belong to ADR 0031 and the
SQLite specification's catchable-interruption amendment; these measurements
were evidence for the recorded snapshots.

## Harness development

The separate harness-development.rds retains earlier incomplete runs, each
with its own snapshot and manifest. They were not acceptance evidence. The
first used Sys.sleep at protected checkpoints, which delivered SIGINT despite
R interruption suspension on this measured build. Subsequent runs exposed an
observer assertion that tried to read an absent new table, condition-message
promise evaluation interrupted before deferred-signal draining, and a fast
unsorted result crossing release before a queued SIGINT was delivered. The
worker changes in the final snapshot addressed those observation problems;
the final complete run above supplied the acceptance verdict.
