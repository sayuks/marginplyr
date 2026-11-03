# Checkpoint interruption acceptance

From a clean committed checkout, with `Config/Needs/review` and the workers'
package test dependencies installed, run:

```sh
Rscript tools/competing-conditions/run.R /tmp/competing-condition-evidence
Rscript tools/sqlite-interruption/run.R /tmp/sqlite-interruption-evidence
```

Use a new or empty evidence directory outside the checkout. Each command runs
its full matrix in separate R workers and stops at the first failed case.
`manifest.json` pins the source SHA and SHA-256 hashes; per-case records retain
configuration, environment, worker logs, reached notifications, signal delivery,
immediate state observations and verdicts. `summary.json` records terminal
worker status and any supervisor failure. A failed or incomplete case is not a
pass. The supervisor owns termination and exit collection on failure or interrupt.

Controlled injection, actual checkpoint SIGINT, and healthy/error controls are
distinct cases. A real signal follows a flushed checkpoint from the identified
worker and confirmation of applicable side effects. These experiments do not
establish native-statement cancellation, actual second interrupts, timeout
cancellation, or behavior on other R/OS/dependency versions. Worker deadlines
bound the experiment; they do not test package timeout recovery.

[ADR 0035](../design/adr/0035-preserve-competing-conditions-at-interruption.md),
the [competing-condition specification](../design/specs/competing-conditions.md)
and the [SQLite specification](../design/specs/sqlite-b-direct.md) own the
interruption requirements and evidence limits.

`Rscript tools/test-interruption-supervisors.R` exercises the command boundary
with disposable workers, including missing/invalid checkpoints, identity and
side-effect failures, early exits, deadlines and supervisor write failures.
Those fixtures do not replace real package checkpoint acceptance.
