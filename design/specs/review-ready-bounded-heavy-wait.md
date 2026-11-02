# Bounded resource use while waiting for heavy capacity

Status: proposed for [#1135](https://github.com/sayuks/marginplyr/issues/1135),
2026-11-02; awaiting maintainer approval. This document specifies a repair;
it does not implement it or amend the accepted operating instructions.

## Problem and existing contract

The issue reported one leaked file descriptor per unsuccessful
`filelock::lock(target, timeout = 0)` attempt with filelock 1.0.3 on the
maintainer's Mac. The loop in `review_ready_heavy_lock()` in
`tools/review-ready-check-lib.R` retried every 0.2 seconds, making descriptor
use grow with waiting time rather than with useful check work.

[ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md) and the
[runner specification](review-ready-repository-runner.md) already require one
stable OS-backed advisory lock across the user's clones and worktrees,
automatic admission after the five mandatory checks, the same execution
identity, and cancellation that prevents a waiter from starting later. This
repair adds an explicit resource bound to that wait.

## Recommended decisions

1. **One acquisition call.** Replace the R retry/sleep loop with one
   `filelock::lock(target, timeout = Inf)` call in the existing runner process.
   While the target is occupied, that call must keep a bounded number of file
   descriptors regardless of waiting duration. Retain the waiting announcement
   before the call and announce acquisition only after obtaining a lock object.
2. **Wait until admitted or cancelled.** Add no overall admission timeout,
   backoff policy, retry counter, heartbeat or queue. The existing execution
   handle remains observable while the R process waits. Admission order remains
   the OS/library's choice; this repair makes no FIFO guarantee.
3. **Keep the stable target.** Use the existing account-home resolution and
   shared target. Never unlink or replace the lock file to resolve contention.
   Existing owners using the earlier loop still exclude the repaired waiter;
   old worktrees need no migration and running invocations are left alone.
4. **Keep ownership in the parent.** Return the acquired lock to the existing
   full-check orchestration and retain its release-on-exit handling. The parent
   holds capacity through check-child shutdown and collection. Introduce no
   helper lock process, separate admission service or new dependency.
5. **Cancel through the original invocation.** Allow R's ordinary interrupt to
   escape the blocking acquisition into the existing cancellation handler.
   Preserve terminal exit 130 and the cancelled result record. Once cancellation
   has completed, releasing the holder must never start work for that waiter.
   Preserve the existing running-cancellation and abrupt-termination contracts;
   strict crash recovery remains outside this repair.
6. **Fail on acquisition errors.** A filesystem or locking error fails this
   invocation through the existing error path. Treat an unexpected `NULL` from
   an infinite wait as failure too. Neither case starts package work or reports
   a pass. Do not retry errors indefinitely or use an alternative lock path.
7. **Use the provisioned dependency.** Support the measured filelock 1.0.3
   without an upgrade prerequisite or a package Imports change. Keep filelock
   among development review tools. A dependency update is a separate choice,
   not the mechanism of this repair.

These decisions preserve the five-check ordering, pinned SHA, source-only
behavior, four test workers, progress, artifacts and retention. Runtime changes
are confined to heavy-lock acquisition and its failure handling.

## Why this mechanism

The documented infinite timeout delegates contention waiting to filelock. On
the inspected Unix implementation it opens one descriptor for the acquisition
and reuses it while waiting; it checks R interrupts during that wait. Repeated
finite timeouts do not solve the reported leak, because the inspected failure
path also leaves their descriptors open.

Slower polling only reduces the leak rate. Garbage collection cannot close a
failed descriptor that was never attached to a returned lock object. Raising
the descriptor limit only postpones exhaustion. Replacing the stable file can
split mutual exclusion between different file objects. A helper process or a
new locking mechanism introduces ownership and cancellation work that the
existing focused library can avoid.

This is a local repair within ADR 0039's accepted library and ownership
boundary. It needs neither a new ADR nor a glossary entry: it introduces no
new domain concept or architectural boundary.

## Acceptance and verification plan

Extend `tools/test-review-ready-runner.R` at its existing command/process seam,
using disposable repositories, an isolated account home and real file locks.
The regression must observe the production acquisition path, rather than
substitute a successful lock function or assert how often a helper was called.

| Scenario | Required observation |
| --- | --- |
| Occupied target | After reaching admission, sample the waiting runner's descriptor count using `ps::ps_num_fds()` on macOS. Compare samples one, two and four seconds into a held wait; allow at most two descriptors of variation. Keep the holder occupied throughout. The former five-per-second leak must fail this test. |
| Release after the measured wait | The same waiting process automatically acquires capacity and completes its pinned check once. There is no new command, user acknowledgment or changed submission SHA. |
| Ordinary waiting cancellation | While the holder remains locked, send SIGINT through the retained child handle and collect terminal exit 130 plus the cancelled result. Then release the holder and confirm no acquisition or check-start evidence for that waiter. Use a bounded fixture deadline, not a production admission timeout. |
| Lock-acquisition failure | Make the isolated target structurally unusable, without relying on user permissions. A full invocation exits nonzero, records failure and never starts package work. An unexpected `NULL` also fails closed; a small boundary assertion may cover this otherwise unreachable case. |
| Existing surrounding contracts | Retain source-only and failed-baseline admission avoidance, clone/worktree exclusion, pinned revision, running cancellation and owned-child collection. Existing fixtures remain authoritative for these behaviors. |

Descriptor assertions apply to the waiting runner process, not to the test
driver's descriptors or check workers. Samples must come after admission
readiness; account for the small race between the waiting announcement and
the acquisition call opening its descriptor. Use the existing ps dependency;
do not introduce `/proc` assumptions or invoke `lsof` in a polling loop.

Every fixture must stop and collect its owned processes on success, failure
and timeout before deleting isolated state. Never use the real user-wide
heavy lock, cancel another invocation, or deliberately exhaust descriptors.
The changed regression must fail on the old loop and pass on the repaired
runner with filelock 1.0.3. Execute the retained runner process suite on the
Mac and in its existing macOS PR CI; progress fixtures remain in their existing
CI job, without becoming a routine Review-ready prerequisite.

For the implementation commit, follow
[local checks](../agents/local-checks.md). Select `full`: the lock controls when
package build/check starts, so its effects are part of package-check tooling.
Run change-specific locking fixtures separately from that gate, retaining
each execution handle through its terminal outcome. Record source SHA,
filelock/R/macOS versions, descriptor samples, cancellation outcome and
fixture/gate results. A small dependency probe is not a runner-suite or
Review-ready pass.

On implementation, add a brief bounded-wait requirement to the accepted
runner specification and guide. Add only the site-specific explanation needed
beside the acquisition call. Preserve the existing user command and avoid
duplicating the dependency's internals across maintained documents.

## Evidence recorded on 2026-11-02

Repository inspection used `a563444f831d793c08c15e94510ff09e3377c300`.
The issue's original main reproduction used
`0dcf2d9f32b59f90fc330cf670b4a10117e9386a`; it reported counts of 5 before
waiting and 15 after ten unsuccessful attempts. That report is separate from
the design-session observations below.

The [official API](https://r-lib.github.io/filelock/reference/lock.html)
documented `Inf` as an indefinite wait, a lock object on success and `NULL` on
timeout. The
[v1.0.3 Unix source](https://github.com/r-lib/filelock/blob/v1.0.3/src/filelock-unix.c)
opened the descriptor in `filelock_lock()` and reused it in
`filelock__interruptible()`, with a 200 ms interrupt-check interval. Both the
zero-timeout contention return and exhausted finite-timeout return omitted a
close. The interrupt path also did not close an unacquired descriptor before
transferring control back to R.

One isolated dependency probe used R 4.6.1 on Darwin 25.6.0/arm64 with filelock
1.0.3, processx 3.9.0 and ps 1.9.3. Its child started with four descriptors;
four samples at 0.25-second intervals during an occupied infinite wait each
reported five. SIGINT reached its interrupt handler and the child exited 130.
The handler still counted five descriptors. A second waiter acquired the same
isolated target after its holder released it and exited zero; the cancelled
child remained terminated. All owned children were collected and isolated
state removed. The actual shared heavy target was untouched.

The probe wrapper exited zero, but printed two `Operation not permitted`
diagnostics at processx object finalization. A separate lock-free
`/usr/bin/true` child exited zero with empty output; explicitly invoking its
processx cleanup-tree finalizer reproduced the diagnostic as an `os_error` /
`ps_error`. This localized the sandbox-sensitive process-inspection behavior
separately from the lock observations; it did not establish a clean run of
the production runner fixture. Execution records were `c8244c` for the lock
probe and `39438c` for finalizer diagnosis, both terminal zero with no live
session. These were small probes, not package or Review-ready results.

The proposal therefore relies on process termination to reclaim the one
unacquired descriptor left by interruption. It does not claim that filelock
closes that descriptor inside an interrupt handler, or support catching
interrupts and repeatedly restarting acquisition in a long-lived process.
The production runner's existing cancellation handler terminates the command.
Longer process-boundary descriptor measurements and production-command
cancellation remain implementation acceptance work above.
