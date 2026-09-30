---
status: accepted
---

# Preserve competing conditions at interruption

Issue #756 selects the condition outcome when interruption competes with
execution failure, SQLite cleanup, or buffered-warning replay. Failed cleanup
takes precedence; after successful cleanup, a catchable user interrupt takes
precedence over an earlier execution error. Replay cannot replace either
outcome. Keep the original conditions available as structured diagnostic
information instead of flattening them into a message or discarding a cause.
This lets callers handle cancellation as cancellation and recognize incomplete
recovery without losing the failure that preceded either.

Retain ADR 0031's protected ownership handoffs and cleanup, interruptible main
computation, and successful-release boundary. Further catchable interrupts
during the same unwind join the existing cancellation; they do not restart
cleanup or displace an established cleanup failure. This does not promise an
exact interrupt count or recovery from native cancellation, backend-aborted
transactions, invalid connections, or process termination.

An enriched notification must reach interrupt handlers with its retained causes.
If no exiting handler takes it, preserve native R interruption hooks and
top-level recovery. Returning calling handlers may receive a further bare
native notification for the same cancellation; notification count is not an
identity or a public guarantee.

The [implementation specification](../specs/competing-conditions.md) owns the
condition information, observable scenarios, evidence limits, and publication
requirements. The decision is accepted under the maintainer's delegation on
2026-09-30; its implementation and public-contract publication are pending.
ADRs 0015 and 0021 retain their ordinary External-condition contract.

## Considered options

Keeping the earlier execution error as the escaping condition would preserve
its usual error handler but hide the caller's cancellation from an interrupt
handler. Choosing whichever condition escapes last would let warning conversion
or a further interrupt conceal an incomplete cleanup. Both are rejected.

Dropping buffered warnings on interruption would avoid replay failures but
discard diagnostics already produced by the caller's expression. Attempt replay
in its existing order, stopping at the first replay failure or further interrupt;
retain that failure without allowing it to replace the selected outcome.

Making the entire operation uninterruptible would simplify selection but make
the caller wait for the complete computation. Protection remains limited to
the resource transitions ADR 0031 owns.

Requiring one enriched notification across both handler and default-action
paths is rejected in favor of preserving native interruption hooks. The
[signaling investigation](../../investigation/condition-resignaling-2026-09-30.md)
records a candidate and the observed notification/default-action trade-off,
with [text evidence](../../investigation/condition-resignaling-2026-09-30/README.md);
the specification owns its implementation acceptance.
