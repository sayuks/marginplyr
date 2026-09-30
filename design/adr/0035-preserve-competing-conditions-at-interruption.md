---
status: accepted
---

# Preserve competing conditions at interruption

Issue #756 selects the condition outcome from conditions caught while package
handlers remain available, when interruption competes with execution failure,
SQLite cleanup, or buffered-warning replay. Failed cleanup takes precedence;
after successful cleanup, a caught user interrupt takes precedence over an
earlier execution error. A caught replay failure cannot replace either outcome.
Keep the original conditions available as structured diagnostic information
instead of flattening them into a message or discarding a cause.
This lets callers handle cancellation as cancellation and recognize incomplete
recovery without losing the failure that preceded either.

Retain ADR 0031's protected ownership handoffs and cleanup, interruptible main
computation, and successful-release boundary. Further interrupts caught
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
requirements. The decision was accepted under the maintainer's delegation on
2026-09-30 and amended by the maintainer on 2026-10-01 as recorded below.
ADRs 0015 and 0021 retain their ordinary External-condition contract; #756 and
its implementation pull request record validation and publication evidence.

## Considered options

Keeping the earlier execution error as the escaping condition would preserve
its usual error handler but hide the caller's cancellation from an interrupt
handler. Choosing whichever condition escapes last would let warning conversion
or a further interrupt conceal an incomplete cleanup. Both are rejected.

Dropping buffered warnings on interruption would avoid replay failures but
discard diagnostics already produced by the caller's expression. Attempt replay
in its existing order, stopping at the first replay failure or further interrupt
caught at the package boundary; retain the caught failure without allowing it
to replace the selected outcome.

Making the entire operation uninterruptible would simplify selection but make
the caller wait for the complete computation. Protection remains limited to
the resource transitions ADR 0031 owns.

Requiring one enriched notification across both handler and default-action
paths is rejected in favor of preserving native interruption hooks. The
[signaling investigation](../../investigation/condition-resignaling-2026-09-30.md)
records a candidate and the observed notification/default-action trade-off;
the specification owns its implementation acceptance.

## Available-handler boundary amendment (2026-10-01)

The maintainer selected normal R handler behavior and supported APIs over the
original requirement to capture failures from arbitrary outer warning handlers.
When R invokes an older calling handler, newer package handlers are unavailable.
Preserving that dispatch also preserves the caller's ability to take control
outside the package's catchers. The
[contract-alternatives investigation](../../investigation/competing-condition-contract-alternatives-2026-10-01.md)
records the public-API counterexamples and cooperative alternatives.

An outer warning handler's error, interrupt, or restart that bypasses package
handling follows R's native transfer. It may replace a pending cancellation or
earlier error without retaining either, and its failure is not promised as
`$replay_error`. This limit does not weaken owned-resource cleanup or change
warning observation, muffling, ordering, `warn`, or native interrupt fallback.
Caught conditions still follow the priorities above.

A caller needing stronger control may catch failures inside its own warning
callback, muffle failed and subsequent warnings, and retain the callback failure
separately from the operation outcome. This is optional caller-owned handling,
not a new package API or replay restart. With no pending cancellation, it may
allow the operation to return a value; the caller owns that choice. Failures in
older handlers reached by nested warnings remain outside that callback's catcher.

A package-owned file log is not selected for #756. A record completed before
replay could preserve earlier diagnostics even when an outer handler bypasses
caller cooperation, but it would not preserve the escaping condition or capture
a later handler failure. That separate persistence requirement would need a
record format and a policy for incomplete or failed writes. Replacing warning
delivery with file output is rejected because callers would lose ordinary
observation and muffling. The
[file-logging comparison](../../investigation/condition-file-logging-comparison-2026-10-01.md)
records the measured benefit, writing failures, and serialization limits.
