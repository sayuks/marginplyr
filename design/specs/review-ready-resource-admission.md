# Coordinate local review-ready checks across worktrees

Status: accepted design. The user confirmed the decisions during the design
interview. Runtime implementation and installation have not started.

## Problem

The user reported on 2026-10-30 that four or five concurrent worktree checks
made the Mac unresponsive enough to require a restart. The pre-restart CPU,
memory, and swap measurements are unavailable, so their relative contributions
are not established.

At the inspected main commit `b5f851a25b227eb511127b4fef19d60817bbbc71`,
`review_ready_rcmdcheck_env()` configures four test workers per check and there
is no shared admission limit. Several worktrees can therefore multiply the
number of test workers. Build and vignette rendering are also part of the
check, so limiting only its test stage would leave other heavy stages free to
overlap.

The existing [local-check policy](../agents/local-checks.md) remains the
authority for what a Review-ready check proves and how its terminal evidence
is retained. Progress and a released execution slot are not success evidence.

## Settled decisions

- Apply the initial safeguard to Review-ready checks across worktrees. Other
  full test runs, coverage, preflight, and site rendering are outside this
  initial scope.
- Prefer Mac responsiveness and stability, accepting longer waits for an
  individual branch to finish checking.
- When capacity is occupied, wait automatically and start when capacity
  becomes available. Waiting status messages are in English.
- Use a common launch command and align agent instructions so existing
  worktrees participate without first incorporating a change into every
  branch. Direct execution of an older script would bypass this cooperative
  limit; rollout must account for that entry point.
- Share capacity across all marginplyr clones under the same user on the same
  Mac. Other projects are outside this initial scope.
- Keep execution control agent-neutral. Codex, Claude Code, Grok Build, other
  AI agents, and a human terminal use the same CLI and admission mechanism.
  Agent-specific instructions explain invocation; agent SDKs, plugins,
  instruction formats, or agent-owned directories do not own the safeguard.
- Admit at most one Review-ready check at a time, retaining four internal test
  workers. Two concurrent checks with four workers each are a later candidate,
  not the initial default. Compare responsiveness, memory pressure, swap growth,
  and completion time before deciding whether to adopt that configuration.
- Require a clean committed HEAD and pin that commit before waiting.
  Immediately before admission,
  cancel the request if HEAD changed or the worktree became dirty. Report the
  cancellation rather than silently selecting another commit; the caller must
  submit a fresh request.
- Do not require strict FIFO ordering. A later request may acquire capacity
  before an earlier waiter; automatic waiting does not promise a queue position
  or a bounded waiting time.
- Observe cancellation at the common CLI boundary: an explicit cancellation
  notification, or termination of the CLI or its management process. Stop the
  request's owned child processes and return capacity only after confirming
  their shutdown.
  If abnormal termination leaves shutdown unconfirmed, do not automatically
  admit another check; explain the recovery action instead.
- Do not infer cancellation merely because an AI conversation or user interface
  closes. Its execution backend may continue running. Callers retain the
  execution handle and propagate cancellation through their execution tool.
  Product-specific application/session termination detection is not required
  by the initial implementation.

## Interface and rollout

The common command is `marginplyr-review-ready`, invoked from the target
worktree. Its stable installation and shared state live in user-scoped,
agent-independent locations. It dispatches the worktree's existing fixed
Review-ready check, keeping that check's source and terminal evidence intact.
Missing setup fails with an English explanation; it does not fall back to an
unrestricted direct invocation.

Waiting processes do not start dependency loading, lint, build, rendering, or
tests. English waiting output identifies the requested commit, elapsed wait,
and the active check where known. It promises neither a queue position nor an
ETA. The launched command remains attached to one retained execution handle
through waiting, checking, cleanup, and terminal completion; queue registration
or admission alone is not a successful exit.

Rollout includes installing the common command, exposing it to each execution
environment in use, and updating applicable agent instructions for marginplyr.
Existing sessions must acknowledge the new entry point before starting another
check; refresh or restart those sessions when necessary. Old worktrees need
not merge the change merely to invoke the common launcher. This is a
cooperative command boundary: an unmodified old script invoked directly does
not participate and cannot be described as protected.

Automatic load-based tuning, result caching, shared results between requests,
and agent-specific background job services are outside the initial
implementation. Each admitted request retains its own check outcome. Two
concurrent four-worker checks remain a separate, later measurement.

## Verification before rollout

- Use lightweight subprocess fixtures to submit several concurrent requests
  from distinct worktrees and clones, proving at most one heavy job is admitted.
- Exercise cancellation while waiting and running, abnormal launcher exit,
  stale commits, dirty worktrees, and incomplete cleanup. Unknown cleanup
  blocks admission instead of reporting success or guessing that resources
  have been released.
- Preserve the complete check and its exact SHA/terminal-result evidence.
  Run one real Review-ready check only after the safeguard is in place.
- Verify the shared command from the execution environments actually used,
  without making product-specific integrations part of its core.

## Design state

No design-interview decisions remain open. The command described here is
planned, not installed. Runtime code, user settings, and agent instructions
have not been changed as part of this design work. Implementation and rollout
are separate work, including verification that unknown cleanup blocks further
admission rather than weakening the accepted safety policy.
