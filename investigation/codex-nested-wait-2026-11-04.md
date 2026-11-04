# Nested terminal wait comparison

Investigated: 2026-11-04

## Boundary and environment

Issue #1201 requested agent-side observation economy while retaining terminal
identity. Inspection started from main `82d0f300` in the dedicated worktree
`.worktrees/1201-nested-wait`, branch `feat/1201-nested-wait`.
The active Codex session exposed per-call `functions.exec` pragma and
`functions.wait` outer yields plus `exec_command` and `write_stdin` inner waits.
The repository's `.codex/config.toml` contained permission configuration, with
no nested-wait setting. No application-wide configuration was changed.

The active runtime required user-facing progress within sixty seconds and
advised against blocking waits longer than sixty seconds. The operational
recipe therefore selected a ten-second poll inside a thirty-second outer
yield, one shell call per wrapper. The experiment below used a ten-second
outer budget to collect an eight-second scenario within one call.

## Same controlled scenario, before and after

Both executions ran `Rscript tools/fixtures/agent-terminal-wait.R success 8`
from that worktree. Each wrapper awaited `exec_command(yield_time_ms: 250)`,
stored its complete result, and then awaited one
`write_stdin(yield_time_ms: 10000)` using the returned session. The only
behavioral difference was the wrapper's outer yield. No actual package check,
FIFO admission or shared lock was used.

| Observation | Before | After |
| --- | --- | --- |
| Outer `functions.exec` yield (ms) | 1000 | 10000 |
| `functions.exec` calls | 1 | 1 |
| Additional `functions.wait` calls | 1 | 0 |
| Nested `exec_command` calls | 1 | 1 |
| Nested `write_stdin` calls | 1 | 1 |
| Original terminal session | 79207 | 67119 |
| Outer pending cell | `g224:11` | none |
| Terminal chunk | `40fdec` | `d8c74f` |
| Terminal exit | 0 | 0 |

The baseline supplied the red observation: a redundant outer resumption was
necessary while the inner poll was still pending. The aligned budget removed
that resumption. Total outer calls fell from two to one; nested calls stayed
at two. Both returned admission, execution, the synthetic worker diagnostic,
cleanup started/completed, and terminal status in order. Full initial and poll
objects were retained under separate transient session-store keys. No process
probe or elapsed-time inference supplied either verdict.

This measured tool-call count, not package-check speed or agent latency over a
real long admission wait. Tool scheduling can still yield an outer cell;
retaining and collecting it remains necessary.

## Non-success collection

These scenarios used a thirty-second outer yield, a 250 ms initial inner
yield, and the same retained-session poll. Each finished before the next one
started. Cancellation used a PTY and `write_stdin(chars: "\u0003")`.

| Scenario | Original session | Terminal chunk | Exit | Observed evidence |
| --- | --- | --- | --- | --- |
| `failure 2` | 27961 | `575844` | 1 | failure diagnostic, cleanup completed |
| `timeout 2` | 4098 | `f1d2a9` | 124 | deadline diagnostic, cleanup completed |
| `cancel 30` | 28196 | `9d4cbc` | 130 | cancellation received, cleanup completed |

Every verdict came through the original invocation. No scenario was silently
resubmitted. The timeout scenario was a controlled command deadline, not a
runtime transport timeout. The fixture had no owned children and established
no new production cancellation guarantee; the runner remained unchanged.
