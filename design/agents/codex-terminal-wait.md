# Codex nested terminal waiting

This recipe configures the agent's shell/tool calls. The user-controlled knobs
are the `functions.exec` pragma, `functions.wait.yield_time_ms`, and the nested
`exec_command` / `write_stdin` arguments. `.codex/config.toml` configures this
repository's permissions, not these per-call intervals. Use the tool schemas
provided by the active runtime; adapt this recipe if that interface changes.

[Local checks](local-checks.md#observing-one-invocation) owns execution identity
and verdicts. This recipe changes observation only: the runner, FIFO, shared
lock, five-second native heartbeat and checks retain their existing behavior.

## Start and retain

Start the command once. The JavaScript below belongs in `functions.exec`, whose
tool adapter requires JavaScript; the invoked development command remains R.
Use a unique store key for each invocation and keep the full initial and latest
results, including the original `session_id`, in transient session state.
Replace the example reason with the scope judgment for the submitted diff.

```javascript
// @exec: {"yield_time_ms": 30000, "max_output_tokens": 3000}
const result = await tools.exec_command({
  cmd: "Rscript tools/review-ready.R --scope source --reason 'Agent observation documentation only.'",
  workdir: "/absolute/path/to/the/submitting/worktree",
  tty: true,
  yield_time_ms: 1000,
  max_output_tokens: 3000
});
store("review-ready-invocation", {
  initial: result, last: result, session_id: result.session_id
});
text(result);
```

A result with `exit_code` is already terminal, including a nonzero code. A
live `session_id` needs observation through that same session. A missing result
or tool exception establishes no verdict; retain the available handles and
report unavailable evidence under local checks rather than resubmitting.

## Observe one bounded poll

At a natural work boundary, run one poll with a ten-second inner wait and a
thirty-second outer yield. The outer budget leaves room for call overhead and
avoids the one-second outer yield that resumes merely to collect a pending
inner poll. Both bounds leave time to communicate within this runtime's
sixty-second progress requirement. Do independent work under local checks
between observations; when no useful work remains, observe the same session.

```javascript
// @exec: {"yield_time_ms": 30000, "max_output_tokens": 3000}
const state = load("review-ready-invocation");
if (!state || state.last.exit_code !== undefined || !state.session_id) {
  throw new Error("Poll only a retained live terminal session.");
}
const result = await tools.write_stdin({
  session_id: state.session_id,
  chars: "",
  yield_time_ms: 10000,
  max_output_tokens: 3000
});
state.last = result;
store("review-ready-invocation", state);
text(result);
```

Preserve complete returned objects and native output on each observation.
Choose a sufficient output budget and retrieve omitted diagnostics from the
invocation's own artifacts when output is truncated. Forward worker diagnostics
without replacing them with a synthesized success summary.

If `functions.exec` still yields `Script running with cell ID ...`, retain that
outer `cell_id` too. Call `functions.wait` on it with `yield_time_ms: 10000`
until the nested result returns. A pending outer cell already owns its inner
poll: finish that cell before issuing another poll. An outer yield is a
resumption boundary, not a command timeout or a terminal result. Keep each
wrapper bounded to one shell call rather than looping for the invocation's
whole lifetime; this keeps reasoning, user steering and communication possible.

## Cancellation and terminal collection

First collect any pending outer cell to recover its latest nested result.
For a retained live PTY session started with `tty: true`, use the same bounded
poll snippet with `chars: "\u0003"` to send Ctrl-C through `write_stdin`.
Then continue empty-input polls through cleanup to the actual terminal result.
The [runner cancellation contract](../../tools/review-ready-runner.md#cancellation)
owns child collection and capacity release. With another shell interface, use
its documented cancellation operation on the retained invocation.

Failure, a command deadline, or a tool-reported timeout does not authorize a
new submission. Collect the original invocation's available output, cleanup
and terminal outcome. If a timeout or lost outer cell prevents collection,
report that evidence as unavailable; a process probe cannot recover the verdict.
Terminating only an outer wrapper is not evidence of command cancellation or
child cleanup. Elapsed time never supplies an exit status.

## Progress communication

Describe observed transitions: admission waiting, execution, cleanup and
terminal completion. Native heartbeat and worker output remain tool output;
they need no repeated natural-language paraphrase. Prefer new evidence and
the next unresolved question in progress messages.

The governing runtime in this session requires a user-facing update at least
every sixty seconds during ongoing work. Repository instructions cannot relax
that requirement. When nothing changes, give a brief periodic update stating
the last observed phase and that the terminal outcome remains pending; do not
invent a transition, percentage, ETA or completion. Shorten observation bounds
if the active runtime requires faster responsiveness.

## Controlled verification

Use `Rscript tools/fixtures/agent-terminal-wait.R success 8` as the command in a
disposable observation experiment. It simulates phases and owns no children,
lock or queue. Start with `yield_time_ms: 250` to obtain a pending terminal
session, then poll that session with `yield_time_ms: 10000`. Compare outer
pragmas of 1000 and 10000 on the same scenario, retaining all returned objects
and counting outer calls separately from nested shell calls. Collect each
experiment's terminal result before the next one.

The fixture also accepts `failure 2`, `timeout 2`, and `cancel 30`. For cancel,
send Ctrl-C after the waiting announcement through the retained PTY session.
Expect cleanup followed by terminal exits 1, 124 and 130 respectively. The
timeout mode models a command's deadline exit; it does not simulate a runtime
tool timeout or prove production runner child cleanup. The retained runner
fixtures own that latter contract. Never run the package suite just to measure
outer resumptions. The [dated comparison](../../investigation/codex-nested-wait-2026-11-04.md)
records the actual tool counts and handles from this recipe's development.
