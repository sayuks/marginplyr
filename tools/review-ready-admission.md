# Shared local Review-ready admission

`marginplyr-review-ready` admits one complete Review-ready check per OS user
across marginplyr clones and worktrees. It retains the fixed gate's four test
workers. Waiting does not load R dependencies or run any check stage. CI and
commands outside this boundary are unchanged. The accepted design is
[resource admission](../design/specs/review-ready-resource-admission.md), and
[local checks](../design/agents/local-checks.md) owns the gate and its evidence.

## Install and migrate

Requirements: macOS or Linux, Python 3.9 or later, Git, POSIX `flock` support on
a local user home filesystem, readable process ownership/environment metadata,
and the existing Review-ready prerequisites including `Rscript` on PATH.
Windows is unsupported: use of this command fails without running a gate.
Windows needs its own process containment implementation and verification;
this does not change Windows package CI or ordinary tests.

From the implementation checkout:

```sh
python3 tools/install-review-ready.py
```

This installs a standalone command at
`~/.local/bin/marginplyr-review-ready`. It does not depend on this checkout
remaining in place. Shared state is in
`~/.local/state/marginplyr/review-ready/`, resolved from the OS account's home,
not `HOME`, `XDG_STATE_HOME`, a Git directory, or an agent profile. There is no
per-checkout capacity or public override. Add the installed bin directory to
PATH for human terminals, Codex, Claude Code, Grok Build, and any other
execution backend. An absolute path is equally valid. Verify each environment
with `command -v marginplyr-review-ready` and
`marginplyr-review-ready --status` before its next check. Installation reports
a PATH problem rather than editing shell profiles automatically.

In every existing session, acknowledge this entry point and replace remembered
`Rscript tools/review-ready-check.R` invocations before starting another check.
Refresh/restart sessions whose instructions or PATH cannot be updated. Older
worktrees need not merge this implementation: run the installed command from
their root. Unmodified older scripts invoked directly bypass this cooperative
boundary. Installation cannot intercept them. The updated R entry point delegates to the
common CLI when invoked directly; it refuses missing installation. Do not launch a fallback direct
check when installation, process inspection, or admission fails.

## Invocation and outcomes

```sh
marginplyr-review-ready
```

The CLI requires and pins a clean committed HEAD before waiting. Waiting lines
show that SHA, elapsed wait, and the active commit/worktree when known. There
is no queue position or ETA. Immediately before admission it revalidates HEAD
and cleanliness; a stale request exits 3 without checking a newer revision.
After admission, a disposable shared Git clone at the pinned SHA invokes that
revision's existing fixed R gate. This prevents later source edits, changed
HEAD, and untracked files from substituting the requested revision, including
on older worktrees. The gate still archives the exact SHA and produces its
usual live progress, full check, diagnostics, and authorized native-crash retry.
The original worktree is never checked out or reset by the CLI.

Retain the same execution handle through waiting, execution, cleanup, and
terminal exit. Exit zero from a check invocation forwards the gate's zero;
nonzero gate outcomes remain nonzero (including their original exit code).
Exit 130 denotes cancellation, 3 a stale waiting request, 4 blocked recovery,
and 2 setup/management failure. A gate can also return one of these nonzero
codes; the English outcome identifies its source. Slot admission is progress,
not success. `--status` and `--recover` explicitly report that they run no check;
their success is never check evidence.

## Cancellation and recovery

Send SIGINT, SIGTERM, or SIGHUP to the retained CLI process. Execution tools
must propagate cancellation, not discard the handle. A changed management
parent also triggers cancellation. Closing a conversation/UI alone is not a
signal when its execution backend remains alive. Waiting cancellation touches
no active job. Running cancellation signals only processes identified as owned
by this request, escalates to SIGKILL after two seconds, and confirms their
shutdown before releasing capacity. Failure to confirm within ten seconds
keeps the journal blocked.

Ownership combines an inherited request marker, recorded PID/birth identity,
process groups, and observed descendants; processes are inspected locally and
full environment output is never printed or saved. Stopped zombies consume no
execution capacity. Permission/inspection failures block rather than assume
cleanup. This is supervision of the fixed trusted gate, not containment of
arbitrary programs that deliberately erase environment and detach between
observations. No guarantee covers every possible macOS descendant.

The permanent advisory lock is not removed. A durable journal is written before
spawning; even SIGKILL between spawn and ownership registration leaves admission
blocked. A killed manager's free OS lock or dead PID is not shutdown evidence.
Inspect using:

```sh
marginplyr-review-ready --status
marginplyr-review-ready --recover
```

Recovery never kills an unverified PID or deletes a lock. It refuses while
known owned work or the manager is alive. If a manager ended abnormally,
same-boot observations cannot rule out a detached, unrecorded descendant, so
recovery requires an OS restart. After reboot, `--recover` recognizes the older
boot identity and clears its unfinished journal; it does not run or pass a
check. Never manually delete the journal to unblock admission. Invalid journals
or unavailable boot/process evidence require inspection and restoration of
readable metadata, not bypassing the safeguard. Uncertain checkouts are
retained at the printed path for inspection; remove them only after recovery.

## Focused verification

```sh
python3 tools/test-review-ready-admission.py
Rscript .github/scripts/verify-review-ready-check.R
```

The Python fixtures substitute the OS-home adapter for independent test
users and the boot-identity adapter for simulated restart recovery and provide a lightweight `Rscript` executable at the external checker
boundary. They run real Git worktrees/clones and subprocesses. They do not run
several real package checks or expose a production capacity override. Process
inspection and signals must be permitted in the execution environment; a
sandbox that prohibits them must grant that capability for the common command.
Linux uses the same POSIX implementation; macOS and Linux fixture results must
be identified separately in delivery evidence.
