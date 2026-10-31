# Shared local Review-ready admission

`marginplyr-review-ready` admits one complete Review-ready check per OS user
across marginplyr clones and worktrees. Full retains four test workers;
Documentation retains package build/check with `--no-tests` and separate
documentation assertions; Repository omits build/check for supported prose. All paths use the
same slot. Waiting does not load R dependencies or run any check stage. CI
and commands outside this boundary are unchanged. The accepted design is
[resource admission](../design/specs/review-ready-resource-admission.md),
and [local checks](../design/agents/local-checks.md) owns the gate and its
evidence.

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

In every existing session, acknowledge this entry point and replace
remembered `Rscript tools/review-ready-check.R` invocations before starting
another check. For waiting/checking work, refresh participating sessions through
[Session refresh](../design/agents/local-checks.md#session-refresh), including
explicit acknowledgement; installation alone does not reload their guidance.
Refresh/restart sessions whose instructions or PATH cannot be updated. Older
worktrees need not merge this implementation: run the installed command from
their root. Selection requires the submitted R entry
point's versioned protocol marker. Unsupported older revisions retain their
existing Full gate; an older installed launcher with new sources also
retains Full. Refresh the installed launcher from the implementation
checkout to enable selection; source edits alone never update the standalone
installed copy. Compare its bytes with `tools/review-ready-admission.py`
after installation, and verify real Full, Documentation and prose-only
Repository invocations through the installed command. Unmodified older
scripts invoked directly bypass this cooperative boundary. Installation
cannot intercept them. The updated R entry point delegates to the common CLI
when invoked directly; it refuses missing installation. Do not launch a
fallback direct check when installation, process inspection, or admission
fails.

## Invocation and outcomes

```sh
marginplyr-review-ready
```

The CLI requires clean committed HEAD and pins submitted SHA, local
`refs/heads/main` SHA, and unique merge-base before waiting. No automatic fetch
or previous-success baseline is used. `Review-ready selection` prints those
identities, selected path, reasons, and required verifiers as JSON. Paths and
reasons containing unusual filename characters are JSON escaped. Full is the
fallback for unavailable or unsupported comparisons. The authoritative path
and verifier registry is `PROSE`, `DOCUMENTATION`, `GENERATED_INPUTS`, `REFERENCE_CHECKS`, and `CONTRACTS` in
`tools/review-ready-admission.py`; [local checks](../design/agents/local-checks.md)
owns the policy. There is no caller-selected path or unrestricted skip flag.

A later local main update does not affect the pinned comparison. Waiting
lines show that SHA, elapsed wait, and the active commit/worktree when
known. There is no queue position or ETA. Immediately before admission it
revalidates HEAD and cleanliness; a stale request exits 3 without checking a
newer revision. After admission, a disposable shared Git clone at the pinned
SHA invokes that revision's R gate with an internal request-bound selection
transport. This prevents later source edits, changed HEAD, and untracked
files from substituting the requested revision, including on older
worktrees. The gate archives the exact SHA. All paths run spelling and both
linters, loading the package before lintr, plus mapped artifact
verification, including generation on Full when mapped inputs change. Completed
stages are printed individually. Repository
explicitly omits package build/check and reports its own result. Full
retains dependency checks, full-suite live progress, snapshots, diagnostics,
NOTE policy, and authorized native-crash retry. Documentation retains the same
dependency checks, installation/loading, help, codoc, examples and vignettes,
runs the registered assertions serially without skips, and explicitly reports
the complete suite and snapshots omitted. Native-crash retry preserves its
`--as-cran --no-tests` arguments and tarball. The terminal CLI outcome
includes the selected path; zero alone does not imply that Full ran. The
original worktree is never checked out or reset by the CLI.

Retain the same execution handle through waiting, execution, cleanup, and
terminal exit. While waiting or checking, follow
[Bounded work while waiting or checking](../design/agents/local-checks.md#bounded-work-while-waiting-or-checking);
that workflow uses the retained handle and does not control admission.
It keeps investigation and any separate-defect publication with the submitting
agent while the selected Review-ready path waits or runs. Its execution recipe
and terminal-completion transitions apply throughout.
Exit zero from a check invocation forwards the gate's zero;
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

Recovery never signals a process or deletes a lock. For a request from the
current OS boot, it holds exclusive admission while taking two complete
process observations, separated by 0.2 seconds. Both must find the recorded
manager and every identified owned process stopped. Ownership uses the same
request marker, PID/birth identities, process groups and descendants as the
running supervisor; an ambiguous group or failed observation leaves the journal
intact. A dead manager or free lock alone is insufficient.

After confirmed shutdown, `--recover` clears the stale
journal without requiring an OS restart. It also recognizes a request from an
earlier boot. Either recovery reports that no check was run; an interrupted
check never becomes a pass. Never manually delete the journal to unblock
admission. Invalid journals or unavailable boot/process evidence require
inspection and restoration of readable metadata. Uncertain checkouts are
retained at the printed path for inspection; remove them only after recovery.

## Focused verification

```sh
python3 tools/test-review-ready-admission.py
Rscript .github/scripts/verify-review-ready-check.R
```

The Python fixtures cover cumulative/final-tree selection, NUL-delimited names,
rename sides, deletions, unsafe modes, missing/ambiguous bases, and pinned main.
They also substitute the OS-home adapter for independent test
users, process-observation faults and late-visible descendants for recovery,
the boot-identity adapter for simulated restart recovery, and a delayed
filesystem open for the completion/read race. A lightweight `Rscript` executable
is provided at the external checker
boundary. They run real Git worktrees/clones and subprocesses. They do not run
several real package checks or expose a production capacity override. Process
inspection and signals must be permitted in the execution environment; a
sandbox that prohibits them must grant that capability for the common command.
Linux uses the same POSIX implementation; macOS and Linux fixture results must
be identified separately in delivery evidence.

### Agent workflow walkthroughs

After refreshing participating sessions, exercise these decision scenarios
against *Bounded work while waiting or checking* in the local check guide.
Use hypothetical defects for the walkthrough; publish only actual confirmed
defects under that guide's evidence requirements. Record the chosen action and
checkpoint in external notes, distinguishing a walkthrough from execution
evidence. These scenarios verify agent decisions, not CLI process containment;
the subprocess fixtures above own that check.

| Scenario | Input at a natural work boundary | Required action |
|---|---|---|
| Continuation | Submission requirements are accounted for; the gate is still waiting or running, and another meaningful hypothesis remains. | Form the hypothesis before inspecting test coverage, investigate and record evidence, then continue into another package area when appropriate. Retain the original gate handle. |
| Hypothesis exhaustion | No meaningful uninvestigated hypothesis remains. | Checkpoint dispositions and what new evidence would reopen them, then wait on the retained invocation without manufacturing activity or findings. |
| Confirmed submission fix | A reproduction establishes a required fix while the request is waiting or running. | Cancel only this request using its handle, collect terminal cleanup and additional owned work, then edit, commit, and resubmit. A suspicion alone continues investigation. |
| Unrelated defect | Evidence confirms a defect outside the submission. | Keep the request alive; collect contract, reproduction, impact, SHA/environment, current-main status, and duplicate search. Publish an English issue only if still valid and nonduplicate; otherwise link or checkpoint the disposition. Keep repair separate and failed publication pending. |
| Terminal completion | The gate returns while an additional execution or unrelated issue draft is unfinished. | Checkpoint, stop and collect the additional execution and children, then use the original terminal outcome. Partial or interrupted work, or work that skipped a relevant test, is not a pass. Proceed to review after gate success and resolution of concrete submission concerns, without waiting for broad exploration or issue preparation. |
