# Shared local Review-ready admission

`marginplyr-review-ready` admits one complete Full Review-ready check per OS user
across marginplyr clones and worktrees. Full retains four test workers;
Documentation retains package build/check with `--no-tests` and separate
documentation assertions; Repository omits build/check for supported artifacts.
Eligible Documentation and Repository requests start immediately under their
own supervision, including while Full runs or needs recovery. They have no
separate aggregate slot or host CPU/memory bound; build/render and limited
verification may overlap. Full waiting does not load R dependencies or run any
check stage. CI
and commands outside this boundary are unchanged. The accepted design is
[resource admission](../design/specs/review-ready-resource-admission.md),
and [local checks](../design/agents/local-checks.md) owns the gate and its
evidence.

The cumulative selection and executed workload are separate under
[ADR 0038](../design/adr/0038-reuse-unaffected-review-ready-stage-evidence.md).
A Full obligation can reuse a compatible ancestor's actual build/check unit
and run its remaining registered contracts under independent supervision.
Every invocation still executes source checks and changed-artifact contracts.
The stage plan lists executed, reused and omitted units and their origins; a
limited execution with inherited Full evidence is not a new Full pass.

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
point's versioned protocols and installed stage fingerprints. Unsupported older
revisions retain their locked Full gate; an older installed launcher with new
sources either uses its verified locked contract or fails safely. Refresh the installed launcher from the implementation
checkout to enable selection; source edits alone never update the standalone
installed copy. The installer embeds hashes of the reviewed stage sources and
their explicit dependencies; `STAGE_CONTRACTS` and `capability_patterns()` in
`tools/review-ready-admission.py` own that registration. Refresh installation
after changing a contract source. The standalone installation embeds the
evidence implementation and its protected expectation fingerprints as well.
Current gates require supervised protocol 3
transport with the pinned comparison base; older transports cannot replace CI
artifact verification with Full. Verify the installed bytes with
`python3 tools/install-review-ready.py --verify`, and verify real Full, Documentation and supported-artifact
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
and verifier registry is `PROSE`, `DOCUMENTATION`, `GENERATED_INPUTS`,
`ARTIFACTS`, `CHECKING`, `REFERENCE_CHECKS`, and `CONTRACTS` in
`tools/review-ready-admission.py`; [local checks](../design/agents/local-checks.md)
owns the policy. There is no caller-selected path or unrestricted skip flag.

`tools/review-ready-evidence.py` registers source/environment identities,
protected expectation recognition and stage evidence. Before capacity selection,
the CLI searches new-format records for compatible ancestor executions; it
never changes the pinned comparison base. Identity probes run no verification
stage. Missing or incompatible evidence schedules the required work. Old
launchers, old logs and CI passes cannot issue this format.
`EXECUTION_METADATA` registers Codex call/proxy-attribution IDs and Claude Code
session/PID metadata by exact format. Unknown formats, other variables,
Claude shell/environment-file settings and proxy configuration stay inputs.
The same registered child environment is used for the identity probe,
documentation proof and actual gate and descendants: known-format IDs are
removed there, while manager environment and supervisor transport remain
available. Both agents use this same command, user-wide capacity and evidence
store. Their profile directories cannot partition admission. A probe without
the versioned visible-name audit acknowledgement cannot establish reusable
identities.

The common supervisor writes a sealed `evidence.json` beside a request's
terminal `record.json` only after confirming process and workspace cleanup.
The private `evidence.key` binds integrity and provenance to the OS user state.
`evidence-index.json` seals the complete issuance inventory and sequence;
`evidence.lock` serializes that short write independently of Full admission.
An authenticated `observation.json` retains attempted stage outcomes before
cleanup, including when cleanup fails. Recovery preserves it and can complete
its interrupted index write; observations never discharge an obligation.
Unobserved authorized execution journals, including running journals after
manager loss, fence reuse conservatively. Unavailable cache reads disable reuse;
interrupted publication writes still require recovery. Stage
observation order prevents delayed cleanup from hiding a later failure.
Keep referenced origins and observations available. Missing,
corrupt or incompatible records disable reuse; they never become omission.
Before gate start the pinned worker revalidates the plan and can request the
existing authenticated Full promotion. Invalidation after gate start fails
without switching gates. The same request and handle own all these transitions.

A later local main update does not affect the pinned comparison. Waiting
lines show that SHA, elapsed wait, and the active commit/worktree when
known. There is no queue position or ETA. Immediately before admission it
revalidates HEAD and cleanliness; a stale request exits 3 without checking a
newer revision. Each request has a durable record, private token and channel,
PID/birth identities, owned-child ledger, gate output and disposable source.
Records and output remain under the shared state's `requests/` directory after
confirmed source cleanup. After supervision setup (and admission for Full), a disposable shared Git clone at the pinned
SHA invokes that revision's R gate with an internal request-bound selection
transport. An independent worker checks its installed source capabilities again
before starting the gate. This prevents later source edits, changed HEAD, and untracked
files from substituting the requested revision, including on older
worktrees. The gate archives the exact SHA. An existing ordinary R source edit
carries a provisional Documentation selection until its supervised proof resolves.
A Full result requests authenticated promotion; parse errors and invalid proof
transport fail. No proof or regeneration runs while waiting for Full capacity.
The resolved path is printed before the gate and in the terminal outcome.
The local check guide owns eligibility and stale-output policy.

All paths run spelling and both
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
original worktree is never checked out or reset by the CLI. Completed and omitted
stages and NOTE dispositions appear in gate output. Terminal output identifies
the request/commit, final path, capacity mode, cleanup and promotion history.

The exemption contract requires serial Documentation assertions and ordinary
limited Repository tests in one test process. Only the registered progress
fixture tests parallel behavior using at most two test workers. Larger parallel
tests, complete suites and unknown or changed stage contracts require Full;
neither filenames, source-text searches nor a caller skip flag establish safety.
Spelling, jarl and package-aware lintr always run, with package loading before
lintr, on every path.

Before the worker starts the gate, later eligibility work may request promotion
over the request-private manager
channel. The manager authenticates protocol, token, request/commit, epoch and
expected running state, refusing duplicates, stale messages and cancellation.
The worker authenticates gate start before executing R or parsing its transport;
promotion after that boundary is refused, preserving validation failures as
terminal outcomes even if R never obtained gate authorization.
It stops and collects all preparatory work and children, persists Full waiting,
acquires the existing common lock, revalidates submitted HEAD/cleanliness and
persists capacity ownership before restarting the complete Full gate. Request,
comparison identities, execution handle and preparatory evidence remain intact.
The R gate obtains live authorization from that manager; environment changes
alone cannot grant Full. Missing/corrupt selection transport fails without
unrestricted fallback. Parse errors, failed assertions and stale generated
outputs remain failures, not promotion. Native retries keep the same path,
tarball and arguments: Documentation stays exempt and Full retains its slot.

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

The permanent Full advisory lock is not removed. A durable request record is
written before spawning; even SIGKILL between spawn and child registration
leaves recovery evidence. Full also maintains the legacy `request.json` fence
under that same lock so older launchers cannot overlook unfinished Full work.
A killed manager's free OS lock or dead PID is not shutdown evidence.
Inspect using:

```sh
marginplyr-review-ready --status
marginplyr-review-ready --recover REQUEST
```

Status lists request IDs, selected paths, capacity ownership, running/waiting
states and requests needing recovery. Recovery requires one explicit request ID;
an absent, unknown or ambiguous selection lists candidates and changes nothing.
Use `legacy` for an old standalone journal. An unfinished exempt request does
not globally block others. Unknown/corrupt evidence or any uncertainty about
entry into Full protects Full admission conservatively. New requests unable to
establish their own supervision fail before starting work.

Recovery never signals a process or deletes a lock. It locks the target request
and, where Full ownership is possible, the common Full slot. For a request from
the current OS boot, it takes two complete
process observations, separated by 0.2 seconds. Both must find the recorded
manager and every identified owned process stopped. Ownership uses the same
request marker, PID/birth identities, process groups and descendants as the
running supervisor; an ambiguous group or failed observation leaves the journal
intact. A dead manager or free lock alone is insufficient.

After confirmed shutdown, recovery removes only the target's disposable source
and channel and records an interrupted outcome, without requiring an OS restart.
It clears a legacy fence only for its matching request. It also recognizes a request from an
earlier boot. Either recovery reports that no check was run; an interrupted
check never becomes a pass. Never manually delete the journal to unblock
admission. Invalid journals or unavailable boot/process evidence require
inspection and restoration of readable metadata. Uncertain checkouts are
retained at the printed path for inspection; remove them only after recovery.

## Focused verification

```sh
python3 tools/test-review-ready-admission.py
python3 tools/test-review-ready-evidence.py
Rscript tools/test-review-ready-roxygen.R
Rscript .github/scripts/verify-review-ready-check.R
python3 tools/test-ci-artifacts.py
python3 tools/verify-ci-artifacts.py
```

The Python fixtures cover cumulative/final-tree selection, NUL-delimited names,
rename sides, deletions, unsafe modes, missing/ambiguous bases, and pinned main,
concurrent exempt execution with Full active or recovery-blocked, isolated
records/cancellation/recovery, authenticated promotion and its waiting states,
and unknown source capabilities that could hide complete-suite work.
The evidence fixtures establish new-format baselines and exercise the #1107
correction with real R verifiers, original/missing/incorrect dependency failures,
mixed branches, repeated origins, incompatible/corrupt/missing evidence,
Documentation boundaries, timestamp comparisons and cancellation under reuse.
Their external package-check stand-in is explicitly a process fixture; it is
not a real package Full pass. The implementation's own Full gate supplies that.
Artifact fixtures exercise removed workflow wiring, changed conditions and
environment, YAML syntax failures, missing actionlint, unsupported inputs, and
CI script behavior. Workflow contracts preserve all parsed fields while
allowing comments and formatting to change. Local checks owns the supported
cases and prerequisites; a package-check result never fills missing CI
verification. Site configuration routes render and verify output within the
same supervised invocation, with Full retaining its slot when selected.

The process fixtures also substitute the OS-home adapter for independent test
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
