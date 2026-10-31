# Local review-ready checks

This is the operational authority for the local boundary before a marginplyr
commit is published for review. It defines which commits need the boundary,
what each selected path proves, and when its evidence expires. Pull-request CI
remains the cross-platform and release-matrix authority.

## Language

A **Package-affecting change** can change the source package, an input that
generates it, installed behavior, or the result of checking it. This includes
package code, tests, examples, vignettes, package data, installed files,
dependency metadata, generated documentation, generation sources, and build
exclusions. A tests-only change is Package-affecting. When the classification
is uncertain, use this one.

A **Repository-only change** can be shown to affect none of those things.
Design and investigation notes and agent instructions that drive no
generation are ordinary examples. Exclusion from the package is not
sufficient: CI, release, or local-check tooling is Package-affecting when it
can change a package check result. Repository-only does not mean unchecked.
Supported artifacts use the Repository path below. Unsupported changes select
Full; a missing CI artifact route remains unresolved and cannot be certified by
a package-check pass.

The **Review-ready check** is the local gate for one clean committed revision.
It selects **Full**, **Documentation**, or **Repository** from the cumulative
final tree difference against the unique merge-base with local `main`. Full
retains the package boundary; Documentation retains build/check without the complete suite, and
Repository omits package build/check for supported repository artifacts.
The selection states cumulative obligations. A separate stage plan records
execution, valid reuse under [ADR 0038](../adr/0038-reuse-unaffected-review-ready-stage-evidence.md),
or omission; a Full obligation discharged by reuse is not a new Full pass.
Focused tests and the wider release process have separate roles.

## Boundary

Run the Review-ready check before any of these transitions:

- opening a non-draft pull request;
- marking a draft pull request ready or requesting review; or
- pushing a change to a pull request already ready for review.

The alternative for the last case is to return the pull request to draft,
push the change, and pass the check before making it ready again. A later
commit requires a fresh invocation; previous successful checks never narrow
the comparison base. They may discharge an unaffected stage only through the
structured evidence mechanism below.

The checked state is a clean committed `HEAD`. The command archives that exact
commit into a disposable directory, so ignored local files cannot enter the
evidence. Run it from the repository root:

```sh
marginplyr-review-ready
```

Install the common command using `python3 tools/install-review-ready.py`.
[Shared admission](../../tools/review-ready-admission.md) owns installation,
platform support, cancellation, recovery, and migration of existing sessions.
All agents and terminals use that command; missing setup has no direct-command
fallback. A waiting request pins this clean commit and is cancelled before
admission if HEAD or cleanliness changes. One complete Full check is admitted
per OS user across all clones and worktrees, preserving four internal workers.
The slot is required when Full build/check actually executes. A cumulative Full
selection whose package unit is validly reused can use independent supervision
when every remaining workload has an installed limited capability.
Eligible Documentation and Repository requests start under independent
supervision without acquiring that slot, including while Full needs recovery.
Their build, rendering and limited verification may overlap; no aggregate slot
or host CPU/memory bound is imposed on them.
When the invocation waits or runs, enter *Bounded work while waiting or
checking* below while retaining the execution identity described under
*Observing one invocation*.

The check invocation takes no path or skip options. Before waiting it pins the
submitted commit, `refs/heads/main` commit, and unique merge-base, using local
main without fetching. A later main update does not replace these identities.
Missing or ambiguous bases, comparison failures, empty differences, unknown
paths, mode/type changes, symlinks, and submodules select Full with a reason.
An unreadable comparison supplies no admissible base; current gates refuse
verification rather than substitute Full for undetermined artifact checks.
Deletion and both sides of a rename participate in the NUL-delimited final
tree comparison. Reverted historical edits do not participate.

The supported categories and required verifier mapping have one home:
`PROSE`, `DOCUMENTATION`, `GENERATED_INPUTS`, `ARTIFACTS`, `CHECKING`,
`REFERENCE_CHECKS`, and `CONTRACTS` in
`tools/review-ready-admission.py`, together with the stage input registration
and expectation recognizer in `tools/review-ready-evidence.py`. Prose includes agent/domain/design Markdown,
Markdown investigation notes, conduct policy, and operational Markdown under
`tools/`. Supported blobs must be UTF-8 text without non-prose control
characters; unknown encodings or binary content select Full. Other formats
and executable investigation scripts remain Full. Package licenses remain Full.
Documentation supports README Markdown/Quarto, NEWS Markdown, generated Rd,
explicit image formats under `man/figures/`, top-level `vignettes/*.qmd`, and
explicit vignette image, bibliography and citation-style formats. Executable
vignette eligibility follows `documentation_sources()` in
`tests/testthat/test-documentation.R`; unscanned document formats and nested
Quarto documents remain Full. Unknown formats remain Full.
Existing ordinary `R/*.R` edits are candidates for the supervised R documentation
proof in `tools/review-ready-roxygen.R`. It compares parsed expressions without
evaluation and preserves the placement of unsafe directives among executable
tokens. Blank lines anywhere, CRLF/LF, ordinary comments, inline lint directives
and formatting may differ when the parsed expressions remain identical. These
edits use the Documentation verification level even when they are not document
edits. A lint correction that changes the parsed expressions remains Full;
its motivation does not establish executable equivalence.
Added/deleted/renamed R files, unsupported forms, coverage directives and package
configuration changes remain Full.
The allowlist in that proof owns the supported tags; unknown and evaluated tags,
namespace/import/export and load-order directives are conservative Full cases.
The provisional Documentation selection carries a pending proof. A compatible
worker runs it under independent supervision against archives of the pinned
base and submitted commit before starting the gate. A failed parse is a
validation failure; a supported result requiring Full requests authenticated
promotion. The resolved selection and terminal outcome identify the final path.
Full wins over Documentation, which wins over Repository, across the cumulative
diff.

CI/configuration routes are deliberately narrow:

| Artifact | Required verification |
|---|---|
| The eight workflows named by `WORKFLOWS` | actionlint syntax, executable workflow contracts, and verifier-invocation wiring |
| The CI check-log summary runner | command fixtures over check status, all diagnostic details, and absent logs/status |
| `altdoc/quarto_website.yml` | install the submitted source in an isolated library, render the site, then verify output and markers |
| Codecov configuration | the existing coverage exclusion policy and its fixtures |
| Codex and Claude configuration | existing network/Codex policy checks; CI artifact checks also reject invalid Claude JSON |
| The literal review dependency expectation in the CRAN preflight verifier | protected comparison structure, independent tool-use and negative contracts, and both preflight and Review-ready verifiers |

Workflow support covers comments and formatting while preserving every parsed
field. Names are protected too because `github.workflow` contributes to the
concurrency key. `tools/ci-workflow-contracts.json` records those semantics: triggers,
job/step conditions, environment, setup, commands, dependency wiring, and all
other executable fields must agree. A change outside that contract fails with
CI validation unresolved. Extend the protected contract and command fixtures
under Full when intentionally changing executable workflow behavior; syntax
alone is insufficient. `tools/verify-ci-artifacts.py` reads the pinned base
passed by admission, so a later main update cannot change its changed-artifact
inventory. Outside admission it derives a unique base from local main, or
origin/main in CI. CI fetches the history that comparison needs.

Provide `actionlint` on PATH (the CI job installs version 1.7.12), and the R
`yaml` and `jsonlite` packages, before checking supported CI artifacts. The
review dependency field records these and altdoc; site rendering also uses the
website dependency field. Missing
prerequisites, unsupported CI scripts/configuration, removed wiring, and known
verification failures fail the artifact step; Full is not substitute evidence.
The syntax check disables optional shellcheck/pyflakes integrations; executable
contract checks and script fixtures supply the additional local evidence.
Remote job execution remains CI's responsibility.

`altdoc/pkgdown.yml` is generated metadata that rendering overwrites, so it has
no Repository route: rendering would erase its submitted changes before they
could be verified. It remains Full with CI validation unresolved.

The dependency expectation route recognizes only an existing ordinary-file
modification whose bytes outside the registered literal vector are identical.
It preserves the strict assertion and independently checks declared tools
against their actual generation calls, reproduces the original #1107 mismatch,
and refuses missing or incorrect dependencies. Changing DESCRIPTION, weakening
the assertion, or changing other verifier logic is outside that route.
Other expected values, fixtures, snapshots, diagnostics and tooling logic need
their own registered independent contract; their location does not qualify them.

Selection/admission/installer/evidence/progress/local-gate machinery, its dependency
helpers, shared NOTE policy, artifact checkers, workflow contracts, and their
fixtures retain Full obligations and mapped contract/process checks. They
cannot certify their own changed implementation for limited execution. The common
reference/context/invocation verifiers and their CI helper also remain Full;
they help decide this check's outcome. The same
routes run for cumulative mixed changes, even when package edits require Full.

### Reusing unaffected stages

`tools/review-ready-evidence.py` owns registered inputs, evidence validation and
deterministic origin selection. Its default source identity includes every
tracked path's name, type, mode and object; only its explicit audited non-inputs
are excluded. Both sides of additions, deletions and renames consequently
participate. The dependency expectation vector is normalized only while all
protected verifier bytes remain identical. All of DESCRIPTION, package tests,
the progress reporter, generation sources and the trusted machinery remain
inputs. A newly registered non-input needs an audit of every unit it affects.

Build and check are one unit. Full and Documentation remain distinct;
compatible Full evidence can discharge Documentation build/check, but never
the reverse. Generation and Documentation assertions are separate units.
Dependency syntax and required Suggests still execute for cumulative package
paths; spelling, both linters, common reference checks and changed-artifact
contracts execute at every submitted SHA. A failing contract ends the request.

The common supervisor seals a new-format success record in its private user
state only after terminal success and confirmed owned-process/workspace cleanup.
It binds the repository, user, request, submitted/main/base identities, installed
supervisor, completed units, inputs, environment, check arguments, attempts and
NOTE classifications. Only an ancestor's actual execution is reusable; repeated
reuse points to that origin. Another clone of the same repository is eligible.
Old logs, CI results and agent reports cannot establish a baseline. Corrupt or
missing records/origins, incomplete cleanup, incompatible identities, unknown
NOTEs, or later same-input/environment stage failures prevent reuse. There is
no age-only expiry.

Environment identities include R and its checker implementation, OS/architecture,
resolved tools and runtime assets, Python and R libraries with content/change
metadata, native dependencies, ordered R libraries, startup/build configuration,
environment variables, locale and timezone. Caller loader overrides, custom
startup/build hooks and unsupported native resolution have unregistered input
closures and disable reuse. The native inspection routes are macOS `otool`
and Linux `ldd`; unknown evidence strengthens verification. These are cooperative
observations, not environment isolation. Keep tools and libraries unchanged
throughout the request. Identity changes during validation fail a request
relying on reuse; unavailable identity cannot issue reusable success.

Build/check reuse additionally reevaluates the original inspected timestamp
inventory and the submitted archive's commit timestamp against the registered
R checker and effective leeway. Equality is allowed; a future boundary violation,
clock rollback, unknown checker or missing inventory requires execution.
Unregistered clock-dependent package contracts cannot reuse build/check.

Eligibility is revalidated before gate start. Invalidation there promotes the
same independently supervised request to Full before validation. After gate
start it is a failure, without fallback. The [case matrix](../specs/review-ready-verification-followups.md#case-matrix)
records the supported family and conservative exclusions. Missing evidence for
a required unit means execution, never omission. This mechanism changes no CI
package-check or coverage obligation.

All paths always execute package spelling, jarl, and package-aware lintr
(the three source stages described below), followed by mapped artifact
verification. Reference/context/invocation verification runs against the
pinned disposable Git checkout; common source checks run against its archive.
Operational docs also run their mapped contracts. The same artifact routes run
on Full, so package checking cannot hide a required verifier failure. Missing
or failing verifiers fail the invocation without changing paths.

Exemption additionally requires compatible supervision and installed source
capabilities for every selected stage. `STAGE_CONTRACTS` in the common launcher
registers the reviewed workloads; installation fingerprints their source and
explicit dependencies. Unknown or changed stage code requires Full, even if
the cumulative prose diff alone would select Repository. This prevents a
Repository verifier from hiding a complete suite. Documentation assertions and
ordinary limited tests run in one test process with parallelism disabled.
Only the explicitly registered parallel-behavior fixture may start testthat
workers outside Full, with at most two actual workers. Larger parallel work,
complete-suite work and unsupported contracts require Full.

Eligibility work ends before the worker starts the gate, including transport
parsing. Once validation starts,
promotion is refused so an observed failure cannot be replaced by another gate.
If later eligibility work requires Full, the supervisor stops and collects
preparation and its owned children, persists Full waiting, acquires the common
slot, revalidates submitted HEAD/cleanliness and records owned capacity before
restarting the complete Full gate. The request, comparison identities, handle
and evidence remain the same. This transition requires the private authenticated
manager channel; changing environment variables cannot grant capacity. Missing
or corrupt transport fails without an unrestricted Full fallback. Known
validation failures, including stale generated output, never become promotion.

Repository stops after those checks. It records completed checks and explicitly
reports package build/check, the full suite, and snapshots as omitted. Its
success is a Repository result, never a Full pass.

Documentation retains the dependency scan and provisioned optional Suggests,
builds the package, and uses `R CMD check --as-cran --no-tests`. Installation,
loading, Rd syntax/references, codoc, examples and vignette execution/rebuilding
remain enabled. It separately runs the exact registration in
`review_ready_documentation_tests()` in `tools/review-ready-documentation.R`
against the submitted Git archive with helpers, setup and teardown intact and
testthat parallelism disabled. Missing or empty selections, failures, warnings
and any skipped assertion fail; selection never expands to the suite.

That registration is maintained by hand: when a test gains documentation
inputs, audit whether Documentation must execute it and update the registration
or narrow eligible inputs. A filename list does not prove completeness. It
currently covers documentation, DuckDB storage and package metadata contracts.
Documentation explicitly omits the full suite and snapshots; its success is
not evidence of their execution.

Mapped generation runs on Full as well as Documentation whenever README/help
outputs or their supported package inputs change. It regenerates roxygen help,
NAMESPACE and README in another disposable committed-tree copy, installing that
copy for README execution. All generated path names, types, permissions and
bytes participate, including added/removed figures and unexpected outputs
outside documentation. A generated namespace, dependency or
runtime-configuration effect requires Full. A mismatch with the submitted tree
still fails validation: fallback never hides stale output, and the corrected
commit receives a fresh selection. The diagnostic records
Quarto and bundled Pandoc versions to distinguish stale output from writer
drift. Regenerate and commit the result; never accept silently repaired output.

For new or changed rejected vignette calls, preserve `error: true`, `purl: false`
and optional-dependency `eval` settings. Review the extracted/rendered call and
confirm its stated refusal with an existing direct assertion or a small
reproduction of that submitted SHA. Record the assertion or reproduction in the
PR; successful rendering alone does not establish the refusal reason.

Full runs, in order:

1. a test-source dependency scan that refuses every package candidate absent
   from `DESCRIPTION`;
2. a presence and version check for every optional Suggest registered by the
   test suite;
3. package spelling, reporting unknown words and their locations only when it
   finds them;
4. `jarl check .`;
5. package-aware lintr after `pkgload::load_all()`; and
6. a source-tarball `R CMD check --as-cran` against disposable empty local
   CRAN and Bioconductor indexes. It sets `NOT_CRAN=true` to run snapshot
   expectations and `MARGINPLYR_REQUIRED_SUGGESTS` to require every registered
   optional Suggest, so the full suite runs once in four test processes without
   coverage measurement. `TESTTHAT_PARALLEL=true` and `TESTTHAT_CPUS=4` are set
   by the check rather than package-wide test configuration.
   `DESCRIPTION`'s `Config/testthat/start-first` queues all measured files in
   descending local per-file elapsed time. Testthat appends newly added unlisted
   files and retains every test; this priority applies to all testthat runs.
   Refresh the list from four-worker measurements when suite timings change.
   [The #1085 measurements](../../investigation/review-ready-test-order-2026-10-30.md)
   record the environment, worker timings, and ordering comparison.
   Remote incoming checks and external-clock verification are disabled; the
   local future-file-timestamp comparison remains enabled.

### Observing one invocation

During the source-tarball check, the monitor forwards testthat's native
`Starting N test processes.` message from its check transcript. This is the
count testthat selected, including its limit by the number of test files,
rather than a repeated configuration value. It also forwards native worker
startup, unexpected-exit, and teardown diagnostics with their cause and
backtrace from that transcript. Reading continues after the reporter finishes,
and does not wait for reporter events to begin, so worker lifecycle failures
remain visible in either phase. These observations do not replace the terminal
check result. English progress lines report elapsed test time, completed files out of the discovered total, their percentage, and the
last completed file. The percentage counts files, not runtime or passing tests;
it does not estimate remaining time. A five-second heartbeat continues while
a long file runs. The ordinary testthat check summary and the check process's
terminal result remain the verdict.

The local check supplies an archived reporter through
`MARGINPLYR_REVIEW_PROGRESS_REPORTER` and an attempt-local event path through
`MARGINPLYR_REVIEW_PROGRESS`. Without these, `tests/testthat.R` uses testthat's
ordinary check reporter, including in CI. Only file completion adds an event;
the check keeps testthat's buffered parallel reporting. `R6` belongs to
`Config/Needs/review` because this local reporter needs it; package runtime and
ordinary package tests do not call it directly.
`Rscript tools/test-review-ready-progress.R` exercises the reporter and process
boundary on passing and failing disposable packages.

The Review-ready check can start child processes while it performs those steps.
They belong to the one check; they are not the shell execution an agent starts
to run it. A subagent is separate again: its work and any shell execution it
starts are not the agent-launched Review-ready invocation or evidence for it.

For one clean commit, start one Review-ready invocation and retain its
execution identity until it returns a terminal completion result. Only that
invocation's terminal exit or timeout outcome is evidence for its selected
path. Do not infer success or failure from partial output, elapsed time,
process absence, or a separate `ps` invocation. If the execution identity or
its result is lost, the evidence is unavailable: report that before any
replacement run, and do not silently rerun the check.

The following are mappings for the current shell interfaces, not a second
definition of the evidence rule:

- **Claude Code:** start the command once with `Bash(run_in_background: true)`
  and consume its automatic completion notification, including the exit status.
  Do not poll with repeated `Bash`, `TaskOutput`, `Monitor`, `ps`, or `sleep`
  calls.
- **Codex:** retain the complete `exec_command` result. When it contains a live
  `session_id`, continue that same terminal session with `write_stdin` until a
  result includes `exit_code`; do not reduce an in-progress result to `.output`
  alone. An outer wrapper preserves the nested result or keeps waiting on the
  same execution instead of completing and discarding its handle.

### Bounded work while waiting or checking

The submitting agent continues a hypothesis/investigation/evidence loop across
both admission waiting and execution of the admitted check. This works with the
selected gate and requires no selective-check implementation.

1. **Account for the submission:** compare every originating requirement with
   the submitted diff. Record omissions and concrete concerns, then prioritize
   hypotheses about changed behavior and directly affected code.
2. **Form a failure hypothesis:** state the contract, the boundary condition or
   error path that might violate it, and the observation that would distinguish
   failure from correct behavior. Derive it from requirements and implementation
   before reading tests to assess coverage.
3. **Investigate:** inspect the relevant implementation, then determine whether
   tests detect the hypothesized failure and whether their assertions establish
   the contract. Use the focused-test recipe or a small reproduction when
   execution would settle the question.
4. **Record and continue:** capture the hypothesis, evidence, coverage gap if
   any, disposition (confirmed, refuted, or pending), and next step. Use the
   evidence to refine the next hypothesis. Once submission concerns are
   accounted for, expand to other package areas while meaningful hypotheses
   remain; route separate confirmed defects through *Separate defects* below.

Use disposable copies of the SHA pinned by the original Review-ready invocation
for investigation and execution. Keep notes, logs, checkpoints, generated
artifacts, and reproductions outside the submitting worktree. Each evidence
record names its SHA and environment; a later-main confirmation is separate
evidence and cannot establish what the submitted commit did. Prepare the PR
explanation and validation record there too, leaving the gate outcome pending.

When meaningful hypotheses are exhausted, checkpoint what was investigated and
what would supply new evidence, then wait on the retained invocation. Finding
no defect is normal; neither activity nor finding counts have quotas. Revisit a
settled hypothesis when new evidence warrants it, rather than repeating a pass.

Reading, searching, small reproductions, and explicitly selected focused tests
are allowed. Full-suite runs, successive files reconstructing a suite, builds,
rendering, and continuously adding agents are outside this workflow. The same
agent owns the loop; it is separate from `mattpocock-skills:code-review`, which
follows a successful Review-ready check. Exploration is not another gate.

Observe the original invocation at natural work boundaries through
*Observing one invocation*. Admission is automatic: continue the bounded work
without an agent-ready handshake or delaying the check. Immediate interruption
of reasoning is not assumed. When a transition is observed:

- **Check completion:** briefly save the current hypothesis and next step;
  stop and collect any additional execution and its owned children, then
  prioritize the actual terminal check outcome. Dispose of the test copy only
  after shutdown. Partial or interrupted execution, or execution that skipped
  a relevant test, is not a pass.
- **Required fix confirmed:** use the retained Review-ready handle to cancel
  only this agent's request under [Cancellation and recovery](../../tools/review-ready-admission.md#cancellation-and-recovery).
  Confirm terminal completion and cleanup, and stop and collect any additional
  owned execution, before editing, committing, and submitting a fresh request.
  A suspicion alone warrants investigation; an unrelated defect follows
  *Separate defects*. Neither cancels this request.
  If the check already completed, preserve its result for that SHA; it does not
  certify the later fix.
- **Pass:** resolve concrete open concerns, then proceed to formal review.
  Unfinished broad exploration or unrelated issue preparation does not delay
  review once concrete submission concerns are resolved. Failure, NOTE, and
  management-error handling remain as defined by this document and shared
  admission.

Reports focus on useful findings and unresolved concerns with concise evidence.

#### Separate defects

For a confirmed defect outside the required submission fix, collect:

- the violated contract and its source;
- a minimal reproduction, expected behavior, observed result, and impact;
- the exact tested SHA, environment, and any relevant-test skips or limitations;
- current-main status, checked at an explicitly recorded SHA using a separate
  disposable copy when reproduction is needed; and
- a duplicate search across open and closed issues and relevant pull requests,
  with the queries and matching links recorded.

Follow [the issue tracker guide](issue-tracker.md) and publish an English GitHub
issue when the defect remains valid on current main and is nonduplicate. Link an
existing report for a duplicate; record a main-fixed defect without opening a
new one. Keep unconfirmed hypotheses, unavailable main/duplicate checks, and
failed publications as pending notes with the evidence and next action outside
the submitting worktree. Keep repairs as separate tasks. If the original check
finishes during this preparation, checkpoint it and prioritize that outcome;
resume publication separately without holding up formal review.

#### Session refresh

Before the next check, every participating existing agent session rereads this
section and [shared admission](../../tools/review-ready-admission.md), then
explicitly acknowledges the common CLI, the continuing same-agent loop, its
execution ownership, and the terminal-completion transitions. Refresh or restart
a session that cannot load the updated guidance. For an invocation already in
flight, refresh at the next natural work boundary while preserving its original
handle and ownership; collect owned work before any restart that would lose
them. New review sessions acknowledge the guidance when they join. Do not
assume an edited instruction file refreshed a running session.

#### Focused-test recipe

Each submitting worktree may have at most one additional focused-test or
reproduction execution at a time, including any later-main confirmation. Keep
testthat parallel execution explicitly disabled. Different worktrees may each
run one concurrently; after an execution and its owned work stop and are
collected, that worktree may start another relevant target independently. There
is no user-wide additional-execution limit, duration cap, backend-thread cap, or
aggregate CPU or memory bound.

Select one relevant existing test file explicitly. Inspect its helpers, setup,
teardown, and any subprocesses before running: one file alone does not establish
that it is lightweight. Account for how to stop and collect its owned work
through the execution interface; select a smaller reproduction if that cannot
be established. Never replace a missing or empty selection with the suite.

Create a disposable copy of the submitted SHA outside the submitting worktree.
Use the SHA pinned by the retained Review-ready invocation, not a later HEAD:

```sh
submitted_sha='<40-character SHA from the Review-ready invocation>'
focus_workspace=$(mktemp -d "${TMPDIR:-/tmp}/marginplyr-focused.XXXXXX")
mkdir "$focus_workspace/source"
git archive --format=tar --output="$focus_workspace/source.tar" "$submitted_sha"
tar -xf "$focus_workspace/source.tar" -C "$focus_workspace/source"
```

Check each preparation command succeeds before continuing. Save the following
as `$focus_workspace/run-focused.R`, outside the submitting worktree:

```r
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !nzchar(args[[2L]])) {
  stop("Supply a disposable source directory and one explicit test filename.")
}
copy <- normalizePath(args[[1L]], mustWork = TRUE)
target <- args[[2L]]
if (!grepl("^test-[A-Za-z0-9_-]+[.][Rr]$", target)) {
  stop("Select one test-<letters/digits/_/->.R filename, not a path or pattern.")
}
test_dir <- file.path(copy, "tests", "testthat")
stem <- sub("[.][Rr]$", "", sub("^test-", "", target))
filter <- paste0("^", stem, "$")
selected <- testthat::find_test_scripts(test_dir, filter = filter,
                                       full.names = FALSE)
if (!identical(selected, target)) {
  stop("Selection must resolve to exactly the requested existing file.")
}
Sys.setenv(TESTTHAT_PARALLEL = "false")
testthat::test_local(copy, filter = filter, reporter = "summary",
                     stop_on_failure = TRUE)
```

Run it with the chosen filename, for example:

```sh
Rscript "$focus_workspace/run-focused.R" "$focus_workspace/source" test-example.R
```

Retain this focused-test execution's handle separately from the original
Review-ready handle. A yielded execution is still active: do not start the
next focused target until it and its owned work have stopped and been
collected. The copy contains generated files and snapshot side effects; the
submitting worktree stays clean for admission. Store logs and checkpoints
outside that worktree too. After shutdown, remove the disposable workspace.
A focused result never replaces the fixed gate or supplies its outcome.

### Recording the outcome

For a spelling finding, correct an actual typo; when the word is intentional,
add it to `inst/WORDLIST` instead.

An ERROR or WARNING fails a package-check attempt. A nonzero or timed-out
check process also fails, even when no condition could be parsed from its
incomplete output. A failed step ends the invocation except for the single
native-crash retry described below. Every NOTE is printed. The shared CRAN
NOTE policy identifies an existing classification; every other NOTE needs a
written explanation before the commit is review-ready. The command remains
non-mutating and removes its disposable checkout, tarball, and check directory
when it finishes. A failed source-tarball check first retains the diagnostics
described below.

Record the request/commit and comparison identities, selected path, capacity
mode, worker policy, completed/omitted stages, promotion history, terminal
outcome, cleanup and NOTE dispositions in the pull request. Exempt success is
not a Full pass. Include the separate stage plan, every reused unit's original
request/SHA, and timestamp reevaluation; distinguish execution from inherited
Full evidence. The common CLI retains each request's record and gate output;
recover one explicit request ID rather than guessing which request to clear.
For Full:

```text
- Review-ready check: <40-character commit SHA>
  - selected path: Full
  - package spelling: passed
  - jarl: passed
  - package-aware lintr: passed
  - source-tarball R CMD check, including snapshots: <errors/warnings/notes and NOTE dispositions>
```

For Documentation, record the same source checks plus mapped generation,
the three registered assertion files with no skips, and source-tarball
`R CMD check --as-cran --no-tests` with ERROR/WARNING/NOTE dispositions; state
`full suite and snapshots omitted`. For Repository, record the common source
checks and mapped verifiers and state `package build/check, full suite and
snapshots omitted`. Never label either as a Full pass.

When the native-crash retry is used, record `passed-after-retry` or `failed`,
both attempt outcomes, the retry eligibility reason, and the retained first
failure's diagnostic directory. A `passed-after-retry` result permits review
under the same NOTE disposition requirements as an ordinary pass.

### Failed source-tarball checks

Every failed source-tarball check, including an exception before `rcmdcheck`
returns, attempts to retain diagnostics and reports the created directory as
`Review-ready failure diagnostics:`. It is under
`file.path(tools::R_user_dir("marginplyr", "cache"), "review-ready-failures")`,
outside the repository and the disposable workspace. Each failed attempt gets
its own directory containing the available check, console, test, and vignette
text logs, the committed SHA, UTC start/failure times, R/OS/Quarto/Deno versions,
and the process exit/timeout or error state. An unavailable exit status or version
is recorded as unavailable. Source trees, tarballs, native crash reports, and
memory images are not included. A first-attempt pass creates no failure bundle;
a retry pass retains the first failure's bundle. Retained bundles are not
pruned automatically; remove them manually after investigation. If retention
is incomplete, the command reports that failure and does not retry.

The bundle includes platform guidance: a candidate `.ips` location and Console
on macOS, Application Error events in the Windows Application log, or
`coredumpctl` metadata on Linux with systemd-coredump. Match the executable and
failure time before associating a native report with the invocation. If the
facility or report is unavailable, record that limitation and use the portable
bundle; do not enable dump collection or change OS settings for this check.

### One retry after a native rendering crash

On macOS, the invocation may retry the source-tarball check once when its
failed vignette-rebuilding output contains the supported Quarto launcher
diagnostic identifying a SIGSEGV of the Deno command it launched. The
diagnostic must belong to that attempt and match its recorded Quarto launcher
and supported Deno invocation form. A generic Quarto error, outer R exit code,
quoted crash text, or native report alone does not establish eligibility.
This identifies a supported failure form, not the SQLite mechanism or a crash
UUID.

Initially, the result must have exit status 1, no timeout, exactly one ERROR,
zero WARNINGs, and no test failure. That ERROR must contain exactly one failed
HTML `.qmd` rebuilding segment with the launcher diagnostic and the measured
Quarto R 1.5.1 error wrapper: the CLI failure is followed by failure to evaluate
the cli expression `QUARTO_DENO`, ending with `object 'QUARTO_DENO' not found`.
Other successfully rebuilt vignette segments are allowed. Unknown wrappers
are refused; extending support requires observed wrapper evidence and
corresponding verifier fixtures.

Complete preservation of the first attempt's available diagnostic text and
metadata is also required. Independent failures, ambiguous evidence, a checker
exception without a result, or incomplete retention end the invocation as
failed. A missing or delayed `.ips` does not prevent retry when the portable
evidence satisfies these conditions; the command does not wait for or collect
a native report.

Both package paths record content hashes of the tarball, Quarto launcher, its
`quarto.js`, and Deno executable, together with the canonical Quarto/Deno executable paths and
versions, before the first attempt. Immediately before retry, require those
identities to remain available and unchanged. An identity change or inability
to establish identity prevents retry. Do not rebuild the tarball, change
caches, or switch installed tools for the retry.

The retry preserves the selected path and exact check arguments on that same
tarball in a fresh check directory: Full includes tests and vignettes;
Documentation retains `--no-tests` and vignette rebuilding. Preserve separate
logs and outcomes for both attempts. The already completed spelling, lint,
build and documentation-assertion stages are not repeated; Full snapshots run
again within the retried check. Documentation continues to omit them.
There is no third attempt; any retry failure, timeout, or exception fails the
invocation.

A successful retry yields the explicit overall result `passed-after-retry`,
while the first attempt remains failed. The final attempt must satisfy the
ordinary ERROR, WARNING, exit, timeout, and NOTE rules above. This recovery
policy applies only to the local macOS gate; it does not extend to other
platforms, CI, or formal release preflight. A passing retry is not evidence
that the crash was prevented or its cause resolved.

## Roles outside the boundary

Focused tests are the tight feedback loop while changing a module. They do not
replace the selected source-tarball check. Full sees the complete suite and
snapshots; Documentation retains its explicit assertions and package boundary.
That check does not replace jarl or lintr. CI owns line-coverage measurement under
[ADR 0030](../adr/0030-require-complete-test-line-coverage.md); its coverage
command rejects source, covr, and Codecov exclusions and reports each uncovered
file and line without rounding its verdict.

The Review-ready check uses the developer's fully provisioned library. The
release matrix remains responsible for depends-only, suite-coverage, library
isolation, optional-backend execution, other R versions, and other operating
systems. The CRAN release playbook owns the stricter exact-candidate preflight,
remote checks, and external-clock evidence.

Unused-Suggests review is not part of the daily Review-ready check. Dependency
metadata still follows `AGENTS.md`'s *Dependency metadata* rules when changed;
the broader dependency-hygiene review belongs to CRAN-release preparation.
