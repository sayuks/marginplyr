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
Supported prose uses the Repository path below; unsupported changes use Full
until a validated route exists.

The **Review-ready check** is the local gate for one clean committed revision.
It selects **Full** or **Repository** from the cumulative final tree difference
against the unique merge-base with local `main`. Full retains the package
boundary; Repository omits package build/check for supported repository prose.
Focused tests and the wider release process have separate roles.

## Boundary

Run the Review-ready check before any of these transitions:

- opening a non-draft pull request;
- marking a draft pull request ready or requesting review; or
- pushing a change to a pull request already ready for review.

The alternative for the last case is to return the pull request to draft,
push the change, and pass the check before making it ready again. A later
commit requires a fresh invocation; previous successful checks never narrow
the comparison base.

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
admission if HEAD or cleanliness changes. One complete check is admitted per
OS user across all clones and worktrees, preserving four internal workers.
When the invocation waits or runs, enter *Bounded work while waiting or
checking* below while retaining the execution identity described under
*Observing one invocation*.

The check invocation takes no path or skip options. Before waiting it pins the
submitted commit, `refs/heads/main` commit, and unique merge-base, using local
main without fetching. A later main update does not replace these identities.
Missing or ambiguous bases, comparison failures, empty differences, unknown
paths, mode/type changes, symlinks, and submodules select Full with a reason.
Deletion and both sides of a rename participate in the NUL-delimited final
tree comparison. Reverted historical edits do not participate.

The supported prose categories and required verifier mapping have one home:
`PROSE`, `REFERENCE_CHECKS`, and `CONTRACTS` in
`tools/review-ready-admission.py`. It supports agent/domain/design Markdown,
Markdown investigation notes, conduct policy, and operational Markdown under
`tools/`. Prose blobs must be UTF-8 text without non-prose control characters;
unknown encodings or binary content select Full. Other formats in those
directories remain Full. Package
documentation, licenses, CI configuration, and checking machinery remain Full.
A Documentation path and CI-specific exemptions are not enabled.

Both paths always execute package spelling, jarl, and package-aware lintr
(the three source stages described below), followed by mapped artifact
verification. Reference/context/invocation verification runs against the
pinned disposable Git checkout; common source checks run against its archive.
Operational docs also run their mapped contracts. The same artifact routes run
on Full, so package checking cannot hide a required verifier failure. Missing
or failing verifiers fail the invocation without changing paths.

Repository stops after those checks. It records completed checks and explicitly
reports package build/check, the full suite, and snapshots as omitted. Its
success is a Repository result, never a Full pass. Full runs, in order:

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

Use the same agent for one ordered pass over the submitted change during both
admission waiting and execution of the admitted check:

1. **Requirements:** account for the originating issue's requirements against
   the diff and identify omissions.
2. **Bug hunt:** inspect changed behavior and directly affected code for
   boundary conditions, error paths, and regressions. Derive failure hypotheses
   from the requirements and implementation before reading tests.
3. **Test inspection:** determine whether tests detect those failures and
   whether their assertions establish the intended behavior. Use the focused
   recipe below or a small reproduction when evidence is needed.
4. **PR preparation:** prepare the explanation and validation record, leaving
   the check outcome pending until its terminal evidence arrives.

Finish each activity after inspecting its relevant scope once and capturing
concrete findings or unresolved hypotheses. Keep hypotheses, evidence, and next
steps outside the submitting worktree. After the list, return to waiting using
the existing interface mapping above; revisit completed work only for new
evidence. Finding no defect is normal. Continuous activity and finding counts
are not goals, and preparation for another issue is outside this pass.

Reading, searching, small reproductions, and explicitly selected focused tests
are allowed. Full-suite runs (including successive individual files covering
the suite), builds, rendering, and continuously running additional agents are
excluded. This pass is separate from `mattpocock-skills:code-review`, which
follows a successful Review-ready check; finishing the pass is not another gate.

Observe the original invocation at natural work boundaries through
*Observing one invocation*. Admission is automatic: continue the bounded work
without an agent-ready handshake or delaying the check. Immediate interruption
of reasoning is not assumed. When a transition is observed:

- **Check completion:** briefly save the current hypothesis and next step;
  stop and collect any focused-test execution and its owned work, then
  prioritize the actual terminal check outcome. Dispose of the test copy only
  after shutdown. Cancelled or partial focused-test output is not a pass.
- **Required fix confirmed:** use the retained Review-ready handle to cancel
  only this agent's request under [Cancellation and recovery](../../tools/review-ready-admission.md#cancellation-and-recovery).
  Confirm terminal completion and cleanup before editing, committing, and
  submitting a fresh request. A suspicion alone warrants investigation, not
  cancellation. If the check already completed, preserve its result for that
  SHA; it does not certify the later fix.
- **Pass:** resolve concrete open concerns, then proceed to formal review.
  Unfinished activities without a concrete concern do not delay it. Failure,
  NOTE, and management-error handling remain as defined by this document and
  shared admission.

Reports focus on useful findings and unresolved concerns with concise evidence.
These instructions apply to future worktrees and sessions loading them; they
add no migration of existing sessions or user-wide instruction changes.

#### Focused-test recipe

Each worktree may have one focused-test invocation at a time, with testthat
parallel execution explicitly disabled. Different worktrees may each run one
concurrently; after its test finishes, a worktree may start another relevant
target independently. There is no fixed per-run time limit, user-wide focused
slot, or cap on backend-internal threads, and no fixed total CPU or memory bound
is implied.

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

Record the exact identity and outcome in the pull request:

```text
- Review-ready check: <40-character commit SHA>
  - package spelling: passed
  - jarl: passed
  - package-aware lintr: passed
  - source-tarball R CMD check, including snapshots: <errors/warnings/notes and NOTE dispositions>
```

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

Full records content hashes of the tarball, Quarto launcher, its `quarto.js`, and
Deno executable, together with the canonical Quarto/Deno executable paths and
versions, before the first attempt. Immediately before retry, require those
identities to remain available and unchanged. An identity change or inability
to establish identity prevents retry. Do not rebuild the tarball, change
caches, or switch installed tools for the retry.

The retry runs the complete `R CMD check --as-cran`, including tests and
vignettes, on that same tarball in a fresh check directory. Preserve separate
logs and outcomes for both attempts. The already completed spelling, lint, and
build stages are not repeated; snapshots run again within the retried check.
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
replace the source-tarball check, which sees the full suite, snapshots, package
metadata, and installed-package boundaries. That check does not replace jarl
or lintr. CI owns line-coverage measurement under
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
