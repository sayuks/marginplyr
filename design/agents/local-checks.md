# Local Review-ready checks

This is the operational authority before publishing a marginplyr commit for
review. The [runner guide](../../tools/review-ready-runner.md) owns setup,
artifacts and cancellation; the [specification](../specs/review-ready-repository-runner.md)
and [ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md) own the
accepted design. Pull-request CI remains the cross-platform and release-matrix
authority.

## Rollout state

The target entry is the repository-owned R command below. Until the maintainer
completes the drained cutover and updates the user-wide instructions, the
installed `/Users/sayuks/.local/bin/marginplyr-review-ready` remains the required
operational entry. Follow that active instruction for production checks; the
presence of the new files alone does not establish cutover. The
[retirement guide](../../tools/review-ready-admission.md) records this one-time
transition. Old worktrees and their existing sessions are outside migration
scope.

## Boundary and scope

Run one invocation against a clean committed `HEAD` before opening a non-draft
pull request, marking a draft ready or requesting review, or pushing to a
pull request already ready for review. Alternatively return the pull request
to draft before pushing, and check before making it ready again.

Inspect the submitted diff and its effects, choose a scope, and supply a
concrete reason. Run from the worktree root after cutover:

```sh
Rscript tools/review-ready.R --scope source --reason 'Explain why package build/check can be omitted.'
Rscript tools/review-ready.R --scope full --reason 'Explain the package-check obligation.'
```

`source` runs the mandatory five checks and omits heavy verification. `full`
runs those same checks, then waits for the shared heavy slot and automatically
builds and checks the source tarball. The AI makes this judgment; the runner
does not infer scope from paths or reuse a previous result.

Package-affecting changes require `full`: changes to package code, tests,
examples, vignettes, data, installed files, dependencies, generated
package documentation, generation sources, build exclusions, or tooling that
can change a package-check result. A tests-only change is package-affecting.
Light prose, documentation or research may use `source` when its effects
justify omitting package build/check. A filename, directory, extension or
exclusion from the source package alone is insufficient. Use `full` when the
effects are uncertain. Change-specific checks still establish the changed
tooling's contract; a package-check pass alone cannot do that.

Both forms pin the submitted SHA at invocation and use its disposable committed
source for every check. Waiting never switches to a later HEAD. A later fix
must be committed and receive a fresh invocation; a result identifies only its
submitted revision. Every invocation executes these five checks, in order:

1. a mechanical test-source dependency-declaration scan against `DESCRIPTION`;
2. the registered optional test Suggests presence and version check;
3. package spelling;
4. `jarl check .`;
5. package-aware lintr after `pkgload::load_all()`.

A failed baseline ends that invocation before admission waiting or package
build/check. Correct and commit the fix explicitly before invoking again.
`source` ends after this baseline and reports its SHA, omission and supplied
reason. It is a five-check-only result, never a full package-check pass.

After the baseline succeeds, `full` holds one stable OS advisory lock shared
by this user's marginplyr clones and worktrees on the Mac. Under that slot it
builds a tarball and runs `R CMD check --as-cran`, including examples, vignette
rebuilding, the complete test suite and snapshots once. It sets
`TESTTHAT_PARALLEL=true`, `TESTTHAT_CPUS=4`, `NOT_CRAN=true` and required-Suggests
enforcement without coverage measurement. The existing local/offline check
environment uses empty local CRAN/Bioconductor indexes, disables remote incoming
and external-clock checks, and retains local future-file-timestamp checking.
`DESCRIPTION`'s testthat start-first order prioritizes measured slow files;
new files still run. There is no admission acknowledgment or AI stop/resume
coordination.

The shared slot covers this heavy check, not every R process. Additional
focused executions follow the recipe below. Routine Review-ready does not run
infrastructure regression suites or regenerate the real README/site. Retained
runner and progress suites run in CI on every PR; change-specific development
verification remains appropriate when those tools change.

### Observing one invocation

During the source-tarball check, the monitor forwards testthat's native
`Starting N test processes.` message from its check transcript. This is the
count testthat selected, including its limit by the number of test files,
rather than a repeated configuration value. It also forwards native worker
startup, unexpected-exit, and teardown diagnostics with their cause and
backtrace from that transcript. Reading continues after the reporter finishes,
and does not wait for reporter events to begin, so worker lifecycle failures
remain visible in either phase. These observations do not replace the terminal
check result. English progress lines report elapsed test time, completed files out of the
discovered total, their percentage, and the last completed file. The percentage counts files, not runtime or passing tests;
it does not estimate remaining time. A five-second heartbeat continues while
a long file runs. The ordinary testthat check summary and the check process's
terminal result remain the verdict.

The local check supplies an archived reporter through
`MARGINPLYR_REVIEW_PROGRESS_REPORTER` and a run-local event path through
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
invocation's terminal exit or timeout outcome is evidence for its chosen
scope. Do not infer success or failure from partial output, elapsed time,
process absence, or a separate `ps` invocation. If the execution identity or
its result is lost, the evidence is unavailable: report that before any
replacement run, and do not silently rerun the check.

The following are mappings for the current shell interfaces, not a second
definition of the evidence rule:

- **Claude Code:** start the command once with `Bash(run_in_background: true)`
  and retain that execution until its terminal result, including the exit
  status. Observe that same execution through the interface available to the
  session; a queued AI-resumption message is not part of this workflow.
- **Codex:** retain the complete `exec_command` result. When it contains a live
  `session_id`, continue that same terminal session with `write_stdin` until a
  result includes `exit_code`; do not reduce an in-progress result to `.output`
  alone. An outer wrapper preserves the nested result or keeps waiting on the
  same execution instead of completing and discarding its handle.

### Bounded work while waiting or checking

The submitting agent continues a hypothesis/investigation/evidence loop across
both admission waiting and execution of the admitted check. Keep the original
execution handle across both phases.

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
  only this invocation under [Cancellation](../../tools/review-ready-runner.md#cancellation).
  Confirm terminal completion and cleanup, and stop and collect any additional
  owned execution, before editing, committing, and submitting a fresh request.
  A suspicion alone warrants investigation; an unrelated defect follows
  *Separate defects*. Neither cancels this request.
  If the check already completed, preserve its result for that SHA; it does not
  certify the later fix.
- **Pass:** resolve concrete open concerns, then proceed to formal review.
  Unfinished broad exploration or unrelated issue preparation does not delay
  review once concrete submission concerns are resolved. Failure and NOTE
  handling remain as defined below.

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

Correct spelling errors; add intentional words to `inst/WORDLIST`.
ERROR, WARNING, nonzero process exit and timeout fail the invocation. Every
NOTE is printed; a NOTE outside the shared CRAN policy needs a written
disposition before review readiness. A failed check ends once. Make and commit
any fix before starting another invocation.

Record the submitted SHA, chosen scope and reason, all five baseline outcomes,
heavy-check execution or omission, terminal outcome, artifact directory and
NOTE dispositions in the pull request. A full result includes the source-tarball
check's errors, warnings and notes. A source result explicitly says
`package build/check, complete suite and snapshots omitted`.

All runner-owned logs, results, source copies, archives, tarballs, check/install
work and supported child temporary/cache files stay together under
`.review-ready/runs/<run-id>/` in the submitting worktree. The runner guide's
[single seven-day policy](../../tools/review-ready-runner.md#artifacts-and-retention)
applies to success, failure and cancellation alike. Native OS crash reports
remain OS-owned and are outside runner cleanup.

## Roles outside the boundary

Focused tests are development feedback and never supply the Review-ready
outcome. CI owns coverage under [ADR 0030](../adr/0030-require-complete-test-line-coverage.md),
with no exclusions or rounded pass threshold. The release matrix owns
minimal-dependency, suite-coverage, library-isolation, optional-backend,
other-R-version and other-OS verification. The CRAN release playbook owns
exact-candidate preflight, remote checks and external-clock evidence.

The local command uses the developer's provisioned library. Dependency changes
still follow *Dependency metadata* in `AGENTS.md`; unused-Suggests review is
part of release preparation rather than every routine invocation.
