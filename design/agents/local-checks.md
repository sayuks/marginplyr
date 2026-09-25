# Local review-ready checks

This is the operational authority for the local boundary before a marginplyr
commit is published for review. It defines which commits need the boundary,
what the fixed check proves, and when its evidence expires. Pull-request CI
remains the cross-platform and release-matrix authority.

## Language

A **Package-affecting change** can change the source package, an input that
generates it, installed behavior, or the result of checking it. This includes
package code, tests, examples, vignettes, package data, installed files,
dependency metadata, generated documentation, generation sources, and build
exclusions. A tests-only change is Package-affecting. When the classification
is uncertain, use this one.

A **Repository-only change** can be shown to affect none of those things.
Design and investigation notes and agent instructions that drive no generation
are ordinary examples. Exclusion from the package is not sufficient: CI,
release, or local-check tooling is Package-affecting when it can change a
package check result. Repository-only does not mean unchecked: run the verifier
or generation command owned by the changed artifact. It means the package
boundary below is not additionally required.

The **Review-ready check** is the fixed local gate for one Package-affecting
commit. It is not a name for focused tests or for the wider release process.

## Boundary

Run the Review-ready check before any of these transitions:

- opening a non-draft pull request;
- marking a draft pull request ready or requesting review; or
- pushing a Package-affecting change to a pull request already ready for
  review.

The alternative for the last case is to return the pull request to draft,
push the change, and pass the check before making it ready again. A later
Package-affecting change invalidates the earlier result. A Repository-only
change does not.

The checked state is a clean committed `HEAD`. The command archives that exact
commit into a disposable directory, so ignored local files cannot enter the
evidence. Run it from the repository root:

```sh
Rscript tools/review-ready-check.R
```

The command takes no options. It runs, in order:

1. a test-source dependency scan that refuses every package candidate absent
   from `DESCRIPTION`;
2. package spelling, reporting unknown words and their locations only when it
   finds them;
3. `jarl check .`;
4. package-aware lintr after `pkgload::load_all()`;
5. the full testthat suite, including local snapshot expectations;
6. strict test line coverage through `tools/coverage-check.R`, requiring every
   measured R source line and every optional Suggest used by the suite; and
7. a source-tarball `R CMD check --as-cran` against disposable empty local
   CRAN and Bioconductor indexes, with remote incoming checks and
   external-clock verification disabled. The local future-file-timestamp
   comparison remains enabled.

### Observing one invocation

The Review-ready check can start child processes while it performs those steps.
They belong to the one check; they are not the shell execution an agent starts
to run it. A subagent is separate again: its work and any shell execution it
starts are not the agent-launched Review-ready invocation or evidence for it.

For one clean commit, start one Review-ready invocation and retain its execution
identity until it returns a terminal completion result. Only that invocation's
terminal exit or timeout outcome is evidence. Do not infer success or failure
from partial output, elapsed time, process absence, or a separate `ps`
invocation. If the execution identity or its result is lost, the evidence is
unavailable: report that before any replacement run, and do not silently rerun
the check.

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

For a spelling finding, correct an actual typo; when the word is intentional,
add it to `inst/WORDLIST` instead.

It stops on the first failed step. An ERROR or WARNING fails the package check.
A nonzero or timed-out check process also fails, even when no condition could
be parsed from its incomplete output. Every NOTE is printed. The shared CRAN
NOTE policy identifies an existing classification; every other NOTE needs a
written explanation before the commit is review-ready. The command remains
non-mutating and removes its disposable checkout, tarball, and check directory
when it finishes.

Record the exact identity and outcome in the pull request:

```text
- Review-ready check: <40-character commit SHA>
  - package spelling: passed
  - jarl: passed
  - package-aware lintr: passed
  - full testthat suite: passed
  - strict test line coverage: <covered/measured lines, covr version>
  - source-tarball R CMD check: <errors/warnings/notes and NOTE dispositions>
```

## Roles outside the boundary

Focused tests are the tight feedback loop while changing a module. They do not
replace the full suite at the boundary. The full suite does not replace the
source-tarball check: CRAN semantics skip snapshot expectations, while the
tarball check alone sees package metadata and installed-package boundaries.
Neither test run replaces jarl or lintr. The line-coverage policy is recorded in
[ADR 0030](../adr/0030-require-complete-test-line-coverage.md); the coverage
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
