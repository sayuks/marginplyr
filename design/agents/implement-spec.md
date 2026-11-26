# Implementing a spec: `mattpocock-skills:implement-spec`

This file holds the brief each of that skill's implementers is pointed at, and
what the coordinator does around it. A brief written afresh for each run is
where #1799's lanes lost both.

## The implementer brief

Point each implementer at its ticket and at this section.

- **Tests.** Run one test file at a time. The complete suite runs once, in the
  integration branch's `full` check.
- **Before reporting done**, after merging the integration tip, commit and run
  `Rscript tools/review-ready.R --scope source --reason '...'` from the
  worktree root. This is development feedback, not the branch's Review-ready,
  and `source` takes no heavy slot, so lanes run it concurrently. It covers
  what focused tests and a lint of the changed files do not: on #1799 a
  fixture package named by a literal failed the test dependency-declaration
  scan, and a vignette chunk failed package lintr, and both surfaced only
  after integration.
- **Scratch files** — repro scripts, notes, anything written outside the
  worktree — go in the coordinator's notes directory from the skill's step 2,
  or under `mktemp -d`. `$TMPDIR` is shared by concurrent sessions; on #1799 a
  lane's repro script was overwritten there.
- **The report** is the final message: branch, the head SHA the `source`
  result names, that result, and anything the ticket left undecided. A
  child's sandbox may not let it write a report file outside its worktree.

## Around the implementers

The integration branch's Review-ready follows
[*Boundary and scope*](local-checks.md#boundary-and-scope) before step 8 marks
the pull request ready.

Before step 7 hands the review's findings to the fixer, disposition each as
*Answering a review* in `design/architecture.md` requires, and hand on the
dispositioned findings.
