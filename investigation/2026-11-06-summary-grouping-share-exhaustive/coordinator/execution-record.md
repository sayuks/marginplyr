# Coordinator replay record

Investigated: 2026-11-06
Tested package SHA: `9ffb24614a71512170298a3827bc11c80354e742`

The coordinator created `/private/tmp/marginplyr-exhaustive-replay/source`
with `git archive` of the tested SHA and `utils::untar()`. The replay driver
received that source root, the retained artifact directory and a separate
output directory. It ran copied harnesses sequentially through processx,
with owned-child cleanup, preserving stdout/stderr and terminal statuses.
The submitting worktree supplied scripts rather than the package under test.

The first retained execution was `65550`, terminal exit one. Fourteen of its
15 individual statuses matched expectations. Its frame-cast matrix used a
working-directory-relative source path for its sibling witness, which was
absent from the package archive. It failed before obtaining a matrix verdict.
[The initial table](replay-initial-results.csv) and [failed log](replay-04.log)
retain that setup failure.

The matrix was changed to locate its sibling script and CSV output relative
to its own script path. The updated script was copied to the replay workspace
and run alone against the same package archive, through retained execution
`78549`, terminal exit zero. [The corrected log](replay-04-corrected.log)
contains all 18 wrong-value cases and passing named controls. Successful
unrelated modes were not rerun. The final table replaces only that row's
observed status and script digest; the original table remains separate.

The [final table](replay-results.csv) contains 15 matching expected/observed
statuses: six intentionally red desired-contract modes and nine successful
observation/control modes. The summary green control uses public named packing
and expansion. Share controls-only modes retain existing refusals and legal
independent reads. Grouping separating controls preserve the runtime selection
while changing only the static proof boundary. These establish harness
discrimination, not a repaired package.

The R environment was 4.6.1 on aarch64 macOS. Lane environment records identify
dependency versions. Each subprocess was collected before the next, and the
driver and corrected matrix both reached terminal completion. No package
suite, build, snapshot run, rendering or implementation repair supplied this
replay evidence. The later Review-ready invocation was separate and its result
was recorded on the PR.

## Auxiliary archive replay

The two auxiliary summary controls also wrote CSVs to working-directory-relative
research paths. Their archive execution failed at output, as observed through
reviewer terminal handles `60659` and `87938`, both exit one. Their output
paths were changed to their own script directory without changing fixture or
comparison expressions. The coordinator then copied both scripts to
`/private/tmp/marginplyr-exhaustive-auxiliary-replay` and invoked them from the
submitted `12f3bc3f89fc62283e34206862fae998b8e1b8f1` source archive, whose
package code matched the investigated main.

Retained execution `29688` reached terminal exit zero. Both individual statuses
were zero, and the resulting 180/120-row comparison CSVs were byte-identical
to the retained originals. The [result table](auxiliary-replay-results.csv)
records that package SHA, the two script digests and statuses; adjacent
`control-flow-controls.R.log` and `dtplyr-special-controls.R.log` retain the
outputs. The earlier 15-mode replay and its evidence were left unchanged.
