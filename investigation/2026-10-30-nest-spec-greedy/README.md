# Nesting equality and Grouping specification lifecycle bug hunt

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

## Scope and outcome

This investigation started from fetched main in a new worktree on
`investigation/nest-spec-greedy-2026-10-30`. It investigated
`nest_with_margins()`, `nest_by_with_margins()`, the five Grouping
specification constructors, and `inspect_grouping()`. The October 29
[shared-cache hunt](../2026-10-29-nest-spec-exhaustive/README.md),
[selector-dispatch follow-up](../2026-10-29-nest-spec-dispatch-followup/README.md),
and [deep hunt](../nesting-grouping-deep-hunt-2026-10-29.md) guided the new
boundary choices. Their earlier measurements were not counted as this run.

One previously unticketed target defect was reproduced: display conversion split a single source
equality group into multiple nested cells. A second, incidental finding arose
while validating repository documentation: the unchanged main failed its
reference verifier on an external temporary script in the focused-test recipe.
The target defect became [#1073](https://github.com/sayuks/marginplyr/issues/1073);
the validation defect became [#1074](https://github.com/sayuks/marginplyr/issues/1074).
Both were labelled `bug` and `ready-for-agent`, with no blocking edges. The user delegated
Issue granularity and publication, so the `to-tickets` breakdown interview was
omitted. No implementation was attempted and no parent Issue was modified.

## F1: equal source keys split when their display text differed

The fixture required no extension package or caller-defined class:

```r
input <- tibble::tibble(
  g = package_version(c("1.0", "1.0.0")),
  row_id = 1:2
)
nest_with_margins(input, .grouping = grouping_set(g))
```

The two package versions represented one equality group. Ordinary
`dplyr::nest_by()` and `summarize_with_margins()` each kept both rows together.
Both nesting Margin verbs with the default non-missing Margin label instead
returned two one-row cells, displayed under `1.0` and `1.0.0`. A single
`grouping_set()` sufficed: no omitted dimension or Grand total was necessary.
With a typed-missing label or a fixed `.by` key, the source group stayed intact.

The [local track](local/README.md) independently reproduced the same defect
with a character-backed vctrs vector whose equality proxy folded case while
its display retained case. The coordinating track repeated it with a numeric
vector whose equality proxy bucketed values while its display retained their
numeric values. These were different equality relations reaching the same
public-seam failure, not three independent defects.

`execute_margin_nest()` copied typed source identities before display
conversion. `nest_expanded_margins()` then grouped by both those identity
columns and the displayed `group_cols`. Display values therefore refined the
source equality partition even when the identity values were equal. The
fixed-key, typed-missing-label, upstream dplyr, and Margin-summary controls
separated this from a failure to group the original type or a request to
restore arbitrary attributes.

The public [nesting reference](../../R/nest_with_margins.R) stated that each
group in each grouping set received one nested data frame. Closed
[#620](https://github.com/sayuks/marginplyr/issues/620), whose full body and
empty comment list were read, also required display choices to preserve group
membership. Its distinct-double and DST examples concerned different groups
merging under equal display text. This finding established the inverse
remaining boundary: one source group split under different display text.
The existing typed-identity copy prevented merging but did not prevent this
split. The proposed repair required no new public option or equality rule.

## Existing structured-key Issue confirmed

During this investigation another audit published
[#1071](https://github.com/sayuks/marginplyr/issues/1071) for the one-dimensional
table prototype. Its full body and empty comment list were read before an
independent [nesting replay](coordinator/existing-table-boundary.R). A local
frequency-table key grouped and nested correctly in an included-only set,
but both nesting verbs with a typed-missing rollup raised the same
`vctrs_error_ptype2` already described by that Issue. The existing report
therefore also reached the nesting path. No duplicate Issue was created and
no existing Issue was modified. The adjacent log retained both consumers.

## B1: the baseline document-reference check rejected an external script

The unmodified main and this worktree both returned terminal exit 1 from:

```sh
Rscript .github/scripts/verify-doc-references.R
```

The diagnostic named `focus_workspace/run-focused.R` in
[the local-check instructions](../../design/agents/local-checks.md). That
recipe explicitly placed the script in a disposable directory outside the
submitting worktree. The verifier treated the shell-variable suffix as a
repository citation and could find no tracked file answering it. Its
[exemption policy](../../.github/scripts/verify-doc-references.R) already
admitted deliberately external generated paths with an expression-specific
reason, including the release audit's temporary records.

The failure preceded all changes in this investigation.
[The captured output](coordinator/doc-references-baseline.log) preserved the
failure. An all-state Issue search for the temporary variable's spelling
returned no duplicate before publication. The separate repair needed to make
the documented external recipe and the reference gate agree while retaining
the gate's rejection of genuinely missing repository citations. No temporary
script was added to the repository and neither authority was changed here.

## Completed search coverage

The counts below belong to different oracles and were not combined into one
passing-assertion total.

| Track | Completed observations |
| --- | --- |
| [Base-version equality validation](spec/independent-equality-validation.md) | 96/96 non-missing-label nesting configurations split the detail group; plain frames/tibbles, both verbs, keep modes, sort choices, single-set/rollup, and payload/no-payload were crossed. Typed-missing-label, fixed-key, summary, and upstream nesting controls remained correct. |
| [Custom equality and recursive factors](local/README.md) | 540 character-equality calls: 72 wrong memberships and 468 faithful controls; 180 Grand total memberships matched. Recursive-factor probes accepted 120/144 calls and retained 378 exact projections; 24 factor-array grouping failures also occurred in ordinary dplyr/vctrs. |
| [Specification lifecycle and capture](spec/README.md) | 3,360 reused/serialized-spec comparisons: 3,128 exact plans and 232 expected empty-unit refusals; 600 rebinding/injection comparisons and 15 helper-head comparisons matched their independent controls. |
| [dtplyr row-selection graphs](backend/README.md) | 819 constructed graphs: 450 matching typed plans and 369 safety refusals; 3,276 nesting comparisons comprised 1,800 matches and 1,476 refusals, with no admitted mismatch or additional warning. |
| Mutable and repeated-mutation graphs | 96 mutable graphs yielded 180 matching inspections, 12 metadata refusals and 192 nesting refusals, without caller callbacks or writes. Of 290 ordinarily collectable generated mutation sequences, 176 typed selections matched and 114 were refused; source-equivalent warnings were distinguished from artificial warnings. |
| Remote boundaries | 24 Arrow calls preserved four readers and 12 source rows. SQLite and DuckDB each supplied 60 calls; their statements numbered zero and 24 respectively, with every DuckDB statement containing `LIMIT 0`. Unsupported nesting and locally decidable invalid calls sent none. |

## Replay and evidence

Each retained R entry point loaded the audited checkout with
`pkgload::load_all()`. Run commands from the repository root. The local minimal
reproducer's observation mode asserted the dated wrong membership and exited
zero; its `--assert` mode required the correct membership and exited one.
The coordinating numeric reproduction also exited one under `--assert`.
Those red results were intended evidence of the unfixed defect, not passing
regression tests. Adjacent logs retained the terminal diagnostics.

The standard-library fixture was retained in
[base-version-equality.R](spec/base-version-equality.R):

```sh
Rscript investigation/2026-10-30-nest-spec-greedy/spec/base-version-equality.R
Rscript investigation/2026-10-30-nest-spec-greedy/spec/base-version-equality.R --assert
```

The track READMEs recorded the completed matrices, exclusions, and entry
points. [The environment record](coordinator/environment.log) captured R,
platform, and loaded dependency versions. The final `MANIFEST.sha256`
identified the retained evidence files.

Only dated investigation evidence was added. Package code, permanent tests,
generated documentation, build exclusions, and public contracts were not
changed. The existing `.Rbuildignore` excluded `investigation/` from the
source package. This was Repository-only under
[the local-check classification](../../design/agents/local-checks.md).
The requested common `marginplyr-review-ready` invocation and publication
checks were nevertheless run, with their exact SHA and terminal outcomes
recorded on the pull request. The baseline document-reference failure above
was kept separate from the passing checks. CI monitoring was outside the
request.

These finite probes did not establish freedom from all bugs, support for
remote nesting, behavior of arbitrary S3/S4 methods, every backend operation,
unbounded plans, performance under resource exhaustion, another R or package
version, or another operating system. Conservative metadata refusals were
reported separately from admitted results; an accepted refusal was not counted
as successful nesting.
