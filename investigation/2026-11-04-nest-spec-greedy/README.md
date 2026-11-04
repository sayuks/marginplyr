# Nesting and Grouping specification greedy bug hunt

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

## Scope and outcome

A new worktree was created from main at
`/private/tmp/marginplyr-nest-spec-20261104`, on
`investigation/nest-spec-greedy-2026-11-04`. Fetched origin main matched the
baseline at creation and immediately before Issue publication. Four
investigators examined both nesting Margin verbs, all five Grouping
specification constructors, `inspect_grouping()`, factor reconstruction and
backend admission. Executions used separate fixed-SHA archives, with one
additional reproduction execution per archive and testthat parallelism disabled.

The November 3 [nesting/specification investigation](../2026-11-03-nest-spec-greedy/README.md),
November 2 [boundary investigation](../2026-11-02-nest-grouping-hunt/README.md),
and October 30/31 [equality](../2026-10-30-nest-spec-greedy/README.md) and
[identity](../2026-10-31-nest-spec-greedy/README.md) records supplied prior
findings and remaining boundaries. Their measurements were not counted as
new executions. Repaired behavior was checked against this baseline rather
than inferred from those dated reports.

Two independently confirmed defects were published through `to-tickets`:

| Finding | User-visible failure | Repair Issue | Blockers |
| --- | --- | --- | --- |
| F1: parenthesized numbered dots lost their specification reading | Valid wrappers failed, selected a different input column, or accepted forbidden nesting | [#1214](https://github.com/sayuks/marginplyr/issues/1214) | None |
| F2: factor display methods disagreed with factor decoding | Source values became missing, another level, or the Margin label | [#1215](https://github.com/sayuks/marginplyr/issues/1215) | None |

Each Issue received `bug` and `ready-for-agent`. Each repair was independently
verifiable; neither required a prefactoring ticket or blocked the other. The
user delegated ticket granularity and publication, so the skill's breakdown
interview was omitted. No parent Issue was modified. No package implementation,
permanent regression test or dependency was changed.

## F1: redundant parentheses changed numbered-dot lookup

The [specification record](spec/README.md) retained constructor/consumer
matrices, boundary controls and duplicate searches. A wrapper receiving
`grouping_set(a)` through dots could inspect `grouping_sets(..1)` successfully.
Changing only that nested argument to `(..1)` instead raised a missing-column
condition. Two pairs of parentheses failed in the same way. Ordinary named
formal arguments worked with and without parentheses.

A second witness established silent result corruption. R accepted `..01` as
its first numbered dot, and the input could also carry that column name.
With `a = x/y` and `..01 = p/p`, bare forwarding selected `a` and produced
two one-row cells. Wrapped forwarding selected `..01` and produced one
two-row cell. The coordinating [false-refusal reproduction](coordinator/numbered-dots.R)
and [silent-membership reproduction](coordinator/numbered-dots-collision.R)
confirmed both boundaries on an independent baseline archive. Their final
desired-behavior commands returned terminal exit 1.

The 450-case matrix contained 150 matching bare controls. Of the 300 wrapped
mismatches, 160 refused valid calls, 40 replaced a required nested-specification
Package condition with a missing-column diagnostic, 80 returned the wrong
valid plan/result, and 20 accepted a nested specification the parent should
have refused. These were variants of one failed lookup, not four repair Issues.

The gate read the unwrapped name through ordinary environment-binding
existence, which did not represent R's numbered-dot lookup. Bare capture
forwarded the dot before that gate; wrapped capture retained it. The reference's
*How an argument is read* section and `CONTEXT.md`'s *Nested specification
position* required transparent parentheses. ADR 0026 supplied the
no-silent-column-precedence boundary. Closed #259's ordinary-binding repair,
the injected/missing-embrace reports #897/#925, and summary-selector #1182
did not cover this lookup form; the specification record retained full-body
and comment comparisons.

## F2: factor display conversion corrupted source values

The [local record](local/README.md) retained a dependency-free factor subclass
whose character representation decorated its level names. Its character
method preserved empty length and missingness, and compatible same-class
vctrs methods made native grouping, nesting and union valid controls.

One source row with factor level `a` and code `1L` sufficed. Both nesting
verbs with one included-only grouping set and a non-missing label returned
an outer factor code `NA`, while a local retained original-key copy still
held code `1L`. No omitted grouping set or branch union was needed.
The coordinating [minimal reproduction](coordinator/factor-display.R)
confirmed both verbs after native and typed-missing-label controls passed;
its desired-behavior assertion returned terminal exit 1.

The shared encoder dispatched the subclass's `as.character()` method but
reconstruction matched those strings against the original declared levels.
Decorated strings matched no level. Other valid methods mapped source values
to an existing different level or to the chosen Margin label instead, so the
failure was not confined to missing values. All 24 additional remapping calls
changed outer values, including checked calls returning `Total` in detail rows.

The local 288-call matrix recorded 44 changed-value cases and 244 matches.
Its included-only companion panel reached all four Margin verbs on local and
immutable dtplyr sources; all eight calls corrupted the factor value. The
[independent backend validation](backend/README.md#independent-factor-subclass-validation)
confirmed that panel with another faithful character method and native
controls. Its 340-call matrix checked outer and inner values separately;
their mismatch counts were not added together. Every caller-table snapshot
stayed unchanged.

On dtplyr, declaring an NA level also activated the encode/rebuild route for
carried fixed keys, nested payloads and original-key copies, including
typed-missing-label requests. Those were the same representation mismatch
at other roles. ADRs 0012 and 0016 placed factor reconstruction on the
package-owned side. The report requested source-value fidelity, without
promising arbitrary subclass restoration. Earlier #1088 concerned private
identity equality; #408/#415/#421 concerned declared NA levels; #491 concerned
deferred helper lookup. The local record retained the all-state searches and
full bodies/comments distinguishing those reports.

## Completed coverage and dispositions

These finite oracles had different purposes and were not combined into one
passing-assertion count. Observation-mode success recorded dated defects;
it was not a post-fix pass.

| Evidence | Completed observations |
| --- | --- |
| [Local temporal/distribution membership](local/README.md#additional-finite-oracles) | 4,608 calls across twelve key families, source representations, fixed/dimension roles, plans, labels and keep choices. Cell memberships and 60,480 value projections matched. |
| Local empty converters | Nine converter families through both nesting verbs returned zero-row character displays, including the repaired empty-hms boundary. |
| Local factor display | 288 calls: 44 corrupted values and 244 matches; eight included-only companion failures and 24 remapping failures were recorded separately. |
| [Specification frontier](spec/README.md) | 450 numbered-dot comparisons, 14 missing/default/order boundary controls, 36 alias controls, and 1,080 independent depth-three family-algebra observations. |
| [Coordinating output-name oracle](coordinator/output-names.R) | 776 comparisons across local/dtplyr, both nesting verbs, twenty unusual list-column names, five identifier names and keep choices: 700 exact matches and 76 documented rowwise `.rows` refusals. |
| [Coordinating algebra oracle](coordinator/plan-algebra.R) | 500 deterministic depth-two trees across all five constructors and three duplicate policies: 1,329 exact observations and 171 expected duplicate refusals. No package compiler/expansion helper supplied the bitset oracle. |
| [Backend special-name graphs](backend/README.md#completed-measurements) | 792 comparisons: 702 exact matches, 78 local refusals and 12 default-collection name-repair constraints. Source snapshots stayed unchanged. |
| Backend cached fan-in | 468 public observations after twelve native graph failures were excluded: 416 exact matches, 36 conservative metadata refusals, twelve fixed-key overlap refusals and four cell-order-only differences. Independent controls matched every cell multiset. |
| Backend Arrow-to-DuckDB bridges | 24 boundary calls across six source graphs. Twelve nesting refusals sent no statements; twelve inspections sent only twelve `LIMIT 0` statements. All six readers retained all 24 original rows. |

Failed native controls, documented refusals, conservative typed-metadata
admission, ordinary strict tibble name repair, and unspecified cell-order
differences were classified separately from package defects. Harness corrections
and interrupted/incomplete exploratory runs were recorded in their track notes;
none supplied a completed count or a passing package result.

The coordinating algebra harness initially handed a data-frame row to
`which()` and stopped before completing comparisons. It was corrected to
pass a logical vector. The first coordinating factor fixture omitted compatible
subclass type/cast methods and failed its typed-missing native-composition
control; the final fixture supplied those methods and passed that control.
Neither harness boundary was published as a package defect.

## Replay, provenance and publication verification

Run retained entry points from a checkout of the investigated source; scripts
accept `MARGINPLYR_HUNT_SOURCE` where an external archive should be loaded.
Track notes describe their output controls. The three coordinating desired
reproductions intentionally returned exit 1 on this unrepaired revision:

```sh
Rscript investigation/2026-11-04-nest-spec-greedy/coordinator/numbered-dots.R
Rscript investigation/2026-11-04-nest-spec-greedy/coordinator/numbered-dots-collision.R
Rscript investigation/2026-11-04-nest-spec-greedy/coordinator/factor-display.R
```

The final coordinating replay, execution session `81951`, collected all five
child commands in order and returned terminal exit 0 only after their actual
exit codes matched `0, 0, 1, 1, 1`. [terminal-results.csv](coordinator/terminal-results.csv)
recorded the individual results. Track notes retained their own original
execution handles and terminal outcomes. No owned reproduction was left active.
`MANIFEST.sha256` identified retained evidence. Logs recorded R 4.6.1 on
macOS Tahoe 26.6.2 arm64 and relevant dependency versions; no dependency was
installed or changed for this investigation.

The submitted diff retained dated research evidence and linked separate repair
tickets. It changed no package behavior, test, example, vignette, generated
documentation, installed file, dependency, build exclusion or checking tool.
The existing `investigation/` source-package exclusion was unchanged. These
effects justified `source` Review-ready scope under
[Local checks](../../design/agents/local-checks.md): package build/check,
complete suite and snapshots omitted. The checked SHA, all five baseline
outcomes, terminal result, main compatibility assessment and review status
belonged on the pull request. Earlier notes were not amended. CI monitoring
was outside the request.

These probes did not establish freedom from all bugs, arbitrary S3/S4 methods,
all evaluation boundaries or backend graphs, hosted databases, another
dependency/R/OS version, unbounded Grouping plans or resource exhaustion.
Remote measurements established the named inspection/refusal boundaries,
not support for SQL or Arrow nesting. No repair or post-fix result was claimed.
