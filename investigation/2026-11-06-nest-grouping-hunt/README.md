# Nesting and Grouping specification dispatch-boundary bug hunt

Investigated: 2026-11-06
Tested package SHA: `9ffb24614a71512170298a3827bc11c80354e742`
Environment: R 4.6.1, aarch64-apple-darwin23, maintainer Mac

Four investigators examined `nest_with_margins()`, `nest_by_with_margins()`,
all five Grouping specification constructors and `inspect_grouping()`.
The worktree was created from fetched `origin/main` at
`/Users/sayuks/Documents/marginplyr/.worktrees/nest-grouping-hunt-2026-11-06`,
on branch `investigation/nest-grouping-hunt-2026-11-06`. Every experimental
source was a disposable archive of the tested SHA. The November 6
[exhaustive investigation] and [greedy investigation], the November 5
[boundary investigation] and earlier track-specific notes supplied prior
findings, repairs and finite coverage; their historical results were not
counted as fresh executions.

## Findings and ticket boundaries

The user delegated ticket granularity and publication without another approval
round. `to-tickets` produced two English issues, each with `bug` and
`ready-for-agent`, independently verifiable acceptance criteria and no blockers.
Each issue covered one complete public-behavior repair. No implementation,
permanent regression test or parent issue was changed.

| Issue | Observed violation | Evidence |
| --- | --- | --- |
| [#1396] | An authored public namespace call around a nested specification was discarded. Its returned selection or specification was replaced with the wrapped specification, producing the wrong Grouping dimension and Nest cells. | [Constructor lane](constructors/README.md) |
| [#1397] | A replaced registered data.table head method received populated roots during typed metadata preparation. It could remove a real dimension or fixed key from the plan, or write to the caller's original table during inspection. | [Backend lane](backend/README.md) |

The first repair concerned interpretation of one captured expression. The
second concerned the source-isolation boundary of typed metadata acquisition.
Neither depended on the other. Wrong partitioning and caller-source mutation
under the second trigger belonged together because both arose before safe
root truncation at the same registered-method entry point.

### Authored namespace operation

The minimal constructor witness used two distinct values of `a` and one value
of `b`. A bound specification selected `a`; the caller's namespace operator
returned a function selecting `b` for an explicitly base-qualified parenthesis
call. Direct typed tidyselect selected `b` after one lookup. Nested
interpretation selected `a` after zero lookups. Both Nest verbs returned two
cells rather than one. The 300-comparison matrix recorded 120 wrong public
results, 30 false singleton-constructor refusals and 150 successful private
qualification controls. Genuine base identity wrappers also passed.

The coordinator's [independent replay](coordinator/independent-qualified.R)
used different column names and made the operator return another Grouping
specification instead of an ordinary selection. Evaluating the authored call
selected `partition`; nesting it selected `detail` without invoking the
operator. Both Nest verbs again returned two cells instead of one. Observation
mode passed its failure-and-control assertions; desired-contract mode exited
one at the plan-equality assertion, as captured in the adjacent logs.

ADR 0019's ordinary-lookup rule and its *isolate generated namespace
operations* amendment retained caller-authored namespace expressions in their
captured environment. ADR 0008 supplied the one-reading budget. Closed #1383
and merged PR #1395 repaired ordinary forwarded selections; the new witness
found a nested specification before the selection-restoration path. The
constructor note recorded the adjacent reports and preserved the distinct
syntactic-parenthesis ambiguity rule from #1240.

### Registered head dispatch

The backend witness used an ordinary data.table/data.frame class, without an
additional subclass. Its replacement registered head method delegated normally
and changed the key's type only in zero-row output. Native collection and
name-only inspection retained the character key. Typed inspection lost it,
and both Nest verbs returned a single keyless cell instead of two keyed cells.
Restoring the registration recovered the intended behavior. Eighteen
graph/role/consumer combinations reproduced the wrong grain.

A second method wrote to its populated argument before delegating. Inspection
changed the caller's original source for both immutable and Mutable-step
inputs. The two Mutable-step Nest refusals remained early and invoked no
method. ADR 0020's dtplyr metadata amendment required faithful metadata without
processing populated source rows, or safe refusal; ADR 0029 protected every
inspection root. Closed #1382 and merged PR #1392 refused additional root
subclasses, while this ordinary-class registered method remained admitted.

The coordinator's [independent replay](coordinator/independent-head.R) confirmed
head inputs of two rows and zero rows, wrong plans and Nest cells, recovery
after registration restoration and original values changed to
`written,written`. Its desired-contract mode accepted either faithful schema
or early safe refusal with a protected source. That mode exited one on the
tested source. Each process restored temporary S3 registration on exit.

## Completed finite coverage

| Boundary | Dated result |
| --- | --- |
| [Local typed integrity](local/README.md) | 1,080 public calls across 30 types, three column roles, both keep options, all sort requests and both Nest verbs: 1,008 semantic matches and 72 native-or-contract refusals. Source serialization was unchanged after every call. |
| [Local Margin order](local/README.md) | All 672 sorted successful variants matched an independently assembled key containing fixed-key missingness, Grouping bits, dimension missingness and displayed values. |
| [Grouped and empty shapes](local/README.md) | 72 grouped-input comparisons, two rowwise refusals and twelve zero-column input comparisons passed. |
| [Ordinary selection correspondence](constructors/README.md) | 1,380 comparisons: 1,136 exact complete-plan matches and 244 paired refusals with identical condition classes and messages. |
| [Qualified specification boundary](constructors/README.md) | 300 comparisons: 150 confirmed failures and 150 matching private-qualification controls. |
| [dtplyr graph correspondence](backend/README.md) | 1,242 graph comparisons: 1,134 exact matches and 108 documented safe refusals; no additional mismatch. |
| [Registered head boundary](backend/README.md) | Eighteen wrong-grain cases, two source-mutation cases and two successful Mutable-step Nest refusal controls. |
| [Remote frontier](backend/README.md) | 96 SQLite/DuckDB/Arrow calls passed inspection/refusal controls. SQLite sent zero statements, DuckDB typed inspection sent six zero-row statements, and 18 original Arrow readers retained all 36 rows. |
| [Mixed-family algebra](coordinator/mixed-algebra.R) | 750 seeded generated trees with nested unions/products, overlapping composite units and typed selections yielded 2,250 exact plan-or-duplicate outcomes across three duplicate policies. |
| [Duplicate position audit](coordinator/audit-positions.R) | All 244 duplicate reports parsed and every reported group referred to genuinely equal memberships at valid occurrence positions. Two deliberately wrong position groups were rejected by the audit. |

The algebra oracle enumerated singleton, union, prefix, subset and Cartesian
families independently of package expansion, masks and deduplication. Its
finite generation limited oracle families to 128 occurrences and nesting to
six constructor levels. The first diagnostic-position parser mishandled cli's
bullet prefix; it was repaired, the entire seeded run repeated, and a separate
saved-result audit checked every duplicate report. Only the corrected run
supplied the table's counts. This was investigation code, not a package repair.

Six repeated-unit rollup observations, from 50 through 1,600 units, returned
exactly the two expected retained sets in 0.081–0.687 seconds on this machine.
That bounded probe established no resource-failure claim or new ticket.

The local lane excluded native factor-array recursion, an AsIs-factor union
cast failure and an AsIs-POSIXlt class-attribute difference after proving each
with native operations. The constructor lane excluded an initial fixture whose
reserved numbered-dot name made all comparisons uninformative, then reran its
complete corrected selection matrix. These controls prevented paired failures
from being reported as successful behavioral coverage.

## Main confirmation, execution and replay

Fetching main immediately before publication returned the unchanged tested SHA.
All-state Issue and PR searches and nearest-report dispositions were recorded
in the lane notes. The constructor search examined snapshots containing 702
issues and 691 PRs; the backend lane retained relevant complete issue/PR
records. No equivalent report for either concrete residual trigger was found.

The coordinator loaded its own archive, separate from each investigator's.
Observation and desired-contract executions were collected to terminal exit
zero and one respectively. The independent qualified pair used session
`14809`; the corrected head pair used session `86371`. The final seeded
algebra run used session `59536`, exit zero. The position audit exited zero.
All investigation executions and owned children were collected; no live
backend connection or experimental S3 registration remained.

Run from a disposable source archive. The scripts accept source/output options
documented in their lane notes. Desired-contract assertions intentionally failed
on the tested source; observation modes asserted the demonstrated failures and
successful controls, without claiming a fix.

```sh
p=/absolute/path/to/investigation/2026-11-06-nest-grouping-hunt
Rscript "$p/constructors/qualified-spec-minimal.R" --assert
Rscript "$p/backend/registered-head-minimal.R" --assert
Rscript "$p/coordinator/independent-qualified.R"
Rscript "$p/coordinator/independent-head.R"
Rscript "$p/coordinator/mixed-algebra.R" . /tmp/nest-grouping-algebra-replay
Rscript "$p/coordinator/audit-positions.R" /tmp/nest-grouping-algebra-replay
```

`MANIFEST.sha256` identifies the retained evidence; trailing horizontal
whitespace in text captures was removed. Scripts were research
fixtures under the existing investigation exclusion. Package sources,
tests/helpers, generated documentation, installed files, dependencies,
exclusions and check machinery retained their baseline bytes. NEWS did not
need an entry for this evidence-only change. The submitted SHA, scope judgment,
Review-ready result and current-main compatibility assessment belong to the
PR verification record.

This finite investigation did not establish arbitrary caller methods,
all possible specification/expression/query graphs, hosted databases,
unbounded-resource behavior, interruptions or other dependency/OS versions.
No package implementation or post-fix success was claimed. CI monitoring was
outside the requested work.

[exhaustive investigation]: ../2026-11-06-nest-spec-exhaustive/README.md
[greedy investigation]: ../2026-11-06-nest-grouping-greedy/README.md
[boundary investigation]: ../2026-11-05-nesting-grouping-spec-hunt/README.md
[#1396]: https://github.com/sayuks/marginplyr/issues/1396
[#1397]: https://github.com/sayuks/marginplyr/issues/1397
