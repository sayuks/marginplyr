# Nesting and Grouping specification boundary bug hunt

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`main`)

## Scope and outcome

A new worktree was created from main on
`investigate/nest-grouping-bughunt-20261102`. Fetched origin main matched the
audited baseline, including the final pre-publication confirmation. Four
investigators examined `nest_with_margins()`, `nest_by_with_margins()`, the five
Grouping specification constructors, `inspect_grouping()` and their shared
selection and Margin-label paths. The October 30
[equality/lifecycle investigation](../2026-10-30-nest-spec-greedy/README.md)
and October 31 [identity investigation](../2026-10-31-nest-spec-greedy/README.md)
supplied prior findings and explicit limits to extend. Their measurements were
not counted as runs on this baseline.

Two new confirmed defects were published through the user's `to-tickets`
workflow:

| Finding | User-visible failure | Ticket | Blockers |
| --- | --- | --- | --- |
| F1: generated dtplyr Margin-label helpers used caller bindings | Wrong outer keys/labels, collection failures, or an explicit label-collision check silently accepted | [#1143](https://github.com/sayuks/marginplyr/issues/1143) | None |
| F2: names-only helper classification forced empty dots | A valid optional-column wrapper raised an internal missing-value error before selection | [#1144](https://github.com/sayuks/marginplyr/issues/1144) | None |

Both tickets received `bug` and `ready-for-agent`. Each repair had a complete,
independently verifiable public behavior and neither gated the other. The user
delegated granularity and publication, so the skill's breakdown interview was
omitted. No parent Issue was modified. No implementation or permanent package
regression test was changed; the fix and post-fix phases of `diagnosing-bugs`
were outside the requested scope.

## F1: caller helpers changed Margin labels and bypassed collision validation

A one-row immutable dtplyr source with character dimension `g = "a"` and an
integer payload sufficed. A nesting result with a single included
`grouping_set(g)` was built before the source root environment acquired an
`as.character` function returning `MASK`. Collecting that same result then
returned `MASK` as its outer key. Its cell still held the original row and,
with `.keep = TRUE`, the original `g = "a"`. Source serialization did not
change. Restoring only that helper binding to the base function restored the
same result's key to `a`.

The [backend record](backend/README.md) retained minimal observation and
desired-behavior modes, ranked hypotheses, generated queries and controls.
Ordinary collection, grouped count and upstream grouped nesting kept their
keys and invoked no masked helper. Root-local `rep` and `length` bindings
also changed omitted labels or raised while collecting rollups. Explicitly
user-written helper calls still used the caller's binding, which was the
ordinary behavior the control was meant to retain.

The shared observed-label collision predicate was affected too. For an actual
source key equal to `Total`, root-local `as.character` or `sum` made both
nesting verbs, expansion and summary accept `.check_margin_label = TRUE`
where the unmasked calls raised a Package condition. All eight helper/verb
combinations reproduced this bypass. This was a requested validation being
altered, not a lazy default declining an unrequested scan.

An [independently authored probe](coordinator/helper-isolation.R) used integer
keys `2, 1, 2` and a root helper returning `CORRUPTED`. Both nesting verbs,
expansion and summary returned corrupted included keys; ordinary collection
and grouped count stayed unchanged. Nesting payloads matched their unmasked
counterparts exactly. The desired-key assertion returned terminal exit 1.

The separating controls supported caller lookup in package-generated label
conversion, omitted-value size/repetition and collision expressions, rather
than input mutation, incorrect source memberships or an inability of upstream
nesting to group that source. The source sites were `label_margin_branch()`
and the observed-label predicate in [Margin-label handling](../../R/margin-label.R),
with omitted-identity sizing in the portable expansion path also exposed by
the helper matrix. The contract was the nesting reference's identification of
one cell per source group and its distinction between display labels and
original `.keep` values, together with the explicitly requested collision
check. ADR 0020 supplied the requested-read boundary.

Closed #1089 and PR #1119 isolated representative display extraction through
`min` and `seq_len`; that fixed behavior was retained on this baseline. This
finding reached distinct label expressions before representative extraction,
including a single included grouping set and collision validation. The full
bodies/comments of those reports and #427 / PR #433 were read, as was the
generated-target repair PR #1058. The backend record retained the all-state
Issue/PR queries and matches. No duplicate report for this boundary was found.

## F2: an empty optional-column wrapper failed before selection

A wrapper accepting `...` and calling inspection with `.by = all_of(c(...))`
raised `getvarError: argument "value" is missing, with no default` when called
without arguments. Direct tidyselect evaluation in that same wrapper returned
an empty selection. Supplying one column name succeeded. Putting the selector
inside `grouping_set()` instead of `.by` reproduced the failure, including
through both nesting verbs. No remote input or Margin label was needed.

The [specification record](spec/README.md) retained the minimized reproduction,
finite matrix and causal controls. Whole-helper wrapping with
`identity(all_of(c(...)))` kept the same caller environment and succeeded;
`all_of(character())` succeeded too. The names-only helper-argument reader
independently reproduced the missing-value error on the empty dots symbol.
Its deferred-binding check did not recognize that symbol as requiring
selection-time resolution. These controls separated the classifier failure
from tidyselect rejecting the selector or losing its captured environment.

The [coordinating reproduction](coordinator/empty-dots.R) independently checked
fixed-key and constructor positions through inspection and both nesting
verbs. All six empty-dots calls failed with the same class/message while
native empty selection and all six populated controls succeeded. Its
desired-behavior assertion returned terminal exit 1.

Open #1138 concerned populated dots forwarded into local summary selectors
through a different staging reader. This finding occurred in the Grouping
names-only classifier and was also reachable through `.by` alone, so neither
repair depended on the other. Full bodies/comments of #1138, #351, #925,
#1139 and PR #1142 were read. The specification record retained the all-state
Issue/PR queries and explained why missing source arguments, omitted embrace
payloads and known fixed-key overlaps were different boundaries. No duplicate
empty-dots classifier report was found.

## Completed coverage

These rows belonged to different finite oracles and were not combined into a
single passing-assertion total. Track records held details and exclusions.

| Evidence | Completed observations |
| --- | --- |
| [Local composed-key oracle](local/README.md) | 2,160 calls from 180 generated sources/plans crossed ten key classes, two dimensions, zero/two fixed keys, both verbs/keep values and all sort choices. Every membership matched; 174,516 cell-column projections matched, with no source mutation. |
| Local empty/scalar boundaries | 500 calls matched expected empty/populated cardinality, special list-column names, attributed scalar `.key`/`.id`, rowwise identifiers and cell values. Historical #1073/#1088 desired-behavior controls passed. |
| [dtplyr label helpers](backend/README.md) | 432 comparisons: 60 wrong results, 36 collection errors and 336 matches. Eight explicit-collision helper/verb combinations incorrectly accepted a colliding source. |
| dtplyr type differential | 864 cases: 612 exact matches, 240 native backend errors and 12 raw-missing refusals; no admitted mismatch. |
| Retained selections and conditions | 765 comparisons: 264 correct selections, 420 matching External conditions, 45 renaming refusals and 36 empty-unit refusals. Another 200 effect comparisons retained one caller evaluation and one warning each. |
| [Grouping helper classification](spec/README.md) | 360 calls: 120 empty-dots failures and 240 populated or whole-helper-wrapper controls with successful selections or required empty-unit refusals. |
| Attributed scalar names, coordinating probe | 400 exact comparisons against plain-string `.key`/`.id`, across empty/populated, keyed/no-key, local/dtplyr and both verbs. |
| [Remote admission and inspection](coordinator/remote-admission.R) | 60 calls across renamed, empty, union and joined sources: SQLite sent no statements; DuckDB sent five, all `LIMIT 0`. Unsupported nesting and locally invalid inspection sent none. Five Arrow reader graphs retained all ten source rows after twenty calls. |

Track records distinguished native limitations and required refusals from
correct admitted results. No full package suite was run for this hunt.

## Replay, provenance and publication validation

Run the R entry points from the investigated checkout root; each loaded that
checkout through `pkgload::load_all()`. The minimal desired-behavior commands
were intentionally red on this unfixed baseline:

```sh
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/reproduce-label-helper.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/backend/collision-helper-controls.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/spec/empty-dots.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/coordinator/helper-isolation.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/coordinator/empty-dots.R --assert
```

Adjacent logs retained observed results. Observation modes exited zero while
checking the dated failures; they were not passing regression tests. The
coordinating [environment log](coordinator/environment.log) recorded R 4.6.1
and dependency versions; the track logs recorded their environments too.
`MANIFEST.sha256` identified the retained evidence.

One initial coordinating helper probe constructed the lazy source in its
parent instead of its intended private environment and therefore measured a
negative control. The final script constructed it inside that environment.
An observation execution handle became unavailable across session resumption;
its log alone was not treated as a terminal pass. Owned-process inspection
found no remaining coordinating probe before the corrected desired-behavior
run was collected with terminal exit 1. Track READMEs documented their other
fixture corrections; incomplete development counts were excluded.

Only dated investigation material was added. It neither changed package code,
tests, examples, generation inputs, installed files, dependencies nor check
behavior. Its effects were evidence retention and links to separate repair
tickets, so `source` scope was selected under
[Local checks](../../design/agents/local-checks.md). The checked SHA, five-check
terminal result, explicit omission of package build/check, complete suite and
snapshots, main compatibility assessment and review record belonged on the
pull request. Earlier investigation notes were not amended. CI monitoring was
outside the request.

These finite probes did not establish freedom from all bugs, arbitrary S3/S4
methods or input classes, every evaluation boundary, a full independent Margin
order oracle, other R/dependency/OS versions, unbounded Grouping plans or
resource-exhaustion behavior. SQL/Arrow checks established inspection and
refusal safety, not support for nesting those backends. Native dtplyr
composition limitations were recorded separately from package findings.
