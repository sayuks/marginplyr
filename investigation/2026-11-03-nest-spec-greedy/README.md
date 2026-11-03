# Nesting and Grouping specification greedy boundary bug hunt

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`main`)

## Scope and outcome

A new worktree was created from main on
`investigation/nest-spec-greedy-2026-11-03`. Fetched origin main matched the
baseline at creation and immediately before Issue publication. Four
investigators examined both nesting Margin verbs, the five Grouping
specification constructors, `inspect_grouping()` and their shared ordering,
display conversion and backend-admission paths. The October 30
[equality/lifecycle](../2026-10-30-nest-spec-greedy/README.md), October 31
[identity](../2026-10-31-nest-spec-greedy/README.md), and November 2
[boundary](../2026-11-02-nest-grouping-hunt/README.md) investigations supplied
earlier evidence and remaining limits. Their counts were not counted as new
executions, and their reported defects were distinguished from the repaired
behavior on this baseline.

Three independently validated defects were published through the user's
`to-tickets` workflow:

| Finding | User-visible failure | Ticket | Blockers |
| --- | --- | --- | --- |
| F1: caller bindings replaced generated dtplyr sort helpers | Missing values or margins appeared at the wrong end despite explicit `.sort` | [#1176](https://github.com/sayuks/marginplyr/issues/1176) | None |
| F2: empty hms display conversion produced a logical key | Empty rollups/cubes failed with an internal branch type error; included-only displays had the wrong type | [#1177](https://github.com/sayuks/marginplyr/issues/1177) | None |
| F3: scalar array display shape reached an internal collision conditional | Valid noncolliding nesting failed, and real collisions received the wrong diagnostic | [#1178](https://github.com/sayuks/marginplyr/issues/1178) | None |

Each Issue received `bug` and `ready-for-agent`. These were independently
verifiable repair slices; no prefactoring or blocking edge was necessary. The
user delegated granularity and publication, so the skill's breakdown interview
was omitted. No parent Issue was modified. No implementation or permanent
package regression test changed. The fix and post-fix phases of
`diagnosing-bugs` were outside the requested investigation-only scope.

## F1: generated ordering depended on caller helpers

The [backend record](backend/README.md) retained minimized public-seam
reproductions, three ranked hypotheses, generated queries and separating
controls. A two-row immutable dtplyr source with character keys `a` and missing
sufficed. A single included grouping set under `.sort = "last"` was built
before the root environment acquired an `is.na` function that inverted base
missingness. Collecting that same query then put the missing key first. A
rollup with a root `%in%` function that inverted membership put the Grand total
set first under the same sort choice. Restoring only each binding recovered
the same query's correct order.

Native source collection, grouped count and upstream grouped nesting invoked
the masked helpers zero times. Source serialization and unordered
cell/key/identifier projections stayed intact. The controls supported late
lookup in package-generated sorting rather than source mutation or general
grouped-backend instability. Explicit user-written helper calls retained their
ordinary lexical behavior.

An independently authored [coordinating reproduction](coordinator/sort-helper-independent.R)
confirmed both helpers through nesting, rowwise nesting, expansion and summary.
Its desired-behavior assertions returned exit 1 after restoration and native
controls passed. Rowwise nesting was invoked while the binding existed because
it collects at construction; the other results were built before rebinding.
Expansion retained one Grand total copy per source row, rather than borrowing
the nesting/summary cardinality.

ADR 0018 supplied the missing-last and Grouping-bit ordering contract.
`margin_missing_last_expr()` and `margin_grouping_bit_expr()` generated the
unqualified calls. Earlier #1089/#1143/#1171 and their repairs concerned
representative extraction, Margin labels and zero-column identifier repetition;
they were not duplicate reports of this sort expression. The backend record
retained the all-state Issue/PR queries and full-body/comment comparisons; the
coordinator also read #1171's full report and comments.

## F2: empty hms conversion broke branch assembly

The [local record](local/README.md) retained the empty-display matrix and
minimal desired-behavior command. An empty local tibble with an hms dimension
was accepted by ordinary grouping and nesting. Both Margin nesting verbs with
the default non-missing label and `rollup(g)` instead raised
`vctrs_error_ptype2`, naming logical and character versions of an internal
column. The same failure reached expansion and summary. A single included set
avoided the union but returned an empty logical display column.

With hms 1.1.4 the empty character conversion answered `logical(0)`; populated
conversion answered character. The package used that first answer directly in
its detail branch and combined it with its own character label branch.
Populated input, ordinary difftime/character keys, and typed-missing labels
separated this boundary from an input ordinary dplyr could not group. The
source stayed unchanged.

The coordinator's [independent reproduction](coordinator/empty-hms-independent.R)
confirmed all four verbs; its desired-behavior assertion returned exit 1 after
the native, populated and typed-missing controls passed. A further
[independent assessment](spec/validation-assessment.md) separated the upstream
empty-conversion trigger from the package's branch mismatch. The public
*Display labels and grouping identity* contract promised character dimensions
for non-missing labels. ADR 0033's limited declared-output table was not used
as a promise about ordinary dimensions. Related scalar-label, dimensional-name
and typed-missing reports did not describe this empty display conversion.

## F3: the collision predicate rejected scalar array displays

A one-row `posterior::rvar` key with four draws of one variable supplied a
normal scalar-per-row dimension. Native unsorted grouping/nesting, checked
fixed-key nesting, typed-missing labels and unchecked Margin nesting retained
its one source row. Default checked nesting instead raised that the internal
condition was a logical one-dimensional array rather than a logical vector.
No rollup or union was needed.

The display conversion answered a character one-dimensional array. The
generated observed-label comparison retained that shape into an internal
conditional requiring a plain vector. Reading those same displayed scalar
values without array shape gave the correct absent/present collision counts.
Both the noncolliding default and a label equal to a source display received
the internal error instead of the promised collision behavior.

The [local dependency-free witness](local/array-display-collision.R) used a
one-dimensional integer-array subclass whose character method retained that
shape. All four Margin verbs reproduced the check error, while disabling the
check succeeded. This allowed focused repair coverage without adding posterior
as a package dependency. The specification investigator independently checked
the rvar fixture and predicate. The coordinator replayed the dependency-free
desired-behavior command against its own baseline copy and collected exit 1.

The local record retained the ranked hypotheses, finite controls and duplicate
searches. This admitted one-dimensional scalar key was distinct from the
existing matrix/higher-dimensional display-label refusal, and from earlier
typed-missing prototype and row-order defects. The repair did not require a new
public option, equality rule or arbitrary attribute restoration.

## Completed coverage and exclusions

These rows belonged to separate finite oracles and were not combined into one
passing-assertion total. Each track retained its environment and replay logs.

| Evidence | Completed observations |
| --- | --- |
| [Independent direct-order oracle](coordinator/order-oracle.R) | 1,180 exact comparisons across seven dimension classes, missing/display labels where admitted, irregular grouping-set order, optional fixed keys, plain/tibble/grouped sources and both nesting verbs. Order, outer keys, cell projections, rowwise identifiers and source immutability matched. |
| [Specification selection frontier](spec/README.md) | 1,368 comparisons: 1,000 exact plans, 264 matching External-condition classes, 80 required empty-unit/composite refusals and 24 renaming refusals. Included forwarded dots/defaults, typed helpers and both composite positions. No new specification defect remained. |
| [dtplyr sort helpers](backend/README.md) | 576 comparisons: 144 wrong orders and 432 matches. Unmasked local-oracle order and all source/cell projections matched. Matching empty output alone did not prove helper isolation. |
| Local exotic-key membership | 1,024 calls: 956 matched membership/projections and 68 errors were classified separately. No admitted membership/projection mismatch remained. |
| Local display conversion | 576 comparisons: 480 character matches, 64 empty-hms rollup/cube errors, 32 empty-hms included-set logical displays. |
| Local populated rvar collisions | 64 comparisons: 32 checked internal errors and 32 successful unchecked controls. The dependency-free witness additionally reached all four Margin verbs and actual collisions. |
| Backend remote boundaries | 66 calls: SQLite sent zero statements across 25 calls; DuckDB sent five `LIMIT 0` statements across 25 calls; sixteen Arrow calls preserved all eight readers and sixteen original rows. |

The independent order oracle constructed source groups and the documented key
without calling marginplyr's plan or ordering helpers. It covered character,
numeric, factor, Date, list, matrix and packed dimensions; structured/list
dimensions used typed-missing labels. It did not require unspecified default
order. An initial `.by` harness injected a quosure as an ordinary value; the
completed oracle used a literal selection. The independent helper replay's
initial expansion expectation was corrected to expansion cardinality before
its retained desired-behavior run. Neither fixture mistake was a finding.
The specification record likewise retained and explained an initial capture
asymmetry in its own direct tidyselect oracle.

The local track classified unsupported structured-display calls and native
extension limitations separately. Its empty-rvar exploratory matrix encountered
a native grouping crash; that incomplete matrix was excluded. The final rvar
matrix used supported populated unsorted nesting controls. The track record
held the terminal crash outcome and the final completed execution separately.
These limitations were not repair requirements or passing package results.

## Replay and publication validation

Run the retained R entry points from the investigated checkout root. They load
that source through `pkgload::load_all()`; the captured executions used separate
disposable archives of the baseline. The minimal desired-behavior commands were
intentionally red on this unrepaired revision:

```sh
Rscript investigation/2026-11-03-nest-spec-greedy/backend/reproduce-sort-helpers.R --assert
Rscript investigation/2026-11-03-nest-spec-greedy/backend/reproduce-sort-helpers.R '%in%' --assert
Rscript investigation/2026-11-03-nest-spec-greedy/local/display-conversion.R --assert
Rscript investigation/2026-11-03-nest-spec-greedy/local/array-display-collision.R --assert
```

Each returned terminal exit 1 for the expected desired-behavior assertion.
Observation modes that asserted the dated defect were not passing regression
tests. All yielded executions were retained through terminal results and owned
work was collected. `MANIFEST.sha256` identified the retained evidence;
the [coordinating environment](coordinator/environment.log) and track logs
recorded R 4.6.1, macOS arm64 and relevant dependency versions.

Only dated research evidence was added. Its effects were retaining observations
and linking independent repair tickets; it changed no package code, tests,
examples, generation inputs, installed files, dependencies or check behavior.
The existing source exclusion was unchanged. `source` scope was selected under
[Local checks](../../design/agents/local-checks.md) on those effects, with package
build/check, complete suite and snapshots omitted. The checked SHA, five-check
terminal result, integration assessment and separate review record belonged on
the pull request. Earlier notes were not amended. CI monitoring was outside
the request.

These finite probes did not establish freedom from all bugs, arbitrary S3/S4
methods or expressions, complete External-condition identity, every remote
graph, another dependency/R/OS version, unbounded Grouping plans or resource
exhaustion. SQL and Arrow measurements established inspection/refusal safety,
not support for nesting those backends. No post-fix result was claimed.
