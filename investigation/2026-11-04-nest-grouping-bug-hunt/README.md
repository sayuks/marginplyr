# Nesting and Grouping specification greedy bug hunt

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

## Scope and outcome

A new worktree was created from main on
`investigate/nest-grouping-bughunt-20261104`. Four investigators examined
`nest_with_margins()`, `nest_by_with_margins()`, the five Grouping
specification constructors, `inspect_grouping()` and their shared factor,
selection, name, metadata and admission paths. The October 30/31 and November
2/3 nesting investigations informed the next boundaries; their historical
observations were not counted as executions on this baseline. The November 3
[nesting record](../2026-11-03-nest-spec-greedy/README.md) and
[summary/selection record](../summary-grouping-share-bughunt-2026-11-03.md)
were the most recent starting points.

Two confirmed defects were published with the user's `to-tickets` workflow:

| Finding | User-visible failure | Ticket | Blockers |
| --- | --- | --- | --- |
| F1: factor display became its stored value | Included outer keys silently changed to another level or became missing | [#1209](https://github.com/sayuks/marginplyr/issues/1209) | None |
| F2: caller-promoted selection errors were hidden by preflight | A metadata query preceded a locally detectable failure, or metadata refusal replaced the original error | [#1210](https://github.com/sayuks/marginplyr/issues/1210) | None |

Each ticket was an independently verifiable repair slice and received `bug`
and `ready-for-agent`. The user delegated ticket granularity and publication,
so the skill's breakdown interview was omitted. No parent issue was modified.
No package implementation or permanent regression test was changed. The fix
and post-fix phases of `diagnosing-bugs` were outside this investigation-only
request.

## F1: original factor values were reconstructed from display strings

The [local investigation](local/README.md) retained the minimized public
reproduction, ranked hypotheses, independent source-membership oracle and
duplicate comparisons. A one-row factor subclass stored level `A` and
displayed it as `A!`. Both nesting verbs with a single included grouping set
and the default label returned a missing factor key. Its retained cell still
held the original factor code. No omitted set, union, source missing value,
sort request or NA level was necessary.

An independently authored [coordinating reproduction](coordinator/factor-values-independent.R)
used declared levels `A` and `B` and a renderer swapping their displays.
Both nesting verbs silently returned outer code 2 for source code 1, while
their nested source columns retained code 1. Native union/nesting, fixed-key,
typed-missing-label and source-immutability controls passed. Expansion and
summary reproduced the same substitution through the shared factor path.
Its desired-behavior assertion returned terminal exit 1.

The separating observations supported display dispatch during factor encoding,
followed by matching that display against the declared source levels. The
private nesting identities retained source grouping membership but could not
protect the reconstructed outer value. ADRs 0012 and 0016 supplied the factor
restoration boundary; the finding required original value content, not
arbitrary subclass or attribute restoration. A plain factor output retaining
the source level value would satisfy it.

Three renderer variants were grouped into one ticket because all reached that
same encoding/reconstruction path: missing values, substitution with another
declared level, and substitution with the synthetic label. Earlier #1088,
#1143, #421, #427 and #1177 concerned different equality, deferred lookup,
NA-level, overwrite or empty-display boundaries. The local record retained
all-state Issue/PR searches, full-body/comment comparisons and an initial
fixture whose native union was itself unsuitable and was excluded.

## F2: ordinary warning options hid a caller's promoted selection error

The [specification investigation](spec/README.md) retained the minimized
reproduction, actual DBI tracing, condition-identity controls, finite matrix
and duplicate comparisons. With ordinary warning options 0 or 1, a caller's
warning handler raised its own error on a names-only selection of an absent
column. Direct tidyselect, local inspection and fixed-key selection propagated
that same error object. A Grouping specification on live DuckDB instead sent
a zero-row metadata query before propagating it. On a derived dtplyr input,
a later typed selection reached metadata refusal before the handler could
raise the caller's error.

The independent [coordinating reproduction](coordinator/handler-independent.R)
used warning option 1 and its own condition carrying a custom marker. It
confirmed replacement by `marginplyr_error` through inspection and both
nesting verbs. Direct selection, local Grouping compilation and derived
fixed-key controls preserved the exact caller condition. Its desired-behavior
assertion returned terminal exit 1.

The observations supported discarded-pass warning suppression, rather than
loss of the selector environment or failure of native selection. ADR 0005
required locally detectable errors before metadata acquisition; ADR 0015
required propagation of External conditions. Closed #1184 and merged #1198
handled option-driven promotion under `warn = 2`. Their repair left ordinary
warning options on the suppressed path, so #1210 was a separate residual
repair ticket. Neither completed report was reopened or modified.

## Completed coverage

These were separate finite oracles, not one combined passing-test count.
Track records and adjacent outputs retained environments and exclusions.

| Evidence | Completed observations |
| --- | --- |
| [Local factor oracle](local/README.md) | 6,912 nesting calls: 576 detected wrong outer values; the other 6,336 calls matched the selected checks. Three renderers, ordered/ordinary factors, NA levels, empty/populated sources, tibble/plain frames, dimension/fixed/payload roles, included/rollup plans, string/NULL labels, keep choices and all sort choices. Every membership, cell projection, fixed value and source-immutability check matched. The outer comparison did not independently establish the NA-level/missing-code distinction. |
| [Backend graph oracle](backend/README.md) | 51 graphs: 51 matching name-only plans, 48 matching typed plans and three conservative metadata refusals; 576 matching nesting results and 36 corresponding refusals. No unexpected errors, warnings, callbacks during construction or source changes. |
| Remote frontier | 72 SQL boundary calls: SQLite sent zero statements; DuckDB sent twelve, all `LIMIT 0`. Twenty-four Arrow boundary calls left eighteen original readers and all 36 source rows available. |
| [Grouping handler matrix](spec/README.md) | 390 cases: fifty derived-dtplyr error replacements and thirty extra actual DuckDB metadata queries under warning options 0/1; all 65 stopping-handler controls under option 2 preserved the original error before metadata. All 195 nonstopping-muffling controls preserved one warning delivery and two predicate evaluations. |
| Grouping warning identity frontier | 600 local comparisons: 492 exact native warning-sequence/error-class matches and 108 already-established quiet empty-unit refusals. Those refusals were not new findings. |
| [Coordinating name oracle](coordinator/name-matrix.R) | 1,748 calls across 44 spellings, five input/output roles, local/dtplyr, both nesting verbs and keep choices. All 1,676 admitted outputs matched outer keys, identifiers, requested order and cell projections. Seventy-two source-name refusals matched native tidyselect's rejection of `...`, `..1` and `..2`; twelve documented rowwise `.rows` combinations were excluded. Sources remained unchanged. |

The name oracle compared renamed calls with a regular-name local control. It
preserved outer sequence under `.sort = "last"` and normalized source row
order inside cells only. Lazy results were collected with minimal name repair
to distinguish the operation from default tibble name validation. A preceding
membership-only development run normalized outer order; its counts were not
added to the retained final order-sensitive run. Native tidyselect controls
independently classified all three rejected source spellings.

Backend initial harness failures were retained separately. Native oracle
collection created data.table secondary indexes until automatic indexing was
disabled; SQLite typed inspection was first incorrectly expected to succeed.
Neither harness assumption became a package finding. Conservative typed
metadata refusals were not counted as matching output comparisons. SQL/Arrow
measurements established inspection and refusal safety, not nesting support.

## Replay, publication and verification boundary

Run retained R entry points from the investigated checkout root. The
coordinating scripts also accept a disposable source directory as their first
argument. The investigated executions loaded separate archives of the exact
baseline through `pkgload::load_all()` and disabled testthat parallelism.
The desired-behavior commands were intentionally red on this unfixed revision:

```sh
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/local/factor-display.R --assert
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/spec/handler-promotion.R --assert
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/coordinator/factor-values-independent.R --assert
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/coordinator/handler-independent.R --assert
```

Each returned terminal exit 1 for its desired-contract assertion. Observation
modes that asserted the dated defect were not passing repaired regression
tests. All yielded executions were retained through terminal completion and
owned work was collected. The final name-oracle session was `37606`; its
terminal exit was 0. Track records retained the other execution identities.
The adjacent environment and output files recorded R 4.6.1 on macOS arm64,
with dependency versions. `MANIFEST.sha256` identified retained evidence.

Before Issue publication, fetched `origin/main` still resolved to the baseline
SHA above. All-state duplicate queries and relevant full bodies/comments were
checked; no matching open report or already-fixed version of either defect
was identified. Publication used two independent tickets with no blocking
edges, as delegated by the user.

Only dated research evidence and repair-ticket links were added. No package
code, tests, examples, vignettes, generation inputs, installed files,
dependencies, exclusions or check behavior changed. These effects justified
`source` scope under [Local checks](../../design/agents/local-checks.md), with
package build/check, complete suite and snapshots omitted. The submitted SHA,
five-check terminal outcome, main compatibility assessment and separate review
record belonged on the pull request. Earlier notes were not amended. CI
monitoring was outside the request.

The finite probes did not establish freedom from all bugs, arbitrary S3/S4
methods, every selection/evaluation boundary, all query graphs or backends,
other dependency/R/OS versions, unbounded plans, performance at scale or
resource-exhaustion behavior. No post-fix result was claimed.
