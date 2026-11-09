# Margin summary and share window, key and callable audit

Investigated: 2026-11-09
Repository snapshot: `2e39d520f293fa7eddaefaa5b68596d6cbea21d0`

The investigation began from freshly fetched main in the new worktree
`/private/tmp/marginplyr-summary-r2-20261109`, branch
`investigate/summary-share-contracts-20261109-r2`. Re-fetching before Issue
publication resolved to the same SHA. Three fresh-context investigators used
separate detached worktrees at that SHA: A started from implementation and
summary/share staging, B from public contracts and admission, and C from
backend consistency and execution boundaries. Their initial hypotheses were
independent until all three had reported. The coordinator separately prepared
input generation, explicit-grain reference calculations and minimal replays.

Each worktree ran at most one reproduction or focused test at a time, with
`TESTTHAT_PARALLEL=false`. Retained execution handles were collected through
terminal completion. Database work used synthetic facts and process-owned
in-memory SQLite/DuckDB databases only. DuckDB used `shared_home=FALSE`;
connections were disconnected, with DuckDB shutdown. No package implementation,
package test, dependency, installed file or generated documentation was edited.

## Sources and independent expectations

The team read CLAUDE.md and its AGENTS.md reference, CONTEXT.md, the repository
local-check, integration, tracker and investigation procedures, and the public
summary, Grouping-constructor, Grouping-helper and share references. Relevant
ADRs covered capture and Grouping grammar (0007-0009/0026), shares
(0010/0014/0017), result ownership and ordering (0016/0018/0028/0033), static
spelling (0019), and lazy execution and conditions (0020/0021/0022/0029/0035).
Required documents were read through complete ranges; truncated reads were
completed rather than counted as full reads.

Recent evidence included the November 7 contracts, transfer and carrier notes,
November 8 discovery and callable/member investigations, and November 9
[evaluation-boundary](summary-spec-share-evaluation-boundaries-2026-11-09.md)
and [independent audit](summary-spec-share-independent-audit-2026-11-09.md)
notes. Repair history through PR #1552 supplied controls for first/later
ordinary across evaluation, Grouping-helper argument context, selector
lifetimes and known callable transfers. Exploration also covered older
denominator joins and the window implementation introduced by #939; it was
not restricted to those recent edits. Historical executions were not counted
as new evidence.

Expected values and input generation were separate. The coordinator explicitly
listed grouping-set occurrences, used ordinary dplyr aggregation at each grain,
and independently aggregated fixed-partition denominators. Its input generator
varied empty/single/multiple groups, duplicate facts, missing keys, factors,
plain/tibble input, zero/negative/missing/nonfinite measures and repeated Grand
total occurrences. Assertions compared values, keys, ratios, cardinality,
column order and input serialization. Backend comparisons used ordinary
same-engine summaries or literal displayed-denominator division. Agreement
between helpers sharing internal processing was never the sole oracle.

## Confirmed findings

The requested `to-tickets` workflow published three independent repair units
with `bug` and `ready-for-agent`, no blockers and no prerequisite refactor.
Each Issue contained its contract, minimal reproduction, expected/observed
result, impact, tested SHA/environment, dependencies, controls and acceptance
criteria. The user delegated splitting and publication without a confirmation
checkpoint. No implementation repair was made.

| Hypothesis and boundary | Executed observation and disposition | Issue |
| --- | --- | --- |
| An inherited SQL frame narrows the hidden denominator window. | SQLite detail shares became missing or otherwise wrong while displayed aggregates stayed correct. Full-frame controls passed. Confirmed. | [#1555](https://github.com/sayuks/marginplyr/issues/1555) |
| Complex missing keys cross-match in local denominator joins. | A four-row ordinary result became eight rows with Total shares, including ratios 1/3 and 3 across distinct fixed partitions. Confirmed. | [#1556](https://github.com/sayuks/marginplyr/issues/1556) |
| Stock base wrappers discard an already known callable's dependency proof. | Fourteen finite transfers bypassed the prohibited-source or later-share-read refusal; repaired direct/converter and legal controls passed. Confirmed. | [#1557](https://github.com/sayuks/marginplyr/issues/1557) |

### Inherited denominator frames

An owned SQLite table contained `g = c("a", "b")`, `x = c(1, 3)`. The
following input construction was sufficient:

```r
input <- facts |>
  dbplyr::window_order(dplyr::desc(g)) |>
  dbplyr::window_frame(-Inf, 0)
summarize_with_margins(
  input, value = sum(x, na.rm = TRUE), p = share_of_total(value),
  .grouping = rollup(g), .check_share_source = FALSE
) |> dplyr::collect()
```

The expected values by key were a: `1/.25`, b: `3/.75`, Total: `4/1`.
The actual detail ratios were missing. This explicitly ordered witness was
deterministic and warning-free; the frame alone also reproduced. Replacing
only the helper with Parent share reproduced. Removing only the frame, or
using `-Inf/Inf`, restored correct ratios. A no-share ordinary summary retained
the correct aggregates. This refuted a changed-source or wrong-root oracle.

The generated conditional `MAX` rendered `ROWS UNBOUNDED PRECEDING`, inheriting
the caller's frame instead of reading the full matching partition. dbplyr was
applying the metadata it was given; the hidden denominator expression and its
full-partition requirement were marginplyr's. Construction stayed lazy, and
the error appeared at explicit collection. DuckDB's materialized CTE provided
a separate metadata boundary in the tested native and portable paths.

C's 336-case matrix crossed two databases, default/forced-portable duplicate
paths, fixed keys absent/present (including missing keys), window order
absent/present, seven frame states and Parent/Total/both. Ordinary outputs
matched in all cases and source tables stayed identical. SQLite had 112/168
cases with wrong ratios (936 cells); all 48 plain/full-frame cases passed.
All 168 DuckDB cases passed. The coordinator separately replayed 72 cases
over six frames, fixed keys and all three sort options: 18 SQLite mismatches,
zero DuckDB mismatches. Sixteen simulator render comparisons supplied
plain/cumulative controls for PostgreSQL, MySQL, MSSQL and Snowflake, including
PostgreSQL's native and portable paths. The bounded queries inherited the
frame; no live execution result was inferred for those engines.

The existing `test-share-visible-denominators.R` file passed without warnings
or skips. It covered volatile values and fixed partitions but supplied no
input frame. Margin-order tests covered window order without this share/frame
intersection. Related #919/#939, #1147 and #769/#770 concerned reevaluation,
DuckDB staging and source-query boundaries, rather than this inherited frame.

### Complex missing-key identity

The minimum used two finite facts and two distinct complex keys:

```r
d <- tibble::tibble(
  k = complex(real = c(NaN, NaN), imaginary = c(0, NaN)),
  g = c("a", "a"), x = c(1, 3)
)
summarize_with_margins(
  d, value = sum(x), p = share_of_total(value),
  .by = k, .grouping = rollup(g), .margin_label = NULL, .id = "set"
)
```

Ordinary `summarize(.by = k)` produced two fixed partitions with totals 1
and 3. The no-share Margin result had one detail and one root per partition:
four rows. Every share should have been one. Total share instead returned
eight rows, with extra cross-partition ratios `1/3` and `3`; Parent share
returned six rows. Changing one key to a finite complex value restored the
four-row result. Nonfinite-summary exclusions did not apply: the summaries
were finite and the unusual values were keys.

The denominator join exposed a vctrs matching inconsistency. Ordinary `.by`
grouping and `vec_group_loc()` separated the keys; `vec_equal(...,
na_equal=TRUE)` distinguished them; `vec_locate_matches(...,
nan_distinct=TRUE)` cross-matched them. A direct dplyr join reproduced the
matching behavior. The package's hidden join nevertheless owed the stronger
partition/cardinality contract. The oracle used integer IDs derived from
ordinary membership, without repeating that join.

Sorted `group_by()` followed a different upstream path and collapsed some of
these keys. An initial coordinator oracle used that path and failed its own
no-share cardinality assertion. It was replaced by `.by` aggregation and
`vec_group_loc()` membership before establishing the independent failure.
This finding did not ask marginplyr to standardize all dplyr grouping behavior.

B's 48 adjacent comparisons found 21 discrepancies: seven distinct complex
NaN/partial-NA forms in each of fixed-key Total, fixed-key Parent and
variable-dimension Parent matching. The other 27 controls passed. A variable
dimension with Total shares needed no dimension denominator join and passed;
identical missing keys, finite keys, `NA_complex_` versus `NaN+0i`, and separate
numeric real/imaginary keys also passed. The coordinator's five-case replay
confirmed two mixed-NaN failures and three controls. Inputs stayed unchanged.
The existing `test-share.R` file passed without skips and contained no complex
key assertion; its NaN tests concerned numerators, not grouping identity.

### Callable-result proof loss

The minimum required a visible closure, a stock base wrapper and a literal
formal removal, without arbitrary reflection or an unknown callback:

```r
summarize_with_margins(
  data.frame(g = c("a", "b"), x = c(1, 3)), prior = sum(x),
  value = (function(prior) {
    h <- base::suppressWarnings(function() sum(prior))
    base::rm(prior)
    h()
  })(11),
  p = share_of_total(value), .grouping = rollup(g)
)
```

The required Package condition was absent; the returned values were `1,3,4`
and ratios `.25,.75,1`. Removing only the wrapper restored the refusal.
Retaining the formal or substituting an independent closure remained legal.
Ordinary explicit-grain dplyr calculations established the numeric alias read.
Reversing the order also admitted an ordinary read after an earlier share.

Ten direct callable-return forms reproduced: six condition wrappers
(`suppressWarnings`, `suppressMessages`, `tryCatch`, `withCallingHandlers`,
`withRestarts`, `try`), `local`, explicit current-environment `eval`, direct
`attr<-` return and `I`. Four finite singleton/stock-callback carriers also
lost the proof: `Filter(is.function, list(f))`, `lapply(list(f), identity)`,
`Map(identity, list(f))`, with literal element access, and singleton
`Reduce(identity, list(f))`. These were one proof-transfer family, not a
claim of unrestricted higher-order interpretation.

The final 816-call matrix used all 17 forms (14 affected and three repaired
controls), four input classes, both helpers, both directions and three
binding/body states. It recorded 224 silent missing refusals, 48 correct
refusals and 544 correct legal results. All inputs stayed unchanged. With an
input `prior = c(10,20)`, eager affected sources returned `1,3,4`, immutable
dtplyr sources `10,20,30`, and all reverse cases `10,20,30`. The final matrix
included that input column; an earlier no-column run had external lookup
errors and was not counted as silent-value evidence. The coordinator's
66-call replay independently confirmed eight wrappers and their controls.

The static callable reader discarded the returned definition, so later
invocation could not revisit its changed scope. Known-list member carriers
lost the corresponding member proof. Existing direct/converter tests and
#1545's seven repairs remained passing controls. #1491 covered two converters;
#1513 covered definition-member mutation; #1430 named three unchanged-list
transfers, not these callbacks. Caller replacements stayed ordinary calls:
correctly scoped eager/dtplyr controls ran only at summary execution. An
initial dtplyr root-environment expectation was rejected after ordinary
same-grain dtplyr reproduced that environment behavior.

## Refuted candidates and finite coverage

| Hypothesis | Execution and disposition | Remaining boundary |
| --- | --- | --- |
| Ordinary numeric Total shares mix fixed keys, missing keys or occurrences | The coordinator's 1,200 explicit-set comparisons over 100 independently generated inputs, three families, fixed/unfixed keys and direct/across sources all matched. | Finite local family; arbitrary extension classes were not inferred. |
| Extreme DuckDB numeric measures break ratio conversion | B's 48 signed/unsigned/decimal comparisons matched ordinary summaries and independent ratios. BIGNUM failed at ordinary tbl creation. | The unsupported ordinary BIGNUM input was excluded. |
| Empty selections inside deep products change expanded sets or root admission | B's 300 comparisons covered 180 plans across duplicate policies and 120 share-value/refusal cases; all matched explicit expansion. | The initial oracle ordered dimensions before expansion rather than by first expanded occurrence; canonicalizing that order removed the harness discrepancy. |
| Other special keys expose new support failures | B's 208-case key/empty/duplicate matrix separated 146 successes, 48 upstream refusals, 12 further classified refusals and the two complex-key cases above. | Eight dtplyr complex refusals also occurred without shares; four local raw typed-missing refusals matched the public contract. |
| Structured keys corrupt general denominator matching | Corrected C probes passed list, packed-frame, matrix, integer64, POSIXlt and factor-NA-level keys. | The initial `[i]` harness selected components instead of rows; `vec_slice()` removed that artifact. |
| A zero-row across carrier or nested selection changes ordinary values | A's factory controls, 12 nested/default/computed-head selection comparisons and six numeric-source type checks matched ordinary dplyr or required type refusals. | No new defect; arbitrary caller programs were not proven. |
| An equal-endpoint SQL frame is another package refusal | dbplyr itself rejected `window_frame(0,0)` during rendering. | Excluded; valid unequal bounds established the reported failure. |

The frame matrix's initial ordinary comparator omitted fixed-key sorting and
failed after its ratio comparisons. The final sorted replay completed and
confirmed source immutability. All three coordinator desired-contract commands
returned terminal exit 1 on the investigated SHA for the actual expected
ratio, row-count or refusal assertion. Observation-mode commands returned
zero while asserting the demonstrated defects and controls; they were not
successful package-check results.

## Tracker reconciliation, artifacts and limits

Duplicate discovery exported all-state bodies for 802 Issues and 750 PRs,
then read relevant full threads and repair PRs. Live searches included
`window_frame`, `window frame`, denominator/frame, share/window,
complex/NaN, suppressWarnings, tryCatch/closure, closure/handler,
closure/attribute and closure/local. Selected scope comparisons included
#130, #1401, #1430, #1491, #1513, #1545, #919/#939, #1147 and #769/#770.
The latest Issue list and open-PR inventory were checked before publication;
no matching report or pending repair was found. All three published Issues
were fetched back for body, state and label verification.

Raw scripts, logs, CSVs, SQL, session information, reading-coverage records and
tracker snapshots stayed in temporary investigation storage under
`/private/tmp/marginplyr-summary-r2-evidence/{root,a,b,c}` and its parent.
The Issue bodies and this note retained the essential public witnesses.
All reproduction executions and owned work were collected before submission
preparation. No complete suite, package build or rendering was used to find
these defects; the two explicitly selected existing test files were coverage
controls, not replacement gates.

Environment: R 4.6.1 on arm64 macOS Tahoe 26.6.2, C.UTF-8, Asia/Tokyo;
marginplyr 0.1.0 from the pinned source; dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1, data.table 1.18.6.1,
dtplyr 1.3.3, DBI 1.3.0, RSQLite 3.53.3 and DuckDB 1.5.5.

Live PostgreSQL/MySQL/MSSQL/Snowflake, hosted data, other platforms or
dependency versions, Arrow's full expression matrix, direct materialization,
interrupt/recovery and concurrency were not established. Further claims
would need isolated engine/version-specific executions and independent
contract assertions. Arbitrary reflection, dispatch, callbacks and unbounded
Grouping plans were not proven. No claim of exhaustive correctness followed
from these finite comparisons.

This submission added only a dated research note for investigation readers.
It changed no package execution, test, dependency, generation, installed-file,
build-exclusion or verification behavior. Those effects justified source
scope and no NEWS entry. The PR's separate pinned verification and review
records owned submission readiness; package build/check, complete suite and
snapshots were omitted. CI monitoring was outside the requested work.
