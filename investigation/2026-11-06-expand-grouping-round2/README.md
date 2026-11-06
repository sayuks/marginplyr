# Expansion and Grouping specification second deep boundary hunt

Investigated: 2026-11-06
Baseline: `9ffb24614a71512170298a3827bc11c80354e742` (freshly fetched main)
Scope: dated investigation evidence and implementation tickets; no fixes

A new worktree from main divided the investigation across mixed local
structured dimensions, Grouping specification capture and family semantics,
deferred Arrow/dtplyr sources, and live SQLite/DuckDB input graphs. The recent
November 5 greedy/adversarial notes and November 6
[boundary hunt](../expand-grouping-bughunt-2026-11-06.md) and
[deep hunt](../expand-grouping-deep-bughunt-2026-11-06.md) supplied earlier
coverage and exclusions. Their measurements were not added to this hunt's
counts. The baseline already contained the #1383/#1384 repairs.

## Findings and ticket boundaries

| Finding | Disposition | Independently verifiable behavior |
| --- | --- | --- |
| S1 | New [#1398](https://github.com/sayuks/marginplyr/issues/1398), P2, `bug`, `ready-for-agent` | Preserve Boolean-word public columns when expanding an ordinary derived SQLite input, including the default identity expansion. |
| G1 | Existing open [#1396](https://github.com/sayuks/marginplyr/issues/1396) independently confirmed | Preserve an authored namespace call around a bound nested Grouping specification. |

`to-tickets` produced one independent implementation slice with no blockers or
prerequisite refactoring. The user delegated ticket granularity and publication,
so the breakdown quiz was omitted. No implementation was attempted. G1 remained
attached to its existing report rather than becoming another ticket.

### S1: a source-type anchor changed a valid derived input's public names

A one-row live SQLite source held `g = "a"` and integer `TRUE = 7`. An ordinary
window mutation added `z = row_number(g)`. Collection, selection of every
column and no-op mutation returned the expected three public columns. Calling
`expand_with_margins(source)` with every remaining argument omitted constructed
successfully but direct collection raised `undefined columns selected`.

Executing its generated SQL directly returned `g`, `column2`, `z`. The first
zero-row type-anchor arm wrapped the window source through a star projection,
which changed the Boolean-word alias. The following branch carried the public
alias correctly, but the compound result took its schema from the anchor.
This isolated the package-generated public-schema boundary rather than requiring
a general repair of ordinary dbplyr's SQLite alias behavior. No multiple-set
source-sharing CTE, omitted dimension, identifier, Margin order or explicit CTE
option was necessary.

The [SQL lane](sql/README.md) retained the minimal replay, raw SQL and three
ranked causal hypotheses with separating controls. Its focused identity matrix
recorded 48 failures and 42 exact matches across five names, three source sizes
and six input shapes; ordinary source/projection/no-op controls all passed.
Root and constant-mutation inputs, ordinary names and explicit early aliases
separated working paths. The broader source-graph sweep found 132 candidate
errors, but some ordinary mutation/union controls also failed upstream; those
were not promoted to 132 package-specific defects.

The coordinator's [independent confirmation](independent/README.md) used a
separate archive and changed the source to three rows, mixed-case `FaLsE`, a
different key, different payloads and a missing key. Source, projection,
no-op mutation, unmodified-root expansion, safe-name expansion and an explicitly
stored-source copy all passed. Default expansion still failed. Both primary and
independent desired-contract assertion modes exited one after successful
controls. These were evidence of an unfixed defect, not package regression
tests added to this branch.

ADR 0016 and ADR 0031 supplied the direct public-schema/type contract. The
existing #826/#1095/#1136/#1180/#1349 reports and their repair discussions covered
summary output, downstream Margin-result operations, CTE collection or
materialization. This witness entered the first Margin operation with an
ordinary derived input. No new data-read or materialization permission followed
from the finding.

### G1: the known authored-namespace boundary remained open

The publication refresh identified #1396. Its full body and empty comments were
read. A [new independent witness](spec/known-1396.R) used a bound nested
specification and a caller-authored public namespace operator. Direct evaluation
selected `b` and invoked that lookup once; Grouping inspection selected `a` and
invoked it zero times. Private qualification and genuine base-operator controls
passed. The assertion mode failed as expected. The earlier forwarding and
retained-call matrices used genuine operators and did not establish this
caller-authored-operator behavior.

## Additional measured coverage and exclusions

| Area | Final observations | Result and scope |
| --- | ---: | --- |
| [Mixed local dimension types and names](local/README.md) | 3,630 | All exact row-copy/schema/type/order and input-immutability comparisons passed. Two eleven-type dimensions, five name pairs, distinct/shared fixed-key layouts, three Margin orders. |
| [Forwarded Grouping selections](spec/README.md) | 360 | All matched ordinary typed tidyselect through composed genuine parenthesis/callable forms. |
| Retained constructor-shaped ordinary calls | 400 | All selected the expected column, evaluated once and left ignored arguments unforced. |
| Composite-family enumeration | 5,284 policy comparisons | 2,642 bounded specifications matched exact families/order under keep/drop; 303 generated top-level selectors and 55 oversized fixtures were excluded. |
| [Arrow physical order](lazy/README.md) | 768 | All source-row-origin sequences matched independently known keys, across eight physical types, four source containers, four families, first/last and three consumption routes. |
| Supplemental Arrow-native values/types | 256 | Direct Arrow tables matched exact source-based values, typed missingness, physical types, schema and row multiplicities; int64/decimal values never crossed R double storage. |
| dtplyr random membership/source ownership | 297 | 270 admitted comparisons matched values/schema/multiplicity/evaluation counts and preserved caller tables; 27 existing safe metadata refusals. |
| Downstream Arrow composition | 117 | 69 public-value/schema matches; 48 failures also occurred in ordinary Arrow controls. |
| [Live SQL graph sweep](sql/README.md) | 1,728 | 1,488 matches, 132 S1 candidates, 108 ordinary-source failures. The focused identity witness supplied the stronger defect isolation. |
| SQL rendering options | 928 executed comparisons | All matched; twelve source/order/label combinations already failing the default S1 path were excluded before the options cross-product. |
| SQL volatile payload copies | 96 | 88 observed payload-copy matches and eight S1 failures; value observations did not prove a universal expression-evaluation count. |

The Arrow order oracle compared returned source-row identifiers and occurrence
identifiers against source-value ranks. The separate exact-value supplement
checked actual Arrow columns with native type/value equality, using source
`Take()` indices with missing indices for omitted dimensions. Its exact-value
coverage concerned direct Arrow tables; compute/reader conversion routes did
not receive that independent supplement. No finite matrix established arbitrary
extension classes, every syntax or unbounded resource behavior.

All lane notes recorded harness corrections separately from final completed
runs. Preliminary oracle mistakes were not package findings or additional
passing observations. A forged-specification lead was rejected under ADR 0015's
explicit documented-interface boundary. Computed-sort errors in downstream
Arrow operations also occurred without marginplyr. An ordinary Arrow rank
operation emitted eighteen absorbing-fallback warnings across the actual and
control executions; they arose in the caller's later ordinary operation.

## Main freshness and duplicate checks

Main was fetched at worktree creation and again before independent confirmation
and ticket publication; both fetches named the baseline SHA. The coordinator
searched a 702-issue all-state snapshot and 300 recent all-state PRs, read the
relevant complete report and repair discussions, and ran live all-state search
queries covering issues and PRs together:

- `repo:sayuks/marginplyr Boolean source`: 34 matches, including the prior
  Boolean-name/input-composition/materialization work; no matching ordinary
  derived-input identity report.
- `repo:sayuks/marginplyr "no such column" "expand"`: five matches, concerning
  earlier summary/output and downstream-result paths.
- `repo:sayuks/marginplyr "anchor" "derived"`: eight matches, including
  direct-CTE and destination/source-binding repairs.

Complete #826/#928/#1095/#1136/#1180/#1349 report discussions and PR
#1134/#1194 discussions distinguished those boundaries. A recent twenty-issue
refresh before publication supplied the existing open #1396 confirmation and
found no S1 duplicate. The new ticket was published in English with explicit
acceptance criteria and no blocking edges.

## Environment, replay and submission effects

The evidence bundle retained the standalone scripts, final CSVs, diagnostics and
per-lane replay commands. All owned executions were collected through terminal
completion. Failed expected-contract assertions were explicitly identified as
defect confirmations. Captured text was normalized only for trailing whitespace
and final blank lines where needed.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1,
Arrow 25.0.1, dtplyr 1.3.3, data.table 1.18.6.1, RSQLite 3.53.3 and
DuckDB 1.5.5. Hosted stores, live PostgreSQL/SQL Server, other platforms and
dependency versions, arbitrary caller extensions and unlimited plans were
outside the measured scope.

Only dated standalone evidence under the existing investigation exclusion was
added. Package code/tests, examples, vignettes, installed files, generated
documentation, dependencies, build rules and check tooling were unchanged.
No package or verification consumer invoked the new probes. These content and
consumer effects justified Review-ready `source` scope. The cumulative baseline
was the latest verifiable full-checked ancestor
`9a61fe6c458e6b35a6d1f6402ce14150565b1507`, recorded in
[PR #1394](https://github.com/sayuks/marginplyr/pull/1394#issuecomment-6683753539).
From that ancestor to fetched main, only the measured test-priority field had
changed; that adoption's source validation was recorded there. All package code,
test/helper bytes and membership, installed files, dependencies and dispatch
machinery were unchanged. The new branch added only this evidence. Package
build/check, complete suite and snapshots were
omitted. A NEWS entry was unnecessary for research without a behavior change.
The PR held the pinned verification, compatibility and Standards/Spec review
records. CI was not monitored.
