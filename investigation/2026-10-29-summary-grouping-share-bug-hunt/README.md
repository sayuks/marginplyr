# Summarize, Grouping specification, and Total share bug hunt

Investigated: 2026-10-29
Repository snapshot: `58356a67a79d1d2961eac6b8865d30afdb751d5c`

The investigation started from `main` in a new managed worktree on branch
`investigation/margins-bug-hunt-2026-10-29`. Local `main` and `origin/main`
resolved to the snapshot above. Four investigators divided ordinary summaries,
Grouping specifications, contextual shares, and cross-backend behavior. Package
implementation, package tests, generated documentation, and metadata were not
changed. The diagnosis workflow stopped at confirmed causes and executable
repair assertions because the user expressly excluded implementation.

Six independently actionable findings were published through `to-tickets` with
`bug` and `ready-for-agent`. All had no blockers. The user delegated splitting
and publication, so the skill's interactive granularity checkpoint was omitted.
The tickets own repairs; this note records evidence at the investigated date.

| Finding | Ticket | Priority | Observed effect |
| --- | --- | --- | --- |
| F1 | [#969](https://github.com/sayuks/marginplyr/issues/969) | P2 | A named `rowwise(data = ...)` input escaped enclosing helper rewriting |
| F2 | [#970](https://github.com/sayuks/marginplyr/issues/970) | P2 | Portable summaries misread `.`, `.da`, and `.dat` outputs as input arguments |
| F3 | [#971](https://github.com/sayuks/marginplyr/issues/971) | P2 | Generated SQL expressions rejected valid dot-dot outputs and identifiers |
| F4 | [#972](https://github.com/sayuks/marginplyr/issues/972) | P2 | Parenthesized ordinary `across()` functions lost share-source eligibility |
| F5 | [#973](https://github.com/sayuks/marginplyr/issues/973) | P2 | Accepted DuckDB structured keys failed when the report selected portable execution |
| F6 | [#974](https://github.com/sayuks/marginplyr/issues/974) | P3 | The public summary reference falsely promised equal duplicate Grand total values |

## F1: naming a nested rowwise input changed the enclosing selection

One row, `tibble(g = "a", x = 1)`, and one grouping set containing g sufficed.
An ordinary grouped dplyr summary of
`ncol(dplyr::rowwise(data = dplyr::pick(everything())))` returned width one.
The Margin summary returned two. Passing rowwise's input positionally returned
one. The rollup variant exposed additional private columns in its Grand total
branch; putting `grouping_bit(g)` in the named input also failed while the
positional control returned the expected bits.

The ranked hypotheses were a wrong registered input formal, a named/positional
difference in dplyr itself, and a broader summary-wrapper effect. The dplyr
named-input and Margin positional controls ruled out the latter two as the
distinguishing cause. `input-formals.R` compared 30 registered operation
spellings with actual function signatures: only rowwise differed. Its formal
was `data`, while `static_spelling_rules()` registered `.data`.
`independent_mask_call_parts()` therefore marked the entire named input as
owned by the nested operation. ADR 0019's independent-mask amendment instead
assigns that input to the enclosing operation.

`rowwise-scope-matrix.R` recorded 36 affected named-input cases and nine passing
positional controls, across tibble/data.frame/data.table inputs, fixed keys,
one variable grouping set, and rollups. Exact `data` and valid partial names
`d`, `da`, and `dat` all reproduced. This finding concerns ownership of an
input subtree, not a request to rewrite rowwise's independently owned body.

## F2: dynamic outputs became partial matches of a backend source argument

A one-row source containing `v = 1` and one dynamically named `sum(v)` sufficed.
Ordinary dplyr returned outputs named `.`, `.da`, or `.dat`. The Margin call
failed on dtplyr, SQLite, and forced-portable DuckDB; DuckDB native succeeded.
No grouping dimension, label, share, or sort request was required. A requested
identifier with `.duplicates = "keep"` selected the affected DuckDB path.

The hypotheses were early argument-name injection, unsupported backend output
names, and a source-class/plan restriction. `dot-output-boundary.R` held the
backend call constant and varied only its `rlang::inject()` wrapper. Direct
dynamic splicing succeeded; early injection reproduced the failures. The
source output names were therefore supported. The injected named arguments
partially captured a backend helper's `.data` formal when its actual source
was forwarded positionally. `summarize_margin_branch()` already protected an
exact `.data` output but did not extend that protection to these partial names.

The retained red script observed nine affected name/backend combinations and
three successful native controls. An additional five-backend, three-plan,
three-sort sweep of `.` produced 18 affected dtplyr/SQLite cases and 27 passing
local/Arrow/DuckDB-native cases. The `.d` output was excluded because the
package deliberately refused it as an option near miss. ADR 0007's capture
boundary and the summary dots' dplyr contract supplied the admission basis.

## F3: package-generated SQL reads reinterpreted valid dot names

Ordinary SQLite and DuckDB summaries accepted `..0` and returned a scalar one.
Adding the Margin verb with `.sort = "last"` failed at SQL rendering with
`indexing '...' with non-positive index 0`. SQLite also failed without sorting;
DuckDB's unordered native output succeeded. A 96-case focused matrix covered
`..0`, `..00`, `..01`, `..-1`, output/identifier roles, both databases, all sort
arguments, and no-dimension/rollup plans. It observed 88 failures, with a
successful ordinary output or ordinary identifier-construction control in
every case. This excludes special input-key names that ordinary lazy grouping
itself could not process.

The hypotheses were package-generated alias reads, unsupported SQL identifiers,
and backend aggregate failure. Successful ordinary summaries ruled out the
last two for these fixtures. A process-local trace showed SQLite's aggregate
preservation conditional translated to an `if_else()` reading the output as a
bare dot name. The ordered DuckDB trace reached another generated projection
after `drop_margin_staged_identifier()`. Both reinterpreted a SQL identifier as
an R dot argument. The trace processes exited without persisting instrumentation.
Earlier #827/#898 repaired local paths and #922 repaired Arrow; their upstream
positive-dot restrictions did not answer these accepted SQL-output fixtures.

## F4: a statically single across function became an unknown share source

With g equal to a,b and x equal to 1,3, the ordinary summary
`dplyr::across(x, (~sum(.x)), .names = "s")` produced s equal to 1,3,4 across a
rollup. Adding `share_of_total(s)` raised the Package condition
`Total share \`share\` refers to unknown preceding ordinary summary \`s\`.`
Removing only the function's redundant parentheses produced 0.25,0.75,1.

The hypotheses were failed static name prediction, backend inability to run the
parenthesized function, and an ineligible source type/cardinality. The ordinary
source and unparenthesized-share controls ruled out the latter two for the
minimal fixture. `known_across_function_names()` returned `"1"` for the bare
formula and no names for its parenthesized form, so output prediction also
lost the literal s. The eligible preceding-across-output rule in ADR 0010 and
the share reference admitted this scalar source. Recognizing its function count
does not require executing a factory or erasing an across-call evaluation
boundary governed by ADR 0019.

The 480-request matrix recorded 120 successful-ordinary-control cases rejected
when a share was added: local 32, eager data.table 32, SQLite 32, DuckDB 16,
dtplyr eight. It covered bound sum/formula/function-literal variants, one/two
parenthesis layers, qualified across, literal/column-derived names, and both
share helpers. Unsupported backend spellings were excluded from the finding.
Parenthesized function literals were passing controls, not claimed failures.
Twelve additional direct/column-wise observations had four passing bare-formula
controls and eight false parenthesized-source refusals.

## F5: duplicate policy exposed a structured missing-value translation bug

Two-row DuckDB STRUCT and INTEGER[2] dimensions worked in ordinary grouping and
native Margin rollups with `.margin_label = NULL` and `.id = "set"`. Changing
only `.duplicates` from `"error"` to `"keep"` failed, despite there being no
repeated sets in the plan. STRUCT failed during omitted-branch construction
with `g = <df[,1]>` and `Unknown input type: list`; ARRAY collection rejected a
STRUCT-like `(NULL, NULL)` literal as an invalid cast to INTEGER[2].

The hypotheses were a schema-derived R value injected as SQL, database inability
to group/union structured keys, and a general occurrence-counting problem.
Ordinary grouping and explicit unions using `NULL::STRUCT(a INTEGER)` and
`NULL::INTEGER[2]` succeeded; a scalar portable control also succeeded. These
controls supported the first hypothesis. `margin_column_info()` chose its
R data-frame/array `vec_init()` arm before the DuckDB SQL-NULL arm.
`label_margin_branch()` then injected that prototype into the omitted branch.
The independent structured-key replay ran both affected types and the scalar
control before its final desired-behavior failure.

The scope is parity for already accepted DuckDB reports when an identifier and
keep policy select portable execution. The public reference described that
adapter as having the same semantics and duplicate policy as controlling set
occurrences. ADR 0016's local-packed amendment and earlier scalar ticket #836
added no general remote structured-key promise; they did not require an already
accepted native report to become invalid under keep. No broad remote-type or
nonmissing structured-label support was requested by this ticket.

## F6: duplicate Grand total values were documented as necessarily equal

A stateful eligible scalar summary returned 1,2 on detail rows and 3,4 on two
retained Grand total occurrences. Their Total shares were both one, and detail
shares used a displayed denominator as required. The public summary reference
still said the duplicate occurrences held the same values and were
interchangeable. `total_set_ids()`'s explanatory comment retained that premise.
ADR 0017's amendment for #919 expressly withdrew it for stateful and
nondeterministic sources. The capture confirmed a documentation defect; it did
not identify another denominator-calculation defect or justify a purity rule.

## Executable evidence and coverage

Run from the checkout root in fresh R processes. Each retained script accepts
an optional first argument naming a checkout to load; its default is the
working directory. These six commands were independently replayed by the
coordinating investigator and each exited **1** on the pinned baseline:

```sh
Rscript investigation/2026-10-29-summary-grouping-share-bug-hunt/summary/reproduce-rowwise-input.R
Rscript investigation/2026-10-29-summary-grouping-share-bug-hunt/summary/reproduce-partial-source-output.R
Rscript investigation/2026-10-29-summary-grouping-share-bug-hunt/backend/repro-sql-special-outputs.R
Rscript investigation/2026-10-29-summary-grouping-share-bug-hunt/share/repro-parenthesized-fns.R
Rscript investigation/2026-10-29-summary-grouping-share-bug-hunt/grouping/duckdb-structured-portable-red.R
Rscript investigation/2026-10-29-summary-grouping-share-bug-hunt/share/volatile-duplicate-totals.R
```

These are intentional desired-behavior assertions, not passing package tests.
Adjacent logs retain actual outputs. A repaired implementation/documentation
should make them pass; no repair was applied or tested. Exploratory scripts
instead report both supported results and refusals; an exit zero in such a
script is not a claim that every observation passed. The matrix CSV retains
individual ordinary-control/share outcomes. Binary debug dumps were omitted,
and trailing whitespace was removed from text captures.

| Retained or referenced harness | Observations at the snapshot |
| --- | --- |
| Prior `2026-10-28-nesting-grouping-bug-hunt/plan-algebra.R` | 2,000 keep/drop comparisons passed |
| `grouping/typed-plan-oracle.R` | 1,354 independent typed-selection/plan-order comparisons passed; 46 explicit occurrence-bound skips |
| `grouping/constructor-result-differential.R` | 425 masked-constructor/parent-kind/return-value comparisons passed; exactly one call evaluation |
| `grouping/dtplyr-schema-differential.R` | 656 typed-plan comparisons passed; 152 documented unsafe-metadata refusals counted separately |
| `summary/backend-expressions.R` | 432 observations: 342 matching results, 75 paired refusals, 15 expected Arrow absorption refusals |
| `summary/rowwise-scope-matrix.R` | 36 affected named inputs and nine passing positional controls |
| `backend/special-names-wide.R` | 688 calls: 12 source/oracle refusals and 37 raw discrepancies, subsequently split into upstream key restrictions and F2/F3 |
| `backend/dotdot-scope.R` | 96 accepted-name cases; 88 failures and eight passing unordered DuckDB outputs |
| `backend/duckdb-native-portable.R` | 204 cases: 180 matching results, 12 paired structured-label refusals, 12 F5 discrepancies; one timestamptz fixture was refused before the matrix |
| `share/parenthesized-fns-matrix.R` | 480 requests and 240 paired ordinary controls; 120 valid-control false share refusals |
| `share/parenthesized-share-styles.R` | Four bare controls passed; eight parenthesized-source requests were refused |
| `share/key-oracle.R` | 216 local/dtplyr key/role/label/order combinations: 156 matching results, 60 backend/type refusals, no supported-result mismatch |
| `share/late-outputs.R` | 18 runtime output collisions were appropriately refused |
| `share/explore.R` | 24 reflected/direct lookup and 76 type/cardinality observations; no further confirmed defect |
| Prior `2026-10-28-summarize-grouping-share-hunt/share/independent-oracle.R` | All 864 result-relative denominator checks passed on replay |
| Prior `2026-10-28-summarize-grouping-share-hunt/share/empty-types.R` | 32 empty-input/type observations completed without a new defect |

The typed plan oracle used seed 20261029, recursive unions/products,
rollups/cubes/composite dimensions, predicate/negative/range/set selections,
parenthesized ordinary heads, metadata readers, and comma/Japanese names.
It bounded generated plans at 256 occurrences; the skipped cases are explicit,
not counted as passes. The share key oracle used seed 29231 and included
NA/NaN, Unicode, factor NA levels, integer64, semantic classes and structured
local keys. Comparisons respected unspecified row order and did not establish
arbitrary attribute preservation or equality of every backend diagnostic.

No additional Grouping specification construction/inspection bug was confirmed.
Constructor preflight retained its first evaluated ordinary-selection result,
so #954 was not reopened. Bare external-vector active-binding selection differed
from dplyr, but that spelling was deprecated; the modern `all_of()` control read
once and agreed. Computed `identity(get)` lookup lay outside the deliberately
static callee grammar. Unknown dynamic function lists, schema-only lazy
predicates, and ordinary backend list/raw/complex restrictions were not treated
as new defects. Earlier repaired cases were checked against closed tickets.

## Runtime, existing suite, and publication boundary

`versions.txt` records R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3, and supporting
package versions. No live PostgreSQL, Spark, hosted SQL, alternate dependency
version, arbitrary S3 implementation, asynchronous interruption stress, or
unbounded plan was established. Finite coverage does not prove absence of bugs.

The existing focused suite completed with terminal exit zero:

```sh
NOT_CRAN=true Rscript -e 'devtools::test(filter="^(grouping-interface|grouping-plan|summarize-operation|share|share-backends|margin-order|metamorphic-margin-semantics)$", reporter="summary")'
```

`existing-suite.log` retains the output. The seven existing test files passed;
package snapshot files had no diff. The broad sandboxed Arrow name probe printed
CPU-cache sysctl notices, which did not terminate the harness.

All changes were investigation material excluded from the source package by
`.Rbuildignore`: Repository-only under `design/agents/local-checks.md`. The
package-affecting Review-ready check was therefore not required. Package-aware
lintr after `load_all()`, jarl, the context-budget verifier, and the maintained
document-reference verifier passed before publication. The PR records its
review snapshot and outcome. CI monitoring was outside the requested work.
