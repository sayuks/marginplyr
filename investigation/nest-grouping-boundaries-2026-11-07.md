# Nesting and Grouping specification evaluation boundaries

Investigated: 2026-11-07
Tested main: `db3a7e4f5215574a71a9af60698d0c945af0cb83`

The worktree started from freshly fetched main on
`investigation/nest-grouping-boundaries-20261107`. Parallel lanes covered local
Nest membership and payloads, Grouping specification capture and selection,
and Arrow and SQL metadata. The coordinating lane tested deferred dtplyr
expressions and replayed both new findings against a separate Git archive.

The earlier [registry investigation][registry], [deep nesting investigation][deep]
and [receiver-dispatch investigation][receiver] supplied prior controls. The
tested SHA included the repairs in PR #1438. The counts below came from fresh
runs.

## Findings

| Result | Tracker disposition |
| --- | --- |
| An omitted dtplyr dimension's generated identity split the Grand total into three one-row cells. | [#1456][identity] |
| A qualified parenthesis call around a bare Grouping constructor callee lost the active selection context. | [#1457][callee] |
| Explicit dtplyr Margin label checks admitted a real collision, and a separate omitted-label operation changed the requested display. | Independent evidence for existing [#1452][label] |

The two new tickets were published with `bug` and `ready-for-agent` under
`to-tickets`. Both are independently verifiable and have no blockers. The
user delegated the ticket split. This branch adds dated evidence and
standalone probes.

### Omitted dimension identity in dtplyr nesting

With the source keys east, east, west and payloads 17, 34, 51, a rollup with
`.margin_label = NULL` and a Grouping set identifier returned one Grand total
cell with all three values under ordinary lookup. A selective root namespace
binding for the generated `vctrs::vec_rep` returned distinct strings. Explicit
collection of an already built `nest_with_margins()` query and the immediate
collection inside `nest_by_with_margins()` each returned three Grand total
cells. All three retained set identifier 2 and a missing outer key. Each
held only one source row.

The control included-only plan and native collection made no selective
lookup. The source stayed byte-identical and removing the binding restored
the original deferred result. The included and omitted paths distinguished
a change to the displayed key, a change in the cell converter and a change to
the fold identity. The failing lookup was in `expand_margin_union()`, which
generated a row-sized private identity for each omitted dimension before
folding. The earlier #1427 repair covered the cell constructor, displayed
representative and included NA-factor identity encoding. The public label in
#1452 and the zero-row identifier in #1453 were separate operations.

[The replay][identity-probe] asserted the dated result in `observed` mode
(exit 0). Its `contract` mode demanded one Grand total cell per verb and zero
generated lookups (exit 1 after the controls).

### Grouping constructor callee and typed selection context

A captured caller bound bare `rollup` to a selector returning the integer
column `units`. The written callee wrapped that bare name with the explicitly
qualified base parenthesis function. The authored namespace operation read
the active data with `tidyselect::peek_data()`. The data had a single units
value and two distinct regions.

Direct typed tidyselect selected units. A fallback implementation of the
authored operation selected region when Grouping preflight ran outside the
selection context. Both Nest verbs returned two cells where the literal
units control returned one. A strict implementation raised
`Selection helpers must be used within a *selecting* function.` Inspection,
expansion and summarization reproduced both outcomes. Across three spellings
and five consumers there were 15 wrong results and 15 strict errors. Ten
qualified-inner controls and eight direct typed selection controls passed.
Every authored namespace lookup occurred once and the source bytes matched.

The comparisons distinguished lost caller lookup, repeated evaluation and
wrong evaluation context. Only the third fit the observation. The
recognition step in `read_grouping_arg()` looked through the explicit
parenthesis callee to the bare constructor. The subsequent decision about
the authored qualifier lost that outer call when selecting an evaluation
path. ADR 0008's authored-namespace amendment supplied the selection-context
contract. #1288 established support and a one-reading budget for the
written spelling. #1396 and PR #1412 addressed related authored wrappers.
Open #1439 concerned receiver methods and #1449 concerned selection output
names.

[The replay][callee-probe] asserted 30 violations in `observed` mode
(exit 0). Its `contract` mode demanded none (exit 1).

### Independent checks of the existing label report

The [label replay][label-probe] reproduced both reported operations through
all four Margin verbs. With a source value of `Total` and
`.check_margin_label = TRUE`, selective lookup of any one of the generated
base character conversion, vector conversion or aggregate changed a
Package condition into admission. All 12 such comparisons invoked the
selected callback once. The four `dplyr::if_else` controls invoked it zero
times and kept the refusal. Ordinary lookup, native collection, explicit
opt-out and source-byte controls passed.

On a separate rollup with distinct keys and no collision, the generated
repetition helper changed `Total` to `CHANGED` in all four verbs. For each
lazy result, the binding was installed after construction and removing it
restored the same query. The row-wise verb was called with the binding in
place. The native and included-only controls stayed correct. Both observation
modes exited 0. Their zero-callback contract modes exited 1.

#1452 was already open when publication-time search returned it. Its
collision-count and omitted-label witnesses cover these results. #1143 and
PR #1168 addressed bare helpers, and #1433 and PR #1438 addressed included
display and factor encoding and reconstruction.

## Finite comparisons and limits

| Probe | Result |
| --- | --- |
| [Local Nest membership, projection and order][local-grid] | 9,600 calls across ten dimension types, two sizes, four carriers, five plans, two labels, both keep modes, three orders and both verbs. The 5,832 admitted results matched. The other 3,768 had documented input or label refusals. |
| [Local empty and zero-column shapes][local-empty] | All 1,512 calls matched independent source membership and cell shape, including empty and populated zero-column cells, all columns used as keys, a list-column name equal to a payload name, and explicit or duplicate Grand total sets. |
| [Local named labels][local-collisions] | 384 paired checks (768 calls) with reversed named-label order. There were 432 matching admitted results and 336 expected observed or declared collision refusals, including factor NA-level cases. |
| [dtplyr names][dtplyr-names] | 860 calls compared 43 spellings in five roles across both keep modes and verbs. There were 830 matches, 24 upstream input name refusals and six documented row-wise identifier refusals. |
| Grouping plan algebra | Three existing independent oracles with seed changed to `71119L` each matched 1,500 generated cases. They covered set algebra, duplicate dropping and the complete inspection plan. |
| [Ordinary lazy selections][lazy-selection] | 405 Arrow, 513 DuckDB and 171 SQLite exact matches against native metadata controls, plus 342 expected SQLite predicate refusals. Six Arrow graph variants failed in native construction or execution and were excluded. |
| [One-shot Arrow readers][arrow-readers] | All 180 inspections matched the selection oracle and retained the original reader rows for a subsequent native read. |

The local oracle enumerated source groups and sliced original columns,
checked outer keys and factor codes and levels, and modeled the Margin order
from fixed keys, Grouping bits, missingness, values and set identifiers.
The local membership refusal counts were 2,400 unsupported row-wise inputs,
720 packed or zero-width display labels, 360 matrix display labels and 288
typed-missing raw keys with an omitted dimension. The label oracle checked
both named orders and source ownership separately.

An ordinary Arrow `right_join(keep = FALSE)` with differently named join keys
and a same-named left non-key exposed a schema mismatch. Arrow declared the
right integer key as string while its native collection returned an integer
column. Arrow's own lazy typed selection and marginplyr agreed with the
declared schema. The [minimal replay][arrow-join] also checked literal names
and `keep = TRUE`. The evidence was classified as an upstream schema
inconsistency under the documented Arrow metadata contract.

SQLite and DuckDB inspection controls used quoted, separator-containing and
non-ASCII names. Name-only selection and invalid format had zero owned SQL
records. A SQLite type predicate had a Package refusal with no owned SQL.
DuckDB typed selection had one `selection_proxy` with `LIMIT 0`. Native
collection of a separate derived value raised its runtime cast or overflow
error where inspection did not.

These finite checks used arm64 macOS, R 4.6.1, dplyr 1.2.1, tidyr 1.3.2,
dtplyr 1.3.3, data.table 1.18.6.1, rlang 1.3.0, tidyselect 1.2.1 and
vctrs 0.7.3. The lazy runs also used Arrow 25.0.1, DBI 1.3.0,
dbplyr 2.6.0, DuckDB 1.5.5 and RSQLite 3.53.3. Hosted backends, other
versions and operating systems, and unbounded specifications and source
graphs were outside the measured corpus.

## Replay and duplicate search

From the repository root, point the three finding probes to a source copy of
the tested SHA. Use `contract` in place of `observed` or append it to the label
commands to check the intended behavior. The remaining probes take the tested
source as their working directory. Redirect the local matrix reports to
scratch files. The names probe takes the source root and an output CSV path.

```sh
probes=investigation/2026-11-07-nest-grouping-boundaries
export TESTTHAT_PARALLEL=false
Rscript "$probes/dtplyr-identity.R" /path/to/tested-source observed
Rscript "$probes/grouping-callee.R" /path/to/tested-source observed
Rscript "$probes/dtplyr-label.R" /path/to/tested-source collision
Rscript "$probes/dtplyr-label.R" /path/to/tested-source display
```

The [prior algebra lane][algebra] holds the three scripts and output overrides.
The fresh run changed only their seeds and wrote all outputs outside the
worktree. The first archive attempt stopped before execution because those
historical scripts required Git to report the source SHA. The retained run
read the same pinned worktree directly.

All-state issue and PR searches included `"Grand total" "vec_rep"`,
`"namespace" "observed" "collision"`, `"namespace" "omitted" "label"`,
`"callee" "selection context"` and `"parenthesis" "peek_data"`. Relevant
bodies and comments included #1288, #1396, #1427, #1433, #1449, #1452 and #1453.
The publication-time fetch retained the tested SHA. Both created issues were
fetched and their state, titles and labels checked.

[registry]: nest-spec-bughunt-2026-11-07.md
[deep]: nest-spec-deep-bughunt-2026-11-07.md
[receiver]: expand-grouping-receiver-dispatch-bughunt-2026-11-07.md
[identity]: https://github.com/sayuks/marginplyr/issues/1456
[callee]: https://github.com/sayuks/marginplyr/issues/1457
[label]: https://github.com/sayuks/marginplyr/issues/1452
[identity-probe]: 2026-11-07-nest-grouping-boundaries/dtplyr-identity.R
[callee-probe]: 2026-11-07-nest-grouping-boundaries/grouping-callee.R
[label-probe]: 2026-11-07-nest-grouping-boundaries/dtplyr-label.R
[local-grid]: 2026-11-07-nest-grouping-boundaries/local-grid.R
[local-empty]: 2026-11-07-nest-grouping-boundaries/local-empty.R
[local-collisions]: 2026-11-07-nest-grouping-boundaries/local-collisions.R
[dtplyr-names]: 2026-11-07-nest-grouping-boundaries/dtplyr-names.R
[lazy-selection]: 2026-11-07-nest-grouping-boundaries/lazy-selection.R
[arrow-readers]: 2026-11-07-nest-grouping-boundaries/arrow-readers.R
[arrow-join]: 2026-11-07-nest-grouping-boundaries/arrow-join-schema.R
[algebra]: 2026-11-07-nest-spec-bughunt/spec/README.md
