# Backend nesting and three-input inspection boundary investigation

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

## Outcome and contracts

This track found no additional independently actionable defect. It examined
three-input dtplyr graphs, nested cache aliases and effectful row predicates,
SQL admission/query boundaries, and Arrow graphs containing three independent
one-shot readers. No implementation, package test or dependency changed.

The complete backend notes from October 30, October 31, November 2 and
[November 3](../../2026-11-03-nest-spec-greedy/backend/README.md), and the
November 3 coordinating note, informed the frontier. Their historical counts
were not counted as new executions. The earlier helper ownership findings
were not republished. The `list` masking candidate already failed ordinary
upstream controls in that evidence and was excluded.

`CONTEXT.md`, the nesting and inspection reference, and ADRs 0001, 0002,
0005, 0016, 0018, 0020 and 0029 supplied the relevant contracts: faithful
metadata without unrequested source-row work, one cell per source group,
opt-in Margin order, no writes to caller tables, and explicit collection for
a rowwise result. ADRs 0032 and 0033 supplied adjacent name/type boundaries;
no result restoration or new backend support was inferred from them.

## Completed finite graph oracle

[`graph-frontier.R`](graph-frontier.R) used an independent explicit source
collection followed by local inspection/nesting as its output oracle. It
checked 17 graph shapes at zero, one and four source rows: roots and counted
filters; two- and three-input joins; anti and semi joins; unions over joins and
joins over unions; cached joins; two cache layers separated by rename, order,
or column removal; cached set inputs; repeated mutations; arithmetic;
empty-operand union; and intersection.

Both nesting verbs, both `.keep` values and all three `.sort` values were
compared. Exact order was required for requested Margin order; unspecified
order alone was normalized. Cell classes were normalized to tibbles only for
comparison, not as a proposed API promise. Every cell retained a source row
identifier, and comparison included the outer keys, identifiers, cell contents,
column types and order. Source serializations and callback counts separately
checked isolation and deferred execution.

| Observation | Completed count |
| --- | ---: |
| Input graphs | 51 |
| Matching name-only inspection plans | 51 |
| Matching typed inspection plans | 48 |
| Conservative typed metadata refusals | 3 |
| Matching nesting results | 576 |
| Conservative nesting metadata refusals | 36 |
| Unexpected errors, mismatches, construction callback reads, source mutations or warnings | 0 |

The three typed refusals and 36 nesting refusals were the parenthesized
arithmetic shape, across the three sizes and twelve nesting options per size.
[`refusal-classification.R`](refusal-classification.R) independently confirmed
the metadata-refusal class and diagnostic. The safety grammar did not admit
parentheses within the arithmetic expression; the output oracle did not turn
that conservative admission boundary into a new correctness guarantee.
Name-only inspection still matched. Refusals were not passing output
comparisons.

The initial matrix returned exit 1 after reporting 39 serialization changes.
A minimized [native control](controls.R) established that ordinary dtplyr
`filter(k == 2L)` added a data.table secondary index to its caller table with
automatic indexing enabled. That explicit oracle read, rather than a Margin
call, supplied the changed attribute. The corrected matrix disabled automatic
index creation and returned exit 0 with unchanged source serializations.
Both [the initial output](graph-frontier-initial.txt) and
[the completed output](graph-frontier.txt) were retained. No source-column
value, name or row change was observed. The same control established sensitivity
of the effectful-filter counter: inspection/lazy nesting read it zero times,
while explicit native collection read it once.

An exploratory reordered-column set candidate failed ordinary data.table set
operations because the operands had different column order. It established no
valid input on which marginplyr returned incorrect metadata and was excluded.
A ten-graph pilot agreed with the local oracle but was not added to the final
matrix's counts.

## SQL and three-reader Arrow frontier

[`remote-frontier.R`](remote-frontier.R) constructed six live SQL shapes per
engine: a join over a union, a union over joins, a three-way join, nested
limits/orderings, grouped window computation, and a projected join. Both
nesting refusals left the supplied specification unforced even with collision
checking explicitly requested. Locally invalid format and missing-column
calls sent nothing. Both `DBI::dbSendQuery` and `DBI::dbSendStatement` were
traced after fixture construction.

SQLite completed 36 boundary calls and sent zero statements. Its six typed
inspection requests were refused because that backend reported no typed
schema without a query; these were the documented boundary. DuckDB completed
36 calls and sent twelve statements, all `LIMIT 0`, one for each valid
inspection. Its nesting refusals and locally invalid calls sent none. These
observations did not establish nesting support on either SQL backend.

Six Arrow shapes each held three fresh readers: join over union, union over
join, double join, hidden right-hand join projections, semi join over union,
and anti join over join. Both nesting refusals left their fixed-key argument
unforced. Typed and name-only inspection returned the expected rollup plan.
Across 24 boundary calls, all eighteen original readers subsequently returned
all 36 source rows. Inspection/refusal added no Sent query record. These were
reader-preservation observations, not executed nesting on Arrow.

The initial remote harness wrongly required typed inspection to succeed on
SQLite and returned exit 1 at that documented refusal. The corrected harness
asserted that refusal and its zero-statement boundary; its completed run
returned exit 0. [Initial](remote-frontier-initial.txt) and
[completed](remote-frontier.txt) logs kept those outcomes distinct.

The coordinating agent simplified the grouped-window fixture to
`mutate(position = row_number(), .by = g)` after jarl rejected the explicit
group/mutate/ungroup spelling. A [replay](remote-frontier-lint-replay.txt)
against the same baseline retained all SQL/Arrow counts and assertions above;
session `32214` reached terminal exit 0. Its repeated observations were not
added to the counts.

## Replay, environment and limits

Each completed script loaded a disposable `git archive` copy of the baseline
through `pkgload::load_all()` with testthat parallelism disabled. To replay
from the investigated checkout root, optionally set `MARGINPLYR_HUNT_SOURCE`
to another source copy of that exact revision:

```sh
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/backend/graph-frontier.R
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/backend/remote-frontier.R
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/backend/controls.R
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/backend/refusal-classification.R
```

The final graph invocation retained session `3344` to terminal exit 0; the
final remote invocation retained session `83675` to terminal exit 0. The
initial graph session `63811` returned exit 1 and was collected before its
replacement. Controls/classification completed synchronously at exit 0, as
did collection of the initial remote harness's exit 1. No owned process
remained. These were focused development reproductions, not Review-ready
results or a complete package suite.

The adjacent outputs recorded R 4.6.1 on Darwin 25.6.0 arm64, dplyr 1.2.1,
dtplyr 1.3.3, data.table 1.18.6.1, vctrs 0.7.3, tibble 3.3.1, tidyr 1.3.2,
DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1. No dependency was
installed or changed. Each execution's precise baseline was the SHA above.

The finite probe did not establish every graph, arbitrary caller bindings or
extension classes, nondeterministic sources, other dependencies/OS versions,
hosted databases, performance at scale or freedom from all bugs. Matching
local-oracle output did not independently establish every backend's own
correctness. No new repair ticket was recommended from this track.
