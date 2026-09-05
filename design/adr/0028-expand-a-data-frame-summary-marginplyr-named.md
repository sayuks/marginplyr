# Expand a data-frame summary marginplyr named

An **Assigned summary name** is the column name marginplyr wrote for an unnamed
summary it rewrote, read from the caller's own expression
([ADR 0007](0007-capture-user-expressions-at-public-verbs.md) captured that
expression, and #430 decided that the name comes from it). It is never a name
the caller wrote, it exists only while the query is built, and it is the
subject of this decision.

Where a local result holds a data-frame column under an Assigned summary name,
that column's own columns take its place and the assigned name does not appear.
A name the caller wrote stays packed, exactly as `dplyr::summarize()` packs it.
Every other summary is unaffected.

## Why the decision cannot be made from the spelling

[ADR 0019](0019-resolve-contextual-helper-names-by-static-spelling.md) resolves
a Contextual helper by how it is spelled, and every rewrite marginplyr performs
is decided that way. This question is not one a spelling can answer.

dplyr expands a data-frame-valued summary's columns into the result while the
summary is unnamed, and packs them into one column under any name. So naming a
summary changes its result shape whenever its value is a data frame, and
`name_rewritten_summary_dots()` must not name one whose value is. Which
summaries those are is a question about the value's type, and two expressions
one call apart fall on opposite sides of it:

- `nrow(pick(v, w))` is scalar-valued and has to be named, or the rewritten
  `all_of()` literal names the column (#430's second reproduction).
- `range_frame(pick(x))` is data-frame-valued and has to not be, or its `lo`
  and `hi` come back packed under `range_frame(dplyr::pick(x))` (#435).

`name_rewritten_summary_dots()` excludes the data-frame-valued shapes a static
reading does reach — a summary written as `across()` or `pick()` itself, and
the `tibble()`/`data.frame()` family — because each names its own outputs and a
reading can say so. What is left is a data-frame-valued expression built from a
function of the caller's own, which no reading separates from a scalar one:
`range_frame` is an ordinary closure and its return type is not in the call.

So the summary is named, and the decision is made after the summarize, from
the value: a result column holding a data frame under an Assigned summary name
is one that was going to expand.

`rlang::quos_auto_name()` applied before any rewriting was #430's own stated fix
direction, and it is rejected here for the reason #435 gives: it packs strictly
more than the shapes at issue, `across()` and `range_frame(x)` included, which
is a behavior change for summaries this decision leaves alone.

## Scope: a local input

The expansion is guarded by `is.data.frame()` and by nothing else. No backend
capability is declared, because there is no difference between backends to
declare — only a local case to repair.

Measured against arrow 25.0.1, duckdb 1.5.5, dtplyr 1.3.3, RSQLite 3.53.3,
dplyr 1.2.1, and dbplyr 2.6.0:

| Input | Before an Assigned summary name existed | Today |
|---|---|---|
| `data.frame`, `data.table` | expanded | packed, and expanded again here |
| Arrow | packed, named or not | packed |
| dtplyr | data.table's own error at `collect()` | unchanged |
| SQL | aliased, never expanded | unchanged |

Only the first row changed when #430 named the summary, so only the first row
is repaired. Expanding on any other backend would be a new behavior rather than
a repair, and the packed/expanded distinction is not dplyr's to make on a SQL
backend at all: DuckDB reaches a data-frame column by `struct_pack()`, which
no summary marginplyr names goes through.

[ADR 0020](0020-ask-before-reading-a-lazy-input.md) is not engaged. The
expansion inspects a result already in memory and sends no query; the guard is
what keeps it away from every input that could be asked one.

## Where the expansion runs

`summarize_margin_union()`, per branch, between the branch's key rename and its
margin labelling. That is the point the grouping columns are under their own
names and the label and identifier columns are not there yet, so the checks
below need to be told to ignore nothing.

`summarize_margin_native()` is not a second site. `stage_margin_summaries()`
chooses it only where `supports_grouping_sets()` holds, which needs the
`native_grouping_sets` capability, which `backend_capabilities()` grants to the
`duckdb` and `postgres` kinds alone — both lazy. So no local result is built
there and the guard above could never pass.

The carrier is `new_summary_arguments()`, which gains a third vector parallel to
the dots and the caller labels: the Assigned summary name for each dot, `NA`
where marginplyr assigned none, under the same length invariant
([ADR 0015](0015-separate-package-conditions-from-internal-invariants.md)).
Share planning is the one step that moves a dot, so the new vector is
subscripted by the same origin positions the caller labels already are.

## What is checked again

`check_summary_output_names()` is asked a second time, of the names that
replaced the assigned one. None of its three questions could have been asked of
them before: the summary stood for one column under a name of marginplyr's, and
no pre-execution reading knew what it held. An inner name equal to a grouping
dimension is refused, and one equal to the caller's `.id` is refused, in the
wording those two already use.

The composite is asked rather than its parts, so this becomes a third path
under the contract that every path asks the same three questions in the same
order. Its `internal_names` is empty at this point, the branch's
`..marginplyr_key_` columns having been renamed or dropped, but the internal
question still has a subject: where the Grouping set identifier is one the
package allocated for itself, `add_grouping_set_id()` has yet to write it, and
`set_id_is_internal` is what puts it back among the internal names. Asking only
the other two would leave an inner name equal to that column silently
overwritten.

## Consequences

`dplyr::mutate()` is what writes the expanded columns, so the result's class
stays dplyr's and two inner columns sharing a name resolve the way dplyr's own
expansion resolves them.
[ADR 0016](0016-delegate-result-class-and-attributes-to-dplyr.md) records this
decision as the exception to its sentence about ending in a dplyr verb applied
to a data frame dplyr produced.

The reference states the rule a caller needs and not this argument: an unnamed
summary expands, a name you write packs, and a name marginplyr assigns does not
appear.

Two things stay outside this decision, both following from #430 rather than
from it, and both are #439: `rlang::as_label()`'s abbreviation producing a
column name the caller cannot read back, and an assigned name differing between
`pick(x)` and `dplyr::pick(x)`. Each applies equally to a scalar summary, which
this decision does not touch.
