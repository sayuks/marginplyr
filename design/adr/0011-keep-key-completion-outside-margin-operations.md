# Keep key completion outside Margin operations

marginplyr will not add `.complete`, `.fill`, or `.empty` arguments to
`summarize_with_margins()`, and will not add a `complete_with_margins()` verb.
Callers that need absent key combinations will complete the input before the
Margin operation with ordinary dplyr, tidyr, or dbplyr operations. Presentation
replacements that should not contribute to totals remain ordinary post-summary
mutations.

## Decision

Input completion and result completion have different semantics. Completing
fact rows before summarization makes the inserted values participate in
detail, subtotal, and grand-total calculations. Completing an already
summarized margin result can add display rows without changing existing
aggregates. One argument cannot hide that distinction without surprising
either row counts and non-additive summaries or the relationship between
detail rows and totals.

Arbitrary summaries also cannot be rebuilt reliably from completed child
summaries. Counts and sums are additive, but means require weights, distinct
counts overlap, and medians and user-defined summaries generally cannot be
combined from child results. A post-summary fill can therefore disagree with
subtotals whenever the fill is not an identity for the original summary.
Renaming the option to `.empty` would describe that limitation more precisely
but would not remove it, and the concept was not clear enough to justify a new
public argument.

Callers will instead make the intended semantics explicit. A local input can
use `tidyr::complete()` or a key relation joined before
`summarize_with_margins()`. To retain observed keys that are absent from an
explicit scaffold, callers can union the scaffold with distinct fact keys
before the join. Inserted fact values then flow through every requested
Grouping set in the usual way. A fact marker or fact identifier can distinguish
inserted rows when calculating counts or other summaries that must ignore
them.

For lazy database inputs, documentation will show the equivalent lazy
`union()`, `left_join()`, and `mutate()` pipeline. Existing dimension tables
should be referenced on the same connection. A small local key table can be
made into a lazy relation with `dbplyr::copy_inline()`, which writes it into
the SQL query with `VALUES`; a larger or reusable table can be moved
explicitly with `dplyr::copy_to()`. marginplyr will not copy, collect, or move
completion data implicitly.

## Why completion is not part of the Grouping specification

A Grouping specification declares which Grouping sets exist. It does not
declare the valid value domain of grouping dimensions, create fact rows, or
assign summary values. Putting completion options on `rollup()` would mix
those responsibilities, leave `cube()` and arbitrary grouping sets without a
consistent interface, and make a data-independent specification depend on
summary output names.

`inspect_grouping()` consequently remains an inspection of the
backend-independent Grouping plan. Completion does not add Grouping sets or
change Grouping set identifiers, Grouping identifiers, or Grouping bits, so
it does not belong in that function's interface.

## Rejected interfaces

Adding `.complete` to `summarize_with_margins()` was rejected after considering
`NULL`, an observed-value cross product, and an explicit local or lazy key
relation. A complete interface would also need rules for fixed `.by` keys,
factor levels, key types, unrelated columns and rows, duplicate occurrences,
Margin label collisions, empty inputs, cross-source lazy joins, and
potentially explosive Cartesian products. That is a large interface around
behavior callers can express before the Margin operation with existing data
manipulation verbs.

A post-summary `complete_with_margins()` was rejected because it would require
the Grouping specification to be repeated or require an ordinary result to
retain hidden source and plan state. Hidden state would be fragile after
dplyr transformations and would impose an ordering constraint that is not
apparent from a tibble or lazy-table result.

`.fill` and `.empty` were rejected as shallow conveniences. A post-summary
`mutate()` can express presentation replacement, while a pre-summary
completion expresses values that must contribute to totals. Keeping those
operations visible makes the aggregation semantics reviewable in caller code.

## Documentation consequences

The local-data documentation will include:

- a `tidyr::complete()` example that uses `tidyr::nesting()` when observed
  key relationships must be preserved;
- an explicit key-scaffold example that unions observed keys before joining;
  and
- a fact-marker example showing how inserted rows affect counts and means.

The database documentation will include:

- an existing dimension-table example on the same connection;
- `dbplyr::copy_inline()` for a small local key table, including its SQL-size
  trade-off;
- `dplyr::copy_to()` for a larger or reusable key table; and
- a reminder that data movement is explicit and the resulting join pipeline
  remains lazy.

This decision can be revisited if repeated real-world callers expose
substantial, consistent completion logic that cannot be expressed clearly at
the input-preparation seam. A future interface must first choose whether it
models synthetic facts or completed result cells; it must not silently mix
the two.
