# Keep Margin operations verb-neutral

A Margin operation contains the semantics shared by summarizing, expanding,
and nesting, but does not record which verb requested it. Summary expressions,
nesting options, and execution remain with their verb-specific implementations;
this keeps the shared module deep without turning it into a dispatcher whose
branches grow as the verbs diverge. The shared module interprets persistent
input groups as fixed keys and normalizes the input to an ungrouped form;
it also restores Margin column types, places grouping columns first, and
applies the common sorting semantics. Each verb remains responsible for its
input-class admission checks, verb-specific arguments, execution, and the
grouping it deliberately adds after finalization. The shared finalizer
guarantees an ungrouped baseline; `nest_by_with_margins()` alone adds row-wise
grouping afterward. This avoids passing a verb kind into the shared module
merely to decide whether a backend or option is admissible.

## Amendment: the finalizer does not order rows

Two clauses of the sentence above are superseded, both narrowing what the
shared finalizer does rather than changing who owns it.

**"applies the common sorting semantics"** no longer holds. `.sort` was removed
with the other legacy ordering arguments, and finalization now leaves row
order unspecified on every backend: a Grouping set identifier records which
occurrence a row came from but promises nothing about the order the rows
arrive in. Callers use an explicit `dplyr::arrange()` when presentation order
matters. ADR 0009 states the same rule from the identifier's side, and
`inspect_grouping()`'s guaranteed Grouping-plan order under ADR 0013 is an
inspection guarantee that deliberately does not extend to Margin-operation
results.

**"restores Margin column types"** is narrower in practice: the finalizer
restores factor and ordered-factor Margin columns from the metadata captured
during preparation. Other column types are never lost, so there is nothing
else to restore.

**"places grouping columns first"** stands, and is now specific: fixed keys,
then grouping dimensions, then the optional Grouping set identifier.

Nothing else in this decision changes. Verb-neutrality was never what made
sorting shared, and removing the sort left the finalizer's remaining
responsibilities in the same place.
