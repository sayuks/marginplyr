# Keep margin operations verb-neutral

A margin operation contains the semantics shared by summarizing, expanding,
and nesting, but does not record which verb requested it. Summary expressions,
nesting options, and execution remain with their verb-specific implementations;
this keeps the shared module deep without turning it into a dispatcher whose
branches grow as the verbs diverge. The shared module interprets persistent
input groups as fixed keys and normalizes the input to an ungrouped form;
it also restores margin column types, places grouping columns first, and
applies the common sorting semantics. Each verb remains responsible for its
input-class admission checks, verb-specific arguments, execution, and the
grouping it deliberately adds after finalization. The shared finalizer
guarantees an ungrouped baseline; `nest_by_with_margins()` alone adds row-wise
grouping afterward. This avoids passing a verb kind into the shared module
merely to decide whether a backend or option is admissible.
