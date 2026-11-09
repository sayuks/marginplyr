# Acquire typed metadata once per Margin operation

Each Margin operation acquires typed selection metadata at most once and
reuses the same snapshot for Grouping selections, summary selections, column
prototypes, and factor metadata. Backend-specific schema reads or zero-row
collections remain hidden behind the Margin operation seam so later stages
cannot issue duplicate metadata queries or observe a different schema. A
prepared operation belongs to one public verb call and is not cached, reused,
or returned to users, which bounds the lifetime of that snapshot.

## Amendment (#604)

The snapshot is the input schema, not the local dplyr data mask after a summary
has run. Ordinary `across()`, `pick()`, `if_any()`, and `if_all()` selections on
local data frames read that current mask when their summary executes, so they
can see preceding ordinary outputs and their current types. This is no new
input-schema acquisition. The complete Grouping plan's columns remain outside
the selection. Lazy inputs and contextual-share selection retain their
existing planning rules.

## What this does not govern

This decision is about the schema an operation plans against: one snapshot, so
that two stages cannot plan against two different schemas. It is not a rule
that a Margin operation reads a backend exactly once. A Margin label validation
query already reads values rather than a schema, and so does the bounded type
sample a contextual share takes of its source summaries
([ADR 0010](0010-compute-parent-shares-as-a-contextual-summary.md)) on a
backend that evaluates them in the database.

Neither is a metadata query, and neither can disagree with the snapshot about a
schema, because neither asks for one. What keeps them admissible is a different
rule — [ADR 0005](0005-reject-local-errors-before-backend-reads.md): a read may
happen only after the snapshot and only for something the call cannot be shown
to be wrong about locally. A read that could be answered by the snapshot
belongs to the snapshot, and this decision is what says so.

## Amendment: a lazy result retains its construction-time schema (2026-11-09)

A returned lazy Margin result is planned against the input schema captured
during construction: column names and types, including factor levels, their
order and ordered status. After changing that schema, the caller constructs
a new Margin result. The prepared operation still belongs to one public verb
call; schema values retained in the returned query do not make that operation
a reusable cache.

This is a schema precondition, not a snapshot of source rows. Value changes
within the same schema follow the backend's ordinary lazy execution semantics.
Execution does not refresh metadata or replan the result, and this decision
promises no complete detection of schema changes. In particular, it does not
add monitoring of every column addition, rename, type transition or factor
outside the captured metadata used by the operation.

[ADR 0012](0012-distinguish-factor-na-levels-from-missing-margin-values.md)
owns the required comparison before R factor processing can lose source
values. The delegated class and attribute boundary in
[ADR 0016](0016-delegate-result-class-and-attributes-to-dplyr.md) remains in
force.

This amendment was accepted on 2026-11-09; implementation remains pending in
[#1550](https://github.com/sayuks/marginplyr/issues/1550).
The shared [construction-time schema and safe factor processing acceptance][factor-schema-acceptance]
records the implementation controls.

[factor-schema-acceptance]: ../specs/grouping-context-and-factor-schema-boundaries.md#3-construction-time-schema-and-safe-factor-processing
