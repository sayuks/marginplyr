# Acquire typed metadata once per Margin operation

Each Margin operation acquires typed selection metadata at most once and
reuses the same snapshot for Grouping selections, summary selections, column
prototypes, and factor metadata. Backend-specific schema reads or zero-row
collections remain hidden behind the Margin operation seam so later stages
cannot issue duplicate metadata queries or observe a different schema. A
prepared operation belongs to one public verb call and is not cached, reused,
or returned to users, which bounds the lifetime of that snapshot.

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
