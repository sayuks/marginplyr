# Acquire typed metadata once per Margin operation

Each Margin operation acquires typed selection metadata at most once and
reuses the same snapshot for Grouping selections, summary selections, column
prototypes, and factor metadata. Backend-specific schema reads or zero-row
collections remain hidden behind the Margin operation seam so later stages
cannot issue duplicate metadata queries or observe a different schema. A
prepared operation belongs to one public verb call and is not cached, reused,
or returned to users, which bounds the lifetime of that snapshot.
