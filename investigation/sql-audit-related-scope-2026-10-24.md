# Selecting SQL related to marginplyr public operations

Investigated: 2026-10-24
Repository snapshot: `26cfe5da96adf102ce8f2f3aedaf88e6f0611692`

## Question and evidence boundary

The question was whether audit history could retain only SQL originating from
or related to marginplyr public functions, including later retrieval. This
investigation read the working tree and installed dependency code and ran a
disposable SQLite experiment. It did not implement an observer or prove complete
attribution. Product code, dependency metadata, and user databases were unchanged.

## Public operations and delegated work

`NAMESPACE` exported 16 names, including the `summarise_with_margins()` alias
of `summarize_with_margins()`. SQL-capable construction paths were summary,
expansion, and inspection. Shared preparation could reach zero-row selection
metadata, requested label scans, or share probes and controls as applicable.
Inspection returned a local plan but could request zero-row metadata. SQL/Arrow
nesting was refused. Grouping constructors captured specifications; contextual
helpers were rewritten within a summary rather than independently executed.
`last_sent_queries()` read package state.

Sources: `NAMESPACE`, the exported functions in `R/`,
`prepare_margin_operation()`, `prepare_grouping_plan()`,
`grouping_selection_proxy()`, `check_observed_label_collision()`, and
`probe_share_dialect_answer()`.

Registered SQLite collection/materialization methods and the DuckDB `.env`
materialization method were package-owned later work, despite not being names
in the export list. They delegated to dbplyr and DBI. The earlier
[SQLite counterexample](sql-audit-recording-2026-10-13.md#sqlite-counterexample)
observed eight sends for one `compute()` fixture, including metadata and CREATE
whose SQL construction was delegated. Selecting only SQL text constructed in
marginplyr's R files would omit necessary steps of that operation. This was not
a universal statement count or a complete driver-path audit.

The inspected entry points forced `.data` before starting the existing record.
Other argument evaluation could invoke caller code, including nested Margin
operations or arbitrary database work. The old record restored the outer
operation without retaining the inner history. Sources: ADR 0027's
input-forcing and reentry amendments and
`tests/testthat/test-sent-queries-reentry.R`.

## Result identity was not audit provenance

No audit operation identifier or parent-operation relation was attached to
the inspected SQL results. `R/sent-queries.R` retained construction state in
a package environment. SQLite attributes served type and order handling;
`sqlite_typed_result_direct()` compared the lazy query with the stored original
query to decide whether direct-result behavior applied. It did not enumerate
ancestry after transformations. The DuckDB `.env` class had a separate
materialization purpose. Sources: `finalize_margin_operation()`,
`R/sqlite-typed-order.R`, and `R/duckdb-env-expansion.R`.

In an independent R process, a Margin result received a disposable top-level
attribute `audit_probe_origin = "A"`; a second copy received `"B"`. The
environment was R 4.6.1, DBI 1.3.0, dplyr 1.2.1, dbplyr 2.6.0, and
RSQLite 3.53.3. marginplyr was loaded from the working tree with
`pkgload::load_all()`.

| Operation | Observed marker on returned object |
| --- | --- |
| `filter(q, ...)`, `select(q, ...)` | A |
| `left_join(q, unrelated, ...)` | A |
| `left_join(unrelated, q, ...)` | Absent |
| `left_join(q_A, q_B, ...)` | A only |
| `collect(q)`, `pull(q, revenue)` | Absent on the local result |
| `compute(q)`, `collapse(q)` | Absent on the new lazy result |
| `tbl(con, "materialized_q")` | Absent |

Reproduce in a disposable R process from the repository root:

```sh
Rscript investigation/sql-audit-observation-2026-10-24/lineage.R
```

Installed dbplyr code retained the left object in the tested join and built
new objects for compute/collapse. Public documentation described
[compute](https://dbplyr.tidyverse.org/reference/compute.tbl_sql.html) as
execution into a new remote table, and
[collapse](https://dbplyr.tidyverse.org/reference/collapse.tbl_sql.html) as
wrapping a query in a subquery. Collapse did not establish execution of a
result retrieval.

This disproved sufficiency of one top-level attribute, not query-tree markers,
a registry, or observation of derived operations. Absence of an attribute
after collection also did not prevent associating retrieval before it returned.
No complete provenance mechanism was tested.

## Design implications, not adopted decisions

An active public-call context could associate delegated SQL with the operation
requesting it. After return, normal retrieval needed an additional relation.
A join could have several origins on either side. Materialization needed to
preserve the relation to its returned object separately from replacing a query
with a table reference. A newly opened table reference, an exported SQL string,
or arbitrary user transformations did not automatically carry an operation ID.
Equal SQL text, table names, and connections did not establish causation.

Observation and persistence were distinct choices: an observer could inspect
enough context to select related work without saving unrelated SQL first.
Whether selection could be complete remained unverified. A lost relation
could not safely be treated as evidence of unrelated work.

ADR 0013 concerned inspection's ordinary local tibble, rather than a blanket
ban on tracking lazy results. ADR 0016 governed result class/attribute
delegation, and ADR 0031 governed the existing SQLite exceptions. A new tracking
mechanism would need evaluation against those decisions. ADR 0037's
construction-only scope did not already satisfy later related retrieval.
