---
status: accepted
---

# Preserve SQLite typed dimensions under Margin order

The original decision below records the first implementation. The accepted
[B-direct amendment](#b-direct-amendment-2026-09-26) supersedes its materialization
mechanism and the explicitly identified boundaries. Implementation of that
amendment is pending; this documentation branch changes no package behavior.

Issue #640 exposes a conflict between two existing contracts. An all-missing
character, integer, or double dimension with a typed-missing Margin label keeps
its collected type when the source-column anchor is the first arm of the
outermost `UNION ALL`. A requested Margin order currently hides its internal
Grouping set identifier with an outer projection, after which RSQLite reports
the all-missing dimension as logical. The Margin order and the typed-missing
Margin label both remain part of the result contract.

## Decision

On a live SQLite connection, a sorted Margin result that needs the source-column
type anchor will use a dedicated lazy result class. This applies to ordinary
and contextual-share summaries and to expansions, for both `NULL` and
`NA_character_` Margin labels. Direct `collect()` and direct `compute()` must
retain the source dimension's collected type, the requested Margin order, and
the public result columns. No extra schema query or unrequested read of the
caller's data is introduced. Other SQL dialects retain their existing paths;
their type and ordering behavior is decided from evidence about each dialect.

The final SQL keeps the zero-row source-column anchor as the first arm of the
outermost compound `SELECT`. It projects collision-free internal ordering
columns from every arm and orders the compound query by those columns. Their
values implement ADR 0018's existing key, including fixed-key missingness,
Grouping bits, dimension missingness and values, and the Grouping set identifier
tie-breaker. SQLite permits a compound `SELECT` to order by output columns but
rejects a new computed Grouping-bit expression in its `ORDER BY`; an outer
projection that hides the internal columns removes the declared type on which
RSQLite relies. The internal columns therefore remain in the SQL result and
the dedicated class hides them at the R result boundary. Its public column
names, display, and collected data contain only the columns the Margin verb
already promised. `sql_render()` and the Sent query record show the SQL that
actually runs, including the internal columns, as ADR 0027 requires.

`compute()` first materializes that ordered compound query in a temporary
SQLite table. It creates the requested table with the public column types
from a zero-row source-column projection, then inserts the public columns
from the temporary table in Margin order. It removes the temporary table on
success or failure, and a failed insertion rolls back creation of the
requested table. The requested table has no internal columns. SQLite assigns
successive rowids as rows are inserted into the empty table, so the returned
lazy object explicitly orders by an unshadowed one of `rowid`, `oid`, or
`_rowid_`. If all three names are
public result columns under SQLite's case-insensitive name matching,
`compute()` refuses with a Package condition before
creating either table: SQLite then offers no name for the implicit rowid, and
silently returning an unordered materialization would break the promise.
Direct `collect()` still works in that case.

The dedicated class is a narrow exception to ADR 0016's delegation of result
class to dplyr. It owns the public projection because marginplyr constructed
the typed-missing values and the hidden sorting columns. A later dplyr verb
sees only the public columns and follows ordinary dbplyr behavior; the special
type and order guarantee applies to the Margin verb's direct result and its
direct materialization, as ADR 0018 places the Margin order. Direct `collect()`
also retains dbplyr's ordinary finite-`n` behavior while hiding the internal
columns. The extra table creation and copy of the result rows under `compute()`
are work the caller requested by materializing the result, not a read during
lazy construction (ADR 0020).

## Why this exception is needed

On live RSQLite, wrapping the typed union in `SELECT g FROM (...)`, selecting
`*` from it, putting it in a CTE, or casting the outer all-NULL column to
`TEXT`, `INTEGER`, or `REAL` all yield a logical R column. Leaving the typed
union outermost preserves the respective character, integer, and double
types. SQLite's [compound-SELECT ordering rule][sqlite-select] requires an
ordering expression to match an output column, and its
[column-declaration API][sqlite-decltype] cannot recover a base declaration
from a general expression or subquery. Neither [dbplyr's `collect()`][collect]
nor [DBI's `dbFetch()`][fetch] accepts a result prototype that could repair the
type. The generic SQL backend does not hold input column prototypes, so an R
side conversion would first need a new read that this decision excludes.

A `CREATE TABLE AS SELECT` from the ordered compound query does not declare
the missing dimension's source type on SQLite. The requested table instead
uses the zero-row source projection to declare its columns, then copies the
ordered rows into that empty table. SQLite's [rowid allocation][sqlite-rowid]
gives these inserts successive rowids; the returned object's explicit
`ORDER BY` uses that property rather than relying on an unordered table scan.

This decision changes the SQLite implementation mechanism in ADR 0018, not
the Margin order key or its scope. The class exception is limited to the live
SQLite case described here. The three shadowed-rowid names are the one
materialization refusal; the implementation must test it before doing work.

[sqlite-select]: https://www.sqlite.org/lang_select.html
[sqlite-decltype]: https://sqlite.org/c3ref/column_decltype.html
[sqlite-rowid]: https://www.sqlite.org/autoinc.html
[collect]: https://dbplyr.tidyverse.org/reference/collect.tbl_sql.html
[fetch]: https://dbi.r-dbi.org/reference/dbFetch.html

## B-direct amendment (2026-09-26)

The maintainer accepted B-direct after the
[2026-09-25 investigation][b-direct-evidence]. Its final prototype retained the
tested type, Margin order, and public-column guarantees while removing the
sorted result's full-result intermediate table. It also resolved the observed
destination, transaction, and downstream-projection defects. These measurements
support the decision; they are not production release validation. The
[implementation specification](../specs/sqlite-b-direct.md) owns the acceptance
matrix, unresolved validation, and existing Issue dispositions. Do not reopen
the A/B/C selection without new contradictory evidence.

### Retained guarantees and smaller materialization

Keep the direct collection boundary and the existing Margin order key. The
boundary has two responsibilities: preserving source-column types through the
zero-row anchor, and restoring types explicitly declared by the package. Its
scope includes applicable sorted and unsorted summaries and expansions, text
Margin labels, one-set plans, identifiers, and contextual shares. Character
text-label declarations, integer identifiers, and double shares also apply to
empty and nonempty all-missing columns. This does not infer new types for
ordinary aggregate expressions.

For direct compute, create the public destination with the zero-row type anchor
and insert the ordered public query directly once. Do not create a full-result
staging table. The destination contains only public columns. Sorted results
retain an explicit rowid ORDER BY, with empty window-order metadata so subsequent
ordinary dplyr selection and renaming remain usable. All three implicit-rowid
aliases being shadowed, case-insensitively, still refuses sorted compute before
writes. Direct collect remains available, and unsorted results do not acquire
that refusal. Later dplyr verbs retain ordinary dbplyr semantics; no additional
Margin order or direct-result type guarantee attaches to their derived results.

Preserve the established finite-collection validation and warning behavior.
The original claim of ordinary dbplyr finite-n parity is qualified: the tested
sorted dedicated boundary accepts a fractional positive limit that the measured
ordinary dbplyr path rejects; delegated collection retains its current behavior.
Neither this distinction nor ordinary aggregate
type inference licenses a regression in the existing result contract.

### Destination safety and explicit metadata exception

Carry one complete, correctly quoted destination identity through every
operation, including overwrite, index creation, insertion, analysis, and the
returned table. Refuse before writes an unqualified persistent destination
shadowed by temp, and an unqualified temporary overwrite whose name is absent
from temp but exists in main. Equivalent identifier spellings must not bypass
these checks. Explicit temp with temporary materialization is distinct from a
non-temp qualified schema. Rowid shadowing is therefore no longer the only
materialization refusal.

Explicit compute may inspect destination metadata only as needed for safe
admission and resolution. This replaces the original blanket prohibition on
extra schema queries; it permits no new source-schema probe or data read during
lazy construction and adds no unrequested-read exemption to ADR 0020. One DBI
metadata call can send multiple SQL statements. Supported creation and index
behavior remains delegated to dbplyr. A qualified-index request that ordinary
dbplyr cannot support may still fail, with state restored; the package does not
introduce a separate index implementation to expand that support.

### Transaction ownership and audit scope

One uniquely named private SAVEPOINT owns the mutation interval. Success
releases only that savepoint; failure rolls back and releases it, preserving
input, the previous destination, and unrelated caller work. The caller retains
commit and rollback ownership when an outer transaction exists. Both supported
in_transaction booleans retain this atomicity; FALSE does not mean no
transaction-control SQL. The inner dbplyr operation must not begin another
transaction. Preserve the causal error if rollback or release itself fails,
and do not claim recovery after connection or whole-transaction failure.
Successful self-overwrite is not added to the contract.

The Sent query record describes the last tracked Margin verb or inspection
call during construction, with its result entry matching full direct rendering.
Finite LIMIT, destination metadata, DDL, INSERT, ANALYZE, and savepoints do not
become retrospective entries in that global record. Computing an older result
must not contaminate a newer call's record. Audit remains independent of
execution, warnings, and errors; it is not a complete DBI execution ledger.

Implementation must reconcile ADR 0016's result-boundary account, ADR 0018's
materialization mechanism, ADR 0020's explicit-execution account and policy
gates, and ADR 0027's audit wording, together with the glossary and affected
public references. This amendment records the accepted decision; the historical
investigation retains its original evidence and conclusions as dated.

[b-direct-evidence]: https://github.com/sayuks/marginplyr/blob/e6046acd2933d943aba71b81630287f8599262b1/investigation/sqlite-b-direct-2026-09-25/review/README.md
