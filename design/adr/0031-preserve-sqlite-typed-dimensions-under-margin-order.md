---
status: accepted
---

# Preserve SQLite typed dimensions under Margin order

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
