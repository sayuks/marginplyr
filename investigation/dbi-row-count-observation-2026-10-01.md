# Observing row counts without additional queries

Investigated: 2026-10-01

## Findings from DBI's specification

The [DBI `dbGetQuery()` contract][get] returned a data frame whose row count
was the number fetched. [The `dbFetch()` contract][fetch] allowed partial
pages. Counting an existing returned frame with `nrow()` therefore needed no
extra SQL, fetch, or retained response payload. It measured rows returned to
R, not rows scanned by the database. A requested limit could leave the full
result size unknown.

| Observation | Contract read | Boundary |
| --- | --- | --- |
| `rows_fetched` | [dbGetRowCount()][count] counted rows fetched so far; summing the caller's returned frames also measured them | It was not the total result size until retrieval had finished. A result audited only after earlier unobserved fetches required an available cumulative count, otherwise its earlier history was unknown. |
| `fetch_complete` | [dbHasCompleted()][complete] answered whether a SELECT had no remaining rows | It did not mean transaction commit. At exactly the final requested row a backend could still leave completion unresolved until a later caller-requested fetch; the auditor must not fetch to discover it. |
| `rows_affected` | [dbGetRowsAffected()][affected] counted DML additions/deletions/updates; unknown counts could be `NA`; `dbExecute()` returned this number | It was different from fetched rows. SELECT's specified zero affected rows was not its output size. Data-definition operations were not evidence of how many rows a created table contained. |

The possible fields above were design implications, not adopted decisions.
`NA` would preserve unknown or inapplicable observations; a known empty fetched
result could be zero. Metadata getter failure would not become a query failure
or erase a previously known count. Existing fetched-row progress could survive
a later fetch error while total output remained unknown. A failed DML call did
not establish that no rows had changed.

The count/completion getters required a live result. DBI specified errors after
`dbClearResult()`. Its [default `dbGetQuery()` implementation][get-source]
cleared the result on exit, and [default `dbExecute()`][execute-source]
retrieved affected rows before clearing. An interceptor needed to observe an
existing result before cleanup or use the existing high-level return value.
Adding generic metadata calls was not, by itself, proof of zero network work
for every driver; that required backend evidence. Observing caller-requested
metadata calls and existing return values avoided that assumption.

[Repeated `dbBind()` calls][bind] could reuse one result. Vector parameters
represented a batch; fetched results were concatenated and affected rows were
reported for that batch. Counts needed attribution to the binding attempt,
not subtraction of a lifetime cumulative counter across reused bindings.

## Arrow boundary

[`dbFetchArrowChunk()`][arrow-chunk] returned a batch of implementation-specific
size; a returned materialized record batch could expose its length without
reading another batch. [`dbFetchArrow()`][arrow] returned an object usable as
a stream. Consuming an unknown stream merely to count it, or converting it to
a data frame, would add retrieval/materialization and was not a passive audit.

The inspected [DBI default Arrow implementation][arrow-source] assembled a
stream by fetching chunks, and its default chunk method called `dbFetch()` on
the wrapped result. Those existing return values were observable if that path
was instrumented, but native backend streams could follow a different path.
`dbGetRowCount()` could not be assumed available for every Arrow result:
[the Arrow class][arrow-class] inherited from `DBIObject`, not `DBIResult`.
Unknown Arrow counts would remain `NA` without consuming the stream.

## Minimal executed SQLite probe

An in-memory SQLite probe used DBI 1.3.0 and RSQLite 3.53.3. No package was
installed. The script and raw output remained in session-temporary
`/tmp/dbi-row-count-probe-2026-10-01.R` and `.log`.

- A three-row SELECT reported fetched counts 0, 1, and 3 before fetch, after a
  one-row fetch, and after the remaining two rows. Completion was FALSE,
  FALSE, then TRUE. `dbGetQuery(..., n = 1)` returned one row.
- Prepared SELECT binding two parameter values reported two fetched rows;
  rebinding one value reset fetched count to zero, then returned one row.
- Prepared INSERT batches of two and then one value reported affected counts
  2 and 1, not a lifetime total of 3. DML fetched count stayed zero.
- All three metadata getters errored after result clearing. Installed SQLite
  methods delegated to native result-pointer getters. nanoarrow was absent,
  so Arrow findings were source readings, not executed observations.

These results verified the SQLite cases only. No audit `COUNT(*)`, extra fetch,
or query for result cardinality was run.

## Other inexpensive observations

The existing call already supplied the DBI entry point (`send`, `bind`,
`fetch`), requested fetch limit, parameter-batch size for materialized
parameters, and connection/result association. Existing returned frames
supplied column count and names without `dbColumnInfo()` or a database query.
These could explain partial reads and errors without retaining result data.
Backend/driver class and locally available package version could identify
translation/runtime differences; server version or query plan was a separate
request and was not automatically queried by this proposal.

[get]: https://dbi.r-dbi.org/reference/dbGetQuery.html
[fetch]: https://dbi.r-dbi.org/reference/dbFetch.html
[count]: https://dbi.r-dbi.org/reference/dbGetRowCount.html
[complete]: https://dbi.r-dbi.org/reference/dbHasCompleted.html
[affected]: https://dbi.r-dbi.org/reference/dbGetRowsAffected.html
[bind]: https://dbi.r-dbi.org/reference/dbBind.html
[arrow]: https://dbi.r-dbi.org/reference/dbFetchArrow.html
[arrow-chunk]: https://dbi.r-dbi.org/reference/dbFetchArrowChunk.html
[get-source]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/dbGetQuery_DBIConnection_character.R
[execute-source]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/dbExecute_DBIConnection_character.R
[arrow-source]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/dbFetchArrow_DBIResultArrow.R
[arrow-class]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/07-DBIResultArrow.R
