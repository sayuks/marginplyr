# SQL audit storage using standard R

Investigated: 2026-10-02

## Scope and finding

This follow-up investigated the added preference to avoid installing more
packages. It did not amend the earlier storage comparison or implement a sink.
Under that preference, CSV event append was a reasonable first candidate for
human-readable SQL history. RDS remained the standard-R alternative when
restoring nested R parameter values mattered more than direct text inspection.
These were suitability inferences, not adopted product decisions.

The repository's `DESCRIPTION` listed `utils` in Imports and RSQLite in Suggests;
it listed no direct JSON encoder dependency such as jsonlite. A mandatory SQLite
sink would therefore require availability of an optional package. An optional
SQLite feature could retain that boundary. JSON Lines would require an encoder
or new encoding implementation; relying on an undeclared transitive package
would not remove the direct dependency requirement. This was a metadata reading,
not a claim that jsonlite was absent from the user's installed library.

## Standard-R choices

| Choice | Evidence and implication |
| --- | --- |
| CSV events | `utils::write.table()` documented appending to a named file. Using comma separation, quoting, and doubled embedded quotes supported SQL text containing commas, quotes, and newlines. An attempt could append a start and then a success/error event with the same ID, preserving both. `write.csv()` itself ignored changes to `append`, so the adjustable writer was needed. |
| RDS snapshot | `saveRDS()` documented one R object per ordinary named-file save, with no append argument. Rewriting all history after each event was possible but introduced work proportional to the accumulated history and endangered the old snapshot during a failed overwrite unless replacement was handled separately. |
| One RDS per event | Saving each event to a distinct file could preserve nested parameters without rewriting earlier files. This was an inference using the documented single-object writer; it required file naming, ordering, and directory management, and an interrupted newest file could still be incomplete. |

Sources: [R table output](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/write.table.html)
and [R serialization](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html).

CSV required a fixed event schema and explicit UTF-8 handling. Reading all
columns as character data avoided automatic conversion of identifier or SQL
fields; numeric/count fields could then be converted deliberately. RFC 4180
specified quoting fields with line breaks, commas, or double quotes and doubling
embedded quotes. Consequently, a logical CSV record could span physical lines.
[RFC 4180, format definition](https://www.rfc-editor.org/rfc/rfc4180)

For CSV parameter display, `dput()`/deparsing could produce an R-like character
representation stored in one field. Its documentation explicitly warned that
some objects did not round-trip identically. The installed `dget()` body was
`eval(parse(file = file, keep.source = keep.source))`. A log reader could therefore
keep parameter text as text rather than evaluate it; safe typed restoration was
a separate interface, for which RDS was the simpler standard-R candidate.
[R object text representation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/dput.html)

## Minimal executable check

The probe `/private/tmp/sql-audit-standard-r-probe-2026-10-02.R` ran with
R 4.6.1 (2026-06-24), using only base and utils. It wrote a start event, caught a
simulated target error, and appended a failed event. It did not execute SQL.

```r
utils::write.table(event, path, append = !first, sep = ",",
  row.names = FALSE, col.names = first, quote = TRUE,
  qmethod = "double", fileEncoding = "UTF-8")
```

Reading with `read.csv(colClasses = "character", na.strings = NULL)` recovered
both event kinds, SQL containing quotes/comma/newline/Japanese text, and the
parameter display string exactly. The header and two records occupied five
physical lines. An RDS snapshot also restored the nested parameter list exactly.

A separate file received a deliberately unfinished quoted CSV tail. Ordinary
`read.csv()` warned about an incomplete final line and returned only one row,
rather than the two previously complete records. This demonstrated that a
generic reader or removal of one physical line was insufficient evidence for
reliable recovery of a multiline event log. It was not an actual crash test.

The checked writer/connection documentation supplied no transactional append
or power-loss durability guarantee. It documented stream flushing and explicit
closure to reduce buffered-data loss. Thus successful writes around a normal
target error and resilience to process/system crashes remained different claims;
CSV did not inherit SQLite's transaction guarantees. Multiple writers to one
file were not tested; separate process files would avoid requiring shared-file
coordination in an initial design.
[R connections](https://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html)
