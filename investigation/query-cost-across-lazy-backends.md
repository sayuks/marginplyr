# What a query costs on a lazy backend

Investigated: 2026-08-16

`.check_margin_label` defaults to `is.data.frame(.data)`, and the reason
recorded in the sources was that checking "would require an additional query"
(`R/summarize_with_margins.R`) and that the extra scan should be opted into
(`vignettes/database_backends.qmd`). Both read as performance notes. The
design intent behind the argument was avoiding unintended billing and database
load, and #122 proposed changing the default without that intent being
recorded anywhere a reader could find it.

This note records what the vendors' own documentation says a query costs, what
could not be found in it, and what was measured locally. It was prompted by
#122 and by the `probe_share_sources()` read that `share_of_parent()` issues.

## Billing models found

Four models, established from vendor documentation read on 2026-08-16.

### Per byte scanned

**BigQuery** (on-demand) charges for the bytes in the columns a query
references, not for the rows it returns.

> For non-clustered tables, applying a `LIMIT` clause to a query doesn't
> affect the amount of data that is read.

— *Estimate and control costs*, https://cloud.google.com/bigquery/docs/best-practices-costs

The same page states that a clustered table is the exception, where a `LIMIT`
can reduce the bytes scanned because scanning stops once enough blocks are
read. The pricing page adds a minimum of 10 MiB of processed data per
referenced table and 10 MiB per query, and states that queries returning an
error or served from cache are not charged.

**Athena** charges $5 per TB scanned from S3, for successful and unsuccessful
queries alike, with a 10 MB minimum per query
(https://aws.amazon.com/athena/pricing/).

**Redshift Spectrum** charges $5.00/TB scanned, rounded up to the next
megabyte, with a 10 MB minimum per query
(https://aws.amazon.com/redshift/pricing/).

### Per unit of time

**Snowflake** bills warehouse credits per second, with a 60-second minimum
each time a warehouse starts
(https://docs.snowflake.com/en/user-guide/cost-understanding-compute). A query
that wakes a suspended warehouse therefore has a floor of one minute of
credits regardless of what it reads.

**Redshift Serverless** bills RPU-hours per second with a 60-second minimum,
and **Aurora Serverless v2** bills ACU per second.

### Per provisioned capacity — no per-query charge

**Amazon RDS** (non-Aurora) charges DB instance-hours, billed per second with
a 10-minute minimum, plus provisioned storage per GB-month. For gp3 and
Provisioned IOPS storage the IOPS are charged as provisioned, regardless of
IOPS consumed; only the legacy magnetic storage class is charged per million
I/O requests (https://aws.amazon.com/rds/pricing/,
https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/User_DBInstanceBilling.html).

An extra query against RDS therefore costs instance CPU and contention with
other work, and no money per query.

### Per I/O request

**Aurora Standard** charges per million I/O requests, and read operations
count. **Aurora I/O-Optimized** removes I/O charges in exchange for a higher
storage rate ($0.225 vs $0.10 per GB-month)
(https://aws.amazon.com/rds/aurora/pricing/).

## What could not be found

Searched for on 2026-08-16 and not located in vendor documentation. Each is
recorded because the absence is the finding: an argument that assumes any of
these is resting on something no document states.

- **Whether `LIMIT 0` is exempt from BigQuery's rule.** The rule quoted above
  names `LIMIT` without qualification and no carve-out for zero was found. No
  statement about `WHERE FALSE` was found either. What BigQuery does document
  for this purpose is *dry run*, which returns the schema and an estimate of
  bytes processed and is stated to incur no charge
  (https://cloud.google.com/bigquery/docs/estimate-costs). That a vendor
  exposed to scan billing built a free path for schema and validation
  questions is the strongest available evidence that paying to scan for a type
  is not the expected practice.
- **Whether a query referencing no table is subject to BigQuery's 10 MiB
  per-query minimum.** The pricing page states the minimum per query without
  qualifying it by whether a table is referenced.
- **Whether a `LIMIT` reduces bytes scanned on Athena's native S3 path.** Two
  Athena performance pages were read in full
  (`performance-tuning-data-optimization-techniques`,
  `performance-tuning-query-optimization-techniques`); neither relates `LIMIT`
  to data scanned. Athena's *federated connector* pages do state it — "A
  `LIMIT N` statement reduces the data scanned by the query. With `LIMIT N`
  pushdown, the connector returns only `N` rows to Athena"
  (https://docs.aws.amazon.com/athena/latest/ug/connectors-redshift.html) —
  but that is the connector path, not the S3 path Athena is normally used
  through.

## What was measured locally

Against duckdb 1.5.5 and RSQLite in this environment, through dbplyr 2.6.0.
Commands are quoted so each result can be reproduced; the scripts were
throwaway.

### `dplyr::tbl()` issues an unrequested zero-row read

```r
trace(DBI::dbSendQuery, tracer = quote(print(statement)))
t1 <- dplyr::tbl(con, "d")
#> "SELECT *\nFROM `d` AS `q01`\nWHERE (0 = 1)"
```

Creating a table reference sends a query before the caller has asked for
anything. A lazy `summarize()` and a `colnames()` call on the result sent
none. A zero-row read of the input is therefore not a shape marginplyr
introduced; it is what the layer below already does per table reference.

### The collision scan costs ~28% on duckdb, in its worst shape

5,000,000 rows, three character grouping dimensions under `rollup()` — the
shape where no dimension's answer is available from metadata, so the scan is
unavoidable:

```
.check_margin_label = FALSE  median 0.074s
.check_margin_label = TRUE   median 0.095s   (+0.021s, +28%)
```

The cost that decides the default is not this one. It is the round trip and
the billing models above, neither of which is visible in an in-process
measurement.

### Backend kind does not determine whether a query leaves the machine

`grouping_backend()` classifies by input class and SQL dialect
(`R/grouping-backend.R`). Nothing it reads distinguishes:

- a local DuckDB file from a hosted DuckDB service — both reach
  `dbConnect(duckdb::duckdb(), ...)` and classify as `kind = "duckdb"`;
- an in-memory Arrow `Table` from a `FileSystemDataset` over object storage —
  both classify as `kind = "arrow"`, though `q$.data` does distinguish the two
  classes, and a `FileSystemDataset` does not distinguish local disk from
  object storage;
- RDS PostgreSQL, which has no per-query charge, from Aurora Standard, which
  charges per I/O request — both classify as `kind = "postgres"`;
- SQLite from BigQuery — both fall into the generic `kind = "sql"`.

`is.data.frame()` is the only predicate available that answers "no external
system is involved", and it answers it exactly rather than approximately.

## What this establishes

A read bounded in the rows it requests and returns is not bounded in what it
costs. On BigQuery a `LIMIT` does not reduce the bytes billed on a
non-clustered table; on Snowflake a one-row query that starts a warehouse
costs a minute of credits; on Aurora Standard every read is a billable I/O
request; on Athena a query that fails is billed like one that succeeds. The
justification recorded in ADR 0010 for the one-row `probe_share_sources()`
read — "one read bounded in rows requested and returned" — is accurate about
rows and says nothing about cost, and cost is what it was offered as an answer
to.

Conversely, three of the four models charge nothing per query, or nothing that
scales with a zero-row read: RDS and provisioned Redshift charge for capacity,
and Aurora I/O-Optimized removes the I/O charge. No single statement covers
all four, and no property `grouping_backend()` can read tells them apart. A
design that tries to price backends is a design that must track four vendors'
pricing pages; one that asks the caller before reading their data does not.
