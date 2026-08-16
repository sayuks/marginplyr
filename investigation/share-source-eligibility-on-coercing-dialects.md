# Share-source eligibility on a dialect that converts rather than refuses

Investigated: 2026-08-16

`investigation/share-source-schema-vs-data-read.md` (2026-08-08) established
that on RSQLite no method reports a computed expression's real type without
the backend computing at least one row, and it framed the choice as zero rows
or real types.

This note revisits the premise underneath that framing: that the real type,
once obtained, establishes whether a share source is eligible. On a dialect
that converts a non-numeric value to a number instead of refusing it, it does
not. Measured against RSQLite and duckdb 1.5.5 through dbplyr 2.6.0 on
2026-08-16, with marginplyr loaded from the working tree via
`pkgload::load_all()`. Scripts were throwaway; commands are quoted inline.

## The type is real and is not evidence

```r
d <- data.frame(g = c("a","a","b"), txt = c("x","y","z"), v = c(1,2,3))
collect(summarize(db, t = sum(txt, na.rm = TRUE)))
```

| backend | result |
|---|---|
| RSQLite | `numeric`, value `0` |
| duckdb | error: `Binder Error: No function matches the given name and argument types` |

SQLite's answer is a genuine double. `is_share_source_type()` (`R/share.R`)
asks `typeof(value) %in% c("integer", "double") && !is.object(value)`, and a
double is what it receives. The eligible-type rule reads the type of the value
the database returned, and on this dialect that type carries no information
about whether the summary meant anything.

End to end, with marginplyr:

```r
collect(summarize_with_margins(
  db, t = sum(txt, na.rm = TRUE), s = share_of_total(t), .grouping = rollup(g)
))
#>   g         t     s
#>   a         0    NA
#>   b         0    NA
#>   Total     0     1
```

The local backend raises a `marginplyr_error` for the same call. The SQLite
result is the symptom #106 described: an all-missing share column carrying the
grand total's own-denominator `1`, which reads as 100%.

## #106's own reproduction is fixed; the class of bug is not

```r
collect(summarize_with_margins(
  db, lab = max(txt), p = share_of_total(lab), .grouping = rollup(g)
))
#> Error: Total share `p` requires source summary `lab` to be a plain integer
#>   or double scalar; detected type ...
```

`max(txt)` returns `character` on SQLite as it does elsewhere, so the one-row
probe added in `e08c3fa` catches it and the diagnostic matches the local one.
What the probe cannot catch is a summary the dialect converts, because there is
no wrong type to detect. The gap is not in the probe's implementation but in
the question it asks.

## A dialect can be asked whether it converts, reading nothing

The property that decides the case is a property of the dialect, not of the
caller's data, and it can be measured with a query that references no table of
theirs:

```r
probe <- dplyr::summarize(
  dplyr::tbl(con, dbplyr::sql("SELECT 1 AS z")),
  p = sum("x", na.rm = TRUE)
)
```

Both dialects rendered the same SQL and answered differently:

```sql
SELECT SUM('x') AS p FROM (SELECT 1 AS z) AS q01
```

| backend | result | reading |
|---|---|---|
| RSQLite | `0`, no error | converts |
| duckdb | `Binder Error` | refuses |

The signal is whether the backend raises, not what value it returns. The
character literal reaches SQL through dbplyr's ordinary translation —
`sum("x")` renders as `SUM('x')` — so the only dialect-specific text is
`SELECT 1 AS z`.

The same distinction was reproduced with the caller's own table as the source,
under both `utils::head(.data, 0L)` and `dplyr::filter(.data, FALSE)`; duckdb
raised a binder error in every shape, because binding precedes execution and
does not depend on rows. The table-free form was preferred for what it avoids
rather than for what it adds: it makes the unresolved question about
`LIMIT 0` billing in `investigation/query-cost-across-lazy-backends.md`
irrelevant to this read.

## Zero-row spellings are not interchangeable across dplyr backends

```r
collect(filter(arrow::as_arrow_table(d), FALSE))
#> Error: filter expressions must be either an expression or a list of expressions
collect(filter(dtplyr::lazy_dt(d), FALSE))
#> 0 rows, classes factor / numeric
collect(head(arrow::as_arrow_table(d), 0))
#> 0 rows
```

Arrow rejected a bare `FALSE` in `filter()`. `head(x, 0)` returned zero rows on
every backend tried. Any zero-row read intended to be portable across the
dplyr backends marginplyr accepts is therefore spelled `head(x, 0)`, whatever
SQL a given dialect renders it as.

## What this establishes

On a dialect that converts non-numeric values to numbers, reading a row of the
caller's data cannot establish that a share source is eligible, because every
source reports an eligible type. The 2026-08-08 note's conclusion stands as
stated — zero rows or real types, not both — but the real type is not the
evidence the eligible-type rule needs on such a dialect, so obtaining it does
not settle the question the rule exists to answer.

The property that does settle it is whether the dialect converts, and that is
answerable by one query per dialect that reads none of the caller's data and
references none of their tables. Whether a dialect converts is a property of
the dialect, so the answer is reusable for every connection sharing it.
