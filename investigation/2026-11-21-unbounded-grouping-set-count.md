# Cost and backend limits of an unbounded grouping-set count

Investigated: 2026-11-21

This note records what a Grouping plan with many grouping sets cost on each
backend on 2026-11-21, and what each backend's own limits were. It was prompted
by a finding of the 2026-11-19 security audit of main `d0262f85`: `cube()` over
n columns resolves to 2^n grouping sets and the package neither caps nor warns.
The note holds evidence only.

The measurements were taken from main `dcbacefa`, installed into a private
library. The grid tables came from the `grid` mode of
[`2026-11-21-unbounded-grouping-set-count.R`](2026-11-21-unbounded-grouping-set-count.R).
The inspection series, the time-limit probes and the DuckDB thread table came
from one-off code, which that script's `inspect`, `limit` and `duckdb` modes
restate; a sample of each, rerun through them on 2026-11-21, agreed with the
figures below except where a rerun is quoted. The
profile, the `SIGINT` probe and the selection-helper check were run from
one-off code that was not kept.

## Summary

- On every path that builds one branch per grouping set, the time went to R
  code in the caller's session and grew with the number of sets S as S^0.99 to
  S^1.22. No path showed quadratic growth.
- The constant differed by backend: at 4,096 sets a summary took 18 s on a
  local data frame, 22 s on dtplyr, 4.3 min on an Arrow table and 2.3 min on
  SQLite, counting construction and `collect()`.
- A native `GROUPING SETS` summary did not build branches. At 4,096 sets
  PostgreSQL 17.11 took 1.2 s end to end and DuckDB 1.5.5 between 1.7 s and
  10 s depending on its thread count.
- The audit's 14-column SQLite case finished: 274 s to construct and 428 s to
  `collect()`, 11.7 min together, for a 61 MB statement the database ran in
  19 s.
- Three engine answers arrived before any client-side limit did. PostgreSQL
  refused a native summary above 4,096 sets. PostgreSQL refused a 16,384-set
  expansion with `stack depth limit exceeded`, after the client had spent 422 s
  constructing it. DuckDB ran out of memory on a native summary of 8,192 sets
  of a 256-row table at one and at ten threads, though not at four, and of
  16,384 sets at both thread counts tried, far below its parser's 65,535-set
  cap.
- `inspect_grouping()` returned the set count in 0.09 s at 4,096 sets and
  1.6 s at 65,536; at 2^20 sets it took 39 s and 1.3 GB.
- A build responded to an interrupt within 0.1 s on all four backends tried,
  and to a three-second time limit on time. The time limit did not bound the
  memory of plan construction: a 24-column cube held 1.1 GB when it was
  stopped at 3.15 s.

## Question

Does the cost of a large Grouping plan come from the number of grouping sets
alone, and at what size does each backend stop — by its own limit, by the
client's time, or by memory — when `cube()` is applied to 2 through 16 columns?

Three statements in the audit finding were checked first.

1. *"The `UNION ALL` fallback reads the source once per grouping set."* That
   held for some combinations of verb and backend only, which #1707 had
   already established and *Portable fallback SQL* in
   `vignettes/database_backends.qmd` stated on 2026-11-21: a portable summary
   read the source once per set; an expansion on DuckDB, PostgreSQL or a live
   SQLite 3.35 or later read it once into a common table expression that every
   branch then read; an expansion on any other connection read it once per
   set; and a native summary was one statement with no branches at all.
2. *"Ten columns: 1,024 branches, 2.4 MB of SQL, 28 s."* Reproduced at
   30.2 s, and split: 13.0 s constructing the lazy query, 16.9 s rendering 2,484,019 bytes,
   0.31 s executing. `collect()` rendered again, so a caller who built the
   query and collected it waited 13.0 s + 17.4 s = 30.4 s, of which the
   database's share was 1%.
3. *"Fourteen columns did not finish in ten minutes."* It finished in 11.7 min;
   see *SQLite at 14 columns* below.

## Measurements

### Environment and method

R 4.6.2 on an Apple-silicon Mac mini; DuckDB's defaults there were
`threads = 10` and `memory_limit = 12.7 GiB`. dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, arrow 25.0.1, duckdb 1.5.5, RSQLite 3.53.3,
RPostgres 1.4.10 against PostgreSQL 17.11 over a local socket with
`max_stack_depth = 2MB` and `work_mem = 4MB`.

One cell was one backend, one verb and one `cube()` over D two-valued
character columns of a 256-row input holding one further numeric column. The
summary was `n = dplyr::n(), s = sum(v, na.rm = TRUE)`; every other argument
kept its default. Each cell ran in its own R process and was killed at 300 s.
A phase that took under five seconds was run three times and its median
recorded; a longer one was run once. Up to three cells ran concurrently, so a
figure is good to its first two digits at best.

The phases were: *construct*, the Margin verb's call, which on a data frame
also computed the result; *render*, `dbplyr::sql_render()` on the result;
*execute*, `DBI::dbGetQuery()` on the rendered text; and *collect*,
`dplyr::collect()` on the result, which on a dbplyr table renders again before
executing. "Simulated" means a `dbplyr::tbl_lazy()` on a simulated connection,
where nothing can be executed.

In the tables, `> 300` marks a phase the 300 s limit cut off or never reached,
`refused` an answer from the engine quoted under *What the engines answered*,
and `–` a cell that was not run.

### Construction

Seconds for the Margin verb's call, by columns in the cube (grouping sets):

| Backend | Verb | 2 (4) | 4 (16) | 6 (64) | 8 (256) | 10 (1,024) | 12 (4,096) | 14 (16,384) | 16 (65,536) |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| local data frame | summarize | 0.02 | 0.06 | 0.21 | 0.88 | 3.83 | 18.0 | 81.4 | > 300 |
| local data frame | expand | 0.01 | 0.03 | 0.11 | 0.49 | 2.06 | 9.23 | 44.6 | 235 |
| dtplyr | summarize | 0.02 | 0.06 | 0.19 | 0.80 | 3.36 | 14.6 | 64.8 | > 300 |
| dtplyr | expand | 0.01 | 0.02 | 0.10 | 0.42 | 1.88 | 8.26 | 39.1 | 211 |
| Arrow table | summarize | 0.12 | 0.51 | 2.16 | 9.95 | 56.0 | 245 | > 300 | – |
| Arrow table | expand | 0.06 | 0.23 | 1.18 | 5.60 | 28.0 | 141 | > 300 | – |
| SQLite, live | summarize | 0.06 | 0.17 | 0.67 | 2.86 | 13.0 | 57.9 | 274 | – |
| SQLite, live | expand | 0.06 | 0.25 | 1.05 | 4.62 | 21.2 | 98.6 | > 300 | – |
| SQL Server, simulated | summarize | 0.05 | 0.26 | 0.85 | 3.52 | 16.0 | 72.3 | 284 | – |
| SQL Server, simulated | expand | 0.03 | 0.11 | 0.48 | 2.34 | 10.9 | 50.0 | 228 | – |
| DuckDB, live | summarize | 0.03 | 0.04 | 0.04 | 0.04 | 0.06 | 0.15 | – | – |
| DuckDB, live | expand | 0.06 | 0.23 | 1.11 | 5.30 | 24.9 | 116 | > 300 | – |
| PostgreSQL, simulated | summarize | 0.03 | 0.03 | 0.03 | 0.04 | 0.06 | 0.12 | 0.32 | 1.38 |
| PostgreSQL, simulated | expand | 0.04 | 0.17 | 0.85 | 4.00 | 19.0 | 89.0 | > 300 | – |
| PostgreSQL 17.11, live | summarize | 0.03 | 0.04 | 0.03 | 0.04 | 0.06 | 0.12 | 0.35 | – |
| PostgreSQL 17.11, live | expand | 0.04 | 0.17 | 0.84 | 4.04 | 18.9 | 87.2 | 422 | – |

The live PostgreSQL expansion at 14 columns and the SQLite summary at 14
columns were rerun with limits of 1,800 s and 2,400 s; the table holds those
runs.

The DuckDB and PostgreSQL summary rows are the native adapter. Every other row
built one branch per grouping set. For each 4-fold step in S from 1,024 sets
upward, a branch-building row's construction time grew as S^0.99 to S^1.16,
except the two eager-sized steps from 16,384 to 65,536 sets, local and dtplyr
expansion, at S^1.20 and S^1.22 while holding 4 GB. A linear cost is S^1, and
the `O(S log S)` pairing in `union_margin_branches()` predicts slightly more
than that.

Per grouping set at 4,096 sets, a summary's construction cost 4.4 ms on a
local data frame (which is the whole operation), 3.6 ms on dtplyr, 14 ms on
SQLite, 18 ms on simulated SQL Server and 60 ms on an Arrow table; an
expansion's cost 12 to 28 ms on the SQL backends and 34 ms on an Arrow table.

### After construction

Seconds for `collect()` on the two lazy backends that are not SQL:

| Backend | Verb | 2 (4) | 4 (16) | 6 (64) | 8 (256) | 10 (1,024) | 12 (4,096) | 14 (16,384) |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| dtplyr | summarize | 0.00 | 0.02 | 0.08 | 0.38 | 1.65 | 7.72 | 38.8 |
| dtplyr | expand | 0.00 | 0.01 | 0.04 | 0.18 | 0.88 | 4.12 | 24.1 |
| Arrow table | summarize | 0.01 | 0.03 | 0.16 | 0.54 | 2.31 | 11.9 | > 300 |
| Arrow table | expand | 0.01 | 0.03 | 0.09 | 0.42 | 1.65 | 9.47 | > 300 |

Seconds for `dbplyr::sql_render()` on a summary. An expansion on SQLite,
DuckDB and PostgreSQL rendered its statement during construction, so its
render time is inside the table above and was under 0.03 s afterwards.

| Backend | 2 (4) | 4 (16) | 6 (64) | 8 (256) | 10 (1,024) | 12 (4,096) | 14 (16,384) |
|---|---:|---:|---:|---:|---:|---:|---:|
| SQLite, live | 0.04 | 0.17 | 0.75 | 3.62 | 16.9 | 79.6 | 384 |
| SQL Server, simulated | 0.05 | 0.19 | 1.17 | 5.31 | 23.8 | 97.6 | > 300 |
| DuckDB, live (native) | 0.01 | 0.02 | 0.04 | 0.07 | 0.21 | 0.71 | – |
| PostgreSQL 17.11, live (native) | 0.01 | 0.01 | 0.02 | 0.05 | 0.12 | 0.43 | 1.69 |

Bytes of rendered SQL:

| Backend | Verb | 2 (4) | 4 (16) | 6 (64) | 8 (256) | 10 (1,024) | 12 (4,096) | 14 (16,384) |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| SQLite, live | summarize | 2,038 | 14,128 | 86,926 | 478,080 | 2,484,019 | 12,462,752 | 60,852,877 |
| SQLite, live | expand | 1,041 | 5,861 | 37,155 | 214,905 | 1,129,000 | 5,736,378 | > 300 |
| SQL Server, simulated | summarize | 1,781 | 10,259 | 54,651 | 274,561 | 1,323,552 | 6,210,385 | > 300 |
| SQL Server, simulated | expand | 916 | 4,948 | 25,780 | 128,098 | 613,155 | 2,861,405 | > 300 |
| DuckDB, live | summarize (native) | 521 | 1,041 | 2,161 | 6,257 | 25,080 | 115,078 | – |
| DuckDB, live | expand | 572 | 3,066 | 16,744 | 85,040 | 413,501 | 1,939,037 | > 300 |
| PostgreSQL 17.11, live | summarize (native) | 551 | 1,135 | 2,583 | 8,351 | 35,374 | 164,292 | 764,762 |
| PostgreSQL 17.11, live | expand | 613 | 3,367 | 18,595 | 94,501 | 454,032 | 2,119,538 | 9,690,964 |

The simulated PostgreSQL summary at 16 columns rendered 3,509,488 bytes in
6.66 s. A portable statement's bytes per set also grew with D, because every
branch names every column: 1.9 kB per set at 8 columns and 3.7 kB at 14 on
SQLite.

Seconds for the database to execute the rendered statement:

| Backend | Verb | 2 (4) | 4 (16) | 6 (64) | 8 (256) | 10 (1,024) | 12 (4,096) | 14 (16,384) |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| SQLite, live | summarize | 0.00 | 0.00 | 0.01 | 0.04 | 0.31 | 2.25 | 18.9 |
| SQLite, live | expand | 0.00 | 0.00 | 0.02 | 0.11 | 0.68 | 3.94 | > 300 |
| DuckDB, live | summarize (native) | 0.00 | 0.00 | 0.01 | 0.04 | 0.25 | 0.83 to 9.1 | refused |
| DuckDB, live | expand | 0.00 | 0.00 | 0.01 | 0.03 | 0.14 | 1.01 | > 300 |
| PostgreSQL 17.11, live | summarize (native) | 0.00 | 0.00 | 0.01 | 0.02 | 0.09 | 0.67 | refused |
| PostgreSQL 17.11, live | expand | 0.00 | 0.00 | 0.01 | 0.04 | 0.23 | 1.10 | refused |

`collect()` on a SQLite summary took 17.4 s at 1,024 sets, 79.9 s at 4,096 and
428 s at 16,384: the render time again plus the execution.

Peak R memory in MB, the sum of the "max used" column of `gc()`, which does
not count memory a database or Arrow allocated outside R's heap:

| Backend | Verb | 8 (256) | 10 (1,024) | 12 (4,096) | 14 (16,384) | 16 (65,536) |
|---|---|---:|---:|---:|---:|---:|
| local data frame | summarize | 92 | 104 | 160 | 596 | > 300 |
| local data frame | expand | 112 | 152 | 290 | 1,072 | 4,581 |
| dtplyr | summarize | 103 | 167 | 277 | 1,025 | > 300 |
| dtplyr | expand | 137 | 220 | 608 | 1,622 | > 300 |
| Arrow table | summarize | 135 | 290 | 944 | > 300 | – |
| SQLite, live | summarize | 153 | 169 | 354 | 1,301 | – |

The local results held 325,047 summary rows at 4,096 sets and 1,901,596 at
16,384; an expansion holds S × 256 rows by definition, 16,777,216 at 65,536
sets.

### SQLite at 14 columns

With a 2,400 s limit the 16,384-set SQLite summary completed: 274 s to
construct, 384 s to render 60,852,877 bytes, 18.9 s to execute, 428 s to
`collect()`, 1,901,596 rows, 1.3 GB peak in R. Construction followed by
`collect()` was 702 s. From 12 to 14 columns construction grew 4.74-fold and
rendering 4.82-fold for a 4-fold increase in sets.

### Where the time went

`Rprof()` at 256 sets, 10 ms sampling:

- **SQLite summary, construction.** 98% under `summarize_margin_union()`; 65%
  under `margin_safe_column_reads()`, almost all of it dbplyr's
  `mutate.tbl_lazy()` called once per branch by `mutate_margin_labels()`; 9%
  under `union_margin_branches()`; 9% under dbplyr's `sqlite_version()`.
- **SQLite summary, rendering.** 71% under dbplyr's `sqlite_version()`, which
  is `numeric_version(RSQLite::rsqliteVersion()[[2]])`, reached from
  `sql_translation.sql_dialect_sqlite()`. *dbplyr 2.6.0 union construction and
  rendering* below reads the source for why it ran per translation. The
  simulated SQL Server rendering, which has no such lookup, was no faster.
- **Arrow summary, construction.** 49% under `label_margin_branch()` and 41%
  under `summarize_margin_branch()`, the two per-branch steps; 6% under
  `union_margin_branches()`. The self time sat in arrow's expression bindings
  (`compute___expr__type` 16%, `compute___expr__get_field_ref_name` 7%).

### What the engines answered

- **PostgreSQL, native summary, 16,384 sets.** `ERROR: too many grouping sets
  present (maximum 4096)`, raised when the statement was prepared, 0.35 s of
  construction and 1.69 s of rendering after the call. 4,096 sets ran in
  0.67 s.
- **PostgreSQL, expansion, 16,384 sets.** `ERROR: stack depth limit exceeded`
  with the hint `Increase the configuration parameter "max_stack_depth"
  (currently 2048kB)`, on a 9,690,964-byte statement of 16,383 `UNION ALL`
  operators that had taken 422 s to construct. 4,096 sets ran in 1.1 s. No
  size between the two was run.
- **DuckDB, native summary.** The grid's 4,096-set cell failed on a sandbox
  restriction instead: DuckDB tried to create its temporary directory and
  could not. Rerun with `temp_directory` pointed at a writable path, on the
  same 256-row table:

  | Sets | `threads` | Seconds | Result |
  |---:|---:|---:|---|
  | 1,024 | 10 | 0.28 | 49,181 rows |
  | 2,048 | 10 | 0.56 | 128,893 rows |
  | 4,096 | 10 | 6.66 and 9.11 | 325,047 rows |
  | 4,096 | 4 | 0.83 | 325,047 rows |
  | 4,096 | 1 | 1.05 | 325,047 rows |
  | 8,192 | 10 | 0.35 | `Out of Memory Error: could not allocate block of size 256.0 KiB (12.7 GiB/12.7 GiB used)` |
  | 8,192 | 4 | 4.33 | 799,362 rows |
  | 8,192 | 1 | 1.36 | `Out of Memory Error: failed to allocate data of size 2.0 MiB (12.7 GiB/12.7 GiB used)` |
  | 16,384 | 10 | 0.38 | the same error as 8,192 at 10 threads |
  | 16,384 | 1 | 1.45 | the same error as 8,192 at 1 thread |
  | 32,768 | 1 | 1.62 | the same error as 8,192 at 1 thread |

  No temporary file was written in any run. Why four threads succeeded at
  8,192 sets where one and ten did not was not investigated.
- **SQLite.** No refusal at any size reached: 16,384 sets nested 14 deep
  executed.

### Inspection, selection and interruption

`inspect_grouping()` on a local data frame, seconds and peak R memory:

| Columns (sets) | `.format = "list"` | `.format = "text"` |
|---|---|---|
| 12 (4,096) | 0.09 s | – |
| 14 (16,384) | 0.37 s | – |
| 16 (65,536) | 1.6 s | – |
| 18 (262,144) | 8.8 s, 378 MB | 13.1 s, 400 MB |
| 20 (1,048,576) | 38.6 s, 1,263 MB | 56.0 s, 1,452 MB |
| 22 (4,194,304) | 154 s, 4,722 MB | 208 s, 5,229 MB |

At 65,536 sets the figure was within 0.1 s of 1.6 s on every backend that
reached that size. The series above ran beside other cells; the script's
`inspect` mode, run alone, gave 6.3 s and 376 MB at 18 columns.

A selection helper reached a wide cube without naming a column: on a frame
with six `flag_*` character columns, `cube(starts_with("flag_"))` and
`cube(where(is.character))` each resolved to 64 grouping sets.

A `SIGINT` sent 3.0 s into the construction of a 4,096-set summary was
delivered as an R `interrupt` condition at 3.0 s on a local data frame, dtplyr,
an Arrow table and SQLite.

`setTimeLimit(elapsed = 3, transient = TRUE)` around the construction of a
16,384-set cube ended `summarize_with_margins()` and `expand_with_margins()`
with an error between 3.01 s and 3.05 s on the same four inputs. Seven of the
eight calls reported the time limit. The expansion on the local data frame
did not, and repeated at other limits it ended with the time-limit message,
with an error dplyr's `mutate()` raised around the interrupted step, or with
"missing value where TRUE/FALSE needed", depending on where the limit
struck: 38, 19 and 3 times at 60 limits between 0.3 s and 1.8 s. The last
form's call was the `if` in `dplyr:::new_error_context()`, and it arose without
marginplyr: `repeat mutate(df, k = "Total")` on a 256-row data frame under 300
limits between 0.05 s and 0.15 s ended that way 31 times, with dplyr 1.2.1.

The same limit around `inspect_grouping()` of a 24-column cube (16,777,216
sets) on a two-row local data frame ended it at 3.15 s, by which time R's peak
memory was 1,083 MB. A time limit therefore bounded the time of plan
construction and not its memory.

### Not measured

- Any input larger than 256 rows. Execution times above are those of a
  near-empty table and say nothing about a scan of real data repeated per
  branch.
- Grouping sets that are not a cube. The 2026-09-12 note
  `2026-09-12-performance-scalability-testing.md` proposed a series varying S
  at fixed D and did not run it; neither did this one.
- Contextual shares, `.sort`, `.id`, `.duplicates = "keep"` and the nesting
  verbs.
- Any SQL engine other than SQLite, DuckDB and PostgreSQL executing a
  statement. The limits other engines document are under *Other SQL dialects*
  below and none was probed.
- The PostgreSQL expansion's first failing size between 4,097 and 16,384 sets.

## Backend and library limits (primary sources)

### SQLite and RSQLite

Source read at SQLite tag `version-3.53.3`
(commit [`92a6c5c`][sqlite-tag]), which is the amalgamation RSQLite 3.53.3
vendors: `src/vendor/sqlite3/sqlite3.h` at RSQLite tag `v3.53.3` (commit
`b4102c6f`) defines `SQLITE_VERSION "3.53.3"`. The documentation page is
<https://www.sqlite.org/limits.html>, which carried "This page was last updated
on 2026-07-10" when read on 2026-11-21.

| Limit | Default | Unit | Hard / configurable | Source | Quoted text |
|---|---|---|---|---|---|
| `SQLITE_MAX_COMPOUND_SELECT` | 500 | terms (SELECTs) of one compound SELECT | Compile-time; lowerable per connection with `sqlite3_limit(db, SQLITE_LIMIT_COMPOUND_SELECT, n)`; compile-time `0` disables | [`src/sqliteLimit.h`][sqlite-limit-h]; enforced in `parserDoubleLinkSelect()`, [`src/parse.y`][sqlite-parse-y] | `sqlite3ErrorMsg(pParse, "too many terms in compound SELECT");` |
| `SQLITE_MAX_SQL_LENGTH` | 1,000,000,000 | bytes of statement text | Compile-time, ceiling 2,147,482,624; lowerable with `SQLITE_LIMIT_SQL_LENGTH`; cannot be turned off | [`src/sqliteLimit.h`][sqlite-limit-h]; enforced in `sqlite3Prepare()`, [`src/prepare.c`][sqlite-prepare-c], and in `sqlite3RunParser()`, [`src/tokenize.c`][sqlite-tokenize-c] | `sqlite3ErrorWithMsg(db, SQLITE_TOOBIG, "statement too long");` |
| `SQLITE_MAX_EXPR_DEPTH` | 1000 | expression-tree height; the same number also bounds subquery/VIEW nesting | Compile-time; lowerable with `SQLITE_LIMIT_EXPR_DEPTH` when the compile-time value is positive; `0` omits the checks | [`src/sqliteLimit.h`][sqlite-limit-h]; `sqlite3ExprCheckHeight()`, [`src/expr.c`][sqlite-expr-c]; `sqlite3ResultSetOfSelect()`, [`src/select.c`][sqlite-select-c] | `"Expression tree is too large (maximum depth %d)"`; `"VIEWs and/or subqueries nested too deep"` |
| `SQLITE_MAX_PARSER_DEPTH` | 2500 | LALR(1) parser stack entries | Compile-time; lowerable with `SQLITE_LIMIT_PARSER_DEPTH` (added to `sqlite3_limit()` in 3.53.0) | [`src/sqliteLimit.h`][sqlite-limit-h]; `%stack_size 50`, `%stack_size_limit parserStackSizeLimit`, `%stack_overflow` in [`src/parse.y`][sqlite-parse-y] | `if( pParse->nErr==0 ) sqlite3ErrorMsg(pParse, "Recursion limit");` |
| `SQLITE_MAX_COLUMN` | 2000 | columns in a table, index, view, SELECT result set; terms in GROUP BY / ORDER BY | Compile-time, ceiling 32767; lowerable with `SQLITE_LIMIT_COLUMN` | [`src/sqliteLimit.h`][sqlite-limit-h] | `# define SQLITE_MAX_COLUMN 2000` / `# error SQLITE_MAX_COLUMN may not exceed 32767` |
| `SQLITE_MAX_VDBE_OP` | 250,000,000 | opcodes in one prepared statement | Compile-time; lowerable with `SQLITE_LIMIT_VDBE_OP` | [`src/sqliteLimit.h`][sqlite-limit-h]; `SQLITE_LIMIT_VDBE_OP` in [`src/sqlite.h.in`][sqlite-h-in] | "If [sqlite3_prepare_v2()] or the equivalent tries to allocate space for more than this many opcodes in a single prepared statement, an SQLITE_NOMEM error is returned." |

[sqlite-tag]: https://github.com/sqlite/sqlite/tree/92a6c5c3636faa021ecc3be5403a00f50f65eda7
[sqlite-limit-h]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/sqliteLimit.h
[sqlite-parse-y]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/parse.y
[sqlite-prepare-c]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/prepare.c
[sqlite-tokenize-c]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/tokenize.c
[sqlite-expr-c]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/expr.c
[sqlite-select-c]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/select.c
[sqlite-h-in]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/sqlite.h.in
[sqlite-main-c]: https://github.com/sqlite/sqlite/blob/92a6c5c3636faa021ecc3be5403a00f50f65eda7/src/main.c
[rsqlite-makevars]: https://github.com/r-dbi/RSQLite/blob/b4102c6f9b6abe33fd4911760d157b301721295a/src/Makevars

**Why the compound limit exists.** `src/sqliteLimit.h`: "The code generator for
compound SELECT statements does one level of recursion for each term. A stack
overflow can result if the number of terms is too large. In practice, most SQL
never has more than 3 or 4 terms. Use a value of 0 to disable any limit on the
number of terms in a compound SELECT." `limits.html` says the same: "The code
generator in SQLite processes compound SELECT statements using a recursive
algorithm. In order to limit the size of the stack, we therefore limit the
number of terms in a compound SELECT. … We think this is a generous allotment
since in practice we almost never see the number of terms in a compound select
exceed single digits." The recursion is `multiSelect()` in `src/select.c`
calling `sqlite3Select(pParse, pPrior, &dest)` on the SELECT to its left.

**What it counts, and whether a subquery resets it.** `parserDoubleLinkSelect()`
walks one `Select.pPrior` chain and counts its links (`cnt++` per `pPrior`
step), then compares `cnt>mxSelect`. A parenthesised subquery in a FROM clause
is a separate `Select` object attached through the `SrcList`, with its own
`pPrior` chain, so each nesting level is counted on its own; the count is per
compound, not per statement. The check sits in the parser and nowhere else:
`aLimit[SQLITE_LIMIT_COMPOUND_SELECT]` is read once in `src/parse.y`, and the
only other mention in `src/select.c` is the comment above
`multiSelectValues()`. The grammar rule is left-recursive
(`selectnowith ::= selectnowith multiselect_op oneselect`), so a flat compound
does not deepen the parser stack per term.

The query flattener runs after that check and can merge a `UNION ALL` subquery
into its parent. Its restriction (17) in `flattenSubquery()`, `src/select.c`,
refuses when "(17b) no terms within the subquery compound may be aggregate or
DISTINCT" fails, or when "(17d) the outer query may not be (17d1) aggregate, or
(17d2) DISTINCT" fails. `flattenSubquery()` reads no
`SQLITE_LIMIT_COMPOUND_SELECT`. Whether a compound that flattening has grown
past 500 arms is then code-generated by the same per-term recursion was not
traced further.

**Subquery nesting depth.** There is no separate constant. `SQLITE_LIMIT_EXPR_DEPTH`
is documented in `src/sqlite.h.in` as "The maximum depth of the parse tree on
any expression and the maximum nesting depth for subqueries and VIEWs", and
`sqlite3ResultSetOfSelect()` raises "VIEWs and/or subqueries nested too deep"
when `pParse->nNestSel >= db->aLimit[SQLITE_LIMIT_EXPR_DEPTH]`. A balanced
pairwise tree over S arms nests ⌈log2 S⌉ deep: 12 for 4,096 arms, 16 for
65,536.

**Parser stack.** `YYSTACKDEPTH` is the Lemon template's name for the initial
stack size; SQLite 3.53.3 sets it through `%stack_size 50` and lets the stack
grow on the heap (`yyGrowStack()` in `tool/lempar.c`: `newSize = oldSize*2 + 100`)
up to `SQLITE_LIMIT_PARSER_DEPTH`. The two SQLite sources disagree on when
heap growth arrived: `src/sqliteLimit.h` says "Prior to version 3.45.0
(2024-01-15), the parser stack was hard-coded to 100 entries", while
<https://www.sqlite.org/changes.html> lists "Allocate additional memory from
the heap for the SQL parser stack if that stack overflows, rather than
reporting a "parser stack overflow" error" under 3.46.0 (2024-05-23). The
message at 3.53.3 is "Recursion limit", not "parser stack overflow".

**`sqlite3_limit()` only lowers.** `sqlite3_limit()` in
[`src/main.c`][sqlite-main-c]: `if( newLimit>aHardLimit[limitId] ){ newLimit =
aHardLimit[limitId]; }`, where `aHardLimit[]` holds the compile-time
`SQLITE_MAX_*` values. `limits.html` words every run-time entry as "can be
lowered at run-time". All six limits in the table have a `SQLITE_LIMIT_*`
category in `src/sqlite.h.in`.

**RSQLite 3.53.3.** [`src/Makevars`][rsqlite-makevars] at tag `v3.53.3`
overrides one limit, `-DSQLITE_MAX_LENGTH=2147483647`; every other limit takes
the `sqliteLimit.h` default. `PRAGMA compile_options` on the installed RSQLite
3.53.3 returned, on 2026-11-21, `MAX_COMPOUND_SELECT=500`,
`MAX_SQL_LENGTH=1000000000`, `MAX_EXPR_DEPTH=1000`, `MAX_COLUMN=2000`,
`MAX_VDBE_OP=250000000`, `MAX_LENGTH=2147483647`; it listed no
`MAX_PARSER_DEPTH` entry. RSQLite's `NEWS.md` records that an earlier release
raised and then reverted two limits: "Compilation limits
`SQLITE_MAX_VARIABLE_NUMBER` and `SQLITE_MAX_COLUMN` have been reset to the
defaults. The documentation suggests setting to such high values is a bad
idea."

RSQLite 3.53.3 exposes no way to change a limit and no dedicated way to read
one: none of the 120 files matching `src/*.{c,cpp,h}` and `R/*.R` at tag
`v3.53.3` (vendored code excluded) contains `sqlite3_limit` or `SQLITE_LIMIT_`,
and the installed namespace has no object whose name matches `limit`. The
compile-time values are readable through `PRAGMA compile_options` as above.
<https://www.sqlite.org/pragma.html> lists no pragma for the compound-select,
SQL-length, expression-depth, parser-depth or column limits; the pragmas with
"limit" in their names are `analysis_limit`, `hard_heap_limit`,
`journal_size_limit` and `soft_heap_limit`.

### PostgreSQL

Source read at tag `REL_17_11` (commit [`083ac03`][pg-tag]); the live server
reported `PostgreSQL 17.11 (Homebrew)` and `max_stack_depth = 2MB` on
2026-11-21.

| Limit | Value | Unit | Hard / configurable | Source | Quoted text |
|---|---|---|---|---|---|
| Grouping sets in one query level | 4096 | expanded grouping sets (product of the lengths of every top-level `GROUP BY` item's expansion) | Hard-coded literal, no GUC | `parseCheckAggregates()` and `expand_grouping_sets()`, [`src/backend/parser/parse_agg.c`][pg-parse-agg] | `errmsg("too many grouping sets present (maximum 4096)")`, `ERRCODE_STATEMENT_TOO_COMPLEX`; comment: "The limit of 4096 is arbitrary and exists simply to avoid resource issues from pathological constructs." |
| `CUBE(...)` elements | 12 | elements of one `CUBE` | Hard-coded literal, no GUC | `transformGroupingSet()`, [`src/backend/parser/parse_clause.c`][pg-parse-clause] | `errmsg("CUBE is limited to 12 elements")`, `ERRCODE_TOO_MANY_COLUMNS`; comment: "Arbitrarily cap the size of CUBE, which has exponential growth" |
| `GROUPING(...)` arguments | 31 | arguments | Hard-coded; follows from the `integer` bit-mask result | `transformGroupingFunc()`, [`src/backend/parser/parse_agg.c`][pg-parse-agg] | `if (list_length(args) > 31)` … `errmsg("GROUPING must have fewer than 32 arguments")`, `ERRCODE_TOO_MANY_ARGUMENTS` |
| Recursion depth of the set-operation tree | `max_stack_depth`, default 2MB (`InitializeGUCOptionsFromEnvironment()` in `guc.c` sets `min(rlimit − STACK_DEPTH_SLOP, 2048)` kB at startup over the table's boot value of 100) | kilobytes of backend stack | `PGC_SUSET`: "Only superusers and users with the appropriate `SET` privilege can change this setting"; capped at the kernel stack rlimit minus `STACK_DEPTH_SLOP` (`512 * 1024L` bytes, `src/include/tcop/tcopprot.h`) | `check_stack_depth()`, [`src/backend/tcop/postgres.c`][pg-postgres-c]; called from `transformSetOperationTree()`, [`src/backend/parser/analyze.c`][pg-analyze-c], and `recurse_set_operations()`, [`src/backend/optimizer/prep/prepunion.c`][pg-prepunion-c]; [`doc/src/sgml/config.sgml`][pg-config-sgml] | `errmsg("stack depth limit exceeded")`, `errhint("Increase the configuration parameter \"max_stack_depth\" (currently %dkB), after ensuring the platform's stack depth limit is adequate.")` |
| Simple-query message length | `MaxAllocSize - 1` = 1,073,741,822 | bytes | Hard-coded | `SocketBackend()`, [`src/backend/tcop/postgres.c`][pg-postgres-c] (`case PqMsg_Query: maxmsglen = PQ_LARGE_MESSAGE_LIMIT;`); [`src/include/libpq/libpq.h`][pg-libpq-h]; [`src/include/utils/memutils.h`][pg-memutils-h] | `#define PQ_LARGE_MESSAGE_LIMIT (MaxAllocSize - 1)`; `#define MaxAllocSize ((Size) 0x3fffffff) /* 1 gigabyte - 1 */` |

[pg-tag]: https://github.com/postgres/postgres/tree/083ac033419f690758508e08c1736089384bbee8
[pg-parse-agg]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/backend/parser/parse_agg.c
[pg-parse-clause]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/backend/parser/parse_clause.c
[pg-postgres-c]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/backend/tcop/postgres.c
[pg-analyze-c]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/backend/parser/analyze.c
[pg-prepunion-c]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/backend/optimizer/prep/prepunion.c
[pg-config-sgml]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/doc/src/sgml/config.sgml
[pg-libpq-h]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/include/libpq/libpq.h
[pg-memutils-h]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/include/utils/memutils.h
[pg-planner-c]: https://github.com/postgres/postgres/blob/083ac033419f690758508e08c1736089384bbee8/src/backend/optimizer/plan/planner.c

**The 4096 cap applies to an explicitly written list.** `parseCheckAggregates()`
calls `expand_grouping_sets(qry->groupingSets, qry->groupDistinct, 4096)` for
every query that has grouping sets, whatever their spelling.
`expand_grouping_sets()` multiplies `numsets *= list_length(current_result)`
over the top-level items and returns `NIL` when `numsets > limit`; for a
`GROUPING_SET_SETS` node, `expand_groupingset_node()` concatenates one entry
per listed set. So a single `GROUPING SETS ((..),(..),…)` list contributes its
length, and the comparison is strict: 4096 passes, 4097 fails. The count is
taken before duplicate sets are removed (deduplication runs only under
`GROUP BY DISTINCT`, after the limit check). A probe against the live 17.11
server on 2026-11-21 agreed: an explicit list of 4,096 sets planned, 4,097
raised `ERROR: too many grouping sets present (maximum 4096)`; `cube()` of 13
columns raised `ERROR: CUBE is limited to 12 elements`; `cube()` of 12 columns
returned 4,096 rows; `cube(a..f), cube(a..g)` (64 × 128 = 8,192) raised the
4096 error; `grouping()` with 32 arguments raised `ERROR: GROUPING must have
fewer than 32 arguments`.

The CUBE cap is on the `CUBE` keyword only. An explicit list is not subject
to it, so 4,096 explicit sets over more than 12 columns pass the parser. The
planner relies on the 4096 cap: [`planner.c`][pg-planner-c]'s comment above
`extract_rollup_sets()` says "Since our N is at most 4096, we don't need to
consider fallbacks to heuristic or approximate methods. (Planning time for a
12-d cube is under half a second on my modest system even with optimization
off and assertions on.)"

None of the three caps is documented in the places read:
`doc/src/sgml/queries.sgml` (the `GROUPING SETS`, `CUBE`, `ROLLUP` section) has
no occurrence of "4096", "12 elements" or "limited to";
`doc/src/sgml/limits.sgml` (Appendix K) has no row for grouping sets, set
operations or statement length; the `GROUPING` entry in `func.sgml` says
"Returns a bit mask" with return type `integer` and states no argument limit.

**Flat `UNION ALL` of thousands of arms.** The grammar rule
`select_clause UNION set_quantifier select_clause` in `gram.y` builds a binary
`SetOperationStmt` tree, left-deep for an unparenthesised chain, and
`transformSetOperationTree()` recurses into it with "Guard against stack
overflow due to overly complex set-expressions" → `check_stack_depth()`. The
planner's `plan_union_children()` flattens nested `UNION ALL` iteratively
through a `pending_rels` list, but `recurse_set_operations()` also calls
`check_stack_depth()`. `config.sgml` on the default: "The default setting is
two megabytes (`2MB`), which is conservatively small and unlikely to risk
crashes. However, it might be too small to allow execution of complex
functions", and on why the margin: "the stack depth is not checked in every
routine in the server, but only in key potentially-recursive routines." No
arm-count constant exists; the bound is stack bytes. A probe on 2026-11-21
against the live 17.11 server at `max_stack_depth = 2MB`, with trivial arms
(`EXPLAIN (COSTS OFF) SELECT 1 AS a UNION ALL SELECT 1 AS a …`): 1,024, 4,096
and 8,192 arms planned; 16,384 arms (393 kB of SQL) raised `ERROR: stack depth
limit exceeded` with the hint quoted in the table. The first failing arm
count between 8,192 and 16,384 was not bisected, and marginplyr's own branches
were not used for the probe.

### DuckDB

Source read at tag `v1.5.5` (commit [`d8cdaa3`][duckdb-tag]). The installed R
package duckdb 1.5.5 reported `version() = v1.5.5` and `source_id =
d8cdaa33fda` on 2026-11-21, so the tag is the code that ran. Documentation
read from `duckdb/duckdb-web` at commit `0a5c7c90`, directory `docs/1.5/`.

| Limit | Value | Unit | Hard / configurable | Source | Quoted text |
|---|---|---|---|---|---|
| Grouping sets in one `GROUP BY` | 65,535 | grouping sets after expansion (explicit list entries, `ROLLUP`/`CUBE` expansion, and cross products) | Hard-coded `static constexpr`, no setting | `CheckGroupingSetMax()`, [`src/parser/transform/helpers/transform_groupby.cpp`][duckdb-groupby] | `static constexpr const idx_t MAX_GROUPING_SETS = 65535;` / `throw ParserException("Maximum grouping set count of %d exceeded", MAX_GROUPING_SETS);` |
| `GROUPING()` / `GROUPING_ID()` arguments | 63 | arguments | Hard-coded; result type is `BIGINT` | `BaseSelectBinder::BindGroupingFunction()`, [`src/planner/expression_binder/base_select_binder.cpp`][duckdb-select-binder] | `if (op.children.size() >= 64) { return BindResult(BinderException(op, "GROUPING statement cannot have more than 64 groups")); }` |
| `max_expression_depth` | 1000 | recursion levels in the transformer, the binder and the expression binder | Setting, `SET max_expression_depth TO x`; scope `local`, type `UBIGINT` | [`src/common/settings.json`][duckdb-settings]; `Transformer::StackCheck()`, [`src/parser/transformer.cpp`][duckdb-transformer]; `Binder::IncreaseDepth()`, [`src/planner/binder.cpp`][duckdb-binder] | `"Max expression depth limit of %lld exceeded. Use \"SET max_expression_depth TO x\" to increase the maximum expression depth."`; description: "The maximum expression depth limit in the parser. WARNING: increasing this setting and using very deep expressions might lead to stack overflow errors." |

[duckdb-tag]: https://github.com/duckdb/duckdb/tree/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa
[duckdb-groupby]: https://github.com/duckdb/duckdb/blob/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa/src/parser/transform/helpers/transform_groupby.cpp
[duckdb-select-binder]: https://github.com/duckdb/duckdb/blob/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa/src/planner/expression_binder/base_select_binder.cpp
[duckdb-settings]: https://github.com/duckdb/duckdb/blob/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa/src/common/settings.json
[duckdb-transformer]: https://github.com/duckdb/duckdb/blob/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa/src/parser/transformer.cpp
[duckdb-binder]: https://github.com/duckdb/duckdb/blob/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa/src/planner/binder.cpp
[duckdb-select-node]: https://github.com/duckdb/duckdb/blob/d8cdaa33fda8df955cc76ef58a280f68f4cd43fa/src/parser/transform/statement/transform_select_node.cpp
[duckdb-limits-doc]: https://github.com/duckdb/duckdb-web/blob/0a5c7c904b54804733000c1d0d9b9a51ed4d5b19/docs/1.5/operations_manual/limits.md
[duckdb-gs-doc]: https://github.com/duckdb/duckdb-web/blob/0a5c7c904b54804733000c1d0d9b9a51ed4d5b19/docs/1.5/sql/query_syntax/grouping_sets.md

**The 65,535 cap applies to an explicit list, and a cube of 16 columns is one
over it.** `Transformer::TransformGroupBy()` calls
`CheckGroupingSetMax(result_sets.size())` after transforming each top-level
`GROUP BY` item, and again on the product when it forms a cross product; the
`GROUPING_SET_SETS` case appends one entry per listed set, and the `CUBE` case
calls `CheckGroupingSetCubes()` before expanding. The comparison is
`count > MAX_GROUPING_SETS`, so 65,535 sets pass and 65,536 (= 2^16) fail.
Probes on the installed duckdb 1.5.5 on 2026-11-21: an explicit
`GROUPING SETS` list of 65,536 sets and `CUBE` over 16 columns both raised
`Parser Error: Maximum grouping set count of 65535 exceeded`; `CUBE` over 15
columns and an explicit list of 1,024 sets were accepted by `EXPLAIN`.

**`GROUPING()` message and condition differ by one.** The condition rejects
64 or more arguments while the message says "more than 64". Probes: 63
arguments returned a row; 64 raised `Binder Error: GROUPING statement cannot
have more than 64 groups`, for both `grouping(...)` and `grouping_id(...)`
(the grammar accepts `GROUPING_ID` as a spelling of the same construct,
`third_party/libpg_query/grammar/statements/select.y`).
[`grouping_sets.md`][duckdb-gs-doc] documents `GROUPING_ID()` as returning
`BIGINT` and states no argument limit and no grouping-set limit.

**Wide `UNION ALL`.** No arm-count limit was found. At v1.5.5
`Transformer::TransformSetOperationChildren()` in
[`transform_select_node.cpp`][duckdb-select-node] unfolds a chain of matching
`UNION`/`UNION ALL` nodes iteratively into one `SetOperationNode` with a
`children` vector ("only generate multi-child nodes for UNION/UNION ALL"), so
a flat chain adds one level of `StackCheck()` rather than one per arm. The
unfolding uses `set_operations.insert(set_operations.begin() + (i + 1), …)`
on a `vector`, which is linear per insertion. A probe on 2026-11-21 with
trivial arms (`EXPLAIN SELECT 1 AS a UNION ALL …`) was accepted at 4,096 arms
(0.1 s), 16,384 arms (0.8 s) and 65,536 arms (17.1 s, 1.6 MB of SQL); these
three timings were taken while another agent's benchmarks were running on the
same machine and are not a measurement of record.

**Documented limits.** [`limits.md`][duckdb-limits-doc] lists "Expression
depth | 1000 | `max_expression_depth`" and no row for grouping sets, set
operations or statement length; its only string row is "String size | 4 GB".

### dbplyr 2.6.0 union construction and rendering

Source read at tag `v2.6.0` (commit [`a4591fd`][dbplyr-tag], committed
2026-06-17); "main" below is commit `f478e207` (2026-06-17), the head of
`tidyverse/dbplyr` when read on 2026-11-21.

| Stage | Function (file) | Cost in the number of arms S | What the source does |
|---|---|---|---|
| Construct | `union_all.tbl_lazy()` → `add_union()` ([`R/verb-set-ops.R`][dbplyr-set-ops]); `op_vars.lazy_union_query()` ([`R/query-union.R`][dbplyr-query-union]) | Θ(m) per call, m = arms already held by the two operands. Left fold: Θ(S²). Balanced pairwise tree of bare unions: Θ(S log S). Operands that are `lazy_select_query` (a union wrapped in a SELECT): Θ(1) per call, Θ(S) total | Calls `op_vars(x)` and `op_vars(y)`; `op_vars.lazy_union_query()` is `purrr::reduce(op$unions$table, \(acc, table) union(acc, op_vars(table$lazy_query)), .init = op_vars(op$x))`, which recurses into nested unions, so it visits every arm. When `x` is already a `lazy_union_query`, it then re-runs `fill_vars()` (another `op_vars()`) over every table already in the list and appends with `c(x_lq$unions$table, list(y))` |
| Build | `sql_build.lazy_union_query()` ([`R/query-union.R`][dbplyr-query-union]); `sql_build.lazy_select_query()`, `get_select_sql()`, `translate_select_sql()` ([`R/query-select.R`][dbplyr-query-select]); `translate_sql_()` ([`R/translate-sql.R`][dbplyr-translate-sql]) | Θ(S) `sql_build()` calls; each SELECT makes a fixed number of `translate_sql_()` calls plus, for a non-`summarise` SELECT that is not a bare `*`, one per output column | No step scans the other arms. Every `translate_sql_()` call with non-empty `dots` runs `variant <- dbplyr_sql_translation(con)`, constructing the dialect's whole `sql_variant` again |
| Optimise | `sql_optimise()` ([`R/backward-compatibility.R`][dbplyr-backcompat]) | none | Roxygen: "Optimise a SQL query / No longer used."; `sql_render.lazy_query()` ([`R/sql-build.R`][dbplyr-sql-build]) calls `sql_build()` then `sql_render()` with no optimise step |
| Render, default | `sql_render.union_query()`, `sql_query_union.DBIConnection()` ([`R/query-union.R`][dbplyr-query-union]); `sql_render.select_query()` ([`R/query-select.R`][dbplyr-query-select]); `sql_indent_subquery()` ([`R/db-sql.R`][dbplyr-db-sql]) | Θ(S) render calls; bytes copied Θ(B·d), B = SQL bytes, d = nesting depth of unions or subqueries (1 for one flat list, log2 S for a balanced tree) | Each union level does `paste0(x, "\n\n", paste0(union_clauses, collapse = "\n\n"))`; each subquery level runs `grepl(x = from, pattern = "\\r\\n\|\\r\|\\n")` over the whole subquery text and pastes it again |
| Render, `sql_options(cte = TRUE)` | `flatten_query.union_query()` ([`R/query-union.R`][dbplyr-query-union]); `querylist_reuse_query()` ([`R/sql-build.R`][dbplyr-sql-build]) | Θ(q²) in the number of queries q | `querylist_reuse_query()` matches each query against all earlier ones with `vctrs::vec_match(list(unclass(qry)), purrr::map(query_list$queries, unclass))` and grows the list with `c(query_list$queries, wrapped_query)` |

[dbplyr-tag]: https://github.com/tidyverse/dbplyr/tree/a4591fd40ae59d03ee70a9178df37606f9f5b8c7
[dbplyr-set-ops]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/verb-set-ops.R
[dbplyr-query-union]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/query-union.R
[dbplyr-query-select]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/query-select.R
[dbplyr-translate-sql]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/translate-sql.R
[dbplyr-backcompat]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/backward-compatibility.R
[dbplyr-sql-build]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/sql-build.R
[dbplyr-db-sql]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/db-sql.R
[dbplyr-sqlite]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/backend-sqlite.R
[dbplyr-bind-queries]: https://github.com/tidyverse/dbplyr/blob/a4591fd40ae59d03ee70a9178df37606f9f5b8c7/R/bind-queries.R

**Where nested unions are flattened.** In two places, neither of which is a
tree rewrite. (1) `add_union()` appends to the left operand only: when
`x$lazy_query` inherits from `lazy_union_query` it pushes `y` onto
`x_lq$unions$table` and returns; a `y` that is itself a union stays one nested
table. (2) `sql_render.union_query()` renders every table with
`subquery = FALSE`, so a nested union comes out as unparenthesised text and
the statement reads as one flat compound whatever the tree shape was. dbplyr's
own many-table helper is the left fold: `bind_queries()` in
[`R/bind-queries.R`][dbplyr-bind-queries] is documented as "a convenient
wrapper around `purrr::reduce(tables, union_all)`".

**The SQLite version lookup is uncached and runs per translation, not per
SELECT.** [`R/backend-sqlite.R`][dbplyr-sqlite] defines
`sqlite_version <- function() { numeric_version(RSQLite::rsqliteVersion()[[2]]) }`
and `sql_translation.sql_dialect_sqlite()` builds its `sql_variant(...)` with
`if (sqlite_version() >= "3.25") { sql_translator(.parent = base_win, …) } else { base_no_win }`
as the third argument. Nothing stores the result: `sql_translation()` in
`R/db-sql.R` is a bare `UseMethod("sql_translation", sql_dialect(con))`,
`dbplyr_sql_translation()` is `check_2ed(con); sql_translation(con)`, and a
grep of `R/` at `v2.6.0` for `cache|memoi` matches only `R/data-cache.R`
(`cache_computation()`, used for the `memdb`, nycflights13 and Lahman
connections) and prose in `R/verb-compute.R`. The caller is `translate_sql_()`,
which returns early on empty `dots` and otherwise reaches
`variant <- dbplyr_sql_translation(con)` on every call. Per
`sql_build.lazy_select_query()` that is one call for a `summarise` SELECT
list, one per column in `translate_select_sql()` otherwise (run twice when the
dialect has a `WINDOW` clause and windows were registered), and one each for
non-empty `where`, `group_by`, `having` and `order_by`. A branch that is
`SELECT <aggregates> … GROUP BY <keys>` therefore constructs the variant, and
calls `sqlite_version()`, twice. `window_funs()` in
`R/translate-sql-window.R`, `env_names(sql_translation(con)$window)`, adds one
more per `filter()` at verb time (`uses_window_fun()` in `R/verb-filter.R`).

**dbplyr main and the tracker.** `R/backend-sqlite.R`, `R/translate-sql.R` and
`R/db-sql.R` at main `f478e207` carry the same three definitions; main's
`NEWS.md` had an empty "dbplyr (development version)" section. Issue and PR
searches on `tidyverse/dbplyr` on 2026-11-21 for `sqlite_version`,
`rsqliteVersion`, `sql_variant cache`, `sql_translation slow`, `union_all
slow`, `slow render`, `cache translation`, `union many tables` and `benchmark`
returned nothing; `sql_translation cache` returned only closed issue #1624
("De-couple class from sql translation"), `translate_sql_ performance` only
closed issue #1104, and open-only searches for `performance` and `memoise`
returned #1801, #1620 and #1870, none about translation cost.

### Other SQL dialects

Documentation and source as read on 2026-11-21. "Not found" means the page
named in the cell was searched for the terms listed under
*Searched for and not found* and has no such statement; it is not a claim that
the engine has no limit.

| Engine | (a) Statement text size | (b) UNION / compound terms, subquery nesting | (c) Engine's own cap on native grouping sets |
|---|---|---|---|
| SQL Server | "Byte length of a string containing Transact-SQL statements (batch size) \| 65,536 * (network packet size)"; "The default packet size is 4 KB" (= 268,435,456 bytes at the default) — [capacity spec][ms-cap] | "Nested subqueries \| 32" — [capacity spec][ms-cap]. No count limit on `UNION` terms in [UNION][ms-union]. Error 8623: "The query processor ran out of internal resources and could not produce a query plan. This is a rare event and only expected for extremely complex queries or queries that reference a very large number of tables or partitions. Please simplify the query." — [MSSQLSERVER_8623][ms-8623] | "Number of grouping sets generated by operators in the `GROUP BY` clause \| 4,096"; "Number of distinct expressions in the `GROUP BY` clause when any of the following are present: `CUBE`, `ROLLUP`, `GROUPING SETS`, `WITH CUBE`, `WITH ROLLUP` \| 32"; "Columns or expressions in a `GROUP BY WITH CUBE` or `GROUP BY WITH ROLLUP` statement \| 10" — [capacity spec][ms-cap]. Hard; "duplicate grouping sets aren't eliminated" — [GROUP BY][ms-groupby] |
| Trino | `query.max-length`: default 1,000,000, maximum 1,000,000,000 characters; "Longer queries are not processed, and terminated with error `QUERY_TEXT_TOO_LARGE`" — [query management properties][trino-props] | `query.max-stage-count`: default 150; "If a query generates more stages than this it will get killed with error `QUERY_HAS_TOO_MANY_STAGES`" — [query management properties][trino-props]. No count limit on `UNION` terms found there | 2048 grouping sets, configurable: `private int maxGroupingSets = 2048;` under `@Config("analyzer.max-grouping-sets")` in [`FeaturesConfig.java`][trino-features], and session property `max_grouping_sets` ("Maximum number of grouping sets in a GROUP BY"). Message from [`StatementAnalyzer.java`][trino-analyzer]: "GROUP BY has %s grouping sets but can contain at most %s", error code `TOO_MANY_GROUPING_SETS` |
| Spark SQL (v4.3.0) | Not found in the sources read | Not found in the sources read | No cap on the number of sets: `cubeExprs()` / `rollupExprs()` in [`grouping.scala`][spark-grouping] expand without a count check. Cap on grouping expressions: `if (groupByExprs.size > GroupingID.dataType.defaultSize * 8) { throw QueryCompilationErrors.groupingSizeTooLargeError(…) }` in [`Analyzer.scala`][spark-analyzer], i.e. 64 (`LongType`), or 32 when `SQLConf.get.integerGroupingIdEnabled`; message `GROUPING_SIZE_LIMIT_EXCEEDED`: "Grouping sets size cannot be greater than <maxSize>." (SQLSTATE 54000) |
| Snowflake | Not found: <https://docs.snowflake.com/en/sql-reference/operators-query> and the 40 section indexes linked from <https://docs.snowflake.com/llms.txt> | Not found in <https://docs.snowflake.com/en/sql-reference/operators-query> | "Snowflake allows up to 128 grouping sets in the same query block." — <https://docs.snowflake.com/en/sql-reference/constructs/group-by-grouping-sets>; "Snowflake allows up to 7 elements (equivalent to 128 grouping sets) in each cube." — <https://docs.snowflake.com/en/sql-reference/constructs/group-by-cube>. No configuration option named on either page |
| BigQuery | "An unresolved GoogleSQL query can be up to 1 MB long. If your query is longer, you receive the following error: The query is too large."; "Maximum resolved legacy and GoogleSQL query length \| 12 MB" — <https://cloud.google.com/bigquery/quotas> (page dated 2026-11-20) | Not found in <https://cloud.google.com/bigquery/quotas> | "GROUPING SETS allows up to 4096 groupable items from at most 50 unique columns."; "ROLLUP allows up to 4095 groupable items (equivalent to 4096 grouping sets) from at most 50 unique columns."; "CUBE allows up to 12 groupable items (equivalent to 4096 grouping sets)." — <https://cloud.google.com/bigquery/docs/reference/standard-sql/query-syntax> (page dated 2026-11-10) |
| Redshift | "The maximum size for a single Amazon Redshift SQL statement is 16 MB." — <https://docs.aws.amazon.com/redshift/latest/dg/c_redshift-sql.html> | Not found in <https://docs.aws.amazon.com/redshift/latest/mgmt/amazon-redshift-limits.html> | "The GROUP BY clause supports up to 64 grouping sets. In the case of ROLLUP and CUBE, or some combination of GROUPING SETS, ROLLUP, and CUBE, this limitation applies to the implied number of grouping sets. For example, GROUP BY CUBE((a), (b)) counts as 4 grouping sets, not 2." — <https://docs.aws.amazon.com/redshift/latest/dg/r_GROUP_BY_aggregation-extensions.html> |
| MySQL 8.4 | `max_allowed_packet`: "Default Value 67108864", "Minimum Value 1024", "The protocol limit for max_allowed_packet is 1GB"; scope Global and Session, dynamic — <https://dev.mysql.com/doc/refman/8.4/en/server-system-variables.html> | `constexpr const int MAX_SELECT_NESTING{sizeof(nesting_map) * 8 - 1};` in [`sql/sql_lex.h`][mysql-lex]; `Query_term::validate_structure()` in [`sql/query_term.cc`][mysql-query-term] raises `ER_TOO_HIGH_LEVEL_OF_NESTING_FOR_SELECT`, "Too high level of nesting for select", when `level > MAX_SELECT_NESTING`. No count limit on `UNION` terms in <https://dev.mysql.com/doc/refman/8.4/en/union.html> | No native grouping sets: <https://dev.mysql.com/doc/refman/8.4/en/group-by-modifiers.html> documents `WITH ROLLUP` only and has no occurrence of "GROUPING SETS" or "CUBE" |
| MariaDB | `max_allowed_packet`: "Default Value: 16777216 (16M) 1073741824 (1GB) (client-side)", "Range: 1024 to 1073741824" — <https://mariadb.com/docs/server/server-management/variables-and-modes/server-system-variables> | Not found on that page | No native grouping sets: <https://mariadb.com/docs/server/reference/sql-statements/data-manipulation/selecting-data/group-by> documents `WITH ROLLUP` and has no occurrence of "GROUPING SETS" or "CUBE" |
| Oracle 19c | "The limit on how long a SQL statement can be depends on many factors, including database configuration, disk space, and memory" — <https://docs.oracle.com/en/database/oracle/oracle-database/19/refrn/logical-database-limits.html> | "Maximum levels of subqueries in a SQL statement \| 255 subqueries in the WHERE clause \| A limit is not defined for subqueries in the FROM clause of the top-level query." — same page. No row for set operators | Not found: the Logical Database Limits page has no occurrence of "GROUP BY"; "Restrictions on the GROUP BY Clause" in <https://docs.oracle.com/en/database/oracle/oracle-database/19/sqlrf/SELECT.html> lists LOB/nested-table/varray columns, scalar subquery expressions and object-type columns, and no count |
| Databricks | Not found in <https://docs.databricks.com/aws/en/resources/limits> (page dated 2026-11-13), which lists account, workspace and API quotas only | Not found there | Not read separately; Databricks SQL is Spark-derived, and whether the Spark row's source applies to it was not established |

[mysql-lex]: https://github.com/mysql/mysql-server/blob/bf5a74e48423ac51200987edcd72a678772cb2ee/sql/sql_lex.h
[mysql-query-term]: https://github.com/mysql/mysql-server/blob/bf5a74e48423ac51200987edcd72a678772cb2ee/sql/query_term.cc

For the engines that took marginplyr's portable `UNION ALL` path on
2026-11-21, column (c) was not a limit on that path: it is the engine's own
choice of cap. Columns (a) and (b) were the ones the portable statement met.
The MySQL source was read at `trunk` commit `bf5a74e4`, not at an 8.4 tag.


[ms-cap]: https://github.com/MicrosoftDocs/sql-docs/blob/c19cfab3bec52d6eead896900478f67468f4fe2d/docs/sql-server/maximum-capacity-specifications-for-sql-server.md
[ms-groupby]: https://github.com/MicrosoftDocs/sql-docs/blob/c19cfab3bec52d6eead896900478f67468f4fe2d/docs/t-sql/queries/select-group-by-transact-sql.md
[ms-union]: https://github.com/MicrosoftDocs/sql-docs/blob/c19cfab3bec52d6eead896900478f67468f4fe2d/docs/t-sql/language-elements/set-operators-union-transact-sql.md
[ms-8623]: https://github.com/MicrosoftDocs/sql-docs/blob/c19cfab3bec52d6eead896900478f67468f4fe2d/docs/relational-databases/errors-events/mssqlserver-8623-database-engine-error.md
[trino-props]: https://github.com/trinodb/trino/blob/6678854c112d055a2f093e61a3a219003a52a5be/docs/src/main/sphinx/admin/properties-query-management.md
[trino-features]: https://github.com/trinodb/trino/blob/6678854c112d055a2f093e61a3a219003a52a5be/core/trino-main/src/main/java/io/trino/FeaturesConfig.java
[trino-analyzer]: https://github.com/trinodb/trino/blob/6678854c112d055a2f093e61a3a219003a52a5be/core/trino-main/src/main/java/io/trino/sql/analyzer/StatementAnalyzer.java
[spark-grouping]: https://github.com/apache/spark/blob/98bc0212c94705434471627068f41e374ccab143/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/expressions/grouping.scala
[spark-analyzer]: https://github.com/apache/spark/blob/98bc0212c94705434471627068f41e374ccab143/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/analysis/Analyzer.scala

### How existing caps are shaped

Each row repeats a limit sourced in the subsections above, or sources it here
for the libraries.

| Engine / library | Cap | Unit | Outcome | Overridable | Message |
|---|---|---|---|---|---|
| PostgreSQL 17.11 | 4096 | grouping sets (expanded; explicit lists included) | Error at parse analysis | No (literal in `parse_agg.c`) | "too many grouping sets present (maximum 4096)" |
| PostgreSQL 17.11 | 12 | elements of one `CUBE` | Error at parse analysis | No | "CUBE is limited to 12 elements" |
| PostgreSQL 17.11 | 31 | `GROUPING()` arguments | Error at parse analysis | No | "GROUPING must have fewer than 32 arguments" |
| DuckDB 1.5.5 | 65,535 | grouping sets (expanded; explicit lists included) | Parser error | No (`static constexpr`) | "Maximum grouping set count of 65535 exceeded" |
| DuckDB 1.5.5 | 63 | `GROUPING()` / `GROUPING_ID()` arguments | Binder error | No | "GROUPING statement cannot have more than 64 groups" |
| SQL Server | 4,096; 32; 10 | grouping sets; distinct `GROUP BY` expressions with `CUBE`/`ROLLUP`/`GROUPING SETS`; expressions in `WITH CUBE`/`WITH ROLLUP` | Statement fails ("The following examples fail because the `GROUP BY` clause has more than 4,096 groups") | No option documented | Message text not given on the pages read |
| BigQuery | 4096; 12; 50 | groupable items in `GROUPING SETS` (4095 in `ROLLUP`); items in `CUBE`; unique columns | Not stated on the page | No option documented | Not given on the page read |
| Trino | 2048 | grouping sets (cross product) | Semantic error `TOO_MANY_GROUPING_SETS` | Yes: config `analyzer.max-grouping-sets`, session property `max_grouping_sets` | "GROUP BY has %s grouping sets but can contain at most %s" |
| Snowflake | 128; 7 | grouping sets per query block; elements per `CUBE` | Not stated on the page | No option documented | Not given on the pages read |
| Redshift | 64 | grouping sets, counted as implied by `ROLLUP`/`CUBE` | Not stated on the page | No option documented | Not given on the page read |
| Spark SQL and `Dataset.cube()` (v4.3.0) | none on sets; 64 (32 with `integerGroupingIdEnabled`) | grouping expressions | `AnalysisException` `GROUPING_SIZE_LIMIT_EXCEEDED` | The width follows `SQLConf.get.integerGroupingIdEnabled`; no count setting | "Grouping sets size cannot be greater than <maxSize>." |
| polars SQL (main `7d4e42e9`) | 4096; 63 | grouping sets (expanded; product across `GROUP BY` items); `GROUPING()` arguments | `SQLInterface` error | No (`const`) | "CUBE with {} grouping expressions exceeds the {} grouping set limit"; "GROUP BY expands to more than {} grouping sets" |
| data.table 1.18.6 `cube()` / `rollup()` / `groupingsets()` | none | — | — | — | — |
| pandas (main `ca8d2daa`) | no grouping-set API found | — | — | — | — |
| SQLite 3.53.3 | no grouping sets; the nearest cap is 500 terms per compound SELECT | terms | Parse error | Lowerable only at run time; raisable only at compile time | "too many terms in compound SELECT" |

[polars-gs]: https://github.com/pola-rs/polars/blob/7d4e42e9b3bc6b6694ef23a4bcd04bb99781cd7d/crates/polars-sql/src/grouping_sets.rs
[dt-groupingsets]: https://github.com/Rdatatable/data.table/blob/c9d5dbecf9f858b84bf43da830ba83210578e30d/R/groupingsets.R

**polars.** [`crates/polars-sql/src/grouping_sets.rs`][polars-gs] opens with
"`GROUP BY GROUPING SETS / ROLLUP / CUBE` lowered to a `UNION ALL` of ordinary
aggregations over one shared input", and defines "Cap on the number of
grouping sets a single `GROUP BY` may expand to. `const MAX_GROUPING_SETS:
usize = 4096;`" and "Widest `GROUPING()` call whose bits fit an `Int64`.
`pub(crate) const MAX_GROUPING_ARGS: usize = 63;`". The file was added by
commit `ec561798` on 2026-09-14, "feat: Support GROUP BY GROUPING SETS,
ROLLUP, CUBE and GROUPING() (#29278)". A code search of `pola-rs/polars` for
`Rollup` in Rust returned only files under `crates/polars-sql/`, so the
grouping-set constructs were reachable through the SQL interface only.

**data.table.** [`R/groupingsets.R`][dt-groupingsets] at tag `1.18.6` (the
installed build was CRAN 1.18.6.1; the two `2L^n` lines of the installed
`cube.data.table` body match the tag). `cube.data.table()` materialises the
power set with `keepBool = sapply(2L^(seq_len(n)-1L), function(k)
rep(c(FALSE, TRUE), times=k, each=((2L^n)/(2L*k))))` and `sets =
lapply((2L^n):1L, function(jj) by[keepBool[jj, ]])`, then
`groupingsets.data.table()` runs `rbindlist(c(list(empty), lapply(sets,
aggregate.set)), use.names=TRUE, fill=TRUE)`: one grouped query per set,
executed eagerly. The file's 27 `stopf()` calls and 2 `warningf()` calls are
all input validation (types, duplicate names, `label`, `:=` in `j`); none
tests `length(by)` or `length(sets)` against a bound. The `id = TRUE` bit mask
is `strtoi(i_str, base=2L)`, an R integer.

**Spark.** `cubeExprs0()` in `grouping.scala` is `case x :: xs => val initial =
cubeExprs0(xs); initial.map(x ++ _) ++ initial`, with no count check, and the
64-expression check sits in `ResolveGroupingAnalytics.constructAggregate()` in
`Analyzer.scala`. `Cube`, `Rollup` and `GroupingSets` are the catalyst
expressions that rule resolves; that `Dataset.cube()` constructs the same
`Cube` expression was not read in source.

**Pattern across the capped engines.** Seven engines and one library cap the
number of grouping sets. Each states its main cap in grouping sets after
expansion (BigQuery's wording is "groupable items"), not in columns;
PostgreSQL, SQL Server, BigQuery and Snowflake state a separate element-count
rule for `CUBE` as well, and polars reports its one cap with a `CUBE`-specific
message. Where the outcome is
stated it is an error, never a warning. Trino is the only one read with a
documented override. 4096 (= 2^12) is the value in PostgreSQL, SQL Server,
BigQuery and polars.

### Searched for and not found

- **A SQLite limit on subquery nesting separate from the expression depth.**
  `src/sqliteLimit.h` and `src/sqlite.h.in` at `version-3.53.3` define none;
  the nesting check reuses `SQLITE_LIMIT_EXPR_DEPTH`.
- **A check of `SQLITE_LIMIT_COMPOUND_SELECT` after query flattening.**
  `grep aLimit src/select.c` at `version-3.53.3` returns two sites
  (`SQLITE_LIMIT_EXPR_DEPTH`, `SQLITE_LIMIT_COLUMN`); the compound limit is
  read only in `src/parse.y`.
- **An RSQLite interface to `sqlite3_limit()`.** `sqlite3_limit` and
  `SQLITE_LIMIT_` occur in none of the 120 non-vendored `src/*.{c,cpp,h}` and
  `R/*.R` files at RSQLite `v3.53.3`.
- **A `MAX_PARSER_DEPTH` entry in `PRAGMA compile_options`** on RSQLite
  3.53.3: absent from the output, although `SQLITE_MAX_PARSER_DEPTH` is
  defined in `sqliteLimit.h`.
- **PostgreSQL documentation of the 4096 / 12 / 31 caps.** Not in
  `queries.sgml`, `limits.sgml` or the `GROUPING` entry of `func.sgml` at
  `REL_17_11` (searched "4096", "12 elements", "limited to", "grouping set").
- **A PostgreSQL limit on the number of `UNION ALL` arms as a count.** None in
  `analyze.c`, `prepunion.c` or `limits.sgml`; the bound is `max_stack_depth`.
  The frame size per arm, and so the arm count at which 2MB is exhausted, was
  not derived from source.
- **A PostgreSQL setting that raises the grouping-set caps.** The three caps
  are literals at their use sites, and `guc_tables.c` at `REL_17_11` has two
  occurrences of "grouping", neither a grouping-set setting (a comment on
  `config_group`, and "Enables partitionwise aggregation and grouping.").
- **A DuckDB limit on the number of `UNION ALL` arms, or on statement
  length.** None in `transform_select_node.cpp`, `bind_setop_node.cpp`, or
  `docs/1.5/operations_manual/limits.md`; `docs/1.5/sql/query_syntax/setops.md`
  has no occurrence of "limit" or "maximum".
- **DuckDB documentation of the 65,535 and 63 caps.**
  `docs/1.5/sql/query_syntax/grouping_sets.md` has no occurrence of "65535",
  "limit" or "maximum".
- **A dbplyr cache of `sql_translation()` or `sqlite_version()`**, at `v2.6.0`
  or main `f478e207`, and **an issue or PR proposing one**: searches listed
  under the dbplyr subsection.
- **A dbplyr guard on the number of union arms.** `add_union()`,
  `lazy_union_query()` and `sql_query_union()` contain no length check; the
  only check in `add_union()` is `check_set_op_sqlite()`, "SQLite does not
  support set operations on LIMITs".
- **Statement-size limits for Snowflake, Spark SQL and Databricks**, and
  **UNION-term limits for every engine in the *Other SQL dialects* table**:
  not found on the pages cited in each cell. Snowflake's
  `/en/user-guide/query-size-limits` returned HTTP 504 and
  `/en/user-guide/querying-size-limits` HTTP 404 on 2026-11-21; no other
  Snowflake page on query text size was located.
- **Oracle's cap on grouping sets**: not on the 19c Logical Database Limits
  page or in the `SELECT` reference's GROUP BY restrictions. Oracle error
  messages were not searched.
- **Error message text for the SQL Server, BigQuery, Snowflake and Redshift
  grouping-set caps**: the documentation pages state the numbers and not the
  messages; no instance of any of the four was available to probe.
- **A grouping-set, rollup or cube API in pandas.** GitHub code search of
  `pandas-dev/pandas` on 2026-11-21: "grouping sets" 0 results; `rollup` under
  `pandas/core` 0 results; `cube` under `pandas/core/groupby` 0 results.
- **A polars DataFrame-API rollup or cube.** Code search for `Rollup` (Rust)
  and `GroupingSets` in `pola-rs/polars` returned only `crates/polars-sql/`
  files.
- **Trino, MariaDB, Oracle, MySQL probes.** None of these engines was run;
  their rows are documentation and source only.
