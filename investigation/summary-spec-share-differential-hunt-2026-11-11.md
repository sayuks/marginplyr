# Differential hunt over Margin summaries, Grouping specifications and shares

Investigated: 2026-11-11
Repository snapshot: `fbf39cd2780365af59f4b25c869e4332a57f19ff`

The investigation ran against freshly fetched main in one worktree, and a
re-fetch before the note was written resolved to the same SHA. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented.

Four fresh-context investigators worked in the same read-only checkout from
separate starting points and reported before any candidate was shared: A from
eager summaries, B from Grouping specifications and identifiers, C from Parent
and Total shares, D from lazy backends without shares. The coordinator probed
condition contexts, variable labels, element names and classed columns, then
replayed every reported finding in a fresh R process from its own script.

Versions were R 4.6.1 on arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, RSQLite 3.53.3, arrow 25.0.1,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3 and tibble 3.3.1. SQL work used
process-owned in-memory SQLite and DuckDB databases holding synthetic temporary
tables; DuckDB used `shared_home = FALSE`. No existing database or outside data
was read.

## Method and independent expectations

Earlier hunts in this area concentrated on closure and dependency-proof
transfers, tidyselect capture forms, DuckDB replacement scans and registered
converter overrides. This one was pointed away from those, at realistic calls
checked by differential and metamorphic comparison.

Expected values never came from marginplyr internals or from agreement between
two marginplyr helpers. Summaries were compared with ordinary dplyr summaries
at each explicit grain, bound in Grouping-plan order. B expanded generated
set, union, rollup, cube and product trees with its own expander. C computed
denominators with a separate summary at the Grand total grain inside each
fixed partition. SQL comparisons kept each engine's NULL-aggregate semantics.

Each candidate was checked against the issue tracker, `investigation/`,
`tests/testthat/`, `NEWS.md` and the ADR governing it before it was reported.
The tracker was read through the public REST and search API, because `gh`
could not verify TLS certificates inside the session's sandbox.

## Confirmed findings

Each script under `investigation/2026-11-11-summary-spec-share/` prints the
observed behaviour and exits zero; with `--assert-contract` it asserts the
documented behaviour and exited one at the investigated SHA.

| Boundary | Established effect | Script |
| --- | --- | --- |
| dtplyr fixed key named `T` or `F` under a share | The key became `TRUE` or `FALSE`, the fixed partitions merged, and shares were wrong with no diagnostic. | `dtplyr-true-false-fixed-key.R` |
| Multi-column `grouping_id()` in arithmetic on native grouping sets | The mask was spliced in unparenthesized, so the surrounding operator bound to one term and values were wrong with no diagnostic. | `native-grouping-id-arithmetic.R` |
| Arrow Margin order | Two different Grouping-bit sort terms received one truncated field name, and collecting failed. | `arrow-sort-label-collision.R` |
| Local Margin order over a double fixed key | `NaN` and `NA` partitions interleaved, separating margin rows from the rows they summarize. | `local-nan-na-margin-order.R` |
| Two or more unnamed local summaries | The Condition context quoted an internal wrapper, and warnings from different arguments collapsed into one Repeated condition. | `unnamed-summary-condition-context.R` |
| Failed column selection | The blamed call was an internal function, and a summary selection also quoted an argument the caller did not write. | `selection-condition-context.R` |

The `to-tickets` workflow split these into six independent repair slices with
no blocking edges and no prerequisite prefactoring; splitting had been
delegated without a confirmation checkpoint. On 2026-11-11 they were written
as drafts under `investigation/2026-11-11-summary-spec-share/ticket-drafts/`
and were not published, because `gh` could not reach the tracker from the
session's sandbox. The tracker is authoritative for whether they have been
published since.

### dtplyr fixed key named `T` or `F` under a share

```r
d <- data.frame(T = c("a", "a", "b", "b"), g = c("x", "y", "x", "y"),
                v = c(1, 2, 3, 4))
collect(summarize_with_margins(
  dtplyr::lazy_dt(d), zz = sum(v), sh = share_of_total(zz),
  .by = T, .grouping = rollup(g)
))
```

Ordinary summaries at `(T, g)` divided by those at `(T)` give shares
`1/3, 2/3` for `a` and `3/7, 4/7` for `b`, with `1` on both Grand total rows;
the local data frame agreed. The dtplyr result held `T = TRUE` on every row,
and the `b` detail rows carried `1.0` and `1.333`: both partitions had been
divided by the `a` total. `share_of_parent()`, a key named `F`, the `across()`
form, two fixed keys and a two-level rollup reproduced.

The same call without a share was correct on dtplyr, as were the local data
frame, a direct data.table, SQLite and DuckDB. `T` or `F` as a variable
dimension, a source name, a share output name or `.id` was correct. The
caller's table was unchanged. The rendered query contained
``[, `:=`(T = TRUE)]`` before `by = .(T)`: the share's window stage grouped by
bare key symbols, which dtplyr reads as the logical constants.
`margin_safe_column_reads()` aliased data.table's special symbols and dot-dot
names, and neither `T` nor `F`.

Closed #603 covered the Margin-label collision check for these names, closed
#952 the special-symbol list for share fixed keys, and
`test-dtplyr-special-nesting.R` the nesting verbs. None reached a share over a
fixed key named `T` or `F`.

### Multi-column `grouping_id()` in arithmetic on native grouping sets

```r
summarize_with_margins(
  duckdb_table, level = grouping_id(), weight = grouping_id() * 10L,
  depth = 3L - grouping_id(), .grouping = rollup(a, b), .id = "id"
)
```

`rollup(a, b)` has masks `0, 1, 3` in plan order, so `weight` is `0, 10, 30`
and `depth` is `3, 2, 0`; the local data frame, dtplyr, SQLite and Arrow
returned those. DuckDB returned `weight` `0, 10, 12` and `depth` `3, 4, 2`,
while the bare `level` stayed correct. The rendered SQL was
`GROUPING(a) * 2.0 + GROUPING(b) * 10` and `3 - GROUPING(a) * 2.0 + GROUPING(b)`.

Multiplication and division on either side, a left-hand subtraction, unary
minus, and a caller's own parentheses around the helper all reproduced.
`+`, a right-hand constant subtraction, `%/%`, `%%`, `^`, comparisons, a
function call around the helper, `grouping_bit()` and a one-column
`grouping_id()` were correct. `.duplicates = "keep"` on DuckDB took the
portable adapter and was correct, which separated the native rendering from
the plan. PostgreSQL was examined only as rendered text through
`dbplyr::simulate_postgres()`, which carried the same unparenthesized sum.

`grouping_id_sql_expr()` returned the reduced sum as bare SQL and
`rewrite_grouping_expr()` substituted it into the caller's expression. The one
existing test of arithmetic around the helper used `+` on SQLite, which takes
the literal path.

### Arrow Margin order

A report of one-way totals over five dimensions,
`grouping_sets(grouping_set(region), ..., grouping_set(tier))` with
`.margin_label = NULL` and `.sort = "last"`, constructed on an Arrow table and
failed at `collect()` with Arrow's
`Invalid sort key column: Multiple matches for FieldRef.Name(...)`. The local
data frame, DuckDB, SQLite and dtplyr returned the ten expected rows, and
`.sort = "none"` on Arrow did too.

Each dimension contributed a term
`dplyr::if_else(.data[[sort_id]] %in% <ids>, 1L, 0L)`. Arrow named an arranged
expression by the first deparsed line followed by `...`, so two different
identifier vectors that wrapped after the same prefix shared one field name.
A longer identifier column left less room on that line: with
`.id = "margin_grouping_set_identifier"` a plain `rollup(region, channel)`
failed, where `.id = "gid"` returned seven rows. Tables, record batches and
in-memory datasets reproduced, under both Margin orders, and D's randomized
comparison met it on 5 of 200 generated specifications.

Closed #541 deduplicated identical Grouping-bit terms, and its note scoped
itself to plans "whose varying bit expressions are unique". Here the
expressions were unique and only their truncated names collided.

### Local Margin order over a double fixed key

```r
d <- data.frame(x = c(NaN, NA, NaN, NA, 1, 1),
                g = c("a", "a", "b", "b", "a", "b"),
                v = c(1L, 2L, 4L, 8L, 16L, 32L))
summarize_with_margins(d, s = sum(v), .by = x, .grouping = rollup(g),
                       .sort = "last")
```

`dplyr::summarise(d, s = sum(v), .by = x)` returned three groups, so `NaN` and
`NA` were separate fixed partitions. The local result ordered them
`NaN a, NA a, NaN b, NA b, NaN Total, NA Total`. dtplyr, DuckDB and Arrow
returned `NaN a, NaN b, NaN Total, NA a, NA b, NA Total` for the same call.
`.sort = "first"` and a leading typed-missing dimension reproduced. A and D
reached this independently.

The documented key, `is.na(by1), by1, ...`, yields exactly this order under
`dplyr::arrange()`, because `is.na()` is true for both and `arrange()` ties
them. What the result contradicted was the consequence the same section draws
from the key — "each partition is one contiguous block" and "a margin row
therefore sits with the rows it summarizes" — and agreement with the lazy
backends. `bug-hunt-2026-09-26.md` set aside NA/NaN sorting differences as a
type-matrix observation and made no claim about contiguity, and
`test-margin-order.R` and ADR 0018 held no `NaN` case.

### Two or more unnamed local summaries

```r
d <- data.frame(g = c("a", "a", "b"), v = 1:3, w = c("1", "x", "4"))
summarize_with_margins(d, sum(v), sum(as.numeric(w)), .grouping = rollup(g))
```

`dplyr::summarise()` attributed the coercion warning to
`` `sum(as.numeric(w))` ``. The Margin verb reported
`` In argument: `((function (x, ns, pos = -1, envir = as.environment(pos)) ...`. ``
An error from `max(w + 1)` beside another unnamed summary carried the same
line. With one unnamed summary, or one named and one unnamed, the caller's
spelling was restored.

Two unnamed summaries that warned in different grouping sets were reported as
one warning with "1 further grouping set raised this warning"; the same pair
written with names produced two warnings, one per argument. Data frames,
tibbles, grouped tibbles, direct data.tables and `.grouping = NULL`
reproduced. dtplyr and DuckDB did not pass through dplyr's summary context and
were unaffected.

`wrap_assigned_local_summaries()` wrapped each unnamed local summary in a call
whose head was an inlined function object. dplyr's label for every such
argument truncated to one string, so the restatement ADR 0022 describes found
several candidates with differing caller labels and left the quotation, and
the deduplication key collided as well. Closed #430 concerned unnamed
summaries named from the rewritten expression, a different collision.

### Failed column selection

A misspelled column in `.by` or `.grouping` raised tidyselect's own class and
message, displayed as ``Error in `resolve_column_selection()` ``; native
`summarise(.by = zz)` displays ``Error in `summarise()` ``. The four Margin
verbs and `inspect_grouping()` reproduced on every backend. In a summary, a
first unnamed `across(zz, sum)` on a local input, and `across()` or `pick()`
at any position on a lazy input, displayed
``Error in `resolve_summary_selection()` ``. A later local `across()`,
`pick()` or `if_any()` kept the Margin verb and the caller's argument on top
and added two layers native dplyr does not have:

```
ℹ In argument: `across(zz, sum)`.
Caused by error in `across()`:
ℹ In argument: `(function (x) ...`.
Caused by error in `resolve_summary_selection()`:
! Can't select columns that don't exist.
```

`resolve_column_selection()` and `resolve_summary_selection()` called
`tidyselect::eval_select()` without an error call, and
`with_margin_error_call()` reassigned the call only for a Package condition.
The share `across()` path passed the caller's own call for the same purpose.
ADRs 0008 and 0026 describe the displaced condition as carrying "tidyselect's
blamed call"; the call observed was marginplyr's. The existing tests compared
the root class and message only. The condition's class, message and cause were
intact, so this is a Condition context defect and no value was affected.

## Rejected candidates

- A lubridate `Period` dimension collapsed to one group: native
  `dplyr::group_by()` did the same.
- A complex dimension under a dtplyr rollup raised data.table's
  `unsupported column type`: recorded as a native limitation in
  `nest-spec-systematic-validation-2026-11-07.md`.
- A warning named `dplyr::summarize()` as the call it arose in: ADR 0021
  leaves that clause as dplyr rendered it.
- dtplyr attached a source variable label to a calculated summary where the
  native summary at the same grain did not: data.table's grouped optimization
  decides it, per `upstream-summary-labels-2026-11-10.md`, and the label
  combination rule keeps an agreeing label.
- A failed SQLite collection blamed `dplyr::collect()` with internal
  arguments: the displayed header matched native dbplyr's.
- A sorted lazy result followed by `summarise()`, `count()`, a join or
  `union_all()` failed or lost its order on DuckDB and Arrow: native
  `arrange()` over an expression behaved the same, and the Margin order
  section disclaims later verbs.
- A string Margin label ordered a numeric dimension as text, and rendered
  booleans and doubles in the backend's own spelling: consistent across
  backends and a property of the displayed column.
- `rollup()` and `cube()` failed after `library(data.table)`: data.table
  masks both and constructors follow ordinary lookup.
- A specification stored in a loop resolved its selection late, and an outer
  `!!` of a specification was refused: quosure semantics and ADR 0007's
  2026-11-09 amendment.
- A specification written in `...` reached the backend as an ordinary
  summary: `...` is dplyr data-masking.
- Duplicate-set diagnostics named family-relative positions and listed some
  duplicate groups: every listed group was a real duplicate, per #1464.
- A measure column named `T` summed as `TRUE` on dtplyr: identical without a
  share and without marginplyr.

## Covered without a finding

- Eager summaries: about 4,300 generated calls over data frames, tibbles,
  grouped tibbles, data.tables and dtplyr steps of 0 to 40 rows, with
  character, factor, ordered, integer, double, logical, Date, POSIXct,
  difftime and integer64 dimensions. Values, names, column order, classes,
  levels, Margin order, warning roots and input immutability matched in 3,202
  comparisons, and 369 duplicate refusals were correct.
- Grouping plans: about 4,700 generated specifications to depth four, on the
  name-only and typed paths and across five backends, covering set count and
  order, `.duplicates`, `.id` renumbering, `grouping_id()` and
  `grouping_bit()`; plans to 30,000 sets; 30, 31, 32 and 60 dimensions.
- Shares: 1,080 generated comparisons over rollups of one to four levels,
  cubes, unions with an empty set and products, with zero to two fixed keys;
  the value rules; DuckDB numeric types; key types including factor NA levels
  and integer64 beyond 2^53; about 60 grammar probes; 99 awkward names in
  seven roles.
- Lazy inputs: query-shaped inputs, schemas and attached catalogs, 19 name
  families, DuckDB and Arrow physical types, the Margin order key on collected
  and computed results, 200 generated specifications across five backends,
  and prepared-statement tracing that found no data-reading query other than
  a requested `.check_margin_label = TRUE`.
- Variable labels, element names and classed columns (integer64, hms, units,
  haven labelled, lubridate durations, difftime, complex) as dimensions,
  fixed keys and summaries on local and dtplyr inputs.

## Not covered

PostgreSQL and other remote dialects were not executed. Arrow record-batch
readers and file-backed datasets, non-default collations, SQLite columns of
mixed storage class, packed, matrix and list keys, transactions and
interruption around `compute()`, and `T` or `F` names in the expansion verb
were not examined. Whether other spliced SQL fragments share the
unparenthesized shape of the `grouping_id()` finding was not audited.
