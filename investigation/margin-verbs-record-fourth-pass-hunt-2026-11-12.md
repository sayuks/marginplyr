# Fourth-pass hunt over the Margin verbs, shares and the Sent query record

Investigated: 2026-11-12
Revised: 2026-11-12 — the triage replay recorded under *Revisions (2026-11-12)* in this note
Repository snapshot: `03e3cef502f7332b69e50608740033043d27f2b4`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented.

Six fresh-context investigators worked read-only in the same checkout from
separate starting points and reported before any candidate was shared: A took
the Sent query record against statements traced below the DBI generics, B
`expand_with_margins()` on every backend, C the nesting verbs away from the
matrix of `nest-spec-hunt-2026-11-12.md`, D Total shares and Parent shares, E
what a lazy Margin summary does under the generics a caller applies to it
afterwards, and F Grouping specifications, Grouping helpers and the local
summary rewriting. The coordinator replayed every confirmed finding in a fresh
R process from the scripts kept beside this note, and read the code each
mechanism names.

Versions were R 4.6.1 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, duckdb 1.5.5, RSQLite 3.53.3, arrow 25.0.1, tidyr 1.3.2,
rlang 1.3.0 and vctrs 0.7.3. SQL work used process-owned in-memory SQLite and
DuckDB databases, or files under the session's temporary directory, holding
synthetic tables. No existing database or outside data was read.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. Summaries and shares were compared with ordinary dplyr
summaries at each explicit grain, divided by hand and bound in Grouping-plan
order. Expansions and nested cells were compared with a row-copy oracle built
on `vctrs::vec_group_loc()`. A traced `duckdb:::rapi_prepare` and
`RSQLite:::result_create`, below the DBI generics, and compared the statements
with `last_sent_queries(details = TRUE)` by text and order. Every difference on
a backend was followed by the native control on that backend before it was
counted.

Each candidate was checked against the issue tracker (open and closed, through
`gh issue list --search`), `investigation/`, `tests/testthat/`, `NEWS.md` and
the ADR governing it.

## Confirmed findings

Each script under `investigation/2026-11-12-margin-verbs-record-fourth-pass/`
is run from the repository root, prints the observed behaviour and exits zero.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| A wrapper forwarding its dots into an unnamed top-level `across()` that dplyr evaluates per group | A call native dplyr answers was refused: "Column `..1` doesn't exist". | `forwarded-across-selection.R` | [#1618](https://github.com/sayuks/marginplyr/issues/1618) |
| A share whose output name is an input column's, followed by an unnamed `across()` on a local input | The call was refused over a generated `dplyr::all_of()` the caller did not write, even for `any_of()`. | `across-after-share-named-like-input.R` | [#1619](https://github.com/sayuks/marginplyr/issues/1619) |
| `c_across()` given an argument it cannot match, on a local input | A bare `match.call()` error named the private placeholder `.marginplyr_arg_2`, and a call in a branch dplyr never evaluates was refused. | `c-across-unmatched-argument.R` | [#1620](https://github.com/sayuks/marginplyr/issues/1620) |
| A Grouping helper call used as the head of another call | rlang's "Can't create call to non-callable object" was raised from an internal call. | `grouping-helper-as-call-head.R` | [#1621](https://github.com/sayuks/marginplyr/issues/1621) |
| A SQL input whose ordering holds a raw `sql()` term or a constant | The Margin verb failed at construction with a dbplyr-internal purrr error whenever it reordered columns. | `sql-order-term-input.R` | [#1622](https://github.com/sayuks/marginplyr/issues/1622) |
| A frequency-limited message raised while SQL is translated, with `marginplyr.audit_sql = TRUE` | The message never reached the caller; with the audit off, and natively, it was delivered once. | `audit-frequency-limited-message.R` | [#1623](https://github.com/sayuks/marginplyr/issues/1623) |
| A data frame carrying a frame-level `tsp` attribute, in both nesting verbs | A base "invalid time series parameters" error where native nesting succeeds and carries the attribute. | `nest-frame-tsp-attribute.R` | [#1624](https://github.com/sayuks/marginplyr/issues/1624) |
| `compute()` of a sorted SQLite Margin summary | The returned object had no `remote_name()` and was refused by set operations and in-place row writes. | `sqlite-sorted-compute-not-a-table.R` | [#1625](https://github.com/sayuks/marginplyr/issues/1625) |

The `to-tickets` workflow published these as eight independent repair slices
with no prerequisite prefactoring and no blocking edge: each changes a
different path. Splitting was delegated by the maintainer without a
confirmation checkpoint. Seven carry `bug` and `ready-for-agent`. #1625 was
published with `bug` and `ready-for-human`, because whether a computed sorted
result owes the caller a table reference is a specification choice.

### A forwarded selection in an unnamed top-level `across()`

```r
native <- function(data, ...) summarise(data, across(...), .by = g)
margin <- function(data, ...) summarize_with_margins(data, across(...), .by = g)
native(d, where(is.numeric), range2, .unpack = TRUE)  # rows
margin(d, where(is.numeric), range2, .unpack = TRUE)  # Column `..1` doesn't exist
```

dplyr expands a plain `across()` once and evaluates the others per group:
`.unpack = TRUE` or a glue string, an extra such as `na.rm = TRUE`, or a
parenthesized `(across(...))`. On that per-group path native dplyr returned
rows for all eight forwarded selection forms tried in each of four shapes, and
marginplyr refused 28 of the 32, every form except a single literal string. On
dplyr's expansion path native dplyr itself fails with the same `..1` message
for a non-literal forwarded selection, so that half is not counted. Tibbles,
data frames and direct data.tables reproduced, under `.by` alone and under
`rollup()`, through both verb spellings. A named `z = across(...)` and a
wrapper that writes the selection and forwards the rest were correct.

*Relationship to dplyr summaries* says the `...` expressions "use
`dplyr::summarize()` data-masking semantics" and that an ordinary `across()`
with parentheses "retains per-group evaluation"; `NEWS.md` says unnamed
top-level `across()` summaries "preserve populated `...` forwarding" (#1151).
By reading, `rewrite_summary_selections()` expands the dots of an unnamed
top-level `across()` to `..1`, `..2` and quotes the selection as `..1` inside a
deferred `all_of()`, which tidyselect then reads as a column name. #1151,
#1170, #1211 and #1310 each concerned a neighbouring shape; the tests forward a
literal string on the expansion path, or write the selection when `.unpack` is
dynamic.

### An unnamed `across()` after a share named like an input column

```r
d <- data.frame(a = c("a1", "a1", "a2", "a2", "a3"), v = c(1L, 2L, 3L, 4L, 6L),
                w = c(10, 20, 30, 40, 50))
summarize_with_margins(d, s = sum(v), w = share_of_total(s),
                       across(everything(), max, .names = "mx_{.col}"),
                       .grouping = rollup(a))
```

The refusal read "In argument: `dplyr::all_of(c("v", "w", "s"))` ... Element
`w` doesn't exist". The same selection inside a named summary in the same call
saw `v, s`, `where(is.numeric)` in the unnamed position returned `mx_v, mx_s`,
and those are the columns an ordinary summary without the share and without the
hidden input returns at each grain. `everything()`, `w`, `all_of("w")`,
`any_of("w")`, `starts_with("w")`, `!s`, `-v`, `v:s` and position `2` failed;
`across(v)`, `c(v, s)`, `last_col()`, `where()`, every named form, an unnamed
`across()` written before the share, and a share name that is no input worked.
Both share helpers, and a share made by `across(s, share_of_total, .names =
"w")`, reproduced on tibbles, data frames and direct data.tables.

The Rd says a later ordinary selection uses "the columns and types then in the
local data mask"; `test-summarize-operation.R` pins that a later
`pick(everything())` leaves a share out silently. A bisection over first-parent
snapshots put the first refusing commit at `ff6fe1e5`, the merge of PR #1192;
the snapshot `b6f9faba` returned `mx_v, mx_s`. The tests cover the reverse
order and a share name that is no input.

On DuckDB and SQLite the lazy counterpart, `across(w, ~ max(.x))` after a share
named `w`, silently read the input column, as native dbplyr does for an
ordinary alias. It is listed under *Left for a decision*.

### `c_across()` with an argument it cannot match

`summarize_with_margins(d, z = sum(c_across(v, w)), .by = g)` raised a
`simpleError` whose call was `match.call(definition, call, expand.dots, envir)`
and whose message was "unused argument (.marginplyr_arg_2)". Native dplyr
reports "Caused by error in `c_across()`: unused argument (w)" in its usual
context. `z = if (FALSE) sum(c_across(v, w)) else 1` returned rows natively and
was refused the same way. A head the package does not recognise, `ca <-
dplyr::c_across`, kept dplyr's context, as did `bquote(v, zzz = 1)` and
`quote(v, w)`.

By reading, `parse_across_arguments()` matches placeholder arguments against
the helper's formals with an unguarded `match.call()`. `across()`, `if_any()`
and `if_all()` take `...`, so the unused-argument failure became reachable
when #1599 registered `c_across()`, whose one formal is `cols`. The
`across(.cols = v, .cols = w)` error from the same call is pinned by
`test-across-argument-matching.R` (#621) and names the caller's formal, so it
is not counted. Lazy backends failed as their native controls do.

### A Grouping helper call as a call head

`a = grouping_bit(g)(1)` and `a = grouping_id()(v)` raised `rlang_error`
"Can't create call to non-callable object" from `rlang::call2(head, !!!args)`.
The control `a = sum(v)(1)` and native `a = (!!0L)(1)` gave "attempt to apply
non-function" in dplyr's context. Tibble, dtplyr and DuckDB reproduced. Heads
that compute a function from a helper, such as `(if (grouping_bit(g) == 1L)
sum else mean)(v)`, worked. The call is meaningless; only its condition
differs.

### A SQL input ordered by a raw `sql()` term

```r
top <- tbl(con, "t") |> arrange(dbplyr::sql("v DESC NULLS LAST")) |> head(2)
expand_with_margins(top, .grouping = rollup(g), .id = "id")
```

The failure was a `purrr_error_indexed`, "Result must be length 1, not 0",
from dbplyr's `rename_order()` under marginplyr's own `select()`. `rollup(g)`
alone succeeded because `g` was already first; `.id`, `.sort`, `rollup(h)`,
`.by = h` and `summarize_with_margins()` failed on SQLite, and `.id`, `.sort`
and the summary on DuckDB. `arrange(sql("random()"))`, `arrange(1L)` and
unlimited orderings reproduced; `arrange(desc(v)) |> head(2)` did not. Native
`collect()`, `mutate()` and `summarise()` of the same input succeeded.

Native `select()` of such an input fails identically, so the defect in
`rename_order()` is upstream, and `design/specs/sqlite-b-direct.md` put
"Repairing all dbplyr raw-SQL ordering" out of that change's scope. The finding
is counted because the caller wrote no `select()`: by reading,
`restore_input_window_order()` keeps every term for which
`all(all.vars(term) %in% result_names)` holds, which is vacuously true of a
`sql()` string or a constant, and so re-attaches the term to a relation the
verb has already sealed in a subquery. Its header says "terms the result cannot
name ... vanish".

### A frequency-limited message under the SQL audit

A translation that calls `rlang::inform(.frequency = "once")` delivered its
message once through native dbplyr and through `summarize_with_margins()` with
the audit off, and never with `marginplyr.audit_sql = TRUE`. `.frequency =
"regularly"` and a message owned by the label-collision query behaved the same.
The warning counterpart was delivered once in all three modes. By reading,
`render_sent_query_sql()` quiets `rlib_warning_verbosity` and
`lifecycle_verbosity` for the diagnostic render, which keeps their frequency
state untouched, and has no counterpart for messages, so the diagnostic render
consumes the once-state. #858 required that auditing on and off "deliver the
same original invocation message condition, the same number of times". No
stock dbplyr, duckdb or RSQLite translation raises such a message; the replay
registers a dialect extension. Run on SQLite only.

### A frame-level `tsp` attribute in the nesting verbs

With `attr(d, "tsp") <- c(1, 3, 1)` on a three-column data frame,
`tidyr::nest(d, .by = a)`, `dplyr::nest_by(d)` and dtplyr's `nest()` returned
cells carrying `tsp`. `nest_with_margins()` on a data frame, a tibble and a
dtplyr step, and `nest_by_with_margins()` with a keyless set, raised a
`simpleError` from `attr(data, name) <- metadata[[name]]`: base validates
`tsp` against the column count of the cell it is assigned to. `.keep = TRUE`
succeeded because its cells kept three columns. *Variable labels and dataset
attributes* names native `tidyr::nest()` and `dplyr::nest_by()` as the
references; ADR 0042 also says a structural field "must not be reattached
blindly to a differently shaped result". `summarize_with_margins()` failed
too, as native `summarise()` does.

### `compute()` of a sorted SQLite Margin summary

The computed object of a `.sort = "last"` or `"first"` SQLite summary had
`remote_name()` `NULL`; `union_all()`, `union()`, `rows_append()` and
`pivot_longer()` failed with "ORDER BY clause should come after UNION ALL",
and `rows_append(in_place = TRUE)` with "Can't determine name for target
table". Native `arrange() |> compute()` and the unsorted Margin `compute()`
returned a named table on which all of these worked, as did DuckDB's sorted
compute. By reading, `compute.marginplyr_sqlite_typed_result()` returns an
arranged query over the destination's rowid. The Rd says a sorted result "is
read back in Margin order using an available implicit rowid alias" and that
later verbs "use ordinary dbplyr behavior"; `design/specs/sqlite-b-direct.md`
says "Return a normal usable lazy table", and #663's criteria named `select`,
`rename`, `filter`, `mutate` and `arrange` only. Whether a computed result owes
the caller a table reference is a specification choice.

## Revisions (2026-11-12)

A triage pass the same day replayed three of the findings against candidate
answers, without editing the checkout, and changed how two of them stand.

- `compute()` of a sorted SQLite Margin summary is not a defect. #663 and
  `design/specs/sqlite-b-direct.md` chose the returned object: a lazy table
  "with explicit ORDER BY on an available implicit rowid alias". It rendered
  as `SELECT * FROM <destination> ORDER BY rowid`, and a native
  `arrange(tbl, g)` had the same `NULL` `remote_name()` and the same
  `union_all()` refusal, so the comparison above with native
  `arrange() |> compute()`, which returns an unordered table and promises no
  order, was the wrong control. `tbl(con, <name>)` gives the table reference.
  #1625 was closed as `wontfix`.
- The Grouping helper used as a call head stands as observed, and #1621 was
  closed as `wontfix`: the call has no meaning and fails natively too.
- For the frequency-limited message, adding `rlib_message_verbosity =
  "quiet"` beside the two options `render_sent_query_sql()` sets delivered
  the message once with the audit on, and left the warning count at one.
- For the `tsp` attribute, naming it among the structural fields
  `margin_frame_metadata()` excludes made every failing call in the script
  return, with cells that carry no `tsp`. Base refuses `tsp` on a data frame
  whose column count differs from it, and `as.data.frame()` of a `ts` matrix
  carried none.

## Left for a decision

- On DuckDB and SQLite, `across(w, ~ max(.x))` after a share named `w` read
  the input column `w` without a diagnostic, where `mx = max(w)` is refused
  with "Ordinary summaries cannot use an earlier Total share". Native dbplyr
  reads the input for an ordinary alias, and the Rd says both that a lazy
  input "retains the alias limitations of its backend" and that "A later
  summary cannot use a share in the same call".
- On DuckDB, `select()` dropping a column, or `pull()`, after a sorted result
  lost the Margin order with dbplyr's subquery warning, where SQLite and
  native `arrange() |> pull()` kept it. The Rd disclaims order after later
  verbs; ADR 0018's measurement sentence lists `select()` among the verbs that
  keep it.
- A DuckDB expansion with a `.data` column, or `.id = ".data"`, could not be
  `compute()`d, where `.env` could. Native `compute()` of such a table fails
  too, and the 2026-10-26 expansion note recorded that #863 did not extend to
  DuckDB's materializer.
- A helper spelling inside a `~` formula used as data, `deparse(v ~ pick(w))`,
  was rewritten and failed where native returned rows. The Rd's list of
  language data does not name `~`, which is also the lambda form.
- One refusal about a share source redefined by an `across()` selection was
  worded "Contextual share requires source summary" for `where()` and "Total
  share ... requires source summary" for `everything()`.
- `.keep = c(k = TRUE)` and a classed `TRUE` were accepted while `1L` was
  refused; a `.key` with "bytes" encoding raised a base error, not replayed.
- A severity-free condition signalled during the diagnostic render leaves the
  record's `sql` missing with no condition summary. Read from the code, not
  run.
- dtplyr refused a Margin summary over a Margin result that held shares, with
  the column-metadata Package condition.

## Rejected candidates

- Set operations, aggregates and joins after a sorted lazy result; SQLite
  columns collecting as logical after a later verb; the DuckDB binder error
  for `count()` after a sorted result: rejected in the 2026-11-11 notes, and
  each native control agreed.
- `tbl(con, sql(...))`'s field query absent from the record: it belongs to the
  caller's `.data` evaluation, before recording.
- A simulated connection recording an errored row with nothing sent: the
  record is of attempts.
- Duplicated, `NA` or empty input column names, and a payload column named
  `..1`: native `tidyr::nest()` and `dplyr::nest_by()` refuse them too.
- dtplyr and direct data.table cells losing element names: `as.data.table()`
  drops them first, natively too.
- `grouping_id(y = h)` accepted with the name ignored, and
  `across(.cols = v, .cols = w)`'s `match.call()` error: pinned by tests
  (#1610, #621).
- `sum(.data[[.env$v2]])` failing, and `rollup(NA)` blaming a tidyselect
  call: native dplyr and tidyr do the same.
- A dimension read on a margin row seeing the whole per-group vector:
  documented.
- SQLite merging `NaN` into `NULL`, DuckDB's `sum()` of an all-`NULL` group,
  and a binder error for a character share source: the databases' own.
- `alias <- share_of_total` refused; a share named `S` beside `s` refused on
  SQL only: ADR 0019 and ADR 0032.
- `collect(n = 2.7)` returning two rows from a sorted SQLite result: #663.
- A unique-index violation in SQLite `compute()` surfacing as a bare error
  after rollback: not pursued.
- A DuckDB `STRUCT` dimension refused under a text label; dynamic DuckDB
  sources and derived dtplyr steps refused: documented Package conditions.
- One Arrow `union_all()` over an expansion hung once and completed in 23
  later runs and 800 in-process iterations; not attributed.

## Covered without a finding

Counts are the investigators' reports.

- Sent query record: about 75 calls on DuckDB and SQLite over all five verbs
  and the options that add a query, each traced statement matching a recorded
  row; the result row against `sql_render()`, `remote_query()` and the
  statement `collect()` sent for 16 results; about 364 later operations
  leaving the record unchanged; encodings, a 2 MB statement, and driver
  warnings at `warn = 0` and `2`.
- Expansions: about 300 comparisons of later verbs on SQLite and DuckDB and 35
  on Arrow inputs; 1,056 arranged or limited inputs on six backends; 133
  DuckDB `sql()` sources; about 600 DuckDB and 10,000 SQLite type cases;
  2,988 factor and label cases; 512-set cubes on four backends; and 72
  constructions traced for ADR 0020, each sending only zero-row or catalog
  statements.
- Nesting: about 7,000 calls. 100 special names in four roles on local and
  dtplyr; variable labels over eight column types; seven dataset attribute
  kinds against native cells; exotic key and payload classes; transported
  objects identical to the source; row-wise result semantics against
  `dplyr::nest_by()`; about 300 option and blamed-call probes.
- Shares: Parent shares over 15 rollups on five backends against a
  hand-computed parent; about 2,470 generated local calls mixing ordinary
  summaries with both shares; about 90 comparisons of later verbs and of
  inputs carrying grouping, ordering or window state; 14 DuckDB numeric types
  at their limits; 28 share spellings; private-prefixed and case-equivalent
  names.
- Lazy summaries afterwards: 39 verbs over 24 result configurations on SQLite,
  DuckDB and dtplyr and 12 on Arrow; about 45 object-level generics; four
  `sql_options()` settings over 144 configurations; a second Margin verb over
  a computed or lazy Margin result; DuckDB compute order over 1.33 million
  rows; cubes of 256, 512 and 1,024 sets on SQLite and DuckDB; `NaN`, infinite
  and negative-zero dimensions and nested types on DuckDB; `COLLATE NOCASE`
  under `.sort`; twelve SQLite relation kinds.
- Local summaries and specifications: about 1,100 comparisons. About 230
  selection expressions on three inputs against native grouped dplyr; 60 value
  types; evaluation counts and seeded values; 75 Grouping helper placements
  and 35 invalid helper calls; specification objects printed, combined,
  subset and serialized; 25 constructor refusals.

## Not covered

No remote dialect was executed. The forwarded `across()` selection was not
tried beside a share or in a top-level `if_any()`; the share-named-input
refusal was not tried on a grouped tibble; the `sql()` ordering term was not
tried in `inspect_grouping()`; the sorted SQLite `compute()` was run for
`summarize_with_margins()` only, though `expand_with_margins()` shares the
method; the audit finding was not run on DuckDB. Interrupts, PostgreSQL
savepoint rows, real `sf` or `tsibble` inputs, DuckDB file sources, DuckDB
`ARRAY`, `UNION` and `BIT` columns, Arrow extension types, `inspect_grouping()`
on lazy inputs, plans above 31 dimensions, and the neighbourhoods of #1602,
#1609 and #1611 were not examined.
