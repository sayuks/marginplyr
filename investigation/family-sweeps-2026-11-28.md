# Sweeps for three families of the 2026-11-27 hunt: names DuckDB binds with no call head, data-mask verbs, and reads of an earlier share by a name not written

Investigated: 2026-11-28
Repository snapshot: `1680498dbfc37296d0d20ea4352606c48441a16e`

`third-eight-lane-workflow-hunt-2026-11-27.md` filed 35 issues, 22 of them in
three families that each compared the package with a vocabulary a dependency
owns: twelve in the share dependency walk, seven in the DuckDB preflight and
three in the data-mask registry. Each hunt before it had found more of the
same. This note records what happened when the dependency was asked for its
vocabulary directly, with a script and no investigator.

Scripts are under `2026-11-28-family-sweeps/`, and each prints what it
observed when run from the repository root. The DuckDB script restores the
preflight's two constants to their state at the snapshot before it measures.

## What was established

| Family | Asked of | Result at the snapshot | Outcome |
| --- | --- | --- | --- |
| Names DuckDB binds with no call head | DuckDB 2.0.1 and 1.5.6, every system function name shadowed by a caller macro | With a catalog snapshot in hand, 44 of 403 confirmed reaches admitted on 2.0.1 and 49 of 407 on 1.5.6: 25 names | [#1843](https://github.com/sayuks/marginplyr/issues/1843), repaired with the sweep kept as a test; its review added `DESC`, DuckDB's own views and one more name |
| dplyr verbs that own a data mask | dplyr 1.2.1's help pages | 39 exports documented with a `<data-masking>` or `<tidy-select>` argument: 29 registered, 10 helpers | No gap; kept as a test |
| tidyr verbs | tidyr 1.3.2's help pages | 46 exports so documented, `uncount()` alone registered; seven of nine verbs tried were refused as a share-source dependency | [#1844](https://github.com/sayuks/marginplyr/issues/1844), closed as not planned |
| Reads of an earlier share by a name not written | Every exported function of 17 packages, 21 argument shapes | 466 calls read the name; 28 refused, 354 ended in an error, 13 returned a value that followed the input column | [#1845](https://github.com/sayuks/marginplyr/issues/1845) |

## Names DuckDB binds with no call head

The sweep created a caller macro for each function name of the system catalog
(689 scalar, aggregate, macro and window names and 98 table function names on
2.0.1), each over a table dropped at once. DuckDB checks a macro's body when
the macro is created and again when a statement binds it, so binding a
statement then failed naming the first macro it reached, and dropping that
macro and binding again named the next. An error raised inside the macro
(`error()`) was tried first and set aside: it reports execution, and a
zero-row statement executes nothing.

439 syntax forms were bound that way, 423 of which both versions accepted,
then every function name and keyword as a bare word, and each as a call head
with zero to two arguments, `(*)`, and the number of arguments each system
macro declares. Each reach was confirmed with that macro as the only caller
macro, and `validate_duckdb_csv_metadata()` was asked for its verdict with a
catalog function that returns nothing until asked to read, as an operation
holds it.

The bare-word and call-head sweeps found no admitted reach. The syntax forms
found these:

| Syntax | Name reached | Version |
| --- | --- | --- |
| `SUMMARIZE` | `min`, `max`, `avg`, `count`, `stddev`, `approx_count_distinct`, `approx_quantile` | both |
| `SHOW TABLES`, `SHOW ALL TABLES`, `SHOW DATABASES`, `SHOW SCHEMAS`, `SHOW VARIABLES`, `SHOW ALL`, `DESCRIBE`, `DESCRIBE DATABASES`, `SHOW TABLES FROM` | table functions `duckdb_tables`, `duckdb_views`, `duckdb_columns`, `duckdb_databases`, `duckdb_schemas`, `duckdb_variables`; `lower` | both |
| `SHOW TABLES`, `SHOW SCHEMAS` | `in_search_path`, `current_database`, `current_schema` | 1.5.6 |
| `SHOW t` | table function `pragma_show` | 2.0.1 |
| `UNPIVOT` | `unpivot_list` | both |
| `FROM '<file>.parquet'` | table function `parquet_scan` | both |
| `ARRAY(SELECT ...)` | `array_agg` | 2.0.1 |
| `[e FOR e IN l IF e > 1]` | `list_filter` | both |
| `NOT LIKE ... ESCAPE`, `NOT ILIKE ... ESCAPE` | `not_like_escape`, `not_ilike_escape` | 1.5.6 |
| postfix `!` | `!__postfix` | 1.5.6 |

`duckdb-names-reached-with-no-call-head.R` replays six of them through
`inspect_grouping()`: with the macro's file removed, each ended in DuckDB's
`IO Error: No files found` naming the file, and the Parquet relation's plan
was `(v)` while the file held a number and `()` while it held text.

Listing all 25 names was tried first. Two tests of the snapshot failed:
`chained and parenthesis-less macro calls are checked (#1647)` and
`reader-like relations follow search paths and temporary catalogs` both held
that a caller macro named `current_database` leaves an unrelated statement its
typed metadata. The repair therefore lists seven names and refuses `SHOW`,
`DESCRIBE` and `SUMMARIZE` by keyword beside any caller macro. With it, every
reach for which the catalog was read was refused: 398 of 398 on 2.0.1 and 404
of 404 on 1.5.6 directly, and 261 of 261 and 258 of 258 through a stored view
of the form.

Reaches the repair leaves, as ADR 0020 admitted before it: a statement naming
no relation and no call head reads no catalog. The sweep met five
such reaches on 2.0.1 and three on 1.5.6: `WITH RECURSIVE c(n) AS (SELECT 1
UNION ALL SELECT n + 1 FROM c WHERE n < 3) SELECT * FROM c` beside a macro
named `+`, and `SELECT distinct(1, 2)` and `SELECT not(1, 2)` beside one named
`row`.

### What the repair's review added

The review of the repair reproduced three things the sweep had not asked.
`DESC` is DuckDB's spelling of `DESCRIBE`, and `DESC TABLES` reached a table
macro `duckdb_tables` unrefused. The keyword rule matched the word anywhere,
so `SELECT v AS show FROM t` was refused beside an unrelated macro. DuckDB 1.5
and 2.0 accepted `SHOW`, `DESCRIBE`, `DESC` and `SUMMARIZE` as the first word
of a statement or after an opening parenthesis, a `WITH` before one being a
parser error on both, and 2.0.1 also after a set operator
(`SELECT 'x' AS name UNION ALL SHOW TABLES`), which a second review round
found. And `SELECT table_name FROM information_schema.tables`
reached the same macro: a two-part name whose schema no search path held
matched no catalog row, so the view was read as an unknown relation.

The sweep was widened to bind each of DuckDB's 47 own views with and without
its schema, and its verdict to ask the preflight of the authored text and of
the rendered statement, as an operation does. Fifteen reaches through
`information_schema` views were admitted on both versions, and the view
`duckdb_logs` reached a table macro named `duckdb_log_contexts`, which the
`duckdb_logs()` function binds by name.

The second review round showed the sweep stopping early: with every name
shadowed by a macro whose body was a scalar subquery, DuckDB 2.0.1 ended 84 of
434 probe statements in an error naming no macro (`Window function macro
bodies must contain exactly one aggregate function`), which hid `row_number`
behind `duckdb_logs`. The macro body became a call to a dropped macro, bound a
second time as one aggregate over that call, and a macro DuckDB names and
declines to expand (`is not a scalar function`, `is a Macro Function`, `Macro
f() does not support the supplied arguments`) steps aside so the binding goes
on. That found `struct_extract` behind the filtered list comprehension on both
versions. What still stopped under both bodies was every `PIVOT` form, and on
1.5.6 every subscript and field access (`Cannot copy bound expression`).

With the two-part lookup ending in DuckDB's own catalog and
`duckdb_log_contexts`, `row_number` and `struct_extract` listed, 636 of 636
asked reaches were refused on 2.0.1 and 630 of 630 on 1.5.6, and 469 of 469
and 481 of 481 through stored views. With no caller macro, none of the 47
views was refused on either version.

With duckdb 1.5.6 first on the library path, the ten DuckDB test files of the
snapshot that need no json extension ran 99 tests and 14,451 expectations with
no failure. As of 2026-11-28 no CI job ran that version;
[#1846](https://github.com/sayuks/marginplyr/issues/1846) asks for one.

## Data-mask verbs

dplyr's `Rd` database was read with `tools::Rd_db()`. An export counted when
its topic's arguments hold `<data-masking>` or `<tidy-select>` and its name
appears as a call in the topic's usage. The ten not in
`static_spelling_names("dplyr_mask")` were `across`, `if_any`, `if_all`,
`pick`, `c_across`, `all_vars`, `any_vars`, `funs`, `vars` and `with_groups`:
the Contextual selections, the scoped verbs' helpers, and the verb ADR 0019
leaves out by name. 36 registered verbs carry no tag, all superseded or
scoped.

The same reading of tidyr gave 46 exports. `nested-tidyr-verbs-share-source.R`
puts nine tidyr verbs in a share source over a nested frame whose column
shares an earlier summary's name: `drop_na()`, `fill()`, `expand()`,
`complete()`, `nest()`, `pivot_longer()` and `unite()` were refused as
depending on the earlier summary, where plain dplyr evaluated each, and
`uncount()`, `replace_na()` and `dplyr::filter()` evaluated.

## Reads of an earlier share by a name not written

`tools/hunt/share-reader-sweep.R` called each exported function of base,
stats, utils, methods, rlang, purrr, glue, withr, magrittr, tidyselect, vctrs,
tibble, dplyr, tidyr, pillar, lifecycle and cli with 21 argument shapes that
hand it a name as text or as a value held in a variable, in a frame where
that name was an active binding recording its reads. 466 calls read it. Each
was then written as a later summary after a share of that name, over an input
holding a column of that name.

Thirteen calls returned a value that changed when only the input column
changed: `cli::cli_warn()`, `cli::format_error()` and `cli::format_warning()`
with a `{}` template; `glue::identity_transformer()` with code as text;
`purrr::invoke_map()` with `get` in three spellings; and `tibble::tibble_()`,
`tidyr::crossing_()` and `tidyr::nesting_()` with code as text or as held
language. `share-reads-through-other-evaluators.R` shows five of them beside
the written spelling: `z = 300, 300, 600`, the input column's sums, where
plain dplyr gave the share. A `dtplyr` input ended each in an error or a
Package refusal.

The same script binds a one-element list holding an environment to the
share's own name in plain dplyr, which is what a marker at the share's dot
position would leave a later read. Three of the five ended in `sum()`'s type
error, `invoke_map(get, "sh")` returned the environment itself, and
`format_error("{sh}")` returned the text `<environment>`.

A first run in one process ended in a segmentation fault inside
`rlang::node_caar()` and left `debug()` set on `environment()`, so the sweep
runs each package in a child process, restarts past a call that kills it, and
skips a list of names that end, block or reach outside the session. One
network attempt was made and refused by the sandbox before
`curlGetHeaders()` joined that list.

## Not covered

- The json and icu extensions: no form that needs either was bound
  (`j -> 'a'`, `ts AT TIME ZONE 'UTC'`), so `timezone` stayed listed and
  unmeasured.
- DuckDB syntax outside the 439 forms.
- Packages outside the 17 swept for share reads, and argument shapes outside
  the 21.
- tidyr verbs under a grouped nested frame, where `expand()` and `complete()`
  evaluate inside `dplyr::reframe()`.
