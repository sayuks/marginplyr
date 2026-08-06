# Joining a Grouping plan onto a Margin result

Investigated: 2026-08-06

Evidence gathered while settling the shape of `vignettes/recipes.qmd` (#82).
The recipes make four claims a reader cannot check for themselves — that a
lazy plan join needs a `copy` argument, that it silently loses the Margin
order, that `.format = "list"` cannot take part in one, and that a two-pass
summary expression is rejected by databases but not locally. Each was
measured rather than reasoned about, and two of #82's original acceptance
criteria did not survive the measurement.

## Environment

All measurements on 2026-08-06, one machine, macOS (Darwin 25.5.0).

| Component | Version |
|---|---|
| R | 4.6.1 |
| dplyr | 1.2.1 |
| dbplyr | 2.6.0 |
| rlang | 1.3.0 |
| duckdb (R package) | 1.5.5 |
| DuckDB engine | v1.5.5 |
| RSQLite | 3.53.3 |
| SQLite engine | 3.53.3 |

Every measurement used `retail_sales`, `.by = year`,
`.grouping = rollup(region, store)`, `.id = "set"`, and where stated
`.sort = "last"`.

## A `copy`-less lazy join is refused, and the refusal names the fix

`left_join()` of `inspect_grouping()` onto a lazy Margin result:

```
Error in `left_join()`:
! `x` and `y` must share the same source.
ℹ Use `copy = "temp-table"` to copy `y` to a temporary table.
ℹ USe `copy = "inline"` to convert `y` to inline SQL.
```

Identical on `dbplyr::simulate_postgres()`, on `dbplyr::simulate_sqlite()`,
and on a live DuckDB connection. The capitalisation of "USe" is dbplyr's, not
a transcription error.

#82 as written assumed this error was "a dbplyr error about a local table,
which does not point back here". It names two fixes. What it does not name is
`copy = TRUE`, which #82's acceptance criteria required the vignette to teach.

## Which `copy` values work

| Value | Live DuckDB | `dbplyr::simulate_sqlite()` |
|---|---|---|
| `TRUE` | works | fails |
| `"inline"` | works | works |
| `"temp-table"` | works | not attempted |

`copy = TRUE` on a simulated connection fails with

```
no applicable method for `@` applied to an object of class "SQLiteConnection"
```

because it writes a real temporary table, which a simulated connection has no
way to do. `"inline"` compiles the plan into a `VALUES` subquery instead, so
it needs neither a live connection nor write permission. For a table of one
row per grouping set that is the cheaper form as well.

Only `"inline"` therefore produces output in a documentation chunk built
without Suggested packages. `AGENTS.md` and `vignettes/recipes.qmd` are
authoritative for what the package documents.

## Which simulated connection is free of Suggests

`dbplyr::simulate_sqlite()` is not. Rendering SQL through it loads R6, and
`vignettes/database_backends.qmd:55-57` records that it also reads the
installed driver version through RSQLite, which is why that vignette guards
its simulator chunks behind `has_sqlite_simulator`.

`dbplyr::simulate_postgres()` loaded no additional namespace. Each of the
three failures above — the `copy`-less refusal, the `ORDER BY is ignored`
warning under `copy = "inline"`, and the `sql_cast_dispatch` failure for
`.format = "list"` — reproduced on it unchanged. As of 2026-08-06
`vignettes/get_started.qmd:725` used it in an unguarded chunk.

## The join destroys the Margin order, and says so only generically

On a live DuckDB connection, with `.sort = "last"`, joining the plan scrambles
the result under **all three** `copy` values. The only diagnostic is dbplyr's

```
Warning: ORDER BY is ignored in subqueries without LIMIT
ℹ Do you need to move arrange() later in the pipeline or use window_order() instead?
```

emitted twice, naming nothing in marginplyr. `show_query()` on the joined
table still prints a `# Ordered by:` line describing the Margin order key, so
the object continues to claim an order the database does not honour.

`.sort` is an argument of the Margin verb, not a verb of its own, so it cannot
be applied again after the join. The remedies measured were `arrange()` after
the join, and `collect()` before it.

`collect()`-ing the Margin result first and joining the plan locally preserved
the Margin order exactly, with no warning. This is consistent with
`CONTEXT.md`'s **Margin order** entry — "a property of the result a Margin
verb returns, not of any table derived from it" — and with ADR 0018; the
measurement establishes what that costs in practice on a lazy backend, which
neither states.

## `.format = "list"` cannot be joined lazily

`inspect_grouping(..., .format = "list")` joined with `copy = "inline"` fails
during SQL rendering:

```
no applicable method for 'sql_cast_dispatch' applied to an object of class "list"
```

on both the simulator and a live connection. The message names neither
marginplyr nor `.format`. The lazy route therefore requires the default
`.format = "text"`; `"list"` remains usable once the result is local.

## A two-pass summary expression is local-only

`sum(revenue[revenue > mean(revenue)]) / sum(revenue)` as a summary inside
`summarize_with_margins()`:

| Input | Outcome |
|---|---|
| local data frame | returns a value for every grouping set |
| DuckDB | `Binder Error: aggregate function calls cannot be nested` |
| SQLite | `misuse of aggregate function AVG()` |

Locally dplyr hands a summary expression the group's whole vector, so the
row-level comparison is already available and no escape hatch is needed. Both
databases reject the compiled nested aggregate, with different messages; the
rejection is an External condition in `CONTEXT.md`'s sense, propagated with
its own class.

`expand_with_margins()` followed by `group_by()`, a `mutate()` window, and
`summarize()` returned the same values as the local expression and stayed
lazy on DuckDB. Expansion produced 72 rows from 24 source rows for a
three-set rollup, one copy per grouping set.
