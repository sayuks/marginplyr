# marginplyr 0.1.0

Initial CRAN release.

* Four Margin verbs produce detail rows, subtotals, and grand totals in one
  call: `summarize_with_margins()` (and its synonym
  `summarise_with_margins()`) summarizes every grouping set,
  `expand_with_margins()` emits one copy of each source row per grouping set,
  and `nest_with_margins()` and `nest_by_with_margins()` nest the rows behind
  every detail group, subtotal, and total.

* Grouping specifications describe the reporting levels: `rollup()` for a
  hierarchy, `cube()` for every combination, `grouping_sets()` and
  `grouping_set()` for an exact list, and `grouping_spec()` for the Cartesian
  product SQL forms from comma-separated items. Specifications nest, accept
  tidy-select expressions, and support composite dimensions and empty sets.
  `.grouping` takes one of these calls; a bare column or `c()` is refused with
  the constructor call to write instead.

* Columns in `.by`, or existing `group_by()` columns, stay fixed in every
  grouping set, so one call produces an independent report per partition.

* `grouping_bit()` and `grouping_id()`, the counterparts of SQL `GROUPING()`,
  identify which dimensions a row's grouping set removed, so a total never has
  to be recognized from its label or told apart from a source `NA` by
  guesswork. `.id` adds a Grouping set identifier to every Margin verb.

* `share_of_parent()` divides a summary by the same summary one `rollup()`
  level up, and `share_of_total()` divides it by the grand total, within each
  fixed `.by` partition.

* `inspect_grouping()` returns the resolved Grouping plan as a tibble without
  running a Margin operation, for checking what a specification asks for.

* `.margin_label` sets the display label of removed dimensions (`"Total"` by
  default, one label per dimension, or `NULL` for typed missing values),
  `.margin_label_position` places that label in a factor's levels, and
  `.check_margin_label` refuses a label that collides with a source value.
  `.sort` puts each subtotal next to the rows it summarizes, and
  `.duplicates` chooses whether repeated grouping sets are an error, dropped,
  or kept.

* Local data frames, dtplyr, dbplyr, and Arrow inputs are supported, and lazy
  inputs stay lazy. DuckDB and PostgreSQL summaries use native
  `GROUP BY GROUPING SETS`; other SQL backends, and every row expansion, use a
  portable `UNION ALL` translation. The nesting verbs support local data frames
  and dtplyr.

* `last_sent_queries()`, with `options(marginplyr.audit_sql = TRUE)`, records
  the SQL a Margin verb prepares or sends.

* Every error marginplyr raises for a correctable call inherits the
  `"marginplyr_error"` class; see `?marginplyr_conditions`.

* Guides cover getting started, common reporting recipes, Grouping identity,
  completing absent keys, and database and lazy backends.

* `retail_sales` is a small synthetic monthly sales data set used throughout
  the examples.
