# marginplyr 0.1.0

* Initial CRAN submission.
* Added `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, and
  `grouping_spec()` for arbitrary SQL-style grouping plans, including empty
  sets, nesting, Cartesian products, and composite dimensions.
* Added contextual `grouping()` and `grouping_id()` summary helpers.
* Added explicit duplicate-set policies: `"error"`, `"drop"`, and `"keep"`.
* Changed the default display label to `"Total"`; `.margin_label = NULL`
  preserves grouping-column types and typed missing values.
* DuckDB and PostgreSQL use native `GROUPING SETS`; other backends use the
  portable `UNION ALL` adapter.
* `summarize_with_margins()`, `union_all_with_margins()`,
  `nest_with_margins()`, and `nest_by_with_margins()` now share one normalized
  grouping-plan implementation.
