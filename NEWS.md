# marginplyr 0.1.0

* Initial CRAN submission.
* Added `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, and
  `grouping_spec()` for arbitrary SQL-style grouping plans, including empty
  sets, nesting, Cartesian products, and composite dimensions.
* Added contextual `grouping_bit()` and `grouping_id()` summary helpers.
* Added the contextual `share_of_parent()` summary helper, which divides a
  preceding numeric scalar summary by the same measure one `rollup()` level
  up, partitioned by the fixed `.by` keys. Local data frames, dbplyr, and
  dtplyr are supported, and lazy inputs stay lazy; Arrow rejects Parent shares
  before a query is built.
* Added the contextual `share_of_total()` summary helper, which divides the
  same kind of source summary by the Grand total set within each fixed `.by`
  partition. It shares every rule of `share_of_parent()` except the
  denominator, so it accepts any Grouping specification whose plan contains a
  Grand total set — `rollup()`, `cube()`, and any `grouping_sets()` including
  an empty `grouping_set()`.
* Added `inspect_grouping()` for reading the resolved Grouping plan as an
  ordinary local tibble, without executing a Margin operation.
* Added `.id` to every Margin verb for one-based Grouping set occurrence
  identifiers, including duplicate-aware local, lazy, expansion, and nesting
  paths.
* Added guides for Grouping identity and explicit key completion, and made the
  function references the canonical source of the Margin, Parent-share, and
  Margin-label contracts.
* Added explicit duplicate-set policies: `"error"`, `"drop"`, and `"keep"`.
* Changed the default display label to `"Total"`; `.margin_label = NULL`
  preserves grouping-column types and typed missing values.
* DuckDB and PostgreSQL use native `GROUPING SETS`; other backends use the
  portable `UNION ALL` adapter.
* `summarize_with_margins()`, `summarise_with_margins()`,
  `expand_with_margins()`,
  `nest_with_margins()`, and `nest_by_with_margins()` now share one normalized
  grouping-plan implementation.
* Existing `dplyr::group_by()` columns act as implicit fixed `.by` keys across
  local and lazy backends. Grouped input cannot also supply `.by`; margin
  summaries, row expansions, and regular nests return ungrouped results, while
  `nest_by_with_margins()` returns a row-wise result.
* Summary column selection now excludes every fixed key and grouping
  dimension consistently across grouping-set branches. Summary results cannot
  overwrite grouping keys, always return ungrouped output, and reject
  branch-local `cur_group*()` helpers in favor of `grouping_bit()` and
  `grouping_id()`.
* Dynamically named data-frame summaries now reserve collision-free internal
  grouping names, and opaque collisions fail with a targeted diagnostic.
  Lazy margin-label checks use portable numeric `CASE` aggregates across
  supported SQL dialects.
* Backend detection now validates the documented Arrow and dtplyr minimum
  versions, centralizes backend capabilities, and reports incompatible dbplyr
  query representations explicitly.
* Every error marginplyr raises for a correctable call now inherits the
  `"marginplyr_error"` class, so `tryCatch(marginplyr_error = )` catches them
  all. It is the only promised class; narrower subclasses and message wording
  remain implementation details. Errors from your summary expressions,
  tidyselect, dplyr, or a backend keep their original class and call, and so do
  internal invariant checks that no change to the call can avoid.
* `nest_with_margins()` and `nest_by_with_margins()` now use collision-free
  internal columns. `.keep = TRUE` retains original pre-margin key values,
  and nesting rejects duplicate sets with `.duplicates = "keep"` because
  their visible outer keys would be indistinguishable.
