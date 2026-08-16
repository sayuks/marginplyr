# marginplyr 0.1.0

* Initial CRAN submission.
* Added `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, and
  `grouping_spec()` for arbitrary SQL-style grouping plans, including empty
  sets, nesting, Cartesian products, and composite dimensions.
* A nested Grouping specification is recognized by how it is written: a call to
  one of those constructors, or a name bound to a specification. Anything else
  is a column selection, which is why a function of your own returning a
  specification is accepted as `.grouping` itself and refused inside another
  specification. That refusal is now marginplyr's own diagnostic, naming the
  recognized forms and the binding that works — `s <- my_spec(region)`, then
  `grouping_sets(s, grade)` — rather than tidyselect reporting the
  specification as an unusable column selection.
* Added contextual `grouping_bit()` and `grouping_id()` summary helpers.
* Added the contextual `share_of_parent()` summary helper, which divides a
  preceding numeric scalar summary by the same measure one `rollup()` level
  up, partitioned by the fixed `.by` keys. Local data frames, dbplyr, and
  dtplyr are supported, and lazy inputs stay lazy; Arrow rejects Parent shares
  before a query is built. A share source must be a plain integer or double on
  every backend: where the type is not readable without asking, marginplyr
  asks each SQL dialect once, with at most two queries that read none of your
  data, whether it converts a non-numeric value to a number instead of
  refusing it, and refuses the share rather than calculate one from a source
  nothing has checked. `.check_share_source = FALSE` opts out for a source you
  have established yourself (#195, #196).
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
* Added `.sort` to every Margin verb for an opt-in Margin order, taking
  `"none"` (the default), `"last"`, or `"first"`. It orders a result by the
  structure of its Grouping plan rather than by displayed values: within each
  fixed `.by` key, every grouping dimension contributes its Grouping bit and
  its missingness before its own value, so a subtotal sits with the rows it
  summarizes whatever the Margin label sorts as, and the grand total comes
  last. Factor dimensions follow their restored levels, missing values come
  last wherever they appear on every backend, `"first"` reverses the Grouping
  bits alone, and lazy inputs stay lazy on a native `GROUPING SETS`
  plan as well as the portable one. As with `dplyr::arrange()`, the order is a
  property of the returned object and may not survive further verbs applied to
  a lazy result. `compute()` materializes a sorted lazy result in the Margin
  order; it records no dbplyr window ordering, because the key reads Grouping
  bits from a column the result does not expose.
* Added guides for Grouping identity and explicit key completion, and made the
  function references the canonical source of the Margin, Parent-share, and
  Margin-label contracts.
* Added explicit duplicate-set policies: `"error"`, `"drop"`, and `"keep"`.
* Changed the default display label to `"Total"`; `.margin_label = NULL`
  preserves grouping-column types and typed missing values.
* `.check_margin_label` controls only the half of the Margin label collision
  check that reads the data: whether an actual value of a Margin dimension
  equals its display label. It defaults to `TRUE` for local data frames and
  `FALSE` for lazy inputs, which are read only when asked. A label equal to a
  declared factor level is rejected on every backend whatever this argument
  says, because the level is already known from the column's metadata and
  finding it sends no query (#122).
* Added `.check_share_source` to `summarize_with_margins()`, `TRUE` by
  default on every backend, including lazy ones, for the same reason: a share
  source's eligibility can be established without reading your data, so the
  check runs unless you opt out. See `share_of_parent()` and
  `share_of_total()` above.
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
* Summary expressions now resolve every contextual helper by spelling.
  `grouping_bit()`, `grouping_id()`, `share_of_parent()`, `share_of_total()`,
  `across()`, `if_any()`, `if_all()`, `pick()`, `where()`, and the rejected
  `cur_group*()` helpers mean what marginplyr rewrites them into, whether they
  are written bare or qualified with the package that owns them, and a binding
  of the same name in the calling environment never changes what the verb does
  with one. A qualifier naming any other package is an ordinary call, and every
  other name — `dplyr::n()` included — follows ordinary lookup. Three
  resolutions changed: `across()`, `if_any()`, and `if_all()` previously ran a
  caller's binding while the rules that reject a selection were checked against
  dplyr's helper; a shadowed `pick()` ran a caller's binding inside a `~`
  lambda or a `function` body, which is the one position plain dplyr also lets
  it; and a `where()` qualified with a package that does not own it is no
  longer read as a selection predicate inside a contextual share's `across()`.
  Redundant parentheses are transparent to all of this, around the name or
  around the whole call, so `(pick)(units)` and `(pick(units))` are the same
  request as `pick(units)`; a nested `(rollup(region))` is the specification it
  is, and `(share_of_total(units))` is a Total share. A head that has to be
  evaluated to know what it calls — `get("pick")(units)` — is an ordinary call
  as it was.
  See *Relationship to dplyr summaries* in `?summarize_with_margins`.
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
* A nesting that leaves no payload column now nests one inner row per source
  row, on detail groups, subtotals, and the Grand total set alike, as
  `dplyr::nest_by()` does, and local and `dtplyr` results agree once collected.
  An input with no columns at all is outside that agreement, because a
  `data.table` cannot represent one. The class of such a cell is described
  rather than promised, as every nested element class is: on both backends it
  is what `dplyr::tibble()` produced.
* A `data.frame` subclass whose `[` is not column selection — a raw
  `data.table` is the case — now reaches every Margin verb that accepts local
  data frames. Factor levels and the prototypes behind an absent Margin label
  are read one column at a time, so a subclass that reads a character index as
  a join key no longer fails before any grouping happens, and the input is not
  modified by reference. The result's class still follows the dplyr verb the
  Margin verb ends in, and is not promised to be the input's.
