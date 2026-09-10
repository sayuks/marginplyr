# marginplyr 0.1.0

* Initial CRAN submission.
* Added `grouping_set()`, `grouping_sets()`, `rollup()`, `cube()`, and
  `grouping_spec()` for arbitrary SQL-style Grouping plans, including empty
  sets, nesting, Cartesian products, and composite dimensions.
* A nested argument a Grouping specification constructor cannot read is refused
  with marginplyr's own diagnostic naming the spelling that works, in place of
  a tidyselect error about a call you never wrote. This reaches a specification
  a function of your own returned, a name that a column and a nested
  specification both claim, an empty argument, and a name written on the
  constructor's own argument (#265, #365). See *How an argument is read* in
  `?grouping_set`.
* Added contextual `grouping_bit()` and `grouping_id()` summary helpers.
  `grouping_id()` written with no columns reads every `.grouping` column of the
  resolved plan, in plan order, so widening a Grouping specification widens the
  mask instead of leaving it at its old width (#366). See `?grouping_bit`.
* Added the contextual `share_of_parent()` summary helper, which divides a
  preceding numeric scalar summary by the same measure one `rollup()` level up,
  partitioned by the fixed `.by` keys. Local data frames, dbplyr, and dtplyr
  are supported and lazy inputs stay lazy; a source marginplyr cannot establish
  as numeric is refused, and `.check_share_source = FALSE` opts out
  (#195, #196, #198). See *Eligible source summaries* in `?share_of_parent`.
* Added the contextual `share_of_total()` summary helper, which divides the
  same kind of source summary by the Grand total set within each fixed `.by`
  partition, and so accepts any Grouping plan that contains one.
* Each of those four Contextual helpers takes a bare name and accepts one
  forwarded by injection, so a function of your own can pass either
  `!!rlang::enquo(col)` or `!!rlang::ensym(col)` (#169).
* Added `inspect_grouping()` for reading the resolved Grouping plan as an
  ordinary local tibble, without executing a Margin operation. Its `.format`
  argument gives `"text"` for compact display values or `"list"` for the exact
  character vectors to program against. See *Formats and ordinary tibble
  behavior* in `?inspect_grouping`.
* Added `.id` to every Margin verb for one-based grouping set occurrence
  identifiers, including duplicate-aware local, lazy, expansion, and nesting
  paths.
* Added `.sort` to every Margin verb for an opt-in Margin order, taking
  `"none"` (the default), `"last"`, or `"first"`. It orders a result by the
  structure of its Grouping plan rather than by displayed values, so a subtotal
  sits with the rows it summarizes rather than wherever its Margin label falls,
  and lazy inputs stay lazy. See *Margin order* in `?summarize_with_margins`.
* Added the `retail_sales` data set, 24 rows of synthetic monthly sales.
  See `?retail_sales`.
* Added guides for Grouping identity and explicit key completion, and made the
  function references the canonical source of the Margin, Parent-share, and
  Margin-label contracts.
* Added explicit duplicate-set policies: `"error"`, `"drop"`, and `"keep"`.
* An argument documented as taking one of a listed set of strings —
  `.duplicates`, `.margin_label_position`, and `.sort` on the Margin verbs, and
  `.format` on `inspect_grouping()` — takes one of those strings spelled in
  full, and refuses an abbreviation and `NULL` alike. Passing the whole
  vocabulary as the signature spells it asks for the default, which is what
  lets a wrapper repeat the signature and forward the argument it was given
  (#110, #144, #210). See *Option arguments* in `?summarize_with_margins`.
* Changed the default display label to `"Total"`; `.margin_label = NULL`
  preserves grouping-column types and typed missing values.
* `.margin_label` also takes a per-dimension spelling: a named character
  vector labels each resolved Margin dimension separately, and a named list
  does the same while letting one dimension take `NULL` where another takes
  a label (#16, #371). See *Display labels and grouping identity* in
  `?summarize_with_margins`.
* `.check_margin_label` controls only the half of the Margin label collision
  check that reads your data, so it defaults to `TRUE` for local data frames
  and `FALSE` for lazy inputs. Where marginplyr holds factor-level metadata, a
  label equal to a declared factor level is rejected whatever this argument
  says. Arrow does not retain or restore those levels: it returns a factor
  dimension with a non-missing Margin label as character, permits an unused
  declared level, and refuses an observed collision only when
  `.check_margin_label = TRUE` (#122, #492).
* Added `.margin_label_position` to every Margin verb, taking `"last"` (the
  default) or `"first"`. It decides where a non-missing Margin label's
  synthetic level sits in a factor dimension's level order, and is a no-op for
  a typed-missing Margin label (#16). See *Display labels and grouping
  identity* in `?summarize_with_margins`.
* Added `.check_share_source` to `summarize_with_margins()`, `TRUE` by default
  on every backend including lazy ones, because a share source's eligibility
  can be established without reading your data. See *When marginplyr queries
  your data* in `?summarize_with_margins`.
* DuckDB and PostgreSQL use native `GROUPING SETS`; other backends use the
  portable `UNION ALL` adapter.
* `summarize_with_margins()`, `summarise_with_margins()`,
  `expand_with_margins()`,
  `nest_with_margins()`, and `nest_by_with_margins()` now share one normalized
  Grouping-plan implementation.
* Existing `dplyr::group_by()` columns act as implicit fixed `.by` keys across
  local and lazy backends. Grouped input cannot also supply `.by`; margin
  summaries, row expansions, and regular nests return ungrouped results, while
  `nest_by_with_margins()` returns a row-wise result.
* Summary column selection now excludes every fixed key and grouping
  dimension consistently across grouping-set branches. Summary results cannot
  overwrite grouping keys, always return ungrouped output, and reject
  branch-local `cur_group*()` helpers in favor of `grouping_bit()` and
  `grouping_id()`.
* Summary expressions now resolve every Contextual helper by spelling, so
  `grouping_bit()`, `grouping_id()`, `share_of_parent()`, `share_of_total()`,
  `across()`, `if_any()`, `if_all()`, `pick()`, `where()`, and the rejected
  `cur_group*()` helpers mean what marginplyr rewrites them into, and a binding
  of the same name in the calling environment never changes what the verb does
  with one. This changed three resolutions that previously ran a caller's
  binding: `across()`, `if_any()`, and `if_all()`; a shadowed `pick()` inside a
  lambda; and a `where()` qualified with a package that does not own it. See
  *Relationship to dplyr summaries* in `?summarize_with_margins`.
* An unnamed summary now takes its column name from the expression you wrote
  rather than from the one marginplyr rewrote it into, identically on every
  backend, so `sum(v) + grouping_bit(a)` and `nrow(pick(v, w))` no longer name
  a column after a SQL literal or a rewritten selection. A local input goes on
  expanding a data-frame value's columns into the result under that naming, and
  a name of your own still packs them (#430, #435).
* A data-frame-valued summary you gave a name to is no longer read for the
  names it packs, because dplyr packs a named data-frame result into the one
  column that name gives. The unnamed forms are unpacked to the top level and
  stay refused for a collision, as is a named one colliding by its own name.
* Dynamically named data-frame summaries now reserve collision-free internal
  grouping names, and opaque collisions fail with a targeted diagnostic.
  Lazy Margin-label checks use portable numeric `CASE` aggregates across
  supported SQL dialects.
* Backend detection now validates the documented Arrow and dtplyr minimum
  versions, centralizes backend capabilities, and reports incompatible dbplyr
  query representations explicitly.
* A summary Arrow's own engine cannot evaluate is now refused before any row is
  read, naming the two rewrites that compute it, in place of Arrow reading the
  whole input to answer it in R. Through arrow 16.0.0 such a call returned the
  right answer, so on those versions this is a breaking change; from arrow
  17.0.0 it aborted with an error carrying no class of its own and naming
  nothing you wrote. See
  *Summaries a lazy backend cannot carry* in `?summarize_with_margins`.
* A dtplyr input built with `dtplyr::lazy_dt(immutable = FALSE)` is now refused
  before any branch is built, because such a step lets data.table write every
  grouping-set branch back to your own table by reference. The refusal names
  the one rewrite, `dtplyr::lazy_dt(immutable = TRUE)`, which is what
  `lazy_dt()` does by default. See *Other lazy backends* in
  `vignette("database_backends")`.
* A `.grouping` or `.by` selection whose failure the column names already
  settle is now refused without querying your input, so a disconnected or slow
  connection no longer hands you its own failure in place of the column
  diagnostic. This reaches the set-difference operator `/`, the arithmetic and
  scalar-boolean spellings `*`, `^`, `&&`, and `||`, `one_of()`, a formula, and
  the `.data` pronoun; a selection carrying `where()` still resolves against
  your input's column types.
* Every error marginplyr raises for a correctable call now inherits the
  `"marginplyr_error"` class, so `tryCatch(marginplyr_error = )` catches them
  all, and the columns, values, and arguments such an error quotes are spelled
  as you spelled them. Errors from your summary expressions, tidyselect, dplyr,
  or a backend keep their original class, diagnostic, and cause, and so do
  internal invariant checks. See *Errors and warnings* in `?marginplyr`.
* An argument you leave empty is now answered by name, in place of R's own
  missing-argument error: a summary written `z = ` is refused as an empty
  summary named `z`, an unnamed one by its position, and an empty `.by` selects
  no columns as it does in dplyr (#340). An empty argument one level further
  down — inside a selection, as in `.by = c(, region)` — is read as tidyselect
  reads one (#351).
* A selection a wrapper forwards into `across()` is now the selection dplyr
  makes of it, so `across({{ cols }}, sum)` works instead of being refused with
  advice to wrap it in `where()`, and a forwarded omission selects no columns
  at both the standalone and nested positions. An `across()` `.names` template
  that cannot be evaluated now reaches dplyr, which names the argument you
  wrote (#349, #350).
* A selection or a source your wrapper forwards into a share now reaches the
  share it names. `across({{ cols }}, share_of_total)` and
  `share_of_total({{ col }})` — with the `!!rlang::enquo()` and
  `!!rlang::ensym()` spellings of each — reported `object 'cols' not found`,
  naming your own wrapper's argument in place of the summary you wrote (#357).
* A Margin verb piped into another one now names itself when it refuses.
  `f(x) |> g()` runs `f` while `g` reads its input, and `g` reported every
  marginplyr error raised there against its own call (#455).
* A condition raised while your summary expression runs now reports its context
  in names you can act on: grouping values under the columns you wrote rather
  than internal ones, and an error blaming the Margin verb you called. A
  warning every grouping set raises is reported once, saying how many further
  sets raised it, in place of one identical warning per set (#141, #108, #411,
  #432). See *Errors and warnings* in `?marginplyr`.
* Added `.key` to `nest_with_margins()` and `nest_by_with_margins()`, a string
  naming the list column. Each follows the function it resembles for `NULL`:
  `nest_with_margins()` reads it as `"data"`, as `tidyr::nest()` does, and
  `nest_by_with_margins()` refuses it, as `dplyr::nest_by()` does. See `.key`
  in `?nest_with_margins` and `?nest_by_with_margins`.
* `nest_with_margins()` and `nest_by_with_margins()` now use collision-free
  internal columns, and reject duplicate sets with `.duplicates = "keep"`
  because their visible outer keys would be indistinguishable.
* `.keep = TRUE` nests the original, pre-margin values of the fixed `.by`
  columns and grouping dimensions inside each nested data frame, so an outer
  `East / Total` subtotal keeps the store values it stands for while its outer
  keys go on showing the Margin label. See *Relationship to tidyr and dplyr* in
  `?nest_with_margins`.
* A nesting that leaves no payload column now nests one inner row per source
  row, on detail groups, subtotals, and the Grand total set alike, as
  `dplyr::nest_by()` does, and local and `dtplyr` results agree once collected.
  An input with rows and no columns is outside that agreement, because a
  `data.table` reads its row count from its first column and so cannot carry
  those rows in at all.
* Attaching a Grouping set identifier to a `dtplyr` input with no columns no
  longer invents a row, which `expand_with_margins(.id = )` and both nesting
  verbs used to report. Such an input now expands to the row count the local
  backend gives it, and the lazy path stays lazy (#184).
* A `data.frame` subclass whose `[` is not column selection — a raw
  `data.table` is the case — now reaches every Margin verb that accepts local
  data frames, and is not modified by reference. The result's class still
  follows the dplyr verb the Margin verb ends in, and is not promised to be the
  input's.
* Added `last_sent_queries()` and the `marginplyr.audit_sql` option, off by
  default. With the option set to `TRUE`, a Margin verb against a SQL backend
  records the queries it sends — the `"result"` query it hands you unexecuted,
  and the ones it sends for its own reasons on the way — and the accessor reads
  the most recent call's record back as a two-column tibble of `purpose` and
  `sql`. Piping one Margin verb into another is two calls, and the record you
  read is the outer one's alone (#318, #400, #401, #409, #455). See *What the
  record promises* in `?last_sent_queries`.
