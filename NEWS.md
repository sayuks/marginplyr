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
  specification as an unusable column selection. It is also the diagnostic for
  a specification stored as a function, which tidyselect calls as a column
  predicate rather than refusing, so the call reported an error about a call
  you never wrote (#265). A name both readings claim —
  a column of your input that is also bound to a nested specification the
  position accepts — is refused rather than resolved by whichever the data
  happens to have, and the refusal names the spelling for each reading:
  `all_of("s")` for the column, `!!s` for the specification. Redundant
  parentheses are transparent to every one of these readings, as they are
  elsewhere, because `(` is the identity function: `(s)`, `(!!s)`, and
  `(rollup(region))` are the arguments they wrap, however many pairs deep, and
  both refusals reach a parenthesized argument as they reach a bare one. An
  argument you left empty gets neither reading, and is refused naming the
  constructor and the position — `grouping_sets(, region)` reports that its
  first argument is empty — rather than reporting an internal name you never
  wrote. A trailing comma is not an empty argument, because R captures no
  argument for it. A name on the constructor's own argument is refused too:
  `rollup(area = region)` reports the name rather than dropping it, and so a
  Margin verb's own argument written one pair of parentheses in —
  `rollup(region, .by = year)` — is reported rather than taken as a second
  dimension (#365).
* Added contextual `grouping_bit()` and `grouping_id()` summary helpers.
  `grouping_id()` written with no columns reads every `.grouping` column of the
  resolved plan, in plan order, which is the order `inspect_grouping()` reports:
  a bare `grouping_id()` is `inspect_grouping()$grouping_id` for the Grouping
  set the row came from, as `.id` is `set_id` for its occurrence. Retyping the
  columns is still accepted, and is how a subset of them or an order other than
  the plan's is encoded, but a retyped list is a copy of a specification written
  in the same call: widening `rollup(cut, color)` to
  `rollup(cut, color, clarity)` renumbered the levels while
  `grouping_id(cut, color)` went on returning a two-bit mask (#366). Columns
  fixed by `.by` are not part of the default, each contributing a bit that is
  always zero. A plan with no dimensions gives `0L`, and one past the 31-column
  cap is refused as a written call of that width is.
* Added the contextual `share_of_parent()` summary helper, which divides a
  preceding numeric scalar summary by the same measure one `rollup()` level
  up, partitioned by the fixed `.by` keys. Local data frames, dbplyr, and
  dtplyr are supported, and lazy inputs stay lazy; Arrow rejects Parent shares
  before a query is built. A share source must be a plain integer or double on
  every backend: where the type is not readable without asking, marginplyr
  asks each SQL dialect, with at most two queries that read none of your data,
  whether it converts a non-numeric value to a number instead of refusing it,
  and refuses the share rather than calculate one from a source nothing has
  checked. An answer is a property of the dialect, so a dialect that answers is
  asked once; a question a dropped connection or a resuming warehouse left
  unanswered is asked again on the next share request rather than refusing
  every later share on that dialect for the rest of the session.
  `.check_share_source = FALSE` opts out for a source you have established
  yourself (#195, #196, #198).
* Added the contextual `share_of_total()` summary helper, which divides the
  same kind of source summary by the Grand total set within each fixed `.by`
  partition. It shares every rule of `share_of_parent()` except the
  denominator, so it accepts any Grouping specification whose plan contains a
  Grand total set — `rollup()`, `cube()`, and any `grouping_sets()` including
  an empty `grouping_set()`.
* Each of those four contextual helpers takes a bare name, and accepts one
  forwarded by injection: a function of your own can pass either
  `!!rlang::enquo(col)` or `!!rlang::ensym(col)`. Only the name is read. It is
  resolved against the Grouping plan or among the preceding summaries, so the
  environment `rlang::enquo()` captured is not consulted, and an injection
  carrying anything but a name is refused exactly where writing that expression
  out would be, saying what was injected (#169).
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
  bits from a column the result does not expose. A dimension descending is the
  same key written as an `arrange()` of your own, and rows ordered by a measure
  is that key with one column added; the recipes guide shows both.
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
* An unnamed summary now takes its column name from the expression you wrote
  rather than from the one marginplyr rewrote it into. `...` is documented as
  `dplyr::summarize()`'s name-value pairs, and the rewrite ran first: a
  `grouping_bit()` or `grouping_id()` became the branch's own `0L` or `1L`, so
  `sum(v) + grouping_bit(a)` named a different column in each Grouping-set
  branch — which stopped the operation inside an internal invariant on every
  backend taking the portable `UNION ALL` path, and named the column after a
  SQL literal on the backends running `GROUP BY GROUPING SETS`. A selection
  helper became a qualified `all_of()` literal, so `nrow(pick(v, w))` named its
  column `nrow(dplyr::pick(dplyr::all_of(c("v", "w"))))`. Both are now named
  after the caller's own expression, identically on every backend. A summary no
  rewrite reaches is named by dplyr exactly as before, and so is a summary
  written as `across()` or `pick()` itself, so both go on expanding a
  data-frame value's columns into the result. A data-frame-valued summary a
  rewrite *does* reach is now named, and dplyr packs a named one:
  `range_frame(pick(x))` returned `lo` and `hi` and now returns one
  data-frame column named after the call. Telling that summary from
  `nrow(pick(v, w))` is a question about the value's type rather than about how
  it is spelled, so give either one a name of your own where you want the
  columns expanded or packed regardless (#430).
* Dynamically named data-frame summaries now reserve collision-free internal
  grouping names, and opaque collisions fail with a targeted diagnostic.
  Lazy margin-label checks use portable numeric `CASE` aggregates across
  supported SQL dialects.
* Backend detection now validates the documented Arrow and dtplyr minimum
  versions, centralizes backend capabilities, and reports incompatible dbplyr
  query representations explicitly.
* A summary Arrow's own engine cannot evaluate is now refused, before any row
  is read. Arrow answers such an expression by reading the whole input — every
  column, not only the ones the summary names — and computing it in R; the
  refusal names the argument you wrote and the two rewrites that compute it,
  collecting first and selecting the columns you need before you collect.
  Which expressions this reaches is Arrow's to decide and moves with its
  version; `?summarize_with_margins` describes the shapes. Ordinary numeric
  summaries, and arithmetic over them, are unaffected and stay lazy, and an
  Arrow dataset keeps raising Arrow's own error for the same expressions.

  What this replaces depends on your Arrow. From arrow 17.0.0 such a call
  aborted with `object of type 'special' is not subsettable`, an error carrying
  no class of its own and naming nothing you wrote. Through arrow 16.0.0 it
  returned the right answer, having read the whole input to get it — so on
  those versions this is a breaking change, and the rewrites the refusal names
  reproduce the old result while letting you choose what is read.
* A `.grouping` or `.by` selection whose failure the column names already
  settle is now refused without querying your input. This reaches the
  set-difference operator `/` that tidyselect reads, the arithmetic and
  scalar-boolean spellings it refuses in a selection — `*`, `^`, `&&`, and
  `||` — and `one_of()`. Each of these used to send the zero-row query first
  on a backend that needs one, so on a disconnected or slow connection you
  were handed the connection's failure instead of the column diagnostic. A
  selection carrying `where()` still resolves against your input's column
  types, including one written under `/`. It reaches two more spellings: a
  formula, which tidyselect refuses on sight whatever it wraps, and the
  `.data` pronoun, whose name your input either holds or does not. Where you
  have turned a deprecation into an error with
  `options(lifecycle_verbosity = "error")`, the deprecation `.data` carries in
  a selection is now raised before the query too.
* Every error marginplyr raises for a correctable call now inherits the
  `"marginplyr_error"` class, so `tryCatch(marginplyr_error = )` catches them
  all. It is the only promised class; narrower subclasses and message wording
  remain implementation details. The columns, values, and arguments such an
  error quotes are yours, though, and it spells them as you spelled them —
  except that a line break and a no-break space inside a name are both shown as
  an ordinary space. Errors from your summary expressions, tidyselect, dplyr,
  or a backend keep their original class, diagnostic, and cause, and so do
  internal invariant checks that no change to the call can avoid.
* An argument you leave empty is now answered by name, in place of R's own
  missing-argument error. A summary written `z = ` — or forwarded by a wrapper
  whose own caller omitted the column it passes on — is refused as an empty
  summary named `z`, where it used to report `argument "expr" is missing, with
  no default`; an unnamed one is refused by its position, the first being
  `..1`. A trailing comma is not an empty argument, here as in a Grouping
  specification constructor, because no argument is captured for one. An empty
  `.by` selects no columns, as it does in dplyr, in place of `attempt to use
  zero-length variable name`; it is still an argument you supplied, so grouped
  input carrying one is refused as it was. An empty `.grouping` is the plan you
  get from omitting it, which is what writing `.grouping = ` already means.
  Redundant parentheses are transparent at all three positions, as they are
  elsewhere: an injected pair wrapping nothing is the empty argument it wraps
  (#340). An empty argument one level further down — inside a selection, as in
  `.by = c(, region)` or `rollup(c(, grade))` — is read as tidyselect reads
  one, which under `c()` is to select nothing for it, in place of the same
  `attempt to use zero-length variable name`. The refusal above reaches a
  constructor argument's own position and not a selection written there, so
  `rollup(, grade)` is still refused (#351).
* A selection a wrapper forwards into `across()` is now the selection dplyr
  makes of it. `across({{ cols }}, sum)` — the ordinary way to pass on a
  selection your own caller wrote — was refused by tidyselect, which advised
  wrapping it in `where()`, whatever columns it named, because the forwarded
  selection was read as the formula shorthand for a predicate. Forwarding an
  argument your caller omitted is one value of the same spelling, and it now
  selects no columns, which is what dplyr selects for it; a selection you write
  empty yourself still means every eligible column, as it did. dplyr reads a
  nested `across()` — one inside another expression rather than standing as the
  summary — differently, taking its deprecated `everything()` default for a
  forwarded omission; marginplyr resolves the selection wherever it is written,
  so the reading above is the one you get at both positions. A share's
  `across()` reads its selection the same way, so a forwarded name that is not
  an eligible preceding summary is now refused by that name rather than
  reported as an unusable selection. An `across()` `.names` template that
  cannot be evaluated — forwarded empty, naming something absent, or a pair of
  parentheses holding nothing — reaches dplyr, which names the argument you
  wrote, in place of R's bare error naming nothing (#349, #350).
* A selection or a source your wrapper forwards into a share now reaches the
  share it names. `across({{ cols }}, share_of_total)` and
  `share_of_total({{ col }})` — with the `!!rlang::enquo()` and
  `!!rlang::ensym()` spellings of each — reported `object 'cols' not found`,
  naming your own wrapper's argument in place of the summary you wrote: the
  check marginplyr wraps around a share's source carried your call back into
  the summary, where dplyr defused it a second time and expanded the
  forwarding where that argument is not bound. The check carries no call
  there now, and a share's conditions still name the call you wrote,
  forwarding and all (#357).
* A condition raised while your summary expression runs now reports its
  context in names you can act on. A margin operation summarizes that
  expression once per grouping set, so the grouping values it reported were
  those of internal `..marginplyr_key_N` columns; they now name the columns you
  wrote, and an error blames the Margin verb you called rather than an internal
  summary. A warning every grouping set raises is reported once and says how
  many further grouping sets raised it, in place of one identical warning per
  set — `2^k` of them for a `cube()` of `k` dimensions — including a warning
  whose own text spells the line dplyr writes to point at
  `dplyr::last_dplyr_warnings()`, while warnings that differ from each other
  are still reported one by one. Only that context changes: the class, the
  diagnostic, and the cause you receive are the ones raised. A lazy input is
  unaffected, because its summary expressions run when you collect the result
  rather than while the verb runs (#141, #108).
* `nest_with_margins()` and `nest_by_with_margins()` now use collision-free
  internal columns. `.keep = TRUE` retains original pre-margin key values,
  and nesting rejects duplicate sets with `.duplicates = "keep"` because
  their visible outer keys would be indistinguishable.
* A nesting that leaves no payload column now nests one inner row per source
  row, on detail groups, subtotals, and the Grand total set alike, as
  `dplyr::nest_by()` does, and local and `dtplyr` results agree once collected.
  An input with rows and no columns is outside that agreement, and the limit is
  on what reaches the backend rather than on nesting: a `data.table` reads its
  row count from its first column, so `dtplyr::lazy_dt()` cannot carry those
  rows in and nothing here can restore them. The class of such a cell is
  described rather than promised, as every nested element class is: on both
  backends it is what `dplyr::tibble()` produced.
* Attaching a Grouping set identifier to a `dtplyr` input with no columns no
  longer invents a row. Because a `data.table` reads its row count from its
  first column, giving a column-less one a column materialized exactly one
  row, and a verb whose result keeps that column reported it:
  `expand_with_margins(.id = )`, and both nesting verbs, which add an
  identifier internally whatever `.id` says. A column-less input that reaches
  the backend with the rows it had — one with no rows either — now expands to
  the row count the local backend gives it, and the lazy path stays lazy. The
  neighboring limit with the same cause has no fix and is documented on
  `summarize_with_margins()`: a summary asked for no summaries and given no
  key has no column to be a row of, so `dtplyr` collects zero rows where a
  local input returns one (#184).
* A `data.frame` subclass whose `[` is not column selection — a raw
  `data.table` is the case — now reaches every Margin verb that accepts local
  data frames. Factor levels and the prototypes behind an absent Margin label
  are read one column at a time, so a subclass that reads a character index as
  a join key no longer fails before any grouping happens, and the input is not
  modified by reference. The result's class still follows the dplyr verb the
  Margin verb ends in, and is not promised to be the input's.
* Added `last_sent_queries()` and the `marginplyr.audit_sql` option. With the
  option set to `TRUE`, a Margin verb against a SQL backend records its
  `"result"` query -- the one it returns unexecuted, rendered before it is
  handed to you -- and the accessor reads the record of the most recent call
  back as a two-column tibble of `purpose` and `sql`. The record promises the
  SQL marginplyr sent and not the execution it caused, so an audited call on
  a data frame or a `dtplyr` table records nothing. Reading before any call
  has run, or after a call made while the option was off, is refused rather
  than answered with zero rows, and a statement dbplyr cannot translate for
  the backend is recorded as `NA` without failing the call. The record also
  holds the queries marginplyr sends for its own reasons on the way: the
  zero-row selection proxy it reads a lazy input's columns from; the scan
  under `.check_margin_label` looking for an observed value equal to a Margin
  label; and the question it asks a dialect before calculating a contextual
  share, with the control that tells a refusal from a question that could
  not be put. Each is recorded before it is sent, so a call refused by a
  collision or by an ineligible share source leaves readable every query it
  had already sent. A call one of these verbs refused before sending anything
  reads back as the empty record it is, rather than as the previous call's,
  and is a call the session has recorded. The option is off by default
  (#318, #400, #401, #409).
