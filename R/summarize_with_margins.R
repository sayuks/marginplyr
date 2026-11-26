#' Summarize data with SQL-style grouping operations
#'
#' [summarize_with_margins()] extends [dplyr::summarize()] with grouping sets,
#' rollups, cubes, totals, and subtotals. The same interface works with local
#' data frames and lazy tables.
#'
#' Use it when one report needs several levels of detail—for example, stores,
#' region subtotals, and a company total—or when that report should use the
#' same code locally and on a database table opened with [dplyr::tbl()]. The
#' [Get started
#' guide][started] builds that report from a hand-written version, and is the
#' page to read before this one.
#'
#' [started]: https://sayuks.github.io/marginplyr/vignettes/get_started.html
#'
#' @param .data A data frame or lazy table.
#' @param ... Name-value pairs as used in [dplyr::summarize()]. Contextual
#'   helpers [grouping_bit()], [grouping_id()], [share_of_parent()], and
#'   [share_of_total()] can also be used here.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns included in
#'   every grouping set. These columns never receive `.margin_label`. When
#'   `.data` is grouped and `.by` is `NULL`, its grouping columns are used as
#'   implicit fixed keys. A fixed key is a column of the input, so a selection
#'   cannot rename it: `c(area = region)` is an error rather than a fixed key
#'   named `area`. Rename the result afterwards with [dplyr::rename()].
#' @param .grouping A Grouping specification made with [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. A bare
#'   column or a `c()` of columns is refused; wrap it in a constructor, as in
#'   `.grouping = rollup(region)`. `NULL` is one grouping set with no Margin
#'   dimension, so only the fixed `.by` keys group the result. The
#'   specification expands to the call's Grouping plan, which
#'   [inspect_grouping()] returns, and the columns it selects are the call's
#'   Margin dimensions; see
#'   *[Fixed columns and Margin dimensions][margin_verbs]* in [margin_verbs].
#'   See [grouping_set()] for how one specification nests inside another.
#' @param .margin_label A display label for dimensions omitted from a grouping
#'   set. An unnamed character scalar applies to every resolved Margin
#'   dimension. A named character vector or named list must name every
#'   resolved Margin dimension exactly once; order is irrelevant, and fixed
#'   `.by` columns must not be named. `NA_character_` and `NULL` use typed
#'   missing values instead of a display label; only the list can give one
#'   dimension a `NULL` while another takes a label, because `c()` drops a
#'   `NULL` element. See
#'   *[Display labels and grouping identity][margin_verbs]* in [margin_verbs].
#' @param .margin_label_position Either `"last"` (the default) or `"first"`,
#'   and nothing else; see *[Option arguments][margin_verbs]* in
#'   [margin_verbs].
#'   This controls the position of a non-missing synthetic label in factor and
#'   ordered-factor levels. It does not sort result rows and has no effect for
#'   `NA_character_` or `NULL`.
#' @param .check_margin_label Whether to check that no value of a Margin
#'   dimension equals that dimension's display label. The default is `TRUE`
#'   for a local data frame and `FALSE` for a lazy input the verb accepts --
#'   a `dtplyr` step, and for a summary or expansion also a database table or
#'   an Arrow object -- because there the check runs a query that reads your
#'   data; see *[When marginplyr queries your data][margin_verbs]* in
#'   [margin_verbs], and *[dtplyr inputs][nest_by_with_margins]* in
#'   [nest_by_with_margins()] for the one exception. A collected input is a
#'   local data frame, so collecting turns the check on; set this explicitly
#'   when re-running a call on collected data.
#'
#'   A label equal to a declared factor level is refused wherever marginplyr
#'   holds the levels, whatever this argument says, because finding it reads
#'   no rows. A typed-missing label, `NULL` or `NA_character_`, takes its
#'   dimension out of the check. Left unchecked, a colliding value gives the
#'   result a margin row and a source row that no key column tells apart;
#'   keeping `.id` in the result distinguishes them.
#' @param .check_share_source Whether to refuse a [share_of_parent()] or
#'   [share_of_total()] whose source summary marginplyr cannot establish is
#'   an integer or double. It matters only on a backend that cannot refuse a
#'   non-numeric source itself, such as SQLite and a `dbplyr::simulate_*()`
#'   connection: there the default `TRUE` refuses the share, and `FALSE`
#'   computes it anyway. Elsewhere it changes nothing. It applies only to the
#'   call it is written in. See
#'   *[Lazy execution boundaries][share_of_parent]* in [share_of_parent()].
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, and nothing
#'   else; see *[Option arguments][margin_verbs]* in [margin_verbs]. A
#'   duplicate is a grouping set whose columns are the same as an earlier
#'   set's once the Grouping specification is expanded, as in
#'   `grouping_sets(region, store, region)`. `"error"` (the
#'   default) refuses the call and names the positions. `"drop"` keeps the
#'   first occurrence of each set and removes the rest. `"keep"` returns the
#'   rows of every occurrence, so a duplicated set's rows appear once per
#'   occurrence; `.id` tells the copies apart.
#' @param .sort One of `"none"` (the default), `"last"`, or `"first"`, and
#'   nothing else; see *[Option arguments][margin_verbs]* in [margin_verbs].
#'   `"none"` leaves row order unspecified. `"last"` places each margin row
#'   after the rows it summarizes and `"first"` before them, within each fixed
#'   `.by` key, with missing values last. Under a non-missing
#'   `.margin_label` a numeric dimension sorts as text; `.margin_label = NULL`
#'   or a factor keeps numeric order. See *[Margin order][margin_verbs]* in
#'   [margin_verbs].
#' @param .id `NULL`, or one non-missing, non-empty character string naming an
#'   integer output column of one-based Grouping set identifiers. Each value
#'   identifies one occurrence in the resolved Grouping plan. The name must not
#'   collide with source columns, grouping keys, or other output columns. See
#'   *[Grouping set identifiers][margin_verbs]* in [margin_verbs].
#'
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy,
#'   with one row per group per grouping set. Its columns are the fixed `.by`
#'   keys, then the Margin dimensions, then `.id` when supplied, then the
#'   summaries in the order written. Its class follows [dplyr::summarize()],
#'   except for the leading class described in *SQLite materialization*; see
#'   *[Result metadata][margin_verbs]* in [margin_verbs]. On zero-row input,
#'   the Grand total set still gives one row when there is no fixed `.by` key,
#'   as [dplyr::summarize()] does without groups; every other grouping set,
#'   and every set once a fixed key is given, gives none.
#'
#'   With a character `.margin_label`, the default, a Margin dimension that
#'   is not a factor becomes character so that it can hold the label:
#'   `.grouping = rollup(year)` over an integer `year` returns `year` as
#'   character, with `"Total"` on the margin row. On a lazy input the text is
#'   the backend's own cast, so a double `10` reads `"10.0"` on DuckDB and
#'   SQLite and a logical `TRUE` reads `"true"` or `"1"`; *Converted
#'   dimension text* in [Database and lazy backends][db] tabulates them. When
#'   the text matters, give the column the type you want with [dplyr::mutate()]
#'   first. A factor dimension instead gains the label as a new level.
#'   `.margin_label = NULL` or `NA_character_` keeps every dimension's type and
#'   marks margin rows with a missing value.
#'
#'   [db]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html#converted-dimension-text
#'
#' @details
#' [grouping_sets()] forms a union of grouping families. [grouping_spec()]
#' combines its arguments by Cartesian product, matching comma-separated SQL
#' `GROUP BY` items. [grouping_set()] is also used to keep multiple columns
#' together as one composite dimension inside [rollup()] or [cube()].
#'
#' Grouping specifications accept column selections, not arbitrary SQL
#' expressions. Create computed grouping columns with [dplyr::mutate()] first.
#'
#' [summarize_with_margins()] and [summarise_with_margins()] are synonyms,
#' following [dplyr::summarize()] and [dplyr::summarise()].
#'
#' @section Differences from summarize():
#' - The result is always ungrouped, whatever groups the input had, because
#'   a result combining several grouping sets has no single grouping to
#'   retain. There is no `.groups` argument; passing one is an error.
#' - Row order is unspecified. Use `.sort` for margins next to the rows they
#'   summarize, or [dplyr::arrange()] for any other order.
#' - A Margin dimension can change type to hold the Margin label; see
#'   Value.
#' - Each summary must return one value per group, and there is no
#'   [dplyr::reframe()] counterpart. Return a list column, as in
#'   `list(range(revenue))`, or call [expand_with_margins()] and then
#'   [dplyr::reframe()] by `.id` and the key columns.
#' - A summary cannot overwrite a fixed key or Margin dimension, including
#'   through an unnamed data-frame-valued summary. Local dplyr allows it but
#'   other backends may not, so marginplyr refuses it everywhere. Give the
#'   summary a new name, or rename the key column before this call.
#' - [dplyr::cur_group()], [dplyr::cur_group_id()],
#'   [dplyr::cur_group_rows()], and the deprecated `cur_data*()` helpers are
#'   refused by name, even when you have bound one to a function of your own,
#'   because a result combining several grouping sets has no single group or
#'   data mask for them to describe. Use [grouping_bit()] or [grouping_id()]
#'   to identify margin rows.
#'
#' @section Relationship to dplyr summaries:
#' The `...` expressions use [dplyr::summarize()] data-masking semantics.
#' Source missing values and margin rows that display alike are told apart
#' by [grouping_bit()] or [grouping_id()] written as summaries.
#'
#' A few spellings mean something only because marginplyr rewrites them before
#' anything runs: [grouping_bit()], [grouping_id()], [share_of_parent()],
#' [share_of_total()], [dplyr::across()], [dplyr::if_any()], [dplyr::if_all()],
#' [dplyr::pick()], [dplyr::c_across()], and [tidyselect::where()]. They are
#' recognized by name, written bare or qualified with the package that owns
#' them, and never looked up where the call was written, so binding a function
#' of your own to one of those names does not change what this verb does.
#' `mypkg::across()` is an ordinary call. Every other name follows ordinary
#' lookup, [dplyr::n()] included.
#'
#' Those spellings are rewritten only where the summary writes them:
#' `eval(quote(grouping_bit(region)))` is rewritten, while a capture made
#' outside the call, or text parsed at run time as by [base::str2lang()], is
#' not, so a `grouping_bit()` reached that way is refused.
#'
#' [dplyr::across()], [dplyr::pick()], and [dplyr::c_across()] cannot select
#' any column named in the complete Grouping plan. This extends dplyr's
#' grouping-column rule across every branch: a dimension remains excluded even
#' in a grouping set from which it is omitted.
#'
#' On a local data frame, a selection in a later summary sees earlier
#' summaries, as in dplyr: after `total = sum(value)`,
#' `across(total, ~ .x * 2)` selects `total`. A lazy input retains the alias
#' limitations of its backend.
#'
#' An unnamed summary whose value is a data frame is unpacked into the result
#' on a local input, and a named one is packed into one column, as
#' [dplyr::summarize()] does. A lazy input gives whatever shape its backend
#' gives.
#'
#' A misspelled option such as `.margin_lable` reaches this verb through
#' `...`, where it would become a summary column. A dot-prefixed summary name
#' that is an option name, or one insertion, deletion, or substitution from
#' one, is therefore an error naming the option it resembles: `.ids = "set"`
#' and `.y = mean(v)` are refused, although [dplyr::summarise()] would return
#' them as columns. A name further away, such as `.margin_lable` or
#' `.sotr = "last"`, is not caught and becomes a summary column; check option
#' names when a result has an unexpected column.
#'
#' A Grouping specification written in `...` rather than in `.grouping` --
#' `rollup(region)` passed unnamed, or as `grouping = rollup(region)` -- is
#' refused with the `.grouping = rollup(region)` spelling to use, on a local
#' and a lazy input alike. A constructor name is looked up as R looks it up, so
#' a function of your own named `rollup()` still runs as a summary.
#'
#' @section Summaries a lazy backend cannot carry:
#' Two lazy inputs cannot carry back a summary with no columns in it: asked
#' for no summaries and given no fixed key or Margin dimension, the answer
#' is one row holding nothing, whatever the input held. A `data.table` reads
#' its row count from its first column, so a `dtplyr` input collects to zero
#' rows where a local data frame returns one, and a SQL table of no columns
#' cannot be written at all, so a database input raises the error dbplyr
#' renders one with. Each is what [dplyr::summarize()] itself answers for that
#' lazy input; an Arrow input answers as a local one does. Anything that puts
#' a column in the result -- one summary, one fixed key, one Margin
#' dimension, or `.id` -- ends the difference.
#'
#' A summary Arrow's own engine cannot evaluate is refused on an Arrow table
#' or record batch before a row is read; see *[Arrow inputs][margin_verbs]*
#' in [margin_verbs].
#'
#' @section Contextual shares:
#' [share_of_parent()] divides a preceding summary by the same summary on its
#' immediate [rollup()] parent, and [share_of_total()] by the Grand total set,
#' within each fixed `.by` partition. Neither judges whether the ratio means
#' anything: the share of a non-additive summary, such as a mean, is a ratio
#' of two means and returns values near one without a warning. Share a sum or
#' a count. [share_of_parent()] is the reference for both helpers.
#'
#' @section SQLite materialization:
#' A result on a live SQLite connection can carry the leading class
#' `marginplyr_sqlite_typed_result`. It is direct until another dplyr verb
#' is applied to it; the verb keeps the class but not the behavior below.
#'
#' On a live SQLite connection, direct [dplyr::compute()] of a result
#' returned by [summarize_with_margins()] or [expand_with_margins()] writes a
#' typed table holding only the public columns, and later dplyr verbs on it
#' use ordinary dbplyr behavior. A sorted result is read back in Margin order
#' through an implicit rowid alias, so sorted compute refuses before writing
#' when the public columns shadow all three of `rowid`, `oid`, and `_rowid_`;
#' direct collection still works. An unqualified destination name that could
#' address a different schema, such as a bare persistent name shadowed by a
#' temporary table, is refused before writing; give an explicit schema or
#' another name.
#'
#' Compute runs inside its own savepoint, so an existing caller transaction
#' keeps its commit and rollback, whatever `in_transaction` says. If an
#' ordinary error or a catchable R interrupt arrives before the savepoint is
#' released, marginplyr restores an overwritten destination's data, schema,
#' indexes, and affected statistics, or removes a new destination, then
#' releases the savepoint; source tables and earlier caller work are kept.
#' This requires a surviving connection and successful cleanup;
#' [marginplyr_conditions] describes what an interrupt or a failed cleanup
#' reports.
#' Once the savepoint is released, the outermost release persists the result
#' and an inner one leaves it to the caller's commit or rollback.
#'
#' There is no recovery guarantee for native SQLite cancellation, which can
#' abort the whole transaction, nor for invalid connections, process
#' termination, fatal signals, or a timeout that does not unwind through R.
#'
#' @section Behavior shared with the other Margin verbs:
#' These rules are stated once, in [margin_verbs], for every Margin verb:
#'
#' - [Fixed columns and Margin dimensions][margin_verbs]
#' - [Grouped and row-wise inputs][margin_verbs]
#' - [Grouping plan size][margin_verbs]
#' - [Display labels and grouping identity][margin_verbs]
#' - [Margin order][margin_verbs]
#' - [Grouping set identifiers][margin_verbs]
#' - [Option arguments][margin_verbs]
#' - [Result metadata][margin_verbs]
#' - [Arrow inputs][margin_verbs]
#' - [Database backend coverage][margin_verbs]
#' - [When marginplyr queries your data][margin_verbs]
#'
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Build one monthly management report with store detail, region subtotals,
#' # and a company total. `set` numbers the grouping sets: 1 is store detail,
#' # 2 is the region subtotal, and 3 is the company total for each month.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   units = sum(units),
#'   revenue = sum(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store),
#'   .id = "set"
#' )
#'
#' # `.sort` puts each subtotal after the stores it summarizes, and each
#' # year's company total last within that year.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .by = year,
#'   .grouping = rollup(region, store),
#'   .sort = "last"
#' )
#'
#' # Existing dplyr groups are implicit fixed keys. The calculation below is
#' # equivalent to `.by = c(year, month)`, but its result is still ungrouped.
#' grouped_report <- retail_sales |>
#'   dplyr::group_by(year, month) |>
#'   summarize_with_margins(
#'     revenue = sum(revenue),
#'     .grouping = rollup(region, store)
#'   )
#' dplyr::group_vars(grouped_report)
#'
#' # Moving year and month into the rollup extends the hierarchy from store
#' # detail through monthly, per-year, and all-period totals.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   level = grouping_id(year, month, region, store),
#'   .grouping = rollup(year, month, region, store)
#' )
#'
#' # A Margin dimension computed from other columns is created with mutate()
#' # before summarizing.
#' summarize_with_margins(
#'   .data = dplyr::mutate(
#'     retail_sales,
#'     period = paste(year, month, sep = "-")
#'   ),
#'   revenue = sum(revenue),
#'   .grouping = rollup(period, region)
#' )
#'
#' # Change the display label, or use NULL to retain the input column types.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(year),
#'   .margin_label = "All years"
#' )
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store),
#'   .margin_label = c(region = "All regions", store = "All stores")
#' )
#' # With `NULL`, `year` stays an integer and the margin row holds `NA`; a
#' # tibble's type header shows it.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   year_is_total = grouping_bit(year),
#'   .grouping = rollup(year),
#'   .margin_label = NULL
#' ) |>
#'   dplyr::as_tibble()
#'
#' # Ordered factors remain ordered, and a new label is placed by
#' # `.margin_label_position`. An existing level -- used or unused -- is
#' # rejected as a label.
#' priority_data <- data.frame(
#'   priority = ordered(
#'     c("standard", "premium"),
#'     levels = c("standard", "premium", "unused")
#'   ),
#'   value = c(1, 2)
#' )
#' try(summarize_with_margins(
#'   .data = priority_data,
#'   total = sum(value),
#'   .grouping = rollup(priority),
#'   .margin_label = "unused"
#' ))
#' priority_result <- summarize_with_margins(
#'   .data = priority_data,
#'   total = sum(value),
#'   .grouping = rollup(priority),
#'   .margin_label = "All priorities",
#'   .margin_label_position = "first"
#' )
#' is.ordered(priority_result$priority)
#' levels(priority_result$priority)
#'
#' # A typed-missing Margin label prints like a real missing value, and
#' # `grouping_bit()` tells them apart. A named list can label one dimension
#' # and leave another missing.
#' status_data <- data.frame(
#'   region = c("east", "east", "west"),
#'   status = factor(c("active", NA, "active"), levels = c("active", "idle")),
#'   value = c(1, 2, 3)
#' )
#' summarize_with_margins(
#'   .data = status_data,
#'   total = sum(value),
#'   status_is_total = grouping_bit(status),
#'   .grouping = rollup(region, status),
#'   .margin_label = list(region = "All regions", status = NULL)
#' )
#'
#' # A cube() crosses every combination of its dimensions, so the result has
#' # a total per region, a total per year, and a company total.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = cube(region, year),
#'   .sort = "last"
#' )
#'
#' # grouping_sets() lists the grouping sets to compute. Region appears twice;
#' # `.duplicates = "keep"` returns both copies, and `.id` tells them apart.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = grouping_sets(region, year, region),
#'   .duplicates = "keep",
#'   .id = "set"
#' )
#'
#' # across() and pick() treat every fixed key and Margin dimension as a
#' # grouping column, including dimensions omitted from a subtotal branch, so
#' # pick(everything()) names neither year nor region.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   dplyr::across(
#'     c(units, revenue),
#'     sum,
#'     .names = "total_{.col}"
#'   ),
#'   picked = paste(
#'     names(dplyr::pick(everything())),
#'     collapse = ", "
#'   ),
#'   .by = year,
#'   .grouping = rollup(region),
#'   .sort = "last"
#' )
#'
#' # DuckDB executes a native GROUP BY GROUPING SETS query.
#' # Runs only when a recent enough DuckDB is installed.
#' source(system.file("suggests", "guard.R", package = "marginplyr"))
#' if (
#'   marginplyr_suggest_available("DBI") &&
#'   marginplyr_suggest_available("duckdb")
#' ) {
#'   # `shared_home = FALSE` keeps DuckDB's extension cache and stored secrets
#'   # inside the session's temporary directory instead of `~/.duckdb`.
#'   con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
#'
#'   sales_db <- dplyr::copy_to(
#'     con,
#'     retail_sales,
#'     name = "retail_sales",
#'     temporary = TRUE,
#'     overwrite = TRUE
#'   )
#'   query <- summarize_with_margins(
#'     .data = sales_db,
#'     revenue = sum(revenue, na.rm = TRUE),
#'     level = grouping_id(region, store),
#'     .by = c(year, month),
#'     .grouping = rollup(region, store),
#'     .sort = "last"
#'   )
#'
#'   dplyr::show_query(query)
#'   result <- dplyr::collect(query)
#'   DBI::dbDisconnect(con)
#'   result
#' }
summarize_with_margins <- function(.data,
                                   ...,
                                   .by = NULL,
                                   .grouping = NULL,
                                   .margin_label = "Total",
                                   .margin_label_position = c("last", "first"),
                                   .check_margin_label = is.data.frame(.data),
                                   .check_share_source = TRUE,
                                   .duplicates = c("error", "drop", "keep"),
                                   .id = NULL,
                                   .sort = c("none", "last", "first")) {
  force(.data)
  suspendInterrupts({
    # The scope is read from this live frame by active_sent_query_scope().
    sent_query_scope <- reset_sent_queries() # nolint: object_usage_linter.
  })
  call <- rlang::current_call()
  dots <- rlang::enquos(...)
  grouping_quo <- capture_public_grouping(
    rlang::enquo0(.grouping), substitute(.grouping)
  )
  by_quo <- rlang::enquo(.by)

  share_kinds <- with_margin_error_call(
    {
      assert_margin_input(.data)
      assert_reusable_margin_input(.data)
      normalize_margin_options(
        .margin_label = .margin_label,
        .margin_label_position = .margin_label_position,
        .check_margin_label = .check_margin_label,
        .duplicates = .duplicates,
        .sort = .sort,
        duplicates_choices = margin_duplicates_choices,
        .id = .id
      )
      assert_logical_scalar(.check_share_source)
      check_option_named_summaries(dots)
      # Ahead of the two readings of a summary expression below, and after the
      # one check that reads only names: an empty argument is refused rather
      # than bound to a local.
      check_empty_summaries(dots)
      check_grouping_spec_summaries(dots, .data)
      check_summary_context_helpers(dots)
      preflight_shares(dots)
    },
    call = call
  )

  operation <- prepare_margin_operation(
    .data,
    by_quo = by_quo,
    grouping_quo = grouping_quo,
    .margin_label = .margin_label,
    .margin_label_position = .margin_label_position,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .sort = .sort,
    duplicates_choices = margin_duplicates_choices,
    # A summary result holds the grouping columns and the caller's outputs, so
    # the fixed `.by` keys are the whole of what it carries from the input. A
    # summary output is free to reuse another column's name, which is why this
    # is not the input's column names narrowed to what the result happens to
    # hold: that name would be rebuilt from the input column's levels.
    carried_columns = function(data_vars, plan) plan$by,
    # A summary's rows do not depend on an unlimited input ordering.
    keeps_input_order = FALSE,
    .id = .id,
    validate_grouping = share_grouping_spec_validator(share_kinds),
    validate_plan_names = function(fixed, dimensions, data_vars, complete) {
      if (
        complete && length(dots) > 0L &&
          !contains_share_helper(rlang::quo_get_expr(dots[[1L]]))
      ) {
        preflight_static_selection(
          dots[[1L]],
          grouping_name_proxy(setdiff(data_vars, c(fixed$columns, dimensions)))
        )
      }
    },
    call = call
  )
  execution <- execute_margin_summary(
    operation,
    dots,
    check_share_source = .check_share_source
  )
  typed_dimensions <- names(operation$margin_labels)[vapply(
    operation$margin_labels,
    is_missing_margin_label,
    logical(1)
  )]
  # Share staging, SQLite Margin order, and the public `.id` projection can
  # wrap the union's source-column anchor. The finalizer anchors the result.
  has_anchor <- length(share_kinds) > 0L ||
    sqlite_final_anchor_path(operation)
  existing_anchor_needed <- (
    identical(operation$backend$kind, "sql") &&
      has_anchor &&
      length(typed_dimensions) > 0L &&
      length(operation$plan$sets) > 1L
  ) || (
    sqlite_declared_type_result(operation) &&
      (length(share_kinds) > 0L || !is.null(operation$set_id_name))
  )
  # A source key and a summary output both reach generated `.env` references.
  sql_env_column <- operation$backend$is_sql &&
    ".env" %in% get_col_names(execution$result, dplyr::everything())
  sqlite_env_column <- sql_env_column && live_sqlite_margin_result(operation)
  sqlite_boolean_column <- live_sqlite_margin_result(operation) &&
    length(sqlite_boolean_columns(dplyr::tbl_vars(execution$result))) > 0L
  anchor_needed <- existing_anchor_needed || (
    sqlite_declared_type_result(operation) &&
      length(execution$declared_types) > 0L
  ) || sqlite_env_column || sqlite_boolean_column
  # The typed result's companion anchors declare these columns where the query
  # would lose them; the query's own types are already right. `CREATE TABLE AS`
  # over a compound SELECT drops a typed-missing dimension's declaration, and
  # the ordering wrapper and a text label's typed destination hide a fixed
  # key's.
  compound_typed_dimension <- length(operation$plan$sets) > 1L &&
    length(typed_dimensions) > 0L
  hidden_fixed_key <- length(operation$plan$by) > 0L &&
    (margin_sorting(operation) ||
       length(typed_dimensions) < length(operation$margin_labels))
  companion_anchor_needed <- !anchor_needed &&
    sqlite_declared_type_result(operation) &&
    (compound_typed_dimension || hidden_fixed_key)
  final_type_anchor <- if (anchor_needed || companion_anchor_needed) {
    if (live_sqlite_margin_result(operation)) {
      always_present <- Reduce(intersect, operation$plan$sets)
      unique(c(operation$plan$by, typed_dimensions, always_present))
    } else {
      c(operation$plan$by, typed_dimensions)
    }
  } else {
    character()
  }
  finalize_margin_operation(
    operation, execution, type_anchor_columns = final_type_anchor,
    anchor_query = !companion_anchor_needed,
    sql_env_column = sql_env_column
  )
}

execute_margin_summary <- function(operation, dots, check_share_source) {
  check_margin_operation(operation)
  with_margin_error_call(
    {
      plan <- operation$plan
      group_vars <- c(plan$by, plan$dimensions)
      summary_plan <- plan_summary_expressions(
        dots,
        data_proxy = operation$data_proxy,
        data_vars = operation$data_vars,
        plan = plan,
        backend_kind = operation$backend$kind,
        set_id_name = operation$set_id_name,
        call = operation$call,
        local_data = if (identical(operation$backend$kind, "local")) {
          operation$data
        } else {
          NULL
        }
      )
      summaries <- summary_plan$summaries
      dots <- summaries$dots
      selection_proxy <- summary_selection_proxy(
        operation$data_proxy,
        data_vars = operation$data_vars,
        group_vars = group_vars
      )
      public_dot_names <- names(dots)
      if (identical(operation$backend$kind, "local")) {
        # A local frame discards its Assigned summary name (ADR 0028).
        # The branch checks the names its value actually retains.
        public_dot_names[!is.na(summaries$assigned_names)] <- ""
      }
      summary_output_names <- unique(c(
        public_dot_names[nzchar(public_dot_names)],
        summary_plan$predictable_names,
        known_summary_output_names(
          dots, selection_proxy,
          defer_local = identical(operation$backend$kind, "local")
        )
      ))
      check_margin_sql_public_names(
        unique(c(group_vars, summary_output_names,
                 operation$set_id_name)),
        operation$backend
      )
      check_summary_group_overwrite(
        summary_output_names,
        group_vars = group_vars
      )
      check_margin_id_collision(
        operation$set_id_name,
        summary_output_names,
        "a summary output"
      )
      reserved_names <- unique(c(
        names(dots),
        operation$data_vars,
        summary_output_names,
        summary_frame_candidates(dots),
        operation$set_id_name
      ))
      has_shares <- length(summary_plan$requests) > 0L
      share_names <- unlist(
        lapply(summary_plan$requests, `[[`, "outputs"), use.names = FALSE
      )
      local_share <- has_shares && identical(
        operation$backend$kind, "local"
      )
      if (local_share) {
        share_positions <- match(share_names, names(summaries$dots))
        stopifnot(!anyNA(share_positions))
        share_aliases <- stats::setNames(new_margin_internal_names(
          length(share_names), used_names = reserved_names,
          prefix = "..marginplyr_share_"
        ), share_names)
        share_tokens <- stats::setNames(lapply(
          share_names, function(name) new.env(parent = emptyenv())
        ), unname(share_aliases))
        for (i in seq_along(share_positions)) {
          position <- share_positions[[i]]
          alias <- unname(share_aliases[[i]])
          existing <- rlang::expr(dplyr::pick(dplyr::any_of(!!alias)))
          summaries$dots[[position]] <- rlang::new_quosure(
            rlang::call2(
              marginplyr_private_call("local_share_marker"),
              alias, share_tokens[[i]], existing
            ),
            env = environment()
          )
          names(summaries$dots)[position] <- alias
        }
        reserved_names <- c(reserved_names, unname(share_aliases))
        summaries$selection_state$share_aliases <- unname(share_aliases)
        summaries$selection_state$share_markers <- share_tokens
      }
      declare_sqlite_types <- sqlite_declared_type_result(operation)
      summaries$grouping_type_state <- if (declare_sqlite_types) {
        new.env(parent = emptyenv())
      } else {
        NULL
      }
      declared_id <- if (!declare_sqlite_types ||
                           is.null(operation$set_id_name)) {
        character()
      } else {
        stats::setNames("integer", operation$set_id_name)
      }

      # A Grouping helper's refusals are decided by the call and the plan, so
      # ADR 0005 places them ahead of the label query below; staging rewrites
      # the same dots again, per branch.
      check_grouping_helper_calls(dots, plan)
      # The Arrow share refusal needs only the planned requests, so it too
      # precedes the label query (ADR 0005).
      if (
        has_shares &&
          identical(operation$backend$kind, "arrow")
      ) {
        abort_arrow_shares(share_request_kinds(summary_plan$requests))
      }
      validate_margin_operation(
        operation,
        on_observed_collision = abort_summary_label_collision
      )

      staged_result <- stage_margin_summaries(
        operation,
        summaries = summaries,
        reserved_names = reserved_names,
        keep_set_identity = has_shares,
        keep_typed_keys = "parent" %in%
          share_request_kinds(summary_plan$requests) ||
          (has_shares && identical(operation$backend$kind, "dtplyr"))
      )

      if (declare_sqlite_types) {
        outputs <- summaries$grouping_type_state$names
        declared_id <- c(declared_id, stats::setNames(
          rep("integer", length(outputs)), outputs
        ))
      }

      if (local_share) {
        staged_result <- restore_local_share_names(
          staged_result, share_aliases, share_tokens,
          summary_plan$requests
        )
      }

      if (has_shares) {
        return(new_margin_execution(
          execute_shares(
            operation,
            staged_result = staged_result,
            requests = summary_plan$requests,
            check_share_source = check_share_source
          ),
          sort_id = margin_summary_stage_sort_id(staged_result),
          # sf moves active summary geometry behind scalar outputs. The
          # per-dot runtime names retain their order, including unnamed frames.
          output_order = if (local_share &&
                               inherits(staged_result$result, "sf")) {
            intersect(
              unlist(summaries$selection_state$summary_aliases,
                     use.names = FALSE),
              names(staged_result$result)
            )
          } else {
            character()
          },
          declared_types = if (declare_sqlite_types) {
            c(declared_id, stats::setNames(
              rep("double", length(share_names)), share_names
            ))
          } else {
            character()
          }
        ))
      }
      new_margin_execution(
        margin_summary_stage_result(staged_result),
        sort_id = margin_summary_stage_sort_id(staged_result),
        declared_types = declared_id
      )
    },
    call = operation$call
  )
}

# Returns one token in a list column at the share's original dot position.
# The caller gives an alias absent from input and known output names. A frame
# that created the alias earlier is captured by the injected `pick()` before
# this call runs; expanding that selection avoids reading unrelated outputs.
local_share_marker <- function(alias, token, existing) {
  if (alias %in% names(existing)) {
    abort_share_alias_collision(alias)
  }
  structure(list(token), marginplyr_share_token = token)
}

# Rejects an ordinary output using a reserved local share alias. The caller
# supplies the alias that was present in the summary mask or staged result.
abort_share_alias_collision <- function(alias) {
  abort_marginplyr(paste0(
    "Ordinary summary output {.var {alias}} conflicts with an internal ",
    "share column."
  ))
}

# Checks each local branch before binding can discard zero-row list attributes.
# The caller passes a dplyr branch and its per-alias tokens; nonempty columns
# carry token cells, while an empty marker carries its token as an attribute.
check_local_share_markers <- function(result, markers) {
  for (alias in names(markers)) {
    value <- result[[alias]]
    valid <- if (length(value) > 0L) {
      is.list(value) && all(vapply(
        value, identical, logical(1), markers[[alias]]
      ))
    } else {
      identical(attr(value, "marginplyr_share_token"), markers[[alias]])
    }
    if (!valid) {
      abort_share_alias_collision(alias)
    }
  }
  invisible(NULL)
}

# Restores public share names after all local branches have been combined.
# The caller passes aliases and tokens whose markers passed each branch check;
# nonempty result cells are checked again before the placeholders are reset.
restore_local_share_names <- function(staged_result, aliases,
                                      tokens, requests) {
  result <- margin_summary_stage_result(staged_result)
  conflicts <- intersect(names(result), names(aliases))
  if (length(conflicts) > 0L) {
    name <- conflicts[[1L]]
    pair <- Filter(function(pair) identical(pair$output, name),
                   share_pairs(requests))[[1L]]
    # The cli template reads this binding; codetools cannot follow it.
    kind <- pair$kind # nolint: object_usage_linter.
    abort_marginplyr(paste0(
      "{share_kind_modifier(kind)} output name {.var {name}} conflicts ",
      "with an ordinary summary."
    ))
  }
  for (alias in unname(aliases)) {
    value <- result[[alias]]
    if (!is.list(value) || !all(vapply(
      value, identical, logical(1), tokens[[alias]]
    ))) {
      abort_share_alias_collision(alias)
    }
    result[[alias]] <- rep(NA_real_, nrow(result))
  }
  result_names <- names(result)
  result_names[match(unname(aliases), result_names)] <- names(aliases)
  staged_result$result <- rlang::set_names(result, result_names)
  staged_result
}

# `summaries` is what `new_summary_arguments()` built: the caller's summary
# dots beside the caller's label for each, one value because the pair may not
# drift apart between the planning that built it and the adapter that reads it.
stage_margin_summaries <- function(operation,
                                   summaries,
                                   reserved_names,
                                   keep_set_identity,
                                   keep_typed_keys = FALSE) {
  plan <- operation$plan
  set_id_name <- operation$set_id_name
  if (keep_set_identity) {
    set_id_name <- new_margin_internal_names(
      1L,
      used_names = reserved_names,
      prefix = "..marginplyr_set_id_",
      backend = operation$backend
    )
    reserved_names <- c(reserved_names, set_id_name)
  }

  # Which adapter runs stays a function of the duplicate policy, of an
  # identifier that has to number occurrences, and of a Grouping helper that
  # has to be a literal (ADR 0009). A Margin order needs Grouping bits rather
  # than occurrences, so the identifier it may add below is allocated after
  # this decision and never changes it.
  use_native <- supports_grouping_sets(
    operation$data,
    plan,
    backend = operation$backend
  ) && !(
    !is.null(set_id_name) &&
      identical(plan$duplicates, "keep")
  ) && !grouping_helper_needs_literal(
    summaries$dots,
    dbplyr::remote_con(operation$data)
  )

  sort_id <- margin_sort_identifier(
    operation,
    set_id_name = set_id_name,
    used_names = reserved_names
  )
  if (!is.null(sort_id)) {
    set_id_name <- sort_id
    reserved_names <- unique(c(reserved_names, set_id_name))
  }

  # Parent matching and dtplyr cardinality validation read these before
  # display conversion can merge distinct typed values. The share executor
  # drops them before finalization.
  parent_key_names <- character()
  if (keep_typed_keys) {
    parent_key_names <- new_margin_internal_names(
      length(plan$dimensions),
      used_names = reserved_names,
      prefix = "..marginplyr_parent_original_",
      backend = operation$backend
    )
    names(parent_key_names) <- plan$dimensions
    reserved_names <- c(reserved_names, unname(parent_key_names))
  }

  # Both branches above may replace the caller's `.id` with a name allocated
  # here -- for keeping set identity under a share, or for a Margin order. The
  # adapters check their result names against whichever they were handed, and
  # only this frame can still tell the two apart. Getting it wrong is not
  # cosmetic: `check_margin_id_collision()` names `.id` in its message, which a
  # caller who wrote no `.id` cannot act on.
  set_id_is_internal <- !identical(set_id_name, operation$set_id_name)

  result <- tryCatch(
    {
      if (use_native) {
        # What this adapter restates is the one error dbplyr raises
        # translating the caller's expressions, and that error alone
        # (ADR 0022).
        summarize_margin_native(
          operation$data,
          summaries = summaries,
          plan = plan,
          margin_labels = operation$margin_labels,
          reserved_names = reserved_names,
          call = operation$call,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal,
          parent_key_names = parent_key_names
        )
      } else {
        input_window_order <- if (operation$backend$records_window_order) {
          dbplyr::op_sort(operation$data)
        } else {
          list()
        }
        summarize_margin_union(
          operation$data,
          summaries = summaries,
          plan = plan,
          margin_labels = operation$margin_labels,
          column_info = operation$column_info,
          backend = operation$backend,
          reserved_names = reserved_names,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal,
          call = operation$call,
          input_window_order = input_window_order,
          parent_key_names = parent_key_names
        )
      }
    },
    error = function(cnd) {
      parent <- cnd$parent
      # dplyr wraps a Package condition raised while it evaluates an unnamed
      # frame; return that condition with its class and diagnostic intact.
      if (inherits(parent, "marginplyr_error")) {
        stop(parent)
      }
      stop(cnd)
    }
  )
  # The adapters check against the identifier they were handed, which is the
  # allocated one wherever the two branches above replaced the caller's, so
  # this is the frame that can still ask the caller's own (ADR 0028).
  if (set_id_is_internal) {
    check_margin_id_collision(
      operation$set_id_name,
      margin_col_names(result),
      "a summary output"
    )
  }
  new_margin_summary_stage(
    result, set_id_name, sort_id = sort_id,
    parent_key_names = parent_key_names
  )
}

new_margin_summary_stage <- function(result, set_id_name, sort_id = NULL,
                                     parent_key_names = character()) {
  structure(
    list(
      result = result,
      set_id_name = set_id_name,
      sort_id = sort_id,
      parent_key_names = parent_key_names
    ),
    class = "marginplyr_summary_stage"
  )
}

check_margin_summary_stage <- function(staged_result) {
  stopifnot(inherits(staged_result, "marginplyr_summary_stage"))
  invisible(staged_result)
}

margin_summary_stage_result <- function(staged_result) {
  check_margin_summary_stage(staged_result)
  staged_result$result
}

margin_summary_stage_set_id <- function(staged_result) {
  check_margin_summary_stage(staged_result)
  staged_result$set_id_name
}

margin_summary_stage_sort_id <- function(staged_result) {
  check_margin_summary_stage(staged_result)
  staged_result$sort_id
}

#' @rdname summarize_with_margins
#' @export
summarise_with_margins <- summarize_with_margins
