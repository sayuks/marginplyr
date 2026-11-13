#' Expand rows with SQL-style grouping margins
#'
#' [expand_with_margins()] returns one copy of each input row for every
#' grouping set, with the dimensions that set omits replaced by
#' `.margin_label`. It is what you would get by applying [dplyr::mutate()] to
#' set each omitted dimension to the label, once per grouping set, and
#' combining the copies with [dplyr::union_all()]. Use it when a later step
#' needs the rows behind each margin rather than one summary row per group,
#' such as a window function over every copy.
#'
#' @section Differences from mutate() and union_all():
#' - The result is always ungrouped, whatever groups the input had; see
#'   *Grouped and row-wise inputs*.
#' - Row order is unspecified. Use `.sort` for margins next to the rows they
#'   cover, or [dplyr::arrange()] for any other order.
#' - A character `.margin_label` changes a non-factor grouping dimension to
#'   character in every copy, detail rows included; see Value. Use
#'   `.margin_label = NULL` or `NA_character_` to keep its type.
#' - The result is one row per input row per grouping set, so it grows with
#'   the number of grouping sets.
#'
#' @section Calculations one summary pass cannot express:
#' A summary that needs both a group-level value and the rows behind it works
#' directly in [summarize_with_margins()] on a local data frame, because dplyr
#' hands the expression the whole group; against a database the same
#' expression has to compile to SQL aggregates, and a nested aggregate is
#' rejected. Expanding first gives ordinary window functions over the copies
#' and stays lazy, at the cost of one copy of the input per grouping set. The
#' [recipes guide][recipes] works the case through.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @inheritParams summarize_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Option arguments
#' @inheritSection summarize_with_margins Result metadata
#' @inheritSection summarize_with_margins Grouping set identifiers
#' @inheritSection summarize_with_margins Margin order
#' @inheritSection summarize_with_margins Display labels and grouping identity
#' @inheritSection summarize_with_margins Arrow inputs
#' @inheritSection summarize_with_margins Database backend coverage
#' @inheritSection summarize_with_margins When marginplyr queries your data
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy,
#'   with one row per input row per grouping set. Its columns are the fixed
#'   `.by` keys, then the grouping dimensions, then `.id` when supplied, then
#'   the remaining input columns in input order. Its class follows
#'   [dplyr::mutate()] combined with [dplyr::union_all()]; see *Result
#'   metadata*.
#'
#'   With a character `.margin_label`, the default, a grouping dimension that
#'   is not a factor becomes character so that it can hold the label. A factor
#'   dimension instead gains the label as a new level. `.margin_label = NULL`
#'   or `NA_character_` keeps every dimension's type and marks margin copies
#'   with a missing value.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Expand a single month's source rows into store, region, and company
#' # branches. Summarizing the expanded rows later reproduces the margins.
#' january_sales <- dplyr::filter(
#'   retail_sales,
#'   year == 2026L,
#'   month == "Jan"
#' )
#' expand_with_margins(
#'   .data = january_sales,
#'   .grouping = rollup(region, store)
#' )
#'
#' # Persistent groups become fixed keys for this operation, then the expanded
#' # result is returned ungrouped.
#' grouped_expansion <- january_sales |>
#'   dplyr::group_by(channel) |>
#'   expand_with_margins(
#'     .grouping = rollup(region, store)
#'   )
#' dplyr::group_vars(grouped_expansion)
#'
#' # A simulated SQLite table shows the portable UNION ALL translation: one
#' # branch per grouping set, after a `WHERE (0)` branch that only fixes the
#' # column types. Runs only when RSQLite is installed, which renders SQLite
#' # SQL.
#' source(system.file("suggests", "guard.R", package = "marginplyr"))
#' if (marginplyr_suggest_available("RSQLite")) {
#'   sqlite_sales <- dbplyr::tbl_lazy(
#'     dplyr::select(january_sales, region, revenue),
#'     con = dbplyr::simulate_sqlite()
#'   )
#'   sqlite_sales |>
#'     expand_with_margins(.grouping = rollup(region)) |>
#'     dplyr::show_query()
#' }
expand_with_margins <- function(.data,
                                .by = NULL,
                                .grouping = NULL,
                                .margin_label = "Total",
                                .margin_label_position = c("last", "first"),
                                .check_margin_label = is.data.frame(.data),
                                .duplicates = c("error", "drop", "keep"),
                                .id = NULL,
                                .sort = c("none", "last", "first")) {
  force(.data)
  suspendInterrupts({
    # The scope is read from this live frame by active_sent_query_scope().
    sent_query_scope <- reset_sent_queries() # nolint: object_usage_linter.
  })
  call <- rlang::current_call()
  with_margin_error_call(
    {
      assert_margin_input(.data)
      assert_reusable_margin_input(.data)
    },
    call = call
  )
  grouping_quo <- capture_public_grouping(
    rlang::enquo0(.grouping), substitute(.grouping)
  )
  by_quo <- rlang::enquo(.by)

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
    # An expansion branch is the input's own rows, so every column of the input
    # reaches the result and none of them is summarised away.
    carried_columns = function(data_vars, plan) data_vars,
    .id = .id,
    call = call
  )
  execution <- execute_margin_expand(operation)
  type_anchor_columns <- if (live_sqlite_margin_result(operation)) {
    operation$data_vars
  } else {
    character()
  }
  finalize_margin_operation(
    operation, execution, type_anchor_columns = type_anchor_columns,
    sql_env_column = operation$backend$is_sql &&
      ".env" %in% c(operation$data_vars, operation$set_id_name)
  )
}

execute_margin_expand <- function(operation) {
  check_margin_operation(operation)
  validate_margin_operation(operation)
  # Expansion always uses the portable adapter, so a Margin order always reads
  # its Grouping bits from a per-branch identifier literal.
  sort_id <- margin_sort_identifier(
    operation,
    set_id_name = operation$set_id_name,
    used_names = operation$data_vars
  )
  set_id_name <- if (is.null(sort_id)) operation$set_id_name else sort_id

  result <- with_margin_error_call(
    expand_margin_union(
      prepare_packed_union_input(operation),
      plan = operation$plan,
      margin_labels = operation$margin_labels,
      column_info = operation$column_info,
      backend = operation$backend,
      set_id_name = set_id_name
    ),
    call = operation$call
  )
  new_margin_execution(
    result,
    sort_id = sort_id,
    declared_types = if (sqlite_declared_type_result(operation) &&
                           !is.null(operation$set_id_name)) {
      stats::setNames("integer", operation$set_id_name)
    } else {
      character()
    }
  )
}
