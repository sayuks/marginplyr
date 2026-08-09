#' Expand rows with SQL-style grouping margins
#'
#' [expand_with_margins()] emits one copy of each input row for every grouping
#' set, replacing omitted grouping dimensions with `.margin_label`. The
#' operation is named for its user-visible row expansion; portable backends
#' implement it by vertically combining branches with SQL `UNION ALL`.
#'
#' @section Relationship to dplyr:
#' This function is not a variant of [dplyr::union_all()]. That function
#' combines two data frames supplied by the caller; [expand_with_margins()]
#' consumes one data frame and a grouping plan, then emits the corresponding
#' row copies. Naming the public operation for row expansion keeps the SQL
#' implementation strategy out of the user-facing API.
#'
#' @section Calculations one summary pass cannot express:
#' Expanded rows are also the route to a calculation the aggregation pass
#' cannot express. A summary that needs both a group-level value and the rows
#' behind it works directly in [summarize_with_margins()] on a local data
#' frame, because dplyr hands the expression the whole group; against a
#' database the same expression has to compile to SQL aggregates, and a nested
#' aggregate is rejected. Expanding first gives ordinary window functions over
#' the copies and stays lazy, at the cost of one copy of the input per
#' grouping set. The [recipes guide][recipes] works the case through.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @inheritParams summarize_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Result class and attributes
#' @inheritSection summarize_with_margins Grouping set identifiers
#' @inheritSection summarize_with_margins Margin order
#' @inheritSection summarize_with_margins Display labels and grouping identity
#' @inheritSection summarize_with_margins Database backend coverage
#' @inheritSection summarize_with_margins Backend extension design
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy. Its
#'   class and attributes follow [dplyr::mutate()] combined with
#'   [dplyr::union_all()]; see *Result class and attributes*.
#'   Result row order is unspecified unless `.sort` asks for a Margin order;
#'   see *Margin order*, or use [dplyr::arrange()] for any other presentation
#'   order.
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
#' # SQLite has no native GROUPING SETS support in marginplyr, so a simulated
#' # lazy table makes the portable UNION ALL translation visible. Rendering
#' # SQLite SQL reads the driver version from the optional RSQLite package,
#' # even though the simulator opens no connection.
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   sqlite_sales <- dbplyr::tbl_lazy(
#'     january_sales,
#'     con = dbplyr::simulate_sqlite()
#'   )
#'   sqlite_sales |>
#'     expand_with_margins(
#'       .grouping = rollup(region, store)
#'     ) |>
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
  call <- rlang::current_call()
  with_margin_error_call(
    {
      assert_margin_input(.data)
      assert_lazy_table(.data)
    },
    call = call
  )
  grouping_quo <- rlang::enquo(.grouping)
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
    .id = .id,
    call = call
  )
  execution <- execute_margin_expand(operation)
  finalize_margin_operation(operation, execution)
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

  new_margin_execution(
    expand_margin_union(
      operation$data,
      plan = operation$plan,
      margin_labels = operation$margin_labels,
      column_info = operation$column_info,
      set_id_name = set_id_name
    ),
    sort_id = sort_id
  )
}
