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
#' @inheritParams summarize_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Grouping set occurrence identifiers
#' @inheritSection summarize_with_margins Database backend coverage
#' @inheritSection summarize_with_margins Backend extension design
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy.
#'   Result row order is unspecified; use [dplyr::arrange()] when presentation
#'   order matters.
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
#'   january_sales,
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
#' # lazy table makes the portable UNION ALL translation visible.
#' sqlite_sales <- dbplyr::tbl_lazy(
#'   january_sales,
#'   con = dbplyr::simulate_sqlite()
#' )
#' sqlite_sales |>
#'   expand_with_margins(
#'     .grouping = rollup(region, store)
#'   ) |>
#'   dplyr::show_query()
expand_with_margins <- function(.data,
                                .by = NULL,
                                .grouping = NULL,
                                .margin_label = "Total",
                                .margin_label_position = c("last", "first"),
                                .check_margin_label = is.data.frame(.data),
                                .duplicates = c("error", "drop", "keep"),
                                .id = NULL) {
  call <- rlang::current_call()
  with_margin_error_call(assert_lazy_table(.data), call = call)
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
    .id = .id,
    call = call
  )
  result <- execute_margin_expand(operation)
  finalize_margin_operation(operation, result)
}

execute_margin_expand <- function(operation) {
  check_margin_operation(operation)
  validate_margin_operation(operation)
  expand_margin_union(
    operation$data,
    plan = operation$plan,
    margin_labels = operation$margin_labels,
    column_info = operation$column_info,
    set_id_name = operation$set_id_name
  )
}
