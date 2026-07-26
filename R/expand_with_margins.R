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
#' @inheritSection summarize_with_margins Database backend coverage
#' @inheritSection summarize_with_margins Backend extension design
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy.
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
                                .check_margin_label = is.data.frame(.data),
                                .duplicates = c("error", "drop", "keep"),
                                .sort = is.data.frame(.data)) {
  assert_lazy_table(.data)
  assert_logical_scalar(.check_margin_label)
  assert_logical_scalar(.sort)
  .margin_label <- normalize_margin_label(.margin_label)
  .duplicates <- match.arg(.duplicates)

  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)
  grouping_spec <- rlang::eval_tidy(grouping_quo)
  input <- prepare_margin_input(.data, by_quo)
  .data <- input$data
  by <- input$by
  backend <- grouping_backend(.data)
  data_vars <- get_col_names(.data, dplyr::everything())
  data_proxy <- grouping_selection_proxy(.data, backend = backend)
  plan <- compile_grouping_spec(
    grouping_spec,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = by,
    .duplicates = .duplicates
  )

  column_info <- margin_column_info(
    .data,
    plan$dimensions,
    backend = backend
  )
  validate_margin_label(
    .data,
    dimensions = plan$dimensions,
    .margin_label = .margin_label,
    .check_margin_label = .check_margin_label,
    column_info = column_info,
    backend = backend
  )

  result <- expand_margin_union(
    .data,
    plan = plan,
    .margin_label = .margin_label,
    column_info = column_info
  )

  finish_margin_result(
    result,
    plan = plan,
    factor_info = column_info$factors,
    .margin_label = .margin_label,
    .sort = .sort
  )
}
