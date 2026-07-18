#' Expand rows with SQL-style grouping margins
#'
#' `union_all_with_margins()` emits one copy of each input row for every
#' grouping set, replacing omitted grouping dimensions with `.margin_label`.
#' This is analogous to vertically combining the grouping branches with SQL
#' `UNION ALL`.
#'
#' @inheritParams summarize_with_margins
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' union_all_with_margins(
#'   mtcars,
#'   .by = am,
#'   .grouping = rollup(cyl, vs)
#' )
union_all_with_margins <- function(.data,
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
  grouping_spec <- rlang::eval_tidy(grouping_quo)
  .data <- dplyr::ungroup(.data)
  by <- get_col_names(.data, {{ .by }})
  data_vars <- get_col_names(.data, dplyr::everything())
  data_proxy <- grouping_selection_proxy(.data)
  plan <- compile_grouping_spec(
    grouping_spec,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = by,
    .duplicates = .duplicates
  )

  column_info <- margin_column_info(.data, plan$dimensions)
  validate_margin_label(
    .data,
    dimensions = plan$dimensions,
    .margin_label = .margin_label,
    .check_margin_label = .check_margin_label,
    column_info = column_info
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
