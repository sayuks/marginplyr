#' Nest by SQL-style grouping margins
#'
#' This is the row-wise counterpart of [nest_with_margins()].
#'
#' @inheritParams nest_with_margins
#' @details A `dtplyr` result is collected before it is made row-wise because
#'   row-wise data frames are local objects.
#' @return A row-wise data frame grouped by the visible grouping columns.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_by_with_margins(
#'   mtcars,
#'   .grouping = rollup(cyl, vs)
#' )
nest_by_with_margins <- function(.data,
                                 .by = NULL,
                                 .grouping = NULL,
                                 .margin_label = "Total",
                                 .check_margin_label = TRUE,
                                 .duplicates = c("error", "drop", "keep"),
                                 .sort = TRUE,
                                 .key = "data",
                                 .keep = FALSE) {
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)

  result <- rlang::inject(
    nest_with_margins(
      .data = .data,
      .by = !!by_quo,
      .grouping = !!grouping_quo,
      .margin_label = .margin_label,
      .check_margin_label = .check_margin_label,
      .duplicates = .duplicates,
      .sort = .sort,
      .key = .key,
      .keep = .keep
    )
  )

  if (inherits(result, "dtplyr_step")) {
    result <- dplyr::collect(result)
  }
  group_cols <- setdiff(colnames(result), .key)
  dplyr::rowwise(result, dplyr::all_of(group_cols))
}
