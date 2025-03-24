# nolint start: line_length_linter
#' Works like `dplyr::nest_by()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [dplyr::nest_by()].
#' See [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' for more details.
#'
#' @inherit nest_with_margins
#' @details
#' * Works like `dplyr::nest_by(<data>, dplyr::pick({{ .without_all }} , {{ .margins }} , {{ .with_all }})`
#'   with margins.
#' * Only works for a local data frame.
#' @return A row-wise data frame grouped by `without_all`, `.margins`
#' and `.with_all`.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_by_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
# nolint end
nest_by_with_margins <- function(.data,
                                 .margins = NULL,
                                 .without_all = NULL,
                                 .with_all = NULL,
                                 .margin_name = "(all)",
                                 .check_margin_name = TRUE,
                                 .sort = TRUE,
                                 .key = "data",
                                 .keep = FALSE) {
  assert_data_frame(.data)
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  assert_logical_scalar(.keep)
  assert_string_scalar(.margin_name)
  assert_string_scalar(.key)

  .data <- nest_with_margins(
    .data = .data,
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    .check_margin_name = .check_margin_name,
    .sort = .sort,
    .key = .key,
    .keep = .keep
  )

  dplyr::rowwise(
    data = .data,
    c({{ .without_all }}, {{ .margins }}, {{ .with_all }})
  )
}
