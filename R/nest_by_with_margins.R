# nolint start: line_length_linter
#' Works like `dplyr::nest_by()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [dplyr::nest_by()].
#' See [Get started](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
#' for more details.
#'
#' @inherit nest_with_margins
#' @details
#' * Works like `dplyr::nest_by(<data>, dplyr::pick({{ .by }} , {{ .rollup }} , {{ .cube }})`
#'   with margins.
#' * Only works for a local data frame.
#' @return A row-wise data frame grouped by `without_all`, `.rollup`
#' and `.cube`.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_by_with_margins(
#'   mtcars,
#'   .rollup = c(cyl, vs),
#'   .by = am,
#'   .cube = gear
#' )
# nolint end
nest_by_with_margins <- function(.data,
                                 .rollup = NULL,
                                 .by = NULL,
                                 .cube = NULL,
                                 .margin_name = "(all)",
                                 .check_margin_name = TRUE,
                                 .sort = TRUE,
                                 .key = "data",
                                 .keep = FALSE) {
  assert_nest_possible(.data)
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  assert_logical_scalar(.keep)
  assert_string_scalar(.margin_name)
  assert_string_scalar(.key)

  .data <- nest_with_margins(
    .data = .data,
    .rollup = {{ .rollup }},
    .by = {{ .by }},
    .cube = {{ .cube }},
    .margin_name = .margin_name,
    .check_margin_name = .check_margin_name,
    .sort = .sort,
    .key = .key,
    .keep = .keep
  )

  dplyr::rowwise(
    data = .data,
    c({{ .by }}, {{ .rollup }}, {{ .cube }})
  )
}
