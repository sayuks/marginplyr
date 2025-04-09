# nolint start: line_length_linter
#' `UNION ALL` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' duplicate the rows and merge them vertically (like `UNION ALL` in SQL).
#' See [Get started](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
#' for more details.
#'
#' @inherit summarize_with_margins
#' @details
#' * Consider each margin as a new category, duplicate the rows and merge them
#'   vertically (like `UNION ALL` in SQL).
#' * The use of arguments in common with `summarize_with_margins()` is the same
#'   as for it.
#' * __Be aware that the number of rows can be huge.__
#' @references
#'  * Online documentation: [Get started](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
#' @family summarize and expand data with margins
#' @export
#' @examples
#' union_all_with_margins(
#'   mtcars,
#'   .rollup = c(cyl, vs),
#'   .by = am,
#'   .cube = gear
#' )
# nolint end
union_all_with_margins <- function(.data,
                                   .rollup = NULL,
                                   .by = NULL,
                                   .cube = NULL,
                                   .margin_name = "(all)",
                                   .check_margin_name = is.data.frame(.data),
                                   .sort = is.data.frame(.data)) {
  assert_lazy_table(.data)
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  assert_string_scalar(.margin_name)

  .f <- function(.data, .margin_pairs, .by) {
    dplyr::mutate(.data = .data, !!!.margin_pairs)
  }

  with_margins(
    .data = .data,
    .rollup = {{ .rollup }},
    .by = {{ .by }},
    .cube = {{ .cube }},
    .margin_name = .margin_name,
    .check_margin_name = .check_margin_name,
    .f = .f,
    .sort = .sort
  )
}
