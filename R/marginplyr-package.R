#' marginplyr: SQL-Style Grouping Sets, Rollups, and Cubes for dplyr
#'
#' marginplyr extends [dplyr::summarize()] with SQL-style `GROUPING SETS`,
#' `ROLLUP`, and `CUBE` summaries: totals, subtotals, and arbitrary grouping
#' combinations, with grouping identifiers to tell the resulting grains
#' apart. Local data frames and lazy tables are supported. Confirmed
#' database backends use native grouping sets; other lazy backends use a
#' `UNION ALL` fallback with the same semantics.
#'
#' @section Get started:
#' Store detail, region subtotals, and a company total from one call:
#'
#' ```r
#' library(marginplyr)
#'
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store)
#' )
#' ```
#'
#' @section Next steps:
#' - **Grouping specifications**: [grouping_set()], [grouping_sets()],
#'   [rollup()], [cube()], and [grouping_spec()] describe which grouping
#'   sets a margin operation computes.
#' - **Margin operations**: [summarize_with_margins()],
#'   [expand_with_margins()], [nest_with_margins()], and
#'   [nest_by_with_margins()] apply a grouping specification to data.
#' - **Grouping-plan inspection and grouping identities**:
#'   [inspect_grouping()], [grouping_bit()], and [grouping_id()] resolve and
#'   identify grouping sets before or after running a margin operation.
#' - **Contextual shares**: [share_of_parent()] and [share_of_total()]
#'   calculate a summary's ratio to its immediate rollup parent, or to the
#'   grand total.
#'
#' @section Errors:
#' Every error marginplyr raises for a call you can correct inherits the
#' `"marginplyr_error"` class, so one handler catches them all:
#'
#' ```r
#' tryCatch(
#'   summarize_with_margins(
#'     retail_sales,
#'     revenue = sum(revenue),
#'     .grouping = rollup(region),
#'     .duplicates = "merge"
#'   ),
#'   marginplyr_error = function(cnd) conditionMessage(cnd)
#' )
#' ```
#'
#' `"marginplyr_error"` is the only class marginplyr promises. Narrower
#' subclasses and the wording of any message are implementation details that
#' can change without a deprecation cycle, so match on the class rather than on
#' message text.
#'
#' Two kinds of error deliberately fall outside the class. Errors raised by
#' your own summary expressions, by tidyselect, by dplyr, or by a database
#' backend propagate with their original class and call intact. So do
#' marginplyr's internal invariant checks, which report a defect no change to
#' your call can avoid; please report those at
#' <https://github.com/sayuks/marginplyr/issues>.
#'
#' @section Guides:
#' - [Get started][g1]
#' - [Database and lazy backends][g2]
#' - [Grouping identity][g3]
#' - [Complete absent keys before margins][g4]
#'
#' [g1]: https://sayuks.github.io/marginplyr/vignettes/get_started.html
#' [g2]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html
#' [g3]: https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html
#' [g4]: https://sayuks.github.io/marginplyr/vignettes/completing_keys.html
#'
#' @keywords internal
#' @examples
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store)
#' )
"_PACKAGE"
