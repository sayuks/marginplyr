#' marginplyr: SQL-Style Grouping Sets, Rollups, and Cubes for dplyr
#'
#' marginplyr extends [dplyr::summarize()] with SQL-style `GROUPING SETS`,
#' `ROLLUP`, and `CUBE` summaries: totals, subtotals, and arbitrary grouping
#' combinations, with Grouping identifiers to tell the resulting grains
#' apart. Local data frames and lazy tables are supported. Most summaries on
#' DuckDB and PostgreSQL use native grouping sets; other database backends,
#' and every expansion, use a `UNION ALL` fallback. The *Database and lazy
#' backends* guide lists the summaries that also take the fallback.
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
#' @section Guides:
#' - [Get started][g1]
#' - [Recipes for common reporting tasks][g5]
#' - [Grouping identity][g3]
#' - [Complete absent keys before margins][g4]
#' - [Database and lazy backends][g2]
#'
#' [g1]: https://sayuks.github.io/marginplyr/vignettes/get_started.html
#' [g2]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html
#' [g3]: https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html
#' [g4]: https://sayuks.github.io/marginplyr/vignettes/completing_keys.html
#' [g5]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @section Next steps:
#' - **Grouping specifications**: [grouping_set()], [grouping_sets()],
#'   [rollup()], [cube()], and [grouping_spec()] describe which grouping
#'   sets a Margin operation computes.
#' - **Margin operations**: [summarize_with_margins()],
#'   [expand_with_margins()], [nest_with_margins()], and
#'   [nest_by_with_margins()] apply a Grouping specification to data.
#' - **Grouping-plan inspection and grouping identities**:
#'   [inspect_grouping()], [grouping_bit()], and [grouping_id()] resolve and
#'   identify grouping sets before or after running a Margin operation.
#' - **Contextual shares**: [share_of_parent()] and [share_of_total()]
#'   calculate a summary's ratio to its immediate rollup parent, or to the
#'   grand total.
#'
#' @section Errors and warnings:
#' Every error marginplyr raises for a call you can correct inherits the
#' `"marginplyr_error"` class, and a Margin verb raises it directly, so a
#' `tryCatch()` handler for that class catches it. A marginplyr helper such as
#' [grouping_bit()] used inside an ordinary dplyr verb, and an error your own
#' summary expression raises, instead arrive as the `$parent` of the error
#' dplyr raises, so match them through the parent with [rlang::try_fetch()],
#' as the examples below do.
#'
#' [marginplyr_conditions] states the rest of the contract.
#'
#' @section Recording the SQL marginplyr prepares:
#' With `options(marginplyr.audit_sql = TRUE)` set before a Margin verb call,
#' [last_sent_queries()] reads back the SQL that call prepared. Its internal
#' rows are what marginplyr sent while building the call, and its `"result"`
#' row is what your own [dplyr::collect()] will send.
#'
#' [last_sent_queries()] states what the record holds and promises.
#'
#' @examples
#' # A Margin verb raises the class directly, so `tryCatch()` catches it.
#' tryCatch(
#'   summarize_with_margins(
#'     retail_sales,
#'     revenue = sum(revenue),
#'     .grouping = rollup(region),
#'     .duplicates = "merge"
#'   ),
#'   marginplyr_error = function(cnd) conditionMessage(cnd)
#' )
#'
#' # A helper inside an ordinary dplyr verb arrives as the parent of dplyr's
#' # error, which `rlang::try_fetch()` searches.
#' rlang::try_fetch(
#'   dplyr::mutate(retail_sales, bit = grouping_bit(region)),
#'   marginplyr_error = function(cnd) conditionMessage(cnd)
#' )
"_PACKAGE"
