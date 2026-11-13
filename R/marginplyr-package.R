#' marginplyr: SQL-Style Grouping Sets, Rollups, and Cubes for dplyr
#'
#' marginplyr extends [dplyr::summarize()] with SQL-style `GROUPING SETS`,
#' `ROLLUP`, and `CUBE` summaries: totals, subtotals, and arbitrary grouping
#' combinations, with Grouping identifiers to tell the resulting grains
#' apart. Local data frames and lazy tables are supported. Summaries on
#' DuckDB and PostgreSQL use native grouping sets; other database backends,
#' and every expansion, use a `UNION ALL` fallback.
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
#' - [Database and lazy backends][g2]
#' - [Grouping identity][g3]
#' - [Complete absent keys before margins][g4]
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
#' `"marginplyr_error"` is the only package-specific class marginplyr promises.
#' Narrower subclasses and the wording of any message are implementation details
#' that can change without a deprecation cycle, so match on the class rather
#' than on message text.
#'
#' Other errors deliberately fall outside the class. Errors raised by
#' your own summary expressions, by tidyselect, by dplyr, or by a database
#' backend propagate with their original class, diagnostic, and cause intact.
#' So do marginplyr's internal invariant checks, which report a defect no
#' change to your call can avoid; please report those at
#' <https://github.com/sayuks/marginplyr/issues>.
#'
#' marginplyr itself raises no warning: it states what it will not do by
#' refusing. How a Margin operation reports the conditions your expressions
#' raise, and what an interrupt, a warning handler, or a failed SQLite cleanup
#' leaves you to catch, are stated in [marginplyr_conditions].
#'
#' @section Recording the SQL marginplyr prepares:
#' marginplyr records prepared SQL representations only when you ask for them.
#' `options(marginplyr.audit_sql = TRUE)` switches the record on, and
#' [last_sent_queries()] reads the most recent call's record in recording order:
#'
#' ```r
#' # `sales_db` is a lazy table on a SQL connection.
#' old <- options(marginplyr.audit_sql = TRUE)
#'
#' summarize_with_margins(
#'   sales_db,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store)
#' )
#'
#' internal <- last_sent_queries(include_result = FALSE)
#' last_sent_queries()
#' options(old)
#' ```
#'
#' The default view includes the prepared, unexecuted result SQL;
#' `include_result = FALSE` selects internal work. Add `details = TRUE` for
#' observed internal outcomes and condition summaries, captured during every
#' audited call. Recording renders queries
#' locally or records literal SQL controls, adding no backend operation.
#' A missing SQL value means diagnostic rendering failed, and a recorded
#' representation does not prove database receipt or completion.
#' *[When marginplyr queries your data][summarize_with_margins]* states which
#' internal operations can run. Copy or export the record before the next call
#' replaces it; a failed call leaves its own partial record readable.
#'
#' What the record holds, which calls keep one, and what an empty or incomplete
#' read means are stated in [last_sent_queries()]'s own reference.
#'
#' @examples
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store)
#' )
"_PACKAGE"
