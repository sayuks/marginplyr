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
#' `"marginplyr_error"` is the only class marginplyr promises. Narrower
#' subclasses and the wording of any message are implementation details that
#' can change without a deprecation cycle, so match on the class rather than on
#' message text.
#'
#' The wording is marginplyr's to change; the columns, values, and arguments a
#' message quotes are yours, and a marginplyr error spells them as you spelled
#' them. Two spellings it cannot keep: a line break and a no-break space inside
#' a name are both shown as an ordinary space. This says nothing about the
#' errors below, which marginplyr does not write.
#'
#' Two kinds of error deliberately fall outside the class. Errors raised by
#' your own summary expressions, by tidyselect, by dplyr, or by a database
#' backend propagate with their original class, diagnostic, and cause intact.
#' So do marginplyr's internal invariant checks, which report a defect no
#' change to your call can avoid; please report those at
#' <https://github.com/sayuks/marginplyr/issues>.
#'
#' marginplyr itself raises no warning: it states what it will not do by
#' refusing. What a Margin verb does adjust, for an error and a warning alike,
#' is the context reported around one, because a margin operation may summarize
#' your expression once per grouping set. Such a condition reports its grouping
#' values under the columns you named rather than under internal ones, quotes
#' the argument as you spelled it rather than as marginplyr rewrote it to
#' compute the grouping sets, and an error blames the Margin verb you wrote
#' rather than the internal summary that ran it. A warning still names that
#' internal summary, because the name is part of a sentence dplyr renders
#' before marginplyr sees the warning at all. Where the expression dplyr quoted
#' cannot be matched -- a long one dplyr shortens to `+...`, or a diagnostic a
#' later dplyr lays out differently -- the quotation stays as dplyr wrote it.
#'
#' A warning that every grouping set raises is reported once, saying how many
#' further grouping sets raised it; warnings that differ from each other are
#' reported one by one. A lazy input is outside this, and visibly so: its
#' summary expressions run when you collect the result rather than while the
#' verb runs, so what they raise is the collecting call's to report.
#'
#' @section Recording the SQL marginplyr sends:
#' marginplyr keeps no record of the SQL it sends unless you ask for one.
#' `options(marginplyr.audit_sql = TRUE)` switches the record on, and
#' [last_sent_queries()] reads back what the most recent call sent:
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
#' last_sent_queries()
#' options(old)
#' ```
#'
#' The record is a tibble of two character columns -- `purpose`, what the
#' query was sent for, and `sql`, the statement as it was rendered -- with one
#' row per query, in the order the queries were sent. It belongs to a single
#' call, being emptied at the start of every one.
#'
#' `"result"` is the one `purpose` promised: the query a Margin verb returns
#' unexecuted, which your own [dplyr::collect()] is still what runs. Every
#' other row is a query marginplyr sends for its own reasons, and the names
#' those rows carry are not fixed.
#'
#' A query is recorded before it is sent, so the record holds what was sent
#' rather than what succeeded, and a call that fails leaves readable every
#' query it had already sent.
#'
#' What is recorded is the SQL marginplyr sent, not the execution marginplyr
#' caused. An audited call on a data frame, a dtplyr table, or an Arrow table
#' therefore records nothing, and that zero-row answer is correct rather than
#' a hole.
#'
#' There is one capture, `dbplyr::sql_render()`. It renders in your own
#' session and sends nothing the call did not already send, so what
#' *[When marginplyr queries your data][summarize_with_margins]* enumerates is
#' the same whether or not you are recording.
#'
#' [last_sent_queries()] gives four answers, and each of the three that could
#' look like an empty record is told apart from the others:
#'
#' - Nothing has been recorded in this session -- neither a Margin verb nor
#'   [inspect_grouping()] has run -- and the call is refused with a
#'   `"marginplyr_error"`.
#' - The last call was not audited, the option being unset or holding a value
#'   other than `TRUE` when the call began, and the call is refused with a
#'   `"marginplyr_error"` naming `marginplyr.audit_sql`. The option is read
#'   once, as the call begins; setting it afterwards does not audit a call
#'   already made.
#' - The call was audited and sent nothing: a zero-row tibble, the only answer
#'   with zero rows.
#' - A statement had no SQL form on this backend -- a translation dbplyr
#'   refuses when the query is rendered -- and its row holds `NA` in `sql`.
#'   The call itself is unaffected.
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
#' @keywords internal
#' @examples
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store)
#' )
"_PACKAGE"
