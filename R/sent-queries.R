# The Sent queries of one call, read back by `last_sent_queries()`. It is
# named for what it holds, after `share_dialect_verdicts` in `R/share.R`, and
# the two are opposite shapes on purpose: that one is a session cache never
# emptied, this one a log emptied at the start of every call (ADR 0027).
sent_queries <- new.env(parent = emptyenv())

# Empties the record for the call beginning now, marks that a call has been
# recorded in this session, and reads `marginplyr.audit_sql` -- the one
# reading per call, kept with this call's record (ADR 0027). Anything but
# `TRUE` is "not audited", and nothing here reports it.
#
# Called as the first statement of every entry point, ahead of the argument
# validation each of them opens with (ADR 0027). The backend is not known yet,
# so the SQL flag starts `FALSE` and `remember_sent_query_backend()` sets it
# where the backend is computed.
reset_sent_queries <- function() {
  sent_queries$recorded <- TRUE
  sent_queries$audited <- isTRUE(getOption("marginplyr.audit_sql"))
  sent_queries$is_sql <- FALSE
  sent_queries$rows <- dplyr::tibble(purpose = character(), sql = character())
  invisible(NULL)
}

# Remembers whether this call's input is a SQL backend, reading the one field
# `grouping_backend()` computes for that. The caller holds the backend of the
# input the reset above was made for.
remember_sent_query_backend <- function(backend) {
  stopifnot(inherits(backend, "marginplyr_backend"))
  sent_queries$is_sql <- isTRUE(backend$is_sql)
  invisible(NULL)
}

# Appends one row for `query`, which the caller is about to send under
# `purpose`. A no-op unless the remembered flags say this call is audited and
# its input is SQL, so a site checks neither and takes no backend.
#
# The render is `dbplyr::sql_render()`, client-side, sending nothing. A
# translation dbplyr refuses raises here rather than when the query was built,
# and the row is kept with `sql = NA` (ADR 0027).
record_sent_query <- function(purpose, query) {
  stopifnot(rlang::is_string(purpose))
  if (!isTRUE(sent_queries$audited) || !isTRUE(sent_queries$is_sql)) {
    return(invisible(NULL))
  }
  sql <- tryCatch(
    as.character(dbplyr::sql_render(query)),
    error = function(cnd) NA_character_
  )
  stopifnot(is.character(sql), length(sql) == 1L)
  sent_queries$rows <- dplyr::bind_rows(
    sent_queries$rows,
    dplyr::tibble(purpose = purpose, sql = sql)
  )
  invisible(NULL)
}

#' Read back the SQL marginplyr sent in the last call
#'
#' A Margin verb applied to a SQL backend returns its result unexecuted and
#' may send queries of its own on the way. With
#' `options(marginplyr.audit_sql = TRUE)` set, marginplyr keeps a record of
#' the SQL of one call -- the most recent call to [summarize_with_margins()],
#' [expand_with_margins()], [nest_with_margins()], [nest_by_with_margins()],
#' or [inspect_grouping()] -- and this function reads it back. The record is
#' emptied at the start of every call, so what it holds belongs to that call
#' alone.
#'
#' @return A tibble with two character columns. `purpose` says what the query
#'   was sent for and `sql` holds the statement as it was rendered. Rows are in
#'   the order the queries were sent, which is the only ordering the record
#'   carries. There is no print method: a multi-line statement is read with
#'   `writeLines(x$sql)`.
#'
#' @section What the record promises:
#' The record holds the SQL marginplyr sent, not the execution marginplyr
#' caused. An audited call on a data frame, a dtplyr table, or an Arrow table
#' therefore records nothing, and that zero-row answer is correct rather than
#' a hole.
#'
#' `"result"` is the one `purpose` promised: the query the Margin verb returns
#' unexecuted, rendered before it is handed to you, which your own
#' [dplyr::collect()] is still what runs. Every other row is a query marginplyr
#' sends for its own reasons, and the names those rows carry are not fixed.
#'
#' A query is recorded before it is sent, so the record holds what was sent
#' rather than what succeeded, and a call that fails leaves readable every
#' query it had already sent.
#'
#' Two identical calls need not produce identical records: the answer to
#' whether a backend can compute a contextual share is cached per SQL dialect
#' for the session, so the query that asks it is sent once.
#'
#' The record is a package environment, which is invisible across `fork`,
#' PSOCK, and callr workers, so a read after a parallel run reports the last
#' call made in this process and not one made in a worker.
#'
#' @section Four answers:
#' - Nothing has been recorded in this session -- no Margin verb has begun --
#'   and the call is refused with a `"marginplyr_error"`. A verb that began and
#'   then refused your call has begun, so what is read after one is its own
#'   record, empty, and not this refusal.
#' - The last call was not audited, the option being unset or holding a value
#'   other than `TRUE` when the call began, and the call is refused with a
#'   `"marginplyr_error"` naming `marginplyr.audit_sql`. The option is read
#'   once, as the call begins; setting it afterwards does not audit a call
#'   already made.
#' - The call was audited and sent nothing: a zero-row tibble, the only answer
#'   with zero rows.
#' - A statement had no SQL form on this backend -- a translation dbplyr
#'   refuses when the query is rendered -- and its row holds `NA` in `sql`.
#'   The call itself is unaffected; the refusal is raised when you render or
#'   collect the result.
#'
#' @section Writing the record out:
#' marginplyr writes no file. The tibble is what you write, in whatever format
#' your audit needs:
#'
#' ```r
#' utils::write.csv(last_sent_queries(), path)
#' ```
#'
#' For a batch of calls, read the record after each one and bind the pieces:
#'
#' ```r
#' records <- lapply(specs, function(spec) {
#'   summarize_with_margins(sales_db, revenue = sum(revenue), .grouping = spec)
#'   last_sent_queries()
#' })
#' utils::write.csv(do.call(rbind, records), path)
#' ```
#'
#' @seealso *[When marginplyr queries your data][summarize_with_margins]* for
#'   the queries a Margin verb sends without being asked, and
#'   [dplyr::show_query()] for the result query alone.
#' @export
#' @examples
#' # The record is kept only while the option is set, so the example restores
#' # it. The guard shipped with marginplyr keeps the example runnable without
#' # DuckDB.
#' source(system.file("suggests", "guard.R", package = "marginplyr"))
#' if (
#'   marginplyr_suggest_available("DBI") &&
#'   marginplyr_suggest_available("duckdb")
#' ) {
#'   old <- options(marginplyr.audit_sql = TRUE)
#'   # `shared_home = FALSE` keeps DuckDB's extension cache and stored secrets
#'   # inside the session's temporary directory instead of `~/.duckdb`.
#'   con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
#'
#'   sales_db <- dplyr::copy_to(
#'     con,
#'     retail_sales,
#'     name = "retail_sales",
#'     temporary = TRUE,
#'     overwrite = TRUE
#'   )
#'   query <- summarize_with_margins(
#'     .data = sales_db,
#'     revenue = sum(revenue, na.rm = TRUE),
#'     .grouping = rollup(region, store)
#'   )
#'
#'   sent <- last_sent_queries()
#'   sent$purpose
#'   writeLines(sent$sql[sent$purpose == "result"])
#'
#'   DBI::dbDisconnect(con)
#'   options(old)
#' }
last_sent_queries <- function() {
  if (!isTRUE(sent_queries$recorded)) {
    abort_marginplyr(c(
      "No Margin operation has run in this session.",
      i = "Run a Margin verb or {.fun inspect_grouping} first."
    ))
  }
  if (!isTRUE(sent_queries$audited)) {
    abort_marginplyr(c(
      "The last Margin operation was not audited.",
      i = paste0(
        "Set {.code options(marginplyr.audit_sql = TRUE)} ",
        "before the call to record its queries."
      )
    ))
  }
  sent_queries$rows
}
