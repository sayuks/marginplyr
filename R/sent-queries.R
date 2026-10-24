# The active call's Sent queries, or the last outer call's after it exits.
sent_queries <- new.env(parent = emptyenv())

# Starts a call's record and returns the active parent's state, or NULL for
# an outer call. Entry points force `.data` first and register restoration
# immediately afterwards. Backend classification follows during preparation.
reset_sent_queries <- function() {
  parent <- if (isTRUE(sent_queries$active)) as.list(sent_queries) else NULL
  sent_queries$active <- TRUE
  sent_queries$recorded <- TRUE
  sent_queries$audited <- isTRUE(getOption("marginplyr.audit_sql"))
  sent_queries$is_sql <- FALSE
  sent_queries$rows <- dplyr::tibble(purpose = character(), sql = character())
  invisible(parent)
}

# Restores the active parent's record on exit, or leaves this outer call's
# own record readable. The caller holds the state returned at its entry.
restore_sent_queries <- function(parent) {
  if (is.null(parent)) {
    sent_queries$active <- FALSE
  } else {
    list2env(parent, envir = sent_queries)
  }
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

# Renders for the record without using rlang's warning frequency state. The
# caller holds the lazy SQL query for an audited site. dbplyr warns about
# implicit NA removal while translating SQL; the actual render must still own
# that warning when `warn = 2`. A refused translation leaves NA.
render_sent_query_sql <- function(query) {
  old <- options(rlib_warning_verbosity = "quiet")
  on.exit(options(old), add = TRUE)
  tryCatch(
    as.character(dbplyr::sql_render(query)),
    error = function(cnd) NA_character_
  )
}

# Appends one prepared SQL representation for a lazy query or literal under
# `purpose`. A no-op unless this call is audited and its input is SQL.
# A site checks neither and takes no backend.
#
# The render is client-side, sending nothing. A translation dbplyr refuses
# raises here rather than when the query was built, and the row is kept with
# `sql = NA` (ADR 0027).
record_sent_query <- function(purpose, query) {
  stopifnot(rlang::is_string(purpose))
  if (!isTRUE(sent_queries$audited) || !isTRUE(sent_queries$is_sql)) {
    return(invisible(NULL))
  }
  sql <- if (inherits(query, "sql")) {
    as.character(query)
  } else {
    render_sent_query_sql(query)
  }
  stopifnot(is.character(sql), length(sql) == 1L)
  sent_queries$rows <- dplyr::bind_rows(
    sent_queries$rows,
    dplyr::tibble(purpose = purpose, sql = sql)
  )
  invisible(NULL)
}

# The first of the four answers below refers to the list of calls the
# description names rather than repeating it, so the page holds one enumeration
# and the two cannot disagree (ADR 0027, *one page states the contract and two
# point at it*). `test-documentation.R` holds that list to the entry points the
# code has.

#' Inspect SQL prepared by the last marginplyr call
#'
#' A Margin verb applied to a SQL backend returns its result unexecuted and
#' may send queries of its own on the way. With
#' `options(marginplyr.audit_sql = TRUE)` set, marginplyr keeps a record of
#' the SQL of one call -- the most recent call to [summarize_with_margins()],
#' [expand_with_margins()], [nest_with_margins()], [nest_by_with_margins()],
#' or [inspect_grouping()] -- and this function reads it back. The record is
#' started afresh for every call, so what it holds belongs to that call
#' alone. Piping one Margin verb into another is two calls, and the outer one
#' is the most recent: its input runs first and its rows are then emptied,
#' leaving the outer call's own.
#'
#' When user code inside a tracked call A invokes another tracked call B,
#' the record belongs to B while B runs, then to A again as soon as B exits.
#' A keeps its earlier rows and its own audit setting, including when it
#' catches B's error. After A returns or fails, its own complete or partial
#' record remains readable; B's rows are not appended or retained separately.
#' An error handler running while B is still active can read B's partial
#' record. This applies to inspection as well as Margin verbs.
#'
#' The input `.data` is evaluated before that call starts recording. If the
#' input fails first, the outer call starts no record. Other arguments do
#' not gain this early evaluation, and recording does not restore user options.
#'
#' @param include_result One nonmissing logical value. `TRUE` (the default)
#'   includes the prepared, unexecuted result SQL. `FALSE` selects internal
#'   rows only, without needing their descriptive purpose names.
#' @return A tibble with two character columns, `purpose` and `sql`, containing
#'   prepared SQL representations in recording order. Only `"result"` is a
#'   stable public purpose value; all other purpose names are descriptive.
#'   A failed diagnostic render leaves `NA_character_` in `sql`. An empty
#'   view retains both character columns. Read a multi-line statement with
#'   `writeLines(x$sql[!is.na(x$sql)])`.
#'
#' @section What the record promises:
#' The record belongs to the last Margin verb or inspection call during query
#' construction. It holds SQL prepared for that call's internal work and a
#' render of its unexecuted result. Finite collection and direct
#' materialization do not append LIMIT, destination metadata, savepoint, DDL,
#' INSERT, or ANALYZE statements. Computing an older result leaves a newer
#' call's record unchanged. An audited call on a data frame, dtplyr, or Arrow
#' table therefore records nothing; that zero-row answer is correct.
#'
#' `"result"` is the one `purpose` promised: the query the Margin verb returns
#' unexecuted, rendered before it is handed to you, which your own
#' [dplyr::collect()] is still what runs. Every other row is a query marginplyr
#' prepares for its own work. `include_result = FALSE` removes only the
#' `"result"` rows, retaining internal rows even when their SQL is missing.
#' Reading either view does no rendering, query, fetch, or EXPLAIN and changes
#' neither the stored rows nor a copy you already hold.
#'
#' With `.check_margin_label = TRUE`, an observed label check aggregates
#' collision counts in the database and retrieves one row, with one count per
#' checked column. It does not retrieve all unique values. Zero-row SQL
#' metadata on DuckDB is a separate internal operation. Recording requests
#' neither check: a cache hit or a label check with no columns adds no query.
#'
#' An internal query is recorded before invoking the backend operation.
#' The SQL is a render, not a capture of DBI arguments or proof that the
#' database received or completed the query. A call that fails leaves its
#' own earlier rows readable; the record contains no execution outcomes.
#'
#' Two identical calls need not produce identical records: the answer to
#' whether a backend can compute a contextual share is cached per SQL dialect
#' for the session, so the query that asks it is sent once.
#' Savepoint controls used during RPostgres dialect detection are recorded too.
#'
#' The record is a package environment, which is invisible across `fork`,
#' PSOCK, and callr workers, so a read after a parallel run reports the last
#' call made in this process and not one made in a worker.
#'
#' @section Four answers:
#' - Nothing has been recorded in this session -- none of the calls the
#'   description above lists has begun -- and the call is refused with a
#'   `"marginplyr_error"`. A call that began and then refused your arguments
#'   has begun, so what is read after one is its own record, empty, and not
#'   this refusal.
#' - The last call was not audited, the option being unset or holding a value
#'   other than `TRUE` when the call began, and the call is refused with a
#'   `"marginplyr_error"` naming `marginplyr.audit_sql`. The option is read
#'   once, as the call begins; setting it afterwards does not audit a call
#'   already made.
#' - The call was audited but the selected view has no SQL rows: a zero-row
#'   tibble. This includes validation-only refusal and an internal-only view
#'   of a call that recorded just its result.
#' - A diagnostic render failed, and its row holds `NA` in `sql`. Recording
#'   does not raise that render error. The internal backend operation, or a
#'   later explicit render or collection of the result, may still fail.
#'
#' @section Writing the record out:
#' Copy or export the tibble before the next tracked call replaces the record.
#' marginplyr keeps no accumulated history and writes no file automatically:
#'
#' ```r
#' internal <- last_sent_queries(include_result = FALSE)
#' utils::write.csv(internal, path, row.names = FALSE)
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
#'   the queries a Margin verb sends without being asked,
#'   [dplyr::show_query()] for the result query alone, and *Record the SQL
#'   marginplyr prepares* in the [database-backends guide][guide] for the record
#'   read against a live connection.
#'
#' [guide]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html
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
#'     .grouping = rollup(region, store),
#'     .check_margin_label = TRUE
#'   )
#'
#'   internal <- last_sent_queries(include_result = FALSE)
#'   internal$purpose
#'   writeLines(internal$sql[!is.na(internal$sql)])
#'   sent <- last_sent_queries()
#'   writeLines(sent$sql[sent$purpose == "result"])
#'   path <- tempfile(fileext = ".rds")
#'   saveRDS(internal, path)
#'
#'   # A failed call also leaves its own partial record readable.
#'   tryCatch(
#'     summarize_with_margins(
#'       dplyr::mutate(sales_db, region = "Total"),
#'       revenue = sum(revenue, na.rm = TRUE),
#'       .grouping = rollup(region),
#'       .check_margin_label = TRUE
#'     ),
#'     marginplyr_error = function(cnd) {
#'       print(last_sent_queries(include_result = FALSE))
#'     }
#'   )
#'   # `internal` and its exported copy still describe the first call.
#'   identical(readRDS(path), internal)
#'   unlink(path)
#'   DBI::dbDisconnect(con)
#'   options(old)
#' }
last_sent_queries <- function(include_result = TRUE) {
  if (!rlang::is_bool(include_result)) {
    abort_marginplyr(
      "{.arg include_result} must be one non-missing logical value."
    )
  }
  if (!isTRUE(sent_queries$recorded)) {
    abort_marginplyr(c(
      "No tracked call has run in this session.",
      i = "Run a Margin verb or {.fun inspect_grouping} first."
    ))
  }
  if (!isTRUE(sent_queries$audited)) {
    abort_marginplyr(c(
      "The last tracked call was not audited.",
      i = paste0(
        "Set {.code options(marginplyr.audit_sql = TRUE)} ",
        "before the call to record its queries."
      )
    ))
  }
  rows <- sent_queries$rows
  if (!include_result) {
    rows <- rows[rows$purpose != "result", , drop = FALSE]
  }
  dplyr::tibble(purpose = rows$purpose, sql = rows$sql)
}
