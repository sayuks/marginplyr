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
  sent_queries$record <- new.env(parent = emptyenv())
  sent_queries$record$rows <- dplyr::tibble(
    purpose = character(), sql = character(), source = character(),
    status = character(), conditions = list()
  )
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

# An empty condition summary with the same column types as populated summaries.
empty_sent_query_conditions <- function() {
  dplyr::tibble(
    classes = list(), message = character(), phase = character(),
    handling = character()
  )
}

# Appends only text and classes to the owning row. `entry` identifies that row
# independently of whichever nested call currently owns the public record.
record_sent_query_condition <- function(entry, cnd, phase, handling) {
  rows <- entry$record$rows
  rows$conditions[[entry$index]] <- dplyr::bind_rows(
    rows$conditions[[entry$index]],
    dplyr::tibble(
      classes = list(class(cnd)), message = conditionMessage(cnd),
      phase = phase, handling = handling
    )
  )
  entry$record$rows <- rows
  invisible(NULL)
}

# Renders an audited row without consuming dbplyr's warning-frequency state
# (#655). Diagnostic warnings are suppressed; the real operation still owns
# warning delivery, including conversion to error under the caller's settings.
render_sent_query_sql <- function(query, entry) {
  old <- options(rlib_warning_verbosity = "quiet")
  on.exit(options(old), add = TRUE)
  tryCatch(
    withCallingHandlers(
      as.character(dbplyr::sql_render(query)),
      warning = function(cnd) {
        record_sent_query_condition(entry, cnd, "render", "suppressed")
        invokeRestart("muffleWarning")
      },
      interrupt = function(cnd) {
        record_sent_query_condition(entry, cnd, "render", "propagated")
      }
    ),
    error = function(cnd) {
      record_sent_query_condition(entry, cnd, "render", "handled")
      NA_character_
    }
  )
}

# Prepares a lazy query or literal row and returns its private update handle.
# A site needs neither a backend nor an audit check: inactive/non-SQL calls
# return NULL. A refused render retains a prepared row with missing SQL.
record_sent_query <- function(purpose, query) {
  stopifnot(rlang::is_string(purpose))
  if (!isTRUE(sent_queries$audited) || !isTRUE(sent_queries$is_sql)) {
    return(invisible(NULL))
  }
  record <- sent_queries$record
  literal <- inherits(query, "sql")
  record$rows <- dplyr::bind_rows(record$rows, dplyr::tibble(
    purpose = purpose, sql = NA_character_,
    source = if (literal) "literal" else "render", status = "prepared",
    conditions = list(empty_sent_query_conditions())
  ))
  entry <- list(record = record, index = nrow(record$rows))
  sql <- if (literal) {
    as.character(query)
  } else {
    render_sent_query_sql(query, entry)
  }
  stopifnot(is.character(sql), length(sql) == 1L)
  record$rows$sql[[entry$index]] <- sql
  invisible(entry)
}

# Observes one owned backend operation without catching or rethrowing it.
# `expr` is the backend call alone; its existing suppression/catch surrounds
# this seam and supplies the handling known there. Nonlocal unwinding without
# an observed error/interrupt leaves unknown completion, never success.
with_sent_query <- function(purpose, query, expr,
                            error_handling = "propagated",
                            warning_handling = "propagated") {
  entry <- record_sent_query(purpose, query)
  if (is.null(entry)) {
    return(force(expr))
  }
  entry$record$rows$status[[entry$index]] <- "invoking"
  status <- "unknown"
  on.exit({
    entry$record$rows$status[[entry$index]] <- status
  }, add = TRUE)
  value <- withCallingHandlers(
    force(expr),
    warning = function(cnd) {
      record_sent_query_condition(entry, cnd, "invocation", warning_handling)
    },
    error = function(cnd) {
      record_sent_query_condition(entry, cnd, "invocation", error_handling)
      status <<- "errored"
    },
    interrupt = function(cnd) {
      record_sent_query_condition(entry, cnd, "invocation", "propagated")
      status <<- "interrupted"
    }
  )
  status <- "returned"
  value
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
#' @param details One nonmissing logical value. `FALSE` (the default) returns
#'   the original two columns. `TRUE` selects the stored diagnostic details;
#'   it does not enable capture or repeat work.
#' @return A tibble in recording order. By default its two character columns
#'   are `purpose` and `sql`. With `details = TRUE` the columns are `purpose`,
#'   `sql`, `source`, `status`, and `conditions`, in that order. The first four
#'   are character; `conditions` is a list of condition-summary tibbles.
#'   Empty views retain these column types. Only `"result"` is a stable public
#'   purpose value; all other purpose names are descriptive.
#'   A failed diagnostic render leaves `NA_character_` in `sql`. Read a
#'   multi-line statement with `writeLines(x$sql[!is.na(x$sql)])`.
#'
#' @section Diagnostic details:
#' Details are captured during an audited call even when you first read only
#' the default view. Both accessor arguments select stored rows and columns.
#' `source` is `"render"` for an attempted diagnostic render or `"literal"`
#' for SQL supplied by the package. Neither proves database receipt.
#'
#' `status` describes only the owned internal backend operation:
#' - `"prepared"`: a row exists but no invocation was observed. The
#'   compatibility `"result"` row stays in this state.
#' - `"invoking"`: control reached the boundary immediately before the
#'   backend call, with no terminal observation yet. This can be read while
#'   the operation is active; it does not prove that the driver body ran.
#' - `"returned"`: the operation returned normally, possibly with warnings.
#' - `"errored"`: an error left the operation, even if package logic then
#'   handled it.
#' - `"interrupted"`: an interrupt left the operation.
#' - `"unknown"`: the call unwound without an established terminal observation
#'   for its invocation attempt.
#'
#' Each `conditions` element is an ordered tibble with `classes` (a list of
#' character class vectors), `message` (character), `phase` (`"render"` or
#' `"invocation"`), and `handling` (`"handled"`, `"suppressed"`,
#' `"propagated"`, or `"unknown"`). No observed conditions gives a typed empty
#' tibble. Summaries retain errors, warnings, and interrupts seen at the owned
#' boundary, without retaining condition objects, environments, results, or
#' connections. Handling describes that boundary: `"propagated"` allows user
#' code to catch the condition later, and handling is stated only when known.
#' Reading the record never signals these conditions again.
#'
#' Render failures retain their summaries independently of invocation status:
#' missing SQL and a `"returned"` operation can coexist. A failed dialect
#' question can have a handled error followed by a distinct, successful numeric
#' control. The probe's existing warning suppression is preserved. Savepoint,
#' rollback, and release controls each have their own row.
#'
#' A label-count query can be `"returned"` before validation refuses a Margin
#' label collision. That public error belongs to the whole call and is not
#' attached to the successful SQL row. Neither status nor SQL establishes
#' server completion or the outcome of the entire public call.
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
#' own earlier rows readable; the detailed view describes observed internal
#' outcomes at the package boundary.
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
#'   internal <- last_sent_queries(include_result = FALSE, details = TRUE)
#'   internal[c("purpose", "status")]
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
#'       # The label-count query returned before validation refused the call.
#'       print(last_sent_queries(include_result = FALSE, details = TRUE))
#'     }
#'   )
#'   # `internal` and its exported copy still describe the first call.
#'   identical(readRDS(path), internal)
#'   unlink(path)
#'
#'   # A cold DuckDB dialect question errors and the numeric control returns.
#'   # A cached dialect skips both operations, leaving no artificial rows.
#'   summarize_with_margins(
#'     sales_db, revenue = sum(revenue, na.rm = TRUE),
#'     share = share_of_total(revenue), .grouping = rollup(region)
#'   )
#'   details <- last_sent_queries(include_result = FALSE, details = TRUE)
#'   details$conditions[details$status == "errored"]
#'   DBI::dbDisconnect(con)
#'   options(old)
#' }
last_sent_queries <- function(include_result = TRUE, details = FALSE) {
  if (!rlang::is_bool(include_result)) {
    abort_marginplyr(
      "{.arg include_result} must be one non-missing logical value."
    )
  }
  if (!rlang::is_bool(details)) {
    abort_marginplyr(
      "{.arg details} must be one non-missing logical value."
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
  rows <- sent_queries$record$rows
  if (!include_result) {
    rows <- rows[rows$purpose != "result", , drop = FALSE]
  }
  columns <- c("purpose", "sql")
  if (details) {
    columns <- c(columns, "source", "status", "conditions")
  }
  rows[, columns, drop = FALSE]
}
