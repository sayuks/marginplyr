args <- commandArgs(TRUE)
main <- function(root = ".") {
  pkgload::load_all(root, quiet = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "dbi_recovery_source")
  fetch <- DBI::dbFetch
  clear <- DBI::dbClearResult
  send <- DBI::dbSendQuery
  leaf <- structure(
    list(message = "resumable DBI interrupt", call = NULL),
    class = c("fixture_dbi_interrupt", "interrupt", "condition")
  )
  failure <- errorCondition("later DBI error", class = "fixture_dbi_error")
  check <- function() {
    summarize_with_margins(
      remote, total = sum(.data$v, na.rm = TRUE), .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    )
  }
  run <- function(audited, phase, ending, signal) {
    options(marginplyr.audit_sql = audited)
    counts <- c(send = 0L, fetch = 0L, clear = 0L)
    forwarded_fetch <- 0L
    cleanup <- character()
    delivered <- list()
    resumed <- FALSE
    recover <- function() {
      withRestarts({
        if (signal == "native") rlang::interrupt() else signalCondition(leaf)
      }, resume_fetch = function() NULL)
    }
    finish <- function() {
      switch(ending,
        returned = NULL,
        unknown = invokeRestart("leave_fetch", "escaped"),
        errored = stop(failure),
        interrupted = stop(leaf)
      )
    }
    testthat::local_mocked_bindings(
      dbSendQuery = function(...) {
        counts[["send"]] <<- counts[["send"]] + 1L
        send(...)
      },
      dbFetch = function(res, ...) {
        counts[["fetch"]] <<- counts[["fetch"]] + 1L
        on.exit({ cleanup <<- c(cleanup, "fetch exit") }, add = TRUE)
        if (phase == "before_fetch") {
          recover()
          finish()
        }
        forwarded_fetch <<- forwarded_fetch + 1L
        value <- fetch(res, ...)
        if (phase == "after_fetch") {
          recover()
          finish()
        }
        if (phase == "onexit") {
          on.exit({ recover(); finish() }, add = TRUE)
        }
        value
      },
      dbClearResult = function(...) {
        counts[["clear"]] <<- counts[["clear"]] + 1L
        cleanup <<- c(cleanup, "clear result")
        clear(...)
      },
      .package = "DBI"
    )
    value <- tryCatch(withRestarts(withCallingHandlers(
      check(),
      interrupt = function(cnd) {
        delivered[[length(delivered) + 1L]] <<- class(cnd)
        if (!resumed) {
          resumed <<- TRUE
          invokeRestart("resume_fetch")
        }
      }
    ), leave_fetch = identity), error = identity, interrupt = identity)
    if (!resumed || !identical(counts, c(send = 1L, fetch = 1L, clear = 1L))) {
      stop("DBI invocation/cleanup count mismatch")
    }
    expected_fetch <- as.integer(phase != "before_fetch" || ending == "returned")
    if (forwarded_fetch != expected_fetch) stop("DBI fetch forwarding mismatch")
    if (audited) {
      rows <- last_sent_queries(FALSE, TRUE)
      if (!identical(rows$status, ending)) stop("DBI invocation status mismatch")
      expected_conditions <- if (ending %in% c("errored", "interrupted")) 2L else 1L
      if (nrow(rows$conditions[[1L]]) != expected_conditions) {
        stop("DBI condition summary count mismatch")
      }
    }
    list(
      class = class(value),
      output = if (inherits(value, "condition")) conditionMessage(value) else if (
        inherits(value, "tbl_sql")
      ) as.character(dbplyr::sql_render(value)) else value,
      counts = counts, forwarded_fetch = forwarded_fetch,
      cleanup = cleanup, delivered = delivered
    )
  }
  cases <- expand.grid(
    phase = c("before_fetch", "after_fetch", "onexit"),
    ending = c("returned", "unknown", "errored", "interrupted"),
    signal = c("bare", "native"), stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(cases))) {
    x <- cases[i, ]
    off <- run(FALSE, x$phase, x$ending, x$signal)
    on <- run(TRUE, x$phase, x$ending, x$signal)
    if (!identical(off, on)) {
      print(x)
      print(off)
      print(on)
      stop("DBI recovery off/on mismatch")
    }
    if (!identical(DBI::dbGetQuery(con, "SELECT 42 AS answer")$answer, 42L)) {
      stop("Connection unusable after recovery")
    }
  }
  cat("PASS:", nrow(cases), "live DBI fetch/cleanup recovery pairs\n")
}
main(if (length(args)) args[[1L]] else ".")
