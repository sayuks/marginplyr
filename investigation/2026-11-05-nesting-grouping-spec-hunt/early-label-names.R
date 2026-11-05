# Run from the package root. --assert requires the local label error before SQL.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

run <- function() {
  x <- tibble::tibble(g = "x", v = 1L)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, x, "label_names", temporary = TRUE)

  state <- new.env(parent = emptyenv())
  old <- options(marginplyr.hunt_label_queries = state)
  on.exit(options(old), add = TRUE)
  signature <- c("duckdb_connection", "character")
  suppressMessages(trace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    tracer = quote({
      recorded <- getOption("marginplyr.hunt_label_queries")
      recorded$sql <- c(recorded$sql, statement)
    }), print = FALSE
  ))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE)

  capture <- function(expr) {
    err <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(err, "marginplyr_error"))
    conditionMessage(err)
  }
  label <- c(g = "all", ghost = "all")
  local_error <- capture(expand_with_margins(
    x, .grouping = rollup(g), .margin_label = label
  ))
  stopifnot(grepl("unknown dimension name", local_error, fixed = TRUE))
  state$sql <- character()
  remote_error <- capture(expand_with_margins(
    remote, .grouping = rollup(g), .margin_label = label
  ))
  actual <- state$sql
  cat(remote_error, "\n")
  print(actual)
  stopifnot(identical(remote_error, local_error))

  state$sql <- character()
  control <- expand_with_margins(remote, .grouping = rollup(g),
                                 .margin_label = c(g = "all"))
  stopifnot(inherits(control, "tbl_lazy"), length(state$sql) == 1L,
            grepl("LIMIT 0", state$sql, fixed = TRUE))

  derived <- dplyr::mutate(dtplyr::lazy_dt(x), new = toupper(v))
  for (consumer in list(nest_with_margins, nest_by_with_margins)) {
    expected <- capture(consumer(x, .grouping = rollup(g), .margin_label = label))
    observed <- capture(consumer(derived, .grouping = rollup(g), .margin_label = label))
    cat(observed, "\n")
    if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
      stopifnot(identical(observed, expected))
    } else {
      stopifnot(grepl("Can't safely determine column metadata", observed, fixed = TRUE))
    }
  }

  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(length(actual) == 0L)
  } else {
    stopifnot(length(actual) == 1L, grepl("LIMIT 0", actual, fixed = TRUE))
  }
}
run()
