# Focused reproduction; run from the investigated checkout root.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)

run <- function() {
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  source <- tibble::tibble(a = 1L, b = "x")
  step <- dtplyr::lazy_dt(source) |> dplyr::mutate(b = "z")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, source, "warning_preflight_minimal")
  observed <- new.env(parent = emptyenv())
  observed$queries <- character()
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      observed$queries <- c(observed$queries, get("statement", parent.frame()))
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE, after = FALSE)

  error <- function(expr) tryCatch(force(expr), error = identity)
  oracle <- error(tidyselect::eval_select(rlang::quo(one_of("absent")), source))
  local <- error(inspect_grouping(source, .grouping = grouping_set(one_of("absent"))))
  typed <- error(inspect_grouping(
    step, .grouping = grouping_set(one_of("absent"), where(is.numeric))
  ))
  observed$queries <- character()
  duck <- error(inspect_grouping(
    remote, .grouping = grouping_set(one_of("absent"))
  ))
  variable_queries <- observed$queries
  observed$queries <- character()
  fixed <- error(inspect_grouping(remote, .by = one_of("absent")))
  fixed_queries <- observed$queries
  converted <- function(value) {
    inherits(value, "error") &&
      grepl("converted from warning", conditionMessage(value), fixed = TRUE)
  }
  cat("SHA:", system("git rev-parse HEAD", intern = TRUE), "\n")
  cat("Expected: the converted warning before any metadata request.\n")
  cat("Direct selector:", conditionMessage(oracle), "\n")
  cat("Local:", conditionMessage(local), "\n")
  cat("dtplyr typed sibling:", conditionMessage(typed), "\n")
  cat("DuckDB:", conditionMessage(duck), "\n")
  cat("DuckDB actual variable-selection queries:", length(variable_queries), "\n")
  cat(variable_queries, sep = "\n")
  cat("\nDuckDB fixed-key control queries:", length(fixed_queries), "\n")
  stopifnot(converted(oracle), converted(local), converted(duck), converted(fixed))
  stopifnot(grepl("Can't safely determine", conditionMessage(typed), fixed = TRUE))
  stopifnot(length(variable_queries) == 1L, length(fixed_queries) == 0L)
  if (assert_contract) {
    stopifnot(converted(typed), length(variable_queries) == 0L)
  }
  invisible(NULL)
}
run()
