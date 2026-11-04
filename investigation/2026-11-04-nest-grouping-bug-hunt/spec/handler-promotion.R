source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = getwd())
pkgload::load_all(source_dir, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)

run <- function() {
  previous <- options(warn = 0L)
  on.exit(options(previous), add = TRUE)
  source <- tibble::tibble(a = 1L, b = "x")
  step <- dtplyr::lazy_dt(source) |> dplyr::mutate(b = "z")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, source, "handler_promotion_probe")
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
  promoted <- structure(list(message = "selector warning promoted by caller", call = NULL),
                        class = c("caller_selector_failure", "error", "condition"))
  capture <- function(expr) {
    tryCatch(withCallingHandlers(force(expr), warning = function(cnd) stop(promoted)),
             error = identity)
  }
  direct <- capture(tidyselect::eval_select(rlang::quo(one_of("absent")), source))
  local <- capture(inspect_grouping(source, .grouping = grouping_set(one_of("absent"))))
  typed <- capture(inspect_grouping(
    step, .grouping = grouping_set(one_of("absent"), where(is.numeric))
  ))
  observed$queries <- character()
  duck <- capture(inspect_grouping(remote, .grouping = grouping_set(one_of("absent"))))
  variable_queries <- observed$queries
  observed$queries <- character()
  fixed <- capture(inspect_grouping(remote, .by = one_of("absent")))
  fixed_queries <- observed$queries
  cat("Baseline: 78ebbc36200bdc5cef70838dd4e772c5c4a1401c\n")
  cat("Warning option:", getOption("warn"), "\n")
  for (name in c("direct", "local", "typed", "duck", "fixed")) {
    condition <- get(name)
    cat(name, ": ", paste(class(condition), collapse = "/"), "\n", sep = "")
    cat(conditionMessage(condition), "\n")
  }
  cat("DuckDB variable query count:", length(variable_queries), "\n")
  cat(variable_queries, sep = "\n")
  cat("\nDuckDB fixed query count:", length(fixed_queries), "\n")
  stopifnot(identical(direct, promoted), identical(local, promoted),
            identical(duck, promoted), identical(fixed, promoted))
  stopifnot(length(fixed_queries) == 0L)
  if (assert_contract) {
    stopifnot(identical(typed, promoted), length(variable_queries) == 0L)
  } else {
    stopifnot(inherits(typed, "marginplyr_error"),
              grepl("Can't safely determine", conditionMessage(typed), fixed = TRUE),
              length(variable_queries) == 1L)
  }
  sessionInfo()
}
run()
