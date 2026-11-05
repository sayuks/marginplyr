# Run from the investigated source root.
pkgload::load_all(quiet = TRUE)

run <- function() {
  assert_contract <- "--assert" %in% commandArgs(trailingOnly = TRUE)
  data <- data.frame(a = "x", b = 1L)
  root <- dtplyr::lazy_dt(data)
  input_reads <- 0L
  derive <- function(x) {
    input_reads <<- input_reads + 1L
    paste0(x, "!")
  }
  derived <- dplyr::mutate(root, extra = derive(a))
  reference <- transform(data, extra = "x!")
  pick <- "a"
  selectors <- list(
    null_walk = rlang::quo(c(a, NULL)),
    null_helper = rlang::quo(c(a, all_of(NULL))),
    env_dollar = rlang::quo(.env$pick),
    env_bracket = rlang::quo(.env[["pick"]])
  )
  capture <- function(expr) tryCatch(expr, error = identity)
  metadata_refusal <- function(x) {
    inherits(x, "marginplyr_error") &&
      grepl("Can't safely determine column metadata", conditionMessage(x), fixed = TRUE)
  }
  inspect <- function(input, select, position) {
    if (position == "by") {
      return(inspect_grouping(input, .by = !!select, .format = "list")$fixed)
    }
    inspect_grouping(
      input, .grouping = grouping_set(!!select), .format = "list"
    )$included
  }
  for (position in c("by", "grouping")) {
    for (label in names(selectors)) {
      selector <- selectors[[label]]
      expected <- list(names(tidyselect::eval_select(selector, reference)))
      stopifnot(identical(expected, list("a")))
      stopifnot(identical(inspect(reference, selector, position), expected))
      stopifnot(identical(inspect(root, selector, position), expected))
      actual <- capture(inspect(derived, selector, position))
      cat(position, label, if (metadata_refusal(actual)) "unsafe metadata refusal" else "success", "\n")
      stopifnot(if (assert_contract) identical(actual, expected) else metadata_refusal(actual))
    }
    for (q in list(rlang::quo(a), rlang::quo(all_of(.env$pick)))) {
      stopifnot(identical(inspect(derived, q, position), list("a")))
    }
    stopifnot(metadata_refusal(capture(inspect(derived, rlang::quo(where(is.character)), position))))
  }
  stopifnot(input_reads == 0L)

  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, data, "name_only")
  recorded <- new.env(parent = emptyenv())
  old <- options(marginplyr.grouping_hunt_queries = recorded)
  on.exit(options(old), add = TRUE)
  suppressMessages(trace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    tracer = quote({
      observed <- getOption("marginplyr.grouping_hunt_queries")
      observed$sql <- c(observed$sql, statement)
    }),
    where = asNamespace("DBI"), print = FALSE
  ))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)
  pick <- "absent"
  for (position in c("by", "grouping")) {
    cases <- list(
      plain = rlang::quo(c(a, absent)),
      wrapped = rlang::quo(all_of(.env$pick)),
      null_walk = rlang::quo(c(NULL, a, absent)),
      null_helper = rlang::quo(c(all_of(NULL), a, absent)),
      env_dollar = rlang::quo(.env$pick),
      env_bracket = rlang::quo(.env[["pick"]])
    )
    for (label in names(cases)) {
      recorded$sql <- character()
      err <- capture(inspect(remote, cases[[label]], position))
      stopifnot(inherits(err, "error"))
      stopifnot(grepl("absent", conditionMessage(err), fixed = TRUE))
      cat(position, label, "DuckDB query count", length(recorded$sql), "\n")
      expected_queries <- if (label %in% c("plain", "wrapped") || assert_contract) 0L else 1L
      stopifnot(length(recorded$sql) == expected_queries,
                all(grepl("LIMIT 0", recorded$sql, fixed = TRUE)))
    }
  }
}
run()
