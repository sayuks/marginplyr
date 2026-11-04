pkgload::load_all(quiet = TRUE)

main <- function(assert_contract) {
  data <- data.frame(a = 1L, b = "x")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, data, "duplicate_prefix")
  derived <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(b = toupper(b))
  materialized <- dplyr::collect(derived)
  counter <- new.env(parent = emptyenv())
  counter$queries <- 0L

  # Count actual calls through the DBI method. Capture the closure with
  # do.call so the S4 trace installed in duckdb can see the private counter.
  signature <- c("duckdb_connection", "character")
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    print = FALSE, tracer = function() {
      counter$queries <- counter$queries + 1L
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE)

  calls <- 0L
  predicate <- function(x) {
    calls <<- calls + 1L
    is.character(x)
  }
  capture <- function(input, spec) {
    tryCatch(inspect_grouping(input, .grouping = spec), error = identity)
  }
  mixed <- grouping_sets(a, a, where(predicate))

  original <- capture(remote, grouping_sets(a, a, b))
  stopifnot(
    inherits(original, "marginplyr_error"),
    grepl("Duplicate grouping sets", conditionMessage(original), fixed = TRUE),
    counter$queries == 0L,
    calls == 0L
  )

  local <- capture(materialized, mixed)
  stopifnot(identical(conditionMessage(local), conditionMessage(original)))
  stopifnot(calls %in% c(0L, 2L))
  calls <- 0L

  observed_remote <- capture(remote, mixed)
  remote_queries <- counter$queries
  remote_calls <- calls
  observed_derived <- capture(derived, mixed)

  if (assert_contract) {
    stopifnot(
      identical(conditionMessage(observed_remote), conditionMessage(original)),
      identical(conditionMessage(observed_derived), conditionMessage(original)),
      remote_queries == 0L,
      remote_calls == 0L,
      calls == 0L
    )
  } else {
    stopifnot(
      identical(conditionMessage(observed_remote), conditionMessage(original)),
      inherits(observed_derived, "marginplyr_error"),
      grepl("Can't safely determine column metadata",
            conditionMessage(observed_derived), fixed = TRUE),
      remote_queries == 1L,
      remote_calls == 2L,
      calls == remote_calls
    )
  }

  cat("DuckDB metadata queries:", remote_queries, "\n")
  cat("DuckDB predicate evaluations:", remote_calls, "\n")
  cat("Derived dtplyr condition:", conditionMessage(observed_derived), "\n")
}

main("--assert" %in% commandArgs(trailingOnly = TRUE))
