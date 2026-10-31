suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
options(lifecycle_verbosity = "quiet", width = 110)

# The trace observes actual DBI invocation, rather than inferring execution
# from the package's construction-time Sent query record.
run_reproduction <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(x = 1L), "grouping_preflight", temporary = TRUE
  )
  unsafe <- dtplyr::lazy_dt(data.frame(x = 1L)) |>
    dplyr::mutate(z = as.character(x))
  assign("grouping_query_log", character(), envir = globalenv())
  on.exit(rm("grouping_query_log", envir = globalenv()), add = TRUE)
  invisible(suppressMessages(trace(
    "dbSendQuery",
    signature = c("duckdb_connection", "character"),
    tracer = quote({
      .GlobalEnv$grouping_query_log <- c(.GlobalEnv$grouping_query_log, statement)
    }),
    where = asNamespace("DBI"), print = FALSE
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)
  observe <- function(x) tryCatch(force(x), error = identity)
  effects <- 0L
  control <- grouping_set(absent, x)
  mixed <- grouping_set(absent, where({ effects <<- effects + 1L; is.numeric }))
  probe <- function(data, spec) {
    assign("grouping_query_log", character(), envir = globalenv())
    result <- suppressMessages(observe(inspect_grouping(data, .grouping = spec)))
    list(result = result, queries = get("grouping_query_log", globalenv()))
  }
  ordinary <- probe(data.frame(x = 1L), mixed)
  duck_control <- probe(remote, control)
  duck_mixed <- probe(remote, mixed)
  dt_control <- probe(unsafe, control)
  dt_mixed <- probe(unsafe, mixed)
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(
      inherits(ordinary$result, "vctrs_error_subscript_oob"),
      length(duck_mixed$queries) == 0L,
      identical(class(dt_mixed$result), class(ordinary$result)),
      effects == 0L
    )
    cat("Desired zero-query and leading-error behavior agreed.\n")
    return(invisible(NULL))
  }
  stopifnot(
    inherits(ordinary$result, "vctrs_error_subscript_oob"),
    identical(class(duck_control$result), class(ordinary$result)),
    identical(class(duck_mixed$result), class(ordinary$result)),
    identical(conditionMessage(duck_control$result), conditionMessage(duck_mixed$result)),
    length(duck_control$queries) == 0L,
    length(duck_mixed$queries) == 1L,
    grepl("LIMIT 0", duck_mixed$queries, fixed = TRUE),
    identical(class(dt_control$result), class(ordinary$result)),
    inherits(dt_mixed$result, "marginplyr_error"),
    grepl("Can't safely determine column metadata", conditionMessage(dt_mixed$result), fixed = TRUE),
    effects == 0L
  )
  invalid_effects <- effects
  valid <- probe(remote, grouping_set(x, where({ effects <<- effects + 1L; is.numeric })))
  stopifnot(!inherits(valid$result, "error"), length(valid$queries) == 1L, effects == 1L)
  cat("Positive predicate control:", effects, "\n")
  cat("DuckDB control queries:", length(duck_control$queries), "\n")
  cat("DuckDB mixed queries:", length(duck_mixed$queries), "\n")
  cat("DuckDB query:", gsub("\n", " ", duck_mixed$queries, fixed = TRUE), "\n")
  cat("dtplyr control:", class(dt_control$result)[[1L]], "\n")
  cat("dtplyr mixed:", class(dt_mixed$result)[[1L]], "\n")
  cat("Later predicate effects:", invalid_effects, "\n")
}
run_reproduction()
